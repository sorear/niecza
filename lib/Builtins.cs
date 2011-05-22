using Niecza;
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace Niecza {
    public class UpCallee: CrossDomainReceiver {
        public override string[] Call(AppDomain from, string[] args) {
            return Builtins.UnboxLoS(Kernel.RunInferior(Builtins.upcall_cb.Fetch().Invoke(
                Kernel.GetInferiorRoot(), new Variable[] { Builtins.BoxLoS(args) },
                null)));
        }
    }
}

public class Builtins {
    public static P6any NominalCheck(string name, STable mo, Variable v) {
        P6any r = v.Fetch();
        if (!r.mo.HasMRO(mo))
            throw new NieczaException("Nominal type check failed for " + name +
                    " needed " + mo.name + " got " + r.mo.name);
        return r;
    }

    public static void AssignV(Variable lhs, P6any rhs) {
        if (!lhs.islist) {
            lhs.Store(rhs);
        } else {
            Frame n = lhs.Fetch().InvokeMethod(Kernel.GetInferiorRoot(),
                    "LISTSTORE",
                    new Variable[2] { lhs, Kernel.NewROScalar(rhs) }, null);
            Kernel.RunInferior(n);
        }
    }

    class SubstrLValue: Variable {
        Variable backing;
        int from;
        int length;

        public SubstrLValue(Variable backing, int from, int length) {
            this.backing = backing;
            this.from = from;
            this.length = length;
            // XXX Should binding a substr lvalue count as binding the original?
            this.whence = null;
            this.rw = backing.rw;
            this.type = Kernel.StrMO;
        }

        public override P6any Fetch() {
            string str = backing.Fetch().mo.mro_raw_Str.Get(backing);
            return Kernel.BoxRaw<string>(Builtins.LaxSubstring2(str, from, length), Kernel.StrMO);
        }

        public override void Store(P6any v) {
            string str = backing.Fetch().mo.mro_raw_Str.Get(backing);
            int left = (from < 0) ? 0 : (from > str.Length) ? str.Length : from;
            int right = ((length > (str.Length - left)) ? (str.Length - left) :
                (length < 0) ? 0 : length) + left;
            string lfr = str.Substring(0, left);
            string mfr = v.mo.mro_raw_Str.Get(Kernel.NewROScalar(v));
            string rfr = str.Substring(right);
            backing.Store(Kernel.BoxRaw<string>(lfr + mfr + rfr, Kernel.StrMO));
        }

        public override Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Kernel.ScalarMO);
        }
    }

    public static string LaxSubstring(string str, int from) {
        if (from <= 0)
            return str;
        if (from >= str.Length)
            return "";
        return str.Substring(from);
    }

    public static string LaxSubstring2(string str, int from, int l) {
        if (from <= 0) from = 0;
        if (from >= str.Length) from = str.Length;
        if (l >= str.Length - from) l = str.Length - from;
        if (l < 0) l = 0;
        return str.Substring(from, l);
    }

    public const int NR_FIXINT  = 0;
    public const int NR_BIGINT  = 1;
    public const int NR_FIXRAT  = 2;
    public const int NR_FATRAT  = 3;
    public const int NR_FLOAT   = 4;
    public const int NR_COMPLEX = 5;

    public static int GetNumRank(P6any vret) {
        if (vret.mo.HasMRO(Kernel.ComplexMO)) return NR_COMPLEX;
        if (vret.mo.HasMRO(Kernel.NumMO)) return NR_FLOAT;
        if (vret.mo.HasMRO(Kernel.FatRatMO)) return NR_FATRAT;
        if (vret.mo.HasMRO(Kernel.RatMO)) return NR_FIXRAT;
        if (vret.mo.HasMRO(Kernel.IntMO)) {
            return (vret is BoxObject<BigInteger>) ? NR_BIGINT : NR_FIXINT;
        }
        throw new NieczaException("Not a valid primitive number " + vret.mo.name);
    }

    public static Complex PromoteToComplex(int rank, P6any vret) {
        Rat r; FatRat fr;
        if (!vret.IsDefined()) return new Complex(0,0);

        switch (rank) {
            case NR_FIXINT:
                return new Complex(Kernel.UnboxAny<int>(vret), 0);
            case NR_BIGINT:
                return new Complex((double)Kernel.UnboxAny<BigInteger>(vret), 0);
            case NR_FIXRAT:
                r = Kernel.UnboxAny<Rat>(vret);
                return new Complex((double)r.num / (double)r.den, 0);
            case NR_FATRAT:
                fr = Kernel.UnboxAny<FatRat>(vret);
                return new Complex((double)fr.num / (double)fr.den, 0);
            case NR_FLOAT:
                return new Complex(Kernel.UnboxAny<double>(vret), 0);
            case NR_COMPLEX:
            default:
                return Kernel.UnboxAny<Complex>(vret);
        }
    }

    public static double PromoteToFloat(int rank, P6any vret) {
        Rat r; FatRat fr;
        if (!vret.IsDefined()) return 0;

        switch (rank) {
            case NR_FIXINT:
                return Kernel.UnboxAny<int>(vret);
            case NR_BIGINT:
                return (double)Kernel.UnboxAny<BigInteger>(vret);
            case NR_FIXRAT:
                r = Kernel.UnboxAny<Rat>(vret);
                return (double)r.num / (double)r.den;
            case NR_FATRAT:
                fr = Kernel.UnboxAny<FatRat>(vret);
                return (double)fr.num / (double)fr.den;
            case NR_FLOAT:
            default:
                return Kernel.UnboxAny<double>(vret);
        }
    }

    public static FatRat PromoteToFatRat(int rank, P6any vret) {
        Rat r;
        if (!vret.IsDefined()) return new FatRat(BigInteger.Zero,BigInteger.One);

        switch (rank) {
            case NR_FIXINT:
                return new FatRat(Kernel.UnboxAny<int>(vret), BigInteger.One);
            case NR_BIGINT:
                return new FatRat(Kernel.UnboxAny<BigInteger>(vret), BigInteger.One);
            case NR_FIXRAT:
                r = Kernel.UnboxAny<Rat>(vret);
                return new FatRat(r.num, r.den);
            case NR_FATRAT:
            default:
                return Kernel.UnboxAny<FatRat>(vret);
        }
    }

    public static Rat PromoteToFixRat(int rank, P6any vret) {
        if (!vret.IsDefined()) return new Rat(BigInteger.Zero, 1);

        switch (rank) {
            case NR_FIXINT:
                return new Rat(Kernel.UnboxAny<int>(vret), 1);
            case NR_BIGINT:
                return new Rat(Kernel.UnboxAny<BigInteger>(vret), 1);
            case NR_FIXRAT:
            default:
                return Kernel.UnboxAny<Rat>(vret);
        }
    }

    public static BigInteger PromoteToBigInt(int rank, P6any vret) {
        if (!vret.IsDefined()) return BigInteger.Zero;

        switch (rank) {
            case NR_FIXINT:
                return Kernel.UnboxAny<int>(vret);
            case NR_BIGINT:
            default:
                return Kernel.UnboxAny<BigInteger>(vret);
        }
    }

    public static int PromoteToFixInt(int rank, P6any vret) {
        if (!vret.IsDefined()) return 0;
        return Kernel.UnboxAny<int>(vret);
    }

    public static Variable MakeInt(int v) {
        return Kernel.BoxAnyMO<int>(v, Kernel.IntMO);
    }

    public static Variable MakeInt(BigInteger v) {
        int vs;
        if (v.AsInt32(out vs)) return Kernel.BoxAnyMO<int>(vs, Kernel.IntMO);
        else return Kernel.BoxAnyMO<BigInteger>(v, Kernel.IntMO);
    }

    public static Variable MakeInt(long v) {
        if (v <= (long)int.MaxValue && v >= (long)int.MinValue)
            return Kernel.BoxAnyMO<int>((int)v, Kernel.IntMO);
        else return Kernel.BoxAnyMO<BigInteger>(v, Kernel.IntMO);
    }

    public static void GetAsRational(Variable v,
            out BigInteger num, out BigInteger den) {
        P6any n = v.Fetch().mo.mro_Numeric.Get(v).Fetch();
        int rk = GetNumRank(n);

        if (rk == NR_COMPLEX || rk == NR_FLOAT) {
            double dbl = 0;
            if (rk == NR_COMPLEX) {
                Complex c = Kernel.UnboxAny<Complex>(n);
                if (c.im != 0)
                    throw new NieczaException("Complex cannot be used here");
                dbl = c.re;
            } else {
                dbl = Kernel.UnboxAny<double>(n);
            }
            ulong bits = (ulong)BitConverter.DoubleToInt64Bits(dbl);
            num = (bits & ((1UL << 52) - 1)) + (1UL << 52);
            den = (1UL << 52);
            if ((bits & (1UL << 63)) != 0) num = -num;
            int power = ((int)((bits >> 52) & 0x7FF)) - 0x3FF;
            if (power > 0) num <<= power;
            else den <<= -power;
            SimplifyFrac(ref num, ref den);
        }
        else if (rk == NR_FATRAT) {
            FatRat r = Kernel.UnboxAny<FatRat>(n);
            num = r.num; den = r.den;
        }
        else if (rk == NR_FIXRAT) {
            Rat r = Kernel.UnboxAny<Rat>(n);
            num = r.num; den = r.den;
        }
        else if (rk == NR_BIGINT) {
            num = Kernel.UnboxAny<BigInteger>(n); den = BigInteger.One;
        }
        else {
            num = Kernel.UnboxAny<int>(n); den = BigInteger.One;
        }
    }

    public static void SimplifyFrac(ref BigInteger num, ref BigInteger den) {
        if (den.Sign < 0) {
            den = -den;
            num = -num;
        }
        if (num.Sign == 0) {
            den = BigInteger.One;
        }
        if (num.Sign != 0 && den.Sign != 0) {
            BigInteger g = BigInteger.GreatestCommonDivisor(num, den);
            if (g != BigInteger.One) {
                num /= g;
                den /= g;
            }
        }
    }

    public static Variable MakeFixRat(BigInteger num, BigInteger den) {
        ulong sden;
        SimplifyFrac(ref num, ref den);
        if (den.AsUInt64(out sden) && sden != 0)
            return Kernel.BoxAnyMO<Rat>(new Rat(num, sden), Kernel.RatMO);
        return MakeFloat((double)num / (double)den);
    }

    public static Variable MakeFatRat(BigInteger num, BigInteger den) {
        SimplifyFrac(ref num, ref den);
        if (den.Sign != 0)
            return Kernel.BoxAnyMO<FatRat>(new FatRat(num, den), Kernel.FatRatMO);
        return MakeFloat(den.Sign * double.PositiveInfinity);
    }

    public static Variable MakeFloat(double val) {
        return Kernel.BoxAnyMO<double>(val, Kernel.NumMO);
    }

    public static Variable MakeComplex(double re, double im) {
        return Kernel.BoxAnyMO<Complex>(new Complex(re, im), Kernel.ComplexMO);
    }

    public static Variable NumericEq(Variable v1, Variable v2) {
        return Compare(v1,v2) == 0 ? Kernel.TrueV : Kernel.FalseV;
    }

    public static Variable NumericLt(Variable v1, Variable v2) {
        return Compare(v1,v2) < 0 ? Kernel.TrueV : Kernel.FalseV;
    }

    public static Variable NumericNe(Variable v1, Variable v2) {
        return Compare(v1,v2) != 0 ? Kernel.TrueV : Kernel.FalseV;
    }

    public static Variable NumericLe(Variable v1, Variable v2) {
        return Compare(v1,v2) <= 0 ? Kernel.TrueV : Kernel.FalseV;
    }

    public static Variable NumericGt(Variable v1, Variable v2) {
        return Compare(v1,v2) > 0 ? Kernel.TrueV : Kernel.FalseV;
    }

    public static Variable NumericGe(Variable v1, Variable v2) {
        return Compare(v1,v2) >= 0 ? Kernel.TrueV : Kernel.FalseV;
    }

    public static Variable StringEq(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Str.Get(v1) == o2.mo.mro_raw_Str.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringNe(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (o1.mo.mro_raw_Str.Get(v1) != o2.mo.mro_raw_Str.Get(v2)) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringLt(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) < 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringLe(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) <= 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringGe(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) >= 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable StringGt(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        if (string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                    o2.mo.mro_raw_Str.Get(v2)) > 0) {
            return Kernel.TrueV;
        } else {
            return Kernel.FalseV;
        }
    }

    public static Variable Substr3(Variable v1, Variable v2, Variable v3) {
        P6any o2 = NominalCheck("$start", Kernel.AnyMO, v2);
        P6any o3 = NominalCheck("$chars", Kernel.AnyMO, v3);
        int r2    = (int)o2.mo.mro_raw_Numeric.Get(v2);
        int r3    = (int)o3.mo.mro_raw_Numeric.Get(v3);
        return new SubstrLValue(v1, r2, r3);
    }

    public static Variable Plus(Variable a1, Variable a2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, a1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, a2);
        P6any n1 = o1.mo.mro_Numeric.Get(a1).Fetch();
        int r1 = GetNumRank(n1);
        P6any n2 = o2.mo.mro_Numeric.Get(a2).Fetch();
        int r2 = GetNumRank(n2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            Complex v2 = PromoteToComplex(r2, n2);
            return MakeComplex(v1.re + v2.re, v1.im + v2.im);
        }
        if (r1 == NR_FLOAT || r2 == NR_FLOAT) {
            return MakeFloat(PromoteToFloat(r1, n1) + PromoteToFloat(r2, n2));
        }
        if (r1 == NR_FATRAT || r2 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            FatRat v2 = PromoteToFatRat(r2, n2);

            return MakeFatRat(v1.num*v2.den + v2.num*v1.den, v1.den*v2.den);
        }
        if (r1 == NR_FIXRAT || r2 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            Rat v2 = PromoteToFixRat(r2, n2);

            return MakeFixRat(v1.num*v2.den + v2.num*v1.den, ((BigInteger)v1.den)*v2.den);
        }
        if (r1 == NR_BIGINT || r2 == NR_BIGINT) {
            return MakeInt(PromoteToBigInt(r1, n1) + PromoteToBigInt(r2, n2));
        }
        return MakeInt((long)PromoteToFixInt(r1, n1) +
                (long)PromoteToFixInt(r2, n2));
    }

    public static Variable Minus(Variable a1, Variable a2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, a1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, a2);
        P6any n1 = o1.mo.mro_Numeric.Get(a1).Fetch();
        int r1 = GetNumRank(n1);
        P6any n2 = o2.mo.mro_Numeric.Get(a2).Fetch();
        int r2 = GetNumRank(n2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            Complex v2 = PromoteToComplex(r2, n2);
            return MakeComplex(v1.re - v2.re, v1.im - v2.im);
        }
        if (r1 == NR_FLOAT || r2 == NR_FLOAT) {
            return MakeFloat(PromoteToFloat(r1, n1) - PromoteToFloat(r2, n2));
        }
        if (r1 == NR_FATRAT || r2 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            FatRat v2 = PromoteToFatRat(r2, n2);

            return MakeFatRat(v1.num*v2.den - v2.num*v1.den, v1.den*v2.den);
        }
        if (r1 == NR_FIXRAT || r2 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            Rat v2 = PromoteToFixRat(r2, n2);

            return MakeFixRat(v1.num*v2.den - v2.num*v1.den, ((BigInteger)v1.den)*v2.den);
        }
        if (r1 == NR_BIGINT || r2 == NR_BIGINT) {
            return MakeInt(PromoteToBigInt(r1, n1) - PromoteToBigInt(r2, n2));
        }
        return MakeInt((long)PromoteToFixInt(r1, n1) -
                (long)PromoteToFixInt(r2, n2));
    }

    public static Variable Negate(Variable a1) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, a1);
        P6any n1 = o1.mo.mro_Numeric.Get(a1).Fetch();
        int r1 = GetNumRank(n1);

        if (r1 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            return MakeComplex(-v1.re, -v1.im);
        }
        if (r1 == NR_FLOAT) {
            return MakeFloat(-PromoteToFloat(r1, n1));
        }
        if (r1 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            return MakeFatRat(-v1.num, v1.den);
        }
        if (r1 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            return MakeFixRat(-v1.num, v1.den);
        }
        if (r1 == NR_BIGINT) {
            return MakeInt(-PromoteToBigInt(r1, n1));
        }
        return MakeInt(-(long)PromoteToFixInt(r1, n1));
    }

    public static int Compare(Variable a1, Variable a2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, a1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, a2);
        P6any n1 = o1.mo.mro_Numeric.Get(a1).Fetch();
        int r1 = GetNumRank(n1);
        P6any n2 = o2.mo.mro_Numeric.Get(a2).Fetch();
        int r2 = GetNumRank(n2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            Complex v2 = PromoteToComplex(r2, n2);
            if (v1.re != v2.re)
                return v1.re > v2.re ? 1 : -1;
            else
                return v1.im > v2.im ? 1 : v1.im < v2.im ? -1 : 0;
        }
        if (r1 == NR_FLOAT || r2 == NR_FLOAT) {
            double df = PromoteToFloat(r1, n1) - PromoteToFloat(r2, n2);
            return df > 0 ? 1 : df < 0 ? -1 : 0;
        }
        if (r1 == NR_FATRAT || r2 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            FatRat v2 = PromoteToFatRat(r2, n2);

            return BigInteger.Compare(v1.num*v2.den, v2.num*v1.den);
        }
        if (r1 == NR_FIXRAT || r2 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            Rat v2 = PromoteToFixRat(r2, n2);

            return BigInteger.Compare(v1.num*v2.den, v2.num*v1.den);
        }
        if (r1 == NR_BIGINT || r2 == NR_BIGINT) {
            return BigInteger.Compare(PromoteToBigInt(r1, n1),
                    PromoteToBigInt(r2, n2));
        }
        return PromoteToFixInt(r1, n1).CompareTo(PromoteToFixInt(r2, n2));
    }

    public static Variable Mul(Variable a1, Variable a2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, a1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, a2);
        P6any n1 = o1.mo.mro_Numeric.Get(a1).Fetch();
        int r1 = GetNumRank(n1);
        P6any n2 = o2.mo.mro_Numeric.Get(a2).Fetch();
        int r2 = GetNumRank(n2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            Complex v2 = PromoteToComplex(r2, n2);
            return MakeComplex(v1.re*v2.re - v1.im*v2.im, v1.im*v2.re + v1.re*v2.im);
        }
        if (r1 == NR_FLOAT || r2 == NR_FLOAT) {
            return MakeFloat(PromoteToFloat(r1, n1) * PromoteToFloat(r2, n2));
        }
        if (r1 == NR_FATRAT || r2 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            FatRat v2 = PromoteToFatRat(r2, n2);

            return MakeFatRat(v1.num*v2.num, v1.den*v2.den);
        }
        if (r1 == NR_FIXRAT || r2 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            Rat v2 = PromoteToFixRat(r2, n2);

            return MakeFixRat(v1.num*v2.num, ((BigInteger)v1.den)*v2.den);
        }
        if (r1 == NR_BIGINT || r2 == NR_BIGINT) {
            return MakeInt(PromoteToBigInt(r1, n1) * PromoteToBigInt(r2, n2));
        }
        return MakeInt((long)PromoteToFixInt(r1, n1) *
                (long)PromoteToFixInt(r2, n2));
    }

    public static Variable Divide(Variable a1, Variable a2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, a1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, a2);
        P6any n1 = o1.mo.mro_Numeric.Get(a1).Fetch();
        int r1 = GetNumRank(n1);
        P6any n2 = o2.mo.mro_Numeric.Get(a2).Fetch();
        int r2 = GetNumRank(n2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            Complex v2 = PromoteToComplex(r2, n2);
            double sn2 = v2.re*v2.re + v2.im*v2.im;
            return MakeComplex((v1.re*v2.re + v1.im*v2.im)/sn2,
                    (v2.re*v1.im - v2.im*v1.re)/sn2);
        }
        if (r1 == NR_FLOAT || r2 == NR_FLOAT) {
            return MakeFloat(PromoteToFloat(r1, n1) / PromoteToFloat(r2, n2));
        }
        if (r1 == NR_FATRAT || r2 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            FatRat v2 = PromoteToFatRat(r2, n2);

            return MakeFatRat(v1.num*v2.den, v1.den*v2.num);
        }
        if (r1 == NR_FIXRAT || r2 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            Rat v2 = PromoteToFixRat(r2, n2);

            return MakeFixRat(v1.num*v2.den, v2.num*v1.den);
        }
        if (r1 == NR_BIGINT || r2 == NR_BIGINT) {
            return MakeFixRat(PromoteToBigInt(r1, n1), PromoteToBigInt(r2, n2));
        }
        return MakeFixRat(PromoteToFixInt(r1, n1), PromoteToFixInt(r2, n2));
    }

    public static Variable Mod(Variable a1, Variable a2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, a1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, a2);
        P6any n1 = o1.mo.mro_Numeric.Get(a1).Fetch();
        int r1 = GetNumRank(n1);
        P6any n2 = o2.mo.mro_Numeric.Get(a2).Fetch();
        int r2 = GetNumRank(n2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            throw new NieczaException("Modulus operation not defined with complex arguments");
        }
        if (r1 == NR_FLOAT || r2 == NR_FLOAT) {
            double v1 = PromoteToFloat(r1, n1);
            double v2 = PromoteToFloat(r2, n2);
            return MakeFloat(v1 - v2 * Math.Floor(v1 / v2));
        }
        if (r1 == NR_FATRAT || r2 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            FatRat v2 = PromoteToFatRat(r2, n2);

            BigInteger c1 = v1.num*v2.den;
            BigInteger c2 = v2.num*v1.den;
            BigInteger cd = v1.den*v2.den;

            BigInteger rem;
            BigInteger red = BigInteger.DivRem(c1, c2, out rem);
            if (red.Sign < 0 && rem.Sign != 0) red--;

            return MakeFatRat(c1 - red*cd, cd);
        }
        if (r1 == NR_FIXRAT || r2 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            Rat v2 = PromoteToFixRat(r2, n2);

            BigInteger c1 = v1.num*v2.den;
            BigInteger c2 = v2.num*v1.den;
            BigInteger cd = ((BigInteger)v1.den)*v2.den;

            BigInteger rem;
            BigInteger red = BigInteger.DivRem(c1, c2, out rem);
            if (red.Sign < 0 && rem.Sign != 0) red--;

            return MakeFixRat(c1 - red*c2, cd);
        }
        if (r1 == NR_BIGINT || r2 == NR_BIGINT) {
            BigInteger v1 = PromoteToBigInt(r1, n1);
            BigInteger v2 = PromoteToBigInt(r2, n2);
            BigInteger rem;
            BigInteger red = BigInteger.DivRem(v1, v2, out rem);
            if (red.Sign < 0 && rem.Sign != 0) red--;
            return MakeInt(v1 - v2*red);
        }
        {
            long v1 = PromoteToFixInt(r1, n1);
            long v2 = PromoteToFixInt(r2, n2);
            long rem;
            long red = Math.DivRem(v1, v2, out rem);
            if (red < 0 && rem != 0) red--;
            return MakeInt(v1 - v2*red);
        }
    }

    public static Variable NumAnd(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 & r2);
    }

    public static Variable NumOr(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 | r2);
    }

    public static Variable NumXor(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 ^ r2);
    }

    public static Variable NumLShift(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 << r2);
    }

    public static Variable NumRShift(Variable v1, Variable v2) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        P6any o2 = NominalCheck("$y", Kernel.AnyMO, v2);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 >> r2);
    }

    public static Variable NumCompl(Variable v1) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v1);
        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        return MakeInt(~r1);
    }

    public static Variable RatApprox(Variable v1, Variable v2) {
        NominalCheck("$x", Kernel.AnyMO, v1);
        NominalCheck("$y", Kernel.AnyMO, v2);

        BigInteger nc, dc, ne, de, na, da;
        GetAsRational(v1, out nc, out dc);
        GetAsRational(v2, out ne, out de);

        RatApproxer.Simplest(nc*de-ne*dc,dc*de,nc*de+ne*dc,dc*de,out na,out da);
        SimplifyFrac(ref na, ref da);

        // since the user controls the denominator size here, use FatRat freely
        // XXX: is it appropriate to return FatRat from a method named Rat?
        ulong sda;
        if (da.AsUInt64(out sda)) {
            return MakeFixRat(na,da);
        } else {
            return MakeFatRat(na,da);
        }
    }

    public static Variable PostIncrement(Variable v) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v);
        AssignV(v, o1.mo.mro_succ.Get(v));
        return Kernel.NewROScalar(o1);
    }

    public static Variable Not(Variable v) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v);
        bool r = o1.mo.mro_raw_Bool.Get(v);
        return r ? Kernel.FalseV : Kernel.TrueV;
    }

    public static Variable Chars(Variable v) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v);
        string r = o1.mo.mro_raw_Str.Get(v);
        return MakeInt(r.Length);
    }

    public static Variable Ord(Variable v) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v);
        string r = o1.mo.mro_raw_Str.Get(v);
        // XXX Failure
        if (r.Length == 0) return Kernel.NewROScalar(Kernel.AnyP);
        return MakeInt((int)r[0]);
    }

    public static Variable Chr(Variable v) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v);
        double r = o1.mo.mro_raw_Numeric.Get(v);
        return Kernel.BoxAnyMO(new string((char)r, 1), Kernel.StrMO);
    }

    public static Variable UniCat(Variable v) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v);
        char c = (char) o1.mo.mro_raw_Numeric.Get(v);
        int ix = (int) char.GetUnicodeCategory(c);
        return MakeInt(ix);
    }

    public static Variable Make(Frame fr, Variable v) {
        if (fr.info.name == "CORE make")
            fr = fr.caller;
        Cursor c = (Cursor) Kernel.StatusHelper(fr, "$*/", 0).Fetch();
        c.Make(v);
        return v;
    }

    public static VarDeque HashIterRaw(int mode, Variable v) {
        P6any o = NominalCheck("$x", Kernel.AnyMO, v);
        VarHash d = Kernel.UnboxAny<VarHash>(o);

        VarDeque lv = new VarDeque();

        foreach (KeyValuePair<string,Variable> kv in d) {
            switch (mode) {
                case 0:
                    lv.Push(Kernel.BoxAnyMO<string>(kv.Key, Kernel.StrMO));
                    break;
                case 1:
                    lv.Push(kv.Value);
                    break;
                case 2:
                    lv.Push(Kernel.BoxAnyMO<string>(kv.Key, Kernel.StrMO));
                    lv.Push(kv.Value);
                    break;
                case 3:
                    P6opaque p = new P6opaque(Kernel.PairMO);
                    p.slots[0] = Kernel.BoxAnyMO<string>(kv.Key, Kernel.StrMO);
                    p.slots[1] = kv.Value;
                    lv.Push(Kernel.NewROScalar(p));
                    break;
            }
        }
        return lv;
    }
    public static Variable HashIter(int mode, Variable v) {
        VarDeque lv = HashIterRaw(mode, v);
        P6opaque l = new P6opaque(Kernel.ListMO);
        l.slots[0] = lv;
        l.slots[1] = new VarDeque();
        return Kernel.NewRWListVar(l);
    }

    public static Variable GetModTime(string path) {
        long t = File.GetLastWriteTimeUtc(path).Ticks;
        return MakeFloat(((double)(t - 621355968000000000L)) / 10000000.0);
    }

    public static Variable GetTimeOfDay() {
        long t = DateTime.UtcNow.Ticks;
        return MakeFloat(((double)(t - 621355968000000000L)) / 10000000.0);
    }

    private static Random rng = new Random();

    public static Variable GetRandom() {
        double i;
        lock (rng) { i = rng.NextDouble(); }
        return MakeFloat(i);
    }

    public static bool FileOrDirExists(string path) {
        return File.Exists(path) || Directory.Exists(path);
    }

    public static Variable BoxLoS(string[] los) {
        VarDeque items = new VarDeque();
        foreach (string i in los)
            items.Push(Kernel.BoxAnyMO(i, Kernel.StrMO));
        P6any l = new P6opaque(Kernel.ListMO);
        l.SetSlot("rest", new VarDeque());
        l.SetSlot("items", items);
        return Kernel.NewRWListVar(l);
    }

    public static string[] UnboxLoS(Variable args) {
        List<string> la = new List<string>();
        VarDeque iter = new VarDeque(args);
        while (Kernel.IterHasFlat(iter, true)) {
            Variable v = iter.Shift();
            la.Add(v.Fetch().mo.mro_raw_Str.Get(v));
        }
        return la.ToArray();
    }

    // temporary until compiler is converted to use only downcalls
    public static Variable RunCLRSubtask(Variable filename, Variable args) {
        string sfn = filename.Fetch().mo.mro_raw_Str.Get(filename);
        //Console.WriteLine("App name {0}", sfn);
        int ret = GetSubDomain().ExecuteAssembly(sfn, null, UnboxLoS(args));
        return MakeInt(ret);
    }

    public static void RunSubtask(string file, string args) {
        System.Diagnostics.Process.Start(file, args).WaitForExit();
    }

    private static AppDomain subDomain;
    private static string backend;
    // Better, but still fudgy.  Relies too mcuh on path structure.
    private static AppDomain GetSubDomain() {
        if (subDomain != null) return subDomain;

        AppDomainSetup ads = new AppDomainSetup();
        string obj = Path.GetFullPath(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, Path.Combine("..", "obj")));
        ads.ApplicationBase = obj;
        backend = Path.Combine(obj, "CLRBackend.exe");
        subDomain = AppDomain.CreateDomain("zyg", null, ads);
        return subDomain;
    }
    public static AppDomain up_domain;
    public static Variable upcall_cb;
    public static Variable eval_result;
    public static Variable DownCall(Variable cb, Variable list) {
        GetSubDomain();
        upcall_cb = cb;
        CrossDomainReceiver r = (CrossDomainReceiver)
            subDomain.CreateInstanceFromAndUnwrap(backend,
                    "Niecza.CLRBackend.DownCallAcceptor");
        return BoxLoS(r.Call(AppDomain.CurrentDomain, UnboxLoS(list)));
    }

    public static Variable SimpleEval(Variable str) {
        if (up_domain == null)
            throw new NieczaException("Cannot eval; no compiler available");
        CrossDomainReceiver r = (CrossDomainReceiver)
            up_domain.CreateInstanceAndUnwrap("Kernel", "Niecza.UpCallee");
        string[] msg = r.Call(AppDomain.CurrentDomain, new string[] { "eval",
                str.Fetch().mo.mro_raw_Str.Get(str) });
        if (msg[0] != "")
            throw new NieczaException(msg[0]);
        Variable rt = eval_result;
        eval_result = null;
        return rt;
    }

    public static Variable ArrayConstructor(Variable bits) {
        VarDeque rest  = new VarDeque(bits);
        VarDeque items = new VarDeque();
        while (Kernel.IterHasFlat(rest, true))
            items.Push(Kernel.NewRWScalar(Kernel.AnyMO, rest.Shift().Fetch()));
        P6any l = new P6opaque(Kernel.ArrayMO);
        l.SetSlot("rest", rest);
        l.SetSlot("items", items);
        return Kernel.NewROScalar(l);
    }

    public static int GetArity(P6any fcni) {
        if (!fcni.Isa(Kernel.SubMO))
            return 1; // can't introspect fake subs (?)
        SubInfo si = (SubInfo) fcni.GetSlot("info");
        int[] sig = si.sig_i;
        if (sig == null)
            return 1;
        int arity = 0;
        for (int i = 0; i < sig.Length; i += SubInfo.SIG_I_RECORD) {
            int fl = sig[i + SubInfo.SIG_I_FLAGS];
            if ((fl & (SubInfo.SIG_F_SLURPY_CAP | SubInfo.SIG_F_SLURPY_POS |
                    SubInfo.SIG_F_SLURPY_PCL)) != 0)
                return int.MaxValue;
            if ((fl & SubInfo.SIG_F_POSITIONAL) == 0) continue;
            arity++;
        }
        return arity;
    }

    class ItemSource {
        protected ItemSource() {}
        public static ItemSource Empty = new ItemSource();
        public virtual bool TryGet(out Variable[] r, bool block) {
            r = null;
            return true;
        }
        protected static int TryOne(VarDeque items, bool block) {
            if (block) {
                return Kernel.IterHasFlat(items, true) ? +1 : -1;
            } else {
again:
                if (items.Count() == 0) return -1;
                Variable v = items[0];
                P6any i = v.Fetch();
                if (i.mo.HasMRO(Kernel.IterCursorMO))
                    return 0;
                if (v.islist) {
                    items.Shift();
                    items.UnshiftD(i.mo.mro_raw_iterator.Get(v));
                    goto again;
                }
                return +1;
            }
        }
    }

    class BatchSource: ItemSource {
        int arity;
        VarDeque items;

        public BatchSource(int count, VarDeque items) {
            this.arity = count;
            this.items = items;
        }

        public override bool TryGet(out Variable[] r, bool block) {
            r = null;
            List<Variable> pen = new List<Variable>();
            while (pen.Count < arity) {
                switch (TryOne(items, block)) {
                    case -1: goto nomore;
                    case 0:
                        for (int i = pen.Count - 1; i >= 0; i--)
                            items.Unshift(pen[i]);
                        return false;
                    case +1: pen.Add(items.Shift()); break;
                }
            }
nomore:
            if (pen.Count != 0)
                r = pen.ToArray();
            return true;
        }
    }

    class ZipSource : ItemSource {
        VarDeque[] sources;
        public ZipSource(Variable[] pcl) {
            sources = new VarDeque[pcl.Length];
            for (int i = 0; i < pcl.Length; i++)
                sources[i] = new VarDeque(pcl[i]);
        }

        public override bool TryGet(out Variable[] r, bool block) {
            r = null;
            for (int i = 0; i < sources.Length; i++)
                switch (TryOne(sources[i], block)) {
                    case -1: return true;
                    case  0: return false;
                }
            r = new Variable[sources.Length];
            for (int i = 0; i < sources.Length; i++)
                r[i] = sources[i].Shift();
            return true;
        }
    }

    class CrossSource: ItemSource {
        VarDeque[] basic;
        VarDeque[] iter;
        Variable[] basic_top;
        Variable[] iter_top;
        // 0=init 1=end i=advance wheel i-2
        int state;

        public CrossSource(Variable[] pcl) {
            basic = new VarDeque[pcl.Length];
            iter  = new VarDeque[pcl.Length];
            basic_top = new Variable[pcl.Length];
            iter_top  = new Variable[pcl.Length];
            for (int i = 0; i < pcl.Length; i++) {
                iter[i] = new VarDeque(pcl[i]);
            }
        }

        public override bool TryGet(out Variable[] r, bool block) {
            r = null;
            if (state == 0) {
                // Make sure all the lists are non-empty.
                for (int i = 0; i < iter.Length; i++) {
                    switch (TryOne(iter[i], block)) {
                        case -1: return true;
                        case 0:  return false;
                        case 1:  break;
                    }
                }
                for (int i = 0; i < iter.Length; i++) {
                    iter_top[i] = iter[i].Shift();
                    if (i != 0) {
                        basic[i] = new VarDeque(iter[i]);
                        basic_top[i] = iter_top[i];
                    }
                }
            }
            else if (state == 1) {
                return true;
            }
            else {
again:
                int wheel = state - 2;
                switch (TryOne(iter[wheel], block)) {
                    case 0:  return false;
                    case +1:
                        iter_top[wheel] = iter[wheel].Shift();
                        break;
                    case -1:
                        if (wheel == 0) return true;
                        iter[wheel] = new VarDeque(basic[wheel]);
                        iter_top[wheel] = basic_top[wheel];
                        state--;
                        goto again;
                }
            }
            r = new Variable[iter_top.Length];
            for (int i = 0; i < iter_top.Length; i++)
                r[i] = iter_top[i];
            state = iter_top.Length + 1;
            return true;
        }
    }

    private static SubInfo CommonMEMap_I = new SubInfo("KERNEL map", null,
            CommonMEMap_C, null, null, new int[] {
                2, 3, SubInfo.ON_NEXT, 0, 0,
                2, 3, SubInfo.ON_REDO, 1, 0,
                2, 3, SubInfo.ON_LAST, 3, 0,
            }, new string[] { "" }, 0, null, null);
    private static Frame CommonMEMap_C(Frame th) {
        ItemSource src = (ItemSource) th.lex0;
        VarDeque outq = (VarDeque) th.lex1;
        P6any fnc = (P6any) th.lex2;
        int tailmode = th.lexi0;

        switch (th.ip) {
            case 0:
                Variable[] pen;
                if (!src.TryGet(out pen, tailmode != 0)) {
                    P6opaque thunk = new P6opaque(Kernel.GatherIteratorMO);
                    th.lex = new Dictionary<string,object>();
                    th.lex["!return"] = null;
                    th.MarkSharedChain();
                    thunk.slots[0] = Kernel.NewRWScalar(Kernel.AnyMO, th);
                    thunk.slots[1] = Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP);
                    P6opaque lst = new P6opaque(Kernel.ListMO);
                    lst.slots[0] = outq;
                    lst.slots[1] = new VarDeque(Kernel.NewROScalar(thunk));
                    th.caller.resultSlot = Kernel.NewRWListVar(lst);
                    th.lexi0 = 1;
                    return th.caller;
                }
                if (pen == null) {
                    if (tailmode != 0)
                        return Kernel.Take(th, Kernel.NewROScalar(Kernel.EMPTYP));
                    P6opaque lst = new P6opaque(Kernel.ListMO);
                    lst.slots[0] = outq;
                    lst.slots[1] = new VarDeque();
                    th.caller.resultSlot = Kernel.NewRWListVar(lst);
                    return th.caller;
                }
                th.lex3 = pen;
                th.ip = 1;
                goto case 1;
            case 1:
                th.ip = 2;
                if (fnc != null)
                    return fnc.Invoke(th, (Variable[])th.lex3, null);
                else {
                    th.resultSlot = Kernel.NewRWListVar(
                            Kernel.BoxRaw((Variable[])th.lex3,
                                Kernel.ParcelMO));
                    goto case 2;
                }
            case 2:
                if (tailmode != 0) {
                    th.ip = 0;
                    return Kernel.Take(th, (Variable)th.resultSlot);
                } else {
                    outq.Push((Variable) th.resultSlot);
                    th.ip = 0;
                    goto case 0;
                }
            case 3:
                th.lex0 = src = ItemSource.Empty;
                th.ip = 0;
                goto case 0;
            default:
                return Kernel.Die(th, "Invalid IP");
        }
    }

    public static Frame MEMap(Frame th, Variable[] lst) {
        VarDeque iter = new VarDeque(lst);
        Variable fcn = iter.Shift();
        P6any fcni = fcn.Fetch();
        int arity = GetArity(fcni);

        Frame fr = th.MakeChild(null, CommonMEMap_I);
        fr.lexi0 = 0;
        fr.lex0 = new BatchSource(arity, iter);
        fr.lex1 = new VarDeque();
        fr.lex2 = fcni;
        return fr;
    }

    static P6any ExtractWith(bool with, ref Variable[] pcl) {
        if (!with) return null;
        Variable[] opcl = pcl;
        pcl = new Variable[pcl.Length - 1];
        for (int j = 0; j < pcl.Length; j++)
            pcl[j] = opcl[j+1];
        return opcl[0].Fetch();
    }

    public static Frame MEZip(Frame th, bool with, Variable[] pcl) {
        Frame fr = th.MakeChild(null, CommonMEMap_I);
        fr.lexi0 = 0;
        fr.lex2 = ExtractWith(with, ref pcl);
        fr.lex0 = new ZipSource(pcl);
        fr.lex1 = new VarDeque();
        return fr;
    }

    public static Frame MECross(Frame th, bool with, Variable[] pcl) {
        Frame fr = th.MakeChild(null, CommonMEMap_I);
        fr.lexi0 = 0;
        fr.lex2 = ExtractWith(with, ref pcl);
        fr.lex0 = new CrossSource(pcl);
        fr.lex1 = new VarDeque();
        return fr;
    }

    private static SubInfo CommonGrep_I = new SubInfo("KERNEL grep", null,
            CommonGrep_C, null, null, new int[] {
                2, 3, SubInfo.ON_NEXT, 0, 0,
                2, 3, SubInfo.ON_REDO, 1, 0,
                2, 3, SubInfo.ON_LAST, 3, 0,
            }, new string[] { "" }, 0, null, null);
    private static Frame CommonGrep_C(Frame th) {
        VarDeque src = (VarDeque) th.lex0;
        VarDeque outq = (VarDeque) th.lex1;
        Variable flt = (Variable) th.lex2;
        int tailmode = th.lexi0;

        switch (th.ip) {
            case 0:
                Variable pen = null;
                while (pen == null) {
                    if (tailmode != 0) {
                        if (!Kernel.IterHasFlat(src, false)) break;
                    } else {
                        if (src.Count() == 0) break;
                        if (src[0].Fetch().mo.HasMRO(Kernel.IterCursorMO)) {
                            P6opaque thunk = new P6opaque(Kernel.GatherIteratorMO);
                            th.lex = new Dictionary<string,object>();
                            th.lex["!return"] = null;
                            th.MarkSharedChain();
                            thunk.slots[0] = Kernel.NewRWScalar(Kernel.AnyMO, th);
                            thunk.slots[1] = Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP);
                            P6opaque lst = new P6opaque(Kernel.ListMO);
                            lst.slots[0] = outq;
                            lst.slots[1] = new VarDeque(Kernel.NewROScalar(thunk));
                            th.caller.resultSlot = Kernel.NewRWListVar(lst);
                            th.lexi0 = 1;
                            return th.caller;
                        }
                    }
                    pen = src.Shift();
                }
                if (pen == null) {
                    if (tailmode != 0)
                        return Kernel.Take(th, Kernel.NewROScalar(Kernel.EMPTYP));
                    P6opaque lst = new P6opaque(Kernel.ListMO);
                    lst.slots[0] = outq;
                    lst.slots[1] = new VarDeque();
                    th.caller.resultSlot = Kernel.NewRWListVar(lst);
                    return th.caller;
                }
                th.lex3 = pen;
                th.ip = 1;
                goto case 1;
            case 1:
                th.ip = 2;
                return flt.Fetch().InvokeMethod(th, "ACCEPTS",
                        new Variable[] { flt, (Variable)th.lex3 }, null);
            case 2:
                Variable r = (Variable) th.resultSlot;
                if (!r.Fetch().mo.mro_raw_Bool.Get(r)) {
                    th.ip = 0;
                    goto case 0;
                }
                if (tailmode != 0) {
                    th.ip = 0;
                    return Kernel.Take(th, (Variable)th.lex3);
                } else {
                    outq.Push((Variable) th.lex3);
                    th.ip = 0;
                    goto case 0;
                }
            case 3:
                th.lex0 = src = new VarDeque();
                th.ip = 0;
                goto case 0;
            default:
                return Kernel.Die(th, "Invalid IP");
        }
    }
    public static Frame MEGrep(Frame th, Variable[] lst) {
        VarDeque iter = new VarDeque(lst);
        Variable fcn = iter.Shift();
        iter = Kernel.IterFlatten(iter);

        Frame fr = th.MakeChild(null, CommonGrep_I);
        fr.lexi0 = 0;
        fr.lex0 = iter;
        fr.lex1 = new VarDeque();
        fr.lex2 = fcn;
        return fr;
    }

    public static Frame CallNext(Frame th, P6any cap) {
        Frame to = th;
        while (to != null && to.curDisp == null)
            to = to.caller;
        if (to == null || to.curDisp.next == null)
            return Kernel.Die(th, "No next function to call!");

        DispatchEnt de = to.curDisp.next;
        P6opaque o = cap as P6opaque;
        Frame nf = th.MakeChild(de.outer, de.info);

        Variable[] p = to.pos;
        VarHash n    = to.named;
        if (o != null) {
            p = (Variable[]) o.slots[0];
            n = o.slots[1] as VarHash;
        }
        nf = nf.info.Binder(nf, p, n, false);
        nf.curDisp = de;
        return nf;
    }

    public static bool can(P6any obj, string mname) {
        return obj.mo.mro_methods.ContainsKey(mname);
    }
}
