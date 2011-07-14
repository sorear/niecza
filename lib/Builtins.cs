using Niecza;
using System;
using System.Collections.Generic;
using System.Diagnostics;
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

    static void CheckSpecialArg(int ix, ref int pivot, ref uint rank,
            P6any val) {
        if (val.mo.is_any) {
            // fine as is
        } else if (val.mo.HasMRO(Kernel.JunctionMO)) {
            int jtype = Kernel.UnboxAny<int>((P6any)(val as P6opaque).slots[0]) / 2;
            if ((uint)jtype < rank) {
                rank = (uint)jtype;
                pivot = ix;
            }
        } else {
            throw new NieczaException("Nominal type check failed for #" + ix +
                    " needed Any got " + val.mo.name);
        }
    }

    // NOTE: Destructive on avs!  Clone first if you need to.
    public static Variable AutoThread(P6any j, Func<Variable,Variable> dgt) {
        P6opaque j_ = (P6opaque)j;
        P6any listObj = (P6any) j_.slots[1];
        Variable[] list = Kernel.UnboxAny<Variable[]>(listObj);
        Variable[] nlist = new Variable[list.Length];
        for (int i = 0; i < list.Length; i++) {
            nlist[i] = dgt(list[i]);
        }
        P6any newList = Kernel.BoxRaw(nlist, Kernel.ParcelMO);
        P6opaque newJunc = new P6opaque(Kernel.JunctionMO);
        newJunc.slots[0] = j_.slots[0];
        newJunc.slots[1] = newList;
        return Kernel.NewROScalar(newJunc);
    }

    // functions containing sub-functions get mangled by the compiler, so
    // keep the critical path away.
    public static Variable HandleSpecial1(Variable av0, P6any ao0,
            Func<Variable,Variable> dgt) {
        uint jrank = uint.MaxValue;
        int jpivot = -1;

        CheckSpecialArg(0, ref jpivot, ref jrank, ao0);

        if (jpivot < 0) return dgt(av0);

        return AutoThread(ao0, dgt);
    }
    public static Variable HandleSpecial2(Variable av0, Variable av1,
            P6any ao0, P6any ao1, Func<Variable,Variable,Variable> dgt) {
        uint jrank = uint.MaxValue;
        int jpivot = -1;

        CheckSpecialArg(0, ref jpivot, ref jrank, ao0);
        CheckSpecialArg(1, ref jpivot, ref jrank, ao1);

        if (jpivot < 0) return dgt(av0, av1);

        Variable[] avs = new Variable[] { av0, av1 };
        return AutoThread(avs[jpivot].Fetch(), delegate(Variable n) {
            avs[jpivot] = n; return dgt(avs[0], avs[1]); });
    }
    public static Variable HandleSpecial3(Variable av0, Variable av1,
            Variable av2, P6any ao0, P6any ao1, P6any ao2,
            Func<Variable,Variable,Variable,Variable> dgt) {
        uint jrank = uint.MaxValue;
        int jpivot = -1;

        CheckSpecialArg(0, ref jpivot, ref jrank, ao0);
        CheckSpecialArg(1, ref jpivot, ref jrank, ao1);
        CheckSpecialArg(2, ref jpivot, ref jrank, ao2);

        if (jpivot < 0) return dgt(av0, av1, av2);

        Variable[] avs = new Variable[] { av0, av1, av2 };
        return AutoThread(avs[jpivot].Fetch(), delegate(Variable n) {
            avs[jpivot] = n; return dgt(avs[0], avs[1], avs[2]); });
    }

    public static void AssignV(Variable lhs, P6any rhs) {
        if (!lhs.islist) {
            lhs.Store(rhs);
        } else {
            lhs.Fetch().mo.mro_LISTSTORE.Get(lhs, Kernel.NewROScalar(rhs));
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
            string sub = Builtins.LaxSubstring2(str, from, length);
            return sub == null ? Kernel.StrMO.typeObject :
                Kernel.BoxRaw(sub,Kernel.StrMO);
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
        if (from < 0 || from > str.Length)
            return null;
        return str.Substring(from);
    }

    public static string LaxSubstring2(string str, int from, int l) {
        if (from < 0 || from > str.Length) return null;
        if (l >= str.Length - from) l = str.Length - from;
        if (l < 0) return null;
        return str.Substring(from, l);
    }

    public const int NR_FIXINT  = 0;
    public const int NR_BIGINT  = 1;
    public const int NR_FIXRAT  = 2;
    public const int NR_FATRAT  = 3;
    public const int NR_FLOAT   = 4;
    public const int NR_COMPLEX = 5;

    public static P6any GetNumber(Variable v, P6any o, out int rank) {
        if (o.mo.num_rank >= 0) {
            rank = o.mo.num_rank;
        } else {
            o = o.mo.mro_Numeric.Get(v).Fetch();
            rank = o.mo.num_rank;
            if (rank < 0)
                throw new NieczaException("Not a valid primitive number " +
                        o.mo.name);
        }
        if (rank == NR_FIXINT && o is BoxObject<BigInteger>)
            rank = NR_BIGINT;
        return o;
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
        int rk;
        P6any n = GetNumber(v, v.Fetch(), out rk);

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

    public static bool GetAsInteger(Variable v, out int small,
            out BigInteger big) {
        int rk;
        P6any n = GetNumber(v, v.Fetch(), out rk);
        small = 0;

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
            big = (bits & ((1UL << 52) - 1)) + (1UL << 52);
            int power = ((int)((bits >> 52) & 0x7FF)) - 0x433;
            if (power > 0) big <<= power;
            else big >>= -power;
            if ((bits & (1UL << 63)) != 0) big = -big;
        }
        else if (rk == NR_FATRAT) {
            FatRat r = Kernel.UnboxAny<FatRat>(n);
            big = r.num / r.den;
        }
        else if (rk == NR_FIXRAT) {
            Rat r = Kernel.UnboxAny<Rat>(n);
            big = r.num / r.den;
        }
        else if (rk == NR_BIGINT) {
            big = Kernel.UnboxAny<BigInteger>(n);
        }
        else {
            big = BigInteger.Zero; small = Kernel.UnboxAny<int>(n); return false;
        }
        return true;
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

    public static Variable MakeParcel(params Variable[] bits) {
        return Kernel.NewRWListVar(Kernel.BoxRaw(bits, Kernel.ParcelMO));
    }

    static Func<Variable,Variable,Variable> numeq_d = numeq;
    public static Variable numeq(Variable v1, Variable v2) {
        return numcompare(v1, v2, O_IS_EQUAL, numeq_d);
    }

    public static Variable numne(Variable v1, Variable v2) {
        // NOTE that junctionalization uses == !  See check in numcompare
        return numcompare(v1, v2, O_IS_LESS | O_IS_GREATER | O_IS_UNORD,
                numeq_d);
    }

    static Func<Variable,Variable,Variable> numlt_d = numlt;
    public static Variable numlt(Variable v1, Variable v2) {
        return numcompare(v1, v2, O_IS_LESS, numlt_d);
    }

    static Func<Variable,Variable,Variable> numle_d = numle;
    public static Variable numle(Variable v1, Variable v2) {
        return numcompare(v1, v2, O_IS_EQUAL | O_IS_LESS, numle_d);
    }

    static Func<Variable,Variable,Variable> numgt_d = numgt;
    public static Variable numgt(Variable v1, Variable v2) {
        return numcompare(v1, v2, O_IS_GREATER, numgt_d);
    }

    static Func<Variable,Variable,Variable> numge_d = numge;
    public static Variable numge(Variable v1, Variable v2) {
        return numcompare(v1, v2, O_IS_GREATER | O_IS_EQUAL, numge_d);
    }

    public static Variable strcompare(Variable v1, Variable v2,
            int mask, Func<Variable,Variable,Variable> d) {
        P6any o1 = v1.Fetch(); P6any o2 = v2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any)) {
            Variable jr = HandleSpecial2(v1, v2, o1, o2, d);
            // treat $x != $y as !($x == $y)
            if (mask == (O_IS_GREATER | O_IS_LESS | O_IS_UNORD))
                return jr.Fetch().mo.mro_raw_Bool.Get(jr) ? Kernel.FalseV :
                    Kernel.TrueV;
            return jr;
        }
        int diff = string.CompareOrdinal(o1.mo.mro_raw_Str.Get(v1),
                o2.mo.mro_raw_Str.Get(v2));
        int mcom = (diff < 0) ? O_IS_LESS : (diff > 0) ? O_IS_GREATER :
            O_IS_EQUAL;
        return ((mask & mcom) != 0) ? Kernel.TrueV : Kernel.FalseV;
    }

    static Func<Variable,Variable,Variable> streq_d = streq;
    public static Variable streq(Variable v1, Variable v2) {
        return strcompare(v1, v2, O_IS_EQUAL, streq_d);
    }

    public static Variable strne(Variable v1, Variable v2) {
        return strcompare(v1, v2, O_IS_LESS | O_IS_GREATER | O_IS_UNORD, streq_d);
    }

    static Func<Variable,Variable,Variable> strlt_d = strlt;
    public static Variable strlt(Variable v1, Variable v2) {
        return strcompare(v1, v2, O_IS_LESS, strlt_d);
    }

    static Func<Variable,Variable,Variable> strle_d = strle;
    public static Variable strle(Variable v1, Variable v2) {
        return strcompare(v1, v2, O_IS_EQUAL | O_IS_LESS, strle_d);
    }

    static Func<Variable,Variable,Variable> strgt_d = strgt;
    public static Variable strgt(Variable v1, Variable v2) {
        return strcompare(v1, v2, O_IS_GREATER, strgt_d);
    }

    static Func<Variable,Variable,Variable> strge_d = strge;
    public static Variable strge(Variable v1, Variable v2) {
        return strcompare(v1, v2, O_IS_GREATER | O_IS_EQUAL, strge_d);
    }

    static Func<Variable,Variable,Variable,Variable> substr3_d = substr3;
    public static Variable substr3(Variable v1, Variable v2, Variable v3) {
        P6any o1 = v1.Fetch(), o2 = v2.Fetch(), o3 = v3.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any && o3.mo.is_any))
            return HandleSpecial3(v1,v2,v3, o1,o2,o3, substr3_d);

        int r2, r3;

        if (o2.Does(Kernel.CodeMO)) {
            string s1 = o1.mo.mro_raw_Str.Get(v1);
            Variable no2 = Kernel.RunInferior(o2.Invoke(
                Kernel.GetInferiorRoot(), new Variable[] {
                    MakeInt(s1.Length) }, null));
            r2 = (int)no2.Fetch().mo.mro_raw_Numeric.Get(no2);
        } else {
            r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        }
        if (o3.Does(Kernel.CodeMO)) {
            string s1 = o1.mo.mro_raw_Str.Get(v1);
            Variable no3 = Kernel.RunInferior(o3.Invoke(
                Kernel.GetInferiorRoot(), new Variable[] {
                    MakeInt(s1.Length) }, null));
            r3 = (int)no3.Fetch().mo.mro_raw_Numeric.Get(no3) - r2;
        } else {
            r3 = (int)o3.mo.mro_raw_Numeric.Get(v3);
        }
        return new SubstrLValue(v1, r2, r3);
    }

    static Func<Variable,Variable,Variable> plus_d = plus;
    public static Variable plus(Variable a1, Variable a2) {
        int r1, r2;
        P6any o1 = a1.Fetch(), o2 = a2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(a1, a2, o1, o2, plus_d);
        P6any n1 = GetNumber(a1, o1, out r1);
        P6any n2 = GetNumber(a2, o2, out r2);

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

    static Func<Variable,Variable,Variable> minus_d = minus;
    public static Variable minus(Variable a1, Variable a2) {
        int r1, r2;
        P6any o1 = a1.Fetch(), o2 = a2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(a1,a2, o1,o2, minus_d);
        P6any n1 = GetNumber(a1, o1, out r1);
        P6any n2 = GetNumber(a2, o2, out r2);

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

    // Untested and probably unusable in the near term
    public static BigInteger big_pow(BigInteger v1, BigInteger v2) {
        int CHUNK = 2000000000;
        int margin = (int) (v2 % (BigInteger) CHUNK);
        BigInteger number_chunks = (v2 - margin) / CHUNK;
        BigInteger margin_pow = BigInteger.Pow(v1, margin);
        BigInteger result = margin_pow;
        if (number_chunks > 0) {
            BigInteger chunk_pow = margin_pow * BigInteger.Pow(v1, CHUNK - margin);
            for (BigInteger i = 0; i < number_chunks; i++) {
                result *= chunk_pow;
            }
        }
        return result;
    }

    static Func<Variable,Variable,Variable> pow_d = pow;
    public static Variable pow(Variable a1, Variable a2) {
        int r1, r2;
        P6any o1 = a1.Fetch(), o2 = a2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(a1, a2, o1, o2, pow_d);
        P6any n1 = GetNumber(a1, o1, out r1);
        P6any n2 = GetNumber(a2, o2, out r2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            Complex v2 = PromoteToComplex(r2, n2);
            double r = Math.Sqrt(v1.re * v1.re + v1.im * v1.im);
            double theta = Math.Atan2(v1.im, v1.re);
            // Log z = ln r + iθ
            // ($a.log * $b).exp;
            double lp_re = Math.Log(r)*v2.re - theta*v2.im;
            double lp_im = theta*v2.re + Math.Log(r)*v2.im;
            return MakeComplex(Math.Exp(lp_re) * Math.Cos(lp_im),
                               Math.Exp(lp_re) * Math.Sin(lp_im));
        }

        if (r1 == NR_FLOAT || (r2 != NR_BIGINT && r2 != NR_FIXINT)) {
            return MakeFloat(Math.Pow(PromoteToFloat(r1, n1), PromoteToFloat(r2, n2)));
        }

        if (r2 == NR_FIXINT) {
            int v2 = PromoteToFixInt(r2, n2);
            if (v2 >= 0) {
                if (r1 == NR_FIXINT || r1 == NR_BIGINT) {
                    return MakeInt(BigInteger.Pow(PromoteToBigInt(r1, n1), v2));
                }
                if (r1 == NR_FATRAT) {
                    FatRat v1 = PromoteToFatRat(r1, n1);
                    return MakeFatRat(BigInteger.Pow(v1.num, v2), BigInteger.Pow(v1.den, v2));
                }
                if (r1 == NR_FIXRAT) {
                    Rat v1 = PromoteToFixRat(r1, n1);
                    return MakeFixRat(BigInteger.Pow((BigInteger)v1.num, v2), BigInteger.Pow(v1.den, v2));
                }
            } else {
                if (r1 == NR_FIXINT || r1 == NR_BIGINT) {
                    return MakeFixRat(1, BigInteger.Pow(PromoteToBigInt(r1, n1), -v2));
                }
                if (r1 == NR_FATRAT) {
                    FatRat v1 = PromoteToFatRat(r1, n1);
                    return MakeFatRat(BigInteger.Pow(v1.den, -v2), BigInteger.Pow(v1.num, -v2));
                }
                if (r1 == NR_FIXRAT) {
                    Rat v1 = PromoteToFixRat(r1, n1);
                    return MakeFixRat(BigInteger.Pow((BigInteger)v1.den, -v2), BigInteger.Pow(v1.num, -v2));
                }
            }
        }

        if (r2 == NR_BIGINT) {
            BigInteger v2 = PromoteToBigInt(r2, n2);
            if (v2 >= 0) {
                if (r1 == NR_FIXINT || r1 == NR_BIGINT) {
                    return MakeInt(big_pow(PromoteToBigInt(r1, n1), v2));
                }
                if (r1 == NR_FATRAT) {
                    FatRat v1 = PromoteToFatRat(r1, n1);
                    return MakeFatRat(big_pow(v1.num, v2), big_pow(v1.den, v2));
                }
                if (r1 == NR_FIXRAT) {
                    Rat v1 = PromoteToFixRat(r1, n1);
                    return MakeFixRat(big_pow((BigInteger)v1.num, v2), big_pow(v1.den, v2));
                }
            } else {
                if (r1 == NR_FIXINT || r1 == NR_BIGINT) {
                    return MakeFixRat(1, big_pow(PromoteToBigInt(r1, n1), -v2));
                }
                if (r1 == NR_FATRAT) {
                    FatRat v1 = PromoteToFatRat(r1, n1);
                    return MakeFatRat(big_pow(v1.den, -v2), big_pow(v1.num, -v2));
                }
                if (r1 == NR_FIXRAT) {
                    Rat v1 = PromoteToFixRat(r1, n1);
                    return MakeFixRat(big_pow((BigInteger)v1.den, -v2), big_pow(v1.num, -v2));
                }
            }
        }

        return MakeFloat(Math.Pow(PromoteToFloat(r1, n1), PromoteToFloat(r2, n2)));
    }

    static Func<Variable,Variable> negate_d = negate;
    public static Variable negate(Variable a1) {
        P6any o1 = a1.Fetch();
        int r1;
        if (!o1.mo.is_any)
            return HandleSpecial1(a1,o1, negate_d);
        P6any n1 = GetNumber(a1, o1, out r1);

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

    static Func<Variable,Variable> abs_d = abs;
    public static Variable abs(Variable a1) {
        P6any o1 = a1.Fetch();
        int r1;
        if (!o1.mo.is_any)
            return HandleSpecial1(a1,o1, abs_d);
        P6any n1 = GetNumber(a1, o1, out r1);

        if (r1 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            return MakeFloat(Math.Sqrt(v1.re * v1.re + v1.im * v1.im));
        }
        if (r1 == NR_FLOAT) {
            double v1 = PromoteToFloat(r1, n1);
            return MakeFloat(v1 < 0 ? -v1 : v1);
        }
        if (r1 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            return v1.num < 0 ? MakeFatRat(-v1.num, v1.den) : MakeFatRat(v1.num, v1.den);
        }
        if (r1 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            return v1.num < 0 ? MakeFixRat(-v1.num, v1.den) : MakeFixRat(v1.num, v1.den);
        }
        if (r1 == NR_BIGINT) {
            BigInteger v1 = PromoteToBigInt(r1, n1);
            return MakeInt(v1 < 0 ? -v1 : v1);
        }
        {
            long v1 = PromoteToFixInt(r1, n1);
            return MakeInt(v1 < 0 ? -v1 : v1);
        }
    }

    static Func<Variable,Variable> ln_d = ln;
    public static Variable ln(Variable a1) {
        P6any o1 = a1.Fetch();
        int r1;
        if (!o1.mo.is_any)
            return HandleSpecial1(a1,o1, ln_d);
        P6any n1 = GetNumber(a1, o1, out r1);

        if (r1 == NR_COMPLEX) {
            // Log z = ln r + iθ
            Complex v1 = PromoteToComplex(r1, n1);
            return MakeComplex(Math.Log(Math.Sqrt(v1.re * v1.re + v1.im * v1.im)),
                               Math.Atan2(v1.im, v1.re));
        }
        {
            double v1 = PromoteToFloat(r1, n1);
            return MakeFloat(Math.Log(v1));
        }
    }

    static Func<Variable,Variable> exp_d = exp;
    public static Variable exp(Variable a1) {
        P6any o1 = a1.Fetch();
        int r1;
        if (!o1.mo.is_any)
            return HandleSpecial1(a1,o1, exp_d);
        P6any n1 = GetNumber(a1, o1, out r1);

        if (r1 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            return MakeComplex(Math.Exp(v1.re) * Math.Cos(v1.im),
                               Math.Exp(v1.re) * Math.Sin(v1.im));
        }
        {
            double v1 = PromoteToFloat(r1, n1);
            return MakeFloat(Math.Exp(v1));
        }
    }

    static Func<Variable,Variable> sin_d = sin;
    public static Variable sin(Variable a1) {
        P6any o1 = a1.Fetch();
        int r1;
        if (!o1.mo.is_any)
            return HandleSpecial1(a1,o1, sin_d);
        P6any n1 = GetNumber(a1, o1, out r1);

        if (r1 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            return MakeComplex(Math.Sin(v1.re) * Math.Cosh(v1.im),
                               Math.Cos(v1.re) * Math.Sinh(v1.im));
        }
        {
            double v1 = PromoteToFloat(r1, n1);
            return MakeFloat(Math.Sin(v1));
        }
    }

    static Func<Variable,Variable> floor_d = floor;
    public static Variable floor(Variable a1) {
        P6any o1 = a1.Fetch();
        int r1;
        if (!o1.mo.is_any)
            return HandleSpecial1(a1,o1, floor_d);
        P6any n1 = GetNumber(a1, o1, out r1);

        if (r1 == NR_COMPLEX) {
            throw new NieczaException("floor is only defined for Reals, you have a Complex()");
        }
        if (r1 == NR_FLOAT) {
            double v1 = PromoteToFloat(r1, n1);
            ulong bits = (ulong)BitConverter.DoubleToInt64Bits(v1);
            BigInteger big = (bits & ((1UL << 52) - 1)) + (1UL << 52);
            int power = ((int)((bits >> 52) & 0x7FF)) - 0x433;
            // note: >>= has flooring semantics for signed values
            if ((bits & (1UL << 63)) != 0) big = -big;
            if (power > 0) big <<= power;
            else big >>= -power;
            return MakeInt(big);
        }
        if (r1 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            BigInteger rem;
            BigInteger red = BigInteger.DivRem(v1.num, v1.den, out rem);
            if (rem.Sign != 0 && v1.num.Sign < 0)
                return MakeInt(red - 1);
            else
                return MakeInt(red);
        }
        if (r1 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            BigInteger rem;
            BigInteger red = BigInteger.DivRem(v1.num, v1.den, out rem);
            if (rem.Sign != 0 && v1.num.Sign < 0)
                return MakeInt(red - 1);
            else
                return MakeInt(red);
        }
        return Kernel.NewROScalar(n1);
    }

    // we don't need to do nominal checking stuff here because this
    // is in a method, never inlined, and as such the binder had to
    // already have been called.
    public static Variable complex_re(Variable a1) {
        return MakeFloat(Kernel.UnboxAny<Complex>(a1.Fetch()).re);
    }
    public static Variable complex_im(Variable a1) {
        return MakeFloat(Kernel.UnboxAny<Complex>(a1.Fetch()).im);
    }
    public static Variable rat_nu(Variable a1) {
        return MakeInt(Kernel.UnboxAny<Rat>(a1.Fetch()).num);
    }
    public static Variable rat_de(Variable a1) {
        return MakeInt(Kernel.UnboxAny<Rat>(a1.Fetch()).den);
    }
    public static Variable fatrat_nu(Variable a1) {
        return MakeInt(Kernel.UnboxAny<FatRat>(a1.Fetch()).num);
    }
    public static Variable fatrat_de(Variable a1) {
        return MakeInt(Kernel.UnboxAny<FatRat>(a1.Fetch()).den);
    }

    const int O_IS_GREATER = 1;
    const int O_IS_LESS    = 2;
    const int O_IS_EQUAL   = 4;
    const int O_IS_UNORD   = 8;
    public static Variable numcompare(Variable a1, Variable a2, int mask,
            Func<Variable,Variable,Variable> dl) {
        int r1, r2, res=0;
        P6any o1 = a1.Fetch(), o2 = a2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any)) {
            Variable jr = HandleSpecial2(a1, a2, o1, o2, dl);
            // treat $x != $y as !($x == $y)
            if (mask == (O_IS_GREATER | O_IS_LESS | O_IS_UNORD))
                return jr.Fetch().mo.mro_raw_Bool.Get(jr) ? Kernel.FalseV :
                    Kernel.TrueV;
            return jr;
        }
        P6any n1 = GetNumber(a1, o1, out r1);
        P6any n2 = GetNumber(a2, o2, out r2);

        if (r1 == NR_COMPLEX || r2 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            Complex v2 = PromoteToComplex(r2, n2);
            if (double.IsNaN(v1.re) || double.IsNaN(v1.im) ||
                    double.IsNaN(v2.re) || double.IsNaN(v2.im))
                res = O_IS_UNORD;
            else if (v1.re != v2.re)
                res = v1.re > v2.re ? O_IS_GREATER : O_IS_LESS;
            else
                res = v1.im > v2.im ? O_IS_GREATER : v1.im < v2.im ? O_IS_LESS : O_IS_EQUAL;
        }
        else if (r1 == NR_FLOAT || r2 == NR_FLOAT) {
            double d1 = PromoteToFloat(r1, n1);
            double d2 = PromoteToFloat(r2, n2);
            if (double.IsNaN(d1) || double.IsNaN(d2)) res = O_IS_UNORD;
            else res =d1 > d2 ? O_IS_GREATER : d1 < d2 ? O_IS_LESS : O_IS_EQUAL;
        }
        else if (r1 == NR_FATRAT || r2 == NR_FATRAT) {
            FatRat v1 = PromoteToFatRat(r1, n1);
            FatRat v2 = PromoteToFatRat(r2, n2);

            r1 = BigInteger.Compare(v1.num*v2.den, v2.num*v1.den);
        }
        else if (r1 == NR_FIXRAT || r2 == NR_FIXRAT) {
            Rat v1 = PromoteToFixRat(r1, n1);
            Rat v2 = PromoteToFixRat(r2, n2);

            r1 = BigInteger.Compare(v1.num*v2.den, v2.num*v1.den);
        }
        else if (r1 == NR_BIGINT || r2 == NR_BIGINT) {
            r1 = BigInteger.Compare(PromoteToBigInt(r1, n1),
                    PromoteToBigInt(r2, n2));
        }
        else
            r1 = PromoteToFixInt(r1, n1).CompareTo(PromoteToFixInt(r2, n2));

        if (res == 0)
            res = (r1 > 0) ? O_IS_GREATER : (r1 < 0) ? O_IS_LESS : O_IS_EQUAL;

        return ((mask & res) != 0) ? Kernel.TrueV : Kernel.FalseV;
    }

    static Func<Variable,Variable,Variable> mul_d = mul;
    public static Variable mul(Variable a1, Variable a2) {
        int r1, r2;
        P6any o1 = a1.Fetch(), o2 = a2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(a1,a2, o1,o2, mul_d);
        P6any n1 = GetNumber(a1, o1, out r1);
        P6any n2 = GetNumber(a2, o2, out r2);

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

    static Func<Variable,Variable,Variable> divide_d = divide;
    public static Variable divide(Variable a1, Variable a2) {
        int r1, r2;
        P6any o1 = a1.Fetch(), o2 = a2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(a1,a2, o1,o2, divide_d);
        P6any n1 = GetNumber(a1, o1, out r1);
        P6any n2 = GetNumber(a2, o2, out r2);

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

    static Func<Variable,Variable,Variable> mod_d = mod;
    public static Variable mod(Variable a1, Variable a2) {
        int r1, r2;
        P6any o1 = a1.Fetch(), o2 = a2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(a1,a2, o1,o2, mod_d);
        P6any n1 = GetNumber(a1, o1, out r1);
        P6any n2 = GetNumber(a2, o2, out r2);

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

    // this is only called from .Int
    public static Variable coerce_to_int(Variable a1) {
        int small; BigInteger big;
        return GetAsInteger(a1, out small, out big) ?
            MakeInt(big) : MakeInt(small);
    }

    // only called from infix for now
    // when it's not, it'll need to be split anyway, I think
    // I fumbled spec reading - only 4 and 5 are actually needed
    public static Variable divop(int opc, Variable a1, Variable a2) {
        int small1, small2; BigInteger big1, big2;
        bool b1 = GetAsInteger(a1, out small1, out big1);
        bool b2 = GetAsInteger(a2, out small2, out big2);

        if (b1 || b2 || small1 == int.MinValue || small2 == int.MinValue) {
            if (!b1) big1 = small1;
            if (!b2) big2 = small2;
            BigInteger rem;
            BigInteger red = BigInteger.DivRem(big1, big2, out rem);
            if (opc >= 4 && red.Sign < 0 && rem.Sign != 0) {
                red--;
                rem += big2;
            }
            switch (opc & 3) {
                case 0: return MakeInt(red);
                case 1: return MakeInt(rem);
                default: return MakeParcel(MakeInt(red), MakeInt(rem));
            }
        } else {
            int rem = small1 % small2;
            int red = small1 / small2;
            if (opc >= 4 && red < 0 && rem != 0) {
                red--;
                rem += small2;
            }
            switch (opc & 3) {
                case 0: return MakeInt(red);
                case 1: return MakeInt(rem);
                default: return MakeParcel(MakeInt(red), MakeInt(rem));
            }
        }
    }

    // called from .Num
    public static Variable coerce_to_num(Variable a1) {
        int r1;
        P6any n1 = GetNumber(a1, a1.Fetch(), out r1);

        if (r1 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            if (v1.im != 0)
                throw new NieczaException("Complex cannot be used here");
            return MakeFloat(v1.re);
        } else {
            return MakeFloat(PromoteToFloat(r1, n1));
        }
    }

    static Func<Variable,Variable> sqrt_d = sqrt;
    public static Variable sqrt(Variable a1) {
        P6any o1 = a1.Fetch();
        int r1;
        if (!o1.mo.is_any)
            return HandleSpecial1(a1,o1, sqrt_d);
        P6any n1 = GetNumber(a1, o1, out r1);

        if (r1 == NR_COMPLEX) {
            Complex v1 = PromoteToComplex(r1, n1);
            double angle = Math.Atan2(v1.im, v1.re) / 2;
            if (angle < 0) angle += Math.PI;
            double mag = Math.Sqrt(Math.Sqrt(v1.im*v1.im + v1.re*v1.re));
            return MakeComplex(mag * Math.Cos(angle), mag * Math.Sin(angle));
        } else {
            double val = PromoteToFloat(r1, n1);
            return (val > 0) ? MakeFloat(Math.Sqrt(val)) : MakeComplex(0, Math.Sqrt(-val));
        }
    }

    static Func<Variable,Variable,Variable> numand_d = numand;
    public static Variable numand(Variable v1, Variable v2) {
        P6any o1 = v1.Fetch(); P6any o2 = v2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(v1,v2, o1,o2, numand_d);

        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 & r2);
    }

    static Func<Variable,Variable,Variable> numor_d = numor;
    public static Variable numor(Variable v1, Variable v2) {
        P6any o1 = v1.Fetch(); P6any o2 = v2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(v1,v2, o1,o2, numor_d);

        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 | r2);
    }

    static Func<Variable,Variable,Variable> numxor_d = numxor;
    public static Variable numxor(Variable v1, Variable v2) {
        P6any o1 = v1.Fetch(); P6any o2 = v2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(v1,v2, o1,o2, numxor_d);

        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 ^ r2);
    }

    static Func<Variable,Variable,Variable> numlshift_d = numlshift;
    public static Variable numlshift(Variable v1, Variable v2) {
        P6any o1 = v1.Fetch(); P6any o2 = v2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(v1,v2, o1,o2, numlshift_d);

        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 << r2);
    }

    static Func<Variable,Variable,Variable> numrshift_d = numrshift;
    public static Variable numrshift(Variable v1, Variable v2) {
        P6any o1 = v1.Fetch(); P6any o2 = v2.Fetch();
        if (!(o1.mo.is_any && o2.mo.is_any))
            return HandleSpecial2(v1,v2, o1,o2, numrshift_d);

        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        int r2 = (int)o2.mo.mro_raw_Numeric.Get(v2);
        return MakeInt(r1 >> r2);
    }

    static Func<Variable,Variable> numcompl_d = numcompl;
    public static Variable numcompl(Variable v1) {
        P6any o1 = v1.Fetch();
        if (!o1.mo.is_any)
            return HandleSpecial1(v1,o1, numcompl_d);

        int r1 = (int)o1.mo.mro_raw_Numeric.Get(v1);
        return MakeInt(~r1);
    }

    // only called from .Rat
    public static Variable rat_approx(Variable v1, Variable v2) {
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

    public static Variable postinc(Variable v) {
        P6any o1 = v.Fetch();
        AssignV(v, o1.mo.mro_succ.Get(v));
        return Kernel.NewROScalar(o1);
    }

    public static Variable preinc(Variable v) {
        AssignV(v, v.Fetch().mo.mro_succ.Get(v));
        return v;
    }

    public static Variable postdec(Variable v) {
        P6any o1 = v.Fetch();
        AssignV(v, o1.mo.mro_pred.Get(v));
        return Kernel.NewROScalar(o1);
    }

    public static Variable predec(Variable v) {
        AssignV(v, v.Fetch().mo.mro_pred.Get(v));
        return v;
    }

    public static Variable not(Variable v) {
        P6any o1 = v.Fetch();
        bool r = o1.mo.mro_raw_Bool.Get(v);
        return r ? Kernel.FalseV : Kernel.TrueV;
    }

    static Func<Variable,Variable> chars_d = chars;
    public static Variable chars(Variable v) {
        P6any o1 = v.Fetch();
        if (!o1.mo.is_any)
            return HandleSpecial1(v,o1, chars_d);

        string r = o1.mo.mro_raw_Str.Get(v);
        return MakeInt(r.Length);
    }

    static Func<Variable,Variable> ord_d = ord;
    public static Variable ord(Variable v) {
        P6any o1 = v.Fetch();
        if (!o1.mo.is_any)
            return HandleSpecial1(v,o1, ord_d);

        string r = o1.mo.mro_raw_Str.Get(v);
        // XXX Failure
        if (r.Length == 0) return Kernel.AnyMO.typeVar;
        return MakeInt((int)r[0]);
    }

    static Func<Variable,Variable> chr_d = chr;
    public static Variable chr(Variable v) {
        P6any o1 = v.Fetch();
        if (!o1.mo.is_any)
            return HandleSpecial1(v,o1, chr_d);

        double r = o1.mo.mro_raw_Numeric.Get(v);
        return Kernel.BoxAnyMO(new string((char)r, 1), Kernel.StrMO);
    }

    public static Variable UniCat(Variable v) {
        P6any o1 = NominalCheck("$x", Kernel.AnyMO, v);
        char c = (char) o1.mo.mro_raw_Numeric.Get(v);
        int ix = (int) char.GetUnicodeCategory(c);
        return MakeInt(ix);
    }

    public static Variable MakeJunction(int type, Variable[] elems) {
        if (type >= 8) {
            type -= 8;
            foreach (Variable e in elems)
                if (e.islist) goto need_flatten;
            goto flat_enough;
need_flatten:;
            VarDeque iter = new VarDeque(elems);
            VarDeque into = new VarDeque();
            while (Kernel.IterHasFlat(iter, true))
                into.Push(iter.Shift());
            elems = into.CopyAsArray();
flat_enough:;
        }
        P6opaque nj = new P6opaque(Kernel.JunctionMO);
        nj.slots[0] = Kernel.BoxRaw(type, Kernel.IntMO);
        nj.slots[1] = Kernel.BoxRaw(elems, Kernel.ParcelMO);
        return Kernel.NewROScalar(nj);
    }

    public static Variable Make(Frame fr, Variable v) {
        if (fr.info.name == "CORE make")
            fr = fr.caller;
        Cursor c = (Cursor) fr.LexicalFind("$/").Fetch();
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

    public static Variable path_modified(string path) {
        long t = File.GetLastWriteTimeUtc(path).Ticks;
        return MakeFloat(((double)(t - 621355968000000000L)) / 10000000.0);
    }

    public static Variable now() {
        long t = DateTime.UtcNow.Ticks;
        return MakeFloat(((double)(t - 621355968000000000L)) / 10000000.0);
    }

    public static Variable times() {
        Process p = Process.GetCurrentProcess();
        Variable[] ret = new Variable[4];
        ret[0] = ret[2] = MakeFloat(((double)p.UserProcessorTime.Ticks) / 10000000.0);
        ret[1] = ret[3] = MakeFloat(((double)p.PrivilegedProcessorTime.Ticks) / 10000000.0);
        return MakeParcel(ret);
    }

    private static Random rng = new Random();

    public static Variable rand() {
        double i;
        lock (rng) { i = rng.NextDouble(); }
        return MakeFloat(i);
    }

    public static bool path_any_exists(string path) {
        return File.Exists(path) || Directory.Exists(path);
    }

    static string[] split_on_colons(string path) {
        List<string> l = new List<string>();
        int ix = -2;
        do {
            int nix = path.IndexOf("::", ix+2);
            l.Add(nix < 0 ? path.Substring(ix+2) :
                    path.Substring(ix+2, nix - (ix+2)));
            ix = nix;
        } while (ix >= 0);
        return l.ToArray();
    }

    public static Variable dynamic_package_var(Frame th, string path) {
        string[] toks = split_on_colons(path);
        return null;
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
        VarDeque iter = start_iter(args);
        while (Kernel.IterHasFlat(iter, true)) {
            Variable v = iter.Shift();
            la.Add(v.Fetch().mo.mro_raw_Str.Get(v));
        }
        return la.ToArray();
    }

    public static Frame you_are_here(Frame th, string sname) {
        string key = "*resume_" + sname;
        uint khash = SubInfo.FilterForName(key);
        object r = null;
        for (Frame c = th; c != null; c = c.caller)
            if (c.TryGetDynamic(key, khash, out r))
                break;
        P6any to_call = Kernel.MakeSub((SubInfo)r, th);
        return to_call.Invoke(th, Variable.None, null);
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
    public static DynBlockDelegate eval_result;
    public static Variable DownCall(Variable cb, Variable list) {
        GetSubDomain();
        upcall_cb = cb;
        CrossDomainReceiver r = (CrossDomainReceiver)
            subDomain.CreateInstanceFromAndUnwrap(backend,
                    "Niecza.CLRBackend.DownCallAcceptor");
        return BoxLoS(r.Call(AppDomain.CurrentDomain, UnboxLoS(list)));
    }

    public static Frame simple_eval(Frame th, Variable str) {
        if (up_domain == null)
            return Kernel.Die(th, "Cannot eval; no compiler available");
        CrossDomainReceiver r = (CrossDomainReceiver)
            up_domain.CreateInstanceAndUnwrap("Kernel", "Niecza.UpCallee");
        SubInfo outer = th.caller.info;
        string[] msg = r.Call(AppDomain.CurrentDomain, new string[] { "eval",
                str.Fetch().mo.mro_raw_Str.Get(str),
                (outer.unit == null ? "" : outer.unit.name),
                outer.xref_no.ToString()
                });
        if (msg[0] != "")
            return Kernel.Die(th, msg[0]);
        return th.MakeChild(null, new SubInfo("boot-" +
                    eval_result.Method.DeclaringType, eval_result), Kernel.AnyP);
    }

    public static Variable pair(Variable key, Variable value) {
        P6any l = new P6opaque(Kernel.PairMO);
        l.SetSlot("key", key);
        l.SetSlot("value", value);
        return Kernel.NewROScalar(l);
    }

    public static VarDeque start_iter(Variable thing) {
        if (thing.islist)
            return thing.Fetch().mo.mro_raw_iterator.Get(thing);
        else
            return new VarDeque(thing);
    }

    public static Variable array_constructor(Variable bits) {
        VarDeque rest  = start_iter(bits);
        VarDeque items = new VarDeque();
        while (Kernel.IterHasFlat(rest, true))
            items.Push(Kernel.NewMuScalar(rest.Shift().Fetch()));
        P6any l = new P6opaque(Kernel.ArrayMO);
        l.SetSlot("rest", rest);
        l.SetSlot("items", items);
        return Kernel.NewROScalar(l);
    }

    public static string frame_subname(Frame fr) {
        return fr.info.name.Substring(fr.info.name.IndexOf(" ")+1);
    }

    public static Variable count(P6any fcni) {
        int i = get_count(fcni);
        return (i == int.MaxValue) ? MakeFloat(double.PositiveInfinity) :
            MakeInt(i);
    }
    public static int get_count(P6any fcni) {
        if (!fcni.Isa(Kernel.CodeMO))
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

    public static Variable arity(P6any fcni) {
        if (!fcni.Isa(Kernel.CodeMO))
            return MakeInt(1); // can't introspect fake subs (?)
        SubInfo si = (SubInfo) fcni.GetSlot("info");
        int[] sig = si.sig_i;
        if (sig == null)
            return MakeInt(1);
        int arity = 0;
        for (int i = 0; i < sig.Length; i += SubInfo.SIG_I_RECORD) {
            int fl = sig[i + SubInfo.SIG_I_FLAGS];
            if ((fl & (SubInfo.SIG_F_SLURPY_CAP | SubInfo.SIG_F_SLURPY_POS |
                    SubInfo.SIG_F_SLURPY_PCL | SubInfo.SIG_F_SLURPY_NAM |
                    SubInfo.SIG_F_OPTIONAL | SubInfo.SIG_F_DEFOUTER |
                    SubInfo.SIG_F_HASDEFAULT)) != 0)
                continue;
            if ((fl & SubInfo.SIG_F_POSITIONAL) == 0) continue;
            arity++;
        }
        return MakeInt(arity);
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
                sources[i] = start_iter(pcl[i]);
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
                iter[i] = start_iter(pcl[i]);
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
        object fnc = th.lex2;
        int tailmode = th.lexi0;

        switch (th.ip) {
            case 0:
                Variable[] pen;
                if (!src.TryGet(out pen, tailmode != 0)) {
                    P6opaque thunk = new P6opaque(Kernel.GatherIteratorMO);
                    th.lex = new Dictionary<string,object>();
                    th.lex["!return"] = null;
                    th.MarkSharedChain();
                    thunk.slots[0] = Kernel.NewMuScalar(th);
                    thunk.slots[1] = Kernel.NewMuScalar(Kernel.AnyP);
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
                if (fnc is P6any) {
                    return ((P6any)fnc).Invoke(th, (Variable[])th.lex3, null);
                } else if (fnc == null) {
                    th.resultSlot = MakeParcel((Variable[]) th.lex3);
                    goto case 2;
                } else {
                    th.resultSlot = ((Func<Variable,Variable>)fnc).Invoke(
                        ((Variable[])th.lex3)[0]);
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
        int arity = get_count(fcni);

        Frame fr = th.MakeChild(null, CommonMEMap_I, Kernel.AnyP);
        fr.lexi0 = 0;
        fr.lex0 = new BatchSource(arity, iter);
        fr.lex1 = new VarDeque();
        fr.lex2 = fcni;
        return fr;
    }

    public static Frame MEMap_for_each(Frame th, P6any lst,
            Func<Variable,Variable> fcn) {
        VarDeque iter = new VarDeque(Kernel.NewRWListVar(lst));

        Frame fr = th.MakeChild(null, CommonMEMap_I, Kernel.AnyP);
        fr.lexi0 = 0;
        fr.lex0 = new BatchSource(1, iter);
        fr.lex1 = new VarDeque();
        fr.lex2 = fcn;
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
        Frame fr = th.MakeChild(null, CommonMEMap_I, Kernel.AnyP);
        Kernel.SetTopFrame(fr);
        fr.lexi0 = 0;
        fr.lex2 = ExtractWith(with, ref pcl);
        fr.lex0 = new ZipSource(pcl);
        fr.lex1 = new VarDeque();
        return fr;
    }

    public static Frame MECross(Frame th, bool with, Variable[] pcl) {
        Frame fr = th.MakeChild(null, CommonMEMap_I, Kernel.AnyP);
        Kernel.SetTopFrame(fr);
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
                            thunk.slots[0] = Kernel.NewMuScalar(th);
                            thunk.slots[1] = Kernel.NewMuScalar(Kernel.AnyP);
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

        Frame fr = th.MakeChild(null, CommonGrep_I, Kernel.AnyP);
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

        Variable[] p = to.pos;
        VarHash n    = to.named;
        if (o != null) {
            p = (Variable[]) o.slots[0];
            n = o.slots[1] as VarHash;
        }

        return de.info.Binder(th, de.outer, de.ip6, p, n, false, de);
    }

    public static bool obj_can(P6any obj, string mname) {
        return obj.mo.mro_methods.ContainsKey(mname);
    }

    // Used mostly to initialize $*PID et al
    public static string programName;
    public static string execName;

    public static Variable getenv(string str) {
        return Kernel.BoxAnyMO(Environment.GetEnvironmentVariable(str), Kernel.StrMO);
    }

    public static void setenv(string key, string val) {
        Environment.SetEnvironmentVariable(key, val);
    }

    public static Variable sysquery(int ix) {
        switch (ix) {
            case 0: return BoxLoS(Kernel.commandArgs);
            case 1: return Kernel.BoxAnyMO(programName ?? AppDomain.CurrentDomain.FriendlyName, Kernel.StrMO);
            case 2: return Kernel.BoxAnyMO(execName, Kernel.StrMO);
            case 3: return Kernel.BoxAnyMO(AppDomain.CurrentDomain.BaseDirectory, Kernel.StrMO);
            case 4: {
                VarHash ret = new VarHash();
                foreach (System.Collections.DictionaryEntry de in Environment.GetEnvironmentVariables()) {
                    ret[(string) de.Key] = Kernel.BoxAnyMO((string)de.Value, Kernel.StrMO);
                }
                return Kernel.BoxAnyMO(ret, Kernel.HashMO);
            }
            default: return null;
        }
    }
}
