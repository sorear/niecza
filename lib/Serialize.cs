using System;
using System.IO;
using System.Security.Cryptography;
using System.Collections.Generic;
using System.Text;
using Niecza.CLRBackend;

// Here in Niecza we have four different kinds of unit scopes:
//
// * COMPILING::UNIT, aka RuntimeUnit: one of these exists for every
//   call into the compiler, whether eval or module.  The REPL will
//   be considered as if it were eval.
//
// * Serialization scopes, which are created when compiling modules
//   or when pre-compiling a main program.  During execution there is
//   no current serialization scope; evals inherit the serialization
//   scope or lack thereof that was in effect.
//
// * Assembly scopes, which are an artifact of the CLR and almost align
//   with precompilation scopes, except that they have to exist always
//   because methods cannot be created free-floating in CLR 2.x.
//
//   An assembly scope is created for all serialization scopes, and
//   non-saved anonymous assemblies are created for eval-and-run of
//   a file and non-BEGIN-time evals.
//
// * GLOBAL scope is very much like serialization scope except that there
//   is a "true globals" scope that is used when not serializing.

// This implements "bounded serialization" for Niecza.  Unfortunately
// the CLR's builtin serialization can't efficiently be made bounded,
// and anyway it would be nice if the serialization format could be
// transported across backends.

// TODO: implement a more Storable-like interface.

// Note, the serialization subsystem is *NOT* thread safe !
namespace Niecza.Serialization {
    // Information kept on a serialization unit after loading or storing,
    // but not before storing.
    class SerUnit {
        internal string name; // eg "File.Copy"
        internal byte[] hash; // hash of entire file, filled at write time
        internal object[] bynum = new object[8]; // objects in unit
        internal object root; // the RuntimeUnit object
        internal int nobj; // = 0
    }

    // The central feature of *bounded* serialization is that object
    // registries are kept distinct from the (de)serializer, and can
    // be shared between serialization runs.
    class ObjectRegistry {
        // TODO: investigate a more specialized representation,
        // ideally not having to hash as many objects
        struct ObjRef {
            public SerUnit unit;
            public int id;
        }
        Dictionary<object,ObjRef> byref = new Dictionary<object,ObjRef>();

        Dictionary<string,SerUnit> units =
            new Dictionary<string,SerUnit>();

        internal static HashAlgorithm NewHash() { return new SHA256Managed(); }

        static readonly string signature = "Niecza-Serialized-Module";
        static readonly int version = 4;

        // Routines for use by serialization code
        public bool CheckWriteObject(SerUnit into, object o,
                out SerUnit lui, out int id) {
            ObjRef or;
            if (byref.TryGetValue(o, out or)) {
                lui = or.unit;
                id  = or.id;
                return true;
            }

            if (into.nobj == into.bynum.Length)
                Array.Resize(ref into.bynum, into.nobj * 2);

            or.unit = lui = into;
            id = or.id = into.nobj++;
            into.bynum[id] = o;

            byref[o] = or;

            return false;
        }

        public void RegisterThawed(SerUnit into, object o) {
            ObjRef or;
            if (into.nobj == into.bynum.Length)
                Array.Resize(ref into.bynum, into.nobj * 2);

            or.unit = into;
            or.id   = into.nobj++;
            into.bynum[or.id] = o;

            byref[o] = or;
        }

        // Routines for use by compilation manager

        // Loads a single unit from the compiled-data directory.
        // Will throw a ThawException if a stale reference is encountered
        // or other data format error.
        public SerUnit LoadUnit(string name) {
            SerUnit su;

            // is the unit already loaded?
            if (units.TryGetValue(name, out su))
                return su;

            string file = Path.Combine(Backend.obj_dir, Backend.prefix +
                    name.Replace("::",".") + ".ser");
            byte[] bytes = File.ReadAllBytes(file);

            su = new SerUnit();
            su.name = name;
            su.hash = NewHash().ComputeHash(bytes);

            ThawBuffer tb = new ThawBuffer(this, su, bytes);

            units[name] = su;
            bool success = false;
            try {
                string rsig = tb.String();
                if (rsig != signature)
                    throw new ThawException("signature mismatch loading " + file);
                int rver = tb.Int();
                if (rver != version)
                    throw new ThawException("version mismatch loading " + file);

                su.root = tb.ObjRef();
                tb.RunFixups();
                success = true;
            } finally {
                // don't leave half-read units in the map
                if (!success)
                    UnloadUnit(name);
            }

            return su;
        }

        // removes a stale unit so a new version can be saved over it.
        public void UnloadUnit(string name) {
            if (!units.ContainsKey(name))
                return;
            SerUnit su = units[name];
            units.Remove(name);

            for (int i = 0; i < su.nobj; i++)
                byref.Remove(su.bynum[i]);
        }

        public SerUnit SaveUnit(string name, IFreeze root) {
            SerUnit su = new SerUnit();
            su.name = name;
            su.root = root;

            if (units.ContainsKey(name))
                throw new InvalidOperationException("unit " +name+ " exists");

            bool success = false;
            string file = Path.Combine(Backend.obj_dir, Backend.prefix +
                    name.Replace("::",".") + ".ser");

            FreezeBuffer fb = new FreezeBuffer(this, su);

            units[name] = su;

            try {
                fb.String(signature);
                fb.Int(version);
                fb.ObjRef(root);

                byte[] data = fb.GetData();
                su.hash = NewHash().ComputeHash(data);
                File.WriteAllBytes(file, data);
                success = true;
            } finally {
                if (!success)
                    UnloadUnit(name);
            }

            return su;
        }
    }

    // One of these codes is written at the beginning of every object ref
    enum SerializationCode : byte {
        // special
        Null,

        // existing objects
        ForeignRef,
        SelfRef,
        NewUnitRef,

        // types of new object
        RuntimeUnit,
        SubInfo,
        STable,
        StashEnt,
        Rat,
        FatRat,
        Complex,
        BigInteger,
        VarDeque,
        VarHash,
        DispatchEnt,
        RxFrame,
        P6how,
        ReflectObj,
        CC,
        AltInfo,

        // types of P6any-reified object
        P6opaque, // eventually let's specialize this
        Frame,
        Cursor,

        // miscellany - keep these in same order as FallbackFreeze
        String,
        ArrP6any,
        ArrVariable,
        ArrString,
        ArrCC,
        Boolean,
        Int,
        Double,
        Type,

        // variables
        SimpleVariable, // allow 4 for flags
        SimpleVariable_1,
        SimpleVariable_2,
        SimpleVariable_3,
        SubstrLValue,
        TiedVariable,

        // vivification hooks
        SubViviHook,
        ArrayViviHook,
        NewArrayViviHook,
        HashViviHook,
        NewHashViviHook,

        // Longest-token automaton descriptors
        LADNone, // no-args
        LADNull,
        LADDot,
        LADDispatcher,
        LADImp,
        LADStr, // string
        LADStrNoCase,
        LADMethod,
        LADParam,
        LADOpt, // LAD
        LADPlus,
        LADStar,
        LADSequence, // LAD[]
        LADAny,
        LADCC, // CC
    }

    // An instance of this class is used to serialize serialization units
    public class FreezeBuffer {
        byte[] data;
        int wpointer;

        Dictionary<SerUnit,int> unit_to_offset;
        int usedunits;

        ObjectRegistry reg;
        SerUnit unit;

        internal FreezeBuffer(ObjectRegistry reg, SerUnit unit) {
            if (reg == null || unit == null)
                throw new ArgumentNullException();
            this.reg = reg;
            this.unit = unit;
            unit_to_offset = new Dictionary<SerUnit,int>();
            data = new byte[256];
        }

        internal byte[] GetData() {
            byte[] ret = new byte[wpointer];
            Array.Copy(data, ret, ret.Length);
            return ret;
        }

        void Ensure(int ct) {
            while (ct + wpointer > data.Length)
                Array.Resize(ref data, data.Length * 2);
        }

        public void Byte(byte x) {
            Ensure(1);
            data[wpointer++] = x;
        }

        public void Long(long x) {
            //Console.WriteLine("Saving {0} at {1}", x, wpointer);
            Ensure(10);
            while (true) {
                if (x >= -64 && x <= 63) {
                    data[wpointer++] = (byte) (127 & (byte)x);
                    break;
                } else {
                    data[wpointer++] = (byte) (128 | (byte)x);
                    x >>= 7;
                }
            }
        }

        public void ULong(ulong x) {
            //Console.WriteLine("Saving {0} at {1}", x, wpointer);
            Ensure(10);
            while (true) {
                if (x <= 127) {
                    data[wpointer++] = (byte) (127 & (byte)x);
                    break;
                } else {
                    data[wpointer++] = (byte) (128 | (byte)x);
                    x >>= 7;
                }
            }
        }

        public void Short(short x) { Long(x); }
        public void Int(int x) { Long(x); }

        public void Double(double x) {
            Long(BitConverter.DoubleToInt64Bits(x));
        }

        public void String(string s) {
            if (s == null) {
                Int(-1);
            } else {
                Int(s.Length);
                foreach (char ch in s)
                    ULong((ulong)ch);
            }
        }

        public void Strings(string[] s) {
            if (s == null) Int(-1);
            else {
                Int(s.Length);
                foreach (string ch in s) String(ch);
            }
        }

        public void Ints(int[] s) {
            if (s == null) {
                Int(-1);
            } else {
                Int(s.Length);
                foreach (int ch in s)
                    Int(ch);
            }
        }

        public void Refs<T> (T[] x) {
            if (x == null) {
                Int(-1);
            } else {
                Int(x.Length);
                foreach (T y in x)
                    ObjRef(y);
            }
        }

        public void Refs<T> (IList<T> x) {
            if (x == null) {
                Int(-1);
            } else {
                Int(x.Count);
                foreach (T y in x)
                    ObjRef(y);
            }
        }

        // This is the main routine you should call from your Freeze
        // callbacks to freeze an object
        public void ObjRef(object o) {
            int id;
            SerUnit altunit;
            if (Config.SerTrace)
                Console.WriteLine("Saving {0} at {1}...", o, wpointer);
            if (o == null) { // null pointers are special
                Byte((byte)SerializationCode.Null);
                return;
            }

            if (reg.CheckWriteObject(unit, o, out altunit, out id)) {
                if (altunit == unit) {
                    Byte((byte)SerializationCode.SelfRef);
                } else {
                    int altcode;
                    if (!unit_to_offset.TryGetValue(altunit, out altcode)) {
                        Byte((byte)SerializationCode.NewUnitRef);
                        String(altunit.name);
                        // save the hash too so stale refs can be caught
                        foreach (byte b in altunit.hash) Byte(b);

                        unit_to_offset[altunit] = usedunits++;
                    } else {
                        Byte((byte)SerializationCode.ForeignRef);
                        Int(altcode);
                    }
                }
                Int((int)id);
            } else {
                // must take responsibility for saving the tag
                IFreeze f = o as IFreeze;
                if (f != null) {
                    f.Freeze(this);
                } else {
                    FallbackFreeze(o);
                }
            }
        }

        [Immutable]
        internal static Type[] boxTypes = new Type[] {
            null, typeof(Rat), typeof(FatRat), typeof(Complex),
            typeof(double), typeof(int), typeof(string), typeof(VarHash),
            typeof(Variable[]), typeof(VarDeque), typeof(STable),
        };
        [Immutable]
        internal static Func<P6opaque>[] boxCreate = new Func<P6opaque>[] {
            P6opaque.Create, BoxObject<Rat>.Create, BoxObject<FatRat>.Create,
            BoxObject<Complex>.Create, BoxObject<double>.Create,
            BoxObject<int>.Create, BoxObject<string>.Create,
            BoxObject<VarHash>.Create, BoxObject<Variable[]>.Create,
            BoxObject<VarDeque>.Create, BoxObject<STable>.Create,
        };
        [Immutable]
        static Type[] anyTypes = new Type[] {
            typeof(string), typeof(P6any[]), typeof(Variable[]),
            typeof(string[]), typeof(CC[]),
            typeof(bool), typeof(int), typeof(double), typeof(Type),
        };

        void FallbackFreeze(object o) {
            int ix = 0;
            Type t = o.GetType();
            while (ix != anyTypes.Length && anyTypes[ix] != t) ix++;
            Byte((byte)(((int)SerializationCode.String) + ix));

            switch(ix) {
                case 0:
                    String((string)o);
                    break;
                case 1:
                    Refs((P6any[])o);
                    break;
                case 2:
                    Refs((Variable[])o);
                    break;
                case 3:
                    Refs((string[])o);
                    break;
                case 4:
                    Refs((CC[])o);
                    break;
                case 5:
                    Byte((byte)((bool)o ? 1 : 0));
                    break;
                case 6:
                    Int((int)o);
                    break;
                case 7:
                    Double((double)o);
                    break;
                case 8:
                    String(((Type)o).AssemblyQualifiedName);
                    break;
                default:
                    throw new NotImplementedException(t.FullName);
            }
        }
    }

    // Note that this interface only handles freezing - thaw is done using
    // a switch statement.
    public interface IFreeze {
        void Freeze(FreezeBuffer fb);
    }
    // implement this if you need to copy in data from other objects, &c
    interface IFixup {
        void Fixup();
    }

    class ThawBuffer {
        byte[] data;
        int rpointer;
        ObjectRegistry reg;

        SerUnit[] unit_map = new SerUnit[8];
        int refed_units;
        SerUnit unit;

        List<IFixup> fixups_needed = new List<IFixup>();
        List<object> revalidate = new List<object>();

        public Type type;
        public Dictionary<string,System.Reflection.MethodInfo> methods;

        internal ThawBuffer(ObjectRegistry reg, SerUnit unit, byte[] data) {
            this.data = data;
            this.reg  = reg;
            this.unit = unit;
        }

        internal void RunFixups() {
            P6how.BulkRevalidate(revalidate);
            foreach (IFixup f in fixups_needed)
                f.Fixup();
            fixups_needed.Clear();
        }

        internal void PushFixup(IFixup f) {
            fixups_needed.Add(f);
        }

        internal void PushRevalidate(STable f) {
            revalidate.Add(f);
        }

        public byte Byte() { return data[rpointer++]; }

        public long Long() {
            int shift = 0;
            long accum = 0;
            while (true) {
                byte b = Byte();
                accum |= (((long)(b & 127)) << shift);
                shift += 7;
                if ((b & 128) == 0) {
                    if ((b & 64) != 0) {
                        accum |= ((-1L) << shift);
                    }
                    //Console.WriteLine("Read {0} end {1}", accum, rpointer);
                    return accum;
                }
            }
        }

        public ulong ULong() {
            int shift = 0;
            ulong accum = 0;
            while (true) {
                byte b = Byte();
                accum |= (((ulong)(b & 127)) << shift);
                shift += 7;
                if ((b & 128) == 0) {
                    //Console.WriteLine("Read {0} end {1}", accum, rpointer);
                    return accum;
                }
            }
        }

        public short Short() {
            return checked((short)Long());
        }

        public int Int() {
            return checked((int)Long());
        }

        public double Double() {
            return BitConverter.Int64BitsToDouble(Long());
        }

        public string String() {
            int l = Int();

            if (l < 0) return null;
            char[] cb = new char[l];

            for (int i = 0; i < l; i++)
                cb[i] = (char)ULong();

            return new string(cb);
        }

        public byte[] Bytes(int k) {
            byte[] buf = new byte[k];

            for (int i = 0; i < k; i++)
                buf[i] = Byte();

            return buf;
        }

        public List<T> RefsL<T>() where T : class {
            int ct = Int();
            if (ct < 0) return null;
            List<T> ret = new List<T>();
            for (int i = 0; i < ct; i++)
                ret.Add((T) ObjRef());
            return ret;
        }

        public int[] Ints() {
            int ct = Int();
            if (ct < 0) return null;
            int[] ret = new int[ct];
            for (int i = 0; i < ct; i++) ret[i] = Int();
            return ret;
        }

        public string[] Strings() {
            int ct = Int();
            if (ct < 0) return null;
            string[] ret = new string[ct];
            for (int i = 0; i < ct; i++) ret[i] = String();
            return ret;
        }

        public T[] RefsA<T>() where T : class {
            int ct = Int();
            if (ct < 0) return null;
            T[] ret = new T[ct];
            for (int i = 0; i < ct; i++)
                ret[i] = (T) ObjRef();
            return ret;
        }

        // used from ObjRef only so guaranteed non-null
        T[] RefsARegister<T>() where T : class {
            int ct = Int();
            T[] ret = new T[ct];
            Register(ret);
            for (int i = 0; i < ct; i++)
                ret[i] = (T) ObjRef();
            return ret;
        }

        public object ObjRef() {
            var tag = (SerializationCode)Byte();
            if (Config.SerTrace)
                Console.WriteLine("Reading {0} from {1}...", tag, rpointer-1);
            int i, j;
            switch(tag) {
                case SerializationCode.Null:
                    return null;

                case SerializationCode.ForeignRef:
                    i = Int();
                    j = Int();
                    return unit_map[i].bynum[j];
                case SerializationCode.SelfRef:
                    i = Int();
                    return unit.bynum[i];
                case SerializationCode.NewUnitRef:
                    return LoadNewUnit();

                case SerializationCode.RuntimeUnit:
                    return RuntimeUnit.Thaw(this);
                case SerializationCode.SubInfo:
                    return SubInfo.Thaw(this);
                case SerializationCode.STable:
                    return STable.Thaw(this);
                case SerializationCode.StashEnt:
                    return StashEnt.Thaw(this);
                case SerializationCode.Rat:
                    return Rat.Thaw(this);
                case SerializationCode.FatRat:
                    return FatRat.Thaw(this);
                case SerializationCode.Complex:
                    return Complex.Thaw(this);
                case SerializationCode.BigInteger:
                    return BigInteger.Thaw(this);
                case SerializationCode.VarDeque:
                    return VarDeque.Thaw(this);
                case SerializationCode.VarHash:
                    return VarHash.Thaw(this);
                case SerializationCode.DispatchEnt:
                    return DispatchEnt.Thaw(this);
                //case SerializationCode.RxFrame:
                //    return RxFrame.Thaw(this);
                case SerializationCode.P6how:
                    return P6how.Thaw(this);
                case SerializationCode.CC:
                    return CC.Thaw(this);
                case SerializationCode.AltInfo:
                    return AltInfo.Thaw(this);

                case SerializationCode.ReflectObj:
                    return ReflectObj.Thaw(this);
                case SerializationCode.P6opaque:
                    return P6opaque.Thaw(this);
                case SerializationCode.Frame:
                    return Frame.Thaw(this);
                //Cursor,

                case SerializationCode.String:
                    return Register(String());
                case SerializationCode.ArrP6any:
                    return RefsARegister<P6any>();
                case SerializationCode.ArrVariable:
                    return RefsARegister<Variable>();
                case SerializationCode.ArrString:
                    return RefsARegister<string>();
                case SerializationCode.ArrCC:
                    return RefsARegister<CC>();
                case SerializationCode.Boolean:
                    return Register(Byte() != 0);
                case SerializationCode.Int:
                    return Register(Int());
                case SerializationCode.Double:
                    return Register(Double());
                case SerializationCode.Type:
                    return Register(Type.GetType(String(), true));

                case SerializationCode.SimpleVariable:
                case SerializationCode.SimpleVariable_1:
                case SerializationCode.SimpleVariable_2:
                case SerializationCode.SimpleVariable_3:
                    return SimpleVariable.Thaw(this,
                            (int)tag - (int)SerializationCode.SimpleVariable);
                case SerializationCode.SubstrLValue:
                    return SubstrLValue.Thaw(this);
                case SerializationCode.TiedVariable:
                    return TiedVariable.Thaw(this);

                case SerializationCode.SubViviHook:
                    return SubViviHook.Thaw(this);
                case SerializationCode.ArrayViviHook:
                    return ArrayViviHook.Thaw(this);
                case SerializationCode.NewArrayViviHook:
                    return NewArrayViviHook.Thaw(this);
                case SerializationCode.HashViviHook:
                    return HashViviHook.Thaw(this);
                case SerializationCode.NewHashViviHook:
                    return NewHashViviHook.Thaw(this);

                case SerializationCode.LADNone:
                    return Register(new LADNone());
                case SerializationCode.LADNull:
                    return Register(new LADNull());
                case SerializationCode.LADDot:
                    return Register(new LADDot());
                case SerializationCode.LADDispatcher:
                    return Register(new LADDispatcher());
                case SerializationCode.LADImp:
                    return Register(new LADImp());
                case SerializationCode.LADStr:
                    return LADStr.Thaw(this);
                case SerializationCode.LADStrNoCase:
                    return LADStrNoCase.Thaw(this);
                case SerializationCode.LADMethod:
                    return LADMethod.Thaw(this);
                case SerializationCode.LADParam:
                    return LADParam.Thaw(this);
                case SerializationCode.LADOpt:
                    return LADOpt.Thaw(this);
                case SerializationCode.LADPlus:
                    return LADPlus.Thaw(this);
                case SerializationCode.LADStar:
                    return LADStar.Thaw(this);
                case SerializationCode.LADSequence:
                    return LADSequence.Thaw(this);
                case SerializationCode.LADAny:
                    return LADAny.Thaw(this);
                case SerializationCode.LADCC:
                    return LADCC.Thaw(this);
                default:
                    throw new ThawException("unexpected object tag " + tag);
            }
        }

        // call this when thawing any new object
        internal object Register(object o) {
            reg.RegisterThawed(unit, o);
            return o;
        }

        object LoadNewUnit() {
            string name = String();
            if (refed_units == unit_map.Length)
                Array.Resize(ref unit_map, refed_units * 2);

            SerUnit su = reg.LoadUnit(name);
            unit_map[refed_units++] = su;

            byte[] hash = Bytes(su.hash.Length);

            for (int i = 0; i < hash.Length; i++)
                if (hash[i] != su.hash[i])
                    goto badhash;

            int ix = Int();
            return su.bynum[ix];

badhash:
            throw new ThawException(string.Format("Hash mismatch for " +
                "unit {0} referenced from {1}, wanted {2}, got {3}",
                su.name, unit.name, Utils.HashToStr(unit.hash),
                Utils.HashToStr(su.hash)));
        }
    }

    // Thrown to indicate data format problems in the serialized stream
    // Not necessarily bugs; could also indicate stale files, including
    // cases where the data format is changed and cases where a depended
    // file was recreated
    class ThawException : Exception {
        public ThawException(string s) : base(s) { }
        public ThawException() : base() { }
    }

    public class ReflectObj : IFreeze {
        protected virtual object[] GetData() { return new object[0]; }
        protected virtual void SetData(object[] a) { }
        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.ReflectObj);
            fb.String(GetType().AssemblyQualifiedName);
            fb.Refs(GetData());
        }

        internal static ReflectObj Thaw(ThawBuffer tb) {
            string nm = tb.String();
            if (Backend.cross_level_load)
                nm = nm.Replace("Run.", "");
            Type nt = Type.GetType(nm, true);
            ReflectObj n = (ReflectObj)
                nt.GetConstructor(new Type[0]).Invoke(null, new object[0]);
            tb.Register(n);
            n.SetData(tb.RefsA<object>());
            return n;
        }
    }
}
