using System;
using System.IO;
using System.Security.Cryptography;
using System.Collections.Generic;
using System.Text;

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
        internal object[] bynum; // objects in unit
        internal object root; // the RuntimeUnit object
        internal int nobj;
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

        static readonly HashAlgorithm hash = SHA256.Create();
        static readonly string signature = "Niecza-Serialized-Module";
        static readonly int version = 1;

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

        // Routines for use by compilation manager

        // Loads a single unit from the compiled-data directory.
        // Will throw a ThawException if a stale reference is encountered
        // or other data format error.
        public SerUnit LoadUnit(string name) {
            SerUnit su;

            // is the unit already loaded?
            if (units.TryGetValue(name, out su))
                return su;

            string file = Path.Combine(AppDomain.CurrentDomain.BaseDirectory,
                    name + ".ser");
            byte[] bytes = File.ReadAllBytes(file);

            su = new SerUnit();
            su.name = name;
            su.hash = hash.ComputeHash(bytes);

            ThawBuffer tb = new ThawBuffer(this, su, bytes);

            bool success = false;
            try {
                string rsig = tb.String();
                if (rsig != signature)
                    throw new ThawException("signature mismatch loading " + file);
                int rver = tb.Int();
                if (rver != version)
                    throw new ThawException("version mismatch loading " + file);

                su.root = tb.ObjRef();
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
            string file = Path.Combine(AppDomain.CurrentDomain.BaseDirectory,
                    name + ".ser");

            FreezeBuffer fb = new FreezeBuffer(this, su);

            try {
                fb.String(signature);
                fb.Int(version);
                fb.ObjRef(root);

                byte[] data = fb.GetData();
                su.hash = hash.ComputeHash(data);
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

        // types of P6any-reified object
        P6opaque, // eventually let's specialize this
        Frame,
        Cursor,

        // miscellany
        Variant, // allow 5, see FallbackFreeze

        // variables
        SimpleVariable = Variant + 5, // allow 4 for flags
        SubstrLValue = SimpleVariable + 4,
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

        public void Short(short x) {
            Ensure(2);
            data[wpointer++] = (byte)(x >>  8);
            data[wpointer++] = (byte)(x      );
        }

        public void Int(int x) {
            Ensure(4);
            data[wpointer++] = (byte)(x >> 24);
            data[wpointer++] = (byte)(x >> 16);
            data[wpointer++] = (byte)(x >>  8);
            data[wpointer++] = (byte)(x      );
        }

        public void Long(long x) {
            Ensure(8);
            data[wpointer++] = (byte)(x >> 56);
            data[wpointer++] = (byte)(x >> 48);
            data[wpointer++] = (byte)(x >> 40);
            data[wpointer++] = (byte)(x >> 32);
            data[wpointer++] = (byte)(x >> 24);
            data[wpointer++] = (byte)(x >> 16);
            data[wpointer++] = (byte)(x >>  8);
            data[wpointer++] = (byte)(x      );
        }

        public void String(string s) {
            if (s == null) {
                Int(-1);
            } else {
                Int(s.Length);
                foreach (char ch in s)
                    Short((short)ch);
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

        // Call this to freeze a variant-typed value.  (Avoid)
        static Type[] anyTypes = new Type[] {
            typeof(string), typeof(P6any[]), typeof(Variable[]),
            typeof(int), typeof(double),
        };

        void FallbackFreeze(object o) {
            int ix = 0;
            Type t = o.GetType();
            while (ix != 11 && anyTypes[ix] != t) ix++;
            Byte((byte)(((int)SerializationCode.Variant) + ix));

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
                    Int((int)o);
                    break;
                case 4:
                    Long(BitConverter.DoubleToInt64Bits((double)o));
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

    class ThawBuffer {
        byte[] data;
        int rpointer;
        ObjectRegistry reg;

        SerUnit[] unit_map = new SerUnit[8];
        int refed_units;
        SerUnit unit;

        internal ThawBuffer(ObjectRegistry reg, SerUnit unit, byte[] data) {
            this.data = data;
            this.reg  = reg;
            this.unit = unit;
        }

        public byte Byte() { return data[rpointer++]; }

        public short Short() {
            return (short)((((int)Byte()) << 8) | Byte());
        }

        public int Int() {
            return (((int)Byte()) << 24) | (((int)Byte()) << 16) |
                (((int)Byte()) << 8) | ((int)Byte());
        }

        public long Long() {
            // try to do as much as possible in 32-bit precision,
            // but suppress sign extension
            return (((long)Int()) << 32) | (long)(uint)Int();
        }

        public string String() {
            int l = Int();

            if (l < 0) return null;
            char[] cb = new char[l];

            for (int i = 0; i < l; i++)
                cb[i] = (char)Short();

            return new string(cb);
        }

        public byte[] Bytes(int k) {
            byte[] buf = new byte[k];

            for (int i = 0; i < k; i++)
                buf[i] = Byte();

            return buf;
        }

        public object ObjRef() {
            var tag = (SerializationCode)Byte();
            int i, j;
            switch(tag) {
                case SerializationCode.Null:
                    return null;
                case SerializationCode.SelfRef:
                    i = Int();
                    return unit.bynum[i];
                case SerializationCode.ForeignRef:
                    i = Int();
                    j = Int();
                    return unit_map[i].bynum[j];
                case SerializationCode.NewUnitRef:
                    return LoadNewUnit();
                default:
                    throw new ThawException("unexpected object tag" + (byte)tag);
            }
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
            StringBuilder sb = new StringBuilder();
            sb.AppendFormat("Hash mismatch for unit {0} referenced from {1}",
                    su.name, unit.name);

            sb.Append(", wanted ");
            foreach (byte b in hash)
                sb.AppendFormat("{0:X2}", b);

            sb.Append(", got ");
            foreach (byte b in su.hash)
                sb.AppendFormat("{0:X2}", b);

            throw new ThawException(sb.ToString());
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
}
