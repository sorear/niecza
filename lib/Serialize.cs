using System;

// This implements "bounded serialization" for Niecza.  Unfortunately
// the CLR's builtin serialization can't efficiently be made bounded,
// and anyway it would be nice if the serialization format could be
// transported across backends.

// TODO: implement a more Storable-like interface.
namespace Niecza.Serialization {

    // The central feature of *bounded* serialization is that object
    // registries are kept distinct from the (de)serializer, and can
    // be shared between serialization runs.
    class ObjectRegistry {
        // TODO: investigate a more specialized representation
        // In each entry here, the top 32 bits select a unit to reference
        // and the bottom 32 select an object within the unit.
        Dictionary<object,long> byref = new Dictionary<object,long>();
        object[][] bynum = new object[8][];
        int[] occupancies = new int[8];
        int nextid;
        int nextunit;

        public int NewUnit() {
            if (nextunit == bynum.Length) {
                Array.Resize(ref bynum, nextunit * 2);
                Array.Resize(ref occupancies, nextunit * 2);
            }
            bynum[nextunit] = new object[8];
            return nextunit++;
        }

        public bool CheckWriteObject(int unit, object o, out long id) {
            if (byref.TryGetValue(o, out id))
                return true;

            if (occupancies[unit] == byref[unit].Length)
                Array.Resize(ref byref[unit], occupancies[unit] * 2);

            int oid = occupancies[unit]++;
            byref[o] = id = (unit << 32) | oid;
            bynum[unit][oid] = o;

            return false;
        }
    }

    // One of these codes is written at the beginning of every object ref
    enum SerializationCode {
        // existing objects
        ForeignRef,
        SelfRef,
        NewUnitRef,
    }

    // An instance of this class is used to serialize serialization units
    class FreezeBuffer {
        byte[] data;
        int wpointer;

        int[] unit_to_offset;

        ObjectRegistry reg;
        int unit;

        public FreezeBuffer(ObjectRegistry reg) {
            this.reg = reg;
            unit = reg.NewUnit();
            unit_to_offset = new int[8];
            data = new byte[256];
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

        public void ObjRef(IFreeze o) {
            long id;
            if (reg.CheckWriteObject(unit, o, out id)) {
                int altunit = (int)(id >> 32);
                if (altunit == unit) {
                    Byte((byte)SerializationCode.SelfRef);
                } else {
                    if (altunit >= unit_to_offset.Length)
                        Array.Resize(ref unit_to_offset, altunit * 3 / 2);
                    if (unit_to_offset[altunit] == 0) {
                        Byte((byte)SerializationCode.NewUnitRef);
                        String(reg.UnitName(altunit));
                        unit_to_offset[altunit] = usedunits++;
                    } else {
                        Byte((byte)SerializationCode.ForeignRef);
                        Int(unit_to_offset[altunit]);
                    }
                }
                Int((int)id);
            } else {
                o.Freeze(this);
            }
        }
    }

    // Note that this interface only handles freezing - thaw is done using
    // a switch statement.
    interface IFreeze {
        void Freeze(FreezeBuffer fb);
    }

    class ThawBuffer {
        byte[] data;
        int dlen;
        int rpointer;
        ObjectRegistry reg;

        int[] unit_map = new int[8];
        int refed_units;
        int unit;

        public ThawBuffer(ObjectRegistry reg, string name, byte[] data,
                int dlen) {
            this.data = data;
            this.dlen = dlen;
            this.reg  = reg;

            unit = reg.NewUnit(name);
        }

        object ObjRef() {
            var tag = (SerializationCode)Byte();
            int i, j;
            string s;
            switch(tag) {
                case SerializationCode.SelfRef:
                    i = Int();
                    return reg.GetObject(unit, i);
                case SerializationCode.ForeignRef:
                    i = Int();
                    j = Int();
                    return reg.GetObject(unit_map[i], j);
                case SerializationCode.NewUnitRef:
                    s = String();
                    if (refed_units == unit_map.Length)
                        Array.Resize(ref unit_map, refed_units * 2);
                    unit_map[refed_units++] = reg.LoadUnit(s);
                    i = Int();
                    return reg.GetObject(unit_map[refed_units-1], i);
                default:
                    throw new ThawException("unexpected object tag" + (byte)tag);
            }
        }
    }
}
