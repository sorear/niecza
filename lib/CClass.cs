using System;
using System.Collections.Generic;

namespace Niecza.Compiler {
    class CClass {
        int[] terms;

        public CClass(params int[] terms) { this.terms = terms; }

        const int ALL = 0x3FFFFFFF;

        public static readonly CClass Empty = new CClass();
        public static readonly CClass Full  = new CClass(0, ALL);

        public static readonly Dictionary<string,int> Gc;
        static CClass() {
            Gc = new Dictionary<string,int>();
            var gc = "LuLlLtLmLoMnMsMeNdNlNoZsZlZpCcCfCs" +
                "CoPcPdPsPePiPfPoSmScSkSoCn";
            for (int i = 0; i < gc.Length; i += 2)
                Gc[gc.Substring(i, 2)] = i / 2;
        }


        public static CClass range(int c1, int c2) {
            if (c1 > c2) return Empty;
            return new CClass(c1, ALL, c2 + 1, 0);
        }

        public static CClass list(params int[] cs) {
            var ch = Empty;
            foreach (int c in cs) ch = ch.plus(c);
            return ch;
        }

        public static CClass catm(params string[] mask) {
            int fl = 0;
            foreach (string m in mask)
                fl |= (1 << Gc[m]);
            return fl != 0 ? new CClass(0, fl) : Empty;
        }

        CClass binop(bool minus, CClass other) {
            int[] al = terms;
            int[] bl = other.terms;
            int alix = 0;
            int blix = 0;
            int alcur = 0;
            int blcur = 0;
            List<int> o = new List<int>();
            int pos = 0;
            int ocur = 0;

            while (pos != int.MaxValue) {
                bool ata = (alix < al.Length) && (al[alix] == pos);
                bool atb = (blix < bl.Length) && (bl[blix] == pos);

                if (ata) {
                    alcur = al[alix+1];
                    alix += 2;
                }

                if (atb) {
                    blcur = bl[blix+1];
                    blix += 2;
                }

                int onew = minus ? (alcur & ~blcur) : (alcur | blcur);
                if (onew != ocur) {
                    o.Add(pos);
                    o.Add(onew);
                    ocur = onew;
                }

                pos = Math.Min((alix < al.Length ? al[alix] : int.MaxValue),
                        (blix < bl.Length ? bl[blix] : int.MaxValue));
            }
            return new CClass(o.ToArray());
        }

        public CClass plus(CClass o) { return binop(false, o); }
        public CClass plus(int o) { return binop(false,range(o,o)); }
        public CClass minus(CClass o) { return binop(true, o); }
        public CClass minus(int o) { return binop(true,range(o,o)); }
        public CClass negate() { return Full.binop(true, this); }

        // the range here is the only part of Other_Alphabetic which is not
        // already contained in M* in Unicode 6.0.0
        public static readonly CClass Word = catm("Me", "Mn", "Ms", "Pc",
                "Nd", "Ll", "Lt", "Lu", "Lm", "Lo", "Nl").
            plus(range(9398, 9450));
        public static readonly CClass Digit = catm("Nd");
        //Unicode :Whitespace property - TODO use db
        public static readonly CClass Space = list(
                0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x0020, 0x0085,
                0x00A0, 0x1680, 0x180E, 0x2000, 0x2001, 0x2002, 0x2003,
                0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200A,
                0x2028, 0x2029, 0x202F, 0x205F, 0x3000);
        public static readonly CClass VSpace = list(
                0x000A, 0x000B, 0x000C, 0x000D, 0x0085, 0x2028, 0x2029);
        public static readonly CClass HSpace = Space.minus(VSpace);

        public static readonly CClass Alpha = CClass.catm("Lu", "Lt", "Ll",
                "Lm", "Lo");
    }
}

