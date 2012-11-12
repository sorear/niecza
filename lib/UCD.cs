using System;
using System.Reflection;
using System.IO;
using System.Runtime.CompilerServices;
using System.Collections.Generic;
using System.Text;
using Niecza;
using Niecza.UCD;

// Subsystem of Niecza that access the Unicode Character Database
//
// UCD data within Niecza is represented as a collection of named
// tables.  Most of these are property tables, which associate a
// character to one or two strings.
//
// Tables are identified by names.  Non-property tables never have
// names not starting with !.
namespace Niecza.UCD {
    abstract class Property {
        public abstract int[] GetRanges(Variable filter);
        public abstract string GetValue(int cp);

        protected static bool DoMatch(string value, Variable filter) {
            var sub = filter.Fetch();
            Variable r = Kernel.RunInferior(sub.InvokeMethod(
                Kernel.GetInferiorRoot(), "ACCEPTS",
                new Variable[] { filter, sub.mo.setting.MakeStr(value) }, null));
            return r.Fetch().mo.mro_raw_Bool.Get(r);
        }
    }

    class LimitedProperty : Property {
        int[] data;
        string[][] values;

        public LimitedProperty(int[] data, string[][] values) {
            this.data = data;
            this.values = values;
        }

        public static string[] NO  = new string[] { "N", "No", "F", "False" };
        public static string[] YES = new string[] { "Y", "Yes", "T", "True" };

        public Property Proxify(bool neg, string prefix) {
            string[][] nvalues = new string[values.Length][];
            prefix = prefix.Substring(prefix.IndexOf('+')+1);
            for (int i = 0; i < nvalues.Length; i++) {
                //Console.WriteLine("{0}/{1}", values[i][0], prefix);
                bool ok = (prefix.Length <= 1) ?
                    values[i][0].Substring(0, prefix.Length) == prefix :
                    StringProperty.Loosen(values[i][0]) ==
                        StringProperty.Loosen(prefix);
                nvalues[i] = (neg ? !ok : ok) ? YES : NO;
            }

            return new LimitedProperty(data, nvalues);
        }

        public override string GetValue(int cp) {
            int lix = 0;
            int hix = data.Length / 2;

            while (true) {
                if ((hix - lix) <= 1) {
                    return values[data[lix*2+1]][0];
                }
                int mix = (lix + hix) / 2;
                if (cp >= data[mix*2]) {
                    lix = mix;
                } else {
                    hix = mix;
                }
            }
        }

        public override int[] GetRanges(Variable filter) {
            bool[] cfilter = new bool[values.Length];
            // minor hack to make :BoolProp work - converts to True/False
            if (filter.Fetch().Isa(Compartment.Top.BoolMO))
                filter = filter.Fetch().mo.mro_Str.Get(filter);
            for (int i = 0; i < values.Length; i++) {
                foreach (string s in values[i]) {
                    if (DoMatch(s, filter)) {
                        cfilter[i] = true;
                        break;
                    }
                }
            }

            List<int> res = new List<int>();
            for (int i = 0; i < data.Length; i += 2) {
                if (cfilter[data[i+1]]) {
                    int upto = (i+2 == data.Length) ? 0x110000 : data[i+2];
                    if (res.Count > 0 && res[res.Count-1] == data[i]) {
                        res[res.Count-1] = upto;
                    } else {
                        res.Add(data[i]);
                        res.Add(upto);
                    }
                }
            }

            return res.ToArray();
        }
    }

    // Name properties include na, na1, and Name_Alias.
    class StringProperty : Property {
        // KISS for now
        int[] codepoints;
        string[] values;

        bool is_na;
        bool is_dm;
        Property jsn;

        public StringProperty(string name, int[] codepoints, string[] values) {
            this.codepoints = codepoints;
            this.values = values;
            this.is_na = (name == "na");
            this.is_dm = (name == "dm");

            this.jsn = (Property)DataSet.GetTable("JSN");
        }

        public static string Loosen(string inp) {
            StringBuilder o = new StringBuilder();
            foreach (char ch in inp)
                if ((ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') ||
                        (ch >= 'A' && ch <= 'Z'))
                    o.Append(char.ToUpper(ch));
            return o.ToString();
        }

        public Dictionary<string,int> MakeInverseMap() {
            Dictionary<string,int> rt = new Dictionary<string,int>();
            for (int i = 0; i < values.Length; i++) {
                if (values[i] == "" || values[i][values[i].Length-1] == '#')
                    continue;
                string shrt = Loosen(values[i]);
                if (codepoints[i+1] - codepoints[i] != 1 ||
                        rt.ContainsKey(shrt)) {
                    //Console.WriteLine("duplicate {0}", values[i]);
                } else
                    rt[shrt] = codepoints[i];
            }

            return rt;
        }

        // Hangul decomposition constants
        const int SBASE  = 0xAC00;
        const int LBASE  = 0x1100;
        const int VBASE  = 0x1161;
        const int TBASE  = 0x11A7;
        const int SCOUNT = 11172;
        const int LCOUNT = 19;
        const int VCOUNT = 21;
        const int TCOUNT = 28;

        const int SEND   = SBASE + SCOUNT;
        const int NCOUNT = VCOUNT * TCOUNT;

        string MapValue(int cp, string raw) {
            if (raw == "\uD800")
                return "";
            if (raw == "\uD801")
                return Utils.Chr(cp);
            if (is_na && raw.Length != 0 && raw[raw.Length - 1] == '#')
                return raw.Substring(0, raw.Length - 1) +
                    string.Format("{0:X4}", cp);
            if ((is_na || is_dm) && cp >= SBASE && cp < SEND) {
                int syl_ix = cp - SBASE;
                int l = LBASE + (syl_ix / NCOUNT);
                int v = VBASE + (syl_ix % NCOUNT) / TCOUNT;
                int t = TBASE + (syl_ix % TCOUNT);

                if (is_na) {
                    StringBuilder sb = new StringBuilder();
                    sb.Append("HANGUL SYLLABLE ");
                    sb.Append(jsn.GetValue(l));
                    sb.Append(jsn.GetValue(v));
                    if (t != TBASE) sb.Append(jsn.GetValue(t));
                    return sb.ToString();
                } else {
                    char[] res = new char[2];
                    // No supplementary characters ever need to be used here
                    if (t == TBASE) {
                        res[0] = (char)l;
                        res[1] = (char)v;
                    } else {
                        res[0] = (char)(SBASE + (l - LBASE) * NCOUNT + (v - VBASE) * TCOUNT);
                        res[1] = (char)t;
                    }
                    return new string(res);
                }
            }
            return raw;
        }


        int MatchJSN(int min, int ct, string into, ref int at) {
            int blen = -1;
            int best = -1;
            for (int code = min; code < min+ct; code++) {
                string name = jsn.GetValue(code);
                if (name.Length + at <= into.Length &&
                        into.Substring(at, name.Length) == name &&
                        name.Length > blen) {
                    blen = name.Length;
                    best = code - min;
                }
            }
            at += blen;
            return best;
        }
        public string ParseHangul(string from, int pos) {
            int l = MatchJSN(LBASE, LCOUNT, from, ref pos);
            int v = MatchJSN(VBASE, VCOUNT, from, ref pos);
            int t = MatchJSN(TBASE, TCOUNT, from, ref pos);
            if (pos != from.Length || l < 0 || v < 0 || t < 0)
                return null;
            return Utils.Chr(SBASE + l * NCOUNT + v * TCOUNT + t);
        }

        bool IsVariable(int cpl, int cph, string raw) {
            if (is_na && raw.Length != 0 && raw[raw.Length - 1] == '#')
                return true; // CJK UNIFIED IDEOGRAPH-#
            if (raw == "\uD801")
                return true;
            if ((is_na || is_dm) && cpl < SEND && cph >= SBASE)
                return true; // Hangul procedural generation
            return false;
        }

        public override string GetValue(int cp) {
            int lix = 0;
            int hix = codepoints.Length;

            while (true) {
                if ((hix - lix) <= 1) {
                    return MapValue(cp, values[lix]);
                }
                int mix = (lix + hix) / 2;
                if (cp >= codepoints[mix]) {
                    lix = mix;
                } else {
                    hix = mix;
                }
            }
        }

        public override int[] GetRanges(Variable filter) {
            List<int> res = new List<int>();
            for (int i = 0; i < values.Length; i ++) {
                int upto = (i+1 == codepoints.Length) ? 0x110000 : codepoints[i+1];
                if (IsVariable(codepoints[i], upto, values[i])) {
                    for (int cp = codepoints[i]; cp < upto; cp++) {
                        if (DoMatch(MapValue(cp, values[i]), filter)) {
                            if (res.Count > 0 && res[res.Count-1] == cp) {
                                res[res.Count-1] = cp+1;
                            } else {
                                res.Add(cp);
                                res.Add(cp+1);
                            }
                        }
                    }
                } else {
                    if (DoMatch(MapValue(codepoints[i], values[i]), filter)) {
                        if (res.Count > 0 && res[res.Count-1] == codepoints[i]) {
                            res[res.Count-1] = upto;
                        } else {
                            res.Add(codepoints[i]);
                            res.Add(upto);
                        }
                    }
                }
            }

            return res.ToArray();
        }
    }

    static class DataSet {
        static Dictionary<string,object> cache;
        static byte[] bits;
        static Dictionary<string,int[]> directory;
        static Dictionary<string,string> aliases;
        static Dictionary<string,string> proxy_aliases;
        static Dictionary<Prod<string,string>,string[]> val_aliases;
        static string[] tokens;
        static bool Trace;

        const int FILES = 4;
        static int Int(ref int from) {
            from += 4;
            return (bits[from-4] << 24) | (bits[from-3] << 16) |
                (bits[from-2] << 8) | (bits[from-1]);
        }

        static int Short(ref int from) {
            from += 2;
            return (bits[from-2] << 8) | (bits[from-1]);
        }

        static uint BER(ref int from) {
            uint buf = 0;
            while (true) {
                byte inp = bits[from++];
                buf = (buf << 7) | (uint)(inp & 127);
                if ((inp & 128) == 0)
                    return buf;
            }
        }

        static string AsciiZ(ref int from) {
            int to = from;
            while (bits[to] != 0) to++;
            char[] buf = new char[to - from];
            for (int i = from; i < to; i++)
                buf[i - from] = (char)bits[i];
            from = to+1;
            return new string(buf);
        }

        static void InflateDirectory() {
            directory = new Dictionary<string,int[]>();
            Trace = Environment.GetEnvironmentVariable("NIECZA_UCD_TRACE") != null;
            if (Trace) Console.WriteLine("Unpacking directory ...");

            int rpos = 0;

            int[] fstart = new int[FILES];
            for (int i = 0; i < FILES; i++)
                fstart[i] = (i == 0 ? 20 : fstart[i-1]) + Int(ref rpos);
            rpos += 4; // skip the extra length

            int dend = fstart[0];

            while (rpos < dend) {
                int rpos0 = rpos;
                int[] loc = new int[FILES * 2];
                string name = AsciiZ(ref rpos);
                int nfiles = bits[rpos++];
                for (int i = 0; i < nfiles; i++) {
                    loc[2*i] = fstart[i];
                    fstart[i] += Int(ref rpos);
                    loc[2*i+1] = fstart[i];
                }
                if (Trace)
                    Console.WriteLine("Entry {0} (d.e. {1}): {2}",
                            name, rpos0, Kernel.JoinS(", ", loc));
                directory[name] = loc;
            }
            if (Trace) Console.WriteLine("done.");
        }

        static void InflateAliases() {
            int[] loc = directory["!PropertyAlias"];
            aliases = new Dictionary<string, string>();
            val_aliases = new Dictionary<Prod<string,string>,string[]>();
            proxy_aliases = new Dictionary<string,string>();

            int rpos = loc[2];
            while (rpos < loc[3]) {
                string main = AsciiZ(ref rpos);
                string alias;
                aliases[StringProperty.Loosen(main)] = main;
                while ((alias = AsciiZ(ref rpos)).Length != 0) {
                    aliases[StringProperty.Loosen(alias)] = main;
                    //if (Trace) Console.WriteLine("Alias {0} -> {1}", alias, main);
                }
            }
            aliases["SCRIPTEXTENSIONS"] = aliases["SCX"] = "scx";

            loc = directory["!pva"];
            rpos = loc[2];
            List<string> aset = new List<string>();
            while (rpos < loc[3]) {
                string tbl = AsciiZ(ref rpos);
                string canon = AsciiZ(ref rpos);
                if (canon == "n/a")
                    canon = AsciiZ(ref rpos);
                string alias;
                aset.Add(canon);
                while ((alias = AsciiZ(ref rpos)).Length != 0)
                    aset.Add(alias);
                //if (Trace) Console.WriteLine("Alias {0},{1} -> {2}", tbl, canon, Kernel.JoinS(", ", aset));
                val_aliases[Prod.C(tbl, canon)] = aset.ToArray();

                if (tbl == "sc" || tbl == "gc" || tbl == "blk") {
                    foreach (string a in aset)
                        aliases[(tbl == "blk" ? "IN" : "") +
                            StringProperty.Loosen(a)] = tbl + "+" + canon;
                    proxy_aliases[tbl+"+"+canon] = tbl == "sc" ? "Script" : tbl;
                }
                aset.Clear();
            }
        }

        static void InflateTokens() {
            if (tokens != null) return;
            int[] loc = directory["!name_tokens"];
            int rpos = loc[2];
            List<string> tks = new List<string>();
            while (rpos < loc[3])
                tks.Add(AsciiZ(ref rpos));
            tokens = tks.ToArray();
            if (Trace) Console.WriteLine("Inflated {0} name tokens", tokens.Length);
        }

        static object InflateBinary(string name, int[] loc) {
            List<int> vec = new List<int>();
            int rpos = loc[2];
            int last = 0;
            int ntyp = 1;
            while (rpos < loc[3]) {
                last += (int)BER(ref rpos);
                vec.Add(last);
                vec.Add(ntyp);
                ntyp = 1 - ntyp;
            }
            return new LimitedProperty(vec.ToArray(), new string[][] {
                    val_aliases[Prod.C(name,"N")],
                    val_aliases[Prod.C(name,"Y")] });
        }

        static object InflateEnum(string name, int[] loc) {
            List<string[]> names = new List<string[]>();
            int rpos2 = loc[6];
            while (rpos2 < loc[7]) {
                string rname = AsciiZ(ref rpos2);
                string[] keys;
                if (!val_aliases.TryGetValue(Prod.C(name, rname), out keys))
                    keys = new string[] { rname };
                names.Add(keys);
            }
            int rpos0 = loc[2];
            int rpos1 = loc[4];
            int[] data = new int[(loc[5] - loc[4]) * 2];
            int lix = 0;
            for (int i = 0; i < data.Length / 2; i++) {
                lix += (int)BER(ref rpos0);
                data[2*i] = lix;
                data[2*i+1] = bits[rpos1++];
                //if (Trace) Console.WriteLine("inflate: {0} = {1}", data[2*i], names[data[2*i+1]][0]);
            }

            return new LimitedProperty(data, names.ToArray());
        }

        static object InflateString(string name, int[] loc) {
            int rpos1 = loc[2];
            int rpos2 = loc[4];

            List<int> codes = new List<int>();
            List<string> strs = new List<string>();

            while (rpos1 < loc[3]) {
                int code = Int(ref rpos1);
                int len = code >> 24;
                int cp = code & 0xFFFFF;
                char[] buf = new char[len];
                if ((code & (1 << 23)) != 0) {
                    for (int i = 0; i < len; i++)
                        buf[i] = (char)Short(ref rpos2);
                    codes.Add(cp);
                    strs.Add(new string(buf));
                } else {
                    int rpost = rpos1;
                    int nextcp = Int(ref rpost) & 0xFFFFF;
                    while (cp < nextcp) {
                        for (int i = 0; i < len; i++)
                            buf[i] = (char)Short(ref rpos2);
                        codes.Add(cp++);
                        strs.Add(new string(buf));
                    }
                }
            }

            return new StringProperty(name, codes.ToArray(), strs.ToArray());
        }

        static object InflateName(string name, int[] loc) {
            InflateTokens();
            List<int> cps = new List<int>();
            List<string> names = new List<string>();
            int cp = 0;
            int rpos = loc[2];
            int[] buf = new int[16];
            StringBuilder nbuf = new StringBuilder();
            int bcnt = 0;
            while (rpos < loc[3]) {
                if (bits[rpos] == 255) {
                    rpos++;
                    cp += (int)BER(ref rpos);
                }
                byte tcode = bits[rpos++];
                bcnt = (tcode >> 4);
                while (bcnt < (tcode & 15)) {
                    buf[bcnt++] = (int)BER(ref rpos);
                }
                nbuf.Length = 0;
                for (int i = 0; i < bcnt; i++)
                    nbuf.Append(tokens[buf[i]-1]).Append(' ');
                if (bcnt != 0) nbuf.Length--;
                cps.Add(cp);
                names.Add(nbuf.ToString());
                cp++;
            }

            return new StringProperty(name, cps.ToArray(), names.ToArray());
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public static object GetTable(string name) {
            if (cache == null)
                cache = new Dictionary<string,object>();
            if (bits == null) {
                Stream unidata = Assembly.GetExecutingAssembly().
                    GetManifestResourceStream("unidata");
                bits = new byte[unidata.Length];
                unidata.Read(bits, 0, bits.Length);
                InflateDirectory();
                InflateAliases();
            }

            if (name != "" && name.Substring(0,1) != "!")
                name = StringProperty.Loosen(name);

            object r;
            string a;
            if (aliases.TryGetValue(name, out a))
                name = a;
            if (cache.TryGetValue(name, out r))
                return r;

            if (proxy_aliases.TryGetValue(name, out a))
                return cache[name] = ((LimitedProperty) GetTable(a)).Proxify(false, name);

            if (name == "ASSIGNED")
                return cache[name] = ((LimitedProperty) GetTable("gc"))
                    .Proxify(true, "+Cn");
            if (name == "ANY")
                return cache[name] = ((LimitedProperty) GetTable("gc"))
                    .Proxify(false, "+");
            if (name == "ASCII")
                return cache[name] = new LimitedProperty(
                    new int[] { 0, 1, 128, 0 },
                    new string[][] { LimitedProperty.NO, LimitedProperty.YES });

            if (name == "!inverse_name") {
                var inv = (GetTable("na") as StringProperty).MakeInverseMap();
                var na1 = GetTable("na1") as StringProperty;
                for (int i = 0; i < 32; i++)
                    inv[StringProperty.Loosen(na1.GetValue(i))] = i;
                for (int i = 128; i < 160; i++)
                    inv[StringProperty.Loosen(na1.GetValue(i))] = i;
                // semi-ad-hoc aliases used in spectests
                // UTS #28 2.5 recommends to add "aliases", but leaves
                // the specific aliases up to the regex impl ...
                inv["LF"] = 10;
                inv["CR"] = 13;
                inv["FF"] = 12;
                inv["NEL"] = 0x85;

                return cache[name] = inv;
            }

            int[] loc;
            if (!directory.TryGetValue(name, out loc))
                throw new NieczaException(name + " does not exist as a UCD table");

            switch (bits[loc[0]]) {
                case (byte)'B':
                    r = InflateBinary(name, loc);
                    break;
                case (byte)'E':
                    r = InflateEnum(name, loc);
                    break;
                case (byte)'N':
                    r = InflateName(name, loc);
                    break;
                case (byte)'S':
                    r = InflateString(name, loc);
                    break;
                default:
                    throw new NieczaException("Unhandled type code " + (char)bits[loc[0]]);
            }

            cache[name] = r;
            return r;
        }

        static bool HanCodepoint(string name, string prefix, ref int cp) {
            if (name.Length < prefix.Length + 4)
                return false;
            if (name.Length > prefix.Length + 6)
                return false;
            if (name.Substring(0,prefix.Length) != prefix)
                return false;
            int ctr = 0;
            for (int i = prefix.Length; i < name.Length; i++) {
                ctr <<= 4;
                if (name[i] >= 'A' && name[i] <= 'F')
                    ctr += ((int)name[i]) - ((int)'A') + 10;
                else if (name[i] >= '0' && name[i] <= '9')
                    ctr += ((int)name[i]) - ((int)'0');
                else
                    return false;
            }
            cp = ctr;
            return true;
        }

        public static string GetCodepoint(string name) {
            var fmap = (StringProperty) GetTable("na");
            var imap = (Dictionary<string,int>) GetTable("!inverse_name");
            string loose = StringProperty.Loosen(name);
            bool dash = name.IndexOf('-') >= 0;

            if (loose == "HANGULJUNGSEONGOE")
                return dash ? "\u1180" : "\u116C";
            if (loose == "TIBETANSUBJOINEDLETTERA")
                return dash ? "\u0FB0" : "\u0FB8";
            if (loose == "TIBETANLETTERA")
                return dash ? "\u0F60" : "\u0F68";

            int codep = 0;
            if (HanCodepoint(loose, "CJKCOMPATIBILITYIDEOGRAPH", ref codep) ||
                    HanCodepoint(loose, "CJKUNIFIEDIDEOGRAPH", ref codep)) {
                if (StringProperty.Loosen(fmap.GetValue(codep)) == loose)
                    return Utils.Chr(codep);
            }

            if (loose.Length >= 14 &&
                    loose.Substring(0,14) == "HANGULSYLLABLE") {
                return fmap.ParseHangul(loose, 14);
            }

            if (imap.ContainsKey(loose))
                return Utils.Chr(imap[loose]);

            throw new NieczaException("Unrecognized character name " + name);
        }

        public static object CompileCClass(Variable pair) {
            P6any pairo = pair.Fetch();
            Variable tbl = (Variable)pairo.GetSlot(Compartment.Top.EnumMO, "$!key");
            Variable sm =  (Variable)pairo.GetSlot(Compartment.Top.EnumMO, "$!value");
            Property p = (Property)DataSet.GetTable(
                    tbl.Fetch().mo.mro_raw_Str.Get(tbl));
            int[] rranges = p.GetRanges(sm);
            object[] cranges = new object[rranges.Length * 2];
            for (int i = 0; i < rranges.Length; i++) {
                cranges[2*i] = rranges[i];
                cranges[2*i+1] = ((i & 1) != 0) ? 0 : 0x3FFFFFFF;
            }
            return cranges;
        }
    }
}

public partial class Builtins {
    [ImplicitConsts] public static Variable ucd_get_ranges(Constants c, Variable tbl, Variable sm) {
        Property p = (Property)DataSet.GetTable(
                tbl.Fetch().mo.mro_raw_Str.Get(tbl));
        int[] rranges = p.GetRanges(sm);
        Variable[] cranges = new Variable[rranges.Length];
        for (int i = 0; i < rranges.Length; i++)
            cranges[i] = c.setting.MakeInt(rranges[i]);
        return c.setting.MakeParcel(cranges);
    }

    [ImplicitConsts] public static Variable ucd_get_value(Constants c, Variable tbl, Variable ch) {
        Property p = (Property)DataSet.GetTable(
                tbl.Fetch().mo.mro_raw_Str.Get(tbl));
        return c.setting.MakeStr(p.GetValue(
                (int) ch.Fetch().mo.mro_raw_Numeric.Get(ch)));
    }

    [ImplicitConsts] public static Variable ucd_get_codepoint(Constants c, Variable ch) {
        return c.setting.MakeStr(DataSet.GetCodepoint(
            ch.Fetch().mo.mro_raw_Str.Get(ch)));
    }

    public static string ucd_titlecase(Variable ch) {
        Property p = (Property) DataSet.GetTable("tc");
        return p.GetValue((int) ch.Fetch().mo.mro_raw_Numeric.Get(ch));
    }
}
