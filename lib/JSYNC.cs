using Niecza;
using System;
using System.Collections.Generic;
using System.Text;

public class JsyncWriter {
    internal static bool FailSoft =
        Environment.GetEnvironmentVariable("NIECZA_JSYNC_WRITER_FAILSOFT") != null;
    StringBuilder o = new StringBuilder();
    Dictionary<object,int> anchors = new Dictionary<object,int>();
    int nextanchor = 0;

    bool contUsed = false;
    bool headerized = false;

    void ScalarCheck() {
        if (!contUsed) {
            contUsed = headerized = true;
            o.Append("[{\"%JSYNC\":\"1.0\"},");
        }
    }

    void WriteObj(P6any obj) {
        int anchor;
        if (anchors.TryGetValue(obj, out anchor)) {
            WriteAnchor(anchor);
        } else if (!obj.IsDefined()) {
            WriteNull();
        } else if (obj.Isa(Kernel.ListMO)) {
            WriteArray(obj);
        } else if (obj.Isa(Kernel.HashMO)) {
            WriteHash(obj);
        } else if (obj.Isa(Kernel.BoolMO)) {
            WriteBool(Kernel.UnboxAny<bool>(obj));
        } else if (obj.Isa(Kernel.StrMO)) {
            WriteStr(true, Kernel.UnboxAny<string>(obj));
        } else if (obj.Isa(Kernel.NumMO)) {
            WriteNum(Kernel.UnboxAny<double>(obj));
        } else {
            WriteGeneral(obj);
        }
    }

    void WriteNull() {
        ScalarCheck();
        o.Append("null");
    }

    void WriteArray(P6any obj) {
        int a = nextanchor++;
        anchors[obj] = a;
        Kernel.RunInferior(obj.InvokeMethod(Kernel.GetInferiorRoot(), "eager",
                new Variable[] { Kernel.NewROScalar(obj) }, null));

        o.AppendFormat("[\"&A{0}\"", a);
        contUsed = true;
        VarDeque vd = (VarDeque) obj.GetSlot("items");
        for (int i = 0; i < vd.Count(); i++) {
            o.Append(',');
            WriteObj(vd[i].Fetch());
        }
        o.Append(']');
    }

    void WriteHash(P6any obj) {
        int a = nextanchor++;
        anchors[obj] = a;
        VarHash entries =
            Kernel.UnboxAny<VarHash>(obj);
        o.Append('{');
        contUsed = true;
        o.AppendFormat("\"&\":\"A{0}\"", a);
        if (obj.mo != Kernel.HashMO) {
            if (obj.mo.nslots != 0)
                throw new NieczaException("Cannot serialize subclasses of Hash  that add attributes");
            o.Append(",\"!\":");
            WriteStr(true, "!perl6/" + obj.mo.name);
        }
        List<string> keys = new List<string>(entries.Keys);
        keys.Sort();
        foreach (string key in keys) {
            o.Append(',');

            // no object keys in hashes yet
            WriteStr(true, key);
            o.Append(':');
            WriteObj(entries[key].Fetch());
        }
        o.Append('}');
    }

    void WriteGeneral(P6any obj) {
        if (FailSoft && !(obj is P6opaque)) {
            o.AppendFormat("\"UNSERIALIZABLE {0}\"", obj.mo.name);
            return;
        }

        int a = nextanchor++;
        anchors[obj] = a;
        P6opaque dyo = (P6opaque) obj;
        STable mo = dyo.mo;

        o.Append('{');
        contUsed = true;
        o.AppendFormat("\"&\":\"A{0}\",\"!\":", a);
        WriteStr(true, "!perl6/" + mo.name);

        for (int i = 0; i < mo.nslots; i++) {
            o.Append(',');
            WriteStr(true, mo.all_slot[i]);
            o.Append(':');
            WriteObj(((Variable)dyo.slots[i]).Fetch());
        }

        o.Append('}');
    }

    bool NeedsEscape(string s) {
        foreach (char ch in s) {
            if (ch == '*' || ch == '%' || ch == '&' || ch == '!')
                return true;
            if (ch != '.')
                return false;
        }
        return false;
    }

    void WriteStr(bool esc, string s) {
        ScalarCheck();
        o.Append('"');
        if (esc && NeedsEscape(s))
            o.Append('.');
        AddStringContents(o, s);
        o.Append('"');
    }

    internal static void AddStringContents(StringBuilder sb, string s) {
        foreach (char ch in s) {
            switch(ch) {
                case '\\':
                    sb.Append('\\');
                    goto default;
                case '\"':
                    sb.Append('\\');
                    goto default;
                default:
                    if ((ch & 0xFF7F) < 32) {
                        sb.AppendFormat("\\u{0:X4}", (int)ch);
                    } else {
                        sb.Append(ch);
                    }
                    break;
                case '\b':
                    sb.Append('\\');
                    sb.Append('b');
                    break;
                case '\f':
                    sb.Append('\\');
                    sb.Append('f');
                    break;
                case '\t':
                    sb.Append('\\');
                    sb.Append('t');
                    break;
                case '\r':
                    sb.Append('\\');
                    sb.Append('r');
                    break;
                case '\n':
                    sb.Append('\\');
                    sb.Append('n');
                    break;
            }
        }
    }

    void WriteBool(bool x) {
        ScalarCheck();
        o.Append(x ? "true" : "false");
    }

    void WriteNum(double x) {
        ScalarCheck();
        o.Append(x);
    }

    void WriteAnchor(int i) {
        ScalarCheck();
        o.AppendFormat("\"*A{0}\"", i);
    }

    public static string ToJsync(P6any obj) {
        JsyncWriter w = new JsyncWriter();
        w.WriteObj(obj);
        if (w.headerized) w.o.Append(']');
        return w.o.ToString();
    }
}

public class JsyncReader {
    string from;
    int ix = 0;
    Dictionary<string,P6any> anchors = new Dictionary<string,P6any>();
    Dictionary<string,List<Variable>> anchorrefs =
        new Dictionary<string,List<Variable>>();

    string s_tag;
    string s_anchor;
    string s_content;
    int s_content_type;

    const int NONE = 0;
    const int SCALAR = 1;
    const int ALIAS = 2;
    const int DIRECTIVE = 3;

    // most parsers are expected to skip white before
    char SkipWhite(bool more) {
        while (ix != from.Length) {
            char ch = from[ix];
            if (ch == ' ' || ch == '\r' || ch == '\t' || ch == '\n')
                ix++;
            else
                break;
        }
        if (more) {
            if (ix == from.Length)
                Err("Unexpected end of text");
            return from[ix];
        } else { return '\0'; }
    }

    void Err(string s) {
        throw new NieczaException(string.Format("{0} in JSYNC string at position {1}", s, ix));
    }

    void SkipToken(string s) {
        if (ix + s.Length > from.Length || from.Substring(ix, s.Length) != s)
            Err("Incomplete match of " + s);
        ix += s.Length;
    }

    void SkipCharWS(char c) {
        if (SkipWhite(true) != c)
            Err("Missing token " + c);
        ix++;
    }

    void SkipChar(char c) {
        if (from[ix] != c)
            Err("Missing token " + c);
        ix++;
    }

    static Variable BoxRW<T>(T o, STable mo) {
        P6opaque dyo = new BoxObject<T>(o, mo);
        return Kernel.NewRWScalar(Kernel.AnyMO, dyo);
    }

    Variable GetObj() {
        switch(SkipWhite(true)) {
            case '{':
                return GetFromHash();
            case '[':
                return GetFromArray();
            case '"':
                GetString();
                return GetFromString();
            case 'n':
                SkipToken("null");
                return Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP);
            case 't':
                SkipToken("true");
                return BoxRW<bool>(true, Kernel.BoolMO);
            case 'f':
                SkipToken("false");
                return BoxRW<bool>(false, Kernel.BoolMO);
            default:
                return GetFromNumber();
        }
    }

    string GetJsonString() {
        SkipCharWS('"');

        StringBuilder sb = new StringBuilder();
        while (true) {
            if (ix == from.Length)
                Err("Unterminated string");
            char ch = from[ix++];
            if (ch == '"')
                return sb.ToString();
            if ((ch & 0xFF7F) >= 32 && ch != '\\') {
                sb.Append(ch);
                continue;
            }

            if (ix == from.Length)
                Err("Unexpected end of input in backslash escape");
            ch = from[ix++];
            switch(ch) {
                case '/': break;
                case '\\': break;
                case '"': break;
                case 'b': ch = '\b'; break;
                case 'n': ch = '\n'; break;
                case 'r': ch = '\r'; break;
                case 't': ch = '\t'; break;
                case 'f': ch = '\f'; break;
                case 'u':
                    if (ix + 4 > from.Length)
                        Err("Unexpected end of input in hex escape");
                    ch = '\0';
                    for (int i = 0; i < 4; i++) {
                        ch = (char) (((int)ch) << 4);
                        char d = from[ix+i];
                        if (d >= '0' && d <= '9')
                            ch += (char)(d - '0');
                        else if (d >= 'a' && d <= 'f')
                            ch += (char)(d - 'a' + 10);
                        else if (d >= 'A' && d <= 'F')
                            ch += (char)(d - 'A' + 10);
                        else
                            Err("Invalid hex character");
                    }
                    ix += 4;
                    break;
                default:
                    Err("Invalid backslash escape");
                    break;
            }
            sb.Append(ch);
        }
    }

    Variable GetFromJson(bool top_level) {
        char look = SkipWhite(true);
        if (look == '[') {
            VarDeque q = new VarDeque();
            SkipChar('[');
            while (true) {
                look = SkipWhite(true);
                if (look == ']')
                    break;
                if (q.Count() != 0)
                    SkipChar(',');
                q.Push(GetFromJson(false));
            }
            SkipWhite(true);
            SkipChar(']');
            P6any i = new P6opaque(Kernel.ArrayMO);
            i.SetSlot("items", q);
            i.SetSlot("rest", new VarDeque());
            return Kernel.NewROScalar(i);
        } else if (look == '{') {
            VarHash q = new VarHash();
            int ct = 0;
            SkipChar('{');
            while (true) {
                look = SkipWhite(true);
                if (look == '}')
                    break;
                if (ct != 0)
                    SkipCharWS(',');
                ct++;
                string key = GetJsonString();
                SkipWhite(true);
                SkipChar(':');
                q[key] = GetFromJson(false);
            }
            SkipWhite(true);
            SkipChar('}');
            return BoxRW<VarHash>(q, Kernel.HashMO);
        } else if (top_level) {
            Err("Top-level scalar found");
            return null;
        } else if (look == '"') {
            return BoxRW<string>(GetJsonString(), Kernel.StrMO);
        } else if (look == 'n') {
            SkipToken("null");
            return Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP);
        } else if (look == 't') {
            SkipToken("true");
            return BoxRW<bool>(true, Kernel.BoolMO);
        } else if (look == 'f') {
            SkipToken("false");
            return BoxRW<bool>(false, Kernel.BoolMO);
        } else {
            double d;
            string tx = GetJsonNumber();
            if (!double.TryParse(tx, out d))
                Err("Unparsable number " + tx);
            return BoxRW<double>(d, Kernel.NumMO);
        }
    }

    void AddAnchor(string anch, P6any obj) {
        for (int i = 0; i < anch.Length; i++) {
            char c = anch[i];
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) continue;
            if (i != 0 && c >= '0' && c <= '9') continue;
            Err("Invalid character " + c + " in anchor " + anch);
        }
        if (anchors.ContainsKey(anch))
            Err("Duplicate anchor " + anch);
        anchors[anch] = obj;
    }

    string GetPiece(char tag, string block, ref int offs) {
        if (offs >= block.Length)
            return null;
        if (block[offs] != tag)
            return null;
        int noffs = offs;
        while (noffs != block.Length && block[noffs] != ' ')
            noffs++;
        string ret = block.Substring(offs+1, noffs-offs-1);
        // this will put offs = length + 1 if there is no trailing
        // space, which is used as a semipredicate
        offs = noffs + 1;
        return ret;
    }

    void GetString() {
        string s = GetJsonString();
        int offs = 0;
        s_tag = GetPiece('!', s, ref offs);
        s_anchor = GetPiece('&', s, ref offs);

        //if (s_tag != null) {
        //    int x = s_tag.IndexOf('!');
        //    if (x >= 0 && s_tag != "!") {
        //        string ns = s_tag.Substring(0, x);
        //        string pfx;
        //        if (!tagAliases.TryGetValue(ns, out pfx))
        //            Err("Undefined tag alias namespace " + ns);
        //        s_tag = pfx + s_tag.Substring(x+1);
        //    }
        //}

        if (offs == s.Length + 1) {
            s_content_type = NONE;
            s_content = null;
            return;
        }

        char first = (offs == s.Length) ? '\0' : s[offs];

        s_content_type = SCALAR;
        if (first == '%') {
            s_content_type = DIRECTIVE;
            offs++;
        } else if (first == '*') {
            s_content_type = ALIAS;
            offs++;
        } else if (first == '.') {
            int noffs = offs;
            while (noffs != s.Length && s[noffs] == '.') noffs++;
            if (noffs != s.Length) {
                first = s[noffs];
                if (first == '!' || first == '&' || first == '*' || first == '%')
                    offs++;
            }
        }

        s_content = s.Substring(offs, s.Length - offs);

        if (s_content_type != SCALAR && (s_tag != null || s_anchor != null))
            Err("Tags and anchors cannot be used with directives or aliases");
    }

    Variable CreateAlias(string name) {
        List<Variable> lv;
        if (!anchorrefs.TryGetValue(name, out lv))
            anchorrefs[name] = lv = new List<Variable>();
        Variable n = Kernel.NewRWScalar(Kernel.AnyMO, Kernel.AnyP);
        lv.Add(n);
        return n;
    }

    Variable ParseScalar() {
        if (s_tag == null) {
            return BoxRW<string>(s_content, Kernel.StrMO);
        } else if (s_tag == "Num") {
            double r;
            if (!double.TryParse(s_content, out r))
                Err("Num format error");
            return BoxRW<double>(r, Kernel.NumMO);
        } else {
            Err("Unhandled scalar tag " + s_tag);
            return null;
        }
    }

    Variable GetFromString() {
        if (s_content_type == ALIAS)
            return CreateAlias(s_content);
        if (s_content_type != SCALAR)
            Err("Found directive or sequence mark where scalar expected");

        Variable obj = ParseScalar();
        if (s_anchor != null) AddAnchor(s_anchor, obj.Fetch());
        return obj;
    }

    Variable GetFromArray() {
        SkipCharWS('[');
        VarDeque kids = new VarDeque();
        P6opaque obj = new P6opaque(Kernel.ArrayMO);
        obj.SetSlot("items", kids);
        obj.SetSlot("rest",  new VarDeque());
        bool comma = false;
        string a_tag = null;

        if (SkipWhite(true) == '"') {
            GetString();
            comma = true;
        }

        if (comma) {
            if (s_content_type == NONE) {
                a_tag = s_tag;
                if (s_anchor != null) AddAnchor(s_anchor, obj);
            } else {
                kids.Push(GetFromString());
            }
        }

        while(true) {
            if (SkipWhite(true) == ']') break;
            if (comma) {
                SkipChar(',');
            }
            kids.Push(GetObj());
            comma = true;
        }
        SkipCharWS(']');
        if (a_tag != null)
            Err("Typed arrays are NYI in Niecza Perl 6");
        return Kernel.NewRWScalar(Kernel.AnyMO, obj);
    }

    string GetSimpleStringValue() {
        SkipCharWS(':');
        if (SkipWhite(true) != '"')
            Err("Expected a simple scalar");
        GetString();
        if (s_content_type != SCALAR || s_tag != null || s_anchor != null)
            Err("Expected a simple scalar");
        return s_content;
    }

    Variable GetFromHash() {
        SkipCharWS('{');
        // we can't make any assumptions about ordering here, as JSON
        // emitters can blindly reorder hashes
        string h_tag = null;
        string h_anchor = null;
        Dictionary<string,string> h_key_ind = null;
        VarHash h_val_ind = null;
        VarHash zyg = new VarHash();
        bool comma = false;

        while(true) {
            if (SkipWhite(true) == '}') break;
            if (comma) {
                SkipChar(',');
            }
            comma = true;
            SkipWhite(false);
            GetString();
            if (s_content_type == NONE && s_anchor == null && s_tag == "") {
                if (h_tag != null)
                    Err("Tag specified twice");
                h_tag = GetSimpleStringValue();
            } else if (s_content_type == NONE && s_tag == null && s_anchor == "") {
                if (h_anchor != null)
                    Err("Anchor specified twice");
                h_anchor = GetSimpleStringValue();
            } else if (s_content_type == NONE) {
                if (s_anchor == null || s_tag != null)
                    Err("Invalid hash key form");
                string k1 = s_anchor;
                if (h_key_ind == null) h_key_ind = new Dictionary<string,string>();
                if (h_key_ind.ContainsKey(k1))
                    Err("Key alias &" + k1 + " specified twice");
                SkipCharWS(':');
                if (SkipWhite(true) != '"')
                    Err("Non-scalar hash keys NYI in Niecza Perl 6");
                GetString();
                if (s_tag != null || s_anchor != null || s_content_type != SCALAR)
                    Err("Typed hash keys NYI in Niecza Perl 6");
                h_key_ind[k1] = s_content;
            } else if (s_content_type == ALIAS) {
                string k1 = s_content;
                if (h_val_ind == null) h_val_ind = new VarHash();
                if (h_val_ind.ContainsKey(k1))
                    Err("Key alias *" + k1 + " used twice");
                SkipCharWS(':');
                h_val_ind[k1] = GetObj();
            } else if (s_content_type == DIRECTIVE) {
                Err("Got directive in hash key position");
            } else {
                if (s_tag != null || s_anchor != null)
                    Err("Typed hash keys NYI in Niecza Perl 6");
                string k1 = s_content;
                SkipCharWS(':');
                zyg[k1] = GetObj();
            }
        }

        SkipChar('}');

        if (h_key_ind != null || h_val_ind != null) {
            h_key_ind = h_key_ind ?? new Dictionary<string,string>();
            h_val_ind = h_val_ind ?? new VarHash();

            foreach (KeyValuePair<string,string> kv in h_key_ind) {
                if (!h_val_ind.ContainsKey(kv.Key))
                    Err("No value provided for indirect key *" + kv.Key);
                Variable val = h_val_ind[kv.Key];
                h_val_ind.Remove(kv.Key);
                if (zyg.ContainsKey(kv.Value))
                    Err("Indirect key &" + kv.Value + " collides with non-indirect key");
                zyg[kv.Value] = val;
            }

            foreach (string k in h_val_ind.Keys) {
                Err("Indirect key &" + k + " is unused");
            }
        }

        Variable obj;
        if (h_tag != null) {
            if (!Utils.StartsWithInvariant("!perl6/", h_tag))
                Err("Unsupported hash tag " + h_tag);
            int s_cursor = 7;
            P6any p_cursor = Kernel.GlobalO;
            while(s_cursor < h_tag.Length) {
                int s_next = h_tag.IndexOf("::", s_cursor);
                if (s_next < 0) s_next = h_tag.Length;
                else s_next = s_next + 2;
                string frag = h_tag.Substring(s_cursor, s_next - s_cursor);
                s_cursor = s_next;

                p_cursor = Kernel.PackageLookup(p_cursor, frag).v.Fetch();
            }

            if (p_cursor.Isa(Kernel.HashMO)) {
                if (p_cursor.mo.nslots != 0)
                    Err("Cannot thaw Hash subclass " + p_cursor.mo.name + "; it has attributes");
                obj = BoxRW<VarHash>(zyg, p_cursor.mo);
            } else {
                P6opaque dyo = new P6opaque(p_cursor.mo);
                for (int i = 0; i < dyo.mo.nslots; i++) {
                    string sn = dyo.mo.all_slot[i];
                    if (!zyg.ContainsKey(sn))
                        Err("No value for attribute " + sn + " in thawed value of class " + dyo.mo.name);
                    dyo.slots[i] = zyg[sn];
                    zyg.Remove(sn);
                }
                foreach (string key in zyg.Keys) {
                    Err("Attribute " + key + " not present in " + dyo.mo.name);
                }
                obj = Kernel.NewRWScalar(Kernel.AnyMO, dyo);
            }
        } else {
            obj = BoxRW<VarHash>(zyg, Kernel.HashMO);
        }
        if (h_anchor != null)
            AddAnchor(h_anchor, obj.Fetch());
        return obj;
    }

    Variable GetFromNumber() {
        s_content = GetJsonNumber();
        s_tag = "Num";
        return ParseScalar();
    }

    string GetJsonNumber() {
        StringBuilder s = new StringBuilder();
        do {
            if (from.Length == ix)
                Err("Unexpected end of input in number");
            char x = from[ix++];
            if (x == ',' || x == ']' || x == '}' || x == ' ' || x == '\r' ||
                    x == '\n' || x == '\t')
                break;
            s.Append(x);
        } while (true);
        ix--;
        return s.ToString();
    }

    int VersionComponent(string dgs) {
        foreach(char ch in dgs)
            if (ch < '0' || ch > '9') Err("Invalid character in version component");
        int r;
        if (!int.TryParse(dgs, out r))
            Err("Version component too big");
        return r;
    }

    // the [ { and first directive have already been processed
    void GetDirectiveBlock() {
        bool first = true;
        string version = null;

        while (true) {
            if (!first) {
                SkipWhite(true);
                if (from[ix] == '}')
                    break;
                SkipToken(",");
                SkipWhite(true);
                GetString();
                if (s_content_type != DIRECTIVE)
                    Err("Found non-directive in directives block");
            }
            first = false;
            SkipWhite(true);

            if (s_content == "JSYNC") {
                if (version != null)
                    Err("Version specified twice");
                version = GetSimpleStringValue();
            } else {
                Err("Unknown directive " + s_content);
            }
        }
        SkipToken("}");
        SkipWhite(true);

        if (version != null) {
            int ixdot = version.IndexOf('.');
            if (ixdot < 1 || ixdot > version.Length - 2)
                Err("Version number must have a period in the middle");
            VersionComponent(version.Substring(ixdot+1));
            if (VersionComponent(version.Substring(0,ixdot)) != 1)
                Err("Unsupported major version");
        }
    }

    Variable GetTopLevel() {
        char f = SkipWhite(true);
        if (f != '[') {
            if (f != '{')
                Err("Top level item must be an aggregate");
            goto bare;
        }
        ix++;
        if (SkipWhite(true) != '{') goto bare;
        ix++;
        if (SkipWhite(true) != '"') goto bare;
        GetString();
        if (s_content_type != DIRECTIVE) goto bare;

        GetDirectiveBlock();
        SkipCharWS(',');
        Variable v = GetObj();
        SkipCharWS(']');
        return v;

bare:
        // backtracking, yes, but only once per document.
        ix = 0;
        return GetObj();
    }

    // TODO GetTopLevel

    public static P6any FromJson(string inp) {
        JsyncReader j = new JsyncReader();
        j.from = inp;
        j.SkipWhite(true);
        Variable top = j.GetFromJson(true);
        j.SkipWhite(false);
        if (j.ix != inp.Length)
            j.Err("Trailing garbage after object");

        return top.Fetch();
    }

    public static P6any FromJsync(string inp) {
        JsyncReader j = new JsyncReader();
        j.from = inp;
        j.SkipWhite(true);
        Variable top = j.GetTopLevel();


        foreach (KeyValuePair<string, List<Variable>> da in j.anchorrefs) {
            P6any r;
            if (!j.anchors.TryGetValue(da.Key, out r))
                j.Err("Undefined anchor " + da.Key);
            foreach (Variable to in da.Value)
                to.Store(r);
        }

        j.SkipWhite(false);
        if (j.ix != inp.Length)
            j.Err("Trailing garbage after object");

        return top.Fetch();
    }
}

public class JsonWriter {
    StringBuilder o = new StringBuilder();

    void WriteVal(P6any obj) {
        if (!obj.IsDefined()) {
            o.Append("null");
        } else if (obj.Isa(Kernel.BoolMO)) {
            o.Append(Kernel.UnboxAny<bool>(obj) ? "true" : "false");
        } else if (obj.Isa(Kernel.NumMO)) {
            o.Append(Kernel.UnboxAny<double>(obj));
        } else if (obj.Isa(Kernel.StrMO)) {
            o.Append('"');
            JsyncWriter.AddStringContents(o, Kernel.UnboxAny<string>(obj));
            o.Append('"');
        } else {
            WriteObj(obj);
        }
    }

    void WriteObj(P6any obj) {
        bool comma = false;
        bool def = obj.IsDefined();
        if (def && obj.Isa(Kernel.HashMO)) {
            VarHash vh = Kernel.UnboxAny<VarHash>(obj);
            o.Append('{');
            foreach(KeyValuePair<string,Variable> kv in vh) {
                if (comma) o.Append(',');
                comma = true;
                o.Append('"');
                JsyncWriter.AddStringContents(o, kv.Key);
                o.Append('"');
                o.Append(':');
                WriteVal(kv.Value.Fetch());
            }
            o.Append('}');
        } else if (def && obj.Isa(Kernel.ListMO)) {
            VarDeque iter = new VarDeque(Kernel.NewRWListVar(obj));
            o.Append('[');
            while (Kernel.IterHasFlat(iter, true)) {
                if (comma) o.Append(',');
                comma = true;
                WriteVal(iter.Shift().Fetch());
            }
            o.Append(']');
        } else if (JsyncWriter.FailSoft) {
            o.Append("\"*UNSERIALIZABLE*\"");
        } else {
            throw new NieczaException("JSON writer encountered value of type " +
                    obj.mo.name);
        }
    }

    public static string ToJson(P6any obj) {
        JsonWriter w = new JsonWriter();
        w.WriteObj(obj);
        return w.o.ToString();
    }
}
