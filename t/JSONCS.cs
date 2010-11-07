using JSDOM;
using System;
using System.Text;
using System.Collections.Generic;

public class JSONTester {
    public static void WalkDOMString(StringBuilder o, string x) {
        if (x == null) {
            o.Append('n');
        } else {
            o.Append(x.Length);
            o.Append(':');
            o.Append(x);
        }
    }

    public static void WalkDOM(StringBuilder o, HashSet<Value> seen,
            Value roots) {
        if (roots == null) {
            o.Append('E');
            return;
        }

        if (seen.Contains(roots))
            throw new Exception("WalkDOM: Cycle detected");
        seen.Add(roots);

        if (roots is Sequence) {
            o.Append('@');
            WalkDOMString(o, roots.tag);
            WalkDOMString(o, roots.anchor);
            WalkDOM(o, seen, ((Sequence) roots).first);
        } else if (roots is Mapping) {
            o.Append('%');
            WalkDOMString(o, roots.tag);
            WalkDOMString(o, roots.anchor);
            WalkDOM(o, seen, ((Mapping) roots).first);
        } else if (roots is Scalar) {
            o.Append('$');
            WalkDOMString(o, roots.tag);
            WalkDOMString(o, roots.anchor);
            WalkDOMString(o, ((Scalar) roots).text);
        }

        WalkDOM(o, seen, roots.next);
    }

    public static string GetDOMString(string t, ref int ix) {
        if (t[ix] == 'n') { ix++; return null; }
        int ac = 0;
        while (t[ix] >= '0' && t[ix] <= '9') { ac *= 10; ac += (t[ix++]- '0'); }
        ix++;
        string r = t.Substring(ix, ac);
        ix += ac;
        return r;
    }

    public static Value GetDOM(string t, ref int ix) {
        char code = t[ix++];
        if (code == 'E') return null;
        Value n = (code == '$') ? new Scalar() :
                  (code == '@') ? new Sequence() :
                  (Value)new Mapping();
        n.tag = GetDOMString(t, ref ix);
        n.anchor = GetDOMString(t, ref ix);
        if (code == '$') {
            ((Scalar)n).text = GetDOMString(t, ref ix);
        } else if (code == '@') {
            ((Sequence)n).first = GetDOM(t, ref ix);
        } else if (code == '%') {
            ((Mapping)n).first = GetDOM(t, ref ix);
        }
        n.next = GetDOM(t, ref ix);
        return n;
    }

    public static string ToStr(Value v) {
        StringBuilder sb = new StringBuilder();
        WalkDOM(sb, new HashSet<Value>(), v);
        return sb.ToString();
    }

    public static Value FromStr(string st) {
        int ix = 0;
        return GetDOM(st, ref ix);
    }

    private static int k = 0;
    public static void Is(string s1, string s2, string msg) {
        Console.WriteLine("{0} {1} - {2}", (s1 == s2 ? "ok" : "not ok"),
                (++k), msg);
        if (s1 != s2) {
            Console.WriteLine("# result: {0}", s1);
            Console.WriteLine("# expect: {0}", s2);
        }
    }

    public static void Main() {
        Is(ToStr(FromStr("E")), "E", "base: null roundtrip");
        Is(ToStr(FromStr("$1:x1:y1:zE")), "$1:x1:y1:zE", "base: scalar roundtrip");
        Is(ToStr(FromStr("$nnn$nnnE")), "$nnn$nnnE", "chain rountrip");
        Is(ToStr(FromStr("@nn$nnnEE")), "@nn$nnnEE", "sequence roundtrip");
        Is(ToStr(FromStr("%nnEE")), "%nnEE", "mapping roundtrip");
        Console.WriteLine("1..{0}", k);
    }
}
