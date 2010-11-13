using Niecza;
using System;
using System.Collections.Generic;
using System.Text;

public class JsyncWriter {
    StringBuilder o = new StringBuilder();
    Dictionary<object,int> anchors = new Dictionary<object,int>();
    int nextanchor = 0;

    void WriteObj(IP6 obj) {
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
            WriteBool((bool) Kernel.UnboxAny(obj));
        } else if (obj.Isa(Kernel.StrMO)) {
            WriteStr(true, (string) Kernel.UnboxAny(obj));
        } else if (obj.Isa(Kernel.NumMO)) {
            WriteNum((double) Kernel.UnboxAny(obj));
        } else {
            WriteGeneral(obj);
        }
    }

    void WriteNull() {
        o.Append("null");
    }

    void WriteArray(IP6 obj) {
        int a = nextanchor++;
        anchors[obj] = a;
        Frame nr = new Frame(null, null, Kernel.ExitRunloopSI);
        nr = obj.InvokeMethod(nr, "eager", new Variable[] { Kernel.NewROScalar(obj) }, null);
        Kernel.RunCore(nr);

        o.AppendFormat("[\"&{0}\"", a);
        VarDeque vd = (VarDeque) obj.GetSlot("items");
        for (int i = 0; i < vd.Count(); i++) {
            o.Append(',');
            WriteObj(vd[i].Fetch());
        }
        o.Append(']');
    }

    void WriteHash(IP6 obj) {
        int a = nextanchor++;
        anchors[obj] = a;
        Dictionary<string,Variable> entries = (Dictionary<string,Variable>)
            Kernel.UnboxAny(obj);
        o.Append('{');
        o.AppendFormat("\"&\":{0}", a);
        foreach (KeyValuePair<string,Variable> kv in entries) {
            o.Append(',');

            // no object keys in hashes yet
            WriteStr(true, kv.Key);
            o.Append(':');
            WriteObj(kv.Value.Fetch());
        }
        o.Append('}');
    }

    void WriteGeneral(IP6 obj) {
        int a = nextanchor++;
        anchors[obj] = a;
        DynObject dyo = (DynObject) obj;
        DynMetaObject mo = dyo.mo;

        o.Append('{');
        o.AppendFormat("\"&\":{0},\"!\":", a);
        WriteStr(false, "!perl6:" + mo.name);

        for (int i = 0; i < mo.nslots; i++) {
            o.Append(',');
            WriteStr(true, mo.all_attr[i]);
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
        o.Append('"');
        if (esc && NeedsEscape(s))
            o.Append('.');
        foreach (char ch in s) {
            switch(ch) {
                case '\\':
                    o.Append('\\');
                    goto default;
                case '\"':
                    o.Append('\\');
                    goto default;
                default:
                    if ((ch & 0xFF7F) < 32) {
                        o.AppendFormat("\\u{0:4X}", (int)ch);
                    } else {
                        o.Append(ch);
                    }
                    break;
                case '\b':
                    o.Append('\\');
                    o.Append('b');
                    break;
                case '\f':
                    o.Append('\\');
                    o.Append('f');
                    break;
                case '\t':
                    o.Append('\\');
                    o.Append('t');
                    break;
                case '\r':
                    o.Append('\\');
                    o.Append('r');
                    break;
                case '\n':
                    o.Append('\\');
                    o.Append('n');
                    break;
            }
        }
        o.Append('"');
    }

    void WriteBool(bool x) {
        o.Append(x ? "true" : "false");
    }

    void WriteNum(double x) {
        o.Append(x);
    }

    void WriteAnchor(int i) {
        o.AppendFormat("\"*{0}\"", i);
    }

    public static string ToJsync(IP6 obj) {
        JsyncWriter w = new JsyncWriter();
        w.WriteObj(obj);
        return w.o.ToString();
    }
}
