using Niecza;
using System;
// this exists to allow O(1) addition, since additions (esp. in the presence
// of backtracking) dominate lookups

public class Matched {
    public Matched  next;
    public string   name;
    public Variable val; // or null for a list-mode sentinel

    public Matched(Matched next, string name, Variable val) {
        this.next = next;
        this.name = name;
        this.val = val;
    }
}
//
//public class Match {
//    public string backing;
//    public int from;
//    public int to;
//    public Dictionary<string,Variable> captures;
//}
//
//public class Xact {
//    

public class Cursor {
    // XXX It's a bit wrong that we ref the string both from the cursor and
    // from $*ORIG.
    public Matched captures;
    public string backing;
    public int pos;

    public Cursor(Matched captures, string backing, int pos) {
        this.captures = captures;
        this.backing = backing;
        this.pos = pos;
    }

    public Cursor(string backing) : this(null, backing, 0) { }

    public Cursor At(int npos) {
        return new Cursor(captures, backing, npos);
    }

    public Cursor Exact(string what) {
        if (backing.Length - what.Length >= pos &&
                backing.Substring(pos, what.Length) == what) {
            return At(pos + what.Length);
        } else {
            return null;
        }
    }

    public Cursor SetCaps(Matched caps) {
        return new Cursor(caps, backing, pos);
    }

    public Cursor Bind(string name, Variable what) {
        return SetCaps(new Matched(captures, name, what));
    }

    public Cursor SimpleWS() {
        int l = backing.Length;
        int p = pos;
        if (p != 0 && p != l && !Char.IsWhiteSpace(backing, p) &&
                !Char.IsWhiteSpace(backing, p-1)) {
            return null;
        }

        while (p != l && Char.IsWhiteSpace(backing, p)) { p++; }

        return At(p);
    }
}
