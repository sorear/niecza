// this exists to allow O(1) addition, since additions (esp. in the presence
// of backtracking) dominate lookups
//public class Matched {
//    Matched  prev;
//    string   name;
//    Variable val;
//    bool     listify_sentinel;
//}
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
    public string backing;
    public int pos;

    public Cursor(string backing, int pos) {
        this.backing = backing;
        this.pos = pos;
    }

    public Cursor(string backing) : this(backing, 0) { }

    public Cursor At(int npos) {
        return new Cursor(backing, npos);
    }

    public Cursor Exact(string what) {
        if (backing.Length - what.Length >= pos &&
                backing.Substring(pos, what.Length) == what) {
            return At(pos + what.Length);
        } else {
            return null;
        }
    }
}
