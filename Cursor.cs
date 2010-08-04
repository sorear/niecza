// this exists to allow O(1) addition, since additions (esp. in the presence
// of backtracking) dominate lookups
public class Matched {
    Matched  prev;
    string   name;
    Variable val;
    bool     listify_sentinel;
}

public class Match {
    public string backing;
    public int from;
    public int to;
    public Dictionary<string,Variable> captures;
}

public class Xact {
    

public class Cursor {
    // TODO: use a mutable form, for <cut>
    public string backing;
    public int pos;
    public Matched captures;
}
