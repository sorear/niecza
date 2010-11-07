namespace JSDOM {
    // maybe should use a node type enum?
    public abstract class Value {
        public Value next;
        public string tag;
        public string anchor;
    }

    public class Scalar : Value {
        public string text;
    }

    public class Sequence : Value {
        public Value first;
    }

    public class Mapping : Value {
        public Value first; // keys and values alternate
    }
}
