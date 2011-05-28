// Misc stuff that isn't really connected to Niecza

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.CompilerServices;

namespace Niecza {
    public sealed class VarDeque {
        private Variable[] data;
        private int head;
        private int count;

        public int Count() { return count; }

        public VarDeque() {
            data = new Variable[8];
        }

        public VarDeque(VarDeque tp) {
            data = (Variable[]) tp.data.Clone();
            head = tp.head;
            count = tp.count;
        }

        public VarDeque(Variable[] parcel) {
            int cap = 8;
            while (cap <= parcel.Length) cap *= 2;
            data = new Variable[cap];
            Array.Copy(parcel, 0, data, 0, parcel.Length);
            count = parcel.Length;
        }

        public VarDeque(Variable item) {
            data = new Variable[8];
            count = 1;
            data[0] = item;
        }

        private int fixindex(int index) {
            int rix = index + head;
            if (rix >= data.Length) rix -= data.Length;
            return rix;
        }

        private int fixindexc(int index) {
            if (index >= count)
                throw new IndexOutOfRangeException();
            return fixindex(index);
        }

        public Variable this[int index] {
            get { return data[fixindexc(index)]; }
            set { data[fixindexc(index)] = value; }
        }

        public void Push(Variable vr) {
            checkgrow();
            data[fixindex(count++)] = vr;
        }

        public Variable Pop() {
            int index = fixindex(--count);
            Variable d = data[index];
            data[index] = null;
            return d;
        }

        public void Unshift(Variable vr) {
            checkgrow();
            head--;
            count++;
            if (head < 0) head += data.Length;
            data[head] = vr;
        }

        public void UnshiftN(Variable[] vrs) {
            for (int i = vrs.Length - 1; i >= 0; i--)
                Unshift(vrs[i]);
        }

        public void PushN(Variable[] vrs) {
            for (int i = 0; i < vrs.Length; i++)
                Push(vrs[i]);
        }

        public void PushD(VarDeque vrs) {
            if (vrs == this) { PushN(vrs.CopyAsArray()); return; }
            for (int i = 0; i < vrs.count; i++)
                Push(vrs[i]);
        }
        public void UnshiftD(VarDeque vrs) {
            if (vrs == this) { UnshiftN(vrs.CopyAsArray()); return; }
            for (int i = vrs.count - 1; i >= 0; i--)
                Unshift(vrs[i]);
        }

        public Variable Shift() {
            int index = head++;
            if (head == data.Length) head = 0;
            count--;
            Variable d = data[index];
            data[index] = null;
            return d;
        }

        private void CopyToArray(Variable[] tg) {
            int z1 = data.Length - head;
            if (z1 >= count) {
                Array.Copy(data, head, tg, 0, count);
            } else {
                Array.Copy(data, head, tg, 0, z1);
                int z2 = count - z1;
                Array.Copy(data, 0, tg, z1, z2);
            }
        }

        public Variable[] CopyAsArray() {
            Variable[] ret = new Variable[count];
            CopyToArray(ret);
            return ret;
        }

        private void checkgrow() {
            if (count == data.Length) {
                Variable[] ndata = new Variable[data.Length * 2];
                CopyToArray(ndata);
                data = ndata;
                head = 0;
            }
        }
    }

    struct VarHashLink {
        internal string key;
        internal Variable value;
        internal int next;
    }

    public sealed class VarHash : IEnumerable<KeyValuePair<string,Variable>> {
        int hfree;
        int count;
        VarHashLink[] heap;
        int[] htab;

        const int INITIAL = 5;
        const int THRESHOLD = 11;

        static int[] grow = new int[] {
            5, 11, 17, 37, 67, 131, 257, 521, 1031, 2053, 4099, 8209, 16411,
            32771, 65537, 131101, 262147, 524309, 1048583, 2097169, 4194319,
            8388617, 16777259, 33554467, 67108879, 134217757, 268435459,
            536870923, 1073741827
        };

        public VarHash() { Clear(); }

        public VarHash(VarHash from) {
            hfree = from.hfree;
            count = from.count;
            int l = from.heap.Length;
            if (from.htab != null) {
                htab  = new int[l];
                Array.Copy(from.htab, 0, htab, 0, l);
            } else {
                htab = null;
            }
            heap  = new VarHashLink[l];
            Array.Copy(from.heap, 0, heap, 0, l);
        }

        public Variable this[string key] {
            get {
                Variable d;
                if (TryGetValue(key, out d))
                    return d;
                else
                    throw new KeyNotFoundException(key);
            }
            set {
                if (hfree < 0) rehash(+1);

                if (htab == null) {
                    for (int i = 0; i < count; i++) {
                        if (heap[i].key == key) {
                            heap[i].value = value;
                            return;
                        }
                    }
                    heap[count].key = key;
                    heap[count].value = value;
                    count++; hfree--;
                    return;
                }

                int bkt = (int)(((uint) key.GetHashCode()) %
                        ((uint) htab.Length));
                int ptr = htab[bkt];

                if (ptr < 0) {
                    int n = hfree;
                    hfree = heap[n].next;
                    heap[n].next = ptr;
                    heap[n].key = key;
                    heap[n].value = value;
                    htab[bkt] = n;
                    count++;
                    return;
                }

                if (heap[ptr].key == key) {
                    heap[ptr].value = value;
                    return;
                }

                bkt = ptr;
                ptr = heap[bkt].next;
                while (true) {
                    if (ptr < 0) {
                        int n = hfree;
                        hfree = heap[n].next;
                        heap[n].next = ptr;
                        heap[n].key = key;
                        heap[n].value = value;
                        heap[bkt].next = n;
                        count++;
                        return;
                    }

                    if (heap[ptr].key == key) {
                        heap[ptr].value = value;
                        return;
                    }

                    bkt = ptr;
                    ptr = heap[bkt].next;
                }
            }
        }

        public bool ContainsKey(string key) {
            Variable scratch;
            return TryGetValue(key, out scratch);
        }

        void rehash(int ordel) {
            int rank = 0;
            while (heap.Length != grow[rank]) rank++;
            rank += ordel;

            VarHashLink[] oheap = heap;
            init(grow[rank]);

            foreach (VarHashLink vhl in oheap)
                if (vhl.key != null)
                    this[vhl.key] = vhl.value;
        }

        public bool Remove(string key) {
            if (count < (heap.Length >> 2) && heap.Length != INITIAL)
                rehash(-1);

            if (htab == null) {
                for (int i = 0; i < count; i++) {
                    if (heap[i].key == key) {
                        if (i != count - 1) {
                            heap[i].key = heap[count - 1].key;
                            heap[i].value = heap[count - 1].value;
                        }
                        heap[count - 1].key = null;
                        heap[count - 1].value = null;
                        count--; hfree++;
                        return true;
                    }
                }
                return false;
            }

            int bkt = (int)(((uint) key.GetHashCode()) % ((uint) htab.Length));
            int ptr = htab[bkt];

            if (ptr < 0)
                return false;

            if (heap[ptr].key == key) {
                int n = heap[ptr].next;
                heap[ptr].next = hfree;
                htab[bkt] = n;
                heap[ptr].key = null;
                heap[ptr].value = null;
                hfree = ptr;
                count--;
                return true;
            }

            bkt = ptr;
            ptr = heap[bkt].next;
            while (ptr >= 0) {
                if (heap[ptr].key == key) {
                    int n = heap[ptr].next;
                    heap[ptr].next = hfree;
                    heap[bkt].next = n;
                    heap[ptr].key = null;
                    heap[ptr].value = null;
                    hfree = ptr;
                    count--;
                    return true;
                }

                bkt = ptr;
                ptr = heap[bkt].next;
            }

            return false;
        }

        void init(int size) {
            hfree = size - 1;
            count = 0;
            heap = new VarHashLink[size];
            if (size > THRESHOLD) {
                htab = new int[size];
                for (int i = 0; i < size; i++) {
                    heap[i].next = i - 1;
                    htab[i] = -1;
                }
            } else {
                htab = null;
            }
        }

        public void Clear() { init(INITIAL); }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() {
            return GetEnumerator();
        }

        public IEnumerator<KeyValuePair<string,Variable>> GetEnumerator() {
            return new Enum(heap);
        }

        public bool IsNonEmpty {
            get { return count != 0; }
        }

        public bool TryGetValue(string key, out Variable value) {
            if (htab == null) {
                for (int i = 0; i < count; i++) {
                    if (heap[i].key == key) {
                        value = heap[i].value;
                        return true;
                    }
                }
                value = null;
                return false;
            }

            int ptr = htab[((uint) key.GetHashCode()) % ((uint) htab.Length)];

            while (ptr >= 0) {
                if (heap[ptr].key == key) {
                    value = heap[ptr].value;
                    return true;
                }

                ptr = heap[ptr].next;
            }

            value = null;
            return false;
        }

        public class Enum : IEnumerator<KeyValuePair<string, Variable>> {
            int cursor;
            VarHashLink[] pool;
            internal Enum(VarHashLink[] p) { cursor = -1; pool = p; }
            void Scan() {
                if (cursor != pool.Length) cursor++;
                while (cursor != pool.Length && pool[cursor].key == null)
                    cursor++;
            }
            public void Reset() { cursor = -1; }
            public bool MoveNext() {
                Scan();
                return (cursor != pool.Length);
            }
            public KeyValuePair<string,Variable> Current {
                get {
                    return new KeyValuePair<string,Variable>(
                        pool[cursor].key, pool[cursor].value);
                }
            }
            object System.Collections.IEnumerator.Current {
                get { return Current; }
            }
            public void Dispose() { }
        }

        public struct VarHashKeys : IEnumerable<string> {
            VarHash th;
            internal VarHashKeys(VarHash x) { th = x; }

            System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() {
                return GetEnumerator();
            }

            public struct KEnum : IEnumerator<string> {
                int cursor;
                VarHashLink[] pool;
                internal KEnum(VarHashLink[] p) { cursor = -1; pool = p; }
                void Scan() {
                    if (cursor != pool.Length) cursor++;
                    while (cursor != pool.Length && pool[cursor].key == null)
                        cursor++;
                }
                public void Reset() { cursor = -1; }
                public bool MoveNext() {
                    Scan();
                    return (cursor != pool.Length);
                }
                public string Current {
                    get { return pool[cursor].key; }
                }
                object System.Collections.IEnumerator.Current {
                    get { return Current; }
                }
                public void Dispose() { }
            }

            public IEnumerator<string> GetEnumerator() {
                return new KEnum(th.heap);
            }
        }

        public VarHashKeys Keys { get { return new VarHashKeys(this); } }
    }

    public class SubscriberSet {
        HashSet<WeakReference> subscribers = new HashSet<WeakReference>();

        [MethodImpl(MethodImplOptions.Synchronized)]
        internal void Add(WeakReference w) { subscribers.Add(w); }

        [MethodImpl(MethodImplOptions.Synchronized)]
        internal void Remove(WeakReference w) { subscribers.Remove(w); }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public List<object> GetSubscribers() {
            List<object> r = new List<object>();
            foreach (WeakReference w in subscribers) {
                Subscription s = w.Target as Subscription;
                object o = (s == null) ? null : s.owner.Target;
                if (o != null) r.Add(o);
            }
            return r;
        }
    }

    public class Subscription {
        WeakReference wr_this;
        internal WeakReference owner;
        SubscriberSet set;

        public Subscription(object owner, SubscriberSet set) {
            this.owner = new WeakReference(owner);
            this.set = set;
            wr_this = new WeakReference(this);
            set.Add(wr_this);
        }

        public void Terminate() {
            set.Remove(wr_this);
        }

        ~Subscription() { Terminate(); }
    }

    public class Utils {
        // s1 must not have embedded nuls
        public static unsafe bool StartsWithInvariant(string s1, string s2) {
            fixed (char* st1 = s1) {
                char* p1 = st1;
                fixed (char* st2 = s2) {
                    char* p2 = st2;
                    while (*p1 != '\0' && *p1 == *p2) {
                        p1++;
                        p2++;
                    }
                    return (*p1 == '\0');
                }
            }
        }

        public static string N2S(double n) {
            return n.ToString(CultureInfo.InvariantCulture);
        }

        public static double S2N(string n) {
            return double.Parse(n, CultureInfo.InvariantCulture);
        }

        public static bool S2NB(string n, out double o) {
            return double.TryParse(n, NumberStyles.Float, CultureInfo.InvariantCulture, out o);
        }

        public static string StrFlip(string n) {
            char[] tempCharArray = n.ToCharArray();
            Array.Reverse(tempCharArray);
            return new string(tempCharArray);
        }
    }

    public sealed class Complex {
        public readonly double re;
        public readonly double im;

        public Complex(double re, double im) {
            this.re = re; this.im = im;
        }
    }

    public sealed class Rat {
        public readonly BigInteger num;
        public readonly ulong den;

        public Rat(BigInteger num, ulong den) {
            this.num = num; this.den = den;
        }
    }

    public sealed class FatRat {
        public readonly BigInteger num;
        public readonly BigInteger den;

        public FatRat(BigInteger num, BigInteger den) {
            this.num = num; this.den = den;
        }
    }

    public sealed class RatApproxer {
        // This could be optimized a fair amount if it becomes necessary.
        // In particular, due to common prefixes the code is doing about
        // four times as much work as it has to; also, it's probably not
        // necessary to actually store the continued fractions in memory.
        // http://en.wikipedia.org/w/index.php?title=Continued_fraction&oldid=429751049#Best_rational_within_an_interval

        static List<BigInteger> GetContinuedFraction(BigInteger n, BigInteger d,
                bool extend) {
            List<BigInteger> r = new List<BigInteger>();
            while (d.Sign != 0) {
                BigInteger nn;
                r.Add(BigInteger.DivRem(n, d, out nn));
                n = d;
                d = nn;
            }
            if (extend) {
                r[r.Count-1]--;
                r.Add(BigInteger.One);
            }
            return r;
        }

        // entry invariant: 0 < l < h
        static void CandidateSimplest(BigInteger numl, BigInteger denl, bool xl,
                BigInteger numh, BigInteger denh, bool xh,
                ref BigInteger snum, ref BigInteger sden) {
            List<BigInteger> cfl = GetContinuedFraction(numl, denl, xl);
            List<BigInteger> cfh = GetContinuedFraction(numh, denh, xh);
            List<BigInteger> cfo = new List<BigInteger>();

            int i = 0;
            while (i != cfl.Count && i != cfh.Count && cfl[i] == cfh[i])
                cfo.Add(cfl[i++]);

            if (i != cfh.Count && (i == cfl.Count || cfl[i] > cfh[i]))
                cfo.Add(cfh[i] + BigInteger.One);
            else
                cfo.Add(cfl[i] + BigInteger.One);

            BigInteger onum = cfo[i];
            BigInteger oden = BigInteger.One;

            for (i--; i >= 0; i--) {
                BigInteger t = onum; onum = oden; oden = t;
                onum += oden * cfo[i];
            }

            if (oden < sden) {
                sden = oden;
                snum = onum;
            }
        }

        public static void Simplest(BigInteger numl, BigInteger denl,
                BigInteger numh, BigInteger denh,
                out BigInteger numo, out BigInteger deno) {
            int signl = numl.Sign * denl.Sign;
            int signh = numh.Sign * denh.Sign;

            if (signl * signh <= 0) {
                // interval includes 0, automatically simplest
                numo = BigInteger.Zero;
                deno = BigInteger.One;
                return;
            }

            if (signl < 0) {
                numl = -numl; numh = -numh;
            }

            BigInteger gl = BigInteger.GreatestCommonDivisor(numl, denl);
            numl /= gl; denl /= gl;
            BigInteger gh = BigInteger.GreatestCommonDivisor(numh, denh);
            numh /= gh; denh /= gh;

            BigInteger snum = numl;
            BigInteger sden = denl;

            if (denh < denl) {
                snum = numh;
                sden = denh;
            }

            for (int k = 0; k < 4; k++)
                CandidateSimplest(numl, denl, (k&1)!=0, numh, denh, (k&2)!=0, ref snum, ref sden);

            if (signl < 0)
                snum = -snum;

            numo = snum;
            deno = sden;
        }
    }
}
