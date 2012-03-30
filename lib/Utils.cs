// Misc stuff that isn't really connected to Niecza

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.CompilerServices;
using Niecza.Serialization;

namespace Niecza {
    // Partially taken from mono's System.Tuple by Zoltan Varga and Marek Safar
    public static class Prod {
        public static Prod<T1,T2> C<T1,T2>(T1 v1, T2 v2) { return new Prod<T1,T2>(v1, v2); }
    }
    public class Prod<T1,T2> {
        public T1 v1;
        public T2 v2;
        public Prod(T1 v1, T2 v2) {
            this.v1 = v1;
            this.v2 = v2;
        }

        public override bool Equals (object other) {
            var t = other as Prod<T1, T2>;
            if (t == null)
                return false;
            if (t.v1 != null || v1 != null) {
                if (t.v1 == null || v1 == null || !v1.Equals(t.v1))
                    return false;
            }
            if (t.v2 != null || v2 != null) {
                if (t.v2 == null || v2 == null || !v2.Equals(t.v2))
                    return false;
            }
            return true;
        }

        public override int GetHashCode() {
            int h = (v1 == null ? 0 : v1.GetHashCode());
            h = (h << 5) - h + (v2 == null ? 0 : v2.GetHashCode());
            return h;
        }

        public override string ToString() {
            return string.Format("({0}, {1})", v1, v2);
        }
    }

    public sealed class VarDeque : IFreeze {
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

        public void Shift_UnshiftN(Variable[] vrs) { Shift(); UnshiftN(vrs); }
        public void Shift_UnshiftD(VarDeque vrs) { Shift(); UnshiftD(vrs); }

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

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.VarDeque);
            fb.Int(count);
            int index = head;
            for (int i = 0; i < count; i++) {
                fb.ObjRef(data[index]);
                index++;
                if (index == data.Length) index = 0;
            }
        }
        internal static VarDeque Thaw(ThawBuffer tb) {
            VarDeque r = new VarDeque();
            tb.Register(r);
            int c = tb.Int();
            for (int i = 0; i < c; i++) r.Push((Variable) tb.ObjRef());
            return r;
        }
    }

    struct VarHashLink {
        internal string key;
        internal Variable value;
        internal int next;
    }

    public sealed class VarHash : IEnumerable<KeyValuePair<string,Variable>>,
            IFreeze {
        int hfree;
        int count;
        VarHashLink[] heap;
        int[] htab;

        const int INITIAL = 4;
        const int THRESHOLD = 8;

        // sacrifices a bit of universality to save a reduction step
        public const int HASH_ARG_MAX = 1073709057;
        [TrueGlobal]
        public static uint string_hash_argument;
        public static uint hash_automorphism;

        public static unsafe int UniversalHash(int buckets, string str) {
            fixed(char *c = str) {
                char *cc = c;
                char *end = cc + str.Length;
                uint accum = 0;
                while (cc < end) {
                    // accum <= 2^32-1-65535
                    accum += *(cc++);
                    // accum <= 2^32-1
                    ulong temp = (ulong)accum * string_hash_argument;
                    // temp <= (2^32-1) * HASH_ARG_MAX
                    // temp <= 4611545284160290815
                    accum = (uint)((temp & 0x7FFFFFFF) + (temp >> 31));
                    // accum <= floor(temp / 2^31) + (2^31-1)
                    // accum <= 4294901760 = 2^32-1-65535
                }
                ulong temp2 = (ulong)accum * (uint)buckets;
                return (int) (temp2 >> 32);
            }
        }

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

                int bkt = UniversalHash(htab.Length, key);
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
            while (heap.Length != (1 << (rank + 2))) rank++;
            rank += ordel;

            VarHashLink[] oheap = heap;
            init(1 << (rank + 2));

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

            int bkt = UniversalHash(htab.Length, key);
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

        public int Count { get { return count; } }

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

            int ptr = htab[UniversalHash(htab.Length, key)];

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

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.VarHash);
            fb.Int(count);
            foreach (KeyValuePair<string,Variable> kv in this) {
                fb.String(kv.Key);
                fb.ObjRef(kv.Value);
            }
        }
        internal static VarHash Thaw(ThawBuffer tb) {
            VarHash r = new VarHash();
            tb.Register(r);
            int c = tb.Int();
            for (int i = 0; i < c; i++)
                r[tb.String()] = (Variable) tb.ObjRef();
            return r;
        }
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
        public static string HashToStr(byte[] hash) {
            char[] buf = new char[hash.Length * 2];
            for (int i = 0; i < hash.Length; i++) {
                buf[i*2]   = "0123456789abcdef"[hash[i] >> 4];
                buf[i*2+1] = "0123456789abcdef"[hash[i] & 15];
            }
            return new string(buf);
        }

        public static void HexDump(byte[] heap) {
            for (int offs = 0; offs < heap.Length; offs += 16) {
                Console.Write("{0:X6}   ", offs);
                int len = heap.Length - offs;
                if (len > 16) len = 16;
                for (int col = 0; col < 16; col++) {
                    if (col >= len)
                        Console.Write("   ");
                    else
                        Console.Write("{0:X2} ", heap[offs+col]);
                    if (col == 7)
                        Console.Write(" ");
                }
                Console.Write("   |");
                for (int col = 0; col < len; col++)
                    Console.Write(
                        (heap[offs+col] < 32 || heap[offs+col] > 126)
                            ? '.' : (char)heap[offs+col]);
                Console.WriteLine("|");
            }
        }

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

        public static string Chr(int r) {
            if (r >= 0x10000) {
                char[] rs = new char[2];
                rs[0] = (char)(0xD800 + ((r - 0x10000) >> 10));
                rs[1] = (char)(0xDC00 + (r & 0x3FF));
                return new string(rs);
            }
            return new string((char)r, 1);
        }

        static NumberFormatInfo p6nfi;
        static Utils() {
            p6nfi = new NumberFormatInfo();
            p6nfi.PositiveInfinitySymbol = "Inf";
            p6nfi.NegativeInfinitySymbol = "-Inf";
        }

        public static string N2S(double n) {
            return n.ToString("R", p6nfi);
        }

        public static double S2N(string n) {
            // XXX compatibility
            if (n.Equals("Infinity")) {
                return double.PositiveInfinity;
            }
            if (n.Equals("-Infinity")) {
                return double.NegativeInfinity;
            }
            return double.Parse(n, p6nfi);
        }

        public static bool S2NB(string n, out double o) {
            if (n.Equals("Infinity")) {
                o = double.PositiveInfinity;
                return true;
            }
            if (n.Equals("-Infinity")) {
                o = double.NegativeInfinity;
                return true;
            }
            return double.TryParse(n, NumberStyles.Float, p6nfi, out o);
        }

        public static string StrFlip(string n) {
            char[] tempCharArray = n.ToCharArray();
            Array.Reverse(tempCharArray);
            return new string(tempCharArray);
        }
    }

    public sealed class Complex : IFreeze {
        public readonly double re;
        public readonly double im;

        public Complex(double re, double im) {
            this.re = re; this.im = im;
        }

        [Immutable]
        public static readonly Complex i = new Complex(0, 1);

        public static Complex operator- (Complex left) {
            return new Complex(-left.re, -left.im);
        }

        public static Complex operator+ (Double left, Complex right) {
            return new Complex(left + right.re, right.im);
        }

        public static Complex operator+ (Complex left, Complex right) {
            return new Complex(left.re + right.re, left.im + right.im);
        }

        public static Complex operator- (Double left, Complex right) {
            return new Complex(left - right.re, -right.im);
        }

        public static Complex operator- (Complex left, Complex right) {
            return new Complex(left.re - right.re, left.im - right.im);
        }

        public static Complex operator* (Complex left, Complex right) {
            return new Complex(left.re*right.re - left.im*right.im,
                               left.im*right.re + left.re*right.im);
        }

        public static Complex operator* (Double left, Complex right) {
            return new Complex(left*right.re,
                               left*right.im);
        }

        public static Complex operator/ (Complex left, Complex right) {
            double sn2 = right.re*right.re + right.im*right.im;
            return new Complex((left.re*right.re + left.im*right.im)/sn2,
                               (right.re*left.im - right.im*left.re)/sn2);
        }

        public static Complex operator/ (Double left, Complex right) {
            double sn2 = right.re * right.re + right.im * right.im;
            return new Complex(left * right.re / sn2,
                               -right.im * left / sn2);
        }

        public static Complex operator/ (Complex left, Double right) {
            return new Complex(left.re / right,
                               left.im / right);
        }

        public Complex log() {
            return new Complex(Math.Log(Math.Sqrt(re * re + im * im)),
                               Math.Atan2(im, re));
        }

        public Complex exp() {
            return new Complex(Math.Exp(re) * Math.Cos(im),
                               Math.Exp(re) * Math.Sin(im));
        }

        public Complex sqrt() {
            return (this.log() / 2).exp();
        }

        public Complex Sin() {
            return new Complex(Math.Sin(re) * Math.Cosh(im),
                               Math.Cos(re) * Math.Sinh(im));
        }

        public Complex Asin() {
            return -i * (this * i + (1 - this * this).sqrt()).log();
        }

        public Complex Cos() {
            return new Complex(Math.Cos(re) * Math.Cosh(im),
                               -Math.Sin(re) * Math.Sinh(im));
        }

        public Complex Acos() {
            return Math.PI / 2 - this.Asin();
        }

        public Complex Tan() {
            return this.Sin() / this.Cos();
        }

        public Complex Atan() {
            return ((1 + this * i).log() - (1 - this * i).log()) / (2 * i);
        }

        public Complex Sec() {
            return 1 / this.Cos();
        }

        public Complex Asec() {
            return (1 / this).Acos();
        }

        public Complex Cosec() {
            return 1 / this.Sin();
        }

        public Complex Acosec() {
            return (1 / this).Asin();
        }

        public Complex Cotan() {
            return this.Cos() / this.Sin();
        }

        public Complex Acotan() {
            return (1 / this).Atan();
        }

        public Complex Sinh() {
            return -((i * this).Sin()) * i;
        }

        public Complex Asinh() {
            return (this + (1 + this * this).sqrt()).log();
        }

        public Complex Cosh() {
            return (i * this).Cos();
        }

        public Complex Acosh() {
            return 2 * (((1 + this) / 2).sqrt() + ((-1 + this) / 2).sqrt()).log();
/*            return (this + (-1 + this * this).sqrt()).log();*/
        }

        public Complex Tanh() {
            return -(i * this).Tan() * i;
        }

        public Complex Atanh() {
            return ((1 + this).log() - (1 - this).log()) / 2;
        }

        public Complex Sech() {
            return 1 / this.Cosh();
        }

        public Complex Asech() {
            return (1 / this).Acosh();
        }

        public Complex Cosech() {
            return 1 / this.Sinh();
        }

        public Complex Acosech() {
            return (1 / this).Asinh();
        }

        public Complex Cotanh() {
            return 1 / this.Tanh();
        }

        public Complex Acotanh() {
            return (1 / this).Atanh();
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.Complex);
            fb.Double(re);
            fb.Double(im);
        }
        internal static Complex Thaw(ThawBuffer tb) {
            // NOTE this deferral only works because we don't call ObjRef
            Complex r = new Complex(tb.Double(), tb.Double());
            tb.Register(r);
            return r;
        }
    }

    public sealed class Rat : IFreeze {
        public BigInteger num;
        public ulong den;

        public Rat(BigInteger num, ulong den) {
            this.num = num; this.den = den;
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.Rat);
            fb.ObjRef(num);
            fb.Long((long)den);
        }
        private Rat() {}
        internal static Rat Thaw(ThawBuffer tb) {
            Rat r = new Rat();
            tb.Register(r);
            r.num = (BigInteger)tb.ObjRef();
            r.den = (ulong)tb.Long();
            return r;
        }
    }

    public sealed class FatRat : IFreeze {
        public BigInteger num;
        public BigInteger den;

        public FatRat(BigInteger num, BigInteger den) {
            this.num = num; this.den = den;
        }

        void IFreeze.Freeze(FreezeBuffer fb) {
            fb.Byte((byte)SerializationCode.FatRat);
            fb.ObjRef(num);
            fb.ObjRef(den);
        }
        private FatRat() {}
        internal static FatRat Thaw(ThawBuffer tb) {
            FatRat r = new FatRat();
            tb.Register(r);
            r.num = (BigInteger)tb.ObjRef();
            r.den = (BigInteger)tb.ObjRef();
            return r;
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
