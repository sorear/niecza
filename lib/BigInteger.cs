/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Apache License, Version 2.0, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Apache License, Version 2.0.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Text;

namespace Niecza {
    using Math = System.Math;
    /// <summary>
    /// arbitrary precision integers
    /// </summary>
    [Serializable]
    public sealed class BigInteger : IFormattable, IComparable, IConvertible, IEquatable<BigInteger> {
        private const int BitsPerDigit = 32;
        private const ulong Base = 0x100000000;

        // -1 if negative, +1 if positive, 0 if zero.
        private readonly short sign;

        // Non-null. data[0] is the least significant 32 bits.
        private readonly uint[] data;

        [SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly BigInteger Zero = new BigInteger(0, new uint[0]);
        [SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly BigInteger One = new BigInteger(+1, new uint[] { 1 });
        private const int bias = 1075;

        //[CLSCompliant(false)]
        public static BigInteger Create(ulong v) {
            return new BigInteger(+1, (uint)v, (uint)(v >> BitsPerDigit));
        }

        //[CLSCompliant(false)]
        public static BigInteger Create(uint v) {
            if (v == 0) return Zero;
            else if (v == 1) return One;
            else return new BigInteger(+1, v);
        }

        public static BigInteger Create(long v) {
            ulong x;
            int s = +1;
            if (v < 0) {
                x = (ulong)-v; s = -1;
            } else {
                x = (ulong)v;
            }

            return new BigInteger(s, (uint)x, (uint)(x >> BitsPerDigit));
        }

        public static BigInteger Create(int v) {
            if (v == 0) return Zero;
            else if (v == 1) return One;
            else if (v < 0) return new BigInteger(-1, (uint)-v);
            else return new BigInteger(+1, (uint)v);
        }

        private const Int32 DecimalScaleFactorMask = 0x00FF0000;
        private const Int32 DecimalSignMask = unchecked((Int32)0x80000000);

        public static BigInteger Create(decimal v) {
            // First truncate to get scale to 0 and extract bits
            int[] bits = Decimal.GetBits(Decimal.Truncate(v));

            Debug.Assert(bits.Length == 4 && (bits[3] & DecimalScaleFactorMask) == 0);

            int size = 3;
            while (size > 0 && bits[size - 1] == 0) size--;

            if (size == 0) {
                return BigInteger.Zero;
            }

            UInt32[] array = new UInt32[size];
            array[0] = (UInt32)bits[0];
            if (size > 1) array[1] = (UInt32)bits[1];
            if (size > 2) array[2] = (UInt32)bits[2];

            return new BigInteger(((bits[3] & DecimalSignMask) != 0) ? -1 : +1, array);
        }

        /// <summary>
        /// Create a BigInteger from a little-endian twos-complement byte array
        /// (inverse of ToByteArray())
        /// </summary>
        public static BigInteger Create(byte[] v) {
            if (v.Length == 0) return Create(0);

            int byteCount = v.Length;
            int unalignedBytes = byteCount % 4;
            int dwordCount = byteCount / 4 + (unalignedBytes == 0 ? 0 : 1);
            uint[] data = new uint[dwordCount];

            bool isNegative = (v[byteCount - 1] & 0x80) == 0x80;

            bool isZero = true;

            // Copy all dwords, except but don't do the last one if it's not a full four bytes
            int curDword, curByte, byteInDword;
            curByte = 3;
            for (curDword = 0; curDword < dwordCount - (unalignedBytes == 0 ? 0 : 1); curDword++) {
                byteInDword = 0;
                while (byteInDword < 4) {
                    if (v[curByte] != 0x00) isZero = false;
                    data[curDword] <<= 8;
                    data[curDword] |= v[curByte];
                    curByte--;
                    byteInDword++;
                }
                curByte += 8;
            }

            // Copy the last dword specially if it's not aligned
            if (unalignedBytes != 0) {
                if (isNegative) data[dwordCount - 1] = 0xffffffff;
                for (curByte = byteCount - 1; curByte >= byteCount - unalignedBytes; curByte--) {
                    if (v[curByte] != 0x00) isZero = false;
                    data[curDword] <<= 8;
                    data[curDword] |= v[curByte];
                }
            }

            if (isZero) return Zero;

            if (isNegative) {
                makeTwosComplement(data);
                return new BigInteger(-1, data);
            }
            return new BigInteger(1, data);
        }


        private static bool Negative(byte[] v) {
            return ((v[7] & 0x80) != 0);
        }

        private static ushort Exponent(byte[] v) {
            return (ushort)((((ushort)(v[7] & 0x7F)) << (ushort)4) | (((ushort)(v[6] & 0xF0)) >> 4));
        }

        private static ulong Mantissa(byte[] v) {
            uint i1 = ((uint)v[0] | ((uint)v[1] << 8) | ((uint)v[2] << 16) | ((uint)v[3] << 24));
            uint i2 = ((uint)v[4] | ((uint)v[5] << 8) | ((uint)(v[6] & 0xF) << 16));

            return (ulong)((ulong)i1 | ((ulong)i2 << 32));
        }

        public static BigInteger Create(double v) {
            if (Double.IsNaN(v) || Double.IsInfinity(v)) {
                throw new OverflowException();
            }

            byte[] bytes = System.BitConverter.GetBytes(v);
            ulong mantissa = Mantissa(bytes);
            if (mantissa == 0) {
                // 1.0 * 2**exp, we have a power of 2
                int exponent = Exponent(bytes);
                if (exponent == 0) return Zero;

                BigInteger res = Negative(bytes) ? Negate(One) : One;
                res = res << (exponent - 0x3ff);
                return res;
            } else {
                // 1.mantissa * 2**exp
                int exponent = Exponent(bytes);
                mantissa |= 0x10000000000000ul;
                BigInteger res = BigInteger.Create(mantissa);
                res = exponent > bias ? res << (exponent - bias) : res >> (bias - exponent);
                return Negative(bytes) ? res * (-1) : res;
            }
        }

        public static implicit operator BigInteger(byte i) {
            return Create((uint)i);
        }

        //[CLSCompliant(false)]
        public static implicit operator BigInteger(sbyte i) {
            return Create((int)i);
        }

        public static implicit operator BigInteger(short i) {
            return Create((int)i);
        }

        //[CLSCompliant(false)]
        public static implicit operator BigInteger(ushort i) {
            return Create((uint)i);
        }

        //[CLSCompliant(false)]
        public static implicit operator BigInteger(uint i) {
            return Create(i);
        }

        public static implicit operator BigInteger(int i) {
            return Create(i);
        }

        //[CLSCompliant(false)]
        public static implicit operator BigInteger(ulong i) {
            return Create(i);
        }

        public static implicit operator BigInteger(long i) {
            return Create(i);
        }

        public static implicit operator BigInteger(decimal i) {
            return Create(i);
        }

        public static explicit operator BigInteger(double self) {
            return Create(self);
        }

        public static explicit operator BigInteger(float self) {
            return Create((double)self);
        }

        public static explicit operator double(BigInteger self) {
            if (object.ReferenceEquals(self, null)) {
                throw new ArgumentNullException("self");
            }
            return self.ToFloat64();
        }

        public static explicit operator float(BigInteger self) {
            if (object.ReferenceEquals(self, null)) {
                throw new ArgumentNullException("self");
            }
            return checked((float)self.ToFloat64());
        }

        public static explicit operator decimal(BigInteger self) {
            decimal res;
            if (self.AsDecimal(out res)) {
                return res;
            }
            throw new OverflowException();
        }

        public static explicit operator byte(BigInteger self) {
            int tmp;
            if (self.AsInt32(out tmp)) {
                return checked((byte)tmp);
            }
            throw new OverflowException();
        }

        //[CLSCompliant(false)]
        public static explicit operator sbyte(BigInteger self) {
            int tmp;
            if (self.AsInt32(out tmp)) {
                return checked((sbyte)tmp);
            }
            throw new OverflowException();
        }

        //[CLSCompliant(false)]
        public static explicit operator UInt16(BigInteger self) {
            int tmp;
            if (self.AsInt32(out tmp)) {
                return checked((UInt16)tmp);
            }
            throw new OverflowException();
        }

        public static explicit operator Int16(BigInteger self) {
            int tmp;
            if (self.AsInt32(out tmp)) {
                return checked((Int16)tmp);
            }
            throw new OverflowException();
        }

        //[CLSCompliant(false)]
        public static explicit operator UInt32(BigInteger self) {
            uint tmp;
            if (self.AsUInt32(out tmp)) {
                return tmp;
            }
            throw new OverflowException();
        }

        public static explicit operator Int32(BigInteger self) {
            int tmp;
            if (self.AsInt32(out tmp)) {
                return tmp;
            }
            throw new OverflowException();
        }

        public static explicit operator Int64(BigInteger self) {
            long tmp;
            if (self.AsInt64(out tmp)) {
                return tmp;
            }
            throw new OverflowException();
        }

        //[CLSCompliant(false)]
        public static explicit operator UInt64(BigInteger self) {
            ulong tmp;
            if (self.AsUInt64(out tmp)) {
                return tmp;
            }
            throw new OverflowException();
        }

        public BigInteger(BigInteger copy) {
            if (object.ReferenceEquals(copy, null)) {
                throw new ArgumentNullException("copy");
            }
            this.sign = copy.sign;
            this.data = copy.data;
        }

        public BigInteger(byte[] bytes) : this(Create(bytes)) { }

        //[CLSCompliant(false)]
        public BigInteger(int sign, params uint[] data) {
            int length = GetLength(data);
            
            this.data = data;
            this.sign = (short)(length == 0 ? 0 : sign);
        }

        /// <summary>
        /// Return the magnitude of this BigInteger as an array of zero or more uints.
        /// Element zero is the value of the least significant four bytes, element one is
        /// the value of the four next most significant bytes, etc.
        /// 
        /// The returned data is the unsigned magnitude of the number. To determine the sign,
        /// use GetSign().
        /// 
        /// It is guaranteed that the highest element of the returned array is never zero.
        /// This means that if the value of this BigInteger is zero, a zero-length array
        /// is returned.
        /// </summary>
        //[CLSCompliant(false)]
        public uint[] GetWords() {
            if (sign == 0) return new uint[] { 0 };
            int w = GetLength();
            uint[] bits = new uint[w];
            Array.Copy(data, bits, w);
            return bits;
        }

        //[CLSCompliant(false)]
        public uint GetWord(int index) {
            return data[index];
        }

        public int GetBitCount() {
            if (IsZero()) {
                return 1;
            }
            int w = GetLength() - 1;
            uint b = data[w];
            Debug.Assert(b > 0);
            int result = w * 32;

            if (b >= 1u << 16) {
                b >>= 16;
                result += 16;
            }
            if (b >= 1u << 8) {
                b >>= 8;
                result += 8;
            }
            if (b >= 1u << 4) {
                b >>= 4;
                result += 4;
            }
            if (b >= 1u << 2) {
                b >>= 2;
                result += 2;
            }
            if (b >= 1u << 1) {
                b >>= 1;
                result++;
            }
            if (b > 0) {
                result++;
            }

            return result;
        }

        public int GetByteCount() {
            return (GetBitCount() + 7) / 8;
        }

        /// <summary>
        /// Return the sign of this BigInteger: -1, 0, or 1.
        /// </summary>
        public short Sign {
            get {
                return sign;
            }
        }

        public bool AsInt64(out long ret) {
            ret = 0;
            if (sign == 0) return true;
            if (GetLength() > 2) return false;
            if (data.Length == 1) {
                ret = sign * (long)data[0];
                return true;
            }
            ulong tmp = (((ulong)data[1]) << 32 | (ulong)data[0]);
            if (tmp > 0x8000000000000000) return false;
            if (tmp == 0x8000000000000000 && sign == 1) return false;
            ret = ((long)tmp) * sign;
            return true;
        }

        //[CLSCompliant(false)]
        public bool AsUInt32(out uint ret) {
            ret = 0;
            if (sign == 0) return true;
            if (sign < 0) return false;
            if (GetLength() > 1) return false;
            ret = data[0];
            return true;
        }

        //[CLSCompliant(false)]
        public bool AsUInt64(out ulong ret) {
            ret = 0;
            if (sign == 0) return true;
            if (sign < 0) return false;
            if (GetLength() > 2) return false;
            ret = (ulong)data[0];
            if (data.Length > 1) {
                ret |= ((ulong)data[1]) << 32;
            }
            return true;
        }

        public bool AsInt32(out int ret) {
            ret = 0;
            if (sign == 0) return true;
            if (GetLength() > 1) return false;
            if (data[0] > 0x80000000) return false;
            if (data[0] == 0x80000000 && sign == 1) return false;
            ret = (int)data[0];
            ret *= sign;
            return true;
        }

        public bool AsDecimal(out Decimal ret) {
            if (sign == 0) {
                ret = Decimal.Zero;
                return true;
            }

            int length = GetLength();
            if (length > 3) {
                ret = default(Decimal);
                return false;
            }

            int lo = 0, mi = 0, hi = 0;
            if (length > 2) hi = (Int32)data[2];
            if (length > 1) mi = (Int32)data[1];
            if (length > 0) lo = (Int32)data[0];

            ret = new Decimal(lo, mi, hi, sign < 0, 0);
            return true;
        }


        //[CLSCompliant(false)]
        public uint ToUInt32() {
            uint ret;
            if (AsUInt32(out ret)) return ret;
            throw new OverflowException("big integer won't fit into uint");
        }

        public int ToInt32() {
            int ret;
            if (AsInt32(out ret)) return ret;
            throw new OverflowException("big integer won't fit into int");
        }

        public decimal ToDecimal() {
            decimal ret;
            if (AsDecimal(out ret)) return ret;
            throw new OverflowException("big integer won't fit into decimal");
        }

        //[CLSCompliant(false)]
        public ulong ToUInt64() {
            ulong ret;
            if (AsUInt64(out ret)) return ret;
            throw new OverflowException("big integer won't fit into ulong");
        }

        public long ToInt64() {
            long ret;
            if (AsInt64(out ret)) return ret;
            throw new OverflowException("big integer won't fit into long");
        }

        public int GetWordCount() {
            if (IsZero()) {
                return 1;
            }
            return GetLength();
        }

        private int GetLength() {
            return GetLength(data);
        }

        private static int GetLength(uint[] data) {
            int ret = data.Length - 1;
            while (ret >= 0 && data[ret] == 0) ret--;
            return ret + 1;
        }


        private static uint[] copy(uint[] v) {
            uint[] ret = new uint[v.Length];
            Array.Copy(v, ret, v.Length);
            return ret;
        }

        private static uint[] resize(uint[] v, int len) {
            if (v.Length == len) return v;
            uint[] ret = new uint[len];
            int n = System.Math.Min(v.Length, len);
            for (int i = 0; i < n; i++) {
                ret[i] = v[i];
            }
            return ret;
        }

        private static uint[] InternalAdd(uint[] x, int xl, uint[] y, int yl) {
            Debug.Assert(xl >= yl);
            uint[] z = new uint[xl];

            int i;
            ulong sum = 0;
            for (i = 0; i < yl; i++) {
                sum = sum + x[i] + y[i];
                z[i] = (uint)sum;
                sum >>= BitsPerDigit;
            }

            for (; i < xl && sum != 0; i++) {
                sum = sum + x[i];
                z[i] = (uint)sum;
                sum >>= BitsPerDigit;
            }
            if (sum != 0) {
                z = resize(z, xl + 1);
                z[i] = (uint)sum;
            } else {
                for (; i < xl; i++) {
                    z[i] = x[i];
                }
            }
            return z;
        }

        private static uint[] sub(uint[] x, int xl, uint[] y, int yl) {
            Debug.Assert(xl >= yl);
            uint[] z = new uint[xl];

            int i;
            bool borrow = false;
            for (i = 0; i < yl; i++) {
                uint xi = x[i];
                uint yi = y[i];
                if (borrow) {
                    if (xi == 0) {
                        xi = 0xffffffff;
                        borrow = true;
                    } else {
                        xi -= 1;
                        borrow = false;
                    }
                }
                if (yi > xi) borrow = true;
                z[i] = xi - yi;
            }

            if (borrow) {
                for (; i < xl; i++) {
                    uint xi = x[i];
                    z[i] = xi - 1;
                    if (xi != 0) { i++; break; }
                }
            }
            for (; i < xl; i++) {
                z[i] = x[i];
            }
            return z;  // may have leading zeros
        }

        private static uint[] add0(uint[] x, int xl, uint[] y, int yl) {
            if (xl >= yl) return InternalAdd(x, xl, y, yl);
            else return InternalAdd(y, yl, x, xl);
        }

        public static int Compare(BigInteger x, BigInteger y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentNullException("y");
            }
            if (x.sign == y.sign) {
                int xl = x.GetLength();
                int yl = y.GetLength();
                if (xl == yl) {
                    for (int i = xl - 1; i >= 0; i--) {
                        if (x.data[i] == y.data[i]) continue;
                        return x.data[i] > y.data[i] ? x.sign : -x.sign;
                    }
                    return 0;
                } else {
                    return xl > yl ? +x.sign : -x.sign;
                }
            } else {
                return x.sign > y.sign ? +1 : -1;
            }
        }

        public static bool operator ==(BigInteger x, int y) {
            return x == (BigInteger)y;
        }

        public static bool operator !=(BigInteger x, int y) {
            return !(x == y);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")] // TODO: fix
        public static bool operator ==(BigInteger x, double y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }

            // we can hold all double values, but not all double values
            // can hold BigInteger values, and we may lose precision.  Convert
            // the double to a big int, then compare.

            if ((y % 1) != 0) return false;  // not a whole number, can't be equal

            return x == BigInteger.Create(y);
        }

        public static bool operator ==(double x, BigInteger y) {
            return y == x;
        }

        public static bool operator !=(BigInteger x, double y) {
            return !(x == y);
        }

        public static bool operator !=(double x, BigInteger y) {
            return !(x == y);
        }


        public static bool operator ==(BigInteger x, BigInteger y) {
            return Compare(x, y) == 0;
        }

        public static bool operator !=(BigInteger x, BigInteger y) {
            return Compare(x, y) != 0;
        }
        public static bool operator <(BigInteger x, BigInteger y) {
            return Compare(x, y) < 0;
        }
        public static bool operator <=(BigInteger x, BigInteger y) {
            return Compare(x, y) <= 0;
        }
        public static bool operator >(BigInteger x, BigInteger y) {
            return Compare(x, y) > 0;
        }
        public static bool operator >=(BigInteger x, BigInteger y) {
            return Compare(x, y) >= 0;
        }

        public static BigInteger Add(BigInteger x, BigInteger y) {
            return x + y;
        }

        public static BigInteger operator +(BigInteger x, BigInteger y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentNullException("y");
            }

            if (x.sign == y.sign) {
                return new BigInteger(x.sign, add0(x.data, x.GetLength(), y.data, y.GetLength()));
            } else {
                return x - new BigInteger(-y.sign, y.data);  //??? performance issue
            }
        }

        public static BigInteger Subtract(BigInteger x, BigInteger y) {
            return x - y;
        }

        public static BigInteger operator -(BigInteger x, BigInteger y) {
            int c = Compare(x, y);
            if (c == 0) return Zero;

            if (x.sign == y.sign) {
                uint[] z;
                switch (c * x.sign) {
                    case +1:
                        z = sub(x.data, x.GetLength(), y.data, y.GetLength());
                        break;
                    case -1:
                        z = sub(y.data, y.GetLength(), x.data, x.GetLength());
                        break;
                    default:
                        return Zero;
                }
                return new BigInteger(c, z);
            } else {
                uint[] z = add0(x.data, x.GetLength(), y.data, y.GetLength());
                return new BigInteger(c, z);
            }
        }

        public static BigInteger Multiply(BigInteger x, BigInteger y) {
            return x * y;
        }

        public static BigInteger operator *(BigInteger x, BigInteger y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentNullException("y");
            }
            int xl = x.GetLength();
            int yl = y.GetLength();
            int zl = xl + yl;
            uint[] xd = x.data, yd = y.data, zd = new uint[zl];

            for (int xi = 0; xi < xl; xi++) {
                uint xv = xd[xi];
                int zi = xi;
                ulong carry = 0;
                for (int yi = 0; yi < yl; yi++) {
                    carry = carry + ((ulong)xv) * yd[yi] + zd[zi];
                    zd[zi++] = (uint)carry;
                    carry >>= BitsPerDigit;
                }
                while (carry != 0) {
                    carry += zd[zi];
                    zd[zi++] = (uint)carry;
                    carry >>= BitsPerDigit;
                }
            }

            return new BigInteger(x.sign * y.sign, zd);
        }

        public static BigInteger Divide(BigInteger x, BigInteger y) {
            return x / y;
        }

        public static BigInteger operator /(BigInteger x, BigInteger y) {
            BigInteger dummy;
            return DivRem(x, y, out dummy);
        }

        public static BigInteger Mod(BigInteger x, BigInteger y) {
            return x % y;
        }

        public static BigInteger operator %(BigInteger x, BigInteger y) {
            BigInteger ret;
            DivRem(x, y, out ret);
            return ret;
        }

        private static int GetNormalizeShift(uint value) {
            int shift = 0;

            if ((value & 0xFFFF0000) == 0) { value <<= 16; shift += 16; }
            if ((value & 0xFF000000) == 0) { value <<= 8; shift += 8; }
            if ((value & 0xF0000000) == 0) { value <<= 4; shift += 4; }
            if ((value & 0xC0000000) == 0) { value <<= 2; shift += 2; }
            if ((value & 0x80000000) == 0) { value <<= 1; shift += 1; }

            return shift;
        }

        [Conditional("DEBUG")]
        [SuppressMessage("Microsoft.Usage", "CA1806:DoNotIgnoreMethodResults", MessageId = "Microsoft.Scripting.Math.BigInteger")]
        private static void TestNormalize(uint[] u, uint[] un, int shift) {
            BigInteger i = new BigInteger(1, u);
            BigInteger j = new BigInteger(1, un);
            BigInteger k = j >> shift;

            Debug.Assert(i == k);
        }

        [Conditional("DEBUG")]
        private static void TestDivisionStep(uint[] un, uint[] vn, uint[] q, uint[] u, uint[] v) {
            int n = GetLength(v);
            int shift = GetNormalizeShift(v[n - 1]);

            BigInteger uni = new BigInteger(1, un);
            BigInteger vni = new BigInteger(1, vn);
            BigInteger qi = new BigInteger(1, q);
            BigInteger ui = new BigInteger(1, u);

            BigInteger expected = vni * qi + uni;
            BigInteger usi = ui << shift;

            Debug.Assert(expected == usi);
        }

        [Conditional("DEBUG")]
        [SuppressMessage("Microsoft.Usage", "CA1806:DoNotIgnoreMethodResults", MessageId = "Microsoft.Scripting.Math.BigInteger")]
        private static void TestResult(uint[] u, uint[] v, uint[] q, uint[] r) {
            BigInteger ui = new BigInteger(1, u);
            BigInteger vi = new BigInteger(1, v);
            BigInteger qi = new BigInteger(1, q);
            BigInteger ri = new BigInteger(1, r);

            BigInteger viqi = vi * qi;
            BigInteger expected = viqi + ri;
            Debug.Assert(ui == expected);
            Debug.Assert(ri < vi);
        }

        private static void Normalize(uint[] u, int l, uint[] un, int shift) {
            Debug.Assert(un.Length == l || un.Length == l + 1);
            Debug.Assert(un.Length == l + 1 || ((u[l - 1] << shift) >> shift) == u[l - 1]);
            Debug.Assert(0 <= shift && shift < 32);

            uint carry = 0;
            int i;
            if (shift > 0) {
                int rshift = BitsPerDigit - shift;
                for (i = 0; i < l; i++) {
                    uint ui = u[i];
                    un[i] = (ui << shift) | carry;
                    carry = ui >> rshift;
                }
            } else {
                for (i = 0; i < l; i++) {
                    un[i] = u[i];
                }
            }

            while (i < un.Length) {
                un[i++] = 0;
            }

            if (carry != 0) {
                Debug.Assert(l == un.Length - 1);
                un[l] = carry;
            }

            TestNormalize(u, un, shift);
        }

        private static void Unnormalize(uint[] un, out uint[] r, int shift) {
            Debug.Assert(0 <= shift && shift < 32);

            int length = GetLength(un);
            r = new uint[length];

            if (shift > 0) {
                int lshift = 32 - shift;
                uint carry = 0;
                for (int i = length - 1; i >= 0; i--) {
                    uint uni = un[i];
                    r[i] = (uni >> shift) | carry;
                    carry = (uni << lshift);
                }
            } else {
                for (int i = 0; i < length; i++) {
                    r[i] = un[i];
                }
            }

            TestNormalize(r, un, shift);
        }

        private static void DivModUnsigned(uint[] u, uint[] v, out uint[] q, out uint[] r) {
            int m = GetLength(u);
            int n = GetLength(v);

            if (n <= 1) {
                if (n == 0) {
                    throw new DivideByZeroException();
                }

                //  Divide by single digit
                //
                ulong rem = 0;
                uint v0 = v[0];
                q = new uint[m];
                r = new uint[1];

                for (int j = m - 1; j >= 0; j--) {
                    rem *= Base;
                    rem += u[j];

                    ulong div = rem / v0;
                    rem -= div * v0;
                    q[j] = (uint)div;
                }
                r[0] = (uint)rem;
            } else if (m >= n) {
                int shift = GetNormalizeShift(v[n - 1]);

                uint[] un = new uint[m + 1];
                uint[] vn = new uint[n];

                Normalize(u, m, un, shift);
                Normalize(v, n, vn, shift);

                q = new uint[m - n + 1];
                r = null;

                TestDivisionStep(un, vn, q, u, v);

                //  Main division loop
                //
                for (int j = m - n; j >= 0; j--) {
                    ulong rr, qq;
                    int i;

                    rr = Base * un[j + n] + un[j + n - 1];
                    qq = rr / vn[n - 1];
                    rr -= qq * vn[n - 1];

                    Debug.Assert((Base * un[j + n] + un[j + n - 1]) == qq * vn[n - 1] + rr);

                    for (; ; ) {
                        // Estimate too big ?
                        //
                        if ((qq >= Base) || (qq * vn[n - 2] > (rr * Base + un[j + n - 2]))) {
                            qq--;
                            rr += (ulong)vn[n - 1];
                            if (rr < Base) continue;
                        }
                        break;
                    }

                    Debug.Assert((Base * un[j + n] + un[j + n - 1]) == qq * vn[n - 1] + rr);

                    //  Multiply and subtract
                    //
                    long b = 0;
                    long t = 0;
                    for (i = 0; i < n; i++) {
                        ulong p = vn[i] * qq;
                        t = (long)un[i + j] - (long)(uint)p - b;
                        un[i + j] = (uint)t;
                        p >>= 32;
                        t >>= 32;
                        Debug.Assert(t == 0 || t == -1 || t == -2);
                        b = (long)p - t;
                    }
                    t = (long)un[j + n] - b;
                    un[j + n] = (uint)t;

                    //  Store the calculated value
                    //
                    q[j] = (uint)qq;

                    //  Add back vn[0..n] to un[j..j+n]
                    //
                    if (t < 0) {
                        q[j]--;
                        ulong c = 0;
                        for (i = 0; i < n; i++) {
                            c = (ulong)vn[i] + un[j + i] + c;
                            un[j + i] = (uint)c;
                            c >>= 32;
                        }
                        c += (ulong)un[j + n];
                        un[j + n] = (uint)c;
                    }

                    TestDivisionStep(un, vn, q, u, v);
                }

                Unnormalize(un, out r, shift);

                //  Test normalized value ... Call TestNormalize
                //  only pass the values in different order.
                //
                TestNormalize(r, un, shift);
            } else {
                q = new uint[] { 0 };
                r = u;
            }

            TestResult(u, v, q, r);
        }

        public static BigInteger DivRem(BigInteger x, BigInteger y, out BigInteger remainder) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentNullException("y");
            }

            uint[] q;
            uint[] r;

            DivModUnsigned(x.data, y.data, out q, out r);

            remainder = new BigInteger(x.sign, r);
            return new BigInteger(x.sign * y.sign, q);
        }

        private static uint extend(uint v, ref bool seenNonZero) {
            if (seenNonZero) {
                return ~v;
            } else {
                if (v == 0) {
                    return 0;
                } else {
                    seenNonZero = true;
                    return ~v + 1;
                }
            }
        }

        private static uint getOne(bool isNeg, uint[] data, int i, ref bool seenNonZero) {
            if (i < data.Length) {
                uint ret = data[i];
                return isNeg ? extend(ret, ref seenNonZero) : ret;
            } else {
                return isNeg ? uint.MaxValue : 0;
            }
        }

        /// <summary>
        /// Do an in-place twos complement of d and also return the result.
        /// </summary>
        private static uint[] makeTwosComplement(uint[] d) {
            // first do complement and +1 as long as carry is needed
            int i = 0;
            uint v = 0;
            for (; i < d.Length; i++) {
                v = ~d[i] + 1;
                d[i] = v;
                if (v != 0) { i++; break; }
            }

            if (v != 0) {
                // now ones complement is sufficient
                for (; i < d.Length; i++) {
                    d[i] = ~d[i];
                }
            } else {
                //??? this is weird
                d = resize(d, d.Length + 1);
                d[d.Length - 1] = 1;
            }
            return d;
        }

        public static BigInteger BitwiseAnd(BigInteger x, BigInteger y) {
            return x & y;
        }

        public static BigInteger operator &(BigInteger x, BigInteger y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentNullException("y");
            }
            int xl = x.GetLength(), yl = y.GetLength();
            uint[] xd = x.data, yd = y.data;

            int zl = System.Math.Max(xl, yl);
            uint[] zd = new uint[zl];

            bool negx = x.sign == -1, negy = y.sign == -1;
            bool seenNonZeroX = false, seenNonZeroY = false;
            for (int i = 0; i < zl; i++) {
                uint xu = getOne(negx, xd, i, ref seenNonZeroX);
                uint yu = getOne(negy, yd, i, ref seenNonZeroY);
                zd[i] = xu & yu;
            }

            if (negx && negy) {

                return new BigInteger(-1, makeTwosComplement(zd));
            } else if (negx || negy) {
                return new BigInteger(+1, zd);
            } else {
                return new BigInteger(+1, zd);
            }
        }

        public static BigInteger BitwiseOr(BigInteger x, BigInteger y) {
            return x | y;
        }

        public static BigInteger operator |(BigInteger x, BigInteger y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentNullException("y");
            }
            int xl = x.GetLength(), yl = y.GetLength();
            uint[] xd = x.data, yd = y.data;

            int zl = System.Math.Max(xl, yl);
            uint[] zd = new uint[zl];

            bool negx = x.sign == -1, negy = y.sign == -1;
            bool seenNonZeroX = false, seenNonZeroY = false;
            for (int i = 0; i < zl; i++) {
                uint xu = getOne(negx, xd, i, ref seenNonZeroX);
                uint yu = getOne(negy, yd, i, ref seenNonZeroY);
                zd[i] = xu | yu;
            }

            if (negx && negy) {
                return new BigInteger(-1, makeTwosComplement(zd));
            } else if (negx || negy) {
                return new BigInteger(-1, makeTwosComplement(zd));
            } else {
                return new BigInteger(+1, zd);
            }
        }

        public static BigInteger Xor(BigInteger x, BigInteger y) {
            return x ^ y;
        }

        public static BigInteger operator ^(BigInteger x, BigInteger y) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (object.ReferenceEquals(y, null)) {
                throw new ArgumentNullException("y");
            }
            int xl = x.GetLength(), yl = y.GetLength();
            uint[] xd = x.data, yd = y.data;

            int zl = System.Math.Max(xl, yl);
            uint[] zd = new uint[zl];

            bool negx = x.sign == -1, negy = y.sign == -1;
            bool seenNonZeroX = false, seenNonZeroY = false;
            for (int i = 0; i < zl; i++) {
                uint xu = getOne(negx, xd, i, ref seenNonZeroX);
                uint yu = getOne(negy, yd, i, ref seenNonZeroY);
                zd[i] = xu ^ yu;
            }

            if (negx && negy) {
                return new BigInteger(+1, zd);
            } else if (negx || negy) {
                return new BigInteger(-1, makeTwosComplement(zd));
            } else {
                return new BigInteger(+1, zd);
            }
        }

        public static BigInteger LeftShift(BigInteger x, int shift) {
            return x << shift;
        }

        public static BigInteger operator <<(BigInteger x, int shift) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (shift == 0) return x;
            else if (shift < 0) return x >> -shift;

            int digitShift = shift / BitsPerDigit;
            int smallShift = shift - (digitShift * BitsPerDigit);

            int xl = x.GetLength();
            uint[] xd = x.data;
            int zl = xl + digitShift + 1;
            uint[] zd = new uint[zl];

            if (smallShift == 0) {
                for (int i = 0; i < xl; i++) {
                    zd[i + digitShift] = xd[i];
                }
            } else {
                int carryShift = BitsPerDigit - smallShift;
                uint carry = 0;
                int i;
                for (i = 0; i < xl; i++) {
                    uint rot = xd[i];
                    zd[i + digitShift] = rot << smallShift | carry;
                    carry = rot >> carryShift;
                }
                zd[i + digitShift] = carry;
            }
            return new BigInteger(x.sign, zd);
        }

        public static BigInteger RightShift(BigInteger x, int shift) {
            return x >> shift;
        }

        public static BigInteger operator >>(BigInteger x, int shift) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            if (shift == 0) return x;
            else if (shift < 0) return x << -shift;

            int digitShift = shift / BitsPerDigit;
            int smallShift = shift - (digitShift * BitsPerDigit);

            int xl = x.GetLength();
            uint[] xd = x.data;
            int zl = xl - digitShift;
            if (zl < 0) zl = 0;
            uint[] zd = new uint[zl];

            if (smallShift == 0) {
                for (int i = xl - 1; i >= digitShift; i--) {
                    zd[i - digitShift] = xd[i];
                }
            } else {
                int carryShift = BitsPerDigit - smallShift;
                uint carry = 0;
                for (int i = xl - 1; i >= digitShift; i--) {
                    uint rot = xd[i];
                    zd[i - digitShift] = rot >> smallShift | carry;
                    carry = rot << carryShift;
                }
            }

            BigInteger res = new BigInteger(x.sign, zd);

            // We wish to always round down, but shifting our data array (which is not
            // stored in 2's-complement form) has caused us to round towards zero instead.
            // Correct that here.
            if (x.IsNegative()) {
                for (int i = 0; i < digitShift; i++) {
                    if (xd[i] != 0u) {
                        return res - One;
                    }
                }
                if (smallShift > 0 && xd[digitShift] << (BitsPerDigit - smallShift) != 0u) {
                    return res - One;
                }
            }

            return res;
        }

        public static BigInteger Negate(BigInteger x) {
            return -x;
        }

        public static BigInteger operator -(BigInteger x) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            return new BigInteger(-x.sign, x.data);
        }

        public BigInteger OnesComplement() {
            return ~this;
        }

        public static BigInteger operator ~(BigInteger x) {
            if (object.ReferenceEquals(x, null)) {
                throw new ArgumentNullException("x");
            }
            return -(x + One);
        }

        public BigInteger Abs() {
            if (this.sign == -1) return -this;
            else return this;
        }

        public BigInteger Power(int exp) {
            if (exp == 0) return One;
            if (exp < 0) {
                throw new ArgumentOutOfRangeException("exp", "exp must be >= 0");
            }
            BigInteger factor = this;
            BigInteger result = One;
            while (exp != 0) {
                if ((exp & 1) != 0) result = result * factor;
                if (exp == 1) break;  // avoid costly factor.square()
                factor = factor.Square();
                exp >>= 1;
            }
            return result;
        }

        public BigInteger ModPow(int power, BigInteger mod) {
            if (object.ReferenceEquals(mod, null)) {
                throw new ArgumentNullException("mod");
            }

            if (power < 0) {
                throw new ArgumentOutOfRangeException("power", "power must be >= 0");
            }
            BigInteger factor = this;
            BigInteger result = One % mod; // Handle special case of power=0, mod=1
            while (power != 0) {
                if ((power & 1) != 0) {
                    result = result * factor;
                    result = result % mod;
                }
                if (power == 1) break;  // avoid costly factor.Square()
                factor = factor.Square();
                factor = factor % mod;
                power >>= 1;
            }
            return result;
        }

        public BigInteger ModPow(BigInteger power, BigInteger mod) {
            if (object.ReferenceEquals(power, null)) {
                throw new ArgumentNullException("power");
            }
            if (object.ReferenceEquals(mod, null)) {
                throw new ArgumentNullException("mod");
            }

            if (power < 0) {
                throw new ArgumentOutOfRangeException("power", "power must be >= 0");
            }

            BigInteger factor = this;
            BigInteger result = One % mod;
            while (power != Zero) {
                if (power.IsOdd()) {
                    result = result * factor;
                    result = result % mod;
                }
                if (power == One) break;  // avoid costly factor.Square()
                factor = factor.Square();
                factor = factor % mod;
                power >>= 1;
            }
            return result;
        }

        public BigInteger Square() {
            return this * this;
        }

        //[Confined]
        public override string ToString() {
            return ToString(10);
        }

        //[Confined]
        public string ToString(int radix) {
            return MathUtils.BigIntegerToString(copy(data), sign, radix);
        }

        //[Confined]
        public override int GetHashCode() {
            // The Object.GetHashCode function needs to be consistent with the Object.Equals function.
            // Languages that build on top of this may have a more flexible equality function and 
            // so may not be able to use this hash function directly.
            // For example, Python allows BigInteger(10) == int32(10), so hashing a BigInt over the Int32
            // domain should return the same value as a hash of the Int32.

            // If this is in the int32 range, this hash function returns the integer.
            if (data.Length == 0) {
                return 0;
            }

            // Add up all uints. We want to incorporate all bits to get good hash distribution. 
            uint total = 0;
            foreach (uint x in data) {
                total = unchecked(total + x);
            }

            int hash = unchecked((int)total);

            // The sign is not part of the data array, so explicitly incorporate that.
            // This is also needed to ensure that hash(-x) == -x for int32.
            if (IsNegative()) {
                return unchecked(-hash);
            } else {
                return hash;
            }
        }

        //[Confined]
        public override bool Equals(object obj) {
            return Equals(obj as BigInteger);
        }

        public bool Equals(BigInteger other) {
            if (object.ReferenceEquals(other, null)) return false;
            return this == other;
        }


        public bool IsNegative() {
            return sign < 0;
        }

        public bool IsZero() {
            return sign == 0;
        }

        public bool IsPositive() {
            return sign > 0;
        }

        private bool IsOdd() {
            // must have the lowest-order bit set to 1
            return (data != null && data.Length > 0 && ((data[0] & 1) != 0));
        }


        public double Log(Double newBase) {
            if (IsNegative() || newBase == 1.0D || this == Zero || (newBase == 0.0D && this != One)) {
                return Double.NaN;
            } else if (newBase == Double.PositiveInfinity) {
                return this == One ? 0.0D : Double.NaN;
            }

            int length = GetLength() - 1;
            int bitCount = -1;
            for (int curBit = 31; curBit >= 0; curBit--) {
                if ((data[length] & (1 << curBit)) != 0) {
                    bitCount = curBit + length * 32;
                    break;
                }
            }

            long bitlen = bitCount;
            Double c = 0, d = 1;

            BigInteger testBit = BigInteger.One;
            long tempBitlen = bitlen;
            while (tempBitlen > Int32.MaxValue) {
                testBit = testBit << Int32.MaxValue;
                tempBitlen -= Int32.MaxValue;
            }
            testBit = testBit << (int)tempBitlen;

            for (long curbit = bitlen; curbit >= 0; --curbit) {
                if ((this & testBit) != BigInteger.Zero)
                    c += d;
                d *= 0.5;
                testBit = testBit >> 1;
            }
            return (System.Math.Log(c) + System.Math.Log(2) * bitlen) / System.Math.Log(newBase);
        }

        /// <summary>
        /// Calculates the natural logarithm of the BigInteger.
        /// </summary>
        public double Log() {
            return Log(System.Math.E);
        }

        /// <summary>
        /// Calculates log base 10 of a BigInteger.
        /// </summary>
        public double Log10() {
            return Log(10);
        }

        #region IComparable Members

        public int CompareTo(object obj) {
            if (obj == null) {
                return 1;
            }
            BigInteger o = obj as BigInteger;
            if (object.ReferenceEquals(o, null)) {
                throw new ArgumentException("expected integer");
            }
            return Compare(this, o);
        }

        #endregion

        #region IConvertible Members

        //[Confined]
        public TypeCode GetTypeCode() {
            return TypeCode.Object;
        }

        //[Confined]
        public bool ToBoolean(IFormatProvider provider) {
            return this != Zero;
        }

        //[Confined]
        public byte ToByte(IFormatProvider provider) {
            uint ret;
            if (AsUInt32(out ret) && (ret & ~0xFF) == 0) {
                return (byte)ret;
            }
            throw new OverflowException("big integer won't fit into byte");
        }

        /// <summary>
        /// Return the value of this BigInteger as a little-endian twos-complement
        /// byte array, using the fewest number of bytes possible. If the value is zero,
        /// return an array of one byte whose element is 0x00.
        /// </summary>
        public byte[] ToByteArray() {
            // We could probably make this more efficient by eliminating one of the passes.
            // The current code does one pass for uint array -> byte array conversion,
            // and then a another pass to remove unneeded bytes at the top of the array.
            if (0 == sign) return new byte[] { 0 };

            uint[] dwords;
            byte highByte;

            if (-1 == sign) {
                dwords = (uint[])this.data.Clone();
                makeTwosComplement(dwords);
                highByte = 0xff;
            } else {
                dwords = this.data;
                highByte = 0x00;
            }

            byte[] bytes = new byte[4 * dwords.Length];
            int curByte = 0;
            uint dword;
            for (int i = 0; i < dwords.Length; i++) {
                dword = dwords[i];
                for (int j = 0; j < 4; j++) {
                    bytes[curByte++] = (byte)(dword & 0xff);
                    dword >>= 8;
                }
            }

            // find highest significant byte
            int msb;
            for (msb = bytes.Length - 1; msb > 0; msb--) {
                if (bytes[msb] != highByte) break;
            }
            // ensure high bit is 0 if positive, 1 if negative
            bool needExtraByte = (bytes[msb] & 0x80) != (highByte & 0x80);

            byte[] trimmedBytes = new byte[msb + 1 + (needExtraByte ? 1 : 0)];
            Array.Copy(bytes, trimmedBytes, msb + 1);

            if (needExtraByte) trimmedBytes[trimmedBytes.Length - 1] = highByte;

            return trimmedBytes;
        }

        //[Confined]
        public char ToChar(IFormatProvider provider) {
            int ret;
            if (AsInt32(out ret) && (ret <= Char.MaxValue) && (ret >= Char.MinValue)) {
                return (char)ret;
            }
            throw new OverflowException("big integer won't fit into char");
        }

        //[Confined]
        public DateTime ToDateTime(IFormatProvider provider) {
            throw new NotImplementedException();
        }

        //[Confined]
        public decimal ToDecimal(IFormatProvider provider) {
            decimal ret;
            if (AsDecimal(out ret)) return ret;
            throw new OverflowException("big integer won't fit into decimal");
        }

        //[Confined]
        public double ToDouble(IFormatProvider provider) {
            return this.ToFloat64();
        }

        //[Confined]
        public short ToInt16(IFormatProvider provider) {
            int ret;
            if (AsInt32(out ret) && (ret <= short.MaxValue) && (ret >= short.MinValue)) {
                return (short)ret;
            }
            throw new OverflowException("big integer won't fit into short");
        }

        //[Confined]
        public int ToInt32(IFormatProvider provider) {
            int ret;
            if (AsInt32(out ret)) {
                return ret;
            }
            throw new OverflowException("big integer won't fit into int");
        }

        //[Confined]
        public long ToInt64(IFormatProvider provider) {
            long ret;
            if (AsInt64(out ret)) {
                return ret;
            }
            throw new OverflowException("big integer won't fit into long");
        }

        //[CLSCompliant(false), Confined]
        public sbyte ToSByte(IFormatProvider provider) {
            int ret;
            if (AsInt32(out ret) && (ret <= sbyte.MaxValue) && (ret >= sbyte.MinValue)) {
                return (sbyte)ret;
            }
            throw new OverflowException("big integer won't fit into sbyte");
        }

        //[Confined]
        public float ToSingle(IFormatProvider provider) {
            return checked((float)ToDouble(provider));
        }

        //[Confined]
        public string ToString(IFormatProvider provider) {
            return ToString();
        }

        //[Confined]
        public object ToType(Type conversionType, IFormatProvider provider) {
            if (conversionType == typeof(BigInteger)) {
                return this;
            }
            throw new NotImplementedException();
        }

        //[CLSCompliant(false), Confined]
        public ushort ToUInt16(IFormatProvider provider) {
            uint ret;
            if (AsUInt32(out ret) && ret <= ushort.MaxValue) {
                return (ushort)ret;
            }
            throw new OverflowException("big integer won't fit into ushort");
        }

        //[CLSCompliant(false), Confined]
        public uint ToUInt32(IFormatProvider provider) {
            uint ret;
            if (AsUInt32(out ret)) {
                return ret;
            }
            throw new OverflowException("big integer won't fit into uint");
        }

        //[CLSCompliant(false), Confined]
        public ulong ToUInt64(IFormatProvider provider) {
            ulong ret;
            if (AsUInt64(out ret)) {
                return ret;
            }
            throw new OverflowException("big integer won't fit into ulong");
        }

        #endregion

        #region IFormattable Members

        string IFormattable.ToString(string format, IFormatProvider formatProvider) {
            if (format == null) return this.ToString();

            switch (format[0]) {
                case 'd':
                case 'D':
                    if (format.Length > 1) {
                        int precision = Convert.ToInt32(format.Substring(1), CultureInfo.InvariantCulture.NumberFormat);
                        string baseStr = ToString(10);
                        if (baseStr.Length < precision) {
                            string additional = new String('0', precision - baseStr.Length);
                            if (baseStr[0] != '-') {
                                return additional + baseStr;
                            } else {
                                return "-" + additional + baseStr.Substring(1);
                            }
                        }
                        return baseStr;
                    }
                    return ToString(10);
                case 'x':
                case 'X':
                    StringBuilder res = new StringBuilder(ToString(16));
                    if (format[0] == 'x') {
                        for (int i = 0; i < res.Length; i++) {
                            if (res[i] >= 'A' && res[i] <= 'F') {
                                res[i] = Char.ToLower(res[i], CultureInfo.InvariantCulture);
                            }
                        }
                    }

                    if (format.Length > 1) {
                        int precision = Convert.ToInt32(format.Substring(1), CultureInfo.InvariantCulture.NumberFormat);
                        if (res.Length < precision) {
                            string additional = new String('0', precision - res.Length);
                            if (res[0] != '-') {
                                res.Insert(0, additional);
                            } else {
                                res.Insert(1, additional);
                            }
                        }
                    }

                    return res.ToString();
                default:
                    throw new NotImplementedException("format not implemented");
            }
        }

        #endregion        
    }

    public static class MathUtils {
        /// <summary>
        /// Calculates the quotient of two 32-bit signed integers rounded towards negative infinity.
        /// </summary>
        /// <param name="x">Dividend.</param>
        /// <param name="y">Divisor.</param>
        /// <returns>The quotient of the specified numbers rounded towards negative infinity, or <code>(int)Floor((double)x/(double)y)</code>.</returns>
        /// <exception cref="DivideByZeroException"><paramref name="y"/> is 0.</exception>
        /// <remarks>The caller must check for overflow (x = Int32.MinValue, y = -1)</remarks>
        public static int FloorDivideUnchecked(int x, int y) {
            int q = x / y;

            if (x >= 0) {
                if (y > 0) {
                    return q;
                } else if (x % y == 0) {
                    return q;
                } else {
                    return q - 1;
                }
            } else {
                if (y > 0) {
                    if (x % y == 0) {
                        return q;
                    } else {
                        return q - 1;
                    }
                } else {
                    return q;
                }
            }
        }

        /// <summary>
        /// Calculates the quotient of two 32-bit signed integers rounded towards negative infinity.
        /// </summary>
        /// <param name="x">Dividend.</param>
        /// <param name="y">Divisor.</param>
        /// <returns>The quotient of the specified numbers rounded towards negative infinity, or <code>(int)Floor((double)x/(double)y)</code>.</returns>
        /// <exception cref="DivideByZeroException"><paramref name="y"/> is 0.</exception>
        /// <remarks>The caller must check for overflow (x = Int64.MinValue, y = -1)</remarks>
        public static long FloorDivideUnchecked(long x, long y) {
            long q = x / y;

            if (x >= 0) {
                if (y > 0) {
                    return q;
                } else if (x % y == 0) {
                    return q;
                } else {
                    return q - 1;
                }
            } else {
                if (y > 0) {
                    if (x % y == 0) {
                        return q;
                    } else {
                        return q - 1;
                    }
                } else {
                    return q;
                }
            }
        }

        /// <summary>
        /// Calculates the remainder of floor division of two 32-bit signed integers.
        /// </summary>
        /// <param name="x">Dividend.</param>
        /// <param name="y">Divisor.</param>
        /// <returns>The remainder of of floor division of the specified numbers, or <code>x - (int)Floor((double)x/(double)y) * y</code>.</returns>
        /// <exception cref="DivideByZeroException"><paramref name="y"/> is 0.</exception>
        public static int FloorRemainder(int x, int y) {
            if (y == -1) return 0;
            int r = x % y;

            if (x >= 0) {
                if (y > 0) {
                    return r;
                } else if (r == 0) {
                    return 0;
                } else {
                    return r + y;
                }
            } else {
                if (y > 0) {
                    if (r == 0) {
                        return 0;
                    } else {
                        return r + y;
                    }
                } else {
                    return r;
                }
            }
        }

        /// <summary>
        /// Calculates the remainder of floor division of two 32-bit signed integers.
        /// </summary>
        /// <param name="x">Dividend.</param>
        /// <param name="y">Divisor.</param>
        /// <returns>The remainder of of floor division of the specified numbers, or <code>x - (int)Floor((double)x/(double)y) * y</code>.</returns>
        /// <exception cref="DivideByZeroException"><paramref name="y"/> is 0.</exception>
        public static long FloorRemainder(long x, long y) {
            if (y == -1) return 0;
            long r = x % y;

            if (x >= 0) {
                if (y > 0) {
                    return r;
                } else if (r == 0) {
                    return 0;
                } else {
                    return r + y;
                }
            } else {
                if (y > 0) {
                    if (r == 0) {
                        return 0;
                    } else {
                        return r + y;
                    }
                } else {
                    return r;
                }
            }
        }

        /// <summary>
        /// Behaves like Math.Round(value, MidpointRounding.AwayFromZero)
        /// Needed because CoreCLR doesn't support this particular overload of Math.Round
        /// </summary>
        public static double RoundAwayFromZero(double value) {
#if !SILVERLIGHT
            return Math.Round(value, MidpointRounding.AwayFromZero);
#else
            if (value < 0) {
                return -RoundAwayFromZero(-value);
            }
        
            // we can assume positive value
            double result = Math.Floor(value);
            if (value - result >= 0.5) {
                result += 1.0;
            }
            return result;
#endif
        }

        private static readonly double[] _RoundPowersOfTens = new double[] { 1E0, 1E1, 1E2, 1E3, 1E4, 1E5, 1E6, 1E7, 1E8, 1E9, 1E10, 1E11, 1E12, 1E13, 1E14, 1E15 };

        private static double GetPowerOf10(int precision) {
            return (precision < 16) ? _RoundPowersOfTens[precision] : Math.Pow(10, precision);
        }

        /// <summary>
        /// Behaves like Math.Round(value, precision, MidpointRounding.AwayFromZero)
        /// However, it works correctly on negative precisions and cases where precision is
        /// outside of the [-15, 15] range.
        /// 
        /// (This function is also needed because CoreCLR lacks this overload.)
        /// </summary>
        public static double RoundAwayFromZero(double value, int precision) {
            if (precision >= 0) {
                double num = GetPowerOf10(precision);
                return RoundAwayFromZero(value * num) / num;
            } else {
                // Note: this code path could be merged with the precision >= 0 path,
                // (by extending the cache to negative powers of 10)
                // but the results seem to be more precise if we do it this way
                double num = GetPowerOf10(-precision);
                return RoundAwayFromZero(value / num) * num;
            }
        }

        public static bool IsNegativeZero(double self) {
#if SILVERLIGHT // BitConverter.DoubleToInt64Bits
            if ( self != 0.0 ) {
              return false;
            }
            byte[] bits = BitConverter.GetBytes(self);
            return (bits[7] == 0x80 && bits[6] == 0x00 && bits[5] == 0x00 && bits[4] == 0x00
                && bits[3] == 0x00 && bits[2] == 0x00 && bits[1] == 0x00 && bits[0] == 0x00);
#else
            return (self == 0.0 && 1.0 / self < 0);
#endif
        }

        #region Special Functions

        public static double Erf(double v0) {
            // Calculate the error function using the approximation method outlined in
            // W. J. Cody's "Rational Chebyshev Approximations for the Error Function"

            if (v0 >= 10.0) {
                return 1.0;
            } else if (v0 <= -10.0) {
                return -1.0;
            }

            if (v0 > 0.47 || v0 < -0.47) {
                return 1.0 - ErfComplement(v0);
            }

            double sq = v0 * v0;
            double numer = EvalPolynomial(sq, ErfNumerCoeffs);
            double denom = EvalPolynomial(sq, ErfDenomCoeffs);

            return v0 * numer / denom;
        }

        public static double ErfComplement(double v0) {
            if (v0 >= 30.0) {
                return 0.0;
            } else if (v0 <= -10.0) {
                return 2.0;
            }

            double a = Math.Abs(v0);
            if (a < 0.47) {
                return 1.0 - Erf(v0);
            }

            // Different approximations are required for different ranges of v0
            double res;
            if (a <= 4.0) {
                // Use the approximation method outlined in W. J. Cody's "Rational Chebyshev
                // Approximations for the Error Function"
                double numer = EvalPolynomial(a, ErfcNumerCoeffs);
                double denom = EvalPolynomial(a, ErfcDenomCoeffs);

                res = Math.Exp(-a * a) * numer / denom;
            } else {
                // Use the approximation method introduced by C. Tellambura and A. Annamalai
                // in "Efficient Computation of erfc(x) for Large Arguments"
                const double h = 0.5;
                const double hSquared = 0.25;
                const int nTerms = 10;
                double sq = a * a;
                res = 0.0;
                for (int i = nTerms; i > 0; i--) {
                    double term = i * i * hSquared;
                    res += Math.Exp(-term) / (term + sq);
                }

                res = h * a * Math.Exp(-sq) / Math.PI * (res * 2 + 1.0 / sq);
            }

            if (v0 < 0.0) {
                res = 2.0 - res;
            }
            return res;
        }

        public static double Gamma(double v0) {
            // Calculate the Gamma function using the Lanczos approximation

            if (double.IsNegativeInfinity(v0)) {
                return double.NaN;
            }
            double a = Math.Abs(v0);

            // Special-case integers
            if (a % 1.0 == 0.0) {
                // Gamma is undefined on non-positive integers
                if (v0 <= 0.0) {
                    return double.NaN;
                }

                // factorial(v0 - 1)
                if (a <= 25.0) {
                    if (a <= 2.0) {
                        return 1.0;
                    }
                    a -= 1.0;
                    v0 -= 1.0;
                    while (--v0 > 1.0) {
                        a *= v0;
                    }
                    return a;
                }
            }

            // lim(Gamma(v0)) = 1.0 / v0 as v0 approaches 0.0
            if (a < 1e-50) {
                return 1.0 / v0;
            }

            double res;
            if (v0 < -150.0) {
                // If Gamma(1 - v0) could overflow for large v0, use the duplication formula to
                // compute Gamma(1 - v0):
                //     Gamma(x) * Gamma(x + 0,5) = sqrt(pi) * 2**(1 - 2x) * Gamma(2x)
                // ==> Gamma(1 - x) = Gamma((1-x)/2) * Gamma((2-x)/2) / (2**x * sqrt(pi))
                // Then apply the reflection formula:
                //     Gamma(x) = pi / sin(pi * x) / Gamma(1 - x)
                double halfV0 = v0 / 2.0;
                res = Math.Pow(Math.PI, 1.5) / SinPi(v0);
                res *= Math.Pow(2.0, v0);
                res /= PositiveGamma(0.5 - halfV0);
                res /= PositiveGamma(1.0 - halfV0);
            } else if (v0 < 0.001) {
                // For values less than or close to zero, just use the reflection formula
                res = Math.PI / SinPi(v0);
                double v1 = 1.0 - v0;
                if (v0 == 1.0 - v1) {
                    res /= PositiveGamma(v1);
                } else {
                    // Computing v1 has resulted in a loss of precision. To avoid this, use the
                    // recurrence relation Gamma(x + 1) = x * Gamma(x).
                    res /= -v0 * PositiveGamma(-v0);
                }
            } else {
                res = PositiveGamma(v0);
            }

            return res;
        }

        public static double LogGamma(double v0) {
            // Calculate the log of the Gamma function using the Lanczos approximation

            if (double.IsInfinity(v0)) {
                return double.PositiveInfinity;
            }
            double a = Math.Abs(v0);

            // Gamma is undefined on non-positive integers
            if (v0 <= 0.0 && a % 1.0 == 0.0) {
                return double.NaN;
            }

            // lim(LGamma(v0)) = -log|v0| as v0 approaches 0.0
            if (a < 1e-50) {
                return -Math.Log(a);
            }

            double res;
            if (v0 < 0.0) {
                // For negative values, use the reflection formula:
                //     Gamma(x) = pi / sin(pi * x) / Gamma(1 - x)
                // ==> LGamma(x) = log(pi / |sin(pi * x)|) - LGamma(1 - x)
                res = Math.Log(Math.PI / AbsSinPi(v0));
                res -= PositiveLGamma(1.0 - v0);
            } else {
                res = PositiveLGamma(v0);
            }

            return res;
        }

        public static double Hypot(double x, double y) {
            //
            // sqrt(x*x + y*y) == sqrt(x*x * (1 + (y*y)/(x*x))) ==
            // sqrt(x*x) * sqrt(1 + (y/x)*(y/x)) ==
            // abs(x) * sqrt(1 + (y/x)*(y/x))
            //

            // Handle infinities
            if (double.IsInfinity(x) || double.IsInfinity(y)) {
                return double.PositiveInfinity;
            }

            //  First, get abs
            if (x < 0.0) x = -x;
            if (y < 0.0) y = -y;

            // Obvious cases
            if (x == 0.0) return y;
            if (y == 0.0) return x;

            // Divide smaller number by bigger number to safeguard the (y/x)*(y/x)
            if (x < y) {
                double temp = y; y = x; x = temp;
            }

            y /= x;

            // calculate abs(x) * sqrt(1 + (y/x)*(y/x))
            return x * System.Math.Sqrt(1 + y * y);
        }

        /// <summary>
        /// Evaluates a polynomial in v0 where the coefficients are ordered in increasing degree
        /// </summary>
        private static double EvalPolynomial(double v0, double[] coeffs) {
            double res = 0.0;
            for (int i = coeffs.Length - 1; i >= 0; i--) {
                res = checked(res * v0 + coeffs[i]);
            }

            return res;
        }

        /// <summary>
        /// Evaluates a polynomial in v0 where the coefficients are ordered in increasing degree
        /// if reverse is false, and increasing degree if reverse is true.
        /// </summary>
        private static double EvalPolynomial(double v0, double[] coeffs, bool reverse) {
            if (!reverse) {
                return EvalPolynomial(v0, coeffs);
            }

            double res = 0.0;
            for (int i = 0; i < coeffs.Length; i++) {
                res = checked(res * v0 + coeffs[i]);
            }

            return res;
        }

        /// <summary>
        /// A numerically precise version of sin(v0 * pi)
        /// </summary>
        private static double SinPi(double v0) {
            double res = Math.Abs(v0) % 2.0;

            if (res < 0.25) {
                res = Math.Sin(res * Math.PI);
            } else if (res < 0.75) {
                res = Math.Cos((res - 0.5) * Math.PI);
            } else if (res < 1.25) {
                res = -Math.Sin((res - 1.0) * Math.PI);
            } else if (res < 1.75) {
                res = -Math.Cos((res - 1.5) * Math.PI);
            } else {
                res = Math.Sin((res - 2.0) * Math.PI);
            }

            return v0 < 0 ? -res : res;
        }

        /// <summary>
        /// A numerically precise version of |sin(v0 * pi)|
        /// </summary>
        private static double AbsSinPi(double v0) {
            double res = Math.Abs(v0) % 1.0;

            if (res < 0.25) {
                res = Math.Sin(res * Math.PI);
            } else if (res < 0.75) {
                res = Math.Cos((res - 0.5) * Math.PI);
            } else {
                res = Math.Sin((res - 1.0) * Math.PI);
            }

            return Math.Abs(res);
        }

        // polynomial coefficients ordered by increasing degree
        private static double[] ErfNumerCoeffs = {
            2.4266795523053175e02, 2.1979261618294152e01,
            6.9963834886191355, -3.5609843701815385e-02
        };
        private static double[] ErfDenomCoeffs = {
            2.1505887586986120e02, 9.1164905404514901e01,
            1.5082797630407787e01, 1.0
        };
        private static double[] ErfcNumerCoeffs = {
            3.004592610201616005e02, 4.519189537118729422e02,
            3.393208167343436870e02, 1.529892850469404039e02,
            4.316222722205673530e01, 7.211758250883093659,
            5.641955174789739711e-01, -1.368648573827167067e-07
        };
        private static double[] ErfcDenomCoeffs = {
            3.004592609569832933e02, 7.909509253278980272e02,
            9.313540948506096211e02, 6.389802644656311665e02,
            2.775854447439876434e02, 7.700015293522947295e01,
            1.278272731962942351e01, 1.0
        };
        private static double[] GammaNumerCoeffs = {
            4.401213842800460895436e13, 4.159045335859320051581e13,
            1.801384278711799677796e13, 4.728736263475388896889e12,
            8.379100836284046470415e11, 1.055837072734299344907e11,
            9.701363618494999493386e09, 6.549143975482052641016e08,
            3.223832294213356530668e07, 1.128514219497091438040e06,
            2.666579378459858944762e04, 3.818801248632926870394e02,
            2.506628274631000502415
        };
        private static double[] GammaDenomCoeffs = {
            0.0, 39916800.0, 120543840.0, 150917976.0,
            105258076.0, 45995730.0, 13339535.0, 2637558.0,
            357423.0, 32670.0, 1925.0, 66.0, 1.0
        };

        /// <summary>
        /// Take the quotient of the 2 polynomials forming the Lanczos approximation
        /// with N=13 and G=13.144565
        /// </summary>
        private static double GammaRationalFunc(double v0) {
            double numer = 0.0;
            double denom = 0.0;

            if (v0 < 1e15) {
                numer = EvalPolynomial(v0, GammaNumerCoeffs);
                denom = EvalPolynomial(v0, GammaDenomCoeffs);
            } else {
                double vRecip = 1.0 / v0;
                numer = EvalPolynomial(vRecip, GammaNumerCoeffs, true);
                denom = EvalPolynomial(vRecip, GammaDenomCoeffs, true);
            }

            return numer / denom;
        }

        /// <summary>
        /// Computes the Gamma function on positive values, using the Lanczos approximation.
        /// Lanczos parameters are N=13 and G=13.144565.
        /// </summary>
        private static double PositiveGamma(double v0) {
            if (v0 > 200.0) {
                return Double.PositiveInfinity;
            }

            double vg = v0 + 12.644565; // v0 + g - 0.5
            double res = GammaRationalFunc(v0);
            res /= Math.Exp(vg);
            if (v0 < 120.0) {
                res *= Math.Pow(vg, v0 - 0.5);
            } else {
                // Use a smaller exponent if we're in danger of overflowing Math.Pow
                double sqrt = Math.Pow(vg, v0 / 2.0 - 0.25);
                res *= sqrt;
                res *= sqrt;
            }

            return res;
        }

        /// <summary>
        /// Computes the Log-Gamma function on positive values, using the Lanczos approximation.
        /// Lanczos parameters are N=13 and G=13.144565.
        /// </summary>
        private static double PositiveLGamma(double v0) {
            double vg = v0 + 12.644565; // v0 + g - 0.5
            double res = Math.Log(GammaRationalFunc(v0)) - vg;
            res += (v0 - 0.5) * Math.Log(vg);

            return res;
        }

        #endregion

        #region BigInteger

        // generated by scripts/radix_generator.py
        private static readonly uint[] maxCharsPerDigit = { 0, 0, 31, 20, 15, 13, 12, 11, 10, 10, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 };
        private static readonly uint[] groupRadixValues = { 0, 0, 2147483648, 3486784401, 1073741824, 1220703125, 2176782336, 1977326743, 1073741824, 3486784401, 1000000000, 2357947691, 429981696, 815730721, 1475789056, 2562890625, 268435456, 410338673, 612220032, 893871739, 1280000000, 1801088541, 2494357888, 3404825447, 191102976, 244140625, 308915776, 387420489, 481890304, 594823321, 729000000, 887503681, 1073741824, 1291467969, 1544804416, 1838265625, 2176782336 };

        internal static string BigIntegerToString(uint[] d, int sign, int radix) {
            int dl = d.Length;
            if (dl == 0) {
                return "0";
            }

            List<uint> digitGroups = new List<uint>();

            uint groupRadix = groupRadixValues[radix];
            while (dl > 0) {
                uint rem = div(d, ref dl, groupRadix);
                digitGroups.Add(rem);
            }

            StringBuilder ret = new StringBuilder();
            if (sign == -1) {
                ret.Append("-");
            }

            int digitIndex = digitGroups.Count - 1;

            char[] tmpDigits = new char[maxCharsPerDigit[radix]];

            AppendRadix((uint)digitGroups[digitIndex--], (uint)radix, tmpDigits, ret, false);
            while (digitIndex >= 0) {
                AppendRadix((uint)digitGroups[digitIndex--], (uint)radix, tmpDigits, ret, true);
            }
            return ret.Length == 0 ? "0" : ret.ToString();
        }

        private const int BitsPerDigit = 32;

        private static uint div(uint[] n, ref int nl, uint d) {
            ulong rem = 0;
            int i = nl;
            bool seenNonZero = false;
            while (--i >= 0) {
                rem <<= BitsPerDigit;
                rem |= n[i];
                uint v = (uint)(rem / d);
                n[i] = v;
                if (v == 0) {
                    if (!seenNonZero) nl--;
                } else {
                    seenNonZero = true;
                }
                rem %= d;
            }
            return (uint)rem;
        }

        private static void AppendRadix(uint rem, uint radix, char[] tmp, StringBuilder buf, bool leadingZeros) {
            const string symbols = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

            int digits = tmp.Length;
            int i = digits;
            while (i > 0 && (leadingZeros || rem != 0)) {
                uint digit = rem % radix;
                rem /= radix;
                tmp[--i] = symbols[(int)digit];
            }
            if (leadingZeros) buf.Append(tmp);
            else buf.Append(tmp, i, digits - i);
        }

        // Helper for GetRandBits
        private static uint GetWord(byte[] bytes, int start, int end) {
            uint four = 0;
            int bits = end - start;
            int shift = 0;
            if (bits > 32) {
                bits = 32;
            }
            start /= 8;
            while (bits > 0) {
                uint value = bytes[start];
                if (bits < 8) {
                    value &= (1u << bits) - 1u;
                }
                value <<= shift;
                four |= value;
                bits -= 8;
                shift += 8;
                start++;
            }

            return four;
        }

        public static BigInteger GetRandBits(this Random generator, int bits) {
            // equivalent to (bits + 7) / 8 without possibility of overflow
            int count = bits % 8 == 0 ? bits / 8 : bits / 8 + 1;

            // Pad the end (most significant) with zeros if we align to the byte
            // to ensure that we end up with a positive value
            byte[] bytes = new byte[bits % 8 == 0 ? count + 1 : count];
            generator.NextBytes(bytes);
            if (bits % 8 == 0) {
                bytes[bytes.Length - 1] = 0;
            } else {
                bytes[bytes.Length - 1] = (byte)(bytes[bytes.Length - 1] & ((1 << (bits % 8)) - 1));
            }

            if (bits <= 32) {
                return (BigInteger)GetWord(bytes, 0, bits);
            } else if (bits <= 64) {
                ulong a = GetWord(bytes, 0, bits);
                ulong b = GetWord(bytes, 32, bits);
                return (BigInteger)(a | (b << 32));
            } else {
                count = (count + 3) / 4;
                uint[] data = new uint[count];
                for (int i = 0; i < count; i++) {
                    data[i] = GetWord(bytes, i * 32, bits);
                }
                return new BigInteger(1, data);
            }
        }

        public static BigInteger Random(this Random generator, BigInteger limit) {
            // TODO: this doesn't yield a uniform distribution (small numbers will be picked more frequently):
            uint[] result = new uint[limit.GetWordCount() + 1];
            for (int i = 0; i < result.Length; i++) {
                result[i] = unchecked((uint)generator.Next());
            }
            return new BigInteger(1, result) % limit;
        }

        public static bool TryToFloat64(this BigInteger self, out double result) {
            return Double.TryParse(
                self.ToString(10),
                System.Globalization.NumberStyles.Number,
                System.Globalization.CultureInfo.InvariantCulture.NumberFormat,
                out result
            );
        }

        public static double ToFloat64(this BigInteger self) {
            return double.Parse(
                self.ToString(10),
                System.Globalization.NumberStyles.Number,
                System.Globalization.CultureInfo.InvariantCulture.NumberFormat
            );
        }

        // Like GetBitCount(Abs(x)), except 0 maps to 0
        public static int BitLength(BigInteger x) {
            if (x.IsZero()) {
                return 0;
            }

            return x.Abs().GetBitCount();
        }

        // Like GetBitCount(Abs(x)), except 0 maps to 0
        public static int BitLength(long x) {
            if (x == 0) {
                return 0;
            }
            if (x == Int64.MinValue) {
                return 64;
            }

            x = Math.Abs(x);
            int res = 1;
            if (x >= 1L << 32) {
                x >>= 32;
                res += 32;
            }
            if (x >= 1L << 16) {
                x >>= 16;
                res += 16;
            }
            if (x >= 1L << 8) {
                x >>= 8;
                res += 8;
            }
            if (x >= 1L << 4) {
                x >>= 4;
                res += 4;
            }
            if (x >= 1L << 2) {
                x >>= 2;
                res += 2;
            }
            if (x >= 1L << 1) {
                res += 1;
            }

            return res;
        }

        // Like GetBitCount(Abs(x)), except 0 maps to 0
        //[CLSCompliant(false)]
        public static int BitLengthUnsigned(ulong x) {
            if (x >= 1uL << 63) {
                return 64;
            }
            return BitLength((long)x);
        }

        // Like GetBitCount(Abs(x)), except 0 maps to 0
        public static int BitLength(int x) {
            if (x == 0) {
                return 0;
            }
            if (x == Int32.MinValue) {
                return 32;
            }

            x = Math.Abs(x);
            int res = 1;
            if (x >= 1 << 16) {
                x >>= 16;
                res += 16;
            }
            if (x >= 1 << 8) {
                x >>= 8;
                res += 8;
            }
            if (x >= 1 << 4) {
                x >>= 4;
                res += 4;
            }
            if (x >= 1 << 2) {
                x >>= 2;
                res += 2;
            }
            if (x >= 1 << 1) {
                res += 1;
            }

            return res;
        }

        // Like GetBitCount(Abs(x)), except 0 maps to 0
        //[CLSCompliant(false)]
        public static int BitLengthUnsigned(uint x) {
            if (x >= 1u << 31) {
                return 32;
            }
            return BitLength((int)x);
        }

        #endregion
    }

}
