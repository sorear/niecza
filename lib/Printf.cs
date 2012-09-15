using Niecza;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;


public partial class Builtins {

    public static Variable sprintf(Variable[] args) {
        char[] fmt = args[0].Fetch().mo.mro_raw_Str.Get(args[0]).ToCharArray();
        List<PrintfFormat> fmtlist = ParseFormatString(fmt);
        return RenderFormat(fmtlist, args);
    }

    private enum PrintfDirective {
        LiteralText,
        CodePoint,
        String,
        Int,
        FloatScientific,
        FloatFixedDecimal,
        FloatEF,
        InvokeCode
    }

    private enum PrintfModifier {
        None,
        NativeShort,
        NativeLong,
        NativeLongLong,
        NativeQuad,
        Complex
    }

    private struct PrintfFormat {
        internal int index; // 0 means "next one" (default), 1-indexed
        internal bool nonNegativeSpacePrefix;
        internal bool nonNegativePlusPrefix;
        internal bool leftJustify;
        internal bool rightJustifyZeroes;
        internal bool leadingRadix;
        internal bool vector;
        internal bool vectorOverride;
        internal int vectorOverrideIndex; // 1-indexed
        internal int minimumWidth;
        internal int minimumWidthIndex; // 1-indexed
        internal int precision;
        internal int precisionIndex; // 1-indexed
        internal PrintfDirective directive;
        internal PrintfModifier modifier;
        internal uint radix;
        internal bool upper;
        internal string literaltext;
    }

    private static List<PrintfFormat> ParseFormatString(char[] fmtstring) {
        List<PrintfFormat> fmtlist = new List<PrintfFormat>();
        for (int pos=0; pos < fmtstring.Length; ++pos) {
            char c = fmtstring[pos];
            PrintfFormat format = new PrintfFormat();
            // We do not have to initialize all these fields, but it's
            // handy to have them all listed together in one place.
            format.index = 0;
            format.nonNegativeSpacePrefix = false;
            format.nonNegativePlusPrefix = false;
            format.leftJustify = false;
            format.rightJustifyZeroes = false;
            format.leadingRadix = false;
            format.vector = false;
            format.vectorOverride = false;
            format.vectorOverrideIndex = 0; // 1-indexed
            format.minimumWidth = 0;
            format.minimumWidthIndex = 0; // 1-indexed
            format.precision = 0;
            format.precisionIndex = 0; // 1-indexed
            format.directive = PrintfDirective.String;
            format.modifier = PrintfModifier.None;
            format.radix = 10;
            format.upper = false;
            format.literaltext = "";
            if (c == '%') {  // begin format specifier
                bool continue_parsing_specifier = true;
                bool parse_minimum_width = true;
                bool seen_first_digit = false;
                while (continue_parsing_specifier && ++pos<fmtstring.Length) {
                    switch (c=fmtstring[pos]) {
                        case '0': case '1': case '2': case '3': case '4':
                        case '5': case '6': case '7': case '8': case '9':
                            int digitvalue = c - '0';
                            if (!seen_first_digit && c == '0') {
                                format.rightJustifyZeroes = true;
                            }
                            seen_first_digit = true;
                            if (parse_minimum_width)
                                format.minimumWidth = 10 * format.minimumWidth + digitvalue;
                            else
                                format.precision = 10 * format.precision + digitvalue;
                            break;
                        case '.':
                            parse_minimum_width = false;
                            break;
                        case '-':  // negative width
                            format.leftJustify = true;
                            break;
                        case '$':  // "%1$" arglist position
                            format.index = format.minimumWidth;
                            format.minimumWidth = 0;
                            break;
                        case '%':  // "%%" a literal '%'
                            format.directive = PrintfDirective.LiteralText;
                            format.literaltext = "" + c;
                            continue_parsing_specifier = false;
                            break;
                        case 'b':  // unsigned binary integer
                            format.directive = PrintfDirective.Int;
                            format.radix = 2;
                            continue_parsing_specifier = false;
                            break;
                        case 'C':  // Code
                            format.directive = PrintfDirective.InvokeCode;
                            continue_parsing_specifier = false;
                            break;
                        case 'c':  // codepoint
                            format.directive = PrintfDirective.CodePoint;
                            continue_parsing_specifier = false;
                            break;
                        case 'd':  // decimal
                        case 'i':  // integer (alias for decimal)
                        case 'u':  // integer (alias for decimal)
                            format.directive = PrintfDirective.Int;
                            format.radix = 10;
                            continue_parsing_specifier = false;
                            break;
                        case 'E':  // scientific uppercase E
                            format.directive = PrintfDirective.FloatScientific;
                            format.upper = true;
                            continue_parsing_specifier = false;
                            break;
                        case 'e':  // scientific lowercase e
                            format.directive = PrintfDirective.FloatScientific;
                            continue_parsing_specifier = false;
                            break;
                        case 'f':  // fixed point
                        case 'F':  // fixed point synonym
                            format.directive = PrintfDirective.FloatFixedDecimal;
                            continue_parsing_specifier = false;
                            break;
                        case 'G':  // e or f uppercase E
                            format.directive = PrintfDirective.FloatEF;
                            format.upper = true;
                            continue_parsing_specifier = false;
                            break;
                        case 'g':  // e or f lowercase e
                            format.directive = PrintfDirective.FloatEF;
                            continue_parsing_specifier = false;
                            break;
                        case 'o':  // unsigned octal
                            format.directive = PrintfDirective.Int;
                            format.radix = 8;
                            continue_parsing_specifier = false;
                            break;
                        case 's':  // string
                            format.directive = PrintfDirective.String;
                            continue_parsing_specifier = false;
                            break;
                        case 'X':  // hex uppercase
                            format.directive = PrintfDirective.Int;
                            format.radix = 16;
                            format.upper = true;
                            continue_parsing_specifier = false;
                            break;
                        case 'x':  // hex lowercase
                            format.directive = PrintfDirective.Int;
                            format.radix = 16;
                            format.upper = false;
                            continue_parsing_specifier = false;
                            break;
                        default:
                            throw new NieczaException("invalid format specifier");
                    }
                }
            }
            else {  // non '%' characters
                format.directive = PrintfDirective.LiteralText;
                format.literaltext = "" + c;
                while (++pos < fmtstring.Length && (c=fmtstring[pos]) != '%') {
                    format.literaltext += c;
                }
                // pos has gone one char too far, but should come back
                // only if there is a '%' before the end of fmtstring
                if (pos < fmtstring.Length && (c=fmtstring[pos]) == '%')
                    --pos;
            }
            fmtlist.Add(format);
        }
        return fmtlist;
    }

    private static string RemoveInitialZero(String s) {
        if (s.Length > 1 && s[0] == '0') {
            return s.Substring(1);
        }
        return s;
    }

    private static string RenderFormat(PrintfFormat format, Variable arg) {
        int n;

        if (format.directive == PrintfDirective.String) {
            return arg.Fetch().mo.mro_raw_Str.Get(arg);
        }

        if (format.directive == PrintfDirective.CodePoint) {
            n = (int) arg.Fetch().mo.mro_raw_Numeric.Get(arg);
            return "" + (char) n;
        }

        bool add_minus_sign = false;
        if (format.directive == PrintfDirective.Int) {

            P6any o1 = arg.Fetch();
            int r1;
            P6any n1 = GetNumber(arg, o1, out r1);
            Variable arg2;

            if (r1 != NR_BIGINT && r1 != NR_FIXINT) {
                arg2 = Builtins.InvokeMethod("Int", arg);
                o1 = arg2.Fetch();
                n1 = GetNumber(arg2, o1, out r1);
            }

            BigInteger value = PromoteToBigInt(r1, n1);
            if (value < 0) {
                add_minus_sign = true;
                value = -value;
            }

            String number;
            if (format.radix == 16) {
                number = RemoveInitialZero(value.ToString(format.radix, null)).ToLower();
            } else {
                number = value.ToString(format.radix, null);
            }

            if (format.upper) {
                number = number.ToUpper();
            }

            if (add_minus_sign) {
                if (format.rightJustifyZeroes) {
                    return "-" + number.PadLeft(format.minimumWidth - 1, '0');
                } else {
                    return "-" + number;
                }
            }

            return number;

        } else {

            double f = arg.Fetch().mo.mro_raw_Numeric.Get(arg);

            if (f < 0.0) {
                add_minus_sign = true;
                f = -f;
            }

            int precision = format.precision > 0 ? format.precision : 6;

            String number = "??";
            switch (format.directive) {
                case PrintfDirective.FloatFixedDecimal:
                    number = f.ToString("F" + precision);
                    break;
                case PrintfDirective.FloatScientific:
                    number = f.ToString("e" + precision);
                    break;
                case PrintfDirective.FloatEF:
                    number = f.ToString("g" + precision);
                    break;
            }

            if (format.upper) {
                number = number.ToUpper();
            }

            if (add_minus_sign) {
                if (format.rightJustifyZeroes) {
                    return "-" + number.PadLeft(format.minimumWidth - 1, '0');
                } else {
                    return "-" + number;
                }
            }

            return number;
        }
    }

    private static Variable RenderFormat(List<PrintfFormat> formatlist, Variable[] args) {
        string result = "";
        int argi = 0;
        foreach (PrintfFormat format in formatlist) {
            if (format.directive == PrintfDirective.LiteralText) {
                result += format.literaltext;
                continue;
            }

            if (format.directive == PrintfDirective.InvokeCode) {
                continue; // Should this do something more?
            }

            int i = format.index > 0 ? format.index : ++argi;
            if (i < args.Length) {
                string s = RenderFormat(format, args[i]);
                if (format.leftJustify) {
                    result += s.PadRight(format.minimumWidth, ' ');
                } else {
                    if (format.rightJustifyZeroes) {
                        result += s.PadLeft(format.minimumWidth, '0');
                    }
                    else {
                        result += s.PadLeft(format.minimumWidth, ' ');
                    }
                }
            }
            else {
                throw new NieczaException("index out of range");
            }
        }
        return Kernel.BoxAnyMO(result, Compartment.Top.StrMO);
    }
}

