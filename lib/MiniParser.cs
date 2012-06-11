// Yuck.  This file is part of the bootstrap mechanism; it is used to parse
// Perl 6 grammars, because Perl 6's own grammar cannot be written using yacc.

using System;
using System.Collections.Generic;

// Implementation note: We don't support supplementary characters here.
// Also, there is a (char)0 sentinel at the end, so that src[next] will
// always work somewhat.
namespace Niecza.Compiler {
    class MiniParser {
        char[] src;
        string src_s;
        int next;
        RuntimeUnit unit;

        public MiniParser(string gsrc, RuntimeUnit core) {
            this.src_s = gsrc + (char)0;
            this.src = gsrc.ToCharArray();
            this.unit = core;
        }

        public void Parse() {
            ws();
            statementlist();
        }

        void carp(string fmt, params object[] args) {
            throw new NieczaException(string.Format(fmt, args) +
                    " at position " + next);
        }

        void ws() {
            if (next > 0 && CC.Word.Accepts(src[next-1]) &&
                    CC.Word.Accepts(src[next]))
                carp("Whitespace expected");

            while (true) {
                char n = src[next];
                if (n == ' ' || n == '\r' || n == '\n' || n == '\t') {
                    next++;
                } else if (n == '#') {
                    while (n != '\n' && n != '\r' && n != '\0')
                        n = src[next++];
                    next--;
                } else {
                    break;
                }
            }
        }

        void statementlist() {
            while (src[next] != 0 && src[next] != '}')
                statement();
        }

        void statement() {
            carp("NYI");
        }
    }
}
