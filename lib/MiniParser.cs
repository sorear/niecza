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

        CompJob job;

        public MiniParser(CompJob job) {
            this.job = job;
            this.src_s = job.source + (char)0;
            this.src = src_s.ToCharArray();
        }

        public void Parse() {
            ws();

            job.curlex = new SubInfo(job.unit, "mainline", null,
                Kernel.RoutineMO, null, true, null);
            job.unit.our_subs.Add(job.curlex);

            statementlist();
        }

        void carp(string fmt, params object[] args) {
            throw new NieczaException(string.Format(fmt, args) +
                    " at position " + next);
        }

        bool str(string look_for) {
            int ll = look_for.Length;
            if (src.Length - next < ll)
                return false;
            if (src_s.Substring(next, ll) != look_for)
                return false;
            if (CC.Word.Accepts(src[next + ll]))
                return false;
            next += look_for.Length;
            ws();
            return true;
        }

        void ch(char what) {
            if (src[next++] != what)
                carp(what + " expected");
            ws();
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
            if (str("grammar")) {
                package(P6how.GRAMMAR);
                return;
            }
            if (str("role")) {
                package(P6how.PARAMETRIZED_ROLE);
                return;
            }
            carp("NYI statement");
        }

        void package(int type) {
            string id = name();
            newlex(false, type != P6how.PARAMETRIZED_ROLE);
            if (type == P6how.PARAMETRIZED_ROLE && src[next] == '[') {
                ch('['); signature(); ch(']');
            }
            if (type == P6how.PARAMETRIZED_ROLE) {
                carp("role sig NYI");
            }

            while (trait());

            // lexvar, obj = do_new_package
            // job.curlex.outervar = lexvar;
            // job.curlex.body_of = job.curlex.cur_pkg = job.curlex.in_class =
            //    obj;

            // job.curlex.name = obj.mo.rkind + "-" + id;

            blockoid();

            // var bodyvar = CompMgr.GenSym();
            //CompUtils.add_my_sub(job.curlex.outer, bodyvar, job.curlex);

            // obj.mo.Compose();

            if (type == P6how.PARAMETRIZED_ROLE) {
                // TODO instantiation block, etc
            }
        }

        // not right, but right enough.
        string name() {
            int from = next;
            while (!char.IsWhiteSpace(src[next])) next++;
            string v = src_s.Substring(from, next-from);
            ws();
            return v;
        }

        void signature() {
            carp("NYI signature");
        }

        void newlex(bool needsig, bool once) {
            job.curlex = new SubInfo(job.unit, "ANON", job.curlex,
                Kernel.BlockMO, null, once &&
                    (job.curlex.special & SubInfo.RUN_ONCE) != 0,
                null);
            job.curlex.in_class = job.curlex.outer.in_class;
        }

        void poplex() {
            job.curlex = job.curlex.outer;
        }

        bool trait() {
            // TODO
            return false;
        }

        void blockoid() {
            ch('{');
            statementlist();
            ch('}');
        }
    }
}
