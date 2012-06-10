// Yuck.  This file is part of the bootstrap mechanism; it is used to parse
// Perl 6 grammars, because Perl 6's own grammar cannot be written using yacc.

using System;
using System.Collections.Generic;

namespace Niecza.Compiler {
    class MiniParser {
        char[] src;
        string src_s;
        RuntimeUnit unit;

        public MiniParser(string gsrc, RuntimeUnit core) {
            this.src_s = gsrc;
            this.src = gsrc.ToCharArray();
            this.unit = core;
        }

        public void Parse() {
            throw new NotImplementedException();
        }
    }
}
