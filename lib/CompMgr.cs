using System;
using System.Collections.Generic;
using System.IO;

// This is the main entry point for the compiler

namespace Niecza.Compiler {
    class CompMgr {
        // TODO user/system split
        public List<string> lib_path = new List<string>();
        public string language = "CORE";
        public bool safe_mode, no_source;
        public int verbose;

        public string obj_dir;
        public string[] run_args;

        public CompMgr() {
            string base_dir = Path.Combine(
                AppDomain.CurrentDomain.BaseDirectory, "..");
            obj_dir = Path.GetFullPath(Path.Combine(base_dir, "obj"));
            lib_path.Add(Path.GetFullPath(Path.Combine(base_dir, "lib")));
            lib_path.Add(Path.GetFullPath("."));
        }

        public void compile_string_repl(string code, bool run) {
            throw new NotImplementedException();
        }

        public void compile_string(string code, bool run) {
            throw new NotImplementedException();
        }

        public void compile_module(string code) {
            throw new NotImplementedException();
        }

        public void compile_file(string fname, bool run) {
            throw new NotImplementedException();
        }
    }

    class CommandDriver {
        static void usage() {
            Console.Write(@"niecza -- a command line wrapper for Niecza

usage: niecza -e 'code'      # run a one-liner
   OR: niecza file.pl [args] # run a program
   OR: niecza -C MyModule    # precompile a module
   OR: niecza                # interactive shell

general options:
   -n                        # short for -L CORN
   -p                        # short for -L CORP
   -B --backend=NAME         # select backend (dotnet)
   -L --language=NAME        # select your setting
   -I --include=DIR          # add a directory to search for modules
   -v --verbose              # detailed timing info
   -c --compile              # don't run (implied with -C)
      --safe                 # disable system interaction
      --help                 # display this message

output options:
      --obj-dir=DIR          # select output location (all)
      --no-include-source    # disable source-based introspection, etc
".Replace("\r\n","\n").Replace("\n", Console.Out.NewLine));
            Environment.Exit (0);
        }

        public static void CompMain(string[] argv) {
            List<string> eval = new List<string>();
            bool comp_module = false;
            bool print_version = false, compile_only = false;

            var cm = new CompMgr();

            var o = new GetoptLong();
            o.Permute = false;
            o.Opt("evaluate|e",       1, (s) => { eval.Add(s); });
            o.Opt("compile-module|C", 1, (s) => { comp_module = true; });
            o.Opt("backend|B",        1, (s) => { Console.Error.WriteLine(
                "niecza: The backend option is deprecated and ignored."); });
            o.Opt("language|L",       1, (s) => { cm.language = s; });
            o.Opt("p",                0, (s) => { cm.language = "CORP"; });
            o.Opt("n",                0, (s) => { cm.language = "CORN"; });
            o.Opt("verbose",          0, (s) => { cm.verbose++; });
            o.Opt("v|version",        0, (s) => { print_version = true; });
            o.Opt("compile|c",        0, (s) => { compile_only = true; });
            o.Opt("safe",             0, (s) => { cm.safe_mode = true; });
            o.Opt("include|I",        1, (s) => { cm.lib_path.Insert(0,s); });
            o.Opt("obj-dir",          1, (s) => { cm.obj_dir = s; });
            o.Opt("help|h",           0, (s) => { usage(); });
            o.Opt("no-include-source",0, (s) => { cm.no_source = true; });
            o.Parse(ref argv);

            if (print_version) {
                cm.compile_string("say qq[This is Niecza Perl 6 {$?PERL<version>}]", true);
                Environment.Exit(0);
            }

            if (comp_module) {
                if (eval.Count != 0) {
                    Console.Error.WriteLine("Module compilation cannot be used with strings to evaluate");
                    Environment.Exit(1);
                }
                if (argv.Length == 0) {
                    Console.WriteLine("No modules named to compile!");
                    Environment.Exit(0);
                }
                foreach (string m in argv)
                    cm.compile_module(m);
            }
            else if (eval.Count != 0) {
                cm.run_args = argv;
                foreach (string e in eval)
                    cm.compile_string(e, !compile_only);
            }
            else if (argv.Length != 0) {
                cm.run_args = Utils.TrimArr(argv, 1, 0);
                cm.compile_file(argv[0], !compile_only);
            }
            else {
                cm.compile_string_repl("$PROCESS::OUTPUT_USED ::= True",
                    !compile_only);
                // XXX the child will also open a TextReader; could be problem
                var inp = Builtins.treader_stdin();
                while (true) {
                    Console.Write("niecza> ");
                    var line = inp.ReadLine();
                    if (line == null)
                        break;
                    try {
                        cm.compile_string_repl(line, !compile_only);
                    } catch (Exception ex) {
                        Console.WriteLine(ex);
                    }
                }
            }
        }
    }

    static class CompUtils {
        public static int GenId() {
            return Kernel.containerRootUnit.nextid++;
        }
        public static string GenSym() {
            return "!anon_" + GenId();
        }

        public static int LineOf(Cursor c) { return 0; }
        public static void Sorry(Cursor c, string msg) { throw new NotImplementedException(); }
        public static SubInfo GetCurSub() { throw new NotImplementedException(); }
        public static void SetRunOnce(SubInfo s) {
            if ((s.outer.special & SubInfo.RUN_ONCE) != 0) {
                s.CreateProtopad(null);
                s.special |= SubInfo.RUN_ONCE;
            }
        }
        public static LexInfo LookupLex(SubInfo s, string n) { throw new NotImplementedException(); }
        public static STable CompileGetPkg(SubInfo s, string n) { throw new NotImplementedException(); }
        public static SubInfo ThunkSub(Op.Op body, string[] args) { throw new NotImplementedException(); }
        public static Op.Lexical BlockExpr(Cursor at, SubInfo blk) { throw new NotImplementedException(); }
        public static Op.Op BetaCall(Cursor at, string name, params Op.Op[] pos) { throw new NotImplementedException(); }
        public static void MarkUsed(Cursor at, string name) { throw new NotImplementedException(); }
    }
}
