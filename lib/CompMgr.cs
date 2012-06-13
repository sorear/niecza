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

        public Frame repl_outer_frame;
        public SubInfo repl_outer_sub;

        // used to exclude recursive compilations from time accounting
        internal double discount_time;

        public CompMgr() {
            string base_dir = Path.Combine(
                AppDomain.CurrentDomain.BaseDirectory, "..");
            obj_dir = Path.GetFullPath(Path.Combine(base_dir, "obj"));
            lib_path.Add(Path.GetFullPath(Path.Combine(base_dir, "lib")));
            lib_path.Add(Path.GetFullPath("."));
        }

        public RuntimeUnit compile_string(string code, bool run, bool eval, bool repl) {
            var job = new CompJob(this);
            job.unitname = main_name();
            job.from_string("(eval)", code);
            job.is_main = true;
            job.is_eval = eval;
            job.is_repl = repl;
            return job.run(run);
        }

        public void compile_module(string name) {
            var job = new CompJob(this);
            job.from_module(name);
            job.run(false);
        }

        public void compile_file(string fname, bool run) {
            var job = new CompJob(this);
            job.unitname = main_name();
            job.from_file(fname);
            job.is_main = true;
            job.run(run);
        }

        int next_main;
        string main_name() { var i = next_main++; return i != 0 ? "MAIN_"+i : "MAIN"; }
    }

    class CompJob {
        public CompMgr mgr;
        public string source;
        public string filename;

        public string unitname;
        public bool is_main, is_eval, is_repl;

        // state vars
        public RuntimeUnit unit;
        public SubInfo     curlex;

        int nextid;

        public CompJob(CompMgr mgr) { this.mgr = mgr; }

        public int genid() { return nextid++; }
        public string gensym() { return "!anon_" + genid(); }

        [ThreadStatic] [CompartmentGlobal]
        public static CompJob cur;

        public RuntimeUnit run(bool runit) {
            // fudge to make -L NULL useful
            if (mgr.language == "NULL")
                unitname = "CORE";
            double start = Builtins.usertime() - mgr.discount_time;

            RuntimeUnit u;
            if (!is_eval)
                Compartment.Push();
            var old = cur;
            try {
                cur = this;
                u = run_internal(runit);
            } finally {
                cur = old;
                if (!is_eval)
                    Compartment.Pop();
            }

            double time = Builtins.usertime() - mgr.discount_time - start;

            if (mgr.verbose > 0) {
                Console.WriteLine("{0}: took {1}", unitname, time);
            }

            // don't count this time towards any other timing in progress
            mgr.discount_time += time;
            return u;
        }

        RuntimeUnit run_internal(bool runit) {
            Console.WriteLine("unitname = " + unitname);
            Console.WriteLine("filename = " + filename);
            Console.WriteLine("source   = " + source.Length);

            RuntimeUnit ru = new RuntimeUnit(unitname, filename,
                    (mgr.no_source ? Utils.HashString(source) : source),
                    is_main, runit);

            if (Kernel.containerRootUnit == null) {
                // this is a module unit
                Kernel.InitCompartment();
                Kernel.containerRootUnit = ru;
                ru.depended_units.Add(ru);
                ru.owner = ru;
                ru.globals = Kernel.currentGlobals =
                    new Dictionary<string,StashEnt>();
            } else {
                // needs to use the same globals as the other units in
                // this serialization unit
                ru.globals = Kernel.currentGlobals;
                ru.owner = Kernel.containerRootUnit;
                ru.owner.subordinates.Add(ru);
            }

            string lang = mgr.language;
            if (unitname == "CORE") {
                lang = "NULL";

                CompJob alt = new CompJob(mgr);
                alt.filename = Path.Combine(Path.GetDirectoryName(filename),
                        "Parser.src");
                alt.source = File.ReadAllText(alt.filename);
                alt.unit   = ru;

                new MiniParser(alt).Parse();
            } else if (unitname != "MAIN") { // modules aren't affected by -L
                lang = "CORE";
            }

            CompUtils.rel_pkg(ru, true, null, "GLOBAL");
            CompUtils.rel_pkg(ru, true, null, "PROCESS");

            // Parser/actions tree runs here
            throw new NotImplementedException();

            if (runit) {
                //if (!is_repl) setnames(execname(), filename);
                //run_unit(ru, is_eval, run_args);
                if (is_repl) {
                    // mgr.repl_outer_frame = replrun();
                    // mgr.repl_outer = ru.mainline;
                }
            } else {
                // save_unit(ru);
                // if (is_repl) mgr.repl_outer = ru.mainline;
            }

            return is_eval ? ru : null;
        }

        // borrowed from STD, try to allow p5 and p6 to coexist
        bool heuristic_check_p5(string text) {
            int len = text.Length;
            CC hs = new CC(CClass.HSpace.terms);
            CC sp = new CC(CClass.Space.terms);
            for (int ix = 0; ix < len; ix++) {
                if (!CC.VSpace.Accepts(text[ix]))
                    continue;
                int i2 = ix;
                while (i2 < len && hs.Accepts(text[i2])) i2++;
                if (len - i2 < 8 || text.Substring(i2, 7) != "package" ||
                        !sp.Accepts(text[i2+7]))
                    continue;
                i2 += 8;
                while (i2 < len && sp.Accepts(text[i2])) i2++;
                if (i2 < len && text[i2] == ';') return true;
            }
            return false;
        }

        public void from_file(string name) {
            filename = Path.GetFullPath(name);
            source = File.ReadAllText(filename);
        }

        public void from_string(string name, string source) {
            filename = name;
            this.source = source;
        }

        public void from_module(string name) {
            var sub = ".";
            var ntmp = name;
            unitname = name;
            int ix;
            while ((ix = ntmp.IndexOf("::")) >= 0) {
                sub = Path.Combine(sub, ntmp.Substring(0,ix));
                ntmp = ntmp.Substring(ix+2);
            }
            sub = Path.Combine(sub, ntmp);
            var path = new List<string>();
            if (Kernel.containerRootUnit != null) {
                StashEnt bv;
                if (Kernel.currentGlobals.TryGetValue(
                            "\u0008::GLOBAL@INC", out bv))
                    foreach (string i in Builtins.UnboxLoS(bv.v))
                        path.Add(i);
            }
            foreach (string i in mgr.lib_path)
                path.Add(i);

            foreach (string pe in path) {
                foreach (string ext in new [] { ".pm6", ".pm", ".setting" }) {
                    var fn = Path.Combine(pe, sub + ext);
                    if (File.Exists(fn)) {
                        var text = File.ReadAllText(fn);
                        if (ext == ".pm" && heuristic_check_p5(text))
                            continue;
                        source = text;
                        filename = Path.GetFullPath(fn);
                        return;
                    }
                }
            }

            throw new NieczaException("Unable to locate module "+name+" in "+
                Kernel.JoinS(" ", path));
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
            o.Opt("compile-module|C", 0, (s) => { comp_module = true; });
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
                cm.compile_string("say qq[This is Niecza Perl 6 {$?PERL<version>}]", true, false, false);
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
                    cm.compile_string(e, !compile_only, false, false);
            }
            else if (argv.Length != 0) {
                cm.run_args = Utils.TrimArr(argv, 1, 0);
                cm.compile_file(argv[0], !compile_only);
            }
            else {
                cm.compile_string("$PROCESS::OUTPUT_USED ::= True",
                    !compile_only, true, true);
                // XXX the child will also open a TextReader; could be problem
                var inp = Builtins.treader_stdin();
                while (true) {
                    Console.Write("niecza> ");
                    var line = inp.ReadLine();
                    if (line == null)
                        break;
                    try {
                        cm.compile_string(line, !compile_only, true, true);
                    } catch (Exception ex) {
                        Console.WriteLine(ex);
                    }
                }
            }
        }
    }

    static class CompUtils {
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
        public static Op.Op BetaCall(Cursor at, string name,
                params Op.Op[] pos) {
            LISub lis = LookupLex(GetCurSub(), name) as LISub;
            if (lis == null || !lis.def.IsInlinable())
                return new Op.CallSub(at, new Op.Lexical(at, name), true, pos);

            lis.def.SetInlined();
            return new Op.RawCgOp(at, Utils.PrependArr<object>(pos, 0,
                        "_inline", lis.def));
        }
        public static void MarkUsed(Cursor at, string name) { throw new NotImplementedException(); }
        public static STable rel_pkg(RuntimeUnit c, bool auto, STable pkg,
                params string[] args) {

            for (int i = 0; i < args.Length; i++) {
                string key = args[i];
                string who = "";
                if (pkg != null) {
                    if (!pkg.who.Isa(Kernel.StashMO))
                        throw new NieczaException(pkg.name + " fails to name a standard package");
                    who = Kernel.UnboxAny<string>(pkg.who);
                }
                StashEnt v;
                string hkey = (char)who.Length + who + key;
                if (c.globals.TryGetValue(hkey, out v)) {
                    if (v.v.Rw || v.v.Fetch().IsDefined())
                        throw new NieczaException((who + "::" + key).Substring(2) + " names a non-package");
                    pkg = v.v.Fetch().mo;
                } else if (!auto) {
                    throw new NieczaException((who + "::" + key).Substring(2) + " does not name any package");
                } else {
                    c.globals[hkey] = v = new StashEnt();
                    v.constant = true;
                    v.v = StashCursor.MakePackage((who + "::" + key).Substring(2), Kernel.BoxRaw<string>(who + "::" + key, Kernel.StashMO));
                    pkg = v.v.Fetch().mo;
                }
            }
            return pkg;
        }

    }
}
