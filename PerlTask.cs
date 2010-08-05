using System;
using System.IO;
using System.Diagnostics;
using Microsoft.Build.Utilities;
using Microsoft.Build.Framework;

public class PerlInterpreter
{
    private Process p;

    public PerlInterpreter(string perlPath)
    {
        p = new Process();
        p.StartInfo.FileName = perlPath;
        p.StartInfo.Arguments = "PerlStub.pl";
        p.StartInfo.UseShellExecute = false;
        p.StartInfo.RedirectStandardInput = true;
        p.StartInfo.RedirectStandardError = true;
        p.StartInfo.CreateNoWindow = true;
        p.Start();
    }

    public bool RunPerl(string code)
    {
        p.StandardInput.WriteLine(code);
        p.StandardInput.Flush();
        int rt = p.StandardError.Read();
        if (rt < 0)
            throw new Exception("Perl interpreter process unexpectedly quit");
        return rt == (int)'R';
    }

    private static string FindInterpreter(string basename)
    {
        string pathvar = Environment.GetEnvironmentVariable("PATH") ?? ".";
        foreach (string pel in pathvar.Split(Path.PathSeparator)) {
            string n1 = Path.Combine(pel, basename);
            if (File.Exists(n1)) return n1;
            string n2 = Path.Combine(pel, basename + ".exe");
            if (File.Exists(n2)) return n2;
        }
        throw new Exception("Cannot find " + basename + " in PATH");
    }

    private static PerlInterpreter sharedInstance;
    public static PerlInterpreter SharedInstance(string path)
    {
        if (sharedInstance == null)
            sharedInstance = new PerlInterpreter(FindInterpreter(path));
        return sharedInstance;
    }
}

namespace Niecza.Tasks
{
    public class Perl : Task
    {
        private string code;
        private string path = "perl";

        public override bool Execute()
        {
            return PerlInterpreter.SharedInstance(path).RunPerl(code);
        }

        [Required]
        public string Code
        {
            get { return code; }
            set { code = value; }
        }

        public string InterpreterPath
        {
            get { return path; }
            set { path = value; }
        }
    }
}
