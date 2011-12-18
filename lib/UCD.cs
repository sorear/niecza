using System;
using System.Runtime.CompilerServices;
using System.Collections.Generic;
using System.IO;
using Niecza;
using Niecza.UCD;

// Subsystem of Niecza that access the Unicode Character Database
//
// UCD data within Niecza is represented as a collection of named
// tables.  Most of these are property tables, which associate a
// character to one or two strings.
//
// Tables are identified by names.  Non-property tables never have
// names not starting with !.
namespace Niecza.UCD {
    static class DataSet {
        static Dictionary<string,object> cache;
        static byte[] bits;
        static Dictionary<string,int[]> directory;
        static bool Trace;

        const int FILES = 4;
        static int Int(ref int from) {
            from += 4;
            return (bits[from-4] << 24) | (bits[from-3] << 16) |
                (bits[from-2] << 8) | (bits[from-1]);
        }

        static string AsciiZ(ref int from) {
            int to = from;
            while (bits[to] != 0) to++;
            char[] buf = new char[to - from];
            for (int i = from; i < to; i++)
                buf[i - from] = (char)bits[i];
            from = to+1;
            return new string(buf);
        }

        static void InflateDirectory() {
            directory = new Dictionary<string,int[]>();
            Trace = Environment.GetEnvironmentVariable("NIECZA_UCD_TRACE") != null;
            if (Trace) Console.WriteLine("Unpacking directory ...");

            int rpos = 0;

            int[] fstart = new int[FILES];
            for (int i = 0; i < FILES; i++)
                fstart[i] = (i == 0 ? 20 : fstart[i-1]) + Int(ref rpos);
            rpos += 4; // skip the extra length

            int dend = fstart[0];

            while (rpos < dend) {
                int rpos0 = rpos;
                int[] loc = new int[FILES * 2];
                string name = AsciiZ(ref rpos);
                int nfiles = bits[rpos++];
                for (int i = 0; i < nfiles; i++) {
                    loc[2*i] = fstart[i];
                    fstart[i] += Int(ref rpos);
                    loc[2*i+1] = fstart[i];
                }
                if (Trace)
                    Console.WriteLine("Entry {0} (d.e. {1}): {2}",
                            name, rpos0, Kernel.JoinS(", ", loc));
                directory[name] = loc;
            }
            if (Trace) Console.WriteLine("done.");
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public static object GetTable(string name) {
            if (cache == null)
                cache = new Dictionary<string,object>();
            object r;
            if (cache.TryGetValue(name, out r))
                return r;

            if (bits == null) {
                bits = File.ReadAllBytes("unidata");
                InflateDirectory();
            }

            int[] loc;
            if (!directory.TryGetValue(name, out loc))
                throw new NieczaException(name + " does not exist as a UCD table");

            switch (bits[loc[0]]) {
                default:
                    throw new NieczaException("Unhandled type code " + bits[loc[0]]);
            }
        }
    }
}

public partial class Builtins {
    public static Variable ucd_test() {
        DataSet.GetTable("gc");
        return null;
    }
}
