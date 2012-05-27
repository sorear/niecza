using Niecza;
using System.Runtime.InteropServices;
using System;
using System.IO;
using System.Collections.Generic;
using Niecza.Serialization;

public class Perl5Interpreter : IForeignInterpreter {
    [DllImport("p5embed", EntryPoint="p5embed_initialize")]
    public static extern void Initialize(string path1,string path2);
  
    [DllImport("p5embed", EntryPoint="p5embed_dispose")]
    public static extern void Dispose();
  
    [DllImport("p5embed", EntryPoint="p5embed_eval")]
    public static extern IntPtr EvalPerl5(string code);

    [DllImport("p5embed", EntryPoint="p5embed_SvIV")]
    public static extern int SvIV(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvPV_nolen")]
    public static extern string SvPV_nolen(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvPVutf8_nolen")]
    public static extern IntPtr SvPVutf8_nolen(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvPVutf8_length")]
    public static extern int SvPVutf8_length(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvNV")]
    public static extern double SvNV(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvIOKp")]
    public static extern int SvIOKp(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvNOKp")]
    public static extern int SvNOKp(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvPOKp")]
    public static extern int SvPOKp(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvOK")]
    public static extern int SvOK(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_SvRV")]
    public static extern IntPtr SvRV(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_sv_isa")]
    public static extern int sv_isa(IntPtr sv,string name);

    [DllImport("p5embed", EntryPoint="p5embed_newSVpvn")]
    public static extern IntPtr newSVpvn(IntPtr s,int length);

    [DllImport("p5embed", EntryPoint="p5embed_SvUTF8_on")]
    public static extern void SvUTF8_on(IntPtr sv);

    [DllImport("p5embed", EntryPoint="p5embed_subcall")]
    public static extern IntPtr SubCall(
        int context,
        IntPtr[] arguments,
        int argument_n
    );


    public delegate int create_LoS_delegate(int len,IntPtr data);

    [DllImport("p5embed", EntryPoint="p5embed_set_create_LoS")]
    public static extern void Set_p5embed_create_LoS(create_LoS_delegate f);


    // We can't use the standard char* conversion because some strings can contain nulls
    public static string UnmarshalString(IntPtr sv) {
        int len = SvPVutf8_length(sv);
        byte[] target = new byte[len];
        IntPtr data = SvPVutf8_nolen(sv);
        Marshal.Copy(data, target, 0, len);
        return System.Text.Encoding.UTF8.GetString(target);
    }

    static Dictionary<int,Variable> ExportedObjects;
    static int ExportedID;

    public static Variable SVToVariable(IntPtr sv) {
        if (sv == IntPtr.Zero) {
            //TODO: check - cargo culted
            return Kernel.Nil;
        }
        if (SvOK(sv) == 0) {
            return Kernel.Nil;
        }

        if (SvIOKp(sv) != 0) {
            return Builtins.MakeInt(SvIV(sv));
        } else if (SvNOKp(sv) != 0) {
            return Builtins.MakeFloat(SvNV(sv));
        } else if (SvPOKp(sv) != 0) {
            string s = UnmarshalString(sv); //SvPV_nolen(sv);
            return Kernel.BoxAnyMO(s, Kernel.StrMO);
        } else if (sv_isa(sv,"Niecza::Object") != 0) {
            return ExportedObjects[SvIV(SvRV(sv))];
        } else {
            return new SVVariable(sv);
        }
    }
  
    public Perl5Interpreter() {
        ExportedObjects = new Dictionary<int,Variable>();
        ExportedID = 8;
        string location = System.Reflection.Assembly.GetExecutingAssembly().Location;

        string[] paths = new string[] {"perl5/Niecza/blib/lib","perl5/Niecza/blib/arch"};
        
        for (int i=0;i<2;i++) {
            // Try to construct the path in a platform portable manner
            string p5lib = Path.GetDirectoryName(Path.GetDirectoryName(location));
            foreach (string part in paths[i].Split('/')) {
                p5lib = Path.Combine(p5lib,part);
            }
            paths[i] = p5lib;
        }
        Set_p5embed_create_LoS(delegate(int len,IntPtr data) {
            int id = ExportedID++;
            IntPtr[] target = new IntPtr[len];
            Marshal.Copy(data, target, 0, len);

            string[] args = new string[len];
            for (int i=0;i<len;i++) {
                args[i] = UnmarshalString(target[i]);
            }
            ExportedObjects[id] = Builtins.BoxLoS(args);
            return id;
        });
        Initialize(paths[0],paths[1]);
    }
    ~Perl5Interpreter() {
        Dispose();
    }
    public Variable Eval(string code) {
        IntPtr sv = EvalPerl5(code);
        return SVToVariable(sv);
    }
}

public class SVVariable : Variable {
    public IntPtr sv;
    public SVVariable(IntPtr _sv) {
        sv = _sv;
    }
    public override P6any Fetch() {
        return new SVany(sv);
    }
    public override void Store(P6any v) {
    }
    public override Variable GetVar() {
            return Kernel.BoxAnyMO<Variable>(this, Kernel.ScalarMO);

    }
    public override void Freeze(FreezeBuffer fb) {
            throw new NieczaException("Freezing perl5 SV* NYI.");
    }
}
public class SVany : P6any {
        [DllImport("obj/p5embed.so", EntryPoint="p5method_call")]
        public static extern IntPtr MethodCall(
            string name,
            IntPtr[] arguments,
            int argument_n
        );

        public override void Freeze(FreezeBuffer fb) {
                throw new NieczaException("Freezing perl5 SV* NYI.");
        }

        
        // We can't use the standard char* conversion because some strings can contain nulls
        public static IntPtr MarshalString(string s) {
            byte[] array = System.Text.Encoding.UTF8.GetBytes(s);
            int size = Marshal.SizeOf(typeof(byte)) * (array.Length + 1);

            IntPtr ptr = Marshal.AllocHGlobal(size);

            /* This is a hack not to crash on mono!!! */
            //allocated.Add(ptr, null);

            Marshal.Copy(array, 0, ptr, array.Length);
            Marshal.WriteByte(ptr, array.Length, 0);

            IntPtr sv = Perl5Interpreter.newSVpvn(ptr,array.Length);
            Perl5Interpreter.SvUTF8_on(sv);
            Marshal.FreeHGlobal(ptr);
            return sv;
        }



        public static IntPtr VariableToSV(Variable var) {
            P6any obj = var.Fetch();
            if (obj is SVany) {
                return ((SVany)obj).sv;
            } else if (obj.Does(Kernel.StrMO)) {
                string s = Kernel.UnboxAny<string>(obj);
                return MarshalString(s);
            } else {
                throw new NieczaException("can't convert argument to p5 type");
            }
        }

        static int Context(Variable var) {
            P6any obj = var.Fetch();
            string s = Kernel.UnboxAny<string>(obj);
            if (s == "list") {
                return 0;
            } else if (s == "scalar") {
                return 1;
            } else if (s == "void") {
                return 2;
            } else {
                throw new NieczaException("unknown p5 context type: "+s);
            }
        }

        static IntPtr[] MarshalPositionals(Variable[] pos) {
                IntPtr[] args = new IntPtr[pos.Length];
                for (int i=0;i<pos.Length;i++) {
                    args[i] = VariableToSV(pos[i]);
                }
                return args;
        }

        public IntPtr sv;
        public override Frame InvokeMethod(Frame caller, string name,
                Variable[] pos, VarHash named) {

                if (name == "postcircumfix:<( )>") {
                    int context = 1;
                    if (named != null && named.ContainsKey("context") && named["context"] != null) {
                        context = Context(named["context"]);
                    }
                    IntPtr[] args = MarshalPositionals(pos);
                    IntPtr ret = Perl5Interpreter.SubCall(context,args,args.Length);

            
                    caller.resultSlot = Perl5Interpreter.SVToVariable(ret);
                    return caller;
                } else {
                    IntPtr[] args = MarshalPositionals(pos);
                    IntPtr ret = MethodCall(name,args,args.Length);
                    caller.resultSlot = Perl5Interpreter.SVToVariable(ret);
                }

                return caller;
        }

        public override string ReprName() { return "P6opaque"; }

        public SVany(IntPtr _sv) {
            mo = Kernel.AnyMO;
            sv = _sv;
        }
}

