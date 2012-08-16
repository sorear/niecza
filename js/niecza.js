// This will be moved to m4 later

(function() {

var Niecza = {};

var NUnit = Niecza.Unit = function() {
};
NUnit.thaw = function (tb) {
    tb.reg(var n = new NUnit());

    n.name      = tb.string();
    var srcinfo = tb.strings();
    //if (Builtins.upcall_receiver != null) {
    //    object[] args = new object[srcinfo.Length + 1];
    //    Array.Copy(srcinfo, 0, args, 1, srcinfo.Length);
    //    args[0] = "check_dated";
    //    object result = Builtins.UpCall(args);
    //    if (result is Exception)
    //        throw (Exception)result;
    //    if ((string)result != "ok")
    //        throw new ThawException("dated sources");
    //}

    n.filename = tb.string();
    n.source   = tb.string();
    n.asm_name = tb.string();
    n.dll_name = tb.string();

    n.owner = tb.obj();
    // master-case initialization has to happen before potentially
    // loading any *other* unit
    if (n.owner == n) {
        // is a master unit
        // set this before any potential of aliasing
        n.globals = {};
        var ncon = tb.int();
        //if (Backend.cross_level_load) {
            // don't load a type, throw away constants
            while (ncon-- > 0) {
                tb.String();
                tb.ObjRef();
            }
        //} else {
        //    Assembly assembly = Assembly.Load(n.asm_name);
        //    n.type = tb.type = assembly.GetType(n.asm_name, true);
        //    n.constants = new Dictionary<object,FieldInfo>();
        //    var fields = new Dictionary<string,FieldInfo>();
        //    foreach (FieldInfo fi in n.type.GetFields())
        //        fields[fi.Name] = fi;
        //    var meth = new Dictionary<string,MethodInfo>();

        //    foreach (MethodInfo mi in n.type.GetMethods())
        //        meth[mi.Name] = mi;
        //    RuntimeUnit.reg.methods[n.asm_name] = meth;
        //    while (ncon-- > 0) {
        //        FieldInfo fi = fields[tb.String()];
        //        object val = tb.ObjRef();
        //        n.constants[val] = fi;
        //        fi.SetValue(null, val);
        //    }
        //}

        var ct = tb.int();
        while(ct--) {
            n.globals[tb.string()] = tb.obj();
        }
    }
    else {
        n.globals = n.owner.globals;
        if (n.globals == null) throw ("load goofed");
        n.type = n.owner.type;
    }

    n.depended_units = tb.refs(); // XXX set
    n.subordinates = tb.refs();
    n.mainline   = tb.ObjRef();
    n.bottom     = tb.ref();
    n.our_subs   = tb.refs();
    n.is_mainish = tb.byte() != 0;

    if (n.name == "CORE") {
        //FieldInfo[] kf = typeof(Kernel).GetFields();
        //Array.Sort<FieldInfo>(kf,
        //        (f1, f2) => string.CompareOrdinal(f1.Name, f2.Name));
        //foreach (FieldInfo f in kf) {
        //    if (f.GetCustomAttributes(typeof(CORESavedAttribute), true).Length != 0) {
        //        f.SetValue(null, tb.ObjRef());
        //    }
        //}
    }
    return n;
};


var NSer = Niecza.Serialization = {};
var NSCodes = Niecza.Serialization.Codes = {};

var NSThaw = Niecza.Serialization.Thaw = function (buffer, callback) {
    this.buffer = buffer;
    this.view = new DataView(buffer);
    this.rawView = new Uint8Array(buffer);
    this.offset = 0;
    this.result = null;

    try {
        var sig = this.string();
        if (sig != this.SIGNATURE)
            throw "Signature mismatch needed "+SIGNATURE+" got "+sig;
        var ver = this.int();
        if (ver != this.VERSION)
            throw "Version mismatch needed "+VERSION+" got "+ver;
        console.log(sig,ver);
        this.result = this.obj();
    } catch(e) {
        console.log(e);
        callback(false, e);
        return;
    }
    callback(true, this.result);
};

NSThaw.prototype.$className = 'Niecza.Serialization.Thaw';
NSThaw.prototype.SIGNATURE = 'Niecza-Serialized-Module';
NSThaw.prototype.VERSION = 29;

NSThaw.prototype.int = function() {
    var shift = 0, accum = 0;
    var offset = this.offset, raw = this.rawView;
    while (true) {
        var b = raw[offset++];
        accum |= ((b & 127) << shift);
        shift += 7;
        if (!(b & 128)) {
            if (b & 64) {
                accum = accum - (1 << shift);
            }
            console.log(accum, " at ", offset);
            this.offset = offset;
            return accum;
        }
    }
};

NSThaw.prototype.uint = function() {
    var shift = 0, accum = 0;
    var offset = this.offset, raw = this.rawView;
    while (true) {
        var b = raw[offset++];
        accum |= ((b & 127) << shift);
        shift += 7;
        if (!(b & 128)) {
            console.log(accum, " at ", offset);
            this.offset = offset;
            return accum;
        }
    }
};

NSCodes.Null = 0;
NSCodes.ForeignRef = 1;
NSCodes.SelfRef = 2;
NSCodes.NewUnitRef = 3;
NSCodes.FakeUnitRef = 4;
NSCodes.RuntimeUnit = 5;
NSCodes.SubInfo = 6;
NSCodes.STable = 7;
NSCodes.StashEnt = 8;
NSCodes.Rat = 9;
NSCodes.FatRat = 10;
NSCodes.Complex = 11;
NSCodes.BigInteger = 12;
NSCodes.VarDeque = 13;
NSCodes.VarHash = 14;
NSCodes.DispatchEnt = 15;
NSCodes.RxFrame = 16;
NSCodes.P6how = 17;
NSCodes.ReflectObj = 18;
NSCodes.CC = 19;
NSCodes.AltInfo = 20;
NSCodes.Signature = 21;
NSCodes.Parameter = 22;
NSCodes.P6opaque = 23;
NSCodes.Frame = 24;
NSCodes.Cursor = 25;
NSCodes.String = 26;
NSCodes.ArrP6any = 27;
NSCodes.ArrVariable = 28;
NSCodes.ArrString = 29;
NSCodes.ArrCC = 30;
NSCodes.Boolean = 31;
NSCodes.Int = 32;
NSCodes.Double = 33;
NSCodes.Type = 34;
NSCodes.RWVariable = 35;
NSCodes.RWVariable_1 = 36;
NSCodes.ListVariable = 37;
NSCodes.SubstrLValue = 38;
NSCodes.TiedVariable = 39;
NSCodes.Blackhole = 40;
NSCodes.SubViviHook = 41;
NSCodes.ArrayViviHook = 42;
NSCodes.NewArrayViviHook = 43;
NSCodes.HashViviHook = 44;
NSCodes.NewHashViviHook = 45;
NSCodes.LADNone = 46;
NSCodes.LADNull = 47;
NSCodes.LADDot = 48;
NSCodes.LADDispatcher = 49;
NSCodes.LADImp = 50;
NSCodes.LADStr = 51;
NSCodes.LADStrNoCase = 52;
NSCodes.LADMethod = 53;
NSCodes.LADParam = 54;
NSCodes.LADQuant = 55;
NSCodes.LADSequence = 56;
NSCodes.LADAny = 57;
NSCodes.LADCC = 58;

NSThaw.prototype.string = function() {
    var l = this.int();
    if (l < 0) return null;
    var ary = [];
    while (l--) ary[ary.length] = this.uint();
    return String.fromCharCode.apply(String, ary); //ick
};

NSThaw.prototype.obj = function() {
    var code = this.rawView[this.offset++];

    switch (code) {
        default:
            throw "Unhandled serialization code " + code;
    }
}

NSer.loadSerFileEnd = function(ev) {
    var xhr = ev.target,
        cb  = xhr.nieczaCallback;
    if (xhr.status >= 200 && xhr.status < 300) {
        new NSThaw(xhr.response, cb);
    } else {
        cb(false, xhr.statusText, xhr);
    }
};

NSer.loadSerFile = function (url, callback) {
    var xhr = new XMLHttpRequest();
    xhr.responseType = 'arraybuffer';
    xhr.onloadend = NSer.loadSerFileEnd;
    xhr.nieczaCallback = callback;
    xhr.open('GET', url);
    xhr.send(null);
};

window.Niecza = Niecza;

})();
