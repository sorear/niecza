// This will be moved to m4 later

(function() {

var Niecza = {};

var NSer = Niecza.Serialization = {};

var NSThaw = Niecza.Serialization.Thaw = function (buffer, callback) {
    this.buffer = buffer;
    this.view = new DataView(buffer);
    this.rawView = new Uint8Array(buffer);
    this.offset = 0;

    try {
        var sig = this.string();
        if (sig != this.SIGNATURE)
            throw "Signature mismatch needed "+SIGNATURE+" got "+sig;
        var ver = this.int();
        if (ver != this.VERSION)
            throw "Version mismatch needed "+VERSION+" got "+ver;
        console.log(sig,ver);
    } catch(e) {
        callback(false, e);
        return;
    }
    callback(true, this);
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

NSThaw.prototype.string = function() {
    var l = this.int();
    if (l < 0) return null;
    var ary = [];
    while (l--) ary[ary.length] = this.uint();
    return String.fromCharCode.apply(String, ary); //ick
};


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
