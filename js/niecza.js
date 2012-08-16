// This will be moved to m4 later

(function() {

var Niecza = {};

var NSer = Niecza.Serialization = {};

var NSThaw = Niecza.Serialization.Thaw = function (buffer, callback) {
    this.buffer = buffer;
    this.view = new DataView(buffer);
    this.offset = 0;
};

NSThaw.prototype.$className = 'Niecza.Serialization.Thaw';


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
