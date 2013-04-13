
define(["external/promise"], function(promise) {
  var decode_json = function(err, str) {
    var p = new promise.Promise();
    if (err) {
      p.done(err, null);
    } else {
      var json = JSON.parse(str);
      if (json) {
        p.done(null, json);
      } else {
        p.done(str, null);
      }
    }
    return p;
  };
  var http = function(method, url, data, onErr, onRet) {
    var net = promise[method].call(promise, url, data, {Accept: "application/json"});
    promise.chain([function() { return net; }, decode_json]).then(function(err, ret) {
      if (err) {
        onErr(err);
      } else {
        onRet(ret);
      }
    });
    return;
  };
  var get = http.bind(null, "get");
  var post = http.bind(null, "post");
  var form = function(form) {
    var method = form.getAttribute("method").toLowerCase();
    var url = form.getAttribute("action");
    var data = {};
    for (var i = 0; i < form.elements.length; ++i) {
      var elem = form.elements[i];
      if (elem.name && elem.name !== "") {
        data[elem.name] = form[elem.value];
      }
    }
    http(method, url, data, alert, function(obj) {
      document.getElementById("output").innerText = JSON.stringify(obj);
    });
  };
  return {
    http: http,
    get: get,
    post: post,
    form: form
  };
});

