
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
    var net = promise.ajax(method, url, data, {Accept: "application/json"});
    promise.chain([function() { return net; }, decode_json]).then(function(err, ret) {
      if (err) {
        onErr(err);
      } else {
        onRet(ret);
      }
    });
    return;
  };

  var get = http.bind(null, "GET");

  var post = http.bind(null, "POST");

  return {
    http: http,
    get: get,
    post: post
  };

});

