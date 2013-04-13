
function decode_json(err, str) {
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
}

function ajax_json(method, url, data) {
  var net = promise[method].call(promise, url, data, {Accept: "application/json"});
  return promise.chain([function() { return net; }, decode_json]);
}

function get_json(url, data) {
  return ajax_json("get", url, data);
}

function submit_for_json(form) {
  var method = form.getAttribute("method").toLowerCase();
  var url = form.getAttribute("action");
  var data = {};
  for (var i = 0; i < form.elements.length; ++i) {
    var elem = form.elements[i];
    if (elem.name && elem.name !== "") {
      data[elem.name] = form[elem.value];
    }
  }
  var p = ajax_json(method, url, data);
  p.then(function(err, json) {
    if (err) {
      alert(err);
      return;
    }
    document.getElementById("output").innerText = JSON.stringify(json);
  });
  return false;
}

function post_json(url, data) {
  return ajax_json("post", url, data);
}

function main() {
}

