
define(["net"], function(net) {

  var pathParam = function(name) {
    return new RegExp(":\\b" + name + "\\b");
  };

  var submit = function(form) {
    var method = form.getAttribute("method");
    var url = form.getAttribute("action");
    var data = {};
    for (var i = 0; i < form.elements.length; ++i) {
      var elem = form.elements[i];
      var name = elem.name;
      if (name && name !== "") {
        var val = elem.value;
        var re = pathParam(name);
        if (re.test(url)) {
          url = url.replace(re, val);
        } else {
          data[name] = val;
        }
      }
    }
    if (url.indexOf(":") >= 0) {
      alert("Unfilled path parameters in " + url);
    } else {
      net.http(method, url, data, alert, function(obj) {
        document.getElementById("output").innerText = JSON.stringify(obj);
      });
    }
  };

  for (var i = 0; i < document.forms.length; ++i) {
    (function(form) {
      form.addEventListener("submit", function(evt) {
        evt.preventDefault();
        submit(form);
      }, false);
    })(document.forms[i]);
  }

  return {
    submit: submit
  };

});

