
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
          if (val) {
            url = url.replace(re, val);
          } else {
            url = form.getAttribute("data-action-new");
            var listener = (function(name, elem) {
              var ret = function(evt) {
                elem.value = evt.detail[name];
                form.removeEventListener("response", ret, false);
              };
              return ret;
            })(name, elem);
            form.addEventListener("response", listener, false);
          }
        } else {
          data[name] = val;
        }
      }
    }
    if (url.indexOf(":") >= 0) {
      alert("Unfilled path parameters in " + url);
    } else {
      net.http(method, url, data, alert, function(obj) {
        form.dispatchEvent(new CustomEvent(
            "response",
            {
              detail: obj,
              bubbles: false,
              cancelable: false
            }));
      });
    }
  };

  var background = function(form) {
    form.addEventListener("submit", function(evt) {
      evt.preventDefault();
      submit(form);
    }, false);
  };

  return {
    submit: submit,
    background: background
  };

});

