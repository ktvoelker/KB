
requirejs(["net"], function(net) {

  for (var i = 0; i < document.forms.length; ++i) {
    (function(form) {
      form.addEventListener("submit", function() {
        net.form(form);
        return false;
      }, false);
    })(document.forms[i]);
  }

  net.form(document.forms[1]);

});

