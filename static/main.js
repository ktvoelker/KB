
requirejs(["form", "notes", "auth"], function(forms, notes, auth) {

  var startApp = function() {
    var ns = new notes.Notes($id("notes"));
    $id("new-note").addEventListener("click", function() {
      ns.newNote();
    }, false);
    $$(".one-line-editor").forEach(function(elem) {
      elem.addEventListener("keypress", function(evt) {
        if (evt.charCode === 13) {
          evt.preventDefault();
        }
        return true;
      }, false);
    });
  };

  var loginForm = document.forms.login;
  forms.background(loginForm);
  loginForm.addEventListener("response", function() {
    startApp();
    auth.showApp();
  });

  auth.check(startApp);

});

