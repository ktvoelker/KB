
requirejs(["form", "notes", "auth"], function(forms, notes, auth) {

  var startApp = function() {
    var ns = new notes.Notes($id("notes"));
    $id("new-note").addEventListener("click", function() {
      ns.newNote();
    }, false);
  };

  var loginForm = document.forms.login;
  forms.background(loginForm);
  loginForm.addEventListener("response", function() {
    startApp();
    auth.showApp();
  });

  auth.check(startApp);

});

