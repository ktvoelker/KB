
requirejs(["form", "view/base", "notes"], function(forms, views, notes) {

  $$("#debug form").forEach(function(form) {
    forms.background(form);
    form.addEventListener("response", function(evt) {
      document.getElementById("output").innerText = JSON.stringify(evt.detail);
    }, false);
  });

  views.mix("view/tabs");

  var notes = new notes.Notes($id("notes"));
  window.DEBUG = window.DEBUG || {};
  window.DEBUG.model = notes;
  $id("new-note").addEventListener("click", function() {
    notes.newNote();
  }, false);

});

