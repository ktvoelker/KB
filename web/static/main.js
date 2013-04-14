
requirejs(["form", "view/base"], function(form, views) {

  views.mix("view/tabs");
  form.submit(document.forms[1]);

});

