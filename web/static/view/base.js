
define([], function() {

  var loadCss = function(css) {
    if (typeof(css) === "string") {
      css = [css];
    }
    for (var i in css) {
      var linkElem = document.createElement("link");
      linkElem.setAttribute("rel", "stylesheet");
      linkElem.setAttribute("type", "text/css");
      linkElem.setAttribute("href", css[i]);
      document.head.appendChild(linkElem);
    }
  };

  var mixMethods = function(moduleName, cls, methods, init) {
    var targets = document.querySelectorAll("." + cls);
    for (var i = 0; i < targets.length; ++i) {
      for (var name in methods) {
        if (targets[i][name]) {
          console.warn("Overwriting method " + name +
            " of a target element of view mixin " + moduleName);
        }
        targets[i][name] = methods[name];
      }
      if (init) {
        init.call(targets[i]);
      }
    }
  };

  var mix = function(moduleName) {
    require([moduleName], function(module) {
      if (!module.cls) {
        throw "View mixin " + moduleName + " does not have a CSS class name";
      }
      if (module.css) {
        loadCss(module.css);
      }
      if (module.methods) {
        mixMethods(moduleName, module.cls, module.methods, module.init);
      }
    });
  };

  return {
    mix: mix
  };

});

