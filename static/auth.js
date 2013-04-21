
define(["net"], function(net) {

  var showLogin = function() {
    $id("curtain").style.display = 'none';
    $id("app").style.display = 'none';
    $id("login").style.display = '';
    $id("splash").style.display = 'none';
    $id("curtain").style.display = 'block';
  };

  var showApp = function() {
    $id("curtain").style.display = 'none';
    $id("login").style.display = 'none';
    $id("app").style.display = '';
    $id("splash").style.display = 'none';
    $id("curtain").style.display = 'block';
  };

  var check = function(startApp) {
    net.get("/ping", {}, showLogin, function() {
      startApp();
      showApp();
    });
  };

  return {
    check: check,
    showApp: showApp
  };

});

