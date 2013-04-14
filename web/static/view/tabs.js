
define(["require"], function(require) {

  var show = function(what) {
    var ty = typeof(what);
    if (ty === "string") {
      this.showId(what);
    } else if (ty === "number") {
      this.showIndex(what);
    } else {
      this.showElem(what);
    }
  };

  const CUR = "cur";

  var showId = function(id) {
    var tab = this.querySelector(":scope > #" + id);
    if (!tab) {
      throw "Tab not found: " + id;
    }
    this.showElem(tab);
  };

  var showIndex = function(index) {
    var tab = this.children[index];
    if (!tab) {
      throw "Tab index out of range: " + index;
    }
    this.showElem(tab);
  };

  var showElem = function(tab) {
    if (!tab) {
      throw "No tab passed to showElem"
    }
    var tabs = this.children;
    for (var i = 0; i < tabs.length; ++i) {
      if (tabs[i] !== tab) {
        tabs[i].style.display = 'none';
      }
    }
    tab.style.display = '';
  };

  var init = function() {
    this.showIndex(0);
  };

  return {
    cls: "tabs",
    css: require.toUrl("./tabs.css"),
    init: init,
    methods: {
      show: show,
      showId: showId,
      showIndex: showIndex,
      showElem: showElem
    }
  };

});

