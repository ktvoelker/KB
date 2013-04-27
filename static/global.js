
window.NodeList.prototype.forEach = function(f) {
  for (var i = 0; i < this.length; ++i) {
    f(this[i]);
  }
};

window.Array.prototype.removeFirst = function(x) {
  var index = this.indexOf(x);
  if (index >= 0) {
    this.splice(index, 1);
    return true;
  } else {
    return false;
  }
};

window.Array.prototype.removeAll = function(x) {
  var index = this.indexOf(x);
  var count = 0;
  while (index >= 0) {
    ++count;
    this.splice(index, 1);
    index = this.indexOf(x);
  }
  return count;
};

window.Node.prototype.removeAllChildren = function() {
  var rc = this.removeChild.bind(this);
  var cn = this.childNodes;
  for (var i = cn.length - 1; i >= 0; --i) {
    rc(cn[i]);
  }
};

window.Selection.prototype.forEach = function(f) {
  // TODO
  console.warn("Not implemented: window.Selection.prototype.forEach");
};

window.$ = document.querySelector.bind(document);

window.$$ = document.querySelectorAll.bind(document);

window.$id = document.getElementById.bind(document);

window.HTMLElement.prototype.$ = window.HTMLElement.prototype.querySelector;

window.HTMLElement.prototype.$$ = window.HTMLElement.prototype.querySelectorAll;

window.identity = function(x) { return x; };

(function() {

  var po = function(c, x) {
    return c.prototype.isPrototypeOf(x);
  };

  var primTypes = {
    string: true,
    number: true,
    boolean: true
  };

  var primProtos = [String, Number, Boolean];
  
  // TODO dates and regexen?

  window.clone = function(x) {
    if (x === null || x === undefined) {
      return x;
    }
    var t = typeof(x);
    if (primTypes[t]) {
      return x;
    }
    if (t !== "object") {
      throw "Unexpected value in clone";
    }
    if (po(Array, x) || po(NodeList, x)) {
      return x.forEach(window.clone);
    }
    if (!po(Object, x)) {
      throw "Unexpected object in clone";
    }
    var y = {};
    for (var i in x) {
      y[i] = window.clone(x[i]);
    }
    return y;
  };

})();

var mod = function(a, b) {
  var x = Math.abs(a);
  return (a + x + b - (x % b)) % b;
};

