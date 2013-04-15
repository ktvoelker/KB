
window.NodeList.prototype.forEach = function(f) {
  for (var i = 0; i < this.length; ++i) {
    f(this[i]);
  }
};

window.$ = document.querySelector.bind(document);

window.$$ = document.querySelectorAll.bind(document);

window.$id = document.getElementById.bind(document);

window.HTMLElement.prototype.$ = window.HTMLElement.prototype.querySelector;

window.HTMLElement.prototype.$$ = window.HTMLElement.prototype.querySelectorAll;

