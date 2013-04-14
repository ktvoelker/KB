
window.$ = document.querySelector.bind(document);

window.$$ = document.querySelectorAll.bind(document);

window.$id = document.getElementById.bind(document);

window.HTMLElement.prototype.$ = window.HTMLElement.prototype.querySelector;

window.HTMLElement.prototype.$$ = window.HTMLElement.prototype.querySelectorAll;

