
define([], function() {

  var Completer = function(f, text) {
    this._f = f;
    this._text = text;
    this._cur = null;
    this._completions = [];
    this._refreshCompletions();
  };

  Completer.prototype.getCurrentCompletion = function() {
    return this._cur === null ? null : this._completions[this._cur];
  };

  Completer.prototype._refreshCompletions = function() {
    var old = this.getCurrentCompletion();
    this._completions = this._f(this._text);
    if (this._completions.length) {
      var index = this._completions.indexOf(old);
      this._cur = index >= 0 ? index : 0;
    } else {
      this._cur = null;
    }
    this._refreshDisplay();
  };

  Completer.prototype._refreshDisplay = function() {
    // TODO
  };

  Completer.prototype.setText = function(text) {
    this._text = text;
    this._refreshCompletions();
  };

  Completer.prototype._addToIndex = function(delta) {
    if (this._cur === null) {
      return null;
    } else {
      this._cur = mod(this._cur + delta, this._completions.length);
      return this._completions[this._cur];
    }
  };

  Completer.prototype.previous = function() {
    return this._addToIndex(-1);
  };

  Completer.prototype.next = function() {
    return this._addToIndex(1);
  };

  Completer.prototype.setVisible = function(vis) {
    // TODO
  };

  return {
    Completer: Completer
  };

});

