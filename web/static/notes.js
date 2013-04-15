
define(["net", "form"], function(net, form) {

  var Note = function(data, elem) {
    this._data = data;
    this._elem = elem;
    this._display = elem.$(".display");
    this._editor = elem.$(".editor");
    form.background(this._editor);
    this._editing = null;
    this._setEditing(false);
    this.refresh();
  };

  Note.prototype._setEditing = function(editing) {
    if (editing !== this._editing) {
      if (editing) {
        this._display.style.display = 'none';
        this._editor.style.display = '';
      } else {
        this._display.style.display = '';
        this._editor.style.display = 'none';
      }
      this._editing = editing;
    }
  };

  Note.prototype.refreshDisplay = function() {
    var d = this._data;
    var v = this._display;
    v.$(".title").innerText = d.title;
    v.$(".updated").innerText = d.updated;
    v.$(".body").innerText = d.body;
  };

  Note.prototype.refreshEditor = function() {
    var d = this._data;
    var v = this._editor;
    v.id.value = d.id;
    v.title.value = d.title;
    v.body.value = d.body;
  };

  Note.prototype.refresh = function() {
    this.refreshDisplay();
    this.refreshEditor();
  };

  Note.prototype.edit = function() {
    this._setEditing(true);
  };

  Note.prototype.save = function() {
    var v = this._editor;
    var d = this._data;
    d.title = v.title.value;
    d.body = v.body.value;
    this.refreshDisplay();
    this._setEditing(false);
    // TODO save on the server
  };

  var Notes = function(elem) {
    this._elem = elem;
    this._template = elem.$(".template");
    this._template.parentElement.removeChild(this._template);
    this._template.removeClass("template");
    this._notes = [];
    this.refresh();
  };

  Notes.prototype.refresh = function() {
    // TODO get all from the server
  };

  Notes.prototype.newNote = function() {
    var elem = this._template.cloneNode(true);
    var note = new Note({title: "", updated: null, body: ""}, elem);
    note.edit();
    this._elem.insertBefore(elem, this._elem.children[0]);
  };

  return {
    Note:  Note,
    Notes: Notes
  };

});

