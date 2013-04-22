
define(["net", "form", "time"], function(net, form, time) {

  var Note = function(data, elem) {
    var that = this;
    this._data = data;
    this._elem = elem;
    this._elem.addEventListener("click", function() {
      that._setBodyVisible(true);
    }, false);
    this._display = elem.$(".display");
    this._display.$(".edit-note").addEventListener("click", function() {
      that.edit();
    });
    this._display.$(".title").addEventListener("click", function(evt) {
      that._toggleBodyVisible();
      evt.stopPropagation();
    });
    this._editor = elem.$(".editor");
    this._editor.addEventListener("submit", function() {
      that.save();
    }, false);
    this._editor.addEventListener("response", function(r) {
      var d = r.detail;
      var any = false;
      for (var i in d) {
        any = true;
        that._data[i] = d[i];
      }
      if (any) {
        that.refresh();
      }
    }, false);
    form.background(this._editor);
    this._editing = null;
    this._setBodyVisible(false);
    this._setEditing(false);
    this.refresh();
  };

  Note.prototype._refreshEditorBodyHeight = function() {
    if (this._bodyVisible && !this._editing) {
      this._editor.$(".body").style.height =
        (this._display.$(".body").offsetHeight - 20) + "px";
    }
  };

  Note.prototype._setBodyVisible = function(vis) {
    var display = vis ? "" : "none";
    this._elem.$$(".body").forEach(function(body) {
      body.style.display = display;
    });
    var prev = this._bodyVisible;
    this._bodyVisible = Boolean(vis);
    this._refreshEditorBodyHeight();
    return prev;
  };

  Note.prototype._toggleBodyVisible = function() {
    this._setBodyVisible(!this._bodyVisible);
  };

  Note.prototype._setEditing = function(editing) {
    if (editing !== this._editing) {
      if (editing) {
        this._prevBodyVisible = this._setBodyVisible(true);
        this._display.style.display = 'none';
        this._editor.style.display = '';
      } else {
        this._display.style.display = '';
        this._editor.style.display = 'none';
        if (this._editing === true) {
          this._refreshEditorBodyHeight();
          if (!this._prevBodyVisible) {
            this._setBodyVisible(false);
          }
          delete this._prevBodyVisible;
        }
      }
      this._editing = editing;
    }
  };

  Note.prototype.refreshDisplay = function() {
    var d = this._data;
    var v = this._display;
    v.$(".title").innerText = d.title;
    if (this._updatedDisplay) {
      this._updatedDisplay.setTime(d.updated);
    } else {
      this._updatedDisplay = new time.Display(d.updated, v.$(".updated"));
    }
    v.$(".body").innerText = d.body;
  };

  Note.prototype.refreshEditor = function() {
    var d = this._data;
    var v = this._editor;
    v.id.value = d.id ? d.id : "";
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
  };

  Note.prototype.fetch = function() {
    if (this._data.id) {
      var that = this;
      net.get("/note/" + this._data.id, {}, alert, function(data) {
        that._data = data;
        that.refresh();
      });
    }
  };

  var Notes = function(elem) {
    this._elem = elem;
    this._template = elem.$(".template");
    this._template.parentElement.removeChild(this._template);
    this._template.removeClass("template");
    this._notes = [];
    this.refresh();
  };

  Notes.prototype._makeNote = function(data) {
    return new Note(data, this._template.cloneNode(true));
  };

  Notes.prototype.refresh = function() {
    var that = this;
    net.get("/note/list", {}, alert, function(obj) {
      that._notes = [];
      that._elem.innerHTML = "";
      obj.notes.forEach(function(data) {
        var note = that._makeNote(data);
        that._notes.push(note);
        that._elem.appendChild(note._elem);
        note.fetch();
      });
    });
  };

  Notes.prototype.newNote = function() {
    var note = this._makeNote({title: "", updated: null, body: ""});
    note.edit();
    this._notes.unshift(note);
    this._elem.insertBefore(note._elem, this._elem.children[0]);
  };

  return {
    Note:  Note,
    Notes: Notes
  };

});

