
define(["net", "time", "ref_editor"], function(net, time, ref_editor) {

  var tagToRef = function(tag) {
    return {
      label: tag,
      menuLabel: tag,
      cssClass: "tag"
    };
  };

  var Note = function(data, elem) {
    var that = this;
    this._data = data;
    this._elem = elem;
    this._elem.addEventListener("click", function() {
      that._setBodyVisible(true);
    }, false);
    this._display = elem.$(".display");
    this._tagEditor = new ref_editor.RefEditor(this._display.$(".tags"), {
      initial: [],
      completer: function(str) {
        return [{
          label: str,
          menuLabel: str,
          cssClass: "ref"
        }];
      },
      shouldSort: false,
      allowInsert: false
    });
    this._button = this._display.$("button");
    this._button.addEventListener("click", function() {
      if (that._editing) {
        that.save();
      } else {
        that.edit();
      }
    });
    this._display.$(".title").addEventListener("click", function(evt) {
      if (!that._editing) {
        that._toggleBodyVisible();
        evt.stopPropagation();
      }
    });
    var editors = {
      title: this._display.$(".title"),
      body:  this._display.$(".body")
    };
    this._editors = editors;
    this._editing = null;
    this._setBodyVisible(false);
    this._setEditing(false);
    this.refresh();
  };

  Note.prototype._handleResponse = function(r) {
    var d = r.detail;
    var any = false;
    for (var i in d) {
      any = true;
      this._data[i] = d[i];
    }
    if (any) {
      this.refresh();
    }
  };

  Note.prototype._setBodyVisible = function(vis) {
    var display = vis ? "" : "none";
    this._elem.$$(".body").forEach(function(body) {
      body.style.display = display;
    });
    var prev = this._bodyVisible;
    this._bodyVisible = Boolean(vis);
    return prev;
  };

  Note.prototype._toggleBodyVisible = function() {
    this._setBodyVisible(!this._bodyVisible);
  };

  Note.prototype._setEditing = function(editing) {
    if (editing !== this._editing) {
      this._button.innerText = editing ? "Save" : "Edit";
      if (editing) {
        this._prevBodyVisible = this._setBodyVisible(true);
      } else {
        if (this._editing === true) {
          if (!this._prevBodyVisible) {
            this._setBodyVisible(false);
          }
          delete this._prevBodyVisible;
        }
      }
      for (var i in this._editors) {
        this._editors[i].contentEditable = editing;
      }
      this._tagEditor.setEditing(editing);
      this._editing = editing;
    }
  };

  Note.prototype.refresh = function() {
    var d = this._data;
    this._editors["title"].innerText = d.title;
    if (this._updatedDisplay) {
      this._updatedDisplay.setTime(d.updated);
    } else {
      this._updatedDisplay = new time.Display(d.updated, this._display.$(".updated"));
    }
    if (d.tags) {
      this._tagEditor.setValue(d.tags.map(tagToRef));
    }
    if (d.body) {
      this._editors["body"].innerText = d.body;
    }
  };

  Note.prototype.edit = function() {
    this._setEditing(true);
  };

  var tagLabel = function(ref) {
    return ref.label;
  };

  Note.prototype.save = function() {
    this._setEditing(false);
    var d = this._data;
    d.title = this._editors["title"].innerText;
    d.body = this._editors["body"].innerText;
    var tagChanges = this._tagEditor.getChanges();
    var url = "/note/" + (d.id ? d.id : "new");
    var params = {
      title:    d.title,
      body:     d.body,
      add_tags: tagChanges.add.map(tagLabel).join(","),
      del_tags: tagChanges.del.map(tagLabel).join(",")
    };
    net.post(url, params, window.alert, this._handleResponse.bind(this));
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
    var note = this._makeNote({title: "", updated: null, body: "", tags: []});
    note.edit();
    this._notes.unshift(note);
    this._elem.insertBefore(note._elem, this._elem.children[0]);
  };

  return {
    Note:  Note,
    Notes: Notes
  };

});

