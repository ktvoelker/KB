
define(["completer"], function(completer) {

  /**
   * Options:
   * initial:      a list of initial refs
   * completer:    a function that returns a list of refs, given a partial ref
   * shouldSort:   whether refs should be sorted when editing is stopped
   * sorter:       a function that sorts a list of refs
   * allowInsert:  whether the user can insert new refs between existing ones
   *
   * Ref properties:
   * label
   * menuLabel
   * cssClass
   *
   * TODO: support allowInsert and shouldSort options
   */
  var RefEditor = function(elem, options) {
    this._elem = elem;
    this._partial = null;
    this._highlights = [];
    this._editing = null;
    this.setEditing(false);
    this._refsAdded = [];
    this._refsDeleted = [];

    /**
     * Read options and coalesce with defaults
     */
    this._refs = (options.initial || []).forEach(identity);
    this._completer = options.completer || function(partial) { return []; };
    this._shouldSort = Boolean(options.shouldSort);
    this._sorter =
      this._shouldSort ? (options.sorter || Array.prototype.sort) : identity;
    this._allowInsert = Boolean(options.allowInsert);

    /**
     * Add event listeners
     */
    this._elem.addEventListener("keypress", this._handleKeyPress.bind(this), false);
    var sc = this._handleSelectionChange.bind(this);
    this._elem.addEventListener("focus", sc, false);
    document.addEventListener("selectionchange", sc, false);
    this._observer = new MutationObserver(this._handleMutation.bind(this));
    this._observer.observe(this._elem, {
      childList: true,
      characterData: true,
      subtree: true
    });
  };
  
  RefEditor.prototype._handleKeyPress = function(evt) {
    var partial = this._partial;
    if (evt.charCode === 11) { // Tab
      if (partial) {
        // Jump to the next completion
        evt.preventDefault();
        partial.completer.next();
      }
    } else if (evt.charCode === 13) { // Enter
      // Save the current completion of the current partial
      evt.preventDefault();
      if (partial) {
        var ref = partial.completer.getCurrentCompletion();
        if (ref) {
          this._partial = null;
          partial.completer.setVisible(false);
          partial.parentNode.insertBefore(this._renderRef(ref), partial);
          partial.parentNode.removeChild(partial);
        }
      } else {
        // Save the form
        this._elem.dispatchEvent(
            new CustomEvent("save", {bubbles: true, cancelable: true}));
      }
    }
    return true;
  };

  RefEditor.prototype._handleSelectionChange = function(evt) {
    var sel = document.getSelection();
    // Clear old highlight, if any
    this._highlights.forEach(function(h) { h.removeClass("selected"); });
    // Set new highlight, if any
    var hl = function(node) {
      if (node instanceof HTMLElement && node.hasClass("ref")) {
        node.addClass("selected");
      }
    };
    var nextPartial = null;
    if (sel.type === "Caret") {
      hl(sel.focusNode);
      if (sel.focusNode instanceof CharacterData) {
        nextPartial = sel.focusNode;
      }
    } else {
      sel.forEach(hl);
    }
    if (nextPartial !== this._partial) {
      if (this._partial) {
        // Hide old completions
        this._partial.completer.setVisible(false);
      }
      // Remember the current partial
      this._partial = nextPartial;
      if (nextPartial) {
        // Show new completions
        if (!nextPartial.completer) {
          console.log("Creating completer in selectionChange callback");
          nextPartial.completer =
            new completer.Completer(this._completer, nextPartial.data);
        }
        nextPartial.completer.setVisible(true);
      }
    }
  };

  RefEditor.prototype._handleMutation = function(records) {
    var that = this;
    records.forEach(function(record) {
      if (record.type === "childList") {
        record.removedNodes.forEach(function(node) {
          if (node instanceof HTMLElement) {
            // Element removed: record the ref deletion
            var ref = node.ref;
            if (!that._refsAdded.removeFirst(ref)) {
              that._refsDeleted.push(ref);
            }
          }
        });
        record.addedNodes.forEach(function(node) {
          if (node instanceof HTMLElement) {
            // Element added: record the ref addition
            var ref = node.ref;
            that._refsDeleted.removeFirst(ref);
            that._refsAdded.push(ref);
          } else if (node instanceof CharacterData) {
            // Text node added: create a completion menu
            console.log("Creating completer in mutation callback");
            node.completer = new completer.Completer(that._completer, node.data);
          }
        });
      } else if (record.type === "characterData") {
        // Text node changed: update the completions
        record.target.completer.setText(record.target.data);
      }
    });
  };

  RefEditor.prototype.getChanges = function() {
    var ret = {
      add: this._refsAdded,
      del: this._refsDeleted
    };
    this._refsAdded = [];
    this._refsDeleted = [];
    return ret;
  };

  RefEditor.prototype.getValue = function() {
    return this._refs.map(identity);
  };

  RefEditor.prototype.setValue = function(refs) {
    this._refs = refs.map(identity);
    this.refresh();
  };

  RefEditor.prototype._renderRef = function(ref) {
    var span = document.createElement("span");
    span.contentEditable = false;
    span.addClass("ref");
    if (ref.cssClass) {
      span.addClass(ref.cssClass);
    }
    span.innerText = ref.label;
    span.ref = ref;
    return span;
  };

  RefEditor.prototype.refresh = function() {
    this._elem.removeAllChildren();
    var that = this;
    this._refs.forEach(function(ref) {
      that._elem.appendChild(that._renderRef(ref));
    });
  };

  RefEditor.prototype.setEditing = function(editing) {
    if (editing !== this._editing) {
      this._elem.contentEditable = editing;
      this._editing = editing;
      if (!editing && this._shouldSort) {
        this.refresh();
      }
    }
  };

  return {
    RefEditor: RefEditor
  };

});

