
define([], function() {

  const MILLIS_PER_MINUTE = 1000 * 60;

  var Unit = function(prevUnit, sizeInPrevUnits, unitName, unitChunkInPrevUnits) {
    if (prevUnit) {
      prevUnit._nextUnit = this;
    }
    this._prevUnit = prevUnit;
    this._nextUnit = null;
    this._sizeInPrevUnits = sizeInPrevUnits;
    this._unitName = unitName;
    this._unitChunkInPrevUnits = unitChunkInPrevUnits;
  };

  // TODO memoize
  Unit.prototype._sizeInBaseUnits = function() {
    return this._prevUnit
      ? this._prevUnit._sizeInBaseUnits() * this._sizeInPrevUnits
      : 1;
  };

  // TODO memoize
  Unit.prototype._unitChunkInBaseUnits = function() {
    return this._prevUnit
      ? this._unitChunkInPrevUnits * this._prevUnit._sizeInBaseUnits()
      : 1;
  };

  // TODO memoize
  Unit.prototype._maxInBaseUnits = function() {
    return this._nextUnit
      ? this._nextUnit._sizeInBaseUnits() - (this._unitChunkInBaseUnits() / 2)
      : null;
  };

  Unit.prototype._displayNum = function(n) {
    return this._prevUnit
      ? Math.round(
          Math.round(n / this._unitChunkInBaseUnits()) * this._unitChunkInPrevUnits)
      : Math.round(n);
  };

  Unit.prototype.format = function(n) {
    var max = this._maxInBaseUnits();
    if (max && n > max) {
      return this._nextUnit.format(n);
    } else {
      return {
        num: this._displayNum(n),
        unit: this._unitName,
        timer: this._sizeInBaseUnits()
      };
    }
  };

  const ONE_MINUTE = new Unit(null, null, "minute", 1);
  const FIFTEEN_MINUTES = new Unit(ONE_MINUTE, 15, "minute", 15);
  const ONE_HOUR = new Unit(FIFTEEN_MINUTES, 4, "hour", 1);
  const ONE_DAY = new Unit(ONE_HOUR, 24, "day", 1);
  const ONE_WEEK = new Unit(ONE_DAY, 7, "week", 1);
  const DAYS_PER_MONTH = (28 * 28 + 4 * 30 * 30 + 7 * 31 * 31) / 365;
  const ONE_MONTH = new Unit(ONE_WEEK, DAYS_PER_MONTH / 7, "month", 1);
  const ONE_YEAR = new Unit(ONE_MONTH, 12, "year", 1);

  var round = function(n, chunk) {
    return Math.round(Math.round(n / chunk) * chunk);
  };

  var plural = function(word) {
    return word + "s";
  };

  var format = function(time) {
    if (time instanceof Date) {
      time = time.getTime();
    } else if (typeof(time) !== "number") {
      time = Date.parse(time);
    }
    var diff = (Date.now() - time) / MILLIS_PER_MINUTE;
    var result = ONE_MINUTE.format(diff);
    result.timer *= MILLIS_PER_MINUTE;
    if (result.num != 1) {
      result.unit = plural(result.unit);
    }
    result.unit += " ago";
    return result;
  };

  var Display = function(time, elem) {
    this._time = time;
    this._elem = elem;
    this._timer = null;
    this.refresh();
  };

  Display.prototype.setTime = function(nextTime) {
    this._time = nextTime;
    this.refresh();
  };

  Display.prototype.refresh = function() {
    window.clearTimeout(this._timer);
    var out = format(this._time);
    this._elem.$(".num").innerText = out.num;
    this._elem.$(".unit").innerText = out.unit;
    this._timer = window.setTimeout(this.refresh.bind(this), out.timer);
  };

  Display.prototype.destroy = function() {
    window.clearTimeout(this._timer);
    this._timer = null;
    this._time = null;
    this._elem = null;
  };

  return {
    Display: Display
  };

});

