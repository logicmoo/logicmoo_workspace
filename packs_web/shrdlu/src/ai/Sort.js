var Sort = /** @class */ (function () {
    function Sort(name, parents) {
        this.parents = null;
        this.ID = Sort.s_next_ID++;
        this.name = name;
        this.parents = parents;
        if (Sort.s_precomputedIsA != null) {
            console.error("s_precomputedIsA was computed already when instantiating sort " + name);
            Sort.s_precomputedIsA = null;
        }
    }
    Sort.prototype.is_a = function (s) {
        if (Sort.s_precomputedIsA != null) {
            var offs = this.ID + s.ID * Sort.s_next_ID;
            if (Sort.s_precomputedIsA[offs] == undefined) {
                Sort.s_precomputedIsA[offs] = this.is_a_internal(s);
            }
            return Sort.s_precomputedIsA[offs];
        }
        else {
            return this.is_a_internal(s);
        }
    };
    Sort.prototype.is_a_internal = function (s) {
        if (s == this)
            return true;
        for (var _i = 0, _a = this.parents; _i < _a.length; _i++) {
            var parent_1 = _a[_i];
            if (parent_1.is_a_internal(s))
                return true;
        }
        return false;
    };
    Sort.prototype.is_a_string = function (s) {
        if (this.name == s)
            return true;
        for (var _i = 0, _a = this.parents; _i < _a.length; _i++) {
            var parent_2 = _a[_i];
            if (parent_2.is_a_string(s))
                return true;
        }
        return false;
    };
    Sort.prototype.subsumes = function (s) {
        return s.is_a(this);
    };
    Sort.prototype.addParent = function (s) {
        if (s == this) {
            console.error("Trying to add a sort as its own parent! " + s.name);
            return;
        }
        for (var _i = 0, _a = this.parents; _i < _a.length; _i++) {
            var s2 = _a[_i];
            if (s2 == s)
                return;
        }
        this.parents.push(s);
        Sort.s_precomputedIsA = null;
    };
    Sort.prototype.getAncestors = function () {
        var closed = [];
        var open = [];
        open = open.concat(this.parents);
        while (open.length > 0) {
            var s = open[0];
            open.splice(0, 1);
            if (closed.indexOf(s) == -1)
                closed.push(s);
            open = open.concat(s.parents);
        }
        return closed;
    };
    Sort.prototype.toString = function () {
        return this.name;
    };
    Sort.precomputeIsA = function () {
        Sort.s_precomputedIsA = new Array(Sort.s_next_ID * Sort.s_next_ID);
        for (var i = 0; i < Sort.s_precomputedIsA.length; i++) {
            Sort.s_precomputedIsA[i] = undefined;
        }
    };
    Sort.clear = function () {
        Sort.s_next_ID = 0;
        Sort.s_precomputedIsA = null;
    };
    Sort.s_next_ID = 0;
    Sort.s_precomputedIsA = null;
    return Sort;
}());
