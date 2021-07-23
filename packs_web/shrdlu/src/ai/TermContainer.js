var TermEntry = /** @class */ (function () {
    function TermEntry(t, p, a, time) {
        this.term = t;
        this.provenance = p;
        this.activation = a;
        this.time = time;
    }
    return TermEntry;
}());
var TermContainer = /** @class */ (function () {
    function TermContainer() {
        this.plainTermList = [];
        this.plainPreviousTermList = [];
        this.termHash = {};
        this.match_cache_term = null;
        this.match_cache_l = null;
        this.match_cache_idx = -1;
    }
    TermContainer.prototype.addTerm = function (t, provenance, activation, time) {
        var te = new TermEntry(t, provenance, activation, time);
        this.plainTermList.push(te);
        var l = this.termHash[t.functor.name];
        if (l == null) {
            l = [];
            this.termHash[t.functor.name] = l;
        }
        l.push(te);
        return te;
    };
    TermContainer.termReplacesPreviousStateTerm = function (t1, t2) {
        return t1.functor == t2.functor &&
            (t1.attributes[0] instanceof ConstantTermAttribute) &&
            (t2.attributes[0] instanceof ConstantTermAttribute) &&
            (t1.attributes[0]).value == (t2.attributes[0]).value;
    };
    TermContainer.prototype.addStateTermIfNew = function (t, provenance, activation, time) {
        // check if we need to replace some term:
        var l = this.termHash[t.functor.name];
        var previous_t_l = [];
        if (l != null) {
            for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
                var te2 = l_1[_i];
                if (TermContainer.termReplacesPreviousStateTerm(t, te2.term))
                    previous_t_l.push(te2);
            }
        }
        if (previous_t_l.length == 0) {
            this.addTerm(t, provenance, activation, time);
            return true;
        }
        else {
            // check if it's different from the previous:
            if (previous_t_l.length == 1 &&
                t.equalsNoBindings(previous_t_l[0].term) == 1) {
                if (previous_t_l[0].activation < activation) {
                    previous_t_l[0].activation = activation;
                    previous_t_l[0].provenance = provenance;
                    // time is not updated
                }
            }
            else {
                // replace old with new, and store old:
                var previous_t_single = null;
                for (var _a = 0, previous_t_l_1 = previous_t_l; _a < previous_t_l_1.length; _a++) {
                    var previous_t = previous_t_l_1[_a];
                    // even if it's a state term, if we have multiple of them in the same cycle, we assume they can coexist:
                    if (previous_t.time != time) {
                        previous_t_single = new TermEntry(previous_t.term, previous_t.provenance, previous_t.activation, previous_t.time);
                        previous_t_single.timeEnd = time;
                        previous_t_single.previousInTime = previous_t.previousInTime;
                        this.plainPreviousTermList.push(previous_t_single);
                        if (this.plainPreviousTermList.length > MAXIMUM_MEMORY_OF_PREVIOUS_STATES)
                            this.plainPreviousTermList.splice(0, 1);
                        this.removeInternal(previous_t);
                    }
                }
                var new_t = this.addTerm(t, provenance, activation, time);
                new_t.previousInTime = previous_t_single;
                return true;
            }
        }
        return false;
    };
    TermContainer.prototype.removeTerm = function (t) {
        //		console.log("removeTerm " + t.toString());
        var l = this.termHash[t.functor.name];
        if (l != null) {
            for (var idx = 0; idx < l.length; idx++) {
                var te2 = l[idx];
                if (t.equalsNoBindings(te2.term) == 1) {
                    l.splice(idx, 1);
                    var t2Idx = this.plainTermList.indexOf(te2);
                    this.plainTermList.splice(t2Idx, 1);
                    //					console.log("found at idx = " + t2Idx + ", now: " + this.plainTermList.length + "," + this.plainActivationList.length);
                    return;
                }
            }
        }
    };
    TermContainer.prototype.removeInternal = function (found) {
        var l = this.termHash[found.term.functor.name];
        if (l != null) {
            var te2Idx_1 = l.indexOf(found);
            l.splice(te2Idx_1, 1);
        }
        var te2Idx = this.plainTermList.indexOf(found);
        this.plainTermList.splice(te2Idx, 1);
    };
    TermContainer.prototype.contains = function (t) {
        var l = this.termHash[t.functor.name];
        if (l != null) {
            for (var _i = 0, l_2 = l; _i < l_2.length; _i++) {
                var te2 = l_2[_i];
                if (t.equalsNoBindings(te2.term) == 1)
                    return te2;
            }
        }
        return null;
    };
    TermContainer.prototype.addTermIfNew = function (t, provenance, activation, time) {
        var te = this.contains(t);
        if (te != null) {
            if (te.activation < activation) {
                te.activation = activation;
                te.provenance = provenance;
                // time is not updated
            }
            return false;
        }
        this.addTerm(t, provenance, activation, time);
        return true;
    };
    TermContainer.prototype.firstMatch = function (t) {
        this.match_cache_term = t;
        this.match_cache_l = [];
        for (var sname in this.termHash) {
            var tel = this.termHash[sname];
            if (tel.length > 0 &&
                (tel[0].term.functor.is_a(t.functor) ||
                    t.functor.is_a(tel[0].term.functor))) {
                this.match_cache_l = this.match_cache_l.concat(tel);
            }
        }
        this.match_cache_idx = 0;
        return this.nextMatch();
    };
    TermContainer.prototype.nextMatch = function () {
        if (this.match_cache_l == null)
            return null;
        while (this.match_cache_idx < this.match_cache_l.length) {
            var te2 = this.match_cache_l[this.match_cache_idx];
            this.match_cache_idx++;
            var bindings = new Bindings();
            //			console.log("nextMatch, unifying " + this.match_cache_term + " with " + te2.term);
            if (this.match_cache_term.unify(te2.term, OCCURS_CHECK, bindings)) {
                return [te2.term, bindings];
            }
            //			console.log("failed!");
        }
        this.match_cache_term = null;
        this.match_cache_l = null;
        this.match_cache_idx = -1;
        return null;
    };
    TermContainer.prototype.allMatches = function (t) {
        var l = [];
        var match = this.firstMatch(t);
        if (match == null)
            return l;
        while (match != null) {
            l.push(match);
            match = this.nextMatch();
        }
        return l;
    };
    /*
        // Returns a list of the terms in this container that can potentially be relevant
        // for unifying with any of the terms in the sentence "s"
        termsRelevantForResolutionWithSentence(s:Sentence) : Term[]
        {
            let l:Term[] = [];
    
            for(let sName in this.termHash) {
                let l2:Term[] = this.termHash[sName];
                if (l2 == null || l2.length == 0) continue;
                let sort:Sort = l2[0].functor;
                let include:boolean = false;
                for(let t of s.terms) {
                    if (sort.subsumes(t.functor) ||
                        t.functor.subsumes(sort)) {
                        include = true;
                        break;
                    }
                }
                if (include) {
                    l = l.concat(l2);
                }
            }
    
            return l;
        }
        */
    TermContainer.prototype.activationUpdate = function () {
        var toDelete = [];
        for (var _i = 0, _a = this.plainTermList; _i < _a.length; _i++) {
            var te = _a[_i];
            te.activation--;
            if (te.activation <= 0) {
                toDelete.push(te);
            }
        }
        for (var _b = 0, toDelete_1 = toDelete; _b < toDelete_1.length; _b++) {
            var te = toDelete_1[_b];
            this.removeInternal(te);
        }
    };
    return TermContainer;
}());
