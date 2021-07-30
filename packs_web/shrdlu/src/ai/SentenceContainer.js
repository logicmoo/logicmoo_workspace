var MAXIMUM_MEMORY_OF_PREVIOUS_STATES = 4096;
var MAXIMUM_MEMORY_OF_PREVIOUS_STATES_PER_TERM = 16;
var SentenceEntry = /** @class */ (function () {
    function SentenceEntry(s, p, a, time) {
        this.firstMatch_counter = -1; // used to prevent linear search, by marking which sentences have already been retrieved in this cycle
        this.allPotentialMatchesWithSentenceForResolution_counter = -1; // used to prevent linear search, by marking which sentences have already been retrieved in this cycle
        this.sentence = s;
        this.provenance = p;
        this.activation = a;
        this.time = time;
    }
    SentenceEntry.prototype.toString = function () {
        return "[" + this.sentence + ", " + this.provenance + ", " + this.activation + "]";
    };
    return SentenceEntry;
}());
var SentenceContainer = /** @class */ (function () {
    function SentenceContainer() {
        this.plainSentenceList = [];
        this.plainPreviousSentenceList = [];
        this.previousSentencesWithNoCurrentSentence = [];
        this.sentenceHash = {};
        this.match_cache_l = null;
        this.match_cache_idx = -1;
        this.firstMatch_counter = 0;
        this.allPotentialMatchesWithSentenceForResolution_counter = 0; // used to prevent linear search, by marking which sentences have already been retrieved in this cycle
    }
    SentenceContainer.prototype.addSentence = function (s, provenance, activation, time) {
        var se = new SentenceEntry(s, provenance, activation, time);
        this.plainSentenceList.push(se);
        for (var _i = 0, _a = s.terms; _i < _a.length; _i++) {
            var t = _a[_i];
            var l = this.sentenceHash[t.functor.name];
            if (l == null) {
                l = [];
                this.sentenceHash[t.functor.name] = l;
            }
            l.push(se);
        }
        return se;
    };
    SentenceContainer.prototype.addPreviousSentence = function (s, provenance, activation, time, timeEnd, current) {
        var se = new SentenceEntry(s, provenance, activation, time);
        se.timeEnd = timeEnd;
        if (current == null) {
            this.previousSentencesWithNoCurrentSentence.push(se);
        }
        else {
            current.previousInTime = se;
            this.plainPreviousSentenceList.push(se);
        }
        return se;
    };
    SentenceContainer.prototype.previousStateSentencesToReplace = function (t, sign) {
        var results = [];
        var l = this.sentenceHash[t.functor.name];
        if (l != null) {
            for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
                var se2 = l_1[_i];
                if (se2.sentence.terms.length == 1 &&
                    ((se2.sentence.sign[0] &&
                        TermContainer.termReplacesPreviousStateTerm(t, se2.sentence.terms[0])) ||
                        (se2.sentence.sign[0] != sign &&
                            t.equalsNoBindings(se2.sentence.terms[0]) == 1))) {
                    results.push(se2.sentence);
                }
            }
        }
        return results;
    };
    SentenceContainer.prototype.addStateSentenceIfNew = function (s, provenance, activation, time) {
        if (s.terms.length != 1) {
            //			console.error("addStateSentenceIfNew: sentence is not a single term sentence! " + s);
            this.addSentence(s, provenance, activation, time);
            return true;
        }
        // check if we need to replace some sentence:
        var t = s.terms[0];
        var l = this.sentenceHash[t.functor.name];
        var previous_s = null;
        if (l != null) {
            for (var _i = 0, l_2 = l; _i < l_2.length; _i++) {
                var se2 = l_2[_i];
                if (se2.sentence.terms.length == 1 &&
                    ((se2.sentence.sign[0] &&
                        TermContainer.termReplacesPreviousStateTerm(t, se2.sentence.terms[0])) ||
                        (se2.sentence.sign[0] != s.sign[0] &&
                            t.equalsNoBindings(se2.sentence.terms[0]) == 1))) {
                    previous_s = se2;
                    break;
                }
            }
        }
        if (previous_s == null) {
            this.addSentence(s, provenance, activation, time);
            return true;
        }
        else {
            // check if it's different from the previous:
            if (!s.equalsNoBindings(previous_s.sentence)) {
                // replace old with new, and store old:
                // "new_s" is the old, and "previous_s" is the new (this seems confusing, but it's for not having to redo the hash calculation,
                // since "previous_s" is already in the correct position where we would like the new sentence to be)
                var new_s = new SentenceEntry(previous_s.sentence, previous_s.provenance, previous_s.activation, previous_s.time);
                new_s.timeEnd = time;
                new_s.previousInTime = previous_s.previousInTime;
                this.plainPreviousSentenceList.push(new_s);
                if (this.plainPreviousSentenceList.length > MAXIMUM_MEMORY_OF_PREVIOUS_STATES)
                    this.plainPreviousSentenceList.splice(0, 1);
                //				console.log("plainPreviousSentenceList.length = " + this.plainPreviousSentenceList.length + ", adding: " + s + " to replace " + previous_s.sentence);
                // we just overwrite, since it's easier (no need to modify plain list nor hash table):
                previous_s.sentence = s;
                previous_s.provenance = provenance;
                previous_s.activation = activation;
                previous_s.time = time;
                previous_s.previousInTime = new_s;
                // Cut chains that are too long:
                var chainLength = 1;
                var se = previous_s;
                while (se != null) {
                    if (se.previousInTime != null) {
                        chainLength++;
                        if (chainLength >= MAXIMUM_MEMORY_OF_PREVIOUS_STATES_PER_TERM) {
                            var idx = this.plainPreviousSentenceList.indexOf(se.previousInTime);
                            if (idx >= 0) {
                                this.plainPreviousSentenceList.splice(idx, 1);
                                //console.log("SPLICED!!! " + idx);
                            }
                            se.previousInTime = null;
                            //console.log("CUT!!!");
                            //console.log("plainPreviousSentenceList:" + this.plainPreviousSentenceList.length);
                        }
                    }
                    se = se.previousInTime;
                }
                //console.log("chainLength: " + chainLength)
                return true;
            }
        }
        return false;
    };
    // Removes all the sentences with a certain provenance (e.g., to clear all coming from perception, of from locations knowledge):
    SentenceContainer.prototype.removeAllWithProvenance = function (provenance) {
        var toDelete = [];
        for (var _i = 0, _a = this.plainSentenceList; _i < _a.length; _i++) {
            var se = _a[_i];
            if (se.provenance == provenance) {
                toDelete.push(se);
            }
        }
        for (var _b = 0, toDelete_1 = toDelete; _b < toDelete_1.length; _b++) {
            var se = toDelete_1[_b];
            this.removeInternal(se);
        }
        // Note: we are ignoring the "plainPreviousSentenceList", or "previousSentencesWithNoCurrentSentence"...
        // - This is because that would involve some expensive search with the current structure... 
        // TODO: add a pointer in SentenceEntry from previous sentence to current, so that I can do this efficiently
        // ...
    };
    SentenceContainer.prototype.removeSentence = function (s) {
        //		console.log("removeSentence " + s.toString());
        var found = null;
        for (var _i = 0, _a = s.terms; _i < _a.length; _i++) {
            var t = _a[_i];
            var l = this.sentenceHash[t.functor.name];
            if (l != null) {
                if (found == null) {
                    for (var idx = 0; idx < l.length; idx++) {
                        var se2 = l[idx];
                        if (s.equalsNoBindings(se2.sentence)) {
                            l.splice(idx, 1);
                            found = se2;
                            //					console.log("found at idx = " + se2Idx + ", now: " + this.plainTermList.length + "," + this.plainActivationList.length);
                            break;
                        }
                    }
                }
                else {
                    var se2Idx = l.indexOf(found);
                    l.splice(se2Idx, 1);
                }
            }
        }
        if (found != null) {
            var se2Idx = this.plainSentenceList.indexOf(found);
            this.plainSentenceList.splice(se2Idx, 1);
        }
    };
    SentenceContainer.prototype.removeInternal = function (found) {
        for (var _i = 0, _a = found.sentence.terms; _i < _a.length; _i++) {
            var t = _a[_i];
            var l = this.sentenceHash[t.functor.name];
            if (l != null) {
                var se2Idx_1 = l.indexOf(found);
                l.splice(se2Idx_1, 1);
            }
        }
        var se2Idx = this.plainSentenceList.indexOf(found);
        this.plainSentenceList.splice(se2Idx, 1);
    };
    /*
        containsTerm(t:Term) : SentenceEntry
        {
            let l:SentenceEntry[] = this.sentenceHash[t.functor.name];
            if (l != null) {
                for(let se of l) {
                    if (se.sentence.terms.length == 1 &&
                        se.sentence.sign[0] &&
                        se.sentence.terms[0].equalsNoBindings(t)) return se;
                }
            }
            return null;
        }
    */
    SentenceContainer.prototype.containsUnifyingTerm = function (t) {
        var l = this.sentenceHash[t.functor.name];
        if (l != null) {
            for (var _i = 0, l_3 = l; _i < l_3.length; _i++) {
                var se = l_3[_i];
                if (se.sentence.terms.length == 1 &&
                    se.sentence.sign[0]) {
                    var bindings = new Bindings();
                    if (t.unify(se.sentence.terms[0], OCCURS_CHECK, bindings))
                        return se;
                }
            }
        }
        return null;
    };
    SentenceContainer.prototype.findSentenceEntry = function (s) {
        if (s.terms.length == 0)
            return null;
        var t = s.terms[0];
        var l = this.sentenceHash[t.functor.name];
        if (l != null) {
            for (var _i = 0, l_4 = l; _i < l_4.length; _i++) {
                var se2 = l_4[_i];
                if (s.equalsNoBindings(se2.sentence))
                    return se2;
            }
        }
        return null;
    };
    SentenceContainer.prototype.contains = function (s) {
        if (this.findSentenceEntry(s) == null)
            return false;
        return true;
    };
    SentenceContainer.prototype.addSentenceIfNew = function (s, provenance, activation, time) {
        if (this.contains(s))
            return false;
        this.addSentence(s, provenance, activation, time);
        return true;
    };
    SentenceContainer.prototype.firstSingleTermMatch = function (s, arity, o) {
        this.match_cache_l = [];
        for (var sortName in this.sentenceHash) {
            var s2 = o.getSort(sortName);
            if (s2.is_a(s) || s.is_a(s2)) {
                var l = this.sentenceHash[sortName];
                for (var _i = 0, l_5 = l; _i < l_5.length; _i++) {
                    var se = l_5[_i];
                    if (se.sentence.terms.length != 1 ||
                        !se.sentence.sign[0])
                        continue;
                    var matchesArity = false;
                    for (var _a = 0, _b = se.sentence.terms; _a < _b.length; _a++) {
                        var t = _b[_a];
                        if (t.functor == s2 && t.attributes.length == arity) {
                            matchesArity = true;
                            break;
                        }
                    }
                    //if (matchesArity && this.match_cache_l.indexOf(se)==-1) {
                    if (matchesArity && se.firstMatch_counter < this.firstMatch_counter) {
                        se.firstMatch_counter = this.firstMatch_counter;
                        this.match_cache_l.push(se);
                    }
                }
            }
        }
        this.firstMatch_counter++;
        this.match_cache_idx = 0;
        return this.nextSingleTermMatch();
    };
    SentenceContainer.prototype.nextSingleTermMatch = function () {
        if (this.match_cache_l == null)
            return null;
        while (this.match_cache_idx < this.match_cache_l.length) {
            var se2 = this.match_cache_l[this.match_cache_idx];
            this.match_cache_idx++;
            return se2.sentence;
        }
        this.match_cache_l = null;
        this.match_cache_idx = -1;
        return null;
    };
    SentenceContainer.prototype.allSingleTermMatches = function (s, arity, o) {
        var l = [];
        var match = this.firstSingleTermMatch(s, arity, o);
        if (match == null)
            return l;
        while (match != null) {
            l.push(match);
            match = this.nextSingleTermMatch();
        }
        return l;
    };
    SentenceContainer.prototype.allPotentialMatchesWithSentenceForResolution = function (s, o) {
        var potentialMatches = [];
        for (var i = 0; i < s.terms.length; i++) {
            var query = s.terms[i];
            var sign = !s.sign[i];
            // special case for this special functor:
            if (query.functor.name == "=")
                continue;
            for (var sortName in this.sentenceHash) {
                var s2 = o.getSort(sortName);
                if (s2.is_a(query.functor) || query.functor.is_a(s2)) {
                    var l = this.sentenceHash[sortName];
                    for (var _i = 0, l_6 = l; _i < l_6.length; _i++) {
                        var se = l_6[_i];
                        if (se.allPotentialMatchesWithSentenceForResolution_counter == this.allPotentialMatchesWithSentenceForResolution_counter)
                            continue;
                        var matches = false;
                        for (var j = 0; j < se.sentence.terms.length; j++) {
                            var t = se.sentence.terms[j];
                            if (se.sentence.sign[j] == sign &&
                                t.attributes.length == query.attributes.length &&
                                t.functor.is_a(query.functor) || query.functor.is_a(t.functor)) {
                                matches = true;
                                for (var k_1 = 0; k_1 < t.attributes.length; k_1++) {
                                    if (query.attributes[k_1] instanceof ConstantTermAttribute) {
                                        if (t.attributes[k_1] instanceof TermTermAttribute) {
                                            matches = false;
                                        }
                                        else if ((t.attributes[k_1] instanceof ConstantTermAttribute) &&
                                            (t.attributes[k_1]).value != (query.attributes[k_1]).value) {
                                            matches = false;
                                        }
                                    }
                                    else if (query.attributes[k_1] instanceof TermTermAttribute) {
                                        if (t.attributes[k_1] instanceof ConstantTermAttribute) {
                                            matches = false;
                                        }
                                        else if (t.attributes[k_1] instanceof TermTermAttribute) {
                                            var qatt = (query.attributes[k_1]);
                                            var tatt = (t.attributes[k_1]);
                                            if (qatt.term.attributes.length != tatt.term.attributes.length ||
                                                !qatt.term.functor.is_a(tatt.term.functor) && !tatt.term.functor.is_a(qatt.term.functor)) {
                                                matches = false;
                                            }
                                        }
                                    }
                                }
                                if (matches)
                                    break;
                            }
                        }
                        if (matches) {
                            if (potentialMatches.indexOf(se.sentence) == -1)
                                potentialMatches.push(se.sentence);
                        }
                        se.allPotentialMatchesWithSentenceForResolution_counter = this.allPotentialMatchesWithSentenceForResolution_counter;
                    }
                }
            }
            this.allPotentialMatchesWithSentenceForResolution_counter++;
        }
        return potentialMatches;
    };
    SentenceContainer.prototype.activationUpdate = function () {
        var toDelete = [];
        for (var idx = 0; idx < this.plainSentenceList.length; idx++) {
            this.plainSentenceList[idx].activation--;
            if (this.plainSentenceList[idx].activation <= 0) {
                toDelete.push(this.plainSentenceList[idx]);
            }
        }
        for (var _i = 0, toDelete_2 = toDelete; _i < toDelete_2.length; _i++) {
            var se = toDelete_2[_i];
            this.removeInternal(se);
        }
    };
    return SentenceContainer;
}());
