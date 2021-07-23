var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var AnswerPredicate_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerPredicate_IntentionAction, _super);
    function AnswerPredicate_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerPredicate_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.predicate")))
            return true;
        if (intention.functor.is_a(ai.o.getSort("action.answer.predicate-negated")))
            return true;
        return false;
    };
    AnswerPredicate_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        var s_l = Term.termToSentences(intention.attributes[2].term, ai.o);
        var additional_sentences = [];
        var forAlls = [];
        if (intention.attributes.length == 5) {
            additional_sentences = Term.termToSentences(intention.attributes[3].term, ai.o);
            forAlls = NLParser.termsInList((intention.attributes[3]).term, "#and");
        }
        else if (intention.attributes.length == 4) {
            forAlls = NLParser.termsInList((intention.attributes[3]).term, "#and");
            if (forAlls.length == 0 || forAlls[0].functor.name != "#forall") {
                forAlls = [];
                additional_sentences = Term.termToSentences(intention.attributes[3].term, ai.o);
            }
        }
        console.log("AnswerPredicate_IntentionAction: " + ai.selfID + " answer predicate: " + intention.attributes[2]);
        var tmp = this.prepareInferenceInputs(s_l, additional_sentences, forAlls, ai);
        if (tmp == null) {
            ir.succeeded = false;
            return true;
        }
        s_l = tmp[0];
        var negated_s_l = tmp[1];
        var variablesPresent = tmp[2];
        var timeTerms = tmp[3];
        var mainTimeTerm = null;
        var negatedForAll_l = tmp[4];
        if (timeTerms.length >= 0)
            mainTimeTerm = timeTerms[0];
        // special cases:
        var conjunction = true;
        var conjunction_terms = [];
        var conjunction_signs = [];
        for (var _i = 0, s_l_1 = s_l; _i < s_l_1.length; _i++) {
            var sentence = s_l_1[_i];
            if (sentence.terms.length != 1) {
                conjunction = false;
            }
            else {
                conjunction_terms.push(sentence.terms[0]);
                conjunction_signs.push(sentence.sign[0]);
            }
        }
        if (conjunction &&
            this.specialCaseConjunction(conjunction_terms, conjunction_signs, timeTerms, ai, requester, intention.functor.is_a(ai.o.getSort("action.answer.predicate-negated")))) {
            ir.succeeded = true;
            return true;
        }
        var targets;
        if (variablesPresent) {
            // if there are variables in the query, we should only add the negated version, since otherwise, we get spurious results!
            targets = [negated_s_l];
        }
        else {
            targets = [s_l, negated_s_l];
        }
        for (var _a = 0, negatedForAll_l_1 = negatedForAll_l; _a < negatedForAll_l_1.length; _a++) {
            var negatedForAll = negatedForAll_l_1[_a];
            targets.push(negatedForAll);
        }
        ai.queuedInferenceProcesses.push(new InferenceRecord(ai, additional_sentences, targets, 1, 0, false, mainTimeTerm, new AnswerPredicate_InferenceEffect(intention)));
        // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
        // that is then replaced with true/false after inference/continuous is done
        ir.succeeded = true;
        return true;
    };
    // Returns:
    // - s_l (updateD)
    // - negated_s_l
    // - variablesPresent, 
    // - timeTerms, 
    // - negatedForAll_l
    AnswerPredicate_IntentionAction.prototype.prepareInferenceInputs = function (s_l, additional_sentences, forAlls, ai) {
        // search for time-related sentences (which just indicate the time at which this query must be performed):
        var toDelete = [];
        var timeTerms = [];
        var variablesPresent = false;
        for (var _i = 0, s_l_2 = s_l; _i < s_l_2.length; _i++) {
            var s = s_l_2[_i];
            if (s.terms.length == 1 &&
                s.sign[0] && // TODO: consider negated time terms
                s.terms[0].functor.is_a(ai.o.getSort("time"))) {
                timeTerms.push(s.terms[0]);
                toDelete.push(s);
            }
        }
        for (var _a = 0, toDelete_1 = toDelete; _a < toDelete_1.length; _a++) {
            var s = toDelete_1[_a];
            s_l.splice(s_l.indexOf(s), 1);
        }
        var negated_s = new Sentence([], []);
        for (var _b = 0, s_l_3 = s_l; _b < s_l_3.length; _b++) {
            var s = s_l_3[_b];
            if (s.getAllVariables().length > 0)
                variablesPresent = true;
            var tmp = s.negate();
            if (tmp == null || tmp.length != 1) {
                console.error("executeIntention answer predicate: cannot negate sentence!: " + s);
                return null;
            }
            negated_s.terms = negated_s.terms.concat(tmp[0].terms);
            negated_s.sign = negated_s.sign.concat(tmp[0].sign);
        }
        console.log("    executeIntention answer predicate: negated_s = " + negated_s);
        // negate the forAlls:
        var negatedForAll_l = [];
        for (var _c = 0, forAlls_1 = forAlls; _c < forAlls_1.length; _c++) {
            var forAll = forAlls_1[_c];
            if (forAll.attributes.length >= 2 &&
                forAll.attributes[1] instanceof TermTermAttribute) {
                var forAllTerm = (forAll.attributes[1]).term;
                var negatedForAll = Term.termToSentences(new Term(ai.o.getSort("#not"), [new TermTermAttribute(forAllTerm)]), ai.o);
                negatedForAll_l.push(negatedForAll);
            }
        }
        console.log("prepareInferenceInputs:");
        console.log("    s_l: " + s_l);
        console.log("    negated_s: " + negated_s);
        console.log("    variablesPresent = " + variablesPresent);
        console.log("    timeTerms: " + timeTerms);
        console.log("    forAlls: " + forAlls);
        console.log("    negatedForAll_l: " + negatedForAll_l);
        console.log("    additional sentences: " + additional_sentences);
        return [s_l, [negated_s], variablesPresent, timeTerms, negatedForAll_l];
    };
    AnswerPredicate_IntentionAction.prototype.specialCaseConjunction = function (terms, sign, timeTerms, ai, requester, negated) {
        var timeRangeStart = ai.timeStamp;
        var timeRangeEnd = ai.timeStamp;
        var ignoreCurrentIfTheresPrevious = false;
        for (var _i = 0, timeTerms_1 = timeTerms; _i < timeTerms_1.length; _i++) {
            var timeTerm = timeTerms_1[_i];
            // add constraints to the time range:
            if (timeTerm.functor.is_a_string("time.past")) {
                if (timeTerm.functor.is_a_string("time.yesterday")) {
                    timeRangeEnd = Math.floor(ai.timeStamp / (24 * 60 * 60)) * (24 * 60 * 60);
                    timeRangeStart = timeRangeEnd - (24 * 60 * 60);
                }
                else {
                    timeRangeStart = 0;
                    timeRangeEnd = ai.timeStamp;
                    ignoreCurrentIfTheresPrevious = true;
                }
            }
            else if (timeTerm.functor.is_a_string("time.future")) {
                timeRangeStart = ai.timeStamp;
                timeRangeEnd = Number.MAX_VALUE;
            }
        }
        console.log("    specialCaseConjunction: " + timeRangeStart + " - " + timeRangeEnd);
        console.log("    memory: " + ai.shortTermMemory.plainTermList.length + "-" + ai.shortTermMemory.plainPreviousTermList.length + ", " +
            ai.longTermMemory.plainSentenceList.length + "-" + ai.longTermMemory.plainPreviousSentenceList.length + "-" + ai.longTermMemory.previousSentencesWithNoCurrentSentence.length);
        var ret = this.specialCaseConjunctionRecursive(0, terms, sign, new Bindings(), timeRangeStart, timeRangeEnd, ignoreCurrentIfTheresPrevious, ai);
        if (ret == null) {
            return false;
        }
        else {
            if (negated)
                ret = !ret;
            if (ret) {
                // answer no
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'yes'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            else {
                // answer yes
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'no'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            return true;
        }
    };
    /*
    Return values:
    - true: match! term is true
    - null: no match
    - false: we found a match with opposite sign and there were no variables, so, we know the inference result is negative
    */
    AnswerPredicate_IntentionAction.prototype.specialCaseConjunctionRecursive = function (idx, terms, sign, bindings, timeRangeStart, timeRangeEnd, ignoreCurrentIfTheresPrevious, ai) {
        // console.log("        specialCaseConjunctionRecursive: " + idx + ", bindings: " + bindings);
        if (idx >= terms.length)
            return true;
        var trail = bindings.l.length;
        for (var _i = 0, _a = ai.shortTermMemory.plainTermList; _i < _a.length; _i++) {
            var t = _a[_i];
            if (ignoreCurrentIfTheresPrevious && t.previousInTime != null)
                continue;
            var ret = this.specialCaseMatchTermInternal(terms[idx], sign[idx], bindings, timeRangeStart, timeRangeEnd, t.term, true, t.time, t.timeEnd);
            if (ret == true) {
                if (this.specialCaseConjunctionRecursive(idx + 1, terms, sign, bindings, timeRangeStart, timeRangeEnd, ignoreCurrentIfTheresPrevious, ai))
                    return true;
                if (bindings.l.length > trail)
                    bindings.l.splice(trail, bindings.l.length - trail);
            }
            else if (ret == false) {
                return false;
            }
        }
        for (var _b = 0, _c = ai.shortTermMemory.plainPreviousTermList; _b < _c.length; _b++) {
            var t = _c[_b];
            var ret = this.specialCaseMatchTermInternal(terms[idx], sign[idx], bindings, timeRangeStart, timeRangeEnd, t.term, true, t.time, t.timeEnd);
            if (ret == true) {
                if (this.specialCaseConjunctionRecursive(idx + 1, terms, sign, bindings, timeRangeStart, timeRangeEnd, ignoreCurrentIfTheresPrevious, ai))
                    return true;
                if (bindings.l.length > trail)
                    bindings.l.splice(trail, bindings.l.length - trail);
            }
            else if (ret == false) {
                return false;
            }
        }
        for (var _d = 0, _e = ai.longTermMemory.plainSentenceList; _d < _e.length; _d++) {
            var s = _e[_d];
            if (ignoreCurrentIfTheresPrevious && s.previousInTime != null)
                continue;
            if (s.sentence.terms.length == 1) {
                var ret = this.specialCaseMatchTermInternal(terms[idx], sign[idx], bindings, timeRangeStart, timeRangeEnd, s.sentence.terms[0], s.sentence.sign[0], s.time, s.timeEnd);
                if (ret == true) {
                    if (this.specialCaseConjunctionRecursive(idx + 1, terms, sign, bindings, timeRangeStart, timeRangeEnd, ignoreCurrentIfTheresPrevious, ai))
                        return true;
                    if (bindings.l.length > trail)
                        bindings.l.splice(trail, bindings.l.length - trail);
                }
                else if (ret == false) {
                    return false;
                }
            }
        }
        for (var _f = 0, _g = ai.longTermMemory.plainPreviousSentenceList; _f < _g.length; _f++) {
            var s = _g[_f];
            if (s.sentence.terms.length == 1) {
                var ret = this.specialCaseMatchTermInternal(terms[idx], sign[idx], bindings, timeRangeStart, timeRangeEnd, s.sentence.terms[0], s.sentence.sign[0], s.time, s.timeEnd);
                if (ret == true) {
                    if (this.specialCaseConjunctionRecursive(idx + 1, terms, sign, bindings, timeRangeStart, timeRangeEnd, ignoreCurrentIfTheresPrevious, ai))
                        return true;
                    if (bindings.l.length > trail)
                        bindings.l.splice(trail, bindings.l.length - trail);
                }
                else if (ret == false) {
                    return false;
                }
            }
        }
        for (var _h = 0, _j = ai.longTermMemory.previousSentencesWithNoCurrentSentence; _h < _j.length; _h++) {
            var s = _j[_h];
            if (s.sentence.terms.length == 1) {
                var ret = this.specialCaseMatchTermInternal(terms[idx], sign[idx], bindings, timeRangeStart, timeRangeEnd, s.sentence.terms[0], s.sentence.sign[0], s.time, s.timeEnd);
                if (ret == true) {
                    if (this.specialCaseConjunctionRecursive(idx + 1, terms, sign, bindings, timeRangeStart, timeRangeEnd, ignoreCurrentIfTheresPrevious, ai))
                        return true;
                    if (bindings.l.length > trail)
                        bindings.l.splice(trail, bindings.l.length - trail);
                }
                else if (ret == false) {
                    return false;
                }
            }
        }
        // console.log("        specialCaseConjunctionRecursive: " + idx + ", backtracking");
        return null;
    };
    /*
    Return values:
    - true: match! term is true
    - null: no match
    - false: we found a match with opposite sign and there were no variables, so, we know the inference result is negative
    */
    AnswerPredicate_IntentionAction.prototype.specialCaseMatchTermInternal = function (term, sign, bindings, timeRangeStart, timeRangeEnd, kbTerm, kbSign, kbTime0, kbTime1) {
        // console.log("    specialCaseInternal: " + kbTerm + "  [" + kbTime0 + " - " + kbTime1 + "]");
        if ((timeRangeEnd > kbTime0 || kbTime0 == undefined) &&
            (timeRangeStart < kbTime1 || kbTime1 == undefined)) {
            if (sign == kbSign) {
                if (term.subsumes(kbTerm, false, bindings)) {
                    return true;
                }
            }
            else if (term.equalsNoBindings(kbTerm) == 1 &&
                !term.containsAnyVariable()) {
                return false;
            }
        }
        return null;
    };
    AnswerPredicate_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerPredicate_IntentionAction\"/>";
    };
    AnswerPredicate_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerPredicate_IntentionAction();
    };
    return AnswerPredicate_IntentionAction;
}(IntentionAction));
