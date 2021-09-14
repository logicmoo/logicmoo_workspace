/*

Note (santi):
- This class implements a resolution engine, which is used by the AI of the NPCs to reason about the queries that the player
  enters.
- In order to make inference efficient, a few pruning heuristics were implemented that can be toggled from the global boolean
  variables at the top of the file (INFERENCE_allow_increasing_sentences, INFERENCE_allow_variable_to_variable_substitutions).
- Moreover, in order to let the game flow in real time, the "InterruptibleResolution" class performs only a few inference steps
  per game cycle (controllable from the INFERENCE_MAX_RESOLUTIONS_PER_STEP variable). After INFERENCE_MAX_TOTAL_RESOLUTIONS
  steps, the inference is abandoned.
- However, if the pruning rules are removed, and INFERENCE_MAX_TOTAL_RESOLUTIONS is set to infinity, this should, in principle
  be a full sound and complete resolution engine.

*/
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
var DEBUG_resolution = false;
var DEBUG_resolution_detailed = false;
// search prunning strategies to make inference faster:
var INFERENCE_maximum_sentence_size = 4;
var INFERENCE_allow_equal_size_sentences = true;
var INFERENCE_allow_increasing_sentences = true;
var INFERENCE_allow_variable_to_variable_substitutions = true;
var INFERENCE_MAX_RESOLUTIONS_PER_STEP = 4000;
var INFERENCE_MAX_TOTAL_RESOLUTIONS = 1000000; // at this point, inference will stop
// var INFERENCE_MAX_TOTAL_RESOLUTIONS:number = 400000;	// at this point, inference will stop
// var INFERENCE_STEP_STATE_STILL_RUNNING:number = 0;
var INFERENCE_STEP_STATE_DONE = 1;
var INFERENCE_STEP_STATE_COMPUTE_LIMIT_REACHED = 2;
var InferenceNode = /** @class */ (function () {
    function InferenceNode(s, b, p1, p2) {
        this.sentence = s;
        this.bindings = b;
        this.parent1 = p1;
        this.parent2 = p2;
    }
    InferenceNode.prototype.getBaseSentences = function (target) {
        var open = [this];
        var l = [];
        while (open.length > 0) {
            var current = open[0];
            if (current.parent1 == null && current.parent2 == null && current.sentence != null) {
                if (l.indexOf(current.sentence) == -1 &&
                    target.indexOf(current.sentence) == -1)
                    l.push(current.sentence);
            }
            else {
                if (current.parent1 != null)
                    open.push(current.parent1);
                if (current.parent2 != null)
                    open.push(current.parent2);
            }
            open.splice(0, 1);
        }
        return l;
    };
    InferenceNode.prototype.getValueForVariableName = function (vName) {
        return this.bindings.getValueForVariableName(vName);
    };
    InferenceNode.prototype.matchesOnVariableNames = function (n2, vNames) {
        for (var _i = 0, vNames_1 = vNames; _i < vNames_1.length; _i++) {
            var vName = vNames_1[_i];
            var v1 = this.getValueForVariableName(vName);
            var v2 = n2.getValueForVariableName(vName);
            if (v1 == null && v2 != null)
                return false;
            if (v1 != null && v1 == null)
                return false;
            if (Term.equalsNoBindingsAttribute(v1, v2) != 1)
                return false;
        }
        return true;
    };
    InferenceNode.prototype.toString = function () {
        return "[ sentence: " + this.sentence + ", bindings: " + this.bindings + "]";
    };
    return InferenceNode;
}());
var InterruptibleResolution = /** @class */ (function () {
    function InterruptibleResolution(KB, additionalSentences, target, occursCheck, treatSpatialPredicatesSpecially, ai) {
        this.KB = null;
        this.additionalSentences = [];
        this.originalTarget = null;
        this.originalTargetVariables = [];
        this.ai = null;
        this.superlativePredicates = [];
        this.firstStep = true;
        // target:InferenceNode[] = [];
        this.open = [];
        this.closed = [];
        this.endResults = [];
        this.internal_step_state = INFERENCE_STEP_STATE_DONE; // 0 if it is still running, 1 if it's done, 2 it's that it has reached computation limit
        // internal_step_state_index:number = 0;	// index of the next element of "target" to consider 
        this.total_resolutions = 0;
        this.sort_cache_spatial_relation = null;
        this.sort_cache_superlative = null;
        this.sort_cache_spatial_relation = ai.o.getSort("spatial-relation");
        this.sort_cache_superlative = ai.o.getSort("superlative-adjective");
        this.KB = KB;
        this.originalTarget = target;
        // get the bindings in the variables from the target:
        for (var _i = 0, _a = this.originalTarget; _i < _a.length; _i++) {
            var ts = _a[_i];
            this.originalTargetVariables = this.originalTargetVariables.concat(ts.getAllVariables());
        }
        this.occursCheck = occursCheck;
        this.treatSpatialPredicatesSpecially = treatSpatialPredicatesSpecially;
        this.superlativePredicates = []; // These are predicates such as "nearest", that can only be checked once we have all the solutions
        this.ai = ai;
        this.internal_step_state = INFERENCE_STEP_STATE_DONE; // signal that we need to start from scratch the first time step_internal is called
        // this.internal_step_state_index = 0;
        for (var _b = 0, additionalSentences_1 = additionalSentences; _b < additionalSentences_1.length; _b++) {
            var s = additionalSentences_1[_b];
            this.additionalSentences.push(new InferenceNode(s, new Bindings(), null, null));
        }
        for (var _c = 0, target_1 = target; _c < target_1.length; _c++) {
            var s = target_1[_c];
            var r = new InferenceNode(s, new Bindings(), null, null);
            this.resolutionEqualityCheck(r);
            if (r.sentence != null && this.treatSpatialPredicatesSpecially) {
                var n = r.sentence.terms.length;
                r.sentence = this.resolutionSpatialPredicatesCheck(r.sentence, true);
                if (n != r.sentence.terms.length && r.sentence.terms.length == 1) {
                    // we need to check this again:
                    this.resolutionEqualityCheck(r);
                }
            }
            if (r.sentence == null)
                continue;
            this.open.push(r);
        }
        console.log("InterruptibleResolution: variables: " + this.originalTargetVariables);
        for (var _d = 0, _e = this.open; _d < _e.length; _d++) {
            var t = _e[_d];
            console.log("   target: " + t.toString());
        }
    }
    // executes resolution till the end:
    InterruptibleResolution.prototype.resolve = function () {
        while (!this.step()) { }
        this.processSuperlatives();
        return this.endResults;
    };
    InterruptibleResolution.prototype.step = function () {
        if (this.stepAccumulatingResults(false))
            return true;
        return false;
    };
    InterruptibleResolution.prototype.stepAccumulatingResults = function (findAllAnswers) {
        if (DEBUG_resolution)
            console.log("resolution stepAccumulatingResults:");
        if (this.originalTargetVariables.length == 0)
            findAllAnswers = false;
        var resolutionsAtStepStart = this.total_resolutions;
        while (this.total_resolutions < resolutionsAtStepStart + INFERENCE_MAX_RESOLUTIONS_PER_STEP) {
            var newResolvents = this.step_internal(this.additionalSentences, this.occursCheck, !findAllAnswers);
            if (DEBUG_resolution && newResolvents != null) {
                console.log(this.debug_inferenceSentenceLengths(newResolvents));
            }
            this.firstStep = false;
            if (newResolvents == null ||
                (this.open.length == 0 && newResolvents.length == 0)) {
                console.log("step: finished with #total_resolutions = " + this.total_resolutions + " closed: " + this.closed.length + ", open: " + this.open.length + ", end results: " + this.endResults.length);
                if (DEBUG_resolution)
                    console.log("  - no more resolvents: " + this.endResults.length + " sets of bindings cause a contradiction! (CLOSED: " + this.closed.length + ")");
                this.processSuperlatives();
                return true;
            }
            // console.log("resolution stepAccumulatingResults: newResolvents.length = " + newResolvents.length);
            if (DEBUG_resolution) {
                for (var _i = 0, newResolvents_1 = newResolvents; _i < newResolvents_1.length; _i++) {
                    var resolvent = newResolvents_1[_i];
                    console.log("    " + resolvent.sentence + " -> " + resolvent.bindings);
                }
            }
            // let anyNewResolvent:boolean = false;
            for (var _a = 0, newResolvents_2 = newResolvents; _a < newResolvents_2.length; _a++) {
                var newResolvent = newResolvents_2[_a];
                var r = newResolvent.sentence;
                var b = newResolvent.bindings;
                if (r.terms.length == 0) {
                    // we have found a contradiction!
                    if (DEBUG_resolution)
                        console.log("  - contradiction! (CLOSED: " + this.closed.length + ")");
                    var endResult = new InferenceNode(r, new Bindings(), newResolvent.parent1, newResolvent.parent2);
                    for (var _b = 0, _c = b.l; _b < _c.length; _b++) {
                        var _d = _c[_b], v = _d[0], t = _d[1];
                        if (this.originalTargetVariables.indexOf(v) >= 0) {
                            var t2 = t.applyBindings(b);
                            if (endResult.bindings.l.indexOf([v, t2]) == -1)
                                endResult.bindings.l.push([v, t2]);
                        }
                    }
                    var found = false;
                    for (var _e = 0, _f = this.endResults; _e < _f.length; _e++) {
                        var tmp = _f[_e];
                        if (tmp.bindings.equals(endResult.bindings)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        this.closed.push(endResult);
                        this.endResults.push(endResult);
                        if (!findAllAnswers) {
                            // we are done!
                            console.log("step: finished with #total_resolutions = " + this.total_resolutions + " closed: " + this.closed.length + ", open: " + this.open.length + ", end results: " + this.endResults.length);
                            this.processSuperlatives();
                            return true;
                        }
                    }
                    else {
                        console.warn("    end result was already there: " + endResult.bindings);
                    }
                }
                else {
                    var found = false;
                    // make sure we are not inferring something we already knew:
                    for (var _g = 0, _h = this.closed; _g < _h.length; _g++) {
                        var tmp = _h[_g];
                        if (this.resultCanBeFilteredOut(newResolvent, tmp.sentence, tmp.bindings)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        for (var _j = 0, _k = this.open; _j < _k.length; _j++) {
                            var tmp = _k[_j];
                            if (this.resultCanBeFilteredOut(newResolvent, tmp.sentence, tmp.bindings)) {
                                found = true;
                                break;
                            }
                        }
                    }
                    if (!found) {
                        for (var _l = 0, _m = this.additionalSentences; _l < _m.length; _l++) {
                            var tmp = _m[_l];
                            if (this.resultCanBeFilteredOut(newResolvent, tmp.sentence, tmp.bindings)) {
                                found = true;
                                break;
                            }
                        }
                    }
                    if (!found) {
                        for (var _o = 0, _p = this.KB.plainSentenceList; _o < _p.length; _o++) {
                            var tmp = _p[_o];
                            if (this.resultCanBeFilteredOut(newResolvent, tmp.sentence, null)) {
                                found = true;
                                break;
                            }
                        }
                    }
                    if (!found) {
                        this.open.push(newResolvent);
                        // anyNewResolvent = true;
                    }
                }
                if (DEBUG_resolution)
                    console.log("  " + r.toString());
            }
        }
        console.log("step: finished with #total_resolutions = " + this.total_resolutions + " closed: " + this.closed.length + ", open: " + this.open.length + ", end results: " + this.endResults.length);
        if (this.total_resolutions >= INFERENCE_MAX_TOTAL_RESOLUTIONS) {
            console.log("step: finished with #total_resolutions = " + this.total_resolutions + " closed: " + this.closed.length + ", open: " + this.open.length + ", end results: " + this.endResults.length);
            this.processSuperlatives();
            return true; // computation limit reached
        }
        if (DEBUG_resolution)
            console.log("  CLOSED: " + this.closed.length);
        if (DEBUG_resolution)
            console.log("  KB.length: " + this.KB.plainSentenceList.length);
        // if (!anyNewResolvent) {
        // 	console.log("all the resolvents in this round where already in the closed list, so we are done!");
        // 	this.processSuperlatives();
        // 	return true;
        // }
        return false;
    };
    InterruptibleResolution.prototype.step_internal = function (sentences, occursCheck, stopOnFirstContradiction) {
        var newResolvents = [];
        if (DEBUG_resolution)
            console.log("InterruptibleResolution.step_internal with sentences.length = " + sentences.length + ", open.length = " + this.open.length);
        if (this.firstStep) {
            for (var i = 0; i < this.open.length; i++) {
                if (this.open[i].sentence.terms.length == 0) {
                    // initial contradiction!
                    this.internal_step_state = INFERENCE_STEP_STATE_DONE;
                    return [this.open[i]]; // we are done!
                }
            }
        }
        // this.internal_step_state_index = 0;
        if (this.open.length == 0)
            return [];
        // pick the smallest (notice that this is NOT a time bottleneck, so, although it can be easily done, there is little to gain optimizing this loop):
        var n1_idx = 0;
        for (var i = 1; i < this.open.length; i++) {
            if (this.open[i].sentence.terms.length < this.open[n1_idx].sentence.terms.length) {
                n1_idx = i;
            }
        }
        var n1 = this.open[n1_idx];
        var s1 = n1.sentence;
        this.open.splice(n1_idx, 1);
        this.closed.push(n1);
        // console.log("n1.sentence = " + n1.sentence);
        // console.log("n1.sentence = " + n1.sentence.terms.length);
        if (this.KB != null) {
            var relevantSentences = this.KB.allPotentialMatchesWithSentenceForResolution(s1, this.ai.o);
            if (DEBUG_resolution) {
                console.log("    sentences relevant for " + s1.toString() + ": " + relevantSentences.length);
                if (DEBUG_resolution_detailed) {
                    for (var _i = 0, relevantSentences_1 = relevantSentences; _i < relevantSentences_1.length; _i++) {
                        var s2 = relevantSentences_1[_i];
                        console.log("        - " + s2);
                    }
                }
            }
            for (var _a = 0, relevantSentences_2 = relevantSentences; _a < relevantSentences_2.length; _a++) {
                var s2 = relevantSentences_2[_a];
                if (this.firstStep) {
                    if (this.originalTarget.length == 1 &&
                        s2.equalsNoBindings(s1)) {
                        if (DEBUG_resolution)
                            console.log("step_internal: we are done! what we want to proof is in the knowledge base!!!");
                        this.internal_step_state = INFERENCE_STEP_STATE_DONE;
                        return null;
                    }
                }
                var tmp = this.resolutionBetweenSentencesWithBindings(n1, new InferenceNode(s2, new Bindings(), null, null), occursCheck);
                if (DEBUG_resolution)
                    console.log("    Resolution (relevantSentences) between " + s1 + "   and   " + s2 + "  --> " + this.debug_inferenceSentenceLengths(tmp));
                this.total_resolutions++;
                for (var _b = 0, tmp_1 = tmp; _b < tmp_1.length; _b++) {
                    var r = tmp_1[_b];
                    this.resolutionEqualityCheck(r);
                    if (r.sentence != null && this.treatSpatialPredicatesSpecially)
                        r.sentence = this.resolutionSpatialPredicatesCheck(r.sentence, false);
                    if (r.sentence == null)
                        continue;
                    var found = false;
                    for (var i = 0; i < newResolvents.length; i++) {
                        if (this.resultCanBeFilteredOut(r, newResolvents[i].sentence, newResolvents[i].bindings)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        newResolvents.push(r);
                        if (DEBUG_resolution)
                            console.log("step_internal: new resolvent: " + r.sentence + " , " + r.bindings);
                        if (r.sentence.terms.length == 0 && stopOnFirstContradiction) {
                            this.internal_step_state = INFERENCE_STEP_STATE_DONE;
                            return newResolvents; // we are done!
                        }
                    }
                }
            }
        }
        for (var _c = 0, sentences_1 = sentences; _c < sentences_1.length; _c++) {
            var n2 = sentences_1[_c];
            if (s1 == n2.sentence)
                continue;
            var s2 = n2.sentence;
            if (this.firstStep) {
                if (this.originalTarget.length == 1 &&
                    s2.equalsNoBindings(s1)) {
                    if (DEBUG_resolution)
                        console.log("step_internal: we are done! what we want to proof is in the knowledge base!!!");
                    this.internal_step_state = INFERENCE_STEP_STATE_DONE;
                    return null;
                }
            }
            var tmp = this.resolutionBetweenSentencesWithBindings(n1, n2, occursCheck);
            if (DEBUG_resolution)
                console.log("    Resolution (sentences) between " + s1 + "   and   " + s2 + "  --> " + this.debug_inferenceSentenceLengths(tmp));
            this.total_resolutions++;
            for (var _d = 0, tmp_2 = tmp; _d < tmp_2.length; _d++) {
                var r = tmp_2[_d];
                this.resolutionEqualityCheck(r);
                if (r.sentence != null && this.treatSpatialPredicatesSpecially)
                    r.sentence = this.resolutionSpatialPredicatesCheck(r.sentence, false);
                if (r.sentence == null)
                    continue;
                var found = false;
                for (var i = 0; i < newResolvents.length; i++) {
                    if (this.resultCanBeFilteredOut(r, newResolvents[i].sentence, newResolvents[i].bindings)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    newResolvents.push(r);
                    if (DEBUG_resolution)
                        console.log("step_internal: new resolvent: " + r.sentence + " , " + r.bindings);
                    if (r.sentence.terms.length == 0 && stopOnFirstContradiction) {
                        this.internal_step_state = INFERENCE_STEP_STATE_DONE;
                        return newResolvents; // we are done!
                    }
                }
            }
        }
        // for(let n2 of this.open) {
        for (var _e = 0, _f = this.closed; _e < _f.length; _e++) {
            var n2 = _f[_e];
            if (s1 == n2.sentence)
                continue;
            var s2 = n2.sentence;
            var tmp = this.resolutionBetweenSentencesWithBindings(n1, n2, occursCheck);
            if (DEBUG_resolution)
                console.log("    Resolution (closed) between " + s1 + "   and   " + s2 + "  --> " + this.debug_inferenceSentenceLengths(tmp));
            this.total_resolutions++;
            for (var _g = 0, tmp_3 = tmp; _g < tmp_3.length; _g++) {
                var r = tmp_3[_g];
                this.resolutionEqualityCheck(r);
                if (r.sentence != null && this.treatSpatialPredicatesSpecially)
                    r.sentence = this.resolutionSpatialPredicatesCheck(r.sentence, false);
                if (r.sentence == null)
                    continue;
                var found = false;
                for (var i = 0; i < newResolvents.length; i++) {
                    if (this.resultCanBeFilteredOut(newResolvents[i], r.sentence, r.bindings)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    newResolvents.push(r);
                    if (DEBUG_resolution)
                        console.log("step_internal: new resolvent: " + r.sentence + " , " + r.bindings);
                    if (r.sentence.terms.length == 0 && stopOnFirstContradiction) {
                        this.internal_step_state = INFERENCE_STEP_STATE_DONE;
                        return newResolvents; // we are done!
                    }
                }
            }
        }
        // if (resolutions >= INFERENCE_MAX_RESOLUTIONS_PER_STEP) {
        // 	// we need to interrupt:
        // 	console.log("step_internal: interrupted with #resolutions = " + resolutions + " (" + this.total_resolutions + "), closed "+this.closed.length+", open: "+this.open.length+", newResolvents: "+newResolvents.length+", end results: " + this.endResults.length);
        // 	return null;
        // }
        // console.log("step_internal: finished with #total_resolutions = " + this.total_resolutions + " closed: "+this.closed.length+", open: "+this.open.length+", newResolvents: "+newResolvents.length+", end results: " + this.endResults.length);
        this.internal_step_state = INFERENCE_STEP_STATE_DONE;
        return newResolvents;
    };
    // Checks to see if the equality predicate results in a contradiction: sets r.sentence = null
    // Or if there are any terms of the from x = x (which are then removed)
    InterruptibleResolution.prototype.resolutionEqualityCheck = function (r) {
        var s = r.sentence;
        var toDelete = [];
        for (var i = 0; i < s.terms.length; i++) {
            if (s.terms[i].functor.name == "=" &&
                s.terms[i].attributes.length == 2) {
                var equals = Term.equalsNoBindingsAttribute(s.terms[i].attributes[0], s.terms[i].attributes[1]);
                if (equals == 0) {
                    if (s.terms.length == 1) {
                        // special case where the sentences is just something of the form: "=(X,constant)"
                        if ((s.terms[i].attributes[0] instanceof VariableTermAttribute) &&
                            (s.terms[i].attributes[1] instanceof ConstantTermAttribute) &&
                            s.terms[i].attributes[0].sort.subsumes(s.terms[i].attributes[1].sort)) {
                            r.bindings.l.push([(s.terms[i].attributes[0]), s.terms[i].attributes[1]]);
                            toDelete.push(s.terms[i]);
                        }
                        else if ((s.terms[i].attributes[1] instanceof VariableTermAttribute) &&
                            (s.terms[i].attributes[0] instanceof ConstantTermAttribute) &&
                            s.terms[i].attributes[1].sort.subsumes(s.terms[i].attributes[0].sort)) {
                            r.bindings.l.push([(s.terms[i].attributes[1]), s.terms[i].attributes[0]]);
                            toDelete.push(s.terms[i]);
                        }
                    }
                    continue;
                }
                if ((s.sign[i] && equals == 1) ||
                    (!s.sign[i] && equals == -1)) {
                    r.sentence = null;
                    return;
                }
                else {
                    toDelete.push(s.terms[i]);
                }
            }
        }
        for (var _i = 0, toDelete_1 = toDelete; _i < toDelete_1.length; _i++) {
            var t = toDelete_1[_i];
            var idx = s.terms.indexOf(t);
            s.terms.splice(idx, 1);
            s.sign.splice(idx, 1);
        }
    };
    InterruptibleResolution.prototype.resolutionSpatialPredicatesCheck = function (s, firstTime) {
        var toDelete = [];
        for (var i = 0; i < s.terms.length; i++) {
            if (firstTime) {
                if (s.terms[i].functor.is_a(this.sort_cache_superlative) &&
                    s.terms[i].functor.is_a(this.sort_cache_spatial_relation)) {
                    toDelete.push(s.terms[i]);
                    this.superlativePredicates.push(new Sentence([s.terms[i]], [!s.sign[i]]));
                    continue;
                }
            }
            if (s.terms[i].functor.is_a(this.sort_cache_spatial_relation) &&
                s.terms[i].attributes.length == 2 &&
                s.terms[i].attributes[0] instanceof ConstantTermAttribute &&
                s.terms[i].attributes[1] instanceof ConstantTermAttribute) {
                // check if it's true or false, and see if it has to be eliminated from the sentence:
                var truth = this.ai.checkSpatialRelation(s.terms[i].functor, s.terms[i].attributes[0].value, s.terms[i].attributes[1].value, this.ai.selfID);
                if (DEBUG_resolution)
                    console.log("checkSpatialRelation: " + s.terms[i] + " -> " + truth);
                if (truth != null &&
                    truth != s.sign[i]) {
                    toDelete.push(s.terms[i]);
                }
            }
        }
        for (var _i = 0, toDelete_2 = toDelete; _i < toDelete_2.length; _i++) {
            var t = toDelete_2[_i];
            var idx = s.terms.indexOf(t);
            s.terms.splice(idx, 1);
            s.sign.splice(idx, 1);
        }
        return s;
    };
    InterruptibleResolution.prototype.processSuperlatives = function () {
        for (var _i = 0, _a = this.superlativePredicates; _i < _a.length; _i++) {
            var superlative = _a[_i];
            this.endResults = this.ai.processSuperlatives(this.endResults, superlative);
        }
    };
    /*
        - algorithm should be: P1...Pn, Q1...Qm, usedP (initially []), usedQ (initially [])
        found = false
        for i = 0;i<n;i++:
            if !usedP.contains(i)
                for j = 0;j<m;j++:
                    if !usedQ.contains(j)
                        if (Pi unifies with Qj):
                            add bindings
                            usedP.add(i), usedQ.add(j)
                            found = true
                            recursivecall(P, Q usedP, usedQ)
                            usedP.remove(i), usedQ.remove(j)
        if !found:
            generate result with Pis and Pjs not in usedP and usedQ
            remove replicate predicates in the result (identical without creating new bindings)
    */
    // Tries to apply the principle of "Resolution" between two sentences, and
    // returns all possible resolvents
    // parent1: is from the query
    // parent2: is from the KB
    InterruptibleResolution.prototype.resolutionBetweenSentencesWithBindings = function (parent1, parent2, occursCheck) {
        var resolvents = [];
        this.resolutionBetweenSentencesWithBindings_internal(parent1.sentence, parent2.sentence, parent1, parent2, resolvents, occursCheck);
        // console.log("    resolutionBetweenSentencesWithBindings: resolvents: " + resolvents.length);
        var resolvents2 = [];
        for (var _i = 0, resolvents_1 = resolvents; _i < resolvents_1.length; _i++) {
            var r = resolvents_1[_i];
            r.bindings = parent1.bindings.concat(parent2.bindings.concat(r.bindings));
            if (r.bindings != null) {
                r.sentence = r.sentence.applyBindings(r.bindings);
                r.bindings.removeUselessBindings(this.originalTargetVariables);
                var found = false;
                for (var _a = 0, resolvents2_1 = resolvents2; _a < resolvents2_1.length; _a++) {
                    var r2 = resolvents2_1[_a];
                    if (this.resultCanBeFilteredOut(r, r2.sentence, r2.bindings)) {
                        found = true;
                    }
                }
                if (!found)
                    resolvents2.push(r);
            }
        }
        return resolvents2;
    };
    // s1: is from the query
    // s2: is from the KB
    InterruptibleResolution.prototype.resolutionBetweenSentencesWithBindings_internal = function (s1, s2, parent1, parent2, resolvents, occursCheck) {
        var s2_term_cache = [];
        var bindings = new Bindings();
        if (parent1.bindings != null) {
            bindings = bindings.concat(parent1.bindings);
        }
        if (parent2.bindings != null) {
            bindings = bindings.concat(parent2.bindings);
        }
        // if they are not compatible:
        if (bindings == null) {
            // console.log("incompatible bindings: " + parent1.bindings + "  vs  " + parent2.bindings);
            return;
        }
        for (var i = 0; i < s1.terms.length; i++) {
            for (var j = 0; j < s2.terms.length; j++) {
                if (s2_term_cache.length <= j)
                    s2_term_cache.push(null);
                if (s1.sign[i] == s2.sign[j])
                    continue;
                var t1 = s1.terms[i].applyBindings(bindings);
                var t2 = s2_term_cache[j];
                if (t2 == null) {
                    t2 = s2.terms[j].applyBindings(bindings);
                    s2_term_cache[j] = t2;
                }
                var bindings2 = new Bindings();
                bindings2 = bindings2.concat(bindings);
                // special cases, where I can use functor sort subsumption and inference is still sound:
                // this is so that I don't need all those ontology rules that make everything very slow!
                if (s1.sign[i]) {
                    if (!t1.functor.is_a(t2.functor))
                        continue;
                    if (!t1.unifyIgnoringFirstfunctor(t2, occursCheck, bindings2))
                        continue;
                }
                else {
                    if (!t2.functor.is_a(t1.functor))
                        continue;
                    if (!t1.unifyIgnoringFirstfunctor(t2, occursCheck, bindings2))
                        continue;
                }
                // only allow steps that replace variables by constants:
                if (!INFERENCE_allow_variable_to_variable_substitutions) {
                    var anyNonVariable = false;
                    for (var k_1 = bindings.l.length; k_1 < bindings2.l.length; k_1++) {
                        if (!(bindings2.l[k_1][1] instanceof VariableTermAttribute))
                            anyNonVariable = true;
                    }
                    if (bindings2.l.length > bindings.l.length && !anyNonVariable)
                        continue;
                }
                // generate one resolvent:
                var r = new InferenceNode(new Sentence([], []), bindings2, parent1, parent2);
                for (var i2 = 0; i2 < s1.terms.length; i2++) {
                    if (i == i2)
                        continue;
                    r.sentence.terms.push(s1.terms[i2].applyBindings(bindings2));
                    r.sentence.sign.push(s1.sign[i2]);
                }
                for (var j2 = 0; j2 < s2.terms.length; j2++) {
                    if (j == j2)
                        continue;
                    r.sentence.terms.push(s2.terms[j2].applyBindings(bindings2));
                    r.sentence.sign.push(s2.sign[j2]);
                }
                // only allow steps that do not increase the size of the sentences:
                if (!INFERENCE_allow_increasing_sentences) {
                    if (r.sentence.terms.length > s1.terms.length &&
                        r.sentence.terms.length > s2.terms.length) {
                        if (DEBUG_resolution_detailed)
                            console.log("Removed because of size (1a)... " + r.sentence.terms.length + " vs " + s1.terms.length + " and " + s2.terms.length);
                        return;
                    }
                }
                if (!INFERENCE_allow_equal_size_sentences) {
                    if (r.sentence.terms.length >= s1.terms.length &&
                        r.sentence.terms.length >= s2.terms.length) {
                        if (DEBUG_resolution_detailed)
                            console.log("Removed because of size (1a)... " + r.sentence.terms.length + " vs " + s1.terms.length + " and " + s2.terms.length);
                        return;
                    }
                }
                if (r.sentence.terms.length > INFERENCE_maximum_sentence_size) {
                    if (DEBUG_resolution_detailed)
                        console.log("Removed because of size (2)... " + r.sentence.terms.length);
                    return;
                }
                resolvents.push(r);
                if (DEBUG_resolution) {
                    console.log("resolutionBetweenSentencesWithBindings_internal: new resolvent");
                    console.log("    s1: " + s1);
                    console.log("    s2: " + s2);
                    console.log("    indexes: " + i + " , " + j);
                    console.log("    bindings: " + r.bindings);
                    console.log("    resolvent: " + r.sentence);
                }
            }
        }
    };
    // --> If previousR subset r (the non contained do not have any variables that can affect the final bindings) -> filter
    InterruptibleResolution.prototype.resultCanBeFilteredOut = function (r, previousSentence, previousBindings) {
        var rl = r.sentence.terms.length;
        var psl = previousSentence.terms.length;
        if (r.sentence.terms.length < psl)
            return false;
        if (previousBindings != null && !r.bindings.equals(previousBindings))
            return false;
        var used = [];
        for (var i = 0; i < rl; i++) {
            used.push(false);
        }
        for (var j = 0; j < psl; j++) {
            var found = false;
            for (var i = 0; i < rl; i++) {
                if (used[i])
                    continue;
                if (r.sentence.sign[i] == previousSentence.sign[j] &&
                    r.sentence.terms[i].equalsNoBindings(previousSentence.terms[j]) == 1) {
                    found = true;
                    used[i] = true;
                    break;
                }
            }
            if (!found)
                return false;
        }
        return true;
    };
    InterruptibleResolution.prototype.filterResultsByForAll = function (queryVariableNames, forAllVariableName, forAllValues) {
        var results = [];
        for (var _i = 0, _a = this.endResults; _i < _a.length; _i++) {
            var r = _a[_i];
            var match = false;
            for (var _b = 0, results_1 = results; _b < results_1.length; _b++) {
                var _c = results_1[_b], prev_r = _c[0], missingValues = _c[1];
                if (r.matchesOnVariableNames(prev_r, queryVariableNames)) {
                    var v = r.getValueForVariableName(forAllVariableName);
                    for (var i = 0; i < missingValues.length; i++) {
                        if (Term.equalsNoBindingsAttribute(missingValues[i], v) == 1) {
                            missingValues.splice(i, 1);
                            break;
                        }
                    }
                    match = true;
                    break;
                }
            }
            if (!match) {
                console.log("filterResultsByForAll (new result): " + r.bindings);
                var bindings = new Bindings();
                for (var _d = 0, _e = r.bindings.l; _d < _e.length; _d++) {
                    var vv = _e[_d];
                    if (vv[0].name != forAllVariableName) {
                        bindings.l.push(vv);
                    }
                }
                var r2 = new InferenceNode(r.sentence, bindings, r.parent1, r.parent2);
                var missingValues = __spreadArray([], forAllValues, true);
                var v = r.getValueForVariableName(forAllVariableName);
                for (var i = 0; i < missingValues.length; i++) {
                    if (Term.equalsNoBindingsAttribute(missingValues[i], v) == 1) {
                        missingValues.splice(i, 1);
                        break;
                    }
                }
                results.push([r2, missingValues]);
            }
        }
        this.endResults = [];
        for (var _f = 0, results_2 = results; _f < results_2.length; _f++) {
            var _g = results_2[_f], r = _g[0], missingValues = _g[1];
            console.log("filterResultsByForAll (final result): " + r.bindings + ", missingValues: " + missingValues);
            if (missingValues.length == 0) {
                this.endResults.push(r);
            }
        }
    };
    InterruptibleResolution.prototype.debug_inferenceSentenceLengths = function (inferences) {
        var lengths = [];
        if (inferences == null)
            return null;
        for (var _i = 0, inferences_1 = inferences; _i < inferences_1.length; _i++) {
            var r = inferences_1[_i];
            lengths.push(r.sentence.terms.length);
        }
        return lengths;
    };
    // the JSON stringify does better outerHTML() : string { return this.saveToXML();}  
    InterruptibleResolution.prototype.saveToXML = function () {
        var str = "<InterruptibleResolution>\n";
        // ...
        str += "</InterruptibleResolution>";
        return str;
    };
    return InterruptibleResolution;
}());
