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
var AnswerQuery_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerQuery_IntentionAction, _super);
    function AnswerQuery_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerQuery_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.query")) ||
            intention.functor.is_a(ai.o.getSort("action.answer.query-followup")))
            return true;
        return false;
    };
    AnswerQuery_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        if (intention.functor == ai.o.getSort("action.answer.query-followup")) {
            if (intention.attributes.length == 3) {
                // follow up query:
                console.log(ai.selfID + " answer query followup: " + intention.attributes[0] + " - " + intention.attributes[1] + " - " + intention.attributes[2]);
                var targetID = intention.attributes[1].value;
                var context = ai.contextForSpeakerWithoutCreatingANewOne(targetID);
                if (context != null) {
                    // get the last sentence we said:
                    var lastPerf = null;
                    // we don't use "lastPerformativeBy", since that would just return the "follow up question"
                    for (var i = 1; i < context.performatives.length; i++) {
                        if (context.performatives[i].speaker == targetID) {
                            lastPerf = context.performatives[i];
                            break;
                        }
                    }
                    var newIntention = null;
                    if (lastPerf != null)
                        newIntention = this.convertPerformativeToQueryfollowupQuestionAnswerIntention(lastPerf, intention.attributes[2].sort, ai);
                    if (newIntention == null) {
                        // this should never happen
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",'unknown'[symbol]))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                        ir.succeeded = false;
                        return true;
                    }
                    intention = newIntention;
                }
                else {
                    ir.succeeded = false;
                    return true;
                }
            }
            else {
                console.error("executeIntention answer queryfollowup: wrong number of arguments: " + intention);
                ir.succeeded = false;
                return true;
            }
        }
        return AnswerQuery_IntentionAction.launchQueryInference(intention, ir, ai, "AnswerQuery_InferenceEffect");
    };
    AnswerQuery_IntentionAction.launchQueryInference = function (intention, ir, ai, InferenceEffectName) {
        console.log(ai.selfID + " answer query: " + intention.attributes[0] + " - " + intention.attributes[1] + " - " + intention.attributes[2]);
        if (intention.attributes[2] instanceof TermTermAttribute) {
            var queryPerformative = intention.attributes[2].term;
            var s_l = Term.termToSentences((queryPerformative.attributes[2]).term, ai.o);
            var forAlls = [];
            if (queryPerformative.attributes.length >= 4) {
                forAlls = NLParser.termsInList((queryPerformative.attributes[3]).term, "#and");
            }
            console.log("forAlls: " + forAlls);
            console.log("expression: " + (queryPerformative.attributes[2]).term);
            console.log("expression to sentences: ");
            for (var _i = 0, s_l_1 = s_l; _i < s_l_1.length; _i++) {
                var s = s_l_1[_i];
                console.log("    " + s.toString());
            }
            // search for time-related sentences (which just indicate the time at which this query must be performed):
            // or terms that require KB updates (e.g., property.age):
            var toDelete = [];
            var timeTerm = null;
            for (var _a = 0, s_l_2 = s_l; _a < s_l_2.length; _a++) {
                var s = s_l_2[_a];
                if (s.terms.length == 1 && s.terms[0].functor.name == "time.past") {
                    timeTerm = s.terms[0]; // TODO: for now, we assume there is only one
                    toDelete.push(s);
                }
                if (s.terms.length == 1 && s.terms[0].functor.name == "property.age") {
                    // recalculate the age of all the characters we know property.botn of:
                    ai.recalculateCharacterAges();
                }
            }
            for (var _b = 0, toDelete_1 = toDelete; _b < toDelete_1.length; _b++) {
                var s = toDelete_1[_b];
                s_l.splice(s_l.indexOf(s), 1);
            }
            // negate the query:
            var targets = [];
            var negatedExpression = new Term(ai.o.getSort("#not"), [new TermTermAttribute(Term.sentencesToTerm(s_l, ai.o))]);
            if (toDelete.length == 0) {
                negatedExpression = new Term(ai.o.getSort("#not"), [new TermTermAttribute((queryPerformative.attributes[2]).term)]);
            }
            console.log("negatedExpression: " + negatedExpression);
            var target = Term.termToSentences(negatedExpression, ai.o);
            targets.push(target);
            // negate the forAlls:
            for (var _c = 0, forAlls_1 = forAlls; _c < forAlls_1.length; _c++) {
                var forAll = forAlls_1[_c];
                if (forAll.attributes.length >= 2 &&
                    forAll.attributes[1] instanceof TermTermAttribute) {
                    var forAllTerm = (forAll.attributes[1]).term;
                    var negatedForAll = Term.termToSentences(new Term(ai.o.getSort("#not"), [new TermTermAttribute(forAllTerm)]), ai.o);
                    targets.push(negatedForAll);
                }
            }
            console.log("targets: ");
            for (var _d = 0, targets_1 = targets; _d < targets_1.length; _d++) {
                var t = targets_1[_d];
                console.log("    " + t);
            }
            // 2) start the inference process:
            var requestingPerformative = null;
            if (ir != null)
                requestingPerformative = ir.requestingPerformative;
            if (InferenceEffectName == "HandleRephrasing_InferenceEffect") {
                ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], targets, 1, 0, true, timeTerm, new HandleRephrasing_InferenceEffect(intention, requestingPerformative)));
            }
            else {
                ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], targets, 1, 0, true, timeTerm, new AnswerQuery_InferenceEffect(intention, requestingPerformative)));
            }
            // 			let  negated_s:Sentence = new Sentence([],[]);
            // 			for(let s of s_l) {
            // 				let  tmp:Sentence[] = s.negate();
            // 				if (tmp == null || tmp.length != 1) {
            // 					console.error("executeIntention answer query: cannot negate query!: " + intention);		
            // 					return true;
            // 				}
            // 				negated_s.terms = negated_s.terms.concat(tmp[0].terms);
            // 				negated_s.sign = negated_s.sign.concat(tmp[0].sign);
            // 			}
            // //				console.log("executeIntention answer query: negated_s = " + negated_s);
            // 			ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [[negated_s]], 1, 0, true, timeTerm, new AnswerQuery_InferenceEffect(intention, ir.requestingPerformative)));
        }
        else {
            console.error("executeIntention answer query: attribute[2] was not a TermTermAttribute: " + intention);
            if (ir != null)
                ir.succeeded = false;
        }
        // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
        // that is then replaced with true/false after inference/continuous is done
        if (ir != null)
            ir.succeeded = true;
        return true;
    };
    AnswerQuery_IntentionAction.prototype.convertPerformativeToQueryfollowupQuestionAnswerIntention = function (nlcp, sortToLookFor, ai) {
        console.log("nlcp.performative: " + nlcp.performative);
        console.log("sortToLookFor: " + sortToLookFor);
        if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.predicate")) &&
            nlcp.performative.attributes.length == 2 &&
            nlcp.performative.attributes[1] instanceof TermTermAttribute) {
            var predicate = nlcp.performative.attributes[1].term;
            var predicateTerms = NLParser.termsInList(predicate, "#and");
            var objectTerms = [];
            for (var _i = 0, predicateTerms_1 = predicateTerms; _i < predicateTerms_1.length; _i++) {
                var t = predicateTerms_1[_i];
                if (t.functor.is_a(sortToLookFor) &&
                    t.attributes.length == 1) {
                    var queryVariable = t.attributes[0];
                    var newPerformative = new Term(ai.o.getSort("perf.q.query"), [nlcp.performative.attributes[0],
                        queryVariable,
                        nlcp.performative.attributes[1]]);
                    var intention = new Term(ai.o.getSort("action.answer.query"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                        new ConstantTermAttribute(nlcp.speaker, ai.cache_sort_id),
                        new TermTermAttribute(newPerformative)]);
                    console.log("convertPerformativeToQueryfollowupQuestionAnswerIntention, newIntention: " + intention);
                    return intention;
                }
                if (t.functor.is_a(ai.o.getSort("object")) && t.attributes.length == 1) {
                    objectTerms.push(t);
                }
            }
            if (objectTerms.length == 1) {
                // If we have not found the desired term, but there is at least an "object", default to that:
                var t = objectTerms[0];
                var queryVariable = t.attributes[0];
                var newPerformative = new Term(ai.o.getSort("perf.q.query"), [nlcp.performative.attributes[0],
                    queryVariable,
                    nlcp.performative.attributes[1]]);
                var intention = new Term(ai.o.getSort("action.answer.query"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                    new ConstantTermAttribute(nlcp.speaker, ai.cache_sort_id),
                    new TermTermAttribute(newPerformative)]);
                console.log("convertPerformativeToQueryfollowupQuestionAnswerIntention, newIntention: " + intention);
                return intention;
            }
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.query")) &&
            nlcp.performative.attributes.length == 3 &&
            (nlcp.performative.attributes[1] instanceof VariableTermAttribute) &&
            (nlcp.performative.attributes[2] instanceof TermTermAttribute)) {
            var queryVariable = nlcp.performative.attributes[1];
            var query = nlcp.performative.attributes[2].term;
            var queryTerms = NLParser.termsInList(query, "#and");
            var newQuery = new Term(sortToLookFor, [queryVariable]);
            for (var _a = 0, queryTerms_1 = queryTerms; _a < queryTerms_1.length; _a++) {
                var t = queryTerms_1[_a];
                var remove = false;
                // remove all the terms that set the type of the entities to find, and leave only the new one
                if (t.attributes.length == 1) {
                    for (var _b = 0, _c = POSParser.sortsToConsiderForTypes; _b < _c.length; _b++) {
                        var sName = _c[_b];
                        if (t.functor.is_a(ai.o.getSort(sName))) {
                            remove = true;
                            break;
                        }
                    }
                }
                if (!remove) {
                    newQuery = new Term(ai.o.getSort("#and"), [new TermTermAttribute(t),
                        new TermTermAttribute(newQuery)]);
                }
            }
            var newPerformative = new Term(ai.o.getSort("perf.q.query"), [nlcp.performative.attributes[0],
                queryVariable,
                new TermTermAttribute(newQuery)]);
            var intention = new Term(ai.o.getSort("action.answer.query"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                new ConstantTermAttribute(nlcp.speaker, ai.cache_sort_id),
                new TermTermAttribute(newPerformative)]);
            console.log("convertPerformativeToQueryfollowupQuestionAnswerIntention, newIntention: " + intention);
            return intention;
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.howmany"))) {
            var attributes = [];
            for (var _d = 0, _e = nlcp.performative.attributes; _d < _e.length; _d++) {
                var att = _e[_d];
                attributes.push(att);
            }
            var newPerformative = new Term(ai.o.getSort("perf.q.query"), attributes);
            var intention = new Term(ai.o.getSort("action.answer.query"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                new ConstantTermAttribute(nlcp.speaker, ai.cache_sort_id),
                new TermTermAttribute(newPerformative)]);
            console.log("convertPerformativeToQueryfollowupQuestionAnswerIntention, newIntention: " + intention);
            return intention;
        }
        return null;
    };
    AnswerQuery_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerQuery_IntentionAction\"/>";
    };
    AnswerQuery_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerQuery_IntentionAction();
    };
    return AnswerQuery_IntentionAction;
}(IntentionAction));
