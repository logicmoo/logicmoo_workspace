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
var AnswerWhy_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerWhy_IntentionAction, _super);
    function AnswerWhy_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerWhy_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.why")))
            return true;
        return false;
    };
    AnswerWhy_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        if (intention.attributes.length == 2) {
            if (intention.attributes[1] instanceof ConstantTermAttribute) {
                var targetID = intention.attributes[1].value;
                console.log(ai.selfID + " answer followup why to " + targetID);
                // this is a follow up question! see if we can reconstruct the question...
                var context = ai.contextForSpeakerWithoutCreatingANewOne(targetID);
                if (context != null) {
                    // get the last sentence we said:
                    var lastPerf = context.lastPerformativeBy(ai.selfID);
                    if (lastPerf.cause != null) {
                        // we already know the cause!!
                        var causeTerm = lastPerf.cause.term;
                        var causeTerms = [];
                        if (causeTerm.functor.name == "#and") {
                            var tal = Term.elementsInList(causeTerm, "#and");
                            for (var _i = 0, tal_1 = tal; _i < tal_1.length; _i++) {
                                var ta = tal_1[_i];
                                if (ta instanceof TermTermAttribute) {
                                    causeTerms.push(ta.term);
                                }
                            }
                        }
                        else {
                            causeTerms = [causeTerm];
                        }
                        for (var _a = 0, causeTerms_1 = causeTerms; _a < causeTerms_1.length; _a++) {
                            var causeTerm2 = causeTerms_1[_a];
                            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + context.speaker + "'[#id], relation.cause([any]," + causeTerm2 + ")))", ai.o);
                            ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, lastPerf.cause.cause, ai.timeStamp));
                        }
                        ir.succeeded = true;
                        return true;
                    }
                    var newIntention = null;
                    if (lastPerf != null)
                        newIntention = this.convertPerformativeToWhyQuestionAnswerIntention(lastPerf, ai, context);
                    if (newIntention != null) {
                        intention = newIntention;
                    }
                    else {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'unknown'[symbol]))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                        ir.succeeded = false;
                        return true;
                    }
                }
                else {
                    // this should never happen
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id],#and(the(NOUN:'perf.question'[perf.question],S:[singular]),noun(NOUN,S))))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                    ir.succeeded = false;
                    return true;
                }
            }
        }
        console.log(ai.selfID + " answer why: " + intention);
        if (intention.attributes.length >= 3 &&
            intention.attributes[2] instanceof TermTermAttribute) {
            var toExplain = (intention.attributes[2]).term;
            // STEP 1: check to see if the term is in the intentionsCausedByRequest list, so we don't need inference:
            for (var _b = 0, _c = ai.intentionsCausedByRequest; _b < _c.length; _b++) {
                var cl = _c[_b];
                var b = new Bindings();
                if (cl.action.unify(toExplain, OCCURS_CHECK, b)) {
                    // found!
                    var term = new Term(ai.o.getSort("verb.ask"), [cl.requester, new ConstantTermAttribute(ai.selfID, ai.cache_sort_id)]);
                    var term2 = new Term(ai.o.getSort("relation.cause"), [new TermTermAttribute(toExplain), new TermTermAttribute(term)]);
                    var term3 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + "))", ai.o);
                    (term3.attributes[1]).term.attributes.push(new TermTermAttribute(term2));
                    ai.intentions.push(new IntentionRecord(term3, requester, null, null, ai.timeStamp));
                    ir.succeeded = true;
                    return true;
                }
            }
            // STEP 2: otherwise, launch an inference process to find an explanation:
            var query = new Term(ai.o.getSort("relation.cause"), [new TermTermAttribute(toExplain),
                new VariableTermAttribute(ai.o.getSort("any"), "CAUSE")]);
            // negate the query:
            var negated_s = new Sentence([query], [false]);
            var negated_toExplain = new Sentence([], []);
            // Second inference objective (negate "toExplain"):
            {
                var s_l = Term.termToSentences(toExplain, ai.o);
                var toDelete = [];
                //let timeTerm:Term = null;
                for (var _d = 0, s_l_1 = s_l; _d < s_l_1.length; _d++) {
                    var s = s_l_1[_d];
                    if (s.terms.length == 1 &&
                        (s.terms[0].functor.is_a(ai.o.getSort("time.past")) ||
                            s.terms[0].functor.is_a(ai.o.getSort("time.present")) ||
                            s.terms[0].functor.is_a(ai.o.getSort("time.future")))) {
                        //timeTerm = s.terms[0];	// TODO: for now, we assume there is only one
                        toDelete.push(s);
                    }
                }
                for (var _e = 0, toDelete_1 = toDelete; _e < toDelete_1.length; _e++) {
                    var s = toDelete_1[_e];
                    s_l.splice(s_l.indexOf(s), 1);
                }
                for (var _f = 0, s_l_2 = s_l; _f < s_l_2.length; _f++) {
                    var s = s_l_2[_f];
                    var tmp = s.negate();
                    if (tmp == null || tmp.length != 1) {
                        console.error("executeIntention answer predicate: cannot negate query!: " + intention);
                        ir.succeeded = false;
                        return true;
                    }
                    negated_toExplain.terms = negated_toExplain.terms.concat(tmp[0].terms);
                    negated_toExplain.sign = negated_toExplain.sign.concat(tmp[0].sign);
                }
                console.log("executeIntention answer why: negated_toExplain = " + negated_toExplain);
            }
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [[negated_s], [negated_toExplain]], 1, 0, false, null, new AnswerWhy_InferenceEffect(intention)));
        }
        else {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'unknown'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
        }
        // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
        // that is then replaced with true/false after inference/continuous is done
        ir.succeeded = true;
        return true;
    };
    AnswerWhy_IntentionAction.prototype.convertPerformativeToWhyQuestionAnswerIntention = function (nlcp, ai, context) {
        if (nlcp.performative.functor.is_a(ai.o.getSort("perf.inform")) &&
            (nlcp.performative.attributes[1] instanceof TermTermAttribute)) {
            console.log("convertPerformativeToWhyQuestionAnswerIntention: perf.inform");
            var predicate = nlcp.performative.attributes[1].term;
            var newIntention = new Term(ai.o.getSort("action.answer.why"), [new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                nlcp.performative.attributes[0],
                new TermTermAttribute(predicate)]);
            console.log("convertPerformativeToWhyQuestionAnswerIntention, newIntention: " + newIntention);
            return newIntention;
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.ack.denyrequest")) &&
            nlcp.performative.attributes.length >= 1 &&
            nlcp.performative.attributes[0] instanceof ConstantTermAttribute) {
            // Look to see which was the request for action:
            var request = null;
            for (var _i = 0, _a = context.performatives; _i < _a.length; _i++) {
                var p = _a[_i];
                if (p.speaker != nlcp.speaker &&
                    (p.performative.functor.name == "perf.request.action" ||
                        p.performative.functor.name == "perf.q.action") &&
                    p.performative.attributes.length >= 2) {
                    request = p;
                    break;
                }
            }
            if (request == null)
                return null;
            console.log("convertPerformativeToWhyQuestionAnswerIntention: perf.ack.denyrequest with request: " + request.performative);
            var requestedAction = request.performative.attributes[1];
            var term1 = new Term(ai.o.getSort("#not"), [new TermTermAttribute(new Term(ai.o.getSort("verb.can"), [new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                    requestedAction]))]);
            var term = null;
            if (request.performative.attributes.length >= 3) {
                term = new Term(ai.o.getSort("#and"), [new TermTermAttribute(term1), request.performative.attributes[2]]);
            }
            else {
                term = term1;
            }
            var newIntention = new Term(ai.o.getSort("action.answer.why"), [new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                nlcp.performative.attributes[0],
                new TermTermAttribute(term)]);
            console.log("convertPerformativeToWhyQuestionAnswerIntention, newIntention: " + newIntention);
            return newIntention;
        }
        return null;
    };
    AnswerWhy_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerWhy_IntentionAction\"/>";
    };
    AnswerWhy_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerWhy_IntentionAction();
    };
    return AnswerWhy_IntentionAction;
}(IntentionAction));
