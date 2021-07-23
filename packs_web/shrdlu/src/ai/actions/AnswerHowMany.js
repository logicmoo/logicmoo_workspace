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
var AnswerHowMany_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerHowMany_IntentionAction, _super);
    function AnswerHowMany_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerHowMany_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.howmany")))
            return true;
        return false;
    };
    AnswerHowMany_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        console.log(ai.selfID + " answer howmany: " + intention.attributes[0] + " - " + intention.attributes[1] + " - " + intention.attributes[2]);
        if (intention.attributes[2] instanceof TermTermAttribute) {
            var queryPerformative = intention.attributes[2].term;
            if (queryPerformative.attributes.length == 1) {
                if (intention.attributes[1] instanceof ConstantTermAttribute) {
                    var targetID = intention.attributes[1].value;
                    console.log(ai.selfID + " answer followup howmany to " + targetID);
                    // this is a follow up question! see if we can reconstruct the question...
                    var context = ai.contextForSpeakerWithoutCreatingANewOne(targetID);
                    if (context != null) {
                        // get the last sentence we said:
                        var lastPerf = null;
                        // we don't use "lastPerformativeBy", since that would just return the "how many?"
                        for (var i = 1; i < context.performatives.length; i++) {
                            if (context.performatives[i].speaker == targetID) {
                                lastPerf = context.performatives[i];
                                break;
                            }
                        }
                        var newIntention = null;
                        if (lastPerf != null)
                            newIntention = this.convertPerformativeToHowManyQuestionAnswerIntention(lastPerf, ai);
                        if (newIntention != null) {
                            intention = newIntention;
                            queryPerformative = intention.attributes[2].term;
                        }
                        else {
                            // this should never happen
                            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id],#and(the(NOUN:'perf.question'[perf.question],S:[singular]),noun(NOUN,S))))))", ai.o);
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
            var s_l = Term.termToSentences((queryPerformative.attributes[2]).term, ai.o);
            // negate the query:
            var negated_s = new Sentence([], []);
            for (var _i = 0, s_l_1 = s_l; _i < s_l_1.length; _i++) {
                var s = s_l_1[_i];
                var tmp = s.negate();
                if (tmp == null || tmp.length != 1) {
                    console.error("executeIntention answer query: cannot negate query!: " + intention);
                    ir.succeeded = false;
                    return true;
                }
                negated_s.terms = negated_s.terms.concat(tmp[0].terms);
                negated_s.sign = negated_s.sign.concat(tmp[0].sign);
            }
            //				console.log("executeIntention answer query: negated_s = " + negated_s);
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [[negated_s]], 1, 0, true, null, new AnswerHowMany_InferenceEffect(intention)));
            // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
            // that is then replaced with true/false after inference/continuous is done
            ir.succeeded = true;
        }
        else {
            console.error("executeIntention answer howmany: attribute[2] was not a TermTermAttribute: " + intention);
            ir.succeeded = false;
        }
        return true;
    };
    AnswerHowMany_IntentionAction.prototype.convertPerformativeToHowManyQuestionAnswerIntention = function (nlcp, ai) {
        if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.predicate")) &&
            (nlcp.performative.attributes[1] instanceof TermTermAttribute)) {
            console.log("convertPerformativeToHowManyQuestionAnswerIntention: perf.q.predicate");
            var predicate = nlcp.performative.attributes[1].term;
            var terms = NLParser.termsInList(predicate, "#and");
            var objectTerms = [];
            for (var _i = 0, terms_1 = terms; _i < terms_1.length; _i++) {
                var term = terms_1[_i];
                if (term.attributes.length == 1) {
                    if (term.functor.is_a(ai.o.getSort("object"))) {
                        objectTerms.push(term);
                    }
                }
            }
            if (objectTerms.length == 1) {
                var newIntention = new Term(ai.o.getSort("action.answer.howmany"), [nlcp.performative.attributes[0],
                    new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                    new TermTermAttribute(new Term(ai.o.getSort("perf.q.howmany"), [nlcp.performative.attributes[0],
                        objectTerms[0].attributes[0],
                        nlcp.performative.attributes[1]]))]);
                console.log("convertPerformativeToHowManyQuestionAnswerIntention, newIntention: " + newIntention);
                return newIntention;
            }
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.whereis")) ||
            nlcp.performative.functor.is_a(ai.o.getSort("perf.q.whereto"))) {
            // ...
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.query"))) {
            var attributes = [];
            for (var _a = 0, _b = nlcp.performative.attributes; _a < _b.length; _a++) {
                var att = _b[_a];
                attributes.push(att);
            }
            var newPerformative = new Term(ai.o.getSort("perf.q.howmany"), attributes);
            var intention = new Term(ai.o.getSort("action.answer.howmany"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                new ConstantTermAttribute(nlcp.speaker, ai.cache_sort_id),
                new TermTermAttribute(newPerformative)]);
            console.log("convertPerformativeToHowManyQuestionAnswerIntention, newIntention: " + intention);
            return intention;
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.howmany"))) {
            return nlcp.performative;
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.moreresults"))) {
            // ...
        }
        return null;
    };
    AnswerHowMany_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerHowMany_IntentionAction\"/>";
    };
    AnswerHowMany_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerHowMany_IntentionAction();
    };
    return AnswerHowMany_IntentionAction;
}(IntentionAction));
