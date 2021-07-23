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
var AnswerHow_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerHow_IntentionAction, _super);
    function AnswerHow_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerHow_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.how")))
            return true;
        return false;
    };
    AnswerHow_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        if (intention.attributes.length == 2) {
            if (intention.attributes[1] instanceof ConstantTermAttribute) {
                var targetID = intention.attributes[1].value;
                console.log(ai.selfID + " answer followup how to " + targetID);
                // this is a follow up question! see if we can reconstruct the question...
                var context = ai.contextForSpeakerWithoutCreatingANewOne(targetID);
                if (context != null) {
                    // get the last sentence we said:
                    var lastPerf = context.lastPerformativeBy(ai.selfID);
                    var newIntention = null;
                    if (lastPerf != null)
                        newIntention = this.convertPerformativeToHowQuestionAnswerIntention(lastPerf, ai, context);
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
        if (intention.attributes.length >= 3 &&
            intention.attributes[2] instanceof TermTermAttribute) {
            var action = intention.attributes[2].term;
            console.log(ai.selfID + " answer how: " + intention.attributes[2]);
            // we add the sentence with negative sign, to see if it introduces a contradiction
            var target1 = [new Sentence([new Term(ai.o.getSort("relation.howto"), [new TermTermAttribute(action),
                        new VariableTermAttribute(ai.o.getSort("any"), "HOW")])], [false])];
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [target1], 1, 0, false, null, new AnswerHow_InferenceEffect(intention)));
            // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
            // that is then replaced with true/false after inference/continuous is done
            ir.succeeded = true;
            /*
            if (requester != null) {
                let term:Term = Term.fromString("action.talk('"+ai.selfID+"'[#id], perf.inform.answer("+requester+",'unknown'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
            }
            */
        }
        ir.succeeded = true;
        return true;
    };
    AnswerHow_IntentionAction.prototype.convertPerformativeToHowQuestionAnswerIntention = function (nlcp, ai, context) {
        if ((nlcp.performative.functor.is_a(ai.o.getSort("perf.request.action")) ||
            nlcp.performative.functor.is_a(ai.o.getSort("perf.q.action"))) &&
            (nlcp.performative.attributes[1] instanceof TermTermAttribute)) {
            console.log("convertPerformativeToHowQuestionAnswerIntention: perf.request.action/perf.q.action");
            var predicate = nlcp.performative.attributes[1].term;
            var newIntention = new Term(ai.o.getSort("action.answer.how"), [new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                nlcp.performative.attributes[0],
                new TermTermAttribute(predicate)]);
            console.log("convertPerformativeToHowQuestionAnswerIntention, newIntention: " + newIntention);
            return newIntention;
        }
        return null;
    };
    AnswerHow_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerHow_IntentionAction\"/>";
    };
    AnswerHow_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerHow_IntentionAction();
    };
    return AnswerHow_IntentionAction;
}(IntentionAction));
