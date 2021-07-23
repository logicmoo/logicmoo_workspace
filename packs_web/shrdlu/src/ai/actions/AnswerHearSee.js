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
var AnswerHearSee_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerHearSee_IntentionAction, _super);
    function AnswerHearSee_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerHearSee_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.hear")) ||
            intention.functor.is_a(ai.o.getSort("verb.see")))
            return true;
        return false;
    };
    AnswerHearSee_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var requester = ir.requester;
        var best = null;
        var l = [ir.action];
        if (requester == null)
            return true;
        if (ir.alternative_actions != null && ir.alternative_actions.length > 0)
            l = ir.alternative_actions;
        for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
            var intention = l_1[_i];
            if (intention.attributes.length == 2 &&
                (intention.attributes[0] instanceof ConstantTermAttribute) &&
                (intention.attributes[0]).value == ai.selfID) {
                if (intention.attributes[1] instanceof ConstantTermAttribute) {
                    // Case where the target is a constant:
                    if (intention.functor.is_a(ai.o.getSort("verb.see"))) {
                        if (ai.canSee((intention.attributes[1]).value)) {
                            // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                            if (ir.requestingPerformative != null) {
                                ir.requestingPerformative.addMentionToPerformative((intention.attributes[1]).value, ai.o);
                            }
                            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'yes'[symbol]))", ai.o);
                            ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
                            ir.succeeded = true;
                            return true;
                        }
                        else {
                            if (best == null)
                                best = "no";
                        }
                    }
                    else if (intention.functor.is_a(ai.o.getSort("verb.hear"))) {
                        if (ai.canHear((intention.attributes[1]).value)) {
                            // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                            if (ir.requestingPerformative != null) {
                                ir.requestingPerformative.addMentionToPerformative((intention.attributes[1]).value, ai.o);
                            }
                            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'yes'[symbol]))", ai.o);
                            ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
                            ir.succeeded = true;
                            return true;
                        }
                        else {
                            if (best == null)
                                best = "no";
                        }
                    }
                    else {
                        // we should never get here...
                        if (best == null || best == "no")
                            best = "unknown";
                    }
                }
                else {
                    if (best == null || best == "no")
                        best = "unknown";
                }
            }
            else {
                if (best == null || best == "no")
                    best = "unknown";
            }
        }
        if (best == "no") {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'no'[symbol]))", ai.o);
            ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
        }
        else {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + requester + ",'unknown'[symbol]))", ai.o);
            ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
        }
        ir.succeeded = true;
        return true;
    };
    AnswerHearSee_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerHearSee_IntentionAction\"/>";
    };
    AnswerHearSee_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerHearSee_IntentionAction();
    };
    return AnswerHearSee_IntentionAction;
}(IntentionAction));
