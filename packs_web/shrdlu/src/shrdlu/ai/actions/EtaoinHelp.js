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
var EtaoinHelp_IntentionAction = /** @class */ (function (_super) {
    __extends(EtaoinHelp_IntentionAction, _super);
    function EtaoinHelp_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EtaoinHelp_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.help")) &&
            intention.attributes.length >= 2)
            return true;
        return false;
    };
    EtaoinHelp_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        console.log(ai.selfID + " help: " + intention);
        if (intention.attributes.length == 2) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.q.how(" + requester + ", verb.help('" + ai.selfID + "'[#id]," + intention.attributes[1] + ")))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            ir.succeeded = true;
        }
        else if (intention.attributes.length == 3 && (intention.attributes[2] instanceof TermTermAttribute)) {
            var nestedIntention = intention.attributes[2].term;
            if (nestedIntention.attributes.length > 0 &&
                (nestedIntention.attributes[0] instanceof ConstantTermAttribute)) {
                var handler = null;
                var newNestedIntention = nestedIntention.clone([]);
                newNestedIntention.attributes[0] = new ConstantTermAttribute(ai.selfID, ai.cache_sort_id);
                if (newNestedIntention.functor.is_a(ai.o.getSort("verb.go"))) {
                    // Special case for verb "go":
                    newNestedIntention = new Term(ai.o.getSort("action.answer.how"), [intention.attributes[0],
                        intention.attributes[1],
                        new TermTermAttribute(nestedIntention)]);
                }
                console.log("EtaoinHelp_IntentionAction, newNestedIntention:" + newNestedIntention);
                for (var _i = 0, _a = ai.intentionHandlers; _i < _a.length; _i++) {
                    var ih = _a[_i];
                    if (ih.canHandle(newNestedIntention, ai)) {
                        handler = ih;
                        break;
                    }
                }
                if (handler != null) {
                    var newIr = new IntentionRecord(newNestedIntention, ir.requester, ir.requestingPerformative, ir.cause, ir.timeStamp);
                    ir.succeeded = true;
                    return handler.execute(newIr, ai);
                }
                else {
                    if (requester != null) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
                    ir.succeeded = false;
                }
            }
            else {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
            }
        }
        else {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
        }
        return true;
    };
    EtaoinHelp_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"EtaoinHelp_IntentionAction\"/>";
    };
    EtaoinHelp_IntentionAction.loadFromXML = function (xml, ai) {
        return new EtaoinHelp_IntentionAction();
    };
    return EtaoinHelp_IntentionAction;
}(IntentionAction));
