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
var A4Locate_IntentionAction = /** @class */ (function (_super) {
    __extends(A4Locate_IntentionAction, _super);
    function A4Locate_IntentionAction() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.targetObject = null;
        return _this;
    }
    A4Locate_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.locate")))
            return true;
        return false;
    };
    A4Locate_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var requester = ir.requester;
        var alternative_actions = ir.alternative_actions;
        if (alternative_actions == null)
            alternative_actions = [ir.action];
        var denyrequestCause = null;
        for (var _i = 0, alternative_actions_1 = alternative_actions; _i < alternative_actions_1.length; _i++) {
            var intention = alternative_actions_1[_i];
            var targetID = (intention.attributes[1]).value;
            // Check if it's an object:
            var targetObjectL = ai.game.findObjectByID(targetID);
            if (targetObjectL == null) {
                denyrequestCause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + targetID + "'[#id]))", ai.o);
                continue;
            }
            this.targetObject = targetID;
            var term_1 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term_1, null, null, null, ai.timeStamp));
            // If the object was not mentioned explicitly in the performative, add it to the natural language context:
            if (ir.requestingPerformative != null)
                ir.requestingPerformative.addMentionToPerformative(targetID, ai.o);
            ir.succeeded = true;
            return true;
        }
        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
        if (denyrequestCause == null) {
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        else {
            ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(denyrequestCause, null, ai.timeStamp), ai.timeStamp));
        }
        ir.succeeded = false;
        return true;
    };
    A4Locate_IntentionAction.prototype.saveToXML = function (ai) {
        var str = "<IntentionAction type=\"A4Locate_IntentionAction\"";
        if (this.targetObject == null) {
            return str + "/>";
        }
        else {
            return str + " targetObject=\"" + this.targetObject + "\"/>";
        }
    };
    A4Locate_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new A4Locate_IntentionAction();
        if (xml.getAttribute("targetObject") != null) {
            a.targetObject = xml.getAttribute("targetObject");
        }
        return a;
    };
    return A4Locate_IntentionAction;
}(IntentionAction));
