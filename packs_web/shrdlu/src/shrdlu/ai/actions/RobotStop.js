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
var RobotStop_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotStop_IntentionAction, _super);
    function RobotStop_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotStop_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.stop")))
            return true;
        return false;
    };
    RobotStop_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        if (intention.attributes.length == 1) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            app.achievement_nlp_all_robot_actions[3] = true;
            app.trigger_achievement_complete_alert();
            ai.clearCurrentAction();
            ai.addLongTermTerm(Term.fromString("verb.do('" + ai.selfID + "'[#id], 'nothing'[nothing])", ai.o), PERCEPTION_PROVENANCE);
        }
        else if (intention.attributes.length == 2 &&
            (intention.attributes[1] instanceof VariableTermAttribute) &&
            (intention.attributes[1].sort.is_a(ai.o.getSort("space.here")) ||
                intention.attributes[1].sort.is_a(ai.o.getSort("space.there")))) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            app.achievement_nlp_all_robot_actions[3] = true;
            app.trigger_achievement_complete_alert();
            ai.clearCurrentAction();
            ai.addLongTermTerm(Term.fromString("verb.do('" + ai.selfID + "'[#id], 'nothing'[nothing])", ai.o), PERCEPTION_PROVENANCE);
        }
        else {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
        }
        return true;
    };
    RobotStop_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotStop_IntentionAction\"/>";
    };
    RobotStop_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotStop_IntentionAction();
    };
    return RobotStop_IntentionAction;
}(IntentionAction));
