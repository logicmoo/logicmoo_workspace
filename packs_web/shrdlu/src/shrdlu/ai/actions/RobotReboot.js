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
var RobotReboot_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotReboot_IntentionAction, _super);
    function RobotReboot_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotReboot_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.reboot")) &&
            intention.attributes.length == 1)
            return true;
        return false;
    };
    RobotReboot_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var requester = ir.requester;
        // Reset the AI variables:
        for (var _i = 0, _a = ai.contexts; _i < _a.length; _i++) {
            var c = _a[_i];
            c.reset();
        }
        ai.intentions = [];
        ai.queuedIntentions = [];
        ai.intentionsCausedByRequest = [];
        ai.currentInferenceProcess = null;
        ai.queuedInferenceProcesses = [];
        ai.respondToPerformatives = true;
        ai.terminateConversationAfterThisPerformative = false;
        ai.clearCurrentAction();
        app.achievement_nlp_all_robot_actions[14] = true;
        app.trigger_achievement_complete_alert();
        if (requester != null) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
            ai.queueIntention(term, requester, null);
        }
        return true;
    };
    RobotReboot_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotReboot_IntentionAction\"/>";
    };
    RobotReboot_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new RobotReboot_IntentionAction();
        return a;
    };
    return RobotReboot_IntentionAction;
}(IntentionAction));
