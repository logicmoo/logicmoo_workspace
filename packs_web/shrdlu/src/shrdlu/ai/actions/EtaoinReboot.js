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
var EtaoinReboot_IntentionAction = /** @class */ (function (_super) {
    __extends(EtaoinReboot_IntentionAction, _super);
    function EtaoinReboot_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EtaoinReboot_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.reboot")) &&
            (intention.attributes.length == 1 ||
                intention.attributes.length == 2))
            return true;
        return false;
    };
    EtaoinReboot_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        if (intention.attributes.length == 1) {
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
            ai.currentEpisodeTerms = [];
            ai.oxygen_message_timer = 0;
            app.achievement_nlp_all_etaoin_actions[6] = true;
            app.trigger_achievement_complete_alert();
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                ai.queueIntention(term, requester, null);
            }
            return true;
        }
        else if (intention.attributes.length == 2 &&
            intention.attributes[1] instanceof ConstantTermAttribute) {
            var target = intention.attributes[1].value;
            if (target == "shrdlu") {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                var term2 = new Term(ai.o.getSort("verb.reboot"), [intention.attributes[0]]);
                ai.game.shrdluAI.queueIntention(term2, null, null);
                ir.succeeded = true;
                return true;
            }
            else if (target == "qwerty") {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                var term2 = new Term(ai.o.getSort("verb.reboot"), [intention.attributes[0]]);
                ai.game.qwertyAI.queueIntention(term2, null, null);
                ir.succeeded = true;
                return true;
            }
            else if (target == "etaoin") {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                var term2 = new Term(ai.o.getSort("verb.reboot"), [intention.attributes[0]]);
                ai.queueIntention(term2, null, null);
                ir.succeeded = true;
                return true;
            }
        }
        if (requester != null) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        ir.succeeded = false;
        return true;
    };
    EtaoinReboot_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"EtaoinReboot_IntentionAction\"/>";
    };
    EtaoinReboot_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new EtaoinReboot_IntentionAction();
        return a;
    };
    return EtaoinReboot_IntentionAction;
}(IntentionAction));
