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
var EtaoinSwitchOn_IntentionAction = /** @class */ (function (_super) {
    __extends(EtaoinSwitchOn_IntentionAction, _super);
    function EtaoinSwitchOn_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EtaoinSwitchOn_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.switch-on")))
            return true;
        return false;
    };
    EtaoinSwitchOn_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var requester = ir.requester;
        var alternative_actions = ir.alternative_actions;
        if (alternative_actions == null)
            alternative_actions = [ir.action];
        var denyrequestCause = null;
        var anyTurnedOn = false;
        var numberConstraint = ir.resolveNumberConstraint(ir.numberConstraint, alternative_actions.length);
        for (var _i = 0, alternative_actions_1 = alternative_actions; _i < alternative_actions_1.length; _i++) {
            var intention = alternative_actions_1[_i];
            var targetID = (intention.attributes[1]).value;
            var light = ai.game.findObjectByIDJustObject(targetID);
            if (light.sort.is_a(ai.o.getSort("light"))) {
                var room = ai.game.getAILocation(light);
                if (ai.game.turnLightOn(room.id)) {
                    anyTurnedOn = true;
                    // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                    if (ir.requestingPerformative != null)
                        ir.requestingPerformative.addMentionToPerformative(light.ID, ai.o);
                    // add a causation record:
                    var causetext = "relation.cause(powered.state('" + targetID + "'[#id], 'powered.on'[powered.on]), verb.switch-on('" + ai.selfID + "'[#id], '" + targetID + "'[#id]))";
                    var causeTerm = Term.fromString(causetext, ai.o);
                    ai.addLongTermTerm(causeTerm, PERCEPTION_PROVENANCE);
                    app.achievement_nlp_all_etaoin_actions[3] = true;
                    app.trigger_achievement_complete_alert();
                    numberConstraint--;
                    if (numberConstraint <= 0)
                        break;
                }
                else {
                    denyrequestCause = Term.fromString("powered.state('" + targetID + "'[#id], 'powered.on'[powered.on])", ai.o);
                    continue;
                }
            }
            else {
                denyrequestCause = Term.fromString("#not(light('" + targetID + "'[#id]))", ai.o);
                continue;
            }
        }
        if (requester != null) {
            if (anyTurnedOn) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                ai.intentionsCausedByRequest.push(ir);
            }
            else {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                if (denyrequestCause == null) {
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                else {
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(denyrequestCause, null, ai.timeStamp), ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
        }
        ir.succeeded = true;
        return true;
    };
    EtaoinSwitchOn_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"EtaoinSwitchOn_IntentionAction\"/>";
    };
    EtaoinSwitchOn_IntentionAction.loadFromXML = function (xml, ai) {
        return new EtaoinSwitchOn_IntentionAction();
    };
    return EtaoinSwitchOn_IntentionAction;
}(IntentionAction));
