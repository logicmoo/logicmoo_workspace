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
var EtaoinClose_IntentionAction = /** @class */ (function (_super) {
    __extends(EtaoinClose_IntentionAction, _super);
    function EtaoinClose_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EtaoinClose_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.close")))
            return true;
        return false;
    };
    EtaoinClose_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var requester = ir.requester;
        var alternative_actions = ir.alternative_actions;
        if (alternative_actions == null)
            alternative_actions = [ir.action];
        var denyrequestCause = null;
        var anySuccessful = false;
        var doors = [];
        var lights = [];
        for (var _i = 0, alternative_actions_1 = alternative_actions; _i < alternative_actions_1.length; _i++) {
            var intention = alternative_actions_1[_i];
            var targetID = (intention.attributes[1]).value;
            // check if it's a door:
            var door_tmp = ai.game.findObjectByIDJustObject(targetID);
            if (door_tmp != null) {
                if (door_tmp instanceof A4Door) {
                    doors.push(door_tmp);
                }
                else if (door_tmp.sort.is_a(ai.o.getSort("light"))) {
                    lights.push(door_tmp);
                }
                else {
                    denyrequestCause = Term.fromString("#not(door('" + targetID + "'[#id]))", ai.o);
                }
            }
            else if (door_tmp == null) {
                // see if it's a location with a door (e.g., a bedroom):
                // We don't launch a whole inference here, as these facts are directly on the knowledge base:
                var belong_l = ai.longTermMemory.allSingleTermMatches(ai.o.getSort("verb.belong"), 2, ai.o);
                for (var _a = 0, belong_l_1 = belong_l; _a < belong_l_1.length; _a++) {
                    var belong = belong_l_1[_a];
                    var t = belong.terms[0];
                    if ((t.attributes[0] instanceof ConstantTermAttribute) &&
                        (t.attributes[1] instanceof ConstantTermAttribute)) {
                        if (t.attributes[1].value == targetID) {
                            var door = ai.game.findObjectByIDJustObject(t.attributes[0].value);
                            if (door != null && (door instanceof A4Door)) {
                                doors.push(door);
                            }
                        }
                    }
                }
            }
        }
        var numberConstraint = ir.resolveNumberConstraint(ir.numberConstraint, doors.length + lights.length);
        if (lights.length > 0) {
            for (var _b = 0, lights_1 = lights; _b < lights_1.length; _b++) {
                var light = lights_1[_b];
                var room = ai.game.getAILocation(light);
                if (ai.game.turnLightOff(room.id)) {
                    // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                    if (ir.requestingPerformative != null)
                        ir.requestingPerformative.addMentionToPerformative(light.ID, ai.o);
                    anySuccessful = true;
                    numberConstraint--;
                    if (numberConstraint <= 0)
                        break;
                }
                else {
                    denyrequestCause = Term.fromString("powered.state('" + light.ID + "'[#id], 'powered.off'[powered.off])", ai.o);
                }
            }
        }
        if (doors.length > 0) {
            // we have found at least a door!
            for (var _c = 0, doors_1 = doors; _c < doors_1.length; _c++) {
                var door = doors_1[_c];
                if (!door.closed) {
                    // see if player has permission:
                    if (ai.doorsPlayerIsNotPermittedToOpen.indexOf(door.doorID) == -1) {
                        door.eventWithID(A4_EVENT_CLOSE, door.doorID, null, door.map, ai.game);
                        // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                        if (ir.requestingPerformative != null)
                            ir.requestingPerformative.addMentionToPerformative(door.ID, ai.o);
                        anySuccessful = true;
                        numberConstraint--;
                        if (numberConstraint <= 0)
                            break;
                    }
                    else {
                        denyrequestCause = Term.fromString("#not(verb.have(" + requester + ",[permission-to]))", ai.o);
                    }
                }
                else {
                    denyrequestCause = Term.fromString("property.closed('" + door.ID + "'[#id])", ai.o);
                }
            }
        }
        if (anySuccessful) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            app.achievement_nlp_all_etaoin_actions[1] = true;
            app.trigger_achievement_complete_alert();
            ir.succeeded = true;
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
        }
        return true;
    };
    EtaoinClose_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"EtaoinClose_IntentionAction\"/>";
    };
    EtaoinClose_IntentionAction.loadFromXML = function (xml, ai) {
        return new EtaoinClose_IntentionAction();
    };
    return EtaoinClose_IntentionAction;
}(IntentionAction));
