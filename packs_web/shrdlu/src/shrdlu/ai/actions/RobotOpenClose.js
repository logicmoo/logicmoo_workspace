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
var RobotOpenClose_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotOpenClose_IntentionAction, _super);
    function RobotOpenClose_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotOpenClose_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.open")) ||
            intention.functor.is_a(ai.o.getSort("action.close")))
            return true;
        return false;
    };
    RobotOpenClose_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var requester = ir.requester;
        var alternative_actions = ir.alternative_actions;
        if (alternative_actions == null)
            alternative_actions = [ir.action];
        var denyrequestCause = null;
        var open = true;
        if (ai.robot.isInVehicle()) {
            if (requester != null) {
                var term_1 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term_1, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (ir.action.functor.is_a(ai.o.getSort("action.close")))
            open = false;
        var closestDoorIntention = null;
        var closestDoor = null;
        var closestContainerIntention = null;
        var closestContainer = null;
        for (var _i = 0, alternative_actions_1 = alternative_actions; _i < alternative_actions_1.length; _i++) {
            var intention = alternative_actions_1[_i];
            var targetID = (intention.attributes[1]).value;
            var door = null;
            var container = null;
            // check if it's a door:
            var object_tmp = ai.game.findObjectByIDJustObject(targetID);
            if (object_tmp != null) {
                if (object_tmp instanceof A4Door) {
                    door = object_tmp;
                }
                else if (object_tmp instanceof A4ObstacleContainer) {
                    container = object_tmp;
                }
                else {
                    denyrequestCause = Term.fromString("#not(door('" + targetID + "'[#id]))", ai.o);
                }
            }
            else if (object_tmp == null) {
                // see if it's a location with a door (e.g., a bedroom):
                // We don't launch a whole inference here, as these facts are directly on the knowledge base:
                var belong_l = ai.longTermMemory.allSingleTermMatches(ai.o.getSort("verb.belong"), 2, ai.o);
                for (var _a = 0, belong_l_1 = belong_l; _a < belong_l_1.length; _a++) {
                    var belong = belong_l_1[_a];
                    var t = belong.terms[0];
                    if ((t.attributes[0] instanceof ConstantTermAttribute) &&
                        (t.attributes[1] instanceof ConstantTermAttribute)) {
                        if (t.attributes[1].value == targetID) {
                            var door2 = ai.game.findObjectByIDJustObject(t.attributes[0].value);
                            if (door2 != null && (door2 instanceof A4Door)) {
                                door = door2;
                                break;
                            }
                        }
                    }
                }
            }
            if (door != null) {
                if (door.closed == open) {
                    // see if player has permission:
                    if (ai.doorsPlayerIsNotPermittedToOpen.indexOf(door.doorID) == -1) {
                        if (door.checkForBlockages(true, null, ai.robot.map, ai.game, [])) {
                            if (closestDoor == null ||
                                closestDoor.pixelDistance(ai.robot) > door.pixelDistance(ai.robot)) {
                                closestDoorIntention = intention;
                                closestDoor = door;
                            }
                        }
                    }
                    else {
                        denyrequestCause = Term.fromString("#not(verb.have(" + requester + ",[permission-to]))", ai.o);
                    }
                }
                else {
                    if (door.closed) {
                        denyrequestCause = Term.fromString("property.closed('" + door.ID + "'[#id])", ai.o);
                    }
                    else {
                        denyrequestCause = Term.fromString("property.opened('" + door.ID + "'[#id])", ai.o);
                    }
                }
            }
            if (container != null) {
                if (container.closeable) {
                    if (container.closed == open) {
                        var containerLocation = ai.game.getAILocation(container);
                        if (containerLocation == null || ai.locationsWherePlayerIsNotPermitted.indexOf(containerLocation.id) == -1) {
                            if (closestContainer == null ||
                                closestContainer.pixelDistance(ai.robot) > container.pixelDistance(ai.robot)) {
                                closestContainerIntention = intention;
                                closestContainer = container;
                            }
                        }
                        else {
                            denyrequestCause = Term.fromString("#not(verb.have(" + requester + ",[permission-to]))", ai.o);
                        }
                    }
                    else {
                        if (container.closed) {
                            denyrequestCause = Term.fromString("property.closed('" + container.ID + "'[#id])", ai.o);
                        }
                        else {
                            denyrequestCause = Term.fromString("property.opened('" + container.ID + "'[#id])", ai.o);
                        }
                    }
                }
            }
        }
        if (closestDoor != null) {
            app.achievement_nlp_all_robot_actions[(open ? 7 : 8)] = true;
            app.trigger_achievement_complete_alert();
            // If the object was not mentioned explicitly in the performative, add it to the natural language context:
            if (ir.requestingPerformative != null)
                ir.requestingPerformative.addMentionToPerformative(closestDoor.ID, ai.o);
            // do it!
            var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
            var s = new A4Script(A4_SCRIPT_INTERACT_WITH_OBJECT, closestDoor.ID, null, 0, false, false);
            q.scripts.push(s);
            ai.setNewAction(closestDoorIntention, ir.requester, q, null);
            ai.addLongTermTerm(new Term(closestDoorIntention.functor, [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                new TermTermAttribute(closestDoorIntention)]), PERCEPTION_PROVENANCE);
            ai.intentionsCausedByRequest.push(ir);
            var term_2 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + ir.requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term_2, null, null, null, ai.timeStamp));
            ir.succeeded = true;
            return true;
        }
        if (closestContainer != null) {
            app.achievement_nlp_all_robot_actions[(open ? 7 : 8)] = true;
            app.trigger_achievement_complete_alert();
            // If the object was not mentioned explicitly in the performative, add it to the natural language context:
            if (ir.requestingPerformative != null)
                ir.requestingPerformative.addMentionToPerformative(closestContainer.ID, ai.o);
            // do it!
            var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
            var s = new A4Script(A4_SCRIPT_INTERACT_WITH_OBJECT, closestContainer.ID, null, 0, false, false);
            q.scripts.push(s);
            ai.setNewAction(closestContainerIntention, ir.requester, q, null);
            ai.addLongTermTerm(new Term(closestContainerIntention.functor, [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                new TermTermAttribute(closestContainerIntention)]), PERCEPTION_PROVENANCE);
            ai.intentionsCausedByRequest.push(ir);
            var term_3 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + ir.requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term_3, null, null, null, ai.timeStamp));
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
    RobotOpenClose_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotOpenClose_IntentionAction\"/>";
    };
    RobotOpenClose_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotOpenClose_IntentionAction();
    };
    return RobotOpenClose_IntentionAction;
}(IntentionAction));
