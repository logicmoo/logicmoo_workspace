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
var RobotTake_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotTake_IntentionAction, _super);
    function RobotTake_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotTake_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.take")) ||
            intention.functor.is_a(ai.o.getSort("verb.get")))
            return true;
        return false;
    };
    RobotTake_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        if (ai.robot.isInVehicle()) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (intention.attributes.length != 2 ||
            !(intention.attributes[0] instanceof ConstantTermAttribute) ||
            !(intention.attributes[1] instanceof ConstantTermAttribute)) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var targetID = (intention.attributes[1]).value;
        //let targetObject:A4Object = ai.game.findObjectByIDJustObject(targetID);
        var targetObjectL = ai.game.findObjectByID(targetID);
        //if (targetObject == null) {
        if (targetObjectL == null) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + targetID + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (!targetObjectL[targetObjectL.length - 1].takeable) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var destinationMap = targetObjectL[0].map;
        var destinationX = targetObjectL[0].x;
        var destinationY = targetObjectL[0].y;
        // Check if the robot can go:
        var destinationLocation = ai.game.getAILocation(targetObjectL[0]);
        var destinationLocationID = null;
        if (destinationLocation != null)
            destinationLocationID = destinationLocation.id;
        var cannotGoCause = ai.canGoTo(destinationMap, destinationLocationID, requester);
        if (cannotGoCause != null) {
            if (requester != null) {
                // deny request:
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var causeRecord = new CauseRecord(cannotGoCause, null, ai.timeStamp);
                ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
                // explain cause:
                term = new Term(ai.o.getSort("action.talk"), [new ConstantTermAttribute(ai.selfID, ai.o.getSort("#id")),
                    new TermTermAttribute(new Term(ai.o.getSort("perf.inform"), [requester, new TermTermAttribute(cannotGoCause)]))]);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (destinationMap == null) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.know('" + ai.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (targetObjectL.length > 2) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        else if (targetObjectL.length == 2) {
            if (targetObjectL[0] instanceof A4ObstacleContainer) {
                if (targetObjectL[0].closeable && targetObjectL[0].closed) {
                    if (requester != null) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                        var cause = Term.fromString("property.closed('" + targetObjectL[0].ID + "'[#id])", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                    }
                    ir.succeeded = false;
                    return true;
                }
            }
            else if (targetObjectL[0] instanceof A4Character) {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    var cause = Term.fromString("verb.have('" + targetObjectL[0].ID + "'[#id], '" + targetObjectL[1].ID + "'[#id])", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
            else {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
        }
        app.achievement_nlp_all_robot_actions[12] = true;
        app.trigger_achievement_complete_alert();
        // go to destination:
        var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
        var s = null;
        if (targetObjectL.length == 1) {
            s = new A4Script(A4_SCRIPT_TAKE, null, null, 0, false, false);
            s.x = destinationX;
            s.y = destinationY;
        }
        else {
            s = new A4Script(A4_SCRIPT_TAKE_FROM_CONTAINER, targetObjectL[0].ID, null, 0, false, false);
            s.ID2 = targetObjectL[1].ID; // the object we want to take
        }
        q.scripts.push(s);
        ai.setNewAction(intention, requester, q, null);
        ai.addLongTermTerm(new Term(intention.functor, [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
            new TermTermAttribute(intention)]), PERCEPTION_PROVENANCE);
        ai.intentionsCausedByRequest.push(ir);
        if (requester != null) {
            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
            var term = Term.fromString(tmp, ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        return true;
    };
    RobotTake_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotTake_IntentionAction\"/>";
    };
    RobotTake_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotTake_IntentionAction();
    };
    return RobotTake_IntentionAction;
}(IntentionAction));
