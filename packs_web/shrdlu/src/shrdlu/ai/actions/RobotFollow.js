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
var RobotFollow_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotFollow_IntentionAction, _super);
    function RobotFollow_IntentionAction() {
        var _this = _super.call(this) || this;
        _this.targetObject = null;
        _this.needsContinuousExecution = true;
        return _this;
    }
    RobotFollow_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.follow")))
            return true;
        return false;
    };
    RobotFollow_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        /*
        if (ai.robot.isInVehicle()) {
            if (requester != null) {
                let term:Term = Term.fromString("action.talk('"+ai.selfID+"'[#id], perf.ack.denyrequest("+requester+"))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            return true;
        }
        */
        if (!ai.visionActive) {
            if (requester != null) {
                var term1 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("property.blind('" + ai.selfID + "'[#id])", ai.o);
                var term2 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", property.blind('" + ai.selfID + "'[#id])))", ai.o);
                ai.intentions.push(new IntentionRecord(term1, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                ai.intentions.push(new IntentionRecord(term2, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (intention.attributes.length == 0 ||
            !(intention.attributes[0] instanceof ConstantTermAttribute)) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var targetID = (intention.attributes[1]).value;
        this.targetObject = ai.game.findObjectByIDJustObject(targetID);
        if (this.targetObject == null) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + targetID + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var destinationMap = this.targetObject.map;
        if (destinationMap == null || destinationMap != ai.robot.map) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.know('" + ai.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (requester != null) {
            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
            var term = Term.fromString(tmp, ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        app.achievement_nlp_all_robot_actions[0] = true;
        app.trigger_achievement_complete_alert();
        ai.intentionsCausedByRequest.push(ir);
        ai.setNewAction(intention, requester, null, this);
        ai.addCurrentActionLongTermTerm(intention);
        this.executeContinuous(ai);
        ir.succeeded = true;
        return true;
    };
    RobotFollow_IntentionAction.prototype.executeContinuous = function (ai_raw) {
        var ai = ai_raw;
        // Check if we need to leave a vehicle:
        if (ai.robot.isInVehicle()) {
            if ((this.targetObject instanceof A4Character)) {
                var target_c = this.targetObject;
                if (target_c.isInVehicle() &&
                    ai.robot.vehicle == target_c.vehicle) {
                    return false;
                }
                else {
                    ai.robot.disembark();
                }
            }
            else {
                ai.robot.disembark();
            }
            return false;
        }
        var destinationX = this.targetObject.x;
        var destinationY = this.targetObject.y;
        // go to destination:
        var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
        var s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, this.targetObject.map.name, null, 0, false, false);
        s.x = destinationX;
        s.y = destinationY;
        q.scripts.push(s);
        ai.currentAction_scriptQueue = q;
        return false;
    };
    RobotFollow_IntentionAction.prototype.saveToXML = function (ai) {
        var str = "<IntentionAction type=\"RobotFollow_IntentionAction\"";
        if (this.targetObject == null) {
            return str + "/>";
        }
        else {
            return str + " targetObject=\"" + this.targetObject.ID + "\"/>";
        }
    };
    RobotFollow_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new RobotFollow_IntentionAction();
        if (xml.getAttribute("targetObject") != null) {
            var game = ai.game;
            var o = game.findObjectByIDJustObject(xml.getAttribute("targetObject"));
            a.targetObject = o;
        }
        return a;
    };
    return RobotFollow_IntentionAction;
}(IntentionAction));
