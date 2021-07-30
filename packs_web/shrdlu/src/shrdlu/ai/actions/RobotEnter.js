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
var RobotEnter_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotEnter_IntentionAction, _super);
    function RobotEnter_IntentionAction() {
        var _this = _super.call(this) || this;
        _this.targetObject = null;
        _this.needsContinuousExecution = true;
        return _this;
    }
    RobotEnter_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.enter"))) {
            if (intention.attributes.length >= 2 &&
                (intention.attributes[1] instanceof ConstantTermAttribute)) {
                var id = intention.attributes[1].value;
                var targetObject = ai.game.findObjectByIDJustObject(id);
                if (targetObject != null &&
                    targetObject.map == ai.robot.map) {
                    return true;
                }
            }
        }
        return false;
    };
    RobotEnter_IntentionAction.prototype.execute = function (ir, ai_raw) {
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
        if (intention.attributes.length == 0 ||
            !(intention.attributes[0] instanceof ConstantTermAttribute)) {
            // we should never get here:
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (intention.attributes.length >= 2 &&
            (intention.attributes[1] instanceof ConstantTermAttribute)) {
            var id = intention.attributes[1].value;
            this.targetObject = ai.game.findObjectByIDJustObject(id);
        }
        if (this.targetObject == null ||
            this.targetObject.map != ai.robot.map) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        // Check if the robot can see it:
        if (!ai.visionActive) {
            var x1 = this.targetObject.x + this.targetObject.getPixelWidth() / 2;
            var y1 = this.targetObject.y + this.targetObject.getPixelHeight() / 2;
            var x2 = ai.robot.x + ai.robot.getPixelWidth() / 2;
            var y2 = ai.robot.y + ai.robot.getPixelHeight() / 2;
            var d_1 = Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
            if (d_1 > 28) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.request.action(" + requester + ", verb.take-to(" + requester + ", '" + ai.selfID + "'[#id], '" + this.targetObject.ID + "'[#id])))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                ir.succeeded = false;
                return true;
            }
        }
        // Check if the robot can go:
        var destinationMap = this.targetObject.map;
        var destinationLocation = ai.game.getAILocation(this.targetObject);
        var destinationLocationID = null;
        if (destinationLocation != null)
            destinationLocationID = destinationLocation.id;
        if (ai.robot.map.name == "Aurora Station" &&
            (this.targetObject instanceof A4Vehicle)) {
            // assume we are going to go out:
            destinationMap = ai.game.getMap("Spacer Valley South");
            destinationLocationID = "spacer-valley-south";
        }
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
        if (requester != null) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        ai.intentionsCausedByRequest.push(ir);
        ai.setNewAction(intention, requester, null, this);
        ai.addCurrentActionLongTermTerm(intention);
        ir.succeeded = true;
        return true;
    };
    RobotEnter_IntentionAction.prototype.executeContinuous = function (ai_raw) {
        var ai = ai_raw;
        var destinationMap = this.targetObject.map;
        // if the targt is outside the map, we just wait
        if (destinationMap == null || destinationMap != ai.robot.map)
            return false;
        var distance = ai.robot.pixelDistance(this.targetObject);
        if (distance == 0) {
            // we made it!
            if (this.targetObject instanceof A4Vehicle) {
                ai.robot.embark(this.targetObject);
            }
            else {
                if (ai.currentAction_requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + ai.currentAction_requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
            }
            return true;
        }
        else {
            // go to destination:
            var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
            var s = new A4Script(A4_SCRIPT_GOTO_CHARACTER, this.targetObject.ID, null, 0, false, false);
            q.scripts.push(s);
            ai.currentAction_scriptQueue = q;
        }
        return false;
    };
    RobotEnter_IntentionAction.prototype.saveToXML = function (ai) {
        var str = "<IntentionAction type=\"RobotEnter_IntentionAction\"";
        if (this.targetObject == null) {
            return str + "/>";
        }
        else {
            return str + " targetObject=\"" + this.targetObject.ID + "\"/>";
        }
    };
    RobotEnter_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new RobotEnter_IntentionAction();
        if (xml.getAttribute("targetObject") != null) {
            var game = ai.game;
            var o = game.findObjectByIDJustObject(xml.getAttribute("targetObject"));
            a.targetObject = o;
        }
        return a;
    };
    return RobotEnter_IntentionAction;
}(IntentionAction));
