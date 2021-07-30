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
var RobotTurn_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotTurn_IntentionAction, _super);
    function RobotTurn_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotTurn_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.rotate")) ||
            intention.functor.is_a(ai.o.getSort("verb.face")))
            return true;
        return false;
    };
    RobotTurn_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        if (ai.robot.isInVehicle() ||
            intention.attributes.length == 0 ||
            !(intention.attributes[0] instanceof ConstantTermAttribute)) {
            // we should never get here:
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var destinationMap = null;
        var destinationX = 0;
        var destinationY = 0;
        if (intention.attributes.length == 2 &&
            (intention.attributes[1] instanceof VariableTermAttribute)) {
            var requesterID = requester.value;
            var targetObject = ai.game.findObjectByIDJustObject(requesterID);
            if (targetObject == null) {
                // we should never get here:
                if (requester != null) {
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
            if (intention.attributes[1].sort.is_a_string("direction")) {
                var movementAmount = 1;
                var targetx = null;
                var targety = null;
                if (intention.attributes[1].sort.name == "west" &&
                    ai.robot.direction != A4_DIRECTION_LEFT) {
                    targetx = ai.robot.x - movementAmount * 8;
                    targety = ai.robot.y;
                }
                if (intention.attributes[1].sort.name == "north" &&
                    ai.robot.direction != A4_DIRECTION_UP) {
                    targetx = ai.robot.x;
                    targety = ai.robot.y - movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "east" &&
                    ai.robot.direction != A4_DIRECTION_RIGHT) {
                    targetx = ai.robot.x + movementAmount * 8;
                    targety = ai.robot.y;
                }
                if (intention.attributes[1].sort.name == "south" &&
                    ai.robot.direction != A4_DIRECTION_DOWN) {
                    targetx = ai.robot.x;
                    targety = ai.robot.y + movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "direction.right") {
                    targetx = ai.robot.x + movementAmount * 8 * direction_x_inc[(ai.robot.direction + 1) % A4_NDIRECTIONS];
                    targety = ai.robot.y + movementAmount * 8 * direction_y_inc[(ai.robot.direction + 1) % A4_NDIRECTIONS];
                }
                if (intention.attributes[1].sort.name == "direction.left") {
                    var dir = ai.robot.direction - 1;
                    if (dir < 0)
                        dir += A4_NDIRECTIONS;
                    targetx = ai.robot.x + movementAmount * 8 * direction_x_inc[dir];
                    targety = ai.robot.y + movementAmount * 8 * direction_y_inc[dir];
                }
                if (intention.attributes[1].sort.name == "backward") {
                    targetx = ai.robot.x + movementAmount * 8 * direction_x_inc[(ai.robot.direction + 2) % A4_NDIRECTIONS];
                    targety = ai.robot.y + movementAmount * 8 * direction_y_inc[(ai.robot.direction + 2) % A4_NDIRECTIONS];
                }
                var x = Math.floor(ai.robot.x / 8) * 8;
                var y = Math.floor(ai.robot.y / 8) * 8;
                while (x != targetx || y != targety) {
                    if (x < targetx)
                        x += 8;
                    if (x > targetx)
                        x -= 8;
                    if (y < targety)
                        y += 8;
                    if (y > targety)
                        y -= 8;
                    if (ai.robot.map.walkableConsideringVehicles(x, y, ai.robot.getPixelWidth(), ai.robot.getPixelHeight(), ai.robot)) {
                        destinationMap = ai.robot.map;
                        destinationX = x;
                        destinationY = y;
                    }
                    else {
                        // obstacle!
                        var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #and(obstacle(X), space.at(X, [space.here]))))";
                        var term = Term.fromString(tmp, ai.o);
                        ai.queueIntention(term, requester, null);
                        break;
                    }
                }
            }
            else {
                // we should never get here:
                if (requester != null) {
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
        }
        else {
            // we should never get here:
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (destinationMap == null) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var cause = Term.fromString("#not(verb.know('" + ai.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", ai.o);
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        // Check if the robot can go:
        var destinationLocation = ai.game.getAILocationTileCoordinate(destinationMap, destinationX / destinationMap.tileWidth, destinationY / destinationMap.tileHeight);
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
        app.achievement_nlp_all_robot_actions[9] = true;
        app.trigger_achievement_complete_alert();
        // go to destination (this is "turn", so it should just be moving one step):
        var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
        var s = new A4Script(A4_SCRIPT_GOTO, ai.robot.map.name, null, 0, false, false);
        s.x = destinationX;
        s.y = destinationY;
        q.scripts.push(s);
        ai.setNewAction(intention, requester, q, null);
        ai.addCurrentActionLongTermTerm(intention);
        ai.intentionsCausedByRequest.push(ir);
        if (requester != null) {
            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
            var term = Term.fromString(tmp, ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        ir.succeeded = true;
        return true;
    };
    RobotTurn_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotTurn_IntentionAction\"/>";
    };
    RobotTurn_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotTurn_IntentionAction();
    };
    return RobotTurn_IntentionAction;
}(IntentionAction));
