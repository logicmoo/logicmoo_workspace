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
var RobotPushPull_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotPushPull_IntentionAction, _super);
    function RobotPushPull_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotPushPull_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.push")) ||
            intention.functor.is_a(ai.o.getSort("action.pull")) ||
            intention.functor.is_a(ai.o.getSort("verb.move")))
            return true;
        return false;
    };
    RobotPushPull_IntentionAction.prototype.execute = function (ir, ai_raw) {
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
        if (intention.attributes.length < 1 ||
            !(intention.attributes[0] instanceof ConstantTermAttribute)) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var targetObject = null;
        var adverbSort = null;
        var numberConstant = null;
        var targetDirection = -1;
        if (intention.attributes.length >= 2) {
            if ((intention.attributes[1] instanceof ConstantTermAttribute)) {
                var targetID = (intention.attributes[1]).value;
                var targetObjectL = ai.game.findObjectByID(targetID);
                if (targetObjectL == null) {
                    if (requester != null) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                        var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + targetID + "'[#id]))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                    }
                    ir.succeeded = false;
                    return true;
                }
                if (targetObjectL.length != 1) {
                    if (requester != null) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
                    ir.succeeded = false;
                    return true;
                }
                targetObject = targetObjectL[0];
            }
            else if ((intention.attributes[1] instanceof VariableTermAttribute)) {
                adverbSort = intention.attributes[1].sort;
            }
            else if ((intention.attributes[1] instanceof ConstantTermAttribute) &&
                intention.attributes[1].sort.is_a_string("number")) {
                numberConstant = Number((intention.attributes[1]).value);
            }
        }
        if (adverbSort == null &&
            intention.attributes.length >= 3 &&
            (intention.attributes[2] instanceof VariableTermAttribute)) {
            adverbSort = intention.attributes[2].sort;
        }
        else if (intention.attributes.length >= 3 &&
            (intention.attributes[2] instanceof ConstantTermAttribute) &&
            intention.attributes[2].sort.is_a_string("number")) {
            numberConstant = Number((intention.attributes[2]).value);
        }
        if (intention.attributes.length >= 4 &&
            (intention.attributes[3] instanceof ConstantTermAttribute) &&
            intention.attributes[3].sort.is_a_string("number")) {
            numberConstant = Number((intention.attributes[3]).value);
        }
        if (adverbSort != null) {
            if (adverbSort.name == "north")
                targetDirection = A4_DIRECTION_UP;
            if (adverbSort.name == "east")
                targetDirection = A4_DIRECTION_RIGHT;
            if (adverbSort.name == "south")
                targetDirection = A4_DIRECTION_DOWN;
            if (adverbSort.name == "west")
                targetDirection = A4_DIRECTION_LEFT;
            if (adverbSort.name == "forward")
                targetDirection = ai.robot.direction;
            if (adverbSort.name == "backward")
                targetDirection = (ai.robot.direction + 2) % A4_NDIRECTIONS;
            if (adverbSort.name == "direction.right")
                targetDirection = (ai.robot.direction + 1) % A4_NDIRECTIONS;
            if (adverbSort.name == "direction.left")
                targetDirection = (ai.robot.direction + 3) % A4_NDIRECTIONS;
        }
        if (targetObject == null) {
            // check if there is an object next to the robot that can be pushed 
            // (preferring one that is in the direction of the push, and otherwise, the one in front):
            var bestDirection = -1;
            for (var direction = 0; direction < 4; direction++) {
                var collisions = ai.robot.map.getAllObjectCollisionsWithOffset(ai.robot, direction_x_inc[direction], direction_y_inc[direction]);
                for (var _i = 0, collisions_1 = collisions; _i < collisions_1.length; _i++) {
                    var o = collisions_1[_i];
                    if (o.isPushable()) {
                        if (targetObject == null) {
                            targetObject = o;
                            bestDirection = direction;
                        }
                        else {
                            if (direction == targetDirection) {
                                // overwrite:
                                targetObject = o;
                                bestDirection = direction;
                            }
                            else if (direction == ai.robot.direction &&
                                bestDirection != targetDirection) {
                                // overwrite:
                                targetObject = o;
                                bestDirection = direction;
                            }
                        }
                        break;
                    }
                }
            }
        }
        if (targetObject == null) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var destinationMap = targetObject.map;
        // Check if the robot can go:
        var destinationLocation = ai.game.getAILocation(targetObject);
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
        if (destinationMap == null || destinationMap != ai.robot.map) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.know('" + ai.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        app.achievement_nlp_all_robot_actions[10] = true;
        app.trigger_achievement_complete_alert();
        // perform the action:
        var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
        if (numberConstant == null)
            numberConstant = 1;
        for (var i = 0; i < numberConstant; i++) {
            var s = null;
            if (intention.functor.is_a(ai.o.getSort("action.pull"))) {
                s = new A4Script(A4_SCRIPT_PULL, targetObject.ID, null, targetDirection, false, false);
            }
            else {
                s = new A4Script(A4_SCRIPT_PUSH, targetObject.ID, null, targetDirection, false, false);
            }
            q.scripts.push(s);
        }
        ai.setNewAction(intention, requester, q, this);
        ai.addLongTermTerm(new Term(intention.functor, [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
            new TermTermAttribute(intention)]), PERCEPTION_PROVENANCE);
        ai.intentionsCausedByRequest.push(ir);
        if (requester != null) {
            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
            var term = Term.fromString(tmp, ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        ir.succeeded = true;
        return true;
    };
    RobotPushPull_IntentionAction.prototype.actionScriptsFailed = function (ai, requester) {
        var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #not(verb.can('" + ai.selfID + "'[#id], verb.move('" + ai.selfID + "'[#id], object-personal-pronoun('object-personal-pronoun.it'[symbol], [singular], [gender-neutral], [third-person]))))))";
        var term = Term.fromString(tmp, ai.o);
        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
    };
    RobotPushPull_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotPushPull_IntentionAction\"/>";
    };
    RobotPushPull_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotPushPull_IntentionAction();
    };
    return RobotPushPull_IntentionAction;
}(IntentionAction));
