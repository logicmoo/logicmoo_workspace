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
var RobotDrop_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotDrop_IntentionAction, _super);
    function RobotDrop_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotDrop_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.drop")))
            return true;
        return false;
    };
    RobotDrop_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var requester = ir.requester;
        var alternative_actions = ir.alternative_actions;
        if (alternative_actions == null)
            alternative_actions = [ir.action];
        var denyrequestCause = null;
        var numberConstraint = ir.resolveNumberConstraint(ir.numberConstraint, alternative_actions.length);
        var itemID_l = [];
        var locationID_l = [];
        if (ai.robot.isInVehicle()) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        for (var _i = 0, alternative_actions_1 = alternative_actions; _i < alternative_actions_1.length; _i++) {
            var intention = alternative_actions_1[_i];
            if (intention.attributes.length == 2 &&
                (intention.attributes[0] instanceof ConstantTermAttribute) &&
                (intention.attributes[1] instanceof ConstantTermAttribute)) {
                var id = (intention.attributes[1]).value;
                if (id != null && itemID_l.indexOf(id) == -1)
                    itemID_l.push(id);
            }
            if (intention.attributes.length == 3 &&
                (intention.attributes[0] instanceof ConstantTermAttribute) &&
                (intention.attributes[1] instanceof ConstantTermAttribute) &&
                (intention.attributes[2] instanceof ConstantTermAttribute)) {
                var id = (intention.attributes[1]).value;
                var id2 = (intention.attributes[2]).value;
                if (id != null && itemID_l.indexOf(id) == -1)
                    itemID_l.push(id);
                if (id2 != null && locationID_l.indexOf(id2) == -1)
                    locationID_l.push(id2);
            }
            if (intention.attributes.length == 3 &&
                (intention.attributes[0] instanceof ConstantTermAttribute) &&
                (intention.attributes[1] instanceof ConstantTermAttribute) &&
                (intention.attributes[2] instanceof VariableTermAttribute) &&
                intention.attributes[2].sort.is_a(ai.o.getSort("space.here"))) {
                var id = (intention.attributes[1]).value;
                if (id != null && itemID_l.indexOf(id) == -1)
                    itemID_l.push(id);
            }
        }
        if (itemID_l.length == 0 || locationID_l.length > 1) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var item_l = [];
        for (var _a = 0, _b = ai.robot.inventory; _a < _b.length; _a++) {
            var o = _b[_a];
            if (itemID_l.indexOf(o.ID) != -1) {
                if (ai.objectsNotAllowedToGive.indexOf(o.ID) == -1) {
                    item_l.push(o);
                }
                else {
                    denyrequestCause = Term.fromString("#not(verb.can('" + ai.selfID + "'[#id], action.give('" + ai.selfID + "'[#id], '" + o.ID + "'[#id], " + requester + ")))", ai.o);
                }
            }
        }
        if (item_l.length == 0) {
            if (requester != null) {
                if (denyrequestCause != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(denyrequestCause, null, ai.timeStamp), ai.timeStamp));
                }
                else {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    var cause = Term.fromString("#not(verb.have('" + ai.selfID + "'[#id], '" + itemID_l[0] + "'[#id]))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                }
            }
            ir.succeeded = false;
            return true;
        }
        if (locationID_l.length == 0) {
            // just drop the objects:
            var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
            for (var _c = 0, item_l_1 = item_l; _c < item_l_1.length; _c++) {
                var item = item_l_1[_c];
                var s = new A4Script(A4_SCRIPT_DROP, item.ID, null, 0, false, false);
                q.scripts.push(s);
                ai.addCurrentActionLongTermTerm(alternative_actions[0]);
                // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                if (ir.requestingPerformative != null)
                    ir.requestingPerformative.addMentionToPerformative(item.ID, ai.o);
                numberConstraint--;
                if (numberConstraint <= 0)
                    break;
            }
            ai.setNewAction(alternative_actions[0], requester, q, null);
            ai.intentionsCausedByRequest.push(ir);
            app.achievement_nlp_all_robot_actions[5] = true;
            app.trigger_achievement_complete_alert();
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = true;
        }
        else {
            var targetLocation = ai.game.getAILocationByID(locationID_l[0]);
            var destinationMap = null;
            var destinationX = void 0;
            var destinationY = void 0;
            if (targetLocation == null) {
                // check if it's an object, and redirect to put-in!
                var containerObjectL = ai.game.findObjectByID(locationID_l[0]);
                if (containerObjectL != null && containerObjectL.length > 0) {
                    var term2 = alternative_actions[0].clone([]);
                    term2.functor = ai.o.getSort("action.put-in");
                    var ir2 = new IntentionRecord(term2, null, null, null, ai.timeStamp);
                    ir2.alternative_actions = [];
                    for (var _d = 0, _e = ir.alternative_actions; _d < _e.length; _d++) {
                        var aa = _e[_d];
                        var aa2 = aa.clone([]);
                        aa2.functor = ai.o.getSort("action.put-in");
                        ir2.alternative_actions.push(aa2);
                    }
                    ir2.numberConstraint = ir.numberConstraint;
                    ai.intentions.push(ir2);
                    ir.succeeded = true;
                    return true;
                }
            }
            else {
                var tmp2 = targetLocation.centerWalkableCoordinatesInMap(ai.robot.map, ai.robot);
                if (tmp2 != null) {
                    destinationMap = ai.robot.map;
                    destinationX = tmp2[0];
                    destinationY = tmp2[1];
                }
                else {
                    if (targetLocation.maps.length > 0 &&
                        targetLocation.maps.indexOf(ai.robot.map) == -1) {
                        // we set this so that we can later give the proper reason for why we cannot go
                        destinationMap = targetLocation.maps[0];
                    }
                }
            }
            // Check if the robot can go:
            var cannotGoCause = ai.canGoTo(destinationMap, locationID_l[0], requester);
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
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                    var cause = Term.fromString("#not(verb.know('" + ai.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", ai.o);
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
            app.achievement_nlp_all_robot_actions[5] = true;
            app.trigger_achievement_complete_alert();
            // go to destination:
            var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
            var s = null;
            s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, destinationMap.name, null, 0, false, false);
            s.x = destinationX;
            s.y = destinationY;
            q.scripts.push(s);
            for (var _f = 0, item_l_2 = item_l; _f < item_l_2.length; _f++) {
                var item = item_l_2[_f];
                s = new A4Script(A4_SCRIPT_DROP, item.ID, null, 0, false, false);
                q.scripts.push(s);
                // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                if (ir.requestingPerformative != null)
                    ir.requestingPerformative.addMentionToPerformative(item.ID, ai.o);
                numberConstraint--;
                if (numberConstraint <= 0)
                    break;
            }
            ai.setNewAction(alternative_actions[0], requester, q, null);
            ai.addCurrentActionLongTermTerm(alternative_actions[0]);
            ai.intentionsCausedByRequest.push(ir);
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
        }
        ir.succeeded = true;
        return true;
    };
    RobotDrop_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotDrop_IntentionAction\"/>";
    };
    RobotDrop_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotDrop_IntentionAction();
    };
    return RobotDrop_IntentionAction;
}(IntentionAction));
