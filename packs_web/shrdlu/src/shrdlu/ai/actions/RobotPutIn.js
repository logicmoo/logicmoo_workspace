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
var RobotPutIn_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotPutIn_IntentionAction, _super);
    function RobotPutIn_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotPutIn_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.put-in")))
            return true;
        return false;
    };
    RobotPutIn_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var requester = ir.requester;
        var alternative_actions = ir.alternative_actions;
        if (alternative_actions == null)
            alternative_actions = [ir.action];
        var denyrequestCause = null;
        var numberConstraint = ir.resolveNumberConstraint(ir.numberConstraint, alternative_actions.length);
        var itemID_l = [];
        var containerID_l = [];
        if (ai.robot.isInVehicle()) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            this.ir.succeeded = false;
            return true;
        }
        for (var _i = 0, alternative_actions_1 = alternative_actions; _i < alternative_actions_1.length; _i++) {
            var intention = alternative_actions_1[_i];
            if (intention.attributes.length == 3 &&
                (intention.attributes[0] instanceof ConstantTermAttribute) &&
                (intention.attributes[1] instanceof ConstantTermAttribute) &&
                (intention.attributes[2] instanceof ConstantTermAttribute)) {
                var id = (intention.attributes[1]).value;
                var id2 = (intention.attributes[2]).value;
                if (id != null && itemID_l.indexOf(id) == -1)
                    itemID_l.push(id);
                if (id2 != null && containerID_l.indexOf(id2) == -1)
                    containerID_l.push(id2);
            }
        }
        // We force only one possible destination:
        if (itemID_l.length == 0 || containerID_l.length != 1) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            this.ir.succeeded = false;
            return true;
        }
        var item_l = [];
        for (var _a = 0, itemID_l_1 = itemID_l; _a < itemID_l_1.length; _a++) {
            var id = itemID_l_1[_a];
            var item = ai.game.findObjectByID(id);
            if (item != null) {
                var itemLocation = ai.game.getAILocation(item[0]);
                var itemLocationID = null;
                if (itemLocation != null)
                    itemLocationID = itemLocation.id;
                var cannotGoCause_1 = ai.canGoTo(item[0].map, itemLocationID, requester);
                if (cannotGoCause_1 == null) {
                    item_l.push(item[item.length - 1]);
                }
                else {
                    denyrequestCause = cannotGoCause_1;
                }
            }
            else {
                denyrequestCause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + itemID_l[0] + "'[#id]))", ai.o);
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
                    var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + itemID_l[0] + "'[#id]))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                }
            }
            this.ir.succeeded = false;
            return true;
        }
        var containerObjectL = ai.game.findObjectByID(containerID_l[0]);
        if (containerObjectL == null) {
            // check if it's a location and redirect to drop:
            var targetLocation = ai.game.getAILocationByID(containerID_l[0]);
            if (targetLocation != null) {
                var term2 = alternative_actions[0].clone([]);
                term2.functor = ai.o.getSort("action.drop");
                var ir2 = new IntentionRecord(term2, null, null, null, ai.timeStamp);
                ir2.alternative_actions = [];
                for (var _b = 0, alternative_actions_2 = alternative_actions; _b < alternative_actions_2.length; _b++) {
                    var aa = alternative_actions_2[_b];
                    var aa2 = aa.clone([]);
                    aa2.functor = ai.o.getSort("action.drop");
                    ir2.alternative_actions.push(aa2);
                }
                ir2.numberConstraint = ir.numberConstraint;
                ai.intentions.push(ir2);
                this.ir.succeeded = true;
                return true;
            }
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + containerID_l[0] + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            this.ir.succeeded = false;
            return true;
        }
        var destinationMap = containerObjectL[0].map;
        // Check if the robot can go:
        var destinationLocation = ai.game.getAILocation(containerObjectL[0]);
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
            this.ir.succeeded = false;
            return true;
        }
        if (destinationMap == null) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.know('" + ai.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            this.ir.succeeded = false;
            return true;
        }
        if (containerObjectL[0] instanceof A4ObstacleContainer) {
            if (containerObjectL[0].closeable && containerObjectL[0].closed &&
                containerObjectL[0].doorID != null) {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    var cause = Term.fromString("property.closed('" + containerObjectL[0].ID + "'[#id])", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                }
                this.ir.succeeded = false;
                return true;
            }
        }
        else if (ai.selfID == "shrdlu" &&
            containerObjectL[0].ID == "garage-shuttle" &&
            itemID_l.length == 1 &&
            itemID_l[0] == "shuttle-engine" &&
            ai.game.gameScript.act_2_repair_shuttle_state == 0) {
            // special case of repairing the shuttle:
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            ai.game.gameScript.act_2_repair_shuttle_state = 1;
            ai.game.gameScript.act_2_repair_shuttle_state_timer = 0;
            this.ir.succeeded = true;
            return true;
        }
        else if (containerObjectL[0].ID == ai.selfID) {
            // put something into ourselves (weird, but, ok...):
        }
        else {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            this.ir.succeeded = false;
            return true;
        }
        app.achievement_nlp_all_robot_actions[4] = true;
        app.trigger_achievement_complete_alert();
        // go to destination:
        var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
        for (var _c = 0, item_l_1 = item_l; _c < item_l_1.length; _c++) {
            var item = item_l_1[_c];
            if (ai.robot.inventory.indexOf(item) == -1) {
                var item2 = ai.game.findObjectByID(item.ID);
                if (item2.length == 1) {
                    var s = new A4Script(A4_SCRIPT_TAKE, null, null, 0, false, false);
                    s.x = item2[0].x;
                    s.y = item2[0].y;
                    s.ID = item2[0].map.name;
                    q.scripts.push(s);
                }
                else {
                    var s = new A4Script(A4_SCRIPT_TAKE_FROM_CONTAINER, item.ID, null, 0, false, false);
                    s.ID2 = item.ID; // the object we want to take
                    q.scripts.push(s);
                }
            }
            if (containerObjectL[0].ID != ai.selfID) {
                var s = new A4Script(A4_SCRIPT_PUT_IN_CONTAINER, containerObjectL[0].ID, null, 0, false, false);
                s.ID2 = item.ID; // the object we want to put in
                q.scripts.push(s);
            }
            // If the object was not mentioned explicitly in the performative, add it to the natural language context:
            if (ir.requestingPerformative != null)
                ir.requestingPerformative.addMentionToPerformative(item.ID, ai.o);
            if (ir.requestingPerformative != null)
                ir.requestingPerformative.addMentionToPerformative(containerObjectL[0].ID, ai.o);
            numberConstraint--;
            if (numberConstraint <= 0)
                break;
        }
        if (q.scripts.length > 0) {
            ai.setNewAction(alternative_actions[0], requester, q, null);
            ai.addCurrentActionLongTermTerm(alternative_actions[0]);
        }
        ai.intentionsCausedByRequest.push(ir);
        if (requester != null) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        this.ir.succeeded = true;
        return true;
    };
    RobotPutIn_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotPutIn_IntentionAction\"/>";
    };
    RobotPutIn_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotPutIn_IntentionAction();
    };
    return RobotPutIn_IntentionAction;
}(IntentionAction));
