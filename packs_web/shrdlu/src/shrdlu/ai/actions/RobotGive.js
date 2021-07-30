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
var RobotGive_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotGive_IntentionAction, _super);
    function RobotGive_IntentionAction() {
        var _this = _super.call(this) || this;
        _this.targetCharacter = null;
        _this.itemsToGive = [];
        _this.needsContinuousExecution = true;
        return _this;
    }
    RobotGive_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.give")) &&
            intention.attributes.length >= 2)
            return true;
        return false;
    };
    RobotGive_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        var alternative_actions = ir.alternative_actions;
        if (alternative_actions == null)
            alternative_actions = [ir.action];
        var denyrequestCause = null;
        var numberConstraint = ir.resolveNumberConstraint(ir.numberConstraint, alternative_actions.length);
        var itemID_l = [];
        var targetID_l = [];
        if (ai.robot.isInVehicle()) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            this.ir.succeeded = false;
            return true;
        }
        for (var _i = 0, alternative_actions_1 = alternative_actions; _i < alternative_actions_1.length; _i++) {
            var intention_1 = alternative_actions_1[_i];
            var id = (intention_1.attributes[1]).value;
            if (id != null && itemID_l.indexOf(id) == -1)
                itemID_l.push(id);
            if (intention_1.attributes.length >= 3) {
                id = (intention_1.attributes[2]).value;
                if (id != null && targetID_l.indexOf(id) == -1)
                    targetID_l.push(id);
            }
        }
        if (targetID_l.length == 0) {
            if (requester != null && (requester instanceof ConstantTermAttribute)) {
                targetID_l.push(requester.value);
            }
        }
        if (targetID_l.length != 1) {
            if (requester != null) {
                // state that it cannot give this item:
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest('" + requester + "'[#id]))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            this.ir.succeeded = false;
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
        if (item_l.length == 0 && denyrequestCause == null) {
            for (var _c = 0, itemID_l_1 = itemID_l; _c < itemID_l_1.length; _c++) {
                var targetID = itemID_l_1[_c];
                var targetObjectL = ai.game.findObjectByID(targetID);
                //if (targetObject == null) {
                if (targetObjectL != null && targetObjectL.length == 1 &&
                    targetObjectL[0].takeable) {
                    item_l.push(targetObjectL[0]);
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
            this.ir.succeeded = false;
            return true;
        }
        var targetCharacter = ai.game.findObjectByIDJustObject(targetID_l[0]);
        if (targetCharacter == null) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], '" + targetID_l[0] + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
            return false;
        }
        else {
            var destinationMap = targetCharacter.map;
            // Check if the robot can go:
            var destinationLocation = ai.game.getAILocation(targetCharacter);
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
                return false;
            }
            else {
                if (requester != null) {
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                app.achievement_nlp_all_robot_actions[6] = true;
                app.trigger_achievement_complete_alert();
                ai.setNewAction(intention, requester, null, this);
                ai.addCurrentActionLongTermTerm(intention);
                ai.intentionsCausedByRequest.push(ir);
                for (var _d = 0, item_l_1 = item_l; _d < item_l_1.length; _d++) {
                    var item = item_l_1[_d];
                    this.itemsToGive.push(item);
                    numberConstraint--;
                    if (numberConstraint <= 0)
                        break;
                }
                this.targetCharacter = targetCharacter;
                for (var _e = 0, item_l_2 = item_l; _e < item_l_2.length; _e++) {
                    var item = item_l_2[_e];
                    // If the object was not mentioned explicitly in the performative, add it to the natural language context:
                    if (ir.requestingPerformative != null)
                        ir.requestingPerformative.addMentionToPerformative(item.ID, ai.o);
                    if (ir.requestingPerformative != null)
                        ir.requestingPerformative.addMentionToPerformative(targetCharacter.ID, ai.o);
                }
            }
        }
        this.ir.succeeded = true;
        return true;
    };
    RobotGive_IntentionAction.prototype.executeContinuous = function (ai_raw) {
        var ai = ai_raw;
        if (this.itemsToGive.length == 0 || this.targetCharacter == null) {
            // we are done!
            return true;
        }
        else {
            var itemToGive = this.itemsToGive[0];
            if (this.targetCharacter.inventory.indexOf(itemToGive) != -1) {
                // the target already has it, we are done with this item
                this.itemsToGive.splice(0, 1);
                return false;
            }
            if (ai.robot.inventory.indexOf(itemToGive) != -1) {
                // we have it, so, we just need to give it to the target:
                var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, this.targetCharacter);
                if (ai.robot.map != this.targetCharacter.map ||
                    ai.robot.pixelDistance(this.targetCharacter) != 0) {
                    q.scripts.push(new A4Script(A4_SCRIPT_GOTO_CHARACTER, this.targetCharacter.ID, null, 0, false, false));
                }
                else {
                    q.scripts.push(new A4Script(A4_SCRIPT_GIVE, itemToGive.ID, null, 0, false, false));
                }
                ai.currentAction_scriptQueue = q;
            }
            else {
                // we don't have the object, we need to go pick it up:
                var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
                var s = new A4Script(A4_SCRIPT_TAKE, itemToGive.map.name, null, 0, false, false);
                s.x = itemToGive.x;
                s.y = itemToGive.y;
                q.scripts.push(s);
                ai.currentAction_scriptQueue = q;
            }
        }
        return false;
    };
    RobotGive_IntentionAction.prototype.saveToXML = function (ai) {
        var str = "<IntentionAction type=\"RobotGive_IntentionAction\"";
        if (this.targetCharacter == null) {
            str += ">";
        }
        else {
            str += " targetCharacter=\"" + this.targetCharacter.ID + "\">";
        }
        for (var _i = 0, _a = this.itemsToGive; _i < _a.length; _i++) {
            var item = _a[_i];
            str += "<itemToGive id=\"" + item.ID + "\"/>";
        }
        return str += "</IntentionAction>";
    };
    RobotGive_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new RobotGive_IntentionAction();
        var game = ai.game;
        if (xml.getAttribute("targetCharacter") != null) {
            var o = game.findObjectByIDJustObject(xml.getAttribute("targetCharacter"));
            if (o instanceof A4Character) {
                a.targetCharacter = o;
            }
        }
        for (var _i = 0, _a = getElementChildrenByTag(xml, "itemToGive"); _i < _a.length; _i++) {
            var item_xml = _a[_i];
            if (item_xml.getAttribute("id") != null) {
                var o = game.findObjectByIDJustObject(item_xml.getAttribute("id"));
                if (o != null)
                    a.itemsToGive.push(o);
            }
        }
        return a;
    };
    return RobotGive_IntentionAction;
}(IntentionAction));
