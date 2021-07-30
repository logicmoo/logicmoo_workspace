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
var QwertyAI = /** @class */ (function (_super) {
    __extends(QwertyAI, _super);
    function QwertyAI(o, nlp, qwerty, game, rulesFileNames) {
        var _this = _super.call(this, o, nlp, qwerty, game, rulesFileNames) || this;
        console.log("QwertyAI.constructor end...");
        _this.robot.ID = "qwerty";
        _this.selfID = "qwerty";
        _this.addLongTermTerm(Term.fromString("name('" + _this.robot.ID + "'[#id],'qwerty'[symbol])", _this.o), BACKGROUND_PROVENANCE);
        _this.objectsNotAllowedToGive.push("garage-key");
        _this.objectsNotAllowedToGive.push("master-key1");
        // this.objectsNotAllowedToGive.push("science-key");
        _this.objectsNotAllowedToGive.push("command-key");
        _this.objectsNotAllowedToGive.push("stasis-key");
        console.log("QwertyAI.constructor end...");
        return _this;
    }
    QwertyAI.prototype.canSatisfyActionRequest = function (ir) {
        var actionRequest = ir.action;
        var repairSort = this.o.getSort("verb.repair");
        if (actionRequest.functor.is_a(repairSort) && actionRequest.attributes.length >= 2) {
            var thingToRepair = actionRequest.attributes[1];
            if (thingToRepair instanceof ConstantTermAttribute) {
                var thingToRepair_id = thingToRepair.value;
                if (thingToRepair_id == "spacesuit") {
                    var thingToRepairObject = this.game.findObjectByIDJustObject(thingToRepair_id);
                    if (thingToRepairObject.sort.name == "brokenspacesuit") {
                        // broken space suit:
                        app.achievement_nlp_all_robot_actions[11] = true;
                        app.trigger_achievement_complete_alert();
                        return ACTION_REQUEST_CAN_BE_SATISFIED;
                    }
                }
                else if (thingToRepair_id == "shuttle-datapad") {
                    app.achievement_nlp_all_robot_actions[11] = true;
                    app.trigger_achievement_complete_alert();
                    return ACTION_REQUEST_CAN_BE_SATISFIED;
                }
            }
            else {
                return ACTION_REQUEST_CANNOT_BE_SATISFIED;
            }
        }
        return _super.prototype.canSatisfyActionRequest.call(this, ir);
    };
    QwertyAI.prototype.executeIntention = function (ir) {
        var intention = ir.action;
        var repairSort = this.o.getSort("verb.repair");
        if (intention.functor.is_a(repairSort)) {
            // just ignore, the story script will take charge of making qwerty do the repair...
            this.clearCurrentAction();
            return true;
        }
        return _super.prototype.executeIntention.call(this, ir);
    };
    /*
    - If it returns "null", it means the robot can go
    - If it returns a Term, it means the robot cannot go, for the reason specified in the Term (e.g., not allowed)
    */
    QwertyAI.prototype.canGoTo = function (map, locationID, requester) {
        if (map != this.robot.map) {
            var cause = Term.fromString("#not(verb.can(ME:'" + this.selfID + "'[#id], verb.go(ME, [space.outside])))", this.o);
            return cause;
        }
        return _super.prototype.canGoTo.call(this, map, locationID, requester);
    };
    return QwertyAI;
}(RobotAI));
