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
var ShrdluAI = /** @class */ (function (_super) {
    __extends(ShrdluAI, _super);
    function ShrdluAI(o, nlp, shrdlu, game, rulesFileNames) {
        var _this = _super.call(this, o, nlp, shrdlu, game, rulesFileNames) || this;
        console.log("ShrdluAI.constructor end...");
        _this.robot.ID = "shrdlu";
        _this.selfID = "shrdlu";
        _this.visionActive = false;
        _this.addLongTermTerm(Term.fromString("name('" + _this.robot.ID + "'[#id],'shrdlu'[symbol])", _this.o), BACKGROUND_PROVENANCE);
        _this.objectsNotAllowedToGive.push("master-key2");
        console.log("ShrdluAI.constructor end...");
        return _this;
    }
    ShrdluAI.prototype.canSatisfyActionRequest = function (ir) {
        var actionRequest = ir.action;
        var repairSort = this.o.getSort("verb.repair");
        if (actionRequest.functor.is_a(repairSort) && actionRequest.attributes.length >= 2) {
            var thingToRepair = actionRequest.attributes[1];
            if (thingToRepair instanceof ConstantTermAttribute) {
                var thingToRepair_id = thingToRepair.value;
                if (thingToRepair_id == "garage-shuttle") {
                    var thingToRepairObject = this.game.findObjectByIDJustObject(thingToRepair_id);
                    if (thingToRepairObject.sort.name == "brokenshuttle") {
                        // broken shuttle:
                        app.achievement_nlp_all_robot_actions[11] = true;
                        app.trigger_achievement_complete_alert();
                        return ACTION_REQUEST_CAN_BE_SATISFIED;
                    }
                }
                else if (thingToRepair_id == "tardis-wall-computer") {
                    app.achievement_nlp_all_robot_actions[11] = true;
                    app.trigger_achievement_complete_alert();
                    return ACTION_REQUEST_CAN_BE_SATISFIED;
                }
                else if (thingToRepair_id == "tardis-broken-cable") {
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
    ShrdluAI.prototype.executeIntention = function (ir) {
        var intention = ir.action;
        var repairSort = this.o.getSort("verb.repair");
        if (intention.functor.is_a(repairSort)) {
            // just ignore, the story script will take charge of making shrdlu do the repair...
            this.clearCurrentAction();
            return true;
        }
        return _super.prototype.executeIntention.call(this, ir);
    };
    /*
    - If it returns "null", it means the robot can go
    - If it returns a Term, it means the robot cannot go, for the reason specified in the Term (e.g., not allowed)
    */
    ShrdluAI.prototype.canGoTo = function (map, locationID, requester) {
        if ((this.robot.map.name == "Aurora Station" || this.robot.map.name == "Aurora Station Outdoors") &&
            map.name != "Aurora Station" &&
            map.name != "Aurora Station Outdoors") {
            if (this.game.getStoryStateVariable("permission-to-take-shrdlu") == "false") {
                if ((requester instanceof ConstantTermAttribute) &&
                    requester.value == "etaoin") {
                    this.game.setStoryStateVariable("permission-to-take-shrdlu", "true");
                    return null;
                }
                else {
                    var cause = Term.fromString("verb.need('" + this.selfID + "'[#id], #and(X:[permission-to], relation.origin(X, 'etaoin'[#id])))", this.o);
                    return cause;
                }
            }
        }
        return _super.prototype.canGoTo.call(this, map, locationID, requester);
    };
    ShrdluAI.prototype.considerGoals = function () {
        // Shrdlu only has goals that are within the station:
        if (this.robot.map.name != "Aurora Station" && this.robot.map.name != "Aurora Station Outdoors")
            return false;
        return _super.prototype.considerGoals.call(this);
    };
    return ShrdluAI;
}(RobotAI));
