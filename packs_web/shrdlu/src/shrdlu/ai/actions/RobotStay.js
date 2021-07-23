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
var RobotStay_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotStay_IntentionAction, _super);
    function RobotStay_IntentionAction() {
        var _this = _super.call(this) || this;
        _this.stayUntil = null;
        _this.needsContinuousExecution = true;
        return _this;
    }
    RobotStay_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.stay")) &&
            (intention.attributes.length == 1 ||
                intention.attributes.length == 5 ||
                (intention.attributes.length == 2 &&
                    ((intention.attributes[1] instanceof VariableTermAttribute) ||
                        (intention.attributes[1] instanceof ConstantTermAttribute))))) {
            return true;
        }
        return false;
    };
    RobotStay_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        var requesterID = null;
        if (requester instanceof ConstantTermAttribute) {
            requesterID = requester.value;
        }
        var targetID = null;
        var targetLocation = null;
        var targetObject = null;
        var targetMap = null;
        this.targetx = null;
        this.targety = null;
        this.targetMapName = null;
        // find the target destination:
        if ((intention.attributes.length == 1 ||
            ((intention.attributes[1] instanceof VariableTermAttribute) &&
                intention.attributes[1].sort.is_a(ai.o.getSort("space.here")))) &&
            requester != null &&
            requester instanceof ConstantTermAttribute) {
            var targetObject_1 = ai.game.findObjectByIDJustObject(requesterID);
            if (targetObject_1 != null) {
                var requesterLocation = ai.game.getAILocation(targetObject_1);
                var robotLocation = ai.game.getAILocation(ai.robot);
                if (robotLocation == requesterLocation) {
                    targetLocation = requesterLocation;
                    targetMap = ai.robot.map;
                    targetID = targetLocation.id;
                    this.targetx = ai.robot.x;
                    this.targety = ai.robot.y;
                }
                else if (ai.visionActive) {
                    targetLocation = requesterLocation;
                    targetMap = targetObject_1.map;
                    targetID = targetLocation.id;
                    this.targetx = targetObject_1.x;
                    this.targety = targetObject_1.y;
                }
                else {
                    if (requester != null) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                        term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #not(verb.can('" + ai.selfID + "'[#id], verb.see('" + ai.selfID + "'[#id], " + requester + ")))))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
                    ir.succeeded = false;
                    return true;
                }
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
        else if (intention.attributes.length == 2 &&
            intention.attributes[1] instanceof ConstantTermAttribute) {
            targetID = (intention.attributes[1]).value;
            targetObject = ai.game.findObjectByIDJustObject(targetID);
            if (ai.visionActive) {
                // destination is the second attribute:
                if (targetObject != null) {
                    targetMap = targetObject.map;
                    this.targetx = targetObject.x;
                    this.targety = targetObject.y;
                    targetLocation = ai.game.getAILocation(targetObject);
                }
                else {
                    var targetLocation_1 = ai.game.getAILocationByID(targetID);
                    if (targetLocation_1 != null) {
                        var tmp2 = targetLocation_1.centerWalkableCoordinatesInMap(ai.robot.map, ai.robot);
                        if (tmp2 != null) {
                            targetMap = ai.robot.map;
                            this.targetx = tmp2[0];
                            this.targety = tmp2[1];
                        }
                        else {
                            if (targetLocation_1.maps.length > 0) {
                                // we set this so that we can later give the proper reason for why we cannot go
                                var tmp3 = targetLocation_1.centerWalkableCoordinatesInMap(targetLocation_1.maps[0], ai.robot);
                                if (tmp3 != null) {
                                    targetMap = targetLocation_1.maps[0];
                                    this.targetx = tmp3[0];
                                    this.targety = tmp3[1];
                                }
                            }
                        }
                    }
                }
            }
            else {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #not(verb.can('" + ai.selfID + "'[#id], verb.see('" + ai.selfID + "'[#id], '" + targetID + "'[#id])))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
        }
        else if (intention.attributes.length >= 2 &&
            (intention.attributes[1] instanceof VariableTermAttribute)) {
            var targetObject_2 = ai.game.findObjectByIDJustObject(requesterID);
            if (targetObject_2 == null) {
                // we should never get here:
                if (requester != null) {
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
            if (intention.attributes.length == 2 && intention.attributes[1].sort.name == "space.away") {
                if (ai.visionActive) {
                    // Find a nearby location and go there:
                    var bestD = null;
                    var bestX = null;
                    var bestY = null;
                    for (var _i = 0, _a = ai.game.locations; _i < _a.length; _i++) {
                        var location_1 = _a[_i];
                        if (ai.locationsWherePlayerIsNotPermitted.indexOf(location_1.id) == -1) {
                            // candidate location to go to:
                            var coords = location_1.centerCoordinatesInMap(targetObject_2.map);
                            if (coords != null) {
                                var d_1 = Math.abs(targetObject_2.x - coords[0]) + Math.abs(targetObject_2.y - coords[1]);
                                console.log("location distance: " + d_1 + " (" + location_1.id + ")");
                                if (d_1 > 128) { // some arbitrary definition of "away"
                                    if (bestD == null || d_1 < bestD) {
                                        bestD = d_1;
                                        bestX = coords[0];
                                        bestY = coords[1];
                                    }
                                }
                            }
                        }
                    }
                    if (bestX != null) {
                        targetMap = targetObject_2.map;
                        this.targetx = bestX;
                        this.targety = bestY;
                        targetLocation = ai.game.getAILocationTileCoordinate(targetMap, this.targetx / targetMap.tileWidth, this.targety / targetMap.tileHeight);
                    }
                    else {
                        targetObject_2 = null; // to triger the denyrequest message
                    }
                }
                else {
                    if (requester != null) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                        term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", property.blind('" + ai.selfID + "'[#id])))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
                    ir.succeeded = false;
                    return true;
                }
            }
            else if (intention.attributes.length == 2 && intention.attributes[1].sort.name == "space.outside") {
                if (ai.visionActive) {
                    // Find a nearby location and go there:
                    var startLocation = ai.game.getAILocation(ai.robot);
                    if (startLocation != null) {
                        targetLocation = ai.locationOutsideOf(startLocation);
                        if (targetLocation != null) {
                            var tmp2 = targetLocation.centerWalkableCoordinatesInMap(ai.robot.map, ai.robot);
                            // ensure that the target location is actually outside of the specified location:
                            var tmp2location = null;
                            if (tmp2 != null)
                                tmp2location = ai.game.getAILocationTileCoordinate(ai.robot.map, tmp2[0] / ai.robot.map.tileWidth, tmp2[1] / ai.robot.map.tileHeight);
                            if (tmp2 != null &&
                                tmp2location != startLocation &&
                                !ai.game.location_in[ai.game.locations.indexOf(tmp2location)][ai.game.locations.indexOf(startLocation)]) {
                                targetMap = ai.robot.map;
                                this.targetx = tmp2[0];
                                this.targety = tmp2[1];
                            }
                            else {
                                for (var mapidx = 0; mapidx < targetLocation.maps.length; mapidx++) {
                                    // we set this so that we can later give the proper reason for why we cannot go
                                    var tmp3 = targetLocation.centerWalkableCoordinatesInMap(targetLocation.maps[mapidx], ai.robot);
                                    // ensure that the target location is actually outside of the specified location:
                                    var tmp3location = null;
                                    if (tmp3 != null)
                                        tmp3location = ai.game.getAILocationTileCoordinate(targetLocation.maps[mapidx], tmp3[0] / targetLocation.maps[mapidx].tileWidth, tmp3[1] / targetLocation.maps[mapidx].tileHeight);
                                    if (tmp3 != null &&
                                        tmp3location != startLocation &&
                                        !ai.game.location_in[ai.game.locations.indexOf(tmp3location)][ai.game.locations.indexOf(startLocation)]) {
                                        targetMap = targetLocation.maps[mapidx];
                                        this.targetx = tmp3[0];
                                        this.targety = tmp3[1];
                                    }
                                }
                            }
                        }
                    }
                }
                else {
                    if (requester != null) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                        term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", property.blind('" + ai.selfID + "'[#id])))", ai.o);
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
        }
        else if (intention.attributes.length >= 2 &&
            (intention.attributes[1] instanceof TermTermAttribute) &&
            intention.attributes[1].sort.is_a(ai.o.getSort("space.outside.of"))) {
            if (ai.visionActive) {
                // go outside of a specific location:
                var ooTerm = intention.attributes[1].term;
                if (ooTerm.attributes.length == 1 &&
                    ooTerm.attributes[0] instanceof ConstantTermAttribute) {
                    // Find a nearby location and go there:
                    var startLocation = ai.game.getAILocationByID(ooTerm.attributes[0].value);
                    if (startLocation != null) {
                        targetLocation = ai.locationOutsideOf(startLocation);
                        if (targetLocation != null) {
                            var tmp2 = targetLocation.centerWalkableCoordinatesInMap(ai.robot.map, ai.robot);
                            // ensure that the target location is actually outside of the specified location:
                            var tmp2location = ai.game.getAILocationTileCoordinate(ai.robot.map, tmp2[0] / ai.robot.map.tileWidth, tmp2[1] / ai.robot.map.tileHeight);
                            if (tmp2 != null &&
                                tmp2location != startLocation &&
                                !ai.game.location_in[ai.game.locations.indexOf(tmp2location)][ai.game.locations.indexOf(startLocation)]) {
                                targetMap = ai.robot.map;
                                this.targetx = tmp2[0];
                                this.targety = tmp2[1];
                            }
                            else {
                                for (var mapidx = 0; mapidx < targetLocation.maps.length; mapidx++) {
                                    // we set this so that we can later give the proper reason for why we cannot go
                                    var tmp3 = targetLocation.centerWalkableCoordinatesInMap(targetLocation.maps[mapidx], ai.robot);
                                    // ensure that the target location is actually outside of the specified location:
                                    var tmp3location = ai.game.getAILocationTileCoordinate(targetLocation.maps[mapidx], tmp3[0] / targetLocation.maps[mapidx].tileWidth, tmp3[1] / targetLocation.maps[mapidx].tileHeight);
                                    if (tmp3 != null &&
                                        tmp3location != startLocation &&
                                        !ai.game.location_in[ai.game.locations.indexOf(tmp3location)][ai.game.locations.indexOf(startLocation)]) {
                                        targetMap = targetLocation.maps[mapidx];
                                        this.targetx = tmp3[0];
                                        this.targety = tmp3[1];
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            else {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", property.blind('" + ai.selfID + "'[#id])))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
        }
        else if (intention.attributes.length == 5 &&
            (intention.attributes[1] instanceof ConstantTermAttribute) &&
            (intention.attributes[2] instanceof ConstantTermAttribute) &&
            (intention.attributes[3] instanceof ConstantTermAttribute) &&
            (intention.attributes[4] instanceof ConstantTermAttribute) &&
            intention.attributes[1].sort.name == "number") {
            // to to some specific coordinates:
            this.targetx = Number((intention.attributes[1]).value);
            this.targety = Number((intention.attributes[2]).value);
            this.targetMapName = (intention.attributes[3]).value;
            this.stayUntil = ai.timeStamp + Number((intention.attributes[4]).value);
            targetMap = ai.game.getMap(this.targetMapName);
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
        // If we do not know the destination map:
        if (targetMap == null) {
            if (requester != null) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))";
                var term = Term.fromString(tmp, ai.o);
                var cause = Term.fromString("#not(verb.know('" + ai.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", ai.o);
                var causeRecord = new CauseRecord(cause, null, ai.timeStamp);
                ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        this.targetMapName = targetMap.name;
        // Check if the robot can go:
        var targetLocationID = null;
        if (targetLocation != null)
            targetLocationID = targetLocation.id;
        var cannotGoCause = ai.canGoTo(targetMap, targetLocationID, requester);
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
        ai.addCurrentActionLongTermTerm(intention);
        ai.intentionsCausedByRequest.push(ir);
        if (requester != null) {
            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))";
            var term = Term.fromString(tmp, ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        app.achievement_nlp_all_robot_actions[13] = true;
        app.trigger_achievement_complete_alert();
        ai.setNewAction(intention, requester, null, this);
        this.executeContinuous(ai);
        this.ir.succeeded = true; // temporarily set this to success
        return true;
    };
    RobotStay_IntentionAction.prototype.executeContinuous = function (ai_raw) {
        var ai = ai_raw;
        if (ai.robot.isInVehicle()) {
            ai.robot.disembark();
            return false;
        }
        if (this.stayUntil != null && ai.timeStamp >= this.stayUntil) {
            if (this.ir != null)
                this.ir.succeeded = true;
            return true;
        }
        // go to destination:
        var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
        var s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, this.targetMapName, null, 0, false, false);
        s.x = this.targetx;
        s.y = this.targety;
        s.stopAfterGoingThroughABridge = false;
        q.scripts.push(s);
        ai.currentAction_scriptQueue = q;
        return false;
    };
    RobotStay_IntentionAction.prototype.saveToXML = function (ai) {
        var tmp = "<IntentionAction type=\"RobotStay_IntentionAction\" " +
            "targetx=\"" + this.targetx + "\" targety=\"" + this.targety + "\" targetMapName=\"" + this.targetMapName + "\"";
        if (this.stayUntil != null) {
            tmp += " stayUntil=\"" + this.stayUntil + "\"";
        }
        tmp += "/>";
        return tmp;
    };
    RobotStay_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new RobotStay_IntentionAction();
        a.targetx = Number(xml.getAttribute("targetx"));
        a.targety = Number(xml.getAttribute("targety"));
        a.targetMapName = xml.getAttribute("targetMapName");
        if (xml.getAttribute("stayUntil") != null) {
            a.stayUntil = Number(xml.getAttribute("stayUntil"));
        }
        return a;
    };
    return RobotStay_IntentionAction;
}(IntentionAction));
