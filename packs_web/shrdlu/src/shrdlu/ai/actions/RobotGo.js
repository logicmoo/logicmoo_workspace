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
var RobotGo_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotGo_IntentionAction, _super);
    function RobotGo_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotGo_IntentionAction.prototype.canHandle = function (intention, ai) {
        if ((intention.functor.is_a(ai.o.getSort("verb.go")) ||
            intention.functor.is_a(ai.o.getSort("verb.move-to")) ||
            (intention.functor.is_a(ai.o.getSort("verb.move")) &&
                intention.attributes.length >= 2 &&
                (intention.attributes[1] instanceof VariableTermAttribute))) &&
            !intention.functor.is_a(ai.o.getSort("verb.leave"))) {
            return true;
        }
        return false;
    };
    RobotGo_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        var requesterID = null;
        if (requester instanceof ConstantTermAttribute) {
            requesterID = requester.value;
        }
        if (ai.robot.isInVehicle()) {
            if (intention.attributes.length == 2 &&
                (intention.attributes[1].sort.name == "space.outside" ||
                    intention.attributes[1].sort.name == "space.away")) {
                // redirect to leave the vehicle:
                var term2 = new Term(ai.o.getSort("verb.leave"), [intention.attributes[0]]);
                ir.action = term2;
                // ai.intentions.push(new IntentionRecord(term2, requester, null, null, ai.timeStamp));
                return null;
            }
        }
        if (intention.attributes.length == 0 ||
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
        // let stopAfterGoingThroughABridge:boolean = false;
        var targetLocation = null;
        var targetLocationID = null;
        var targetMap = null;
        this.targetx = null;
        this.targety = null;
        this.targetObjectID = null;
        this.targetMapName = null;
        this.stepByStepMovement = false;
        // process "direction.towards" (replace by a move-to(self, [cardinal direction]))
        if (intention.functor.name == "verb.move-to" &&
            intention.attributes.length == 2 &&
            (intention.attributes[1] instanceof TermTermAttribute) &&
            intention.attributes[1].sort.is_a_string("direction.towards")) {
            var towards = intention.attributes[1].term;
            if (towards.attributes.length == 1 &&
                towards.attributes[0] instanceof ConstantTermAttribute) {
                var targetID = towards.attributes[0].value;
                var target_l = ai.game.findObjectByID(targetID);
                if (target_l != null && target_l.length > 0) {
                    if (target_l[0].map.name == ai.robot.map.name) {
                        if (requesterID == targetID || ai.canSee(targetID)) {
                            var direction = ai.getDirectionTowardObject(ai.robot, target_l[0]);
                            if (direction != null) {
                                intention = new Term(intention.functor, [intention.attributes[0],
                                    new VariableTermAttribute(direction, null)]);
                            }
                        }
                    }
                }
            }
        }
        // find the target destination:
        if (intention.functor.name == "verb.come-back" ||
            intention.functor.name == "verb.come" &&
                (intention.attributes.length == 1 ||
                    intention.attributes.length == 2 &&
                        (intention.attributes[1] instanceof VariableTermAttribute) &&
                        intention.attributes[1].sort.is_a(ai.o.getSort("space.here"))) &&
                requester != null &&
                requester instanceof ConstantTermAttribute) {
            if (ai.visionActive) {
                // destination is the position of the speaker:
                var targetObject = ai.game.findObjectByIDJustObject(requesterID);
                if (targetObject != null) {
                    targetMap = targetObject.map;
                    this.targetx = targetObject.x;
                    this.targety = targetObject.y;
                    this.targetObjectID = targetObject.ID;
                    targetLocation = ai.game.getAILocation(targetObject);
                    if (targetLocation != null)
                        targetLocationID = targetLocation.id;
                }
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
        else if (intention.attributes.length == 2 &&
            intention.attributes[1] instanceof ConstantTermAttribute) {
            var targetID = (intention.attributes[1]).value;
            var targetObject = ai.game.findObjectByIDJustObject(targetID);
            if (ai.visionActive) {
                // destination is the second attribute:
                if (targetObject != null) {
                    targetMap = targetObject.map;
                    this.targetx = targetObject.x;
                    this.targety = targetObject.y;
                    this.targetObjectID = targetObject.ID;
                    targetLocation = ai.game.getAILocation(targetObject);
                    if (targetLocation != null)
                        targetLocationID = targetLocation.id;
                }
                else {
                    var targetLocation_1 = ai.game.getAILocationByID(targetID);
                    if (targetLocation_1 != null) {
                        targetLocationID = targetLocation_1.id;
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
                            var coords = location_1.centerCoordinatesInMap(targetObject.map);
                            if (coords != null) {
                                var d_1 = Math.abs(targetObject.x - coords[0]) + Math.abs(targetObject.y - coords[1]);
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
                        targetMap = targetObject.map;
                        this.targetx = bestX;
                        this.targety = bestY;
                        targetLocation = ai.game.getAILocationTileCoordinate(targetMap, this.targetx / targetMap.tileWidth, this.targety / targetMap.tileHeight);
                        if (targetLocation != null)
                            targetLocationID = targetLocation.id;
                    }
                    else {
                        targetObject = null; // to triger the denyrequest message
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
                            targetLocationID = targetLocation.id;
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
            else if (intention.attributes[1].sort.is_a_string("direction")) {
                var movementAmount = 4;
                if (intention.attributes.length == 3 &&
                    (intention.attributes[2] instanceof VariableTermAttribute)) {
                    if (intention.attributes[2].sort.is_a_string("small-amount"))
                        movementAmount = 1;
                    if (intention.attributes[2].sort.is_a_string("large-amount"))
                        movementAmount = 8;
                    if (intention.attributes[2].sort.is_a_string("space.far"))
                        movementAmount = 8;
                }
                else if (intention.attributes.length == 3 &&
                    intention.attributes[2].sort.is_a_string("number") &&
                    intention.attributes[2] instanceof ConstantTermAttribute) {
                    movementAmount = Number(intention.attributes[2].value) * 4;
                }
                this.targetx = null;
                this.targety = null;
                if (intention.attributes[1].sort.name == "west") {
                    this.targetx = ai.robot.x - movementAmount * 8;
                    this.targety = ai.robot.y;
                }
                if (intention.attributes[1].sort.name == "north") {
                    this.targetx = ai.robot.x;
                    this.targety = ai.robot.y - movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "east") {
                    this.targetx = ai.robot.x + movementAmount * 8;
                    this.targety = ai.robot.y;
                }
                if (intention.attributes[1].sort.name == "south") {
                    this.targetx = ai.robot.x;
                    this.targety = ai.robot.y + movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "northeast") {
                    this.targetx = ai.robot.x + movementAmount * 8;
                    this.targety = ai.robot.y - movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "northwest") {
                    this.targetx = ai.robot.x - movementAmount * 8;
                    this.targety = ai.robot.y - movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "southeast") {
                    this.targetx = ai.robot.x + movementAmount * 8;
                    this.targety = ai.robot.y + movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "southwest") {
                    this.targetx = ai.robot.x - movementAmount * 8;
                    this.targety = ai.robot.y + movementAmount * 8;
                }
                if (intention.attributes[1].sort.name == "forward") {
                    this.targetx = ai.robot.x + movementAmount * 8 * direction_x_inc[ai.robot.direction];
                    this.targety = ai.robot.y + movementAmount * 8 * direction_y_inc[ai.robot.direction];
                }
                if (intention.attributes[1].sort.name == "backward") {
                    this.targetx = ai.robot.x - movementAmount * 8 * direction_x_inc[ai.robot.direction];
                    this.targety = ai.robot.y - movementAmount * 8 * direction_y_inc[ai.robot.direction];
                }
                if (intention.attributes[1].sort.name == "direction.right") {
                    this.targetx = ai.robot.x + movementAmount * 8 * direction_x_inc[(ai.robot.direction + 1) % A4_NDIRECTIONS];
                    this.targety = ai.robot.y + movementAmount * 8 * direction_y_inc[(ai.robot.direction + 1) % A4_NDIRECTIONS];
                }
                if (intention.attributes[1].sort.name == "direction.left") {
                    var dir = ai.robot.direction - 1;
                    if (dir < 0)
                        dir += A4_NDIRECTIONS;
                    this.targetx = ai.robot.x + movementAmount * 8 * direction_x_inc[dir];
                    this.targety = ai.robot.y + movementAmount * 8 * direction_y_inc[dir];
                }
                this.stepByStepMovement = true;
                targetMap = ai.robot.map;
                this.targetx = Math.floor(this.targetx / 8) * 8;
                this.targety = Math.floor(this.targety / 8) * 8;
                // calculate the first step of the path, to see the map/target location:
                var x = Math.floor(ai.robot.x / 8) * 8;
                var y = Math.floor(ai.robot.y / 8) * 8;
                if (x < this.targetx)
                    x += 8;
                if (x > this.targetx)
                    x -= 8;
                if (y < this.targety)
                    y += 8;
                if (y > this.targety)
                    y -= 8;
                if (targetMap != null)
                    targetLocation = ai.game.getAILocationTileCoordinate(targetMap, x / targetMap.tileWidth, y / targetMap.tileHeight);
                if (targetLocation != null)
                    targetLocationID = targetLocation.id;
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
                            targetLocationID = targetLocation.id;
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
        else if (intention.attributes.length == 4 &&
            (intention.attributes[1] instanceof ConstantTermAttribute) &&
            (intention.attributes[2] instanceof ConstantTermAttribute) &&
            (intention.attributes[3] instanceof ConstantTermAttribute) &&
            intention.attributes[1].sort.name == "number") {
            // to to some specific coordinates:
            this.targetx = Number((intention.attributes[1]).value);
            this.targety = Number((intention.attributes[2]).value);
            this.targetMapName = (intention.attributes[3]).value;
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
        if (!app.achievement_nlp_all_robot_actions[1]) {
            app.achievement_nlp_all_robot_actions[1] = true;
            app.trigger_achievement_complete_alert();
        }
        // ai.setNewAction(intention, requester, null, null);
        // if (stepByStepMovement || ai.robot.isInVehicle()) {
        // if (!this.executeContinuous(ai)) {
        this.needsContinuousExecution = true;
        ai.setNewAction(intention, requester, null, this);
        // } else {
        // 	this.needsContinuousExecution = false;
        // }
        // } else {
        // 	// go to destination:
        //        let q:A4ScriptExecutionQueue = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
        //        let s:A4Script = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, targetMap.name, null, 0, false, false);
        //        s.x = this.targetx;
        //        s.y = this.targety;
        //        s.stopAfterGoingThroughABridge = stopAfterGoingThroughABridge;
        //        q.scripts.push(s);
        // 	this.needsContinuousExecution = false;
        //        ai.setNewAction(intention, requester, q, null);
        // }
        this.ir.succeeded = true; // temporarily set this to success
        return true;
    };
    RobotGo_IntentionAction.prototype.executeContinuous = function (ai_raw) {
        var ai = ai_raw;
        if (ai.robot.isInVehicle()) {
            ai.robot.disembark();
            return false;
        }
        var x = Math.floor(ai.robot.x / 8) * 8;
        var y = Math.floor(ai.robot.y / 8) * 8;
        if (x == this.targetx && y == this.targety && ai.robot.map.name == this.targetMapName) {
            // we almost made it, done!
            if (this.ir != null)
                this.ir.succeeded = true;
            return true;
        }
        else if (this.stepByStepMovement) {
            if (ai.robot.map.name == this.targetMapName) {
                if (x < this.targetx)
                    x += 8;
                if (x > this.targetx)
                    x -= 8;
                if (y < this.targety)
                    y += 8;
                if (y > this.targety)
                    y -= 8;
                if (!ai.robot.map.walkableConsideringVehicles(x, y, ai.robot.getPixelWidth(), ai.robot.getPixelHeight(), ai.robot)) {
                    // obstacle!
                    var collisions = ai.robot.map.getAllObjects(x, y, ai.robot.getPixelWidth(), ai.robot.getPixelHeight());
                    var collision = null;
                    for (var _i = 0, collisions_1 = collisions; _i < collisions_1.length; _i++) {
                        var o = collisions_1[_i];
                        if (o != ai.robot &&
                            !o.isWalkable()) {
                            // collision!
                            collision = o;
                            break;
                        }
                    }
                    if (collision != null) {
                        ai.addEpisodeTerm("verb.collide-with('" + ai.selfID + "'[#id],'" + collision.ID + "'[#id])", MEMORIZE_PROVENANCE);
                        if (ai.currentAction_requester != null) {
                            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform(" + ai.currentAction_requester + ", verb.collide-with('" + ai.selfID + "'[#id],'" + collision.ID + "'[#id])))";
                            var term = Term.fromString(tmp, ai.o);
                            ai.queueIntention(term, ai.currentAction_requester, null);
                        }
                    }
                    else {
                        if (ai.currentAction_requester != null) {
                            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform(" + ai.currentAction_requester + ", #and(obstacle(X), space.at(X, [space.here]))))";
                            var term = Term.fromString(tmp, ai.o);
                            ai.queueIntention(term, ai.currentAction_requester, null);
                        }
                    }
                    if (this.ir != null)
                        this.ir.succeeded = false;
                    return true;
                }
                // go to destination:
                var q = new A4ScriptExecutionQueue(ai.robot, ai.robot.map, ai.game, null);
                var s = new A4Script(A4_SCRIPT_GOTO_OPENING_DOORS, ai.robot.map.name, null, 0, false, false);
                s.x = x;
                s.y = y;
                s.stopAfterGoingThroughABridge = true;
                q.scripts.push(s);
                ai.currentAction_scriptQueue = q;
                return false;
            }
            else {
                // we changed maps, so, stop just in case:
                if (this.ir != null)
                    this.ir.succeeded = false;
                return true;
            }
        }
        else {
            if (this.targetObjectID != null) {
                var targetObject = ai.game.findObjectByID(this.targetObjectID);
                if (targetObject != null && targetObject.length > 0 &&
                    ai.robot.map == targetObject[0].map &&
                    ai.robot.pixelDistance(targetObject[0]) == 0) {
                    // we made it!
                    if (this.ir != null)
                        this.ir.succeeded = true;
                    return true;
                }
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
        }
    };
    RobotGo_IntentionAction.prototype.saveToXML = function (ai) {
        var tmp = "<IntentionAction type=\"RobotGo_IntentionAction\" " +
            "targetx=\"" + this.targetx + "\" targety=\"" + this.targety + "\" targetMapName=\"" + this.targetMapName + "\"";
        if (this.targetObjectID != null)
            tmp += " targetObjectID=\"" + this.targetObjectID + "\"";
        tmp += " stepByStepMovement=\"" + this.stepByStepMovement + "\"/>";
        return tmp;
    };
    RobotGo_IntentionAction.loadFromXML = function (xml, ai) {
        var a = new RobotGo_IntentionAction();
        a.targetx = Number(xml.getAttribute("targetx"));
        a.targety = Number(xml.getAttribute("targety"));
        a.targetObjectID = xml.getAttribute("targetObjectID");
        a.targetMapName = xml.getAttribute("targetMapName");
        a.stepByStepMovement = xml.getAttribute("stepByStepMovement") == "true";
        return a;
    };
    return RobotGo_IntentionAction;
}(IntentionAction));
