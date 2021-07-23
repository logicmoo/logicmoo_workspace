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
var RobotAI = /** @class */ (function (_super) {
    __extends(RobotAI, _super);
    function RobotAI(o, nlp, robot, game, rulesFileNames) {
        var _this = _super.call(this, o, nlp, game, 12, 4, DEFAULT_QUESTION_PATIENCE_TIMER) || this;
        _this.robot = null;
        _this.intentionActionFactory = null;
        _this.lastActionRequestTime = -1;
        // In addition to the script queues directly in the robot, these are script queues that are managed directly by the AI. 
        // Specifically, scripts responsible for making the robot perform the current action the robot is trying to accomplish,
        // are placed here. Each time the robot is given a new task, this script is cleared.
        _this.currentAction = null;
        _this.currentAction_requester = null;
        _this.currentAction_scriptQueue = null;
        _this.currentActionHandler = null;
        _this.intentionsToExecuteAfterTheCurrentAction = []; // some times the player requests more than one action at a time in the same
        // performative (e.g., "give me all the keys"). These have to be handled one by
        // one. This list stores those that are waiting to be executed
        // the IDs of the objects we do not want to give to the player:
        _this.objectsNotAllowedToGive = [];
        _this.etaoin_perception_term = null;
        _this.perceptionRadius = 16;
        _this.getOutOfTheWay = true;
        console.log("RobotAI.constructor Start...");
        _this.robot = robot;
        _this.intentionActionFactory = new A4IntentionActionFactory();
        _this.intentionHandlers.push(new RobotFollow_IntentionAction());
        _this.intentionHandlers.push(new RobotTalk_IntentionAction());
        _this.intentionHandlers.push(new RobotEnter_IntentionAction());
        _this.intentionHandlers.push(new RobotExit_IntentionAction());
        _this.intentionHandlers.push(new RobotGo_IntentionAction());
        _this.intentionHandlers.push(new RobotTakeTo_IntentionAction());
        _this.intentionHandlers.push(new RobotStay_IntentionAction());
        _this.intentionHandlers.push(new RobotStop_IntentionAction());
        _this.intentionHandlers.push(new RobotTake_IntentionAction());
        _this.intentionHandlers.push(new RobotPutIn_IntentionAction());
        _this.intentionHandlers.push(new RobotDrop_IntentionAction());
        _this.intentionHandlers.push(new RobotGive_IntentionAction());
        _this.intentionHandlers.push(new RobotOpenClose_IntentionAction());
        _this.intentionHandlers.push(new RobotHelp_IntentionAction());
        _this.intentionHandlers.push(new RobotTurn_IntentionAction());
        _this.intentionHandlers.push(new RobotPushPull_IntentionAction());
        _this.intentionHandlers.push(new RobotReboot_IntentionAction());
        // load specific knowledge:
        for (var _i = 0, rulesFileNames_1 = rulesFileNames; _i < rulesFileNames_1.length; _i++) {
            var rulesFileName = rulesFileNames_1[_i];
            var xmlhttp = new XMLHttpRequest();
            xmlhttp.overrideMimeType("text/xml");
            xmlhttp.open("GET", rulesFileName, false);
            xmlhttp.send();
            _this.loadLongTermRulesFromXML(xmlhttp.responseXML.documentElement);
        }
        _this.precalculateLocationKnowledge(game, o);
        _this.add3DPrintingKnowledge(game, o, "etaoin");
        _this.robot.AI.doorsNotToOpenWhileWalking = _this.doorsPlayerIsNotPermittedToOpen;
        console.log("RobotAI.constructor end...");
        return _this;
    }
    RobotAI.prototype.update = function (timeStamp) {
        _super.prototype.update.call(this, timeStamp);
        // continuous actions:
        if (this.currentActionHandler != null &&
            this.currentActionHandler.needsContinuousExecution) {
            if (this.currentActionHandler.executeContinuous(this)) {
                if (this.currentGoal == null) {
                    this.addLongTermTerm(new Term(this.o.getSort("verb.do"), [new ConstantTermAttribute(this.selfID, this.cache_sort_id),
                        new ConstantTermAttribute("nothing", this.o.getSort("nothing"))]), PERCEPTION_PROVENANCE);
                }
                // if (this.currentActionHandler.ir.succeeded == null) {
                // 	throw new Error("continuous action handler for " + this.currentActionHandler.ir.action + "  did not set succeeded!");
                // }
                if (!this.currentActionHandler.ir.succeeded) {
                    this.removeQueuedPerformativesDependingOnIntentionSuccess(this.currentActionHandler.ir);
                }
                this.clearCurrentAction();
            }
        }
        // actions waiting to be executed:
        if (this.intentionsToExecuteAfterTheCurrentAction.length > 0 &&
            this.intentions.length == 0 &&
            this.queuedIntentions.length == 0 &&
            this.currentAction == null &&
            this.timeStamp > this.lastActionRequestTime) {
            this.queueIntentionRecord(this.intentionsToExecuteAfterTheCurrentAction[0]);
            this.intentionsToExecuteAfterTheCurrentAction.splice(0, 1);
        }
        // get out of the way of the player:
        if (this.getOutOfTheWay &&
            this.robot.map == this.game.currentPlayer.map && this.robot.scriptQueues.length == 0 && this.visionActive) {
            var d_1 = this.robot.pixelDistance(this.game.currentPlayer);
            if (d_1 == 0) {
                // see if the player were to advance in its current facing will collide with us, and in this case, move way:
                if (this.game.currentPlayer.collisionObjectOffset(direction_x_inc[this.game.currentPlayer.direction], direction_y_inc[this.game.currentPlayer.direction], this.robot)) {
                    // find a nearby position further from from the player and move there to get out of the way of the player:
                    d_1 = this.game.currentPlayer.pixelDistanceBetweenCentersOffset(this.robot, 0, 0);
                    var targetx = null;
                    var targety = null;
                    for (var offy = -1; offy <= 1; offy += 1) {
                        for (var offx = -1; offx <= 1; offx += 1) {
                            var d2 = this.game.currentPlayer.pixelDistanceBetweenCentersOffset(this.robot, offx * this.robot.map.tileWidth, offy * this.robot.map.tileHeight);
                            if (d2 > d_1) {
                                if (this.robot.map.walkable(this.robot.x + offx * this.robot.map.tileWidth, this.robot.y + offy * this.robot.map.tileHeight, this.robot.getPixelWidth(), this.robot.getPixelHeight(), this.robot)) {
                                    targetx = this.robot.x + offx * this.robot.map.tileWidth;
                                    targety = this.robot.y + offy * this.robot.map.tileHeight;
                                    d_1 = d2;
                                }
                            }
                        }
                    }
                    if (targetx != null) {
                        var q = new A4ScriptExecutionQueue(this.robot, this.robot.map, this.game, null);
                        var s = new A4Script(A4_SCRIPT_GOTO, this.robot.map.name, null, 0, false, false);
                        s.x = targetx;
                        s.y = targety;
                        s.timeOut = 4; // just try it for a little bit of time, otherwise, give up
                        q.scripts.push(s);
                        this.robot.addScriptQueue(q);
                    }
                }
            }
        }
        this.executeScriptQueues();
    };
    RobotAI.prototype.isIdle = function () {
        if (!this.robot.isIdle())
            return false;
        if (this.currentActionHandler != null ||
            this.currentAction_scriptQueue != null)
            return false;
        return _super.prototype.isIdle.call(this);
    };
    RobotAI.prototype.reactToRepeatActionPerformative = function (perf, speaker, context) {
        if (_super.prototype.reactToRepeatActionPerformative.call(this, perf, speaker, context))
            return true;
        if (!(speaker instanceof ConstantTermAttribute))
            return false;
        var speakerID = speaker.value;
        // find what was the last action that was requested:
        for (var i = 0; i < context.performatives.length; i++) {
            if (context.performatives[i].speaker == speakerID) {
                var request = context.performatives[i].performative;
                if (request.functor.is_a_string("perf.request.action") ||
                    request.functor.is_a_string("perf.q.action")) {
                    //  we have the action request!
                    this.reactToRequestActionPerformative(request, speaker, context);
                    return true;
                }
                else if (request.functor.is_a_string("perf.request.repeataction")) {
                    // see if there was an action request before this
                }
                else {
                    return false;
                }
            }
        }
        return false;
    };
    RobotAI.prototype.attentionAndPerception = function () {
        this.clearPerception();
        // just perceive an area around the robot:
        var attention_map = this.robot.map;
        var attention_x = this.robot.x + this.robot.getPixelWidth() / 2;
        var attention_y = this.robot.y + this.robot.getPixelHeight() / 2;
        // find the AILocation:
        var location = null;
        var location_idx = -1;
        var occupancyMap = null;
        var tile_x = Math.floor(attention_x / attention_map.tileWidth);
        var tile_y = Math.floor(attention_y / attention_map.tileHeight);
        var offset = tile_x + tile_y * attention_map.width;
        for (var location_idx2 = 0; location_idx2 < this.game.locations.length; location_idx2++) {
            var l = this.game.locations[location_idx2];
            for (var i = 0; i < l.maps.length; i++) {
                if (l.maps[i] == attention_map) {
                    if (l.mapOccupancyMaps[i][offset]) {
                        if (location == null) {
                            location = l;
                            location_idx = location_idx2;
                            occupancyMap = l.mapOccupancyMaps[i];
                        }
                        else {
                            if (this.game.location_in[location_idx2][location_idx]) {
                                location = l;
                                location_idx = location_idx2;
                                occupancyMap = l.mapOccupancyMaps[i];
                            }
                        }
                    }
                }
            }
        }
        //console.log("RobotAI: attention = " + attention_object.name + " at " + attention_map.name + ", " + location.name);
        // this is for some edge cases while transitioning map:
        if (location == null)
            return;
        var visibilityRegion = attention_map.visibilityRegion(tile_x, tile_y);
        // perception:
        for (var _i = 0, _a = this.robot.inventory; _i < _a.length; _i++) {
            var o = _a[_i];
            // perceived an object!
            var term1 = new Term(o.sort, [new ConstantTermAttribute(o.ID, this.cache_sort_id)]);
            var term2 = new Term(this.cache_sort_space_at, [new ConstantTermAttribute(o.ID, this.cache_sort_id),
                new ConstantTermAttribute(location.id, this.cache_sort_id)
            ]);
            this.addTermToPerception(term1);
            this.addTermToPerception(term2);
            for (var _b = 0, _c = this.getBaseObjectProperties(o); _b < _c.length; _b++) {
                var property = _c[_b];
                this.addTermToPerception(property);
            }
            this.addTermToPerception(new Term(this.cache_sort_verb_have, [new ConstantTermAttribute(this.selfID, this.cache_sort_id),
                new ConstantTermAttribute(o.ID, this.cache_sort_id)]));
        }
        if (this.visionActive) {
            this.perception((tile_x - this.perceptionRadius) * attention_map.tileWidth, (tile_y - this.perceptionRadius) * attention_map.tileHeight, (tile_x + this.perceptionRadius) * attention_map.tileWidth, (tile_y + this.perceptionRadius) * attention_map.tileHeight, location, attention_map, visibilityRegion, occupancyMap, null);
        }
        else {
            this.perception((tile_x - this.perceptionRadius) * attention_map.tileWidth, (tile_y - this.perceptionRadius) * attention_map.tileHeight, (tile_x + this.perceptionRadius) * attention_map.tileWidth, (tile_y + this.perceptionRadius) * attention_map.tileHeight, location, attention_map, visibilityRegion, occupancyMap, [(tile_x - 2) * attention_map.tileWidth,
                (tile_y - 2) * attention_map.tileHeight,
                (tile_x + 2) * attention_map.tileWidth,
                (tile_y + 2) * attention_map.tileHeight]);
        }
        if (this.canSee("etaoin")) {
            if (this.etaoin_perception_term == null) {
                this.etaoin_perception_term = Term.fromString("disembodied-ai('etaoin'[#id])", this.o);
            }
            this.addTermToPerception(this.etaoin_perception_term);
        }
        if (location != null) {
            if (attention_map.name == "Aurora Station") {
                this.addTermToPerception(Term.fromString("temperature('" + location.id + "'[#id],'" + this.game.aurora_station_temperature_sensor_indoors + "'[temperature.unit.celsius])", this.o));
            }
            else {
                this.addTermToPerception(Term.fromString("temperature('" + location.id + "'[#id],'" + this.game.aurora_station_temperature_sensor_outdoors + "'[temperature.unit.celsius])", this.o));
            }
        }
    };
    /* safely clear the current script queues, without erasing certain scripts that should never be removed from there! */
    RobotAI.prototype.clearScriptQueues = function () {
        var newQueues = [];
        for (var _i = 0, _a = this.robot.scriptQueues; _i < _a.length; _i++) {
            var q = _a[_i];
            var q2 = new A4ScriptExecutionQueue(q.object, q.map, q.game, q.otherCharacter);
            for (var _b = 0, _c = q.scripts; _b < _c.length; _b++) {
                var s = _c[_b];
                if (s.type == A4_SCRIPT_FAMILIARWITHMAP) {
                    q2.scripts.push(s);
                }
            }
            if (q2.scripts.length > 0)
                newQueues.push(q2);
        }
        this.robot.scriptQueues = newQueues;
    };
    RobotAI.prototype.stopAction = function (actionRequest, requester) {
        if (_super.prototype.stopAction.call(this, actionRequest, requester)) {
            this.clearScriptQueues();
            return true;
        }
        if (this.currentAction != null &&
            this.currentAction.equalsNoBindings(actionRequest) == 1) {
            this.clearCurrentAction();
            this.intentionsToExecuteAfterTheCurrentAction = [];
            return true;
        }
        return false;
    };
    RobotAI.prototype.clearCurrentAction = function () {
        this.clearScriptQueues();
        this.currentAction = null;
        this.currentAction_requester = null;
        this.currentAction_scriptQueue = null;
        this.currentActionHandler = null;
    };
    RobotAI.prototype.setNewAction = function (action, requester, scriptQueue, handler) {
        this.clearScriptQueues();
        this.currentAction = action;
        this.currentAction_requester = requester;
        this.currentAction_scriptQueue = scriptQueue;
        this.currentActionHandler = handler;
    };
    RobotAI.prototype.executeScriptQueues = function () {
        while (this.currentAction_scriptQueue != null) {
            var s = this.currentAction_scriptQueue.scripts[0];
            var retval = s.execute((this.currentAction_scriptQueue.object == null ? this.robot : this.currentAction_scriptQueue.object), (this.currentAction_scriptQueue.map == null ? this.robot.map : this.currentAction_scriptQueue.map), (this.currentAction_scriptQueue.game == null ? this.game : this.currentAction_scriptQueue.game), this.currentAction_scriptQueue.otherCharacter);
            if (retval == SCRIPT_FINISHED) {
                this.currentAction_scriptQueue.scripts.splice(0, 1);
                if (this.currentAction_scriptQueue.scripts.length == 0) {
                    this.currentAction_scriptQueue = null;
                    if (this.currentGoal == null &&
                        (this.currentActionHandler == null ||
                            !this.currentActionHandler.needsContinuousExecution)) {
                        this.clearCurrentAction();
                        this.addLongTermTerm(Term.fromString("verb.do('" + this.selfID + "'[#id], 'nothing'[nothing])", this.o), PERCEPTION_PROVENANCE);
                    }
                }
            }
            else if (retval == SCRIPT_NOT_FINISHED) {
                // see if we are stuck in front of a door (and we will not accidentally let the player into a forbidden area):
                var collisions = this.robot.map.getAllObjectCollisionsWithOffset(this.robot, direction_x_inc[this.robot.direction], direction_y_inc[this.robot.direction]);
                for (var _i = 0, collisions_1 = collisions; _i < collisions_1.length; _i++) {
                    var o = collisions_1[_i];
                    if ((o instanceof A4Door) &&
                        o.closed &&
                        o.canOpen(this.robot, this.game) &&
                        this.doorsPlayerIsNotPermittedToOpen.indexOf(o.doorID) == -1) {
                        // try to open it!
                        var cmd = new A4CharacterCommand(A4CHARACTER_COMMAND_INTERACT, 0, this.robot.direction, null, null, 10);
                        this.robot.issueCommand(cmd, this.game);
                    }
                }
                break;
            }
            else if (retval == SCRIPT_FAILED) {
                if (this.currentActionHandler != null)
                    this.currentActionHandler.actionScriptsFailed(this, this.currentAction_requester);
                this.clearCurrentAction();
                if (this.currentGoal == null) {
                    this.addLongTermTerm(Term.fromString("verb.do('" + this.selfID + "'[#id], 'nothing'[nothing])", this.o), PERCEPTION_PROVENANCE);
                }
            }
        }
    };
    RobotAI.prototype.canSatisfyActionRequest = function (ir) {
        var tmp = _super.prototype.canSatisfyActionRequest.call(this, ir);
        if (tmp == ACTION_REQUEST_CAN_BE_SATISFIED) {
            if (this.timeStamp == this.lastActionRequestTime) {
                // multiple requests in the same performative, just queue them:
                this.intentionsToExecuteAfterTheCurrentAction.push(ir);
                return ACTION_REQUEST_WILL_BE_HANDLED_EXTERNALLY;
            }
            else {
                // clear the queue, as there will be a new request, that overwrites the previous requests:
                this.intentionsToExecuteAfterTheCurrentAction = [];
            }
            this.lastActionRequestTime = this.timeStamp;
        }
        return tmp;
    };
    RobotAI.prototype.canSee = function (characterID) {
        if (!this.visionActive)
            return false;
        if (characterID == "etaoin") {
            if (this.robot.map.name == "Aurora Station" ||
                this.robot.map.name == "Aurora Station Outdoors")
                return true;
        }
        return _super.prototype.canSee.call(this, characterID);
    };
    RobotAI.prototype.canHear = function (objectID) {
        var map = this.robot.map;
        // etaoin exception:
        if (objectID == "etaoin") {
            if (this.game.comm_tower_repaired)
                return true;
            if (map.name == "Aurora Station" ||
                map.name == "Aurora Station Outdoors" ||
                map.name == "Spacer Valley South")
                return true;
        }
        // exception of the player through the communicator:
        if (objectID == "communicator" || objectID == "player") {
            if (this.game.communicatorConnectedTo == this.selfID)
                return true;
        }
        var o = this.game.findObjectByIDJustObject(objectID);
        if (o == null)
            return false;
        if (map != o.map)
            return false;
        var dx = this.robot.x - o.x;
        var dy = this.robot.y - o.y;
        if (dx > this.perceptionRadius * map.tileWidth)
            return false;
        if (dy > this.perceptionRadius * map.tileHeight)
            return false;
        return true;
    };
    /*
    - If it returns "null", it means the robot can go
    - If it returns a Term, it means the robot cannot go, and the Term is the intention the robot
    */
    RobotAI.prototype.canGoTo = function (map, locationID, requester) {
        if (this.locationsWherePlayerIsNotPermitted.indexOf(locationID) >= 0) {
            var cause = Term.fromString("#not(verb.have('player'[#id],[permission-to]))", this.o);
            return cause;
        }
        // if the robot does not know the path:
        if (this.robot.AI.map2mapPaths != null) {
            var map1idx = this.robot.AI.map2mapNames.indexOf(this.robot.map.name);
            var map2idx = this.robot.AI.map2mapNames.indexOf(map.name);
            if (map1idx >= 0 && map2idx >= 0 && map1idx != map2idx) {
                if (this.robot.AI.map2mapPaths[map1idx][map2idx] == null) {
                    var cause = Term.fromString("#not(verb.know('" + this.selfID + "'[#id], #and(the(P:'path'[path], N:[singular]), noun(P, N))))", this.o);
                    return cause;
                }
            }
        }
        return null;
    };
    // Returns "true" if the AI is still trying to execute "ir"
    RobotAI.prototype.IRpendingCompletion = function (ir) {
        if (this.currentActionHandler != null &&
            this.currentActionHandler.ir == ir)
            return true;
        return _super.prototype.IRpendingCompletion.call(this, ir);
    };
    RobotAI.prototype.considerGoals = function () {
        // If the robot is doing something for the player, then ignore the goals:
        if (this.currentAction != null && this.currentAction_requester != null)
            return false;
        return _super.prototype.considerGoals.call(this);
    };
    RobotAI.prototype.restoreFromXML = function (xml) {
        _super.prototype.restoreFromXML.call(this, xml);
        this.robot.AI.doorsNotToOpenWhileWalking = this.doorsPlayerIsNotPermittedToOpen;
        this.objectsNotAllowedToGive = [];
        for (var _i = 0, _a = getElementChildrenByTag(xml, "objectsNotAllowedToGive"); _i < _a.length; _i++) {
            var onatg_xml = _a[_i];
            this.objectsNotAllowedToGive.push(onatg_xml.getAttribute("value"));
        }
        var ca_xml = getFirstElementChildByTag(xml, "currentAction");
        if (ca_xml == null) {
            this.currentAction = null;
        }
        else {
            this.currentAction = Term.fromString(ca_xml.getAttribute("value"), this.o);
        }
        var car_xml = getFirstElementChildByTag(xml, "currentAction_requester");
        if (car_xml == null) {
            this.currentAction_requester = null;
        }
        else {
            this.currentAction_requester = Term.parseAttribute(car_xml.getAttribute("value"), this.o, [], []);
        }
        this.currentAction_scriptQueue = null;
        var casq_xml = getFirstElementChildByTag(xml, "currentAction_scriptQueue");
        if (casq_xml != null) {
            var tmpq = null;
            var casq_xml_l = casq_xml.children;
            for (var j = 0; j < casq_xml_l.length; j++) {
                var script_xml = casq_xml_l[j];
                var s = A4Script.fromXML(script_xml);
                if (tmpq == null)
                    tmpq = new A4ScriptExecutionQueue(this.robot, this.robot.map, this.game, null);
                tmpq.scripts.push(s);
            }
            this.currentAction_scriptQueue = tmpq;
        }
        this.currentActionHandler = null;
        var cah_xml = getFirstElementChildByTag(xml, "currentActionHandler");
        if (cah_xml != null) {
            var ah_xml = getFirstElementChildByTag(cah_xml, "IntentionAction");
            if (ah_xml != null) {
                this.currentActionHandler = this.intentionActionFactory.loadFromXML(ah_xml, this);
            }
        }
    };
    RobotAI.prototype.savePropertiesToXML = function () {
        var str = _super.prototype.savePropertiesToXML.call(this);
        for (var _i = 0, _a = this.objectsNotAllowedToGive; _i < _a.length; _i++) {
            var o = _a[_i];
            str += "<objectsNotAllowedToGive value=\"" + o + "\"/>\n";
        }
        if (this.currentAction != null) {
            str += "<currentAction value=\"" + this.currentAction.toStringXML() + "\"/>\n";
        }
        if (this.currentAction_requester != null) {
            str += "<currentAction_requester value=\"" + this.currentAction_requester.toStringXML() + "\"/>\n";
        }
        if (this.currentAction_scriptQueue != null) {
            str += "<currentAction_scriptQueue>\n";
            for (var _b = 0, _c = this.currentAction_scriptQueue.scripts; _b < _c.length; _b++) {
                var s = _c[_b];
                str += s.saveToXML() + "\n";
            }
            str += "</currentAction_scriptQueue>\n";
        }
        if (this.currentActionHandler != null) {
            str += "<currentActionHandler>\n";
            str += this.currentActionHandler.saveToXML(this) + "\n";
            str += "</currentActionHandler>\n";
        }
        return str;
    };
    return RobotAI;
}(A4RuleBasedAI));
