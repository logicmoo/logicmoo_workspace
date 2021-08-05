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
var LOG_ACTIONS_IN_DEBUG_LOG = false;
var SHRDLU_INVENTORY_DISPLAY_SIZE = 12;
var SHRDLU_HUD_STATE_MESSAGES = 0;
var SHRDLU_HUD_STATE_MESSAGES_INPUT = 1;
var SHRDLU_HUD_STATE_INVENTORY = 2;
var SHRDLU_HUD_STATE_SPLIT_INVENTORY = 3;
var SHRDLU_MAX_SPACESUIT_OXYGEN = 8 * 50 * 60; // 8 minutes of game time
var COMMUNICATOR_CONNECTION_TIMEOUT = 50 * 60; // 1 minute of game time
var SHRDLU_START_DATE = 45186163200; // Thursday, October 21st, 2432
var SHRDLU_TILE_SIZE = 8;
var SPACE_NEAR_FAR_THRESHOLD = 12; // in meters
var EYESOPEN_SPEED = 180;
var ShrdluA4Game = /** @class */ (function (_super) {
    __extends(ShrdluA4Game, _super);
    function ShrdluA4Game(xml, game_path, ontology_path, GLTM, SFXM, a_sfx_volume, gender, app) {
        var _this = _super.call(this, xml, game_path, ontology_path, GLTM, SFXM, new ShrdluA4ObjectFactory(), a_sfx_volume) || this;
        _this.app = null;
        _this.naturalLanguageParser = null;
        _this.naturalLanguageGenerator = null;
        _this.etaoinAI = null;
        _this.qwertyAI = null;
        _this.shrdluAI = null;
        _this.playerAI = null;
        _this.communicatorConnectedTo = null;
        _this.communicatorConnectionTime = 0;
        _this.gameScript = null;
        _this.cutScenes = null;
        _this.textInputAllowed = false;
        _this.eyesClosedState = 0; // 0: eyes closed, 1: opening, 2: open, 3: closing
        _this.eyesClosedTimer = 0;
        _this.narrationMessages = [];
        _this.cutSceneActivated = -1;
        _this.introact_request = 0; // to notify the A4GameApp that an act is over, and we need to introduce the next one
        _this.gameover_request = 0; // to notify the A4GameApp that player is dead, and we need to go to the game over state
        _this.locations = [];
        _this.additional_location_connects = [];
        _this.playerGender = null;
        _this.suit_oxygen = SHRDLU_MAX_SPACESUIT_OXYGEN;
        _this.comm_tower_repaired = false;
        _this.rooms_with_lights = [];
        _this.rooms_with_lights_on = [];
        _this.aurora_station_temperature_sensor_indoors = 20;
        _this.aurora_station_temperature_sensor_outdoors = 44;
        // if these are != null, each time an AI executes an action, or a text bubble is created, it will be logged here:
        _this.debugActionLog = null;
        // debugTextBubbleLog:[number,string,A4TextBubble][] = null;  // this is already defined in A4Game
        // serverToken is immutable after initialization
        _this.serverToken = '';
        _this.playerGender = gender;
        _this.in_game_seconds = SHRDLU_START_DATE;
        _this.app = app;
        _this.rooms_with_lights = ["location-as4",
            "location-as5",
            "location-as6",
            "location-as7",
            "location-as8",
            "location-as9",
            "location-as10",
            "location-as11",
            "location-as12",
            "location-as13",
            "location-as14",
            "location-as15",
            "location-as16",
            "location-as17",
            "location-as18",
            "location-as19",
            "location-as20",
            "location-as21",
            "location-as22",
            "location-as23",
            "location-as24",
            "location-as25",
            "location-as26",
            "location-as27",
            "location-maintenance",
            "location-as29",
            "location-garage",
            "location-west-cave-dark",
            "location-east-cave-dark",
            "tardis8-bridge",
            "tardis8-computer",
            "tardis8-corridor-east",
            "tardis8-corridor-west",
            "tardis8-stasis1",
            "tardis8-stasis2",
            "tardis8-engineering",
        ];
        _this.rooms_with_lights_on = ["location-as4",
            "location-as5",
            "location-as6",
            "location-as7",
            "location-as8",
            "location-as9",
            "location-as10",
            "location-as11",
            "location-as12",
            "location-as13",
            "location-as14",
            "location-as15",
            "location-as16",
            "location-as17",
            "location-as18",
            "location-as19",
            "location-as20",
            "location-as21",
            //"location-as22",    // we start with all the storage rooms with the lights off
            //"location-as23",
            //"location-as24",
            "location-as25",
            "location-as26",
            "location-as27",
            //"location-maintenance",
            "location-as29",
            "location-garage",
        ];
        _this.three_d_printer_recipies = [["plastic-cup", ["plastic"]],
            ["plastic-plate", ["plastic"]],
            ["plastic-fork", ["plastic"]],
            ["plastic-spoon", ["plastic"]],
            ["plastic-knife", ["plastic"]],
            ["plastic-chopstick", ["plastic"]],
            ["screwdriver", ["plastic", "iron"]],
            ["pliers", ["plastic", "iron"]],
            ["wrench", ["plastic", "iron"]],
            ["cable", ["plastic", "copper"]],
            ["extension-cord", ["plastic", "copper"]],
        ];
        return _this;
    }
    ShrdluA4Game.prototype.loadContentFromXML = function (xml, game_path, ontology_path, GLTM, SFXM) {
        this.serverToken = xml.getAttribute("serverToken") || '';
        _super.prototype.loadContentFromXML.call(this, xml, game_path, ontology_path, GLTM, SFXM);
    };
    // if "saveGameXml" is != null, this is a call to restore from a save state
    ShrdluA4Game.prototype.finishLoadingGame = function (saveGameXml) {
        // overwrite spawned characters:
        if (this.playerGender == null) {
            // if no player gender specified, read it from the xml file:
            for (var _i = 0, _a = getElementChildrenByTag(this.xml, "variable"); _i < _a.length; _i++) {
                var variable_xml = _a[_i];
                var vname = variable_xml.getAttribute("name");
                if (vname == "playerGender")
                    this.playerGender = variable_xml.getAttribute("value");
            }
        }
        var players_xml = getElementChildrenByTag(this.xml, "player");
        for (var i = 0; i < players_xml.length; i++) {
            var player_xml = players_xml[i];
            if (this.playerGender == "female")
                player_xml.setAttribute("class", "susan");
            if (this.playerGender == "male")
                player_xml.setAttribute("class", "david");
        }
        _super.prototype.finishLoadingGame.call(this, saveGameXml);
        // load the location information
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.overrideMimeType("text/xml");
        xmlhttp.open("GET", "data/map-locations.xml", false);
        xmlhttp.send();
        AILocation.loadLocationsFromXML(xmlhttp.responseXML.documentElement, this, this.ontology);
        // create the natural language parser:
        xmlhttp = new XMLHttpRequest();
        xmlhttp.overrideMimeType("text/xml");
        xmlhttp.open("GET", "data/nlpatternrules.xml", false);
        xmlhttp.send();
        this.naturalLanguageParser = NLParser.fromXML(xmlhttp.responseXML.documentElement, this.ontology);
        this.naturalLanguageGenerator = new NLGenerator(this.ontology, this.naturalLanguageParser.posParser);
        this.naturalLanguageParser.talkingTargets = ["player", "shrdlu", "qwerty", "etaoin"];
        // load the AIs:
        this.etaoinAI = new EtaoinAI(this.ontology, this.naturalLanguageParser, [], this, ["data/general-kb.xml", "data/etaoin-kb.xml"]);
        this.playerAI = new PlayerAI(this.ontology, this.naturalLanguageParser, this.currentPlayer, [], this, ["data/general-kb.xml", "data/etaoin-kb.xml"]);
        this.qwertyAI = new QwertyAI(this.ontology, this.naturalLanguageParser, (this.findObjectByName("Qwerty")[0]), this, ["data/general-kb.xml", "data/qwerty-kb.xml"]);
        this.shrdluAI = new ShrdluAI(this.ontology, this.naturalLanguageParser, (this.findObjectByName("Shrdlu")[0]), this, ["data/general-kb.xml", "data/shrdlu-kb.xml"]);
        if (LOG_ACTIONS_IN_DEBUG_LOG) {
            this.debugActionLog = [];
            this.debugTextBubbleLog = [];
            this.etaoinAI.debugActionLog = this.debugActionLog;
            this.qwertyAI.debugActionLog = this.debugActionLog;
            this.shrdluAI.debugActionLog = this.debugActionLog;
            this.playerAI.debugActionLog = this.debugActionLog;
        }
        if (saveGameXml) {
            var ais_xml = getElementChildrenByTag(saveGameXml, "RuleBasedAI");
            this.etaoinAI.restoreFromXML(ais_xml[0]);
            this.qwertyAI.restoreFromXML(ais_xml[1]);
            this.shrdluAI.restoreFromXML(ais_xml[2]);
            this.playerAI.restoreFromXML(ais_xml[0]); // uses etaoin's
        }
        Sort.precomputeIsA();
        this.gameScript = new ShrdluGameScript(this, this.app);
        this.cutScenes = new ShrdluCutScenes(this, this.app);
        // preload the images:
        this.GLTM.get("data/cutscene-corpse1.png");
        this.GLTM.get("data/cutscene-diary1.png");
        this.GLTM.get("data/cutscene-poster1.png");
        this.GLTM.get("data/cutscene-death-oxygen.png");
        for (var _b = 0, _c = getElementChildrenByTag(this.xml, "variable"); _b < _c.length; _b++) {
            var variable_xml = _c[_b];
            var vname = variable_xml.getAttribute("name");
            if (vname == "serverToken")
                this.serverToken = variable_xml.getAttribute("value");
            if (vname == "communicatorConnectedTo")
                this.communicatorConnectedTo = variable_xml.getAttribute("value");
            if (vname == "communicatorConnectionTime")
                this.communicatorConnectionTime = Number(variable_xml.getAttribute("value"));
            if (vname == "textInputAllowed")
                this.textInputAllowed = variable_xml.getAttribute("value") == "true";
            if (vname == "eyesClosedState")
                this.eyesClosedState = Number(variable_xml.getAttribute("value"));
            if (vname == "eyesClosedTimer")
                this.eyesClosedTimer = Number(variable_xml.getAttribute("value"));
            if (vname == "cutSceneActivated")
                this.cutSceneActivated = Number(variable_xml.getAttribute("value"));
            if (vname == "introact_request")
                this.introact_request = Number(variable_xml.getAttribute("value"));
            if (vname == "gameover_request")
                this.gameover_request = Number(variable_xml.getAttribute("value"));
            if (vname == "in_game_seconds")
                this.in_game_seconds = Number(variable_xml.getAttribute("value"));
            if (vname == "suit_oxygen")
                this.suit_oxygen = Number(variable_xml.getAttribute("value"));
            if (vname == "comm_tower_repaired")
                this.comm_tower_repaired = variable_xml.getAttribute("value") == "true";
            if (vname == "narrationMessages") {
                for (var _d = 0, _e = getElementChildrenByTag(variable_xml, "message"); _d < _e.length; _d++) {
                    var tmp_xml = _e[_d];
                    this.narrationMessages.push(tmp_xml.firstChild.nodeValue);
                }
            }
            if (vname == "rooms_with_lights_on") {
                this.rooms_with_lights_on = [];
                for (var _f = 0, _g = getElementChildrenByTag(variable_xml, "room"); _f < _g.length; _f++) {
                    var tmp_xml = _g[_f];
                    this.rooms_with_lights_on.push(tmp_xml.firstChild.nodeValue);
                }
            }
            if (vname == "errorMessagesForLog") {
                for (var _h = 0, _j = getElementChildrenByTag(variable_xml, "message"); _h < _j.length; _h++) {
                    var tmp_xml = _j[_h];
                    var tmp_array = tmp_xml.firstChild.nodeValue.split("\t");
                    if (tmp_array[tmp_array.length - 1] == "")
                        tmp_array.splice(tmp_array.length - 1, 1);
                    this.errorMessagesForLog.push(tmp_array);
                }
            }
            if (vname == "inGameActionsForLog") {
                for (var _k = 0, _l = getElementChildrenByTag(variable_xml, "action"); _k < _l.length; _k++) {
                    var tmp_xml = _l[_k];
                    var tmp_array = tmp_xml.firstChild.nodeValue.split("\t");
                    if (tmp_array[tmp_array.length - 1] == "")
                        tmp_array.splice(tmp_array.length - 1, 1);
                    this.inGameActionsForLog.push(tmp_array);
                }
            }
        }
        for (var i = 0; i < this.maps.length; i++) {
            this.maps[i].recalculateLightsOnStatus(this.rooms_with_lights, this.rooms_with_lights_on, this.map_location_names[i]);
        }
        var script_e = getFirstElementChildByTag(this.xml, "ShrdluGameScript");
        if (script_e != null)
            this.gameScript.restoreFromXML(script_e);
        if (this.gameScript.act == "3") {
            this.loadTardis8LocationKnowledge();
        }
        // make sure SHRDLU knows how to go places for which it has to traverse multiple maps:
        this.shrdluAI.robot.AI.precomputeMap2mapPaths(this);
    };
    ShrdluA4Game.prototype.loadTardis8LocationKnowledge = function () {
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.overrideMimeType("text/xml");
        xmlhttp.open("GET", "data/map-locations-tardis.xml", false);
        xmlhttp.send();
        AILocation.loadLocationsFromXML(xmlhttp.responseXML.documentElement, this, this.ontology);
        this.etaoinAI.precalculateLocationKnowledge(this, this.ontology);
        this.shrdluAI.precalculateLocationKnowledge(this, this.ontology);
        this.qwertyAI.precalculateLocationKnowledge(this, this.ontology);
        this.playerAI.precalculateLocationKnowledge(this, this.ontology);
        this.getMap("Tardis 8").reevaluateVisibility();
        this.getMap("Tardis 8").recalculateLightsOnStatus(this.rooms_with_lights, this.rooms_with_lights_on, this.map_location_names[this.getMapIndex("Tardis 8")]);
    };
    ShrdluA4Game.prototype.getSaveGameXml = function () {
        return this.toSaveGameXml("saveName");
    };
    ShrdluA4Game.prototype.toSaveGameXml = function (saveName) {
        _super.prototype.saveGame.call(this, saveName);
        var complete_xmlString = "<SHRDLU_savegame>\n";
        var xmlString = this.saveToXML();
        console.log("A4Game.saveGame: game xmlString length " + xmlString.length);
        complete_xmlString += xmlString;
        for (var i = 0; i < this.maps.length; i++) {
            xmlString = this.maps[i].saveToXML(this);
            complete_xmlString += "\n\n\n" + xmlString;
            console.log("A4Game.saveGame: map " + i + " xmlString length " + xmlString.length);
        }
        xmlString = this.etaoinAI.saveToXML();
        complete_xmlString += "\n\n\n" + xmlString;
        console.log("A4Game.saveGame: etaoin xmlString length " + xmlString.length);
        xmlString = this.qwertyAI.saveToXML();
        complete_xmlString += "\n\n\n" + xmlString;
        console.log("A4Game.saveGame: qwerty xmlString length " + xmlString.length);
        xmlString = this.shrdluAI.saveToXML();
        complete_xmlString += "\n\n\n" + xmlString;
        console.log("A4Game.saveGame: shrdlu xmlString length " + xmlString.length);
        complete_xmlString += "</SHRDLU_savegame>";
        return complete_xmlString;
    };
    ShrdluA4Game.prototype.saveGame = function (saveName) {
        var complete_xmlString = this.toSaveGameXml(saveName);
        // save it:
        console.log("Size of sample is: " + complete_xmlString.length);
        var compressed = LZString.compressToUTF16(complete_xmlString);
        console.log("Size of compressed sample is: " + compressed.length);
        localStorage.setItem(A4SAVEGAME_STORAGE_KEY + "-" + saveName, compressed);
        // downloadStringAsFile(complete_xmlString, "A4Game-saveGame.xml");
        // savegame name:
        var seconds = Math.floor(this.cycle / 60) % 60;
        var minutes = Math.floor(this.cycle / (60 * 60)) % 60;
        var hours = Math.floor(this.cycle / (60 * 60 * 60));
        var name = hours + ":";
        if (minutes < 10) {
            name += "0" + minutes + ":";
        }
        else {
            name += minutes + ":";
        }
        if (seconds < 10) {
            name += "0" + seconds;
        }
        else {
            name += seconds;
        }
        localStorage.setItem(A4SAVEGAME_STORAGE_KEY + "-" + saveName + "-name", name);
    };
    ShrdluA4Game.prototype.saveToXMLInnerContent = function () {
        var xmlString = _super.prototype.saveToXMLInnerContent.call(this);
        xmlString += "<variable name=\"serverToken\" value=\"" + this.serverToken + "\"/>\n";
        if (this.communicatorConnectedTo != null) {
            xmlString += "<variable name=\"communicatorConnectedTo\" value=\"" + this.communicatorConnectedTo + "\"/>\n";
            xmlString += "<varuable name=\"communicatorConnectionTime\" value=\"" + this.communicatorConnectionTime + "\"/>\n";
        }
        // game variables
        xmlString += "<variable name=\"playerGender\" value=\"" + this.playerGender + "\"/>\n";
        xmlString += "<variable name=\"textInputAllowed\" value=\"" + this.textInputAllowed + "\"/>\n";
        xmlString += "<variable name=\"eyesClosedState\" value=\"" + this.eyesClosedState + "\"/>\n";
        xmlString += "<variable name=\"eyesClosedTimer\" value=\"" + this.eyesClosedTimer + "\"/>\n";
        xmlString += "<variable name=\"cutSceneActivated\" value=\"" + this.cutSceneActivated + "\"/>\n";
        xmlString += "<variable name=\"introact_request\" value=\"" + this.introact_request + "\"/>\n";
        xmlString += "<variable name=\"gameover_request\" value=\"" + this.gameover_request + "\"/>\n";
        xmlString += "<variable name=\"in_game_seconds\" value=\"" + this.in_game_seconds + "\"/>\n";
        xmlString += "<variable name=\"suit_oxygen\" value=\"" + this.suit_oxygen + "\"/>\n";
        xmlString += "<variable name=\"comm_tower_repaired\" value=\"" + this.comm_tower_repaired + "\"/>\n";
        xmlString += "<variable name=\"narrationMessages\">\n";
        for (var _i = 0, _a = this.narrationMessages; _i < _a.length; _i++) {
            var nm = _a[_i];
            xmlString += "<message>" + nm + "</message>\n";
        }
        xmlString += "</variable>\n";
        xmlString += "<variable name=\"rooms_with_lights_on\">\n";
        for (var _b = 0, _c = this.rooms_with_lights_on; _b < _c.length; _b++) {
            var r = _c[_b];
            xmlString += "<room>" + r + "</room>\n";
        }
        xmlString += "</variable>\n";
        xmlString += "<variable name=\"errorMessagesForLog\">\n";
        for (var _d = 0, _e = this.errorMessagesForLog; _d < _e.length; _d++) {
            var m = _e[_d];
            xmlString += "<message>";
            for (var _f = 0, m_1 = m; _f < m_1.length; _f++) {
                var tmp = m_1[_f];
                xmlString += tmp + "\t";
            }
            xmlString += "</message>\n";
        }
        xmlString += "</variable>\n";
        xmlString += "<variable name=\"inGameActionsForLog\">\n";
        for (var _g = 0, _h = this.inGameActionsForLog; _g < _h.length; _g++) {
            var m = _h[_g];
            xmlString += "<action>";
            for (var _j = 0, m_2 = m; _j < m_2.length; _j++) {
                var tmp = m_2[_j];
                xmlString += tmp + "\t";
            }
            xmlString += "</action>\n";
        }
        xmlString += "</variable>\n";
        xmlString += this.gameScript.saveToXML();
        return xmlString;
    };
    ShrdluA4Game.prototype.turnLightOn = function (room) {
        if (this.rooms_with_lights_on.indexOf(room) == -1) {
            this.rooms_with_lights_on.push(room);
            for (var i = 0; i < this.maps.length; i++) {
                this.maps[i].recalculateLightsOnStatus(this.rooms_with_lights, this.rooms_with_lights_on, this.map_location_names[i]);
            }
            return true;
        }
        else {
            return false;
        }
    };
    ShrdluA4Game.prototype.turnLightOff = function (room) {
        if (this.rooms_with_lights_on.indexOf(room) != -1) {
            this.rooms_with_lights_on.splice(this.rooms_with_lights_on.indexOf(room), 1);
            for (var i = 0; i < this.maps.length; i++) {
                this.maps[i].recalculateLightsOnStatus(this.rooms_with_lights, this.rooms_with_lights_on, this.map_location_names[i]);
            }
            return true;
        }
        else {
            return false;
        }
    };
    ShrdluA4Game.prototype.takeRoverOutOfTheGarage = function (rover, player) {
        // 1) spawn a new vehicle on the outside
        var newRover = this.objectFactory.createObject("driveable-rover", this, true, false);
        if (newRover == null)
            return false;
        newRover.ID = rover.ID;
        newRover.direction = 2;
        var map = this.getMap("Spacer Valley South");
        if (map == null)
            return false;
        if (!map.walkable(336, 408, 40, 40, newRover))
            return false;
        newRover.warp(336, 408 + 16, map);
        // 2) remove rover from the game
        rover.map.removeObject(rover);
        this.requestDeletion(rover);
        // 3) teleport the player, and any other robots that were inside, and embark
        player.warp(336, 408 + 16, map);
        player.embark(newRover);
        if (this.qwertyAI.robot.vehicle == rover) {
            this.qwertyAI.robot.disembark();
            this.qwertyAI.robot.warp(336, 408 + 16, map);
            this.qwertyAI.robot.embark(newRover);
        }
        if (this.shrdluAI.robot.vehicle == rover) {
            this.shrdluAI.robot.disembark();
            this.shrdluAI.robot.warp(336, 408 + 16, map);
            this.shrdluAI.robot.embark(newRover);
        }
        return true;
    };
    ShrdluA4Game.prototype.takeShuttleToTrantorCrater = function (shuttle, player) {
        // 1) spawn a new vehicle on the outside
        var newShuttle = this.objectFactory.createObject("driveable-shuttle", this, true, false);
        if (newShuttle == null)
            return false;
        newShuttle.ID = shuttle.ID;
        newShuttle.direction = A4_DIRECTION_LEFT;
        var map = this.getMap("Trantor Crater");
        if (map == null)
            return false;
        if (!map.walkable(57 * 8, 15 * 8, 40, 40, newShuttle))
            return false;
        newShuttle.warp(57 * 8, 15 * 8 + 16, map);
        // 2) remove shuttle from the game
        shuttle.map.removeObject(shuttle);
        this.requestDeletion(shuttle);
        // 3) teleport the player, and any other robots that were inside, and embark
        player.warp(57 * 8, 15 * 8 + 16, map);
        player.embark(newShuttle);
        if (this.qwertyAI.robot.vehicle == shuttle) {
            this.qwertyAI.robot.disembark();
            this.qwertyAI.robot.warp(57 * 8, 15 * 8 + 16, map);
            this.qwertyAI.robot.embark(newShuttle);
        }
        if (this.shrdluAI.robot.vehicle == shuttle) {
            this.shrdluAI.robot.disembark();
            this.shrdluAI.robot.warp(57 * 8, 15 * 8 + 16, map);
            this.shrdluAI.robot.embark(newShuttle);
        }
        return true;
    };
    ShrdluA4Game.prototype.putRoverBackInGarage = function (rover) {
        // 1) spawn a new vehicle on the garage
        var newRover = this.objectFactory.createObject("garage-rover", this, true, false);
        if (newRover == null)
            return false;
        newRover.ID = rover.ID;
        newRover.direction = 2;
        var map = this.getMap("Aurora Station");
        if (map == null)
            return false;
        if (!map.walkable(848, 72, 40, 40, newRover))
            return false;
        newRover.warp(848, 72 + 16, map);
        // 2) remove rover from the outside
        rover.disembark(this.currentPlayer);
        this.currentPlayer.state = A4CHARACTER_STATE_IDLE;
        this.currentPlayer.vehicle = null;
        rover.map.removeObject(rover);
        this.requestDeletion(rover);
        // 3) teleport the player, and any robots in the vehicle, and disembark
        this.currentPlayer.warp(848, 72 + 16, map);
        this.currentPlayer.embark(newRover);
        this.currentPlayer.disembark();
        if (this.qwertyAI.robot.vehicle == rover) {
            this.qwertyAI.robot.warp(848, 72 + 16, map);
            this.qwertyAI.robot.embark(newRover);
            this.qwertyAI.robot.disembark();
        }
        if (this.shrdluAI.robot.vehicle == rover) {
            this.shrdluAI.robot.warp(848, 72 + 16, map);
            this.shrdluAI.robot.embark(newRover);
            this.shrdluAI.robot.disembark();
        }
        return true;
    };
    ShrdluA4Game.prototype.takeShuttleFromTrantorCrater = function (shuttle) {
        // 1) spawn a new vehicle on the garage
        var newShuttle = this.objectFactory.createObject("garage-shuttle", this, true, false);
        if (newShuttle == null)
            return false;
        newShuttle.ID = shuttle.ID;
        newShuttle.direction = 2;
        var map = this.getMap("Aurora Station");
        if (map == null)
            return false;
        if (!map.walkable(848, 192, 40, 40, newShuttle))
            return false;
        newShuttle.warp(848, 192 + 16, map);
        // 2) remove shuttle from the outside
        shuttle.disembark(this.currentPlayer);
        this.currentPlayer.state = A4CHARACTER_STATE_IDLE;
        this.currentPlayer.vehicle = null;
        shuttle.map.removeObject(shuttle);
        this.requestDeletion(shuttle);
        // 3) teleport the player, and any robots in the vehicle, and disembark
        this.currentPlayer.warp(848, 192 + 16, map);
        this.currentPlayer.embark(newShuttle);
        this.currentPlayer.disembark();
        if (this.qwertyAI.robot.vehicle == shuttle) {
            this.qwertyAI.robot.warp(848, 192 + 16, map);
            this.qwertyAI.robot.embark(newShuttle);
            this.qwertyAI.robot.disembark();
        }
        if (this.shrdluAI.robot.vehicle == shuttle) {
            this.shrdluAI.robot.warp(848, 192 + 16, map);
            this.shrdluAI.robot.embark(newShuttle);
            this.shrdluAI.robot.disembark();
        }
        return true;
    };
    /*
    - Prevents the robots from accidentally going to a map that they do not have permission to
    - This is to avoid edge cases where the player finds some unforeseen edge case to skip through important story plot points
    */
    ShrdluA4Game.prototype.checkPermissionToWarp = function (character, target) {
        if (!_super.prototype.checkPermissionToWarp.call(this, character, target))
            return false;
        if (character.ID == "qwerty")
            return false;
        if (character.ID == "shrdlu") {
            if (character.map.name == "Aurora Station" ||
                character.map.name == "Aurora Station Outdoors") {
                if (this.getStoryStateVariable("permission-to-take-shrdlu") == "false" &&
                    character.map.name != "Aurora Station" &&
                    character.map.name != "Aurora Station Outdoors") {
                    return false;
                }
            }
        }
        return true;
    };
    ShrdluA4Game.prototype.updateNoAIs = function (k) {
        if (!this.updateInternal(k, [this.shrdluAI.robot.map]))
            return false;
        if (!this.updateInternalShrdlu(k, false))
            return false;
        this.cycle++;
        this.in_game_seconds++; // we keep a separate count from cycles, since in some game scenes, time might advance faster
        if (this.cycles_without_redrawing > 0)
            this.cycles_without_redrawing--;
        return true;
    };
    ShrdluA4Game.prototype.update = function (k) {
        if (!this.updateInternalShrdlu(k, true))
            return false;
        this.cycle++;
        this.in_game_seconds++; // we keep a separate count from cycles, since in some game scenes, time might advance faster
        if (this.cycles_without_redrawing > 0)
            this.cycles_without_redrawing--;
        return true;
    };
    ShrdluA4Game.prototype.updateInternalShrdlu = function (k, updateAIs) {
        if (this.cycle != 0) {
            // do not execute a story update on the first cycle, since that's when all the "onStart" methods are started,
            // which can mess up with the story
            this.gameScript.update();
        }
        if (this.cutSceneActivated >= 0) {
            if (this.cutScenes.update(this.cutSceneActivated, k)) {
                this.cutSceneActivated = -1;
            }
            else {
                return true;
            }
        }
        if (!_super.prototype.updateInternal.call(this, k, [this.shrdluAI.robot.map]))
            return false;
        if (updateAIs) {
            this.etaoinAI.update(this.in_game_seconds);
            this.qwertyAI.update(this.in_game_seconds);
            this.shrdluAI.update(this.in_game_seconds);
        }
        switch (this.eyesClosedState) {
            case 0:
                break;
            case 1:
                this.eyesClosedTimer++;
                if (this.eyesClosedTimer > EYESOPEN_SPEED) {
                    this.eyesClosedState = 2;
                    this.eyesClosedTimer = 0;
                }
                break;
            case 2:
                break;
            case 3:
                this.eyesClosedTimer++;
                if (this.eyesClosedTimer > EYESOPEN_SPEED) {
                    this.eyesClosedState = 0;
                    this.eyesClosedTimer = 0;
                }
                break;
        }
        if (this.getStoryStateVariable("spacesuit") == "helmet") {
            if (this.currentPlayer.map.name == "Aurora Station" ||
                this.currentPlayer.isInVehicle()) {
                if (this.suit_oxygen < SHRDLU_MAX_SPACESUIT_OXYGEN) {
                    this.suit_oxygen += 16;
                    this.suit_oxygen = Math.min(this.suit_oxygen, SHRDLU_MAX_SPACESUIT_OXYGEN);
                }
            }
            else {
                if (this.suit_oxygen > 0) {
                    if (this.currentPlayer.map.name == "Spacer Valley South" ||
                        this.currentPlayer.map.name == "Spacer Valley North") {
                        this.suit_oxygen -= 4;
                        this.suit_oxygen = Math.max(this.suit_oxygen, 0);
                    }
                    else {
                        this.suit_oxygen--;
                    }
                }
                else {
                    this.gameover_request = 1; // OUT Of OXYGEN!
                }
            }
        }
        else {
            if (this.suit_oxygen < SHRDLU_MAX_SPACESUIT_OXYGEN) {
                this.suit_oxygen += 16;
                this.suit_oxygen = Math.min(this.suit_oxygen, SHRDLU_MAX_SPACESUIT_OXYGEN);
            }
        }
        if (this.communicatorConnectedTo != null) {
            if ((this.etaoinAI.timeStamp - this.communicatorConnectionTime) > COMMUNICATOR_CONNECTION_TIMEOUT) {
                this.communicatorConnectedTo = null;
                this.communicatorConnectionTime = 0;
            }
        }
        return true;
    };
    ShrdluA4Game.prototype.draw = function (screen_width, screen_height) {
        var tileSize = (screen_height / 24);
        var split = Math.floor(tileSize * 17);
        if (this.cutSceneActivated >= 0) {
            this.cutScenes.draw(this.cutSceneActivated, screen_width, split);
            return;
        }
        _super.prototype.draw.call(this, screen_width, screen_height);
        var y = 8;
        for (var i = 0; i < this.narrationMessages.length; i++) {
            var width = this.narrationMessages[i].length * 6 * PIXEL_SIZE;
            fillTextTopLeft(this.narrationMessages[i], screen_width / 2 - width / 2, y, fontFamily32px, MSX_COLOR_WHITE);
            y += 8;
        }
    };
    ShrdluA4Game.prototype.drawWorld = function (screen_width, screen_height) {
        this.screen_width_last = screen_width;
        this.screen_height_last = screen_height;
        if (this.mouse_draw_skip > 0) {
            this.mouse_draw_skip--;
            if (this.mouse_draw_skip < 1) {
                console.log("mouse_draw_skip is done");
            }
        }
        if (this.currentPlayer != null) {
            var map = this.currentPlayer.map;
            var mapx = this.getCameraX(this.currentPlayer, map.width * this.tileWidth, screen_width);
            var mapy = this.getCameraY(this.currentPlayer, map.height * this.tileHeight, screen_height);
            var tx = Math.floor(this.currentPlayer.x / this.tileWidth);
            var ty = Math.floor(this.currentPlayer.y / this.tileHeight);
            map.drawRegion(mapx, mapy, this.zoom, screen_width, screen_height, map.visibilityRegion(tx, ty), this);
            this.drawEyesclosedCover(screen_width, screen_height);
            map.drawTextBubblesRegion(mapx, mapy, this.zoom, screen_width, screen_height, map.visibilityRegion(tx, ty), this);
        }
        else {
            /*
            let map:A4Map = this.maps[0];
            map.draw(0, 0, this.zoom,screen_width, screen_height, this);
            this.drawEyesclosedCover(screen_width, screen_height);
            map.drawTextBubbles(0, 0, this.zoom, screen_width, screen_height, this);
            */
        }
    };
    ShrdluA4Game.prototype.drawEyesclosedCover = function (screen_width, screen_height) {
        switch (this.eyesClosedState) {
            case 0:
                ctx.fillStyle = "black";
                ctx.fillRect(0, 0, screen_width, screen_height);
                break;
            case 1:
                {
                    var f = (EYESOPEN_SPEED - this.eyesClosedTimer) / EYESOPEN_SPEED;
                    if (f < 0)
                        f = 0;
                    if (f > 1)
                        f = 1;
                    var x = 4 * f - 1;
                    f = 0.125 * (5 + x - 3 * x * x + x * x * x);
                    if (f < 0)
                        f = 0;
                    if (f > 1)
                        f = 1;
                    f = Math.sqrt(f) / 2;
                    var height = Math.floor((screen_height / PIXEL_SIZE) * f) * PIXEL_SIZE;
                    ctx.fillStyle = "black";
                    ctx.fillRect(0, 0, screen_width, height);
                    ctx.fillRect(0, screen_height - height, screen_width, height);
                }
                break;
            case 2:
                break;
            case 3:
                {
                    var f = this.eyesClosedTimer / EYESOPEN_SPEED;
                    if (f < 0)
                        f = 0;
                    if (f > 1)
                        f = 1;
                    var x = 4 * f - 1;
                    f = 0.125 * (5 + x - 3 * x * x + x * x * x);
                    if (f < 0)
                        f = 0;
                    if (f > 1)
                        f = 1;
                    f = Math.sqrt(f) / 2;
                    var height = Math.floor((screen_height / PIXEL_SIZE) * f) * PIXEL_SIZE;
                    ctx.fillStyle = "black";
                    ctx.fillRect(0, 0, screen_width, height);
                    ctx.fillRect(0, screen_height - height, screen_width, height);
                }
                break;
        }
    };
    ShrdluA4Game.prototype.drawHUD = function (screen_width, screen_height, split) {
        _super.prototype.drawHUD.call(this, screen_width, screen_height, split);
        if (this.HUD_hseparator == null) {
            this.HUD_hseparator = this.GLTM.getPiece("data/GUI.png", 0, 0, 8, 8);
            this.HUD_vseparator = this.GLTM.getPiece("data/GUI.png", 0, 8, 8, 8);
            this.HUD_tseparator = this.GLTM.getPiece("data/GUI.png", 48, 0, 8, 8);
            this.HUD_uparrow1 = this.GLTM.getPiece("data/GUI.png", 8, 0, 8, 8);
            this.HUD_uparrow2 = this.GLTM.getPiece("data/GUI.png", 8, 8, 8, 8);
            this.HUD_downarrow1 = this.GLTM.getPiece("data/GUI.png", 16, 0, 8, 8);
            this.HUD_downarrow2 = this.GLTM.getPiece("data/GUI.png", 16, 8, 8, 8);
            this.HUD_button1 = this.GLTM.getPiece("data/GUI.png", 24, 0, 8, 8);
            this.HUD_button2 = this.GLTM.getPiece("data/GUI.png", 24, 8, 8, 8);
            this.HUD_oxygen = this.GLTM.getPiece("data/GUI.png", 0, 48, 40, 8);
            this.HUD_oxygen_bar = this.GLTM.getPiece("data/GUI.png", 32, 40, 32, 8);
        }
        if (this.HUD_hseparator != null) {
            for (var i = 0; i < screen_width; i += PIXEL_SIZE * 8) {
                this.HUD_hseparator.drawWithZoom(i, split, PIXEL_SIZE);
            }
        }
        if (this.HUD_button1 != null && this.HUD_button2 != null) {
            if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY ||
                this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
                this.HUD_button2.drawWithZoom(27 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
            }
            else {
                this.HUD_button1.drawWithZoom(27 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
            }
        }
        // we can only show either the inventory or the messags (not both):
        if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY) {
            // inventory:
            var inventoryMaxSize = SHRDLU_INVENTORY_DISPLAY_SIZE;
            if (this.currentPlayer.inventory.length <= inventoryMaxSize)
                this.HUD_inventory_start = 0;
            if (this.currentPlayer.selectedItem >= 0 &&
                this.currentPlayer.selectedItem < this.HUD_inventory_start) {
                this.HUD_inventory_start = this.currentPlayer.selectedItem - (this.currentPlayer.selectedItem % 2);
                //console.log("selected: " + this.currentPlayer.selectedItem + ", start: " + this.HUD_inventory_start);
            }
            if (this.currentPlayer.selectedItem >= this.HUD_inventory_start + inventoryMaxSize) {
                this.HUD_inventory_start = this.currentPlayer.selectedItem - (inventoryMaxSize - 1);
                this.HUD_inventory_start += (this.HUD_inventory_start % 2);
                //console.log("selected: " + this.currentPlayer.selectedItem + ", start: " + this.HUD_inventory_start);
            }
            // draw the inventory UI:
            this.HUD_tseparator.drawWithZoom(26 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
            for (var i = split + PIXEL_SIZE * 8; i < screen_height; i += PIXEL_SIZE * 8) {
                this.HUD_vseparator.drawWithZoom(26 * 8 * PIXEL_SIZE, i, PIXEL_SIZE);
            }
            if (!this.currentPlayer.isInVehicle()) {
                if (this.currentPlayer.selectedItem >= 0) {
                    var item = this.currentPlayer.inventory[this.currentPlayer.selectedItem];
                    if (item != null) {
                        if (item.droppable) {
                            fillTextTopLeft("  o ", 28 * 8 * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_LIGHT_GREEN);
                            fillTextTopLeft("Dr p", 28 * 8 * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_WHITE);
                        }
                        else {
                            fillTextTopLeft("Drop", 28 * 8 * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
                        }
                        if (item.usable) {
                            fillTextTopLeft("U  ", 28 * 8 * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_LIGHT_GREEN);
                            fillTextTopLeft(" se", 28 * 8 * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_WHITE);
                        }
                        else {
                            fillTextTopLeft("Use", 28 * 8 * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
                        }
                    }
                    else {
                        fillTextTopLeft("Drop", 28 * 8 * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
                        fillTextTopLeft("Use", 28 * 8 * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
                    }
                }
                else {
                    fillTextTopLeft("Drop", 28 * 8 * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
                    fillTextTopLeft("Use", 28 * 8 * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
                }
            }
            if (this.HUD_uparrow1 != null && this.HUD_uparrow2 != null) {
                if (this.HUD_inventory_start > 0) {
                    this.HUD_uparrow1.drawWithZoom(29 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_uparrow2.drawWithZoom(29 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            if (this.HUD_downarrow1 != null && this.HUD_downarrow2 != null) {
                if (this.HUD_inventory_start + inventoryMaxSize < this.currentPlayer.inventory.length) {
                    this.HUD_downarrow1.drawWithZoom(30 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_downarrow2.drawWithZoom(30 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            // draw the inventory:
            for (var i = 0; i < inventoryMaxSize &&
                i + this.HUD_inventory_start < this.currentPlayer.inventory.length; i++) {
                var x = (i % 2) * 8 * 14 * PIXEL_SIZE;
                var y = split + (1 + Math.floor(i / 2)) * 8 * PIXEL_SIZE;
                var item = this.currentPlayer.inventory[i + this.HUD_inventory_start];
                if (i + this.HUD_inventory_start == this.currentPlayer.selectedItem) {
                    ctx.fillStyle = MSX_COLOR_DARK_BLUE;
                    ctx.fillRect(x, y, 12 * 8 * PIXEL_SIZE, 8 * PIXEL_SIZE);
                }
                // item icon:
                var anim = item.getCurrentAnimation();
                if (anim != null) {
                    anim.drawWithZoom(x, y, this.zoom);
                }
                // item name:
                fillTextTopLeft(item.name, x + 2 * 8 * PIXEL_SIZE, y, fontFamily32px, MSX_COLOR_WHITE);
            }
        }
        else if (this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
            // inventory:
            var inventoryMaxSize = SHRDLU_INVENTORY_DISPLAY_SIZE / 2;
            var inventoryRemoteMaxSize = (SHRDLU_INVENTORY_DISPLAY_SIZE / 2) - 1;
            // player inventory scroll:
            if (this.currentPlayer.inventory.length <= inventoryMaxSize)
                this.HUD_inventory_start = 0;
            if (this.currentPlayer.selectedItem >= 0 &&
                this.currentPlayer.selectedItem < this.HUD_inventory_start) {
                //console.log("selected: " + this.currentPlayer.selectedItem + ", start: " + this.HUD_inventory_start);
            }
            if (this.currentPlayer.selectedItem >= this.HUD_inventory_start + inventoryMaxSize) {
                this.HUD_inventory_start = this.currentPlayer.selectedItem - (inventoryMaxSize - 1);
                //console.log("selected: " + this.currentPlayer.selectedItem + ", start: " + this.HUD_inventory_start);
            }
            // remote inventory scroll:
            if (this.HUD_remote_inventory.content.length <= inventoryRemoteMaxSize)
                this.HUD_remote_inventory_start = 0;
            if (this.HUD_remote_inventory_selected >= 0 &&
                this.HUD_remote_inventory_selected < this.HUD_remote_inventory_start) {
                this.HUD_remote_inventory_start = this.HUD_remote_inventory_selected;
                //console.log("selected: " + this.currentPlayer.selectedItem + ", start: " + this.HUD_inventory_start);
            }
            if (this.HUD_remote_inventory_selected >= this.HUD_remote_inventory_start + inventoryRemoteMaxSize) {
                this.HUD_remote_inventory_start = this.HUD_remote_inventory_selected - (inventoryRemoteMaxSize - 1);
                //console.log("selected: " + this.currentPlayer.selectedItem + ", start: " + this.HUD_inventory_start);
            }
            // draw the split inventory UI:
            this.HUD_tseparator.drawWithZoom(13 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
            this.HUD_tseparator.drawWithZoom(19 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
            for (var i = split + PIXEL_SIZE * 8; i < screen_height; i += PIXEL_SIZE * 8) {
                this.HUD_vseparator.drawWithZoom(13 * 8 * PIXEL_SIZE, i, PIXEL_SIZE);
                this.HUD_vseparator.drawWithZoom(19 * 8 * PIXEL_SIZE, i, PIXEL_SIZE);
            }
            if (this.HUD_remote_inventory_selected >= 0 &&
                this.HUD_remote_inventory.content[this.HUD_remote_inventory_selected].takeable) {
                fillTextTopLeft(" U ", (15 * 8 + 3) * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_LIGHT_GREEN);
                fillTextTopLeft("< <", (15 * 8 + 3) * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_WHITE);
            }
            else {
                fillTextTopLeft("<U<", (15 * 8 + 3) * PIXEL_SIZE, split + 2 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
            }
            if (this.currentPlayer.selectedItem >= 0) {
                fillTextTopLeft(" O ", (15 * 8 + 3) * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_LIGHT_GREEN);
                fillTextTopLeft("> >", (15 * 8 + 3) * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_WHITE);
            }
            else {
                fillTextTopLeft(">O>", (15 * 8 + 3) * PIXEL_SIZE, split + 4 * 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_DARK_BLUE);
            }
            // draw the inventory:
            if (this.HUD_uparrow1 != null && this.HUD_uparrow2 != null) {
                if (this.HUD_inventory_start > 0) {
                    this.HUD_uparrow1.drawWithZoom(10 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_uparrow2.drawWithZoom(10 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            if (this.HUD_downarrow1 != null && this.HUD_downarrow2 != null) {
                if (this.HUD_inventory_start + inventoryMaxSize < this.currentPlayer.inventory.length) {
                    this.HUD_downarrow1.drawWithZoom(11 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_downarrow2.drawWithZoom(11 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            for (var i = 0; i < inventoryMaxSize &&
                i + this.HUD_inventory_start < this.currentPlayer.inventory.length; i++) {
                var x = 0;
                var y = split + (1 + i) * 8 * PIXEL_SIZE;
                var item = this.currentPlayer.inventory[i + this.HUD_inventory_start];
                if (item == null)
                    continue;
                if (i + this.HUD_inventory_start == this.currentPlayer.selectedItem) {
                    ctx.fillStyle = MSX_COLOR_DARK_BLUE;
                    ctx.fillRect(x, y, 12 * 8 * PIXEL_SIZE, 8 * PIXEL_SIZE);
                }
                // item icon:
                var anim = item.getCurrentAnimation();
                if (anim != null) {
                    anim.drawWithZoom(x, y, this.zoom);
                }
                // item name:
                fillTextTopLeft(item.name, x + 2 * 8 * PIXEL_SIZE, y, fontFamily32px, MSX_COLOR_WHITE);
            }
            // draw the remote inventory:
            if (this.HUD_uparrow1 != null && this.HUD_uparrow2 != null) {
                if (this.HUD_remote_inventory_start > 0) {
                    this.HUD_uparrow1.drawWithZoom(29 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_uparrow2.drawWithZoom(29 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            if (this.HUD_downarrow1 != null && this.HUD_downarrow2 != null) {
                if (this.HUD_remote_inventory_start + inventoryRemoteMaxSize < this.HUD_remote_inventory.content.length) {
                    this.HUD_downarrow1.drawWithZoom(30 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_downarrow2.drawWithZoom(30 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            // name of the remote container:
            fillTextTopLeft(this.HUD_remote_inventory.name + ":", 20 * 8 * PIXEL_SIZE, split + 8 * PIXEL_SIZE, fontFamily32px, MSX_COLOR_WHITE);
            // inventory:
            for (var i = 0; i < inventoryRemoteMaxSize &&
                i + this.HUD_remote_inventory_start < this.HUD_remote_inventory.content.length; i++) {
                var x = 20 * 8 * PIXEL_SIZE;
                var y = split + (2 + i) * 8 * PIXEL_SIZE;
                var item = this.HUD_remote_inventory.content[i + this.HUD_remote_inventory_start];
                if (item == null)
                    continue;
                if (i + this.HUD_remote_inventory_start == this.HUD_remote_inventory_selected) {
                    ctx.fillStyle = MSX_COLOR_DARK_BLUE;
                    ctx.fillRect(x, y, 12 * 8 * PIXEL_SIZE, 8 * PIXEL_SIZE);
                }
                // item icon:
                var anim = item.getCurrentAnimation();
                if (anim != null) {
                    anim.drawWithZoom(x, y, this.zoom);
                }
                // item name:
                fillTextTopLeft(item.name, x + 2 * 8 * PIXEL_SIZE, y, fontFamily32px, MSX_COLOR_WHITE);
            }
        }
        else {
            // messages:
            var x = 0;
            var y = split + 8 * PIXEL_SIZE;
            var start = 0;
            if (this.console_first_message == -1) {
                start = this.messages.length - A4_N_MESSAGES_IN_HUD;
            }
            else {
                start = this.console_first_message;
            }
            if (start < 0)
                start = 0;
            ctx.fillStyle = "white";
            ctx.font = fontFamily32px;
            ctx.textBaseline = "top";
            ctx.textAlign = "left";
            for (var i = 0; i < A4_N_MESSAGES_IN_HUD && start + i < this.messages.length; i++) {
                ctx.fillStyle = this.messages[start + i][1];
                ctx.fillText(this.messages[start + i][0], x, y);
                y += 8 * PIXEL_SIZE;
            }
            if (this.HUD_uparrow1 != null && this.HUD_uparrow2 != null) {
                if (start > 0) {
                    this.HUD_uparrow1.drawWithZoom(29 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_uparrow2.drawWithZoom(29 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            if (this.HUD_downarrow1 != null && this.HUD_downarrow2 != null) {
                if (start + A4_N_MESSAGES_IN_HUD < this.messages.length) {
                    this.HUD_downarrow1.drawWithZoom(30 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
                else {
                    this.HUD_downarrow2.drawWithZoom(30 * 8 * PIXEL_SIZE, split, PIXEL_SIZE);
                }
            }
            if (this.HUD_state == SHRDLU_HUD_STATE_MESSAGES_INPUT) {
                // draw cursor:
                if ((this.cycle % 30) < 15) {
                    if (this.textInputAllowed &&
                        !this.anyoneTalking()) {
                        //                        !this.currentPlayer.isTalking()) {
                        ctx.fillStyle = MSX_COLOR_DARK_GREEN;
                    }
                    else {
                        ctx.fillStyle = MSX_COLOR_DARK_RED;
                    }
                    ctx.fillRect((this.HUD_text_input_cursor + 1) * 6 * PIXEL_SIZE, 184 * PIXEL_SIZE, 6 * PIXEL_SIZE, 8 * PIXEL_SIZE);
                }
                ctx.fillStyle = MSX_COLOR_LIGHT_GREEN;
            }
            else {
                ctx.fillStyle = MSX_COLOR_DARK_BLUE;
            }
            ctx.fillText(">" + this.HUD_text_input_buffer, 0, 184 * PIXEL_SIZE);
        }
        // oxygen bar:
        if (this.getStoryStateVariable("spacesuit") == "helmet") {
            // show oxygen bar:
            this.HUD_oxygen.drawWithZoom(23 * 8 * PIXEL_SIZE, 0, PIXEL_SIZE);
            this.HUD_oxygen_bar.drawWithZoom(28 * 8 * PIXEL_SIZE, 0, PIXEL_SIZE);
            var oxygen_bar_size = Math.floor((32 - 6) * (this.suit_oxygen / SHRDLU_MAX_SPACESUIT_OXYGEN));
            if (this.suit_oxygen < SHRDLU_MAX_SPACESUIT_OXYGEN * 0.2) {
                ctx.fillStyle = MSX_COLOR_RED;
            }
            else {
                ctx.fillStyle = MSX_COLOR_DARK_BLUE;
            }
            ctx.fillRect((28 * 8 + 3) * PIXEL_SIZE, 3 * PIXEL_SIZE, oxygen_bar_size * PIXEL_SIZE, PIXEL_SIZE);
        }
        // when any of the AIs is thinking:
        if ((this.cycle % 32) < 16) {
            var thinkingY = 128;
            if (this.etaoinAI.currentInferenceProcess != null) {
                // etaoin is thinking:
                ctx.fillStyle = MSX_COLOR_BLACK;
                ctx.fillRect(2 * PIXEL_SIZE, thinkingY * PIXEL_SIZE, 130 * PIXEL_SIZE, 10 * PIXEL_SIZE);
                ctx.fillStyle = MSX_COLOR_WHITE;
                ctx.fillText("Etaoin is thinking...", 2 * PIXEL_SIZE, (thinkingY + 1) * PIXEL_SIZE);
                thinkingY -= 10;
            }
            if (this.playerAI.currentInferenceProcess != null) {
                // player is thinking:
                ctx.fillStyle = MSX_COLOR_BLACK;
                ctx.fillRect(2 * PIXEL_SIZE, thinkingY * PIXEL_SIZE, 130 * PIXEL_SIZE, 10 * PIXEL_SIZE);
                ctx.fillStyle = MSX_COLOR_WHITE;
                ctx.fillText("playerAI is thinking...", 2 * PIXEL_SIZE, (thinkingY + 1) * PIXEL_SIZE);
                thinkingY -= 10;
            }
            if (this.qwertyAI.currentInferenceProcess != null) {
                // etaoin is thinking:
                ctx.fillStyle = MSX_COLOR_BLACK;
                ctx.fillRect(2 * PIXEL_SIZE, thinkingY * PIXEL_SIZE, 130 * PIXEL_SIZE, 10 * PIXEL_SIZE);
                ctx.fillStyle = MSX_COLOR_WHITE;
                ctx.fillText("Qwerty is thinking...", 2 * PIXEL_SIZE, (thinkingY + 1) * PIXEL_SIZE);
                thinkingY -= 10;
            }
            if (this.shrdluAI.currentInferenceProcess != null) {
                // etaoin is thinking:
                ctx.fillStyle = MSX_COLOR_BLACK;
                ctx.fillRect(2 * PIXEL_SIZE, thinkingY * PIXEL_SIZE, 130 * PIXEL_SIZE, 10 * PIXEL_SIZE);
                ctx.fillStyle = MSX_COLOR_WHITE;
                ctx.fillText("Shrdlu is thinking...", 2 * PIXEL_SIZE, (thinkingY + 1) * PIXEL_SIZE);
                thinkingY -= 10;
            }
        }
    };
    ShrdluA4Game.prototype.anyoneTalking = function () {
        if (this.currentPlayer.isTalking())
            return true;
        if (this.qwertyAI.robot.isTalking())
            return true;
        if (this.shrdluAI.robot.isTalking())
            return true;
        if (this.currentPlayer.map.textBubbles.length != 0)
            return true;
        return false;
    };
    ShrdluA4Game.prototype.getAILocation = function (o) {
        var map = o.map;
        if (map == null) {
            var tmp = this.findObjectByID(o.ID);
            if (tmp == null || tmp.length == 0)
                return null;
            if (tmp[0].map == null)
                return null;
            map = tmp[0].map;
            o = tmp[0];
        }
        var tile_x = Math.floor((o.x + o.getPixelWidth() / 2) / map.tileWidth);
        var tile_y = Math.floor((o.y + o.getPixelHeight() / 2) / map.tileHeight);
        return this.getAILocationTileCoordinate(map, tile_x, tile_y);
    };
    ShrdluA4Game.prototype.getAILocationTileCoordinate = function (map, tile_x, tile_y) {
        var offset = tile_x + tile_y * map.width;
        var location = null;
        var location_idx = -1;
        for (var location_idx2 = 0; location_idx2 < this.locations.length; location_idx2++) {
            var l = this.locations[location_idx2];
            for (var i = 0; i < l.maps.length; i++) {
                if (l.maps[i] == map) {
                    if (l.mapOccupancyMaps[i][offset]) {
                        if (location == null) {
                            location = l;
                            location_idx = location_idx2;
                        }
                        else {
                            if (this.location_in[location_idx2][location_idx]) {
                                location = l;
                                location_idx = location_idx2;
                            }
                        }
                    }
                }
            }
        }
        return location;
    };
    ShrdluA4Game.prototype.getAILocationByID = function (id) {
        for (var _i = 0, _a = this.locations; _i < _a.length; _i++) {
            var location_1 = _a[_i];
            if (location_1.id == id)
                return location_1;
        }
        return null;
    };
    ShrdluA4Game.prototype.skipSpeechBubble = function () {
        if (this.cutSceneActivated >= 0) {
            this.cutScenes.ESCPressed(this.cutSceneActivated);
            return true;
        }
        if (_super.prototype.skipSpeechBubble.call(this))
            return;
        for (var _i = 0, _a = [this.qwertyAI.robot, this.shrdluAI.robot]; _i < _a.length; _i++) {
            var character = _a[_i];
            if (character.talkingBubble != null) {
                character.stateCycle = character.talkingBubbleDuration;
                return true;
            }
        }
        return false;
    };
    ShrdluA4Game.prototype.textInputRequest = function () {
        if (this.textInputAllowed)
            this.HUD_state = SHRDLU_HUD_STATE_MESSAGES_INPUT;
    };
    ShrdluA4Game.prototype.textInputExit = function () {
        this.HUD_state = SHRDLU_HUD_STATE_MESSAGES;
    };
    ShrdluA4Game.prototype.textInputSubmit = function (SFXM) {
        if (!this.textInputAllowed ||
            this.anyoneTalking()) {
            //            SFXM.play("data/sfx/beep.wav");
            return;
        }
        //        if (this.currentPlayer.isTalking()) return;
        this.HUD_state = SHRDLU_HUD_STATE_MESSAGES;
        if (this.HUD_text_input_buffer != "") {
            /*
            this.currentPlayer.map.addPerceptionBufferRecord(
                new PerceptionBufferRecord("talk", this.currentPlayer.ID, this.currentPlayer.sort,
                                           null, null, this.HUD_text_input_buffer,
                                           null, null,
                                           this.currentPlayer.x, this.currentPlayer.y, this.currentPlayer.x+this.currentPlayer.getPixelWidth(), this.currentPlayer.y+this.currentPlayer.getPixelHeight()));

            this.addMessageWithColor(">"+this.HUD_text_input_buffer, MSX_COLOR_LIGHT_GREEN);
            */
            this.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_TALK, this.HUD_text_input_buffer, A4_DIRECTION_NONE, this);
            this.inputBufferHistory.push(this.HUD_text_input_buffer);
            this.lastInputBufferBeforeBrowsingHistory = null;
            this.inputBufferHistory_position = -1;
            this.HUD_text_input_buffer = "";
            this.HUD_text_input_cursor = 0;
        }
    };
    ShrdluA4Game.prototype.textInputEvent = function (e, SFXM) {
        var textInputLimit = 40;
        //        console.log("key: " + e.key + ", keyCode:" + e.keyCode + ", modifiers: " + e.getModifierState("Shift") + " | " + e.getModifierState("CapsLock"));
        if (e.key.length == 1 && this.HUD_text_input_buffer.length <= textInputLimit) {
            if ((e.key >= 'a' && e.key <= 'z') ||
                (e.key >= 'A' && e.key <= 'Z') ||
                (e.key >= '0' && e.key <= '9') ||
                e.key == ' ' ||
                e.key == ',' ||
                e.key == '.' ||
                e.key == '\'' ||
                e.key == '?' ||
                e.key == '!' ||
                e.key == '-' ||
                // just for entering logic:
                e.key == '#' ||
                e.key == ':' ||
                e.key == '(' ||
                e.key == ')' ||
                e.key == '[' ||
                e.key == ']') {
                if (this.HUD_text_input_cursor == this.HUD_text_input_buffer.length) {
                    this.HUD_text_input_buffer += e.key;
                    this.HUD_text_input_cursor++;
                }
                else {
                    this.HUD_text_input_buffer = this.HUD_text_input_buffer.substring(0, this.HUD_text_input_cursor) +
                        e.key +
                        this.HUD_text_input_buffer.substring(this.HUD_text_input_cursor);
                    this.HUD_text_input_cursor++;
                }
            }
        }
        else if (e.key == "ArrowRight") {
            if (this.HUD_text_input_cursor < this.HUD_text_input_buffer.length)
                this.HUD_text_input_cursor++;
        }
        else if (e.key == "ArrowLeft") {
            if (this.HUD_text_input_cursor > 0)
                this.HUD_text_input_cursor--;
        }
        else if (e.key == "ArrowUp") {
            if (this.inputBufferHistory_position == -1) {
                this.lastInputBufferBeforeBrowsingHistory = this.HUD_text_input_buffer;
                if (this.inputBufferHistory.length > 0) {
                    this.inputBufferHistory_position = this.inputBufferHistory.length - 1;
                    this.HUD_text_input_buffer = this.inputBufferHistory[this.inputBufferHistory_position];
                    this.HUD_text_input_cursor = this.HUD_text_input_buffer.length;
                }
            }
            else {
                if (this.inputBufferHistory_position > 0) {
                    this.inputBufferHistory_position--;
                    this.HUD_text_input_buffer = this.inputBufferHistory[this.inputBufferHistory_position];
                    this.HUD_text_input_cursor = this.HUD_text_input_buffer.length;
                }
            }
        }
        else if (e.key == "ArrowDown") {
            if (this.inputBufferHistory_position >= 0 && this.inputBufferHistory_position < this.inputBufferHistory.length - 1) {
                this.inputBufferHistory_position++;
                this.HUD_text_input_buffer = this.inputBufferHistory[this.inputBufferHistory_position];
                this.HUD_text_input_cursor = this.HUD_text_input_buffer.length;
            }
            else {
                if (this.inputBufferHistory_position >= 0 && this.inputBufferHistory_position == this.inputBufferHistory.length - 1) {
                    this.HUD_text_input_buffer = this.lastInputBufferBeforeBrowsingHistory;
                    this.lastInputBufferBeforeBrowsingHistory = null;
                    this.inputBufferHistory_position = -1;
                    this.HUD_text_input_cursor = this.HUD_text_input_buffer.length;
                }
            }
        }
        else if (e.key == "Backspace") {
            if (this.HUD_text_input_cursor > 0) {
                if (this.HUD_text_input_cursor == this.HUD_text_input_buffer.length) {
                    this.HUD_text_input_cursor--;
                    this.HUD_text_input_buffer = this.HUD_text_input_buffer.substring(0, this.HUD_text_input_cursor);
                }
                else {
                    this.HUD_text_input_cursor--;
                    this.HUD_text_input_buffer = this.HUD_text_input_buffer.substring(0, this.HUD_text_input_cursor) +
                        this.HUD_text_input_buffer.substring(this.HUD_text_input_cursor + 1);
                }
            }
        }
        else if (e.key == "Delete") {
            if (this.HUD_text_input_cursor < this.HUD_text_input_buffer.length) {
                this.HUD_text_input_buffer = this.HUD_text_input_buffer.substring(0, this.HUD_text_input_cursor) +
                    this.HUD_text_input_buffer.substring(this.HUD_text_input_cursor + 1);
            }
        }
        if (this.HUD_text_input_cursor > textInputLimit) {
            this.HUD_text_input_cursor = textInputLimit;
            SFXM.play("data/sfx/beep.wav");
        }
    };
    ShrdluA4Game.prototype.checkCustomVehicleCollisionEvents = function (vehicle) {
        //  check if we have collided with the station, and get the rover into the garage:
        if (vehicle.map.name == "Spacer Valley South" &&
            ((vehicle.direction == A4_DIRECTION_LEFT && vehicle.x == 38 * vehicle.map.tileWidth &&
                vehicle.y >= 47 * vehicle.map.tileHeight && vehicle.y <= 60 * vehicle.map.tileHeight) ||
                (vehicle.direction == A4_DIRECTION_RIGHT && vehicle.x == 28 * vehicle.map.tileWidth &&
                    vehicle.y >= 47 * vehicle.map.tileHeight && vehicle.y <= 60 * vehicle.map.tileHeight) ||
                (vehicle.direction == A4_DIRECTION_DOWN && vehicle.y == 47 * vehicle.map.tileHeight &&
                    vehicle.x >= 29 * vehicle.map.tileWidth && vehicle.x <= 37 * vehicle.map.tileWidth) ||
                (vehicle.direction == A4_DIRECTION_UP && vehicle.y == 54 * vehicle.map.tileHeight &&
                    vehicle.x >= 29 * vehicle.map.tileWidth && vehicle.x <= 37 * vehicle.map.tileWidth))) {
            if (!this.putRoverBackInGarage(vehicle)) {
                this.addMessage("There is something in the garage blocking the parking spot!");
            }
        }
        // check if we need to go back to the station with the shuttle:
        if (vehicle.map.name == "Trantor Crater" &&
            vehicle.direction == A4_DIRECTION_RIGHT && vehicle.x == 59 * vehicle.map.tileWidth) {
            // Make sure Shrdlu is in the shuttle, just in case:
            if (vehicle.load.indexOf(this.shrdluAI.robot) == -1) {
                this.currentPlayer.issueCommandWithString(A4CHARACTER_COMMAND_THOUGHT_BUBBLE, "I cannot leave Shrdlu behind!", A4_DIRECTION_NONE, this);
            }
            else {
                if (!this.takeShuttleFromTrantorCrater(vehicle)) {
                    this.addMessage("There is something in the garage blocking the parking spot!");
                }
                else {
                    this.cutSceneActivated = CUTSCENE_SHUTTLE_LAND;
                }
            }
        }
    };
    return ShrdluA4Game;
}(A4Game));
