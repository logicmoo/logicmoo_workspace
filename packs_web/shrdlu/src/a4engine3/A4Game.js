var A4_HASH_SIZE = 1000;
var A4_DIRECTION_NONE = -1;
var A4_DIRECTION_LEFT = 0;
var A4_DIRECTION_UP = 1;
var A4_DIRECTION_RIGHT = 2;
var A4_DIRECTION_DOWN = 3;
var A4_NDIRECTIONS = 4;
var A4_ANIMATION_IDLE = 0;
var A4_ANIMATION_IDLE_LEFT = 1;
var A4_ANIMATION_IDLE_UP = 2;
var A4_ANIMATION_IDLE_RIGHT = 3;
var A4_ANIMATION_IDLE_DOWN = 4;
var A4_ANIMATION_MOVING = 5;
var A4_ANIMATION_MOVING_LEFT = 6;
var A4_ANIMATION_MOVING_UP = 7;
var A4_ANIMATION_MOVING_RIGHT = 8;
var A4_ANIMATION_MOVING_DOWN = 9;
var A4_ANIMATION_INTERACTING = 10;
var A4_ANIMATION_INTERACTING_LEFT = 11;
var A4_ANIMATION_INTERACTING_UP = 12;
var A4_ANIMATION_INTERACTING_RIGHT = 13;
var A4_ANIMATION_INTERACTING_DOWN = 14;
var A4_ANIMATION_TALKING = 15;
var A4_ANIMATION_TALKING_LEFT = 16;
var A4_ANIMATION_TALKING_UP = 17;
var A4_ANIMATION_TALKING_RIGHT = 18;
var A4_ANIMATION_TALKING_DOWN = 19;
var A4_ANIMATION_DEATH = 32;
var A4_ANIMATION_DEATH_LEFT = 21;
var A4_ANIMATION_DEATH_UP = 22;
var A4_ANIMATION_DEATH_RIGHT = 23;
var A4_ANIMATION_DEATH_DOWN = 24;
var A4_ANIMATION_CLOSED = 25;
var A4_ANIMATION_IDLE_FULL = 26;
var A4_ANIMATION_CLOSED_FULL = 27;
var A4_N_ANIMATIONS = 28;
// aliases:
var A4_ANIMATION_OPEN = 0;
var A4_ANIMATION_IDLE_EMPTY = 0;
var A4_ANIMATION_OPEN_EMPTY = 0;
var A4_ANIMATION_OPEN_FULL = 36;
var A4_ANIMATION_CLOSED_EMPTY = 35;
//var A4_LAYER_BG:number = 0;
//var A4_LAYER_FG:number = 1;
//var A4_LAYER_CHARACTERS:number = 2;
var A4_N_LAYERS = 4;
var A4_TILE_WALKABLE = 0;
var A4_TILE_WALL = 1;
var A4_TILE_TREE = 2;
var A4_TILE_CHOPPABLE_TREE = 3;
var A4_TILE_WATER = 4;
//var A4_INVENTORY_SIZE:number = 8;
var A4_INVENTORY_SIZE = 256; // some very large number
var A4_N_MESSAGES_IN_HUD = 5;
var A4_MAX_MESSAGE_LENGTH = 42;
var CYCLES_IN_PERCEPTION_BUFFER = 50;
var TEXT_INITIAL_DELAY = 60;
var TEXT_SPEED = 8;
var animationNames = [
    "idle",
    "idle-left",
    "idle-up",
    "idle-right",
    "idle-down",
    "moving",
    "moving-left",
    "moving-up",
    "moving-right",
    "moving-down",
    "interacting",
    "interacting-left",
    "interacting-up",
    "interacting-right",
    "interacting-down",
    "talking",
    "talking-left",
    "talking-up",
    "talking-right",
    "talking-down",
    "death",
    "death-left",
    "death-up",
    "death-right",
    "death-down",
    "closed",
    "full",
    "closed-full"
];
var direction_x_inc = [-1, 0, 1, 0];
var direction_y_inc = [0, -1, 0, 1];
var WarpRequest = /** @class */ (function () {
    function WarpRequest(o, map, x, y) {
        this.o = o;
        this.map = map;
        this.x = x;
        this.y = y;
        //        this.layer = layer;
    }
    return WarpRequest;
}());
var A4Game = /** @class */ (function () {
    function A4Game(xml, game_path, ontology_path, GLTM, SFXM, a4of, a_sfx_volume) {
        this.xml = null; // the XML definition of the game
        this.drawTextBubbles = true;
        this.gameName = null;
        this.gameTitle = null;
        this.gameSubtitle = null;
        this.gameTitleImage = null;
        this.storyText = null;
        this.endingIDs = [];
        this.endingTexts = [];
        this.allowSaveGames = false;
        this.characterDefinitionFiles = [];
        this.objectDefinitionFiles = [];
        this.cycle = 0;
        this.cycles_without_redrawing = 0;
        this.in_game_seconds = 0;
        this.gameComplete = false;
        this.gameComplete_ending_ID = null;
        this.game_path = null;
        this.GLTM = null;
        this.SFXM = null;
        this.graphicFiles = [];
        this.objectFactory = null;
        this.tileWidth = 16;
        this.tileHeight = 16;
        this.mapTiles = {};
        // AI:
        this.ontology = new Ontology();
        // HUD:
        this.HUD_state = SHRDLU_HUD_STATE_MESSAGES;
        this.messages = []; // [text, color, timestamp]
        this.trade_requested = null;
        this.console_first_message = -1;
        this.HUD_hseparator = null;
        this.HUD_vseparator = null;
        this.HUD_tseparator = null;
        this.HUD_uparrow1 = null;
        this.HUD_uparrow2 = null;
        this.HUD_downarrow1 = null;
        this.HUD_downarrow2 = null;
        this.HUD_button1 = null;
        this.HUD_button2 = null;
        this.HUD_oxygen = null;
        this.HUD_oxygen_bar = null;
        this.HUD_inventory_start = 0;
        this.HUD_remote_inventory_start = 0;
        this.HUD_remote_inventory = null;
        this.HUD_remote_inventory_selected = -1;
        this.HUD_text_input_buffer = "";
        this.HUD_text_input_cursor = 0;
        this.inputBufferHistory = []; // messages typed by the player, so that she can browse it quickly ussing up/down
        this.inputBufferHistory_position = -1;
        this.lastInputBufferBeforeBrowsingHistory = null;
        this.maps = [];
        this.players = [];
        this.currentPlayer = null;
        this.currentPlayerIndex = 0;
        this.warpRequests = [];
        this.deletionRequests = [];
        // camera:
        this.zoom = 1;
        this.targetZoom = 1;
        this.defaultZoom = 1;
        // scripts:
        this.eventScripts = new Array(A4_NEVENTS);
        // script excution queues (these contain scripts that are pending execution, will be executed in the next "cycle"):
        this.scriptQueues = [];
        // story state:
        this.storyState = {};
        this.lastTimeStoryStateChanged = 0;
        // error logging (the engine just logs them into these arrays, it's up to the specific game to do
        // something with these):
        this.errorMessagesForLog = [];
        this.inGameActionsForLog = [];
        this.debugTextBubbleLog = null; // this is already defined in A4Game
        this.screen_width_last = -1;
        this.screen_height_last = -1;
        this.mouse_x_last = -1;
        this.mouse_y_last = -1;
        this.mouse_draw_skip = -1;
        // LOGICMOO ADDED
        window['theA4Game'] = this;
        this.objectFactory = a4of;
        this.loadContentFromXML(xml, game_path, ontology_path, GLTM, SFXM);
        this.sfx_volume = a_sfx_volume;
    }
    A4Game.prototype.loadContentFromXML = function (xml, game_path, ontology_path, GLTM, SFXM) {
        this.ontology = new Ontology();
        Sort.clear();
        this.screen_height_last = 1;
        this.screen_width_last = 1;
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.overrideMimeType("text/xml");
        xmlhttp.open("GET", ontology_path, false);
        xmlhttp.send();
        this.ontology.loadSortsFromXML(xmlhttp.responseXML.documentElement);
        A4Object.s_nextID = 10000;
        this.game_path = game_path;
        this.GLTM = GLTM;
        this.SFXM = SFXM;
        this.xml = xml;
        this.gameName = xml.getAttribute("name");
        this.gameTitle = xml.getAttribute("title");
        this.gameSubtitle = xml.getAttribute("subtitle");
        console.log("game name: " + this.gameName);
        console.log("game title: " + this.gameTitle);
        console.log("game subtitle: " + this.gameSubtitle);
        var title_xml = getFirstElementChildByTag(xml, "titleImage");
        if (title_xml != null) {
            console.log("Title image:" + title_xml.firstChild.nodeValue);
            this.gameTitleImage = title_xml.firstChild.nodeValue;
        }
        var tmp = xml.getAttribute("allowSaveGames");
        if (tmp != null)
            this.allowSaveGames = (tmp == "true");
        if (xml.getAttribute("cycle") != null) {
            this.cycle = Number(xml.getAttribute("cycle"));
        }
        var story_xml = getFirstElementChildByTag(xml, "story");
        if (story_xml != null) {
            var story_lines = getElementChildrenByTag(story_xml, "line");
            this.storyText = [];
            for (var i = 0; i < story_lines.length; i++) {
                var line_xml = story_lines[i];
                this.storyText.push(line_xml.firstChild.nodeValue);
            }
        }
        var ending_xmls = getElementChildrenByTag(xml, "ending");
        for (var i = 0; i < ending_xmls.length; i++) {
            var ending_xml = ending_xmls[i];
            var ID = ending_xml.getAttribute("id");
            if (ID == null) {
                console.log("Ending in game definition file does not specify an ID!");
                continue;
            }
            var ending_lines = getElementChildrenByTag(ending_xml, "line");
            var endingText = [];
            for (var i_1 = 0; i_1 < ending_lines.length; i_1++) {
                var line_xml = ending_lines[i_1];
                endingText.push(line_xml.firstChild.nodeValue);
            }
            this.addEnding(ID, endingText);
        }
        var tiles_xml = getFirstElementChildByTag(xml, "tiles");
        var targetwidth = Number(tiles_xml.getAttribute("targetwidth"));
        this.tileWidth = Number(tiles_xml.getAttribute("sourcewidth"));
        this.tileHeight = Number(tiles_xml.getAttribute("sourceheight"));
        this.defaultZoom = targetwidth / this.tileWidth;
        this.zoom = this.targetZoom = this.defaultZoom;
        // In a first pass, we just trigger loading all the images (since browser games load them asynchronously)
        // later in finishLoadingGame, the rest of the game is loaded...
        for (var _i = 0, _a = getElementChildrenByTag(tiles_xml, "graphicFile"); _i < _a.length; _i++) {
            var graphifile_xml = _a[_i];
            var file = graphifile_xml.getAttribute("file");
            var gf = this.getGraphicFile(file);
            if (gf == null) {
                gf = new A4GraphicFile(file, this.tileWidth, this.tileHeight, this.game_path, this.GLTM);
                this.graphicFiles.push(gf);
            }
            else {
                console.error("Cannot find graphic file: " + file);
            }
        }
        // for(let i:number = 0;i<tiles_xml.children.length;i++) {
        //     let c:Element = tiles_xml.children[i];
        //     let file:string = c.getAttribute("file");
        //     let gf:A4GraphicFile = this.getGraphicFile(file);
        //     if (gf == null) {
        //         gf = new A4GraphicFile(file, this.tileWidth, this.tileHeight, this.game_path, this.GLTM);
        //         this.graphicFiles.push(gf);
        //     }
        // }
    };
    A4Game.prototype.imagesLoaded = function () {
        for (var _i = 0, _a = this.graphicFiles; _i < _a.length; _i++) {
            var gf = _a[_i];
            if (gf.img.width == 0)
                return false;
        }
        return true;
    };
    // if "saveGameXml" is != null, this is a call to restore from a save state
    A4Game.prototype.finishLoadingGame = function (saveGameXml) {
        var tiles_xml = getFirstElementChildByTag(this.xml, "tiles");
        for (var _i = 0, _a = getElementChildrenByTag(tiles_xml, "tile"); _i < _a.length; _i++) {
            var tile_xml = _a[_i];
            var tile = A4MapTile.loadFromXML(tile_xml, this);
            if (tile.ID in this.mapTiles) {
                console.error("redefining tile:" + tile.ID);
            }
            this.mapTiles[tile.ID] = tile;
        }
        for (var _b = 0, _c = getElementChildrenByTag(tiles_xml, "graphicFile"); _b < _c.length; _b++) {
            var graphifile_xml = _c[_b];
            var file = graphifile_xml.getAttribute("file");
            var gf = this.getGraphicFile(file);
            if (gf.tiles == null)
                gf.updateAfterLoaded();
        }
        /*
        for(let idx:number = 0;idx<tiles_xml.children.length;idx++) {
            let c:Element = tiles_xml.children[idx];
            let file:string = c.getAttribute("file");
            let gf:A4GraphicFile = this.getGraphicFile(file);
            if (gf.tiles == null) gf.updateAfterLoaded();

            if (c.tagName == "types") {
                let values:string[] = c.firstChild.nodeValue.split(new RegExp(",| |\n|\t|\r"));
                let j:number = 0;
                for(let i:number = 0;i<values.length;i++) {
                    if (values[i] != "") {
                        gf.tileTypes[j] = Number(values[i]);
                        j++;
                    }
                }
                console.log("Loaded " + j + " types for image " + gf.name);
            } else if (c.tagName == "seeThrough") {
                let values:string[] = c.firstChild.nodeValue.split(new RegExp(",| |\n|\t|\r"));
                let j:number = 0;
                for(let i:number = 0;i<values.length;i++) {
                    if (values[i] != "") {
                        gf.tileSeeThrough[j] = Number(values[i]);
                        j++;
                    }
                }
                console.log("Loaded " + j + " seeThroughs for image " + gf.name);
            } else if (c.tagName == "canDig") {
                let values:string[] = c.firstChild.nodeValue.split(new RegExp(",| |\n|\t|\r"));
                let j:number = 0;
                for(let i:number = 0;i<values.length;i++) {
                    if (values[i] != "") {
                        gf.tileCanDig[j] = Number(values[i]);
                        j++;
                    }
                }
                console.log("Loaded " + j + " canDigs for image " + gf.name);
            } else {
                console.log("undefined tag inside of the tile definition: " + c.tagName);
            }
        }*/
        // loading object types:
        {
            var files_xml = getElementChildrenByTag(this.xml, "objectDefinition");
            for (var i = 0; i < files_xml.length; i++) {
                var file_xml = files_xml[i];
                var objects_file = file_xml.getAttribute("file");
                this.objectDefinitionFiles.push(objects_file);
                console.log("Loading objects from file " + this.game_path + "/" + objects_file);
                var xmlhttp = new XMLHttpRequest();
                xmlhttp.overrideMimeType("text/xml");
                xmlhttp.open("GET", this.game_path + "/" + objects_file, false);
                xmlhttp.send();
                var obejcts_xml = xmlhttp.responseXML.documentElement;
                this.objectFactory.addDefinitions(obejcts_xml, this, "ObjectClass");
            }
        }
        // loading character types:
        {
            var files_xml = getElementChildrenByTag(this.xml, "characterDefinition");
            for (var i = 0; i < files_xml.length; i++) {
                var file_xml = files_xml[i];
                var objects_file = file_xml.getAttribute("file");
                this.characterDefinitionFiles.push(objects_file);
                console.log("Loading characters from file " + this.game_path + "/" + objects_file);
                var xmlhttp = new XMLHttpRequest();
                xmlhttp.overrideMimeType("text/xml");
                xmlhttp.open("GET", this.game_path + "/" + objects_file, false);
                xmlhttp.send();
                var obejcts_xml = xmlhttp.responseXML.documentElement;
                this.objectFactory.addDefinitions(obejcts_xml, this, "CharacterClass");
            }
        }
        // loading maps:
        var objectsToRevisit_xml = [];
        var objectsToRevisit_object = [];
        {
            if (saveGameXml != null) {
                var maps_xml = getElementChildrenByTag(saveGameXml, "map");
                for (var i = 0; i < maps_xml.length; i++) {
                    var tmx = maps_xml[i];
                    var map = new A4Map(tmx, this, objectsToRevisit_xml, objectsToRevisit_object);
                    map.cacheDrawTiles();
                    this.maps.push(map);
                }
            }
            else {
                var maps_xml = getElementChildrenByTag(this.xml, "map");
                for (var i = 0; i < maps_xml.length; i++) {
                    var map_xml = maps_xml[i];
                    var tmx_file = map_xml.getAttribute("file");
                    var tmx = null;
                    console.log("loading A4Map from file... " + this.game_path + "/" + tmx_file);
                    var xmlhttp = new XMLHttpRequest();
                    xmlhttp.overrideMimeType("text/xml");
                    xmlhttp.open("GET", this.game_path + "/" + tmx_file, false);
                    xmlhttp.send();
                    tmx = xmlhttp.responseXML.documentElement;
                    var map = new A4Map(tmx, this, objectsToRevisit_xml, objectsToRevisit_object);
                    map.cacheDrawTiles();
                    this.maps.push(map);
                }
            }
            // link bridges/bridge destinations:
            for (var _d = 0, _e = this.maps; _d < _e.length; _d++) {
                var map1 = _e[_d];
                for (var _f = 0, _g = this.maps; _f < _g.length; _f++) {
                    var map2 = _g[_f];
                    if (map1 != map2) {
                        for (var _h = 0, _j = map1.bridges; _h < _j.length; _h++) {
                            var b = _j[_h];
                            for (var _k = 0, _l = map2.bridgeDestinations; _k < _l.length; _k++) {
                                var bd = _l[_k];
                                if (b.name == bd.name) {
                                    //                                    console.log("Map bridge " + b.name + " linked between " + map1.name + " and " + map2.name);
                                    b.link(bd);
                                }
                            }
                        }
                    }
                }
            }
        }
        // loading scripts:
        {
            // on start:
            var onstarts_xml = getElementChildrenByTag(this.xml, "onStart");
            for (var i = 0; i < onstarts_xml.length; i++) {
                var onstart_xml = onstarts_xml[i];
                var tmp = null;
                //                let script_xml_l:NodeListOf<Element> = onstart_xml.children;
                var script_xml_l = onstart_xml.children;
                for (var j = 0; j < script_xml_l.length; j++) {
                    var script_xml = script_xml_l[j];
                    var s = A4Script.fromXML(script_xml);
                    if (tmp == null)
                        tmp = new A4ScriptExecutionQueue(null, null, this, null);
                    tmp.scripts.push(s);
                }
                if (tmp != null)
                    this.scriptQueues.push(tmp);
            }
            // event rules:
            var eventrules_xml = getElementChildrenByTag(this.xml, "eventRule");
            for (var i = 0; i < eventrules_xml.length; i++) {
                var rule_xml = eventrules_xml[i];
                var r = A4EventRule.fromXML(rule_xml);
                if (this.eventScripts[r.event] == null)
                    this.eventScripts[r.event] = [];
                this.eventScripts[r.event].push(r);
            }
        }
        this.currentPlayer = null;
        {
            var players_xml = getElementChildrenByTag(this.xml, "player");
            for (var i = 0; i < players_xml.length; i++) {
                var player_xml = players_xml[i];
                var id = player_xml.getAttribute("id");
                var className = player_xml.getAttribute("class");
                if (className == null)
                    className = player_xml.getAttribute("type");
                var x = Number(player_xml.getAttribute("x"));
                var y = Number(player_xml.getAttribute("y"));
                var mapIdx = Number(player_xml.getAttribute("map"));
                console.log("Spawning player " + className + " at " + x + "," + y + " in map " + mapIdx);
                var completeRedefinition = false;
                if (player_xml.getAttribute("completeRedefinition") == "true")
                    completeRedefinition = true;
                var p = this.objectFactory.createObject(className, this, true, completeRedefinition);
                if (id != null) {
                    p.ID = id;
                    if (!isNaN(Number(id)) &&
                        Number(id) >= A4Object.s_nextID)
                        A4Object.s_nextID = Number(id) + 1;
                }
                p.loadObjectAdditionalContent(player_xml, this, this.objectFactory, objectsToRevisit_xml, objectsToRevisit_object);
                p.warp(x, y, this.maps[mapIdx]);
                this.players.push(p);
            }
        }
        // load messages:
        {
            var console_xml = getFirstElementChildByTag(this.xml, "console");
            if (console_xml != null) {
                for (var _m = 0, _o = getElementChildrenByTag(console_xml, "message"); _m < _o.length; _m++) {
                    var message_xml = _o[_m];
                    if (message_xml.getAttribute("timeStamp") != null) {
                        this.addMessageWithColorTime(message_xml.getAttribute("text"), message_xml.getAttribute("color"), Number(message_xml.getAttribute("timeStamp")));
                    }
                    else {
                        this.addMessageWithColor(message_xml.getAttribute("text"), message_xml.getAttribute("color"));
                    }
                }
            }
        }
        for (var i = 0; i < objectsToRevisit_xml.length; i++) {
            objectsToRevisit_object[i].revisitObject(objectsToRevisit_xml[i], this);
        }
        // set initial camera:
        if (this.players.length > 0) {
            this.currentPlayerIndex = 0;
            this.currentPlayer = this.players[this.currentPlayerIndex];
        }
        this.gameComplete = false;
        this.gameComplete_ending_ID = null;
        this.ensureUniqueObjectIDs();
        console.log("A4Game created\n");
        console.log("currentPlayer = " + this.currentPlayer);
    };
    A4Game.prototype.saveGame = function (saveName) {
        var complete_xmlString = "<A4Game_savegame>\n";
        var xmlString = this.saveToXML();
        console.log("A4Game.saveGame: game xmlString length " + xmlString.length);
        complete_xmlString += xmlString;
        for (var i = 0; i < this.maps.length; i++) {
            xmlString = this.maps[i].saveToXML(this);
            complete_xmlString += "\n\n\n" + xmlString;
            console.log("A4Game.saveGame: map " + i + " xmlString length " + xmlString.length);
        }
        complete_xmlString += "</A4Game_savegame>";
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
    A4Game.prototype.saveToXMLInnerContent = function () {
        var xmlString = "";
        if (this.gameTitleImage != null) {
            xmlString += "<titleImage>" + this.gameTitleImage + "</titleImage>\n";
        }
        if (this.storyText != null) {
            xmlString += "<story>\n";
            for (var _i = 0, _a = this.storyText; _i < _a.length; _i++) {
                var text = _a[_i];
                xmlString += "<line>" + text + "</line>\n";
            }
            xmlString += "</story>\n";
        }
        for (var i = 0; i < this.endingIDs.length; i++) {
            xmlString += "<ending id=\"" + this.endingIDs[i] + "\">\n";
            for (var _b = 0, _c = this.endingTexts[i]; _b < _c.length; _b++) {
                var text = _c[_b];
                xmlString += "<line>" + text + "</line>\n";
            }
            xmlString += "</ending>\n";
        }
        // tiles:
        xmlString += "<tiles sourcewidth=\"" + this.tileWidth + "\"" +
            " sourceheight=\"" + this.tileHeight + "\"" +
            " targetwidth=\"" + this.tileWidth * this.defaultZoom + "\"" +
            " targetheight=\"" + this.tileHeight * this.defaultZoom + "\">\n";
        for (var _d = 0, _e = this.graphicFiles; _d < _e.length; _d++) {
            var gf = _e[_d];
            xmlString += "<graphicFile file=\"" + gf.name + "\"/>\n";
            // xmlString += "<types file=\"" + gf.name + "\">\n"
            // for(let i:number = 0;i<gf.n_tiles;i+=gf.tilesPerRow) {
            //     for(let j:number = 0;j<gf.tilesPerRow;j++) {
            //         xmlString += gf.tileTypes[i+j] +",";
            //     }
            //     xmlString += "\n";
            // }
            // xmlString += "</types>\n";
            // xmlString += "<seeThrough file=\"" + gf.name + "\">\n"
            // for(let i:number = 0;i<gf.n_tiles;i+=gf.tilesPerRow) {
            //     for(let j:number = 0;j<gf.tilesPerRow;j++) {
            //         xmlString += gf.tileSeeThrough[i+j] +",";
            //     }
            //     xmlString += "\n";
            // }
            // xmlString += "</seeThrough>\n";
            // xmlString += "<canDig file=\"" + gf.name + "\">\n"
            // for(let i:number = 0;i<gf.n_tiles;i+=gf.tilesPerRow) {
            //     for(let j:number = 0;j<gf.tilesPerRow;j++) {
            //         xmlString += gf.tileCanDig[i+j] +",";
            //     }
            //     xmlString += "\n";
            // }
            // xmlString += "</canDig>\n";
        }
        for (var ID in this.mapTiles) {
            var mp = this.mapTiles[ID];
            xmlString += mp.saveToXML();
        }
        xmlString += "</tiles>\n";
        // character and object definition files ...
        for (var _f = 0, _g = this.characterDefinitionFiles; _f < _g.length; _f++) {
            var tmp = _g[_f];
            xmlString += "<characterDefinition file=\"" + tmp + "\"/>\n";
        }
        for (var _h = 0, _j = this.objectDefinitionFiles; _h < _j.length; _h++) {
            var tmp = _j[_h];
            xmlString += "<objectDefinition file=\"" + tmp + "\"/>\n";
        }
        // maps
        for (var idx = 0; idx < this.maps.length; idx++) {
            xmlString += "<map file=\"map" + idx + ".xml\"/>\n";
        }
        // players:
        for (var _k = 0, _l = this.players; _k < _l.length; _k++) {
            var pc = _l[_k];
            for (var idx = 0; idx < this.maps.length; idx++) {
                if (pc.map == this.maps[idx]) {
                    xmlString += pc.saveToXMLForMainFile(this, "player", idx) + "\n";
                    break;
                }
            }
        }
        // save state:
        var onStarttagOpen = false;
        /*
        for(let sa of this.knownSpeechActs) {
            if (sa.performative==A4_TALK_PERFORMATIVE_ASK) {
                if (!onStarttagOpen) {
                    xmlString += "<onStart>\n";
                    onStarttagOpen = true;
                }
                xmlString+="<addTopic topic=\""+sa.keyword+"\" text=\""+sa.text+"\"/>\n";
            }
        }
        */
        for (var v in this.storyState) {
            if (!onStarttagOpen) {
                xmlString += "<onStart>\n";
                onStarttagOpen = true;
            }
            xmlString += "<storyState variable=\"" + v + "\"" +
                " value=\"" + this.storyState[v] + "\"" +
                " scope=\"game\"/>\n";
        }
        if (onStarttagOpen)
            xmlString += "</onStart>\n";
        // each execution queue goes to its own "onStart" block:
        for (var _m = 0, _o = this.scriptQueues; _m < _o.length; _m++) {
            var seq = _o[_m];
            xmlString += "<onStart>\n";
            for (var _p = 0, _q = seq.scripts; _p < _q.length; _p++) {
                var s = _q[_p];
                xmlString += s.saveToXML() + "\n";
            }
            xmlString += "</onStart>\n";
        }
        // rules:
        for (var i = 0; i < A4_NEVENTS; i++) {
            if (this.eventScripts[i] != null) {
                for (var _r = 0, _s = this.eventScripts[i]; _r < _s.length; _r++) {
                    var er = _s[_r];
                    xmlString += er.saveToXML() + "\n";
                }
            }
        }
        // console messages:
        xmlString += "<console>\n";
        for (var _t = 0, _u = this.messages; _t < _u.length; _t++) {
            var m = _u[_t];
            xmlString += "<message text=\"" + m[0] + "\" color=\"" + m[1] + "\" timeStamp=\"" + m[2] + "\"/>\n";
        }
        xmlString += "</console>\n";
        return xmlString;
    };
    A4Game.prototype.saveToXML = function () {
        var xmlString = "";
        xmlString += "<A4Game";
        if (this.gameName != null)
            xmlString += " name=\"" + this.gameName + "\"";
        if (this.gameTitle != null)
            xmlString += " title=\"" + this.gameTitle + "\"";
        if (this.gameSubtitle != null)
            xmlString += " subtitle=\"" + this.gameSubtitle + "\"";
        xmlString += " allowSaveGames=\"" + this.allowSaveGames + "\"";
        xmlString += " cycle=\"" + this.cycle + "\"";
        xmlString += ">\n";
        xmlString += this.saveToXMLInnerContent();
        xmlString += "</A4Game>";
        return xmlString;
    };
    A4Game.prototype.checkSaveGame = function (saveName) {
        return localStorage.getItem(A4SAVEGAME_STORAGE_KEY + "-" + saveName + "-name");
    };
    A4Game.prototype.deleteSaveGame = function (saveName) {
        localStorage.removeItem(A4SAVEGAME_STORAGE_KEY + "-" + saveName + "-name");
        localStorage.removeItem(A4SAVEGAME_STORAGE_KEY + "-" + saveName);
    };
    A4Game.prototype.getNEndings = function () {
        return this.endingIDs.length;
    };
    A4Game.prototype.getGameEnding = function (ID) {
        for (var i = 0; i < this.endingIDs.length; i++) {
            if (ID == this.endingIDs[i])
                return this.endingTexts[i];
        }
        return null;
    };
    A4Game.prototype.getGameTitle = function () {
        return this.gameTitle;
    };
    A4Game.prototype.getGameTitleImage = function () {
        return this.gameTitleImage;
    };
    A4Game.prototype.getGameSubtitle = function () {
        return this.gameSubtitle;
    };
    A4Game.prototype.addEnding = function (ID, endingText) {
        this.endingIDs.push(ID);
        this.endingTexts.push(endingText);
    };
    A4Game.prototype.update = function (k) {
        if (!this.updateInternal(k, []))
            return false;
        this.cycle++;
        this.in_game_seconds++; // we keep a separate count from cycles, since in some game scenes, time might advance faster
        if (this.cycles_without_redrawing > 0)
            this.cycles_without_redrawing--;
        return true;
    };
    A4Game.prototype.updateInternal = function (k, additional_maps_to_update) {
        if (this.cycle == 0) {
            if (this.eventScripts[A4_EVENT_START] != null) {
                for (var _i = 0, _a = this.eventScripts[A4_EVENT_START]; _i < _a.length; _i++) {
                    var rule = _a[_i];
                    rule.executeEffects(null, null, this, null);
                }
            }
        }
        this.zoom = (0.95 * this.zoom + 0.05 * this.targetZoom);
        // update all the objects in the game:
        // figure out which maps to update:
        var maps_to_update = [];
        for (var _b = 0, _c = this.currentPlayer.map.getNeighborMaps(); _b < _c.length; _b++) {
            var m = _c[_b];
            if (maps_to_update.indexOf(m) == -1)
                maps_to_update.push(m);
        }
        for (var _d = 0, additional_maps_to_update_1 = additional_maps_to_update; _d < additional_maps_to_update_1.length; _d++) {
            var m = additional_maps_to_update_1[_d];
            if (maps_to_update.indexOf(m) == -1)
                maps_to_update.push(m);
        }
        for (var _e = 0, _f = this.players; _e < _f.length; _e++) {
            var player = _f[_e];
            if (maps_to_update.indexOf(player.map) == -1)
                maps_to_update.push(player.map);
        }
        for (var _g = 0, maps_to_update_1 = maps_to_update; _g < maps_to_update_1.length; _g++) {
            var map = maps_to_update_1[_g];
            map.update(this);
        }
        this.processWarpRequests();
        for (var _h = 0, _j = this.deletionRequests; _h < _j.length; _h++) {
            var o = _j[_h];
            o.event(A4_EVENT_END, null, o.map, this);
            this.objectRemoved(o);
        }
        this.deletionRequests = [];
        // rules:
        if (this.eventScripts[A4_EVENT_TIMER] != null) {
            for (var _k = 0, _l = this.eventScripts[A4_EVENT_TIMER]; _k < _l.length; _k++) {
                var r = _l[_k];
                r.execute(null, null, this, null);
            }
        }
        if (this.eventScripts[A4_EVENT_STORYSTATE] != null) {
            for (var _m = 0, _o = this.eventScripts[A4_EVENT_STORYSTATE]; _m < _o.length; _m++) {
                var r = _o[_m];
                r.execute(null, null, this, null);
            }
        }
        if (this.currentPlayer == null)
            return false;
        this.executeScriptQueues();
        return true;
    };
    A4Game.prototype.processWarpRequests = function () {
        for (var _i = 0, _a = this.warpRequests; _i < _a.length; _i++) {
            var wr = _a[_i];
            var m = wr.map;
            var createRecord = wr.o.isCharacter() || wr.o.isVehicle();
            var acceptWarp = true;
            if (createRecord &&
                m != null &&
                !m.walkableConsideringVehicles(wr.x, wr.y, wr.o.getPixelWidth(), wr.o.getPixelHeight(), wr.o))
                acceptWarp = false;
            if (acceptWarp) {
                if (m != null && createRecord) {
                    wr.o.map.addPerceptionBufferObjectWarpedRecord(new PerceptionBufferObjectWarpedRecord(wr.o.ID, wr.o.sort, m.name, wr.o.x, wr.o.y, wr.o.x + wr.o.getPixelWidth(), wr.o.y + wr.o.getPixelHeight()));
                }
                wr.o.warp(wr.x, wr.y, wr.map); //,wr.layer);
            }
            else {
                // can't warp, since there is a collision!
                if (wr.o == this.currentPlayer)
                    this.addMessage("Something is blocking the way!");
            }
        }
        this.warpRequests = [];
    };
    A4Game.prototype.draw = function (screen_width, screen_height) {
        var tileSize = (screen_height / 24);
        var split = Math.floor(tileSize * 17);
        // do not draw anything unless we have already executed a cycle:
        if (this.cycle == 0)
            return;
        if (this.cycles_without_redrawing > 0)
            return;
        this.drawWorld(screen_width, split + tileSize);
        this.drawHUD(screen_width, screen_height, split);
    };
    A4Game.prototype.drawWorld = function (screen_width, screen_height) {
        this.screen_width_last = screen_width;
        this.screen_height_last = screen_height;
        if (this.currentPlayer != null) {
            var map = this.currentPlayer.map;
            var mapx = this.getCameraX(this.currentPlayer, map.width * this.tileWidth, screen_width);
            var mapy = this.getCameraY(this.currentPlayer, map.height * this.tileHeight, screen_height);
            var tx = Math.floor(this.currentPlayer.x / this.tileWidth);
            var ty = Math.floor(this.currentPlayer.y / this.tileHeight);
            map.drawRegion(mapx, mapy, this.zoom, screen_width, screen_height, map.visibilityRegion(tx, ty), this);
            map.drawTextBubblesRegion(mapx, mapy, this.zoom, screen_width, screen_height, map.visibilityRegion(tx, ty), this);
        }
        else {
            var map = this.maps[0];
            map.drawRegion(0, 0, this.zoom, screen_width, screen_height, map.visibilityRegion(0, 0), this);
            map.drawTextBubbles(0, 0, this.zoom, screen_width, screen_height, this);
        }
    };
    A4Game.prototype.drawHUD = function (screen_width, screen_height, split) {
        ctx.fillStyle = "black";
        ctx.fillRect(0, split + PIXEL_SIZE, screen_width, (screen_height - split));
        // this is game dependent, so, this function is empty:
        // ...
    };
    A4Game.prototype.mouseClick = function (mouse_x, mouse_y, button) {
        if (mouse_y < PIXEL_SIZE * 8 * 17) {
            // click in the game screen: this should skip text bubbles, etc.
            this.skipSpeechBubble();
            if (this.currentPlayer != null) {
                if (!(this.mouse_draw_skip > 0)) {
                    this.mouse_draw_skip = 100;
                    this.mouse_x_last = mouse_x;
                    this.mouse_y_last = mouse_y;
                    var map = this.currentPlayer.map;
                    var screen_width = this.screen_width_last;
                    var screen_height = this.screen_height_last;
                    var mapx = this.getCameraX(this.currentPlayer, map.width * this.tileWidth, screen_width);
                    var mapy = this.getCameraY(this.currentPlayer, map.height * this.tileHeight, screen_height);
                    var tx = Math.floor(mouse_x / this.tileWidth);
                    var ty = Math.floor(mouse_y / this.tileHeight);
                    var vr = map.visibilityRegion(tx, ty);
                    //  map.drawRegion(mapx, mapy, this.zoom, screen_width, screen_height, vr, this);							 
                    console.log("map.drawRegion(".concat(mapx, ", ").concat(mapy, ", ").concat(this.zoom, ", ").concat(screen_width, ", ").concat(screen_height, ", ").concat(vr, ", this);"));
                }
                console.log("mouse_x/y= ".concat(mouse_x - this.currentPlayer.x, "/").concat(mouse_y - this.currentPlayer.y, " = ").concat(button, " "));
            }
        }
        if (mouse_x >= PIXEL_SIZE * 8 * 27 &&
            mouse_x < PIXEL_SIZE * 8 * 28 &&
            mouse_y >= PIXEL_SIZE * 8 * 17 &&
            mouse_y < PIXEL_SIZE * 8 * 18) {
            this.playerInput_ToogleInventory();
        }
        if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY) {
            // inventory window:
            if (mouse_x >= PIXEL_SIZE * 8 * 29 &&
                mouse_x < PIXEL_SIZE * 8 * 30 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                this.HUD_inventory_start -= 2;
                if (this.HUD_inventory_start < 0)
                    this.HUD_inventory_start = 0;
                if (this.currentPlayer.selectedItem >= 0) {
                    while (this.currentPlayer.selectedItem >= this.HUD_inventory_start + SHRDLU_INVENTORY_DISPLAY_SIZE) {
                        this.currentPlayer.previousItem();
                    }
                }
            }
            if (mouse_x >= PIXEL_SIZE * 8 * 30 &&
                mouse_x < PIXEL_SIZE * 8 * 31 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                if (this.currentPlayer.inventory.length > this.HUD_inventory_start + SHRDLU_INVENTORY_DISPLAY_SIZE) {
                    this.HUD_inventory_start += 2;
                    if (this.currentPlayer.selectedItem >= 0) {
                        while (this.currentPlayer.selectedItem < this.HUD_inventory_start) {
                            this.currentPlayer.nextItem();
                        }
                    }
                }
            }
            if (mouse_x < PIXEL_SIZE * 8 * 12 &&
                mouse_y >= PIXEL_SIZE * 8 * 18) {
                var y = Math.floor((mouse_y - PIXEL_SIZE * 8 * 18) / (PIXEL_SIZE * 8));
                var selected = this.HUD_inventory_start + y * 2;
                if (selected < this.currentPlayer.inventory.length &&
                    this.currentPlayer.inventory[selected] != null) {
                    this.currentPlayer.selectedItem = selected;
                }
            }
            if (mouse_x >= PIXEL_SIZE * 8 * 14 &&
                mouse_x < PIXEL_SIZE * 8 * 26 &&
                mouse_y >= PIXEL_SIZE * 8 * 18) {
                var y = Math.floor((mouse_y - PIXEL_SIZE * 8 * 18) / (PIXEL_SIZE * 8));
                var selected = this.HUD_inventory_start + 1 + y * 2;
                if (selected < this.currentPlayer.inventory.length &&
                    this.currentPlayer.inventory[selected] != null) {
                    this.currentPlayer.selectedItem = selected;
                }
            }
            // Use
            if (mouse_x >= PIXEL_SIZE * 8 * 27 &&
                mouse_y >= PIXEL_SIZE * 8 * 19 &&
                mouse_y < PIXEL_SIZE * 8 * 20) {
                this.playerInput_UseItem();
            }
            // Drop
            if (mouse_x >= PIXEL_SIZE * 8 * 27 &&
                mouse_y >= PIXEL_SIZE * 8 * 21 &&
                mouse_y < PIXEL_SIZE * 8 * 22) {
                this.playerInput_DropItem();
            }
        }
        else if (this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
            if (mouse_x < PIXEL_SIZE * 8 * 12 &&
                mouse_y >= PIXEL_SIZE * 8 * 18) {
                var y = Math.floor((mouse_y - PIXEL_SIZE * 8 * 18) / (PIXEL_SIZE * 8));
                var selected = this.HUD_inventory_start + y;
                if (selected < this.currentPlayer.inventory.length &&
                    this.currentPlayer.inventory[selected] != null) {
                    this.currentPlayer.selectedItem = selected;
                    this.HUD_remote_inventory_selected = -1;
                }
            }
            // select on the other inventory:
            if (mouse_x >= PIXEL_SIZE * 8 * 20 &&
                mouse_y >= PIXEL_SIZE * 8 * 19) {
                var y = Math.floor((mouse_y - PIXEL_SIZE * 8 * 19) / (PIXEL_SIZE * 8));
                var selected = this.HUD_remote_inventory_start + y;
                if (selected < this.HUD_remote_inventory.content.length &&
                    this.HUD_remote_inventory.content[selected] != null) {
                    this.HUD_remote_inventory_selected = selected;
                    this.currentPlayer.selectedItem = -1;
                }
            }
            // arrows (inventory):
            if (mouse_x >= PIXEL_SIZE * 8 * 10 &&
                mouse_x < PIXEL_SIZE * 8 * 11 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                this.HUD_inventory_start--;
                if (this.HUD_inventory_start < 0)
                    this.HUD_inventory_start = 0;
                if (this.currentPlayer.selectedItem >= 0) {
                    while (this.currentPlayer.selectedItem >= this.HUD_inventory_start + SHRDLU_INVENTORY_DISPLAY_SIZE / 2) {
                        this.currentPlayer.previousItem();
                    }
                }
            }
            if (mouse_x >= PIXEL_SIZE * 8 * 11 &&
                mouse_x < PIXEL_SIZE * 8 * 12 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                if (this.currentPlayer.inventory.length > this.HUD_inventory_start + SHRDLU_INVENTORY_DISPLAY_SIZE / 2) {
                    this.HUD_inventory_start++;
                    if (this.currentPlayer.selectedItem >= 0) {
                        while (this.currentPlayer.selectedItem < this.HUD_inventory_start) {
                            this.currentPlayer.nextItem();
                        }
                    }
                }
            }
            // arrows (remote):
            if (mouse_x >= PIXEL_SIZE * 8 * 29 &&
                mouse_x < PIXEL_SIZE * 8 * 30 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                this.HUD_remote_inventory_start--;
                if (this.HUD_remote_inventory_start < 0)
                    this.HUD_remote_inventory_start = 0;
                if (this.HUD_remote_inventory_selected >= 0) {
                    while (this.HUD_remote_inventory_selected >= this.HUD_remote_inventory_start + (SHRDLU_INVENTORY_DISPLAY_SIZE / 2) - 1) {
                        this.HUD_remote_inventory_selected--;
                    }
                }
            }
            if (mouse_x >= PIXEL_SIZE * 8 * 30 &&
                mouse_x < PIXEL_SIZE * 8 * 31 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                if (this.HUD_remote_inventory.content.length > this.HUD_remote_inventory_start + (SHRDLU_INVENTORY_DISPLAY_SIZE / 2) - 1) {
                    this.HUD_remote_inventory_start++;
                    if (this.HUD_remote_inventory_selected >= 0) {
                        while (this.HUD_remote_inventory_selected < this.HUD_remote_inventory_start) {
                            this.HUD_remote_inventory_selected++;
                        }
                    }
                }
            }
            // buttons:
            // take
            if (mouse_x >= PIXEL_SIZE * 8 * 14 &&
                mouse_x < PIXEL_SIZE * 8 * 19 &&
                mouse_y >= PIXEL_SIZE * 8 * 19 &&
                mouse_y < PIXEL_SIZE * 8 * 20) {
                if (this.HUD_remote_inventory_selected >= 0 &&
                    this.HUD_remote_inventory.content[this.HUD_remote_inventory_selected].takeable) {
                    var item = this.HUD_remote_inventory.content[this.HUD_remote_inventory_selected];
                    var idx = this.HUD_remote_inventory.content.indexOf(item);
                    this.HUD_remote_inventory.content.splice(idx, 1);
                    this.HUD_remote_inventory.objectRemoved(item);
                    this.currentPlayer.inventory.push(item);
                    this.HUD_remote_inventory_selected = -1;
                    this.playSound("data/sfx/itemPickup.wav");
                }
            }
            // put
            if (mouse_x >= PIXEL_SIZE * 8 * 14 &&
                mouse_x < PIXEL_SIZE * 8 * 19 &&
                mouse_y >= PIXEL_SIZE * 8 * 21 &&
                mouse_y < PIXEL_SIZE * 8 * 22) {
                if (this.currentPlayer.selectedItem >= 0) {
                    var item = this.currentPlayer.inventory[this.currentPlayer.selectedItem];
                    var idx = this.currentPlayer.inventory.indexOf(item);
                    this.currentPlayer.inventory.splice(idx, 1);
                    this.HUD_remote_inventory.addContent(item);
                    this.currentPlayer.selectedItem = -1;
                    this.playSound("data/sfx/itemPickup.wav");
                }
            }
        }
        else {
            // message window:
            if (mouse_x >= PIXEL_SIZE * 8 * 29 &&
                mouse_x < PIXEL_SIZE * 8 * 30 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                this.messageConsoleUp();
            }
            if (mouse_x >= PIXEL_SIZE * 8 * 30 &&
                mouse_x < PIXEL_SIZE * 8 * 31 &&
                mouse_y >= PIXEL_SIZE * 8 * 17 &&
                mouse_y < PIXEL_SIZE * 8 * 18) {
                this.messageConsoleDown();
            }
        }
    };
    A4Game.prototype.setGameComplete = function (gc, ID) {
        this.gameComplete = gc;
        this.gameComplete_ending_ID = ID;
    };
    A4Game.prototype.executeScriptQueues = function () {
        var toDelete = [];
        for (var _i = 0, _a = this.scriptQueues; _i < _a.length; _i++) {
            var seb = _a[_i];
            while (true) {
                var s = seb.scripts[0];
                var retval = s.execute(seb.object, seb.map, (seb.game == null ? this : seb.game), seb.otherCharacter);
                if (retval == SCRIPT_FINISHED) {
                    seb.scripts.splice(0, 1);
                    if (seb.scripts.length == 0) {
                        toDelete.push(seb);
                        break;
                    }
                }
                else if (retval == SCRIPT_NOT_FINISHED) {
                    break;
                }
                else if (retval == SCRIPT_FAILED) {
                    toDelete.push(seb);
                    break;
                }
            }
        }
        for (var _b = 0, toDelete_1 = toDelete; _b < toDelete_1.length; _b++) {
            var seb = toDelete_1[_b];
            var idx = this.scriptQueues.indexOf(seb);
            this.scriptQueues.splice(idx, 1);
        }
    };
    A4Game.prototype.addScriptQueue = function (seq) {
        this.scriptQueues.push(seq);
    };
    // if an object is removed from a map, this needs to be called, to notify
    // the game that this happened.
    A4Game.prototype.objectRemoved = function (o) {
        var idx = this.players.indexOf(o);
        if (idx >= 0)
            this.players.splice(idx, 1);
        if (this.currentPlayer == o) {
            this.currentPlayerIndex = 0;
            if (this.players.length > 0) {
                this.currentPlayer = this.players[this.currentPlayerIndex];
            }
            else {
                this.currentPlayer = null;
            }
        }
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var map = _a[_i];
            map.objectRemoved(o);
        }
        if (this.HUD_remote_inventory == o) {
            this.HUD_remote_inventory = null;
            if (this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY)
                this.HUD_state = SHRDLU_HUD_STATE_INVENTORY;
        }
    };
    A4Game.prototype.contains = function (o) {
        for (var _i = 0, _a = this.deletionRequests; _i < _a.length; _i++) {
            var o2 = _a[_i];
            if (o2 == o)
                return false;
        }
        for (var _b = 0, _c = this.maps; _b < _c.length; _b++) {
            var map = _c[_b];
            if (map.contains(o))
                return true;
        }
        return false;
    };
    /*
    - Prevents the characters from accidentally going to a map that they should not go to
    - Should be overwriten by each specific game with any custom rules necessary
    */
    A4Game.prototype.checkPermissionToWarp = function (character, target) {
        return true;
    };
    // Teleports an object to a requested map and position. This queues up the request,
    // but it is not executed until at the end of a game cycle, to prevent this from 
    // happening while we are still looping through lists of objects (concurrent modification)
    A4Game.prototype.requestWarp = function (o, map, x, y) {
        this.warpRequests.push(new WarpRequest(o, map, x, y));
    };
    A4Game.prototype.requestedWarp = function (o) {
        for (var _i = 0, _a = this.warpRequests; _i < _a.length; _i++) {
            var request = _a[_i];
            if (request.o == o)
                return true;
        }
        return false;
    };
    // waits until the end of a cycle, and then deletes o
    A4Game.prototype.requestDeletion = function (o) {
        this.deletionRequests.push(o);
    };
    A4Game.prototype.setStoryStateVariable = function (variable, value) {
        this.storyState[variable] = value;
        this.lastTimeStoryStateChanged = this.cycle;
    };
    A4Game.prototype.getStoryStateVariable = function (variable) {
        return this.storyState[variable];
    };
    A4Game.prototype.playSound = function (sound) {
        this.SFXM.play(sound);
    };
    A4Game.prototype.getGraphicFile = function (file) {
        for (var _i = 0, _a = this.graphicFiles; _i < _a.length; _i++) {
            var gf = _a[_i];
            if (file == gf.name)
                return gf;
        }
        return null;
    };
    A4Game.prototype.getMap = function (name) {
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var m = _a[_i];
            if (name == m.name)
                return m;
        }
        return null;
    };
    A4Game.prototype.getMapIndex = function (name) {
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var m = _a[_i];
            if (name == m.name)
                return this.maps.indexOf(m);
        }
        return -1;
    };
    A4Game.prototype.getCameraX = function (focus, map_width, screen_width) {
        var target_x = 0;
        if (map_width < screen_width / this.zoom) {
            target_x = (map_width - screen_width / this.zoom) / 2;
        }
        else {
            target_x = focus.x + this.tileWidth / 2;
            target_x -= (screen_width / 2) / this.zoom;
            if (target_x < 0)
                target_x = 0;
            if (map_width - target_x < screen_width / this.zoom)
                target_x = map_width - screen_width / this.zoom;
        }
        target_x = Math.floor(target_x);
        return target_x;
    };
    A4Game.prototype.getCameraY = function (focus, map_height, screen_height) {
        var top_HUD = 40;
        var bottom_HUD = 56;
        var center_Y = (screen_height - (top_HUD + bottom_HUD)) / 2 + top_HUD;
        var target_y = 0;
        if (map_height < (screen_height - (top_HUD + bottom_HUD)) / this.zoom) {
            target_y = (map_height - (center_Y * 2) / this.zoom) / 2;
        }
        else {
            target_y = focus.y + this.tileHeight / 2;
            target_y -= (center_Y) / this.zoom;
            if (target_y < -top_HUD / this.zoom)
                target_y = -top_HUD / this.zoom; // 40 pixels to leave space for the HUD
            if (map_height - target_y < (screen_height - bottom_HUD) / this.zoom)
                target_y = map_height - (screen_height - bottom_HUD) / this.zoom;
        }
        target_y = Math.floor(target_y);
        //        target_y  = (target_y - target_y%PIXEL_SIZE);
        return target_y;
    };
    A4Game.prototype.setDoorGroupState = function (doorGroup, state, character) {
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var map = _a[_i];
            map.setDoorGroupState(doorGroup, state, character, map, this);
        }
    };
    A4Game.prototype.checkIfDoorGroupStateCanBeChanged = function (doorGroup, state, character) {
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var map = _a[_i];
            if (!map.checkIfDoorGroupStateCanBeChanged(doorGroup, state, character, map, this))
                return false;
        }
        return true;
    };
    A4Game.prototype.addMessage = function (text) {
        this.addMessageWithColor(text, "white");
    };
    A4Game.prototype.addMessageWithColor = function (text, color) {
        this.addMessageWithColorTime(text, color, this.in_game_seconds);
    };
    A4Game.prototype.addMessageWithColorTime = function (text, color, timeStamp) {
        // Prevent an infinite stream of error messages:
        if (this.messages.length >= 3 &&
            text.substring(0, 7) == "[ERROR:" &&
            this.messages[this.messages.length - 3][0].substring(0, 7) == "[ERROR:")
            return;
        // split longer messages into different lines:
        var buffer = "";
        var last_space = 0;
        for (var i = 0; i < text.length; i++) {
            buffer += text.charAt(i);
            if (text.charAt(i) == ' ')
                last_space = i;
            if (buffer.length >= A4_MAX_MESSAGE_LENGTH) {
                if (last_space == 0) {
                    // a single word doesn't fit, just split it!
                    this.messages.push([buffer, color, "" + timeStamp]);
                    buffer = "";
                }
                else {
                    var backspaces = i - last_space;
                    var tmp = buffer.substring(0, buffer.length - backspaces);
                    this.messages.push([tmp, color, "" + timeStamp]);
                    buffer = "  " + buffer.substring((buffer.length - backspaces));
                }
            }
        }
        if (buffer != "")
            this.messages.push([buffer, color, "" + timeStamp]);
    };
    // message added only if the "originator" is in the same map as the "this.current_player"
    A4Game.prototype.addMessageWithOriginator = function (originator, msg) {
        if (this.currentPlayer.map != originator.map)
            return;
        this.addMessage(msg);
    };
    A4Game.prototype.addMessageWithOriginatorAndColor = function (originator, msg, color) {
        if (this.currentPlayer.map != originator.map)
            return;
        this.addMessageWithColor(msg, color);
    };
    // getting input form the player:
    A4Game.prototype.playerInput_ToogleInventory = function () {
        if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY ||
            this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
            this.HUD_state = SHRDLU_HUD_STATE_MESSAGES;
            this.HUD_remote_inventory = null;
        }
        else {
            this.HUD_state = SHRDLU_HUD_STATE_INVENTORY;
        }
    };
    A4Game.prototype.playerInput_UseItem = function () {
        if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY) {
            if (!this.currentPlayer.isInVehicle() &&
                this.currentPlayer.selectedItem >= 0) {
                this.playerInput_issueCommand(A4CHARACTER_COMMAND_USE, app.game.currentPlayer.selectedItem, A4_DIRECTION_NONE);
            }
        }
        else if (this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
            if (!this.currentPlayer.isInVehicle() &&
                this.HUD_remote_inventory_selected >= 0 &&
                this.HUD_remote_inventory.content[this.HUD_remote_inventory_selected].takeable) {
                var item = this.HUD_remote_inventory.content[this.HUD_remote_inventory_selected];
                var idx = this.HUD_remote_inventory.content.indexOf(item);
                this.HUD_remote_inventory.content.splice(idx, 1);
                this.HUD_remote_inventory.objectRemoved(item);
                this.currentPlayer.inventory.push(item);
                this.HUD_remote_inventory_selected = -1;
                this.playSound("data/sfx/itemPickup.wav");
            }
        }
    };
    A4Game.prototype.playerInput_DropItem = function () {
        if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY) {
            if (!this.currentPlayer.isInVehicle() &&
                this.currentPlayer.selectedItem >= 0) {
                var item = this.currentPlayer.inventory[this.currentPlayer.selectedItem];
                if (item != null && item.droppable) {
                    this.playerInput_issueCommand(A4CHARACTER_COMMAND_DROP, app.game.currentPlayer.selectedItem, null);
                    this.currentPlayer.selectedItem = -1;
                }
            }
        }
        else if (this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
            if (!this.currentPlayer.isInVehicle() &&
                this.currentPlayer.selectedItem >= 0) {
                var item = this.currentPlayer.inventory[this.currentPlayer.selectedItem];
                if (item != null && item.droppable) {
                    var item_1 = this.currentPlayer.inventory[this.currentPlayer.selectedItem];
                    var idx = this.currentPlayer.inventory.indexOf(item_1);
                    this.currentPlayer.inventory.splice(idx, 1);
                    this.HUD_remote_inventory.addContent(item_1);
                    this.currentPlayer.selectedItem = -1;
                    this.playSound("data/sfx/itemPickup.wav");
                }
            }
        }
    };
    A4Game.prototype.playerInput_NextItem = function () {
        this.currentPlayer.nextItem();
    };
    A4Game.prototype.messageConsoleUp = function () {
        if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY) {
            this.currentPlayer.previousItem();
        }
        else if (this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
            if (this.currentPlayer.selectedItem == 0) {
                if (this.HUD_remote_inventory.content.length > 0) {
                    this.currentPlayer.selectedItem = -1;
                    this.HUD_remote_inventory_selected = this.HUD_remote_inventory.content.length - 1;
                }
                else {
                    this.currentPlayer.previousItem();
                }
            }
            else if (this.currentPlayer.selectedItem > 0) {
                this.currentPlayer.previousItem();
            }
            else if (this.HUD_remote_inventory_selected == 0) {
                this.HUD_remote_inventory_selected = -1;
                this.currentPlayer.previousItem();
            }
            else if (this.HUD_remote_inventory_selected > 0) {
                this.HUD_remote_inventory_selected--;
            }
            else {
                if (this.currentPlayer.inventory.length > 0) {
                    this.currentPlayer.previousItem();
                }
                else {
                    if (this.HUD_remote_inventory.content.length > 0) {
                        this.HUD_remote_inventory_selected = this.HUD_remote_inventory.content.length - 1;
                    }
                }
            }
        }
        else {
            if (this.console_first_message > 0)
                this.console_first_message--;
            if (this.console_first_message == -1 &&
                this.messages.length > A4_N_MESSAGES_IN_HUD)
                this.console_first_message = this.messages.length - (A4_N_MESSAGES_IN_HUD + 1);
        }
    };
    A4Game.prototype.messageConsoleDown = function () {
        if (this.HUD_state == SHRDLU_HUD_STATE_INVENTORY) {
            this.currentPlayer.nextItem();
        }
        else if (this.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
            if (this.currentPlayer.selectedItem >= 0 &&
                this.currentPlayer.selectedItem == this.currentPlayer.inventory.length - 1) {
                if (this.HUD_remote_inventory.content.length > 0) {
                    this.currentPlayer.selectedItem = -1;
                    this.HUD_remote_inventory_selected = 0;
                }
                else {
                    this.currentPlayer.nextItem();
                }
            }
            else if (this.currentPlayer.selectedItem >= 0 &&
                this.currentPlayer.selectedItem < this.currentPlayer.inventory.length - 1) {
                this.currentPlayer.nextItem();
            }
            else if (this.HUD_remote_inventory_selected >= 0 &&
                this.HUD_remote_inventory_selected == this.HUD_remote_inventory.content.length - 1) {
                this.HUD_remote_inventory_selected = -1;
                this.currentPlayer.nextItem();
            }
            else if (this.HUD_remote_inventory_selected >= 0 &&
                this.HUD_remote_inventory_selected < this.HUD_remote_inventory.content.length - 1) {
                this.HUD_remote_inventory_selected++;
            }
            else {
                if (this.currentPlayer.inventory.length > 0) {
                    this.currentPlayer.nextItem();
                }
                else {
                    if (this.HUD_remote_inventory.content.length > 0) {
                        this.HUD_remote_inventory_selected = 0;
                    }
                }
            }
        }
        else {
            if (this.console_first_message != -1) {
                if (this.console_first_message < this.messages.length - (A4_N_MESSAGES_IN_HUD + 1)) {
                    this.console_first_message++;
                }
                else {
                    this.console_first_message = -1;
                }
            }
        }
    };
    A4Game.prototype.playerInput_issueCommand = function (cmd, arg, direction) {
        /*
        if (cmd==A4CHARACTER_COMMAND_WALK) {
            // detect whether we should change "walk" to attack or talk if we walk against an enemy or npc:
            // only change action if there is an obstacle and we cannot walk around it:
            if (!this.currentPlayer.canMove(direction, false)) {
                let map:A4Map = this.currentPlayer.map;
                // detect whether we will collide with another character/object, and decide whether to change to another action:
                let collisions:A4Object[] = map.getAllObjectCollisionsWithOffset(this.currentPlayer, direction_x_inc[direction], direction_y_inc[direction]);
                for(let o of collisions) {
                    if (o.isCharacter()) {
                        // determine whether the character is friendly or unfriendly
                        if (o.isAICharacter()) {
                            let ai:A4AI = (<A4AICharacter>o).AI;
                            if (ai.isUnfriendly(this.currentPlayer.ID)) {
                                // attack!
                                this.currentPlayer.issueCommandWithArguments(A4CHARACTER_COMMAND_ATTACK, arg, direction, o, this);
                                return A4CHARACTER_COMMAND_ATTACK;
                            }
//                            if (ai.conversationGraph!=null) {
//                                // talk:
//                                // don't issue anything, the calling code will trigger the talk dialog
//                                return A4CHARACTER_COMMAND_TALK;
//                            }
                            this.currentPlayer.issueCommandWithArguments(cmd, arg, direction, null, this);
                            return cmd;
                        }
                    }
                }
            }
        }
        */
        this.currentPlayer.issueCommandWithArguments(cmd, arg, direction, null, this);
        return cmd;
    };
    A4Game.prototype.playerInput_issueCommandWithOther = function (cmd, arg, target) {
        this.currentPlayer.issueCommandWithArguments(cmd, arg, 0, target, this);
    };
    // This function returns a list with the hierarchy of objects necessary to find the desired object
    // For example, if an object is directly in a map, the list with be length 1, but if 
    // the object is in the inventory of a character, then we will get a list with the character and then the object 
    A4Game.prototype.findObjectByName = function (name) {
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var m = _a[_i];
            var o2 = m.findObjectByName(name);
            if (o2 != null)
                return o2;
        }
        return null;
    };
    A4Game.prototype.findObjectByIDJustObject = function (ID) {
        var tmp = this.findObjectByID(ID);
        if (tmp == null)
            return null;
        return tmp[tmp.length - 1];
    };
    // This function returns a list with the hierarchy of objects necessary to find the desired object
    // For example, if an object is directly in a map, the list with be length 1, but if 
    // the object is in the inventory of a character, then we will get a list with the character and then the object 
    A4Game.prototype.findObjectByID = function (ID) {
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var m = _a[_i];
            var o2 = m.findObjectByID(ID);
            if (o2 != null)
                return o2;
        }
        return null;
    };
    A4Game.prototype.skipSpeechBubble = function () {
        if (this.currentPlayer.map.textBubbles.length > 0) {
            this.currentPlayer.map.textBubbles[0][1] = 0;
            return true;
        }
        if (this.currentPlayer.talkingBubble != null) {
            this.currentPlayer.stateCycle = this.currentPlayer.talkingBubbleDuration;
            return true;
        }
        return false;
    };
    /*
    Checks to see if there is any repeated IDs, and returns false if there are any repeated, pringing info about them
    */
    A4Game.prototype.ensureUniqueObjectIDs = function () {
        var IDs = [];
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var map = _a[_i];
            for (var _b = 0, _c = map.objects; _b < _c.length; _b++) {
                var object = _c[_b];
                var ID = object.ID;
                if (IDs.indexOf(ID) >= 0) {
                    console.error("Repeated ID: " + ID + " in map " + map.name);
                }
                IDs.push(ID);
                if (object instanceof A4Character) {
                    for (var _d = 0, _e = object.inventory; _d < _e.length; _d++) {
                        var o2 = _e[_d];
                        var ID2 = o2.ID;
                        if (IDs.indexOf(ID2) >= 0) {
                            console.error("Repeated ID: " + ID2 + " in map " + map.name);
                        }
                        IDs.push(ID2);
                    }
                }
                if (object instanceof A4Container) {
                    for (var _f = 0, _g = object.content; _f < _g.length; _f++) {
                        var o2 = _g[_f];
                        var ID2 = o2.ID;
                        if (IDs.indexOf(ID2) >= 0) {
                            console.error("Repeated ID: " + ID2 + " in map " + map.name);
                        }
                        IDs.push(ID2);
                    }
                }
            }
        }
        //console.log("IDs: " + IDs);
    };
    // To be overwriten by each individual game
    A4Game.prototype.checkCustomVehicleCollisionEvents = function (vehicle) {
    };
    return A4Game;
}());
