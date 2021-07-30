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
var AttentionRecord = /** @class */ (function () {
    function AttentionRecord(o, p, a) {
        this.object = null;
        this.priority = 1;
        this.anxiety = 0;
        this.object = o;
        this.priority = p;
        this.anxiety = a;
    }
    return AttentionRecord;
}());
var EtaoinAI = /** @class */ (function (_super) {
    __extends(EtaoinAI, _super);
    function EtaoinAI(o, nlp, stationMaps, game, rulesFileNames) {
        var _this = _super.call(this, o, nlp, game, 12, 0, DEFAULT_QUESTION_PATIENCE_TIMER) || this;
        // perception:
        _this.stationMaps = []; // these are the maps representing Aurora Station, over which Etaoin has direct perception
        _this.self_perception_term = null;
        // special objects:
        _this.player_object = null;
        _this.communicator_object = null;
        _this.baseindoors_location = null;
        _this.baseoutdoors_location = null;
        _this.spacervalleysouth_location = null;
        // attention:
        _this.attention = [];
        _this.oxygen_message_timer = 0;
        console.log("EtaoinAI.constructor Start...");
        _this.selfID = "etaoin";
        _this.stationMaps = stationMaps;
        _this.intentionHandlers.push(new EtaoinTalk_IntentionAction());
        _this.intentionHandlers.push(new EtaoinOpen_IntentionAction());
        _this.intentionHandlers.push(new EtaoinClose_IntentionAction());
        _this.intentionHandlers.push(new EtaoinSwitchOn_IntentionAction());
        _this.intentionHandlers.push(new EtaoinSwitchOff_IntentionAction());
        _this.intentionHandlers.push(new EtaoinConnectTo_IntentionAction());
        _this.intentionHandlers.push(new EtaoinHelp_IntentionAction());
        _this.intentionHandlers.push(new Etaoin3DPrint_IntentionAction());
        _this.intentionHandlers.push(new EtaoinRead_IntentionAction());
        _this.intentionHandlers.push(new EtaoinReboot_IntentionAction());
        // load specific knowledge:
        for (var _i = 0, rulesFileNames_1 = rulesFileNames; _i < rulesFileNames_1.length; _i++) {
            var rulesFileName = rulesFileNames_1[_i];
            _this.loadLongTermRulesFromFile(rulesFileName);
        }
        _this.precalculateLocationKnowledge(game, o);
        _this.add3DPrintingKnowledge(game, o, "etaoin");
        // get objects the AI cares about:
        var tmp = game.findObjectByID("player");
        if (tmp != null)
            _this.player_object = tmp[tmp.length - 1];
        tmp = game.findObjectByName("communicator");
        if (tmp != null)
            _this.communicator_object = tmp[tmp.length - 1];
        var attention_IDs = ["player", "communicator", "shrdlu", "qwerty"];
        var attention_priorities = [5, 1, 1, 1];
        for (var i = 0; i < attention_IDs.length; i++) {
            var obj_l = game.findObjectByID(attention_IDs[i]);
            if (obj_l != null) {
                var obj = obj_l[obj_l.length - 1];
                _this.attention.push(new AttentionRecord(obj, attention_priorities[i], 0));
            }
        }
        console.log("EtaoinAI.constructor End...");
        return _this;
    }
    EtaoinAI.prototype.update = function (timeStamp) {
        _super.prototype.update.call(this, timeStamp);
        // if the player is running out of oxygen, notify her:
        if (this.game.suit_oxygen < SHRDLU_MAX_SPACESUIT_OXYGEN * 0.25 &&
            this.oxygen_message_timer == 0) {
            if (this.queuedIntentions.length == 0 &&
                this.withinEtaoinViewRange(this.game.currentPlayer)) {
                var term2 = new Term(this.game.ontology.getSort("action.talk"), [new ConstantTermAttribute("player", this.game.ontology.getSort("#id")),
                    new TermTermAttribute(Term.fromString("perf.inform(V0:'player'[#id], oxygen-level('player'[#id],'low'[low]))", this.game.ontology))]);
                this.queueIntention(term2, null, null);
                term2 = new Term(this.game.ontology.getSort("action.talk"), [new ConstantTermAttribute("player", this.game.ontology.getSort("#id")),
                    new TermTermAttribute(Term.fromString("perf.request.action('player'[#id], verb.come-back(E:'player'[#id]))", this.game.ontology))]);
                this.queueIntention(term2, null, null);
                this.oxygen_message_timer = 50 * 20; // do not say anything for 20 seconds
                app.achievement_interact_low_oxygen = true;
                app.trigger_achievement_complete_alert();
            }
        }
        else {
            if (this.oxygen_message_timer > 0)
                this.oxygen_message_timer--;
        }
    };
    EtaoinAI.prototype.isIdle = function () {
        if (this.game.currentPlayer.map.textBubbles.length != 0)
            return false;
        return _super.prototype.isIdle.call(this);
    };
    EtaoinAI.prototype.attentionAndPerception = function () {
        // attention selection:
        // pick the object that generates the maximum anxiety:
        var max_anxiety_object = null;
        for (var i = 0; i < this.attention.length; i++) {
            // increment anxiety of objects:
            this.attention[i].anxiety += this.attention[i].priority;
            //			console.log("EtaoinAI: attention["+this.attention[i].object.name+"].anxiety = " + this.attention[i].anxiety);
            if (max_anxiety_object == null ||
                this.attention[i].anxiety > max_anxiety_object.anxiety) {
                if (this.withinEtaoinViewRange(this.attention[i].object))
                    max_anxiety_object = this.attention[i];
            }
        }
        var attention_object = null;
        var attention_object_l = null;
        if (max_anxiety_object != null) {
            max_anxiety_object.anxiety = 0;
            attention_object = max_anxiety_object.object;
            if (attention_object.map == null) {
                attention_object_l = this.game.findObjectByName(attention_object.name);
                if (attention_object_l != null) {
                    attention_object = attention_object_l[0];
                }
            }
        }
        if (attention_object == null) {
            //console.log("EtaoinAI: attention_map = null");
            return;
        }
        if (attention_object_l == null)
            attention_object_l = [max_anxiety_object.object];
        this.perceptionFocusedOnObject(attention_object_l, max_anxiety_object.object);
    };
    EtaoinAI.prototype.perceptionFocusedOnObject = function (attention_object_l, max_anxiety_object) {
        this.clearPerception();
        var attention_object = attention_object_l[0];
        var attention_map = attention_object.map;
        var attention_x = attention_object.x;
        var attention_y = attention_object.y;
        if (attention_map == null) {
            //console.log("EtaoinAI: attention_map = null");
            return;
        }
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
        // perception:
        if (location != null) {
            var visibilityRegion = attention_map.visibilityRegion(tile_x, tile_y);
            var perceptionRadius = 10;
            this.perception((tile_x - perceptionRadius) * attention_map.tileWidth, (tile_y - perceptionRadius) * attention_map.tileHeight, (tile_x + perceptionRadius) * attention_map.tileWidth, (tile_y + perceptionRadius) * attention_map.tileHeight, location, attention_map, visibilityRegion, occupancyMap, null);
            if (max_anxiety_object.name == "communicator" &&
                max_anxiety_object != attention_object &&
                attention_object_l.length > 1) {
                // the communicator is in the pocket of someone, or in some container:
                var container = attention_object_l[attention_object_l.length - 2];
                var o = attention_object_l[attention_object_l.length - 1];
                //			console.log("comm: " + o.ID + ", container: " + container.ID);
                var term1 = new Term(o.sort, [new ConstantTermAttribute(o.ID, this.cache_sort_id)]);
                var term2 = new Term(this.cache_sort_verb_have, [new ConstantTermAttribute(container.ID, this.cache_sort_id),
                    new ConstantTermAttribute(o.ID, this.cache_sort_id)
                    //									   new ConstantTermAttribute(tile_ox, this.cache_sort_number),
                    //									   new ConstantTermAttribute(tile_oy, this.cache_sort_number),
                    //									   new ConstantTermAttribute(map.name, this.cache_sort_symbol)
                ]);
                //			console.log(term1.toString());
                //			console.log(term2.toString());
                this.addTermToPerception(term1);
                this.addTermToPerception(term2);
                for (var _i = 0, _a = this.getBaseObjectProperties(o); _i < _a.length; _i++) {
                    var property = _a[_i];
                    this.addTermToPerception(property);
                }
            }
        }
        // etaoin perceives itself:
        if (this.self_perception_term == null) {
            this.self_perception_term = Term.fromString("disembodied-ai('" + this.selfID + "'[#id])", this.o);
        }
        this.addTermToPerception(this.self_perception_term);
        this.addTermToPerception(Term.fromString("temperature('location-aurora-station'[#id],'" + this.game.aurora_station_temperature_sensor_indoors + "'[temperature.unit.celsius])", this.o));
        this.addTermToPerception(Term.fromString("temperature('location-aurora-settlement'[#id],'" + this.game.aurora_station_temperature_sensor_outdoors + "'[temperature.unit.celsius])", this.o));
        this.addTermToPerception(Term.fromString("temperature('spacer-valley'[#id],'" + this.game.aurora_station_temperature_sensor_outdoors + "'[temperature.unit.celsius])", this.o));
        this.addTermToPerception(Term.fromString("property.sighted('" + this.selfID + "'[#id])", this.o));
    };
    EtaoinAI.prototype.reactToPerformative = function (perf2, speaker, context) {
        // if the player is talking to us, then we close communicator connections to other characters:
        if (this.game.communicatorConnectedTo != null &&
            (speaker instanceof ConstantTermAttribute) &&
            speaker.value == "player") {
            this.game.communicatorConnectedTo = null;
            this.game.communicatorConnectionTime = 0;
        }
        return _super.prototype.reactToPerformative.call(this, perf2, speaker, context);
    };
    EtaoinAI.prototype.withinEtaoinViewRange = function (o) {
        if (this.baseindoors_location == null) {
            this.baseindoors_location = this.game.getAILocationByID("location-aurora-station");
            this.baseoutdoors_location = this.game.getAILocationByID("location-aurora-settlement");
            this.spacervalleysouth_location = this.game.getAILocationByID("spacer-valley-south");
        }
        var l = this.game.getAILocation(o);
        if (l == null)
            return false;
        // if the object is in the station (settlement):
        if (l == this.baseindoors_location)
            return true;
        if (this.game.location_in[this.game.locations.indexOf(l)][this.game.locations.indexOf(this.baseoutdoors_location)])
            return true;
        // if it is the comunicator (of the object has the communicator):
        if (o == this.communicator_object ||
            o.findObjectByID(this.communicator_object.ID) != null) {
            // once the comm tower is repaired, the communicator can be reached anywhere
            if (this.game.comm_tower_repaired)
                return true;
            // communicator works only in spacer valley south:
            if (l == this.baseindoors_location)
                return true;
            if (l == this.baseoutdoors_location)
                return true;
            if (l == this.spacervalleysouth_location)
                return true;
            if (this.game.location_in[this.game.locations.indexOf(l)][this.game.locations.indexOf(this.spacervalleysouth_location)])
                return true;
        }
        return false;
    };
    // For ETAOIN seeing/hearing is the same:
    EtaoinAI.prototype.canHear = function (objectID) {
        return this.canSee(objectID);
    };
    EtaoinAI.prototype.restoreFromXML = function (xml) {
        _super.prototype.restoreFromXML.call(this, xml);
        var xml_tmp = getFirstElementChildByTag(xml, "oxygen_message_timer");
        if (xml_tmp != null) {
            this.oxygen_message_timer = Number(xml_tmp.getAttribute("value"));
        }
    };
    EtaoinAI.prototype.savePropertiesToXML = function () {
        var str = _super.prototype.savePropertiesToXML.call(this) + "\n";
        str += "<oxygen_message_timer value=\"" + this.oxygen_message_timer + "\"/>";
        return str;
    };
    return EtaoinAI;
}(A4RuleBasedAI));
