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
var A4RuleBasedAI = /** @class */ (function (_super) {
    __extends(A4RuleBasedAI, _super);
    function A4RuleBasedAI(o, nlp, game, pf, pfoffset, qpt) {
        var _this = _super.call(this, o, nlp, pf, pfoffset, qpt) || this;
        _this.game = null;
        _this.alreadyProcessedPBRs = []; // this is a small cache, so that we do not parse the same text
        // more than once to save CPU resources.
        // If the player requests to go here, or instructs qwerty/shrdlu to go ther, she will not be allowed:
        _this.locationsWherePlayerIsNotPermitted = [];
        _this.doorsPlayerIsNotPermittedToOpen = [];
        // variables so that the game script can control the AI:
        _this.respondToPerformatives = false;
        _this.visionActive = true; // if this is "false", the robot is blind
        _this.cache_sort_bright = null;
        _this.cache_sort_dark = null;
        _this.cache_sort_west = null;
        _this.cache_sort_north = null;
        _this.cache_sort_east = null;
        _this.cache_sort_south = null;
        _this.cache_sort_powered_state = null;
        _this.cache_sort_powered_on = null;
        _this.cache_sort_powered_off = null;
        _this.cache_sort_space_iside_of = null;
        _this.cache_sort_have = null;
        _this.cache_sort_need = null;
        _this.game = game;
        _this.inferenceEffectFactory = new A4InferenceEffectFactory();
        _this.intentionHandlers.push(new Call_IntentionAction());
        _this.intentionHandlers.push(new Memorize_IntentionAction());
        _this.intentionHandlers.push(new AnswerPredicate_IntentionAction());
        _this.intentionHandlers.push(new AnswerWhoIs_IntentionAction());
        _this.intentionHandlers.push(new AnswerWhatIs_IntentionAction());
        _this.intentionHandlers.push(new AnswerQuery_IntentionAction());
        _this.intentionHandlers.push(new AnswerHowMany_IntentionAction());
        _this.intentionHandlers.push(new AnswerWhen_IntentionAction());
        _this.intentionHandlers.push(new AnswerWhy_IntentionAction());
        _this.intentionHandlers.push(new A4AnswerHow_IntentionAction());
        _this.intentionHandlers.push(new AnswerDefine_IntentionAction());
        _this.intentionHandlers.push(new AnswerHearSee_IntentionAction());
        _this.intentionHandlers.push(new AnswerWhere_IntentionAction());
        _this.intentionHandlers.push(new AnswerDistance_IntentionAction());
        _this.intentionHandlers.push(new A4Locate_IntentionAction());
        _this.allowPlayerInto("location-as1", null);
        _this.allowPlayerInto("location-as2", null);
        _this.allowPlayerInto("location-as3", null);
        _this.doNotAllowPlayerInto("location-as4", "BEDROOM1");
        _this.doNotAllowPlayerInto("location-as5", "BEDROOM2");
        _this.doNotAllowPlayerInto("location-as6", "BEDROOM3");
        _this.doNotAllowPlayerInto("location-as7", "BEDROOM4");
        _this.allowPlayerInto("location-as8", "BEDROOM5");
        _this.doNotAllowPlayerInto("location-as9", "BEDROOM6");
        _this.doNotAllowPlayerInto("location-as10", "BEDROOM7");
        _this.doNotAllowPlayerInto("location-as11", "BEDROOM8");
        _this.doNotAllowPlayerInto("location-as12", "BEDROOM9");
        _this.doNotAllowPlayerInto("location-as13", "BEDROOM10");
        _this.doNotAllowPlayerInto("location-as14", "BEDROOM11");
        _this.doNotAllowPlayerInto("location-as15", "BEDROOM12");
        _this.allowPlayerInto("location-as16", null);
        _this.allowPlayerInto("location-as17", null);
        _this.allowPlayerInto("location-as18", null);
        _this.allowPlayerInto("location-as19", null);
        _this.allowPlayerInto("location-as20", null);
        _this.allowPlayerInto("location-as21", null);
        _this.allowPlayerInto("location-as22", null);
        _this.allowPlayerInto("location-as23", null);
        _this.allowPlayerInto("location-as24", null);
        _this.allowPlayerInto("location-as25", null);
        _this.doNotAllowPlayerInto("location-as26", "STASIS");
        _this.allowPlayerInto("location-as27", null);
        _this.doNotAllowPlayerInto("location-maintenance", "MAINTENANCE");
        _this.doNotAllowPlayerInto("location-as29", "GARAGE");
        _this.doNotAllowPlayerInto("location-garage", "COMMAND");
        _this.allowPlayerInto("location-as31", null);
        _this.allowPlayerInto("location-as32", null);
        _this.allowPlayerInto("location-as33", null);
        _this.allowPlayerInto("location-as34", null);
        _this.cache_sort_bright = _this.o.getSort("bright");
        _this.cache_sort_dark = _this.o.getSort("dark");
        _this.cache_sort_west = _this.o.getSort("west");
        _this.cache_sort_north = _this.o.getSort("north");
        _this.cache_sort_east = _this.o.getSort("east");
        _this.cache_sort_south = _this.o.getSort("south");
        _this.cache_sort_powered_state = _this.o.getSort("powered.state");
        _this.cache_sort_powered_on = _this.o.getSort("powered.on");
        _this.cache_sort_powered_off = _this.o.getSort("powered.off");
        _this.cache_sort_space_iside_of = _this.o.getSort("space.inside.of");
        _this.cache_sort_have = _this.o.getSort("verb.have");
        _this.cache_sort_need = _this.o.getSort("verb.need");
        _this.predicatesToStoreInLongTermMemory = [];
        _this.predicatesToStoreInLongTermMemory.push(_this.cache_sort_action_talk);
        _this.predicatesToStoreInLongTermMemory.push(_this.cache_sort_space_at);
        return _this;
    }
    A4RuleBasedAI.prototype.precalculateLocationKnowledge = function (game, o) {
        // First, remove any location knowledge that was previously in the KB:
        this.longTermMemory.removeAllWithProvenance(LOCATIONS_PROVENANCE);
        // console.log("RuleBasedAI.precalculateLocationKnowledge...");
        for (var _i = 0, _a = game.locations; _i < _a.length; _i++) {
            var location_1 = _a[_i];
            var str = location_1.sort.name + "('" + location_1.id + "'[#id])";
            var term = Term.fromString(str, o);
            //console.log(term.toString());
            this.addLongTermTerm(term, LOCATIONS_PROVENANCE);
            if (location_1.name != null) {
                var str_1 = "name('" + location_1.id + "'[#id], '" + location_1.name + "'[symbol])";
                var term_1 = Term.fromString(str_1, o);
                //console.log(term.toString());
                //this.addLongTermTerm(term, LOCATIONS_PROVENANCE);
                // if has to be added this way, since otherwise, it's treated like a #StateSort, and it removes the previous
                // names we might have added!
                this.addLongTermRuleNow(new Sentence([term_1], [true]), LOCATIONS_PROVENANCE);
                if (location_1.name.indexOf(' ') != -1) {
                    // it's a multitoken! we should add it:
                    this.naturalLanguageParser.posParser.addMultiToken(location_1.name);
                }
                this.naturalLanguageParser.posParser.addTokenPOS(new PartOfSpeech(location_1.name, location_1.name, Term.fromString("proper-noun('" + location_1.name + "'[symbol], [singular])", o), 1.0));
            }
        }
        var n_space_at = 0;
        var n_not_space_at = 0;
        var n_space_connects = 0;
        // let debug_text:string = "";
        for (var idx_l1 = 0; idx_l1 < game.locations.length; idx_l1++) {
            var l1 = game.locations[idx_l1];
            for (var idx_l2 = 0; idx_l2 < game.locations.length; idx_l2++) {
                var l2 = game.locations[idx_l2];
                if (l1 == l2)
                    continue;
                if (game.location_in[idx_l1][idx_l2]) {
                    var somethingInBetween = false;
                    for (var idx_l3 = 0; idx_l3 < game.locations.length; idx_l3++) {
                        if (idx_l3 != idx_l1 && idx_l3 != idx_l2 &&
                            game.location_in[idx_l1][idx_l3] &&
                            game.location_in[idx_l3][idx_l2]) {
                            somethingInBetween = true;
                            break;
                        }
                    }
                    if (!somethingInBetween) {
                        //let term:Term = Term.fromString("space.at('"+l1.id+"'[#id], '"+l2.id+"'[#id])", o);
                        var term = Term.fromString("space.inside.of('" + l1.id + "'[#id], '" + l2.id + "'[#id])", o);
                        //if (this.selfID == "etaoin") console.log(term.toString());
                        // this.addLongTermTerm(term, LOCATIONS_PROVENANCE);
                        // if has to be added this way, since otherwise, it's treated like a #StateSort, and it removes the previous
                        // names we might have added!
                        this.addLongTermRuleNow(new Sentence([term], [true]), LOCATIONS_PROVENANCE);
                        n_space_at++;
                        // debug_text += term + "\n";
                    }
                }
                else {
                    var mostSpecific = true;
                    for (var idx_l3 = 0; idx_l3 < game.locations.length; idx_l3++) {
                        if (idx_l3 != idx_l1 && idx_l3 != idx_l2 &&
                            game.location_in[idx_l1][idx_l3] &&
                            !game.location_in[idx_l2][idx_l3]) {
                            mostSpecific = false;
                            break;
                        }
                    }
                    if (mostSpecific) {
                        var s = Sentence.fromString("~space.inside.of('" + l1.id + "'[#id], '" + l2.id + "'[#id])", o);
                        this.addLongTermRuleNow(s, LOCATIONS_PROVENANCE);
                        n_not_space_at++;
                        // debug_text += s + "\n";
                    }
                }
            }
        }
        for (var idx_l1 = 0; idx_l1 < game.locations.length; idx_l1++) {
            //			console.log("idx: " + idx_l1);
            var l1 = game.locations[idx_l1];
            for (var idx_l2 = 0; idx_l2 < game.locations.length; idx_l2++) {
                var l2 = game.locations[idx_l2];
                if (l1 == l2)
                    continue;
                if (game.location_in[idx_l1][idx_l2] ||
                    game.location_in[idx_l2][idx_l1])
                    continue;
                if (game.location_connects[idx_l1][idx_l2]) {
                    var str = "space.connects('" + l1.id + "'[" + l1.sort.name + "], '" + l2.id + "'[" + l2.sort.name + "])";
                    var term = Term.fromString(str, o);
                    //console.log(term.toString());
                    this.addLongTermTerm(term, LOCATIONS_PROVENANCE);
                    n_space_connects++;
                    // debug_text += str + "\n";
                }
            }
        }
        console.log("RuleBasedAI.precalculateLocationKnowledge: " + n_space_at + ", " + n_not_space_at + ", " + n_space_connects);
        // downloadStringAsFile(debug_text, "location-predicates.txt");
    };
    A4RuleBasedAI.prototype.add3DPrintingKnowledge = function (game, o, IDofAIthatCanPrint) {
        for (var _i = 0, _a = game.three_d_printer_recipies; _i < _a.length; _i++) {
            var recipe = _a[_i];
            var item = recipe[0];
            var materials = recipe[1];
            var term = Term.fromString("verb.can('" + IDofAIthatCanPrint + "'[#id], action.print('" + IDofAIthatCanPrint + "'[#id], [" + item + "]))", o);
            this.addLongTermRuleNow(new Sentence([term], [true]), BACKGROUND_PROVENANCE);
            term = Term.fromString("verb.can('player'[#id], action.print('player'[#id], [" + item + "]))", o);
            this.addLongTermRuleNow(new Sentence([term], [true]), BACKGROUND_PROVENANCE);
            var sentence = Sentence.fromString("~metal-3dprinter(X) ; verb.can('" + IDofAIthatCanPrint + "'[#id], action.print('" + IDofAIthatCanPrint + "'[#id], [" + item + "], X))", o);
            this.addLongTermRuleNow(sentence, BACKGROUND_PROVENANCE);
            sentence = Sentence.fromString("~metal-3dprinter(X) ; verb.can('player'[#id], action.print('player'[#id], [" + item + "], X))", o);
            console.log(sentence.toString());
            this.addLongTermRuleNow(sentence, BACKGROUND_PROVENANCE);
            if (materials.length == 1 && materials[0] == "plastic") {
                var sentence2 = Sentence.fromString("~plastic-3dprinter(X) ; verb.can('" + IDofAIthatCanPrint + "'[#id], action.print('" + IDofAIthatCanPrint + "'[#id], [" + item + "], X))", o);
                this.addLongTermRuleNow(sentence2, BACKGROUND_PROVENANCE);
                sentence2 = Sentence.fromString("~plastic-3dprinter(X) ; verb.can('player'[#id], action.print('player'[#id], [" + item + "], X))", o);
                this.addLongTermRuleNow(sentence2, BACKGROUND_PROVENANCE);
            }
            for (var _b = 0, materials_1 = materials; _b < materials_1.length; _b++) {
                var material = materials_1[_b];
                term = Term.fromString("verb.need-for(X:[#id], [" + material + "], action.print(X, '" + item + "'[" + item + "]))", o);
                this.addLongTermRuleNow(new Sentence([term], [true]), BACKGROUND_PROVENANCE);
            }
        }
    };
    A4RuleBasedAI.prototype.perception = function (x0, y0, x1, y1, location, map, visibilityRegion, occupancyMap, ifVisionNotActive) {
        var l = [];
        if (this.visionActive) {
            l = map.getAllObjects(x0, y0, (x1 - x0), (y1 - y0));
        }
        else if (ifVisionNotActive != null) {
            l = map.getAllObjects(ifVisionNotActive[0], ifVisionNotActive[1], ifVisionNotActive[2] - ifVisionNotActive[0], ifVisionNotActive[3] - ifVisionNotActive[1]);
        }
        //		console.log("location: " + location.name + " l.length = " + l.length + " l.sort = " + location.sort);
        if (this.visionActive) {
            this.addTermToPerception(Term.fromString("property.sighted('" + this.selfID + "'[#id])", this.o));
        }
        else {
            this.addTermToPerception(Term.fromString("property.blind('" + this.selfID + "'[#id])", this.o));
        }
        this.addTermToPerception(new Term(location.sort, [new ConstantTermAttribute(location.id, this.cache_sort_id)]));
        // perceive the light status:
        if (this.visionActive) {
            if (this.game.rooms_with_lights.indexOf(location.id) != -1) {
                if (this.game.rooms_with_lights_on.indexOf(location.id) != -1) {
                    // lights on! room is bright:
                    this.addTermToPerception(new Term(this.cache_sort_bright, [new ConstantTermAttribute(location.id, this.cache_sort_id)]));
                }
                else {
                    // lights off! room is dark:
                    this.addTermToPerception(new Term(this.cache_sort_dark, [new ConstantTermAttribute(location.id, this.cache_sort_id)]));
                }
            }
        }
        var perceivedLocations = [location];
        for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
            var o = l_1[_i];
            var tile_ox = Math.floor(o.x / map.tileWidth);
            var tile_oy = Math.floor(o.y / map.tileHeight);
            var tile_ow = Math.floor(o.getPixelWidth() / map.tileWidth);
            var tile_oh = Math.floor(o.getPixelHeight() / map.tileWidth);
            var offset = tile_ox + tile_oy * map.width;
            // - Doors are usually in between visibility regions, and thus, we just perceive them all, and that's it!
            // - East cave is also an exception, since the rocks are just to prevent the player from seeing Shrdlu, but
            //   Shrdlu should be able to hear the player from a different visibilityRegion
            if (map.visibilityRegions[offset] == visibilityRegion ||
                (tile_ox > 0 && map.visibilityRegions[offset - 1] == visibilityRegion) ||
                (tile_ox < map.width - tile_ow && map.visibilityRegions[offset + tile_ow] == visibilityRegion) ||
                (tile_oy > 0 && map.visibilityRegions[offset - map.width] == visibilityRegion) ||
                (tile_oy < map.height - tile_oh && map.visibilityRegions[offset + tile_oh * map.width] == visibilityRegion) ||
                o instanceof A4Door ||
                o.ID == "tardis-broken-cable" || // exception: since this is inside the wall, they don't see it otherwise!
                map.name == "East Cave") {
                var locationID = location.id;
                if (!occupancyMap[offset]) {
                    // it's not in "location":
                    var l2 = this.game.getAILocation(o);
                    if (l2 != null)
                        locationID = l2.id;
                    if (perceivedLocations.indexOf(l2) == -1) {
                        perceivedLocations.push(l2);
                        this.addTermToPerception(new Term(l2.sort, [new ConstantTermAttribute(l2.id, this.cache_sort_id)]));
                    }
                }
                // perceived an object!
                var term1 = new Term(o.sort, [new ConstantTermAttribute(o.ID, this.cache_sort_id)]);
                var term2 = new Term(this.cache_sort_space_iside_of, [new ConstantTermAttribute(o.ID, this.cache_sort_id),
                    new ConstantTermAttribute(locationID, this.cache_sort_id)
                    //										   new ConstantTermAttribute(tile_ox, this.cache_sort_number),
                    //										   new ConstantTermAttribute(tile_oy, this.cache_sort_number),
                    //										   new ConstantTermAttribute(map.name, this.cache_sort_symbol)
                ]);
                //				console.log(term1.toString());
                //				console.log(term2.toString());
                this.addTermToPerception(term1);
                this.addTermToPerception(term2);
                for (var _a = 0, _b = this.getBaseObjectProperties(o); _a < _b.length; _a++) {
                    var property = _b[_a];
                    this.addTermToPerception(property);
                }
                if (o instanceof A4Character) {
                    for (var _c = 0, _d = o.inventory; _c < _d.length; _c++) {
                        var o2 = _d[_c];
                        var term3 = new Term(o2.sort, [new ConstantTermAttribute(o2.ID, this.cache_sort_id)]);
                        var term4 = new Term(this.cache_sort_space_iside_of, [new ConstantTermAttribute(o2.ID, this.cache_sort_id),
                            new ConstantTermAttribute(locationID, this.cache_sort_id)
                        ]);
                        this.addTermToPerception(term3);
                        this.addTermToPerception(term4);
                        this.addTermToPerception(new Term(this.cache_sort_verb_have, [new ConstantTermAttribute(o.ID, this.cache_sort_id),
                            new ConstantTermAttribute(o2.ID, this.cache_sort_id)]));
                        for (var _e = 0, _f = this.getBaseObjectProperties(o2); _e < _f.length; _e++) {
                            var property = _f[_e];
                            this.addTermToPerception(property);
                        }
                    }
                }
                else if (o instanceof A4Container) {
                    for (var _g = 0, _h = o.content; _g < _h.length; _g++) {
                        var o2 = _h[_g];
                        var term3 = new Term(o2.sort, [new ConstantTermAttribute(o2.ID, this.cache_sort_id)]);
                        var term4 = new Term(this.cache_sort_space_iside_of, [new ConstantTermAttribute(o2.ID, this.cache_sort_id),
                            new ConstantTermAttribute(locationID, this.cache_sort_id)
                        ]);
                        this.addTermToPerception(term3);
                        this.addTermToPerception(term4);
                        this.addTermToPerception(new Term(this.cache_sort_verb_contains, [new ConstantTermAttribute(o.ID, this.cache_sort_id),
                            new ConstantTermAttribute(o2.ID, this.cache_sort_id)]));
                        for (var _j = 0, _k = this.getBaseObjectProperties(o2); _j < _k.length; _j++) {
                            var property = _k[_j];
                            this.addTermToPerception(property);
                        }
                    }
                }
                else if (o instanceof A4Vehicle) {
                    for (var _l = 0, _m = o.load; _l < _m.length; _l++) {
                        var o2 = _m[_l];
                        var term3 = new Term(o2.sort, [new ConstantTermAttribute(o2.ID, this.cache_sort_id)]);
                        var term4 = new Term(this.cache_sort_space_iside_of, [new ConstantTermAttribute(o2.ID, this.cache_sort_id),
                            new ConstantTermAttribute(locationID, this.cache_sort_id)
                        ]);
                        this.addTermToPerception(term3);
                        this.addTermToPerception(term4);
                        this.addTermToPerception(new Term(this.cache_sort_verb_contains, [new ConstantTermAttribute(o.ID, this.cache_sort_id),
                            new ConstantTermAttribute(o2.ID, this.cache_sort_id)]));
                        for (var _o = 0, _p = this.getBaseObjectProperties(o2); _o < _p.length; _o++) {
                            var property = _p[_o];
                            this.addTermToPerception(property);
                        }
                    }
                }
                else if (o.sort.is_a_string("light")) {
                    if (this.game.rooms_with_lights.indexOf(location.id) != -1) {
                        if (this.game.rooms_with_lights_on.indexOf(location.id) != -1) {
                            this.addTermToPerception(new Term(this.cache_sort_powered_state, [new ConstantTermAttribute(o.ID, this.cache_sort_id), new ConstantTermAttribute(this.cache_sort_powered_on.name, this.cache_sort_powered_on)]));
                        }
                        else {
                            this.addTermToPerception(new Term(this.cache_sort_powered_state, [new ConstantTermAttribute(o.ID, this.cache_sort_id), new ConstantTermAttribute(this.cache_sort_powered_off.name, this.cache_sort_powered_off)]));
                        }
                    }
                }
                else if (o.sort.is_a_string("plastic-3dprinter")) {
                    for (var _q = 0, _r = ["plastic"]; _q < _r.length; _q++) {
                        var material = _r[_q];
                        if (o.getStoryStateVariable(material) == "true") {
                            this.addTermToPerception(new Term(this.cache_sort_have, [new ConstantTermAttribute(o.ID, this.cache_sort_id), new VariableTermAttribute(this.o.getSort(material), null)]));
                        }
                        else {
                            this.addTermToPerception(new Term(this.cache_sort_need, [new ConstantTermAttribute(o.ID, this.cache_sort_id), new VariableTermAttribute(this.o.getSort(material), null)]));
                        }
                    }
                }
                else if (o.sort.is_a_string("metal-3dprinter")) {
                    for (var _s = 0, _t = ["plastic", "iron", "aluminium", "copper"]; _s < _t.length; _s++) {
                        var material = _t[_s];
                        if (o.getStoryStateVariable(material) == "true") {
                            this.addTermToPerception(new Term(this.cache_sort_have, [new ConstantTermAttribute(o.ID, this.cache_sort_id), new VariableTermAttribute(this.o.getSort(material), null)]));
                        }
                        else {
                            this.addTermToPerception(new Term(this.cache_sort_need, [new ConstantTermAttribute(o.ID, this.cache_sort_id), new VariableTermAttribute(this.o.getSort(material), null)]));
                        }
                    }
                }
            }
        }
        // actions:
        for (var _u = 0, _v = map.perceptionBuffer; _u < _v.length; _u++) {
            var pbr = _v[_u];
            if (pbr.x0 < x1 && pbr.x1 > x0 &&
                pbr.y0 < y1 && pbr.y1 > y0) {
                var tile_ox = Math.floor(pbr.x0 / map.tileWidth);
                var tile_oy = Math.floor(pbr.y0 / map.tileHeight);
                var offset = tile_ox + tile_oy * map.width;
                if ((map.visibilityRegions[offset] == visibilityRegion ||
                    map.name == "East Cave") &&
                    this.alreadyProcessedPBRs.indexOf(pbr) == -1) {
                    // we always perceive "talk", but the rest only if the AI can see:
                    if (pbr.action == "talk" || this.visionActive) {
                        this.perceivePBR(pbr);
                    }
                }
            }
        }
        if (this.game.communicatorConnectedTo == this.selfID &&
            this.game.currentPlayer.findObjectByID("communicator") != null) {
            // we can hear david throught the communicator:
            for (var _w = 0, _x = this.game.currentPlayer.map.perceptionBuffer; _w < _x.length; _w++) {
                var pbr = _x[_w];
                if (pbr.action == "talk" && pbr.subjectID == "player" &&
                    this.alreadyProcessedPBRs.indexOf(pbr) == -1) {
                    // we can hear it!
                    this.perceivePBR(pbr);
                }
            }
        }
        // internal clock:
        var timeTerm = new Term(this.cache_sort_time_current, [new ConstantTermAttribute(this.timeStamp, this.o.getSort("number"))]);
        this.addTermToPerception(timeTerm);
    };
    A4RuleBasedAI.prototype.perceivePBR = function (pbr) {
        if (this.alreadyProcessedPBRs.length >= 50)
            this.alreadyProcessedPBRs.slice(0, 49);
        this.alreadyProcessedPBRs.push(pbr);
        // perceived an action!
        var actionTerms = [Term.fromString("action." + pbr.action + "(" +
                // "'"+pbr.time+"'[number],"+
                "'" + pbr.subjectID + "'[#id])", this.o)];
        if (pbr.directObjectID != null) {
            for (var _i = 0, actionTerms_1 = actionTerms; _i < actionTerms_1.length; _i++) {
                var actionTerm = actionTerms_1[_i];
                actionTerm.addAttribute(new ConstantTermAttribute(pbr.directObjectID, this.o.getSort("#id")));
            }
        }
        else if (pbr.directObjectSymbol != null &&
            pbr.subjectID != this.selfID) {
            // assume that this is a "talk" action:
            var context = null;
            var speaker = pbr.subjectID;
            for (var _a = 0, actionTerms_2 = actionTerms; _a < actionTerms_2.length; _a++) {
                var actionTerm = actionTerms_2[_a];
                actionTerm.addAttribute(new ConstantTermAttribute(pbr.directObjectSymbol, this.o.getSort("#id")));
                // update context perception:
                context = this.updateContext(speaker);
            }
            // parse the text:
            this.parsePerceivedText(pbr.directObjectSymbol, speaker, context, actionTerms);
        }
        if (pbr.indirectObjectID != null) {
            for (var _b = 0, actionTerms_3 = actionTerms; _b < actionTerms_3.length; _b++) {
                var actionTerm = actionTerms_3[_b];
                actionTerm.addAttribute(new ConstantTermAttribute(pbr.indirectObjectID, pbr.indirectObjectSort));
            }
        }
        for (var _c = 0, actionTerms_4 = actionTerms; _c < actionTerms_4.length; _c++) {
            var actionTerm = actionTerms_4[_c];
            // console.log(actionTerm + " added to perception");
            this.addTermToPerception(actionTerm);
        }
    };
    // distance in meters (assuming each tile is a meter)
    A4RuleBasedAI.prototype.distanceBetweenIds = function (source, target) {
        // We assume that unless the AI is seeing an object, it does not know where it is:
        var sourceObject = null;
        var targetObject = null;
        // We assume that the AIs know all the locations:
        var sourceLocation = this.game.getAILocationByID(source);
        var targetLocation = this.game.getAILocationByID(target);
        for (var _i = 0, _a = this.shortTermMemory.plainTermList; _i < _a.length; _i++) {
            var te = _a[_i];
            var t = te.term;
            if (te.provenance == PERCEPTION_PROVENANCE &&
                t.functor.is_a(this.cache_sort_object) &&
                t.attributes.length == 1) {
                if (t.attributes[0].value == source) {
                    var tmp = this.game.findObjectByID(source);
                    if (tmp != null && tmp.length > 0)
                        sourceObject = tmp[0];
                }
                else if (t.attributes[0].value == target) {
                    var tmp = this.game.findObjectByID(target);
                    if (tmp != null && tmp.length > 0)
                        targetObject = tmp[0];
                }
            }
        }
        if (sourceObject != null && targetObject != null) {
            var x1 = targetObject.x + targetObject.getPixelWidth() / 2;
            var y1 = targetObject.y + targetObject.getPixelHeight() / 2;
            var x2 = sourceObject.x + sourceObject.getPixelWidth() / 2;
            var y2 = sourceObject.y + sourceObject.getPixelHeight() / 2;
            return this.distanceBetweenCoordinates(x1, y1, targetObject.map, x2, y2, sourceObject.map);
        }
        if (sourceObject != null && targetLocation != null) {
            var mapIdx = targetLocation.maps.indexOf(sourceObject.map);
            if (mapIdx != -1) {
                return targetLocation.distanceFromObject(sourceObject, mapIdx);
            }
            else {
                var _b = targetLocation.centerCoordinatesInMap(targetLocation.maps[0]), x1 = _b[0], y1 = _b[1];
                var x2 = sourceObject.x + sourceObject.getPixelWidth() / 2;
                var y2 = sourceObject.y + sourceObject.getPixelHeight() / 2;
                return this.distanceBetweenCoordinates(x1, y1, targetLocation.maps[0], x2, y2, sourceObject.map);
            }
        }
        if (sourceLocation != null && targetObject != null) {
            var mapIdx = sourceLocation.maps.indexOf(targetObject.map);
            if (mapIdx != -1) {
                return sourceLocation.distanceFromObject(targetObject, mapIdx);
            }
            else {
                var x1 = targetObject.x + targetObject.getPixelWidth() / 2;
                var y1 = targetObject.y + targetObject.getPixelHeight() / 2;
                var _c = sourceLocation.centerCoordinatesInMap(sourceLocation.maps[0]), x2 = _c[0], y2 = _c[1];
                return this.distanceBetweenCoordinates(x1, y1, targetObject.map, x2, y2, sourceLocation.maps[0]);
            }
        }
        if (sourceLocation != null && targetLocation != null) {
            var d_1 = sourceLocation.distanceFromLocation(targetLocation, this.game);
            if (d_1 != null) {
                return d_1;
            }
            else {
                var _d = targetLocation.centerCoordinatesInMap(targetLocation.maps[0]), x1 = _d[0], y1 = _d[1];
                var _e = sourceLocation.centerCoordinatesInMap(sourceLocation.maps[0]), x2 = _e[0], y2 = _e[1];
                return this.distanceBetweenCoordinates(x1, y1, targetLocation.maps[0], x2, y2, sourceLocation.maps[0]);
            }
        }
        // special cases:
        /**
        // NOTE: this is bad, I should have these in the KB in some declarative form!
        if (source == 'earth') {
            if (targetObject != null || targetLocation != null || target == 'aurora') {
                return 1.1263e+17;
            }
        }
        if (target == 'earth') {
            if (sourceObject != null || sourceLocation != null || source == 'aurora') {
                return 1.1263e+17;
            }
        }
        if (source == 'aurora') {
            if (targetObject != null || targetLocation != null) {
                return 0;
            }
        }
        if (target == 'aurora') {
            if (sourceObject != null || sourceLocation != null) {
                return 0;
            }
        }
        */
        return _super.prototype.distanceBetweenIds.call(this, source, target);
    };
    /* using a simple BFS search strategy, and returns results in meters */
    A4RuleBasedAI.prototype.distanceBetweenCoordinates = function (x1, y1, map1, x2, y2, map2) {
        var best_d = this.distanceBetweenCoordinates_internal(x1, y1, map1, x2, y2, map2, [map1]);
        if (best_d == null && (map1.name == "Trantor Crater" || map2.name == "Trantor Crater")) {
            // since there are no connections to Trantor Crater, this has to be hardcoded (Assuming we are on the other maps):
            return 8192000;
        }
        if (best_d == null && (map1.name == "Tardis 8" || map2.name == "Tardis 8")) {
            // since there are no connections to Trantor Crater, this has to be hardcoded:
            return 8192000;
        }
        return best_d;
    };
    A4RuleBasedAI.prototype.distanceBetweenCoordinates_internal = function (x1, y1, map1, x2, y2, map2, path) {
        if (map1 == map2) {
            return (Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))) / map1.pixelsPerMeter;
        }
        var best_d = null;
        for (var _i = 0, _a = map1.bridges; _i < _a.length; _i++) {
            var bridge = _a[_i];
            if (path.indexOf(bridge.linkedTo.map) == -1) {
                path.push(bridge.linkedTo.map);
                var bx = bridge.x + bridge.width / 2;
                var by = bridge.y + bridge.height / 2;
                var d1 = this.distanceBetweenCoordinates_internal(bridge.linkedTo.x + bridge.linkedTo.width / 2, bridge.linkedTo.y + bridge.linkedTo.height / 2, bridge.linkedTo.map, x2, y2, map2, path);
                var d2 = (Math.sqrt((x1 - bx) * (x1 - bx) + (y1 - by) * (y1 - by))) / map1.pixelsPerMeter;
                path.pop();
                if (d1 != null) {
                    if (best_d == null || (d1 + d2) < best_d)
                        best_d = d1 + d2;
                }
            }
        }
        if (map1.name == "Aurora Station" || map1.name == "Aurora Station Outdoors") {
            for (var _b = 0, _c = map1.objects; _b < _c.length; _b++) {
                var o = _c[_b];
                if (o instanceof ShrdluAirlockDoor) {
                    var ald = o;
                    var targetMap = this.game.getMap(ald.targetMap);
                    if (path.indexOf(targetMap) == -1) {
                        path.push(targetMap);
                        var bx = ald.x + ald.getPixelWidth() / 2;
                        var by = ald.y + ald.getPixelHeight() / 2;
                        var d1 = this.distanceBetweenCoordinates_internal(ald.targetX, ald.targetY, targetMap, x2, y2, map2, path);
                        var d2 = (Math.sqrt((x1 - bx) * (x1 - bx) + (y1 - by) * (y1 - by))) / map1.pixelsPerMeter;
                        path.pop();
                        if (d1 != null) {
                            if (best_d == null || (d1 + d2) < best_d)
                                best_d = d1 + d2;
                        }
                    }
                }
            }
        }
        return best_d;
    };
    A4RuleBasedAI.prototype.updateContext = function (speaker) {
        var context = this.contextForSpeaker(speaker);
        if (context.lastTimeUpdated >= this.timeStamp)
            return context;
        context.lastTimeUpdated = this.timeStamp;
        context.shortTermMemory = [];
        //		console.log("updateContext: speaker: " + speakerObject)
        // add from perception:
        var alreadyUpdatedEntities = [];
        for (var _i = 0, _a = this.shortTermMemory.plainTermList; _i < _a.length; _i++) {
            var te = _a[_i];
            var t = te.term;
            if ((t.functor.is_a(this.cache_sort_object) ||
                t.functor.is_a(this.cache_sort_space_location) ||
                t.functor.is_a(this.cache_sort_property) ||
                t.functor.is_a(this.cache_sort_relation))) {
                if (t.attributes[0] instanceof ConstantTermAttribute) {
                    var id = t.attributes[0].value;
                    if (alreadyUpdatedEntities.indexOf(id) == -1) {
                        alreadyUpdatedEntities.push(id);
                        var distanceFromSpeaker = this.distanceBetweenIds(speaker, id);
                        var e = context.newContextEntity(t.attributes[0], 0, distanceFromSpeaker, this.o, false);
                        if (e != null && context.shortTermMemory.indexOf(e) == -1)
                            context.shortTermMemory.push(e);
                    }
                }
            }
        }
        context.sortEntities();
        return context;
    };
    A4RuleBasedAI.prototype.resolveThere = function (otherCharacterID, otherCharacterLocation) {
        var context = this.contextForSpeaker(otherCharacterID);
        if (context == null)
            return null;
        // We need to see what "there" refers to:
        // 1) if there is a place mentioned in the previous performative, then that's it
        //		let perf:NLContextPerformative = context.lastPerformativeBy(otherCharacterID);
        var perf = context.lastPerformativeBy(this.selfID);
        if (perf != null) {
            var IDs = [];
            for (var i = 0; i < perf.performative.attributes.length; i++) {
                if (perf.performative.attributes[i] instanceof ConstantTermAttribute) {
                    IDs.push((perf.performative.attributes[0]));
                }
                else if (perf.performative.attributes[i] instanceof TermTermAttribute) {
                    NLContext.searchForIDsInClause(perf.performative.attributes[i].term, IDs, context.ai.o);
                }
            }
            var locations = [];
            for (var _i = 0, IDs_1 = IDs; _i < IDs_1.length; _i++) {
                var ID = IDs_1[_i];
                var loc = this.game.getAILocationByID(ID.value);
                if (loc != null)
                    locations.push(loc);
            }
            if (locations.length == 1)
                return locations[0];
        }
        // 2) otherwise, it refers to the location of the listener, unless it's the same as that of the speaker
        var me = this.game.findObjectByIDJustObject(this.selfID);
        if (me != null) {
            var myLoc = this.game.getAILocation(me);
            if (myLoc != otherCharacterLocation)
                return myLoc;
        }
        return null;
    };
    A4RuleBasedAI.prototype.canSee = function (characterID) {
        if (!this.visionActive)
            return false;
        // if the character is in the perception buffer:
        var objectSort = this.o.getSort("object");
        var locationSort = this.o.getSort("space.location");
        for (var _i = 0, _a = this.shortTermMemory.plainTermList; _i < _a.length; _i++) {
            var tc = _a[_i];
            var t = tc.term;
            if ((t.functor.is_a(objectSort) || t.functor.is_a(locationSort)) &&
                t.attributes.length == 1 &&
                t.attributes[0] instanceof ConstantTermAttribute &&
                t.attributes[0].value == "" + characterID) {
                return true;
            }
        }
        return false;
    };
    A4RuleBasedAI.prototype.canHear = function (objectID) {
        var o = this.game.findObjectByIDJustObject(objectID);
        if (o == null)
            return false;
        return true;
    };
    // reference object is used in case o2 is not a directional object, to determine what is "behind" and "in front"
    A4RuleBasedAI.prototype.checkSpatialRelation = function (relation, o1ID, o2ID, referenceObject) {
        if (relation.is_a(this.cache_sort_space_at) ||
            relation.name == "space.outside.of") {
            if (o1ID == o2ID)
                return false;
            var loc2 = this.game.getAILocationByID(o2ID); // see if o2 is a location
            if (loc2 == null) {
                // if o2ID is not a location, maybe it's a container or a character:
                if (relation.is_a(this.cache_sort_space_at)) {
                    var o1l_2 = this.game.findObjectByID(o1ID); // see if o1 is an object
                    if (o1l_2 == null)
                        return null;
                    for (var _i = 0, o1l_1 = o1l_2; _i < o1l_1.length; _i++) {
                        var o2 = o1l_1[_i];
                        if (o2.ID == o2ID)
                            return true;
                    }
                    return null;
                }
                else {
                    return null;
                }
            }
            var o1l = this.game.findObjectByID(o1ID); // see if o1 is an object
            var loc1 = null;
            if (o1l == null) {
                loc1 = this.game.getAILocationByID(o1ID); // if it's not an object, maybe it's a location
                if (loc1 == null)
                    return null; // we don't know!
            }
            else {
                loc1 = this.game.getAILocation(o1l[0]);
            }
            if (loc1 == null)
                return null;
            if (loc1 == loc2) {
                if (relation.is_a(this.cache_sort_space_at))
                    return true;
                return false;
            }
            if (relation.is_a(this.cache_sort_space_at)) {
                return this.game.location_in[this.game.locations.indexOf(loc1)][this.game.locations.indexOf(loc2)];
            }
            else {
                return !this.game.location_in[this.game.locations.indexOf(loc1)][this.game.locations.indexOf(loc2)] &&
                    !this.game.location_in[this.game.locations.indexOf(loc1)][this.game.locations.indexOf(loc2)];
            }
        }
        else if (relation.name == "space.near") {
            var distance = this.distanceBetweenIds(o1ID, o2ID);
            if (distance == null)
                return null;
            if (distance < SPACE_NEAR_FAR_THRESHOLD)
                return true;
            return false;
        }
        else if (relation.name == "space.far") {
            var distance = this.distanceBetweenIds(o1ID, o2ID);
            if (distance == null)
                return null;
            if (distance >= SPACE_NEAR_FAR_THRESHOLD)
                return true;
            return false;
        }
        else if (relation.name == "space.north.of" ||
            relation.name == "space.east.of" ||
            relation.name == "space.west.of" ||
            relation.name == "space.south.of" ||
            relation.name == "space.northeast.of" ||
            relation.name == "space.northwest.of" ||
            relation.name == "space.southeast.of" ||
            relation.name == "space.southwest.of" ||
            relation.name == "space.in.front.of" ||
            relation.name == "space.behind") {
            var o1 = this.game.findObjectByIDJustObject(o1ID);
            var o2 = this.game.findObjectByIDJustObject(o2ID);
            var inFrontDirection = A4_DIRECTION_NONE;
            if (o1 != null && o2 != null) {
                if (o1.map != o2.map)
                    return null;
                var x1 = Math.floor(o1.x + o1.getPixelWidth() / 2);
                var y1 = Math.floor(o1.y + o1.getPixelHeight() / 2);
                var x2 = Math.floor(o2.x + o2.getPixelWidth() / 2);
                var y2 = Math.floor(o2.y + o2.getPixelHeight() / 2);
                var dx = x1 - x2;
                var dy = y1 - y2;
                if (o2.x >= o1.x && o2.x + o2.getPixelWidth() <= o1.x + o1.getPixelWidth())
                    dx = 0;
                if (o2.y >= o1.y && o2.y + o2.getPixelHeight() <= o1.y + o1.getPixelHeight())
                    dy = 0;
                // find the reference direction:
                if (o2 instanceof A4Character) {
                    inFrontDirection = o2.direction;
                }
                else {
                    var or = this.game.findObjectByIDJustObject(referenceObject);
                    if (or == null || !(or instanceof A4Character)) {
                        // in this case, we just take the reference of the player:
                        or = this.game.currentPlayer;
                    }
                    if (or.map == o2.map) {
                        var o_dx = Math.floor(or.x + or.getPixelWidth() / 2) - x2;
                        var o_dy = Math.floor(or.y + or.getPixelHeight() / 2) - y2;
                        var angle = Math.atan2(o_dy, o_dx);
                        if (angle > -(6 * Math.PI / 8) && angle <= -(2 * Math.PI / 8)) {
                            inFrontDirection = A4_DIRECTION_UP;
                        }
                        else if (angle > -(2 * Math.PI / 8) && angle <= (2 * Math.PI / 8)) {
                            inFrontDirection = A4_DIRECTION_RIGHT;
                        }
                        else if (angle > (2 * Math.PI / 8) && angle <= (6 * Math.PI / 8)) {
                            inFrontDirection = A4_DIRECTION_DOWN;
                        }
                        else {
                            inFrontDirection = A4_DIRECTION_LEFT;
                        }
                    }
                }
                return this.checkSpatialRelationBetweenCoordinates(relation, dx, dy, inFrontDirection);
            }
            else {
                if (o1 == null) {
                    if (o2 == null) {
                        var loc1 = this.game.getAILocationByID(o1ID);
                        var loc2 = this.game.getAILocationByID(o2ID);
                        if (loc1 == null || loc2 == null)
                            return null;
                        // relation between two locations:
                        if (this.game.location_in[this.game.locations.indexOf(loc1)][this.game.locations.indexOf(loc2)] ||
                            this.game.location_in[this.game.locations.indexOf(loc2)][this.game.locations.indexOf(loc1)])
                            return false;
                        for (var _a = 0, _b = loc1.maps; _a < _b.length; _a++) {
                            var map = _b[_a];
                            if (loc2.maps.indexOf(map) != -1) {
                                var x1_y1 = loc1.centerCoordinatesInMap(map);
                                var x2_y2 = loc2.centerCoordinatesInMap(map);
                                if (x1_y1 != null && x2_y2 != null) {
                                    // find the reference direction:
                                    var or = this.game.findObjectByIDJustObject(referenceObject);
                                    if (or == null || !(or instanceof A4Character)) {
                                        // in this case, we just take the reference of the player:
                                        or = this.game.currentPlayer;
                                    }
                                    if (or.map == map) {
                                        var o_dx = Math.floor(or.x + or.getPixelWidth() / 2) - x2_y2[0];
                                        var o_dy = Math.floor(or.y + or.getPixelHeight() / 2) - x2_y2[0];
                                        var angle = Math.atan2(o_dy, o_dx);
                                        if (angle > -(6 * Math.PI / 8) && angle <= -(2 * Math.PI / 8)) {
                                            inFrontDirection = A4_DIRECTION_UP;
                                        }
                                        else if (angle > -(2 * Math.PI / 8) && angle <= (2 * Math.PI / 8)) {
                                            inFrontDirection = A4_DIRECTION_RIGHT;
                                        }
                                        else if (angle > (2 * Math.PI / 8) && angle <= (6 * Math.PI / 8)) {
                                            inFrontDirection = A4_DIRECTION_DOWN;
                                        }
                                        else {
                                            inFrontDirection = A4_DIRECTION_LEFT;
                                        }
                                    }
                                    return this.checkSpatialRelationBetweenCoordinates(relation, x1_y1[0] - x2_y2[0], x1_y1[1] - x2_y2[1], inFrontDirection);
                                }
                            }
                        }
                    }
                    else {
                        var loc1 = this.game.getAILocationByID(o1ID);
                        if (loc1 != null) {
                            var loc2 = this.game.getAILocation(o2);
                            if (loc2 != null) {
                                if (loc2 == loc1)
                                    return false;
                                if (this.game.location_in[this.game.locations.indexOf(loc1)][this.game.locations.indexOf(loc2)] ||
                                    this.game.location_in[this.game.locations.indexOf(loc2)][this.game.locations.indexOf(loc1)])
                                    return false;
                                var x1_y1 = loc1.centerCoordinatesInMap(o2.map);
                                if (x1_y1 == null)
                                    return;
                                var x2 = Math.floor(o2.x + o2.getPixelWidth() / 2);
                                var y2 = Math.floor(o2.y + o2.getPixelHeight() / 2);
                                // find the reference direction:
                                if (o2 instanceof A4Character) {
                                    inFrontDirection = o2.direction;
                                }
                                else {
                                    var or = this.game.findObjectByIDJustObject(referenceObject);
                                    if (or == null || !(or instanceof A4Character)) {
                                        // in this case, we just take the reference of the player:
                                        or = this.game.currentPlayer;
                                    }
                                    if (or.map == o2.map) {
                                        var o_dx = Math.floor(or.x + or.getPixelWidth() / 2) - x2;
                                        var o_dy = Math.floor(or.y + or.getPixelHeight() / 2) - y2;
                                        var angle = Math.atan2(o_dy, o_dx);
                                        if (angle > -(6 * Math.PI / 8) && angle <= -(2 * Math.PI / 8)) {
                                            inFrontDirection = A4_DIRECTION_UP;
                                        }
                                        else if (angle > -(2 * Math.PI / 8) && angle <= (2 * Math.PI / 8)) {
                                            inFrontDirection = A4_DIRECTION_RIGHT;
                                        }
                                        else if (angle > (2 * Math.PI / 8) && angle <= (6 * Math.PI / 8)) {
                                            inFrontDirection = A4_DIRECTION_DOWN;
                                        }
                                        else {
                                            inFrontDirection = A4_DIRECTION_LEFT;
                                        }
                                    }
                                }
                                return this.checkSpatialRelationBetweenCoordinates(relation, x1_y1[0] - x2, x1_y1[1] - y2, inFrontDirection);
                            }
                        }
                    }
                }
                else {
                    var loc2 = this.game.getAILocationByID(o2ID);
                    if (loc2 != null) {
                        var loc1 = this.game.getAILocation(o1);
                        if (loc1 != null) {
                            if (loc2 == loc1)
                                return false;
                            if (this.game.location_in[this.game.locations.indexOf(loc1)][this.game.locations.indexOf(loc2)] ||
                                this.game.location_in[this.game.locations.indexOf(loc2)][this.game.locations.indexOf(loc1)])
                                return false;
                            var x2_y2 = loc2.centerCoordinatesInMap(o1.map);
                            if (x2_y2 == null)
                                return;
                            var x1 = Math.floor(o1.x + o1.getPixelWidth() / 2);
                            var y1 = Math.floor(o1.y + o1.getPixelHeight() / 2);
                            // find the reference direction:
                            var or = this.game.findObjectByIDJustObject(referenceObject);
                            if (or == null || !(or instanceof A4Character)) {
                                // in this case, we just take the reference of the player:
                                or = this.game.currentPlayer;
                            }
                            if (or.map == o1.map) {
                                var o_dx = Math.floor(or.x + or.getPixelWidth() / 2) - x2_y2[0];
                                var o_dy = Math.floor(or.y + or.getPixelHeight() / 2) - x2_y2[0];
                                var angle = Math.atan2(o_dy, o_dx);
                                if (angle > -(6 * Math.PI / 8) && angle <= -(2 * Math.PI / 8)) {
                                    inFrontDirection = A4_DIRECTION_UP;
                                }
                                else if (angle > -(2 * Math.PI / 8) && angle <= (2 * Math.PI / 8)) {
                                    inFrontDirection = A4_DIRECTION_RIGHT;
                                }
                                else if (angle > (2 * Math.PI / 8) && angle <= (6 * Math.PI / 8)) {
                                    inFrontDirection = A4_DIRECTION_DOWN;
                                }
                                else {
                                    inFrontDirection = A4_DIRECTION_LEFT;
                                }
                            }
                            return this.checkSpatialRelationBetweenCoordinates(relation, x1 - x2_y2[0], y1 - x2_y2[1], inFrontDirection);
                        }
                    }
                }
            }
        }
        else if (relation.name == "space.next-to") {
            if (o1ID == o2ID)
                return false;
            var o1 = this.game.findObjectByIDJustObject(o1ID);
            var o2 = this.game.findObjectByIDJustObject(o2ID);
            if (o1 != null && o2 != null) {
                var dx2 = 0;
                var dy2 = 0;
                if (o1.x == null || o2.x == null)
                    return null;
                if (o1.x + o1.getPixelWidth() < o2.x) {
                    dx2 = o2.x - (o1.x + o1.getPixelWidth());
                }
                else if (o2.x + o2.getPixelWidth() < o1.x) {
                    dx2 = o1.x - (o2.x + o2.getPixelWidth());
                }
                if (o1.y + o1.getPixelHeight() < o2.y) {
                    dy2 = o2.y - (o1.y + o1.getPixelHeight());
                }
                else if (o2.y + o2.getPixelHeight() < o1.y) {
                    dy2 = o1.y - (o2.y + o2.getPixelHeight());
                }
                if (dx2 == 0 && dy2 == 0) {
                    if (o1.findObjectByID(o2ID) == null &&
                        o2.findObjectByID(o1ID) == null)
                        return true;
                }
                return false;
            }
        }
        return null;
    };
    A4RuleBasedAI.prototype.checkSpatialRelationBetweenCoordinates = function (relation, dx, dy, frontDirection) {
        if (Math.abs(dx) >= 1 || Math.abs(dy) >= 1) {
            var angle = Math.atan2(dy, dx);
            if (relation.name == "space.north.of") {
                return angle > -(7 * Math.PI / 8) && angle <= -(1 * Math.PI / 8);
            }
            else if (relation.name == "space.east.of") {
                return angle > -(3 * Math.PI / 8) && angle <= (3 * Math.PI / 8);
            }
            else if (relation.name == "space.west.of") {
                return angle <= -(5 * Math.PI / 8) || angle > (5 * Math.PI / 8);
            }
            else if (relation.name == "space.south.of") {
                return angle > (1 * Math.PI / 8) && angle <= (7 * Math.PI / 8);
            }
            else if (relation.name == "space.northeast.of") {
                return angle > -(3 * Math.PI / 8) && angle <= -(1 * Math.PI / 8);
            }
            else if (relation.name == "space.northwest.of") {
                return angle > -(7 * Math.PI / 8) && angle <= -(5 * Math.PI / 8);
            }
            else if (relation.name == "space.southeast.of") {
                return angle > (1 * Math.PI / 8) && angle <= (3 * Math.PI / 8);
            }
            else if (relation.name == "space.southwest.of") {
                return angle > (5 * Math.PI / 8) && angle <= (7 * Math.PI / 8);
            }
            else if ((relation.name == "space.in.front.of" && frontDirection == A4_DIRECTION_UP) ||
                (relation.name == "space.behind" && frontDirection == A4_DIRECTION_DOWN)) {
                return angle > -(6 * Math.PI / 8) && angle <= -(2 * Math.PI / 8);
            }
            else if ((relation.name == "space.in.front.of" && frontDirection == A4_DIRECTION_RIGHT) ||
                (relation.name == "space.behind" && frontDirection == A4_DIRECTION_LEFT)) {
                return angle > -(2 * Math.PI / 8) && angle <= (2 * Math.PI / 8);
            }
            else if ((relation.name == "space.in.front.of" && frontDirection == A4_DIRECTION_LEFT) ||
                (relation.name == "space.behind" && frontDirection == A4_DIRECTION_RIGHT)) {
                return angle <= -(6 * Math.PI / 8) || angle > (6 * Math.PI / 8);
            }
            else if ((relation.name == "space.in.front.of" && frontDirection == A4_DIRECTION_DOWN) ||
                (relation.name == "space.behind" && frontDirection == A4_DIRECTION_UP)) {
                return angle > (2 * Math.PI / 8) && angle <= (6 * Math.PI / 8);
            }
        }
        return null;
    };
    // Calculates spatial relations (e.g., "space.west.of") of o1 with respect to o2. 
    // E.g.: if "o1 is to the west of o2", this will return [this.o.getSort("space.west.of")]
    A4RuleBasedAI.prototype.spatialRelations = function (o1ID, o2ID) {
        var relations = _super.prototype.spatialRelations.call(this, o1ID, o2ID);
        var o1 = this.game.findObjectByIDJustObject(o1ID);
        var o2 = this.game.findObjectByIDJustObject(o2ID);
        if (relations == null)
            relations = [];
        if (o1 != null && o2 == null) {
            // try to see if o2ID is a location:
            var loc1 = this.game.getAILocation(o1);
            var loc2 = this.game.getAILocationByID(o2ID); // see if o2 is a location
            if (loc1 != null && loc1 == loc2) {
                relations.push(this.cache_sort_space_at);
            }
        }
        if (o1 == null || o2 == null)
            return null;
        if (o2 instanceof A4Container) {
            if (o2.content.indexOf(o1) != -1)
                relations.push(this.o.getSort("space.inside.of"));
        }
        else if (o1 instanceof A4Character) {
            if (o2.inventory.indexOf(o1) != -1)
                relations.push(this.o.getSort("verb.have"));
        }
        if (o1.map != o2.map)
            return relations;
        var x1 = Math.floor(o1.x + o1.getPixelWidth() / 2);
        var y1 = Math.floor(o1.y + o1.getPixelHeight() / 2);
        var x2 = Math.floor(o2.x + o2.getPixelWidth() / 2);
        var y2 = Math.floor(o2.y + o2.getPixelHeight() / 2);
        var dx = x1 - x2;
        var dy = y1 - y2;
        var distance = Math.sqrt(dx * dx + dy * dy);
        if (distance < SPACE_NEAR_FAR_THRESHOLD)
            relations.push(this.o.getSort("space.near"));
        if (distance >= SPACE_NEAR_FAR_THRESHOLD)
            relations.push(this.o.getSort("space.far"));
        if (o2.x >= o1.x && o2.x + o2.getPixelWidth() <= o1.x + o1.getPixelWidth())
            dx = 0;
        if (o2.y >= o1.y && o2.y + o2.getPixelHeight() <= o1.y + o1.getPixelHeight())
            dy = 0;
        //		console.log("dx: " + dx + ", dy: " + dy);
        if (Math.abs(dx) >= 1 || Math.abs(dy) >= 1) {
            var angle = Math.atan2(dy, dx);
            //			console.log("angle: " + angle + ", dx: " + dx + ", dy: " + dy);
            if (angle > -(7 * Math.PI / 8) && angle <= -(5 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.northwest.of"));
            }
            else if (angle > -(5 * Math.PI / 8) && angle <= -(3 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.north.of"));
                if (o2.direction == A4_DIRECTION_UP)
                    relations.push(this.o.getSort("space.in.front.of"));
                if (o2.direction == A4_DIRECTION_DOWN)
                    relations.push(this.o.getSort("space.behind"));
            }
            else if (angle > -(3 * Math.PI / 8) && angle <= -(1 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.northeast.of"));
            }
            else if (angle > -(1 * Math.PI / 8) && angle <= (1 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.east.of"));
                if (o2.direction == A4_DIRECTION_RIGHT)
                    relations.push(this.o.getSort("space.in.front.of"));
                if (o2.direction == A4_DIRECTION_LEFT)
                    relations.push(this.o.getSort("space.behind"));
            }
            else if (angle > (1 * Math.PI / 8) && angle <= (3 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.southeast.of"));
            }
            else if (angle > (3 * Math.PI / 8) && angle <= (5 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.south.of"));
                if (o2.direction == A4_DIRECTION_DOWN)
                    relations.push(this.o.getSort("space.in.front.of"));
                if (o2.direction == A4_DIRECTION_UP)
                    relations.push(this.o.getSort("space.behind"));
            }
            else if (angle > (5 * Math.PI / 8) && angle <= (7 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.southwest.of"));
            }
            else {
                relations.push(this.o.getSort("space.west.of"));
                if (o2.direction == A4_DIRECTION_LEFT)
                    relations.push(this.o.getSort("space.in.front.of"));
                if (o2.direction == A4_DIRECTION_RIGHT)
                    relations.push(this.o.getSort("space.behind"));
            }
        }
        {
            var dx2 = 0;
            var dy2 = 0;
            if (o1.x + o1.getPixelWidth() < o2.x) {
                dx2 = o2.x - (o1.x + o1.getPixelWidth());
            }
            else if (o2.x + o2.getPixelWidth() < o1.x) {
                dx2 = o1.x - (o2.x + o2.getPixelWidth());
            }
            if (o1.y + o1.getPixelHeight() < o2.y) {
                dy2 = o2.y - (o1.y + o1.getPixelHeight());
            }
            else if (o2.y + o2.getPixelHeight() < o1.y) {
                dy2 = o1.y - (o2.y + o2.getPixelHeight());
            }
            if (dx2 == 0 && dy2 == 0) {
                if (o1.findObjectByID(o2ID) == null &&
                    o2.findObjectByID(o1ID) == null)
                    relations.push(this.o.getSort("space.next-to"));
            }
        }
        return relations;
    };
    A4RuleBasedAI.prototype.spatialRelationsFromLocation = function (l1, o2) {
        var relations = [];
        var tmp = l1.centerCoordinatesInMap(o2.map);
        if (tmp == null)
            return relations;
        var x1 = tmp[0];
        var y1 = tmp[1];
        var x2 = Math.floor(o2.x + o2.getPixelWidth() / 2);
        var y2 = Math.floor(o2.y + o2.getPixelHeight() / 2);
        var dx = x1 - x2;
        var dy = y1 - y2;
        if (Math.abs(dx) >= 1 || Math.abs(dy) >= 1) {
            var angle = Math.atan2(dy, dx);
            //		console.log("angle: " + angle + ", dx: " + dx + ", dy: " + dy);
            if (angle > -(7 * Math.PI / 8) && angle <= -(5 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.northwest.of"));
            }
            else if (angle > -(5 * Math.PI / 8) && angle <= -(3 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.north.of"));
            }
            else if (angle > -(3 * Math.PI / 8) && angle <= -(1 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.northeast.of"));
            }
            else if (angle > -(1 * Math.PI / 8) && angle <= (1 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.east.of"));
            }
            else if (angle > (1 * Math.PI / 8) && angle <= (3 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.southeast.of"));
            }
            else if (angle > (3 * Math.PI / 8) && angle <= (5 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.south.of"));
            }
            else if (angle > (5 * Math.PI / 8) && angle <= (7 * Math.PI / 8)) {
                relations.push(this.o.getSort("space.southwest.of"));
            }
            else {
                relations.push(this.o.getSort("space.west.of"));
            }
        }
        return relations;
    };
    A4RuleBasedAI.prototype.spatialRelationsLocationsAsNouns = function (l1, l2) {
        var relations = [];
        if (this.game.location_in[this.game.locations.indexOf(l1)][this.game.locations.indexOf(l2)])
            return relations;
        if (this.game.location_in[this.game.locations.indexOf(l2)][this.game.locations.indexOf(l1)])
            return relations;
        var map = null;
        for (var _i = 0, _a = l1.maps; _i < _a.length; _i++) {
            var map2 = _a[_i];
            if (l2.maps.indexOf(map2) != -1) {
                map = map2;
                break;
            }
        }
        if (map == null)
            return relations;
        var tmp = l1.centerCoordinatesInMap(map);
        if (tmp == null)
            return relations;
        var x1 = tmp[0];
        var y1 = tmp[1];
        var tmp2 = l2.centerCoordinatesInMap(map);
        if (tmp2 == null)
            return relations;
        var x2 = tmp2[0];
        var y2 = tmp2[1];
        var dx = x1 - x2;
        var dy = y1 - y2;
        if (Math.abs(dx) >= 1 || Math.abs(dy) >= 1) {
            var angle = Math.atan2(dy, dx);
            //		console.log("angle: " + angle + ", dx: " + dx + ", dy: " + dy);
            if (angle > -(7 * Math.PI / 8) && angle <= -(5 * Math.PI / 8)) {
                relations.push(this.o.getSort("northwest"));
            }
            else if (angle > -(5 * Math.PI / 8) && angle <= -(3 * Math.PI / 8)) {
                relations.push(this.o.getSort("north"));
            }
            else if (angle > -(3 * Math.PI / 8) && angle <= -(1 * Math.PI / 8)) {
                relations.push(this.o.getSort("northeast"));
            }
            else if (angle > -(1 * Math.PI / 8) && angle <= (1 * Math.PI / 8)) {
                relations.push(this.o.getSort("east"));
            }
            else if (angle > (1 * Math.PI / 8) && angle <= (3 * Math.PI / 8)) {
                relations.push(this.o.getSort("southeast"));
            }
            else if (angle > (3 * Math.PI / 8) && angle <= (5 * Math.PI / 8)) {
                relations.push(this.o.getSort("south"));
            }
            else if (angle > (5 * Math.PI / 8) && angle <= (7 * Math.PI / 8)) {
                relations.push(this.o.getSort("southwest"));
            }
            else {
                relations.push(this.o.getSort("west"));
            }
        }
        return relations;
    };
    A4RuleBasedAI.prototype.getDirectionTowardObject = function (source, target) {
        var dx = target.x - source.x;
        var dy = target.y - source.y;
        if (Math.abs(dx) >= 1 || Math.abs(dy) >= 1) {
            var angle = Math.atan2(dy, dx);
            if (angle > -(7 * Math.PI / 8) && angle <= -(5 * Math.PI / 8)) {
                return this.o.getSort("northwest");
            }
            else if (angle > -(5 * Math.PI / 8) && angle <= -(3 * Math.PI / 8)) {
                return this.o.getSort("north");
            }
            else if (angle > -(3 * Math.PI / 8) && angle <= -(1 * Math.PI / 8)) {
                return this.o.getSort("northeast");
            }
            else if (angle > -(1 * Math.PI / 8) && angle <= (1 * Math.PI / 8)) {
                return this.o.getSort("east");
            }
            else if (angle > (1 * Math.PI / 8) && angle <= (3 * Math.PI / 8)) {
                return this.o.getSort("southeast");
            }
            else if (angle > (3 * Math.PI / 8) && angle <= (5 * Math.PI / 8)) {
                return this.o.getSort("south");
            }
            else if (angle > (5 * Math.PI / 8) && angle <= (7 * Math.PI / 8)) {
                return this.o.getSort("southwest");
            }
            else {
                return this.o.getSort("west");
            }
        }
        return null;
    };
    A4RuleBasedAI.prototype.processSuperlatives = function (results, superlative) {
        if (superlative.terms.length == 1 &&
            superlative.terms[0].functor.name == "space.nearest-to" &&
            superlative.terms[0].attributes.length == 2) {
            var best = null;
            var best_distance = null;
            for (var _i = 0, results_1 = results; _i < results_1.length; _i++) {
                var result = results_1[_i];
                var tmp = superlative.terms[0].applyBindings(result.bindings);
                if ((tmp.attributes[0] instanceof ConstantTermAttribute) &&
                    (tmp.attributes[1] instanceof ConstantTermAttribute)) {
                    var d_2 = this.distanceBetweenIds(tmp.attributes[0].value, tmp.attributes[1].value);
                    if (!superlative.sign[0] && d_2 != null)
                        d_2 = -d_2;
                    console.log("processSuperlatives: d = " + d_2 + ", from: " + tmp);
                    if (best_distance == null) {
                        best = result;
                        best_distance = d_2;
                    }
                    else if (d_2 != null &&
                        best_distance > d_2) {
                        best = result;
                        best_distance = d_2;
                    }
                }
                else {
                    if (best == null) {
                        best = result;
                    }
                }
            }
            console.log("processSuperlatives: best = " + best);
            return [best];
        }
        else if (superlative.terms.length == 1 &&
            superlative.terms[0].functor.name == "space.farthest-from" &&
            superlative.terms[0].attributes.length == 2) {
            var best = null;
            var best_distance = null;
            for (var _a = 0, results_2 = results; _a < results_2.length; _a++) {
                var result = results_2[_a];
                var tmp = superlative.terms[0].applyBindings(result.bindings);
                if ((tmp.attributes[0] instanceof ConstantTermAttribute) &&
                    (tmp.attributes[1] instanceof ConstantTermAttribute)) {
                    var d_3 = this.distanceBetweenIds(tmp.attributes[0].value, tmp.attributes[1].value);
                    if (!superlative.sign[0] && d_3 != null)
                        d_3 = -d_3;
                    console.log("processSuperlatives: d = " + d_3 + ", from: " + tmp);
                    if (best_distance == null) {
                        best = result;
                        best_distance = d_3;
                    }
                    else if (d_3 != null &&
                        best_distance < d_3) {
                        best = result;
                        best_distance = d_3;
                    }
                }
                else {
                    if (best == null) {
                        best = result;
                    }
                }
            }
            console.log("processSuperlatives: best = " + best);
            return [best];
        }
        return results;
    };
    /* This is used by the perception routines, to assign properties to the objects, that
       can then be used to reason about them: */
    A4RuleBasedAI.prototype.getBaseObjectProperties = function (obj) {
        var properties = [];
        for (var _i = 0, _a = obj.perceptionProperties; _i < _a.length; _i++) {
            var p = _a[_i];
            var s = this.o.getSort(p);
            if (s.is_a(this.cache_sort_property_with_value)) {
                if (s.parents.length == 1 &&
                    s.parents[0].is_a(this.cache_sort_property_with_value)) {
                    properties.push(new Term(s.parents[0], [new ConstantTermAttribute(obj.ID, this.cache_sort_id),
                        new ConstantTermAttribute(s.name, s)]));
                }
                else {
                    properties.push(new Term(s, [new ConstantTermAttribute(obj.ID, this.cache_sort_id),
                        new ConstantTermAttribute(s.name, s)]));
                }
            }
            else {
                properties.push(new Term(s, [new ConstantTermAttribute(obj.ID, this.cache_sort_id)]));
            }
        }
        if (obj instanceof A4Container) {
            if (obj.content.length == 0) {
                properties.push(new Term(this.o.getSort("empty"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id)]));
            }
            else {
                properties.push(new Term(this.o.getSort("full"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id)]));
            }
            if (obj instanceof A4ObstacleContainer) {
                if (obj.closed) {
                    properties.push(new Term(this.o.getSort("property.closed"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id)]));
                }
                else {
                    properties.push(new Term(this.o.getSort("property.opened"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id)]));
                }
            }
        }
        if (obj instanceof A4Door) {
            if (obj.closed) {
                properties.push(new Term(this.o.getSort("property.closed"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id)]));
            }
            else {
                properties.push(new Term(this.o.getSort("property.opened"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id)]));
            }
        }
        // object types that can potentially have a direction:
        if ((obj instanceof A4Character) ||
            (obj instanceof A4Vehicle) ||
            (obj instanceof A4Door) ||
            (obj instanceof A4Obstacle) ||
            (obj instanceof A4ObstacleContainer)) {
            var direction = obj.direction;
            if (direction == A4_DIRECTION_LEFT) {
                properties.push(new Term(this.o.getSort("facing-direction"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id),
                    new ConstantTermAttribute(this.cache_sort_west.name, this.cache_sort_west)]));
            }
            else if (direction == A4_DIRECTION_UP) {
                properties.push(new Term(this.o.getSort("facing-direction"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id),
                    new ConstantTermAttribute(this.cache_sort_north.name, this.cache_sort_north)]));
            }
            else if (direction == A4_DIRECTION_RIGHT) {
                properties.push(new Term(this.o.getSort("facing-direction"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id),
                    new ConstantTermAttribute(this.cache_sort_east.name, this.cache_sort_east)]));
            }
            else if (direction == A4_DIRECTION_DOWN) {
                properties.push(new Term(this.o.getSort("facing-direction"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id),
                    new ConstantTermAttribute(this.cache_sort_south.name, this.cache_sort_south)]));
            }
        }
        /*
        // the name is not directly visible, so, they should not know!
        if (obj instanceof A4Character) {
            properties.push(new Term(this.o.getSort("name"), [new ConstantTermAttribute(obj.ID, this.cache_sort_id),
                                                              new ConstantTermAttribute(obj.name.toLowerCase(), this.cache_sort_symbol)]));
        }
        */
        return properties;
    };
    A4RuleBasedAI.prototype.replaceSpatialAdverbsInReferenceToAnotherSpeaker = function (term, originalSpeaker) {
        if (term.functor.is_a(this.o.getSort("verb.come"))) {
            if (term.attributes.length == 1) {
                term.functor = this.o.getSort("verb.go-to");
                // add destination:
                var speakerObject = this.game.findObjectByIDJustObject(originalSpeaker);
                if (speakerObject != null) {
                    var location_2 = this.game.getAILocation(speakerObject);
                    if (location_2 != null) {
                        term.attributes.push(new ConstantTermAttribute(location_2.id, this.cache_sort_id));
                    }
                }
            }
        }
        for (var i = 0; i < term.attributes.length; i++) {
            term.attributes[i] = this.replaceSpatialAdverbsInReferenceToAnotherSpeaker_internal(term.attributes[i], originalSpeaker);
        }
    };
    A4RuleBasedAI.prototype.replaceSpatialAdverbsInReferenceToAnotherSpeaker_internal = function (ta, originalSpeaker) {
        if (ta instanceof TermTermAttribute) {
            this.replaceSpatialAdverbsInReferenceToAnotherSpeaker(ta.term, originalSpeaker);
        }
        else if (ta instanceof VariableTermAttribute) {
            if (ta.sort.is_a(this.o.getSort("space.here"))) {
                var speakerObject = this.game.findObjectByIDJustObject(originalSpeaker);
                if (speakerObject != null) {
                    var location_3 = this.game.getAILocation(speakerObject);
                    if (location_3 != null) {
                        return new ConstantTermAttribute(location_3.id, this.cache_sort_id);
                    }
                }
            }
        }
        return ta;
    };
    A4RuleBasedAI.prototype.reactToPerformative = function (perf2, speaker, context) {
        if (this.respondToPerformatives)
            return _super.prototype.reactToPerformative.call(this, perf2, speaker, context);
        return [];
    };
    A4RuleBasedAI.prototype.reactToMoreResultsPerformative = function (perf, speaker, context) {
        app.achievement_nlp_asked_for_more = true;
        app.trigger_achievement_complete_alert();
        _super.prototype.reactToMoreResultsPerformative.call(this, perf, speaker, context);
    };
    A4RuleBasedAI.prototype.reactToParseError = function (speakerID, sentence) {
        if (!this.respondToPerformatives)
            return;
        var context = this.contextForSpeakerWithoutCreatingANewOne(speakerID);
        if (context != null) {
            if (this.talkingToUs(context, speakerID, null)) {
                app.achievement_nlp_parse_error = true;
                app.trigger_achievement_complete_alert();
                _super.prototype.reactToParseError.call(this, speakerID, sentence);
            }
        }
    };
    A4RuleBasedAI.prototype.canSatisfyActionRequest = function (ir) {
        var actionRequest = ir.action;
        if (this.game.gameScript.actionRequestHandleByScript(actionRequest))
            return ACTION_REQUEST_WILL_BE_HANDLED_EXTERNALLY;
        return _super.prototype.canSatisfyActionRequest.call(this, ir);
    };
    A4RuleBasedAI.prototype.handlePerfInform = function (perf2, speaker, context) {
        if (perf2.attributes.length == 2) {
            var predicate = (perf2.attributes[1]).term;
            var pattern = Term.fromString("#not(verb.can('player'[#id], X))", this.o);
            if (pattern.subsumes(predicate, OCCURS_CHECK, new Bindings())) {
                // the player probably means "please help me do X", so, convert into an action request for help
                var X = (predicate.attributes[0]).term.attributes[1];
                var newPerf = new Term(this.o.getSort("perf.request.action"), [perf2.attributes[0],
                    new TermTermAttribute(new Term(this.o.getSort("verb.help"), [perf2.attributes[0], speaker, X]))]);
                console.log("A4RuleBasedAI.handlePerfInform perf converted to: " + newPerf);
                this.reactToRequestActionPerformative(newPerf, speaker, context);
                return;
            }
        }
        _super.prototype.handlePerfInform.call(this, perf2, speaker, context);
    };
    A4RuleBasedAI.prototype.handlePerfQPredicate = function (perf2, speaker, context) {
        if (this.game.gameScript.perfQPredicateHandleByScript(perf2))
            return;
        _super.prototype.handlePerfQPredicate.call(this, perf2, speaker, context);
    };
    A4RuleBasedAI.prototype.allowPlayerIntoEveryWhere = function () {
        for (var _i = 0, _a = this.locationsWherePlayerIsNotPermitted; _i < _a.length; _i++) {
            var location_4 = _a[_i];
            // remove the long term permission term and add a negated one:
            this.longTermMemory.removeSentence(Sentence.fromString("~permitted-in('player'[#id], '" + location_4 + "'[#id])", this.o));
            this.longTermMemory.addSentence(Sentence.fromString("permitted-in('player'[#id], '" + location_4 + "'[#id])", this.o), BACKGROUND_PROVENANCE, 1, this.timeStamp);
        }
        this.locationsWherePlayerIsNotPermitted = [];
        this.doorsPlayerIsNotPermittedToOpen = [];
    };
    A4RuleBasedAI.prototype.allowPlayerInto = function (location, door) {
        var idx = this.locationsWherePlayerIsNotPermitted.indexOf(location);
        if (idx != -1) {
            this.locationsWherePlayerIsNotPermitted.splice(idx, 1);
            // remove the long term permission term and add a negated one:
            this.longTermMemory.removeSentence(Sentence.fromString("~permitted-in('player'[#id], '" + location + "'[#id])", this.o));
            this.longTermMemory.addSentence(Sentence.fromString("permitted-in('player'[#id], '" + location + "'[#id])", this.o), BACKGROUND_PROVENANCE, 1, this.timeStamp);
        }
        if (door != null) {
            idx = this.doorsPlayerIsNotPermittedToOpen.indexOf(door);
            if (idx != -1)
                this.doorsPlayerIsNotPermittedToOpen.splice(idx, 1);
        }
    };
    A4RuleBasedAI.prototype.doNotAllowPlayerInto = function (location, door) {
        var idx = this.locationsWherePlayerIsNotPermitted.indexOf(location);
        if (idx == -1) {
            this.locationsWherePlayerIsNotPermitted.push(location);
            // remove the long term permission term and add a negated one:
            this.longTermMemory.removeSentence(Sentence.fromString("permitted-in('player'[#id], '" + location + "'[#id])", this.o));
            this.longTermMemory.addSentence(Sentence.fromString("~permitted-in('player'[#id], '" + location + "'[#id])", this.o), BACKGROUND_PROVENANCE, 1, this.timeStamp);
        }
        if (door != null) {
            idx = this.doorsPlayerIsNotPermittedToOpen.indexOf(door);
            if (idx == -1)
                this.doorsPlayerIsNotPermittedToOpen.push(door);
        }
    };
    A4RuleBasedAI.prototype.pathBetweenLocations = function (loc1, loc2) {
        var open = [];
        var open_parents = [];
        var closed = [];
        var closed_parents = [];
        var loc1_idx = this.game.locations.indexOf(loc1);
        console.log("pathBetweenLocations: loc1_idx = " + loc1_idx);
        if (loc1_idx == -1)
            return null;
        var loc2_idx = this.game.locations.indexOf(loc2);
        console.log("pathBetweenLocations: loc2_idx = " + loc2_idx);
        if (loc2_idx == -1)
            return null;
        open.push(loc1_idx);
        open_parents.push(-1);
        while (open.length > 0) {
            var current = open[0];
            var parent_1 = open_parents[0];
            open.splice(0, 1);
            open_parents.splice(0, 1);
            closed.push(current);
            closed_parents.push(parent_1);
            //			console.log("open: " + open.length + ", closed: " + closed.length + ", current = " + this.game.locations[current].id);
            if (current == loc2_idx || this.game.location_in[current][loc2_idx]) {
                // we found the goal!
                var path = [];
                while (current != -1) {
                    path.unshift(this.game.locations[current]);
                    if (parent_1 == -1)
                        break;
                    current = closed[parent_1];
                    parent_1 = closed_parents[parent_1];
                }
                return path;
            }
            else {
                for (var next = 0; next < this.game.locations.length; next++) {
                    if (this.game.location_connects[current][next]) {
                        if (closed.indexOf(next) == -1 &&
                            open.indexOf(next) == -1) {
                            open.push(next);
                            open_parents.push(closed.length - 1);
                        }
                    }
                }
            }
        }
        return null;
    };
    A4RuleBasedAI.prototype.pathToGetOutOf = function (loc1, loc2, outside_flag) {
        var open = [];
        var open_parents = [];
        var closed = [];
        var closed_parents = [];
        var loc1_idx = this.game.locations.indexOf(loc1);
        console.log("pathToGetOutOf: loc1_idx = " + loc1_idx);
        if (loc1_idx == -1)
            return null;
        var loc2_idx = this.game.locations.indexOf(loc2);
        console.log("pathToGetOutOf: loc2_idx = " + loc2_idx);
        if (loc2_idx == -1)
            return null;
        open.push(loc1_idx);
        open_parents.push(-1);
        while (open.length > 0) {
            var current = open[0];
            var parent_2 = open_parents[0];
            open.splice(0, 1);
            open_parents.splice(0, 1);
            closed.push(current);
            closed_parents.push(parent_2);
            //			console.log("open: " + open.length + ", closed: " + closed.length + ", current = " + this.game.locations[current].id);
            if (current != loc2_idx &&
                ((outside_flag && this.game.location_in[loc2_idx][current]) ||
                    (!outside_flag && !this.game.location_in[current][loc2_idx]))) {
                // we found the goal!
                var path = [];
                while (current != -1) {
                    path.unshift(this.game.locations[current]);
                    if (parent_2 == -1)
                        break;
                    current = closed[parent_2];
                    parent_2 = closed_parents[parent_2];
                }
                return path;
            }
            else {
                for (var next = 0; next < this.game.locations.length; next++) {
                    if (this.game.location_connects[current][next]) {
                        if (closed.indexOf(next) == -1 &&
                            open.indexOf(next) == -1) {
                            open.push(next);
                            open_parents.push(closed.length - 1);
                        }
                    }
                }
            }
        }
        return null;
    };
    A4RuleBasedAI.prototype.locationOutsideOf = function (loc) {
        var loc_idx = this.game.locations.indexOf(loc);
        for (var loc2_idx = 0; loc2_idx < this.game.locations.length; loc2_idx++) {
            if (this.game.location_connects[loc_idx][loc2_idx] &&
                !this.game.location_in[loc2_idx][loc_idx]) {
                return this.game.locations[loc2_idx];
            }
        }
        return null;
    };
    A4RuleBasedAI.prototype.executeIntention = function (ir) {
        var intention = ir.action;
        for (var _i = 0, _a = this.intentionHandlers; _i < _a.length; _i++) {
            var ih = _a[_i];
            if (ih.canHandle(intention, this)) {
                if (ih instanceof AnswerPredicate_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[0] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerWhere_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[1] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerWhoIs_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[2] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerWhatIs_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[3] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerQuery_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[4] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerHowMany_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[5] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerWhen_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[6] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerWhy_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[7] = true;
                    app.trigger_achievement_complete_alert();
                    var intention_1 = ir.action;
                    if (intention_1.attributes.length == 2) {
                        if (intention_1.attributes[1] instanceof ConstantTermAttribute) {
                            var targetID = intention_1.attributes[1].value;
                            var context = this.contextForSpeakerWithoutCreatingANewOne(targetID);
                            if (context != null) {
                                var lastPerf = context.lastPerformativeBy(this.selfID);
                                if (lastPerf.cause != null) {
                                    if (lastPerf.cause.causesComeFromInference) {
                                        app.achievement_nlp_resolution_explanation = true;
                                        app.trigger_achievement_complete_alert();
                                    }
                                }
                            }
                        }
                    }
                }
                else if (ih instanceof A4AnswerHow_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[8] = true;
                    app.trigger_achievement_complete_alert();
                }
                else if (ih instanceof AnswerDefine_IntentionAction) {
                    app.achievement_nlp_all_types_of_questions[9] = true;
                    app.trigger_achievement_complete_alert();
                    var intention_2 = ir.action;
                    if (intention_2.attributes.length == 2 &&
                        (intention_2.attributes[0] instanceof ConstantTermAttribute) &&
                        ((intention_2.attributes[1] instanceof VariableTermAttribute) ||
                            (intention_2.attributes[1] instanceof ConstantTermAttribute)) &&
                        (intention_2.attributes[0]).value == this.selfID) {
                        if (intention_2.attributes[1].sort.name == "three-laws-of-robotics") {
                            app.achievement_secret_3_laws_of_robotics = true;
                            app.trigger_achievement_complete_alert();
                        }
                    }
                }
                return ih.execute(ir, this);
            }
        }
        return false;
    };
    A4RuleBasedAI.prototype.restoreFromXML = function (xml) {
        _super.prototype.restoreFromXML.call(this, xml);
        var xml_tmp = getFirstElementChildByTag(xml, "respondToPerformatives");
        if (xml_tmp != null) {
            this.respondToPerformatives = xml_tmp.getAttribute("value") == "true";
        }
        xml_tmp = getFirstElementChildByTag(xml, "locationsWherePlayerIsNotPermitted");
        if (xml_tmp != null) {
            this.locationsWherePlayerIsNotPermitted = xml_tmp.getAttribute("value").split(",");
        }
        xml_tmp = getFirstElementChildByTag(xml, "doorsPlayerIsNotPermittedToOpen");
        if (xml_tmp != null) {
            this.doorsPlayerIsNotPermittedToOpen = xml_tmp.getAttribute("value").split(",");
        }
        this.visionActive = true;
        xml_tmp = getFirstElementChildByTag(xml, "visionActive");
        if (xml_tmp != null && xml_tmp.getAttribute("value") == "false")
            this.visionActive = false;
    };
    A4RuleBasedAI.prototype.savePropertiesToXML = function () {
        var str = _super.prototype.savePropertiesToXML.call(this);
        str += "<respondToPerformatives value=\"" + this.respondToPerformatives + "\"/>\n";
        str += "<locationsWherePlayerIsNotPermitted value=\"" + this.locationsWherePlayerIsNotPermitted + "\"/>\n";
        str += "<doorsPlayerIsNotPermittedToOpen value=\"" + this.doorsPlayerIsNotPermittedToOpen + "\"/>";
        str += "<visionActive value=\"" + this.visionActive + "\"/>\n";
        return str;
    };
    A4RuleBasedAI.prototype.generateAILocationDOTGraph = function () {
        var str = "digraph locations {\n";
        str += "graph[rankdir=LR];\n";
        for (var i = 0; i < this.game.locations.length; i++) {
            if (this.game.locations[i].name == null) {
                str += "s" + i + "[shape=box label=\"" + this.game.locations[i].sort.name + "\"];\n";
            }
            else {
                str += "s" + i + "[shape=box label=\"" + this.game.locations[i].name + "\"];\n";
            }
        }
        for (var i = 0; i < this.game.locations.length; i++) {
            for (var j = 0; j < this.game.locations.length; j++) {
                if (this.game.location_connects[i][j]) {
                    str += "s" + i + " -> s" + j + " [label=\"connects\"];\n";
                }
            }
        }
        str += "}\n";
        return str;
    };
    return A4RuleBasedAI;
}(RuleBasedAI));
