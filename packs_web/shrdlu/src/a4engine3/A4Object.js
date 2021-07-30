var A4Object = /** @class */ (function () {
    function A4Object(name, sort) {
        // modifiers to the shape:
        this.sprite_offs_x = 0;
        this.sprite_offs_y = 0;
        this.pixel_width = 0; // if these are != 0 they are used, otherwise, widht/height are calculated from the current Animation
        this.pixel_height = 0;
        this.pixel_tallness = 0;
        //layer:number;
        this.map = null;
        this.animations = null;
        this.currentAnimation = A4_ANIMATION_IDLE;
        this.previousSeeThrough = null;
        this.cycle = 0;
        this.deadRequest = false; // this is set to true, when the script "die" is executed
        // pixel width/height cache:
        this.pixel_width_cache_cycle = -1;
        this.pixel_width_cache = 0;
        this.pixel_height_cache = 0;
        // attributes:
        this.gold = 0;
        this.takeable = false;
        this.usable = false;
        this.interacteable = false;
        this.burrowed = false;
        this.canSwim = false;
        this.canWalk = true;
        this.drawDarkIfNoLight = true; // If this is set to false, turning the light off will not affect this object
        // this is used, for example, for objects that emit their own light
        this.direction = A4_DIRECTION_NONE;
        // scripts:
        this.eventScripts = new Array(A4_NEVENTS);
        // script excution queues (these contain scripts that are pending execution, will be executed in the next "cycle"):
        this.scriptQueues = [];
        // story state:
        this.storyState = {};
        this.lastTimeStoryStateChanged = 0;
        // agendas:
        this.agendas = [];
        // list of properties that the AI will perceive this object having:
        this.perceptionProperties = [];
        this.ID = "" + A4Object.s_nextID++;
        this.name = name;
        this.sort = sort;
        this.animations = new Array(A4_N_ANIMATIONS);
        for (var i = 0; i < A4_N_ANIMATIONS; i++)
            this.animations[i] = null;
    }
    A4Object.prototype.loadObjectAdditionalContent = function (xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object) {
        // add animations:
        var animations_xml = getElementChildrenByTag(xml, "animation");
        for (var i = 0; i < animations_xml.length; i++) {
            var animation_xml = animations_xml[i];
            var a = A4Animation.fromXML(animation_xml, game);
            for (var idx = 0; idx < A4_N_ANIMATIONS; idx++) {
                if (animationNames[idx] == animation_xml.getAttribute("name")) {
                    this.setAnimation(idx, a);
                    a = null;
                    break;
                }
            }
            if (a != null)
                console.error("A4ObjectFactory: not supported animation " + animation_xml.getAttribute("name"));
        }
        // set attributes (we allow them to be either "attribute" tags, or the "property" tags set by TILED):
        var canWalkSet = false;
        var canSwimSet = false;
        var attributes_xml = getElementChildrenByTag(xml, "attribute");
        for (var i = 0; i < attributes_xml.length; i++) {
            var attribute_xml = attributes_xml[i];
            var a_name = attribute_xml.getAttribute("name");
            if (a_name == "canSwim") {
                canSwimSet = true;
                this.canSwim = false;
                if (attribute_xml.getAttribute("value") == "true")
                    this.canSwim = true;
            }
            else if (a_name == "canWalk") {
                canWalkSet = true;
                this.canWalk = false;
                if (attribute_xml.getAttribute("value") == "true")
                    this.canWalk = true;
            }
            else if (!this.loadObjectAttribute(attribute_xml)) {
                console.error("Unknown attribute: " + a_name + " for object " + xml.getAttribute("class"));
            }
        }
        var properties_l_xml = getFirstElementChildByTag(xml, "properties");
        if (properties_l_xml != null) {
            var properties_xml = getElementChildrenByTag(properties_l_xml, "property");
            for (var i = 0; i < properties_xml.length; i++) {
                var attribute_xml = properties_xml[i];
                var a_name = attribute_xml.getAttribute("name");
                if (a_name == "canSwim") {
                    canSwimSet = true;
                    this.canSwim = false;
                    if (attribute_xml.getAttribute("value") == "true")
                        this.canSwim = true;
                }
                else if (a_name == "canWalk") {
                    canWalkSet = true;
                    this.canWalk = false;
                    if (attribute_xml.getAttribute("value") == "true")
                        this.canWalk = true;
                }
                else if (!this.loadObjectAttribute(attribute_xml)) {
                    console.error("Unknown attribute: " + a_name + " for object " + xml.getAttribute("class"));
                }
            }
        }
        if (canWalkSet) {
            if (!canSwimSet)
                this.canSwim = !this.canWalk;
        }
        else {
            if (canSwimSet)
                this.canWalk = !this.canSwim;
        }
        // loading scripts:
        {
            // on start:
            var onstarts_xml = getElementChildrenByTag(xml, "onStart");
            for (var i = 0; i < onstarts_xml.length; i++) {
                var onstart_xml = onstarts_xml[i];
                var tmpq = null;
                //                let onstart_xml_l:NodeListOf<Element> = onstart_xml.children;
                var onstart_xml_l = onstart_xml.children;
                for (var j = 0; j < onstart_xml_l.length; j++) {
                    var script_xml = onstart_xml_l[j];
                    var s = A4Script.fromXML(script_xml);
                    if (tmpq == null)
                        tmpq = new A4ScriptExecutionQueue(this, null, null, null);
                    tmpq.scripts.push(s);
                }
                if (tmpq != null)
                    this.scriptQueues.push(tmpq);
            }
            // on end:
            var onends_xml = getElementChildrenByTag(xml, "onEnd");
            for (var i = 0; i < onends_xml.length; i++) {
                var onend_xml = onends_xml[i];
                //                let script_xml_l:NodeListOf<Element> = onend_xml.children;
                var script_xml_l = onend_xml.children;
                for (var j = 0; j < script_xml_l.length; j++) {
                    var script_xml = script_xml_l[j];
                    var s = A4Script.fromXML(script_xml);
                    if (this.eventScripts[A4_EVENT_END] == null)
                        this.eventScripts[A4_EVENT_END] = [];
                    this.eventScripts[A4_EVENT_END].push(new A4EventRule(A4_EVENT_END, s, false, 0, 0));
                }
            }
            // event rules:
            var eventrules_xml = getElementChildrenByTag(xml, "eventRule");
            for (var i = 0; i < eventrules_xml.length; i++) {
                var rule_xml = eventrules_xml[i];
                var r = A4EventRule.fromXML(rule_xml);
                if (this.eventScripts[r.event] == null)
                    this.eventScripts[r.event] = [];
                this.eventScripts[r.event].push(r);
            }
        }
        // perception properties:
        this.perceptionProperties = [];
        for (var _i = 0, _a = getElementChildrenByTag(xml, "perceptionProperty"); _i < _a.length; _i++) {
            var prop_xml = _a[_i];
            this.perceptionProperties.push(prop_xml.getAttribute("value"));
        }
        // the coordinates in the xml files are of the top-left of the object (as it's easier to
        // edit it this way in Tiled), so, we need to correct by adding tallness:
        if (xml.getAttribute("x") != null) {
            this.x = Number(xml.getAttribute("x"));
            this.y = Number(xml.getAttribute("y")) + this.pixel_tallness;
        }
        this.pixel_height -= this.pixel_tallness;
    };
    A4Object.prototype.loadObjectAttribute = function (attribute_xml) {
        var name = attribute_xml.getAttribute("name");
        /*
        if (name == "ID") {
            this.ID = attribute_xml.getAttribute("value");
            if (A4Object.s_nextID <= Number(this.ID)) A4Object.s_nextID = Number(this.ID)+1;
            return true;
        } else */
        if (name == "name") {
            this.name = attribute_xml.getAttribute("value");
            return true;
        }
        else if (name == "gold" ||
            name == "value") {
            this.gold = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (name == "takeable") {
            this.takeable = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.takeable = true;
            return true;
        }
        else if (name == "usable" ||
            name == "useable") {
            this.usable = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.usable = true;
            return true;
        }
        else if (name == "interacteable") {
            this.interacteable = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.interacteable = true;
            return true;
        }
        else if (name == "burrowed") {
            this.burrowed = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.burrowed = true;
            return true;
        }
        else if (name == "direction") {
            this.direction = Number(attribute_xml.getAttribute("value"));
            this.currentAnimation = A4_ANIMATION_IDLE_LEFT + this.direction;
            //            console.log(this.name + ".direction = " + this.direction);
            return true;
        }
        else if (name == "tallness") {
            this.pixel_tallness = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (name == "sprite_offs_x") {
            this.sprite_offs_x = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (name == "sprite_offs_y") {
            this.sprite_offs_y = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (name == "pixel_width") {
            this.pixel_width = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (name == "pixel_height") {
            this.pixel_height = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (name == "drawDarkIfNoLight") {
            this.drawDarkIfNoLight = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.drawDarkIfNoLight = true;
            return true;
        }
        return false;
    };
    A4Object.prototype.revisitObject = function (xml, game) {
    };
    A4Object.prototype.pushScripttoExecute = function (script, map, game, otherCharacter) {
        var sq = new A4ScriptExecutionQueue(this, map, game, otherCharacter);
        sq.scripts.push(script);
        this.addScriptQueue(sq);
    };
    A4Object.prototype.saveToXML = function (game, type, saveLocation) {
        var xmlString = "<object id=\"" + this.ID + "\"";
        if (type == 0) {
            xmlString += " type=\"" + this.sort.name + "\"";
            xmlString += " completeRedefinition=\"true\"";
        }
        else if (type == 1) {
            xmlString += " name=\"" + this.name + "\"";
            xmlString += " type=\"Bridge\"";
        }
        else if (type == 2) {
            xmlString += " name=\"" + this.name + "\"";
            xmlString += " type=\"BridgeDestination\"";
        }
        if (saveLocation) {
            // Coordinates in xml do not take tallness into account (it's easier to see it this way
            // in Tiled), so, we correct by subtracting tallness:
            xmlString += " x=\"" + this.x + "\"";
            xmlString += " y=\"" + (this.y - this.pixel_tallness) + "\"";
            xmlString += " width=\"" + this.getPixelWidth() + "\"";
            xmlString += " height=\"" + (this.getPixelHeight() + this.pixel_tallness) + "\"";
        }
        xmlString += ">\n";
        xmlString += this.savePropertiesToXML(game) + "\n";
        for (var _i = 0, _a = this.perceptionProperties; _i < _a.length; _i++) {
            var prop = _a[_i];
            xmlString += "<perceptionProperty value=\"" + prop + "\"/>\n";
        }
        xmlString += "</object>";
        return xmlString;
    };
    A4Object.prototype.saveToXMLForMainFile = function (game, tag, mapNumber) {
        var xmlString = "";
        xmlString += "<" + tag + " id=\"" + this.ID + "\"" +
            " type=\"" + this.sort.name + "\"" +
            " completeRedefinition=\"true\"" +
            " x=\"" + this.x + "\"" +
            " y=\"" + this.y + "\"" +
            " map=\"" + mapNumber + "\">\n";
        xmlString += this.savePropertiesToXML(game);
        xmlString += "</" + tag + ">";
        return xmlString;
    };
    A4Object.prototype.savePropertiesToXML = function (game) {
        var xmlString = "";
        for (var i = 0; i < A4_N_ANIMATIONS; i++) {
            if (this.animations[i] != null) {
                xmlString += this.animations[i].saveToXML(animationNames[i]) + "\n";
            }
        }
        if (this.name != null)
            xmlString += this.saveObjectAttributeToXML("name", this.name) + "\n";
        if (this.gold > 0)
            xmlString += this.saveObjectAttributeToXML("gold", this.gold) + "\n";
        if (this.isCharacter() || this.isVehicle()) {
            xmlString += this.saveObjectAttributeToXML("canWalk", this.canWalk) + "\n";
            xmlString += this.saveObjectAttributeToXML("canSwim", this.canSwim) + "\n";
        }
        xmlString += this.saveObjectAttributeToXML("takeable", this.takeable) + "\n";
        xmlString += this.saveObjectAttributeToXML("usable", this.usable) + "\n";
        xmlString += this.saveObjectAttributeToXML("interacteable", this.interacteable) + "\n";
        xmlString += this.saveObjectAttributeToXML("burrowed", this.burrowed) + "\n";
        xmlString += this.saveObjectAttributeToXML("direction", this.direction) + "\n";
        if (this.pixel_tallness != 0)
            xmlString += this.saveObjectAttributeToXML("tallness", this.pixel_tallness) + "\n";
        if (this.sprite_offs_x != 0)
            xmlString += this.saveObjectAttributeToXML("sprite_offs_x", this.sprite_offs_x) + "\n";
        if (this.sprite_offs_y != 0)
            xmlString += this.saveObjectAttributeToXML("sprite_offs_y", this.sprite_offs_y) + "\n";
        if (this.pixel_width != 0)
            xmlString += this.saveObjectAttributeToXML("pixel_width", this.pixel_width) + "\n";
        if (this.pixel_height != 0)
            xmlString += this.saveObjectAttributeToXML("pixel_height", this.pixel_height + this.pixel_tallness) + "\n";
        if (!this.drawDarkIfNoLight)
            xmlString += this.saveObjectAttributeToXML("drawDarkIfNoLight", this.drawDarkIfNoLight) + "\n";
        var onStarttagOpen = false;
        for (var v in this.storyState) {
            if (!onStarttagOpen) {
                xmlString += "<onStart>\n";
                onStarttagOpen = true;
            }
            xmlString += "<storyState variable=\"" + v + "\"" +
                " value=\"" + this.storyState[v] + "\"" +
                " scope=\"object\"/>\n";
        }
        if (onStarttagOpen)
            xmlString += "</onStart>\n";
        // each execution queue goes to its own "onStart" block:
        for (var _i = 0, _a = this.scriptQueues; _i < _a.length; _i++) {
            var seq = _a[_i];
            xmlString += "<onStart>\n";
            for (var _b = 0, _c = seq.scripts; _b < _c.length; _b++) {
                var s = _c[_b];
                xmlString += s.saveToXML() + "\n";
            }
            xmlString += "</onStart>\n";
        }
        if (this.deadRequest) {
            xmlString += "<onStart>\n";
            xmlString += "<die/>\n";
            xmlString += "</onStart>\n";
        }
        if (this.agendas.length > 0) {
            xmlString += "<onStart>\n";
            // create a script for the agenda
            for (var _d = 0, _e = this.agendas; _d < _e.length; _d++) {
                var a = _e[_d];
                xmlString += "<addAgenda agenda=\"" + a.name + "\"" +
                    " duration=\"" + a.duration + "\"" +
                    " loop=\"" + a.loop + "\"" +
                    " cycle=\"" + a.cycle + "\">\n";
                for (var _f = 0, _g = a.entries; _f < _g.length; _f++) {
                    var ae = _g[_f];
                    xmlString += "<entry time=\"" + ae.time + "\">\n";
                    for (var _h = 0, _j = ae.scripts; _h < _j.length; _h++) {
                        var s = _j[_h];
                        xmlString += s.saveToXML() + "\n";
                    }
                    xmlString += "</entry>\n";
                }
                xmlString += "</addAgenda>\n";
            }
            xmlString += "</onStart>\n";
        }
        // rules:
        for (var i = 0; i < A4_NEVENTS; i++) {
            if (this.eventScripts[i] != null) {
                for (var _k = 0, _l = this.eventScripts[i]; _k < _l.length; _k++) {
                    var er = _l[_k];
                    xmlString += er.saveToXML() + "\n";
                }
            }
        }
        return xmlString;
    };
    A4Object.prototype.saveObjectAttributeToXML = function (property, value) {
        return "<attribute name=\"" + property + "\" value=\"" + value + "\"/>";
    };
    A4Object.prototype.update = function (game) {
        this.executeScriptQueues(game); // this has to be done first, since "onStart" is put here
        if (this.cycle == 0)
            this.event(A4_EVENT_START, null, this.map, game);
        if (this.eventScripts[A4_EVENT_TIMER] != null) {
            for (var _i = 0, _a = this.eventScripts[A4_EVENT_TIMER]; _i < _a.length; _i++) {
                var r = _a[_i];
                r.execute(this, this.map, game, null);
            }
        }
        if (this.eventScripts[A4_EVENT_STORYSTATE] != null) {
            for (var _b = 0, _c = this.eventScripts[A4_EVENT_STORYSTATE]; _b < _c.length; _b++) {
                var r = _c[_b];
                r.execute(this, this.map, game, null);
            }
        }
        var toDelete = [];
        for (var _d = 0, _e = this.agendas; _d < _e.length; _d++) {
            var a = _e[_d];
            if (a.execute(this, this.map, game, null))
                toDelete.push(a);
        }
        for (var _f = 0, toDelete_1 = toDelete; _f < toDelete_1.length; _f++) {
            var a = toDelete_1[_f];
            var idx = this.agendas.indexOf(a);
            this.agendas.splice(idx, 1);
        }
        if (this.map != null) {
            // only for those items actually on a map (and not inside of others)
            // change to the proper animation given the current direction:
            if (this.currentAnimation == A4_ANIMATION_IDLE && this.direction != A4_DIRECTION_NONE) {
                this.currentAnimation = this.direction + 1;
            }
            if (this.animations[this.currentAnimation] != null) {
                if (!this.animations[this.currentAnimation].update()) {
                    if (this.previousSeeThrough != this.animations[this.currentAnimation].seeThrough) {
                        //console.log("object " + this.name + " changed seeThrough from " + this.previousSeeThrough + " to " + !this.previousSeeThrough);
                        this.map.reevaluateVisibilityRequest();
                        this.previousSeeThrough = !this.previousSeeThrough;
                    }
                }
            }
        }
        if (this.deadRequest)
            return false;
        this.cycle++;
        return true;
    };
    A4Object.prototype.draw = function (offsetx, offsety, game) {
        if (this.currentAnimation >= 0 && this.animations[this.currentAnimation] != null) {
            // debugging: draw the base of the object in red color to check collisions
            // ctx.fillStyle = "red";
            // ctx.fillRect((this.x + offsetx) - this.sprite_offs_x, 
            //              (this.y + offsety) - this.sprite_offs_y, 
            //              this.getPixelWidth(), this.getPixelHeight());
            this.animations[this.currentAnimation].draw((this.x + offsetx) - this.sprite_offs_x, (this.y + offsety) - this.sprite_offs_y - this.pixel_tallness);
        }
    };
    A4Object.prototype.drawDark = function (offsetx, offsety, game) {
        if (this.drawDarkIfNoLight) {
            if (this.currentAnimation >= 0 && this.animations[this.currentAnimation] != null) {
                this.animations[this.currentAnimation].drawDark((this.x + offsetx) - this.sprite_offs_x, (this.y + offsety) - this.sprite_offs_y - this.pixel_tallness);
            }
        }
        else {
            this.draw(offsetx, offsety - this.pixel_tallness, game);
        }
    };
    // this executes all the A4EventRules with the given event:
    A4Object.prototype.event = function (event, otherCharacter, map, game) {
        if (this.eventScripts[event] == null)
            return false;
        for (var _i = 0, _a = this.eventScripts[event]; _i < _a.length; _i++) {
            var rule = _a[_i];
            rule.executeEffects(this, map, game, otherCharacter);
        }
        return true;
    };
    A4Object.prototype.eventWithID = function (event, ID, otherCharacter, map, game) {
    };
    A4Object.prototype.eventWithObject = function (event, otherCharacter, object, map, game) {
        if (this.eventScripts[event] == null)
            return;
        for (var _i = 0, _a = this.eventScripts[event]; _i < _a.length; _i++) {
            var rule = _a[_i];
            if (event == A4_EVENT_RECEIVE ||
                event == A4_EVENT_ACTION_TAKE ||
                event == A4_EVENT_ACTION_DROP ||
                event == A4_EVENT_ACTION_USE ||
                event == A4_EVENT_ACTION_INTERACT ||
                event == A4_EVENT_ACTION_CHOP ||
                event == A4_EVENT_ACTION_GIVE ||
                event == A4_EVENT_ACTION_SELL ||
                event == A4_EVENT_ACTION_BUY) {
                if (rule.item == null ||
                    object.name == rule.item ||
                    object.is_a_string(rule.item)) {
                    rule.executeEffects(this, map, game, otherCharacter);
                }
            }
            else {
                console.error("eventWithObject for event " + event + ", is undefined\n");
            }
        }
    };
    A4Object.prototype.eventWithInteger = function (event, value, otherCharacter, map, game) {
        if (this.eventScripts[event] == null)
            return;
        for (var _i = 0, _a = this.eventScripts[event]; _i < _a.length; _i++) {
            var rule = _a[_i];
            console.error("eventWithInteger for event " + event + " is undefined, cannot execute rule: " + rule);
        }
    };
    A4Object.prototype.executeScriptQueues = function (game) {
        var toDelete = [];
        for (var _i = 0, _a = this.scriptQueues; _i < _a.length; _i++) {
            var seb = _a[_i];
            while (true) {
                var s = seb.scripts[0];
                var retval = s.execute((seb.object == null ? this : seb.object), (seb.map == null ? this.map : seb.map), (seb.game == null ? game : seb.game), seb.otherCharacter);
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
        for (var _b = 0, toDelete_2 = toDelete; _b < toDelete_2.length; _b++) {
            var seb = toDelete_2[_b];
            var idx = this.scriptQueues.indexOf(seb);
            this.scriptQueues.splice(idx, 1);
        }
    };
    A4Object.prototype.addScriptQueue = function (seq) {
        this.scriptQueues.push(seq);
    };
    A4Object.prototype.addEventRule = function (event, r) {
        if (this.eventScripts[event] == null)
            this.eventScripts[event] = [];
        this.eventScripts[event].push(r);
    };
    A4Object.prototype.setStoryStateVariable = function (variable, value, game) {
        //        console.log("A4Object.setStoryStateVariable ("+this.ID+"): " + variable + " = " + value + " (at time " + game.cycle + ")");
        this.storyState[variable] = value;
        this.lastTimeStoryStateChanged = game.cycle;
    };
    A4Object.prototype.getStoryStateVariable = function (variable) {
        return this.storyState[variable];
    };
    A4Object.prototype.warp = function (x, y, map) {
        var reAdd = true;
        if (this.map != null)
            reAdd = this.map.removeObject(this);
        this.x = x;
        this.y = y;
        this.map = map;
        if (reAdd && this.map != null)
            this.map.addObject(this);
    };
    A4Object.prototype.getPixelWidth = function () {
        if (this.pixel_width > 0)
            return this.pixel_width;
        if (this.pixel_width_cache_cycle == this.cycle)
            return this.pixel_width_cache;
        if (this.currentAnimation < 0)
            return 0;
        var a = this.animations[this.currentAnimation];
        if (a == null)
            return 0;
        this.pixel_width_cache = a.getPixelWidth();
        this.pixel_height_cache = a.getPixelHeight() - this.pixel_tallness;
        this.pixel_width_cache_cycle = this.cycle;
        return this.pixel_width_cache;
    };
    A4Object.prototype.getPixelHeight = function () {
        if (this.pixel_height > 0)
            return this.pixel_height;
        if (this.pixel_width_cache_cycle == this.cycle)
            return this.pixel_height_cache;
        if (this.currentAnimation < 0)
            return 0;
        var a = this.animations[this.currentAnimation];
        if (a == null)
            return 0;
        this.pixel_width_cache = a.getPixelWidth();
        this.pixel_height_cache = a.getPixelHeight() - this.pixel_tallness;
        this.pixel_width_cache_cycle = this.cycle;
        return this.pixel_height_cache;
    };
    A4Object.prototype.setAnimation = function (idx, a) {
        this.animations[idx] = a;
    };
    A4Object.prototype.getAnimation = function (idx) {
        return this.animations[idx];
    };
    A4Object.prototype.getCurrentAnimation = function () {
        return this.animations[this.currentAnimation];
    };
    A4Object.prototype.die = function () {
        this.deadRequest = true;
    };
    A4Object.prototype.isWalkable = function () { return true; };
    A4Object.prototype.isHeavy = function () { return false; }; // this is used by pressure plates
    A4Object.prototype.isPlayer = function () { return false; };
    //    isInteracteable():boolean {return false;}
    A4Object.prototype.isKey = function () { return false; };
    A4Object.prototype.isCharacter = function () { return false; };
    A4Object.prototype.isDoor = function () { return false; };
    A4Object.prototype.isVehicle = function () { return false; };
    A4Object.prototype.isAICharacter = function () { return false; };
    A4Object.prototype.isTrigger = function () { return false; };
    A4Object.prototype.isPushable = function () { return false; };
    A4Object.prototype.respawns = function () { return false; };
    A4Object.prototype.seeThrough = function () {
        var a = this.animations[this.currentAnimation];
        if (a == null)
            return true;
        return a.seeThrough;
    };
    A4Object.prototype.collision = function (x2, y2, dx2, dy2) {
        var dx = this.getPixelWidth();
        var dy = this.getPixelHeight();
        if (this.x + dx > x2 && x2 + dx2 > this.x &&
            this.y + dy > y2 && y2 + dy2 > this.y)
            return true;
        return false;
    };
    A4Object.prototype.collisionObject = function (o2) {
        var dx = this.getPixelWidth();
        var dy = this.getPixelHeight();
        var dx2 = o2.getPixelWidth();
        var dy2 = o2.getPixelHeight();
        if (this.x + dx > o2.x && o2.x + dx2 > this.x &&
            this.y + dy > o2.y && o2.y + dy2 > this.y)
            return true;
        return false;
    };
    A4Object.prototype.collisionObjectOffset = function (xoffs, yoffs, o2) {
        var dx = this.getPixelWidth();
        var dy = this.getPixelHeight();
        var dx2 = o2.getPixelWidth();
        var dy2 = o2.getPixelHeight();
        if (this.x + xoffs + dx > o2.x && o2.x + dx2 > this.x + xoffs &&
            this.y + yoffs + dy > o2.y && o2.y + dy2 > this.y + yoffs)
            return true;
        return false;
    };
    A4Object.prototype.canMove = function (direction, treatBridgesAsWalls) {
        if (treatBridgesAsWalls) {
            if (this.canMoveWithoutGoingThroughABridge(direction))
                return true;
            return false;
        }
        if (this.map.walkableConsideringVehicles(this.x + direction_x_inc[direction] * this.map.tileWidth, this.y + direction_y_inc[direction] * this.map.tileHeight, this.getPixelWidth(), this.getPixelHeight(), this))
            return true;
        return false;
    };
    A4Object.prototype.canMoveIgnoringObject = function (direction, treatBridgesAsWalls, toIgnore) {
        if (treatBridgesAsWalls) {
            if (this.canMoveWithoutGoingThroughABridgeIgnoringObject(direction, toIgnore))
                return true;
            return false;
        }
        if (this.map.walkableIgnoringObject(this.x + direction_x_inc[direction] * this.map.tileWidth, this.y + direction_y_inc[direction] * this.map.tileHeight, this.getPixelWidth(), this.getPixelHeight(), this, toIgnore))
            return true;
        return false;
    };
    A4Object.prototype.canMoveWithoutGoingThroughABridge = function (direction) {
        if (this.map.getBridge(Math.floor(this.x + direction_x_inc[direction] * this.map.tileWidth + this.getPixelWidth() / 2), Math.floor(this.y + direction_y_inc[direction] * this.map.tileHeight + this.getPixelHeight() / 2)) != null)
            return false;
        if (this.map.walkableConsideringVehicles(this.x + direction_x_inc[direction] * this.map.tileWidth, this.y + direction_y_inc[direction] * this.map.tileHeight, this.getPixelWidth(), this.getPixelHeight(), this))
            return true;
        return false;
    };
    A4Object.prototype.canMoveWithoutGoingThroughABridgeIgnoringObject = function (direction, toIgnore) {
        if (this.map.getBridge(Math.floor(this.x + direction_x_inc[direction] * this.map.tileWidth + this.getPixelWidth() / 2), Math.floor(this.y + direction_y_inc[direction] * this.map.tileHeight + this.getPixelHeight() / 2)) != null)
            return false;
        if (this.map.walkableIgnoringObject(this.x + direction_x_inc[direction] * this.map.tileWidth, this.y + direction_y_inc[direction] * this.map.tileHeight, this.getPixelWidth(), this.getPixelHeight(), this, toIgnore))
            return true;
        return false;
    };
    A4Object.prototype.pixelDistance = function (o2) {
        var dx = 0;
        if (this.x > o2.x + o2.getPixelWidth()) {
            dx = this.x - (o2.x + o2.getPixelWidth());
        }
        else if (o2.x > this.x + this.getPixelWidth()) {
            dx = o2.x - (this.x + this.getPixelWidth());
        }
        var dy = 0;
        if (this.y > o2.y + o2.getPixelHeight()) {
            dy = this.y - (o2.y + o2.getPixelHeight());
        }
        else if (o2.y > this.y + this.getPixelHeight()) {
            dy = o2.y - (this.y + this.getPixelHeight());
        }
        return dx + dy;
    };
    A4Object.prototype.pixelDistanceBetweenCentersOffset = function (o2, o2xoff, o2yoff) {
        var dx = (this.x + this.getPixelWidth() / 2) - (o2xoff + o2.x + o2.getPixelWidth() / 2);
        var dy = (this.y + this.getPixelHeight() / 2) - (o2yoff + o2.y + o2.getPixelHeight() / 2);
        return Math.sqrt(dx * dx + dy * dy);
    };
    A4Object.prototype.addAgenda = function (a) {
        this.removeAgenda(a.name);
        this.agendas.push(a);
    };
    A4Object.prototype.removeAgenda = function (agenda) {
        for (var _i = 0, _a = this.agendas; _i < _a.length; _i++) {
            var a2 = _a[_i];
            if (a2.name == agenda) {
                var idx = this.agendas.indexOf(a2);
                this.agendas.splice(idx, 1);
                return;
            }
        }
    };
    A4Object.prototype.objectRemoved = function (o) {
    };
    A4Object.prototype.findObjectByName = function (name) {
        return null;
    };
    A4Object.prototype.findObjectByID = function (ID) {
        return null;
    };
    // sorts:
    A4Object.prototype.is_a = function (s) {
        return this.sort.is_a(s);
    };
    A4Object.prototype.is_a_string = function (s) {
        return this.sort.is_a_string(s);
    };
    A4Object.s_nextID = 10000; // start with a high-enough number so that there is no collisions with the maps
    return A4Object;
}());
