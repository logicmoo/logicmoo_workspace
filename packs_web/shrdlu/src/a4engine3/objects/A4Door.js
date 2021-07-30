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
var A4Door = /** @class */ (function (_super) {
    __extends(A4Door, _super);
    function A4Door(sort, ID, closed, consumeKey, a_closed, a_open) {
        var _this = _super.call(this, "door", sort) || this;
        _this.doorGroupID = null;
        _this.closed = true;
        _this.consumeKey = true;
        _this.automatic = false;
        _this.automaticTimmer = 0;
        _this.canBeOpen = true;
        _this.doorID = ID;
        _this.closed = closed;
        _this.consumeKey = consumeKey;
        _this.interacteable = true;
        _this.canBeOpen = true;
        _this.animations[A4_ANIMATION_CLOSED] = a_closed;
        _this.animations[A4_ANIMATION_OPEN] = a_open;
        if (_this.closed)
            _this.currentAnimation = A4_ANIMATION_CLOSED;
        else
            _this.currentAnimation = A4_ANIMATION_OPEN;
        return _this;
    }
    A4Door.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "doorID") {
            this.doorID = attribute_xml.getAttribute("value");
            return true;
        }
        else if (a_name == "doorgroup") {
            this.doorGroupID = attribute_xml.getAttribute("value");
            return true;
        }
        else if (a_name == "closed") {
            this.closed = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.closed = true;
            if (this.closed)
                this.currentAnimation = A4_ANIMATION_CLOSED;
            else
                this.currentAnimation = A4_ANIMATION_OPEN;
            return true;
        }
        else if (a_name == "consumeKey") {
            this.consumeKey = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.consumeKey = true;
            return true;
        }
        else if (a_name == "automatic") {
            this.automatic = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.automatic = true;
            return true;
        }
        else if (a_name == "canBeOpen") {
            this.canBeOpen = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.canBeOpen = true;
            return true;
        }
        return false;
    };
    A4Door.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        if (this.doorID != null)
            xmlString += this.saveObjectAttributeToXML("doorID", this.doorID) + "\n";
        if (this.doorGroupID != null)
            xmlString += this.saveObjectAttributeToXML("doorgroup", this.doorGroupID) + "\n";
        xmlString += this.saveObjectAttributeToXML("closed", this.closed) + "\n";
        xmlString += this.saveObjectAttributeToXML("consumeKey", this.consumeKey) + "\n";
        xmlString += this.saveObjectAttributeToXML("automatic", this.automatic) + "\n";
        xmlString += this.saveObjectAttributeToXML("canBeOpen", this.canBeOpen) + "\n";
        return xmlString;
    };
    A4Door.prototype.isWalkable = function () {
        return !this.closed;
    };
    A4Door.prototype.update = function (game) {
        var ret = _super.prototype.update.call(this, game);
        if (this.canBeOpen && this.automatic) {
            // do not check every frame
            this.automaticTimmer--;
            if (this.automaticTimmer <= 0) {
                var radius = 2;
                var x1 = this.x - radius * this.map.tileWidth;
                var dx = this.getPixelWidth() + (2 * radius) * this.map.tileWidth;
                var y1 = this.y - radius * this.map.tileHeight;
                var dy = this.getPixelHeight() + (2 * radius) * this.map.tileHeight;
                var l = this.map.getAllObjects(x1, y1, dx, dy);
                var characterAround = null;
                for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
                    var o = l_1[_i];
                    if (o.isCharacter()) {
                        characterAround = o;
                        break;
                    }
                }
                if (this.closed) {
                    if (characterAround != null) {
                        this.changeStateRecursively(false, characterAround, this.map, game);
                    }
                }
                else {
                    if (characterAround == null) {
                        this.changeStateRecursively(true, null, this.map, game);
                    }
                }
                this.automaticTimmer = 8;
            }
        }
        return ret;
    };
    A4Door.prototype.canOpen = function (character, game) {
        if (!this.canBeOpen)
            return false;
        if (this.doorID == null)
            return true;
        // see if the character has the key:
        for (var _i = 0, _a = character.inventory; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.isKey()) {
                var key = o;
                // the player has the proper key!
                if (key.keyID == this.doorID)
                    return true;
                if (key.keyID == "MASTERKEY") {
                    return true;
                }
            }
        }
        return false;
    };
    A4Door.prototype.canOpenKey = function (character, game) {
        if (!this.canBeOpen)
            return null;
        if (this.doorID == null)
            return null;
        // see if the character has the key:
        for (var _i = 0, _a = character.inventory; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.isKey()) {
                var key = o;
                // the player has the proper key!
                if (key.keyID == this.doorID)
                    return key;
                if (key.keyID == "MASTERKEY") {
                    return key;
                }
            }
        }
        return null;
    };
    A4Door.prototype.event = function (a_event, character, map, game) {
        var retval = _super.prototype.event.call(this, a_event, character, map, game);
        if (a_event == A4_EVENT_INTERACT) {
            if (this.consumeKey && !this.closed)
                return false; // if it consumes the key, it cannot be reopened!
            if (this.canOpen(character, game)) {
                if (this.checkForBlockages(!this.closed, character, map, game, [])) {
                    var key = this.canOpenKey(character, game);
                    // change all the doors in the same doorgroup:
                    if (this.doorGroupID == null) {
                        this.changeStateRecursively(!this.closed, character, map, game);
                        if (this.consumeKey && key != null) {
                            character.removeFromInventory(key);
                            game.requestDeletion(key);
                        }
                        return true;
                    }
                    else {
                        if (game.checkIfDoorGroupStateCanBeChanged(this.doorGroupID, this.closed, character)) {
                            this.changeStateRecursively(!this.closed, character, map, game);
                            game.setDoorGroupState(this.doorGroupID, this.closed, character);
                            if (this.consumeKey && key != null) {
                                character.removeFromInventory(key);
                                game.requestDeletion(key);
                                return true;
                            }
                        }
                    }
                }
            }
        }
        return retval;
    };
    A4Door.prototype.eventWithID = function (a_event, ID, character, map, game) {
        _super.prototype.eventWithID.call(this, a_event, ID, character, map, game);
        if ((a_event == A4_EVENT_OPEN ||
            a_event == A4_EVENT_CLOSE) && this.doorID == ID) {
            if (this.eventScripts[a_event] != null) {
                for (var _i = 0, _a = this.eventScripts[a_event]; _i < _a.length; _i++) {
                    var rule = _a[_i];
                    rule.executeEffects(this, map, game, character);
                }
            }
            this.closed = (this.closed ? false : true);
            if (this.closed) {
                this.currentAnimation = A4_ANIMATION_CLOSED;
                this.event(A4_EVENT_CLOSE, character, map, game);
            }
            else {
                this.currentAnimation = A4_ANIMATION_OPEN;
                this.event(A4_EVENT_OPEN, character, map, game);
            }
            if (this.animations[this.currentAnimation] != null)
                this.animations[this.currentAnimation].reset();
            //            if (character!=null) {
            //            map.reevaluateVisibilityRequest();
            //            }
        }
    };
    A4Door.prototype.isDoor = function () {
        return true;
    };
    A4Door.prototype.changeStateRecursively = function (closed, character, map, game) {
        if (this.closed == closed)
            return;
        this.eventWithID(A4_EVENT_OPEN, this.doorID, character, map, game);
    };
    A4Door.prototype.checkForBlockages = function (closed, character, map, game, alreadyVisited) {
        if (closed) {
            for (var _i = 0, alreadyVisited_1 = alreadyVisited; _i < alreadyVisited_1.length; _i++) {
                var d_1 = alreadyVisited_1[_i];
                if (this == d_1)
                    return true;
            }
            alreadyVisited.push(this);
            // closing the doors:
            var blockage = false;
            var l = this.map.getAllObjectCollisions(this);
            for (var _a = 0, l_2 = l; _a < l_2.length; _a++) {
                var caught = l_2[_a];
                if (caught.isCharacter()) {
                    blockage = true;
                }
                else if (caught.isVehicle()) {
                    blockage = true;
                }
            }
            return !blockage;
        }
        else {
            // opening the doors:
            return true;
        }
    };
    A4Door.prototype.getPixelWidth = function () {
        if (this.pixel_width_cache_cycle == this.cycle)
            return this.pixel_width_cache;
        var dx1 = (this.animations[A4_ANIMATION_CLOSED] == null ? 0 : this.animations[A4_ANIMATION_CLOSED].getPixelWidth());
        var dy1 = (this.animations[A4_ANIMATION_CLOSED] == null ? 0 : this.animations[A4_ANIMATION_CLOSED].getPixelHeight() - this.pixel_tallness);
        var dx2 = (this.animations[A4_ANIMATION_OPEN] == null ? 0 : this.animations[A4_ANIMATION_OPEN].getPixelWidth());
        var dy2 = (this.animations[A4_ANIMATION_OPEN] == null ? 0 : this.animations[A4_ANIMATION_OPEN].getPixelHeight() - this.pixel_tallness);
        var dx = (dx1 > dx2 ? dx1 : dx2);
        var dy = (dy1 > dy2 ? dy1 : dy2);
        this.pixel_width_cache = dx;
        this.pixel_height_cache = dy;
        this.pixel_width_cache_cycle = this.cycle;
        return this.pixel_width_cache;
    };
    A4Door.prototype.getPixelHeight = function () {
        if (this.pixel_width_cache_cycle == this.cycle)
            return this.pixel_height_cache;
        var dx1 = (this.animations[A4_ANIMATION_CLOSED] == null ? 0 : this.animations[A4_ANIMATION_CLOSED].getPixelWidth());
        var dy1 = (this.animations[A4_ANIMATION_CLOSED] == null ? 0 : this.animations[A4_ANIMATION_CLOSED].getPixelHeight() - this.pixel_tallness);
        var dx2 = (this.animations[A4_ANIMATION_OPEN] == null ? 0 : this.animations[A4_ANIMATION_OPEN].getPixelWidth());
        var dy2 = (this.animations[A4_ANIMATION_OPEN] == null ? 0 : this.animations[A4_ANIMATION_OPEN].getPixelHeight() - this.pixel_tallness);
        var dx = (dx1 > dx2 ? dx1 : dx2);
        var dy = (dy1 > dy2 ? dy1 : dy2);
        this.pixel_width_cache = dx;
        this.pixel_height_cache = dy;
        this.pixel_width_cache_cycle = this.cycle;
        return this.pixel_height_cache;
    };
    return A4Door;
}(A4Object));
