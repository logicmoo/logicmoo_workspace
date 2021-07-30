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
var A4ObstacleContainer = /** @class */ (function (_super) {
    __extends(A4ObstacleContainer, _super);
    function A4ObstacleContainer(name, sort, ID, closed, consumeKey, a_closed, a_open) {
        var _this = _super.call(this, name, sort, closed ? a_closed : a_open) || this;
        _this.closed = true;
        _this.closeable = true;
        _this.consumeKey = true;
        _this.doorID = ID;
        _this.closed = closed;
        _this.consumeKey = consumeKey;
        _this.interacteable = true;
        _this.takeable = false;
        _this.animations[A4_ANIMATION_CLOSED] = a_closed;
        _this.animations[A4_ANIMATION_OPEN] = a_open;
        if (_this.closed)
            _this.currentAnimation = A4_ANIMATION_CLOSED;
        else
            _this.currentAnimation = A4_ANIMATION_OPEN;
        return _this;
    }
    A4ObstacleContainer.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "doorID") {
            this.doorID = attribute_xml.getAttribute("value");
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
        else if (a_name == "closeable") {
            this.closeable = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.closeable = true;
            if (!this.closeable)
                this.closed = false;
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
        return false;
    };
    A4ObstacleContainer.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        if (this.doorID != null)
            xmlString += this.saveObjectAttributeToXML("doorID", this.doorID) + "\n";
        xmlString += this.saveObjectAttributeToXML("closed", this.closed) + "\n";
        xmlString += this.saveObjectAttributeToXML("closeable", this.closeable) + "\n";
        xmlString += this.saveObjectAttributeToXML("consumeKey", this.consumeKey) + "\n";
        return xmlString;
    };
    A4ObstacleContainer.prototype.event = function (a_event, character, map, game) {
        var retval = _super.prototype.event.call(this, a_event, character, map, game);
        if (a_event == A4_EVENT_INTERACT) {
            //console.log(this.name + " receives event interact!");
            if (this.consumeKey && !this.closed)
                return false; // if it consumes the key, it cannot be reopened!
            if (this.doorID == null) {
                this.eventWithID(A4_EVENT_OPEN, null, character, map, game);
                return true;
            }
            else {
                // see if the character has the key:
                for (var _i = 0, _a = character.inventory; _i < _a.length; _i++) {
                    var o = _a[_i];
                    if (o.isKey()) {
                        var key = o;
                        if (key.keyID == this.doorID) {
                            // the player has the proper key!
                            this.eventWithID(A4_EVENT_OPEN, key.keyID, character, map, game);
                            if (this.consumeKey) {
                                character.removeFromInventory(key);
                                game.requestDeletion(key);
                            }
                            return true;
                            break;
                        }
                    }
                }
            }
        }
        return retval;
    };
    A4ObstacleContainer.prototype.eventWithID = function (a_event, ID, character, map, game) {
        _super.prototype.eventWithID.call(this, a_event, ID, character, map, game);
        if (a_event == A4_EVENT_OPEN && this.doorID == ID) {
            //console.log(this.name + " receives event open " + ID + "!");
            if (this.eventScripts[a_event] != null) {
                for (var _i = 0, _a = this.eventScripts[a_event]; _i < _a.length; _i++) {
                    var rule = _a[_i];
                    rule.executeEffects(this, map, game, character);
                }
            }
            if (this.closeable)
                this.closed = (this.closed ? false : true);
            if (this.closed) {
                this.currentAnimation = A4_ANIMATION_CLOSED;
                if (this.content.length > 0 && this.animations[A4_ANIMATION_CLOSED_FULL] != null)
                    this.currentAnimation = A4_ANIMATION_CLOSED_FULL;
                this.event(A4_EVENT_CLOSE, character, map, game);
                //<shrdluspecific>
                if (character == game.currentPlayer &&
                    game.HUD_state == SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
                    game.HUD_state = SHRDLU_HUD_STATE_INVENTORY;
                    game.HUD_remote_inventory = null;
                }
                //</shrdluspecific>
            }
            else {
                this.currentAnimation = A4_ANIMATION_OPEN;
                if (this.content.length > 0 && this.animations[A4_ANIMATION_OPEN_FULL] != null)
                    this.currentAnimation = A4_ANIMATION_OPEN_FULL;
                this.event(A4_EVENT_OPEN, character, map, game);
                //<shrdluspecific>
                if (character == game.currentPlayer) {
                    game.HUD_state = SHRDLU_HUD_STATE_SPLIT_INVENTORY;
                    game.HUD_remote_inventory = this;
                    game.HUD_remote_inventory_start = 0;
                    game.HUD_remote_inventory_selected = -1;
                }
                //</shrdluspecific>
            }
            if (this.animations[this.currentAnimation] != null)
                this.animations[this.currentAnimation].reset();
        }
    };
    A4ObstacleContainer.prototype.isWalkable = function () { return false; };
    A4ObstacleContainer.prototype.isTakeable = function () { return false; };
    A4ObstacleContainer.prototype.addContent = function (o) {
        this.content.push(o);
        if (this.closed) {
            if (this.currentAnimation == A4_ANIMATION_CLOSED_EMPTY) {
                if (this.animations[A4_ANIMATION_CLOSED_FULL] != null)
                    this.currentAnimation = A4_ANIMATION_CLOSED_FULL;
            }
        }
        else {
            if (this.currentAnimation == A4_ANIMATION_OPEN_EMPTY) {
                if (this.animations[A4_ANIMATION_OPEN_FULL] != null)
                    this.currentAnimation = A4_ANIMATION_OPEN_FULL;
            }
        }
        if (this.animations[this.currentAnimation] != null)
            this.animations[this.currentAnimation].reset();
    };
    A4ObstacleContainer.prototype.objectRemoved = function (o) {
        _super.prototype.objectRemoved.call(this, o);
        for (var _i = 0, _a = this.content; _i < _a.length; _i++) {
            var o2 = _a[_i];
            o2.objectRemoved(o);
        }
        if (this.content.length == 0) {
            if (this.closed) {
                if (this.currentAnimation == A4_ANIMATION_CLOSED_FULL) {
                    if (this.animations[A4_ANIMATION_CLOSED_EMPTY] != null)
                        this.currentAnimation = A4_ANIMATION_CLOSED_EMPTY;
                }
            }
            else {
                if (this.currentAnimation == A4_ANIMATION_OPEN_FULL) {
                    if (this.animations[A4_ANIMATION_OPEN_EMPTY] != null)
                        this.currentAnimation = A4_ANIMATION_OPEN_EMPTY;
                }
            }
            if (this.animations[this.currentAnimation] != null)
                this.animations[this.currentAnimation].reset();
        }
    };
    return A4ObstacleContainer;
}(A4Container));
;
