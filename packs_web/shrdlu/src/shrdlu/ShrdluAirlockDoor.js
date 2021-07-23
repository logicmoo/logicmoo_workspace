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
var ShrdluAirlockDoor = /** @class */ (function (_super) {
    __extends(ShrdluAirlockDoor, _super);
    function ShrdluAirlockDoor(name, sort) {
        var _this = _super.call(this, name, sort) || this;
        _this.otherDoorID = null;
        _this.requireSuit = false;
        _this.targetMap = null;
        _this.targetX = -1;
        _this.targetY = -1;
        _this.interacteable = true;
        return _this;
    }
    ShrdluAirlockDoor.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "otherDoorID") {
            this.otherDoorID = attribute_xml.getAttribute("value");
            if (this.otherDoorID == "null")
                this.otherDoorID = null;
            return true;
        }
        else if (a_name == "requireSuit") {
            this.requireSuit = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.requireSuit = true;
            return true;
        }
        else if (a_name == "targetMap") {
            this.targetMap = attribute_xml.getAttribute("value");
            return true;
        }
        else if (a_name == "targetX") {
            this.targetX = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "targetY") {
            this.targetY = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        return false;
    };
    ShrdluAirlockDoor.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        if (this.otherDoorID != null)
            xmlString += this.saveObjectAttributeToXML("otherDoorID", this.otherDoorID) + "\n";
        xmlString += this.saveObjectAttributeToXML("requireSuit", this.requireSuit) + "\n";
        xmlString += this.saveObjectAttributeToXML("targetMap", this.targetMap) + "\n";
        xmlString += this.saveObjectAttributeToXML("targetX", this.targetX) + "\n";
        xmlString += this.saveObjectAttributeToXML("targetY", this.targetY) + "\n";
        return xmlString;
    };
    ShrdluAirlockDoor.prototype.isEquipable = function () {
        return true;
    };
    ShrdluAirlockDoor.prototype.event = function (event_type, character, map, game) {
        var retval = _super.prototype.event.call(this, event_type, character, map, game);
        if (event_type == A4_EVENT_INTERACT) {
            if (this.otherDoorID != null) {
                var otherdoor = game.findObjectByIDJustObject(this.otherDoorID);
                if (!otherdoor.closed) {
                    var script = new A4Script(A4_SCRIPT_TALK, null, "I need to close the other airlock door first", 0, true, true);
                    character.pushScripttoExecute(script, map, game, null);
                    return false;
                }
            }
            if (this.requireSuit) {
                var suit = game.getStoryStateVariable("spacesuit");
                if (suit != "helmet") {
                    var script = new A4Script(A4_SCRIPT_TALK, null, "I cannot go through the airlock without a spacesuit!", 0, true, true);
                    character.pushScripttoExecute(script, map, game, null);
                    return false;
                }
            }
            // go to the target destination:
            var targetMap = game.getMap(this.targetMap);
            if (targetMap != null) {
                game.requestWarp(character, targetMap, this.targetX, this.targetY);
            }
            return true;
        }
        return retval;
    };
    return ShrdluAirlockDoor;
}(A4Obstacle));
;
