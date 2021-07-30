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
var A4WalkingObject = /** @class */ (function (_super) {
    __extends(A4WalkingObject, _super);
    function A4WalkingObject(name, sort) {
        var _this = _super.call(this, name, sort) || this;
        // attributes:
        _this.walkSpeed = 16;
        _this.previousDirection = A4_DIRECTION_NONE;
        _this.state = A4CHARACTER_STATE_IDLE;
        _this.previousState = A4CHARACTER_STATE_NONE;
        _this.stateCycle = 0;
        // walking temporary counters (to make sure characters walk at the desired speed):
        _this.walkingCounter = 0;
        // some variables to make moving the character around intuitive:
        _this.continuous_direction_command_timers = new Array(A4_NDIRECTIONS);
        _this.continuous_direction_command_max_movement = new Array(A4_NDIRECTIONS); // a command might specify a direction and a maximum amount of pixels to move in that direction
        _this.direction_command_received_this_cycle = new Array(A4_NDIRECTIONS);
        _this.currentAnimation = A4_ANIMATION_IDLE_RIGHT;
        _this.direction = A4_DIRECTION_RIGHT;
        for (var i = 0; i < A4_NDIRECTIONS; i++) {
            _this.continuous_direction_command_timers[i] = 0;
            _this.continuous_direction_command_max_movement[i] = 0;
            _this.direction_command_received_this_cycle[i] = false;
        }
        return _this;
    }
    A4WalkingObject.prototype.checkIfPushingAgainstMapEdgeBridge = function (direction) {
        if (direction == A4_DIRECTION_LEFT && this.x <= 0) {
            var bridge = this.map.getBridge(1, this.y + this.getPixelHeight() / 2);
            return bridge;
        }
        else if (direction == A4_DIRECTION_RIGHT && this.x >= (this.map.width * this.map.tileWidth - this.getPixelWidth())) {
            var bridge = this.map.getBridge(this.map.width * this.map.tileWidth - 1, this.y + this.getPixelHeight() / 2);
            return bridge;
        }
        else if (direction == A4_DIRECTION_UP && this.y <= 0) {
            var bridge = this.map.getBridge(this.x + this.getPixelWidth() / 2, 1);
            return bridge;
        }
        else if (direction == A4_DIRECTION_DOWN && this.y >= (this.map.height * this.map.tileHeight - this.getPixelHeight())) {
            var bridge = this.map.getBridge(this.x + this.getPixelWidth() / 2, this.map.height * this.map.tileHeight - 1);
            return bridge;
        }
        return null;
    };
    A4WalkingObject.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "walk_speed") {
            this.walkSpeed = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "previous_direction") {
            this.previousDirection = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "state") {
            this.state = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "previous_state") {
            this.previousState = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "state_cycle") {
            this.stateCycle = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        return false;
    };
    A4WalkingObject.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("previous_direction", this.previousDirection) + "\n";
        xmlString += this.saveObjectAttributeToXML("state", this.state) + "\n";
        xmlString += this.saveObjectAttributeToXML("previous_state", this.previousState) + "\n";
        xmlString += this.saveObjectAttributeToXML("state_cycle", this.stateCycle) + "\n";
        xmlString += this.saveObjectAttributeToXML("walk_speed", this.walkSpeed) + "\n";
        return xmlString;
    };
    // I need a function for this, since items can change it!
    A4WalkingObject.prototype.getWalkSpeed = function () {
        return this.walkSpeed;
    };
    return A4WalkingObject;
}(A4Object));
