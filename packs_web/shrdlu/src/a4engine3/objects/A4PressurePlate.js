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
var TRIGGER_PRESSURE_ITEM = 1;
var TRIGGER_PRESSURE_HEAVY_ITEM = 2;
var TRIGGER_PRESSURE_PLAYER = 3;
var A4PressurePlate = /** @class */ (function (_super) {
    __extends(A4PressurePlate, _super);
    function A4PressurePlate(sort, pressed, released, pr) {
        var _this = _super.call(this, "pressure-plate", sort) || this;
        _this.pressurePlateState = false;
        _this.animations[A4_ANIMATION_CLOSED] = pressed;
        _this.animations[A4_ANIMATION_OPEN] = released;
        _this.pressureRequired = pr;
        if (_this.pressurePlateState)
            _this.currentAnimation = A4_ANIMATION_CLOSED;
        else
            _this.currentAnimation = A4_ANIMATION_OPEN;
        return _this;
    }
    A4PressurePlate.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "pressurePlateState") {
            this.pressurePlateState = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.pressurePlateState = true;
            return true;
        }
        else if (a_name == "pressureRequired") {
            this.pressureRequired = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        return false;
    };
    A4PressurePlate.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("pressureRequired", this.pressureRequired) + "\n";
        xmlString += this.saveObjectAttributeToXML("pressurePlateState", this.pressurePlateState) + "\n";
        return xmlString;
    };
    A4PressurePlate.prototype.update = function (game) {
        _super.prototype.update.call(this, game);
        var l = this.map.getAllObjectCollisions(this);
        var heaviest = null;
        var pressure = 0;
        for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
            var o = l_1[_i];
            if (pressure < TRIGGER_PRESSURE_ITEM) {
                pressure = TRIGGER_PRESSURE_ITEM; // if there is an object, at least there is pressure 1
                heaviest = o;
            }
            if (pressure < TRIGGER_PRESSURE_HEAVY_ITEM && o.isHeavy()) {
                pressure = TRIGGER_PRESSURE_HEAVY_ITEM;
                heaviest = o;
            }
            if (pressure < TRIGGER_PRESSURE_PLAYER && o.isPlayer()) {
                pressure = TRIGGER_PRESSURE_PLAYER;
                heaviest = o;
            }
        }
        if (this.pressurePlateState) {
            if (pressure >= this.pressureRequired) {
                // nothing to do, keep pressed
            }
            else {
                // release
                this.pressurePlateState = false;
                this.event(A4_EVENT_DEACTIVATE, null, this.map, game);
                this.event(A4_EVENT_USE, null, this.map, game);
                this.currentAnimation = A4_ANIMATION_OPEN;
            }
        }
        else {
            if (heaviest != null && pressure >= this.pressureRequired) {
                // press!
                this.pressurePlateState = true;
                if (heaviest.isPlayer()) {
                    this.event(A4_EVENT_ACTIVATE, heaviest, this.map, game);
                    this.event(A4_EVENT_USE, heaviest, this.map, game);
                }
                else {
                    this.event(A4_EVENT_ACTIVATE, null, this.map, game);
                    this.event(A4_EVENT_USE, null, this.map, game);
                }
                this.currentAnimation = A4_ANIMATION_CLOSED;
            }
            else {
                // nothing to do, keep released
            }
        }
        return true;
    };
    return A4PressurePlate;
}(A4Object));
