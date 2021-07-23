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
var A4Lever = /** @class */ (function (_super) {
    __extends(A4Lever, _super);
    function A4Lever(sort, ID, state, a_closed, a_open) {
        var _this = _super.call(this, "lever", sort) || this;
        _this.leverID = ID;
        _this.leverState = state;
        _this.animations[A4_ANIMATION_CLOSED] = a_closed;
        _this.animations[A4_ANIMATION_OPEN] = a_open;
        if (_this.leverState)
            _this.currentAnimation = A4_ANIMATION_CLOSED;
        else
            _this.currentAnimation = A4_ANIMATION_OPEN;
        _this.usable = true;
        return _this;
    }
    A4Lever.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "leverID") {
            this.leverID = attribute_xml.getAttribute("value");
            return true;
        }
        else if (a_name == "leverState") {
            this.leverState = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.leverState = true;
            if (this.leverState)
                this.currentAnimation = A4_ANIMATION_CLOSED;
            else
                this.currentAnimation = A4_ANIMATION_OPEN;
            return true;
        }
        return false;
    };
    A4Lever.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("leverID", this.leverID) + "\n";
        xmlString += this.saveObjectAttributeToXML("leverState", this.leverState) + "\n";
        return xmlString;
    };
    A4Lever.prototype.event = function (event_type, character, map, game) {
        var retval = _super.prototype.event.call(this, event_type, character, map, game);
        if (event_type == A4_EVENT_USE) {
            var s = new A4Script(A4_SCRIPT_OPENDOORS, this.leverID, null, 0, false, false);
            s.execute(this, map, game, character);
            this.leverState = (this.leverState ? false : true);
            if (this.leverState) {
                this.event(A4_EVENT_ACTIVATE, character, this.map, game);
            }
            else {
                this.event(A4_EVENT_DEACTIVATE, character, this.map, game);
            }
            if (this.leverState)
                this.currentAnimation = A4_ANIMATION_CLOSED;
            else
                this.currentAnimation = A4_ANIMATION_OPEN;
            return true;
        }
        return retval;
    };
    return A4Lever;
}(A4Object));
