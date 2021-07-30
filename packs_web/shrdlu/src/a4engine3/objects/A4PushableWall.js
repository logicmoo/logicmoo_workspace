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
var A4PushableWall = /** @class */ (function (_super) {
    __extends(A4PushableWall, _super);
    function A4PushableWall(name, sort, a) {
        var _this = _super.call(this, name, sort) || this;
        _this.weight = 1;
        _this.animations[A4_ANIMATION_IDLE] = a;
        return _this;
    }
    A4PushableWall.prototype.isWalkable = function () {
        return false;
    };
    A4PushableWall.prototype.isPushable = function () {
        return true;
    };
    A4PushableWall.prototype.event = function (a_event, character, map, game) {
        var retval = _super.prototype.event.call(this, a_event, character, map, game);
        if (a_event == A4_EVENT_PUSH &&
            character.canMoveIgnoringObject(character.direction, true, this) &&
            this.canMoveIgnoringObject(character.direction, true, character)) {
            var d_1 = character.direction;
            this.x += direction_x_inc[d_1] * map.tileWidth;
            this.y += direction_y_inc[d_1] * map.tileHeight;
            if (character != null)
                map.reevaluateVisibilityRequest();
            return true;
        }
        return retval;
    };
    A4PushableWall.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "weight") {
            this.weight = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        return false;
    };
    A4PushableWall.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("weight", this.weight) + "\n";
        return xmlString;
    };
    return A4PushableWall;
}(A4Object));
