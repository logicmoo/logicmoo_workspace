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
var A4Key = /** @class */ (function (_super) {
    __extends(A4Key, _super);
    function A4Key(name, sort, id, a1) {
        var _this = _super.call(this, name, sort) || this;
        _this.keyID = id;
        _this.animations[A4_ANIMATION_IDLE] = a1;
        return _this;
    }
    A4Key.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "keyID") {
            this.keyID = attribute_xml.getAttribute("value");
            return true;
        }
        return false;
    };
    A4Key.prototype.isKey = function () {
        return true;
    };
    A4Key.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("keyID", this.keyID) + "\n";
        return xmlString;
    };
    return A4Key;
}(A4Item));
