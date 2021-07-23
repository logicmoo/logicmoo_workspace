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
var A4Item = /** @class */ (function (_super) {
    __extends(A4Item, _super);
    function A4Item(name, sort) {
        var _this = _super.call(this, name, sort) || this;
        _this.useUponTake = false;
        _this.droppable = true;
        _this.weight = 1;
        _this.takeable = true;
        _this.burrowed = false;
        return _this;
    }
    A4Item.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "useUponTake") {
            this.useUponTake = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.useUponTake = true;
            return true;
        }
        else if (a_name == "droppable") {
            this.droppable = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.droppable = true;
            return true;
        }
        else if (a_name == "weight") {
            this.weight = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        return false;
    };
    A4Item.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("useUponTake", this.useUponTake) + "\n";
        xmlString += this.saveObjectAttributeToXML("droppable", this.droppable) + "\n";
        xmlString += this.saveObjectAttributeToXML("weight", this.weight) + "\n";
        return xmlString;
    };
    return A4Item;
}(A4Object));
;
