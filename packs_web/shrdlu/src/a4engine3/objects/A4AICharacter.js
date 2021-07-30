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
var A4AICharacter = /** @class */ (function (_super) {
    __extends(A4AICharacter, _super);
    function A4AICharacter(name, sort) {
        var _this = _super.call(this, name, sort) || this;
        _this.AI = null;
        _this.AI = new A4PathFinding(_this);
        return _this;
    }
    A4AICharacter.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "AI.sightRadius" || a_name == "sightRadius") {
            this.AI.sightRadius = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "AI.period") {
            this.AI.period = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "AI.cycle") {
            this.AI.cycle = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        return false;
    };
    A4AICharacter.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("AI.sightRadius", this.AI.sightRadius) + "\n";
        xmlString += this.saveObjectAttributeToXML("AI.period", this.AI.period) + "\n";
        xmlString += this.saveObjectAttributeToXML("AI.cycle", this.AI.cycle) + "\n";
        var tagOpen = false;
        for (var _i = 0, _a = this.AI.maps_familiar_with; _i < _a.length; _i++) {
            var map_name = _a[_i];
            if (!tagOpen) {
                xmlString += "<onStart>\n";
                tagOpen = true;
            }
            xmlString += "<familiarWithMap map=\"" + map_name + "\"/>\n";
        }
        if (tagOpen)
            xmlString += "</onStart>\n";
        return xmlString;
    };
    A4AICharacter.prototype.update = function (game) {
        if (!_super.prototype.update.call(this, game))
            return false;
        if (this.map != null)
            this.AI.update(game);
        return true;
    };
    A4AICharacter.prototype.isAICharacter = function () {
        return true;
    };
    A4AICharacter.prototype.objectRemoved = function (o) {
        _super.prototype.objectRemoved.call(this, o);
        this.AI.objectRemoved(o);
    };
    return A4AICharacter;
}(A4Character));
