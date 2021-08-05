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
var A4PlayerCharacter = /** @class */ (function (_super) {
    __extends(A4PlayerCharacter, _super);
    function A4PlayerCharacter(name, sort) {
        var _this = _super.call(this, name, sort) || this;
        _this.selectedItem = -1;
        return _this;
    }
    A4PlayerCharacter.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        // This function is just so that we can reuse object class definitions between Players and AI characters.
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "sightRadius") {
            return true;
        }
        else if (a_name == "respawn") {
            return true;
        }
        else if (a_name == "AI.period") {
            return true;
        }
        else if (a_name == "AI.cycle") {
            return true;
        }
        else if (a_name == "respawnRecordID") {
            return true;
        }
        return false;
    };
    A4PlayerCharacter.prototype.isPlayer = function () {
        return true;
    };
    A4PlayerCharacter.prototype.nextItem = function () {
        for (var i = 0; i < this.inventory.length; i++) {
            this.selectedItem++;
            this.selectedItem = this.selectedItem % this.inventory.length;
            if (this.inventory[this.selectedItem] != null)
                return;
        }
        this.selectedItem = -1;
    };
    A4PlayerCharacter.prototype.previousItem = function () {
        for (var i = 0; i < this.inventory.length; i++) {
            this.selectedItem--;
            if (this.selectedItem < 0)
                this.selectedItem = this.inventory.length - 1;
            if (this.inventory[this.selectedItem] != null)
                return;
        }
        this.selectedItem = -1;
    };
    return A4PlayerCharacter;
}(A4AICharacter));
