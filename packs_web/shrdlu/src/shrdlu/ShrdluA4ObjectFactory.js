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
var ShrdluA4ObjectFactory = /** @class */ (function (_super) {
    __extends(ShrdluA4ObjectFactory, _super);
    function ShrdluA4ObjectFactory() {
        var _this = _super.call(this) || this;
        _this.baseClasses.push("ShrdluAirlockDoor");
        return _this;
    }
    ShrdluA4ObjectFactory.prototype.createObjectFromBaseClass = function (baseClassName, s, o_name, isPlayer, dead) {
        if (baseClassName == "ShrdluAirlockDoor") {
            return new ShrdluAirlockDoor(o_name, s);
        }
        return _super.prototype.createObjectFromBaseClass.call(this, baseClassName, s, o_name, isPlayer, dead);
    };
    return ShrdluA4ObjectFactory;
}(A4ObjectFactory));
;
