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
var A4Spade = /** @class */ (function (_super) {
    __extends(A4Spade, _super);
    function A4Spade(sort, a, gold) {
        var _this = _super.call(this, "Spade", sort) || this;
        _this.animations[A4_ANIMATION_IDLE] = a;
        _this.gold = gold;
        _this.usable = true;
        return _this;
    }
    A4Spade.prototype.event = function (a_event, otherCharacter, map, game) {
        var retval = _super.prototype.event.call(this, a_event, otherCharacter, map, game);
        if (a_event == A4_EVENT_USE) {
            var o = map.getBurrowedObject(otherCharacter.x, otherCharacter.y, otherCharacter.getPixelWidth(), otherCharacter.getPixelHeight());
            if (o == null) {
                game.addMessage("Nothing to dig here...");
            }
            else {
                o.burrowed = false;
                return true;
            }
        }
        return retval;
    };
    return A4Spade;
}(A4Item));
