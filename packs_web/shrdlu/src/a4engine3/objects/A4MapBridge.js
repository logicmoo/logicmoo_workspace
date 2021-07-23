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
var A4MapBridge = /** @class */ (function (_super) {
    __extends(A4MapBridge, _super);
    function A4MapBridge(bridge_xml, map) {
        var _this = _super.call(this, null, null) || this;
        _this.linkedTo = null;
        _this.name = bridge_xml.getAttribute("name");
        _this.x = Number(bridge_xml.getAttribute("x"));
        _this.y = Number(bridge_xml.getAttribute("y"));
        _this.width = Number(bridge_xml.getAttribute("width"));
        _this.height = Number(bridge_xml.getAttribute("height"));
        _this.appearDirection = A4_DIRECTION_NONE;
        _this.appearWalking = false;
        _this.linkedTo = null;
        _this.map = map;
        return _this;
    }
    A4MapBridge.prototype.link = function (b) {
        this.linkedTo = b;
        b.linkedTo = this;
    };
    A4MapBridge.prototype.findAvailableTargetLocation = function (o, tile_dx, tile_dy) {
        var best_x, best_y;
        var best_d = null;
        for (var i = 0; i <= this.height - o.getPixelHeight(); i += tile_dy) {
            for (var j = 0; j <= this.width - o.getPixelWidth(); j += tile_dx) {
                //                if (this.map.walkable(this.x+j, this.y+i, o.getPixelWidth(), o.getPixelHeight(), o)) {
                if (this.y + i >= 0) {
                    if (this.map.walkable(this.x + j, this.y + i, o.getPixelWidth(), o.getPixelHeight(), o)) {
                        var d_1 = Math.abs((j + o.getPixelWidth() / 2) - this.width / 2) +
                            Math.abs((i + o.getPixelHeight() / 2) - this.height / 2);
                        if (best_d == null || d_1 < best_d) {
                            best_d = d_1;
                            best_x = j;
                            best_y = i;
                        }
                    }
                }
            }
        }
        if (best_d != null) {
            return [this.x + best_x, this.y + best_y];
        }
        return null;
    };
    A4MapBridge.prototype.getPixelWidth = function () {
        return this.width;
    };
    A4MapBridge.prototype.getPixelHeight = function () {
        return this.height;
    };
    return A4MapBridge;
}(A4Object));
