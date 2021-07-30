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
var BFrame = /** @class */ (function (_super) {
    __extends(BFrame, _super);
    function BFrame(x, y, width, height) {
        return _super.call(this, x, y, width, height) || this;
    }
    BFrame.prototype.drawAlpha = function (alpha) {
        ctx.save();
        // background:
        var color = generateRGBColor(40, 40, 40);
        ctx.globalAlpha = 0.8 * alpha;
        ctx.fillStyle = color;
        ctx.fillRect(this.x + 2, this.y, this.width - 4, this.height);
        // top bar:
        ctx.globalAlpha = alpha;
        ctx.fillStyle = "white";
        ctx.fillRect(this.x, this.y, this.width, 6);
        // bottom bar:
        ctx.fillRect(this.x, this.y + this.height - 6, this.width, 6);
        ctx.restore();
    };
    return BFrame;
}(BInterfaceElement));
var BTextFrame = /** @class */ (function (_super) {
    __extends(BTextFrame, _super);
    function BTextFrame(initial_text, centered, font, fontHeight, x, y, width, height) {
        var _this = _super.call(this, x, y, width, height) || this;
        _this.centered = false;
        _this.font = null;
        _this.fontHeight = 8;
        _this.text = null;
        _this.centered = centered;
        _this.font = font;
        _this.fontHeight = fontHeight;
        _this.text = initial_text;
        return _this;
    }
    BTextFrame.prototype.drawAlpha = function (alpha) {
        _super.prototype.drawAlpha.call(this, alpha);
        var x = this.x + 10;
        var y = this.y + 10 + this.fontHeight;
        if (this.centered) {
            x = this.x + this.width / 2;
            ctx.textAlign = "center";
        }
        else {
            ctx.textAlign = "left";
        }
        ctx.fillStyle = "white";
        ctx.font = this.font;
        ctx.textBaseline = "bottom";
        for (var _i = 0, _a = this.text; _i < _a.length; _i++) {
            var line = _a[_i];
            ctx.fillText(line, x, y);
            y += this.fontHeight + 4;
        }
    };
    return BTextFrame;
}(BFrame));
