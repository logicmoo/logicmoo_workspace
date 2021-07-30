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
var BBUTTON_STATE_NORMAL = 0;
var BBUTTON_STATE_MOUSEOVER = 1;
var BBUTTON_STATE_PRESSED = 2;
var BButton = /** @class */ (function (_super) {
    __extends(BButton, _super);
    function BButton(text, font, x, y, width, height, ID, callback) {
        var _this = _super.call(this, x, y, width, height) || this;
        _this.text = null;
        _this.font = null;
        _this.status = BBUTTON_STATE_NORMAL;
        _this.cycle = 0;
        _this.text = text;
        _this.font = font;
        _this.ID = ID;
        _this.callback = callback;
        _this.active = true;
        return _this;
    }
    BButton.prototype.setText = function (text) {
        this.text = text;
    };
    BButton.prototype.update = function (mouse_x, mouse_y, k, arg) {
        this.cycle++;
        this.status = BBUTTON_STATE_NORMAL;
        if (!this.enabled)
            return false;
        if (this.highlighted(mouse_x, mouse_y)) {
            this.status = BBUTTON_STATE_MOUSEOVER;
            if (k.key_press(KEY_CODE_RETURN) ||
                k.key_press(KEY_CODE_SPACE)) {
                if (this.callback != null)
                    this.callback(arg, this.ID);
            }
        }
        return false;
    };
    BButton.prototype.mouseClick = function (mouse_x, mouse_y, button, arg) {
        if (this.callback != null)
            this.callback(arg, this.ID);
    };
    BButton.prototype.drawAlpha = function (alpha) {
        ctx.save();
        if (!this.enabled)
            alpha /= 3;
        ctx.globalAlpha = alpha;
        switch (this.status) {
            case BBUTTON_STATE_MOUSEOVER:
                ctx.fillStyle = generateRGBColor(80, 80, 160);
                break;
            case BBUTTON_STATE_PRESSED:
                ctx.fillStyle = generateRGBColor(160, 160, 224);
                break;
            default:
                ctx.fillStyle = generateRGBColor(40, 40, 80);
        }
        ctx.fillRect(this.x, this.y, this.width, this.height);
        ctx.fillStyle = "white";
        ctx.font = this.font;
        ctx.textBaseline = "middle";
        ctx.textAlign = "center";
        ctx.fillText(this.text, this.x + this.width / 2, this.y + this.height / 2);
        ctx.restore();
    };
    return BButton;
}(BInterfaceElement));
var BButtonTransparent = /** @class */ (function (_super) {
    __extends(BButtonTransparent, _super);
    function BButtonTransparent(text, font, x, y, width, height, ID, color, callback) {
        var _this = _super.call(this, text, font, x, y, width, height, ID, callback) || this;
        _this.color = color;
        return _this;
    }
    BButtonTransparent.prototype.drawAlpha = function (alpha) {
        ctx.save();
        if (!this.enabled)
            alpha /= 3;
        var f = (0.5 + 0.3 * Math.sin((this.cycle) * 0.3));
        ctx.fillStyle = generateRGBColor(192, 192, 192);
        ctx.globalAlpha = alpha;
        switch (this.status) {
            case BBUTTON_STATE_MOUSEOVER:
                ctx.fillStyle = generateRGBColor(f, f, f);
                ctx.globalAlpha = alpha * f;
                break;
            case BBUTTON_STATE_PRESSED:
                ctx.fillStyle = this.color;
                break;
            default:
        }
        ctx.font = this.font;
        ctx.textBaseline = "middle";
        ctx.textAlign = "center";
        ctx.fillText(this.text, this.x + this.width / 2, this.y + this.height / 2);
        ctx.restore();
    };
    return BButtonTransparent;
}(BButton));
