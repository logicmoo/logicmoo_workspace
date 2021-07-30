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
var BTextInputFrame = /** @class */ (function (_super) {
    __extends(BTextInputFrame, _super);
    function BTextInputFrame(question, initial_text, max_characters, font, fontWidth, fontHeight, x, y, dx, dy, ID, callback) {
        var _this = _super.call(this, x, y, dx, dy) || this;
        _this.editing = "";
        _this.focus = false;
        _this.cycle = 0;
        _this.change_in_last_cycle = false;
        _this.fontHeight = 8;
        _this.question = question;
        if (initial_text != null)
            _this.editing += initial_text;
        _this.editing_position = _this.editing.length;
        _this.font = font;
        _this.max_characters = max_characters;
        _this.ID = ID;
        _this.callback = callback;
        _this.active = true;
        _this.cursor_dx = 4;
        _this.cursor_dy = fontHeight;
        _this.fontWidth = fontWidth;
        _this.fontHeight = fontHeight;
        return _this;
    }
    BTextInputFrame.prototype.update = function (mouse_x, mouse_y, k, arg) {
        this.cycle++;
        this.change_in_last_cycle = false;
        if (this.enabled) {
            if (this.focus) {
                var pep = this.editing_position;
                var el = this.editing.length;
                this.string_editor_cycle(k, arg);
                if (this.editing_position != pep || this.editing.length != el) {
                    this.change_in_last_cycle = true;
                }
                if (k.key_press(KEY_CODE_RETURN)) {
                    return true;
                }
            } // if 
        } // if
        return false;
    };
    BTextInputFrame.prototype.mouseClick = function (mousex, mousey, button, arg) {
        if (mousex >= this.x && mousex < this.x + this.width &&
            mousey >= this.y && mousey < this.y + this.height)
            this.focus = true;
        else
            this.focus = false;
    };
    BTextInputFrame.prototype.drawAlpha = function (alpha) {
        // draw frame:
        _super.prototype.drawAlpha.call(this, alpha);
        // draw question:
        var x = this.x + this.width / 2;
        var y = this.y + 10 + this.fontHeight;
        ctx.fillStyle = "white";
        ctx.font = this.font;
        ctx.textBaseline = "bottom";
        if (this.question != null) {
            ctx.textAlign = "center";
            ctx.fillText(this.question, x, y);
            y += this.fontHeight + 4;
        }
        // draw editing text:
        x = this.x + 8;
        ctx.textAlign = "left";
        ctx.fillText(this.editing, x, y);
        // draw cursor:
        var tdx = this.fontWidth * this.editing_position;
        var cursor_x = this.x + 8 + tdx;
        var cursor_y = y - this.fontHeight;
        ctx.fillStyle = generateRGBColor(Math.floor(140 + 80 * Math.sin(this.cycle * 0.3)), 0, 0);
        ctx.fillRect(cursor_x, cursor_y, this.cursor_dx, this.cursor_dy);
    };
    BTextInputFrame.prototype.string_editor_cycle = function (k, arg) {
        for (var _i = 0, _a = k.keyevents; _i < _a.length; _i++) {
            var ke = _a[_i];
            if (ke.keyCode == KEY_CODE_BACKSPACE) {
                if (this.editing_position > 0) {
                    this.editing = this.editing.substring(0, this.editing_position - 1) +
                        this.editing.substring(this.editing_position);
                    this.editing_position--;
                } // if
            } // if
            if (ke.keyCode == KEY_CODE_DELETE) {
                if (this.editing_position < this.editing.length) {
                    this.editing = this.editing.substring(0, this.editing_position) +
                        this.editing.substring(this.editing_position + 1);
                } // if
            } // if
            if (ke.keyCode == KEY_CODE_LEFT) {
                if (this.editing_position > 0) {
                    this.editing_position--;
                } // if
            } // if
            if (ke.keyCode == KEY_CODE_RIGHT) {
                if (this.editing_position < this.editing.length) {
                    this.editing_position++;
                } // if
            } // if
            if (ke.keyCode == KEY_CODE_HOME)
                this.editing_position = 0;
            if (ke.keyCode == KEY_CODE_END)
                this.editing_position = this.editing.length;
            if ((ke.keyCode >= KEY_CODE_0 &&
                ke.keyCode <= KEY_CODE_9) ||
                (ke.keyCode >= KEY_CODE_A &&
                    ke.keyCode <= KEY_CODE_Z)) {
                if (this.editing.length < this.max_characters) {
                    if (this.editing_position < this.editing.length) {
                        this.editing = this.editing.substring(0, this.editing_position) +
                            KEYCODE_NAMES[ke.keyCode] +
                            this.editing.substring(this.editing_position);
                        this.editing_position++;
                    }
                    else {
                        this.editing = this.editing + KEYCODE_NAMES[ke.keyCode];
                        this.editing_position++;
                    }
                }
            }
            if (ke.keyCode == KEY_CODE_RETURN) {
                // callback!
                this.callback(arg, this.ID);
            }
        } // while
        k.keyevents = [];
    };
    return BTextInputFrame;
}(BFrame));
