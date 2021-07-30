function getTextTile(text, font, height, color) {
    var tmpCanvas = document.createElement("canvas");
    var tmpCtx = tmpCanvas.getContext("2d");
    tmpCtx.imageSmoothingEnabled = false;
    tmpCtx.font = font;
    tmpCanvas.width = tmpCtx.measureText(text).width;
    tmpCanvas.height = height;
    tmpCtx.font = font; // not sure why, but after changing the size, I need to set the font again
    tmpCtx.textBaseline = "middle";
    tmpCtx.textAlign = "left";
    tmpCtx.fillStyle = color;
    tmpCtx.fillText(text, 0, height / 2);
    var img = document.createElement("img");
    img.src = tmpCanvas.toDataURL();
    return img;
}
function getTextTileWithOutline(text, font, height, color, outlineColor) {
    // Generate the text tile:
    var tmpCanvas = document.createElement("canvas");
    var tmpCtx = tmpCanvas.getContext("2d");
    tmpCtx.imageSmoothingEnabled = false;
    tmpCtx.font = font;
    tmpCanvas.width = tmpCtx.measureText(text).width + 2;
    tmpCanvas.height = height + 2;
    tmpCtx.font = font; // not sure why, but after changing the size, I need to set the font again
    tmpCtx.textBaseline = "top";
    tmpCtx.textAlign = "left";
    tmpCtx.fillStyle = color;
    tmpCtx.fillText(text, 1, 1);
    // Draw an outline:
    var imageData = tmpCtx.getImageData(0, 0, tmpCanvas.width, tmpCanvas.height);
    var data = imageData.data;
    tmpCtx.fillStyle = outlineColor;
    for (var y = 1; y < tmpCanvas.height - 1; y++) {
        for (var x = 1; x < tmpCanvas.width - 1; x++) {
            var alpha = data[(x + y * tmpCanvas.width) * 4 + 3];
            if (alpha > 200) {
                if (data[((x - 1) + (y - 1) * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x - 1, y - 1, 1, 1);
                if (data[(x + (y - 1) * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x, y - 1, 1, 1);
                if (data[((x + 1) + (y - 1) * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x + 1, y - 1, 1, 1);
                if (data[((x - 1) + y * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x - 1, y, 1, 1);
                if (data[((x + 1) + y * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x + 1, y, 1, 1);
                if (data[((x - 1) + (y + 1) * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x - 1, y + 1, 1, 1);
                if (data[(x + (y + 1) * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x, y + 1, 1, 1);
                if (data[((x + 1) + (y + 1) * tmpCanvas.width) * 4 + 3] < 200)
                    tmpCtx.fillRect(x + 1, y + 1, 1, 1);
            }
        }
    }
    var img = document.createElement("img");
    img.src = tmpCanvas.toDataURL();
    return img;
}
var BInterface = /** @class */ (function () {
    function BInterface() {
    }
    BInterface.push = function () {
        //        console.log("BInterface.push");
        //        console.log(new Error().stack);
        var enabled_since_last_push = [];
        for (var _i = 0, _a = this.added_since_last_push; _i < _a.length; _i++) {
            var e = _a[_i];
            enabled_since_last_push.push(e.getEnabled());
        }
        this.stack.push(this.added_since_last_push);
        this.enabledStack.push(enabled_since_last_push);
        this.added_since_last_push = [];
        for (var _b = 0, _c = this.elements; _b < _c.length; _b++) {
            var e = _c[_b];
            e.enabled = false;
        }
        this.ignoreBeforeThisIndexStack.push(this.ignoreBeforeThisIndex);
        BInterface.highlightedByKeyboard = -1;
    };
    BInterface.pushIgnoringCurrent = function () {
        this.push();
        this.ignoreBeforeThisIndex = this.elements.length;
    };
    BInterface.pop = function () {
        //        console.log("BInterface.pop");
        //        console.log(new Error().stack);
        if (this.stack.length == 0)
            return;
        this.elements.splice(this.elements.length - this.added_since_last_push.length, this.added_since_last_push.length);
        this.added_since_last_push = this.stack.pop();
        var enabled_since_last_push = this.enabledStack.pop();
        for (var i = 0; i < this.added_since_last_push.length; i++) {
            this.added_since_last_push[i].setEnabled(enabled_since_last_push[i]);
        }
        this.ignoreBeforeThisIndex = this.ignoreBeforeThisIndexStack.pop();
        BInterface.highlightedByKeyboard = -1;
    };
    BInterface.addElement = function (b) {
        this.elements.push(b);
        this.added_since_last_push.push(b);
        BInterface.highlightedByKeyboard = -1;
    };
    BInterface.getElementByID = function (ID) {
        for (var _i = 0, _a = this.elements; _i < _a.length; _i++) {
            var e = _a[_i];
            if (e.ID == ID)
                return e;
        }
        return null;
    };
    BInterface.reset = function () {
        this.elements = [];
        this.added_since_last_push = [];
        this.stack = [];
        this.enabledStack = [];
        BInterface.highlightedByKeyboard = -1;
    };
    BInterface.mouseOverElement = function (mouse_x, mouse_y) {
        for (var i = this.ignoreBeforeThisIndex; i < this.elements.length; i++) {
            var e = this.elements[i];
            if (e.getEnabled() && e.mouseOver(mouse_x, mouse_y))
                return true;
        }
        return false;
    };
    BInterface.update = function (mouse_x, mouse_y, k, arg) {
        var modal = null;
        var to_delete = [];
        if (BInterface.elements.length > 0) {
            if (k.key_press(KEY_CODE_DOWN)) {
                var maxCycles = BInterface.elements.length;
                do {
                    BInterface.highlightedByKeyboard++;
                    if (BInterface.highlightedByKeyboard < this.ignoreBeforeThisIndex)
                        BInterface.highlightedByKeyboard = this.ignoreBeforeThisIndex;
                    if (BInterface.highlightedByKeyboard >= BInterface.elements.length)
                        BInterface.highlightedByKeyboard = this.ignoreBeforeThisIndex;
                    if (BInterface.elements[BInterface.highlightedByKeyboard].enabled &&
                        BInterface.elements[BInterface.highlightedByKeyboard].active &&
                        BInterface.elements[BInterface.highlightedByKeyboard] instanceof BButton)
                        break;
                    maxCycles--;
                } while (maxCycles > 0);
            }
            if (k.key_press(KEY_CODE_UP)) {
                var maxCycles = BInterface.elements.length;
                do {
                    BInterface.highlightedByKeyboard--;
                    if (BInterface.highlightedByKeyboard < this.ignoreBeforeThisIndex)
                        BInterface.highlightedByKeyboard = BInterface.elements.length - 1;
                    if (BInterface.elements[BInterface.highlightedByKeyboard].enabled &&
                        BInterface.elements[BInterface.highlightedByKeyboard].active &&
                        BInterface.elements[BInterface.highlightedByKeyboard] instanceof BButton)
                        break;
                    maxCycles--;
                } while (maxCycles > 0);
            }
        }
        else {
            BInterface.highlightedByKeyboard = -1;
        }
        for (var _i = 0, _a = this.elements; _i < _a.length; _i++) {
            var e = _a[_i];
            if (e.modal && e.active && e.enabled) {
                modal = e;
                break;
            }
        }
        if (modal != null) {
            modal.update(mouse_x, mouse_y, k, arg);
            if (modal.to_be_deleted)
                to_delete.push(modal);
        }
        else {
            for (var i = this.ignoreBeforeThisIndex; i < this.elements.length; i++) {
                var e = this.elements[i];
                e.update(mouse_x, mouse_y, k, arg);
                if (e.to_be_deleted)
                    to_delete.push(e);
            }
        } // if
        for (var _b = 0, to_delete_1 = to_delete; _b < to_delete_1.length; _b++) {
            var e = to_delete_1[_b];
            var idx = this.elements.indexOf(e);
            this.elements.splice(idx, 1);
        } // while
    };
    BInterface.mouseClick = function (mouse_x, mouse_y, button, arg) {
        // we need this intermediate list, just in case mouseclick calls cause the creation of more elements
        var l = [];
        for (var i = this.ignoreBeforeThisIndex; i < this.elements.length; i++) {
            var e = this.elements[i];
            if (e.getEnabled() && e.mouseOver(mouse_x, mouse_y))
                l.push(e);
        }
        for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
            var e = l_1[_i];
            e.mouseClick(mouse_x, mouse_y, button, arg);
        }
    };
    BInterface.mouseMove = function (mouse_x, mouse_y) {
        BInterface.highlightedByKeyboard = -1;
    };
    BInterface.draw = function () {
        BInterface.drawAlpha(1.0);
    };
    BInterface.drawAlpha = function (alpha) {
        for (var i = this.ignoreBeforeThisIndex; i < this.elements.length; i++) {
            var e = this.elements[i];
            e.drawAlpha(alpha);
        }
    };
    BInterface.createMenu = function (lines, callbacks, font, font_heigth, x, y, width, height, interline_space, starting_ID) {
        BInterface.addElement(new BFrame(x, y, width, height));
        var by = y + 10;
        for (var i = 0; i < lines.length; i++) {
            BInterface.addElement(new BButtonTransparent(lines[i], font, x, by, width, font_heigth, starting_ID, "white", callbacks[i]));
            starting_ID++;
            by += font_heigth + interline_space;
        }
    };
    BInterface.disable = function (ID) {
        for (var _i = 0, _a = BInterface.elements; _i < _a.length; _i++) {
            var e = _a[_i];
            if (e.getID() == ID)
                e.setEnabled(false);
        }
    };
    BInterface.elements = [];
    BInterface.added_since_last_push = [];
    BInterface.stack = [];
    BInterface.enabledStack = [];
    BInterface.ignoreBeforeThisIndexStack = [];
    BInterface.ignoreBeforeThisIndex = 0;
    BInterface.highlightedByKeyboard = -1;
    return BInterface;
}());
