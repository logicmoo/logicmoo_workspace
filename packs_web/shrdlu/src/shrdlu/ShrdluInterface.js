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
var SHRDLU_FADEIN_TIME = 30;
var BShrdluFrame = /** @class */ (function (_super) {
    __extends(BShrdluFrame, _super);
    function BShrdluFrame(x, y, width, height, GLTM) {
        var _this = _super.call(this, x, y, width, height) || this;
        _this.GLTM = null;
        _this.tile_tl = null;
        _this.tile_tr = null;
        _this.tile_bl = null;
        _this.tile_br = null;
        _this.tile_h = null;
        _this.tile_v = null;
        _this.x = Math.floor(x - (x % (PIXEL_SIZE * 8)));
        _this.y = Math.floor(y - (y % (PIXEL_SIZE * 8)));
        _this.width = Math.floor(width - (width % (PIXEL_SIZE * 8)));
        _this.height = Math.floor(height - (height % (PIXEL_SIZE * 8)));
        _this.GLTM = GLTM;
        return _this;
    }
    BShrdluFrame.prototype.drawAlpha = function (alpha) {
        // background:
        var color = generateRGBColor(40, 40, 40);
        ctx.fillStyle = color;
        ctx.fillRect(this.x + PIXEL_SIZE * 4, this.y + PIXEL_SIZE * 4, this.width - PIXEL_SIZE * 8, this.height - PIXEL_SIZE * 8);
        if (this.tile_tl == null) {
            this.tile_tl = this.GLTM.getPiece("data/GUI.png", 32, 0, 8, 8);
            this.tile_tr = this.GLTM.getPiece("data/GUI.png", 40, 0, 8, 8);
            this.tile_bl = this.GLTM.getPiece("data/GUI.png", 32, 8, 8, 8);
            this.tile_br = this.GLTM.getPiece("data/GUI.png", 40, 8, 8, 8);
            this.tile_h = this.GLTM.getPiece("data/GUI.png", 0, 0, 8, 8);
            this.tile_v = this.GLTM.getPiece("data/GUI.png", 0, 8, 8, 8);
        }
        if (this.tile_tl != null) {
            for (var i = 0; i < this.width; i += 8 * PIXEL_SIZE) {
                if (i == 0) {
                    this.tile_tl.drawWithZoom(this.x + i, this.y, PIXEL_SIZE);
                    this.tile_bl.drawWithZoom(this.x + i, this.y + this.height - 8 * PIXEL_SIZE, PIXEL_SIZE);
                }
                else if (i == this.width - 8 * PIXEL_SIZE) {
                    this.tile_tr.drawWithZoom(this.x + i, this.y, PIXEL_SIZE);
                    this.tile_br.drawWithZoom(this.x + i, this.y + this.height - 8 * PIXEL_SIZE, PIXEL_SIZE);
                }
                else {
                    this.tile_h.drawWithZoom(this.x + i, this.y, PIXEL_SIZE);
                    this.tile_h.drawWithZoom(this.x + i, this.y + this.height - 8 * PIXEL_SIZE, PIXEL_SIZE);
                }
            }
            for (var i = 8 * PIXEL_SIZE; i < this.height - 8 * PIXEL_SIZE; i += 8 * PIXEL_SIZE) {
                this.tile_v.drawWithZoom(this.x, this.y + i, PIXEL_SIZE);
                this.tile_v.drawWithZoom(this.x + this.width - 8 * PIXEL_SIZE, this.y + i, PIXEL_SIZE);
            }
        }
    };
    return BShrdluFrame;
}(BInterfaceElement));
var BShrdluTextFrame = /** @class */ (function (_super) {
    __extends(BShrdluTextFrame, _super);
    function BShrdluTextFrame(initial_text, centered, font, fontHeight, x, y, width, height, GLTM) {
        var _this = _super.call(this, x, y, width, height, GLTM) || this;
        _this.centered = false;
        _this.font = null;
        _this.fontWidth = 6 * PIXEL_SIZE; // note: this number is hardcoded!
        _this.fontHeight = 8 * PIXEL_SIZE;
        _this.text = null;
        _this.centered = centered;
        _this.font = font;
        _this.fontHeight = fontHeight;
        _this.text = initial_text;
        return _this;
    }
    BShrdluTextFrame.prototype.drawAlpha = function (alpha) {
        _super.prototype.drawAlpha.call(this, alpha);
        var x = this.x + 8 * PIXEL_SIZE;
        var y = this.y + 8 * PIXEL_SIZE + this.fontHeight;
        var highlighted = false;
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
            var line_x = x;
            for (var i = 0; i < line.length; i++) {
                if (line[i] == '*') {
                    if (highlighted) {
                        ctx.fillStyle = "white";
                        highlighted = false;
                    }
                    else {
                        ctx.fillStyle = MSX_COLOR_LIGHT_GREEN;
                        highlighted = true;
                    }
                }
                else {
                    ctx.fillText(line[i], line_x, y);
                    line_x += this.fontWidth;
                }
            }
            y += this.fontHeight;
        }
    };
    return BShrdluTextFrame;
}(BShrdluFrame));
var BShrdluButton = /** @class */ (function (_super) {
    __extends(BShrdluButton, _super);
    function BShrdluButton(text, font, x, y, width, height, ID, color, centered, callback) {
        var _this = _super.call(this, text, font, x, y, width, height, ID, callback) || this;
        _this.color = color;
        _this.centered = centered;
        return _this;
    }
    BShrdluButton.prototype.drawAlpha = function (alpha) {
        ctx.fillStyle = MSX_COLOR_GREY;
        if (!this.enabled)
            ctx.fillStyle = MSX_COLOR_DARK_BLUE;
        switch (this.status) {
            case BBUTTON_STATE_MOUSEOVER:
                ctx.fillStyle = MSX_COLOR_WHITE;
                break;
            case BBUTTON_STATE_PRESSED:
                ctx.fillStyle = MSX_COLOR_WHITE;
                break;
            default:
        }
        ctx.font = this.font;
        ctx.textBaseline = "middle";
        if (this.centered) {
            ctx.textAlign = "center";
            ctx.fillText(this.text, this.x + this.width / 2, this.y + this.height / 2);
        }
        else {
            ctx.textAlign = "left";
            ctx.fillText(this.text, this.x, this.y + this.height / 2);
        }
    };
    return BShrdluButton;
}(BButton));
function createShrdluMenu(lines, callbacks, font, font_heigth, x, y, width, height, interline_space, starting_ID, GLTM) {
    BInterface.addElement(new BShrdluFrame(x, y, width, height, GLTM));
    var by = y + 8 * PIXEL_SIZE;
    for (var i = 0; i < lines.length; i++) {
        BInterface.addElement(new BShrdluButton(lines[i], font, x, by, width, font_heigth, starting_ID, "white", true, callbacks[i]));
        starting_ID++;
        by += font_heigth + interline_space;
    }
}
function getShrdluInstructionsString() {
    var instructions = [];
    instructions.push("");
    instructions.push(" Controls:");
    instructions.push(" - Move with ARROW KEYS");
    instructions.push(" - Type to talk");
    instructions.push(" - SPACE to take/interact");
    instructions.push(" - SPACE + ARROW KEYS to push/pull");
    instructions.push(" - TAB to toogle inventory");
    instructions.push(" - O to drop items");
    instructions.push(" - U to use items in inventory");
    instructions.push(" - PGUP/PGDOWN to navigate messages");
    instructions.push("   and the inventory");
    instructions.push(" - ESC/ENTER to skip text bubbles");
    instructions.push(" - SHIFT to speed up walking");
    instructions.push(" - ESC to pause/load/save/quit");
    instructions.push(" - You can also use the mouse");
    /*
        instructions.push("- Levers open/close doors or trigger other secrets.");
        if (this.game.allowStats) {
            instructions.push("- You might lose hit points by fighting.");
            instructions.push("- Find potions to recover hit/magic points.");
        }
        if (this.game.allowInventory) {
            instructions.push("- Remember to equip the most powerful items.");
        }
        instructions.push("- Explore methodically, you might find some clues...");
        if (this.game.allowInventory) {
            instructions.push("- To fight while on a vehicle, first disembark.");
        }
    */
    return instructions;
}
function drawFadeInOverlay(f) {
    var offset = 8 - Math.floor(8 * f);
    var squareSize = 16 - offset * 2;
    ctx.fillStyle = "black";
    for (var y = 0; y < 192; y += 16) {
        for (var x = 0; x < 256; x += 16) {
            ctx.fillRect((x + offset) * PIXEL_SIZE, (y + offset) * PIXEL_SIZE, squareSize * PIXEL_SIZE, squareSize * PIXEL_SIZE);
        }
    }
}
function generateDebugLog(game, writeLogsToServer) {
    //let newline:string = "%0a";    // we need this, if we append the text to the page at the end
    var newline = "\n";
    var tab = "\t";
    var mailContent = "SHRDLU " + SHRDLU_VERSION + " log:" + newline;
    if (writeLogsToServer) {
        mailContent += "Session: " + getIDFromSessionToken(game.serverToken) + newline + newline;
    }
    mailContent += "Please email this file to santi.ontanon@gmail.com to help improve this game!" + newline + newline;
    for (var _i = 0, _a = game.messages; _i < _a.length; _i++) {
        var m = _a[_i];
        mailContent += (Number(m[2]) - SHRDLU_START_DATE) + tab + m[0] + newline;
    }
    mailContent += newline + "In-game Actions:" + newline;
    for (var _b = 0, _c = game.inGameActionsForLog; _b < _c.length; _b++) {
        var m = _c[_b];
        mailContent += (Number(m[1]) - SHRDLU_START_DATE) + tab + m[0] + newline;
    }
    mailContent += newline + "Error messages:" + newline;
    for (var _d = 0, _e = game.errorMessagesForLog; _d < _e.length; _d++) {
        var m = _e[_d];
        mailContent += (Number(m[1]) - SHRDLU_START_DATE) + tab + m[0] + newline;
    }
    return mailContent;
}
function generateDebugLogForDownload(game) {
    var mailContent = generateDebugLog(game, false);
    /*
    // method 1: mailto
    let mail = "mailto:santi.ontanon@gmail.com?subject=SHRDLU DEMO 1 log&body=" + mailContent + "";
    let win = window.open(mail, 'emailWindow');
    if (win && win.open && !win.closed) win.close();

    // method 2: append to the page
    document.getElementById("log").innerHTML = mailContent.split("%0a").join("<br>");
    */
    // method 3: downloadable file
    downloadStringAsFile(mailContent, "debug-log.txt");
}
function downloadStringAsFile(s, fileName) {
    var blob = new Blob([s], { type: 'text/csv' });
    if (window.navigator.msSaveOrOpenBlob) {
        window.navigator.msSaveOrOpenBlob(blob, fileName);
    }
    else {
        var elem = window.document.createElement('a');
        elem.href = window.URL.createObjectURL(blob);
        elem.download = fileName;
        document.body.appendChild(elem);
        elem.click();
        document.body.removeChild(elem);
    }
}
