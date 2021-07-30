function generateRGBColor(r, g, b) {
    return "rgb(" + r + "," + g + "," + b + ")";
}
function fillTextTopLeft(text, x, y, font, color) {
    ctx.fillStyle = color;
    ctx.font = font;
    ctx.textBaseline = "top";
    ctx.textAlign = "left";
    ctx.fillText(text, x, y);
}
function fillTextTopCenter(text, x, y, font, color) {
    ctx.fillStyle = color;
    ctx.font = font;
    ctx.textBaseline = "top";
    ctx.textAlign = "center";
    ctx.fillText(text, x, y);
}
function fillTextTopRight(text, x, y, font, color) {
    ctx.fillStyle = color;
    ctx.font = font;
    ctx.textBaseline = "top";
    ctx.textAlign = "right";
    ctx.fillText(text, x, y);
}
// Code adapted from here: http://fabiensanglard.net/fizzlefade/index.php
var FizzleFade = /** @class */ (function () {
    function FizzleFade(w, h) {
        this.rndval = 1;
        this.width = 256;
        this.height = 192;
        this.width = w;
        this.height = h;
    }
    FizzleFade.prototype.done = function () {
        return this.rndval == 1;
    };
    FizzleFade.prototype.nextPixelToFizzle = function () {
        do {
            var y = this.rndval & 0x000FF;
            var x = (this.rndval & 0x1FF00) >> 8;
            var lsb = this.rndval & 1;
            this.rndval >>= 1;
            if (lsb != 0) {
                this.rndval ^= 0x00012000;
            }
            if (x < this.width && y <= this.height)
                return [x, y];
        } while (!this.done());
        return null;
    };
    return FizzleFade;
}());
// Cache to prevent generating them again and again!
// note: we do not store colors not font, since for this particular game, they are always the same
var textTilesWithOutline = {};
function fillTextTopLeftWithOutline(text, x, y, font, color, outlineColor) {
    var img;
    if (textTilesWithOutline[text] != null) {
        img = textTilesWithOutline[text];
    }
    else {
        img = getTextTileWithOutline(text, font, 8, color, outlineColor);
        textTilesWithOutline[text] = img;
    }
    // draw it:
    ctx.drawImage(img, 0, 0, img.width, img.height, x, y, img.width, img.height);
}
