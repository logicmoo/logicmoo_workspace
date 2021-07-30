var GLTile = /** @class */ (function () {
    function GLTile(src, x1, y1, width, height) {
        this.src = src;
        this.x1 = x1;
        this.y1 = y1;
        this.width = width;
        this.height = height;
    }
    GLTile.prototype.draw = function (x, y) {
        ctx.drawImage(this.src, this.x1, this.y1, this.width, this.height, x, y, this.width, this.height);
    };
    GLTile.prototype.drawWithZoom = function (x, y, zoom) {
        ctx.drawImage(this.src, this.x1, this.y1, this.width, this.height, x, y, this.width * zoom, this.height * zoom);
    };
    GLTile.prototype.drawWithAlpha = function (x, y, alpha) {
        var tmp = ctx.globalAlpha;
        ctx.globalAlpha = alpha;
        ctx.drawImage(this.src, this.x1, this.y1, this.width, this.height, x, y, this.width, this.height);
        ctx.globalAlpha = tmp;
    };
    GLTile.prototype.drawCentered = function (x, y) {
        ctx.drawImage(this.src, this.x1, this.y1, this.width, this.height, x - this.width / 2, y - this.height / 2, this.width, this.height);
    };
    return GLTile;
}());
