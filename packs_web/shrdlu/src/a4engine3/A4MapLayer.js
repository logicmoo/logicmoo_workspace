var A4MapLayer = /** @class */ (function () {
    function A4MapLayer(width, height, gfs) {
        this.elevation = 0; // this indicates the elevation offset of the tiles in this layer
        this.graphicFiles = [];
        this.gfs_startTile = [];
        this.canDig = [];
        this.tiles = [];
        this.width = width;
        this.height = height;
        this.graphicFiles = gfs;
        var startTile = 0;
        for (var i = 0; i < gfs.length; i++) {
            this.gfs_startTile.push(startTile);
            startTile += gfs[i].n_tiles;
        }
        for (var i = 0; i < width * height; i++) {
            this.tiles.push(null);
            this.canDig.push(0);
        }
        this.tileWidth = gfs[0].tileWidth;
        this.tileHeight = gfs[0].tileHeight;
    }
    A4MapLayer.prototype.cacheDrawTiles = function () {
        for (var i = 0; i < this.width * this.height; i++) {
            if (this.tiles[i] != null) {
                this.tiles[i].cacheDrawTiles(this.graphicFiles, this.gfs_startTile);
            }
        }
    };
    A4MapLayer.prototype.draw = function (offsetx, offsety, zoom, SCREEN_X, SCREEN_Y, game) {
        ctx.save();
        ctx.scale(zoom, zoom);
        var y = -offsety;
        var ZSY = Math.floor(SCREEN_Y / zoom);
        var ZSX = Math.floor(SCREEN_X / zoom);
        for (var i = 0; i < this.height && y < ZSY; i++, y += this.tileHeight) {
            if (y + this.tileHeight < 0)
                continue;
            var offset = i * this.width;
            var x = -offsetx;
            for (var j = 0; j < this.width && x < ZSX; j++, x += this.tileWidth, offset++) {
                if (x + this.tileWidth < 0)
                    continue;
                if (this.tiles[offset] != null) {
                    this.tiles[offset].draw(x, y, this.tileHeight);
                }
            }
        }
        ctx.restore();
    };
    A4MapLayer.prototype.drawRegion = function (offsetx, offsety, zoom, SCREEN_X, SCREEN_Y, visibility, visibilityRegion, game, map) {
        ctx.save();
        ctx.scale(zoom, zoom);
        var y = -offsety;
        var ZSY = Math.floor(SCREEN_Y / zoom);
        var ZSX = Math.floor(SCREEN_X / zoom);
        for (var i = 0; i < this.height && y < ZSY; i++, y += this.tileHeight) {
            if (y + this.tileHeight < 0)
                continue;
            var offset = i * this.width;
            var x = -offsetx;
            for (var j = 0; j < this.width && x < ZSX; j++, x += this.tileWidth, offset++) {
                if (x + this.tileWidth < 0)
                    continue;
                if (this.tiles[offset] != null) {
                    if (visibility[offset] == visibilityRegion) {
                        if (map.lightOnStatus[offset]) {
                            this.tiles[offset].draw(x, y, this.tileHeight);
                        }
                        else {
                            this.tiles[offset].drawDark(x, y, this.tileHeight);
                        }
                    }
                    else if (visibility[offset] == 0 &&
                        ((j > 0 && visibility[offset - 1] == visibilityRegion) ||
                            (j < this.width - 1 && visibility[offset + 1] == visibilityRegion) ||
                            (i > 0 && visibility[offset - this.width] == visibilityRegion) ||
                            (i < this.height - 1 && visibility[offset + this.width] == visibilityRegion) ||
                            (j > 0 && i > 0 && visibility[offset - (1 + this.width)] == visibilityRegion) ||
                            (j > 0 && i < this.height - 1 && visibility[offset + this.width - 1] == visibilityRegion) ||
                            (j < this.width - 1 && i > 0 && visibility[offset + 1 - this.width] == visibilityRegion) ||
                            (j < this.width - 1 && i < this.height - 1 && visibility[offset + 1 + this.width] == visibilityRegion))) {
                        if (map.lightOnStatus[offset]) {
                            this.tiles[offset].draw(x, y, this.tileHeight);
                        }
                        else {
                            this.tiles[offset].drawDark(x, y, this.tileHeight);
                        }
                    }
                }
            }
        }
        ctx.restore();
    };
    A4MapLayer.prototype.drawRegionRow = function (offsetx, offsety, i, zoom, SCREEN_X, SCREEN_Y, visibility, visibilityRegion, game, map) {
        i -= this.elevation;
        if (i < 0)
            return;
        if (i >= this.height)
            return;
        var y = i * this.tileHeight - offsety;
        var offset = i * this.width;
        var offsetWithoutElevation = (i + this.elevation) * this.width;
        var visibilityOffset = (i + this.elevation) * this.width;
        var ZSX = Math.floor(SCREEN_X / zoom);
        var x = -offsetx;
        for (var j = 0; j < this.width && x < ZSX; j++, x += this.tileWidth, offset++, offsetWithoutElevation++, visibilityOffset++) {
            if (x + this.tileWidth < 0)
                continue;
            if (this.tiles[offset] != null) {
                if (visibility[visibilityOffset] == visibilityRegion) {
                    if (map.lightOnStatus[offsetWithoutElevation]) {
                        this.tiles[offset].draw(x, y, this.tileHeight);
                    }
                    else {
                        this.tiles[offset].drawDark(x, y, this.tileHeight);
                    }
                }
                else if (visibility[visibilityOffset] == 0 &&
                    ((j > 0 && visibility[visibilityOffset - 1] == visibilityRegion) ||
                        (j < this.width - 1 && visibility[visibilityOffset + 1] == visibilityRegion) ||
                        (i > 0 && visibility[visibilityOffset - this.width] == visibilityRegion) ||
                        (i < this.height - 1 && visibility[visibilityOffset + this.width] == visibilityRegion) ||
                        (j > 0 && i > 0 && visibility[visibilityOffset - (1 + this.width)] == visibilityRegion) ||
                        (j > 0 && i < this.height - 1 && visibility[visibilityOffset + this.width - 1] == visibilityRegion) ||
                        (j < this.width - 1 && i > 0 && visibility[visibilityOffset + 1 - this.width] == visibilityRegion) ||
                        (j < this.width - 1 && i < this.height - 1 && visibility[visibilityOffset + 1 + this.width] == visibilityRegion))) {
                    if (map.lightOnStatus[offsetWithoutElevation]) {
                        this.tiles[offset].draw(x, y, this.tileHeight);
                    }
                    else {
                        this.tiles[offset].drawDark(x, y, this.tileHeight);
                    }
                }
            }
        }
    };
    A4MapLayer.prototype.walkableOnlyBackground = function (x, y, dx, dy, subject) {
        if (x < -this.tileWidth || y < -this.tileHeight)
            return false;
        if (x + dx >= (this.width + 1) * this.tileWidth ||
            y + dy >= (this.height + 1) * this.tileHeight)
            return false;
        var tile_x = Math.floor(x / this.tileWidth);
        var tile_y = Math.floor(y / this.tileHeight);
        var tile_x2 = Math.floor((x + dx - 1) / this.tileWidth);
        var tile_y2 = Math.floor((y + dy - 1) / this.tileHeight);
        var tile = null;
        for (var i = tile_y; i <= tile_y2; i++) {
            if (i < 0)
                continue;
            if (i >= this.height)
                continue;
            for (var j = tile_x; j <= tile_x2; j++) {
                //if (j>=this.width) return false;
                if (j >= this.width)
                    continue;
                tile = this.tiles[j + i * this.width];
                if (tile != null && !tile.walkable) {
                    return false;
                }
            }
        }
        return true;
    };
    A4MapLayer.prototype.seeThrough = function (tilex, tiley) {
        var tile = this.tiles[tilex + tiley * this.width];
        if (tile != null && !tile.seeThrough) {
            return false;
        }
        return true;
    };
    return A4MapLayer;
}());
;
