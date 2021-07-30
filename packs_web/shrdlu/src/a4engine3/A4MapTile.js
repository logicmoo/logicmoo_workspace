var A4MapTile = /** @class */ (function () {
    function A4MapTile() {
        // the number used to identify this tile in Tiled:
        this.ID = -1;
        // Each of the entries in these lists will be drawn one after another vertically, when this tile has to be drawn:
        this.tileIds = null;
        this.walkable = true;
        this.seeThrough = true;
        this.glTiles = null;
        this.glTilesDark = null;
    }
    A4MapTile.loadFromXML = function (object_xml, game) {
        var t = new A4MapTile();
        t.ID = Number(object_xml.getAttribute("ID"));
        var tilesStr = object_xml.getAttribute("tiles").split(",");
        t.tileIds = [];
        for (var _i = 0, tilesStr_1 = tilesStr; _i < tilesStr_1.length; _i++) {
            var tileStr = tilesStr_1[_i];
            t.tileIds.push(Number(tileStr));
        }
        if (object_xml.getAttribute("walkable") == "false") {
            t.walkable = false;
        }
        if (object_xml.getAttribute("seeThrough") == "false") {
            t.seeThrough = false;
        }
        return t;
    };
    A4MapTile.prototype.outerHTML = function () { return this.saveToXML(); };
    A4MapTile.prototype.saveToXML = function () {
        var xmlString = "<tile ID=\"" + this.ID + "\" tiles=\"";
        var first = true;
        for (var _i = 0, _a = this.tileIds; _i < _a.length; _i++) {
            var id = _a[_i];
            if (first) {
                first = false;
                xmlString += id;
            }
            else {
                xmlString += "," + id;
            }
        }
        xmlString += "\"";
        if (!this.walkable)
            xmlString += " walkable=\"false\"";
        if (!this.seeThrough)
            xmlString += " seeThrough=\"false\"";
        return xmlString + "/>\n";
    };
    A4MapTile.prototype.cacheDrawTiles = function (graphicFiles, gfs_startTile) {
        this.glTiles = new Array(this.tileIds.length);
        this.glTilesDark = new Array(this.tileIds.length);
        for (var i = 0; i < this.tileIds.length; i++) {
            if (this.tileIds[i] >= 0) {
                for (var j = 0; j < graphicFiles.length; j++) {
                    if (j < graphicFiles.length - 1 && this.tileIds[i] >= gfs_startTile[j + 1])
                        continue;
                    this.glTiles[i] = graphicFiles[j].getTile(this.tileIds[i] - gfs_startTile[j]);
                    this.glTilesDark[i] = graphicFiles[j].getTileDark(this.tileIds[i] - gfs_startTile[j]);
                    // if images are not yet loaded, wait!
                    if (this.glTiles[i] == null || this.glTilesDark[i] == null) {
                        this.glTiles = null;
                        this.glTilesDark = null;
                        return;
                    }
                    break;
                }
            }
            else {
                this.glTiles[i] = null;
                this.glTilesDark[i] = null;
            }
        }
    };
    A4MapTile.prototype.draw = function (x, y, stride) {
        for (var _i = 0, _a = this.glTiles; _i < _a.length; _i++) {
            var glTile = _a[_i];
            if (glTile != null)
                glTile.draw(x, y);
            y -= stride;
        }
    };
    A4MapTile.prototype.drawDark = function (x, y, stride) {
        for (var _i = 0, _a = this.glTilesDark; _i < _a.length; _i++) {
            var glTile = _a[_i];
            if (glTile != null)
                glTile.draw(x, y);
            y -= stride;
        }
    };
    return A4MapTile;
}());
