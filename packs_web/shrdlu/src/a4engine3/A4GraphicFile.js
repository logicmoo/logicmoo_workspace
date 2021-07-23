var A4GraphicFile = /** @class */ (function () {
    function A4GraphicFile(name, tile_width, tile_height, path, GLTM) {
        this.img = null;
        this.tiles = null;
        this.tilesDark = null;
        this.name = name;
        this.path = path;
        this.fullName = path + "/" + name;
        this.tileWidth = tile_width;
        this.tileHeight = tile_height;
        this.GLTM = GLTM;
        this.img = this.GLTM.getImage(this.fullName);
        if (this.img == null)
            console.log("Graphic file '" + this.fullName + "' cannot be found!");
    }
    A4GraphicFile.prototype.updateAfterLoaded = function () {
        this.tilesPerRow = Math.floor(this.img.width / this.tileWidth);
        this.n_tiles = this.tilesPerRow * Math.floor(this.img.height / this.tileHeight);
        this.tiles = new Array(this.n_tiles);
        this.tilesDark = new Array(this.n_tiles);
        // this.tileTypes = new Array(this.n_tiles);
        // this.tileSeeThrough = new Array(this.n_tiles);
        // this.tileCanDig = new Array(this.n_tiles);
        console.log("GraphicFile created " + this.n_tiles + " tiles (" + this.tilesPerRow + " x " + Math.floor(this.img.height / this.tileHeight) + ")");
    };
    A4GraphicFile.prototype.getTile = function (n) {
        if (n < 0 || n >= this.n_tiles) {
            console.log("Requesting tile outside of range: " + n);
            return null;
        }
        if (this.tiles[n] != null)
            return this.tiles[n];
        var x = (n % this.tilesPerRow) * this.tileWidth;
        var y = Math.floor(n / this.tilesPerRow) * this.tileHeight;
        //    output_debug_message("%i -> %i,%i - %i,%i\n",n,x,y,m_tile_dx,m_tile_dy);
        this.tiles[n] = this.GLTM.getPiece(this.fullName, x, y, this.tileWidth, this.tileHeight);
        return this.tiles[n];
    };
    A4GraphicFile.prototype.getTileDark = function (n) {
        if (n < 0 || n >= this.n_tiles) {
            console.log("Requesting tile outside of range: " + n);
            return null;
        }
        if (this.tilesDark[n] != null)
            return this.tilesDark[n];
        var x = (n % this.tilesPerRow) * this.tileWidth;
        var y = Math.floor(n / this.tilesPerRow) * this.tileHeight;
        //    output_debug_message("%i -> %i,%i - %i,%i\n",n,x,y,m_tile_dx,m_tile_dy);
        this.tilesDark[n] = this.GLTM.getPieceDark(this.fullName, x, y, this.tileWidth, this.tileHeight);
        return this.tilesDark[n];
    };
    return A4GraphicFile;
}());
