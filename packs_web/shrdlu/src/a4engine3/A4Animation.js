var A4Animation = /** @class */ (function () {
    function A4Animation() {
        this.gf = null;
        this.widthInTiles = 1;
        this.heightInTiles = 1;
        this.period = 1;
        this.looping = false;
        this.length = 0;
        this.sequence = null;
        this.cycle = 0;
        this.state = 0;
        this.completed = false;
        this.seeThrough = true;
    }
    A4Animation.fromXML = function (xml, game) {
        // <animation name="curious" dx="1" dy="1" period="8" looping="true" file="graphics2x.png">74,-1</animation>
        var a = new A4Animation();
        var file = xml.getAttribute("file");
        a.gf = game.getGraphicFile(file);
        if (a.gf == null)
            console.log("A4Animation: cannot get graphic file " + file);
        a.widthInTiles = Number(xml.getAttribute("dx"));
        a.heightInTiles = Number(xml.getAttribute("dy"));
        a.period = Number(xml.getAttribute("period"));
        if (xml.getAttribute("looping") == "true")
            a.looping = true;
        if (xml.getAttribute("seeThrough") == "false")
            a.seeThrough = false;
        var sequenceText = xml.firstChild.nodeValue;
        var sequenceFrames = sequenceText.split(",");
        a.length = sequenceFrames.length;
        a.sequence = [];
        for (var _i = 0, sequenceFrames_1 = sequenceFrames; _i < sequenceFrames_1.length; _i++) {
            var frame = sequenceFrames_1[_i];
            a.sequence.push(Number(frame));
        }
        return a;
    };
    A4Animation.fromAnimation = function (a) {
        var a2 = new A4Animation();
        a2.gf = a.gf;
        a2.widthInTiles = a.widthInTiles;
        a2.heightInTiles = a.heightInTiles;
        a2.period = a.period;
        a2.looping = a.looping;
        a2.length = a.length;
        a2.sequence = [];
        for (var i = 0; i < a2.length; i++)
            a2.sequence.push(a.sequence[i]);
        a2.cycle = a.cycle;
        a2.state = a.state;
        a2.completed = a.completed;
        return a2;
    };
    A4Animation.prototype.saveToXML = function (name) {
        var xmlString = "";
        xmlString += "<animation ";
        xmlString += "name=\"" + name + "\" ";
        xmlString += "dx=\"" + this.widthInTiles + "\" ";
        xmlString += "dy=\"" + this.heightInTiles + "\" ";
        xmlString += "period=\"" + this.period + "\" ";
        xmlString += "looping=\"" + this.looping + "\" ";
        if (!this.seeThrough)
            xmlString += "seeThrough=\"false\" ";
        xmlString += "file=\"" + this.gf.name + "\">";
        for (var i = 0; i < this.length; i++) {
            if (i == 0) {
                xmlString += this.sequence[i];
            }
            else {
                xmlString += "," + this.sequence[i];
            }
        }
        xmlString += "</animation>";
        return xmlString;
    };
    A4Animation.prototype.reset = function () {
        this.cycle = 0;
        this.state = 0;
        this.completed = false;
    };
    A4Animation.prototype.update = function () {
        if (this.completed)
            return true;
        this.cycle++;
        if (this.cycle >= this.period) {
            this.cycle -= this.period;
            this.state++;
            if (this.state >= this.length) {
                if (this.looping) {
                    this.state = 0;
                }
                else {
                    this.state = this.length - 1;
                    this.completed = true;
                }
            }
        }
        return this.completed;
    };
    A4Animation.prototype.draw = function (x, y) {
        var t = this.getTile();
        if (t < 0)
            return;
        for (var i = 0; i < this.heightInTiles; i++) {
            for (var j = 0; j < this.widthInTiles; j++) {
                var tile = this.gf.getTile(t + j + i * this.gf.tilesPerRow);
                tile.draw(x + (j * tile.width), y + (i * tile.height));
            }
        }
    };
    A4Animation.prototype.drawDark = function (x, y) {
        var t = this.getTile();
        if (t < 0)
            return;
        for (var i = 0; i < this.heightInTiles; i++) {
            for (var j = 0; j < this.widthInTiles; j++) {
                var tile = this.gf.getTileDark(t + j + i * this.gf.tilesPerRow);
                if (tile != null)
                    tile.draw(x + (j * tile.width), y + (i * tile.height));
            }
        }
    };
    A4Animation.prototype.drawWithZoom = function (x, y, zoom) {
        var t = this.getTile();
        if (t < 0)
            return;
        for (var i = 0; i < this.heightInTiles; i++) {
            for (var j = 0; j < this.widthInTiles; j++) {
                var tile = this.gf.getTile(t + j + i * this.gf.tilesPerRow);
                if (tile != null)
                    tile.drawWithZoom(x + (j * tile.width), y + (i * tile.height), zoom);
            }
        }
    };
    // seeThrough() : boolean
    // {
    //     let tile:number = this.getTile();
    //     if (tile<0) return true;
    //     if (this.gf.tileSeeThrough[tile]==1) return false;
    //     return true;   
    // }
    A4Animation.prototype.getTile = function () {
        if (this.state >= 0)
            return this.sequence[this.state];
        return -1;
    };
    A4Animation.prototype.getPixelWidth = function () {
        return this.widthInTiles * this.gf.tileWidth;
    };
    A4Animation.prototype.getPixelHeight = function () {
        return this.heightInTiles * this.gf.tileHeight;
    };
    return A4Animation;
}());
