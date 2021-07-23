var MAP_MAX_ALTITUDE = 3;
var PerceptionBufferRecord = /** @class */ (function () {
    function PerceptionBufferRecord(action, subjectID, subjectSort, objectID, objectSort, objectSymbol, indirectID, indirectSort, x0, y0, x1, y1) {
        this.directObjectID = null;
        this.indirectObjectID = null;
        this.subjectSort = null;
        this.directObjectSort = null;
        this.indirectObjectSort = null;
        this.directObjectSymbol = null;
        this.action = action;
        this.subjectID = subjectID;
        this.subjectSort = subjectSort;
        this.directObjectID = objectID;
        this.directObjectSort = objectSort;
        this.directObjectSymbol = objectSymbol;
        this.indirectObjectID = indirectID;
        this.indirectObjectSort = indirectSort;
        this.x0 = x0;
        this.y0 = y0;
        this.x1 = x1;
        this.y1 = y1;
    }
    return PerceptionBufferRecord;
}());
var PerceptionBufferObjectWarpedRecord = /** @class */ (function () {
    function PerceptionBufferObjectWarpedRecord(ID, sort, map, x0, y0, x1, y1) {
        this.ID = ID;
        this.sort = sort;
        this.targetMap = map;
        this.x0 = x0;
        this.y0 = y0;
        this.x1 = x1;
        this.y1 = y1;
    }
    return PerceptionBufferObjectWarpedRecord;
}());
;
var A4Map = /** @class */ (function () {
    function A4Map(xml, game, objectsToRevisit_xml, objectsToRevisit_object) {
        this.xml = null;
        this.name = null;
        this.tileWidth = 8;
        this.tileHeight = 8;
        this.pixelsPerMeter = 8;
        this.layers = [];
        this.bridges = [];
        this.bridgeDestinations = [];
        this.objects = [];
        this.cycle = 0;
        this.visibilityReevaluationRequested = true;
        this.visibilityRegions = null;
        this.lightOnStatus = null;
        this.textBubbles = []; // the second number is the timer
        // scripts:
        this.eventScripts = new Array(A4_NEVENTS);
        // script excution queues (these contain scripts that are pending execution, will be executed in the next "cycle"):
        this.scriptQueues = [];
        // story state:
        this.storyState = {};
        this.lastTimeStoryStateChanged = 0;
        // perception buffers:
        this.perceptionBuffer = [];
        this.warpPerceptionBuffer = [];
        this.xml = xml;
        this.width = Number(xml.getAttribute("width"));
        this.height = Number(xml.getAttribute("height"));
        var properties_xml = getFirstElementChildByTag(xml, "properties");
        var properties_xmls = getElementChildrenByTag(properties_xml, "property");
        for (var i = 0; i < properties_xmls.length; i++) {
            var property = properties_xmls[i];
            if (property.getAttribute("name") == "name") {
                this.name = property.getAttribute("value");
            }
            else if (property.getAttribute("name") == "pixels_per_meter") {
                this.pixelsPerMeter = Number(property.getAttribute("value"));
            }
        }
        this.visibilityRegions = [];
        for (var i = 0; i < this.width * this.height; i++)
            this.visibilityRegions.push(0);
        this.lightOnStatus = [];
        for (var i = 0; i < this.width * this.height; i++)
            this.lightOnStatus.push(1);
        // load tilesets:
        var gfs = [];
        var tilesets_xmls = getElementChildrenByTag(xml, "tileset");
        for (var i = 0; i < tilesets_xmls.length; i++) {
            var source_xmls = getElementChildrenByTag(tilesets_xmls[i], "image");
            for (var j = 0; j < source_xmls.length; j++) {
                var gf = game.getGraphicFile(source_xmls[j].getAttribute("source"));
                // console.log("Loaded tileset: " + source_xmls[j].getAttribute("source") + " -> " + gf);
                gfs.push(gf);
            }
        }
        this.tileWidth = gfs[0].tileWidth;
        this.tileHeight = gfs[0].tileHeight;
        // load tile layers:
        var layers_xmls = getElementChildrenByTag(xml, "layer");
        this.layers = [];
        for (var i = 0; i < A4_N_LAYERS; i++) {
            this.layers.push(new A4MapLayer(this.width, this.height, gfs));
            if (i < layers_xmls.length) {
                var layer_properties_xml = getFirstElementChildByTag(layers_xmls[i], "properties");
                if (layer_properties_xml != null) {
                    var layer_properties_xmls = getElementChildrenByTag(layer_properties_xml, "property");
                    for (var j = 0; j < layer_properties_xmls.length; j++) {
                        var property = layer_properties_xmls[j];
                        if (property.getAttribute("name") == "elevation") {
                            this.layers[i].elevation = Number(property.getAttribute("value"));
                        }
                    }
                }
                var data_xml = getFirstElementChildByTag(layers_xmls[i], "data");
                var encoding = data_xml.getAttribute("encoding");
                if (encoding == "csv") {
                    var values = data_xml.firstChild.nodeValue.split(new RegExp(",| |\n|\t|\r"));
                    var idx = 0;
                    for (var j = 0; j < values.length; j++) {
                        if (values[j] != "") {
                            this.layers[i].tiles[idx] = game.mapTiles[Number(values[j]) - 1];
                            if (this.layers[i].tiles[idx] == null && (Number(values[j]) - 1) >= 0) {
                                console.error("Cannot find mapTile with ID: " + (Number(values[j]) - 1));
                            }
                            idx++;
                        }
                    }
                }
                else {
                    var tile_xmls = getElementChildrenByTag(data_xml, "tile");
                    for (var j = 0; j < tile_xmls.length; j++) {
                        this.layers[i].tiles[j] = game.mapTiles[Number(tile_xmls[j].getAttribute("gid")) - 1];
                    }
                }
            }
        }
        // load object layers:
        var objectgroups_xml = getElementChildrenByTag(xml, "objectgroup");
        for (var i = 0; i < objectgroups_xml.length; i++) {
            var object_xml_l = getElementChildrenByTag(objectgroups_xml[i], "object");
            for (var j = 0; j < object_xml_l.length; j++) {
                var object_xml = object_xml_l[j];
                var o = this.loadObjectFromXML(object_xml, game, objectsToRevisit_xml, objectsToRevisit_object);
                if (o != null) {
                    this.addObject(o);
                }
            }
        }
        // loading scripts:
        {
            // on start:
            var onstarts_xml = getElementChildrenByTag(xml, "onStart");
            for (var i = 0; i < onstarts_xml.length; i++) {
                var onstart_xml = onstarts_xml[i];
                var tmp = null;
                var script_xml_l = onstart_xml.children;
                for (var j = 0; j < script_xml_l.length; j++) {
                    var script_xml = script_xml_l[j];
                    var s = A4Script.fromXML(script_xml);
                    if (tmp == null)
                        tmp = new A4ScriptExecutionQueue(null, this, game, null);
                    tmp.scripts.push(s);
                }
                if (tmp != null)
                    this.scriptQueues.push(tmp);
            }
            // event rules:
            var eventrules_xml = getElementChildrenByTag(xml, "eventRule");
            for (var i = 0; i < eventrules_xml.length; i++) {
                var rule_xml = eventrules_xml[i];
                var r = A4EventRule.fromXML(rule_xml);
                if (this.eventScripts[r.event] == null)
                    this.eventScripts[r.event] = [];
                this.eventScripts[r.event].push(r);
            }
        }
        this.reevaluateVisibilityRequest();
    }
    A4Map.prototype.loadObjectFromXML = function (object_xml, game, objectsToRevisit_xml, objectsToRevisit_object) {
        var o = null;
        var of = game.objectFactory;
        //let layer:number = A4_LAYER_FG;
        var o_ID = object_xml.getAttribute("id");
        var o_class = object_xml.getAttribute("class");
        if (o_class == null)
            o_class = object_xml.getAttribute("type");
        var completeRedefinition = false;
        if (object_xml.getAttribute("completeRedefinition") == "true")
            completeRedefinition = true;
        if (o_class == "Bridge") {
            var mb = new A4MapBridge(object_xml, this);
            mb.loadObjectAdditionalContent(object_xml, game, of, objectsToRevisit_xml, objectsToRevisit_object);
            this.bridges.push(mb);
            if (o_ID != null) {
                mb.ID = o_ID;
                if (!isNaN(Number(o_ID)) &&
                    Number(o_ID) >= A4Object.s_nextID)
                    A4Object.s_nextID = Number(o_ID) + 1;
            }
            return null;
        }
        else if (o_class == "BridgeDestination") {
            var mb = new A4MapBridge(object_xml, this);
            mb.loadObjectAdditionalContent(object_xml, game, of, objectsToRevisit_xml, objectsToRevisit_object);
            this.bridgeDestinations.push(mb);
            if (o_ID != null) {
                mb.ID = o_ID;
                if (!isNaN(Number(o_ID)) &&
                    Number(o_ID) >= A4Object.s_nextID)
                    A4Object.s_nextID = Number(o_ID) + 1;
            }
            return null;
        }
        else if (o_class == "Trigger") {
            o = new A4Trigger(game.ontology.getSort("Trigger"), Number(object_xml.getAttribute("width")), Number(object_xml.getAttribute("height")));
            o.loadObjectAdditionalContent(object_xml, game, of, objectsToRevisit_xml, objectsToRevisit_object);
            var once = true;
            if (object_xml.getAttribute("repeat") == "true")
                once = false;
            var scripts_xmls = getElementChildrenByTag(object_xml, "script");
            if (scripts_xmls != null && scripts_xmls.length > 0) {
                var tmp = scripts_xmls[0].children;
                for (var i = 0; i < tmp.length; i++) {
                    var s = A4Script.fromXML(tmp[i]);
                    o.addEventRule(A4_EVENT_ACTIVATE, new A4EventRule(A4_EVENT_ACTIVATE, s, once, 0, 0));
                }
            }
        }
        else {
            o = of.createObject(o_class, game, false, completeRedefinition);
            if (o == null) {
                console.error("Unknown object class: " + o_class);
                return null;
            }
            o.loadObjectAdditionalContent(object_xml, game, of, objectsToRevisit_xml, objectsToRevisit_object);
        }
        if (o_ID != null) {
            o.ID = o_ID;
            if (!isNaN(Number(o_ID)) &&
                Number(o_ID) >= A4Object.s_nextID)
                A4Object.s_nextID = Number(o_ID) + 1;
        }
        return o;
    };
    A4Map.prototype.saveToXML = function (game) {
        var xmlString = "";
        xmlString += "<map version=\"1.0\" " +
            "orientation=\"orthogonal\" " +
            "width=\"" + this.width + "\" " +
            "height=\"" + this.height + "\" " +
            "tilewidth=\"" + this.tileWidth + "\" " +
            "tileheight=\"" + this.tileHeight + "\">\n";
        xmlString += "<properties>\n";
        xmlString += "<property name=\"name\" value=\"" + this.name + "\"/>\n";
        xmlString += "<property name=\"pixels_per_meter\" value=\"" + this.pixelsPerMeter + "\"/>\n";
        xmlString += "</properties>\n";
        var firstID = 1;
        for (var _i = 0, _a = game.graphicFiles; _i < _a.length; _i++) {
            var gf = _a[_i];
            xmlString += "<tileset";
            xmlString += " firstgid=\"" + firstID + "\"";
            xmlString += " name=\"graphics\"";
            xmlString += " tilewidth=\"" + gf.tileWidth + "\"";
            xmlString += " tileheight=\"" + gf.tileHeight + "\">\n";
            xmlString += "<image source=\"" + gf.name + "\"" +
                " width=\"" + (gf.tilesPerRow * gf.tileWidth) + "\"" +
                " height=\"" + (gf.tileHeight * Math.floor(gf.n_tiles / gf.tilesPerRow)) + "\"/>\n";
            xmlString += "</tileset>\n";
            firstID += gf.n_tiles;
        }
        // tile layers:
        for (var i = 0; i < this.layers.length; i++) {
            var ml = this.layers[i];
            xmlString += "<layer name=\"Tile Layer " + (i + 1) + "\"" +
                " width=\"" + this.width + "\"" +
                " height=\"" + this.height + "\">\n";
            xmlString += "<properties>\n";
            xmlString += "<property name=\"elevation\" value=\"" + ml.elevation + "\"/>\n";
            xmlString += "</properties>\n";
            /*
                        xmlString += "<data>\n";
                        for(let y:number = 0;y<this.height;y++) {
                            for(let x:number = 0;x<this.width;x++) {
                                xmlString += "<tile gid=\""+(ml.tiles[x+y*this.width]+1)+"\"/>\n";
                            }
                        }
            */
            //              <data encoding="csv">
            xmlString += "<data encoding=\"csv\">\n";
            for (var y = 0; y < this.height; y++) {
                for (var x = 0; x < this.width; x++) {
                    if (ml.tiles[x + y * this.width] != null) {
                        xmlString += (ml.tiles[x + y * this.width].ID + 1) + ",";
                    }
                    else {
                        xmlString += "0,";
                    }
                }
                xmlString += "\n";
            }
            xmlString += "</data>\n";
            xmlString += "</layer>\n";
        }
        // object layers:
        xmlString += "<objectgroup name=\"objects\" width=\"" + this.width + "\" height=\"" + this.height + "\">\n";
        for (var _b = 0, _c = this.bridges; _b < _c.length; _b++) {
            var b = _c[_b];
            xmlString += b.saveToXML(game, 1, true) + "\n";
        }
        for (var _d = 0, _e = this.bridgeDestinations; _d < _e.length; _d++) {
            var b = _e[_d];
            xmlString += b.saveToXML(game, 2, true) + "\n";
        }
        for (var _f = 0, _g = this.objects; _f < _g.length; _f++) {
            var o = _g[_f];
            if (!o.isPlayer())
                xmlString += o.saveToXML(game, 0, true) + "\n";
        }
        /*
        for(let i:number = 0;i<A4_N_LAYERS;i++) {
            let ml:A4MapLayer = this.layers[i];
            for(let o of ml.objects) {
                if (!o.isPlayer())
                    xmlString += o.saveToXML(game,0,true) + "\n";
            }
        }
        */
        xmlString += "</objectgroup>\n";
        // save state:
        var onStarttagOpen = false;
        for (var v in this.storyState) {
            if (!onStarttagOpen) {
                xmlString += "<onStart>\n";
                onStarttagOpen = true;
            }
            xmlString += "<storyState variable=\"" + v + "\"" +
                " value=\"" + this.storyState[v] + "\"" +
                " scope=\"map\"/>\n";
        }
        if (onStarttagOpen)
            xmlString += "</onStart>\n";
        // each execution queue goes to its own "onStart" block:
        for (var _h = 0, _j = this.scriptQueues; _h < _j.length; _h++) {
            var seq = _j[_h];
            xmlString += "<onStart>\n";
            for (var _k = 0, _l = seq.scripts; _k < _l.length; _k++) {
                var s = _l[_k];
                xmlString += s.saveToXML() + "\n";
            }
            xmlString += "</onStart>\n";
        }
        // rules:
        for (var i = 0; i < A4_NEVENTS; i++) {
            if (this.eventScripts[i] != null) {
                for (var _m = 0, _o = this.eventScripts[i]; _m < _o.length; _m++) {
                    var er = _o[_m];
                    xmlString += er.saveToXML() + "\n";
                }
            }
        }
        xmlString += "</map>";
        return xmlString;
    };
    A4Map.prototype.cacheDrawTiles = function () {
        for (var _i = 0, _a = this.layers; _i < _a.length; _i++) {
            var layer = _a[_i];
            layer.cacheDrawTiles();
        }
    };
    A4Map.prototype.update = function (game) {
        if (this.cycle == 0 && this.eventScripts[A4_EVENT_START] != null) {
            for (var _i = 0, _a = this.eventScripts[A4_EVENT_START]; _i < _a.length; _i++) {
                var rule = _a[_i];
                rule.executeEffects(null, this, game, null);
            }
        }
        // objects:
        var toDelete = [];
        for (var _b = 0, _c = this.objects; _b < _c.length; _b++) {
            var o = _c[_b];
            if (!o.update(game)) {
                game.requestDeletion(o);
                toDelete.push(o);
            }
        }
        for (var _d = 0, toDelete_1 = toDelete; _d < toDelete_1.length; _d++) {
            var o = toDelete_1[_d];
            var idx = this.objects.indexOf(o);
            this.objects.splice(idx, 1);
        }
        // scripts:
        if (this.eventScripts[A4_EVENT_TIMER] != null) {
            for (var _e = 0, _f = this.eventScripts[A4_EVENT_TIMER]; _e < _f.length; _e++) {
                var r = _f[_e];
                r.execute(null, this, game, null);
            }
        }
        if (this.eventScripts[A4_EVENT_STORYSTATE] != null) {
            for (var _g = 0, _h = this.eventScripts[A4_EVENT_STORYSTATE]; _g < _h.length; _g++) {
                var r = _h[_g];
                r.execute(null, this, game, null);
            }
        }
        this.executeScriptQueues(game);
        {
            var toDeleteSB = [];
            for (var _j = 0, _k = this.textBubbles; _j < _k.length; _j++) {
                var sb = _k[_j];
                sb[1]--;
                if (sb[1] <= 0)
                    toDeleteSB.push(sb);
            }
            for (var _l = 0, toDeleteSB_1 = toDeleteSB; _l < toDeleteSB_1.length; _l++) {
                var sb = toDeleteSB_1[_l];
                var idx = this.textBubbles.indexOf(sb);
                this.textBubbles.splice(idx, 1);
            }
        }
        // perception buffers:
        {
            var toDelete2 = [];
            for (var _m = 0, _o = this.perceptionBuffer; _m < _o.length; _m++) {
                var pbr = _o[_m];
                if ((pbr.time + CYCLES_IN_PERCEPTION_BUFFER) < this.cycle)
                    toDelete2.push(pbr);
            }
            for (var _p = 0, toDelete2_1 = toDelete2; _p < toDelete2_1.length; _p++) {
                var dv = toDelete2_1[_p];
                var idx = this.perceptionBuffer.indexOf(dv);
                this.perceptionBuffer.splice(idx, 1);
            }
        }
        {
            var toDelete3 = [];
            for (var _q = 0, _r = this.warpPerceptionBuffer; _q < _r.length; _q++) {
                var wpbr = _r[_q];
                if ((wpbr.time + CYCLES_IN_PERCEPTION_BUFFER) < this.cycle)
                    toDelete3.push(wpbr);
            }
            for (var _s = 0, toDelete3_1 = toDelete3; _s < toDelete3_1.length; _s++) {
                var dv = toDelete3_1[_s];
                var idx = this.warpPerceptionBuffer.indexOf(dv);
                this.warpPerceptionBuffer.splice(idx, 1);
            }
        }
        if (this.visibilityReevaluationRequested)
            this.reevaluateVisibility();
        this.cycle++;
    };
    A4Map.prototype.drawRegion = function (offsetx, offsety, zoom, SCREEN_X, SCREEN_Y, visibilityRegion, game) {
        this.sortObjectByYCoordinate();
        ctx.save();
        ctx.scale(zoom, zoom);
        var object_idx = 0;
        var y = -offsety;
        var ZSY = Math.floor(SCREEN_Y / zoom) + MAP_MAX_ALTITUDE * 8;
        for (var row = 0; row < this.layers[0].height + MAP_MAX_ALTITUDE && y < ZSY; y += this.tileHeight, row++) {
            if (y + this.tileHeight < 0)
                continue;
            for (var i = 0; i < this.layers.length; i++) {
                this.layers[i].drawRegionRow(offsetx, offsety, row, zoom, SCREEN_X, SCREEN_Y, this.visibilityRegions, visibilityRegion, game, this);
            }
            // objects:
            var xx = void 0;
            var yy = void 0;
            var offset = void 0;
            for (; object_idx < this.objects.length; object_idx++) {
                var o = this.objects[object_idx];
                if (o.burrowed)
                    continue;
                if ((o.y + o.getPixelHeight()) < (row - 1) * this.tileHeight)
                    continue;
                if ((o.y + o.getPixelHeight()) > (row + 1) * this.tileHeight)
                    break;
                var tx = Math.floor(o.x / this.tileWidth);
                var ty = Math.floor(o.y / this.tileHeight);
                var draw = false;
                var dark = true;
                for (var i = 0; i < Math.floor(o.getPixelHeight() / this.tileHeight) && !draw; i++) {
                    for (var j = 0; j < Math.floor(o.getPixelWidth() / this.tileWidth) && !draw; j++) {
                        xx = tx + j;
                        yy = ty + i;
                        offset = xx + yy * this.width;
                        if (xx >= 0 && xx < this.width &&
                            yy >= 0 && yy < this.height) {
                            if (this.visibilityRegions[offset] == visibilityRegion) {
                                draw = true;
                                if (this.lightOnStatus[offset] == 1)
                                    dark = false;
                            }
                            else if (this.visibilityRegions[offset] == 0 &&
                                ((xx > 0 && this.visibilityRegions[offset - 1] == visibilityRegion) ||
                                    (xx < this.width - 1 && this.visibilityRegions[offset + 1] == visibilityRegion) ||
                                    (yy > 0 && this.visibilityRegions[offset - this.width] == visibilityRegion) ||
                                    (yy < this.height - 1 && this.visibilityRegions[offset + this.width] == visibilityRegion) ||
                                    (xx > 0 && yy > 0 && this.visibilityRegions[offset - (1 + this.width)] == visibilityRegion) ||
                                    (xx > 0 && yy < this.height - 1 && this.visibilityRegions[offset + this.width - 1] == visibilityRegion) ||
                                    (xx < this.width - 1 && yy > 0 && this.visibilityRegions[offset + 1 - this.width] == visibilityRegion) ||
                                    (xx < this.width - 1 && yy < this.height - 1 && this.visibilityRegions[offset + 1 + this.width] == visibilityRegion))) {
                                draw = true;
                                if (this.lightOnStatus[offset] == 1)
                                    dark = false;
                            }
                        }
                    }
                }
                if (draw) {
                    if (dark) {
                        o.drawDark(-offsetx, -offsety, game);
                    }
                    else {
                        o.draw(-offsetx, -offsety, game);
                    }
                }
            }
        }
        ctx.restore();
    };
    A4Map.prototype.drawTextBubbles = function (offsetx, offsety, zoom, SCREEN_X, SCREEN_Y, game) {
        ctx.save();
        ctx.scale(zoom, zoom);
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.burrowed)
                continue;
            if (!o.isCharacter())
                continue;
            var tx = Math.floor(o.x / this.tileWidth);
            var ty = Math.floor(o.y / this.tileHeight);
            var draw = false;
            for (var i = 0; i < Math.floor(o.getPixelHeight() / this.tileHeight) && !draw; i++) {
                for (var j = 0; j < Math.floor(o.getPixelWidth() / this.tileWidth) && !draw; j++) {
                    if (tx + j >= 0 && tx + j < this.width &&
                        ty + i >= 0 && ty + i < this.height) {
                        draw = true;
                    }
                }
            }
            if (draw && game.drawTextBubbles)
                o.drawTextBubbles(-offsetx, -offsety, SCREEN_X / zoom, SCREEN_Y / zoom, game);
        }
        var y = 0;
        for (var _b = 0, _c = this.textBubbles; _b < _c.length; _b++) {
            var sb = _c[_b];
            sb[0].drawNoArrow(Math.floor(SCREEN_X / zoom / 2) - (sb[0].width) / 2, y, false, 1);
            y += sb[0].height;
        }
        ctx.restore();
    };
    A4Map.prototype.drawTextBubblesRegion = function (offsetx, offsety, zoom, SCREEN_X, SCREEN_Y, visibilityRegion, game) {
        ctx.save();
        ctx.scale(zoom, zoom);
        var xx;
        var yy;
        var offset;
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.burrowed)
                continue;
            if (!o.isCharacter())
                continue;
            var tx = Math.floor(o.x / this.tileWidth);
            var ty = Math.floor(o.y / this.tileHeight);
            var draw = false;
            for (var i = 0; i < Math.floor(o.getPixelHeight() / this.tileHeight) && !draw; i++) {
                for (var j = 0; j < Math.floor(o.getPixelWidth() / this.tileWidth) && !draw; j++) {
                    xx = tx + j;
                    yy = ty + i;
                    offset = xx + yy * this.width;
                    if (xx >= 0 && xx < this.width &&
                        yy >= 0 && yy < this.height) {
                        if (this.visibilityRegions[offset] == visibilityRegion) {
                            draw = true;
                        }
                        else if (this.visibilityRegions[offset] == 0 &&
                            ((xx > 0 && this.visibilityRegions[offset - 1] == visibilityRegion) ||
                                (xx < this.width - 1 && this.visibilityRegions[offset + 1] == visibilityRegion) ||
                                (yy > 0 && this.visibilityRegions[offset - this.width] == visibilityRegion) ||
                                (yy < this.height - 1 && this.visibilityRegions[offset + this.width] == visibilityRegion) ||
                                (xx > 0 && yy > 0 && this.visibilityRegions[offset - (1 + this.width)] == visibilityRegion) ||
                                (xx > 0 && yy < this.height - 1 && this.visibilityRegions[offset + this.width - 1] == visibilityRegion) ||
                                (xx < this.width - 1 && yy > 0 && this.visibilityRegions[offset + 1 - this.width] == visibilityRegion) ||
                                (xx < this.width - 1 && yy < this.height - 1 && this.visibilityRegions[offset + 1 + this.width] == visibilityRegion))) {
                            draw = true;
                        }
                    }
                }
            }
            if (draw && game.drawTextBubbles)
                o.drawTextBubbles(-offsetx, -offsety, SCREEN_X / zoom, SCREEN_Y / zoom, game);
        }
        var y = 0;
        for (var _b = 0, _c = this.textBubbles; _b < _c.length; _b++) {
            var sb = _c[_b];
            sb[0].drawNoArrow(Math.floor(SCREEN_X / zoom / 2) - (sb[0].width) / 2, y, false, 1);
            y += sb[0].height;
        }
        ctx.restore();
    };
    A4Map.prototype.getNeighborMaps = function () {
        var l = [];
        for (var _i = 0, _a = this.bridges; _i < _a.length; _i++) {
            var mb = _a[_i];
            if (mb.linkedTo != null) {
                if (l.indexOf(mb.linkedTo.map) == -1) {
                    l.push(mb.linkedTo.map);
                }
            }
        }
        return l;
    };
    A4Map.prototype.executeScriptQueues = function (game) {
        var toDelete = [];
        for (var _i = 0, _a = this.scriptQueues; _i < _a.length; _i++) {
            var seb = _a[_i];
            while (true) {
                var s = seb.scripts[0];
                var retval = s.execute(seb.object, (seb.map == null ? this : seb.map), (seb.game == null ? game : seb.game), seb.otherCharacter);
                if (retval == SCRIPT_FINISHED) {
                    seb.scripts.splice(0, 1);
                    if (seb.scripts.length == 0) {
                        toDelete.push(seb);
                        break;
                    }
                }
                else if (retval == SCRIPT_NOT_FINISHED) {
                    break;
                }
                else if (retval == SCRIPT_FAILED) {
                    toDelete.push(seb);
                    break;
                }
            }
        }
        for (var _b = 0, toDelete_2 = toDelete; _b < toDelete_2.length; _b++) {
            var seb = toDelete_2[_b];
            var idx = this.scriptQueues.indexOf(seb);
            this.scriptQueues.splice(idx, 1);
        }
    };
    A4Map.prototype.addScriptQueue = function (seq) {
        this.scriptQueues.push(seq);
    };
    A4Map.prototype.setStoryStateVariable = function (variable, value, game) {
        this.storyState[variable] = value;
        this.lastTimeStoryStateChanged = game.cycle;
    };
    A4Map.prototype.getStoryStateVariable = function (variable) {
        return this.storyState[variable];
    };
    A4Map.prototype.removeObject = function (o) {
        var idx = this.objects.indexOf(o);
        if (idx >= 0) {
            this.objects.splice(idx, 1);
            return true;
        }
        return false;
    };
    A4Map.prototype.addObject = function (o) {
        this.objects.push(o);
        o.map = this;
    };
    A4Map.prototype.contains = function (o) {
        if (this.objects.indexOf(o) != -1)
            return true;
        return false;
    };
    // This function returns a list with the hierarchy of objects necessary to find the desired object
    // For example, if an object is directly in a map, the list with be length 1, but if 
    // the obeject is in the inventory of a character, then we will get a list with the character and then the object 
    A4Map.prototype.findObjectByName = function (name) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.name == name)
                return [o];
            var o2 = o.findObjectByName(name);
            if (o2 != null)
                return [o].concat(o2);
        }
        return null;
    };
    // This function returns a list with the hierarchy of objects necessary to find the desired object
    // For example, if an object is directly in a map, the list with be length 1, but if 
    // the obeject is in the inventory of a character, then we will get a list with the character and then the object 
    A4Map.prototype.findObjectByID = function (ID) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.ID == ID)
                return [o];
            var o2 = o.findObjectByID(ID);
            if (o2 != null)
                return [o].concat(o2);
        }
        return null;
    };
    A4Map.prototype.objectRemoved = function (o) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o2 = _a[_i];
            o2.objectRemoved(o);
        }
    };
    A4Map.prototype.checkIfDoorGroupStateCanBeChanged = function (doorGroup, state, character, map, game) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.isDoor()) {
                var d_1 = o;
                if (d_1.doorGroupID == doorGroup) {
                    if (!d_1.checkForBlockages(state, character, map, game, []))
                        return false;
                }
            }
        }
        return true;
    };
    A4Map.prototype.setDoorGroupState = function (doorGroup, state, character, map, game) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.isDoor()) {
                var d_2 = o;
                if (d_2.doorGroupID == doorGroup) {
                    d_2.changeStateRecursively(state, character, map, game);
                }
            }
        }
    };
    A4Map.prototype.getTileWidth = function () {
        return this.layers[0].tileWidth;
    };
    A4Map.prototype.getTileHeight = function () {
        return this.layers[0].tileHeight;
    };
    A4Map.prototype.addPerceptionBufferRecord = function (pbr) {
        pbr.time = this.cycle;
        this.perceptionBuffer.push(pbr);
    };
    A4Map.prototype.addPerceptionBufferObjectWarpedRecord = function (pbr) {
        pbr.time = this.cycle;
        this.warpPerceptionBuffer.push(pbr);
    };
    A4Map.prototype.walkableOnlyBackground = function (x, y, dx, dy, subject) {
        for (var i = 0; i < this.layers.length; i++) {
            if (!this.layers[i].walkableOnlyBackground(x, y, dx, dy, subject))
                return false;
        }
        return true;
    };
    A4Map.prototype.walkableOnlyObjects = function (x, y, dx, dy, subject) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o != subject) {
                //                if (subject.isCharacter() && o.isVehicle()) {
                //                    // characters can always walk onto empty vehicles:
                //                    if ((<A4Vehicle>o).isEmpty()) continue;
                //                }
                if (!o.isWalkable()) {
                    if (o.collision(x, y, dx, dy))
                        return false;
                }
            }
        }
        return true;
    };
    A4Map.prototype.walkableOnlyObjectsIgnoringObject = function (x, y, dx, dy, subject, toIgnore) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o == toIgnore)
                continue;
            if (o != subject) {
                //                if (subject.isCharacter() && o.isVehicle()) {
                //                    // characters can always walk onto empty vehicles:
                //                    if ((<A4Vehicle>o).isEmpty()) continue;
                //                }
                if (!o.isWalkable()) {
                    if (o.collision(x, y, dx, dy))
                        return false;
                }
            }
        }
        return true;
    };
    A4Map.prototype.walkable = function (x, y, dx, dy, subject) {
        if (!this.walkableOnlyBackground(x, y, dx, dy, subject))
            return false;
        return this.walkableOnlyObjects(x, y, dx, dy, subject);
    };
    A4Map.prototype.walkableIgnoringObject = function (x, y, dx, dy, subject, toIgnore) {
        if (!this.walkableOnlyBackground(x, y, dx, dy, subject))
            return false;
        return this.walkableOnlyObjectsIgnoringObject(x, y, dx, dy, subject, toIgnore);
    };
    A4Map.prototype.walkableConsideringVehicles = function (x, y, dx, dy, subject) {
        var rettiles = true;
        var retobjects = true;
        for (var i = 0; i < this.layers.length; i++) {
            if (rettiles && !this.layers[i].walkableOnlyBackground(x, y, dx, dy, subject))
                rettiles = false;
        }
        if (rettiles && !this.walkableOnlyObjects(x, y, dx, dy, subject))
            retobjects = false;
        return rettiles && retobjects;
    };
    A4Map.prototype.getBridge = function (x, y) {
        for (var _i = 0, _a = this.bridges; _i < _a.length; _i++) {
            var b = _a[_i];
            if (b.x < x && b.x + b.width > x &&
                b.y < y && b.y + b.height > y) {
                return b;
            }
        }
        return null;
    };
    A4Map.prototype.getTakeableObject = function (x, y, dx, dy) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.takeable && !o.burrowed && o.collision(x, y, dx, dy))
                return o;
        }
        return null;
    };
    A4Map.prototype.getBurrowedObject = function (x, y, dx, dy) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.burrowed && o.collision(x, y, dx, dy))
                return o;
        }
        return null;
    };
    A4Map.prototype.getUsableObject = function (x, y, dx, dy) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.usable && !o.burrowed && o.collision(x, y, dx, dy))
                return o;
        }
        return null;
    };
    A4Map.prototype.getVehicleObject = function (x, y, dx, dy) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.isVehicle() && o.collision(x, y, dx, dy))
                return o;
        }
        return null;
    };
    A4Map.prototype.getAllObjectCollisions = function (o) {
        return this.getAllObjectCollisionsWithOffset(o, 0, 0);
    };
    A4Map.prototype.getAllObjectCollisionsWithOffset = function (o, xoffs, yoffs) {
        var l = [];
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o2 = _a[_i];
            if (o2 != o && o.collisionObjectOffset(xoffs, yoffs, o2))
                l.push(o2);
        }
        return l;
    };
    A4Map.prototype.getAllObjectCollisionsOnlyWithOffset = function (o, xoffs, yoffs) {
        var l = [];
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o2 = _a[_i];
            if (o2 != o &&
                o.collisionObjectOffset(xoffs, yoffs, o2) &&
                !o.collisionObjectOffset(0, 0, o2))
                l.push(o2);
        }
        return l;
    };
    A4Map.prototype.getAllObjects = function (x, y, dx, dy) {
        var l = [];
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.collision(x, y, dx, dy))
                l.push(o);
        }
        return l;
    };
    A4Map.prototype.getAllObjectsInRegion = function (x, y, dx, dy, region) {
        var l = [];
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.collision(x, y, dx, dy)) {
                var tx = Math.floor(o.x / this.tileWidth);
                var ty = Math.floor(o.y / this.tileHeight);
                var region2 = this.visibilityRegion(tx, ty);
                if (region == region2)
                    l.push(o);
            }
        }
        return l;
    };
    A4Map.prototype.getAllObjectsInRegionPlusDoorsAndObstacles = function (x, y, dx, dy, region) {
        var l = [];
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.collision(x, y, dx, dy)) {
                var tx = Math.floor(o.x / this.tileWidth);
                var ty = Math.floor(o.y / this.tileHeight);
                var region2 = this.visibilityRegion(tx, ty);
                if (region == region2 ||
                    (o instanceof A4Door) ||
                    (o instanceof A4Obstacle) ||
                    (o instanceof A4ObstacleContainer) ||
                    (o instanceof A4PushableWall))
                    l.push(o);
            }
        }
        return l;
    };
    A4Map.prototype.triggerObjectsEvent = function (event, otherCharacter, map, game) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            o.event(event, otherCharacter, map, game);
        }
    };
    A4Map.prototype.triggerObjectsEventWithID = function (event, ID, otherCharacter, map, game) {
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            o.eventWithID(event, ID, otherCharacter, map, game);
        }
    };
    A4Map.prototype.reevaluateVisibilityRequest = function () {
        this.visibilityReevaluationRequested = true;
    };
    A4Map.prototype.reevaluateVisibility = function () {
        var x;
        var y;
        var nextRegion = 1;
        var inOpen = new Array(this.width * this.height);
        for (var i = 0; i < this.width * this.height; i++) {
            this.visibilityRegions[i] = 0;
            inOpen[i] = false;
        }
        for (var start_y = 0; start_y < this.height; start_y++) {
            for (var start_x = 0; start_x < this.width; start_x++) {
                if (this.visibilityRegions[start_x + start_y * this.width] == 0 && this.seeThrough(start_x, start_y)) {
                    var stack = [];
                    stack.push(start_x + start_y * this.width);
                    inOpen[start_x + start_y * this.width] = true;
                    while (stack.length > 0) {
                        var tmp = stack[0];
                        stack.splice(0, 1);
                        x = tmp % this.width;
                        y = Math.floor(tmp / this.width);
                        if (this.seeThrough(x, y)) {
                            this.visibilityRegions[tmp] = nextRegion;
                            if (x > 0 && this.visibilityRegions[x + y * this.width - 1] == 0 && !inOpen[x + y * this.width - 1]) {
                                stack.push(x + y * this.width - 1);
                                inOpen[x + y * this.width - 1] = true;
                            }
                            if (y > 0 && this.visibilityRegions[x + (y - 1) * this.width] == 0 && !inOpen[x + (y - 1) * this.width]) {
                                stack.push(x + (y - 1) * this.width);
                                inOpen[x + (y - 1) * this.width] = true;
                            }
                            if (x < (this.width - 1) && this.visibilityRegions[x + y * this.width + 1] == 0 && !inOpen[x + y * this.width + 1]) {
                                stack.push(x + y * this.width + 1);
                                inOpen[x + y * this.width + 1] = true;
                            }
                            if (y < (this.height - 1) && this.visibilityRegions[x + (y + 1) * this.width] == 0 && !inOpen[x + (y + 1) * this.width]) {
                                stack.push(x + (y + 1) * this.width);
                                inOpen[x + (y + 1) * this.width] = true;
                            }
                            if (x > 0 && y > 0 && this.visibilityRegions[x + (y - 1) * this.width - 1] == 0 && !inOpen[x + (y - 1) * this.width - 1]) {
                                stack.push(x + (y - 1) * this.width - 1);
                                inOpen[x + (y - 1) * this.width - 1] = true;
                            }
                            if (x < (this.width - 1) && y > 0 && this.visibilityRegions[x + (y - 1) * this.width + 1] == 0 && !inOpen[x + (y - 1) * this.width + 1]) {
                                stack.push(x + (y - 1) * this.width + 1);
                                inOpen[x + (y - 1) * this.width + 1] = true;
                            }
                            if (x > 0 && y < (this.height - 1) && this.visibilityRegions[x + (y + 1) * this.width - 1] == 0 && !inOpen[x + (y + 1) * this.width - 1]) {
                                stack.push(x + (y + 1) * this.width - 1);
                                inOpen[x + (y + 1) * this.width - 1] = true;
                            }
                            if (x < (this.width - 1) && y < (this.height - 1) && this.visibilityRegions[x + (y + 1) * this.width + 1] == 0 && !inOpen[x + (y + 1) * this.width + 1]) {
                                stack.push(x + (y + 1) * this.width + 1);
                                inOpen[x + (y + 1) * this.width + 1] = true;
                            }
                        }
                    }
                    nextRegion++;
                }
            }
        }
        //console.log("Regions: " + this.visibilityRegions);
        /*
        console.log("ReevaluateVisibility called on " + this.name);
        let debugstr:string = "";
        for(let i:number = 0;i<this.height;i++) {
            for(let j:number = 0;j<this.width;j++) {
                debugstr += ""+this.visibilityRegions[j+i*this.width];
            }
            debugstr += "\n";
        }
        console.log(debugstr);
        */
        this.visibilityReevaluationRequested = false;
    };
    // coordinates for these functions are in tiles:
    A4Map.prototype.seeThrough = function (tilex, tiley) {
        for (var i = 0; i < this.layers.length; i++) {
            if (!this.layers[i].seeThrough(tilex, tiley))
                return false;
        }
        for (var _i = 0, _a = this.objects; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.x <= tilex * this.tileWidth && o.x + o.getPixelWidth() >= (tilex + 1) * this.tileWidth &&
                o.y <= tiley * this.tileHeight && o.y + o.getPixelHeight() >= (tiley + 1) * this.tileHeight) {
                if (!o.seeThrough())
                    return false;
            }
        }
        return true;
    };
    A4Map.prototype.visible = function (tilex, tiley, region) {
        return this.visibilityRegions[tilex + tiley * this.width] == region;
    };
    A4Map.prototype.visibilityRegion = function (tilex, tiley) {
        return this.visibilityRegions[tilex + tiley * this.width];
    };
    // By default, all the lights are on, but this function can be used to make any region of the map
    // dark. The regionsWithLights/regionsWithLightsOn variables should be handled by each specific
    // game, and are used to tell the engine which parts of the maps are lit and which are not.
    // By default, if a region is not in the "regionsWithLights" list, it is assumed it is lit.
    A4Map.prototype.recalculateLightsOnStatus = function (regionsWithLights, regionsWithLightsOn, regionNames) {
        for (var i = 0; i < regionNames.length; i++) {
            if (regionsWithLights.indexOf(regionNames[i]) == -1) {
                this.lightOnStatus[i] = 1; // by default, lights are on!
            }
            else {
                if (regionsWithLightsOn.lastIndexOf(regionNames[i]) == -1) {
                    this.lightOnStatus[i] = 0;
                }
                else {
                    this.lightOnStatus[i] = 1;
                }
            }
        }
    };
    // this function assumes they are already almost sorted, and thus uses simple shuffle sort:
    A4Map.prototype.sortObjectByYCoordinate = function () {
        var change = true;
        var tmp = null;
        while (change) {
            change = false;
            // going up:
            for (var i = 0; i < this.objects.length - 1; i++) {
                var yi1 = this.objects[i].y + this.objects[i].getPixelHeight();
                var yi2 = this.objects[i + 1].y + this.objects[i + 1].getPixelHeight();
                if (yi1 > yi2 ||
                    // A4Characters have preference over other objects:
                    (yi1 == yi2 &&
                        this.objects[i] instanceof A4Character &&
                        !(this.objects[i + 1] instanceof A4Character))) {
                    tmp = this.objects[i];
                    this.objects[i] = this.objects[i + 1];
                    this.objects[i + 1] = tmp;
                    change = true;
                }
            }
            // going down:
            if (change) {
                change = false;
                for (var i = this.objects.length - 2; i >= 0; i--) {
                    var yi1 = this.objects[i].y + this.objects[i].getPixelHeight();
                    var yi2 = this.objects[i + 1].y + this.objects[i + 1].getPixelHeight();
                    if (yi1 > yi2 ||
                        // A4Characters have preference over other objects:
                        (yi1 == yi2 &&
                            this.objects[i] instanceof A4Character &&
                            !(this.objects[i + 1] instanceof A4Character))) {
                        tmp = this.objects[i];
                        this.objects[i] = this.objects[i + 1];
                        this.objects[i + 1] = tmp;
                        change = true;
                    }
                }
            }
        }
    };
    return A4Map;
}());
