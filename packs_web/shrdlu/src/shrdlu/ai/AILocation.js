var AILocation = /** @class */ (function () {
    function AILocation() {
        this.id = null;
        this.name = null;
        this.sort = null;
        this.maps = [];
        this.mapOccupancyMaps = [];
        this.preferredCenterCoordinatesInMap = []; // if these are != null, they will be used as center coordiantes.
    }
    // Used, for example, for the Tardis 8, when you ask a robot to go there, so that they just
    // enter, rather than entering and then navigating half the ship to get to the center.
    AILocation.prototype.distanceFromObject = function (o, mapIdx) {
        var map = this.maps[mapIdx];
        var tile_x = Math.floor((o.x + o.getPixelWidth() / 2) / map.tileWidth);
        var tile_y = Math.floor((o.y + o.getPixelHeight() / 2) / map.tileHeight);
        var offset = tile_x + tile_y * map.width;
        if (this.mapOccupancyMaps[mapIdx][offset]) {
            return 0;
        }
        else {
            var closestDistance = null;
            for (var i = 0; i < this.mapOccupancyMaps[mapIdx].length; i++) {
                if (this.mapOccupancyMaps[mapIdx][i]) {
                    var x = i % map.width;
                    var y = Math.floor(i / map.width);
                    var d_1 = Math.sqrt((tile_x - x) * (tile_x - x) + (tile_y - y) * (tile_y - y)) * SHRDLU_TILE_SIZE;
                    if (closestDistance == null || d_1 < closestDistance)
                        closestDistance = d_1;
                }
            }
            return closestDistance / map.pixelsPerMeter;
        }
    };
    AILocation.prototype.distanceFromLocation = function (l2, game) {
        var l1_idx = game.locations.indexOf(this);
        var l2_idx = game.locations.indexOf(l2);
        if (l1_idx >= 0 && l2_idx >= 0 &&
            (game.location_in[l1_idx][l2_idx] || game.location_in[l2_idx][l1_idx]))
            return 0;
        for (var _i = 0, _a = this.maps; _i < _a.length; _i++) {
            var map = _a[_i];
            for (var _b = 0, _c = l2.maps; _b < _c.length; _b++) {
                var map2 = _c[_b];
                if (map == map2) {
                    var c1 = this.centerCoordinatesInMap(map);
                    var c2 = l2.centerCoordinatesInMap(map);
                    if (c1 != null && c2 != null) {
                        return Math.sqrt((c1[0] - c2[0]) * (c1[0] - c2[0]) + (c1[1] - c2[1]) * (c1[1] - c2[1])) / map.pixelsPerMeter;
                    }
                }
            }
        }
        return null;
    };
    AILocation.prototype.centerCoordinatesInMap = function (map) {
        var x1 = 0;
        var y1 = 0;
        var total = 0;
        for (var i = 0; i < this.maps.length; i++) {
            if (this.maps[i] == map) {
                if (this.preferredCenterCoordinatesInMap[i] != null) {
                    return this.preferredCenterCoordinatesInMap[i];
                }
                var offset = 0;
                for (var y = 0; y < this.maps[i].height; y++) {
                    for (var x = 0; x < this.maps[i].width; x++, offset++) {
                        if (this.mapOccupancyMaps[i][offset]) {
                            x1 += x * this.maps[i].tileWidth;
                            y1 += y * this.maps[i].tileHeight;
                            total++;
                        }
                    }
                }
            }
        }
        if (total == 0)
            return null;
        x1 = Math.floor(x1 / (total * map.tileWidth)) * map.tileWidth;
        y1 = Math.floor(y1 / (total * map.tileHeight)) * map.tileHeight;
        return [x1, y1];
    };
    AILocation.prototype.centerWalkableCoordinatesInMap = function (map, character) {
        var x1 = 0;
        var y1 = 0;
        var total = 0;
        for (var i = 0; i < this.maps.length; i++) {
            if (this.maps[i] == map) {
                if (this.preferredCenterCoordinatesInMap[i] != null) {
                    var x = this.preferredCenterCoordinatesInMap[i][0];
                    var y = this.preferredCenterCoordinatesInMap[i][1];
                    if (map.walkable(x, y, character.getPixelWidth(), character.getPixelHeight(), character)) {
                        return this.preferredCenterCoordinatesInMap[i];
                    }
                }
                // calculate the center of the walkable area:
                var offset = 0;
                for (var y = 0; y < this.maps[i].height; y++) {
                    for (var x = 0; x < this.maps[i].width; x++, offset++) {
                        if (this.mapOccupancyMaps[i][offset] &&
                            map.walkable(x * map.tileWidth, y * map.tileHeight, character.getPixelWidth(), character.getPixelHeight(), character)) {
                            x1 += x * this.maps[i].tileWidth;
                            y1 += y * this.maps[i].tileHeight;
                            total++;
                        }
                    }
                }
                if (total == 0)
                    return null;
                x1 = Math.floor(x1 / (total * map.tileWidth)) * map.tileWidth;
                y1 = Math.floor(y1 / (total * map.tileHeight)) * map.tileHeight;
                // find walkable coordinates outside of a bridge:
                var half_width = character.getPixelWidth() / 2;
                var half_height = character.getPixelHeight() / 2;
                var best = null;
                var best_d = 0;
                offset = 0;
                for (var y = 0; y < this.maps[i].height; y++) {
                    for (var x = 0; x < this.maps[i].width; x++, offset++) {
                        if (this.mapOccupancyMaps[i][offset] &&
                            map.walkable(x * map.tileWidth, y * map.tileHeight, character.getPixelWidth(), character.getPixelHeight(), character)) {
                            var bridge = map.getBridge(x * map.tileWidth + half_width, y * map.tileHeight + half_height);
                            if (bridge == null) {
                                var xtmp = x * this.maps[i].tileWidth;
                                var ytmp = y * this.maps[i].tileHeight;
                                var d_2 = (x1 - xtmp) * (x1 - xtmp) + (y1 - ytmp) * (y1 - ytmp);
                                if (best == null || d_2 < best_d) {
                                    best_d = d_2;
                                    best = [xtmp, ytmp];
                                }
                            }
                        }
                    }
                }
                return best;
            }
        }
        return null;
    };
    AILocation.loadLocationsFromXML = function (xml, game, o) {
        // load the new locations:
        for (var _i = 0, _a = getElementChildrenByTag(xml, "map"); _i < _a.length; _i++) {
            var map_xml = _a[_i];
            var mapName = map_xml.getAttribute("name");
            var w = Number(map_xml.getAttribute("width"));
            var h = Number(map_xml.getAttribute("height"));
            var data = getFirstElementChildByTag(map_xml, "data").firstChild.nodeValue;
            var splitData = data.split(",");
            for (var i = 0; i < splitData.length; i++)
                splitData[i] = splitData[i].trim();
            //			console.log("location data for " + mapName + ": " + splitData);
            for (var _b = 0, _c = getElementChildrenByTag(map_xml, "location"); _b < _c.length; _b++) {
                var location_xml = _c[_b];
                var code = location_xml.getAttribute("code");
                var id = location_xml.getAttribute("id");
                var name_1 = location_xml.getAttribute("name");
                var sort = location_xml.getAttribute("sort");
                var location_1 = null;
                for (var _d = 0, _e = game.locations; _d < _e.length; _d++) {
                    var l2 = _e[_d];
                    if (l2.id == id) {
                        location_1 = l2;
                        break;
                    }
                }
                if (location_1 == null) {
                    location_1 = new AILocation();
                    location_1.id = id;
                    location_1.name = name_1;
                    location_1.sort = o.getSort(sort);
                    game.locations.push(location_1);
                }
                var occupancyMap = new Array(w * h);
                for (var i = 0; i < w * h; i++) {
                    if (splitData[i] == code)
                        occupancyMap[i] = true;
                    else
                        occupancyMap[i] = false;
                }
                location_1.maps.push(game.getMap(mapName));
                location_1.mapOccupancyMaps.push(occupancyMap);
                location_1.preferredCenterCoordinatesInMap.push(null);
            }
        }
        // recalculate in and connect caches:
        game.location_in = new Array(game.locations.length);
        game.location_connects = new Array(game.locations.length);
        for (var idx_l1 = 0; idx_l1 < game.locations.length; idx_l1++) {
            game.location_in[idx_l1] = new Array(game.locations.length);
            game.location_connects[idx_l1] = new Array(game.locations.length);
            for (var idx_l2 = 0; idx_l2 < game.locations.length; idx_l2++) {
                game.location_in[idx_l1][idx_l2] = false;
                game.location_connects[idx_l1][idx_l2] = false;
            }
        }
        for (var idx_l1 = 0; idx_l1 < game.locations.length; idx_l1++) {
            var l1 = game.locations[idx_l1];
            for (var idx_l2 = 0; idx_l2 < game.locations.length; idx_l2++) {
                var l2 = game.locations[idx_l2];
                if (l1 == l2)
                    continue;
                game.location_in[idx_l1][idx_l2] = true;
                for (var mapIdx1 = 0; mapIdx1 < l1.maps.length; mapIdx1++) {
                    var map = l1.maps[mapIdx1];
                    var mapIdx2 = l2.maps.indexOf(map);
                    if (mapIdx2 == -1) {
                        // console.log(this.locations[idx_l1].name + "["+this.locations[idx_l1].sort+"] not in " + this.locations[idx_l2].name + "["+this.locations[idx_l2].sort+"] due to map mismatch!");
                        game.location_in[idx_l1][idx_l2] = false;
                        break;
                    }
                    for (var i = 0; i < l1.mapOccupancyMaps[mapIdx1].length; i++) {
                        if (l1.mapOccupancyMaps[mapIdx1][i] &&
                            !l2.mapOccupancyMaps[mapIdx2][i]) {
                            game.location_in[idx_l1][idx_l2] = false;
                            //							if (this.locations[idx_l1].name == null) {
                            //								console.log(this.locations[idx_l1].name + "["+this.locations[idx_l1].sort+"] not in " + this.locations[idx_l2].name + "["+this.locations[idx_l2].sort+"] due to coordinate! " + 
                            //											i + " in map " + map.name + " (" + l1.mapOccupancyMaps[mapIdx1].length + " - " + l2.mapOccupancyMaps[mapIdx2].length + ")");
                            //							}
                            break;
                        }
                    }
                    if (!game.location_in[idx_l1][idx_l2])
                        break;
                }
            }
        }
        for (var idx_l1 = 0; idx_l1 < game.locations.length; idx_l1++) {
            //			console.log("idx: " + idx_l1);
            var l1 = game.locations[idx_l1];
            for (var idx_l2 = 0; idx_l2 < game.locations.length; idx_l2++) {
                var l2 = game.locations[idx_l2];
                if (l1 == l2)
                    continue;
                if (game.location_in[idx_l1][idx_l2] ||
                    game.location_in[idx_l2][idx_l1])
                    continue;
                // automatically generate the "location.connects" terms
                game.location_connects[idx_l1][idx_l2] = false;
                for (var mapIdx1 = 0; mapIdx1 < l1.maps.length; mapIdx1++) {
                    var map = l1.maps[mapIdx1];
                    var mapIdx2 = l2.maps.indexOf(map);
                    if (mapIdx2 == -1)
                        break;
                    for (var i = 0; i < l1.mapOccupancyMaps[mapIdx1].length; i++) {
                        var i2_l = [i - 1, i + 1, i - map.width, i + map.width];
                        for (var j = 0; j < i2_l.length; j++) {
                            var i2 = i2_l[j];
                            if (i2 >= 0 && i2 < map.width * map.height &&
                                l1.mapOccupancyMaps[mapIdx1][i] &&
                                l2.mapOccupancyMaps[mapIdx2][i2]) {
                                game.location_connects[idx_l1][idx_l2] = true;
                                break;
                            }
                        }
                    }
                }
            }
        }
        game.map_location_names = [];
        for (var _f = 0, _g = game.maps; _f < _g.length; _f++) {
            var map = _g[_f];
            var location_names = [];
            for (var y = 0; y < map.height; y++) {
                for (var x = 0; x < map.width; x++) {
                    var l = game.getAILocationTileCoordinate(map, x, y);
                    if (l == null) {
                        location_names.push(null);
                    }
                    else {
                        location_names.push(l.id);
                    }
                }
                //				console.log(location_names.slice(location_names.length-map.width));
            }
            game.map_location_names.push(location_names);
        }
        for (var _h = 0, _j = getElementChildrenByTag(xml, "location_connects"); _h < _j.length; _h++) {
            var connection_xml = _j[_h];
            var l1_name = connection_xml.getAttribute("l1");
            var l2_name = connection_xml.getAttribute("l2");
            game.additional_location_connects.push([l1_name, l2_name]);
        }
        for (var _k = 0, _l = game.additional_location_connects; _k < _l.length; _k++) {
            var _m = _l[_k], l1_name = _m[0], l2_name = _m[1];
            var l1_idx = game.locations.indexOf(game.getAILocationByID(l1_name));
            var l2_idx = game.locations.indexOf(game.getAILocationByID(l2_name));
            game.location_connects[l1_idx][l2_idx] = true;
            game.location_connects[l2_idx][l1_idx] = true;
        }
        for (var _o = 0, _p = getElementChildrenByTag(xml, "preferred_center"); _o < _p.length; _o++) {
            var preferred_center_xml = _p[_o];
            // <preferred_center id="tardis8" map=="Tardis 8" x="720" y="120"/>
            var l_id = preferred_center_xml.getAttribute("id");
            var map_name = preferred_center_xml.getAttribute("map");
            var x = Number(preferred_center_xml.getAttribute("x"));
            var y = Number(preferred_center_xml.getAttribute("y"));
            for (var _q = 0, _r = game.locations; _q < _r.length; _q++) {
                var l = _r[_q];
                if (l.id == l_id) {
                    for (var i = 0; i < l.maps.length; i++) {
                        if (l.maps[i].name == map_name) {
                            l.preferredCenterCoordinatesInMap[i] = [x, y];
                        }
                    }
                }
            }
        }
    };
    return AILocation;
}());
