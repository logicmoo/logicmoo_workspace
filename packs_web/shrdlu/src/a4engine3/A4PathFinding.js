var NAVIGATION_BUFFER_WALKABLE = 0;
var NAVIGATION_BUFFER_NOT_WALKABLE = 1;
var NAVIGATION_BUFFER_BRIDGE = 2;
var PERCEIVED_ACTION_ACTIVATION = 100;
var PERCEIVED_WARP_ACTIVATION = 200;
var PFTarget = /** @class */ (function () {
    function PFTarget() {
    }
    return PFTarget;
}());
var A4PathFinding = /** @class */ (function () {
    function A4PathFinding(c) {
        // navigation perception buffer (this is public, since it will be used by all the behaviors):
        this.navigationBuffer_lastUpdated = -1;
        this.navigationBuffer = null;
        this.navigationBuffer_bridges = null;
        this.navigationBuffer_size = 0;
        this.navigationBuffer_mapWidth = 0;
        this.navigationBuffer_x = 0;
        this.navigationBuffer_y = 0;
        this.navigationBuffer_map = null;
        // pathfinding: all in tile coordinates
        this.pathfinding_result_x = -1;
        this.pathfinding_result_y = -1;
        this.pathfinding_result_priority = -1;
        this.pathfinding_result_offset_x = 0;
        this.pathfinding_result_offset_y = 0;
        this.pathfinding_targets = [];
        this.pathfinding_closed = null;
        this.pathfinding_open = null; // internal buffers for pathfinding
        this.pathFinding_lastUpdated = -1;
        this.pathFinding_iterations = 0; // for debugging purposes
        this.doorsNotToOpenWhileWalking = [];
        this.period = 2; // the AI will only run once each period cycles
        this.cycle = 0; // current cycle
        this.sightRadius = 5;
        this.character = null;
        this.lastPerceptionCycle = -1;
        // some values cached for efficiency:
        this.tileWidth = 0;
        this.tileHeight = 0;
        this.object_perception_buffer = [];
        this.long_term_object_perception_buffer = []; // this stores objects of the "familiar" maps
        this.maps_familiar_with = [];
        this.map2mapNames = null;
        this.map2mapPaths = null;
        this.canDriveVehicles = false;
        this.character = c;
    }
    A4PathFinding.prototype.update = function (game) {
        if (this.character.isIdle()) {
            // pathfinding:
            var c = this.navigationCycle(game);
            if (c != null)
                this.character.issueCommand(c, game);
        }
        this.pathfinding_targets = [];
        this.cycle++;
    };
    A4PathFinding.prototype.perception = function (game) {
        if (this.lastPerceptionCycle != -1 &&
            this.lastPerceptionCycle + this.period > this.cycle)
            return;
        this.lastPerceptionCycle = this.cycle;
        var map = this.character.map;
        this.tileWidth = map.getTileWidth();
        this.tileHeight = map.getTileHeight();
        var tx = Math.floor(this.character.x / this.tileWidth);
        var ty = Math.floor(this.character.y / this.tileHeight);
        var perception_x0 = this.character.x - this.tileWidth * this.sightRadius;
        var perception_y0 = this.character.y - this.tileHeight * this.sightRadius;
        var perception_x1 = this.character.x + this.character.getPixelWidth() + this.tileWidth * this.sightRadius;
        var perception_y1 = this.character.y + this.character.getPixelHeight() + this.tileHeight * this.sightRadius;
        var region = map.visibilityRegion(tx, ty);
        this.object_perception_buffer = map.getAllObjectsInRegionPlusDoorsAndObstacles(perception_x0, perception_y0, perception_x1 - perception_x0, perception_y1 - perception_y0, region);
    };
    A4PathFinding.prototype.navigationCycle = function (game) {
        var subject = this.character;
        if (this.character.isInVehicle()) {
            if (this.canDriveVehicles) {
                subject = this.character.vehicle;
            }
            else {
                return;
            }
        }
        if (subject == null)
            return;
        if (subject.map != this.navigationBuffer_map ||
            game.requestedWarp(this.character)) {
            this.pathfinding_result_x = -1;
            this.pathfinding_result_y = -1;
            this.pathfinding_result_priority = 0;
            return;
        }
        var highest_priority_target = -1;
        var highest_priority = 0;
        if (this.pathfinding_targets.length == 0)
            return null;
        for (var i = 0; i < this.pathfinding_targets.length; i++) {
            if (highest_priority_target == -1 ||
                this.pathfinding_targets[i].priority > highest_priority) {
                highest_priority_target = i;
                highest_priority = this.pathfinding_targets[i].priority;
            }
        }
        var pathfind = false;
        if (this.pathfinding_result_x == -1 ||
            (this.cycle - this.pathFinding_lastUpdated) > subject.getWalkSpeed()) {
            pathfind = true;
        }
        if (pathfind) {
            this.pathfinding_result_x = -1;
            this.pathfinding_result_y = -1;
            this.pathfinding_result_priority = 0;
            if (this.navigationBuffer_lastUpdated < this.cycle)
                this.updateNavigationPerceptionBuffer(game, true);
            this.pathFinding(subject);
        }
        if (this.pathfinding_result_x != -1) {
            var pixelx = subject.x;
            var pixely = subject.y;
            //let pixelx1:number = subject.x + subject.getPixelWidth();
            //let pixely1:number = subject.y + subject.getPixelHeight();
            var pixel_target_x = this.pathfinding_result_x * this.tileWidth;
            var pixel_target_y = this.pathfinding_result_y * this.tileHeight;
            this.pathfinding_result_offset_x = pixel_target_x - pixelx;
            this.pathfinding_result_offset_y = pixel_target_y - pixely;
            var abs_diff_x = (this.pathfinding_result_offset_x < 0 ? -this.pathfinding_result_offset_x : this.pathfinding_result_offset_x);
            var abs_diff_y = (this.pathfinding_result_offset_y < 0 ? -this.pathfinding_result_offset_y : this.pathfinding_result_offset_y);
            //            console.log("pixelx: " + pixelx + ", pixely: " + pixely);
            //            console.log("abs_diff_x: " + abs_diff_x + ", abs_diff_y: " + abs_diff_y);
            //            console.log("this.pathfinding_result_offset_x: " + this.pathfinding_result_offset_x + ", this.pathfinding_result_offset_y: " + this.pathfinding_result_offset_y);
            if (abs_diff_x > 0 && (abs_diff_x <= abs_diff_y || abs_diff_y == 0)) {
                if (this.pathfinding_result_offset_x > 0) {
                    return new A4CharacterCommand(A4CHARACTER_COMMAND_WALK, abs_diff_x, A4_DIRECTION_RIGHT, null, null, this.pathfinding_result_priority);
                }
                else {
                    return new A4CharacterCommand(A4CHARACTER_COMMAND_WALK, abs_diff_x, A4_DIRECTION_LEFT, null, null, this.pathfinding_result_priority);
                }
            }
            else if (abs_diff_y > 0) {
                if (this.pathfinding_result_offset_y > 0) {
                    return new A4CharacterCommand(A4CHARACTER_COMMAND_WALK, abs_diff_y, A4_DIRECTION_DOWN, null, null, this.pathfinding_result_priority);
                }
                else {
                    return new A4CharacterCommand(A4CHARACTER_COMMAND_WALK, abs_diff_y, A4_DIRECTION_UP, null, null, this.pathfinding_result_priority);
                }
            }
            else {
                // check if the goal is underneath us and action is TAKE
                if ((this.pathfinding_targets[highest_priority_target].action == A4CHARACTER_COMMAND_TAKE ||
                    this.pathfinding_targets[highest_priority_target].action == A4CHARACTER_COMMAND_INTERACT) &&
                    this.pathfinding_targets[highest_priority_target].x0 == pixelx &&
                    this.pathfinding_targets[highest_priority_target].y0 == pixely) {
                    return new A4CharacterCommand(this.pathfinding_targets[highest_priority_target].action, 0, 0, null, null, this.pathfinding_result_priority);
                }
                // we are at the goal, stay!
                return new A4CharacterCommand(A4CHARACTER_COMMAND_IDLE, 0, 0, null, null, this.pathfinding_result_priority);
            }
        }
        return null;
    };
    A4PathFinding.prototype.addBridgeToLongTermMemory = function (o) {
        this.long_term_object_perception_buffer.push(o);
    };
    A4PathFinding.prototype.canSeeObject = function (object, game) {
        this.perception(game);
        if (this.object_perception_buffer.indexOf(object) != -1)
            return true;
        if (this.long_term_object_perception_buffer.indexOf(object) != -1)
            return true;
        return false;
    };
    A4PathFinding.prototype.updateNavigationPerceptionBuffer = function (game, force) {
        var subject = this.character;
        if (this.character.isInVehicle()) {
            if (this.canDriveVehicles) {
                subject = this.character.vehicle;
            }
            else {
                return;
            }
        }
        if (subject == null)
            return;
        if (!force && this.navigationBuffer != null && this.navigationBuffer_lastUpdated > this.cycle - this.period)
            return;
        // update the perceived objects:
        this.perception(game);
        if (this.navigationBuffer == null) {
            this.navigationBuffer_size = Math.floor(this.sightRadius * 2 + subject.getPixelWidth() / subject.map.getTileWidth());
            this.navigationBuffer = new Array(this.navigationBuffer_size * this.navigationBuffer_size);
            this.navigationBuffer_bridges = new Array(this.navigationBuffer_size * this.navigationBuffer_size);
        }
        var map = subject.map;
        this.navigationBuffer_map = map;
        this.navigationBuffer_mapWidth = map.width;
        var cx = Math.floor((subject.x + subject.getPixelWidth() / 2) / this.tileWidth);
        var cy = Math.floor((subject.y + subject.getPixelHeight() / 2) / this.tileHeight);
        this.navigationBuffer_x = cx - Math.floor(this.sightRadius + (subject.getPixelWidth() / 2) / this.tileWidth);
        this.navigationBuffer_y = cy - Math.floor(this.sightRadius + (subject.getPixelHeight() / 2) / this.tileHeight);
        for (var i = 0; i < this.navigationBuffer_size; i++) {
            for (var j = 0; j < this.navigationBuffer_size; j++) {
                this.navigationBuffer_bridges[j + i * this.navigationBuffer_size] = null;
                //                if (map.walkableOnlyBackground((this.navigationBuffer_x+j)*this.tileWidth,(this.navigationBuffer_y+i)*this.tileHeight, character_tileWidth*this.tileWidth, character_tileHeight*this.tileHeight, subject)) {
                if (map.walkableOnlyBackground((this.navigationBuffer_x + j) * this.tileWidth, (this.navigationBuffer_y + i) * this.tileHeight, this.tileWidth, this.tileHeight, subject)) {
                    this.navigationBuffer[j + i * this.navigationBuffer_size] = NAVIGATION_BUFFER_WALKABLE;
                }
                else {
                    this.navigationBuffer[j + i * this.navigationBuffer_size] = NAVIGATION_BUFFER_NOT_WALKABLE;
                }
            }
        }
        // add objects:
        for (var _i = 0, _a = this.object_perception_buffer; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o != this.character && !o.isWalkable() && o != this.character.vehicle) {
                // if it's a door, and we have the key, then ignore the door:
                var add = true;
                if ((o instanceof A4Door) &&
                    this.doorsNotToOpenWhileWalking.indexOf(o.doorID) == -1 &&
                    (o.canOpen(this.character, game) ||
                        o.automatic))
                    add = false;
                if (add) {
                    var x0 = Math.floor(o.x / this.tileWidth) - this.navigationBuffer_x;
                    var y0 = Math.floor(o.y / this.tileHeight) - this.navigationBuffer_y;
                    var x1 = Math.floor((o.x + o.getPixelWidth() - 1) / this.tileWidth) - this.navigationBuffer_x;
                    var y1 = Math.floor((o.y + o.getPixelHeight() - 1) / this.tileHeight) - this.navigationBuffer_y;
                    //                x0 -= (character_tileWidth - 1);
                    //                y0 -= (character_tileHeight - 1);
                    for (var y = y0; y <= y1; y++) {
                        if (y >= 0 && y < this.navigationBuffer_size) {
                            for (var x = x0; x <= x1; x++) {
                                if (x >= 0 && x < this.navigationBuffer_size) {
                                    this.navigationBuffer[x + y * this.navigationBuffer_size] = NAVIGATION_BUFFER_NOT_WALKABLE;
                                }
                            }
                        }
                    }
                }
            }
        }
        // add bridges:
        for (var _b = 0, _c = map.bridges; _b < _c.length; _b++) {
            var b = _c[_b];
            var x0 = Math.floor(b.x / this.tileWidth) - this.navigationBuffer_x;
            var y0 = Math.floor(b.y / this.tileHeight) - this.navigationBuffer_y;
            var x1 = Math.floor((b.x + b.width - 1) / this.tileWidth) - this.navigationBuffer_x;
            var y1 = Math.floor((b.y + b.height - 1) / this.tileHeight) - this.navigationBuffer_y;
            //            x0 -= (character_tileWidth - 1);
            //            y0 -= (character_tileHeight - 1);
            for (var y = y0; y <= y1; y++) {
                if (y >= 0 && y < this.navigationBuffer_size) {
                    for (var x = x0; x <= x1; x++) {
                        if (x >= 0 && x < this.navigationBuffer_size) {
                            if (this.navigationBuffer[x + y * this.navigationBuffer_size] == NAVIGATION_BUFFER_WALKABLE) {
                                this.navigationBuffer[x + y * this.navigationBuffer_size] = NAVIGATION_BUFFER_BRIDGE;
                                this.navigationBuffer_bridges[x + y * this.navigationBuffer_size] = b;
                            }
                        }
                    }
                }
            }
        }
        this.navigationBuffer_lastUpdated = this.cycle;
        /*
        console.log("Navigation buffer:");
        for(let i:number = 0;i<this.navigationBuffer_size;i++) {
            let line:string = "";
            for(let j:number = 0;j<this.navigationBuffer_size;j++) {
                if (this.navigationBuffer[j+i*this.navigationBuffer_size] == NAVIGATION_BUFFER_WALKABLE) {
                    line += ".";
                } else {
                    line += "X";
                }
            }
            console.log(line + " -> " + i);
        }
        */
    };
    // pathfinding:
    A4PathFinding.prototype.addPFTargetObject = function (action, priority, flee, target, game) {
        if (this.navigationBuffer_lastUpdated == -1 ||
            this.navigationBuffer_lastUpdated <= this.cycle - this.period) {
            this.updateNavigationPerceptionBuffer(game, false);
        }
        if (target.map != this.navigationBuffer_map) {
            this.addPFTargetMap(action, priority, flee, target.map, game);
            return;
        }
        var x0 = target.x;
        var y0 = target.y;
        var x1 = (target.x + target.getPixelWidth());
        var y1 = (target.y + target.getPixelHeight());
        for (var _i = 0, _a = this.pathfinding_targets; _i < _a.length; _i++) {
            var pft = _a[_i];
            if (pft.flee)
                continue;
            if (pft.x0 == x0 && pft.y0 == y0 &&
                pft.x1 == x1 && pft.y1 == y1) {
                if (pft.priority <= priority) {
                    pft.action = action;
                    pft.priority = priority;
                    pft.target = target;
                }
                return;
            }
        }
        var pft2 = new PFTarget();
        pft2.x0 = x0;
        pft2.y0 = y0;
        pft2.x1 = x1;
        pft2.y1 = y1;
        pft2.action = action;
        pft2.priority = priority;
        pft2.flee = flee;
        pft2.target = target;
        this.pathfinding_targets.push(pft2);
    };
    // pathfinding:
    A4PathFinding.prototype.addPFTarget = function (x0, y0, x1, y1, map, game, action, priority, flee, target) {
        if (this.navigationBuffer_lastUpdated == -1 ||
            this.navigationBuffer_lastUpdated <= this.cycle - this.period) {
            this.updateNavigationPerceptionBuffer(game, false);
        }
        if (map != this.navigationBuffer_map) {
            this.addPFTargetMap(action, priority, flee, map, game);
            return;
        }
        for (var _i = 0, _a = this.pathfinding_targets; _i < _a.length; _i++) {
            var pft = _a[_i];
            if (pft.flee)
                continue;
            if (pft.x0 == x0 && pft.y0 == y0 &&
                pft.x1 == x1 && pft.y1 == y1) {
                if (pft.priority <= priority) {
                    pft.action = action;
                    pft.priority = priority;
                    pft.target = target;
                }
                return;
            }
        }
        var pft2 = new PFTarget();
        pft2.x0 = x0;
        pft2.y0 = y0;
        pft2.x1 = x1;
        pft2.y1 = y1;
        pft2.action = action;
        pft2.priority = priority;
        pft2.flee = flee;
        pft2.target = target;
        this.pathfinding_targets.push(pft2);
    };
    A4PathFinding.prototype.addPFTargetMap = function (action, priority, flee, map, game) {
        // different map, just target the map:
        var targetMapName = map.name;
        if (this.navigationBuffer_map == null)
            return;
        if (this.map2mapPaths != null) {
            var idx1 = game.getMapIndex(this.navigationBuffer_map.name);
            var idx2 = game.getMapIndex(targetMapName);
            if (idx1 >= 0 && idx2 >= 0 && this.map2mapPaths[idx1][idx2] != null) {
                targetMapName = this.map2mapPaths[idx1][idx2];
            }
        }
        for (var _i = 0, _a = this.long_term_object_perception_buffer; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o instanceof A4MapBridge) {
                var b = o;
                if (b.map == this.navigationBuffer_map &&
                    b.linkedTo.map.name == targetMapName) {
                    this.addPFTarget(b.x, b.y, b.x + b.width, b.y + b.height, this.navigationBuffer_map, game, A4CHARACTER_COMMAND_IDLE, priority, flee, null);
                }
            }
            else if (o instanceof ShrdluAirlockDoor) {
                var b = o;
                if (b.map == this.navigationBuffer_map &&
                    b.targetMap == targetMapName) {
                    this.addPFTarget(b.x, b.y, b.x + b.getPixelWidth(), b.y + b.getPixelHeight(), this.navigationBuffer_map, game, A4CHARACTER_COMMAND_IDLE, priority, flee, null);
                }
            }
        }
    };
    A4PathFinding.prototype.pathFinding = function (subject) {
        var _a;
        if (this.pathfinding_targets.length == 0)
            return false;
        if (isNaN(this.navigationBuffer_x))
            return false;
        var otx;
        var oty;
        // compute the origin tile cordinates (from the center of the top-left tile of the sprite):
        var pw = subject.getPixelWidth();
        var ph = subject.getPixelHeight();
        var tw = Math.floor(pw / this.tileWidth);
        var th = Math.floor(ph / this.tileHeight);
        if (pw > this.tileWidth) {
            otx = Math.floor((subject.x + this.tileWidth / 2) / this.tileWidth);
        }
        else {
            otx = Math.floor((subject.x + pw / 2) / this.tileWidth);
        }
        if (ph > this.tileHeight) {
            oty = Math.floor((subject.y + this.tileHeight / 2) / this.tileHeight);
        }
        else {
            oty = Math.floor((subject.y + ph / 2) / this.tileHeight);
        }
        if (otx < this.navigationBuffer_x || otx > this.navigationBuffer_x + this.navigationBuffer_size ||
            oty < this.navigationBuffer_y || oty > this.navigationBuffer_y + this.navigationBuffer_size) {
            console.error("A4PathFinding: character is out of the navigation buffer!!!\n");
            return false;
        }
        var start = (otx - this.navigationBuffer_x) + (oty - this.navigationBuffer_y) * this.navigationBuffer_size;
        var openInsertPosition = 0;
        var openRemovePosition = 0;
        var current, currentx, currenty;
        var next;
        var i;
        var bestScore = undefined;
        var bestPriority = undefined;
        var bestTarget = start;
        var score = 0, priority = 0;
        var size_sq = this.navigationBuffer_size * this.navigationBuffer_size;
        // initialize open/closed lists:
        if (this.pathfinding_open == null) {
            this.pathfinding_open = new Array(size_sq);
            this.pathfinding_closed = new Array(size_sq);
        }
        for (i = 0; i < size_sq; i++)
            this.pathfinding_closed[i] = -1;
        this.pathfinding_open[openInsertPosition++] = start;
        this.pathfinding_closed[start] = start;
        openInsertPosition = openInsertPosition % size_sq;
        // pathfinding:
        this.pathFinding_iterations = 0;
        while (openRemovePosition != openInsertPosition) {
            current = this.pathfinding_open[openRemovePosition];
            currentx = (current % this.navigationBuffer_size) + this.navigationBuffer_x;
            currenty = Math.floor(current / this.navigationBuffer_size) + this.navigationBuffer_y;
            openRemovePosition++;
            openRemovePosition = openRemovePosition % size_sq;
            _a = this.pathFindingScore(currentx, currenty, subject), score = _a[0], priority = _a[1];
            //            console.log("pfs: " + currentx +", " + currenty + " -> " + score);
            if ((bestPriority == undefined || priority > bestPriority) ||
                (priority == bestPriority && score > bestScore)) {
                bestScore = score;
                bestTarget = current;
                bestPriority = priority;
            }
            // neighbors:
            if (this.navigationBuffer[current] != NAVIGATION_BUFFER_BRIDGE || current == start) {
                // LEFT:
                if (currentx > this.navigationBuffer_x) {
                    next = current - 1;
                    var canWalk = true;
                    for (i = 0; i < th; i++) {
                        if (this.navigationBuffer[next + i * this.navigationBuffer_size] == NAVIGATION_BUFFER_NOT_WALKABLE) {
                            canWalk = false;
                            break;
                        }
                    }
                    if (canWalk &&
                        this.pathfinding_closed[next] == -1) {
                        this.pathfinding_open[openInsertPosition++] = next;
                        openInsertPosition = openInsertPosition % size_sq;
                        this.pathfinding_closed[next] = current; // store the parent
                    }
                }
                // RIGHT:
                if (currentx < this.navigationBuffer_x + this.navigationBuffer_size - tw) {
                    next = current + 1;
                    var canWalk = true;
                    for (i = 0; i < th; i++) {
                        if (this.navigationBuffer[next + (tw - 1) + i * this.navigationBuffer_size] == NAVIGATION_BUFFER_NOT_WALKABLE) {
                            canWalk = false;
                            break;
                        }
                    }
                    if (canWalk &&
                        this.pathfinding_closed[next] == -1) {
                        this.pathfinding_open[openInsertPosition++] = next;
                        openInsertPosition = openInsertPosition % size_sq;
                        this.pathfinding_closed[next] = current; // store the parent
                    }
                }
                // UP:
                if (currenty > this.navigationBuffer_y) {
                    next = current - this.navigationBuffer_size;
                    var canWalk = true;
                    for (i = 0; i < tw; i++) {
                        if (this.navigationBuffer[next + i] == NAVIGATION_BUFFER_NOT_WALKABLE) {
                            canWalk = false;
                            break;
                        }
                    }
                    if (canWalk &&
                        this.pathfinding_closed[next] == -1) {
                        this.pathfinding_open[openInsertPosition++] = next;
                        openInsertPosition = openInsertPosition % size_sq;
                        this.pathfinding_closed[next] = current; // store the parent
                    }
                }
                // DOWN:
                if (currenty < this.navigationBuffer_y + this.navigationBuffer_size - th) {
                    next = current + this.navigationBuffer_size;
                    var canWalk = true;
                    for (i = 0; i < tw; i++) {
                        if (this.navigationBuffer[next + (th - 1) * this.navigationBuffer_size + i] == NAVIGATION_BUFFER_NOT_WALKABLE) {
                            canWalk = false;
                            break;
                        }
                    }
                    if (canWalk &&
                        this.pathfinding_closed[next] == -1) {
                        this.pathfinding_open[openInsertPosition++] = next;
                        openInsertPosition = openInsertPosition % size_sq;
                        this.pathfinding_closed[next] = current; // store the parent
                    }
                }
            }
            this.pathFinding_iterations++;
        }
        current = bestTarget;
        while (this.pathfinding_closed[current] != -1 && this.pathfinding_closed[current] != start) {
            current = this.pathfinding_closed[current];
        }
        this.pathfinding_result_x = (current % this.navigationBuffer_size) + this.navigationBuffer_x;
        this.pathfinding_result_y = Math.floor(current / this.navigationBuffer_size) + this.navigationBuffer_y;
        this.pathfinding_result_priority = bestPriority;
        this.pathFinding_lastUpdated = this.cycle;
        return true;
    };
    A4PathFinding.prototype.pathFindingScore = function (character_x0, character_y0, subject) {
        var out_score;
        var priority;
        var bestGotoScorePriority = -1;
        var bestGotoScore = Number.MAX_VALUE;
        var bestFleeScorePriority = -1;
        var bestFleeScore = 0;
        var score = 0;
        var dx;
        var dy;
        var x0, y0; //,x1:number,y1:number;
        for (var i = 0; i < this.pathfinding_targets.length; i++) {
            if (this.pathfinding_targets[i].flee) {
                if (this.pathfinding_targets[i].priority >= bestFleeScorePriority) {
                    //                    dx = dy = 0;
                    x0 = Math.floor(this.pathfinding_targets[i].x0 / this.tileWidth);
                    y0 = Math.floor(this.pathfinding_targets[i].y0 / this.tileHeight);
                    //                    x1 = Math.floor((this.pathfinding_targets[i].x1-1)/this.tileWidth);
                    //                    y1 = Math.floor((this.pathfinding_targets[i].y1-1)/this.tileHeight);
                    //                    if (character_x1<x0) dx = x0 - character_x1;
                    //                    if (character_x0>x1) dx = character_x0 - x1;
                    //                    if (character_y1<y0) dy = y0 - character_y1;
                    //                    if (character_y0>y1) dy = character_y0 - y1;
                    dx = Math.abs(x0 - character_x0);
                    dy = Math.abs(y0 - character_y0);
                    score = dx + dy;
                    if (this.pathfinding_targets[i].priority == bestFleeScorePriority) {
                        bestFleeScore += score; // for "flee", if it's the same priority, just add the scores (to "flee" from all of them, and not just one)
                    }
                    else {
                        if (score > bestFleeScore) {
                            bestFleeScore = score;
                            bestFleeScorePriority = this.pathfinding_targets[i].priority;
                        }
                    }
                }
            }
            else {
                if (this.pathfinding_targets[i].priority >= bestGotoScorePriority) {
                    //                    dx = dy = 0;
                    x0 = Math.floor(this.pathfinding_targets[i].x0 / this.tileWidth);
                    y0 = Math.floor(this.pathfinding_targets[i].y0 / this.tileHeight);
                    //                    x1 = Math.floor((this.pathfinding_targets[i].x1-1)/this.tileWidth);
                    //                    y1 = Math.floor((this.pathfinding_targets[i].y1-1)/this.tileHeight);
                    //                    if (character_x1<x0) dx = x0 - character_x1;
                    //                    if (character_x0>x1) dx = character_x0 - x1;
                    //                    if (character_y1<y0) dy = y0 - character_y1;
                    //                    if (character_y0>y1) dy = character_y0 - y1;
                    dx = Math.abs(x0 - character_x0);
                    dy = Math.abs(y0 - character_y0);
                    score = dx + dy;
                    if (score < bestGotoScore) {
                        bestGotoScore = score;
                        bestGotoScorePriority = this.pathfinding_targets[i].priority;
                    }
                }
            }
        }
        if (bestGotoScorePriority > bestFleeScorePriority) {
            out_score = -bestGotoScore;
            priority = bestGotoScorePriority;
        }
        else if (bestGotoScorePriority < bestFleeScorePriority) {
            out_score = bestFleeScore;
            priority = bestFleeScorePriority;
        }
        else {
            out_score = bestFleeScore - bestGotoScore;
            priority = bestGotoScorePriority;
        }
        return [out_score, priority];
    };
    A4PathFinding.prototype.objectRemoved = function (o) {
        var idx = this.object_perception_buffer.indexOf(o);
        if (idx != -1)
            this.object_perception_buffer.splice(idx, 1);
        idx = this.long_term_object_perception_buffer.indexOf(o);
        if (idx != -1)
            this.long_term_object_perception_buffer.splice(idx, 1);
    };
    A4PathFinding.prototype.precomputeMap2mapPaths = function (game) {
        var map = [];
        var n = game.maps.length;
        this.map2mapNames = [];
        this.map2mapPaths = [];
        for (var i = 0; i < n; i++) {
            var row = [];
            var rowPaths = [];
            for (var j = 0; j < n; j++) {
                var linked = false;
                // see if there is a bridge or an airlock linking the two maps:
                for (var _i = 0, _a = game.maps[i].bridges; _i < _a.length; _i++) {
                    var b = _a[_i];
                    if (b.linkedTo != null &&
                        b.linkedTo.map == game.maps[j]) {
                        linked = true;
                    }
                }
                for (var _b = 0, _c = game.maps[i].objects; _b < _c.length; _b++) {
                    var o = _c[_b];
                    if (o instanceof ShrdluAirlockDoor) {
                        if (o.targetMap == game.maps[j].name) {
                            linked = true;
                        }
                    }
                }
                rowPaths.push(null);
                row.push(linked);
            }
            map.push(row);
            this.map2mapPaths.push(rowPaths);
            this.map2mapNames.push(game.maps[i].name);
        }
        for (var i = 0; i < n; i++) {
            for (var j = 0; j < n; j++) {
                if (i == j)
                    continue;
                // find a path from one map to the other:
                var path = this.map2mapPath(i, j, map);
                // cache the first step of the path in this.map2mapPaths:
                if (path != null && path.length > 1) {
                    this.map2mapPaths[i][j] = game.maps[path[1]].name;
                }
            }
        }
        console.log(map);
        console.log(this.map2mapPaths);
    };
    // BFS (since the map is very small, it's fast enough):
    A4PathFinding.prototype.map2mapPath = function (start, end, map) {
        var open = [[start]];
        var closed = [];
        while (open.length > 0) {
            var currentPath = open[0];
            var current = currentPath[currentPath.length - 1];
            if (current == end) {
                return currentPath;
            }
            open.splice(0, 1);
            closed.push(current);
            for (var i = 0; i < map.length; i++) {
                if (map[current][i] && closed.indexOf(i) == -1) {
                    open.push(currentPath.concat([i]));
                }
            }
        }
        return null;
    };
    return A4PathFinding;
}());
