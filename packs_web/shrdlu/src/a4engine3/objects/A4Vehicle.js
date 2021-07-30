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
var A4Vehicle = /** @class */ (function (_super) {
    __extends(A4Vehicle, _super);
    function A4Vehicle(name, sort) {
        var _this = _super.call(this, name, sort) || this;
        // attributes:
        _this.hp = 10;
        _this.max_hp = 10;
        _this.magicImmune = false;
        _this.load = [];
        _this.currentAnimation = A4_ANIMATION_IDLE_RIGHT;
        _this.walkingCounter = 0;
        return _this;
    }
    A4Vehicle.prototype.loadObjectAdditionalContent = function (xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object) {
        _super.prototype.loadObjectAdditionalContent.call(this, xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object);
        var l = getElementChildrenByTag(xml, "load");
        if (l.length > 0) {
            objectsToRevisit_xml.push(xml);
            objsctsToRevisit_object.push(this);
        }
    };
    A4Vehicle.prototype.loadObjectAttribute = function (attribute_xml) {
        if (_super.prototype.loadObjectAttribute.call(this, attribute_xml))
            return true;
        var a_name = attribute_xml.getAttribute("name");
        if (a_name == "hp") {
            this.hp = Number(attribute_xml.getAttribute("value"));
            this.max_hp = this.hp;
            return true;
        }
        else if (a_name == "max_hp") {
            this.max_hp = Number(attribute_xml.getAttribute("value"));
            return true;
        }
        else if (a_name == "magicImmune") {
            this.magicImmune = false;
            if (attribute_xml.getAttribute("value") == "true")
                this.magicImmune = true;
            return true;
        }
        return false;
    };
    A4Vehicle.prototype.revisitObject = function (xml, game) {
        var l = getElementChildrenByTag(xml, "load");
        for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
            var n = l_1[_i];
            var o_ID = n.getAttribute("ID");
            var tmp = game.findObjectByID(o_ID);
            if (tmp == null) {
                console.error("Revisiting A4Vehicle, and cannot find object with ID " + o_ID);
            }
            else {
                var o = tmp[tmp.length - 1];
                this.load.push(o);
            }
        }
    };
    A4Vehicle.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("hp", this.hp) + "\n";
        xmlString += this.saveObjectAttributeToXML("max_hp", this.max_hp) + "\n";
        xmlString += this.saveObjectAttributeToXML("magicImmune", this.magicImmune) + "\n";
        for (var _i = 0, _a = this.load; _i < _a.length; _i++) {
            var o = _a[_i];
            xmlString += "<load ID=\"" + o.ID + "\"/>\n";
        }
        return xmlString;
    };
    A4Vehicle.prototype.update = function (game) {
        var ret = _super.prototype.update.call(this, game);
        // update the positions/maps of the load:
        for (var _i = 0, _a = this.load; _i < _a.length; _i++) {
            var o = _a[_i];
            o.x = this.x;
            o.y = this.y;
            o.map = this.map;
        }
        var max_movement_pixels_requested = 0;
        if (this.hp <= 0)
            this.state = A4CHARACTER_STATE_DYING;
        // direction control:
        for (var i = 0; i < A4_NDIRECTIONS; i++) {
            if (this.direction_command_received_this_cycle[i]) {
                this.continuous_direction_command_timers[i]++;
            }
            else {
                this.continuous_direction_command_timers[i] = 0;
            }
        }
        if (this.state == A4CHARACTER_STATE_IDLE) {
            var most_recent_viable_walk_command = A4_DIRECTION_NONE;
            var timer = 0;
            for (var i = 0; i < A4_NDIRECTIONS; i++) {
                if (this.direction_command_received_this_cycle[i]) { //} && this.canMove(i, false)) {
                    if (most_recent_viable_walk_command == A4_DIRECTION_NONE ||
                        this.continuous_direction_command_timers[i] < timer) {
                        most_recent_viable_walk_command = i;
                        timer = this.continuous_direction_command_timers[i];
                    }
                }
            }
            if (most_recent_viable_walk_command != A4_DIRECTION_NONE) {
                this.state = A4CHARACTER_STATE_WALKING;
                this.direction = most_recent_viable_walk_command;
                max_movement_pixels_requested = this.continuous_direction_command_max_movement[most_recent_viable_walk_command];
            }
        }
        for (var i = 0; i < A4_NDIRECTIONS; i++)
            this.direction_command_received_this_cycle[i] = false;
        if (this.state != this.previousState || this.direction != this.previousDirection)
            this.stateCycle = 0;
        this.previousState = this.state;
        this.previousDirection = this.direction;
        switch (this.state) {
            case A4CHARACTER_STATE_IDLE:
                if (this.stateCycle == 0) {
                    if (this.animations[A4_ANIMATION_IDLE_LEFT + this.direction] != null) {
                        this.currentAnimation = A4_ANIMATION_IDLE_LEFT + this.direction;
                    }
                    else {
                        this.currentAnimation = A4_ANIMATION_IDLE;
                    }
                    this.animations[this.currentAnimation].reset();
                }
                else {
                    //                    this.animations[this.currentAnimation].update();
                }
                this.stateCycle++;
                break;
            case A4CHARACTER_STATE_WALKING:
                {
                    if (this.stateCycle == 0) {
                        if (this.animations[A4_ANIMATION_MOVING_LEFT + this.direction] != null) {
                            this.currentAnimation = A4_ANIMATION_MOVING_LEFT + this.direction;
                        }
                        else if (this.animations[A4_ANIMATION_MOVING] != null) {
                            this.currentAnimation = A4_ANIMATION_MOVING;
                        }
                        else if (this.animations[A4_ANIMATION_IDLE_LEFT + this.direction] != null) {
                            this.currentAnimation = A4_ANIMATION_IDLE_LEFT + this.direction;
                        }
                        else {
                            this.currentAnimation = A4_ANIMATION_IDLE;
                        }
                        this.animations[this.currentAnimation].reset();
                    }
                    else {
                        //                    this.animations[this.currentAnimation].update();
                    }
                    if ((this.x % this.map.tileWidth == 0) && (this.y % this.map.tileHeight == 0)) {
                        if (!this.canMove(this.direction, false) || (this.y <= 0 && this.direction == A4_DIRECTION_UP)) {
                            this.state = A4CHARACTER_STATE_IDLE;
                            this.currentAnimation = A4_ANIMATION_IDLE_LEFT + this.direction;
                            this.animations[this.currentAnimation].reset();
                            // check if we are pushing against the edge of a map with a "bridge":
                            var bridge = null;
                            bridge = this.checkIfPushingAgainstMapEdgeBridge(this.direction);
                            if (bridge != null) {
                                // teleport!
                                var target = bridge.linkedTo.findAvailableTargetLocation(this, this.map.tileWidth, this.map.tileHeight);
                                if (target != null) {
                                    game.requestWarp(this, bridge.linkedTo.map, target[0], target[1]);
                                }
                                else {
                                    if (this.load.indexOf(game.currentPlayer) != -1) {
                                        game.addMessage("Something is blocking the way!");
                                    }
                                }
                            }
                            game.checkCustomVehicleCollisionEvents(this);
                            break;
                        }
                    }
                    this.stateCycle++;
                    // the following kind of messy code just makes characters walk at the proper speed
                    // it follows the idea of Bresenham's algorithms for proportionally scaling the speed of
                    // the characters without using any floating point calculations.
                    // it also makes the character move sideways a bit, if they need to align to fit through a corridor
                    var step = game.tileWidth;
                    if (this.direction == A4_DIRECTION_UP || this.direction == A4_DIRECTION_DOWN)
                        step = game.tileHeight;
                    //let bridge:A4MapBridge = null;
                    while (this.walkingCounter <= step) {
                        var dir = this.direction;
                        this.x += direction_x_inc[dir];
                        this.y += direction_y_inc[dir];
                        this.walkingCounter += this.getWalkSpeed();
                        // pixelsMoved++;
                        if ((this.x % game.tileWidth) == 0 && (this.y % game.tileHeight) == 0) {
                            this.state = A4CHARACTER_STATE_IDLE;
                            this.walkingCounter = 0;
                            /*
                            bridge = this.map.getBridge(this.x+this.getPixelWidth()/2,this.y+this.getPixelHeight()/2);
                            if (bridge!=null) {
                                // if we enter a bridge, but it's not with the first pixel we moved, then stop and do not go through the bridfge,
                                // to give the AI a chance to decide whether to go through the bridge or not
                                if (pixelsMoved>1) {
                                    this.x -= direction_x_inc[dir];
                                    this.y -= direction_y_inc[dir];
                                    bridge = null;
                                }
                                break;
                            }
                            */
                        }
                        // walk in blocks of a tile wide:
                        if (direction_x_inc[dir] != 0 && (this.x % game.tileWidth) == 0) {
                            this.walkingCounter = 0;
                            break;
                        }
                        if (direction_y_inc[dir] != 0 && (this.y % game.tileHeight) == 0) {
                            this.walkingCounter = 0;
                            break;
                        }
                        if (max_movement_pixels_requested > 0) {
                            max_movement_pixels_requested--;
                            if (max_movement_pixels_requested <= 0)
                                break;
                        }
                        if ((this.x % game.tileWidth) == 0 && (this.y % game.tileHeight) == 0)
                            break;
                    }
                    if (this.walkingCounter >= step)
                        this.walkingCounter -= step;
                    /*
                    if (bridge!=null) {
                        // teleport!
                        let target:[number,number] = bridge.linkedTo.findAvailableTargetLocation(this, this.map.tileWidth, this.map.tileHeight);
                        if (target!=null) {
                            game.requestWarp(this, bridge.linkedTo.map, target[0], target[1]);
                        } else {
                            if (this.load.indexOf(game.currentPlayer)!=-1)
                                game.addMessage("Something is blocking the way!");
                        }
                    }
                    */
                    break;
                }
            case A4CHARACTER_STATE_DYING:
                if (this.stateCycle == 0) {
                    if (this.animations[A4_ANIMATION_DEATH_LEFT + this.direction] != null) {
                        this.currentAnimation = A4_ANIMATION_DEATH_LEFT + this.direction;
                    }
                    else if (this.animations[A4_ANIMATION_DEATH] != null) {
                        this.currentAnimation = A4_ANIMATION_DEATH;
                    }
                    else if (this.animations[A4_ANIMATION_IDLE_LEFT + this.direction] != null) {
                        this.currentAnimation = A4_ANIMATION_IDLE_LEFT + this.direction;
                    }
                    else {
                        this.currentAnimation = A4_ANIMATION_IDLE;
                    }
                    this.animations[this.currentAnimation].reset();
                }
                else {
                    //                    this.animations[this.currentAnimation].update();
                }
                this.stateCycle++;
                if (this.stateCycle >= this.getWalkSpeed()) {
                    // drop the load of characters:
                    var l = []; // we need "l" to avoic concurrent modifications of this.load
                    for (var _b = 0, _c = this.load; _b < _c.length; _b++) {
                        var o = _c[_b];
                        l.push(o);
                        game.requestWarp(o, this.map, this.x, this.y); //, o.layer);
                    }
                    this.load = [];
                    for (var _d = 0, l_2 = l; _d < l_2.length; _d++) {
                        var o = l_2[_d];
                        o.disembark();
                    }
                    return false;
                }
                break;
        }
        return ret;
    };
    A4Vehicle.prototype.issueCommand = function (command, argument, direction, target, game) {
        if (this.state == A4CHARACTER_STATE_IN_VEHICLE) {
            if (command != A4CHARACTER_COMMAND_TAKE)
                return;
        }
        else {
            if (this.state != A4CHARACTER_STATE_IDLE)
                return;
        }
        switch (command) {
            case A4CHARACTER_COMMAND_WALK:
            case A4CHARACTER_COMMAND_INTERACT:
                this.direction_command_received_this_cycle[direction] = true;
                this.continuous_direction_command_max_movement[direction] = argument;
                break;
        }
    };
    A4Vehicle.prototype.embark = function (l) {
        this.load.push(l);
    };
    A4Vehicle.prototype.disembark = function (l) {
        var idx = this.load.indexOf(l);
        if (idx >= 0) {
            this.load.splice(idx, 1);
        }
    };
    A4Vehicle.prototype.isEmpty = function () {
        return this.load.length == 0;
    };
    A4Vehicle.prototype.isWalkable = function () { return false; }; // vehicles are not walkable, there is a special case in the collision function the maps
    // that makes them walkable to characters
    A4Vehicle.prototype.isHeavy = function () { return true; }; // this is used by pressure plates
    A4Vehicle.prototype.isVehicle = function () { return true; };
    A4Vehicle.prototype.objectRemoved = function (o) {
        _super.prototype.objectRemoved.call(this, o);
        for (var _i = 0, _a = this.load; _i < _a.length; _i++) {
            var o2 = _a[_i];
            o2.objectRemoved(o);
        }
    };
    A4Vehicle.prototype.findObjectByName = function (name) {
        for (var _i = 0, _a = this.load; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.name == name)
                return [o];
            var o2 = o.findObjectByName(name);
            if (o2 != null)
                return [o].concat(o2);
        }
        return null;
    };
    A4Vehicle.prototype.findObjectByID = function (ID) {
        for (var _i = 0, _a = this.load; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.ID == ID)
                return [o];
            var o2 = o.findObjectByID(ID);
            if (o2 != null)
                return [o].concat(o2);
        }
        return null;
    };
    A4Vehicle.prototype.warp = function (x, y, map) {
        _super.prototype.warp.call(this, x, y, map);
        for (var _i = 0, _a = this.load; _i < _a.length; _i++) {
            var load = _a[_i];
            load.warp(x, y, map);
        }
    };
    return A4Vehicle;
}(A4WalkingObject));
