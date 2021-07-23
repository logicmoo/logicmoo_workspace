// PATH FINDING: -----------------------------------------------------------------------------------------

pathToFollow = function(map, x, y, character, targetx, targety, targetmap, radius, canSwim) {
	return pathToFollowMultipleTargets(map, x, y, character, [{x:targetx, y:targety, map:targetmap}], radius, canSwim);
}


pathToFleeMultipleTargets = function(map, x, y, character, targetList, radius, canSwim) {
	var buffer = boundedPathFinding(map, x, y, character, radius, canSwim);
	var bw = radius*2+1;

	var destoffs = 0;
	var destination = null;
	var d = 0;
	var offs = 0;
	var bestCost = 0;
	for(var i = 0;i<bw;i++) {
		for(var j = 0;j<bw;j++, offs++) {
			if (buffer[offs]!=null) {
				var minimumDistance = null;
				for(var k = 0;k<targetList.length;k++) {
					var dx = targetList[k].x - (j+x-radius);
					var dy = targetList[k].y - (i+y-radius);
					var d2 = dx*dx + dy*dy;
					if (minimumDistance == null || d2<minimumDistance) minimumDistance = d2;
				}
				if (destination==null || minimumDistance>d || (minimumDistance==d && buffer[offs].cost<bestCost)) {
					destination = buffer[offs];
					destoffs = offs;
					d = minimumDistance;
					bestCost = buffer[offs].cost;
				}
			}
		}		
	}
	if (destination==null) return null;

	var path = [];
	while(destination.move!=null) {
		path.unshift(destination.move);
		destoffs = destoffs - (direction_offx[destination.move] + direction_offy[destination.move]*bw);
		destination = buffer[destoffs];
	}

	return path;
}


pathToFollowMultipleTargets = function(map, x, y, character, targetList, radius, canSwim) {
	var buffer = boundedPathFinding(map, x, y, character, radius, canSwim);
	var bridges = map.bridgesInRadius(x, y, radius);
	var bw = radius*2+1;

	var destoffs = 0;
	var destination = null;
	var d = 0;
	var offs = 0;
	var bestCost = 0;
	/*
	console.log("pathToFollowMultipleTargets... " + x + "," + y + " at " + map.ID);
	for(var k = 0;k<targetList.length;k++) {
		console.log("  " + targetList[k].name + " : " + targetList[k].x + "," + targetList[k].y + " at " + targetList[k].map )
	}
	*/
	for(var i = 0;i<bw;i++) {
		for(var j = 0;j<bw;j++, offs++) {
			if (buffer[offs]!=null) {
				var minimumDistance = null;
				var isBridge = false;
				for(var k = 0;k<targetList.length;k++) {
					if (targetList[k].map == map.ID) {
						var dx = targetList[k].x - (j+x-radius);
						var dy = targetList[k].y - (i+y-radius);
						var d2 = dx*dx + dy*dy;
						if (minimumDistance == null || d2<minimumDistance) {
							minimumDistance = d2;
							isBridge = false;
						}
					} else {
//						console.log("Target is in another map, looking for bridges...");

						for(var l = 0;l<bridges.length;l++) {
							var bridge = bridges[l];
							if (bridge.mapTarget==targetList[k].map) {
								var dx = bridge.x - (j+x-radius);
								var dy = bridge.y - (i+y-radius);
								var d2 = dx*dx + dy*dy;
//								console.log("Bridge found at distance " + d2);
								// only consider a bridge if there is no other object
								if (minimumDistance == null || 
									(d2<minimumDistance && isBridge)) {
									minimumDistance = d2;
									isBridge = true;
								}
							}
						}		
		
					}
				}

				if (minimumDistance!=null) {
					if (destination==null || minimumDistance<d || (minimumDistance==d && buffer[offs].cost<bestCost)) {
						destination = buffer[offs];
						destoffs = offs;
						d = minimumDistance;
						bestCost = buffer[offs].cost;
					}
				}
			}
		}		
	}
	if (destination==null) return null;

	var path = [];
	while(destination.move!=null) {
		path.unshift(destination.move);
		destoffs = destoffs - (direction_offx[destination.move] + direction_offy[destination.move]*bw);
		destination = buffer[destoffs];
	}

	return path;
}


boundedPathFinding = function(map, x, y, character, radius, canSwim) {
	var sqr = radius*radius;
	var bw = radius*2+1;
	var buffer = new Array(bw*bw);
	var center = radius + radius*bw;
	var stack = new Array();
	for(var i = 0;i<bw;i++) {
		for(var j = 0;j<bw;j++) {
			buffer[j + i*bw] = null;
		}		
	}
	stack.push({x:0, y:0});
	buffer[center] = {cost:0, move:null};

	while(stack.length>0) {
		var current = stack[0];
		var offs = (current.x + radius) + (current.y + radius)*bw;
		stack.splice(0,1);

		// check to see if there is a bridge: characters cannot cross bridges. The only exception to this is if they
		// start ON a bridge, in which case they should be able to move, otherwise, they would be stuck
		if (map.getBridge(x+current.x, y+current.y)==null || 
			(current.x==0 && current.y==0)) {
			for(var i = 0;i<4;i++) {
				var nextx = current.x + direction_offx[i];
				var nexty = current.y + direction_offy[i];

				if (nextx*nextx + nexty*nexty <= sqr) {
					var nextoffs = (nextx + radius) + (nexty + radius)*bw;
					if (buffer[nextoffs]==null && 
						((canSwim==false && map.walkable(x+nextx,y+nexty,character)) ||
						 canSwim && map.sailable(x+nextx,y+nexty,character))) {
						buffer[nextoffs] = {cost:buffer[offs].cost+1, move:i};
						stack.push({x:nextx, y:nexty});
					}
				}
			}
		}
	}

	return buffer;
}


// BASIC BEHAVIORS: -----------------------------------------------------------------------------------------


function Behavior() {
	this.state = null;
	this.update = function(character, map, time, game) {
		return null;
	}

	this.generateSaveData = function(data) {
		data.state = this.state;
		return data;
	}
}


BRUnfriendlyIfAttackingFriendly.prototype = new Behavior();
function BRUnfriendlyIfAttackingFriendly() {
	this.update = function(character, map, time, game) {
		var objectwmes = character.AI.getAllWMEs("object");
		for(var i = 0;i<objectwmes.length;i++) {
			var wme = objectwmes[i];
			if (wme.params instanceof Character) {
				var c = wme.params;
				if (c.just_attacked_by!=null &&
					character.AI.WMEexists2("friendly", c.getClassName())) {
					character.AI.addShortTermWME({type:"unfriendly", params: c.just_attacked_by.attacker.getClassName()}, time, 3200);
				}
			}
		}							
		return null;
 	};
}


BRUnfriendlyIfSeenAttackingSelf.prototype = new Behavior();
function BRUnfriendlyIfSeenAttackingSelf() {
	this.update = function(character, map, time, game) {
		if (character.just_attacked_by!=null) {
			character.AI.addShortTermWME({type:"unfriendly", params: character.just_attacked_by.attacker.getClassName()}, time, 3200);
		}			
		return null;
 	};  
}


BRLeftRight.prototype = new Behavior();
function BRLeftRight() {
	this.state = 0;
	this.update = function(character, map, time, game) {
//		console.log("LeftAndRight behavior");
		if (this.state == 0) {
			if (character.x>0 && 
				((character.canSwim==false && map.walkable(character.x-1,character.y,character)) || 
			     (character.canSwim==true && map.sailable(character.x-1,character.y,character)))) {
				return {type:ACTION_MOVE, direction:DIRECTION_LEFT};
			} else {
				this.state = 1;
			}
		} else {
			if (character.x<map.width-1 && 
				((character.canSwim==false && map.walkable(character.x+1,character.y,character)) || 
			     (character.canSwim==true && map.sailable(character.x+1,character.y,character)))) {
				return {type:ACTION_MOVE, direction:DIRECTION_RIGHT};
			} else {
				this.state = 0;
			}
		}
		return null;
	}
}


BRWander.prototype = new Behavior();
function BRWander(forwardFactor, returnHome) {
	this.state = {};
	this.state.forwardFactor = forwardFactor;
	this.state.returnHome = returnHome;
	this.state.state = DIRECTION_RIGHT;	// stores the last direction
	this.returnHomeBehavior = null;

	this.update = function(character, map, time, game) {
		if (this.state.returnHome) {
			if (this.returnHomeBehavior==null) this.returnHomeBehavior = new BRReturnHome();
			var returnAction = this.returnHomeBehavior.update(character, map, time, game);
			if (character.map!=this.returnHomeBehavior.getHomeMap()) {
				return returnAction;
			}

			var dx = Math.abs(this.returnHomeBehavior.getHomeX() - character.x);
			var dy = Math.abs(this.returnHomeBehavior.getHomeY() - character.y);
			if (dx>character.sightRadius || dy>character.sightRadius) return returnAction;
		}

		var directions = [];
		for(var i = 0;i<4;i++) {
			if ((character.canSwim==false && map.walkable(character.x + direction_offx[i], character.y + direction_offy[i], character)) ||
				(character.canSwim==true && map.sailable(character.x + direction_offx[i], character.y + direction_offy[i], character))) {
				directions.push(i);
				// if the last one is possible, add it more than once, for having more chances of going in a straight line:
				if (i==this.state.state) {
					for(var j = 0;j<this.state.forwardFactor;j++) directions.push(i);
				}
			}
		}

		if (directions.length>0) {
			this.state.state = directions[Math.floor(Math.random() * directions.length)];
			if (this.state.state==-1) return {type:ACTION_NONE};
			return {type:ACTION_MOVE, direction:this.state.state};
		} else {
			return null;
		}
	}
}


BRWanderStopping.prototype = new Behavior();
function BRWanderStopping(radiusX, radiusY, stopTime) {
	this.state = {};
	this.state.radiusX = radiusX;
	this.state.radiusY = radiusY;
	this.state.stopTime = stopTime;
	this.state.timer = 0;
	this.state.homeX = null;
	this.state.homeY = null;
	this.state.homeMap = null;
	this.subBehavior = null;

	this.update = function(character, map, time, game) {
		if (this.state.homeX==null) {
			this.state.homeX = character.x;
			this.state.homeY = character.y;
			this.state.homeMap = character.map;
		}
		// don't move if it's talking:
		if (character.conversationEngines.length>0) return null;
		if (this.state.timer<=0) {
			// choose a new target destination:
			if (this.subBehavior==null) this.subBehavior = new BRGoTo(null,null,null);
			this.subBehavior.state.targetmap = character.map;
			do {
				this.subBehavior.state.targetx = this.state.homeX + Math.floor(Math.random()*(this.state.radiusX*2+1)) - this.state.radiusX;
				this.subBehavior.state.targety = this.state.homeY + Math.floor(Math.random()*(this.state.radiusY*2+1)) - this.state.radiusY;
			}while(!map.walkable(this.subBehavior.state.targetx, this.subBehavior.state.targety, character));
			this.state.timer = this.state.stopTime;
		} else {
			if (this.subBehavior==null) {
				this.subBehavior = new BRGoTo(null,null,null);
				this.subBehavior.state.targetmap = character.map;
			}
			var tmp = this.subBehavior.update(character, map, time, game);
			if (tmp == null && character.state==STATE_READY) this.state.timer--;
			return tmp;
		}
		return null;
	}
}


BRWanderInCircles.prototype = new Behavior();
function BRWanderInCircles() {
	this.state = DIRECTION_RIGHT;	// stores the last direction
	this.update = function(character, map, time, game) {
		var directions = [];
		for(var i = 0;i<4;i++) {
			if (map.sailable(character.x + direction_offx[i], character.y + direction_offy[i])) {
				directions.push(i);
				// if the last one is possible, add it more than once, for having more chances of going in a straight line:
				if (i==this.state) {
					directions.push(i);
					directions.push(i);
					directions.push(i);
					directions.push(i);
					directions.push(i);
				}
				// if it's the next direction in the circle, also add it more than once, to tend to move in circles:
				if (i==((this.state+1)%4)) {
					directions.push(i);
					directions.push(i);
					directions.push(i);
				}
			}
		}

		if (directions.length>0) {
			this.state = directions[Math.floor(Math.random() * directions.length)];
			return {type:ACTION_MOVE, direction:this.state};
		} else {
			return null;
		}
	}
}


BRGoTo.prototype = new Behavior();
function BRGoTo(targetx, targety, targetmap) {
	this.state = {};
	this.state.targetx = targetx;
	this.state.targety = targety;
	this.state.targetmap = targetmap;

	this.update = function(character, map, time, game) {
		if (this.state.targetmap==map.ID) {
			if (character.x!=this.state.targetx || character.y!=this.state.targety) {
				var path = pathToFollow(map, character.x, character.y, character, this.state.targetx, this.state.targety, this.state.targetmap, character.sightRadius, character.canSwim);
				if (path!=null && path.length>0) {
					return {type:ACTION_MOVE, direction:path[0]};
				}
			}
		} else {
			// see if there is a bridge in sight:
			var bridges = map.bridgesInRadius(character.x, character.y, character.sightRadius);

			for(var i = 0;i<bridges.length;i++) {
				var bridge = bridges[i];
				if (bridge.mapTarget==this.state.targetmap) {
					var path = pathToFollow(map, character.x, character.y, character, bridge.x, bridge.y, character.map, character.sightRadius, character.canSwim);
					if (path!=null && path.length>0) {
						return {type:ACTION_MOVE, direction:path[0]};
					}					
				}
			}
		}
		return null;
	}
}


BRGoToWithWander.prototype = new Behavior();
function BRGoToWithWander(targetx, targety, targetmap) {
	this.state = {};
	this.state.targetx = targetx;
	this.state.targety = targety;
	this.state.targetmap = targetmap;
	this.wanderBehavior = null;

	this.update = function(character, map, time, game) {
		if (this.state.targetmap==map.ID) {
			if (character.x!=this.state.targetx || character.y!=this.state.targety) {
				var path = pathToFollow(map, character.x, character.y, character, this.state.targetx, this.state.targety, this.state.targetmap, character.sightRadius, character.canSwim);
				if (path!=null && path.length>0) {
					return {type:ACTION_MOVE, direction:path[0]};
				}
			} else {
				return null;
			}
		} else {
			// see if there is a bridge in sight:
			var bridges = map.bridgesInRadius(character.x, character.y, character.sightRadius);

			for(var i = 0;i<bridges.length;i++) {
				var bridge = bridges[i];
				if (bridge.mapTarget==this.state.targetmap) {
					var path = pathToFollow(map, character.x, character.y, character, bridge.x, bridge.y, character.map, character.sightRadius, character.canSwim);
					if (path!=null && path.length>0) {
						return {type:ACTION_MOVE, direction:path[0]};
					}					
				}
			}
		}
		character.AI.addShortTermWME({type:"lost", params: null}, time, 25);
		if (this.wanderBehavior==null) this.wanderBehavior = new BRWander(2,false);
		return this.wanderBehavior.update(character,map,time);;
	}
}



BRReturnHome.prototype = new Behavior();
function BRReturnHome() {
	this.subBehavior = new BRGoToWithWander(null,null,null);

	this.update = function(character, map, time, game) {
		if (this.subBehavior.state.targetx == null) {
			this.subBehavior.state.targetx = character.x;
			this.subBehavior.state.targety = character.y;
			this.subBehavior.state.targetmap = character.map;
		}
		var action = this.subBehavior.update(character,map,time,game);
		return action;
	}

	this.getHomeX = function() {
		return this.subBehavior.state.targetx;
	}
	this.getHomeY = function() {
		return this.subBehavior.state.targety;
	}
	this.getHomeMap = function() {
		return this.subBehavior.state.targetmap;
	}
}



BREnemyReturnHome.prototype = new Behavior();
function BREnemyReturnHome() {
	this.state = {};
	this.state.timer = Math.round(Math.random()*200);
	this.state.target_offset_x = 0;
	this.state.target_offset_y = 0;
	this.wanderBehavior = null;

	this.update = function(character, map, time, game) {
		if (this.state.targetx == null) {
			this.state.targetx = character.x;
			this.state.targety = character.y;
			this.state.targetmap = character.map;
		}
		this.state.timer += 1;
		if ((this.state.timer%200)==0) {
			this.state.target_offset_x = Math.round(Math.random()*2)-1;
			this.state.target_offset_y = Math.round(Math.random()*2)-1;
			if (!map.walkable(this.state.targetx+this.state.target_offset_x, this.state.targety+this.state.target_offset_y, character)) {
				this.state.target_offset_x = 0;
				this.state.target_offset_y = 0;
			}
		}
		if (this.state.targetmap==map.ID) {
			if (character.x!=this.state.targetx+this.state.target_offset_x || character.y!=this.state.targety+this.state.target_offset_y) {
				var path = pathToFollow(map, character.x, character.y, character, this.state.targetx+this.state.target_offset_x, this.state.targety+this.state.target_offset_y, this.state.targetmap, character.sightRadius, character.canSwim);
				if (path!=null && path.length>0) {
					return {type:ACTION_MOVE, direction:path[0]};
				}
			} else {
				return null;
			}
		} else {
			// see if there is a bridge in sight:
			var bridges = map.bridgesInRadius(character.x, character.y, character.sightRadius);

			for(var i = 0;i<bridges.length;i++) {
				var bridge = bridges[i];
				if (bridge.mapTarget==this.state.targetmap) {
					var path = pathToFollow(map, character.x, character.y, character, bridge.x, bridge.y, character.map, character.sightRadius, character.canSwim);
					if (path!=null && path.length>0) {
						return {type:ACTION_MOVE, direction:path[0]};
					}					
				}
			}
		}
		character.AI.addShortTermWME({type:"lost", params: null}, time, 25);
		if (this.wanderBehavior==null) this.wanderBehavior = new BRWander(2,false);
		return this.wanderBehavior.update(character,map,time);;
	}
}


BRFollowAtDistance.prototype = new Behavior();
function BRFollowAtDistance(target, mindistance, maxdistance) {
	this.state = {};
	this.state.target = target;
	this.state.mindistance = mindistance;
	this.state.maxdistance = maxdistance;
	this.BRGoto = null;
	this.FleeBehavior = null;

	this.update = function(character, map, time, game) {
		if (this.state.target!=null) {
			if (this.BRGoTo==null) this.BRGoTo = new BRGoTo(null);
			this.BRGoTo.state.targetx = this.state.target.x;
			this.BRGoTo.state.targety = this.state.target.y;
			this.BRGoTo.state.targetmap = this.state.target.map;
			var dx = character.x - this.state.target.x;
			var dy = character.y - this.state.target.y;
			var d = Math.sqrt(dx*dx + dy*dy);
			if (d>maxdistance || this.state.target.map != character.map) {
				if (this.BRGoTo==null) this.BRGoTo = new BRGoTo(null);
				return this.BRGoTo.update(character, map, time);
			} else if (d<mindistance) {
				if (this.FleeBehavior==null) this.FleeBehavior = new BRFleeMultipleTargets(new Array());
				this.FleeBehavior.state.target = [this.state.target];
				return this.FleeBehavior.update(character, map, time);
			} else {
				return {type:ACTION_NONE};
			}
		}
		return null;
	}
}


BRFleeMultipleTargets.prototype = new Behavior();
function BRFleeMultipleTargets(targetList) {
	this.state = {};
	this.state.target = targetList;

	this.update = function(character, map, time, game) {
		var path = pathToFleeMultipleTargets(map, character.x, character.y, character, this.state.target, character.sightRadius, character.canSwim);
//		console.log("BRFleeMultipleTargets, with " + this.target.length + " targets. Path: " + path);

		if (path!=null && path.length>0) {
			return {type:ACTION_MOVE, direction:path[0]};
		}

		return null;
	}
}


BRAttackMultiple.prototype = new Behavior();
function BRAttackMultiple(targetList) {
	this.state = {};
	this.state.target = targetList;
	this.state.timeSinceLastSpell = 0;

	this.update = function(character, map, time, game) {
		// spells:
		var tmp = [SPELL_MAGIC_MISSILE,SPELL_FIREBALL,SPELL_INCINERATE];
		for(var i = 0;i<tmp.length;i++) {
			if (time - this.state.timeSinceLastSpell >= 50 && 
				character.mp>=spell_cost[tmp[i]] &&
				character.spells.indexOf(tmp[i])!=-1) {
				for(var ix = 0;ix<character.width;ix++) {
					for(var iy = 0;iy<character.height;iy++) {
						for(var j = 0;j<this.state.target.length;j++) {
							var target = this.state.target[j];
							if (target.map==character.map &&
								target.x==character.x+ix ||
								target.y==character.y+iy) {
								// target is aligned:
								var direction = DIRECTION_UP;
								var offsx = target.x - (character.x+ix);
								var offsy = target.y - (character.y+iy);
								if (offsx>0) {
									offsx = 1; 
									direction = DIRECTION_RIGHT;
								}
								if (offsx<0) {
									offsx = -1;
									direction = DIRECTION_LEFT;
								}
								if (offsy>0) {
									offsy = 1;
									direction = DIRECTION_DOWN;
								}
								if (offsy<0) {
									offsy = -1;
									direction = DIRECTION_UP;
								}
								if (offsx!=0 || offsy!=0) {
									// see if there is anything in between:
									var x = character.x+ix + offsx;
									var y = character.y+iy + offsy;
									var path = true;
									while(x!=target.x && y!=target.y && path) {
										if ((character.canSwim && !map.walkable(x,y,character)) ||
											(!character.canSwim && !map.sailable(x,y,character))) path = false;
										x = x + offsx;
										y = y + offsy;
									}
									if (path) {
										// can use spell!
										this.state.timeSinceLastSpell = time;
										return {type:ACTION_SPELL, spell:tmp[i], direction:direction, offsx:ix, offsy:iy};
									}
								}
							}
						}
					}
				}
			}
		}
//		console.log("BRAttackMultiple...");
		// check if we can attack:
		for(var ix = 0;ix<character.width;ix++) {
			for(var iy = 0;iy<character.height;iy++) {
				for(var i = 0;i<4;i++) {
					var tmpx = character.x + direction_offx[i] + ix;
					var tmpy = character.y + direction_offy[i] + iy;

					for(var j = 0;j<this.state.target.length;j++) {
						if (tmpx==this.state.target[j].x && tmpy==this.state.target[j].y) {
	//						console.log("Attack!!!!!");
							return {type:ACTION_ATTACK, direction:i};
						}
					}
				}
			}
		}

		// else path:
		var path = pathToFollowMultipleTargets(map, character.x, character.y, character, this.state.target, character.sightRadius, character.canSwim);		

		if (path!=null && path.length>0) {
			return {type:ACTION_MOVE, direction:path[0]};
		}

		return null;
	}
}


BRPendingTalk.prototype = new Behavior();
function BRPendingTalk() {
	this.update = function(character, map, time, game) {
	//	console.log("BRPendingTalk for " + character.name);
		if (character.talk_state!=STATE_READY) {
			// the character is doing something, hold on
			return;
		}
		for(var idx = 0;idx<character.pendingTalk.length;idx++) {
			var pendingTalk = character.pendingTalk[idx];
			if (pendingTalk.list.length>0) {
				var objectwmes = character.AI.getAllWMEs("object");
				for(var i = 0;i<objectwmes.length;i++) {
					var wme = objectwmes[i];
					if (eval("wme.params instanceof " + pendingTalk.character)) {
						// a character is found!
						var ce = character.getConversationEngineForCharacter(wme.params);
						if (ce==null) {
							// start a conversation
							ce = new ConversationEngine(character, wme.params);
							character.conversationEngines.push(ce);
							character.sendAction({type:ACTION_TALK, 
												  performative:{type:TALK_HI, target:wme.params}}, 
											      map, game);
							ce.state = CONVERSATION_STATE_SENT_HI;
						} else {
							// check the conversation status, and, when possible, send the message:
							if (ce.state == CONVERSATION_STATE_IN_CONVERSATION ||
								ce.state == CONVERSATION_STATE_IN_CONVERSATION_ONLY_HI) {
								var retval = executeRuleEffect(pendingTalk.list[0], character, map, game, wme.params);
	//							character.sendAction({type:ACTION_TALK, 
	//												  performative:{type:pendingTalk.list[0].text, target:wme.params}}, 
	//											      map, game);
								if (retval!=undefined) {
									ce.state_timer = 0;
									pendingTalk.list.splice(0,1);
								}
							}
						}
					}
				}
			}
		}
		return null;
	};
}


BRPendingTalkWithoutConversation.prototype = new Behavior();
function BRPendingTalkWithoutConversation() {
	this.update = function(character, map, time, game) {
		for(var idx = 0;idx<character.pendingTalk.length;idx++) {
			var pendingTalk = character.pendingTalk[idx];
			if (pendingTalk.list.length>0) {
				var objectwmes = character.AI.getAllWMEs("object");
				for(var i = 0;i<objectwmes.length;i++) {
					var wme = objectwmes[i];
					if (eval("wme.params instanceof " + pendingTalk.character)) {
						// a character is found!
						if (character.talk_state==STATE_READY) {
							var retval = executeRuleEffect(pendingTalk.list[0], character, map, game, wme.params);
							if (retval!=undefined) pendingTalk.list.splice(0,1);
						}
					}
				}
			}
		}
		return null;
	};
}


BRAttackUnfriendly.prototype = new Behavior();
function BRAttackUnfriendly() {

	this.BRAttackMultiple = null;


	this.update = function(character, map, time, game) {
		var objectwmes = character.AI.getAllWMEs("object");
		var warpedobjectwmes = character.AI.getAllWMEs("warpedobject");
		objectwmes = objectwmes.concat(warpedobjectwmes);
		var unfriendlywmes = character.AI.getAllWMEs("unfriendly");
		var friendlywmes = character.AI.getAllWMEs("friendly");
		var dangerous = [];
		for(var i = 0;i<objectwmes.length;i++) {
			var wme = objectwmes[i];
			var unfriendly = false;
			for(var j = 0;j<unfriendlywmes.length;j++) {
				var wme2 = unfriendlywmes[j];
				var tmp = "wme.params instanceof " + wme2.params;
				if (eval(tmp)) {
					unfriendly = true;
					break;
				}
			}
			if (unfriendly) {
				for(var j = 0;j<friendlywmes.length;j++) {
					var wme2 = friendlywmes[j];
					var tmp = "wme.params instanceof " + wme2.params;
					if (eval(tmp)) {
						unfriendly = false;
						break;
					}
				}
			}
			if (unfriendly) dangerous.push(wme.params);
		}
		if (dangerous.length!=0) {
			if (this.BRAttackMultiple==null) this.BRAttackMultiple = new BRAttackMultiple(null);
			character.AI.addShortTermWME({type:"angry", params:null}, time, 400);
			this.BRAttackMultiple.state.target = dangerous;
			var action = this.BRAttackMultiple.update(character,map,time, game);
			return action;
		}
		return null;
 	};
}


BRFleeUnfriendly.prototype = new Behavior();
function BRFleeUnfriendly() {

	this.fleeMultipleBehavior = null;

	this.update = function(character, map, time, game) {
		var objectwmes = character.AI.getAllWMEs("object");
		var dangerouswmes = character.AI.getAllWMEs("unfriendly");
		var dangerous = [];
		for(var i = 0;i<objectwmes.length;i++) {
			var wme = objectwmes[i];
			for(var j = 0;j<dangerouswmes.length;j++) {
				var wme2 = dangerouswmes[j];
				var tmp = "wme.params instanceof " + wme2.params;
				if (eval(tmp)) {
//										console.log("Frog is scared of " + wme.params.name + " because it is of type " + wme2.params);
					character.AI.addShortTermWME({type:"scared", params: wme.params}, time, 400);
					dangerous.push(wme.params);
					break;
				}
			}
		}
		if (dangerous.length!=0) {
			if (this.fleeMultipleBehavior==null) this.fleeMultipleBehavior = new BRFleeMultipleTargets(null); 
			this.fleeMultipleBehavior.state.target = dangerous;
			return this.fleeMultipleBehavior.update(character,map,time, game);
		}

		return null;
 	};
}


BRFollow.prototype = new Behavior();
function BRFollow() {
	this.subBehavior = null;

	this.update = function(character, map, time, game) {
		var followwmes = character.AI.getAllWMEs("follow");
		if (followwmes.length>0) {
			var objectwmes = character.AI.getAllWMEs("object");
			var warpedobjectwmes = character.AI.getAllWMEs("warpedobject");
			objectwmes = objectwmes.concat(warpedobjectwmes);
			for(var i = 0;i<followwmes.length;i++) {
				var c = followwmes[i].params;
				for(var j = 0;j<objectwmes.length;j++) {
					var wme2 = objectwmes[j];
					if (eval("wme2.params instanceof " + c)) {
						if (this.subBehavior==null) this.subBehavior = new BRFollowAtDistance(null,1,2);
						this.subBehavior.state.target = wme2.params;
						console.log("following!");
						return this.subBehavior.update(character,map,time,game);
					}
				}
			}
		}
	}
}


BRHailNotUnfriendly.prototype = new Behavior();
function BRHailNotUnfriendly() {

	this.update = function(character, map, time, game) {
		var objectwmes = character.AI.getAllWMEs("object");
		if (character.AI.getWME("angry")!=null) return;	// no talking while angry!
		for(var i = 0;i<objectwmes.length;i++) {
			var wme = objectwmes[i];
			if (wme.time == time &&
				wme.params!=character &&
				(wme.params instanceof Character)) {
				var unfriendly = false;
				var unfriendlywmes = character.AI.getAllWMEs("unfriendly");
				for(var j = 0;j<unfriendlywmes.length;j++) {
					var wme2 = unfriendlywmes[j];
					var tmp = "wme.params instanceof " + wme2.params;
					if (eval(tmp)) {
						unfriendly = true;
						break;
					}
				}
				if (!unfriendly) {
					var c = wme.params;
					// we just saw a new character, start a conversation unless we are in one already:
					if (character.conversationEngines.length==0) {
						var ce = new ConversationEngine(character, c);
						character.conversationEngines.push(ce);
						character.sendAction({type:ACTION_TALK, 
										      performative:{type:TALK_HI, target:c}}, 
										      map, game);
						ce.state = CONVERSATION_STATE_SENT_HI;
					}
				}
			}
		}
		return null;
	}
}


BRProtectPossessions.prototype = new Behavior();
function BRProtectPossessions() {
	this.state= {};
	this.state.map = null;
	this.state.posessions = null;
	this.state.lookingFor = [];
	this.recoverLostPosessionsBehavior = null;

	this.update = function(character, map, time, game) {
		if (this.state.map==null) {
			this.state.map = character.map;
			this.state.posessions = [];
			this.state.posession_locations = [];
			var l = map.objectsInRadius(character.x,character.y,character.sightRadius,game);
			for(var i = 0;i<l.length;i++) {
				var object = l[i];
				if (object instanceof Item) {
					this.state.posessions.push(object);
					this.state.posession_locations.push({x:object.x, y:object.y, map:object.map});
				}
			}
		}

		if (character.map==this.state.map) {
			var objectwmes = character.AI.getAllWMEs("object");
			for(var i = 0;i<this.state.posessions.length;i++) {
				var posession = this.state.posessions[i];
				var found = false;
				for(var j = 0;j<objectwmes.length;j++) {
					var wme = objectwmes[j];
					if (wme.params==posession) {
						found = true;
						break;
					}
				}
				if (!found) {
					for(var j = 0;j<character.inventory.length;j++) {
						if (character.inventory[j]==posession) {
							found = true;
							break;
						}
					}
				}
				if (!found) {
					this.state.lookingFor.push(i);
					// posession stolen! he will blame anyone in sight!
					for(var j = 0;j<objectwmes.length;j++) {
						var wme = objectwmes[j];
						if (wme.params!=character &&
							(wme.params instanceof Character)) {
							var c = wme.params;
							character.AI.addShortTermWME({type:"unfriendly", params: c.getClassName()}, time, 3200);
						}
					}
					return {type:ACTION_TALK_ANGRY, performative:{type:"Thief!", target:null}};
				}
			}	
		}

		for(var i = 0;i<this.state.lookingFor.length;i++) {
			var object = this.state.posessions[this.state.lookingFor[i]];
			var objectwmes = character.AI.getAllWMEs("object");

			// check if we have it in the inventory:
			for(var j = 0;j<character.inventory.length;j++) {
				if (character.inventory[j]==object) {
					var loc = this.state.posession_locations[this.state.lookingFor[i]];
					if (loc.x == character.x && loc.y == character.y && loc.map == character.map) {
						// drop the object:
						this.state.lookingFor.splice(i,1);
						return {type:ACTION_DROP, item:j};
					} else {
						if (this.recoverLostPosessionsBehavior==null) this.recoverLostPosessionsBehavior = new BRGoToWithWander(null,null,null);
						// go back to the object's original position:
						this.recoverLostPosessionsBehavior.state.targetx = loc.x;
						this.recoverLostPosessionsBehavior.state.targety = loc.y;
						this.recoverLostPosessionsBehavior.state.targetmap = loc.map;
						return this.recoverLostPosessionsBehavior.update(character,map,time);
					}
					return null;
				}
			}

			for(var j = 0;j<objectwmes.length;j++) {
				var wme2 = objectwmes[j];
				if (wme2.params == object) {
					if (object.x == character.x && object.y == character.y && object.map == character.map) {
						return {type:ACTION_TAKE};
					} else {
						if (this.recoverLostPosessionsBehavior==null) this.recoverLostPosessionsBehavior = new BRGoToWithWander(null,null,null);
						// move towards the object:
						this.recoverLostPosessionsBehavior.state.targetx = wme2.params.x;
						this.recoverLostPosessionsBehavior.state.targety = wme2.params.y;
						this.recoverLostPosessionsBehavior.state.targetmap = wme2.params.map;
						var action = this.recoverLostPosessionsBehavior.update(character,map,time);
						return action;
					}
				}
			}
			
		}		
		return null;
	}
} 


BRTalkAloud.prototype = new Behavior();
function BRTalkAloud(messages, period, angry) {
	this.state = {};
	this.state.messages = messages;
	this.state.period = period;
	this.state.angry = angry;
	this.state.timer = 0;

	this.update = function(character, map, time, game) {
		if (character.conversationEngines.length>0) return null;
		if (this.state.timer<=0) {
			var message = this.state.messages[Math.floor(Math.random()*this.state.messages.length)];
			this.state.timer = this.state.period;
			if (this.state.angry) {
				return {type:ACTION_TALK_ANGRY, performative:{type:message, target:null}}
			} else {
				return {type:ACTION_TALK, performative:{type:message, target:null}}
			}		
		} else {
			this.state.timer--;
		}
		return null;
	}
}


BRCurious.prototype = new Behavior();
function BRCurious() {
	this.followBehavior = new BRFollowAtDistance(null,0,0); 

	this.update = function(character, map, time, game) {
		var wmes = character.AI.getAllWMEs("object");
		for(var i = 0;i<wmes.length;i++) {
			var wme = wmes[i];
			if (wme.time == time && (wme.params instanceof Item)) {
				character.AI.addShortTermWME({type:"curious", params: wme.params}, time, 200);
			}
		}
		var wme = character.AI.getWME("curious");
		if (wme!=null && wme.params!=null) {
			if (character.x == wme.params.x && character.y == wme.params.y && character.map == wme.params.map) {
				character.AI.removeShortTermWME(wme);
				return {type:ACTION_TAKE};
			} else {
				this.followBehavior.target = wme.params;
				return this.followBehavior.update(character,map,time,game);
			}
		}		
		return null;
	}
}


BRTired.prototype = new Behavior();
function BRTired(timeWalking, restTime) {
	this.state = {};
	this.state.timeWalking = timeWalking;
	this.state.restTime = restTime;
	this.state.startWalkingTime = null;

	this.update = function(character, map, time, game) {
		if (character.AI.WMEexists("tired")) return {type:ACTION_NONE};
		if (this.state.startWalkingTime==null) this.state.startWalkingTime = time;
		if (time - this.state.startWalkingTime >= this.state.timeWalking) {
			character.AI.addShortTermWME({type:"tired", params:null}, time, this.state.restTime);
			this.state.startWalkingTime = null;
		} 
		return null;
	}
}


BRYellToUnfriendly.prototype = new Behavior();
function BRYellToUnfriendly(message) {
	this.state = {};
	this.state.message = message;

	this.update = function(character, map, time, game) {
		var objectwmes = character.AI.getAllWMEs("object");
		var warpedobjectwmes = character.AI.getAllWMEs("warpedobject");
		objectwmes = objectwmes.concat(warpedobjectwmes);
		var unfriendlywmes = character.AI.getAllWMEs("unfriendly");
		var friendlywmes = character.AI.getAllWMEs("friendly");
		var dangerous = [];
		for(var i = 0;i<objectwmes.length;i++) {
			var wme = objectwmes[i];
			var unfriendly = false;
			for(var j = 0;j<unfriendlywmes.length;j++) {
				var wme2 = unfriendlywmes[j];
				var tmp = "wme.params instanceof " + wme2.params;
				if (eval(tmp)) {
					unfriendly = true;
					break;
				}
			}
			if (unfriendly) {
				for(var j = 0;j<friendlywmes.length;j++) {
					var wme2 = friendlywmes[j];
					var tmp = "wme.params instanceof " + wme2.params;
					if (eval(tmp)) {
						unfriendly = false;
						break;
					}
				}
			}
			if (unfriendly) dangerous.push(wme.params);
		}
		if (dangerous.length!=0) {
			if (!character.AI.WMEexists("angry")) {
				character.AI.addShortTermWME({type:"angry", params:null}, time, 400);
				return {type:ACTION_TALK_ANGRY, performative:{type:this.state.message, target:null}};
			}
		}
		return null;
	}
}


BRGoToAngryFriendly.prototype = new Behavior();
function BRGoToAngryFriendly() {
	this.subBehavior = new BRGoTo(null,null,null);

	this.update = function(character, map, time, game) {
		var objectwmes = character.AI.getAllWMEs("object");
		var warpedobjectwmes = character.AI.getAllWMEs("warpedobject");
		objectwmes = objectwmes.concat(warpedobjectwmes);
		var friendlywmes = character.AI.getAllWMEs("friendly");
		// check to see if any friendly is angry, and get angry as well:
		for(var i = 0;i<objectwmes.length;i++) {
			var wme = objectwmes[i];
			var friendly = false;
			for(var j = 0;j<friendlywmes.length;j++) {
				var wme2 = friendlywmes[j];
				var tmp = "wme.params instanceof " + wme2.params;
				if (eval(tmp)) {
					friendly = true;
					break;
				}
			}
			if (friendly) {
				var friendlyobject = wme.params;
				var wme = friendlyobject.AI.getWME("angry");
				if (wme!=null) {
					if (wme.activation>350) {
						// if it's recent:
						var wme2 = character.AI.getWME("angry");
						if (wme2==null) {
//							character.AI.addShortTermWME({type:"angry", params:null}, time, 400);
							// follow:
							this.subBehavior.state.targetx = friendlyobject.x;
							this.subBehavior.state.targety = friendlyobject.y;
							this.subBehavior.state.targetmap = friendlyobject.map;
							return this.subBehavior.update(character, map, time, game);
						}
					}
				}
			}
		}
	}
}

