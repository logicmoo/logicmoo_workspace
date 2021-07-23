function createMaps(mapCreationData, game) {
	var maps = new Array();
	for(var i = 0;i<mapCreationData.length;i++) {
		var tmp = mapCreationData[i];
		if (tmp.type=="file") {
			maps.push(new Map(tmp.path, game));
		} else if (tmp.type=="method") {
			maps.push(eval(tmp.method));
		} else {
			console.log("ERROR! unknown map creation procedure in 'createMaps'!");
		}
	}

	// connect all the bridges and setting the "map" properties:
//	console.log("Connecting bridges...");
	for(var i = 0;i<maps.length;i++) {
		var map = maps[i];
		map.ID = i;
//		console.log("Map has " + map.bridges.length + " bridges and " + map.bridgeDestinations.length + " destinations");
		for(bridge_idx=0;bridge_idx<map.bridges.length;bridge_idx++) {
			var bridge = map.bridges[bridge_idx];
//			console.log("Processing bridge " + bridge.id);
			for(var j = 0;j<maps.length;j++) {
				if (i!=j) {
					var map2 = maps[j];
					for(bridge_idx2=0;bridge_idx2<map2.bridgeDestinations.length;bridge_idx2++) {
						var bridge2 = map2.bridgeDestinations[bridge_idx2];
						if (bridge.id == bridge2.id) {
							bridge.xTarget = bridge2.x;
							bridge.yTarget = bridge2.y;
							bridge.widthTarget = bridge2.width;
							bridge.heightTarget = bridge2.height;
							bridge.mapTarget = j;
							bridge.appearWalking = bridge2.appearWalking;
							bridge.appearDirection = bridge2.appearDirection;
//							console.log("Bridge " + bridge.id + " connected!");
							if (map.neighbors.indexOf(j)==-1) map.neighbors.push(j);
						}
					}
				}
			}
		}

//		console.log("Map neighbors: " + map.neighbors);

		for(var j = 0;j<maps[i].layers.length;j++) {
			for(var k = 0;k<maps[i].layers[j].objects.length;k++) {
				maps[i].layers[j].objects[k].map = i;
			}
		}

		maps[i].collectRespawnData();
	}

	return maps;
}


function Map(fileName, game) {


	this.walkableInternal = function(x,y,o) {
		var idx = x + y*this.width;
		for(var i = 0;i<this.layers.length;i++) {
			var type = this.layers[i].types[idx];
			if (type==1) return false;
			if (type==2) return false;
			if (type==3) return false;
			if (type==4) {
				// see if there is a vehicle here:
				var vehicleThere = false;
				for(var l = 0;l<this.layers.length;l++) {
					for(var j = 0;j<this.layers[l].objects.length;j++) {
						object = this.layers[l].objects[j];
						if (object.x==x && object.y==y && object instanceof Vehicle) {
							vehicleThere = true;
						}
					}
				}
				if (vehicleThere == false) return false;
			}

			for(var j = 0;j<this.layers[i].objects.length;j++) {
				object = this.layers[i].objects[j];
				if (object!=o &&
					!object.burrowed &&
				    !object.walkable() && object.x<=x && object.y<=y && 
				    (object.x+object.width)>x && (object.y+object.height)>y) return false;
			}

		}
		return true;
	}

	this.walkable = function(x,y,o) {
		if (o==null) return this.walkableInternal(x,y,null);
		for(var i = 0;i<o.width;i++) {
			for(var j = 0;j<o.height;j++) {
				if (!this.walkableInternal(x+i,y+j,o)) return false;
			}
		}
		return true;
	}


	this.sailableInternal = function(x,y,o) {
		var idx = x + y*this.width;
		for(var i = 0;i<this.layers.length;i++) {
			var type = this.layers[i].types[idx];
			if (type==4) {
				// see if there is a vehicle or a wall here:
				for(var l = 0;l<this.layers.length;l++) {
					var type2 = this.layers[l].types[idx];
					if (type2==1 || type2==2 || type2==3) return false;
					for(var j = 0;j<this.layers[l].objects.length;j++) {
						object = this.layers[l].objects[j];
						if (object.x==x && object.y==y && object instanceof Vehicle) return false;
						if (object!=o &&
							!object.burrowed &&
				    		!object.walkable() && object.x<=x && object.y<=y && 
				    		(object.x+object.width)>x && (object.y+object.height)>y) return false;
					}
				}
				return true;
			}
		}
		return false;
	}

	this.sailable = function(x,y,o) {
		if (o==null) return this.sailableInternal(x,y,null);
		for(var i = 0;i<o.width;i++) {
			for(var j = 0;j<o.height;j++) {
				if (!this.sailableInternal(x+i,y+j,o)) return false;
			}
		}
		return true;
	}


	this.canSeeThrough = function(x,y,game) {
		var idx = x + y*this.width;
		for(var i = 0;i<this.layers.length;i++) {
			if (this.layers[i].seeThrough[idx]==1) return false;
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object.x == x && object.y == y) {
					var tmp1 = object.getTile();
					var tmp2 = object.getTileEntry(game);
					if (tmp2!=null && tmp1>=0 && tmp2.seeThrough[tmp1]==1) return false;
				}
			}
		}
		return true;
	}


	this.canDig = function(x,y) {
		var idx = x + y*this.width;
		for(var i = 0;i<this.layers.length;i++) {
			if (this.layers[i].canDig[idx]==1) return true;
		}
		return false;
	}


	this.choppeable = function(x,y) {
		var idx = x + y*this.width;
		for(var i = 0;i<this.layers.length;i++) {
			var type = this.layers[i].types[idx];
			if (type==3) return true;
		}
		return false;
	}

	this.nonchoppeabletree = function(x,y) {
		var idx = x + y*this.width;
		for(var i = 0;i<this.layers.length;i++) {
			var type = this.layers[i].types[idx];
			if (type==2) return true;
		}
		return false;
	}

	this.chopTree = function(x,y) {
		var idx = x + y*this.width;
		for(var i = 0;i<this.layers.length;i++) {
			var type = this.layers[i].types[idx];
			if (type==3) {
				this.layers[i].types[idx]=0;
				this.layers[i].tiles[idx]=0;
			}
		}
	}

	this.getBridge = function(x,y) {
		for(var i = 0;i<this.bridges.length;i++) {
			var bridge = this.bridges[i];
//			console.log(x + "," + y + " == " + bridge.x + "," + bridge.y);
			if (bridge.x <= x && bridge.y <= y &&
				bridge.x + bridge.width > x &&
				bridge.y + bridge.height > y) return bridge;
		}
		return null;
	}

	this.getTakeable = function(x,y) {
		var l = [];
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object.takeable == true && !object.burrowed &&
					object.x <= x && object.y <= y &&
					object.x + object.width > x &&
					object.y + object.height > y) l.push(object);
			}
		}
		return l;
	}

	this.getBurrowed = function(x,y) {
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object.burrowed &&
					object.x <= x && object.y <= y &&
					object.x + object.width > x &&
					object.y + object.height > y) return object;
			}
		}
		return null;
	}

	this.getDoor = function(x,y) {
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object instanceof Door &&
					object.x <= x && object.y <= y &&
					object.x + object.width > x &&
					object.y + object.height > y) return object;
			}
		}
		return null;
	}	

	this.getPushableWall = function(x,y) {
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object instanceof PushableWall &&
					object.x <= x && object.y <= y &&
					object.x + object.width > x &&
					object.y + object.height > y) return object;
			}
		}
		return null;
	}	

	this.getDoorsWithID = function(ID) {
		var doors = [];
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if ((object instanceof Door) && object.name == ID) doors.push(object);
			}
		}
		return doors;		
	}

	this.getLever = function(x,y) {
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object instanceof Lever &&
					object.x <= x && object.y <= y &&
					object.x + object.width > x &&
					object.y + object.height > y) return object;
			}
		}
		return null;
	}		


	this.getVehicle = function(x,y) {
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object.x == x && object.y == y && object instanceof Vehicle) return object;
			}
		}
		return null;
	}		


	this.getTrigger = function(x,y) {
		for(var j = 0;j<this.triggers.length;j++) {
			var object = this.triggers[j];
			if (object.x <= x && object.y <= y &&
				object.x+object.width > x && object.y+object.height > y) return object;
		}
		return null;		
	}

	this.removeTrigger = function(object) {
		var idx = this.triggers.indexOf(object);
		if (idx != -1) {
			this.triggers.splice(idx,1);
		}		
	}


	this.getMessage = function(x,y) {
		for(var j = 0;j<this.messages.length;j++) {
			var object = this.messages[j];
			if (object.x <= x && object.y <= y &&
				object.x+object.width > x && object.y+object.height > y) return object;
		}
		return null;		
	}

	this.removeMessage = function(object) {
		var idx = this.messages.indexOf(object);
		if (idx != -1) {
			// this.layers[i].objects = 
			this.messages.splice(idx,1);
		}		
	}

	this.getCharacter = function(x,y) {
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object.x<=x && object.y<=y && 
				    (object.x+object.width)>x && (object.y+object.height)>y && 
				    object instanceof Character) return object;
			}
		}
		return null;
	}	


	this.getObjects = function(x,y) {
		var objects = [];
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object.x<=x && object.y<=y && 
				    (object.x+object.width)>x && (object.y+object.height)>y) objects.push(object);
			}
		}
		return objects;
	}	



	this.getObjectsOfType = function(type) {
		var objects = [];
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				if (object.getClassName()==type) objects.push(object);
			}
		}
		return objects;		
	}


	this.visibilityCheck = function(x1, y1, x2, y2, game) {
		var dx = Math.abs(x2-x1);
		var dy = Math.abs(y2-y1);

		if (dx>dy) {
			var incx = 1;
			if (x2<x1) incx = -1;
			var px = x1;
			var py = y1;
			while(px!=x2) {
				px += incx;
				py = Math.round(y1 + (y2-y1)*((px-x1)/(x2-x1)));				
				if (px!=x2) {
					if (!this.canSeeThrough(px,py,game)) return false;
				}
			}
		} else {
			var incy = 1;
			if (y2<y1) incy = -1;
			var px = x1;
			var py = y1;
			while(py!=y2) {
				py += incy;
				px = Math.round(x1 + (x2-x1)*((py-y1)/(y2-y1)));				
				if (py!=y2) {
					if (!this.canSeeThrough(px,py,game)) return false;
				}
			}
		}

		return true;
	}


	this.objectsInRadius = function(x,y,radius, game) {
		var objects = [];
		var sqr = radius*radius;
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				var dx = object.x-x;
				var dy = object.y-y;
				var d = dx*dx+dy*dy;
				if (!object.burrowed && d<=sqr &&
					this.visibilityCheck(x,y, object.x, object.y, game)) objects.push(object);
			}
		}
		return objects;
	}


	this.bridgesInRadius = function(x,y,radius) {
		var bridges = [];
		var sqr = radius*radius;
		for(var j = 0;j<this.bridges.length;j++) {
			var bridge = this.bridges[j];
			var dx = (bridge.x + (bridge.width/2))-x;
			var dy = (bridge.y + (bridge.height/2))-y;
			var d = dx*dx+dy*dy;
			if (d<=sqr) bridges.push(bridge);
		}
		return bridges;
	}

	this.findClosestCharacterToTalk = function(character,game) {
		var l = this.objectsInRadius(character.x, character.y,5,game);
		var closest = null;
		var closest_distance = 0;
		var NPCfound = false;
		for(var i = 0;i<l.length;i++) {
			var object = l[i];
			if (object!=character) {
				var dx = character.x - object.x;
				var dy = character.y - object.y;
				var d = dx*dx + dy*dy;
				if (object instanceof AICharacter) {
					if (closest==null || !NPCfound || d<closest_distance) {
						closest = object;
						closest_distance = d;
						NPCfound = true;
					}
				} else if (!NPCfound && (object instanceof PlayerCharacter)) {
					closest = object;
					closest_distance = d;					
				}
			}
		}
		return closest;
	}

	this.addObject = function(object, layer) {
		while(this.layers.length<=layer) this.newLayer();
		this.layers[layer].objects.push(object);
		object.layer = layer;
	}

	this.removeObject = function(object) {
		for(var i = 0;i<this.layers.length;i++) {
			var idx = this.layers[i].objects.indexOf(object);
			if (idx != -1) {
				// this.layers[i].objects = 
				this.layers[i].objects.splice(idx,1);
			}
		}
	}

	this.draw = function(ww,wh,centerx, centery, game) {
		// compute the offset:
		if (this.width*TILE_WIDTH>ww) {
			this.desiredoffsetx = ww/2 - centerx*TILE_WIDTH;
			if (this.desiredoffsetx>0) this.desiredoffsetx = 0;
			if (ww-this.desiredoffsetx>this.width*TILE_WIDTH) this.desiredoffsetx = - (this.width*TILE_WIDTH - ww);
		} else {
			this.desiredoffsetx = (ww - (this.width*TILE_WIDTH))/2;
		}
		if (this.height*TILE_HEIGHT>wh) {
			this.desiredoffsety = wh/2 - centery*TILE_HEIGHT;
			if (this.desiredoffsety>0) this.desiredoffsety = 0;
			if (wh-this.desiredoffsety>this.height*TILE_HEIGHT) this.desiredoffsety = - (this.height*TILE_HEIGHT - wh);
		} else {
			this.desiredoffsety = (wh - (this.height*TILE_HEIGHT))/2;
		}
		if (this.offsetx!=this.desiredoffsetx) this.offsetx = Math.floor((this.offsetx*3 + this.desiredoffsetx)/4);
		if (this.offsety!=this.desiredoffsety) this.offsety = Math.floor((this.offsety*3 + this.desiredoffsety)/4);

		for(var i = 0;i<this.layers.length;i++) {
			// draw tiles:
			var offset = 0;
			var tileEntry = this.layers[i].tileEntry;
			for(var y = 0;y<this.height;y++) {
				for(var x = 0;x<this.width;x++,offset++) {
					if (this.visibility[offset]) {
						var tile = this.layers[i].tiles[offset];
						if (tile > 0) {
							drawTile(tile-1,x*TILE_WIDTH + this.offsetx,y*TILE_HEIGHT + this.offsety,1, tileEntry);
						}
					}
				}
			}		
			// draw objects:
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var object = this.layers[i].objects[j];
				offset = object.x + object.y*this.width;
				if (this.visibility[offset] && !object.burrowed) {
					object.draw(1,this.offsetx,this.offsety,game);
				}
			}
		}

		// draw fog of war:
		{
			var offset = 0;
			var overlap = 0.5;
			var overlapsolid = 0.33;
			ctx.fillStyle="black";
			for(var y = 0;y<this.height;y++) {
				for(var x = 0;x<this.width;x++,offset++) {
					if (!this.visibility[offset]) {
						if ((x>0 && this.visibility[offset-1]) ||
							(x<this.width-1 && this.visibility[offset+1]) ||
							(y>0 && this.visibility[offset-this.width]) ||
							(y<this.height-1 && this.visibility[offset+this.width])) {
							ctx.globalAlpha=0.33;
							ctx.fillRect(Math.floor(x*TILE_WIDTH + this.offsetx - overlap*TILE_WIDTH),
										 Math.floor(y*TILE_HEIGHT + this.offsety - overlap*TILE_HEIGHT),
										 Math.floor(TILE_WIDTH*(1+overlap*2)),
										 Math.floor(TILE_HEIGHT*(1+overlap*2)));
							ctx.globalAlpha=1.0;
							ctx.fillRect(Math.floor(x*TILE_WIDTH + this.offsetx - overlapsolid*TILE_WIDTH),
										 Math.floor(y*TILE_HEIGHT + this.offsety - overlapsolid*TILE_HEIGHT),
										 Math.floor(TILE_WIDTH*(1+overlapsolid*2)),
										 Math.floor(TILE_HEIGHT*(1+overlapsolid*2)));
						}
					}
				}
			}
		}


		this.drawTextOverlays(1,this.offsetx,this.offsety);
	}


	this.update = function(game) {
		this.updateTextOverlays(game);

		var toDelete = [];
		for(var i in this.warps) {
			this.warps[i].timer--;
			if (this.warps[i].timer<=0) toDelete.push(this.warps[i]);
		}
		for(var i in toDelete) {
			this.warps.splice(this.warps.indexOf(toDelete[i]),1);
		}	

		for(var i = 0;i<this.layers.length;i++) {
//			if (this.layers[i]==null) console.log("Undefined layer " + i);
			// get a copy of the objects (in case one disappears or one is created):
			var tmpObjects = [];
			for(var j = 0;j<this.layers[i].objects.length;j++) tmpObjects.push(this.layers[i].objects[j]);
			for(var j = 0;j<tmpObjects.length;j++) {
				var retVal = tmpObjects[j].update(null, this, game);
				if (!retVal) {
//					console.log("Removing object: " + tmpObjects[j].name);
					this.removeObject(tmpObjects[j]);
					var idx = game.characters.indexOf(tmpObjects[j]);
					if (idx!=-1) {
						if (game.currentCharacter==idx) {
							game.characters.splice(idx,1);
							if (game.currentCharacter>=game.characters.length) game.currentCharacter--;
							if (game.currentCharacter==-1) {
								// game over!!!
								return false;
							}
						} else if (game.currentCharacter>idx) {
							game.characters.splice(idx,1);
							game.currentCharacter--;
						} else {
							game.characters.splice(idx,1);
						}
					}
				}
			}
		}


		if (this.onStart.length>0) {
			var retVal = executeRuleEffect(this.onStart[0], null, this, game, null);
			if (retVal!=undefined) this.onStart.splice(0,1);
		}

		// check for story state rules:
		if (game.storyStateLastCycleUpdated > this.storyStateRulesLastCycleChecked ||
			this.storyStateLastCycleUpdated > this.storyStateRulesLastCycleChecked) {
			for(var i = 0;i<this.storyStateRules.length;i++) {
				var rule = this.storyStateRules[i];
				if (!rule.once || !rule.triggered) {
					var triggered = false;
					if (rule.scope=="game") {
						if (game.storyState[rule.variable] == rule.value) triggered = true;
					} else if (rule.scope=="map") {
						if (this.storyState[rule.variable] == rule.value) triggered = true;
//					} else if (rule.scope=="character") {
//						if (this.storyState[rule.variable] == rule.value) triggered = true;
					} else {
						console.log("storyStateRule in a map has an incorrect scope (should be game, map): " + rule.scope);
					}
					if (triggered) {
						for(var j = 0;j<rule.actions.length;j++) {
							this.actionsToExecute.push(rule.actions[j]);
						}
					}
				}
			}
		}
		for(var i = 0;i<this.timerEventRules.length;i++) {
		    var rule = this.timerEventRules[i];
		    if ((rule.period==undefined && (this.cycle)==rule.time) ||
		        (rule.period!=undefined && (this.cycle%rule.period)==rule.time)) {
		      if (!rule.once || !rule.triggered) {
		        rule.triggered = true;
		        for(var j = 0;j<rule.actions.length;j++) {
		          this.actionsToExecute.push(rule.actions[j]);
		        }
		      }  
		    }
		}
		if(this.actionsToExecute.length>0) {
			var retVal = executeRuleEffect(this.actionsToExecute[0], null, this, game, null);
			if (retVal!=undefined) this.actionsToExecute.splice(0,1);
		}


		return true;
	}


	this.pushTextOverlay = function(text,x,y,r,g,b) {
		this.textOverlays.push({text:text, x:x, y:y, offy:0, r:r, g:g, b:b, cycle:0, duration:100, bubble:false});
	}

	this.pushTextBubble = function(text,speaker,time) {
		this.textOverlays.push({text:text, r:255, g:255, b:255, cycle:0, duration:time, bubble:true, speaker:speaker,
								textColor:'#000000', bubbleColor:'#ffffff'});
	}

	this.pushTextBubbleColor = function(text,speaker,time, textColor, bubbleColor) {
		this.textOverlays.push({text:text, r:255, g:255, b:255, cycle:0, duration:time, bubble:true, speaker:speaker, 
								textColor:textColor, bubbleColor:bubbleColor});
	}

	this.updateTextOverlays = function(game) {
		var toDelete = [];

		for(var i in this.textOverlays) {
			var msg = this.textOverlays[i];
			msg.cycle++;
			if (!msg.bubble && (msg.cycle%2)==0) msg.offy--;
			if (msg.bubble) {
				if (msg.speaker.map!=this.ID) {
					// move this buble to the other map:
					game.maps[msg.speaker.map].textOverlays.push(msg);
					toDelete.push(msg);
				} else {
					if (msg.cycle>=msg.duration) toDelete.push(msg);
				}
			} else {
				if (msg.cycle>=msg.duration) toDelete.push(msg);
			}
		}
		for(var i in toDelete) {
			this.textOverlays.splice(this.textOverlays.indexOf(toDelete[i]),1);
		}	
	}

	this.drawTextOverlays = function(alpha,offsetx,offsety) {
		ctx.font = "8px Emulogic";  
		for(var i = this.textOverlays.length-1;i>=0;i--) {
			var msg = this.textOverlays[i];
		    var f1 = msg.cycle/25.0;
		    var f2 = (msg.duration-msg.cycle)/25.0;
		    if (f1<0) f1 = 0;
		    if (f1>1) f1 = 1;
		    if (f2<0) f2 = 0;
		    if (f2>1) f2 = 1;
		    var f = f1*f2;

		    var tmp = ctx.globalAlpha;
		    ctx.globalAlpha = alpha*f;
		    if (msg.bubble) {
			    var speakerx = msg.speaker.getScreenX() + TILE_WIDTH/2 + offsetx;
			    var speakery = msg.speaker.getScreenY() + offsety;

		    	this.drawTextBubble(alpha,speakerx,speakery,msg);   	
		    } else {
			    ctx.fillStyle = "rgb(" + msg.r + "," + msg.g + "," + msg.b + ")";
			    ctx.fillText(msg.text,msg.x*TILE_WIDTH+8+offsetx,msg.y*TILE_HEIGHT+offsety+msg.offy);
			}
		    ctx.globalAlpha = tmp;
		}
	}	


	this.drawTextBubble = function(alpha,speakerx,speakery,msg) {
    	var lines = splitLineIntoLines(msg.text,24);
    	var bubbleWidth = 0;
    	var bubbleHeight = lines.length * 16;
    	for(var idx in lines) {
    		var l = lines[idx].length * 8;
    		if (l>bubbleWidth) bubbleWidth = l;
    	}

    	bubbleWidth +=24;
    	bubbleHeight += 16;

    	msg.x = speakerx;
	    if (y<bubbleHeight + 32) {
	    	// character is in the top of the screen:
	    	speakery += 32;
	    	msg.y = speakery + bubbleHeight + 32;
	    } else {
	    	msg.y = speakery - 32;
	    }

    	// coordinates of the top-left of the bubble:
    	var startx = msg.x - bubbleWidth/2;
    	if (startx<0) startx = 0;
    	if (startx + bubbleWidth > canvas.width-HUD_WIDTH) startx = (canvas.width-HUD_WIDTH) - bubbleWidth;
    	var starty = msg.y - bubbleHeight;
    	if (starty<0) starty = 0;
    	if (starty + bubbleHeight > canvas.height-64) starty = (canvas.height-64) - bubbleHeight;

    	// coordinates of the tip of the arrow towards the speaker:
    	if (speakerx<0) speakerx = 0;
    	if (speakerx> canvas.width-HUD_WIDTH) startx = (canvas.width-HUD_WIDTH);
    	if (speakery<0) speakery = 0;
    	if (speakery> canvas.height-64) starty = (canvas.height-64);

    	// draw the bubble:
    	drawBubble(startx, starty, bubbleWidth, bubbleHeight, speakerx, speakery, msg.bubbleColor);

    	// draw the text:
	    ctx.fillStyle = msg.textColor;
    	for(var idx in lines) {
		    ctx.fillText(lines[idx],startx+12,starty+20+idx*16);
		}		 
	}


	this.collectRespawnData = function() {
		this.respawnData = [];
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var obj = this.layers[i].objects[j];
				if ((obj instanceof AICharacter)) {
					obj.respawnID = this.respawnData.length;
					obj.respawnMap = this.ID;
					this.respawnData.push({x:obj.x, y:obj.y, lastCycleAlive: 0, respawn:obj.respawn, type:obj.getClassName()});
//					console.log("respawn data of " + obj.name + ": " + obj.respawnMap + ", " + obj.respawnID + " -> " + obj.respawn);
				}
			}
		}
	}


	this.respawnEnemies = function(cycle) {
//		console.log("Respawning enemies in map " + this.name);

		for(var i=0;i<this.respawnData.length;i++) {
			var rd = this.respawnData[i];
			if (rd.respawn>0 &&
				rd.lastCycleAlive<cycle-2000) {
				if (this.walkable(rd.x, rd.y)) {
					if (Math.random()<=rd.respawn) {
						// respawn:
						var enemy = eval("new " + rd.type + "()");
						enemy.x = rd.x;
						enemy.y = rd.y;
						enemy.map = this.ID;
						enemy.respawnID = i;
						enemy.respawnMap = this.ID;
						this.addObject(enemy,2);
	//					console.log("Respawned a " + rd.type);					
					} else {
						// do not respawn, this character is permanently dead
						rd.respawn = 0;
					}
				}
			}
		}
	}


	this.updateVisibility = function(sx,sy, game) {
		// flood fill:
		var open = [];
		var closed = new Array(this.width*this.height);
		for(var i = 0;i<this.width*this.height;i++) {
			closed[i] = false;
			this.visibility[i] = false;
		}
		var current = {x:sx, y:sy};
		var offset = 0;
		open.push(current);
		while(open.length>0) {
			current = open.pop();
			offset = current.x + current.y*this.width;
			closed[offset] = true;
			this.visibility[offset] = true;
			if (this.canSeeThrough(current.x,current.y,game)) {
				if (current.x>0 && !closed[offset-1]) open.push({x:current.x-1, y:current.y});
				if (current.y>0 && !closed[offset-this.width]) open.push({x:current.x, y:current.y-1});
				if (current.x<this.width-1 && !closed[offset+1]) open.push({x:current.x+1, y:current.y});
				if (current.y<this.height-1 && !closed[offset+this.width]) open.push({x:current.x, y:current.y+1});
				if (current.x>0 && current.y>0 && !closed[offset-1-this.width]) open.push({x:current.x-1, y:current.y-1});
				if (current.x<this.width-1 && current.y>0 && !closed[offset+1-this.width]) open.push({x:current.x+1, y:current.y-1});
				if (current.x>0 && current.y<this.height-1 && !closed[offset-1+this.width]) open.push({x:current.x-1, y:current.y+1});
				if (current.x<this.width-1 && current.y<this.height-1 && !closed[offset+1+this.width]) open.push({x:current.x+1, y:current.y+1});
			}
		}
	}

	this.completeVisibility = function() {
		for(var i = 0;i<this.width*this.height;i++) this.visibility[i] = true;
	}


	this.newLayer = function(tileEntry) {
		var l = this.layers.length;
		this.layers.push({tiles:new Array(this.width*this.height),
						  types:new Array(this.width*this.height),
						  seeThrough:new Array(this.width*this.height),
						  canDig:new Array(this.width*this.height),
						  objects:[],
						  tileEntry:tileEntry});
		var offset = 0;
		for(var y = 0;y<this.height;y++) {
			for(var x = 0;x<this.width;x++,offset++) {
				this.layers[l].tiles[offset] = 0;
				this.layers[l].types[offset] = 0;
				this.layers[l].seeThrough[offset] = 0;
				this.layers[l].canDig[offset] = 0;
			}
		}
		return l;
	}	

	this.setTile = function(offset,layer,tile) {
  	 	this.layers[layer].tiles[offset] = tile;
  	 	var tileEntry = this.layers[layer].tileEntry;
  	 	if (tile>0) {
	  		this.layers[layer].types[offset] = tileEntry.tileTypes[tile-1];
			this.layers[layer].seeThrough[offset] = tileEntry.seeThrough[tile-1];
			this.layers[layer].canDig[offset] = tileEntry.canDig[tile-1];
		} else {
	  		this.layers[layer].types[offset] = 0;
			this.layers[layer].seeThrough[offset] = 0;
			this.layers[layer].canDig[offset] = 0;
		}
	}


	// save maps:
	this.generateSaveData = function(objects) {
		var mapdata = {};
		mapdata.visibility = this.visibility;
		mapdata.messages = this.messages;
		mapdata.triggers = this.triggers;
		mapdata.respawnData = this.respawnData;
		var layersdata = [];
		for(var i = 0;i<this.layers.length;i++) {
			var layerdata = {};
			layerdata.tiles = this.layers[i].tiles;
			layerdata.types = this.layers[i].types;
			layerdata.seeThrough = this.layers[i].seeThrough;
			layerdata.objects = [];
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				layerdata.objects.push(objects.indexOf(this.layers[i].objects[j]));
			}
			layersdata.push(layerdata);
		}
		mapdata.layers = layersdata;

		return mapdata;
	}	


	this.generateObjectListForSaveData = function(objects) {
		for(var i = 0;i<this.layers.length;i++) {
			for(var j = 0;j<this.layers[i].objects.length;j++) {
				var o = this.layers[i].objects[j];
				objects = o.generateObjectListForSaveData(objects);
			}
		}		
		return objects;
	}


	this.offsetx = 0;
	this.offsety = 0;
	this.desiredoffsetx = 0;
	this.desiredoffsety = 0;
	this.neighbors = [];
	this.warps = [];	// This variable stores any objects that just left the map in the current cycle
						// it is used for the AI module of the characters, so that if an AI saw a character
						// leave the map, the AI can remember that the character was there.
	this.layers = [];
	this.bridges = [];
	this.bridgeDestinations = [];
	this.messages = [];
	this.triggers = [];
	this.textOverlays = [];
	initAIRules(this);

	if (fileName!=undefined) {
		console.log("Loading map: '" + fileName + "' ...");
	  	var xmlhttp = new XMLHttpRequest();
	  	xmlhttp.overrideMimeType("text/xml");
		xmlhttp.open("GET",fileName,false);	
		xmlhttp.send();
	//	console.log("Request response: '" + xmlhttp.statusText + "'.");
		var xmlDoc=xmlhttp.responseXML;

		// parse map:
		var mapElement = xmlDoc.documentElement;
		var properties = mapElement.getElementsByTagName("properties")[0].getElementsByTagName("property");
		this.name = "map";
		for(var i = 0;i<properties.length;i++) {
			if (properties[i].attributes.getNamedItem("name").nodeValue == "name") 
				this.name = properties[i].attributes.getNamedItem("value").nodeValue;
		}

		var tilesets = [];
		var sourceElements = mapElement.getElementsByTagName("tileset");
		for(var i = 0;i<sourceElements.length;i++) {
			var sourceElement = sourceElements[i];
			var firstgid = eval(sourceElement.attributes.getNamedItem("firstgid").nodeValue);
			var sourceImageElement = sourceElement.getElementsByTagName("image")[0];
			var sourceImage = sourceImageElement.attributes.getNamedItem("source").nodeValue;
			tilesets.push({tileset:game.tiles[sourceImage], firstgid:firstgid});
		}

	//	console.log("Map name: '" + this.name + "'");
		var layerElements = mapElement.getElementsByTagName("layer");

		var tmpStr = mapElement.attributes.getNamedItem("width").nodeValue;
	//	console.log("Map width: '" + tmpStr + "'");
		this.width = parseInt(tmpStr);

		tmpStr = mapElement.attributes.getNamedItem("height").nodeValue;
	//	console.log("Map height: '" + tmpStr + "'");
		this.height = parseInt(tmpStr);

		initAIRules(this);
		parseAIRules(this,mapElement);

	//	console.log("Map layers: '" + layerElements.length + "'");


//		this.tileEntry = Game.tiles[sourceImage];
		var layerTable = {};
		this.visibility = new Array(this.width*this.height);
		for(var i = 0;i<layerElements.length;i++) {
			var layerElement = layerElements[i];
			var tiles = layerElement.getElementsByTagName("data")[0].getElementsByTagName("tile");
			var offset = 0;
			for(var y = 0;y<this.height;y++) {
				for(var x = 0;x<this.width;x++,offset++) {
//					console.log("setting tile " + offset + "/" + tiles.length + " of layer " + i);

					var tmpStr = eval(tiles[offset].attributes.getNamedItem("gid").nodeValue);
					var tileset = null;
					for(var j = 0;j<tilesets.length;j++) {
						if (tmpStr>=tilesets[j].firstgid) tileset = j;
					}
					if (tileset!=null) {
						tmpStr -= tilesets[tileset].firstgid - 1;
						var layerId = layerTable[i + "-" + tileset];
						if (layerId == undefined) {
							layerId = this.newLayer(tilesets[tileset].tileset);
							layerTable[i + "-" + tileset] = layerId;
						}
//						console.log(x + "," + y + " -> " + tmpStr);
						this.setTile(offset,layerId,tmpStr);
						// console.log(x + "," + y + " -> " + tmpStr + "(" + tileset + ")");
					}
					if (i==0) this.visibility[offset] = false;
				}
			}
		}

		var objectlayers = mapElement.getElementsByTagName("objectgroup");
	//	console.log(TILE_WIDTH_ORIGINAL);
	//	console.log(objectlayers.length + " object layers...");
		for(var i = 0;i<objectlayers.length;i++) {
			var layerElement = objectlayers[i];
			var objects = layerElement.getElementsByTagName("object");
	//		console.log("Processing layer: " + layerElement.attributes.getNamedItem("name").nodeValue + " with " + objects.length + " objects");
			for(var j = 0;j<objects.length;j++) {
				var objectElement = objects[j];
	//			console.log("parsing object with type: " + objectElement.attributes.getNamedItem("type").nodeValue);

				if (objectElement.attributes.getNamedItem("type").nodeValue=="bridge") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var width = parseInt(objectElement.attributes.getNamedItem("width").nodeValue);
					var height = parseInt(objectElement.attributes.getNamedItem("height").nodeValue);
					var name = objectElement.attributes.getNamedItem("name").nodeValue;
					this.bridges.push({x:Math.floor(x/TILE_WIDTH_ORIGINAL), y:Math.floor(y/TILE_HEIGHT_ORIGINAL), 
									   width:Math.floor(width/TILE_WIDTH_ORIGINAL), height:Math.floor(height/TILE_HEIGHT_ORIGINAL), id:name});
				}
				if (objectElement.attributes.getNamedItem("type").nodeValue=="bridgedestination") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var width = parseInt(objectElement.attributes.getNamedItem("width").nodeValue);
					var height = parseInt(objectElement.attributes.getNamedItem("height").nodeValue);
					var name = objectElement.attributes.getNamedItem("name").nodeValue;
					var appearWalking = true;
					var appearDirection = DIRECTION_NONE;
					if (objectElement.attributes.getNamedItem("appearWalking")!=null) {
						appearWalking = objectElement.attributes.getNamedItem("appearWalking").nodeValue == "true";
					}
					if (objectElement.attributes.getNamedItem("appearDirection")!=null) {
						appearDirection = parseInt(objectElement.attributes.getNamedItem("appearDirection").nodeValue);
					}
					this.bridgeDestinations.push({x:Math.floor(x/TILE_WIDTH_ORIGINAL), y:Math.floor(y/TILE_WIDTH_ORIGINAL), 
									   			  width:Math.floor(width/TILE_WIDTH_ORIGINAL), height:Math.floor(height/TILE_HEIGHT_ORIGINAL), id:name,
									   			  appearWalking:appearWalking, appearDirection:appearDirection});					
	//				console.log("destination...");
				}
				if (objectElement.attributes.getNamedItem("type").nodeValue=="burroweditem") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var name = objectElement.attributes.getNamedItem("name").nodeValue;
					var item = eval(name);
					item.x = Math.floor(x/TILE_WIDTH_ORIGINAL);
					item.y = Math.floor(y/TILE_HEIGHT_ORIGINAL);
					item.burrowed = true;
					initAIRules(item);
					parseAIRules(item, objectElement);
					this.addObject(item,1);
				}
				if (objectElement.attributes.getNamedItem("type").nodeValue=="item") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var name = objectElement.attributes.getNamedItem("name").nodeValue;
					var item = eval(name);
					item.x = Math.floor(x/TILE_WIDTH_ORIGINAL);
					item.y = Math.floor(y/TILE_HEIGHT_ORIGINAL);
//					console.log("new item: " + name);
					initAIRules(item);
					parseAIRules(item,objectElement);
					this.addObject(item,1);
				}
				if (objectElement.attributes.getNamedItem("type").nodeValue=="character" ||
					objectElement.attributes.getNamedItem("type").nodeValue=="enemy" ||
					objectElement.attributes.getNamedItem("type").nodeValue=="npc") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var name = objectElement.attributes.getNamedItem("name").nodeValue;
					console.log("Creating character");
					var item = eval(name);
					item.x = Math.floor(x/TILE_WIDTH_ORIGINAL);
					item.y = Math.floor(y/TILE_HEIGHT_ORIGINAL);
					/*
					// additional inventory:
					var inventoryElements = objectElement.getElementsByTagName("item");
					if (inventoryElements!=null) {
						for(var k = 0;k<inventoryElements.length;k++) {
							var inventoryElement = inventoryElements[k];
							var p = inventoryElement.attributes.getNamedItem("probability");
//							console.log("additional inventory item: " + inventoryElement.textContent);
							if (p!=null) {
								p = eval(p.nodeValue);
							} else {
								p = 1.0;
							}
							if (Math.random()<=p) {
								item.inventory.push(eval(inventoryElement.textContent));
							}
						}
					}
					// additional AI rules:
					parseAIRules(item,objectElement);
					*/
					item.xml = objectElement;
					item.decodeCharacterFromXML(false);
					this.addObject(item,2);
				}
				if (objectElement.attributes.getNamedItem("type").nodeValue=="message") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var width = parseInt(objectElement.attributes.getNamedItem("width").nodeValue);
					var height = parseInt(objectElement.attributes.getNamedItem("height").nodeValue);
					var name = objectElement.attributes.getNamedItem("name").nodeValue;
					var repeat = false;
					if (objectElement.attributes.getNamedItem("repeat")!=null &&
						objectElement.attributes.getNamedItem("repeat").nodeValue=="true") repeat = true;
					var topic = objectElement.attributes.getNamedItem("topic");
					var topic_text = null;
					if (topic!=null) {
						topic = topic.nodeValue;
						topic_text = objectElement.attributes.getNamedItem("topictext").nodeValue;
					}
					var tmp = {x:Math.floor(x/TILE_WIDTH_ORIGINAL), y:Math.floor(y/TILE_WIDTH_ORIGINAL),
										width:Math.floor(width/TILE_WIDTH_ORIGINAL), height:Math.floor(height/TILE_WIDTH_ORIGINAL), 
										bubble:false,
										repeat:repeat,
										text:name,
										topic:topic, topictext: topic_text};
					initAIRules(tmp);
					parseAIRules(tmp,objectElement);
					this.messages.push(tmp);
				}
				if (objectElement.attributes.getNamedItem("type").nodeValue=="bubble") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var width = parseInt(objectElement.attributes.getNamedItem("width").nodeValue);
					var height = parseInt(objectElement.attributes.getNamedItem("height").nodeValue);
					var name = objectElement.attributes.getNamedItem("name").nodeValue;
					var repeat = false;
					if (objectElement.attributes.getNamedItem("repeat")!=null &&
						objectElement.attributes.getNamedItem("repeat").nodeValue=="true") repeat = true;
					var topic = objectElement.attributes.getNamedItem("topic");
					var topic_text = null;
					if (topic!=null) {
						topic = topic.nodeValue;
						topic_text = objectElement.attributes.getNamedItem("topictext").nodeValue;
					}
					var tmp = {x:Math.floor(x/TILE_WIDTH_ORIGINAL), y:Math.floor(y/TILE_WIDTH_ORIGINAL),
										width:Math.floor(width/TILE_WIDTH_ORIGINAL), height:Math.floor(height/TILE_WIDTH_ORIGINAL), 
										bubble:true,
										repeat:repeat,
										text:name,
										topic:topic, topictext: topic_text};
					initAIRules(tmp);
					parseAIRules(tmp,objectElement);
					this.messages.push(tmp);
	//				console.log(name + " -> " + width + "," + height);
				}
				if (objectElement.attributes.getNamedItem("type").nodeValue=="script") {
					var x = parseInt(objectElement.attributes.getNamedItem("x").nodeValue);
					var y = parseInt(objectElement.attributes.getNamedItem("y").nodeValue);
					var width = parseInt(objectElement.attributes.getNamedItem("width").nodeValue);
					var height = parseInt(objectElement.attributes.getNamedItem("height").nodeValue);
					var repeat = false;
					if (objectElement.attributes.getNamedItem("repeat")!=null &&
						objectElement.attributes.getNamedItem("repeat").nodeValue=="true") repeat = true;

					// load the script
					var actions = [];
					for(var k = 0;k<objectElement.childNodes.length;k++) {
						var actionElement = objectElement.childNodes[k];
						var action = null;
						if (actionElement.tagName!=undefined) action = decodeRuleEffectFromXML(actionElement);
						if (action!=null) actions.push(action);
					}
					var tmp = {x:Math.floor(x/TILE_WIDTH_ORIGINAL), y:Math.floor(y/TILE_WIDTH_ORIGINAL),
										width:Math.floor(width/TILE_WIDTH_ORIGINAL), height:Math.floor(height/TILE_WIDTH_ORIGINAL), 
										repeat:repeat,
										script:actions};
					initAIRules(tmp);
					parseAIRules(tmp,objectElement);
					this.triggers.push(tmp);
				}
			}
		}
	}
}


splitLineIntoLines = function(raw_text, lineLength) {
	var lines = [];
	var text = raw_text + " ";

	var currentLine = "";
	var currentWord = "";
	for(var i in text) {
		if (text[i]==' ') {
			// end of word:
			var l1 = currentLine.length;
			var l2 = currentWord.length;

			if (l1+l2+1 > lineLength) {
				lines.push(currentLine);
				currentLine = currentWord;
				currentWord = "";
			} else {
				if (currentLine == "") currentLine = currentWord;
							      else currentLine = currentLine + " " + currentWord;
				currentWord = "";
			}
		} else {
			currentWord = currentWord + text[i];
		}
	}
	if (currentLine != "") lines.push(currentLine);

	return lines;
}


drawBubble = function(x, y, width, height, pointx, pointy, fillColor) {
    // make sure the point s not inside of the bubble:
    if (pointx>x && pointx<x+width &&
    	pointy>y && pointy<y+height) {
	    ctx.fillStyle = fillColor;  
	    ctx.strokeStyle = '#000000';  
	    ctx.beginPath();  
	    ctx.rect(x, y, width, height);  
	    ctx.closePath();  
	    ctx.fill();  
	    ctx.stroke();			    
    	return;
	}	

    var vx = x - pointx;
    var vy = y - pointy;
    var n = Math.sqrt(vx*vx + vy*vy);
    vx /= n;
    vy /= n;
    var wx = -vy;
    var wy = vx;

    var cx = x + width/2;
    var cy = y + height/2;

    var triangleWidth = Math.floor(width-50)/10;
    if (triangleWidth<0) triangleWidth = 0;
    if (triangleWidth>10) triangleWidth = 10;
    triangleWidth+=10;

    // 1) add the arrow tip to a list
    var points = [{x:pointx,y:pointy,type:0}];

    // 2) Compute the 2 collision points of the arrow with the rectangle, and add them to a list
	points.push(lineRectangleCollision(pointx,pointy,Math.floor(cx + wx*triangleWidth), Math.floor(cy + wy*triangleWidth),
								  	   x,y,x+width,y+height));
	points.push(lineRectangleCollision(pointx,pointy,Math.floor(cx - wx*triangleWidth), Math.floor(cy - wy*triangleWidth),
									   x,y,x+width,y+height));

	// 3) Add all the 4 vertives of the rectangle:
	points.push({x:x, y:y, type:1});
	points.push({x:x+width, y:y, type:1});
	points.push({x:x+width, y:y+height, type:1});
	points.push({x:x, y:y+height, type:1});

	// 4) sort those vertices clock-wise
	for(var idx in points) {
		points[idx].angle = Math.atan2(points[idx].y - cy, points[idx].x - cx);
	}
	points = points.sort(function(p1,p2) {return p1.angle - p2.angle;});

	// 5) if after sorting, any point of type 1 is isolated in between points of type 0, it is removed (that means that
	//	  such point is inside of the pointing triangle):
	{
		var toDelete = -1;
		var lastIndex = points.length-1;
		var last = points[points.length-1].type;
		var count = 0;
		for(var idx2 = points.length-1;points[idx2].type==last;idx2--) {
			count++;
		}
		for(var idx2 in points) {
			if (points[idx2].type==last) {
				count++;
				lastIndex = idx2;
			} else {
				if (last==1 && count==1) {
					toDelete = lastIndex;
				} 
				last = points[idx2].type;
				count = 1;
				lastIndex = idx2;
			}
		}
		if (toDelete!=-1) {
			points.splice(toDelete,1);
		}
	}
	
	// 5) draw them
    ctx.fillStyle = fillColor;  
    ctx.strokeStyle = '#000000';  
    ctx.beginPath();      
    ctx.moveTo(points[points.length-1].x, points[points.length-1].y);
	for(var idx in points) {
	    ctx.lineTo(points[idx].x, points[idx].y);
	}
    ctx.closePath();  
    ctx.fill();  
    ctx.stroke();
}

// This method assumes that (lp1x,lp1y) is outside of the rectangle, and (lp2x,lp2y) is inside
lineRectangleCollision = function(lp1x,lp1y, lp2x, lp2y, rp1x, rp1y, rp2x, rp2y) {
	var points = [];
	var vx = lp2x-lp1x;
	var vy = lp2y-lp1y;

	// collision 1: left side
	if (lp1x<=rp1x && vx!=0) {
		var k = (rp1x - lp1x)/vx;
		var collission_y = lp1y + k*vy;
		if (collission_y>=rp1y && collission_y<rp2y) return {x:rp1x,y:collission_y, type:0};
	}
	// collision 2: right side
	if (lp1x>=rp2x && vx!=0) {
		var k = (rp2x - lp1x)/vx;
		var collission_y = lp1y + k*vy;
		if (collission_y>=rp1y && collission_y<rp2y) return {x:rp2x,y:collission_y, type:0};
	}
	// collision 3: top side
	if (lp1y<=rp1y && vy!=0) {
		var k = (rp1y - lp1y)/vy;
		var collission_x = lp1x + k*vx;
		if (collission_x>=rp1x && collission_x<rp2x) return {x:collission_x,y:rp1y, type:0};
	}
	// collision 3: bottom side
	if (lp1y>=rp2y && vy!=0) {
		var k = (rp2y - lp1y)/vy;
		var collission_x = lp1x + k*vx;
		if (collission_x>=rp1x && collission_x<rp2x) return {x:collission_x,y:rp2y, type:0};
	}
	return null;
}


