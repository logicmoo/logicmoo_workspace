// High-level AI: -------------------------------------------------------------------------------------
//

/*
	- Character AI is implemented as a very simple rule-based system. There are 3 memory containers, containing WMEs
	(Working Memory Elements):
		- perceptions (change at every frame)
		- short-term memory (WMEs here have a timer that decays, when reached 0, they are forgotten)
		- long-term memory (WMEs here are remembered forever)
	- When a WME has been in short-term memory or perception for a long time, it goes to long-term
	- Each character defines a set of rules and initial memory elements, which determine its behavior.
*/

HighLevelAI.prototype = new Behavior();
function HighLevelAI() {

	this.freezeThreshold = 50*60;	// activation needed to freeze into long-term memory (50*60 is one minute worth of activation)

	this.shortTermMemory = [];
	this.longTermMemory = [];
	this.rules = [];
	this.actions = [];
	this.lastTimeExecuted = 0;

	this.generateSaveData = function(objects, data) {

		// behaviors:
		data.rules = [];
		for(var i = 0;i<this.rules.length;i++) {
			data.rules.push({priority:this.rules[i].priority, 
							   id:this.rules[i].id, 
							   className:this.rules[i].className, 
							   data:this.rules[i].behavior.generateSaveData({})});
		}

		// Working memory:
		data.shortTermMemory = [];
		data.longTermMemory = [];
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			var wme = this.shortTermMemory[i];
			if (wme.params instanceof Object) {
				var idx = objects.indexOf(wme.params);
				if (idx==-1) {
					objects.push(wme.params);
					idx = objects.indexOf(wme.params);
					data.shortTermMemory.push([wme.type, idx, wme.activation, wme.perception, null]);
				} else {
					data.shortTermMemory.push([wme.type, idx, wme.activation, wme.perception, null]);
				}
			} else {
				data.shortTermMemory.push([wme.type, -1, wme.activation, wme.perception, wme.params]);
			}
		}
		for(var i = 0;i<this.longTermMemory.length;i++) {
			var wme = this.longTermMemory[i];
			if (wme.params instanceof Object) {
				var idx = objects.indexOf(wme.params);
				if (idx==-1) {
					objects.push(wme.params);
					idx = objects.indexOf(wme.params);
					data.shortTermMemory.push([wme.type, idx, wme.activation, wme.perception, null]);
				} else {
					data.shortTermMemory.push([wme.type, idx, wme.activation, wme.perception, null]);
				}
			} else {
				data.shortTermMemory.push([wme.type, -1, wme.activation, wme.perception, wme.params]);
			}
		}

		return data;
	}


	this.reconstructFromSaveData = function(objects, object, data) {

		// behaviors:
        console.log("object has behaviors:" + data.rules);
        for(var j = 0;j<data.rules.length;j++) {
          tmp = "new " + data.rules[j].className + "()";
          console.log("adding behavior: " + data.rules[j].className);
          var behavior = eval(tmp);
          behavior.state = data.rules[j].data.state;
          this.rules.push({priority:data.rules[j].priority,
                           id:data.rules[j].id,
                           behavior:behavior});
        }

		// Working memory:
		this.shortTermMemory = [];
		this.longTermMemory = [];
		console.log("object " + object.name +  " with workming memory " + data.shortTermMemory.length + " - " + data.longTermMemory.length);
		for(var i = 0;i<data.shortTermMemory.length;i++) {
			var tmp = data.shortTermMemory[i];
			if (tmp[1]==-1) {
				this.shortTermMemory.push({type:tmp[0], params:tmp[4], activation:tmp[2], perception:tmp[3]});
			} else {
				this.shortTermMemory.push({type:tmp[0], params:objects[tmp[1]], activation:tmp[2], perception:tmp[3]});
			}
		}
		for(var i = 0;i<data.longTermMemory.length;i++) {
			var tmp = data.longTermMemory[i];
			if (tmp[1]==-1) {
				this.longTermMemory.push({type:tmp[0], params:tmp[4], activation:tmp[2], perception:tmp[3]});
			} else {
				this.longTermMemory.push({type:tmp[0], params:objects[tmp[1]], activation:tmp[2], perception:tmp[3]});
			}
		}
	}

	
	this.perceive = function(character, map, time, game) {
		var perception = map.objectsInRadius(character.x, character.y, character.sightRadius,game);
		for(var i = 0;i<perception.length;i++) {
			this.addPerceptionWME({type:"object", params:perception[i]}, time);
		}		

		var sqr = character.sightRadius*character.sightRadius;
		for(var i = 0;i<map.warps.length;i++) {
			var warp = map.warps[i];
			var dx = warp.x-character.x;
			var dy = warp.y-character.y;
			var d = dx*dx+dy*dy;
			if (d<=sqr) this.addPerceptionWME({type:"warpedobject", params:map.warps[i].object}, time);
		}
	}


	this.update = function(character, map, time, game) {
		var interval = time - this.lastTimeExecuted;
		this.lastTimeExecuted = time;

		// update activation in shortTermMemory:
		var toDelete = [];
		var toFreeze = [];
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			var wme = this.shortTermMemory[i];
			if (wme.perception) wme.activation--;
						   else wme.activation-=interval;

			if (wme.activation<=0) toDelete.push(wme);
			if (!wme.perception &&
				(time - wme.time)>=this.freezeThreshold) toFreeze.push(wme);
		}
		for(var i = 0;i<toDelete.length;i++) {
			var idx = this.shortTermMemory.indexOf(toDelete[i]);
			this.shortTermMemory.splice(idx,1);
		}
		for(var i = 0;i<toFreeze.length;i++) {
			var idx = this.shortTermMemory.indexOf(toFreeze[i]);
			this.longTermMemory.push(this.shortTermMemory[idx]);
			this.shortTermMemory.splice(idx,1);
		}

		// execute rules:
		for(var i = 0;i<this.rules.length;i++) {
			var action = this.rules[i].behavior.update(character, map, time, game);
			if (action!=null) {
				this.actions.push({action:action, priority:this.rules[i].priority});
			}
		}

		var highestPriorityAction = null;
		var highestPriority = null;
		for(var i = 0;i<this.actions.length;i++) {
			var tmp = this.actions[i];
			if (highestPriority == null || tmp.priority > highestPriority) {
				highestPriorityAction = tmp.action;
				highestPriority = tmp.priority;
//				console.log("Highest priority action: " + highestPriorityAction);
			}

		}

		// clean-up:
		this.actions = [];

		return highestPriorityAction;
	}

	this.getWME = function(type) {
		var bestwme = null;
		var bestActivation = 0;
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			var wme = this.shortTermMemory[i];
			if (wme.type==type) {
				if (bestwme==null || wme.activation>bestActivation) {
					bestwme = wme;
					bestActivation = wme.activation;
				}
			}
		}
		for(var i = 0;i<this.longTermMemory.length;i++) {
			var wme = this.longTermMemory[i];
			if (wme.type==type) {
				if (bestwme==null || wme.activation>bestActivation) {
					bestwme = wme;
					bestActivation = wme.activation;
				}
			}
		}
		return bestwme;
	}

	this.getWME2 = function(type,param) {
		var bestwme = null;
		var bestActivation = 0;
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			var wme = this.shortTermMemory[i];
			if (wme.type==type && wme.params==param) {
				if (bestwme==null || wme.activation>bestActivation) {
					bestwme = wme;
					bestActivation = wme.activation;
				}
			}
		}
		for(var i = 0;i<this.longTermMemory.length;i++) {
			var wme = this.longTermMemory[i];
			if (wme.type==type && wme.params==param) {
				if (bestwme==null || wme.activation>bestActivation) {
					bestwme = wme;
					bestActivation = wme.activation;
				}
			}
		}
		return bestwme;
	}	

	this.getAllWMEs = function(type) {
		var wmes = [];
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			var wme = this.shortTermMemory[i];
			if (wme.type==type) wmes.push(wme);
		}
		for(var i = 0;i<this.longTermMemory.length;i++) {
			var wme = this.longTermMemory[i];
			if (wme.type==type) wmes.push(wme);
		}
		return wmes;
	}

	this.WMEexists = function(type) {
		if (this.getWME(type)!=null) return true;
		return false;
	}

	this.WMEexists2 = function(type,param) {
		if (this.getWME2(type,param)!=null) return true;
		return false;
	}

	this.addShortTermWME = function(wme, time, activation) {
		for(var i = 0;i<this.longTermMemory.length;i++) {
			var wme2 = this.longTermMemory[i];
			if (wme2.type==wme.type &&
				wme2.params==wme.params) return;
		}
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			var wme2 = this.shortTermMemory[i];
			if (wme2.type==wme.type &&
				wme2.params==wme.params) {
				wme2.activation = activation;	// reinforce existing wme
				wme.perception = false;
				return;
			}
		}
		wme.time = time;
		wme.activation = activation;
		wme.perception = false;
		this.shortTermMemory.push(wme);
	}

	this.addPerceptionWME = function(wme, time) {
		for(var i = 0;i<this.longTermMemory.length;i++) {
			var wme2 = this.longTermMemory[i];
			if (wme2.type==wme.type &&
				wme2.params==wme.params) return;
		}
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			var wme2 = this.shortTermMemory[i];
			if (wme2.type==wme.type &&
				wme2.params==wme.params) {
				wme2.activation = 2;	// reinforce existing wme
				wme.perception = true;
				return;
			}
		}
		wme.time = time;
		wme.activation = 2;
		wme.perception = true;
		this.shortTermMemory.push(wme);
	}	

	this.removeShortTermWME = function(wme) {
		for(var i = 0;i<this.shortTermMemory.length;i++) {
			if (this.shortTermMemory[i] == wme) {
				this.shortTermMemory.splice(i,1);
				return;
			}
		}
	}
}

WMEToString = function(wme) {
	var tmp = wme.type + "(";
	if (wme.params==null) {
		tmp += ")";
	} else {
		if (wme.params instanceof Array) {
			tmp += "[ ";
			for(var i = 0;i<wme.params.length;i++) {
				var param = wme.params[i];
				if (param instanceof Object) {
					tmp += param.name + "(" + param.x + "," + param.y + " ";
				} else {
					tmp += param;
				}
				tmp += "]";
			}
		} else if (wme.params instanceof Object) {
			tmp += wme.params.name + "(" + wme.params.x + "," + wme.params.y + ")";
		} else {
			tmp += wme.params + ")";
		}
	}
	return tmp;
}

AIDebuggerDraw = function(focus) {
  var tmp = ctx.globalAlpha;
  ctx.globalAlpha =1 ;
  ctx.strokeStyle = "#000000";
  ctx.fillStyle = '#000000';  
  ctx.beginPath();  
  ctx.rect(0,0, 400, 200);  
  ctx.closePath();  
  ctx.fill();  

  // focus info:
  ctx.font = "8px Emulogic";  
  ctx.fillStyle = "rgb(255,255,255)";
  ctx.fillText(focus.name + " " + focus.x + "," + focus.y + " [ state: " + focus.state + ", hp:" + focus.hp + ", attack: " + focus.getAttack() + "]", 8, 8);

  // short term memory:
  var AI = focus.AI;
  var y = 24;
  for(var i=0;i<AI.shortTermMemory.length;i++) {
  	var wme = AI.shortTermMemory[i];
  	ctx.fillText("" + wme.time + " - " + WMEToString(wme) + " : " + wme.activation, 8, y);
  	y+=8;
  }
  ctx.fillText(" -------", 16, y);
  y+=8;
  for(var i=0;i<AI.longTermMemory.length;i++) {
  	var wme = AI.longTermMemory[i];
  	ctx.fillText("" + wme.time + " - " + WMEToString(wme), 8, y);
  	y+=8;
  }
  ctx.fillText(" -- CONVERSATIONS: --", 16, y);
  y+=8;
  for(var topic in focus.conversationKnowledge) {
  	ctx.fillText("Topic: " + topic, 8, y);
  	y+=8;
  }
  for(var i=0;i<focus.conversationEngines.length;i++) {
  	var ce = focus.conversationEngines[i];
  	ctx.fillText("[" + ce.state + "," + ce.state_timer + "]: " + ce.otherCharacter.name, 8, y);
  	y+=8;
  }
  ctx.fillText(" -- STORY STATE: --", 16, y);
  y+=8;
  for(var key in Game.storyState) {
  	ctx.fillText("Game, " + key + ": "  + Game.storyState[key], 8, y);
  	y+=8;
  }
  for(var key in Game.maps[focus.map].storyState) {
  	ctx.fillText("Map, " + key + ": "  + Game.maps[focus.map].storyState[key], 8, y);
  	y+=8;
  }
  for(var key in focus.storyState) {
  	ctx.fillText("Character, " + key + ": "  + focus.storyState[key], 8, y);
  	y+=8;
  }


  ctx.globalAlpha = tmp;
}
