function Object() {
	this.name = null;
	this.x = 0;
	this.y = 0;
	this.width = 1;
	this.height = 1;
	this.map = 0;
	this.animations = {};
	this.currentAnimation = null;
	this.takeable = false;	// by default, objects cannot be taken

	this.decodeObjectFromXML = function() {
		this.name = this.xml.attributes.getNamedItem("name").nodeValue;

		this.time = 0;

		// animations:
		this.animations = {};
		var animationElements = this.xml.getElementsByTagName("animation");
		for(var i = 0;i<animationElements.length;i++) {
			var animationElement = animationElements[i];
			var animationName =  animationElement.attributes.getNamedItem("name").nodeValue;
//			console.log("Loading animation for " + animationName);
			var animation = Animation.fromXML(animationElement);
			this.animations[animationName] = animation;
		}		

		// attributes:
		var attributesElements = this.xml.getElementsByTagName("attribute");
		for(var i = 0;i<attributesElements.length;i++) {
			var attributesElement = attributesElements[i];
			var name = attributesElement.attributes.getNamedItem("name").nodeValue;
			var value = attributesElement.attributes.getNamedItem("value").nodeValue;
			// parse the value: it can be something like this:
			// 1
			// 1,2
			// 1-5,3,10-20
			value = value.split(",");
			value = value[Math.floor(Math.random()*value.length)];
			value = value.split("-");
			if (value.length==0) {
				value = 0;
			} else if (value.length==2) {
				value = Math.floor(Math.random()*((eval(value[1])+1)-eval(value[0]))+value[0]);
			} else {
				value = eval(value[0]);
			}
			this[name] = value;
		}

		initAIRules(this);
		parseAIRules(this,this.xml);		
	}	


	this.getClassName = function() {
		return this.constructor.name;
	}

	this.draw = function(alpha,offsetx,offsety,game) {
		if (this.currentAnimation!=null) {
			var animation = this.animations[this.currentAnimation];
			if (animation!=null) {
				animation.draw(this.x*TILE_WIDTH + offsetx, this.y*TILE_HEIGHT + offsety,alpha,game);		
			}
		}
	}

	this.getTile = function() {
		if (this.currentAnimation!=null) {
			var animation = this.animations[this.currentAnimation];
			if (animation!=null) {
				return animation.getTile();
			}
		}
		return -1;
	}


	this.getTileEntry = function(game) {
		if (this.currentAnimation!=null) {
			var animation = this.animations[this.currentAnimation];
			if (animation!=null) {
				return animation.getTileEntry(game);
			}
		}
		return null;
	}


	this.update = function(character, map, game) {
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
			var otherCharacter = null;
			var retVal = executeRuleEffect(this.actionsToExecute[0], character, map, game, otherCharacter);
			if (retVal!=undefined) this.actionsToExecute.splice(0,1);
		}

		if (this.currentAnimation!=null && this.animations[this.currentAnimation]!=null) {
			this.animations[this.currentAnimation].update();
			this.width = this.animations[this.currentAnimation].getWidth();
			this.height = this.animations[this.currentAnimation].getHeight();
		}

		return true;
	}

	this.walkable = function() {
		return true;
	}

	this.generateObjectListForSaveData = function(objects) {
		var idx = objects.indexOf(this);
		if (idx==-1) {
			objects.push(this);
			if (this.itemInside!=undefined) objects = this.itemInside.generateObjectListForSaveData(objects);
			if (this.inventory!=undefined) {
				for(var i = 0;i<this.inventory.length;i++) {
					if (this.inventory[i]!=null) objects = this.inventory[i].generateObjectListForSaveData(objects);
				}
			}
			if (this.equippedItems!=undefined) {
				for(var i = 0;i<this.equippedItems.length;i++) {
					if (this.equippedItems[i]!=null) objects = this.equippedItems[i].generateObjectListForSaveData(objects);
				}
			}
		}
		return objects;
	}

	this.generateSaveData = function(objects, data) {
		data.className = this.getClassName();
		data.name = this.name;
		data.x = this.x;
		data.y = this.y;
		data.map = this.map;
		data.burrowed = this.burrowed;

		// object-specific variables:
		if (this.value!=undefined) data.value = this.value;
		if (this.keyID!=undefined) data.keyID = this.keyID;
		if (this.bonus!=undefined) data.bonus = this.bonus;
		if (this.gold!=undefined) data.gold = this.gold;
		if (this.itemType!=undefined) data.itemType = this.itemType;
		if (this.canChop!=undefined) data.canChop = this.canChop;
		if (this.attackBonus!=undefined) data.attackBonus = this.attackBonus;
		if (this.defenseBonus!=undefined) data.defenseBonus = this.defenseBonus;
		if (this.magicMultiplier!=undefined) data.magicMultiplier = this.magicMultiplier;
		if (this.examinedOnce!=undefined) data.examinedOnce = this.examinedOnce;
		if (this.state!=undefined) data.state = this.state;
		if (this.state_timer!=undefined) data.state_timer = this.state_timer;
		if (this.direction!=undefined) data.direction = this.direction;
		if (this.duration!=undefined) data.duration = this.duration;
		if (this.damage!=undefined) data.damage = this.damage;
		if (this.radius!=undefined) data.radius = this.radius;
		if (this.spellInside!=undefined) data.spellInside = this.spellInside;
		if (this.itemInside!=undefined) data.itemInside = objects.indexOf(this.itemInside);
		if (this.owner!=undefined) data.owner = objects.indexOf(this.owner);
		if (this.target!=undefined) data.target = objects.indexOf(this.target);
		if (this.storyStateRules!=undefined) data.storyStateRules = this.storyStateRules;
		if (this.eventRules!=undefined) data.eventRules = this.eventRules;
		if (this.timerEventRules!=undefined) data.timerEventRules = this.timerEventRules;
		if (this.actionsToExecute!=undefined) data.actionsToExecute = this.actionsToExecute;
		if (this.pressureRequired!=undefined) data.pressureRequired = this.pressureRequired;
		if (this.animations!=undefined) {
			data.animations = {};
			for (var key in this.animations) {
			  if (this.animations.hasOwnProperty(key) && this.animations[key]!=null) {
			  	var str = this.animations[key].generateSaveString();
//			  	console.log(key + " -> " + str);
    			data.animations[key] = str;
			  }
			}
		}
		if (this.currentAnimation!=undefined) data.currentAnimation = this.currentAnimation;

		return data;
	}

}


/*
 *	------------------------ ITEMS ------------------------
 */

Item.prototype = new Object();

function Item(name, animation) {
	this.name = name;
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.takeable = true;
	this.useable = false;
	this.equipable = false;
	this.useUponTake = false;
	this.attackBonus = 0;
	this.defenseBonus = 0;
	this.magicMultiplier = 1;
	this.value = 1;	// value in gold coins of the object

	initAIRules(this);

	this.effect = function(character, map, game) {
		fireEventRule(this, "use");
	}

	this.getClassName = function() {
		return "Item";
	}	
}

Food.prototype = new Item("Food",null);
function Food(name, animation, value) {
	this.name = name;
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.takeable = true;
	this.useable = true;
	this.equipable = false;
	this.burrowed = false;
	this.value = value;

	initAIRules(this);

	this.effect = function(character, map, game) {
		if (character.player_character) game.pushMessage("Nah, I am not very hungry...");
	}


	this.getClassName = function() {
		return "Food";
	}

}

Spade.prototype = new Item("Spade",null);
function Spade(animation, value) {
	this.value = value;
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.takeable = true;
	this.useable = true;
	this.equipable = false;
	this.burrowed = false;

	initAIRules(this);

	this.effect = function(character, map, game) {
		if (map.canDig(character.x,character.y)) {
			var object = map.getBurrowed(character.x,character.y);
			if (object!=null) {
				object.burrowed = false;
				game.pushMessage("You unearthed a " + object.name + "!");
			} else {
				game.pushMessage("You dig a hole, but find nothing...");
			}
		} else {
			game.pushMessage("You cannot dig a hole here!");
		}
	}

	this.getClassName = function() {
		return "Spade";
	}

}

Key.prototype = new Item("Key",null);
function Key(name, ID, animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.name = name;
	this.keyID = ID;
	this.takeable = true;
	this.useable = false;
	this.equipable = false;
	this.value = 0;	// items with value = 0 cannot be sold

	initAIRules(this);

	this.getClassName = function() {
		return "Key";
	}

}

Chest.prototype = new Item("Chest",null);
function Chest(name, itemInside, animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.name = name;
	this.takeable = true;
	this.useable = true;
	this.equipable = false;
	this.itemInside = itemInside;
	this.value = 0;	// items with value = 0 cannot be sold

	initAIRules(this);

	this.effect = function(character, map, game) {
		fireEventRule(this, "open");
		character.inventory.splice(character.inventory.indexOf(this),1);
		character.inventory.push(this.itemInside);
		game.pushMessage("You open the chest and find a... " + this.itemInside.name);
		if (this.itemInside.useUponTake) itemInside.effect(character,map,game);
	}	

	this.getClassName = function() {
		return "Chest";
	}

}

ItemHPotion.prototype = new Item(null, null);
function ItemHPotion(bonus, animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.name = "H.Potion+" + bonus;
	this.useable = true;
	this.bonus = bonus;
	this.value = 5 + bonus*2;	

	initAIRules(this);

	this.effect = function(character, map, game) {
		if (character.max_hp > character.hp) {
			character.hp  += this.bonus;
			if (character.hp>character.max_hp) character.hp = character.max_hp;
			character.inventory.splice(character.inventory.indexOf(this),1);
			game.pushMessage("You feel healthier...");
		} else {
			game.pushMessage("Nah... It would be a waste");			
		}
	};

	this.getClassName = function() {
		return "ItemHPotion";
	}

}

ItemMPotion.prototype = new Item(null, null);
function ItemMPotion(bonus, animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.name = "M.Potion+" + bonus;
	this.useable = true;
	this.bonus = bonus;
	this.value = 5 + bonus*2;

	initAIRules(this);

	this.effect = function(character, map, game) {
		if (character.max_mp > character.mp) {
			character.mp  += this.bonus;
			if (character.mp>character.max_mp) character.mp = character.max_mp;
			character.inventory.splice(character.inventory.indexOf(this),1);
			game.pushMessage("Magic is flowing through your veins...");
		} else {
			game.pushMessage("Nah... It would be a waste");
		}
	}; 

	this.getClassName = function() {
		return "ItemMPotion";
	}

}

ItemXPPotion.prototype = new Item(null, null);
function ItemXPPotion(bonus, animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.name = "XP.Potion+" + bonus;
	this.useable = true;
	this.bonus = bonus;
	this.value = 10 + bonus*4;

	initAIRules(this);

	this.effect = function(character, map, game) {
		character.experienceGain(this.bonus, map);
		character.inventory.splice(character.inventory.indexOf(this),1);
		game.pushMessage("You feel wiser...");
	};

	this.getClassName = function() {
		return "ItemXPPotion";
	}

}

ItemCoinPurse.prototype = new Item("", null);
function ItemCoinPurse(gold, animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.name = gold + " gold purse";
	this.gold = gold;
	this.useUponTake = true;
	this.value = gold;
	this.animations[this.currentAnimation].reset();

	initAIRules(this);

	this.effect = function(character, map, game) {
		character.gold += this.gold;
		character.inventory.splice(character.inventory.indexOf(this),1);
		game.pushMessage("You got " + this.gold + " gold coins.");
	};

	this.getClassName = function() {
		return "ItemCoinPurse";
	}
}


EquipableItem.prototype = new Item(null, null);
function EquipableItem(name, animation, itemType, attackBonus, defenseBonus, magicMultiplier, canChop, value) {
	this.name = name;
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.itemType = itemType;
	this.equipable = true;
	this.attackBonus = attackBonus;
	this.defenseBonus = defenseBonus;
	this.magicMultiplier = magicMultiplier;
	this.canChop = canChop;
	this.value = value;

	initAIRules(this);

	this.getClassName = function() {
		return "EquipableItem";
	}

}


Scroll.prototype = new Item("Scroll");
function Scroll(name, spellInside, value, animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.name = name;
	this.takeable = true;
	this.useable = true;
	this.equipable = false;
	this.spellInside = spellInside;
	this.value = value;

	initAIRules(this);

	this.effect = function(character, map, game) {
		if (character.spells.indexOf(this.spellInside)!=-1) {
			game.pushMessage("You already know that spell...");
		} else {
			character.inventory.splice(character.inventory.indexOf(this),1);
			character.spells.push(this.spellInside);
			game.pushMessage("You have learned the spell " + this.spellInside);
		}
	}	

	this.getClassName = function() {
		return "Scroll";
	}

}


/*
 *	------------------------ DOORS AND SWITCHES ------------------------
 */

Lever.prototype = new Object();
function Lever(name, state, animation1, animation2) {
	this.name = name;
	this.currentAnimation = state;
	this.animations = {};
	this.animations[true] = (animation1==null ? null:animation1.clone());
	this.animations[false] = (animation2==null ? null:animation2.clone());
	this.state = state;

	initAIRules(this);

	this.effect = function(map) {
//		console.log("Running lever effect!!");
		fireEventRule(this, "use");
		if (this.state) {
			this.state = false;
			fireEventRule(this, "deactivate");
		} else {
			this.state = true;
			fireEventRule(this, "activate");
		}
		// look for doors to operate:
		var doors = map.getDoorsWithID(this.name);
//		console.log("Found " + doors.length + " doors to operate with ID " + this.name);
		for(var i = 0;i<doors.length;i++) {
			var door = doors[i];
			if (door.state) {
				door.setStateNonPropagating(false,map);
			} else {
				door.setStateNonPropagating(true,map);
			}
		}
		this.currentAnimation = this.state;
	}

	this.getClassName = function() {
		return "Lever";
	}
}


PressurePlate.prototype = new Object();
function PressurePlate(animation1, animation2, pressureRequired) {
	this.name = "PressurePlate";
	this.currentAnimation = false;
	this.animations = {};
	this.animations[true] = (animation1==null ? null:animation1.clone());
	this.animations[false] = (animation2==null ? null:animation2.clone());
	this.state = false;
	this.pressureRequired = pressureRequired;	// 0 : any item, 1: only characters/walls, 2: only players

	initAIRules(this);

	this.update = function(character, map, game) {
		var ontop = map.getObjects(this.x,this.y);
		var pressedNow = false;

		for(var i = 0;i<ontop.length;i++) {
			var object = ontop[i];
			if (object!=this) {
				if (object instanceof PlayerCharacter) {
					pressedNow = true;
				} else if ((object instanceof Character) ||
						   (object instanceof PushableWall)) {
					if (this.pressureRequired<=1) pressedNow = true;
				} else if ((object instanceof Item)) {
					if (this.pressureRequired==0) pressedNow = true;
				}
			}
		}

		if (pressedNow && !this.state) {
			fireEventRule(this,"use");
			fireEventRule(this,"activate");
		}
		if (!pressedNow && this.state) {
			fireEventRule(this,"use");
			fireEventRule(this,"deactivate");
		}
		this.state = pressedNow;
		this.currentAnimation = this.state;
		return PressurePlate.prototype.update.call(this, character, map, game);
	}

	this.getClassName = function() {
		return "PressurePlate";
	}
}


Door.prototype = new Object();
function Door(name, state, animation1, animation2) {
	this.name = name;
	this.currentAnimation = state;
	this.animations = {};
	this.animations[true] = (animation1==null ? null:animation1.clone());
	this.animations[false] = (animation2==null ? null:animation2.clone());
	this.state = state;
	initAIRules(this);

	this.walkable = function() {
		if (this.state) return false;
			  	   else return true;
	}

	this.setState = function(state,map) {
		this.setStateNonPropagating(state,map);
		// check for contiguous doors:
		var x1 = this.x - 1;
		do {
			door = map.getDoor(x1,this.y);
			if (door!=null && door.name == this.name) {
				door.setStateNonPropagating(state,map);
			} else break;
			x1--;
		}while(x1>=0);
		x1 = this.x + 1;
		do {
			door = map.getDoor(x1,this.y);
			if (door!=null && door.name == this.name) {
				door.setStateNonPropagating(state,map);
			} else break;
			x1++;
		}while(x1<map.width);
		var y1 = this.y - 1;
		do {
			door = map.getDoor(this.x,y1);
			if (door!=null && door.name == this.name) {
				door.setStateNonPropagating(state,map);
			} else break;
			y1--;
		}while(y1>=0);
		y1 = this.y + 1;
		do {
			door = map.getDoor(this.x,y1);
			if (door!=null && door.name == this.name) {
				door.setStateNonPropagating(state,map);
			} else break;
			y1++;
		}while(y1<map.height);	
		this.currentAnimation = this.state;
	}


	this.setStateNonPropagating = function(state,map) {
		this.state = state;
		this.currentAnimation = this.state;

		if (state==true) {
			fireEventRule(this, "close");
			// check to see if any character was here and kill it!
			var character = map.getCharacter(this.x, this.y);
			if (character!=null) {
				character.takeDamage(character.hp, null, map);
			}
		} else {
			fireEventRule(this, "open");
		}
	}	

	this.getClassName = function() {
		return "Door";
	}

}


PushableWall.prototype = new Object();
function PushableWall(animation) {
	this.currentAnimation = ANIM_IDLE;
	this.animations = {};
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());

	initAIRules(this);

	this.walkable = function() {
		return false;
	}

	this.getClassName = function() {
		return "PushableWall";
	}

}

/*
 *	------------------------ SPELLS ------------------------
 */

Spell.prototype = new Object();
function Spell(direction, duration, damage, radius, owner, animation) {
	this.name = "spell";
	this.direction = direction;
	this.duration = duration;
	this.damage = damage;
	this.radius = radius;	// 0 only damages one guy, 1 damages 4 neighbors, etc. (damages people in less or equal than the radius)
	this.owner = owner;
	this.animations = {};
	this.currentAnimation = ANIM_IDLE;
	this.animations[ANIM_IDLE] = (animation==null ? null:animation.clone());
	this.takeable = false;
	this.walk_speed = 8;
	this.state = STATE_READY;
	this.state_timer = 0;

	initAIRules(this);

	this.update = function(character, map, game) {
		if (this.state==STATE_READY) {
			this.state = STATE_MOVING;
			var ox = this.x;
			var oy = this.y;
			if (this.direction==DIRECTION_LEFT) this.x--;
			if (this.direction==DIRECTION_UP) this.y--;
			if (this.direction==DIRECTION_RIGHT) this.x++;
			if (this.direction==DIRECTION_DOWN) this.y++;
			this.state_timer = 0;
			this.duration--;
			var collision = false;
			var tmp = null;

			if (!map.walkable(this.x,this.y, this)) {
				collision = true;
				tmp = map.getCharacter(this.x,this.y);
			} else {
				// collision with the previous tile where the spell was (to avoid spells going through enemies)
				if (!map.walkable(ox,oy, this)) {
					collision = true;
					tmp = map.getCharacter(ox,oy);
					if (tmp!=this.owner) {
						this.x = ox;
						this.y = oy;
					}
				}				
			}

			if (collision) {
				// radius of damage:
				if (tmp!=this.owner) {
					for(var xoff = -Math.ceil(this.radius);xoff<=Math.ceil(this.radius);xoff++) {
						for(var yoff = -Math.ceil(this.radius);yoff<=Math.ceil(this.radius);yoff++) {
							if (xoff*xoff + yoff*yoff <= this.radius*this.radius) {
								var enemy = map.getCharacter(this.x+xoff,this.y+yoff);
								if (enemy!=null) {
									enemy.takeDamage(this.damage,this.owner,map);
									if (enemy.hp<=0) {
										// experience gain!
										if (owner instanceof PlayerCharacter) {
											if (owner.player_character) game.pushMessage("You killed " + enemy.name);
											owner.experienceGain(enemy.gives_experience, map);
										}
									}
								}							
							}
						}					
					}
					return false;
				}
			}
		} else {
			this.state_timer++;
			if (this.state_timer>=this.walk_speed) {
				this.state = STATE_READY;
				if (this.duration<=0) return false;
			}
		}
		return true;
	}	

	this.draw = function(alpha,offsetx,offsety,game) {
		switch(this.state) {
			case STATE_MOVING:
					var offs = Math.floor(TILE_HEIGHT*(this.walk_speed-this.state_timer)/this.walk_speed);
					switch(this.direction) {
					case DIRECTION_UP: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offsetx, this.y*TILE_HEIGHT + offs + offsety,alpha,game); break;
					case DIRECTION_RIGHT: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH - offs + offsetx,this.y*TILE_HEIGHT + offsety,alpha,game); break;
					case DIRECTION_DOWN: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offsetx,this.y*TILE_HEIGHT - offs + offsety,alpha,game); break;
					case DIRECTION_LEFT: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offs + offsetx,this.y*TILE_HEIGHT + offsety,alpha,game); break;
					}
					break;
			default:
					this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offsetx, this.y*TILE_HEIGHT + offsety,alpha,game);
					break;
		}
	}	

	this.getClassName = function() {
		return "Spell";
	}

}


/*
 *	------------------------ VEHICLES ------------------------
 */



Vehicle.prototype = new Object();
function Vehicle(name) {
	this.name = name;
	this.canSwim = false;
	this.state_timer = 0;
	this.state = STATE_READY;
	this.direction = DIRECTION_RIGHT;
	this.animations = {};
	this.currentAnimation = null;

	this.animation_speed = 16;
	this.walk_speed = 16;

	initAIRules(this);

	this.effect = function(map) {
	}

	this.update = function(character, map, game) {

		if (this.currentAnimation!=null) {
			this.animations[this.currentAnimation].update();
			this.width = this.animations[this.currentAnimation].getWidth();
			this.height = this.animations[this.currentAnimation].getHeight();
		}

		switch(this.state) {
			case STATE_READY:
					if (this.state_timer==0) this.currentAnimation = ANIM_IDLE + ANIM_DIRECTIONS[this.direction];
					this.state_timer++;
					break;
			case STATE_MOVING:
					if (this.state_timer==0) {
						this.currentAnimation = ANIM_MOVING + ANIM_DIRECTIONS[this.direction];
						if (this.animations[this.currentAnimation]==null) this.currentAnimation = ANIM_IDLE + ANIM_DIRECTIONS[this.direction];
					}
					this.state_timer++;
					if (this.state_timer>=this.walk_speed) {
						this.state_timer=0;
						this.state = STATE_READY;
					}
					break;
			default:
					break;
		}


		return Vehicle.prototype.update.call(this, character, map, game);
	}	

	this.draw = function(alpha,offsetx,offsety,game) {
		if (this.currentAnimation!=null) {			
			switch(this.state) {
				case STATE_MOVING:
						var offs = Math.floor(TILE_HEIGHT*(this.walk_speed-this.state_timer)/this.walk_speed);

						switch(this.direction) {
						case DIRECTION_UP: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offsetx, this.y*TILE_HEIGHT + offs + offsety,alpha,game); break;
						case DIRECTION_RIGHT: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH - offs + offsetx,this.y*TILE_HEIGHT + offsety,alpha,game); break;
						case DIRECTION_DOWN: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offsetx,this.y*TILE_HEIGHT - offs + offsety,alpha,game); break;
						case DIRECTION_LEFT: this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offs + offsetx,this.y*TILE_HEIGHT + offsety,alpha,game); break;
						}
						break;
				default:
						this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offsetx, this.y*TILE_HEIGHT + offsety,alpha,game);
						break;
			}
		}
	}	

	this.getClassName = function() {
		return "Vehicle";
	}

}
