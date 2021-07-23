Character.prototype = new Object();
function Character() {
	this.animations = {};

	this.hp = this.max_hp = 10;
	this.mp = this.max_mp = 10;

	this.inventory = new Array();
	this.spells = new Array();
	this.equippedItems = [null,null,null];
	this.activeSpells = new Array();

	this.player_character = false;	// this variable is only true in Player Characters, so the user has feedback of the actions
	this.walk_speed = 16;
	this.animation_speed = 4;
	this.attack_speed = 32;
	this.animations = {};
	this.currentAnimation = null;

	this.attack = 4;
	this.defense = 0;
	this.attack_modifier = 1;
	this.defense_modifier = 1;
	this.magicImmune = false;

	this.target = null;		// this will store the target of an attack, if any	
	this.direction = DIRECTION_RIGHT;
	this.state = STATE_READY;
	this.state_timer = 0;

	this.talk_state = STATE_READY;
	this.talk_state_timer = 0;
	this.talk_message = null;

	this.just_attacked_by = null;	// holds if another character attacked "this" in the last frame (used by the AI modules)
	this.receivedDamageTimmer = 0;	// this is used to draw a graphic effect when a character is hit

	this.vehicle = null;


	this.generateSaveData = function(objects, data) {
		data = Character.prototype.generateSaveData.call(this,objects,data);

		// character data:
		if (this.level!=undefined) data.level = this.level;
		if (this.experience!=undefined) data.experience = this.experience;
		if (this.hp!=undefined) {
			data.hp = this.hp;
			data.mp = this.mp;
			data.max_hp = this.max_hp;
			data.max_mp = this.max_mp;
		}
		if (this.inventory!=undefined) {
			data.inventory = [];
			for(var i = 0;i<this.inventory.length;i++) {
				data.inventory.push(objects.indexOf(this.inventory[i]));
			}
		}
		if (this.equippedItems!=undefined) {
			data.equippedItems = [];
			for(var i = 0;i<this.equippedItems.length;i++) {
				data.equippedItems.push(objects.indexOf(this.equippedItems[i]));
			}
		}

		if (this.spells!=undefined) data.spells = this.spells;
		if (this.activeSpells!=undefined) data.activeSpells = this.activeSpells;

		if (this.attack_modifier!=undefined) data.attack_modifier = this.attack_modifier;
		if (this.defense_modifier!=undefined) data.defense_modifier = this.defense_modifier;

		if (this.just_attacked_by!=undefined) data.just_attacked_by = objects.indexOf(this.just_attacked_by);
		if (this.receivedDamageTimmer!=undefined) data.receivedDamageTimmer = this.receivedDamageTimmer;
		if (this.vehicle!=undefined) data.vehicle = objects.indexOf(this.vehicle);
		return data;
	}


	// "reset" is expected to be "true" when xml is the character type definition, and "false" when it is
	// the specification of each specific character in the map files.
	this.decodeCharacterFromXML = function(reset) {
		if (reset) this.name = this.xml.attributes.getNamedItem("name").nodeValue;

		this.time = 0;

		// animations:
		if (reset) this.animations = {};
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
		if (reset) this.levelupadd = {};
		if (reset) this.levelupmultiply = {};
		for(var i = 0;i<attributesElements.length;i++) {
			var attributesElement = attributesElements[i];
			var name = attributesElement.attributes.getNamedItem("name").nodeValue;
			var value = attributesElement.attributes.getNamedItem("value").nodeValue;
			var levelupadd = attributesElement.attributes.getNamedItem("levelupadd");
			if (levelupadd!=null) this.levelupadd[name] = levelupadd.nodeValue;
			var levelupmultiply = attributesElement.attributes.getNamedItem("levelupmultiply");
			if (levelupmultiply!=null) this.levelupmultiply[name] = levelupmultiply.nodeValue;
			// parse the value: it can be something like this:
			// 1
			// 1,2
			// 1-5,3,10-20
			var value2 = parseStringValue(value);
			this[name] = value2;
//			console.log(this.name + "." + name + " = " + value2 + " (from " + value + ")");
			if (name=="hp") this.max_hp = value2;
			if (name=="mp") this.max_mp = value2;
		}

		// inventory:
		if (reset) this.equippedItems = [null,null,null];
		if (reset) this.inventory = [];
		var inventoryElements = this.xml.getElementsByTagName("item");
//		console.log("Inventory: " + inventoryElements.length);
		for(var i = 0;i<inventoryElements.length;i++) {
			var inventoryElement = inventoryElements[i];
			var p = inventoryElement.attributes.getNamedItem("probability");
			if (p!=null) {
				p = eval(p.nodeValue);
			} else {
				p = 1.0;
			}
			if (Math.random()<=p) {
				this.inventory.push(eval(inventoryElement.textContent));
			}
		}

		if (reset) this.activeSpells = new Array();
		if (reset) this.spells = new Array();
		var spellElements = this.xml.getElementsByTagName("spell");
		for(var i = 0;i<spellElements.length;i++) {
			var spellElement = spellElements[i];
			var p = spellElement.attributes.getNamedItem("probability");
			if (p!=null) {
				p = eval(p.nodeValue);
			} else {
				p = 1.0;
			}
			if (Math.random()<=p) {
				this.spells.push(spellElement.textContent);
			}
		}
	}	


	this.getClassName = function() {
		return "Character";
	}

	this.walkable = function() {
		return false;
	}

	this.getAttack = function() {
		var attack = this.attack;
		// check equiped items:
		for(i = 0;i<3;i++) {
			item = this.equippedItems[i];
			if (item!=null) attack+=item.attackBonus;
		}
		attack += this.getAttackBonusFromSpells();
		return Math.floor(attack * this.attack_modifier);
	}


	this.getMagicMultiplier = function() {
		var multiplier = 1;
		// check equiped items:
		for(i = 0;i<3;i++) {
			item = this.equippedItems[i];
			if (item!=null) multiplier*=item.magicMultiplier;
		}
		return multiplier;
	}

	this.getAttackBonusFromSpells = function() {
		var ret = 0;
		for(var i=0;i<this.activeSpells.length;i++) {
			var activeSpell = this.activeSpells[i];
			if (activeSpell.spell == SPELL_INCREASE) ret+=Math.floor(this.getMagicMultiplier()*4);
			if (activeSpell.spell == SPELL_DECREASE) ret-=Math.floor(this.getMagicMultiplier()*4);
		}
		return ret;
	}

	this.getDefense = function() {
		var defense = this.defense;
		// check equiped items
		for(i = 0;i<3;i++) {
			item = this.equippedItems[i];
			if (item!=null) defense+=item.defenseBonus;
		}
		defense += this.getDefenseBonusFromSpells();
		return Math.floor(defense * this.defense_modifier);
	}

	this.getDefenseBonusFromSpells = function() {
		var ret = 0;
		for(var i=0;i<this.activeSpells.length;i++) {
			var activeSpell = this.activeSpells[i];
			if (activeSpell.spell == SPELL_SHIELD) ret+=Math.floor(this.getMagicMultiplier()*5);
			if (activeSpell.spell == SPELL_INCREASE) ret+=Math.floor(this.getMagicMultiplier()*2);
			if (activeSpell.spell == SPELL_DECREASE) ret-=Math.floor(this.getMagicMultiplier()*2);
		}
		return ret;
	}


	this.attackRoll = function(target) {
		var atk = this.getAttack();
		var def = target.getDefense();

		var rawAttack = Math.floor(atk/2 + atk*Math.random());
		if (rawAttack<=0) rawAttack = 0;
		var defenseModifier = Math.pow(0.97,def);
//		var defenseModifier = Math.exp(def*Math.log(0.97));
		var dmg = Math.floor(rawAttack*defenseModifier);
		return dmg;
	}

	this.takeDamage = function(damage, attacker, map) {
		if (this.magicImmune) damage = 0;
		this.hp -= damage;
		if (damage>0) {
			map.pushTextOverlay("" + damage, this.x, this.y, 255,0,0);
			this.receivedDamageTimmer = 25;
		} else {
			map.pushTextOverlay("miss", this.x, this.y, 255,255,0);
		}
		if (attacker!=null) this.just_attacked_by = {attacker:attacker, timer:25};
	}

	this.sendAction = function(action,map,game) {
		if (this.state == STATE_READY) {
			if (action.type == ACTION_INTERACT) {
				var oldx = this.x;
				var oldy = this.y;
				var nextx = this.x + direction_offx[action.direction];
				var nexty = this.y + direction_offy[action.direction];

				if (map.walkable(nextx,nexty,this) ||
					(this.canSwim == true && map.sailable(nextx,nexty,this))) {
					// change action to move:
					action.type = ACTION_MOVE;
				} else {
					// check for trees to he chopped:
					var weapon = this.equippedItems[ITEM_WEAPON];
					if (weapon!=null && weapon.canChop) {
						if (map.choppeable(nextx,nexty)) {
							// chop the tree!!
							if (this.player_character) game.pushMessage("You chop the tree with the " + weapon.name + "!");
							map.chopTree(nextx,nexty);
							this.state = STATE_INTERACTING;			// chop a tree is like an attack
							this.state_timer = 0;
							return true;
						} else if (map.nonchoppeabletree(nextx,nexty)) {
							// chop the tree!!
							if (this.player_character) game.pushMessage("You try to chop the tree with the axe, but you fail...");
							return true;
						}
					}

					// check for doors:
					var door = map.getDoor(nextx,nexty);
					if (door!=null && door.state) {
//						console.log("bumped onto door: " + door.name);
						// check if we have the key:
						for(i = 0;i<this.inventory.length;i++) {
							item = this.inventory[i];
							if (item instanceof Key) {
								if (item.keyID == door.name) {
									door.setState(false,map);
									this.inventory.splice(i,1);
									if (this.player_character) game.pushMessage("You use the " + item.name + " to open the door.");
									map.updateVisibility(this.x,this.y,game);
									return true;
								}
							}
						}
					}

					// check for pushable walls:
					var wall = map.getPushableWall(nextx,nexty);
					if (wall!=null) {
						if (map.walkable(nextx+direction_offx[action.direction], nexty+direction_offy[action.direction],this)) {
							wall.x = nextx+direction_offx[action.direction];
							wall.y = nexty+direction_offy[action.direction];
							map.updateVisibility(this.x,this.y,game);
							fireEventRule(wall,"push");
						}
					}

					return false;
				}
			} else if (action.type == ACTION_TAKE) {
				// check for items to pick up:
				var items = map.getTakeable(this.x,this.y);
				if (items.length>0) {
					if (!this.player_character || this.inventory.length<INVENTORY_SIZE) {
						map.removeObject(items[0]);
						this.inventory.push(items[0]);
						fireEventRule(items[0],"pickup");
						if (items[0].useUponTake) items[0].effect(this,map,game);
						return true;
					} else {
						game.pushMessage("Inventory full!");	
						return false;					
					}
				} else {
					// otherwise, look for levers:
					var item = map.getLever(this.x,this.y);
					if (item != null) {
						item.effect(map);
						for(var i = 0;i<game.characters.length;i++) {
							map.updateVisibility(game.characters[i].x,game.characters[i].y,game);
						}
					} else {
						// otherwise, check for vehicles to control:
						var vehicle = map.getVehicle(this.x, this.y);
						if (vehicle != null) {
							// take conrol of the vehicle:
							this.state = STATE_VEHICLE_READY;
							this.vehicle = vehicle;
							return true;
						} else {
							if (this.player_character) game.pushMessage("Nothing to pick up here!");
							return false;
						}
					}
				}
			} else if (action.type == ACTION_DROP) {
//				console.log("ACTION_DROP " + action.item);
				if (action.item<0 || action.item>=this.inventory.length) {
					if (this.player_character) game.pushMessage("You don't have that item!");
					return false;
				} else {
					var item = this.inventory[action.item];
					this.inventory.splice(action.item,1);
					game.warpObject(item, this.x, this.y, this.map, 1);
					return true;
				}
			} else if (action.type == ACTION_DROP_GOLD) {
//				console.log("ACTION_DROP_GOLD " + action.gold);
				if (this.gold<action.gold) {
					if (this.player_character) game.pushMessage("You don't have that much!");
					return false;
				} else {
					var item = new ItemCoinPurse(action.gold, game.defaultAnimations[COINPURSE_ANIMATION]);
					this.gold -= action.gold;
					game.warpObject(item, this.x, this.y, this.map, 1);
				}
			} else if (action.type == ACTION_USE) {
//				console.log("ACTION_USE " + action.item);
				if (action.item<0 || action.item>=this.inventory.length) {
					if (this.player_character) game.pushMessage("You don't have that item!");
					return false;
				} else {
					var item = this.inventory[action.item];
					if (item.equipable==true) {
						// Equip item:
						this.inventory.splice(action.item,1);
						previousItem = this.equippedItems[item.itemType];
						this.equippedItems[item.itemType] = item;
						if (previousItem!=null) this.inventory.push(previousItem);
						return true;
					} else if (item.useable==true) {
						item.effect(this,map,game);
					} else {
						if (this.player_character) game.pushMessage("You cannot use that item!");
						return false;
					}
				}
			} else if (action.type == ACTION_UNEQUIP) {
//				console.log("ACTION_UNEQUIP " + action.item);
				if (action.item<0 || this.equippedItems[action.item]==null) {
					if (this.player_character) game.pushMessage("You don't have that item!");
					return false;
				} else {
					var item = this.equippedItems[action.item];
					if (this.inventory.length>=INVENTORY_SIZE) {
						if (this.player_character) game.pushMessage("Inventory full!");
						return false;
					} else {
						this.equippedItems[item.itemType] = null;
						this.inventory.push(item);
						return true;
					}
				}
			} else if (action.type == ACTION_ATTACK) {
				var nextx = this.x;
				var nexty = this.y;
				if (action.direction == DIRECTION_UP) nexty--;
				else if (action.direction == DIRECTION_DOWN) nexty++;
				else if (action.direction == DIRECTION_LEFT) nextx--;
				else if (action.direction == DIRECTION_RIGHT) nextx++;
				if (map.walkable(nextx,nexty,this) ||
					(this.canSwim == true && map.sailable(nextx,nexty,this))) {
					// change action to move:
					action.type = ACTION_MOVE;
				} else {
					for(var ix = 0;ix<this.width;ix++) {
						for(var iy = 0;iy<this.height;iy++) {
							var enemy = map.getCharacter(nextx+ix,nexty+iy);
							if (enemy==this) enemy = null;
							if (enemy!=null) {
								this.state = STATE_ATTACKING;
								this.target = enemy;
								this.state_timer = 0;

								var damage = this.attackRoll(enemy);
								//game.pushMessage(enemy.name + " receives " + damage + " damage!");
								enemy.takeDamage(damage,this,map);
								if (enemy.hp<=0) {
									// experience gain!
									if (this instanceof PlayerCharacter) {
										if (this.player_character) game.pushMessage("You killed " + enemy.name);
										this.experienceGain(enemy.gives_experience, map);
									}
								}
								return true;
							} else {
								// check for trees to he chopped:
								var weapon = this.equippedItems[ITEM_WEAPON];
								if (weapon!=null && weapon.canChop) {
									if (map.choppeable(nextx,nexty)) {
										// chop the tree!!
										if (this.player_character) game.pushMessage("You chop the tree with the " + weapon.name + "!");
										map.chopTree(nextx,nexty);
										this.state = STATE_INTERACTING;			// chop a tree is like an attack
										this.state_timer = 0;
										return true;
									} else if (map.nonchoppeabletree(nextx,nexty)) {
										// chop the tree!!
										if (this.player_character) game.pushMessage("You try to chop the tree with the axe, but you fail...");
										return true;
									}
								}								
							}
						}
					}
					if (this.player_character) game.pushMessage("There is no enemy here to attack!");
					return false;					
				}
			} else if (action.type == ACTION_SPELL) {
				if (this.player_character) game.pushMessage("casting spell " + action.spell + "!");
				if (action.spell == SPELL_MAGIC_MISSILE) {
					if (action.direction==DIRECTION_LEFT ||
					    action.direction==DIRECTION_UP ||
					    action.direction==DIRECTION_RIGHT ||
					    action.direction==DIRECTION_DOWN) {
						this.mp-=spell_cost[action.spell];
						var anim = game.defaultAnimations[SPELL_MAGIC_MISSILE + ANIM_DIRECTIONS[action.direction]];
						if (anim==null) anim = game.defaultAnimations[SPELL_MAGIC_MISSILE]
						game.warpObject(new Spell(action.direction, 6, Math.floor(this.getMagicMultiplier()*4), 0, this, anim), 
									    this.x+action.offsx,this.y+action.offsy, this.map, 1);
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}
				} else if (action.spell == SPELL_HEAL) {
					var targetx = this.x;
					var targety = this.y;
					if (action.direction==DIRECTION_LEFT) targetx--;
					if (action.direction==DIRECTION_UP) targety--;
					if (action.direction==DIRECTION_RIGHT) targetx+=this.width;
					if (action.direction==DIRECTION_DOWN) targety+=this.height;
					var target = map.getCharacter(targetx,targety);
					if (target!=null) {
						target.hp+=Math.floor(this.getMagicMultiplier()*5);
						this.mp-=spell_cost[action.spell];
						if (target.hp>=target.max_hp) target.hp = target.max_hp;
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}
				} else if (action.spell == SPELL_SHIELD) {
					var targetx = this.x;
					var targety = this.y;
					if (action.direction==DIRECTION_LEFT) targetx--;
					if (action.direction==DIRECTION_UP) targety--;
					if (action.direction==DIRECTION_RIGHT) targetx+=this.width;
					if (action.direction==DIRECTION_DOWN) targety+=this.height;
					var target = map.getCharacter(targetx,targety);
					if (target!=null) {
						this.mp-=spell_cost[action.spell];
						var found = false;
						for(var i = 0;i<target.activeSpells.length;i++) {
							if (target.activeSpells[i].spell == SPELL_SHIELD) {
								found = true;
								target.activeSpells[i].timer = 1500;
							}
						}
						if (!found) target.activeSpells.push({spell:SPELL_SHIELD, timer:1500});
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}
				} else if (action.spell == SPELL_INCREASE) {
					var targetx = this.x;
					var targety = this.y;
					if (action.direction==DIRECTION_LEFT) targetx--;
					if (action.direction==DIRECTION_UP) targety--;
					if (action.direction==DIRECTION_RIGHT) targetx+=this.width;
					if (action.direction==DIRECTION_DOWN) targety+=this.height;
					var target = map.getCharacter(targetx,targety);
					if (target!=null) {
						this.mp-=spell_cost[action.spell];
						var found = false;
						for(var i = 0;i<target.activeSpells.length;i++) {
							if (target.activeSpells[i].spell == SPELL_INCREASE) {
								found = true;
								target.activeSpells[i].timer = 1500;
							}
						}
						if (!found) target.activeSpells.push({spell:SPELL_INCREASE, timer:1500});
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}					
				} else if (action.spell == SPELL_DECREASE) {
					var targetx = this.x;
					var targety = this.y;
					if (action.direction==DIRECTION_LEFT) targetx--;
					if (action.direction==DIRECTION_UP) targety--;
					if (action.direction==DIRECTION_RIGHT) targetx+=this.width;
					if (action.direction==DIRECTION_DOWN) targety+=this.height;
					var target = map.getCharacter(targetx,targety);
					if (target!=null) {
						this.mp-=spell_cost[action.spell];
						var found = false;
						for(var i = 0;i<target.activeSpells.length;i++) {
							if (target.activeSpells[i].spell == SPELL_DECREASE) {
								found = true;
								target.activeSpells[i].timer = 1500;
							}
						}
						if (!found) target.activeSpells.push({spell:SPELL_DECREASE, timer:1500});
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}										
				} else if (action.spell == SPELL_FIREBALL) {
					if (action.direction==DIRECTION_LEFT ||
					    action.direction==DIRECTION_UP ||
					    action.direction==DIRECTION_RIGHT ||
					    action.direction==DIRECTION_DOWN) {
						this.mp-=spell_cost[action.spell];
						var anim = game.defaultAnimations[SPELL_FIREBALL + ANIM_DIRECTIONS[action.direction]];
						if (anim==null) anim = game.defaultAnimations[SPELL_FIREBALL]
						game.warpObject(new Spell(action.direction, 10, Math.floor(this.getMagicMultiplier()*12), 1.5, this, anim), 
										this.x+action.offsx,this.y+action.offsy, this.map, 1);
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}
				} else if (action.spell == SPELL_MAGIC_EYE) {
					this.mp-=spell_cost[action.spell];
					map.completeVisibility();
				} else if (action.spell == SPELL_REGENERATE) {
					var targetx = this.x;
					var targety = this.y;
					if (action.direction==DIRECTION_LEFT) targetx--;
					if (action.direction==DIRECTION_UP) targety--;
					if (action.direction==DIRECTION_RIGHT) targetx+=this.width;
					if (action.direction==DIRECTION_DOWN) targety+=this.height;
					var target = map.getCharacter(targetx,targety);
					if (target!=null) {
						target.hp+=50;
						if (target.hp>=target.max_hp) target.hp = target.max_hp;
						this.mp-=spell_cost[action.spell];
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}					
				} else if (action.spell == SPELL_INCINERATE) {
					if (action.direction==DIRECTION_LEFT ||
					    action.direction==DIRECTION_UP ||
					    action.direction==DIRECTION_RIGHT ||
					    action.direction==DIRECTION_DOWN) {
						this.mp-=spell_cost[action.spell];
						var anim = game.defaultAnimations[SPELL_INCINERATE + ANIM_DIRECTIONS[action.direction]];
						if (anim==null) anim = game.defaultAnimations[SPELL_INCINERATE]
						game.warpObject(new Spell(action.direction, 24, Math.floor(this.getMagicMultiplier()*32), 0, this, anim), 
										this.x+action.offsx,this.y+action.offsy, this.map, 1);
						this.state = STATE_CASTING;
						this.state_timer = 0;
					}
				}
			}
			if (action.type == ACTION_MOVE) {
				var oldx = this.x;
				var oldy = this.y;
				var nextx = this.x + direction_offx[action.direction];
				var nexty = this.y + direction_offy[action.direction];

				if (map.walkable(nextx,nexty,this) ||
					(this.canSwim == true && map.sailable(nextx,nexty,this))) {
					this.x = nextx;
					this.y = nexty;
					this.state = STATE_MOVING;
					this.state_timer = 0;
					this.direction = action.direction;
					
					return true;
				} else {
					return false;
				}
			}			
		} else if (this.state == STATE_VEHICLE_READY) {
			// controlling a vehicle:
			if (action.type == ACTION_TAKE) {
				this.state = STATE_READY;
				this.vehicle = null;
			} else if (action.type == ACTION_MOVE) {
				var oldx = this.x;
				var oldy = this.y;
				var nextx = this.x + direction_offx[action.direction];
				var nexty = this.y + direction_offy[action.direction];

				if ((this.vehicle.canSwim && map.sailable(nextx,nexty,this)) ||
					(this.vehicle.canWalk && map.walkable(nextx,nexty,this))) {
					this.x = nextx;
					this.y = nexty;
					this.state = STATE_VEHICLE_MOVING;
					this.state_timer = 0;
					this.direction = action.direction;

					this.vehicle.x = nextx;
					this.vehicle.y = nexty;
					this.vehicle.state = STATE_MOVING;
					this.vehicle.state_timer = 0;
					this.vehicle.direction = action.direction;
					
					return true;
				} else {
					return false;
				}
			}

		}

		if (this.talk_state==STATE_READY) {
			if (action.type == ACTION_TALK ||
			    action.type == ACTION_TALK_ANGRY) {
				var text = action.performative.type;
				if (text==TALK_HI) text = "Good day!";
				else if (text==TALK_BYE) text = "Farewell!";
				else if (text==TALK_TRADE) {
					text = "Would you like to trade with me?";
				} else if (text==TALK_ASK) {
					text = action.performative.text;
				}
				var time = 50 + text.length*4;
				if (action.type == ACTION_TALK_ANGRY) {
					map.pushTextBubbleColor(text,this,time,'#000000','#ffff3f');
				} else {
					map.pushTextBubble(text,this,time);
				}
				if (this.player_character) {
					game.pushMessage("You say: " + text);
				} else {
					var currentCharacter = game.characters[game.currentCharacter]
					if (this.map == currentCharacter.map) game.pushMessage(this.name + " says: " + text);
				}

				// Send the performative:
				if (action.performative.target instanceof AICharacter &&
					this.map == action.performative.target.map) {
					action.performative.target.receiveDelayedMessage(action.performative, this, time);
				}
				this.talk_state = STATE_TALKING;
				this.talk_state_timer = time;
				this.talk_message = action.performative;
			}	
		}	
		
		return false;
	}

	this.update = function(character, map, game) {
		if (this.hp<=0) {
			if (this.state == STATE_DEATH &&
				(this.animations[this.currentAnimation]==null ||
				 this.animations[this.currentAnimation].isCompleted())) {
				// drop items and gold:
				for(var i = 0;i<this.inventory.length;i++) {
					item = this.inventory[i];
					game.warpObject(item, this.x, this.y, this.map, 1);
				}
				if (this.gold>0) {
					item = new ItemCoinPurse(this.gold, game.defaultAnimations[COINPURSE_ANIMATION]);
					game.warpObject(item, this.x, this.y, this.map, 1);
				}

				// pop all the input menus from the game stack (otherwise, the other character will receive the input)
				if (this instanceof PlayerCharacter) game.menuStack = [];

				return false;
			} else {
				this.state = STATE_DEATH;
			}
		}

		if (this.currentAnimation!=null && this.animations[this.currentAnimation]!=null) {
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
						this.target = null;
						this.state = STATE_READY;
						this.state_timer = 0;

						// check for bridges:
						var bridge = map.getBridge(this.x,this.y);
						if (bridge != null) {
							var targetMap = game.maps[bridge.mapTarget];
							for(tx = bridge.xTarget;tx<bridge.xTarget+bridge.widthTarget;tx++) {
								for(ty = bridge.yTarget;ty<bridge.yTarget+bridge.heightTarget;ty++) {
									if (targetMap.walkable(tx,ty,this)) {
								        game.warpObject(this, tx, ty, bridge.mapTarget, 2);
								        if (bridge.appearWalking) {
								        	if (bridge.appearDirection!=DIRECTION_NONE) this.direction = bridge.appearDirection;
								        	this.state = STATE_MOVING;
								        	this.state_timer = 0;
								        } else {
								        	this.state = STATE_READY;
								        }
					                  	if (this.player_character) game.pushMessage("You have entered " + targetMap.name);
					                  	targetMap.updateVisibility(this.x,this.y,game);
					                  	return true;
									}
								}
							}
							if (this.player_character) game.pushMessage("Something is blocking the way...");
							this.state = STATE_MOVING;
							this.direction = opposite_direction[this.direction];
							this.state_timer = 0;
							this.x = this.x + direction_offx[this.direction];
							this.y = this.y + direction_offy[this.direction];
						}
					}
					break;
			case STATE_VEHICLE_READY:
					if (this.state_timer==0) this.currentAnimation = null;
					break;
			case STATE_VEHICLE_MOVING:
					if (this.state_timer==0) this.currentAnimation = null;
					this.state_timer++;
					if (this.state_timer>=this.walk_speed) {
						this.target = null;
						this.state = STATE_VEHICLE_READY;
						this.state_timer = 0;

						// check for bridges:
						var bridge = map.getBridge(this.x,this.y);
						if (bridge != null) {
							var targetMap = game.maps[bridge.mapTarget];
							for(tx = bridge.xTarget;tx<bridge.xTarget+bridge.widthTarget;tx++) {
								for(ty = bridge.yTarget;ty<bridge.yTarget+bridge.heightTarget;ty++) {
									if (targetMap.sailable(tx,ty,this)) {
								        game.warpObject(this, tx, ty, bridge.mapTarget, 2);
								        game.warpObject(this.vehicle, tx, ty, bridge.mapTarget, 1);
								        if (bridge.appearWalking) {
								        	if (bridge.appearDirection!=DIRECTION_NONE) this.direction = bridge.appearDirection;
								        	if (bridge.appearDirection!=DIRECTION_NONE) this.vehicle.direction = bridge.appearDirection;
								        	this.state = STATE_MOVING;
								        	this.vehicle.state = STATE_READY;
								        	this.state_timer = 0;
								        	this.vehicle.state_timer = 0;
								        } else {
								        	this.state = STATE_VEHICLE_READY;
								        	this.vehicle.state = STATE_READY;
								        }
					                  	if (this.player_character) game.pushMessage("You have entered " + targetMap.name);
					                  	targetMap.updateVisibility(this.x,this.y,game);
					                  	return true;
									}
								}
							}
							if (this.player_character) game.pushMessage("Something is blocking the way...");
							this.state = STATE_VEHICLE_MOVING;
							this.vehicle.state = STATE_MOVING;
							this.direction = opposite_direction[this.direction];
							this.vehicle.direction = opposite_direction[this.direction];
							this.state_timer = 0;
							this.vehicle.state_timer = 0;
							this.x = this.x + direction_offx[this.direction];
							this.y = this.y + direction_offy[this.direction];							
							this.vehicle.x = this.x;
							this.vehicle.y = this.y;
							return false;
						}					
					}
					break;
			case STATE_ATTACKING:
					if (this.state_timer==0) {
						this.currentAnimation = ANIM_ATTACKING + ANIM_DIRECTIONS[this.direction];
						if (this.animations[this.currentAnimation]==null) this.currentAnimation = ANIM_IDLE + ANIM_DIRECTIONS[this.direction];
					}
					this.state_timer++;
					if (this.state_timer>=this.attack_speed) {
						this.state = STATE_READY;
						this.state_timer = 0;
					}
				break;
			case STATE_INTERACTING:
					if (this.state_timer==0) {
						this.currentAnimation = ANIM_INTERACTING + ANIM_DIRECTIONS[this.direction];
						if (this.animations[this.currentAnimation]==null) this.currentAnimation = ANIM_IDLE + ANIM_DIRECTIONS[this.direction];
					}
					this.state_timer++;
					if (this.state_timer>=this.attack_speed) {
						this.state = STATE_READY;
						this.state_timer = 0;
					}
				break;
			case STATE_CASTING:
					if (this.state_timer==0) {
						this.currentAnimation = ANIM_TALKING + ANIM_DIRECTIONS[this.direction];
						if (this.animations[this.currentAnimation]==null) this.currentAnimation = ANIM_IDLE + ANIM_DIRECTIONS[this.direction];
					}
					this.state_timer++;
					if (this.state_timer>=this.attack_speed) {
						this.state = STATE_READY;
						this.state_timer = 0;
					}
				break;
			case STATE_DEATH:
				this.currentAnimation = ANIM_DEATH;
				this.state_timer++;
				break;
			default:
					break;
		}

		switch(this.talk_state) {
			case STATE_READY:
					break;
			case STATE_TALKING:
					this.talk_state_timer--;
					if (this.talk_state_timer<=0) {
						this.talk_state = STATE_READY;
						this.talk_state_timer = 0;
						this.talk_message = null;
					}
				break;
			default:
					break;
		}		

		if (this.receivedDamageTimmer>0) this.receivedDamageTimmer--;

		for(var i = 0;i<this.activeSpells.length;i++) {
			var activeSpell = this.activeSpells[i];
			activeSpell.timer--;
			if (activeSpell.timer<=0) {
				this.activeSpells.splice(i,1);
				break;	// stop the loop, since the splice would have messed up the indexes, 
						// also, it is not likely that two spells end at the same cycle
			}
		}

		if (this.just_attacked_by!=null) {
			this.just_attacked_by.timer--;
			if (this.just_attacked_by.timer<=0) this.just_attacked_by = null;
		}

		// update equipped items and inventory:
		for(var i = 0;i<this.inventory.length;i++) {
			if (this.inventory[i]!=null) {
				this.inventory[i].update(this,map,game);
			}
		}
		for(var i = 0;i<this.equippedItems.length;i++) {
			if (this.equippedItems[i]!=null) {
				this.equippedItems[i].update(this,map,game);
			}
		}

		return true;
	}

	this.getScreenX = function() {
		switch(this.state) {
			case STATE_MOVING:
				var offs = Math.floor(TILE_HEIGHT*(this.walk_speed-this.state_timer)/this.walk_speed);

				switch(this.direction) {
					case DIRECTION_UP: return this.x*TILE_WIDTH;
					case DIRECTION_RIGHT: return this.x*TILE_WIDTH - offs;
					case DIRECTION_DOWN: return this.x*TILE_WIDTH;
					case DIRECTION_LEFT: return this.x*TILE_WIDTH + offs;
				}
			default:
				return this.x*TILE_WIDTH;
		}		
	}

	this.getScreenY = function() {
		switch(this.state) {
			case STATE_MOVING:
				var offs = Math.floor(TILE_HEIGHT*(this.walk_speed-this.state_timer)/this.walk_speed);

				switch(this.direction) {
					case DIRECTION_UP: return this.y*TILE_HEIGHT + offs;
					case DIRECTION_RIGHT: return this.y*TILE_HEIGHT;
					case DIRECTION_DOWN: return this.y*TILE_HEIGHT - offs;
					case DIRECTION_LEFT: return this.y*TILE_HEIGHT;
				}
			default:
				return this.y*TILE_HEIGHT;
		}		
	}	
	

	this.drawInSpecificCoordinates = function(alpha,x,y,game) {
		if (this.currentAnimation!=null && this.animations[this.currentAnimation]!=null) {			
			switch(this.state) {
				case STATE_VEHICLE_READY:
				case STATE_VEHICLE_MOVING:
						break;
				default:
						this.animations[this.currentAnimation].draw(x,y,alpha,game);
						break;
			}
		}
	}	


	this.draw = function(alpha,offsetx,offsety,game) {
		if (this.currentAnimation!=null && this.animations[this.currentAnimation]!=null) {			
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
				case STATE_VEHICLE_READY:
						break;
				case STATE_VEHICLE_MOVING:
						break;
				default:
						this.animations[this.currentAnimation].draw(this.x*TILE_WIDTH + offsetx, this.y*TILE_HEIGHT + offsety,alpha,game);
						break;
			}
		}
	}		



	this.objectDrawTile = function(tile, x, y, alpha) {
		drawTile(tile, x, y, alpha);

		if (this.receivedDamageTimmer>0) {
			var f = this.receivedDamageTimmer/25;
			if (f>1) f = 1;
			if (f<0) f = 0;
			drawRedTintedTile(tile,x,y,alpha*f);
		}
	}
}
