Game.drawGame = function() {
	// Draw the game:
  if (this.currentCharacter>=0) {
    var currentMapIdx = this.characters[this.currentCharacter].map;
    var currentMap = this.maps[currentMapIdx];
  } else {
    var currentMapIdx = -1;
  }
	currentMap.draw(width-HUD_WIDTH,height-64,this.characters[this.currentCharacter].x, this.characters[this.currentCharacter].y, this);

	// Draw the HUD:
	this.drawHUD();

	// draw the manu stack:
  if (this.menuStack.length>0) {
	  for(i = 0;i<this.menuStack.length;i++) {
	 	  this.menuStack[i].draw();
	  }
  }
};


Game.drawHUD = function() {
  if (this.currentCharacter>=0) {
  	character = this.characters[this.currentCharacter];

    // Draw HUD:
    var tmp = ctx.globalAlpha;
    ctx.globalAlpha =1;
    ctx.strokeStyle = "#000000";
    ctx.fillStyle = '#000000';  
    ctx.beginPath();  
    ctx.rect(width-HUD_WIDTH, 0, HUD_WIDTH, height);  
    ctx.closePath();  
    ctx.fill();  
    ctx.globalAlpha = tmp;

    // selected character:
    ctx.font = "11px Emulogic";  
    ctx.fillStyle = "rgb(255,255,255)";
    var startx = width - (HUD_WIDTH-4);
    ctx.fillText(character.name, startx,16);
    ctx.fillText("L" + character.level + "  XP: " + character.experience, startx,16*3);
    ctx.fillText("HP: " + character.hp + "/" + character.max_hp, startx,16*4);
    ctx.fillText("MP: " + character.mp + "/" + character.max_mp, startx,16*5);
    if (character.getAttackBonusFromSpells()!=0) {
      ctx.fillStyle = "rgb(0,255,0)";
      ctx.fillText("ATK: " + character.getAttack(), startx,16*6);
      ctx.fillStyle = "rgb(255,255,255)";
    } else {
      ctx.fillText("ATK: " + character.getAttack(), startx,16*6);
    }
    if (character.getDefenseBonusFromSpells()!=0) {
      ctx.fillStyle = "rgb(0,255,0)";
      ctx.fillText("DEF: " + character.getDefense(), startx,16*7);
      ctx.fillStyle = "rgb(255,255,255)";
    } else {
      ctx.fillText("DEF: " + character.getDefense(), startx,16*7);
    }
    ctx.fillText("GOLD: " + character.gold, startx,16*8);

    ctx.fillText("EQUIPPED: ", startx,16*10);
    var current_y = 11;
    if (character.equippedItems[0]!=null) {
      ctx.fillText("A-" + character.equippedItems[0].name, startx,16*current_y);
      current_y++;
    }
    if (character.equippedItems[1]!=null) {
      ctx.fillText("B-" + character.equippedItems[1].name, startx,16*current_y);
      current_y++;
    }
    if (character.equippedItems[2]!=null) {
      ctx.fillText("C-" + character.equippedItems[2].name, startx,16*current_y);
      current_y++;
    }

    current_y++;
    ctx.fillText("ITEMS: ", startx,16*current_y);
    current_y++;
    for(var i = 0;i<character.inventory.length;i++) {
      ctx.fillText((i+1) + "-" + character.inventory[i].name, startx,16*current_y);
      current_y++;
    }

    current_y++;
    ctx.fillText("SPELLS: ", startx,16*current_y);
    current_y++;
    for(var i = 0;i<character.spells.length;i++) {
      var f = character.getMagicMultiplier();
      f = (f - 1)/3;
      if (f<0) f = 0;
      if (f>1) f = 1;
      var r = Math.floor(255 - f*255);
      var g = 255;
      var b = Math.floor(255 - f*255);
      ctx.fillStyle = "rgb(" +r + "," + g + "," + b + ")";
      ctx.fillText(String.fromCharCode(65 + 3 + i) + "-" + character.spells[i], startx,16*current_y);        
      ctx.fillStyle = "rgb(255,255,255)";
      current_y++;
    }
  }

  // messages:
  var tmp = ctx.globalAlpha;
  ctx.globalAlpha = 1;
  ctx.fillStyle = '#000000';  
  ctx.beginPath();  
  ctx.rect(0, height-HUD_HEIGHT, width-HUD_WIDTH, HUD_HEIGHT);  
  ctx.closePath();  
  ctx.fill();  
  ctx.globalAlpha = tmp;

  ctx.fillStyle = "rgb(255,255,255)";
//  var y = (height-64)+14;
  var y = (height-16)+14;
  for(var idx = this.messages.length-1;idx>=0;idx--) {
    var lines = splitLineIntoLines(this.messages[idx][1],72);
    var time = (new Date).getTime() - this.messages[idx][0];
    var f = time/5000;
    if (f<0) f = 0;
    if (f>1) f = 1;
    /*
    var f2 = (1 + Math.sin(time/50))/2;
    f = f + (1-f)*f2;
    */
    ctx.fillStyle = "rgb(" + Math.floor(255*f) + ",255," + Math.floor(255*f) + ")";
    for(var i = lines.length-1;i>=0;i--) {
      ctx.fillText(lines[i], 0,y);
      y-=16;
      if (y<(height-HUD_HEIGHT)) break;
    }
    if (y<(height-HUD_HEIGHT)) break;
  }	

  if (AI_DEBUGGER_FOCUS!=null) AIDebuggerDraw(AI_DEBUGGER_FOCUS);

}


Game.updateGame = function() {
	// The result from user input is transformed into a set of actions:
	this.actions = [];
  var currentCharacterIdx = this.currentCharacter;
  if (this.currentCharacter>=0) {
    var character = this.characters[this.currentCharacter];
    var currentMapIdx = character.map;
    var currentMap = this.maps[currentMapIdx];
  } else {
    var character = null;
    var currentMapIdx = -1;
  }

	if (this.menuStack.length == 0 && character!=null) {
		// process game input:
    if (keyPressed(KEY_SHIFT) && character.state == STATE_READY) {
      if (keyPressed(KEY_UP)) this.actions.push({type:ACTION_ATTACK, direction:DIRECTION_UP});
      if (keyPressed(KEY_RIGHT)) this.actions.push({type:ACTION_ATTACK, direction:DIRECTION_RIGHT});
      if (keyPressed(KEY_DOWN)) this.actions.push({type:ACTION_ATTACK, direction:DIRECTION_DOWN});
      if (keyPressed(KEY_LEFT)) this.actions.push({type:ACTION_ATTACK, direction:DIRECTION_LEFT});
      for(var i = 0;i<9;i++) {
        if (keyPress(KEY_ITEMS[i]) &&
            character.inventory.length>i) {
          this.actions.push({type:ACTION_DROP, item:i});
        } 
      }
    } else if (keyPressed(KEY_ALT)) {
      var direction = null;
      var target_x = character.x;
      var target_y = character.y;
      if (keyPressed(KEY_UP)) {
        target_y--;
        direction = DIRECTION_UP;
      }
      if (keyPressed(KEY_RIGHT)) {
        target_x++;
        direction = DIRECTION_RIGHT;
      }
      if (keyPressed(KEY_DOWN)) {
        target_y++;
        direction = DIRECTION_DOWN;
      }
      if (keyPressed(KEY_LEFT)) {
        target_x--;
        direction = DIRECTION_LEFT;
      }
      if (direction!=null) {
        // check if there is a character in the desired position, and if there is check for potential dialog actions
        var target = currentMap.getCharacter(target_x, target_y);
        if (target!=null) {
          // determine if we are unfriendly to that character:
          var unfriendlywmes = target.AI.getAllWMEs("unfriendly");
          var unfriendly = false;
          for(var j = 0;j<unfriendlywmes.length;j++) {
            var wme2 = unfriendlywmes[j];
            var tmp = "character instanceof " + wme2.params;
            if (eval(tmp)) {
              unfriendly = true;
              break;
            }
          }
          if (target!=null && character.talk_state == STATE_READY && !unfriendly) {
            this.talkDialogue(character, target, currentMap);
          } else {
            // otherwise, just send an interact action to the character:
            if (character.state == STATE_READY) this.actions.push({type:ACTION_INTERACT, direction:direction});          
          }
        } else {
          // otherwise, just send an interact action to the character:
          if (character.state == STATE_READY) this.actions.push({type:ACTION_INTERACT, direction:direction});          
        }
      }
    } else {
      // not SHIFT and not ALT
      var spell_key = -1;
      for(var i = 0;i<9;i++) {
        if (keyPressed(KEY_SPELLS[i]) && character.spells.length>i && character.state == STATE_READY) spell_key = i;
      } 

      // if no spell keys are pressed and the character can move:
      if (spell_key == -1 && (character.state == STATE_READY || character.state == STATE_VEHICLE_READY)) {
        var target_x = character.x;
        var target_y = character.y;
        var direction = null;
        if (keyPressed(KEY_UP)) {
          target_y--;
          direction = DIRECTION_UP;
        }
        if (keyPressed(KEY_RIGHT)) {
          target_x++;
          direction = DIRECTION_RIGHT;
        }
        if (keyPressed(KEY_DOWN)) {
          target_y++;
          direction = DIRECTION_DOWN;
        }
        if (keyPressed(KEY_LEFT)) {
          target_x--;
          direction = DIRECTION_LEFT;
        }
        if (direction!=null) {
          var c = currentMap.getCharacter(target_x,target_y);
          if (c==null) {
            this.actions.push({type:ACTION_MOVE, direction:direction});
          } else {
            if (c!=null) {
              // determine if we are unfriendly to that character:
              var unfriendlywmes = c.AI.getAllWMEs("unfriendly");
              var unfriendly = false;
              for(var j = 0;j<unfriendlywmes.length;j++) {
                var wme2 = unfriendlywmes[j];
                var tmp = "character instanceof " + wme2.params;
                if (eval(tmp)) {
                  unfriendly = true;
                  break;
                }
              }
              if (unfriendly) {
                this.actions.push({type:ACTION_ATTACK, direction:direction});
              } else {
                if (character.talk_state == STATE_READY) {
                  this.talkDialogue(character, c, currentMap);
                } else {
                  if (character.state == STATE_READY) this.actions.push({type:ACTION_INTERACT, direction:direction});          
                }
              }
            } else {
              this.actions.push({type:ACTION_MOVE, direction:direction});
            }
          }
        }
      }
      if (character.talk_state == STATE_READY && keyPress(KEY_TALK)) { // T: for talking
        // find the nearest other character:
        var target = currentMap.findClosestCharacterToTalk(character,this);
        this.talkDialogue(character, target, currentMap);
      }

      // take/activate objects: SPACE
      if (keyPress(KEY_SPACE)) this.actions.push({type:ACTION_TAKE});
      for(var i = 0;i<9;i++) {
        if (keyPress(KEY_ITEMS[i]) &&
            character.inventory.length>i) {
          this.actions.push({type:ACTION_USE, item:i});
        } 
      }

      // drop gold: R
      if (keyPress(KEY_DROP_GOLD)) {
        var menu = new InputDialogue(-HUD_WIDTH/2,0,15.0,"How much gold to drop?","10",10);
        menu.effectFunction = function(game, map, character, value) {
//                                  console.log("Drop coins dialogue finished with value '" + value + "'");
                                  if (value!="") {
                                    var amount = parseInt(value);
                                    if (!isNaN(amount)) {
                                      game.actions.push({type:ACTION_DROP_GOLD, gold:amount});
                                    } else {
                                      game.pushMessage("Invalid amount");
                                    }
                                  }
                                  return GAMESTATE_GAME;
                                };
        this.pushMenu(menu);
      }

      // unequip:
      if (keyPress(KEY_EQUIPPED_ITEMS[0]) && character.equippedItems[0]!=null) this.actions.push({type:ACTION_UNEQUIP, item:0});
      if (keyPress(KEY_EQUIPPED_ITEMS[1]) && character.equippedItems[1]!=null) this.actions.push({type:ACTION_UNEQUIP, item:1});
      if (keyPress(KEY_EQUIPPED_ITEMS[2]) && character.equippedItems[2]!=null) this.actions.push({type:ACTION_UNEQUIP, item:2});    

      // spell casting:
      if (spell_key!=-1) {
        var i = spell_key;
        if (character.mp>=spell_cost[character.spells[i]]) {
          if (character.spells[i]==SPELL_MAGIC_EYE) {
            this.actions.push({type:ACTION_SPELL, spell:character.spells[i]});
          } else if (character.spells[i]==SPELL_MAGIC_MISSILE || 
                     character.spells[i]==SPELL_FIREBALL || 
                     character.spells[i]==SPELL_INCINERATE) {
            var direction = null;
            if (keyPressed(KEY_UP)) direction = DIRECTION_UP;
            if (keyPressed(KEY_RIGHT)) direction = DIRECTION_RIGHT;
            if (keyPressed(KEY_DOWN)) direction = DIRECTION_DOWN;
            if (keyPressed(KEY_LEFT)) direction = DIRECTION_LEFT;
            if (direction!=null) {
              this.actions.push({type:ACTION_SPELL, spell:character.spells[i], direction:direction,offsx:0,offsy:0});              
            }
          } else {
            var direction = null;
            if (keyPressed(KEY_UP)) direction = DIRECTION_UP;
            if (keyPressed(KEY_RIGHT)) direction = DIRECTION_RIGHT;
            if (keyPressed(KEY_DOWN)) direction = DIRECTION_DOWN;
            if (keyPressed(KEY_LEFT)) direction = DIRECTION_LEFT;
            if (keyPressed(KEY_ENTER)) direction = -1;
            if (direction!=null) {
              this.actions.push({type:ACTION_SPELL, spell:character.spells[i], direction:direction,offsx:0,offsy:0});
            }
          }
        } else {
          this.pushMessage("Not enough spell points");
        }
      }
    }

    if (keyPress(KEY_CHANGE_CHARACTER)) {
      // switch characters:
      this.currentCharacter++;
      if (this.currentCharacter>=this.characters.length) this.currentCharacter = 0;
      this.maps[this.characters[this.currentCharacter].map].updateVisibility(this.characters[this.currentCharacter].x,this.characters[this.currentCharacter].y,this);
    }

    // zoom in/out
    if (keyPress(KEY_ZOOM)) {
      if (TARGET_TILE_WIDTH==TILE_WIDTH_ORIGINAL) {
        TARGET_TILE_WIDTH = TILE_WIDTH_ORIGINAL*2;
        TARGET_TILE_HEIGHT = TILE_WIDTH_ORIGINAL*2;
//      } else if (TARGET_TILE_WIDTH==64) {
//        TARGET_TILE_WIDTH = 16;
//        TARGET_TILE_HEIGHT = 16;
      } else {
        TARGET_TILE_WIDTH = TILE_WIDTH_ORIGINAL;
        TARGET_TILE_HEIGHT = TILE_WIDTH_ORIGINAL;
      }
    }

    // pause/quit (ESC):
    if (keyPress(KEY_ESC)) {
      this.paused = true;
      var menu = new Menu(-HUD_WIDTH/2,0,15.0,
                             ["Continue","Save","Load","Instructions","Quit"],
                             [0,1,2,3,4],0);
      menu.effectFunction = function(game, map, character, value) {
                              game.paused = false;
                              if (value==1) {
                                // save:
                                var m = new Menu(-HUD_WIDTH/2,0,15.0,
                                                 ["back",
                                                  "save slot 1: " + game.getSaveGameSummary(1),
                                                  "save slot 2: " + game.getSaveGameSummary(2),
                                                  "save slot 3: " + game.getSaveGameSummary(3),
                                                  "save slot 4: " + game.getSaveGameSummary(4)],
                                                 [0,1,2,3,4],0);
                                m.effectFunction = function(game, map, character, value) {
                                  if (!game.saveGame(value)) {
                                    game.pushMessage("ERROR: GAME COULD NOT BE SAVED!");
                                    console.log("Error saving game, displaying error message");
                                  }
                                  return GAMESTATE_GAME;
                                }
                                game.pushMenu(m);
                              } else if (value==2) {
                                // load:
                                var m = new Menu(-HUD_WIDTH/2,0,15.0,
                                                 ["back",
                                                  "load slot 1: " + game.getSaveGameSummary(1),
                                                  "load slot 2: " + game.getSaveGameSummary(2),
                                                  "load slot 3: " + game.getSaveGameSummary(3),
                                                  "load slot 4: " + game.getSaveGameSummary(4)],
                                                 [0,1,2,3,4],0);
                                m.effectFunction = function(game, map, character, value) {
                                  game.loadGame(value);
                                  return GAMESTATE_GAME;
                                }
                                game.pushMenu(m);
                              } else if (value==3) {
                                return GAMESTATE_INSTRUCTIONS;
                              } else if (value==4) {
                                // quit:
                                return GAMESTATE_TITLE;
                              }
                              return GAMESTATE_GAME;
                            }
      this.pushMenu(menu);
    }

	} else {
		// process menu input:
    var menu = this.menuStack[this.menuStack.length-1];
    var retVal = menu.update(this);
    if (retVal!=null) {
      this.menuStack.pop();
      if (menu.effectFunction!=null) {
        var state = menu.effectFunction(this, currentMap, character, retVal);
        if (state!=null && state!=GAMESTATE_GAME) return state;
      }
    }
	}

  // activate the AI debugger: SHIFT + D
  if (keyPressed(KEY_SHIFT) && keyPress(68)) {
    // AI debugger on!
    var debuggable = [];
    for(var i = 0;i<currentMap.layers.length;i++) {
      for(var j = 0;j<currentMap.layers[i].objects.length;j++) {
        var object = currentMap.layers[i].objects[j];
        if (object instanceof AICharacter &&
            object.AI != null &&
            object.AI instanceof HighLevelAI) debuggable.push(object); 
      }
    }

    if (debuggable.length==0) {
      AI_DEBUGGER_FOCUS = null
    } else {
      var idx = debuggable.indexOf(AI_DEBUGGER_FOCUS);
      if (idx==-1) {
        AI_DEBUGGER_FOCUS = debuggable[0];
      } else {
        if (idx==debuggable.length-1) {
          AI_DEBUGGER_FOCUS = null;
        } else {
          AI_DEBUGGER_FOCUS = debuggable[idx+1];
        }
      }
    }

    if (AI_DEBUGGER_FOCUS!=null) console.log("AI DEBUGGER FOCUS: " + AI_DEBUGGER_FOCUS.name + " at " + AI_DEBUGGER_FOCUS.x + "," + AI_DEBUGGER_FOCUS.y);
  }

  if (!this.paused) {
    if (character!=null) {
    	// Send all the actions from user input to the current character:
    	for(i = 0;i<this.actions.length;i++) {
    		action = this.actions[i];
    		character.sendAction(action, currentMap, this);
    	}
    }

    // if a character has gone through a bridge to reach a room where the other character is not there, respawn:
    if (currentMapIdx!=character.map && currentCharacterIdx==this.currentCharacter && 
        (this.characters.length<2 || this.characters[0].map != this.characters[1].map)) {
      var map = this.maps[character.map];
      map.respawnEnemies(this.cycle);
    }

    // update all the maps where there is a player, and all the neighbors (for enemies coming in and out):
    {
      var mapsToUpdate = [];
      for(var i = 0;i<this.characters.length;i++) {
        var mapIdx = this.characters[i].map;
        if (mapsToUpdate.indexOf(mapIdx)==-1) mapsToUpdate.push(mapIdx);
        var map = this.maps[this.characters[i].map];
        for(var j = 0;j<map.neighbors.length;j++) {
          if (mapsToUpdate.indexOf(map.neighbors[j])==-1) mapsToUpdate.push(map.neighbors[j]);
        }
      }

      for(var i = 0;i<mapsToUpdate.length;i++) {
        if (!this.maps[mapsToUpdate[i]].update(this)) return GAMESTATE_TITLE;
      }
    }
  }


  if (this.onStart.length>0) {
    var retVal = executeRuleEffect(this.onStart[0], null, null, this, null);
    if (retVal!=undefined) this.onStart.splice(0,1);
  }

  // check for story state rules:
  if (this.storyStateLastCycleUpdated > this.storyStateRulesLastCycleChecked) {
    for(var i = 0;i<this.storyStateRules.length;i++) {
      var rule = this.storyStateRules[i];
      if (!rule.once || !rule.triggered) {
        var triggered = false;
        if (rule.scope=="game") {
          if (this.storyState[rule.variable] == rule.value) triggered = true;
//        } else if (rule.scope=="map") {
//          if (this.storyState[rule.variable] == rule.value) triggered = true;
//          } else if (rule.scope=="character") {
//            if (this.storyState[rule.variable] == rule.value) triggered = true;
        } else {
          console.log("storyStateRule in a game has an incorrect scope (should be game): " + rule.scope);
        }
        if (triggered) {
          rule.triggered = true;
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
    var retVal = executeRuleEffect(this.actionsToExecute[0], null, null, this, null);
    if (retVal!=undefined) this.actionsToExecute.splice(0,1);
  }


  this.cycle++;

  // Detect game completed:
  if (this.gameComplete==1) {
    return GAMESTATE_ENDING;    
  }

   return GAMESTATE_GAME;
};


Game.talkDialogue = function(character, target, currentMap) {
  var performatives = character.availableTalkPerformatives(target, currentMap, this);
  var names = [];
  for(var idx in performatives) {
    if (performatives[idx].type == TALK_ASK) {
//      names.push("tell me about " + performatives[idx].topic);
      names.push(performatives[idx].topic);
    } else {
      names.push(performatives[idx].type);
    }
  }
  names.push("CANCEL");
  performatives.push(0);

  var menu = new Menu(-HUD_WIDTH/2,0,15.0,names,
                                          performatives,names.length-1);
  menu.effectFunction = function(game, map, character, value) {
                            if (value!=0) {
//                              console.log("Option selected: " + value);
                              game.actions.push({type:ACTION_TALK, performative:value});
                            } else {
//                              console.log("Talk command canceled");
                            }
                            return GAMESTATE_GAME;
                          };
  this.pushMenu(menu);  
}


Game.pushMenu = function(menu) {
  this.menuStack.push(menu);
}
