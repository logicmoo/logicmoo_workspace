// Definition of all the constants of the game:

// Keys:
var KEY_LEFT = 37;
var KEY_UP = 38;
var KEY_RIGHT = 39;
var KEY_DOWN = 40;
var KEY_SHIFT = 16;
var KEY_ALT = 18;
var KEY_SPACE = 32;
var KEY_ENTER = 13;
var KEY_ESC = 27;
var KEY_SPELLS = [68,69,70,71,72,73,74,75,76];  // letters from from 'd' to 'l'
var KEY_ITEMS =  [49,50,51,52,53,54,55,56];     // numbers from 1 to 8
var KEY_EQUIPPED_ITEMS = [65,66,67]             // a,b,c
var KEY_TALK = 84 // t
var KEY_DROP_GOLD = 82; // r
var KEY_CHANGE_CHARACTER = 78;  // n
var KEY_ZOOM = 90;  // z

// Direction:
var DIRECTION_NONE = -1;
var DIRECTION_UP = 0;
var DIRECTION_RIGHT = 1;
var DIRECTION_DOWN = 2;
var DIRECTION_LEFT = 3;
var direction_offx = [0,1,0,-1];
var direction_offy = [-1,0,1,0];
var opposite_direction = [2,3,0,1];

// Character actions and states:
var ACTION_NONE = 0;
var ACTION_MOVE = 1;
var ACTION_TAKE = 2;
var ACTION_DROP = 3;
var ACTION_DROP_GOLD = 4;
var ACTION_USE = 5;
var ACTION_UNEQUIP = 6;
var ACTION_ATTACK = 7;
var ACTION_INTERACT = 8;  // chop trees, open doors, etc. 
var ACTION_TALK = 9; 
var ACTION_TALK_ANGRY = 10; 
var ACTION_SPELL = 11; 

var STATE_READY = 0;
var STATE_MOVING = 1;
var STATE_ATTACKING = 2;
var STATE_INTERACTING = 3;
var STATE_CASTING = 4;
var STATE_VEHICLE_READY = 5;
var STATE_VEHICLE_MOVING = 6;
var STATE_DEATH = 7;
var STATE_TALKING = 1;  // only for talk_state

// Animations:
// these are suffixes to add to the animations below:
var ANIM_DIRECTIONS = ["-up","-right","-down","-left"]; // indexed by DIRECTON_UP, DIRECTION_RIGHT, ...
var ANIM_IDLE = "idle";
var ANIM_MOVING = "moving";
var ANIM_ATTACKING = "attacking";
var ANIM_INTERACTING = "interacting";
var ANIM_CASTING = "casting";
var ANIM_TALKING = "talking";
var ANIM_DEATH = "death";

// Events:
var EVENT_USE = "use";                // when an object is used, or a lever is activated/deactivated
var EVENT_ACTIVATE = "activate";      // when a character moves a lever to the "on" position
var EVENT_DEACTIVATE = "deactivate";  // when a character moves a lever to the "off" position
var EVENT_PICKUP = "pickup";          // when a character picks up an object
var EVENT_OPEN = "open";              // when a door or a chest is open
var EVENT_CLOSE = "close";            // when a door is closed
var EVENT_PUSH = "push";              // when a pushable wall block is pushed
var EVENT_PRESS = "press";            // when an item/character is placed over a pressure plate
var EVENT_CHARACTERPRESS = "characterpress";  // when a character moves onto a pressure plate
var EVENT_PLAYERPRESS = "playerpress";        // when a player moves onto a pressure plate
var EVENT_TIMER = "timer";            // used to program timers

// Spells:
var SPELL_MAGIC_MISSILE = "magic missile";
var SPELL_HEAL = "heal";
var SPELL_SHIELD = "shield";
var SPELL_INCREASE = "increase";
var SPELL_DECREASE = "decrease";
var SPELL_FIREBALL = "fireball";
var SPELL_MAGIC_EYE = "magic eye";
var SPELL_REGENERATE = "regenerate";
var SPELL_INCINERATE = "incinerate";
var spell_cost = {};
spell_cost[SPELL_MAGIC_MISSILE] = 1;
spell_cost[SPELL_HEAL] = 1;
spell_cost[SPELL_SHIELD] = 2;
spell_cost[SPELL_INCREASE] = 2;
spell_cost[SPELL_DECREASE] = 2;
spell_cost[SPELL_FIREBALL] = 4;
spell_cost[SPELL_MAGIC_EYE] = 4;
spell_cost[SPELL_REGENERATE] = 4;
spell_cost[SPELL_INCINERATE] = 8;

// Inventory:
var ITEM_WEAPON = 0;
var ITEM_OFF_HAND = 1;
var ITEM_RING = 2;
var INVENTORY_SIZE = 8;

// Conversation constants:
var TALK_HI = "hi";
var TALK_BYE = "bye";
//var TALK_YES = "yes";
//var TALK_NO = "no";
var TALK_TRADE = "trade/give";
var TALK_ASK = "ask";

// Screen configuration:
var HUD_WIDTH = 176;    // size of the side bar
var HUD_HEIGHT = 64;   // size of the message window
var TILE_WIDTH_ORIGINAL = 64;
var TILE_HEIGHT_ORIGINAL = 64;
var TILE_WIDTH = 64;
var TILE_HEIGHT = 64;
var TARGET_TILE_WIDTH = 64;
var TARGET_TILE_HEIGHT = 64;

// animations for basic objects:
var COINPURSE_ANIMATION = "coinpurse";  
var STATUS_CURIOUS_ANIMATION = "curious";  
var STATUS_SCARED_ANIMATION = "scared";  
var STATUS_ANGRY_ANIMATION = "angry";  
var STATUS_TIRED_ANIMATION = "tired";  
var STATUS_HAPPY_ANIMATION = "happy";  


// Game states:
var GAMESTATE_TITLE = 0;
var GAMESTATE_GAME = 1;
var GAMESTATE_INSTRUCTIONS = 2;
var GAMESTATE_ENDING = 4;

var AI_DEBUGGER_FOCUS = null;

// Create the game:
var Game = {
  cycle : 0, // cycles of actual game play (Reset each time a game restarts)
  state : 0,
  state_cycle : 0,
  paused : false,

};


Game.loadCharacters = function(xmlFilePath) {
  var xmlhttp=new XMLHttpRequest();
  xmlhttp.overrideMimeType("text/xml");
  xmlhttp.open("GET",xmlFilePath,false); 
  xmlhttp.send();
  var xmlDoc=xmlhttp.responseXML;
  var topElement = xmlDoc.documentElement;

  var characterTypeElements = topElement.getElementsByTagName("CharacterClass");
  for(var i = 0;i<characterTypeElements.length;i++) {
    var characterTypeElement = characterTypeElements[i];
    var className = characterTypeElement.attributes.getNamedItem("class").nodeValue;
    var name = characterTypeElement.attributes.getNamedItem("name").nodeValue;
    var superName = characterTypeElement.attributes.getNamedItem("super").nodeValue;

    console.log("Loading character type '" + characterTypeElement.attributes.getNamedItem("class").nodeValue + "'...");

//    this.characterTypeTable[className] = characterTypeElement;

    eval("window[className] = function() { this.getClassName = function() {return \"" + className + "\";}; this.decodeCharacterFromXML(true); };");
    window[className].prototype = eval("new " + superName + "()");
    window[className].prototype.xml = characterTypeElement;
  }
}



Game.loadObjects = function(xmlFilePath) {
  var xmlhttp=new XMLHttpRequest();
  xmlhttp.overrideMimeType("text/xml");
  xmlhttp.open("GET",xmlFilePath,false); 
  xmlhttp.send();
  var xmlDoc=xmlhttp.responseXML;
  var topElement = xmlDoc.documentElement;

  var objectTypeElements = topElement.getElementsByTagName("ObjectClass");
  for(var i = 0;i<objectTypeElements.length;i++) {
    var objectTypeElement = objectTypeElements[i];
    var className = objectTypeElement.attributes.getNamedItem("class").nodeValue;
    var name = objectTypeElement.attributes.getNamedItem("name").nodeValue;
    var superName = objectTypeElement.attributes.getNamedItem("super").nodeValue;

    console.log("Loading object type '" + objectTypeElement.attributes.getNamedItem("class").nodeValue + "'...");

    eval("window[className] = function() { this.getClassName = function() {return \"" + className + "\";}; this.decodeObjectFromXML(); };");
    window[className].prototype = eval("new " + superName + "()");
    window[className].prototype.xml = objectTypeElement;
  }
}


Game.initialize = function(gamepath, gamefile) {
  // read the game file:
  var gamefilePath = gamepath + gamefile;
  var xmlhttp=new XMLHttpRequest();
  xmlhttp.overrideMimeType("text/xml");
  xmlhttp.open("GET",gamefilePath,false); 
  xmlhttp.send();
  var xmlDoc=xmlhttp.responseXML;
  var gameElement = xmlDoc.documentElement;

  // load game name:
  this.gameName = gameElement.attributes.getNamedItem("name").nodeValue;

  // load game title image:
  this.titleIMG_bg = null;
  if (gameElement.getElementsByTagName("titleImage").length>0) {
    this.titleIMG_bg = new Image();
    this.titleIMG_bg.src = gamepath + gameElement.getElementsByTagName("titleImage")[0].textContent;
  }

  // load game story:
  this.story = [];
  var storyElements = gameElement.getElementsByTagName("story")[0].getElementsByTagName("line");
  for(var i = 0;i<storyElements.length;i++) {
    this.story.push(storyElements[i].textContent);
  }
  this.ending = [];
  var endingElements = gameElement.getElementsByTagName("ending")[0].getElementsByTagName("line");
  for(var i = 0;i<endingElements.length;i++) {
    this.ending.push(endingElements[i].textContent);
  }
  this.instructions = ["Quick keys:",
                    "- Move with the arrow keys.",
                    "- Walk into enemies to attack, and into NPCs to talk.",
                    "- SHIFT + arrow keys to force attack",
                    "- ALT + arrow keys to interact (talk, open, etc.)",
                    "- 'T' to talk to the closest character.",
                    "- SPACE to take/use/embark/disembark",
                    "- Switch characters with 'N'.",
                    "- Use/equip/unquip items by pressing their number.",
                    "- SHIFT + number to drop an item.",
                    "- 'R' to drop gold.",
                    "- letters to cast spells (+ arrow keys for direction",
                    "  or 'enter' to cast on self)",
                    "- 'Z' to zoom in and out.",
                    "- Hold 'TAB' to increase the game speed x4.",
                    "- 'ESC' to pause/load/save/quit.",
                    "",
                    "Hints:",
                    "- Levers open/close doors or trigger other secrets.", 
                    "- You might lose hit points by fighting.",
                    "- Find potions to recover hit/magic points.",
                    "- Remember to equip the most powerful items.",
                    "- Explore methodically, you might find some clues...",
                    "- To fight while on a vehicle, first disembark."];  

  // audio:
  this.audioFiles = {};

  // Load tile types:
//  this.defaultTileEntry = null;
  this.tiles = {};
  var tilesElement = gameElement.getElementsByTagName("tiles")[0];
  TILE_WIDTH_ORIGINAL = parseInt(tilesElement.attributes.getNamedItem("sourcewidth").nodeValue);
  TILE_HEIGHT_ORIGINAL = parseInt(tilesElement.attributes.getNamedItem("sourceheight").nodeValue);
  TARGET_TILE_WIDTH = TILE_WIDTH = parseInt(tilesElement.attributes.getNamedItem("targetwidth").nodeValue);
  TARGET_TILE_HEIGHT = TILE_WIDTH = parseInt(tilesElement.attributes.getNamedItem("targetheight").nodeValue);

  // default animations:
  this.defaultAnimations = {};
  var animationElements = tilesElement.getElementsByTagName("animation");
  for(var i = 0;i<animationElements.length;i++) {
    var ae = animationElements[i];
    this.defaultAnimations[ae.attributes.getNamedItem("name").nodeValue] = Animation.fromXML(ae);
  }

  var tileTypesElements = tilesElement.getElementsByTagName("types");
  for(var i = 0;i<tileTypesElements.length;i++) {
    var tmp = tileTypesElements[i];
    var name = tmp.attributes.getNamedItem("file").nodeValue;
    console.log("tileTypes for " + name);
    var types = eval("[" + tmp.textContent + "]");
    if (!(name in this.tiles)) {
      console.log("starting to load " + name);
      var te = {image:new Image()};
      te.image.te = te;
      te.image.onload = function() {generateRedTintedImage(this.te);};
      te.image.src = gamepath + name;
      this.tiles[name] = te;
//      if (this.defaultTileEntry==null) this.defaultTileEntry = te;
    }
    var tilesEntry = this.tiles[name];  
    tilesEntry.tileTypes = types;  
  }
  var tileTypesElements = tilesElement.getElementsByTagName("seeThrough");
  for(var i = 0;i<tileTypesElements.length;i++) {
    var tmp = tileTypesElements[i];
    var name = tmp.attributes.getNamedItem("file").nodeValue;
    console.log("seeThrough for " + name);
    var seeThrough = eval("[" + tmp.textContent + "]");
    if (!(name in this.tiles)) {
      console.log("starting to load " + name);
      var te = {image:new Image()};
      te.image.te = te;
      te.image.onload = function() {generateRedTintedImage(this.te);};
      te.image.src = gamepath + name;
      this.tiles[name] = te;
//      if (this.defaultTileEntry==null) this.defaultTileEntry = te;
    }
    var tilesEntry = this.tiles[name];  
    tilesEntry.seeThrough = seeThrough;  
  }
  var tileTypesElements = tilesElement.getElementsByTagName("canDig");
  for(var i = 0;i<tileTypesElements.length;i++) {
    var tmp = tileTypesElements[i];
    var name = tmp.attributes.getNamedItem("file").nodeValue;
    console.log("canDig for " + name);
    var canDig = eval("[" + tmp.textContent + "]");
    if (!(name in this.tiles)) {
      console.log("starting to load " + name);
      var te = {image:new Image()};
      te.image.te = te;
      te.image.onload = function() {generateRedTintedImage(this.te);};
      te.image.src = gamepath + name;
      this.tiles[name] = te;
//      if (this.defaultTileEntry==null) this.defaultTileEntry = te;
    }
    var tilesEntry = this.tiles[name];  
    tilesEntry.canDig = canDig;  
  }

  // load conversation topics:
  this.defaultConversationTopics = [];

  // load npc/enemy/player definitions:
  console.log("Loading character types...");
//  this.characterTypeTable = {};
  var characterDefinitionElements = gameElement.getElementsByTagName("characterDefinition");
  for(var i = 0;i<characterDefinitionElements.length;i++) {
    var filePath = gamepath + characterDefinitionElements[i].attributes.getNamedItem("file").nodeValue;
    console.log("Loading characters from '" + filePath + "'...");
    this.loadCharacters(filePath, this);
  }

  console.log("Loading object types...");
//  this.characterTypeTable = {};
  var objectDefinitionElements = gameElement.getElementsByTagName("objectDefinition");
  for(var i = 0;i<objectDefinitionElements.length;i++) {
    var filePath = gamepath + objectDefinitionElements[i].attributes.getNamedItem("file").nodeValue;
    console.log("Loading objects from '" + filePath + "'...");
    this.loadObjects(filePath, this);
  }


  // load maps:
  this.mapCreationData = [];
  var mapElements = gameElement.getElementsByTagName("map");
  for(var i = 0;i<mapElements.length;i++) {
    if (mapElements[i].attributes.getNamedItem("file")!=null) {
      this.mapCreationData.push({type:"file", path:gamepath + mapElements[i].attributes.getNamedItem("file").nodeValue});
    } else {
      if (mapElements[i].attributes.getNamedItem("method")!=null) {
        this.mapCreationData.push({type:"method", method:mapElements[i].attributes.getNamedItem("method").nodeValue});
      } else {
        console.log("ERROR! map specified without a file or a creation method in game file!");
      }
    }
  }

  // load player starting positions:
  this.playerStartingPositions = [];
  var playerElements = gameElement.getElementsByTagName("player");
  for(var i = 0;i<playerElements.length;i++) {
    var playerElement = playerElements[i];
    this.playerStartingPositions.push({type : playerElement.attributes.getNamedItem("class").nodeValue,
                                       x : parseInt(playerElement.attributes.getNamedItem("x").nodeValue),
                                       y : parseInt(playerElement.attributes.getNamedItem("y").nodeValue),
                                       map : parseInt(playerElement.attributes.getNamedItem("map").nodeValue)});
  }

  initAIRules(this);
  parseAIRules(this,gameElement);
}


Game.loadAudioFile = function(fileName) {
  var file = this.audioFiles[fileName];
  if (file==undefined) {
    file = new Audio(gamepath + fileName);
    this.audioFiles[fileName] = file;
  }
  return file;
}


Game.draw = function() { 
  // clear background:
  var tmp = ctx.globalAlpha;
  ctx.globalAlpha = 1;
  ctx.fillStyle = '#000000';  
  ctx.beginPath();  
  ctx.rect(0, 0, canvas.width, canvas.height);  
  ctx.closePath();  
  ctx.fill();  
  ctx.globalAlpha = tmp;

  switch(this.state) {
    case GAMESTATE_TITLE: this.drawTitle();
            break;
    case GAMESTATE_GAME: this.drawGame();
            break;
    case GAMESTATE_ENDING: this.drawEnding();
            break;
    case GAMESTATE_INSTRUCTIONS: this.drawInstructions();
            break;
  }

/*
    var tmp = ctx.globalAlpha;
    ctx.globalAlpha = 1;
    ctx.fillStyle = "#ffffff";
    ctx.font = "11px Emulogic";  
    ctx.fillText("Mouse: " + mouseState.x + ", " + mouseState.y , 8, 16);
    ctx.fillText("Cycle: " + this.cycle + " (fps " + FPS + ")", 8, 32)
    ctx.fillText("State: " + this.state + "  State Cycle: " + this.state_cycle, 8, 48);
    ctx.globalAlpha = tmp;
*/
};

Game.update = function() { 
  var oldState = this.state;
  switch(this.state) {
    case GAMESTATE_TITLE: this.state = this.updateTitle();
            break;
    case GAMESTATE_GAME: this.state = this.updateGame();
            break;
    case GAMESTATE_ENDING: this.state = this.updateEnding();
            break;
    case GAMESTATE_INSTRUCTIONS: this.state = this.updateInstructions();
            break;
  }
  if (oldState==this.state) {
    this.state_cycle++;
  } else {
    this.state_cycle=0;
  }

  if (TILE_HEIGHT<TARGET_TILE_HEIGHT) {
    TILE_HEIGHT++;
    TILE_WIDTH = TILE_HEIGHT;
  }
  if (TILE_HEIGHT>TARGET_TILE_HEIGHT) {
    TILE_HEIGHT--;
    TILE_WIDTH = TILE_HEIGHT;
  }
};

Game.pushMessage = function(msg) {
  if (this.messages.length>0 && this.messages[this.messages.length-1][1] == msg) {
    this.messages[this.messages.length-1][0] = (new Date).getTime();
  } else {
    this.messages.push([(new Date).getTime(),msg]);  
    if (this.messages.length>4) {
      this.messages.splice(0,1);
    }
  }
}

Game.warpObject = function(object, x, y, map, layer) {
  old_map = this.maps[object.map];
  new_map = this.maps[map];

  // record the WARP for the perception module of the character AI:
  old_map.warps.push({x:object.x, y:object.y, object: object, timer: 200});

  old_map.removeObject(object);
  new_map.addObject(object,layer);

  object.x = x;
  object.y = y;
  object.map = map;
}

// 'topic' should be a list of two elements, something like: ["monks","There is a massive amount of monks in the Black rock!"]
Game.addConversationTopic = function(topic) {
  for(var i=0;i<this.conversationTopics.length;i++) {
    if (this.conversationTopics[i][0]==topic[0]) return;
  }
  this.conversationTopics.push(topic);
}


Game.resetGame = function() {
  this.gameComplete = 0;  // 0: not complete, 1: complete, but not shown to the player, 2: complete after shown to the player
  this.endingMenu = null;
  this.conversationTopics = [];
  for(var i = 0;i<this.defaultConversationTopics.length;i++) {
    this.conversationTopics.push(this.defaultConversationTopics[i]);
  }
  this.cycle = 0;
  this.paused = false;

  this.storyState = {}; // the story state is where all the variables used by the scripts are stored
  this.storyStateRulesLastCycleChecked = -1;
  this.storyStateLastCycleUpdated = -1;
  this.actionsToExecute = [];  // when rules get triggered, they push their actions into this queue

  this.maps = createMaps(this.mapCreationData, this);
  this.characters = createCharacters(this.playerStartingPositions, this);
  this.currentCharacter = 0;
  this.messages = [];
  this.menuStack = [];
  this.pushMessage("You have entered " + this.maps[this.characters[this.currentCharacter].map].name);
  this.maps[this.characters[this.currentCharacter].map].updateVisibility(this.characters[this.currentCharacter].x,this.characters[this.currentCharacter].y,this);
}


// Load/Save game:
Game.saveGame = function(slot) {
  var saveID = this.gameName + "slot" + slot;
  var summaryString = this.generateSummarySaveString();
  var saveString = this.generateSaveString();
  if (saveString!=null) {
    localStorage.setItem(saveID+"summary",summaryString);
    localStorage.setItem(saveID,saveString);
    return true;
  }
  console.log("Error saving the game");
  return false;
}


Game.loadGame = function(slot) {
  var saveID = this.gameName + "slot" + slot;
  var saveString = localStorage.getItem(saveID);
  if (saveString==null) return;

  // decode string:
  this.decodeSaveString(saveString);
}


Game.getSaveGameSummary = function(slot) {
  var saveID = this.gameName + "slot" + slot;
  var data = localStorage.getItem(saveID + "summary");
  if (data==null) return "-";
  return data;
}


Game.generateSummarySaveString = function() {
  var tmp = "";
  for(var i = 0;i<this.characters.length;i++) {
    if (tmp!="") tmp = tmp + ", ";
    tmp = tmp + "LVL" + this.characters[0].level;
  }
  return tmp;
}


Game.generateSaveString = function() {
  // encode game state:
  // encode the global Game variables:
  var data = {};
  data.cycle = this.cycle;
  data.endingMenu = this.endingMenu;
  data.conversationTopics = this.conversationTopics;
  data.storyState = this.StoryState;
  data.currentCharacter = this.currentCharacter;
  data.messages = this.messages;

  // encode the objects:
  var objects = [];
  for(var i = 0;i<this.characters.length;i++) {
    objects = this.characters[i].generateObjectListForSaveData(objects);
  }
  for(var i = 0;i<this.maps.length;i++) {
    objects = this.maps[i].generateObjectListForSaveData(objects);
  }  
//  console.log("Objects: " + objects.length);

  data.characters = [];
  for(var i = 0;i<this.characters.length;i++) {
    data.characters.push(objects.indexOf(this.characters[i]));
  }
  
  // encode each of the maps:
  var mapsdata = [];
  for(var i = 0;i<this.maps.length;i++) {
    var mapdata = this.maps[i].generateSaveData(objects);
    mapsdata.push(mapdata);
  }
  data.maps = mapsdata;

  // encode each of the objects:
  var objectsdata = [];
  for(var i = 0;i<objects.length;i++) {
    var objectdata = objects[i].generateSaveData(objects,{});
    objectsdata.push(objectdata);
  }
  data.objects = objectsdata;

  try {
    var globalString = JSON.stringify(data);
    return globalString;
  } catch (err) {
    console.log(err);
    console.log("Error generating the save string!");
    /*
    findCircularityFeature(data);
    for(var i = 0;i<data.objects.length;i++) {
      if (data.objects[i].AI!=undefined)
        findCircularityFeature(data.objects[i].AI);
    }
    */
    return null;
  }
}

/*
function findCircularityFeature(p) {
  for(var propt in p) {
    try {
      JSON.stringify(p[propt]);
    } catch(err) {
      console.log("Circularity in " + propt);
    }
  }  
}
*/

Game.decodeSaveString = function(saveString) {
  var data = JSON.parse(saveString);
  if (data!=null) {
    // recover the state:
    this.resetGame();
    this.cycle = data.cycle;
    this.endingMenu = data.endingMenu;
    this.conversationTopics = data.conversationTopics;
    this.storyState = data.storyState;
    this.currentCharacter = data.currentCharacter;
    this.messages = data.messages;

    // decode the objects:
    // first pass (create dummy versions of each object, of the right class):
    var objects = [];
    for(var i = 0;i<data.objects.length;i++) {
      var otmp = data.objects[i];
      var tmp = "new " + otmp.className + "()";
      var o = eval(tmp);
      objects.push(o);
    }
    // second pass (set their properties):
    for(var i = 0;i<data.objects.length;i++) {
      var otmp = data.objects[i];
      var o = objects[i];
      for(var propt in otmp) {
        o[propt] = otmp[propt];
      }
      // decode animations:
      if (otmp.animations!=undefined) {
        for(var key in otmp.animations) {
          o.animations[key] = eval(otmp.animations[key]);
        }
      }
      if (otmp.AI!=undefined) {
        o.AI = new HighLevelAI();
        o.AI.reconstructFromSaveData(objects, o, otmp.AI);
      }
    }
    // third pass (resolve references):
    for(var i = 0;i<objects.length;i++) {
      var o = objects[i];
      if (o.itemInside!=undefined) {
        if (o.itemInside==-1) {
          o.itemInside = null;
        } else {
          o.itemInside = objects[o.itemInside];
        }
      }
      if (o.owner!=undefined) {
        if (o.owner==-1) {
          o.owner = null;
        } else {
          o.owner = objects[o.owner];
        }
      }
      if (o.target!=undefined) {
        if (o.target==-1) {
          o.target = null;
        } else {
          o.target = objects[o.target];
        }
      }
      if (o.just_attacked_by!=undefined) {
        if (o.just_attacked_by==-1) {
          o.just_attacked_by = null;
        } else {
          o.just_attacked_by = objects[o.just_attacked_by];
        }
      }
      if (o.vehicle!=undefined) {
        if (o.vehicle==-1) {
          o.vehicle = null;
        } else {
          o.vehicle = objects[o.vehicle];
        }
      }
      if (o.inventory!=undefined) {
        for(var j = 0;j<o.inventory.length;j++) {
          if (o.inventory[j]==-1) {
            o.inventory[j] = null;
          } else {
            o.inventory[j] = objects[o.inventory[j]];
          }         
        }
      }
      if (o.equippedItems!=undefined) {
        for(var j = 0;j<o.equippedItems.length;j++) {
          if (o.equippedItems[j]==-1) {
            o.equippedItems[j] = null;
          } else {
            o.equippedItems[j] = objects[o.equippedItems[j]];
          }         
        }
      }
      if (o.posessions!=undefined) {
        for(var j = 0;j<o.posessions.length;j++) {
          if (o.posessions[j]==-1) {
            o.posessions[j] = null;
          } else {
            o.posessions[j] = objects[o.posessions[j]];
          }         
        }
      }      
      if (o.following!=undefined) {
        for(var j = 0;j<o.following.length;j++) {
          if (o.following[j]==-1) {
            o.following[j] = null;
          } else {
            o.following[j] = objects[o.following[j]];
          }         
        }
      }      
    }
    for(var i = 0;i<data.characters.length;i++) {
      this.characters[i] = objects[data.characters[i]];
    }

    // decode the maps:
    for(var i = 0;i<data.maps.length;i++) {
      var mapdata = data.maps[i];
      var map = this.maps[i];
      map.visibility = mapdata.visibility;
      map.messages = mapdata.messages;
      map.respawnData = mapdata.respawnData;
      for(var j = 0;j<map.layers.length;j++) {
        var layerdata = mapdata.layers[j];
        var layer = map.layers[j];
        layer.tiles = layerdata.tiles;
        layer.types = layerdata.types;
        layer.seeThrough = layerdata.seeThrough;
        var tmp = [];
        for(var k = 0;k<layerdata.objects.length;k++) {
          tmp.push(objects[layerdata.objects[k]]);
        }
        layer.objects = tmp;
      }
    }
  }
}


function isInt(value) {
   return !isNaN(value) && parseInt(Number(value)) === value;
}


// parse the value: it can be something like this:
// 1
// 1,2
// 1-5,3,10-20
function parseStringValue(value) {
    value = value.split(",");
    value = value[Math.floor(Math.random()*value.length)];
    value = value.split("-");
    if (value.length==0) {
      value = 0;
    } else if (value.length==2) {
      value = Math.floor(Math.random()*((eval(value[1])+1)-eval(value[0]))+eval(value[0]));
    } else {
      value = eval(value[0]);
    }
    return value;
}


