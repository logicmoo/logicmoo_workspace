// Code to load actions:

function decodeRuleEffectFromXML(actionElement) {
  if (actionElement.tagName=="gameComplete") {
    return {action : actionElement.tagName};
  } else 
  if (actionElement.tagName=="addBehavior") {
    var priority = parseInt(actionElement.attributes.getNamedItem("priority").nodeValue);
    var behaviorName = actionElement.textContent;
    var id = actionElement.attributes.getNamedItem("id");
    if (id!=null) id = id.nodeValue;
    return {action : actionElement.tagName, 
            behaviorName : behaviorName,
            priority : priority,
            id : id};
  } else 
  if (actionElement.tagName=="removeBehavior") {
    var id = actionElement.attributes.getNamedItem("id").nodeValue;
    return {action : actionElement.tagName,
            id: id};
  } else 
  if (actionElement.tagName=="teleport") {
    var xstr = eval(actionElement.attributes.getNamedItem("x").nodeValue);
    var ystr = eval(actionElement.attributes.getNamedItem("y").nodeValue);
    var mapstr = actionElement.attributes.getNamedItem("map");    
    if (mapstr!=null) mapstr = eval(mapstr.nodeValue);
    return {action : actionElement.tagName, 
        x : xstr,
        y : ystr,
        map : mapstr};
  } else 
  if (actionElement.tagName=="goTo") {
    var xstr = eval(actionElement.attributes.getNamedItem("x").nodeValue);
    var ystr = eval(actionElement.attributes.getNamedItem("y").nodeValue);
    var mapstr = actionElement.attributes.getNamedItem("map");    
    if (mapstr!=null) mapstr = eval(mapstr.nodeValue);
    return {action : actionElement.tagName, 
        x : xstr,
        y : ystr,
        map : mapstr};
  } else 
  if (actionElement.tagName=="die") {
    return {action : actionElement.tagName};
  } else 
  if (actionElement.tagName=="activateLever") {
    var xstr = eval(actionElement.attributes.getNamedItem("x").nodeValue);
    var ystr = eval(actionElement.attributes.getNamedItem("y").nodeValue);
    var mapstr = actionElement.attributes.getNamedItem("map");    
    if (mapstr!=null) mapstr = eval(mapstr.nodeValue);
    return {action : actionElement.tagName, 
        x : xstr,
        y : ystr,
        map : mapstr};
  } else 
  if (actionElement.tagName=="openDoor") {
    var doorstr = eval(actionElement.attributes.getNamedItem("door").nodeValue);
    return {action : actionElement.tagName, door : doorstr};
  } else 
  if (actionElement.tagName=="message") {
    return {action : actionElement.tagName,
            text : actionElement.attributes.getNamedItem("text").nodeValue};
  } else 
  if (actionElement.tagName=="talk") {
    var subActions = [];
    for(var j = 0;j<actionElement.childNodes.length;j++) {
      var subActionElement = actionElement.childNodes[j];
      var subAction = null;
      if (subActionElement.tagName!=undefined) subAction = this.decodeRuleEffectFromXML(subActionElement);
      if (subAction!=null) subActions.push(subAction);
    }
    return {action : actionElement.tagName, 
        text : actionElement.attributes.getNamedItem("text").nodeValue,
        IP: 0,
        subactions: subActions};
  } else 
  if (actionElement.tagName=="pendingTalk") {
    var subActions = [];
    for(var j = 0;j<actionElement.childNodes.length;j++) {
      var subActionElement = actionElement.childNodes[j];
      var subAction = null;
      if (subActionElement.tagName!=undefined) subAction = this.decodeRuleEffectFromXML(subActionElement);
      if (subAction!=null) subActions.push(subAction);
    }
    return {action : actionElement.tagName, 
        character : actionElement.attributes.getNamedItem("character").nodeValue,
        text : actionElement.attributes.getNamedItem("text").nodeValue,
        IP: 0,
        subactions: subActions};
  } else 
  if (actionElement.tagName=="addTopic") {
    return {action : actionElement.tagName, 
        topic : actionElement.attributes.getNamedItem("topic").nodeValue,
        text : actionElement.attributes.getNamedItem("text").nodeValue};
  } else 
  if (actionElement.tagName=="updateConversationRule") {
    var subActions = [];
    for(var j = 0;j<actionElement.childNodes.length;j++) {
      var subActionElement = actionElement.childNodes[j];
      var subAction = null;
      if (subActionElement.tagName!=undefined) subAction = this.decodeRuleEffectFromXML(subActionElement);
      if (subAction!=null) subActions.push(subAction);
    }
    return {action : actionElement.tagName, 
        topic : actionElement.attributes.getNamedItem("topic").nodeValue,
        effects : subActions};
  } else 
  if (actionElement.tagName=="updateTradeRule") {
    var subActions = [];
    for(var j = 0;j<actionElement.childNodes.length;j++) {
      var subActionElement = actionElement.childNodes[j];
      var subAction = null;
      if (subActionElement.tagName!=undefined) subAction = this.decodeRuleEffectFromXML(subActionElement);
      if (subAction!=null) subActions.push(subAction);
    }
    return {action : actionElement.tagName, 
        eventItem : actionElement.attributes.getNamedItem("event").nodeValue + "-" + actionElement.attributes.getNamedItem("item").nodeValue,
        effects : subActions};
  } else 
  if (actionElement.tagName=="storyState") {
    return {action : actionElement.tagName,
        scope : actionElement.attributes.getNamedItem("scope").nodeValue,
        variable : actionElement.attributes.getNamedItem("variable").nodeValue,
        value : actionElement.attributes.getNamedItem("value").nodeValue};
  } else 
  if (actionElement.tagName=="addLongTermWME") {
    return {action : actionElement.tagName,
        type : actionElement.attributes.getNamedItem("type").nodeValue,
        params : actionElement.attributes.getNamedItem("params").nodeValue};
  } else 
  if (actionElement.tagName=="addLongTermWMEToOthers") {
    return {action : actionElement.tagName,
        characterClass : actionElement.attributes.getNamedItem("class").nodeValue,
        select : actionElement.attributes.getNamedItem("select").nodeValue,
        type : actionElement.attributes.getNamedItem("type").nodeValue,
        params : actionElement.attributes.getNamedItem("params").nodeValue};
  } else 
  if (actionElement.tagName=="steal") {
    return {action : actionElement.tagName,
        itemName : actionElement.attributes.getNamedItem("name").nodeValue};
  } else 
  if (actionElement.tagName=="give") {
    var inventory = actionElement.attributes.getNamedItem("inventory");
    var newItem = actionElement.attributes.getNamedItem("new");
    if (inventory!=null) inventory = inventory.nodeValue;
    if (newItem!=null) newItem = newItem.nodeValue;
    return {action : actionElement.tagName,
        inventory : inventory,
        newItem : newItem};
  } else 
  if (actionElement.tagName=="sell") {
    var inventory = actionElement.attributes.getNamedItem("inventory");
    var newItem = actionElement.attributes.getNamedItem("new");
    if (inventory!=null) inventory = inventory.nodeValue;
    if (newItem!=null) newItem = newItem.nodeValue;
    return {action : actionElement.tagName,
        inventory : inventory,
        newItem : newItem};
  } else 
  if (actionElement.tagName=="drop") {
    var inventory = actionElement.attributes.getNamedItem("inventory");
    var newItem = actionElement.attributes.getNamedItem("new");
    if (inventory!=null) inventory = inventory.nodeValue;
    if (newItem!=null) newItem = newItem.nodeValue;
    return {action : actionElement.tagName,
        inventory : inventory,
        newItem : newItem};
  } else 
  if (actionElement.tagName=="loseItem") {
    var inventory = actionElement.attributes.getNamedItem("inventory");
    if (inventory!=null) inventory = inventory.nodeValue;
    return {action : actionElement.tagName,
        inventory : inventory};
  } else 
  if (actionElement.tagName=="gainItem") {
    var newItem = actionElement.attributes.getNamedItem("new");
    if (newItem!=null) newItem = newItem.nodeValue;
    return {action : actionElement.tagName,
        newItem : newItem};
  } else 
  if (actionElement.tagName=="experienceGain") {
    var xp = actionElement.attributes.getNamedItem("xp");
    if (xp!=null) xp = eval(xp.nodeValue);
    return {action : actionElement.tagName, xp : xp};
  } else 
  if (actionElement.tagName=="delay") {
    var tmp = eval(actionElement.attributes.getNamedItem("cycles").nodeValue);
    return {action : actionElement.tagName, cycles : tmp};
  } else 
  if (actionElement.tagName=="playSound") {
    var sound = actionElement.attributes.getNamedItem("sound").nodeValue;
    return {action : actionElement.tagName, sound : sound};
  } else 
  if (actionElement.tagName=="stopSound") {
    var sound = actionElement.attributes.getNamedItem("sound").nodeValue;
    return {action : actionElement.tagName, sound : sound};
  } else 
  if (actionElement.tagName=="if") {
    var conditionElement = actionElement.getElementsByTagName("condition");
    var thenElement = actionElement.getElementsByTagName("then");
    var elseElement = actionElement.getElementsByTagName("else");
    var subActionsCondition = [];
    var subActionsThen = [];
    var subActionsElse = [];
    if (conditionElement.length>0) {
      conditionElement = conditionElement[0];
      for(var j = 0;j<conditionElement.childNodes.length;j++) {
        var subActionElement = conditionElement.childNodes[j];
        var subAction = null;
        if (subActionElement.tagName!=undefined) subAction = this.decodeRuleEffectFromXML(subActionElement);
        if (subAction!=null) subActionsCondition.push(subAction);
      }
    }  
    if (thenElement.length>0) {
      thenElement = thenElement[0];
      for(var j = 0;j<thenElement.childNodes.length;j++) {
        var subActionElement = thenElement.childNodes[j];
        var subAction = null;
        if (subActionElement.tagName!=undefined) subAction = this.decodeRuleEffectFromXML(subActionElement);
        if (subAction!=null) subActionsThen.push(subAction);
      }
    }  
    if (elseElement.length>0) {
      elseElement = elseElement[0];
      for(var j = 0;j<elseElement.childNodes.length;j++) {
        var subActionElement = elseElement.childNodes[j];
        var subAction = null;
        if (subActionElement.tagName!=undefined) subAction = this.decodeRuleEffectFromXML(subActionElement);
        if (subAction!=null) subActionsElse.push(subAction);
      }
    }
    return {action: actionElement.tagName,
        conditionResult: undefined,
        conditionIP: 0, // these are "instruction pointers", to keep track of execution state
        thenIP: 0,
        elseIP: 0,
        conditionActions: subActionsCondition,
        thenActions: subActionsThen,
        elseActions: subActionsElse}
  }

  console.log("Error parsing XML action with tag name: " + actionElement.tagName);
  return null;
}



// Code to execute a given action:
/*
Note: this code is awful, I should change this in some future version to eliminate the
long chain of if-then-else. But for now, it's a quick'n'dirty code that works fine, since
the number of actions actually executed is very low...
*/ 

function executeRuleEffect(action, character, map, game, otherCharacter) {
  if (action.action=="gameComplete") {
    console.log("gameComplete executed on " + game);
    if (game.gameComplete==0) game.gameComplete = 1;
    return true;
  }
  if (action.action=="addBehavior") {
    var tmp = {priority: action.priority, id:action.id, behavior: eval("new " + action.behaviorName)};
    var bname = action.behaviorName.split("(")[0];
    tmp.className = bname;
    character.AI.rules.push(tmp);
    return true;
  }
  if (action.action=="removeBehavior") {
      for(var i = 0;i<character.AI.rules.length;i++) {
        var b = character.AI.rules[i];
        if (b.id==action.id) {
          character.AI.rules.splice(i,1);
          return true;
        }
      }
    return false;     
  }
  if (action.action=="teleport") {
    console.log(character.layer);
    if (action.map==null) {
      game.warpObject(character, action.x, action.y, character.map, character.layer);
      return true;
    } else {
      game.warpObject(character, action.x, action.y, action.map, character.layer);
      return true;
    }
  }
  if (action.action=="goTo") {
    var b = null;
    if (action.map==null) {
      if (character.x==action.x && character.y==action.y) return true;
      b = new BRGoTo(action.x,action.y,character.map);
    } else {
      if (character.x==action.x && character.y==action.y && character.map==action.map) return true;
      b = new BRGoTo(action.x,action.y,action.map);
    }
    var action = b.update(character, map, character.time, game);
    if (action!=null) character.sendAction(action,map,game);
    return undefined;
  }
  if (action.action=="die") {
    character.hp = 0;
    return true;
  }
  if (action.action=="activateLever") {
    var b = null;
    if (action.map==null) {
      if (character.x==action.x && character.y==action.y) {
        character.sendAction({type:ACTION_TAKE},map,game);
        return true;
      }
      b = new BRGoTo(action.x,action.y,character.map);
    } else {
      if (character.x==action.x && character.y==action.y && character.map==action.map) {
        character.sendAction({type:ACTION_TAKE},map,game);
        return true;
      }
      b = new BRGoTo(action.x,action.y,action.map);
    }
    var action = b.update(character, map, character.time, game);
    if (action!=null) character.sendAction(action,map,game);
    return undefined;
  }
  if (action.action=="openDoor") {
    var doors = map.getDoorsWithID(action.door);
//    console.log("Found " + doors.length + " doors to operate with ID " + this.name);
    for(var i = 0;i<doors.length;i++) {
      var door = doors[i];
      if (door.state) {
        door.setStateNonPropagating(false,map);
      } else {
        door.setStateNonPropagating(true,map);
      }
    }
  }
  if (action.action=="message") {
    game.pushMessage(action.text);
    return true;
  }
  if (action.action=="talk") {
    if (action.IP<action.subactions.length) {
      var subaction = action.subactions[action.IP];
      var retVal = executeRuleEffect(subaction, character, map, game, otherCharacter);
      // when "executeRuleEffect" returns "undefined", it means that the action has not completed execution, so we have to keep it:
      if (retVal!=undefined) action.IP++;
      if (action.IP>=action.subactions.length) {
        character.sendAction({type:ACTION_TALK, 
                    performative:{type:action.text, target:otherCharacter}}, 
                map, game);
        return true;
      }
      return undefined;
    } else {
      character.sendAction({type:ACTION_TALK, 
                  performative:{type:action.text, target:otherCharacter}}, 
              map, game);
      return true;
    }
  } 
  if (action.action=="pendingTalk") {
    var pt = null;
    for(var i = 0;i<character.pendingTalk.length;i++) {
      if (character.pendingTalk[i].character == action.character) {
        pt = character.pendingTalk[i];
        break;
      }
    }
    if (pt==null) {
      pt = {character:action.character, list:[]};
      character.pendingTalk.push(pt);
    }
    pt.list.push({action:"talk",
            text : action.text,
            IP: 0,
            subactions: action.subactions});
    return true;
  }
  if (action.action=="addTopic") {
    // if character==null, it means that the script is executed form a map or from the game itself:
    // otherwise, if it's from a character, we only want to add the topic if one of the characters is a player
    if (character==null ||      
        (character instanceof PlayerCharacter) ||
        (otherCharacter instanceof PlayerCharacter)) {
        game.addConversationTopic([action.topic, action.text]);
    }
    return true;
  } 
  if (action.action=="updateConversationRule") {
    character.conversationKnowledge[action.topic] = action.effects;
    return true;
  }
  if (action.action=="updateTradeRule") {
    character.conversationKnowledge[action.eventItem] = action.effects;
    return true;
  }
  if (action.action=="storyState") {
    if (action.scope=="game") {
      game.storyState[action.variable] = action.value;
      game.storyStateLastCycleUpdated = game.cycle;
    } else if (action.scope=="map") {
      map.storyState[action.variable] = action.value;
      map.storyStateLastCycleUpdated = game.cycle;
    } else if (action.scope=="character") {
      character.storyState[action.variable] = action.value;
      character.storyStateLastCycleUpdated = game.cycle;
    } else {
      console.log("scope of storyState is invalid (should be game, map or character): " + action.scope);
    }
    return true;
  }
  if (action.action=="addLongTermWME") {
    character.AI.longTermMemory.push({type:action.type, params:action.params, time:character.time});
    return true;
  }
  if (action.action=="addLongTermWMEToOthers") {
    var objectwmes = character.AI.getAllWMEs("object");
    for(var i = 0;i<objectwmes.length;i++) {
      var wme = objectwmes[i];
      if (eval("wme.params instanceof " + action.characterClass)) {
        var target = wme.params;
        target.AI.longTermMemory.push({type:action.type, params:action.params, time:character.time});
        if (action.select=="first") break;
      }
    }
    return true;
  } 
  if (action.action=="steal") {
    var found = -1;
    for(var j = 0;j<otherCharacter.inventory.length;j++) {
      var item = otherCharacter.inventory[j];
      if (item.name==action.itemName) {
        found = j;
      }
    }
    if (found>=0) {
      var item = otherCharacter.inventory[found];
      otherCharacter.inventory.splice(found,1);
      character.inventory.push(item);
      return true;
    }
    return false;
  } 
  if (action.action=="give") {
    if (action.inventory!=null) {
      for(var i = 0;i<character.inventory.length;i++) {
        if (character.inventory[i].name==action.inventory) {
          var item = character.inventory[i];
          character.inventory.splice(i,1);
          if (otherCharacter.inventory.length<INVENTORY_SIZE) {
            otherCharacter.inventory.push(item);
          } else {
            game.warpObject(item, otherCharacter.x, otherCharacter.y, otherCharacter.map, 1);
          }
          // trigger "give" script in character:
          var eventItem = "give-" + item.name;
          var actions = character.tradeRules[eventItem];
          if (actions!=null) {
            if (character.actionsToExecute==null) character.actionsToExecute = [];
            character.actionsToExecute = character.actionsToExecute.concat(actions);
          }

          // trigger "receive" script in otherCharacter:
          eventItem = "receive-" + item.name;
          var actions = otherCharacter.tradeRules[eventItem];
          if (actions!=null) {
            if (otherCharacter.actionsToExecute==null) otherCharacter.actionsToExecute = [];
            otherCharacter.actionsToExecute = otherCharacter.actionsToExecute.concat(actions);
          }

          return true;
        }
      }
      return false;
    } 
    if (action.newItem!=null) {
      var item = eval(action.newItem);
      if (otherCharacter.inventory.length<INVENTORY_SIZE) {
          otherCharacter.inventory.push(item);
      } else {
          game.warpObject(item, otherCharacter.x, otherCharacter.y, otherCharacter.map, 1);
      }
      // trigger "give" script in character:
      var eventItem = "give-" + item.name;
      var actions = character.tradeRules[eventItem];
      if (actions!=null) {
        if (character.actionsToExecute==null) character.actionsToExecute = [];
        character.actionsToExecute = character.actionsToExecute.concat(actions);
      }

      // trigger "receive" script in otherCharacter:
      eventItem = "receive-" + item.name;
      var actions = otherCharacter.tradeRules[eventItem];
      if (actions!=null) {
        if (otherCharacter.actionsToExecute==null) otherCharacter.actionsToExecute = [];
        otherCharacter.actionsToExecute = otherCharacter.actionsToExecute.concat(actions);
      }
      return true;
    }
    return false;
  }
  if (action.action=="sell") {
    if (action.inventory!=null) {
      for(var i = 0;i<character.inventory.length;i++) {
        if (character.inventory[i].name==action.inventory) {
          var item = character.inventory[i];
          var price = item.value;
          if (otherCharacter.gold>=price) {
            if (otherCharacter.inventory.length<INVENTORY_SIZE) {
              character.inventory.splice(i,1);
              otherCharacter.gold-=price;
              character.gold+=price;
              otherCharacter.inventory.push(item);
              // trigger "sell" script in character:
              var eventItem = "sell-" + item.name;
              var actions = character.tradeRules[eventItem];
              if (actions!=null) {
                if (character.actionsToExecute==null) character.actionsToExecute = [];
                character.actionsToExecute = character.actionsToExecute.concat(actions);
              }

              // trigger "receive" script in otherCharacter:
              eventItem = "receive-" + item.name;
              var actions = otherCharacter.tradeRules[eventItem];
              if (actions!=null) {
                if (otherCharacter.actionsToExecute==null) otherCharacter.actionsToExecute = [];
                otherCharacter.actionsToExecute = otherCharacter.actionsToExecute.concat(actions);
              }
              return true;
            } else {
              return false;
            }
          }
        }
      }
      return false;
    } 
    if (action.newItem!=null && otherCharacter.gold>=price) {
      var item = eval(action.newItem);
      var price = action.newItem.value;
      if (otherCharacter.gold>=price) {
        if (otherCharacter.inventory.length<INVENTORY_SIZE) {
          otherCharacter.gold-=price;
          character.gold+=price;
          otherCharacter.inventory.push(item);
          // trigger "sell" script in character:
          var eventItem = "sell-" + item.name;
          var actions = character.tradeRules[eventItem];
          if (actions!=null) {
            if (character.actionsToExecute==null) character.actionsToExecute = [];
            character.actionsToExecute = character.actionsToExecute.concat(actions);
          }

          // trigger "receive" script in otherCharacter:
          eventItem = "receive-" + item.name;
          var actions = otherCharacter.tradeRules[eventItem];
          if (actions!=null) {
            if (otherCharacter.actionsToExecute==null) otherCharacter.actionsToExecute = [];
            otherCharacter.actionsToExecute = otherCharacter.actionsToExecute.concat(actions);
          }
          return true;
        } else {
          return false;
        }
      }
    }
    return false;
  }

  if (action.action=="drop") {
    if (action.inventory!=null) {
      for(var i = 0;i<character.inventory.length;i++) {
        if (character.inventory[i].name==action.inventory) {
          var item = character.inventory[i];
          character.inventory.splice(i,1);
          game.warpObject(item, character.x, character.y, character.map, 1);
          return true;
        }
      }
      return false;
    } 
    if (action.newItem!=null) {
      game.warpObject(eval(action.newItem), character.x, character.y, character.map, 1);
      return true;
    }
    return false;
  }   
  if (action.action=="loseItem") {
    for(var i = 0;i<character.inventory.length;i++) {
      if (character.inventory[i].name==action.inventory) {
        var item = character.inventory[i];
        character.inventory.splice(i,1);
        return true;
      }
    }
    return false;
  }   
  if (action.action=="gainItem") {
    if (action.newItem!=null && character.inventory.length<INVENTORY_SIZE) {
      character.inventory.push(eval(action.newItem));
      return true;
    }
    return false;
  }   
  if (action.action=="experienceGain") {
    if (character instanceof PlayerCharacter) {
      character.experienceGain(action.xp,map);
      return true;
    } else {
      return false;
    }
  }
  if (action.action=="delay") {
    if (action.startTime == undefined) action.startTime = game.cycle;
    console.log("delay, still  " + (action.cycles - (game.cycle - action.startTime)) +  " cycles left (of " + action.cycles + ")");
    if ((game.cycle - action.startTime)>=action.cycles) {
      // reset the action:
      action.startTime = undefined;
      return true;
    }
    return undefined;
  }
  if (action.action=="playSound") {
    var s = game.loadAudioFile(action.sound);
    if (s.duration) s.currentTime=0;
    s.play();
    return true;
  }
  if (action.action=="stopSound") {
    var s = game.loadAudioFile(action.sound);
    s.stop();
    return true;
  }
  if (action.action=="if") {
    if (action.conditionResult == undefined) {
      var action2 = action.conditionActions[action.conditionIP];
      var retVal = executeRuleEffect(action2, character, map, game, otherCharacter);
      // when "executeRuleEffect" returns "undefined", it means that the action has not completed execution, so we have to keep it:
      if (retVal!=undefined) action.conditionIP++;
      if (action.conditionIP>=action.conditionActions.length) action.conditionResult = retVal;
      return undefined;
    } else if (action.conditionResult == true) {
      var action2 = action.thenActions[action.thenIP];
      var retVal = executeRuleEffect(action2, character, map, game, otherCharacter);
      // when "executeRuleEffect" returns "undefined", it means that the action has not completed execution, so we have to keep it:
      if (retVal!=undefined) action.thenIP++;
      if (action.thenIP>=action.thenActions.length) {
        // reset the action:
        action.conditionResult = undefined;
        action.conditionIP = 0;
        action.thenIP = 0;
        action.elseIP = 0;
        return retVal;
      }
      return undefined;
    } else {
      var action2 = action.elseActions[action.elseIP];
      var retVal = executeRuleEffect(action2, character, map, game, otherCharacter);
      // when "executeRuleEffect" returns "undefined", it means that the action has not completed execution, so we have to keep it:
      if (retVal!=undefined) action.elseIP++;
      if (action.elseIP>=action.elseActions.length) {
        // reset the action:
        action.conditionResult = undefined;
        action.conditionIP = 0;
        action.thenIP = 0;
        action.elseIP = 0;
        return retVal;
      }
      return undefined;
    }
    return true;
  }
  return false;
} 


function initAIRules(object) {
  object.onStart = [];
  object.onEnd = [];
  object.storyState = {};
  object.storyStateRules = [];
  object.eventRules = {};
  object.timerEventRules = [];
  object.storyStateLastCycleUpdated = -1;
  object.storyStateRulesLastCycleChecked = -1;
  object.actionsToExecute = [];  // when rules get triggered, they push their actions into this queue  
}

function parseAIRules(object, xml) {

  // story state rules:
  var storystateElements = xml.getElementsByTagName("storyStateRule");
  for(var i = 0;i<storystateElements.length;i++) {
    var storystateElement = storystateElements[i];
    if (storystateElement.parentNode!=xml) continue;
    var actions = [];
    for(var j = 0;j<storystateElement.childNodes.length;j++) {
      var actionElement = storystateElement.childNodes[j];
      var action = null;
      if (actionElement.tagName!=undefined) action = decodeRuleEffectFromXML(actionElement);
      if (action!=null) actions.push(action);
    }
    var once = false;
    var tmp = storystateElement.attributes.getNamedItem("once");
    if (tmp!=null) once = eval(tmp.nodeValue);
    object.storyStateRules.push({scope : storystateElement.attributes.getNamedItem("scope").nodeValue,
                   variable : storystateElement.attributes.getNamedItem("variable").nodeValue,
                   value : storystateElement.attributes.getNamedItem("value").nodeValue,
                   once : once,
                   triggered : false,
                   actions: actions});
  }

  // event rules:
  var eventRuleElements = xml.getElementsByTagName("eventRule");
  for(var i = 0;i<eventRuleElements.length;i++) {
    var eventRuleElement = eventRuleElements[i];
    if (eventRuleElement.parentNode!=xml) continue;
    var actions = [];
    for(var j = 0;j<eventRuleElement.childNodes.length;j++) {
      var actionElement = eventRuleElement.childNodes[j];
      var action = null;
      if (actionElement.tagName!=undefined) action = decodeRuleEffectFromXML(actionElement);
      if (action!=null) actions.push(action);
    }
    var e = eventRuleElement.attributes.getNamedItem("event").nodeValue;
    var once = false;
    var tmp = eventRuleElement.attributes.getNamedItem("once");
    if (tmp!=null) once = eval(tmp.nodeValue);
    if (e==EVENT_TIMER) {
      var time = eval(eventRuleElement.attributes.getNamedItem("time").nodeValue);
      var period = undefined;
      if (eventRuleElement.attributes.getNamedItem("period")!=null) period = eval(eventRuleElement.attributes.getNamedItem("period").nodeValue);
      object.timerEventRules.push({time: time, period: period, triggered: false, actions:actions});
    } else {
      object.eventRules[e] = {once : once,
                              triggered : false,
                              actions: actions};
    }
  }

  // things to execute at the start/end:
  var onStartElements = xml.getElementsByTagName("onStart");
  for(var i = 0;i<onStartElements.length;i++) {
    var onStartElement = onStartElements[i];
    if (onStartElement.parentNode!=xml) continue;
    for(var j = 0;j<onStartElement.childNodes.length;j++) {
      var actionElement = onStartElement.childNodes[j];
      var action = null;
      if (actionElement.tagName!=undefined) action = decodeRuleEffectFromXML(actionElement);
      if (action!=null) object.onStart.push(action);
    }
  }
  console.log("after loading object " + object.name + " onStart is of size: " + object.onStart.length);
  var onEndElements = xml.getElementsByTagName("onEnd");
  for(var i = 0;i<onEndElements.length;i++) {
    var onEndElement = onEndElements[i];
    if (onEndElement.parentNode!=xml) continue;
    for(var j = 0;j<onEndElement.childNodes.length;j++) {
      var actionElement = onEndElement.childNodes[j];
      var action = null;
      if (actionElement.tagName!=undefined) action = decodeRuleEffectFromXML(actionElement);
      if (action!=null) object.onEnd.push(action);
    }
  }
}


function fireEventRule(object, event) {
  var rule = object.eventRules[event];
//  console.log("fireEventRule '" + event + "' for " + object.name);
  if (rule!=null) {
    //console.log("  there is a rule!");
    if (!rule.once || !rule.triggered) {
//      console.log("  fired! (" + rule.actions.length + " actions)");
      rule.triggered = true;
      for(var j = 0;j<rule.actions.length;j++) {
        object.actionsToExecute.push(rule.actions[j]);
      }
    }
  }
}


