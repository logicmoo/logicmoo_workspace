AICharacter.prototype = new Character();
function AICharacter() {

	this.respawn = 0.0;
	this.picky = true;

	// This list is used to store all the messages received from other characters. They are stored in this list
	// as soon as the original character starts saying the message, and stay here until the text bubble disappears,
	// then they are processed.
	this.delayedMessages = null;

	// This variable holds a list of the "conversation engines", one per conversation the character is having. 
	// If the constructor of the character leaves this set fo null, the AI will assume this character cannot talk
	this.conversationEngines = null;

	// list of actions that will be executed as soon as the character is created and when the character dies:
	this.onStart = [];
	this.onEnd = [];

	// list of things this character wants to say to the other characters in the game:
	this.pendingTalk = [];

	this.storyState = {};	// the story state is where all the variables used by the scripts are stored
	this.storyStateLastCycleUpdated = -1;
	this.storyStateRules = [];
	this.tradeRules = {};
	this.conversationKnowledge = {};
	this.storyStateRulesLastCycleChecked = -1;
	this.actionsToExecute = [];	// when rules get triggered, they push their actions into this queue
	this.statusAnimation = null;
	this.statusAnimationName = null;

	this.generateSaveData = function(objects, data) {
		data = AICharacter.prototype.generateSaveData.call(this,objects,data);
		
		data.respawn = this.respawn;
		// data.delayedMessages = ???
		data.picky = this.picky;
		data.onStart = this.onStart;
		data.onEnd = this.onEnd;
		data.pendingTalk = this.pendingTalk;
		data.storyState = this.storyState;
		data.storyStateLastCycleUpdated = this.storyStateLastCycleUpdated;
		data.tradeRules = this.tradeRules;
		data.conversationKnowledge = this.conversationKnowledge;
		data.storyStateRulesLastCycleChecked = this.storyStateRulesLastCycleChecked;
		data.actionsToExecute = this.actionsToExecute;

		if (this.home!=undefined) data.home = this.home;
		if (this.respawnID!=undefined) data.respawnID = this.respawnID;
		if (this.respawnMap!=undefined) data.respawnMap = this.respawnMap;

		data.AI = this.AI.generateSaveData(objects, {});

		return data;
	}


	this.decodeCharacterFromXML = function(reset) {
		AICharacter.prototype.decodeCharacterFromXML.call(this,reset);

		if (reset) {
			this.AI = new HighLevelAI();
			this.conversationEngines = [];
			this.delayedMessages = [];
			this.conversationKnowledge = {};
			this.tradeRules = {};
			this.pendingTalk = [];
			initAIRules(this);
		}
		if (this.gold==undefined || this.gold==null) this.gold = 0;

		this.decodeCharacterAIRulesFromXML(this.xml);
	}

	this.decodeCharacterAIRulesFromXML = function(xml) {
		// conversation rules:
		var conversationElements = xml.getElementsByTagName("conversationRule");
//		console.log("xml: " + new XMLSerializer().serializeToString(xml));
//		console.log("additional conversations: " + conversationElements.length);
		for(var i = 0;i<conversationElements.length;i++) {
			var conversationElement = conversationElements[i];
			var actions = [];
			for(var j = 0;j<conversationElement.childNodes.length;j++) {
				var actionElement = conversationElement.childNodes[j];
				var action = null;
				if (actionElement.tagName!=undefined) action = decodeRuleEffectFromXML(actionElement);
				if (action!=null) actions.push(action);
			}
			this.conversationKnowledge[conversationElement.attributes.getNamedItem("topic").nodeValue] = actions;
		}

		// trade rules:
		var tradeElements = xml.getElementsByTagName("tradeRule");
		for(var i = 0;i<tradeElements.length;i++) {
			var tradeElement = tradeElements[i];
			var actions = [];
			for(var j = 0;j<tradeElement.childNodes.length;j++) {
				var actionElement = tradeElement.childNodes[j];
				var action = null;
				if (actionElement.tagName!=undefined) action = decodeRuleEffectFromXML(actionElement);
				if (action!=null) actions.push(action);
			}
			var eventItem = tradeElement.attributes.getNamedItem("event").nodeValue + "-" + 
						    tradeElement.attributes.getNamedItem("item").nodeValue;
			this.tradeRules[eventItem] = actions;
			console.log("tradeRule for '" + eventItem + "'");
		}

		parseAIRules(this,xml);

		var behaviorElements = xml.getElementsByTagName("behavior");
		for(var i = 0;i<behaviorElements.length;i++) {
			var behaviorElement = behaviorElements[i];
			var priority = parseInt(behaviorElement.attributes.getNamedItem("priority").nodeValue);
			var behaviorName = behaviorElement.textContent;
		    var id = behaviorElement.attributes.getNamedItem("id");
		    if (id!=null) id = id.nodeValue;
			var tmp = {priority: priority, id: id, behavior: eval("new " + behaviorName)};
			this.AI.rules.push(tmp);
		}		
	}	


	this.walkable = function () {
		return false;
	};

	this.draw = function(alpha,offsetx,offsety,game) {
		AICharacter.prototype.draw.call(this,alpha,offsetx,offsety,game);

		if (this.AI==null) return;

		var animationName = null;
		var bestActivation = 0;
		var curious = this.AI.getWME("curious");
		var scared = this.AI.getWME("scared");
		var angry = this.AI.getWME("angry");
		var tired = this.AI.getWME("tired");
		var happy = this.AI.getWME("happy");

		if (curious!=null) {
			animationName = STATUS_CURIOUS_ANIMATION;
			bestActivation = curious.activation;
		}
		if (scared!=null) {
			if (animationName==null || scared.activation>bestActivation) {
				animationName = STATUS_SCARED_ANIMATION;
				bestActivation = scared.activation;
			}
		}
		if (angry!=null) {
			if (animationName==null || angry.activation>bestActivation) {
				animationName = STATUS_ANGRY_ANIMATION;
				bestActivation = angry.activation;
			}
		}
		if (tired!=null) {
			if (animationName==null || tired.activation>bestActivation) {
				animationName = STATUS_TIRED_ANIMATION;
				bestActivation = tired.activation;
			}
		}
		if (happy!=null) {
			if (animationName==null || happy.activation>bestActivation) {
				animationName = STATUS_HAPPY_ANIMATION;
				bestActivation = happy.activation;
			}
		}

		if (animationName!=null) {
			if (this.statusAnimation==null || this.statusAnimationName!=animationName) {
				this.statusAnimation = game.defaultAnimations[animationName].clone();
				this.statusAnimationName = animationName;
				this.statusAnimation.reset();
			}
			var offs = 0;
			if (this.state==STATE_MOVING) offs = Math.floor(TILE_HEIGHT*(this.walk_speed-this.state_timer)/this.walk_speed);

			switch(this.direction) {
				case DIRECTION_UP: this.statusAnimation.draw(this.x*TILE_WIDTH + offsetx, this.y*TILE_HEIGHT + offs + offsety,alpha,game); break;
				case DIRECTION_RIGHT: this.statusAnimation.draw(this.x*TILE_WIDTH - offs + offsetx,this.y*TILE_HEIGHT + offsety,alpha,game); break;
				case DIRECTION_DOWN: this.statusAnimation.draw(this.x*TILE_WIDTH + offsetx,this.y*TILE_HEIGHT - offs + offsety,alpha,game); break;
				case DIRECTION_LEFT: this.statusAnimation.draw(this.x*TILE_WIDTH + offs + offsetx,this.y*TILE_HEIGHT + offsety,alpha,game); break;
			}
		} else {
			this.statusAnimation = null;
			this.statusAnimationName = null;
		}
	}	

	this.update = function(character, map, game) {
//		console.log("updating " + this.name + " onStart: " + this.onStart.length);
		if (this.onStart.length>0) {
			// console.log("executing on start of character: " + this.name);
			// console.log("   rule is: " + this.onStart[0].action);
			var retVal = executeRuleEffect(this.onStart[0], this, map, game, null);
			if (retVal!=undefined) this.onStart.splice(0,1);
		}

		// check for story state rules:
		if (game.storyStateLastCycleUpdated > this.storyStateRulesLastCycleChecked ||
			map.storyStateLastCycleUpdated > this.storyStateRulesLastCycleChecked ||
			this.storyStateLastCycleUpdated > this.storyStateRulesLastCycleChecked) {
			for(var i = 0;i<this.storyStateRules.length;i++) {
				var rule = this.storyStateRules[i];
				if (!rule.once || !rule.triggered) {
					var triggered = false;
					if (rule.scope=="game") {
						if (game.storyState[rule.variable] == rule.value) triggered = true;
					} else if (rule.scope=="map") {
						if (map.storyState[rule.variable] == rule.value) triggered = true;
					} else if (rule.scope=="character") {
						if (this.storyState[rule.variable] == rule.value) triggered = true;
					} else {
						console.log("storyStateRule has an incorrect scope (should be game, map, character): " + rule.scope);
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
		if(this.actionsToExecute.length>0) {
			var otherCharacter = null;
			if (this.conversationEngines!=null && this.conversationEngines.length>0) {
				otherCharacter = this.conversationEngines[0].otherCharacter;
			}
			var retVal = executeRuleEffect(this.actionsToExecute[0], this, map, game, otherCharacter);
			if (retVal!=undefined) this.actionsToExecute.splice(0,1);
		}

		if (this.state==STATE_READY) {
//			if (this.AI==undefined) console.log("No AI for character: " + this.name);
			this.AI.perceive(this, map, this.time, game);
			var action = this.AI.update(this, map, this.time, game);
			if (action!=null) {
				this.conversationEngines=[]; // clear all the conversation engines
				this.sendAction(action,map,game);
			}
		}
		this.time++;

		if (this.respawn>0.0) {
			var tmp = game.maps[this.respawnMap].respawnData[this.respawnID];
//			if (tmp == null) {
//				console.log("Respawn list: " + map.respawnData.length + " ID: " + this.respawnID + " class: " + this.getClassName());
//			}
			tmp.lastCycleAlive = game.cycle;
		}

		if (this.conversationEngines!=null) {
			var toDelete = [];
			var msg = this.updateDelayedMessages();
			var otherCharacter = null;
			if (msg!=null) {
//				console.log(this.name + " received a message from " + msg.character.name);
				otherCharacter = msg.character;
			}
			for(var idx = 0;idx<this.conversationEngines.length;idx++) {
				var ce = this.conversationEngines[idx];
				if (otherCharacter == ce.otherCharacter) {
					if (!ce.update(msg,map,game)) toDelete.push(ce);
					msg = null;
				} else {
					if (!ce.update(null,map,game)) toDelete.push(ce);
				}
			}
			if (msg!=null) {
				// this means the message was not sent to any ConversationEngine:
				var ce = new ConversationEngine(this, otherCharacter);
				this.conversationEngines.push(ce);
				if (!ce.update(msg,map,game)) toDelete.push(ce);
			}
			for(var i = 0;i<toDelete.length;i++) {
				this.conversationEngines.splice(this.conversationEngines.indexOf(toDelete[i]),1);
			}
		}

		if (this.statusAnimation!=null) {
			this.statusAnimation.update();
		}

		var retVal = AICharacter.prototype.update.call(this, character, map, game);
		if (retVal==false) {
			while(this.onEnd.length>0) {
				var retVal = executeRuleEffect(this.onEnd[0], this, map, game, null);
				if (retVal!=undefined) this.onEnd.splice(0,1);
			}
		}
		return retVal;
	}		

	this.getConversationEngineForCharacter = function(otherCharacter) {
		for(var idx = 0;idx<this.conversationEngines.length;idx++) {
			var ce = this.conversationEngines[idx];
			if (otherCharacter == ce.otherCharacter) return ce;
		}
		return null;
	}

	this.receiveDelayedMessage = function(performative, character, delay) {
//		console.log(this.name + " enqueing a delayed message from " + character.name);
		this.delayedMessages.push({performative:performative, character:character, delay:delay});
	}

	// notifies that trading is over with character 'character':
	this.endOfTrace = function(character) {
		for(var idx = 0;idx<this.conversationEngines.length;idx++) {
			var ce = this.conversationEngines[idx];
			if (ce.otherCharacter == character) {
				if (ce.state = CONVERSATION_STATE_IN_TRADE) {
					ce.state = CONVERSATION_STATE_IN_CONVERSATION;
					ce.state_timer = 0;
				}
			}
		}		
	}

	this.updateDelayedMessages = function() {
		for(var i = 0;i<this.delayedMessages.length;i++) {
			var msg = this.delayedMessages[i];
			msg.delay--;
			if (msg.delay<=0) {
				this.delayedMessages.splice(i,1);
				// receive message!
				return msg;
			}
		}
		return null;
	}
}
