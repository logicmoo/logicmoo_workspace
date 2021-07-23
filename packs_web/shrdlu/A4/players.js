function createCharacters(playerStartingPositions, game) {
	var characters = new Array();
	for(var i = 0;i<playerStartingPositions.length;i++) {		
		var tmp = eval("new " + playerStartingPositions[i].type + "()")
//		console.log("Player created, with inventory of size: " + tmp.inventory.length);
		Game.warpObject(tmp,playerStartingPositions[i].x, playerStartingPositions[i].y, playerStartingPositions[i].map, 2);
		characters.push(tmp);
//		console.log("class name:" + tmp.getClassName());
	}
	return characters;
}


PlayerCharacter.prototype = new Character();
function PlayerCharacter() {
	this.name = "PlayerCharacter";
	this.gold = 0;
	this.level = 1;
	this.experience = 0;
	this.experienceToLevelUp = 10;
	this.levelupadd = {hp:"4", sp:"2", attack:"1", defense:"1"};
	this.levelupmultiply = {experienceToLevelUp:"1.5"};
	this.hp = this.max_hp = 10;
	this.mp = this.max_mp = 10;
	this.attack = 4;
	this.defense = 0;
	this.attack_modifier = 1.0;
	this.defense_modifier = 1.0;
	this.map = 0;
	this.inventory = new Array();
	this.spells = new Array();
	this.player_character = true;
	this.equippedItems = [null,null,null];
	this.currentAnimation = null;
	this.scriptsToExecute = [];
	this.tradeRules = {};

	this.generateSaveData = function(objects, data) {
		data = PlayerCharacter.prototype.generateSaveData.call(this,objects,data);

		data.scriptsToExecute = this.scriptsToExecute;		

		return data;
	}



	this.decodeCharacterFromXML = function(reset) {
		PlayerCharacter.prototype.decodeCharacterFromXML.call(this,reset);
		if (reset) initAIRules(this);
		parseAIRules(this,this.xml);
	}	

	this.getClassName = function() {
		return "PlayerCharacter";
	}

	this.walkable = function () {
		return false;
	};

	this.experienceGain = function(amount, map) {
		console.log("experience gain: " + amount);
		this.experience += amount;
//		console.log("experience: " + this.experience);
		// check for level up!
//		if (this.level<MAX_LEVEL) {
//			while(this.experience>=LEVEL_EXPERIENCE[this.level-1]) {
//				this.experience-=LEVEL_EXPERIENCE[this.level-1];
			while(this.experience>=this.experienceToLevelUp) {
				this.experience-=this.experienceToLevelUp;
//				console.log("experience: " + this.experience);
				this.levelUp(map);
			}
//		}
	}

	this.levelUp = function(map) {
		this.level++;
		// increase HP, MP, etc.
		/*
		if (this.max_mp == 0) {
			this.max_hp += 4;
			this.hp += Math.max(4,this.max_hp/2);
			if (this.hp>=this.max_hp) this.hp = this.max_hp;
		} else {
			this.max_hp += 3;
			this.max_mp += 2;
			this.hp += 3;
			this.mp = this.max_mp;
			this.hp += Math.max(3,this.max_hp/2);
			if (this.hp>=this.max_hp) this.hp = this.max_hp;
		}
		this.attack++;
		this.defense++;
		*/
		for(var key in this.levelupadd) {
			var v = parseStringValue(this.levelupadd[key]);
			if (key=="hp") {
				this.max_hp+=v;
				this.hp += Math.floor(this.max_hp/2);
				if (this.hp>=this.max_hp) this.hp = this.max_hp;
			} else if (key=="mp") {
				this.max_mp+=v;
				this.hp += Math.floor(this.max_hp/2);
				if (this.mp>=this.max_mp) this.mp = this.max_mp;
			} else {
				this[key]+=v;
			}
		}
		for(var key in this.levelupmultiply) {
			this[key] *= parseStringValue(this.levelupmultiply[key]);

			var v = parseStringValue(this.levelupmultiply[key]);
			if (key=="hp") {
				this.max_hp = Math.floor(this.max_hp*v);
				this.hp += Math.floor(this.max_hp/2);
				if (this.hp>=this.max_hp) this.hp = this.max_hp;
			} else if (key=="mp") {
				this.max_mp = Math.floor(this.max_mp*v);;
				this.mp += Math.floor(this.max_mp/2);
				if (this.mp>=this.max_mp) this.mp = this.max_mp;
			} else {
				this[key] = Math.floor(this[key]*v);;
			}
		}
		map.pushTextOverlay("Level up!", this.x, this.y, 255,255,0);
	}

	this.update = function(character, map, game) {
		if (this.onStart.length>0) {
			var retVal = executeRuleEffect(this.onStart[0], this, map, game, null);
			if (retVal!=undefined) this.onStart.splice(0,1);
		}

		var old_state = this.state;

		var ret_val = PlayerCharacter.prototype.update.call(this, character, map, game);

		if (this.state==0 && old_state==1) {
			// check for messages:
			var msg = map.getMessage(this.x,this.y);
			if (msg!=null) {
				if (msg.bubble) {
					map.pushTextBubble(msg.text, this, 200);
				}
				game.pushMessage(msg.text);
				if (!msg.repeat) map.removeMessage(msg);
				if (msg.topic!=null) {
					game.conversationTopics.push([msg.topic,msg.topictext]);
				}
			}			
			// check for triggers:
			var trigger = map.getTrigger(this.x,this.y);
			if (trigger!=null) {
				for(var i = 0;i<trigger.script.length;i++) {
					this.scriptsToExecute.push(trigger.script[i]);
				}
				if (!trigger.repeat) map.removeTrigger(trigger);
			}			
		}

	    if(this.scriptsToExecute.length>0) {
	    	var retVal = executeRuleEffect(this.scriptsToExecute[0], this, map, game, null);
	    	if (retVal!=undefined) this.scriptsToExecute.splice(0,1);
	    }

		return ret_val;
	}


	// Given another 'character' returns the available set of talk performatives to interact with him/her
	this.availableTalkPerformatives = function(character, map, game) {
		var performatives = [];
		performatives.push({type:TALK_HI, target:character});
		performatives.push({type:TALK_BYE, target:character});

		// No/yes should only be available if the character just asked a question:
		// ...
//		performatives.push({type:TALK_YES, target:character});
//		performatives.push({type:TALK_NO, target:character});

		performatives.push({type:TALK_TRADE, target:character});

		for(var i=0;i<game.conversationTopics.length;i++) {
			performatives.push({type:TALK_ASK, topic:game.conversationTopics[i][0], text:game.conversationTopics[i][1], target:character});
		}

//		performatives.push({type:"inform"???, target:character});

		return performatives;
	}

}
