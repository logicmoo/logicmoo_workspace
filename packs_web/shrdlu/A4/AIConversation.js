//
// Conversation Engine: -------------------------------------------------------------------------------------
//

var CONVERSATION_STATE_NONE = 0;
var CONVERSATION_STATE_RECEIVED_HI = 1;
var CONVERSATION_STATE_SENT_HI = 2;
var CONVERSATION_STATE_IN_CONVERSATION = 3;
var CONVERSATION_STATE_IN_CONVERSATION_ONLY_HI = 4;	// only "HI" messages have been sent, so conversation can end any time
var CONVERSATION_STATE_IN_CONVERSATION_TALKING = 5;
var CONVERSATION_STATE_IN_TRADE = 6;
var CONVERSATION_STATE_RECEIVED_BYE = 7;
var CONVERSATION_STATE_SENT_BYE = 8;
var CONVERSATION_STATE_ANNOYED = 9;

var MAX_CONVERSATION_DISTANCE = 5;


// This creates a conversation engine between the characters "c1" and "c2"
function ConversationEngine(c1, c2) {
	this.state = CONVERSATION_STATE_NONE;
	this.state_timer = 0;
	this.previous_state = CONVERSATION_STATE_NONE;
	this.character = c1;
	this.otherCharacter = c2;
	this.messageToProcess = null;
	this.conversationStartTime = c1.time;	// so that the charcters can know how long they have been talking.
	this.actionsToExecute = null;	// this is the list of actions that the character wants to execute as soon as possible (e.g. 
									// responses to a question by the player, etc.)

	this.update = function(message, map, game) {	
		var tmp_state = this.state;
		var predisposition = 0;
		var oneMoreCycle = false;

		if (this.character.talk_state!=STATE_READY) {
			// the character is doing something, hold on
			if (message!=null) this.messageToProcess = message;
			return true;
		}

		if (this.character.AI.WMEexists2("friendly",this.otherCharacter.getClassName())) predisposition = 1;
		else if (this.character.AI.WMEexists2("unfriendly",this.otherCharacter.getClassName())) predisposition = -1;

		// characters would not talk to unfriendly characters
		if (predisposition==-1) {
//			console.log(this.character.name + " is ignoring the message from " + this.otherCharacter.name + " because the later is unfriendly");
			return false;
		}

		if (message==null && this.messageToProcess!=null) {
			message = this.messageToProcess;
			this.messageToProcess = null;
		}

		var dx = Math.abs(this.character.x - this.otherCharacter.x);
		var dy = Math.abs(this.character.y - this.otherCharacter.y);
		var d = dx*dx + dy*dy;
		if (this.character.map != this.otherCharacter.map) d = MAX_CONVERSATION_DISTANCE*MAX_CONVERSATION_DISTANCE*10;

		if (this.actionsToExecute!=null && this.actionsToExecute.length>0) {
			// actions to execute:
			var action = this.actionsToExecute[0];
			var retVal = executeRuleEffect(action, this.character, map, game, this.otherCharacter);
			// when "executeRuleEffect" returns "undefined", it means that the action has not completed execution, so we have to keep it:
			if (retVal!=undefined) this.actionsToExecute.splice(0,1);
		}

		switch(this.state) {
			case CONVERSATION_STATE_NONE:
				if (message!=null) {
					if (message.performative.type==TALK_HI) {
						// good start! 
						this.state = CONVERSATION_STATE_RECEIVED_HI;
					} else if (message.performative.type==TALK_BYE) {
						this.state = CONVERSATION_STATE_RECEIVED_BYE;
					} else {
						if (predisposition==1 || this.character.picky==false) {
							// handle the message like if it was "in_conversation"
							this.state = CONVERSATION_STATE_IN_CONVERSATION;
							this.messageToProcess = message;
						} else {
							// get annoyed (characters expect to start with "hi"):
							this.state = CONVERSATION_STATE_ANNOYED;
							this.messageToProcess = message;
						}
					}
				}
				break;

			case CONVERSATION_STATE_RECEIVED_HI:
				if (message==null) {
					this.character.sendAction({type:ACTION_TALK, 
											   performative:{type:TALK_HI, target:this.otherCharacter}}, 
											   map, game);
					this.state = CONVERSATION_STATE_IN_CONVERSATION_ONLY_HI;
				} else {
					// get annoyed, he did not have time to reply with a hi!:
					this.state = CONVERSATION_STATE_ANNOYED;
					this.messageToProcess = message;
				}
				break;

			case CONVERSATION_STATE_SENT_HI:
				if (message!=null) {
					if (message.performative.type==TALK_HI) {
						this.state = CONVERSATION_STATE_IN_CONVERSATION_ONLY_HI;
					} else {
						this.state = CONVERSATION_STATE_ANNOYED;
						this.messageToProcess = message;
					}
				} else {
					if (this.state_timer>500 || d>5*5) {
						this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
						this.character.sendAction({type:ACTION_TALK_ANGRY, 
												   performative:{type:"Some people don't reply, so unpolite!", target:this.otherCharacter}}, 
												   map, game);
						return false;
					}
				}				
				break;

			// TRADE and CONVERSATION are the same state, except that characters do not lose patience while trading:
			case CONVERSATION_STATE_IN_CONVERSATION:
				if (this.state_timer>750) {
					// time out
					if (this.character.picky) {
						this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
						this.character.sendAction({type:ACTION_TALK_ANGRY, 
												   performative:{type:"I thought we were talking!", target:this.otherCharacter}}, 
												   map, game);
					}
					this.messageToProcess = message;
					return false;
				}
				// Notice that there is NO break statement
			case CONVERSATION_STATE_IN_CONVERSATION_ONLY_HI:
			case CONVERSATION_STATE_IN_TRADE:
				if (message!=null) {
					if (message.performative.type==TALK_BYE) {
						this.state = CONVERSATION_STATE_RECEIVED_BYE;
					} else if (message.performative.type==TALK_ASK) {
						var topic = message.performative.topic;
						var otherCharacterName = this.otherCharacter.getClassName();
						var answer = this.character.conversationKnowledge[topic];

						// check to see if this was already asked before:
						var wme = this.character.AI.getWME2("conversationAnswered",topic + "-" + otherCharacterName);

						// remember this:
						this.character.AI.addShortTermWME({type:"conversationAnswered", params:topic + "-" + otherCharacterName}, this.character.time, 2000);
						if (wme!=null) {
							this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
							this.character.sendAction({type:ACTION_TALK_ANGRY, 
													   performative:{type:"I already told you about that!", target:this.otherCharacter}}, 
													   map, game);
						} else {
							if (answer==null) {
								this.character.sendAction({type:ACTION_TALK, 
														   performative:{type:"I am sorry, I don't know anything about that.", target:this.otherCharacter}}, 
														   map, game);							
							} else {
								console.log("processing answer with " + answer.length + " actions");
								if (this.actionsToExecute==null) this.actionsToExecute = [];
								this.actionsToExecute = this.actionsToExecute.concat(answer);
							}
						}
						this.state = CONVERSATION_STATE_IN_CONVERSATION_TALKING;
					} else if (message.performative.type==TALK_TRADE) {
				      game.pushMenu(new TradeDialogue(-HUD_WIDTH/2,0,15,this.otherCharacter,this.character,map));
				      this.state = CONVERSATION_STATE_IN_TRADE;
					} else if (message.performative.type==TALK_HI) {
						if (this.character.picky) {
							this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
							this.character.sendAction({type:ACTION_TALK_ANGRY, 
													   performative:{type:"Yeah, we already went through that...", target:this.otherCharacter}}, 
													   map, game);
						}
						this.state = CONVERSATION_STATE_IN_CONVERSATION_TALKING;
					}
				} else {
					if (d>MAX_CONVERSATION_DISTANCE*MAX_CONVERSATION_DISTANCE) {
						if (this.state == CONVERSATION_STATE_IN_CONVERSATION_ONLY_HI) {
							return false;
						} else {	
							if (this.character.picky) {					
								this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
								this.character.sendAction({type:ACTION_TALK_ANGRY, 
														   performative:{type:"At least you could say good bye! So unpolite!", target:this.otherCharacter}}, 
														   map, game);
							}
							return false;
						}
					}
				}
				break;

			case CONVERSATION_STATE_IN_CONVERSATION_TALKING:
				if (this.character.talk_state == STATE_READY) {
					this.state = CONVERSATION_STATE_IN_CONVERSATION;
				}
				break;

			case CONVERSATION_STATE_RECEIVED_BYE:
				if (message==null) {
					this.character.sendAction({type:ACTION_TALK, 
											   performative:{type:TALK_BYE, target:this.otherCharacter}}, 
											   map, game);
					return false;
				} else {
					// get annoyed, he did not have time to reply with a bye!:
					this.state = CONVERSATION_STATE_ANNOYED;
					this.messageToProcess = message;
				}
				break;

			case CONVERSATION_STATE_SENT_BYE:
				if (message!=null) {
					if (message.performative.type==TALK_BYE) {
						return false;
					} else {
						this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
						this.character.sendAction({type:ACTION_TALK_ANGRY, 
												   performative:{type:"As I said... farewell!", target:this.otherCharacter}}, 
												   map, game);
						return false;						
					}
				} else {
					if (this.state_timer>500 || d>5*5) {
						if (this.character.picky) {
							this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
							this.character.sendAction({type:ACTION_TALK_ANGRY, 
													   performative:{type:"At least you could say good bye! So unpolite!", target:this.otherCharacter}}, 
													   map, game);
						}
						return false;
					}
				}
				break;

			case CONVERSATION_STATE_ANNOYED:
				this.character.AI.addShortTermWME({type:"angry", params:null}, this.character.time, 100);
				if (this.previous_state == CONVERSATION_STATE_NONE ||
					this.previous_state == CONVERSATION_STATE_SENT_HI) {
					if (this.character.picky) {
						this.character.sendAction({type:ACTION_TALK_ANGRY, 
												   performative:{type:"That's no way to start a conversation...", target:this.otherCharacter}}, 
												   map, game);
					}
					if (message.performative.type==TALK_BYE) {
						this.messageToProcess = message;
						this.state = CONVERSATION_STATE_RECEIVED_BYE;
					} else {
						this.messageToProcess = message;
						this.state = CONVERSATION_STATE_IN_CONVERSATION;
					}
				} else if (this.previous_state == CONVERSATION_STATE_RECEIVED_HI) {
					if (this.character.picky) {
						this.character.sendAction({type:ACTION_TALK_ANGRY, 
												   performative:{type:"Let me talk!", target:this.otherCharacter}}, 
												   map, game);
					}
					this.messageToProcess = message;
					this.state = CONVERSATION_STATE_IN_CONVERSATION;					
				} else if (this.previous_state == CONVERSATION_STATE_RECEIVED_BYE) {
					if (this.character.picky) {
						this.character.sendAction({type:ACTION_TALK_ANGRY, 
												   performative:{type:"Let me talk!", target:this.otherCharacter}}, 
												   map, game);
					}
					this.messageToProcess = message;
					this.state = CONVERSATION_STATE_NONE;
				} else {
					this.character.sendAction({type:ACTION_TALK_ANGRY, 
											   performative:{type:"That is so unpolite...", target:this.otherCharacter}}, 
											   map, game);
					return false;
				}
				break;
		}
		if (this.state!=tmp_state) {
			this.previous_state = tmp_state;
			this.state_timer = 0;
		} else {
			this.state_timer++;
		}

		return true;
	}
}

