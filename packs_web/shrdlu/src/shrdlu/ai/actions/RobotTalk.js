var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var RobotTalk_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotTalk_IntentionAction, _super);
    function RobotTalk_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotTalk_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.talk")))
            return true;
        return false;
    };
    RobotTalk_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        var needToSpecifyListener = false;
        var txt = null;
        var performative = (intention.attributes[1]).term;
        var context = null;
        if (intention.attributes[1] instanceof TermTermAttribute) {
            var canTalk = ai.canHear(performative.attributes[0].value);
            if (ai.game.communicatorConnectedTo == ai.selfID) {
                // if (canTalk) {
                // we can see the player, cut the connection:
                // ai.game.communicatorConnectedTo = null;
                // }
                canTalk = true;
            }
            if (ai.robot.isTalking())
                return null;
            if ((performative.attributes[0] instanceof ConstantTermAttribute) && canTalk) {
                var targetID = performative.attributes[0].value;
                // context = ai.contextForSpeaker(targetID);
                context = ai.updateContext(targetID);
                if (!context.inConversation &&
                    performative.functor.name != "perf.callattention" &&
                    performative.functor.name != "perf.greet") {
                    needToSpecifyListener = true;
                }
                for (var _i = 0, _a = ai.contexts; _i < _a.length; _i++) {
                    var c = _a[_i];
                    if (c != context && c.inConversation) {
                        needToSpecifyListener = true;
                        c.inConversation = false; // terminate the other conversations
                    }
                }
                if (requester instanceof ConstantTermAttribute &&
                    requester.value != targetID) {
                    // someone asked us to tell someone else something, change the spatial adverbs accordingly (here/there):
                    ai.replaceSpatialAdverbsInReferenceToAnotherSpeaker(performative, requester.value);
                }
                console.log(ai.selfID + " trying to say: " + performative);
                if (needToSpecifyListener) {
                    txt = ai.game.naturalLanguageGenerator.termToEnglish(performative, ai.selfID, performative.attributes[0], context);
                }
                else {
                    txt = ai.game.naturalLanguageGenerator.termToEnglish(performative, ai.selfID, null, context);
                }
                txt = ai.game.naturalLanguageGenerator.capitalize(txt);
            }
        }
        else if (intention.attributes[1] instanceof ConstantTermAttribute) {
            // this is just a shortcut for the 3 laws of robotics easter egg:
            txt = intention.attributes[1].value;
        }
        else {
            console.error("RobotAI.executeIntention: malformed performative: " + performative.toString());
        }
        if (txt != null) {
            if (txt == "Ok" ||
                txt == "I cannot do that") {
                // prevent two "ok" or "I cannot do that" in a row when requests are parsed to more than one
                // performative:
                if (context.performatives[0].text == txt &&
                    context.performatives[0].timeStamp > ai.timeStamp - 60 * 60) {
                    // we are done, prevent repeating the text:
                    this.ir.succeeded = true; // temporarily set this to success
                    return true;
                }
            }
            if (!ai.robot.issueCommandWithString(A4CHARACTER_COMMAND_TALK, txt, 0, ai.game)) {
                return null; // not yet!
            }
            // see if we also need to create a speech bubble, since the player can hear this character through the communicator:
            if (ai.game.communicatorConnectedTo == ai.selfID) {
                ai.game.currentPlayer.map.textBubbles.push([new A4TextBubble(ai.selfID + ": " + txt, 32, fontFamily8px, 6, 8, ai.game, null),
                    TEXT_INITIAL_DELAY + txt.length * TEXT_SPEED]);
            }
            // update natural language context:
            if (performative != null)
                context.newPerformative(ai.selfID, txt, performative, null, null, ir.cause, ai.o, ai.timeStamp);
            for (var _b = 0, _c = ai.contexts; _b < _c.length; _b++) {
                var c2 = _c[_b];
                if (c2 != context)
                    c2.inConversation = false;
            }
        }
        else {
            return false; // empty txt!
        }
        this.ir.succeeded = true;
        return true;
    };
    RobotTalk_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotTalk_IntentionAction\"/>";
    };
    RobotTalk_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotTalk_IntentionAction();
    };
    return RobotTalk_IntentionAction;
}(IntentionAction));
