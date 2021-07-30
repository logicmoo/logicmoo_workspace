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
var EtaoinTalk_IntentionAction = /** @class */ (function (_super) {
    __extends(EtaoinTalk_IntentionAction, _super);
    function EtaoinTalk_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EtaoinTalk_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.talk")))
            return true;
        return false;
    };
    EtaoinTalk_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        var needToSpecifyListener = false;
        var performative = null;
        if (intention.attributes[1] instanceof TermTermAttribute) {
            performative = (intention.attributes[1]).term;
            if ((performative.attributes[0] instanceof ConstantTermAttribute) &&
                ai.canHear(performative.attributes[0].value)) {
                // if we are already talking, just wait:
                if (ai.player_object.map.textBubbles.length > 0)
                    return null;
                var targetID = performative.attributes[0].value;
                var context = ai.updateContext(targetID);
                var txt = null;
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
                    // this also replaces "verb.come" to "ver.go-to" with the appropriate location
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
                    ai.game.addMessage(ai.selfID + ": " + txt);
                    var bubble = new A4TextBubble(txt, 32, fontFamily8px, 6, 8, ai.game, null);
                    ai.player_object.map.textBubbles.push([bubble, TEXT_INITIAL_DELAY + txt.length * TEXT_SPEED]);
                    if (ai.game.debugTextBubbleLog != null) {
                        ai.game.debugTextBubbleLog.push([ai.game.cycle, ai.selfID, bubble]);
                    }
                    // create a perception buffer entry:
                    var targetObject = ai.game.findObjectByIDJustObject(targetID);
                    if (targetObject != null) {
                        targetObject.map.addPerceptionBufferRecord(new PerceptionBufferRecord("talk", ai.selfID, ai.o.getSort("disembodied-ai"), null, null, txt, null, null, targetObject.x, targetObject.y, targetObject.x + targetObject.getPixelWidth(), targetObject.y + targetObject.getPixelHeight()));
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
            }
        }
        else if (intention.attributes[1] instanceof ConstantTermAttribute) {
            // this is just a shortcut for the 3 laws of robotics easter egg:
            var txt = intention.attributes[1].value;
            ai.game.addMessage(ai.selfID + ": " + txt);
            var bubble = new A4TextBubble(txt, 32, fontFamily8px, 6, 8, ai.game, null);
            ai.player_object.map.textBubbles.push([bubble, TEXT_INITIAL_DELAY + txt.length * TEXT_SPEED]);
            if (ai.game.debugTextBubbleLog != null) {
                ai.game.debugTextBubbleLog.push([ai.game.cycle, ai.selfID, bubble]);
            }
        }
        else {
            console.error("EtaoinAI.executeIntention: malformed intention: " + intention.toString());
        }
        this.ir.succeeded = true; // temporarily set this to success
        return true;
    };
    EtaoinTalk_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"EtaoinTalk_IntentionAction\"/>";
    };
    EtaoinTalk_IntentionAction.loadFromXML = function (xml, ai) {
        return new EtaoinTalk_IntentionAction();
    };
    return EtaoinTalk_IntentionAction;
}(IntentionAction));
