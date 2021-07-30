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
var Memorize_IntentionAction = /** @class */ (function (_super) {
    __extends(Memorize_IntentionAction, _super);
    function Memorize_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Memorize_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.memorize")))
            return true;
        return false;
    };
    Memorize_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        // execute the memorize action:
        console.log(ai.selfID + " memorize: " + intention.attributes[2]);
        // we add the sentence with positive sign, to see if it introduces a contradiction
        var s_l = Term.termToSentences((intention.attributes[2]).term, ai.o);
        console.log("term to sentences (#sentences = " + s_l.length + "): " + s_l);
        var variablesPresent = false;
        var timeModifierPresent = false;
        // 1) see if it has variables AND is more than one sentence:
        for (var _i = 0, s_l_1 = s_l; _i < s_l_1.length; _i++) {
            var s = s_l_1[_i];
            if (s.getAllVariables().length > 0)
                variablesPresent = true;
            for (var _a = 0, _b = s.terms; _a < _b.length; _a++) {
                var t = _b[_a];
                if (t.functor.is_a(ai.o.getSort("time.past")))
                    timeModifierPresent = true;
                if (t.functor.is_a(ai.o.getSort("time.future")))
                    timeModifierPresent = true;
            }
        }
        if (timeModifierPresent) {
            console.warn("time modifiers present, not memorizing for now...");
            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.unsure(" + requester + "))";
            var term = Term.fromString(tmp, ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            ir.succeeded = true;
            return true;
        }
        // check for the special case where the player is stating that they "know/remember" something, so we follow up
        // asking about it:
        if (variablesPresent && s_l[0].terms.length == 1 && s_l[0].sign[0]) {
            var term = s_l[0].terms[0];
            if (term.functor.is_a(ai.o.getSort("verb.know")) &&
                term.attributes.length == 2 &&
                term.attributes[0] instanceof ConstantTermAttribute &&
                term.attributes[1] instanceof TermTermAttribute &&
                requester instanceof ConstantTermAttribute) {
                var term2 = term.attributes[1].term;
                if (term.attributes[0].value == requester.value &&
                    term2.functor.is_a(ai.o.getSort("property-with-value")) &&
                    term2.attributes.length == 2 &&
                    term2.attributes[0] instanceof ConstantTermAttribute &&
                    term2.attributes[1] instanceof VariableTermAttribute) {
                    // ask the requester about it, no need to memorize this yet:
                    var queryTerm = new Term(ai.o.getSort("perf.q.query"), [requester, term2.attributes[1], term.attributes[1]]);
                    var actionTerm = Term.fromString("action.talk('" + ai.selfID + "'[#id], " + queryTerm + ")", ai.o);
                    ai.intentions.push(new IntentionRecord(actionTerm, null, null, null, ai.timeStamp));
                    ir.succeeded = true;
                    return true;
                }
            }
        }
        if (s_l.length > 1 && variablesPresent) {
            // Note: I added this case earlier in the development of the game, but I cannot figure out what case was it covering,
            // so, I removed it as I cannot find any sentence for which this is needed.
            // this is the complicated case, we can just launch an inference process to see if we need to memorize or we already knew
            // console.log("executeIntention memorize: sentence of length > 1 with variables, this is a complex case, we need to try to negate the sentences: " + s_l);
            // let negated_s_l:Sentence[] = [];
            // let negated_s:Sentence = new Sentence([],[]);
            // for(let s of s_l) {
            // 	if (s.getAllVariables().length > 0) variablesPresent = true;
            // 	for(let t of s.terms) {
            // 		if (t.functor.is_a(ai.o.getSort("time.past"))) timeModifierPresent = true;
            // 		if (t.functor.is_a(ai.o.getSort("time.future"))) timeModifierPresent = true;
            // 	}
            // 	let tmp:Sentence[] = s.negate();
            // 	if (tmp == null || tmp.length != 1) {
            // 		console.error("executeIntention memorize: cannot negate sentences in intention!: " + intention);		
            // 		let tmp:string = "action.talk('"+ai.selfID+"'[#id], perf.ack.unsure("+requester+"))";
            // 		let term:Term = Term.fromString(tmp, ai.o);
            // 		ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            // 		return true;
            // 	}
            // 	negated_s.terms = negated_s.terms.concat(tmp[0].terms);
            // 	negated_s.sign = negated_s.sign.concat(tmp[0].sign);
            // }
            // negated_s_l = [negated_s];
            // ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [negated_s_l], 1, 0, false, null, new Memorize_InferenceEffect(intention, true)));
            // Alternative code with better negation:
            // let targets:Sentence[][] = [];
            // let negatedExpression:Term = new Term(ai.o.getSort("#not"),
            // 								 	  [new TermTermAttribute((<TermTermAttribute>(intention.attributes[2])).term)])
            // console.log("Memorize, negatedExpression: " + negatedExpression);
            // let target:Sentence[] = Term.termToSentences(negatedExpression, ai.o);
            // targets.push(target)			
            // ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], targets, 1, 0, false, null, new Memorize_InferenceEffect(intention, true)));
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [s_l], 1, 0, false, null, new Memorize_InferenceEffect(intention, false)));
        }
        else {
            if (s_l.length == 1 && s_l[0].terms.length == 1) {
                // Check for the special case, where the player is just correcting a wrong statement she stated in the past:
                var negatedToMemorize = new Sentence(s_l[0].terms, [!s_l[0].sign[0]]);
                var se = ai.longTermMemory.findSentenceEntry(negatedToMemorize);
                if (se != null && se.provenance == MEMORIZE_PROVENANCE) {
                    console.log("Correcting a wrong statement she stated in the past! checking if this would cause a contradiction");
                    // We can safely remove the "negatedToMemorize" sentence, since, if the new one causes a contradiction, it means it was already
                    // implied by the KB, so it wasn't needed. But if it does not, then we had to remove it anyway:
                    ai.longTermMemory.removeSentence(negatedToMemorize);
                }
            }
            console.log("executeIntention memorize: sentence list of length 1, easy case: " + s_l);
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [s_l], 1, 0, false, null, new Memorize_InferenceEffect(intention, false)));
        }
        ir.succeeded = true;
        return true;
    };
    Memorize_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"Memorize_IntentionAction\"/>";
    };
    Memorize_IntentionAction.loadFromXML = function (xml, ai) {
        return new Memorize_IntentionAction();
    };
    return Memorize_IntentionAction;
}(IntentionAction));
