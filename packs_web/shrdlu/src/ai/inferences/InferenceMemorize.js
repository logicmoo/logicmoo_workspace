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
var Memorize_InferenceEffect = /** @class */ (function (_super) {
    __extends(Memorize_InferenceEffect, _super);
    function Memorize_InferenceEffect(effectParameter, negated) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.negated = false;
        _this.effectParameter = effectParameter;
        _this.negated = negated;
        return _this;
    }
    Memorize_InferenceEffect.prototype.execute = function (inf, ai) {
        // memorize the target:
        console.log("Memorize_InferenceEffect");
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("Memorize_InferenceEffect.executeInferenceEffect: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var targetCharacterID = (this.effectParameter.attributes[1]).value;
        var memorize = false;
        if (this.negated) {
            // this was the complex case:
            if (inf.inferences[0].endResults.length == 0) {
                // there was no contradiction...
                // We are not sure..., let's not memorize, just in case...
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.unsure('" + targetCharacterID + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            else {
                // we already knew, just say ok:
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok('" + targetCharacterID + "'[#id]))", ai.o);
                var causeRecord = this.generateCauseRecord(inf.inferences[0].originalTarget, inf.inferences[0].endResults[0], ai);
                ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
                console.log("Memorize_InferenceEffect, we already knew: " + inf.inferences[0].endResults);
            }
        }
        else {
            if (inf.inferences[0].endResults.length == 0) {
                // there was no contradiction... we can add the sentence safely
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok('" + targetCharacterID + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                memorize = true;
            }
            else {
                // contradiction:
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.contradict('" + targetCharacterID + "'[#id]))", ai.o);
                var causeRecord = this.generateCauseRecord(inf.inferences[0].originalTarget, inf.inferences[0].endResults[0], ai);
                ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
            }
        }
        if (memorize) {
            var s_l = Term.termToSentences((this.effectParameter.attributes[2]).term, ai.o);
            for (var _i = 0, s_l_1 = s_l; _i < s_l_1.length; _i++) {
                var s = s_l_1[_i];
                if (s.terms.length == 1 && s.sign[0] == true) {
                    console.log("Memorize_InferenceEffect, term: " + s);
                    ai.addLongTermTerm(s.terms[0], MEMORIZE_PROVENANCE);
                }
                else {
                    console.log("Memorize_InferenceEffect, sentence: " + s);
                    ai.addLongTermRuleNow(s, MEMORIZE_PROVENANCE);
                }
            }
        }
    };
    Memorize_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"Memorize_InferenceEffect\" " +
            "effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\" " +
            "negated=\"" + this.negated + "/>";
    };
    Memorize_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new Memorize_InferenceEffect(t, xml.getAttribute("negated") == "true");
    };
    return Memorize_InferenceEffect;
}(InferenceEffect));
