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
var AnswerHow_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerHow_InferenceEffect, _super);
    function AnswerHow_InferenceEffect(effectParameter) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerHow_InferenceEffect.prototype.execute = function (inf, ai) {
        console.log("executeInferenceEffect: INFERENCE_RECORD_EFFECT_ANSWER_HOW");
        console.log("inf.inferences.length: " + inf.inferences.length);
        console.log("inf.inferences[0].endResults: " + inf.inferences[0].endResults);
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("AnswerHow_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var speakerCharacterID = (this.effectParameter.attributes[1]).value;
        console.log("query result, answer how (source): " + inf.inferences[0].endResults);
        if (inf.inferences[0].endResults.length == 0) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],'unknown'[symbol]))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        else {
            // get the location ID
            var how = null;
            if (inf.inferences[0].endResults.length != 0) {
                for (var _i = 0, _a = inf.inferences[0].endResults[0].bindings.l; _i < _a.length; _i++) {
                    var b = _a[_i];
                    if (b[0].name == "HOW") {
                        var v = b[1];
                        if (v instanceof TermTermAttribute) {
                            how = v.term;
                            break;
                        }
                    }
                }
            }
            if (how == null) {
                var term_1 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],'unknown'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term_1, null, null, null, ai.timeStamp));
                return;
            }
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]))", ai.o);
            term.attributes[1].term.attributes.push(new TermTermAttribute(how));
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
    };
    AnswerHow_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerHow_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    AnswerHow_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new AnswerHow_InferenceEffect(t);
    };
    return AnswerHow_InferenceEffect;
}(InferenceEffect));
