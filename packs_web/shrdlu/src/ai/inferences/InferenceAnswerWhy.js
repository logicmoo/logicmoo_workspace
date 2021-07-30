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
var AnswerWhy_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerWhy_InferenceEffect, _super);
    function AnswerWhy_InferenceEffect(effectParameter) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerWhy_InferenceEffect.prototype.execute = function (inf, ai) {
        console.log("executeInferenceEffect: INFERENCE_RECORD_EFFECT_ANSWER_WHY");
        console.log("inf.inferences.length: " + inf.inferences.length);
        console.log("inf.inferences[0].endResults: " + inf.inferences[0].endResults);
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("AnswerWhy_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var speakerCharacterID = (this.effectParameter.attributes[1]).value;
        var toExplain = this.effectParameter.attributes[2];
        var negativeAnswer = "'unknown'[symbol]";
        if (inf.inferences[0].endResults.length != 0) {
            var results = [];
            for (var _i = 0, _a = inf.inferences[0].endResults; _i < _a.length; _i++) {
                var result = _a[_i];
                for (var _b = 0, _c = result.bindings.l; _b < _c.length; _b++) {
                    var _d = _c[_b], variable = _d[0], value = _d[1];
                    if (variable.name == "CAUSE" &&
                        results.indexOf(value) == -1) {
                        // we have a result!
                        results.push(value);
                    }
                }
            }
            //				console.log("result: " + result);
            if (results.length > 0) {
                var answer = new Term(ai.o.getSort("relation.cause"), [toExplain, results[0]]);
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + answer + "))", ai.o);
                //					console.log("term: " + term);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            else {
                console.error("Inference produced a result, but none of the resulting variables is the query variable!");
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + negativeAnswer + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
        }
        else if (inf.inferences[1].endResults.length != 0) {
            var causeRecord = this.generateCauseRecord(inf.inferences[1].originalTarget, inf.inferences[1].endResults[0], ai);
            if (causeRecord == null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + negativeAnswer + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            else {
                var causeTerm = causeRecord.term;
                var causeTerms = [];
                if (causeTerm.functor.name == "#and") {
                    var tal = Term.elementsInList(causeTerm, "#and");
                    for (var _e = 0, tal_1 = tal; _e < tal_1.length; _e++) {
                        var ta = tal_1[_e];
                        if (ta instanceof TermTermAttribute) {
                            causeTerms.push(ta.term);
                        }
                    }
                }
                else {
                    causeTerms = [causeTerm];
                }
                for (var _f = 0, causeTerms_1 = causeTerms; _f < causeTerms_1.length; _f++) {
                    var causeTerm2 = causeTerms_1[_f];
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id], relation.cause([any]," + causeTerm2 + ")))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
            }
        }
        else {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + negativeAnswer + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
    };
    AnswerWhy_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerWhy_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    AnswerWhy_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new AnswerWhy_InferenceEffect(t);
    };
    return AnswerWhy_InferenceEffect;
}(InferenceEffect));
