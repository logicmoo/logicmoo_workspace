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
var AnswerHowMany_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerHowMany_InferenceEffect, _super);
    function AnswerHowMany_InferenceEffect(effectParameter) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerHowMany_InferenceEffect.prototype.execute = function (inf, ai) {
        console.log("executeInferenceEffect: INFERENCE_RECORD_EFFECT_ANSWER_HOWMANY");
        console.log("inf.inferences.length: " + inf.inferences.length);
        console.log("inf.inferences[0].endResults.length: " + inf.inferences[0].endResults.length);
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("AnswerHowMany_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var speakerCharacterID = (this.effectParameter.attributes[1]).value;
        var queryPerformative = (this.effectParameter.attributes[2]).term;
        var queryVariable = (queryPerformative.attributes[1]);
        var queryTerm = null;
        if (queryPerformative.attributes[2] instanceof TermTermAttribute) {
            queryTerm = (queryPerformative.attributes[2]).term;
        }
        var negativeAnswer = "'no-matches-found'[symbol]";
        if (queryTerm != null &&
            (queryTerm.functor.is_a(ai.cache_sort_property_with_value) ||
                queryTerm.functor.is_a(ai.cache_sort_relation_with_value)))
            negativeAnswer = "'unknown'[symbol]";
        if (inf.inferences[0].endResults.length != 0) {
            var results = [];
            for (var _i = 0, _a = inf.inferences[0].endResults; _i < _a.length; _i++) {
                var result = _a[_i];
                for (var _b = 0, _c = result.bindings.l; _b < _c.length; _b++) {
                    var _d = _c[_b], variable = _d[0], value = _d[1];
                    if (variable == queryVariable) {
                        var found = false;
                        for (var _e = 0, results_1 = results; _e < results_1.length; _e++) {
                            var value2 = results_1[_e];
                            if (Term.equalsAttribute(value2, value, new Bindings())) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            // we have a result!
                            results.push(value);
                        }
                    }
                }
            }
            console.log("results: " + results);
            if (results.length > 0) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],'" + results.length + "'[number]))", ai.o);
                // store the state in case there are more answers to be given later using perf.more answers
                var context = ai.contextForSpeaker(speakerCharacterID);
                if (context != null) {
                    ai.intentions.push(new IntentionRecord(term, null, context.getNLContextPerformative(queryPerformative), null, ai.timeStamp));
                }
                else {
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
            }
            else {
                console.error("Inference produced a result, but none of the resulting variables is the query variable!");
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + negativeAnswer + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
        }
        else {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + negativeAnswer + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
    };
    AnswerHowMany_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerHowMany_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    AnswerHowMany_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new AnswerHowMany_InferenceEffect(t);
    };
    return AnswerHowMany_InferenceEffect;
}(InferenceEffect));
