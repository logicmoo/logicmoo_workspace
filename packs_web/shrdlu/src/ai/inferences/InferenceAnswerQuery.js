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
var AnswerQuery_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerQuery_InferenceEffect, _super);
    function AnswerQuery_InferenceEffect(effectParameter, nlcp) {
        var _this = _super.call(this) || this;
        _this.nlcp = null;
        _this.effectParameter = null;
        _this.nlcp = nlcp;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerQuery_InferenceEffect.prototype.execute = function (inf, ai) {
        console.log("AnswerQuery_InferenceEffect");
        console.log("inf.inferences.length: " + inf.inferences.length);
        console.log("inf.inferences[0].endResults.length: " + inf.inferences[0].endResults.length);
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("AnswerQuery_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var speakerCharacterID = (this.effectParameter.attributes[1]).value;
        var queryPerformative = (this.effectParameter.attributes[2]).term;
        var queryVariable = (queryPerformative.attributes[1]);
        var queryTerm = null;
        if (queryPerformative.attributes[2] instanceof TermTermAttribute) {
            queryTerm = (queryPerformative.attributes[2]).term;
        }
        var forAllVariableNames = [];
        if (queryPerformative.attributes.length >= 4) {
            for (var _i = 0, _a = NLParser.termsInList((queryPerformative.attributes[3]).term, "#and"); _i < _a.length; _i++) {
                var forAll = _a[_i];
                if (forAll.attributes.length >= 1 &&
                    forAll.attributes[0] instanceof VariableTermAttribute) {
                    forAllVariableNames.push(forAll.attributes[0].name);
                }
            }
        }
        console.log("forAllVariableNames: " + forAllVariableNames);
        if (inf.inferences.length != 1 + forAllVariableNames.length) {
            console.error("number of inferences is wrong, should be 1 + " + forAllVariableNames.length + ", but is: " + inf.inferences.length);
        }
        else {
            // filter by the forAll results:
            for (var i = 0; i < forAllVariableNames.length; i++) {
                if (inf.inferences.length >= i + 1) {
                    var allValues = [];
                    for (var _b = 0, _c = inf.inferences[i + 1].endResults; _b < _c.length; _b++) {
                        var result = _c[_b];
                        var v = result.getValueForVariableName(forAllVariableNames[i]);
                        if (v != null)
                            allValues.push(v);
                    }
                    console.log("forAll values (" + forAllVariableNames[i] + "): " + allValues);
                    inf.inferences[0].filterResultsByForAll([queryVariable.name], forAllVariableNames[i], allValues);
                }
            }
        }
        var negativeAnswer = "'no-matches-found'[symbol]";
        if (queryTerm != null) {
            if (queryTerm.functor.is_a(ai.cache_sort_property_with_value) ||
                queryTerm.functor.is_a(ai.cache_sort_relation_with_value)) {
                negativeAnswer = "'unknown'[symbol]";
            }
            else if (queryTerm.functor.is_a(ai.o.getSort("verb.happen"))) {
                negativeAnswer = "'nothing'[symbol]";
            }
        }
        if (inf.inferences[0].endResults.length != 0) {
            var results = [];
            for (var _d = 0, _e = inf.inferences[0].endResults; _d < _e.length; _d++) {
                var result = _e[_d];
                for (var _f = 0, _g = result.bindings.l; _f < _g.length; _f++) {
                    var _h = _g[_f], variable = _h[0], value = _h[1];
                    if (variable == queryVariable) {
                        // we have a result! check for duplicates:
                        var found = false;
                        for (var _j = 0, results_1 = results; _j < results_1.length; _j++) {
                            var res = results_1[_j];
                            if ((res instanceof ConstantTermAttribute) &&
                                (value instanceof ConstantTermAttribute)) {
                                if (res.value == value.value) {
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if (!found)
                            results.push(value);
                    }
                }
            }
            console.log("result: " + results);
            if (results.length > 0) {
                var resultsTA = null;
                if (results.length > ai.maximum_answers_to_give_at_once_for_a_query) {
                    resultsTA = new ConstantTermAttribute("etcetera", ai.o.getSort("etcetera"));
                    for (var i = 0; i < ai.maximum_answers_to_give_at_once_for_a_query; i++) {
                        var result = results[i];
                        // See if we need to provide results in past tense:
                        if (inf.timeTerm != null && inf.timeTerm.functor.is_a(ai.o.getSort("time.past"))) {
                            if (result instanceof TermTermAttribute) {
                                result = new TermTermAttribute(new Term(ai.o.getSort("#and"), [result,
                                    new TermTermAttribute(new Term(ai.o.getSort("time.past"), [result]))]));
                            }
                        }
                        resultsTA = new TermTermAttribute(new Term(ai.o.getSort("#and"), [result, resultsTA]));
                    }
                }
                else {
                    for (var i = 0; i < results.length; i++) {
                        var result = results[i];
                        // See if we need to provide results in past tense:
                        if (inf.timeTerm != null && inf.timeTerm.functor.is_a(ai.o.getSort("time.past"))) {
                            if (result instanceof TermTermAttribute) {
                                result = new TermTermAttribute(new Term(ai.o.getSort("#and"), [result,
                                    new TermTermAttribute(new Term(ai.o.getSort("time.past"), [result]))]));
                            }
                        }
                        if (resultsTA == null) {
                            resultsTA = result;
                        }
                        else {
                            resultsTA = new TermTermAttribute(new Term(ai.o.getSort("#and"), [result, resultsTA]));
                        }
                    }
                }
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + resultsTA + "))", ai.o);
                //					console.log("term: " + term);
                // store the state in case there are more answers to be given later using perf.more answers
                var context = ai.contextForSpeaker(speakerCharacterID);
                if (context != null) {
                    context.lastEnumeratedQuestion_answered = this.nlcp;
                    context.lastEnumeratedQuestion_answers = results;
                    context.lastEnumeratedQuestion_next_answer_index = Math.min(results.length, ai.maximum_answers_to_give_at_once_for_a_query);
                    if (results.length == 1) {
                        // if we only have one result, record the reason for the result:
                        var causeRecord = this.generateCauseRecord(inf.inferences[0].originalTarget, inf.inferences[0].endResults[0], ai);
                        ai.intentions.push(new IntentionRecord(term, null, context.getNLContextPerformative(queryPerformative), causeRecord, ai.timeStamp));
                    }
                    else {
                        ai.intentions.push(new IntentionRecord(term, null, context.getNLContextPerformative(queryPerformative), null, ai.timeStamp));
                    }
                }
                else {
                    if (results.length == 1) {
                        // if we only have one result, record the reason for the result:
                        var causeRecord = this.generateCauseRecord(inf.inferences[0].originalTarget, inf.inferences[0].endResults[0], ai);
                        ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
                    }
                    else {
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
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
    AnswerQuery_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        if (this.nlcp != null) {
            var context = ai.contextForSpeaker(this.nlcp.speaker);
            if (context != null) {
                return "<InferenceEffect type=\"AnswerQuery_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\" nlcp=\"" + context.performatives.indexOf(this.nlcp) + "\" speaker=\"" + this.nlcp.speaker + "\"/>";
            }
            else {
                return "<InferenceEffect type=\"AnswerQuery_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
            }
        }
        else {
            return "<InferenceEffect type=\"AnswerQuery_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
        }
    };
    AnswerQuery_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        var speaker = xml.getAttribute("speaker");
        if (speaker != null) {
            var nlcp = Number(xml.getAttribute("nlcp"));
            var context = ai.contextForSpeaker(speaker);
            if (context != null) {
                return new AnswerQuery_InferenceEffect(t, context.performatives[nlcp]);
            }
            else {
                return new AnswerQuery_InferenceEffect(t, null);
            }
        }
        else {
            return new AnswerQuery_InferenceEffect(t, null);
        }
    };
    return AnswerQuery_InferenceEffect;
}(InferenceEffect));
