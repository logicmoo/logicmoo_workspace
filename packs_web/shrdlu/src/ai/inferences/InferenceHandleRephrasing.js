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
var HandleRephrasing_InferenceEffect = /** @class */ (function (_super) {
    __extends(HandleRephrasing_InferenceEffect, _super);
    function HandleRephrasing_InferenceEffect(effectParameter, nlcp) {
        var _this = _super.call(this) || this;
        _this.nlcp = null;
        _this.effectParameter = null;
        _this.nlcp = nlcp;
        _this.effectParameter = effectParameter;
        return _this;
    }
    HandleRephrasing_InferenceEffect.prototype.execute = function (inf, ai) {
        console.log("HandleRephrasing_InferenceEffect");
        console.log("inf.inferences.length: " + inf.inferences.length);
        console.log("inf.inferences[0].endResults.length: " + inf.inferences[0].endResults.length);
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("HandleRephrasing_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var speakerCharacterID = (this.effectParameter.attributes[1]).value;
        var queryPerformative = (this.effectParameter.attributes[2]).term;
        var originalText = (this.effectParameter.attributes[3]).value;
        var originalError = (this.effectParameter.attributes[4]);
        var queryVariable = (queryPerformative.attributes[1]);
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
        if (inf.inferences[0].endResults.length != 0) {
            var results = [];
            for (var _d = 0, _e = inf.inferences[0].endResults; _d < _e.length; _d++) {
                var result = _e[_d];
                if (result == null || result.bindings == null)
                    continue;
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
                // TODO: for now we just take the first result, but what when there is more than one?
                var result = results[0];
                HandleRephrasing_InferenceEffect.handleRephrasing(originalText, result, originalError, speakerCharacterID, ai);
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
    HandleRephrasing_InferenceEffect.handleRephrasing = function (text, resultTerm, error, speaker, ai) {
        if (resultTerm instanceof ConstantTermAttribute) {
            var result = resultTerm.value;
            var context = ai.contextForSpeaker(speaker);
            if (context.dereference_hints.length == 0) {
                context.dereference_hints = [new NLDereferenceHint(error, result)];
                ai.parsePerceivedText(text, speaker, context, []);
                context.dereference_hints = [];
            }
            else {
                // This is probably because we have created a loop, so, we should just cut it, and respond we don't understand or something
                ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id]))))", ai.o), null, null, null, ai.timeStamp));
            }
        }
        else {
            console.error("handleRephrasing: resultTerm is not a constant: " + resultTerm);
        }
    };
    HandleRephrasing_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        if (this.nlcp != null) {
            var context = ai.contextForSpeaker(this.nlcp.speaker);
            if (context != null) {
                return "<InferenceEffect type=\"HandleRephrasing_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\" nlcp=\"" + context.performatives.indexOf(this.nlcp) + "\" speaker=\"" + this.nlcp.speaker + "\"/>";
            }
            else {
                return "<InferenceEffect type=\"HandleRephrasing_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
            }
        }
        else {
            return "<InferenceEffect type=\"HandleRephrasing_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
        }
    };
    HandleRephrasing_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        var speaker = xml.getAttribute("speaker");
        if (speaker != null) {
            var nlcp = Number(xml.getAttribute("nlcp"));
            var context = ai.contextForSpeaker(speaker);
            if (context != null) {
                return new HandleRephrasing_InferenceEffect(t, context.performatives[nlcp]);
            }
            else {
                return new HandleRephrasing_InferenceEffect(t, null);
            }
        }
        else {
            return new HandleRephrasing_InferenceEffect(t, null);
        }
    };
    return HandleRephrasing_InferenceEffect;
}(InferenceEffect));
