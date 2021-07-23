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
var AnswerPredicate_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerPredicate_InferenceEffect, _super);
    function AnswerPredicate_InferenceEffect(effectParameter) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerPredicate_InferenceEffect.prototype.execute = function (inf, ai) {
        console.log("AnswerPredicate_InferenceEffect");
        console.log("inf.inferences.length: " + inf.inferences.length);
        for (var i = 0; i < inf.inferences.length; i++) {
            console.log("inf.inferences[" + i + "].endResults.length: " + inf.inferences[i].endResults.length);
            if (inf.inferences[i].endResults.length > 0) {
                console.log("    Reasons for first result:");
                for (var _i = 0, _a = inf.inferences[i].endResults[0].getBaseSentences(inf.inferences[i].originalTarget); _i < _a.length; _i++) {
                    var t = _a[_i];
                    console.log("        " + t);
                }
            }
        }
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("AnswerPredicate_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var intention = this.effectParameter;
        var targetCharacterID = (intention.attributes[1]).value;
        var forAlls = [];
        var forAllVariableNames = [];
        var predicateVariables = intention.attributes[2].term.getAllVariables();
        var predicateVariableNames = [];
        for (var _b = 0, predicateVariables_1 = predicateVariables; _b < predicateVariables_1.length; _b++) {
            var v = predicateVariables_1[_b];
            predicateVariableNames.push(v.name);
        }
        if (intention.attributes.length == 5) {
            forAlls = NLParser.termsInList((intention.attributes[3]).term, "#and");
        }
        else if (intention.attributes.length == 4) {
            forAlls = NLParser.termsInList((intention.attributes[3]).term, "#and");
            if (forAlls.length == 0 || forAlls[0].functor.name != "#forall") {
                forAlls = [];
            }
        }
        for (var _c = 0, forAlls_1 = forAlls; _c < forAlls_1.length; _c++) {
            var forAll = forAlls_1[_c];
            if (forAll.attributes.length >= 1 &&
                forAll.attributes[0] instanceof VariableTermAttribute) {
                forAllVariableNames.push(forAll.attributes[0].name);
            }
        }
        var nBaseInferences = inf.inferences.length - forAllVariableNames.length;
        console.log("predicateVariableNames: " + predicateVariableNames);
        for (var _d = 0, forAllVariableNames_1 = forAllVariableNames; _d < forAllVariableNames_1.length; _d++) {
            var v = forAllVariableNames_1[_d];
            var idx = predicateVariableNames.indexOf(v);
            if (idx >= 0)
                predicateVariableNames.splice(idx, 1);
        }
        console.log("predicateVariableNames (without forAll): " + predicateVariableNames);
        console.log("forAllVariableNames: " + forAllVariableNames);
        // filter by the forAll results:
        for (var j = 0; j < nBaseInferences; j++) {
            for (var i = 0; i < forAllVariableNames.length; i++) {
                if (inf.inferences.length >= i + nBaseInferences) {
                    var allValues = [];
                    for (var _e = 0, _f = inf.inferences[i + nBaseInferences].endResults; _e < _f.length; _e++) {
                        var result = _f[_e];
                        var v = result.getValueForVariableName(forAllVariableNames[i]);
                        if (v != null)
                            allValues.push(v);
                    }
                    console.log("forAll values (" + forAllVariableNames[i] + "): " + allValues);
                    inf.inferences[j].filterResultsByForAll(predicateVariableNames, forAllVariableNames[i], allValues);
                }
            }
        }
        if (nBaseInferences == 1) {
            // this means that there was a variable in the query, and thus only the negation was launched:
            if (inf.inferences[0].endResults.length == 0) {
                //					console.log("inference.endResults.length == 0, and no inferenceNegated");
                var answer = "no";
                if (this.effectParameter.functor.is_a(ai.o.getSort("action.answer.predicate-negated")))
                    answer = "yes";
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + targetCharacterID + "'[#id],'" + answer + "'[symbol]))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                //				console.log("new intention: " + term);
            }
            else if (inf.inferences[0].endResults[0].bindings.l.length == 0) {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + targetCharacterID + "'[#id],'unknown'[symbol]))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            else {
                for (var _g = 0, _h = inf.inferences[0].endResults[0].bindings.l; _g < _h.length; _g++) {
                    var tmp = _h[_g];
                    var value = tmp[1];
                    if (value instanceof ConstantTermAttribute &&
                        value.sort.name == "#id") {
                        // we need to add this mention to the context entity list:
                        var context = ai.contextForSpeaker(targetCharacterID);
                        if (context != null) {
                            var ce = context.newContextEntity(value, ai.timeStamp, null, ai.o, false);
                            if (ce != null) {
                                var idx = context.mentions.indexOf(ce);
                                if (idx != -1)
                                    context.mentions.splice(idx, 1);
                                context.mentions.unshift(ce);
                            }
                        }
                    }
                }
                var answer = "yes";
                if (this.effectParameter.functor.is_a(ai.o.getSort("action.answer.predicate-negated")))
                    answer = "no";
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + targetCharacterID + "'[#id],'" + answer + "'[symbol]))", ai.o);
                var causeRecord = this.generateCauseRecord(inf.inferences[0].originalTarget, inf.inferences[0].endResults[0], ai);
                ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
            }
        }
        else {
            if (inf.inferences[0].endResults.length == 0) {
                if (inf.inferences[1].endResults.length == 0) {
                    //						console.log("inference.endResults.length == 0, and inferenceNegated.endResults.length == 0");
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + targetCharacterID + "'[#id],'unknown'[symbol]))";
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    //				console.log("new intention: " + term);
                }
                else {
                    //						console.log("inference.endResults.length == 0, and inferenceNegated.endResults.length != 0");
                    var answer = "yes";
                    if (this.effectParameter.functor.is_a(ai.o.getSort("action.answer.predicate-negated")))
                        answer = "no";
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + targetCharacterID + "'[#id],'" + answer + "'[symbol]))";
                    var term = Term.fromString(tmp, ai.o);
                    var causeRecord = this.generateCauseRecord(inf.inferences[1].originalTarget, inf.inferences[1].endResults[0], ai);
                    ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
                }
            }
            else {
                //console.log("inference.endResults.length != 0");
                var answer = "no";
                if (this.effectParameter.functor.is_a(ai.o.getSort("action.answer.predicate-negated")))
                    answer = "yes";
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + targetCharacterID + "'[#id],'" + answer + "'[symbol]))";
                var term = Term.fromString(tmp, ai.o);
                var causeRecord = this.generateCauseRecord(inf.inferences[0].originalTarget, inf.inferences[0].endResults[0], ai);
                ai.intentions.push(new IntentionRecord(term, null, null, causeRecord, ai.timeStamp));
            }
        }
    };
    AnswerPredicate_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerPredicate_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    AnswerPredicate_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new AnswerPredicate_InferenceEffect(t);
    };
    return AnswerPredicate_InferenceEffect;
}(InferenceEffect));
