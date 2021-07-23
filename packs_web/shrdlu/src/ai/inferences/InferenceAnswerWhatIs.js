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
var AnswerWhatIs_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerWhatIs_InferenceEffect, _super);
    function AnswerWhatIs_InferenceEffect(effectParameter) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerWhatIs_InferenceEffect.prototype.execute = function (inf, ai) {
        var listenerID = (this.effectParameter.attributes[1]).value;
        var targetID = (this.effectParameter.attributes[2]).value;
        var targetName = null;
        if (inf.inferences[0].endResults.length != 0) {
            for (var _i = 0, _a = inf.inferences[0].endResults[0].bindings.l; _i < _a.length; _i++) {
                var b = _a[_i];
                if (b[0].name == "NAME") {
                    var v = b[1];
                    if (v instanceof ConstantTermAttribute) {
                        targetName = v.value;
                        break;
                    }
                }
            }
        }
        AnswerWhatIs_InferenceEffect.executeInferenceEffect_AnswerWhatis(targetName, targetID, listenerID, ai);
    };
    AnswerWhatIs_InferenceEffect.executeInferenceEffect_AnswerWhatis = function (name, whatID, listenerID, ai) {
        //		console.log("executeInferenceEffect_AnswerWhatis: " + name + ", " + whatID);
        // get the types:
        var mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("object('" + whatID + "'[#id])", ai.o));
        if (mostSpecificTypes == null || mostSpecificTypes.length == 0)
            mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("space.location('" + whatID + "'[#id])", ai.o));
        if (mostSpecificTypes == null || mostSpecificTypes.length == 0)
            mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("abstract-entity('" + whatID + "'[#id])", ai.o));
        if (mostSpecificTypes == null || mostSpecificTypes.length == 0)
            mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("role('" + whatID + "'[#id])", ai.o));
        console.log("executeInferenceEffect_AnswerWhatis: mostSpecificTypes: " + mostSpecificTypes);
        // generate the talk intentions:
        if (name == null && mostSpecificTypes.length == 0) {
            ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id],'unknown'[symbol]))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
        }
        else {
            if (name != null) {
                ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id],name('" + whatID + "'[#id],'" + name + "'[symbol])))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
            }
            if (mostSpecificTypes.length != 0) {
                ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id], " + mostSpecificTypes[0] + "))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
            }
        }
    };
    AnswerWhatIs_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerWhatIs_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    AnswerWhatIs_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new AnswerWhatIs_InferenceEffect(t);
    };
    return AnswerWhatIs_InferenceEffect;
}(InferenceEffect));
