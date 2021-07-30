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
var AnswerWho_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerWho_InferenceEffect, _super);
    function AnswerWho_InferenceEffect(effectParameter) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerWho_InferenceEffect.prototype.execute = function (inf, ai) {
        var listenerID = (this.effectParameter.attributes[1]).value;
        var targetIDVariableName = null;
        var targetID = null;
        var targetName = null;
        if (this.effectParameter.attributes[2] instanceof ConstantTermAttribute) {
            targetID = (this.effectParameter.attributes[2]).value;
        }
        else if (this.effectParameter.attributes[2] instanceof VariableTermAttribute) {
            targetIDVariableName = (this.effectParameter.attributes[2]).name;
        }
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
                if (targetIDVariableName != null && b[0].name == targetIDVariableName) {
                    var v = b[1];
                    if (v instanceof ConstantTermAttribute) {
                        targetID = v.value;
                        break;
                    }
                }
            }
        }
        AnswerWho_InferenceEffect.AnswerWhois(targetName, targetID, listenerID, false, ai);
    };
    AnswerWho_InferenceEffect.AnswerWhois = function (name, whoID, listenerID, playerMentionedName, ai) {
        //		console.log("executeInferenceEffect_AnswerWhois: " + name + ", " + whoID);
        // get the types:
        var mostSpecificTypes = null;
        for (var _i = 0, _a = POSParser.sortsToConsiderForTypes; _i < _a.length; _i++) {
            var typeSortName = _a[_i];
            if (mostSpecificTypes == null || mostSpecificTypes.length == 0)
                mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString(typeSortName + "('" + whoID + "'[#id])", ai.o));
        }
        /*
                ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("object('"+whoID+"'[#id])", ai.o));
                if (mostSpecificTypes == null || mostSpecificTypes.length == 0) mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("space.location('"+whoID+"'[#id])", ai.o));
                if (mostSpecificTypes == null || mostSpecificTypes.length == 0) mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("abstract-entity('"+whoID+"'[#id])", ai.o));
                if (mostSpecificTypes == null || mostSpecificTypes.length == 0) mostSpecificTypes = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("role('"+whoID+"'[#id])", ai.o));
        */
        //		console.log("executeInferenceEffect_AnswerWhois: mostSpecificTypes: " + mostSpecificTypes);
        // get the role/profession:
        var mostSpecificRoles1 = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("role('" + whoID + "'[#id],[role])", ai.o));
        var mostSpecificRoles2 = ai.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered(Term.fromString("role('" + whoID + "'[#id],[any],[role])", ai.o));
        //		console.log("executeInferenceEffect_AnswerWhois: mostSpecificRoles1: " + mostSpecificRoles1);
        //		console.log("executeInferenceEffect_AnswerWhois: mostSpecificRoles2: " + mostSpecificRoles2);
        // generate the talk intentions:
        if (name == null &&
            //mostSpecificTypes.length == 0 && 
            mostSpecificRoles1.length == 0 && mostSpecificRoles2.length == 0 && !playerMentionedName) {
            ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id],'unknown'[symbol]))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
        }
        else if (name == null &&
            mostSpecificTypes.length == 0 &&
            mostSpecificRoles1.length == 0 && mostSpecificRoles2.length == 0 && playerMentionedName) {
            ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id],'unknown'[symbol]))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
        }
        else {
            if (name != null) {
                ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id],name('" + whoID + "'[#id],'" + name + "'[symbol])))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
            }
            if (mostSpecificTypes.length != 0) {
                ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id], " + mostSpecificTypes[0] + "))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
            }
            if (mostSpecificRoles2.length != 0) {
                ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id], " + mostSpecificRoles2[0] + "))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
            }
            else if (mostSpecificRoles1.length != 0) {
                ai.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + listenerID + "'[#id], " + mostSpecificRoles1[0] + "))", ai.o), new ConstantTermAttribute(listenerID, ai.o.getSort("#id")), null, null, ai.timeStamp));
            }
        }
    };
    AnswerWho_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerWho_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    AnswerWho_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new AnswerWho_InferenceEffect(t);
    };
    return AnswerWho_InferenceEffect;
}(InferenceEffect));
