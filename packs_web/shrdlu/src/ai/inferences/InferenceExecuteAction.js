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
var ExecuteAction_InferenceEffect = /** @class */ (function (_super) {
    __extends(ExecuteAction_InferenceEffect, _super);
    function ExecuteAction_InferenceEffect(perf) {
        var _this = _super.call(this) || this;
        _this.perf = null;
        _this.perf = perf;
        return _this;
    }
    ExecuteAction_InferenceEffect.prototype.execute = function (inf, ai) {
        var raw_action = (this.perf.attributes[1]).term;
        if (inf.inferences.length >= 1 &&
            inf.inferences[0].endResults.length > 0) {
            var speaker = inf.triggeredBySpeaker;
            var context = ai.contextForSpeaker(speaker);
            var nlcp = context.getNLContextPerformative(inf.triggeredBy);
            var action_variables = raw_action.getAllVariables();
            var action_variableNames = [];
            for (var _i = 0, action_variables_1 = action_variables; _i < action_variables_1.length; _i++) {
                var v = action_variables_1[_i];
                action_variableNames.push(v.name);
            }
            var forAllVariableNames = [];
            if (this.perf.attributes.length >= 5) {
                for (var _a = 0, _b = NLParser.termsInList((this.perf.attributes[4]).term, "#and"); _a < _b.length; _a++) {
                    var forAll = _b[_a];
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
                        for (var _c = 0, _d = inf.inferences[i + 1].endResults; _c < _d.length; _c++) {
                            var result = _d[_c];
                            var v = result.getValueForVariableName(forAllVariableNames[i]);
                            if (v != null)
                                allValues.push(v);
                        }
                        console.log("forAll values (" + forAllVariableNames[i] + "): " + allValues);
                        inf.inferences[0].filterResultsByForAll(action_variableNames, forAllVariableNames[i], allValues);
                    }
                }
            }
            var action = raw_action.applyBindings(inf.inferences[0].endResults[0].bindings);
            var ir = new IntentionRecord(action, new ConstantTermAttribute(speaker, ai.cache_sort_id), nlcp, null, ai.timeStamp);
            var tmp = ai.canSatisfyActionRequest(ir);
            if (tmp == ACTION_REQUEST_CAN_BE_SATISFIED) {
                ir.alternative_actions = [];
                for (var _e = 0, _f = inf.inferences[0].endResults; _e < _f.length; _e++) {
                    var result = _f[_e];
                    ir.alternative_actions.push(raw_action.applyBindings(result.bindings));
                    ai.applyBindingsToSubsequentActionsOrInferences(result.bindings);
                }
                if (nlcp.performative.attributes.length >= 4)
                    ir.numberConstraint = nlcp.performative.attributes[3];
                ai.planForAction(ir);
            }
            else if (tmp == ACTION_REQUEST_CANNOT_BE_SATISFIED) {
                var tmp2 = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest('" + speaker + "'[#id]))";
                var term = Term.fromString(tmp2, ai.o);
                ai.intentions.push(new IntentionRecord(term, new ConstantTermAttribute(speaker, ai.cache_sort_id), nlcp, null, ai.timeStamp));
            }
        }
        else {
            if (raw_action.functor.is_a(ai.o.getSort("verb.hear")) ||
                raw_action.functor.is_a(ai.o.getSort("verb.see"))) {
                var tmp2 = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + inf.triggeredBySpeaker + "'[#id],'no'[symbol]))";
                var term = Term.fromString(tmp2, ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            else {
                var speaker = inf.triggeredBySpeaker;
                var context = ai.contextForSpeaker(speaker);
                var nlcp = context.getNLContextPerformative(inf.triggeredBy);
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest('" + inf.triggeredBySpeaker + "'[#id]))", ai.o);
                if (nlcp.performative.attributes.length >= 3) {
                    var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id], " + nlcp.performative.attributes[2] + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                }
                else {
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
            }
        }
    };
    ExecuteAction_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"ExecuteAction_InferenceEffect\" perf=\"" + this.perf.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    ExecuteAction_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("perf"), o, variableNames, variables).term;
        return new ExecuteAction_InferenceEffect(t);
    };
    return ExecuteAction_InferenceEffect;
}(InferenceEffect));
