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
var StopAction_InferenceEffect = /** @class */ (function (_super) {
    __extends(StopAction_InferenceEffect, _super);
    function StopAction_InferenceEffect(action) {
        var _this = _super.call(this) || this;
        _this.action = null;
        _this.action = action;
        return _this;
    }
    StopAction_InferenceEffect.prototype.execute = function (inf, ai) {
        if (inf.inferences.length == 1 &&
            inf.inferences[0].endResults.length > 0) {
            var speaker = inf.triggeredBySpeaker;
            var context = ai.contextForSpeaker(speaker);
            var nlcp = context.getNLContextPerformative(inf.triggeredBy);
            this.action = this.action.applyBindings(inf.inferences[0].endResults[0].bindings);
            if (ai.stopAction(this.action, speaker)) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok('" + context.speaker + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            else {
                var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest('" + speaker + "'[#id]))";
                var term = Term.fromString(tmp, ai.o);
                ai.intentions.push(new IntentionRecord(term, new ConstantTermAttribute(speaker, ai.cache_sort_id), nlcp, null, ai.timeStamp));
            }
        }
        else {
            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest('" + inf.triggeredBySpeaker + "'[#id]))";
            var term = Term.fromString(tmp, ai.o);
            var cause = Term.fromString("#not(" + this.action + ")", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
        }
    };
    StopAction_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"StopAction_InferenceEffect\" action=\"" + this.action.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    StopAction_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("action"), o, variableNames, variables).term;
        return new StopAction_InferenceEffect(t);
    };
    return StopAction_InferenceEffect;
}(InferenceEffect));
