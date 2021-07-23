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
var A4InferenceEffectFactory = /** @class */ (function (_super) {
    __extends(A4InferenceEffectFactory, _super);
    function A4InferenceEffectFactory() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    A4InferenceEffectFactory.prototype.loadFromXML = function (xml, ai, o, variables, variableNames) {
        if (xml.getAttribute("type") == "AnswerHowGoto_InferenceEffect")
            return AnswerHowGoto_InferenceEffect.loadFromXML(xml, ai, o, variables, variableNames);
        if (xml.getAttribute("type") == "AnswerWhere_InferenceEffect")
            return AnswerWhere_InferenceEffect.loadFromXML(xml, ai, o, variables, variableNames);
        return _super.prototype.loadFromXML.call(this, xml, ai, o, variables, variableNames);
    };
    return A4InferenceEffectFactory;
}(InferenceEffectFactory));
