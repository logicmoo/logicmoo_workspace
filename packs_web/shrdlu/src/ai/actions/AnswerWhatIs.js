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
var AnswerWhatIs_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerWhatIs_IntentionAction, _super);
    function AnswerWhatIs_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerWhatIs_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.whatis.name")) ||
            intention.functor.is_a(ai.o.getSort("action.answer.whatis.noname")))
            return true;
        return false;
    };
    AnswerWhatIs_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        if (intention.functor == ai.o.getSort("action.answer.whatis.name")) {
            console.log(ai.selfID + " answer whatis.name: " + intention.attributes[2]);
            if (intention.attributes[1] instanceof ConstantTermAttribute &&
                intention.attributes[2] instanceof ConstantTermAttribute) {
                var listenerID = intention.attributes[1].value;
                // Don't do any inference for now (we'll see if I need it later on), 
                // directly call the same function that will be called after the inference in whatis.noname:
                AnswerWhatIs_InferenceEffect.executeInferenceEffect_AnswerWhatis(null, intention.attributes[2].value, listenerID, ai);
                // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
                // that is then replaced with true/false after inference/continuous is done
                ir.succeeded = true;
            }
            else {
                console.error("executeIntention answer whatis.name: attribute[1] or attribute[2] was not a ConstantTermAttribute: " + intention);
                ir.succeeded = false;
            }
            return true;
        }
        else if (intention.functor == ai.o.getSort("action.answer.whatis.noname")) {
            console.log(ai.selfID + " answer whatis.noname: " + intention.attributes[2]);
            if (intention.attributes[1] instanceof ConstantTermAttribute &&
                intention.attributes[2] instanceof ConstantTermAttribute) {
                // target 1: name of the entity:
                var target1 = [new Sentence([new Term(ai.o.getSort("name"), [intention.attributes[2],
                            new VariableTermAttribute(ai.o.getSort("symbol"), "NAME")])], [false])];
                ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [target1], 1, 0, false, null, new AnswerWhatIs_InferenceEffect(intention)));
                // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
                // that is then replaced with true/false after inference/continuous is done
                ir.succeeded = true;
            }
            else {
                console.error("executeIntention answer whatis.noname: attribute[1] or attribute[2] was not a ConstantTermAttribute: " + intention);
                ir.succeeded = false;
            }
            return true;
        }
        return false;
    };
    AnswerWhatIs_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerWhatIs_IntentionAction\"/>";
    };
    AnswerWhatIs_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerWhatIs_IntentionAction();
    };
    return AnswerWhatIs_IntentionAction;
}(IntentionAction));
