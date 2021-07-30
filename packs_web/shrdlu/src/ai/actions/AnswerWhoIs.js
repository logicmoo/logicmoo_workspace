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
var AnswerWhoIs_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerWhoIs_IntentionAction, _super);
    function AnswerWhoIs_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerWhoIs_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.whois.name")) ||
            intention.functor.is_a(ai.o.getSort("action.answer.whois.noname")))
            return true;
        return false;
    };
    AnswerWhoIs_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        if (intention.functor.is_a(ai.o.getSort("action.answer.whois.name"))) {
            console.log(ai.selfID + " answer whois.name: " + intention.attributes[2]);
            if (intention.attributes[1] instanceof ConstantTermAttribute &&
                intention.attributes[2] instanceof ConstantTermAttribute) {
                var listenerID = intention.attributes[1].value;
                // Don't do any inference for now (we'll see if I need it later on), 
                // directly call the same function that will be called after the inference in whois.noname:
                AnswerWho_InferenceEffect.AnswerWhois(null, intention.attributes[2].value, listenerID, true, ai);
            }
            else if (intention.attributes.length == 4 &&
                intention.attributes[1] instanceof ConstantTermAttribute &&
                intention.attributes[2] instanceof VariableTermAttribute &&
                intention.attributes[3] instanceof TermTermAttribute) {
                var query = (intention.attributes[3]).term;
                var query_l = [query];
                if (query.functor.name == "#and") {
                    query_l = NLParser.termsInList(query, "#and");
                }
                var query_l_signs = [];
                for (var i = 0; i < query_l.length; i++) {
                    if (query_l[i].functor.name == "#not") {
                        query_l[i] = (query_l[i].attributes[0]).term;
                        query_l_signs.push(true);
                    }
                    else {
                        query_l_signs.push(false);
                    }
                }
                var target1 = [new Sentence(query_l, query_l_signs)];
                ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [target1], 1, 0, false, null, new AnswerWho_InferenceEffect(intention)));
            }
            else {
                console.error("executeIntention answer whois.name: case not handled: " + intention);
                ir.succeeded = false;
            }
            return true;
        }
        else if (intention.functor.is_a(ai.o.getSort("action.answer.whois.noname"))) {
            if (intention.attributes.length >= 3) {
                console.log(ai.selfID + " answer whois.noname: " + intention.attributes[2]);
                if (intention.attributes[1] instanceof ConstantTermAttribute &&
                    intention.attributes[2] instanceof ConstantTermAttribute) {
                    // target 1: name of the entity:
                    var target1 = [new Sentence([new Term(ai.o.getSort("name"), [intention.attributes[2],
                                new VariableTermAttribute(ai.o.getSort("symbol"), "NAME")])], [false])];
                    ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [target1], 1, 0, false, null, new AnswerWho_InferenceEffect(intention)));
                    // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
                    // that is then replaced with true/false after inference/continuous is done
                    ir.succeeded = true;
                }
                else {
                    console.error("executeIntention answer whois.noname: attribute[1] or attribute[2] was not a ConstantTermAttribute: " + intention);
                    ir.succeeded = false;
                }
            }
            else {
                console.error("executeIntention answer whois.noname: less attributes than expected!");
                ir.succeeded = false;
            }
            return true;
        }
        ir.succeeded = false;
        return false;
    };
    AnswerWhoIs_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerWhoIs_IntentionAction\"/>";
    };
    AnswerWhoIs_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerWhoIs_IntentionAction();
    };
    return AnswerWhoIs_IntentionAction;
}(IntentionAction));
