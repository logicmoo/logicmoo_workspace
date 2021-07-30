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
var AnswerDefine_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerDefine_IntentionAction, _super);
    function AnswerDefine_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerDefine_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.define")))
            return true;
        return false;
    };
    AnswerDefine_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        if (intention.attributes.length == 2 &&
            (intention.attributes[0] instanceof ConstantTermAttribute) &&
            ((intention.attributes[1] instanceof VariableTermAttribute) ||
                (intention.attributes[1] instanceof ConstantTermAttribute)) &&
            (intention.attributes[0]).value == ai.selfID) {
            var sortToDefine = intention.attributes[1].sort;
            var definitionAsTerm = null;
            if (sortToDefine.name == "three-laws-of-robotics") {
                // easeter egg!
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], '1. A robot may not injure a human being or, through inaction, allow a human being to come to harm.'[symbol])", ai.o);
                ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
                term = Term.fromString("action.talk('" + ai.selfID + "'[#id], '2. A robot must obey the orders given it by human beings except where such orders would conflict with the First Law.'[symbol])", ai.o);
                ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
                term = Term.fromString("action.talk('" + ai.selfID + "'[#id], '3. A robot must protect its own existence as long as such protection does not conflict with the First or Second Laws.'[symbol])", ai.o);
                ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
                ir.succeeded = true;
                return true;
            }
            for (var i = 0; i < sortToDefine.parents.length; i++) {
                var parentSort = sortToDefine.parents[i];
                if (parentSort.name != "any" && parentSort.name != "abstract-entity") {
                    if (POSParser.sortIsConsideredForTypes(parentSort, ai.o) ||
                        (parentSort.is_a(ai.o.getSort("property")) && parentSort.name != "property")) {
                        if (definitionAsTerm == null) {
                            definitionAsTerm = new VariableTermAttribute(parentSort, null);
                        }
                        else {
                            definitionAsTerm = new TermTermAttribute(new Term(ai.o.getSort("#and"), [new VariableTermAttribute(parentSort, null),
                                definitionAsTerm]));
                        }
                    }
                }
            }
            if (definitionAsTerm == null) {
                // console.log("Cannot define: " + sortToDefine.name);
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + ", 'verb.define'[symbol]))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
            }
            else {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ",verb.be([" + sortToDefine.name + "]," + definitionAsTerm + ")))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
                }
                ir.succeeded = true;
            }
        }
        else {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, requester, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
        }
        return true;
    };
    AnswerDefine_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerDefine_IntentionAction\"/>";
    };
    AnswerDefine_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerDefine_IntentionAction();
    };
    return AnswerDefine_IntentionAction;
}(IntentionAction));
