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
var Call_IntentionAction = /** @class */ (function (_super) {
    __extends(Call_IntentionAction, _super);
    function Call_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Call_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.call")) &&
            intention.attributes.length == 3)
            return true;
        if (intention.functor.name == "#not" &&
            (intention.attributes[0] instanceof TermTermAttribute) &&
            intention.attributes[0].term.functor.is_a(ai.o.getSort("verb.call")) &&
            intention.attributes[0].term.attributes.length == 3)
            return true;
        return false;
    };
    Call_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        // execute the memorize action:
        console.log(ai.selfID + " call: " + intention);
        if (intention.functor.name == "#not") {
            intention = intention.attributes[0].term;
            // ask about the name of the character:
            var question = Term.fromString("perf.q.query(" + requester + ", X, name(" + intention.attributes[1] + ",X))", ai.o);
            var action = new Term(ai.o.getSort("action.talk"), [intention.attributes[0],
                new TermTermAttribute(question)]);
            ai.intentions.push(new IntentionRecord(action, null, null, null, ai.timeStamp));
            ir.succeeded = true;
        }
        else if (intention.attributes[2] instanceof ConstantTermAttribute) {
            // see if we were waiting for an answer to this question:
            if (requester instanceof ConstantTermAttribute) {
                var context = ai.contextForSpeakerWithoutCreatingANewOne(requester.value);
                var pattern1 = Term.fromString("perf.q.query(" + requester + ", X, name(" + intention.attributes[1] + ",X))", ai.o);
                var pattern2 = Term.fromString("perf.q.predicate(" + requester + ", verb.know($PLAYER,#and(#query(Y), name($PLAYER,Y))))", ai.o);
                for (var i = 0; i < context.expectingAnswerToQuestion_stack.length; i++) {
                    var q = context.expectingAnswerToQuestion_stack[i];
                    if (q.performative.unify(pattern1, OCCURS_CHECK, new Bindings()) ||
                        q.performative.unify(pattern2, OCCURS_CHECK, new Bindings())) {
                        // pop it from the stack:
                        context.expectingAnswerToQuestion_stack.splice(i, 1);
                        context.expectingAnswerToQuestionTimeStamp_stack.splice(i, 1);
                        break;
                    }
                }
            }
            var fact = Term.fromString("name(" + intention.attributes[1] + ", " + intention.attributes[2] + ")", ai.o);
            var action = new Term(ai.o.getSort("action.memorize"), [intention.attributes[0],
                ir.requester,
                new TermTermAttribute(fact)]);
            ai.intentions.push(new IntentionRecord(action, null, null, null, ai.timeStamp));
            ir.succeeded = true;
        }
        else {
            // ???
        }
        return true;
    };
    Call_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"Call_IntentionAction\"/>";
    };
    Call_IntentionAction.loadFromXML = function (xml, ai) {
        return new Call_IntentionAction();
    };
    return Call_IntentionAction;
}(IntentionAction));
