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
var AnswerWhen_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerWhen_IntentionAction, _super);
    function AnswerWhen_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerWhen_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.when")))
            return true;
        return false;
    };
    AnswerWhen_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        console.log(ai.selfID + " answer when: " + intention);
        var resolution = null;
        var target = null;
        if (intention.attributes.length == 4 &&
            (intention.attributes[3] instanceof VariableTermAttribute || intention.attributes[3] instanceof ConstantTermAttribute)) {
            target = intention.attributes[2];
            resolution = intention.attributes[3].sort;
        }
        else if (intention.attributes.length == 3) {
            target = intention.attributes[2];
        }
        if (target instanceof VariableTermAttribute || target instanceof ConstantTermAttribute) {
            // asking about the time of some time pronoun ("now", "today", etc.):
            if (intention.attributes[2].sort.is_a(ai.o.getSort("time.now"))) {
                var resolution_1 = intention.attributes[3].sort;
                console.log("executeIntention answer when: answering what time is it now at resolution " + resolution_1);
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",time.date('" + ai.timeStamp + "'[number]," + intention.attributes[3] + ")))", ai.o);
                ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                ir.succeeded = true;
            }
            else {
                console.error("executeIntention answer when: unsupported when question!");
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",'unknown'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                ir.succeeded = false;
            }
        }
        else if (target instanceof TermTermAttribute) {
            // asking about the time of some event:
            var time = this.timeOfEvent(target, ai);
            console.log("executeIntention answer when: answering what time is event " + target + " -> " + time);
            if (time == null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",'unknown'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                ir.succeeded = false;
            }
            else {
                if (resolution == null) {
                    // if it's the same day, report minutes, otherwise, report date:
                    if (getCurrentYear(ai.timeStamp) == getCurrentYear(time) &&
                        getCurrentYearDay(ai.timeStamp) == getCurrentYearDay(time)) {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",time.date('" + time + "'[number], [time.minute])))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                        ir.succeeded = true;
                    }
                    else {
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",time.date('" + time + "'[number], [time.day])))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                        ir.succeeded = true;
                    }
                }
                else {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",time.date('" + time + "'[number], [" + resolution.name + "])))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    ir.succeeded = true;
                }
            }
        }
        else {
            console.error("executeIntention answer when: unsupported case: " + intention);
            ir.succeeded = false;
        }
        return true;
    };
    AnswerWhen_IntentionAction.prototype.timeOfEvent = function (eventAtt, ai) {
        if (!(eventAtt instanceof TermTermAttribute))
            return null;
        var event = eventAtt.term;
        // search for it in the knowledge base:
        var entry = ai.longTermMemory.containsUnifyingTerm(event);
        if (entry != null)
            return entry.time;
        // try in the previous memory:
        for (var _i = 0, _a = ai.longTermMemory.plainPreviousSentenceList; _i < _a.length; _i++) {
            var se = _a[_i];
            if (se.sentence.terms.length == 1 &&
                se.sentence.sign[0]) {
                var bindings = new Bindings();
                if (event.unify(se.sentence.terms[0], OCCURS_CHECK, bindings))
                    return se.time;
            }
        }
        for (var _b = 0, _c = ai.longTermMemory.previousSentencesWithNoCurrentSentence; _b < _c.length; _b++) {
            var se = _c[_b];
            if (se.sentence.terms.length == 1 &&
                se.sentence.sign[0]) {
                var bindings = new Bindings();
                if (event.unify(se.sentence.terms[0], OCCURS_CHECK, bindings))
                    return se.time;
            }
        }
        return null;
    };
    AnswerWhen_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerWhen_IntentionAction\"/>";
    };
    AnswerWhen_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerWhen_IntentionAction();
    };
    return AnswerWhen_IntentionAction;
}(IntentionAction));
