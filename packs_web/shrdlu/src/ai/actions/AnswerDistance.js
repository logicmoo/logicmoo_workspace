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
var AnswerDistance_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerDistance_IntentionAction, _super);
    function AnswerDistance_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerDistance_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.distance")))
            return true;
        return false;
    };
    AnswerDistance_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        var requester = ir.requester;
        var o1ID = null;
        var o2ID = null;
        var units = ai.o.getSort("meter");
        if (intention.attributes.length >= 4) {
            if (intention.attributes[2] instanceof ConstantTermAttribute) {
                o1ID = intention.attributes[2].value;
            }
            if (intention.attributes[3] instanceof ConstantTermAttribute) {
                o2ID = intention.attributes[3].value;
            }
        }
        if (intention.attributes.length >= 5)
            units = intention.attributes[4].sort;
        if (o1ID == null || o2ID == null) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",'unknown'[symbol]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        var d = ai.distanceBetweenIds(o1ID, o2ID);
        if (d != null) {
            var d2 = null;
            d2 = this.convertToUnits(d, units);
            if (d2 != null) {
                d = d2;
            }
            else {
                units = ai.o.getSort("meter");
            }
            if (requester != null) {
                // we know the answer already without inference!
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer(" + intention.attributes[1] + ",'" + d + "'[" + units.name + "]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = true;
            return true;
        }
        // launch an inference process:
        {
            var newPerformative = Term.fromString("action.answer.distance('" + ai.selfID + "'[#id], " + requester + ", perf.q.query('" + ai.selfID + "'[#id], DISTANCE, distance('" + o1ID + "'[#id],'" + o2ID + "'[#id], DISTANCE)))", ai.o);
            var negated_s_l = Term.termToSentences(new Term(ai.o.getSort("#not"), [(newPerformative.attributes[2]).term.attributes[2]]), ai.o);
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [negated_s_l], 1, 0, false, null, new AnswerQuery_InferenceEffect(newPerformative, ir.requestingPerformative)));
            // TODO: this should have some temporary value (in all actions that require inference or continuous execution)
            // that is then replaced with true/false after inference/continuous is done
            ir.succeeded = true;
        }
        // if (requester != null) {
        // 	let term:Term = Term.fromString("action.talk('"+ai.selfID+"'[#id], perf.inform.answer("+intention.attributes[1]+",'unknown'[symbol]))", ai.o);
        // 	ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
        // }		
        return true;
    };
    AnswerDistance_IntentionAction.prototype.convertToUnits = function (meters, unit) {
        /*
    <sort name="milimeter" super="distance.unit,length.unit"/>
    <sort name="meter" super="distance.unit,length.unit"/>
    <sort name="kilometer" super="distance.unit,length.unit"/>
    <sort name="light-year" super="distance.unit,length.unit"/>
        */
        if (unit.name == "milimiter")
            return meters * 1000;
        if (unit.name == "meter")
            return meters;
        if (unit.name == "kilometer")
            return meters / 1000;
        if (unit.name == "light-year")
            return meters / 9.461E15;
        return null;
    };
    AnswerDistance_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerDistance_IntentionAction\"/>";
    };
    AnswerDistance_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerDistance_IntentionAction();
    };
    return AnswerDistance_IntentionAction;
}(IntentionAction));
