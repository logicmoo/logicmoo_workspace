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
var RobotExit_IntentionAction = /** @class */ (function (_super) {
    __extends(RobotExit_IntentionAction, _super);
    function RobotExit_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    RobotExit_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.leave"))) {
            if (intention.attributes.length >= 2 &&
                (intention.attributes[1] instanceof ConstantTermAttribute)) {
                //let id:string = (<ConstantTermAttribute>intention.attributes[1]).value;
                //let targetObject:A4Object = (<RobotAI>ai).game.findObjectByIDJustObject(id);
                //if (targetObject != null &&
                //	targetObject.map == (<RobotAI>ai).robot.map) {
                return true;
                //}
            }
            if ( /*(<RobotAI>ai).robot.isInVehicle() && */intention.attributes.length == 1) {
                return true;
            }
        }
        return false;
    };
    RobotExit_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        if (!ai.robot.isInVehicle()) {
            if (intention.attributes.length == 1) {
                var term2 = new Term(ai.o.getSort("verb.go"), [intention.attributes[0], new VariableTermAttribute(ai.o.getSort("space.outside"), null)]);
                ai.intentions.push(new IntentionRecord(term2, requester, null, null, ai.timeStamp));
                ir.succeeded = true;
                return true;
            }
            else if (intention.attributes.length == 2) {
                var term2 = new Term(ai.o.getSort("verb.go"), [intention.attributes[0],
                    new TermTermAttribute(new Term(ai.o.getSort("space.outside.of"), [intention.attributes[1]]))]);
                ai.intentions.push(new IntentionRecord(term2, requester, null, null, ai.timeStamp));
                ir.succeeded = true;
                return true;
            }
            else {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
        }
        if (ai.selfID == "shrdlu" && !ai.visionActive) {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.request.action('player'[#id], verb.bring('player'[#id], 'shrdlu'[#id], 'location-aurora-station'[#id])))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (intention.attributes.length == 0 ||
            !(intention.attributes[0] instanceof ConstantTermAttribute)) {
            // we should never get here:
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        if (intention.attributes.length == 1) {
            // just exit:
            ai.robot.disembark();
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ai.intentionsCausedByRequest.push(ir);
        }
        else if (intention.attributes.length >= 2 &&
            (intention.attributes[1] instanceof ConstantTermAttribute)) {
            // make sure ID and vehicle match:
            var id = intention.attributes[1].value;
            if (ai.robot.vehicle.ID == id) {
                ai.robot.disembark();
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ai.intentionsCausedByRequest.push(ir);
            }
            else {
                if (requester != null) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
        }
        else {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
            return true;
        }
        ir.succeeded = true;
        return true;
    };
    RobotExit_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"RobotExit_IntentionAction\"/>";
    };
    RobotExit_IntentionAction.loadFromXML = function (xml, ai) {
        return new RobotExit_IntentionAction();
    };
    return RobotExit_IntentionAction;
}(IntentionAction));
