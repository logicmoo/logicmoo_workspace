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
var EtaoinRead_IntentionAction = /** @class */ (function (_super) {
    __extends(EtaoinRead_IntentionAction, _super);
    function EtaoinRead_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EtaoinRead_IntentionAction.prototype.canHandle = function (intention, ai) {
        if ((intention.functor.is_a(ai.o.getSort("verb.analyze")) ||
            intention.functor.is_a(ai.o.getSort("verb.examine")) ||
            intention.functor.is_a(ai.o.getSort("verb.read"))) &&
            intention.attributes.length >= 2) {
            var targetID = (intention.attributes[1]).value;
            if (targetID == "tardis-memory-core") {
                return true;
            }
            else {
                return false;
            }
        }
        return false;
    };
    EtaoinRead_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        var targetID = (intention.attributes[1]).value;
        console.log(ai.selfID + " read: " + intention);
        var item_tmp = ai.game.findObjectByIDJustObject(targetID);
        if (item_tmp != null && (item_tmp instanceof A4Item)) {
            if (targetID == "tardis-memory-core") {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", action.put-in(" + requester + ", '" + targetID + "'[#id], [console])))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                ir.succeeded = true;
            }
            else {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                ir.succeeded = false;
            }
        }
        else if (item_tmp == null) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            ir.succeeded = false;
        }
        else {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            ir.succeeded = false;
        }
        return true;
    };
    EtaoinRead_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"EtaoinRead_IntentionAction\"/>";
    };
    EtaoinRead_IntentionAction.loadFromXML = function (xml, ai) {
        return new EtaoinRead_IntentionAction();
    };
    return EtaoinRead_IntentionAction;
}(IntentionAction));
