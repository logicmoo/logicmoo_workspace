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
var EtaoinConnectTo_IntentionAction = /** @class */ (function (_super) {
    __extends(EtaoinConnectTo_IntentionAction, _super);
    function EtaoinConnectTo_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    EtaoinConnectTo_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("verb.connect-to")) &&
            intention.attributes.length == 3 &&
            (intention.attributes[1] instanceof ConstantTermAttribute) &&
            (intention.attributes[2] instanceof ConstantTermAttribute))
            return true;
        return false;
    };
    EtaoinConnectTo_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var game = ai.game;
        var intention = ir.action;
        var requester = ir.requester;
        // execute the memorize action:
        console.log(ai.selfID + " connect to: " + intention);
        var target = intention.attributes[2].value;
        if (target == "etaoin") {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #and(#and(X:verb.can(" + requester + ", #and(Y:action.talk(" + requester + "), relation.target(Y, '" + target + "'[#id]))), relation.tool(X, 'communicator'[#id]), time.now(X)))))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        else {
            var targetObject = game.findObjectByIDJustObject(target);
            if (targetObject != null &&
                ai.withinEtaoinViewRange(targetObject)) {
                // Etaoin can see the target:
                if (target == "qwerty" ||
                    target == "shrdlu") {
                    game.communicatorConnectedTo = target;
                    game.communicatorConnectionTime = ai.timeStamp;
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #and(#and(X:verb.can(" + requester + ", #and(Y:action.talk(" + requester + "), relation.target(Y, '" + target + "'[#id]))), relation.tool(X, 'communicator'[#id]), time.now(X)))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    app.achievement_nlp_all_etaoin_actions[0] = true;
                    app.trigger_achievement_complete_alert();
                }
                else {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    var cause = Term.fromString("#not(verb.can('" + target + "'[#id], action.talk('" + target + "'[#id])))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
                }
            }
            else {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                var cause = Term.fromString("#not(verb.see('" + ai.selfID + "'[#id],'" + target + "'[#id]))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, new CauseRecord(cause, null, ai.timeStamp), ai.timeStamp));
            }
        }
        return true;
    };
    EtaoinConnectTo_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"EtaoinConnectTo_IntentionAction\"/>";
    };
    EtaoinConnectTo_IntentionAction.loadFromXML = function (xml, ai) {
        return new EtaoinConnectTo_IntentionAction();
    };
    return EtaoinConnectTo_IntentionAction;
}(IntentionAction));
