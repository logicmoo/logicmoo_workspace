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
var Etaoin3DPrint_IntentionAction = /** @class */ (function (_super) {
    __extends(Etaoin3DPrint_IntentionAction, _super);
    function Etaoin3DPrint_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Etaoin3DPrint_IntentionAction.prototype.canHandle = function (intention, ai) {
        if ((intention.functor.is_a(ai.o.getSort("action.print")) ||
            intention.functor.is_a(ai.o.getSort("verb.make")) ||
            intention.functor.is_a(ai.o.getSort("verb.create"))) &&
            intention.attributes.length == 2)
            return true;
        return false;
    };
    Etaoin3DPrint_IntentionAction.prototype.canHandleWithoutInference = function (perf) {
        if (perf.attributes.length == 4 &&
            perf.attributes[1] instanceof TermTermAttribute &&
            perf.attributes[2] instanceof TermTermAttribute) {
            var action = (perf.attributes[1]).term;
            if (action.attributes.length == 2 &&
                (action.attributes[0] instanceof ConstantTermAttribute) &&
                (action.attributes[1] instanceof VariableTermAttribute)) {
                return true;
            }
        }
        return _super.prototype.canHandleWithoutInference.call(this, perf);
    };
    Etaoin3DPrint_IntentionAction.prototype.execute = function (ir, ai_raw) {
        this.ir = ir;
        var ai = ai_raw;
        var intention = ir.action;
        var requester = ir.requester;
        var toPrint = null;
        if (intention.attributes.length == 2) {
            var toPrintAttribute = intention.attributes[1];
            if ((toPrintAttribute instanceof VariableTermAttribute) ||
                (toPrintAttribute instanceof ConstantTermAttribute)) {
                toPrint = toPrintAttribute.sort;
            }
            var perf = ir.requestingPerformative.performative;
            if (perf.attributes.length == 4 &&
                (perf.attributes[2] instanceof TermTermAttribute) &&
                (toPrintAttribute instanceof VariableTermAttribute)) {
                var constraint = perf.attributes[2].term;
                toPrint = constraint.functor;
            }
        }
        console.log("Etaoin3DPrint_IntentionAction, toPrint: " + toPrint);
        if (toPrint != null) {
            var recipe_idx = -1;
            var recipe = null;
            if (toPrint.name == "power-cord")
                toPrint = ai.o.getSort("cable");
            // find a recipe that matches the request:
            for (var _i = 0, _a = ai.game.three_d_printer_recipies; _i < _a.length; _i++) {
                var tmp = _a[_i];
                var canPrint = tmp[0];
                if (ai.o.getSort(canPrint).is_a(toPrint)) {
                    toPrint = ai.o.getSort(canPrint);
                    recipe = tmp[1];
                    recipe_idx = ai.game.three_d_printer_recipies.indexOf(tmp);
                    break;
                }
            }
            if (recipe == null) {
                if (requester != null) {
                    var term_1 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term_1, null, null, null, ai.timeStamp));
                    term_1 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #not(X:verb.know-how(E:'" + ai.selfID + "'[#id], action.print(E, [" + toPrint.name + "])))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term_1, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
            // Find a 3d printer with enough materials:
            var doWeNeedMetal = false;
            for (var _b = 0, recipe_1 = recipe; _b < recipe_1.length; _b++) {
                var material = recipe_1[_b];
                if (ai.o.getSort(material).is_a(ai.o.getSort("metal"))) {
                    doWeNeedMetal = true;
                    break;
                }
            }
            var printers = [];
            var map = ai.game.getMap("Aurora Station");
            for (var _c = 0, _d = map.objects; _c < _d.length; _c++) {
                var o = _d[_c];
                if (o.name == "plastic 3d printer" && !doWeNeedMetal)
                    printers.push(o);
                if (o.name == "metal 3d printer")
                    printers.push(o);
            }
            var bestPrinter = null;
            var bestMissingMaterials = [];
            for (var _e = 0, printers_1 = printers; _e < printers_1.length; _e++) {
                var printer = printers_1[_e];
                var missing = [];
                for (var _f = 0, recipe_2 = recipe; _f < recipe_2.length; _f++) {
                    var material = recipe_2[_f];
                    if (printer.getStoryStateVariable(material) != "true") {
                        missing.push(material);
                    }
                }
                if (bestPrinter == null || missing.length < bestMissingMaterials.length) {
                    bestPrinter = printer;
                    bestMissingMaterials = missing;
                }
                else {
                    // change printer randomly, so that we don't always just use the left-most one!
                    if (missing.length == 0 && Math.random() < 0.5) {
                        bestPrinter = printer;
                        bestMissingMaterials = missing;
                    }
                }
            }
            if (bestMissingMaterials.length > 0) {
                if (requester != null) {
                    var term_2 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                    ai.intentions.push(new IntentionRecord(term_2, null, null, null, ai.timeStamp));
                    term_2 = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + requester + ", #not(X:verb.have('" + bestPrinter.ID + "'[#id], [" + bestMissingMaterials[0] + "]))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term_2, null, null, null, ai.timeStamp));
                }
                ir.succeeded = false;
                return true;
            }
            // Materialize the object in front of it:
            var obj = ai.game.objectFactory.createObject(toPrint.name, ai.game, false, false);
            obj.x = bestPrinter.x + ai.game.tileWidth;
            obj.y = bestPrinter.y + bestPrinter.getPixelHeight();
            map.addObject(obj);
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.ok(" + ir.requester + "))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform(" + ir.requester + ", space.at('" + obj.ID + "'[#id], 'location-maintenance'[#id])))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            ai.intentionsCausedByRequest.push(ir);
            // force a perception update on the maintenance room, to make sure we can talk about the newly printed object:
            ai.perceptionFocusedOnObject([obj], obj);
            // add the object existence to long term memory:
            ai.addLongTermTerm(Term.fromString(obj.sort.name + "('" + obj.ID + "'[#id])", ai.o), PERCEPTION_PROVENANCE);
            app.achievement_interact_3d_printed_one_of_each_kind[recipe_idx] = true;
            app.achievement_nlp_all_etaoin_actions[5] = true;
            app.trigger_achievement_complete_alert();
            ir.succeeded = true;
        }
        else {
            if (requester != null) {
                var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.ack.denyrequest(" + requester + "))", ai.o);
                ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            }
            ir.succeeded = false;
        }
        return true;
    };
    Etaoin3DPrint_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"Etaoin3DPrint_IntentionAction\"/>";
    };
    Etaoin3DPrint_IntentionAction.loadFromXML = function (xml, ai) {
        return new Etaoin3DPrint_IntentionAction();
    };
    return Etaoin3DPrint_IntentionAction;
}(IntentionAction));
