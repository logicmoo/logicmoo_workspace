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
var AnswerWhere_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerWhere_InferenceEffect, _super);
    function AnswerWhere_InferenceEffect(effectParameter, whereto) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.whereto = false;
        _this.effectParameter = effectParameter;
        _this.whereto = whereto;
        return _this;
    }
    AnswerWhere_InferenceEffect.prototype.execute = function (inf, ai_raw) {
        var ai = ai_raw;
        var where_preposition = "space.at";
        var query_perf = "perf.q.whereis";
        if (this.whereto) {
            where_preposition = "relation.target";
            query_perf = "perf.q.whereto";
        }
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("AnswerWhere_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var speakerCharacterID = (this.effectParameter.attributes[1]).value;
        var targetID = null;
        var targetTermString = null;
        console.log("query result, answer where (target): " + inf.inferences[0].endResults);
        console.log("query result, answer where (speaker): " + inf.inferences[1].endResults);
        if (this.effectParameter.attributes[2] instanceof ConstantTermAttribute) {
            targetID = (this.effectParameter.attributes[2]).value;
            if (targetID == "hypothetical-character") {
                targetID = null;
                targetTermString = "[any]";
            }
            else {
                targetTermString = "'" + targetID + "'[#id]";
            }
        }
        else if (this.effectParameter.attributes[2] instanceof VariableTermAttribute) {
            targetTermString = "[" + this.effectParameter.attributes[2].sort + "]";
        }
        if (inf.inferences[0].endResults.length == 0) {
            var term1 = null;
            if (targetID != null) {
                term1 = Term.fromString("perf.inform.answer('" + speakerCharacterID + "'[#id],'unknown'[symbol]," + query_perf + "('" + ai.selfID + "'[#id]," + targetTermString + "))", ai.o);
            }
            else {
                term1 = Term.fromString("perf.inform.answer('" + speakerCharacterID + "'[#id],'unknown'[symbol])", ai.o);
            }
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id])", ai.o);
            term.attributes.push(new TermTermAttribute(term1));
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
            console.log("new intention: " + term);
        }
        else {
            // get the location ID
            var selectedBindings = null;
            var targetLocation = null;
            var targetIfItsALocation = ai.game.getAILocationByID(targetID);
            var targetLocationID = null;
            var speakerLocation = null;
            var speakerLocationID = null;
            for (var _i = 0, _a = inf.inferences[0].endResults; _i < _a.length; _i++) {
                var result = _a[_i];
                for (var _b = 0, _c = result.bindings.l; _b < _c.length; _b++) {
                    var b = _c[_b];
                    if (b[0].name == "WHERE") {
                        var v = b[1];
                        if (v instanceof ConstantTermAttribute) {
                            // select the most specific one (and also, we do not like "communicator range" as an answer):
                            if (targetLocation == null || targetLocationID == "communicator-range") {
                                targetLocationID = v.value;
                                targetLocation = ai.game.getAILocationByID(targetLocationID);
                                selectedBindings = result.bindings;
                            }
                            else {
                                var targetLocationID2 = v.value;
                                var targetLocation2 = ai.game.getAILocationByID(targetLocationID2);
                                var idx1 = ai.game.locations.indexOf(targetLocation);
                                var idx2 = ai.game.locations.indexOf(targetLocation2);
                                if (idx1 >= 0 && idx2 >= 0 && ai.game.location_in[idx2][idx1]) {
                                    targetLocationID = targetLocationID2;
                                    targetLocation = targetLocation2;
                                    selectedBindings = result.bindings;
                                }
                            }
                        }
                    }
                }
            }
            if (inf.inferences[1].endResults.length != 0) {
                for (var _d = 0, _e = inf.inferences[1].endResults; _d < _e.length; _d++) {
                    var result = _e[_d];
                    for (var _f = 0, _g = result.bindings.l; _f < _g.length; _f++) {
                        var b = _g[_f];
                        if (b[0].name == "WHERE") {
                            var v = b[1];
                            if (v instanceof ConstantTermAttribute) {
                                // select the most specific one:
                                if (speakerLocation == null || speakerLocationID == "communicator-range") {
                                    speakerLocationID = v.value;
                                    speakerLocation = ai.game.getAILocationByID(speakerLocationID);
                                }
                                else {
                                    var speakerLocationID2 = v.value;
                                    var speakerLocation2 = ai.game.getAILocationByID(speakerLocationID2);
                                    var idx1 = ai.game.locations.indexOf(speakerLocation);
                                    var idx2 = ai.game.locations.indexOf(speakerLocation2);
                                    if (idx1 >= 0 && idx2 >= 0 && ai.game.location_in[idx2][idx1]) {
                                        speakerLocationID = speakerLocationID2;
                                        speakerLocation = speakerLocation2;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (selectedBindings != null && this.effectParameter.attributes[2] instanceof VariableTermAttribute) {
                var tmp = this.effectParameter.attributes[2].applyBindings(selectedBindings);
                if (tmp instanceof ConstantTermAttribute) {
                    targetID = tmp.value;
                    if (targetID == "hypothetical-character") {
                        targetID = null;
                        targetTermString = "[any]";
                    }
                    else {
                        targetTermString = "'" + targetID + "'[#id]";
                    }
                }
            }
            if (targetLocationID == null) {
                console.error("A4RuleBasedAI.executeInferenceEffect: cannot find location from results " + inf.inferences[0].endResults);
                return;
            }
            if (targetLocation != null && targetLocation == speakerLocation &&
                targetID != null &&
                speakerCharacterID != targetID &&
                ai.canSee(targetID)) {
                // if we can see the target, and it's in the same room as the speaker (who is a different entity), then
                // explain where it is relative to the speaker:
                var speakerObject = ai.game.findObjectByIDJustObject(speakerCharacterID);
                var targetObject_l = ai.game.findObjectByID(targetID);
                if (speakerObject != null) {
                    if (targetObject_l.length == 1) {
                        var relations = ai.spatialRelations(targetID, speakerCharacterID);
                        if (relations != null && relations.length > 0) {
                            var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + relations[relations.length - 1].name + "(" + targetTermString + ",'" + speakerCharacterID + "'[#id])))";
                            var term = Term.fromString(tmp, ai.o);
                            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                            return;
                        }
                    }
                    else if (targetObject_l[0].ID == ai.selfID) {
                        var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],verb.have('" + ai.selfID + "'[#id], " + targetTermString + ")))";
                        var term = Term.fromString(tmp, ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
                    else if (targetObject_l[0].ID == speakerCharacterID) {
                        var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],verb.have('" + speakerCharacterID + "'[#id], " + targetTermString + ")))";
                        var term = Term.fromString(tmp, ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
                    else {
                        console.warn("executeInferenceEffect.answer_where: We cannot find target or speaker! " + targetID + ", " + speakerCharacterID);
                        var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + where_preposition + "(" + targetTermString + ",'" + targetLocationID + "'[#id])))";
                        var term = Term.fromString(tmp, ai.o);
                        ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                    }
                }
            }
            else {
                var speakerLocation_idx = ai.game.locations.indexOf(speakerLocation);
                var targetLocation_idx = ai.game.locations.indexOf(targetLocation);
                if (speakerLocation_idx >= 0 &&
                    targetLocation_idx >= 0 &&
                    (ai.game.location_in[speakerLocation_idx][targetLocation_idx] ||
                        speakerLocation_idx == targetLocation_idx)) {
                    // If the speakerLocation is in targetLocation: 
                    var speakerObject = ai.game.findObjectByIDJustObject(speakerCharacterID);
                    if (targetIfItsALocation != null &&
                        targetIfItsALocation != speakerLocation &&
                        speakerObject != null &&
                        !ai.game.location_in[speakerLocation_idx][ai.game.locations.indexOf(targetIfItsALocation)]) {
                        // if the object we are asking about is a location, and we are NOT in that location, then report directions:
                        var relations = ai.spatialRelationsFromLocation(targetIfItsALocation, speakerObject);
                        if (relations != null && relations.length > 0) {
                            var tmp_1 = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + relations[relations.length - 1].name + "(" + targetTermString + ",'" + speakerCharacterID + "'[#id])))";
                            var term_1 = Term.fromString(tmp_1, ai.o);
                            ai.intentions.push(new IntentionRecord(term_1, null, null, null, ai.timeStamp));
                            return;
                        }
                        // if we are here, we could not compute relations. We will attempt to say "it's outside of X"
                        // find the largest area such that:
                        // - speaker is in it
                        // - it is within targetlocation
                        // - targetIfItsALocation is not in it
                        var targetIfItsALocation_idx = ai.game.locations.indexOf(targetIfItsALocation);
                        var l = -1;
                        for (var i = 0; i < ai.game.locations.length; i++) {
                            if (ai.game.location_in[speakerLocation_idx][i] &&
                                ai.game.location_in[i][targetLocation_idx] &&
                                !ai.game.location_in[targetIfItsALocation_idx][i]) {
                                if (l == -1) {
                                    l = i;
                                }
                                else {
                                    if (ai.game.location_in[l][i]) {
                                        l = i;
                                    }
                                }
                            }
                        }
                        if (l != -1) {
                            var tmp_2 = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],#and(" + where_preposition + "(TARGET:" + targetTermString + ",'" + targetLocationID + "'[#id]), space.outside.of(TARGET, '" + ai.game.locations[l].id + "'[#id]) )))";
                            var term_2 = Term.fromString(tmp_2, ai.o);
                            ai.intentions.push(new IntentionRecord(term_2, null, null, null, ai.timeStamp));
                            return;
                        }
                    }
                    // otherwise just say where the target is:
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + where_preposition + "(" + targetTermString + ",'" + targetLocationID + "'[#id])))";
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                else {
                    // otherwise just say where the target is:
                    var tmp = "action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id]," + where_preposition + "(" + targetTermString + ",'" + targetLocationID + "'[#id])))";
                    var term = Term.fromString(tmp, ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
            }
        }
    };
    AnswerWhere_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerWhere_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\" whereto=\"" + this.whereto + "\"/>";
    };
    AnswerWhere_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        var wt = xml.getAttribute("whereto") == "true";
        return new AnswerWhere_InferenceEffect(t, wt);
    };
    return AnswerWhere_InferenceEffect;
}(InferenceEffect));
