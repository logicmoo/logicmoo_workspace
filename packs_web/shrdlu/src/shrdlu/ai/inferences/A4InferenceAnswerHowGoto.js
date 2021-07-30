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
var AnswerHowGoto_InferenceEffect = /** @class */ (function (_super) {
    __extends(AnswerHowGoto_InferenceEffect, _super);
    function AnswerHowGoto_InferenceEffect(effectParameter) {
        var _this = _super.call(this) || this;
        _this.effectParameter = null;
        _this.effectParameter = effectParameter;
        return _this;
    }
    AnswerHowGoto_InferenceEffect.prototype.execute = function (inf, ai_raw) {
        var ai = ai_raw;
        console.log("AnswerHowGoto_InferenceEffect");
        console.log("inf.inferences.length: " + inf.inferences.length);
        console.log("inf.inferences[0].endResults: " + inf.inferences[0].endResults);
        if (!(this.effectParameter.attributes[1] instanceof ConstantTermAttribute)) {
            console.error("AnswerHowGoto_InferenceEffect.execute: Trying to talk to a character for which we don't know the ID!");
            return;
        }
        var speakerCharacterID = (this.effectParameter.attributes[1]).value;
        console.log("query result, answer how goto (source): " + inf.inferences[0].endResults);
        if (inf.inferences[0].endResults.length == 0) {
            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],'unknown'[symbol]))", ai.o);
            ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
        }
        else {
            // get the location ID
            var sourceLocation = null;
            var intention = this.effectParameter;
            var action = intention.attributes[2].term;
            if (inf.inferences[0].endResults.length != 0) {
                // get the location ID
                for (var _i = 0, _a = inf.inferences[0].endResults; _i < _a.length; _i++) {
                    var result = _a[_i];
                    var bindings = result.bindings;
                    for (var _b = 0, _c = bindings.l; _b < _c.length; _b++) {
                        var b = _c[_b];
                        if (b[0].name == "WHERE") {
                            var v = b[1];
                            if (v instanceof ConstantTermAttribute) {
                                // select the most specific one:
                                if (sourceLocation == null) {
                                    sourceLocation = ai.game.getAILocationByID(v.value);
                                }
                                else {
                                    var sourceLocation2 = ai.game.getAILocationByID(v.value);
                                    var idx1 = ai.game.locations.indexOf(sourceLocation);
                                    var idx2 = ai.game.locations.indexOf(sourceLocation2);
                                    if (idx1 >= 0 && idx2 >= 0 && ai.game.location_in[idx2][idx1]) {
                                        sourceLocation = sourceLocation2;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            var path = null;
            if (action.attributes[1] instanceof ConstantTermAttribute) {
                var destination = action.attributes[1];
                var targetLocation = ai.game.getAILocationByID(destination.value);
                if (sourceLocation != null && targetLocation != null) {
                    //						console.log(ai.generateAILocationDOTGraph());
                    if (action.functor.is_a(ai.o.getSort("verb.leave"))) {
                        path = ai.pathToGetOutOf(sourceLocation, targetLocation, false);
                    }
                    else {
                        path = ai.pathBetweenLocations(sourceLocation, targetLocation);
                    }
                }
            }
            else if (action.attributes[1] instanceof VariableTermAttribute) {
                var destinationSort = action.attributes[1].sort;
                if (destinationSort.is_a(ai.o.getSort("space.outside"))) {
                    path = ai.pathToGetOutOf(sourceLocation, sourceLocation, true);
                }
                else if (destinationSort.is_a(ai.o.getSort("space.there"))) {
                    var targetLocation = ai.resolveThere(speakerCharacterID, sourceLocation);
                    if (sourceLocation != null && targetLocation != null) {
                        //						console.log(ai.generateAILocationDOTGraph());
                        if (action.functor.is_a(ai.o.getSort("verb.leave"))) {
                            path = ai.pathToGetOutOf(sourceLocation, targetLocation, false);
                        }
                        else {
                            path = ai.pathBetweenLocations(sourceLocation, targetLocation);
                        }
                    }
                }
            }
            if (path == null) {
                //				let term:Term = Term.fromString("action.talk('"+ai.selfID+"'[#id], perf.inform.answer('"+speakerCharacterID+"'[#id],'unknown'[symbol]))", ai.o);
                //				ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                // we don't know! Default to a standard query, to see if we can figure it out that way:
                var action_1 = intention.attributes[2].term;
                console.log(ai.selfID + " answer how: " + intention.attributes[2]);
                // we add the sentence with positive sign, to see if it introduces a contradiction
                var target1 = [new Sentence([new Term(ai.o.getSort("relation.howto"), [new TermTermAttribute(action_1),
                            new VariableTermAttribute(ai.o.getSort("any"), "HOW")])], [false])];
                ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [target1], 1, 0, false, null, new AnswerHow_InferenceEffect(intention)));
            }
            else if (path.length == 1) {
                if (action.functor.is_a(ai.o.getSort("verb.leave"))) {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],#not(space.at(" + action.attributes[0] + "," + action.attributes[1] + "))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                else {
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.answer('" + speakerCharacterID + "'[#id],space.at(" + action.attributes[0] + "," + action.attributes[1] + ")))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
            }
            else {
                var answer = null;
                var needsSpacesuit = false;
                var stationIndoorsLocation = ai.game.getAILocationByID("location-aurora-station");
                var stationIndoorsLocationIdx = ai.game.locations.indexOf(stationIndoorsLocation);
                var startIndoors = ai.game.location_in[ai.game.locations.indexOf(path[0])][stationIndoorsLocationIdx];
                // we skip the very first one
                for (var i = path.length - 1; i > 0; i--) {
                    var locPrev = path[i - 1];
                    var loc = path[i];
                    var relations = ai.spatialRelationsLocationsAsNouns(loc, locPrev);
                    if (!needsSpacesuit && !ai.game.location_in[ai.game.locations.indexOf(loc)][stationIndoorsLocationIdx]) {
                        needsSpacesuit = true;
                    }
                    console.log(loc.name + " - " + loc.sort + " -> relations: " + relations);
                    var tmpTerm = new Term(ai.o.getSort("verb.go-to"), [action.attributes[0],
                        new ConstantTermAttribute(loc.id, ai.cache_sort_id)]);
                    if (relations != null && relations.length == 1) {
                        tmpTerm.functor = ai.o.getSort("verb.go");
                        tmpTerm.attributes = [tmpTerm.attributes[0],
                            new ConstantTermAttribute(relations[0].name, relations[0]),
                            tmpTerm.attributes[1]];
                    }
                    if (answer == null) {
                        answer = tmpTerm;
                    }
                    else {
                        answer = new Term(ai.o.getSort("time.subsequently"), [new TermTermAttribute(tmpTerm),
                            new TermTermAttribute(answer)]);
                    }
                }
                if (speakerCharacterID == action.attributes[0].value) {
                    var term = new Term(ai.o.getSort("action.talk"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                        new TermTermAttribute(new Term(ai.o.getSort("perf.inform.answer"), [new ConstantTermAttribute(speakerCharacterID, ai.cache_sort_id),
                            new TermTermAttribute(answer)]))]);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                else {
                    var term1 = new Term(ai.o.getSort("perf.request.action"), [action.attributes[0],
                        new TermTermAttribute(answer)]);
                    var term = new Term(ai.o.getSort("action.talk"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                        new TermTermAttribute(new Term(ai.o.getSort("perf.inform.answer"), [new ConstantTermAttribute(speakerCharacterID, ai.cache_sort_id),
                            new TermTermAttribute(new Term(ai.o.getSort("action.talk"), [new ConstantTermAttribute(speakerCharacterID, ai.cache_sort_id),
                                new TermTermAttribute(term1)]))]))]);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
                if (startIndoors && needsSpacesuit) {
                    var term = new Term(ai.o.getSort("action.talk"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                        new TermTermAttribute(Term.fromString("perf.inform.answer('" + speakerCharacterID + "'[#id],#and(X:verb.need(" + action.attributes[0] + ",[spacesuit]),time.future(X)))", ai.o))]);
                    ai.intentions.push(new IntentionRecord(term, null, null, null, ai.timeStamp));
                }
            }
        }
    };
    AnswerHowGoto_InferenceEffect.prototype.saveToXMLInternal = function (ai, variables, variableNames) {
        return "<InferenceEffect type=\"AnswerHowGoto_InferenceEffect\" effectParameter=\"" + this.effectParameter.toStringXMLInternal(variables, variableNames) + "\"/>";
    };
    AnswerHowGoto_InferenceEffect.loadFromXML = function (xml, ai, o, variables, variableNames) {
        var t = Term.fromStringInternal(xml.getAttribute("effectParameter"), o, variableNames, variables).term;
        return new AnswerHowGoto_InferenceEffect(t);
    };
    return AnswerHowGoto_InferenceEffect;
}(InferenceEffect));
