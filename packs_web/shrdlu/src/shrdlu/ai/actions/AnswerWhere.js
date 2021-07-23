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
var AnswerWhere_IntentionAction = /** @class */ (function (_super) {
    __extends(AnswerWhere_IntentionAction, _super);
    function AnswerWhere_IntentionAction() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    AnswerWhere_IntentionAction.prototype.canHandle = function (intention, ai) {
        if (intention.functor.is_a(ai.o.getSort("action.answer.whereis")) ||
            intention.functor.is_a(ai.o.getSort("action.answer.whereto")))
            return true;
        return false;
    };
    AnswerWhere_IntentionAction.prototype.execute = function (ir, ai) {
        this.ir = ir;
        var intention = ir.action;
        //		let where_answer:number = INFERENCE_RECORD_EFFECT_ANSWER_WHEREIS;
        //		if (intention.functor == ai.o.getSort("action.answer.whereto")) where_answer = INFERENCE_RECORD_EFFECT_ANSWER_WHERETO;
        console.log("AnswerWhere.execute: " + intention);
        if (intention.attributes.length == 2) {
            if (intention.attributes[1] instanceof ConstantTermAttribute) {
                var targetID = intention.attributes[1].value;
                console.log(ai.selfID + " answer followup where is/to " + targetID);
                // this is a follow up question! see if we can reconstruct the question...
                var context = ai.contextForSpeakerWithoutCreatingANewOne(targetID);
                if (context != null) {
                    // get the last sentence we said (the last one that is not a follow up):
                    var lastPerf = null;
                    // we don't use "lastPerformativeBy", since that would just return the "where?"
                    for (var i = 1; i < context.performatives.length; i++) {
                        if (context.performatives[i].speaker == targetID &&
                            context.performatives[i].performative.attributes.length > 1) {
                            lastPerf = context.performatives[i];
                            break;
                        }
                    }
                    var newIntention = null;
                    if (lastPerf != null)
                        newIntention = this.convertPerformativeToWhereQuestionAnswerIntention(lastPerf, ai);
                    if (newIntention != null) {
                        // intention = newIntention;
                        ai.intentions.push(new IntentionRecord(newIntention, newIntention.attributes[1], null, null, ai.timeStamp));
                        ir.succeeded = true;
                        return true;
                    }
                    else {
                        // this should never happen
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id],#and(the(NOUN:'perf.question'[perf.question],S:[singular]),noun(NOUN,S))))))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                        ir.succeeded = false;
                        return true;
                    }
                }
                else {
                    // this should never happen
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id],#and(the(NOUN:'perf.question'[perf.question],S:[singular]),noun(NOUN,S))))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                    ir.succeeded = false;
                    return true;
                }
            }
            // console.log("AnswerWhere.execute (new intention): " + intention);
        }
        else if (intention.attributes.length == 4) {
            if (intention.attributes[1] instanceof ConstantTermAttribute) {
                var targetID = intention.attributes[1].value;
                console.log(ai.selfID + " answer followup where with constraint to " + targetID);
                // this is a follow up question! see if we can reconstruct the question...
                var context = ai.contextForSpeakerWithoutCreatingANewOne(targetID);
                if (context != null) {
                    // get the last sentence we said:
                    var lastPerf = null;
                    // we don't use "lastPerformativeBy", since that would just return the "where?"
                    for (var i = 1; i < context.performatives.length; i++) {
                        if (context.performatives[i].speaker == targetID) {
                            lastPerf = context.performatives[i];
                            break;
                        }
                    }
                    if ((lastPerf.performative.functor.name == "perf.q.whereis" ||
                        lastPerf.performative.functor.name == "perf.q.whereto")) {
                        if (lastPerf.performative.attributes.length == 2 &&
                            lastPerf.performative.attributes[1] instanceof ConstantTermAttribute) {
                            // insert the missing subject of the where:
                            intention.attributes.splice(2, 0, lastPerf.performative.attributes[1]);
                        }
                        else if (lastPerf.performative.attributes.length == 4 &&
                            lastPerf.performative.attributes[3] instanceof TermTermAttribute) {
                            // insert the missing subject of the where:
                            intention.attributes.splice(2, 0, lastPerf.performative.attributes[1]);
                            // add additional query terms:
                            var queryTermsList = Term.elementsInList(lastPerf.performative.attributes[3].term, "#and");
                            for (var _i = 0, queryTermsList_1 = queryTermsList; _i < queryTermsList_1.length; _i++) {
                                var queryTerm = queryTermsList_1[_i];
                                if (queryTerm instanceof TermTermAttribute) {
                                    intention.attributes[4] = new TermTermAttribute(new Term(ai.o.getSort("#and"), [queryTerm, intention.attributes[4]]));
                                }
                            }
                        }
                        else {
                            // this should never happen
                            var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id],#and(the(NOUN:'perf.question'[perf.question],S:[singular]),noun(NOUN,S))))))", ai.o);
                            ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                            ir.succeeded = false;
                            return true;
                        }
                    }
                    else {
                        // this should never happen
                        var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id],#and(the(NOUN:'perf.question'[perf.question],S:[singular]),noun(NOUN,S))))))", ai.o);
                        ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                        ir.succeeded = false;
                        return true;
                    }
                }
                else {
                    // this should never happen
                    var term = Term.fromString("action.talk('" + ai.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + ai.selfID + "'[#id],#and(the(NOUN:'perf.question'[perf.question],S:[singular]),noun(NOUN,S))))))", ai.o);
                    ai.intentions.push(new IntentionRecord(term, intention.attributes[1], null, null, ai.timeStamp));
                    ir.succeeded = false;
                    return true;
                }
            }
        }
        console.log(ai.selfID + " answer where: " + intention);
        if (intention.attributes[2] instanceof ConstantTermAttribute &&
            intention.attributes.length == 3) {
            // We look for a place different from "communicator-range", which we do not like as an answer...
            var comrange = new ConstantTermAttribute("communicator-range", ai.o.getSort("#id"));
            var where1 = new VariableTermAttribute(ai.o.getSort("#id"), "WHERE");
            var target1 = [new Sentence([new Term(ai.o.getSort("space.at"), [intention.attributes[2],
                        where1]),
                    new Term(ai.o.getSort("="), [where1, comrange])], [false, true])];
            var where2 = new VariableTermAttribute(ai.o.getSort("#id"), "WHERE");
            var target2 = [new Sentence([new Term(ai.o.getSort("space.at"), [intention.attributes[1],
                        where2]),
                    new Term(ai.o.getSort("="), [where2, comrange])], [false, true])];
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [target1, target2], 1, 0, false, null, new AnswerWhere_InferenceEffect(intention, intention.functor == ai.o.getSort("action.answer.whereto"))));
            ir.succeeded = true;
        }
        else if (intention.attributes.length >= 5 &&
            (intention.attributes[2] instanceof VariableTermAttribute) &&
            (intention.attributes[3] instanceof VariableTermAttribute) &&
            (intention.attributes[4] instanceof TermTermAttribute)) {
            // this is a question with a query inside, so, we need to trigger an inference procedure:
            var whoVariable = intention.attributes[2];
            var whereVariable = intention.attributes[3];
            whoVariable.name = "WHO";
            whereVariable.name = "WHERE";
            var additionalTermsTmp = Term.elementsInList((intention.attributes[4]).term, "#and");
            var target1Terms = [new Term(ai.o.getSort("space.at"), [whoVariable,
                    whereVariable])];
            var target1Signs = [false];
            for (var _a = 0, additionalTermsTmp_1 = additionalTermsTmp; _a < additionalTermsTmp_1.length; _a++) {
                var qtTmp = additionalTermsTmp_1[_a];
                if (qtTmp instanceof TermTermAttribute) {
                    var additionalTerm = qtTmp.term;
                    var additionalSign = false;
                    if (additionalTerm.functor.name == "#not") {
                        additionalTerm = (additionalTerm.attributes[0]).term;
                        additionalSign = true;
                    }
                    target1Terms.push(additionalTerm);
                    target1Signs.push(additionalSign);
                }
            }
            var target1 = [new Sentence(target1Terms, target1Signs)];
            console.log("target1: " + target1);
            var target2 = [new Sentence([new Term(ai.o.getSort("space.at"), [intention.attributes[1],
                        new VariableTermAttribute(ai.o.getSort("#id"), "WHERE")])], [false])];
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, [], [target1, target2], 1, 0, false, null, new AnswerWhere_InferenceEffect(intention, intention.functor == ai.o.getSort("action.answer.whereto"))));
            ir.succeeded = true;
        }
        else if (intention.attributes.length >= 5 &&
            (intention.attributes[2] instanceof ConstantTermAttribute) &&
            (intention.attributes[3] instanceof VariableTermAttribute) &&
            (intention.attributes[4] instanceof TermTermAttribute)) {
            // this is a question with a query inside, so, we need to trigger an inference procedure:
            var whoVariable = intention.attributes[2];
            var whereVariable = intention.attributes[3];
            whereVariable.name = "WHERE";
            var additionalTermsTmp = Term.elementsInList((intention.attributes[4]).term, "#and");
            var additionalTerms = [];
            var target1Terms = [new Term(ai.o.getSort("space.at"), [whoVariable,
                    whereVariable])];
            var target1Signs = [false];
            for (var _b = 0, additionalTermsTmp_2 = additionalTermsTmp; _b < additionalTermsTmp_2.length; _b++) {
                var qtTmp = additionalTermsTmp_2[_b];
                if (qtTmp instanceof TermTermAttribute) {
                    var additionalTerm = qtTmp.term;
                    var additionalSign = true;
                    if (additionalTerm.functor.name == "#not") {
                        additionalTerm = (additionalTerm.attributes[0]).term;
                        additionalSign = false;
                        if (additionalTerm.containsVariable(whereVariable)) {
                            // needs to be added to the query, otherwise, it's not going to work...
                            target1Terms.push(additionalTerm);
                            target1Signs.push(true);
                        }
                        else {
                            additionalTerms.push(new Sentence([additionalTerm], [additionalSign]));
                        }
                    }
                    else if (additionalTerm.functor.name == "#or") {
                        additionalTerms = additionalTerms.concat(Term.termToSentences(additionalTerm, ai.o));
                    }
                    else {
                        if (additionalTerm.containsVariable(whereVariable)) {
                            // needs to be added to the query, otherwise, it's not going to work...
                            target1Terms.push(additionalTerm);
                            target1Signs.push(false);
                        }
                        else {
                            additionalTerms.push(new Sentence([additionalTerm], [additionalSign]));
                        }
                    }
                }
            }
            var target1 = [new Sentence(target1Terms, target1Signs)];
            console.log("additionalTerms: " + additionalTerms);
            console.log("target1: " + target1);
            var target2 = [new Sentence([new Term(ai.o.getSort("space.at"), [intention.attributes[1],
                        new VariableTermAttribute(ai.o.getSort("#id"), "WHERE")])], [false])];
            //				console.log("target2: " + target2);
            ai.queuedInferenceProcesses.push(new InferenceRecord(ai, additionalTerms, [target1, target2], 1, 0, false, null, new AnswerWhere_InferenceEffect(intention, intention.functor == ai.o.getSort("action.answer.whereto"))));
            ir.succeeded = true;
        }
        else {
            console.error("executeIntention answer where: attribute[2] was not a ConstantTermAttribute nor a VariableTermAttribute: " + intention.attributes[2]);
        }
        ir.succeeded = true;
        return true;
    };
    AnswerWhere_IntentionAction.prototype.convertPerformativeToWhereQuestionAnswerIntention = function (nlcp, ai) {
        if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.predicate")) &&
            (nlcp.performative.attributes[1] instanceof TermTermAttribute)) {
            var predicate = nlcp.performative.attributes[1].term;
            var terms = NLParser.termsInList(predicate, "#and");
            console.log("convertPerformativeToWhereQuestionAnswerIntention: " + nlcp.performative);
            var objectTerms = [];
            var verbWithLocationTerms = [];
            //let spaceAtVariableTerms:Term[] = [];
            for (var _i = 0, terms_1 = terms; _i < terms_1.length; _i++) {
                var term = terms_1[_i];
                if (term.attributes.length == 1) {
                    if (term.functor.is_a(ai.o.getSort("object"))) {
                        objectTerms.push(term);
                    }
                }
                else if ((term.attributes.length == 3 || term.attributes.length == 2) &&
                    term.functor.is_a_string("verb-with-optional-location-3rd-argument")) {
                    verbWithLocationTerms.push(term);
                } /*else if (term.functor.name == "space.at" &&
                           (term.attributes[1] instanceof VariableTermAttribute)) {
                    spaceAtVariableTerms.push(term);
                }*/
            }
            console.log("objectTerms: " + objectTerms);
            console.log("verbWithLocationTerms: " + verbWithLocationTerms);
            //console.log("spaceAtVariableTerms: " + spaceAtVariableTerms);
            if (objectTerms.length == 1) {
                var newIntention = new Term(ai.o.getSort("action.answer.whereis"), [nlcp.performative.attributes[0],
                    new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                    objectTerms[0].attributes[0],
                    new VariableTermAttribute(ai.o.getSort("any"), null),
                    nlcp.performative.attributes[1]]);
                console.log("convertPerformativeToWhereQuestionAnswerIntention, newIntention: " + newIntention);
                return newIntention;
            }
            else if (verbWithLocationTerms.length == 1) {
                var query = predicate;
                var variable = null;
                if (verbWithLocationTerms[0].attributes.length == 3) {
                    variable = verbWithLocationTerms[0].attributes[2];
                }
                else {
                    variable = new VariableTermAttribute(ai.o.getSort("any"), null);
                    verbWithLocationTerms[0].attributes.push(variable);
                }
                var perfQuery = new Term(ai.o.getSort("perf.q.query"), [nlcp.performative.attributes[0],
                    variable,
                    new TermTermAttribute(query)]);
                var newIntention = new Term(ai.o.getSort("action.answer.query"), [nlcp.performative.attributes[0],
                    new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                    new TermTermAttribute(perfQuery)]);
                console.log("convertPerformativeToWhereQuestionAnswerIntention, newIntention: " + newIntention);
                return newIntention;
            } /*else if (spaceAtVariableTerms.length == 1) {
                let query:Term = predicate;
                let variable:TermAttribute = spaceAtVariableTerms[0].attributes[1];
                let perfQuery:Term = new Term(ai.o.getSort("perf.q.query"),
                                              [nlcp.performative.attributes[0],
                                               variable,
                                               new TermTermAttribute(query)]);
                let newIntention:Term = new Term(ai.o.getSort("action.answer.query"),
                                                 [nlcp.performative.attributes[0],
                                                  new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                                                  new TermTermAttribute(perfQuery)]);
                console.log("convertPerformativeToWhereQuestionAnswerIntention, newIntention: " + newIntention);
                return newIntention;
            }*/
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.whereis")) ||
            nlcp.performative.functor.is_a(ai.o.getSort("perf.q.whereto"))) {
            if (nlcp.performative.attributes.length == 2) {
                var newIntention = new Term(nlcp.performative.functor, [nlcp.performative.attributes[0],
                    new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                    nlcp.performative.attributes[1]]);
                console.log("convertPerformativeToWhereQuestionAnswerIntention, newIntention: " + newIntention);
                return newIntention;
            }
            else {
                // console.log("convertPerformativeToWhereQuestionAnswerIntention perf.q.whereis/perf.q.whereto: " + nlcp.performative);
                return nlcp.performative;
            }
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.query"))) {
            // ...
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.howmany"))) {
            // ...
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.moreresults"))) {
            // ...
        }
        else if (nlcp.performative.functor.is_a(ai.o.getSort("perf.q.action")) &&
            nlcp.performative.attributes.length >= 2 &&
            nlcp.performative.attributes[1] instanceof TermTermAttribute) {
            // perf.q.action(LISTENER_0:[any], V1:verb.see(LISTENER_0, QUERY_V_0:[any]), V3:rover(QUERY_V_0))
            // perf.q.whereis(V:'etaoin'[#id], V, L, space.at(L,'location-aurora-station'[#id]))
            var action = nlcp.performative.attributes[1].term;
            if (action.functor.is_a(ai.o.getSort("verb.see")) &&
                action.attributes.length == 2) {
                console.log("convertPerformativeToWhereQuestionAnswerIntention: perf.q.action(verb.see)");
                if (nlcp.performative.attributes.length == 2 &&
                    action.attributes[1] instanceof ConstantTermAttribute) {
                    var newIntention = new Term(ai.o.getSort("action.answer.whereis"), [nlcp.performative.attributes[0],
                        new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                        action.attributes[1]]);
                    console.log("convertPerformativeToWhereQuestionAnswerIntention, newIntention: " + newIntention);
                    return newIntention;
                }
                else if (nlcp.performative.attributes.length == 3 &&
                    action.attributes[1] instanceof VariableTermAttribute) {
                    var newIntention = new Term(ai.o.getSort("action.answer.whereis"), [nlcp.performative.attributes[0],
                        new ConstantTermAttribute(nlcp.speaker, ai.o.getSort("#id")),
                        action.attributes[1],
                        new VariableTermAttribute(ai.o.getSort("#id"), null),
                        nlcp.performative.attributes[2]]);
                    console.log("convertPerformativeToWhereQuestionAnswerIntention, newIntention: " + newIntention);
                    return newIntention;
                }
            }
        }
        return null;
    };
    AnswerWhere_IntentionAction.prototype.saveToXML = function (ai) {
        return "<IntentionAction type=\"AnswerWhere_IntentionAction\"/>";
    };
    AnswerWhere_IntentionAction.loadFromXML = function (xml, ai) {
        return new AnswerWhere_IntentionAction();
    };
    return AnswerWhere_IntentionAction;
}(IntentionAction));
