/*

FIXME: this class needs a refactoring BADLY. There is a significant amount of redundant code, and there are way too many special cases.

*/
var NLGenerator = /** @class */ (function () {
    function NLGenerator(o, pos) {
        this.o = null;
        this.pos = null;
        this.renderingSentenceVariables = null;
        this.nlg_cache_sort_id = null;
        this.nlg_cache_sort_symbol = null;
        this.nlg_cache_sort_number = null;
        this.nlg_cache_sort_name = null;
        this.nlg_cache_sort_object = null;
        this.nlg_cache_sort_role = null;
        this.nlg_cache_sort_unique_role = null;
        this.nlg_cache_sort_space_location = null;
        this.nlg_cache_sort_property = null;
        this.nlg_cache_sort_propertywithvalue = null;
        this.nlg_cache_sort_relationwithvalue = null;
        this.nlg_cache_sort_haveableproperty = null;
        this.nlg_cache_sort_haveablepropertywithvalue = null;
        this.nlg_cache_sort_relation = null;
        this.nlg_cache_sort_symmetric_relation = null;
        this.nlg_cache_sort_verb = null;
        this.nlg_cache_sort_verb_to_do = null;
        this.nlg_cache_sort_modal_verb = null;
        this.nlg_cache_sort_past = null;
        this.nlg_cache_sort_present = null;
        this.nlg_cache_sort_future = null;
        this.nlg_cache_sort_pronoun = null;
        this.nlg_cache_sort_timedate = null;
        this.nlg_cache_sort_measuringunit = null;
        this.consonants = "qwrtypsdfghjklzxcvbnmQWRTYPSDFGHJKLZXCVBNM";
        this.consonants_for_a_vs_an = "qwrtpsdfghjklzxcvbnmQWRTPSDFGJKLZXCVBNM012345679";
        this.o = o;
        this.pos = pos;
        // cache the sorts:
        this.nlg_cache_sort_id = o.getSort("#id");
        this.nlg_cache_sort_symbol = o.getSort("symbol");
        this.nlg_cache_sort_number = o.getSort("number");
        this.nlg_cache_sort_name = o.getSort("name");
        this.nlg_cache_sort_object = o.getSort("object");
        this.nlg_cache_sort_role = o.getSort("role");
        this.nlg_cache_sort_unique_role = o.getSort("unique-role");
        this.nlg_cache_sort_space_location = o.getSort("space.location");
        this.nlg_cache_sort_property = o.getSort("property");
        this.nlg_cache_sort_propertywithvalue = o.getSort("property-with-value");
        this.nlg_cache_sort_haveableproperty = o.getSort("haveable-property");
        this.nlg_cache_sort_haveablepropertywithvalue = o.getSort("haveable-property-with-value");
        this.nlg_cache_sort_relationwithvalue = o.getSort("relation-with-value");
        this.nlg_cache_sort_relation = o.getSort("relation");
        this.nlg_cache_sort_symmetric_relation = o.getSort("symmetric-relation");
        this.nlg_cache_sort_verb = o.getSort("verb");
        this.nlg_cache_sort_verb_to_do = o.getSort("verb.do");
        this.nlg_cache_sort_modal_verb = o.getSort("modal-verb");
        this.nlg_cache_sort_past = o.getSort("time.past");
        this.nlg_cache_sort_present = o.getSort("time.present");
        this.nlg_cache_sort_future = o.getSort("time.future");
        this.nlg_cache_sort_pronoun = o.getSort("pronoun");
        this.nlg_cache_sort_timedate = o.getSort("time.date");
        this.nlg_cache_sort_measuringunit = o.getSort("measuring-unit");
    }
    // listenerID will usually be null, if it is != null, the sentence will start with a reference to that listener, 
    // to help the listener know we are talking to her
    NLGenerator.prototype.termToEnglish = function (t, speakerID, listenerID, context) {
        var listenerPrefix = "";
        this.renderingSentenceVariables = null; // mark that we are NOT rendering a sentence with more than one #or-red term
        if (listenerID != null) {
            var targetString = this.termToEnglish_EntityName(listenerID, context);
            if (targetString != null) {
                // special case to prevent "hello man!", which sounds like "hello dude!"
                if (targetString == "man" || targetString == "woman")
                    targetString = "human";
                listenerPrefix = targetString + ", ";
            }
        }
        var render = this.termToEnglishInternal(t, speakerID, context, true);
        if (render == null)
            return null;
        return listenerPrefix + render;
    };
    NLGenerator.prototype.termToEnglishInternal = function (t, speakerID, context, rootPerformative) {
        var o = context.ai.o;
        if (!t.functor.is_a(o.getSort("performative"))) {
            console.error("termToEnglish, term is not a performative! " + t.toString());
            return "[NLG ERROR]";
        }
        if (t.functor.is_a(o.getSort("perf.callattention")))
            return this.termToEnglish_CallAttention(t, speakerID, context);
        if (t.functor.is_a(o.getSort("perf.greet")))
            return this.termToEnglish_Greet(t, speakerID, context);
        if (t.functor.is_a(o.getSort("perf.farewell")))
            return this.termToEnglish_Farewell(t, context);
        if (t.functor.is_a(o.getSort("perf.thankyou")))
            return "Thank you";
        if (t.functor.is_a(o.getSort("perf.youarewelcome")))
            return "You are welcome";
        if (t.functor.is_a(o.getSort("perf.nicetomeetyou")))
            return "Nice to meet you!";
        if (t.functor.is_a(o.getSort("perf.nicetomeetyoutoo")))
            return "Nice to meet you too!";
        if (t.functor.is_a(o.getSort("perf.ack.ok")))
            return this.termToEnglish_Ack_Ok(t);
        if (t.functor.is_a(o.getSort("perf.ack.unsure")))
            return this.termToEnglish_Ack_Unsure(t);
        if (t.functor.is_a(o.getSort("perf.ack.contradict")))
            return this.termToEnglish_Ack_Contradict(t);
        if (t.functor.is_a(o.getSort("perf.ack.invalidanswer")))
            return this.termToEnglish_Ack_InvalidAnswer(t);
        if (t.functor.is_a(o.getSort("perf.ack.denyrequest")))
            return this.termToEnglish_Ack_DenyRequest(t, context);
        if (t.functor.is_a(o.getSort("perf.ackresponse")))
            return "Ok";
        if (t.functor.is_a(o.getSort("perf.inform.answer")))
            return this.termToEnglish_Inform_Answer(t, speakerID, context);
        if (t.functor.is_a(o.getSort("perf.inform")))
            return this.termToEnglish_Inform(t, speakerID, context);
        if (t.functor.is_a(o.getSort("perf.sentiment")))
            return this.termToEnglish_Sentiment(t);
        if (t.functor.is_a(o.getSort("perf.q.action")))
            return this.termToEnglish_QuestionAction(t, speakerID, context);
        if (t.functor.is_a(o.getSort("perf.request.action")))
            return this.termToEnglish_RequestAction(t, speakerID, context, rootPerformative, true);
        if (t.functor.is_a(o.getSort("perf.q.predicate")))
            return this.termToEnglish_Q_Predicate(t, speakerID, context);
        if (t.functor.is_a(o.getSort("perf.q.query")))
            return this.termToEnglish_Query(t.attributes[1], t.attributes[2], speakerID, context);
        if (t.functor.is_a(o.getSort("perf.q.whereis")))
            return this.termToEnglish_Where(t.attributes[1], speakerID, context);
        if (t.functor.is_a(o.getSort("perf.q.how")))
            return this.termToEnglish_How(t, speakerID, context);
        // default case, just convert to string:
        console.error("termToEnglishInternal: could not render " + t.toString());
        return t.toString();
    };
    NLGenerator.prototype.capitalize = function (sentence) {
        if (sentence == null)
            return null;
        sentence = sentence.trim();
        if (sentence.length > 0) {
            sentence = sentence.substring(0, 1).toUpperCase() + sentence.substring(1);
        }
        sentence = sentence.split(" etaoin").join(" Etaoin");
        sentence = sentence.split(" qwerty").join(" Qwerty");
        sentence = sentence.split(" shrdlu").join(" Shrdlu");
        sentence = sentence.split(" aurora").join(" Aurora");
        return sentence;
    };
    NLGenerator.prototype.termToEnglish_CallAttention = function (t, speakerID, context) {
        //		let ai:RuleBasedAI = context.ai;
        var target = t.attributes[0];
        if (target instanceof ConstantTermAttribute) {
            var targetString = this.termToEnglish_EntityName(target, context);
            if (targetString != null) {
                // special case to prevent "hello man!", which sounds like "hello dude!"
                if (targetString == "man" || targetString == "woman")
                    targetString = "human";
                return targetString + "?";
            }
            else {
                console.error("termToEnglish_CallAttention: cannot render performative " + t);
                return null;
            }
        }
        else {
            console.error("termToEnglish_CallAttention: cannot render performative " + t);
            return null;
        }
    };
    NLGenerator.prototype.termToEnglish_Greet = function (t, speakerID, context) {
        //		let ai:RuleBasedAI = context.ai;
        var target = t.attributes[0];
        if (target instanceof ConstantTermAttribute) {
            var targetString = this.termToEnglish_EntityName(target, context);
            if (targetString != null) {
                // special case to prevent "hello man!", which sounds like "hello dude!"
                if (targetString == "man" || targetString == "woman")
                    targetString = "human";
                return "Hello " + targetString + "!";
            }
            else {
                return "Hello!";
            }
        }
        else {
            return "Hello!";
        }
    };
    NLGenerator.prototype.termToEnglish_Farewell = function (t, context) {
        //		let ai:RuleBasedAI = context.ai;
        var target = t.attributes[0];
        if (target instanceof ConstantTermAttribute) {
            var targetString = this.termToEnglish_EntityName(target, context);
            if (targetString != null) {
                // special case to prevent "hello man!", which sounds like "hello dude!"
                if (targetString == "man" || targetString == "woman")
                    targetString = "human";
                return "Farewell " + targetString + "!";
            }
            else {
                return "Farewell!";
            }
        }
        else {
            return "Farewell!";
        }
    };
    NLGenerator.prototype.termToEnglish_Ack_Ok = function (t) {
        return "Ok";
    };
    NLGenerator.prototype.termToEnglish_Ack_Unsure = function (t) {
        return "I am not sure";
    };
    NLGenerator.prototype.termToEnglish_Ack_Contradict = function (t) {
        return "That is a contradiction";
    };
    NLGenerator.prototype.termToEnglish_Ack_InvalidAnswer = function (t) {
        return "That does not answer my question";
    };
    NLGenerator.prototype.termToEnglish_Ack_DenyRequest = function (t, context) {
        if (t.attributes.length < 2) {
            return "I cannot do that";
        }
        else {
            var action = t.attributes[1];
            var actionSort = null;
            if (action instanceof ConstantTermAttribute) {
                actionSort = context.ai.o.getSortSilent(action.value);
            }
            else if (action instanceof VariableTermAttribute) {
                actionSort = action.sort;
            }
            var actionString = this.pos.getVerbString(actionSort, 0, 0, 0);
            if (actionString != null) {
                return "I cannot " + actionString + " that";
            }
            else {
                return "I cannot do that";
            }
        }
    };
    NLGenerator.prototype.termToEnglish_Sentiment = function (t) {
        if (t.attributes[1] instanceof ConstantTermAttribute) {
            var value = t.attributes[1].value;
            if (value == "good")
                return "Good!";
            else if (value == "bad")
                return "That's bad";
            else if (value == "surprise")
                return "Wow!";
            else
                return null;
        }
        else {
            return null;
        }
    };
    NLGenerator.prototype.termToEnglish_Inform_Answer = function (t, speakerID, context) {
        if (t.attributes[1] instanceof TermTermAttribute &&
            (t.attributes[1]).term.functor.name == "#and") {
            var o = context.ai.o;
            var t_l = Term.elementsInList((t.attributes[1]).term, "#and");
            var timeTerm = null;
            // console.log("t_l: " + t_l);
            // this "list" rendering format is only for when we want to generate text for lists of answers:
            for (var _i = 0, t_l_1 = t_l; _i < t_l_1.length; _i++) {
                var t2 = t_l_1[_i];
                if ((t2 instanceof TermTermAttribute) &&
                    (t2.term.functor.is_a(o.getSort("time.past")) ||
                        t2.term.functor.is_a(o.getSort("time.future")))) {
                    timeTerm = t2;
                    break;
                }
            }
            if (t_l.length > 1 && timeTerm == null) {
                var etcetera = false;
                var last_answer = t_l[t_l.length - 1];
                if ((last_answer instanceof ConstantTermAttribute) &&
                    last_answer.sort.name == "etcetera") {
                    etcetera = true;
                }
                var answers_l = [];
                var last_t = null;
                for (var _a = 0, t_l_2 = t_l; _a < t_l_2.length; _a++) {
                    var ta2 = t_l_2[_a];
                    if (etcetera && ta2 == last_answer) {
                        answers_l.push("...");
                    }
                    else {
                        var current_t = null;
                        if (ta2 instanceof TermTermAttribute)
                            current_t = ta2.term;
                        if (last_t != null && current_t != null &&
                            last_t.attributes.length >= 1 && current_t.attributes.length >= 1 &&
                            Term.equalsNoBindingsAttribute(last_t.attributes[0], current_t.attributes[0]) == 1) {
                            if (last_t.functor == current_t.functor &&
                                last_t.attributes.length == current_t.attributes.length &&
                                last_t.attributes.length == 2) {
                                // identical subject and functor, just render second attribute:
                                var t2 = new Term(o.getSort("perf.inform.answer"), [(t.attributes[1]).term.attributes[0], current_t.attributes[1]]);
                                var answer = this.termToEnglish_Inform_Answer(t2, speakerID, context);
                                if (answer == null)
                                    return null;
                                answers_l.push(answer);
                            }
                            else {
                                // identical subject:
                                var answer = null;
                                if (!current_t.functor.is_a(this.nlg_cache_sort_property) &&
                                    !current_t.functor.is_a(this.nlg_cache_sort_relation) &&
                                    current_t.functor.is_a(this.nlg_cache_sort_verb)) {
                                    answer = this.termToEnglish_Inform_Verb(current_t, [new TermTermAttribute(current_t)], false, speakerID, context, true);
                                }
                                else {
                                    var current_t_modified = current_t.clone([]);
                                    current_t_modified.attributes[0] = new VariableTermAttribute(o.getSort("any"), null);
                                    var t2 = new Term(o.getSort("perf.inform.answer"), [(t.attributes[1]).term.attributes[0], new TermTermAttribute(current_t_modified)]);
                                    answer = this.termToEnglish_Inform_Answer(t2, speakerID, context);
                                }
                                if (answer == null)
                                    return null;
                                answers_l.push(answer);
                            }
                        }
                        else {
                            var t2 = new Term(o.getSort("perf.inform.answer"), [t.attributes[0], ta2]);
                            var answer = this.termToEnglish_Inform_Answer(t2, speakerID, context);
                            if (answer == null)
                                return null;
                            answers_l.push(answer);
                        }
                        last_t = current_t;
                    }
                }
                var finalAnswer = null;
                for (var i = 0; i < answers_l.length; i++) {
                    if (finalAnswer == null) {
                        finalAnswer = answers_l[i];
                    }
                    else if (i < answers_l.length - 1) {
                        finalAnswer += ", " + answers_l[i];
                    }
                    else {
                        if (etcetera) {
                            finalAnswer += ", " + answers_l[i];
                        }
                        else {
                            finalAnswer += " and " + answers_l[i];
                        }
                    }
                }
                return finalAnswer;
            }
            // check to see if there is more than one verb, to handle them separately:
            if (timeTerm == null) {
                var verbs = [];
                var verbNegated = [];
                var nonVerbs = false;
                var etcetera = false;
                for (var _b = 0, t_l_3 = t_l; _b < t_l_3.length; _b++) {
                    var ta = t_l_3[_b];
                    if (ta instanceof TermTermAttribute) {
                        var ta_t = ta.term;
                        if (ta_t.functor.is_a(this.nlg_cache_sort_verb)) {
                            verbs.push(ta.term);
                            verbNegated.push(false);
                        }
                        else if (ta_t.functor.name == "#not" &&
                            ta_t.attributes.length == 1 &&
                            (ta_t.attributes[0] instanceof TermTermAttribute) &&
                            ta_t.attributes[0].term.functor.is_a(this.nlg_cache_sort_verb)) {
                            verbs.push(ta.term);
                            verbNegated.push(true);
                        }
                        else {
                            nonVerbs = true;
                        }
                    }
                    else {
                        if ((ta instanceof ConstantTermAttribute) &&
                            ta.value == "etcetera") {
                            etcetera = true;
                        }
                    }
                }
                if (verbs.length > 1 && !nonVerbs) {
                    // this is a list of verbs! (probably an answer with a list of actions):
                    var text = "";
                    for (var i = 0; i < verbs.length; i++) {
                        var v = verbs[i];
                        var vtext = this.termToEnglish_Inform_Verb(v, [new TermTermAttribute(v)], verbNegated[i], speakerID, context, false);
                        if (vtext == null)
                            return null;
                        if (i == 0) {
                            text = vtext;
                        }
                        else if (i == verbs.length - 1 && !etcetera) {
                            text = text + " and " + vtext;
                        }
                        else {
                            text = text + ", " + vtext;
                        }
                    }
                    if (etcetera)
                        text += ", ...";
                    return text;
                }
            }
        }
        if (t.attributes[1] instanceof ConstantTermAttribute) {
            var v = t.attributes[1].value;
            if (v == 'yes') {
                return "Yes";
            }
            else if (v == 'no') {
                return "No";
            }
            else if (v == 'unknown') {
                if (t.attributes.length >= 3 && t.attributes[2] instanceof TermTermAttribute) {
                    var query = t.attributes[2].term;
                    //					console.log("termToEnglish_Inform_Answer: query: " + query);
                    if (query.functor.is_a(context.ai.o.getSort("perf.question"))) {
                        var queryString = this.termToEnglishInternal(query, speakerID, context, false);
                        if (queryString != null && queryString[queryString.length - 1] == '?') {
                            return "I don't know " + queryString.substring(0, queryString.length - 1);
                        }
                        else {
                            return "I don't know";
                        }
                    }
                    else {
                        return "I don't know";
                    }
                }
                else {
                    return "I don't know";
                }
            }
            else if (v == 'no-matches-found') {
                return 'No matches found';
            }
            else if (v == 'nothing') {
                return 'Nothing';
            }
            else if (v == 'fine') {
                return 'I am fine';
            }
            else if (t.attributes[1].sort == this.nlg_cache_sort_id) {
                //				if (v == speakerID) return "you";
                //				if (v == context.selfID) return "me";
                return this.termToEnglish_Entity(t.attributes[1], speakerID, true, context, null, true)[0];
            }
            else if (t.attributes[1].sort.is_a(this.nlg_cache_sort_measuringunit)) {
                var unitStr = this.termToEnglish_MeasuringUnit((t.attributes[1]).value, t.attributes[1].sort);
                if (unitStr == null)
                    return null;
                return unitStr[0];
            }
            else if (t.attributes[1].sort == this.nlg_cache_sort_symbol) {
                return v;
            }
            else if (t.attributes[1].sort == this.nlg_cache_sort_number) {
                return '' + v;
            }
            else if (t.attributes[1].sort.name == v) {
                var v2 = this.pos.getTypeString(t.attributes[1].sort, 0);
                if (v2 == null)
                    v2 = this.pos.getNounString(t.attributes[1].sort, 0, false); // without trying ancestors
                if (v2 == null)
                    v2 = this.pos.getPropertyString(t.attributes[1].sort);
                if (v2 == null)
                    v2 = this.pos.getNounString(t.attributes[1].sort, 0, true); // we are despearte, try ancestors
                return v2;
            }
            else {
                console.error("termToEnglish_Inform_Answer: cannot render performative " + t);
                return t.toString();
            }
        }
        else if (t.attributes[1] instanceof TermTermAttribute) {
            var t2 = (t.attributes[1]).term;
            var negated_t = false;
            if (t2.functor.name == "#not" && t2.attributes.length == 1 &&
                t2.attributes[0] instanceof TermTermAttribute) {
                negated_t = true;
                t2 = t2.attributes[0].term;
            }
            if (t2.functor.is_a(this.nlg_cache_sort_relation) &&
                (t2.attributes[0] instanceof VariableTermAttribute) &&
                t2.attributes[0].sort.name == "any") {
                // special case of an answer of the type "in the bedroom":
                var relationStr = this.pos.getRelationString(t2.functor, true);
                var objectStr = this.termToEnglish_RelationArgument(t2.attributes[1], speakerID, true, context, true, ((t2.attributes[0] instanceof ConstantTermAttribute) ?
                    (t2.attributes[0]).value : null), true);
                if (objectStr != null) {
                    var relationsAggregateStr = "";
                    if (relationStr + " " + objectStr[0] == "of I" ||
                        relationStr + " " + objectStr[0] == "of me") {
                        relationsAggregateStr += " " + (negated_t ? "not " : "") + "mine";
                    }
                    else if (relationStr + " " + objectStr[0] == "of you") {
                        relationsAggregateStr += " " + (negated_t ? "not " : "") + "yours";
                    }
                    else {
                        if (negated_t) {
                            if (relationStr.substring(0, 8) == "that has") {
                                relationsAggregateStr += " that does not have" + relationStr.substring(8) + " " + objectStr[0];
                            }
                            else {
                                relationsAggregateStr += " " + relationStr + " " + objectStr[0];
                            }
                        }
                        else {
                            relationsAggregateStr += " " + (negated_t ? "not " : "") + relationStr + " " + objectStr[0];
                        }
                    }
                    relationsAggregateStr = relationsAggregateStr.trim();
                    // console.log("subjectStr: " + subjectStr +"\nverbStr: " + verbStr + "\nrelationStr: " + relationStr + "\nobjectStr: " + objectStr);
                    if (relationsAggregateStr != "")
                        return relationsAggregateStr;
                }
                else {
                    console.error("Could not render objectStr: " + t2.attributes[1]);
                }
            }
            return this.termToEnglish_Inform(t, speakerID, context);
        }
        else if (t.attributes[1] instanceof VariableTermAttribute) {
            var tmp = this.termToEnglish_ConceptEntity(t.attributes[1], speakerID, context);
            if (tmp != null)
                return tmp[0];
            return null;
        }
        else {
            return this.termToEnglish_Inform(t, speakerID, context);
        }
    };
    NLGenerator.prototype.termToEnglish_Inform = function (pt, speakerID, context) {
        //		let listenerID:string = null;
        var ai = context.ai;
        //		if (pt.attributes[0] instanceof ConstantTermAttribute) listenerID = (<ConstantTermAttribute>pt.attributes[0]).value;
        if (!(pt.attributes[1] instanceof TermTermAttribute)) {
            console.error("termToEnglish_Inform: could not render " + pt);
            return pt.toString();
        }
        var or_tl = Term.elementsInList(pt.attributes[1].term, "#or");
        if (or_tl.length > 1) {
            // we are trying to render a sentence, not just a term:
            return this.termToEnglish_Inform_ComplexSentence(or_tl, speakerID, context);
        }
        var tl = Term.elementsInList(pt.attributes[1].term, "#and");
        for (var _i = 0, tl_1 = tl; _i < tl_1.length; _i++) {
            var tmp_t = tl_1[_i];
            if (!(tmp_t instanceof TermTermAttribute)) {
                console.error("termToEnglish_Inform: could not render (one of the elements in the list is not a term) " + pt);
                return null;
            }
        }
        var t = tl[0].term; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
        var adverb_str = null;
        var adverb_str_term = null;
        var negated_t = false;
        if (t.functor.name == "#not" && t.attributes.length == 1 &&
            t.attributes[0] instanceof TermTermAttribute) {
            negated_t = true;
            t = t.attributes[0].term;
        }
        for (var _a = 0, tl_2 = tl; _a < tl_2.length; _a++) {
            var tmp_t = tl_2[_a];
            var tmp_t2 = tmp_t.term;
            if (tmp_t2 != t && tmp_t2.attributes.length == 1) {
                adverb_str = this.pos.getAdverbString(tmp_t2.functor);
                adverb_str_term = tmp_t2;
            }
        }
        // Look for the special case of "these is a X in Y", which is of the form #and(type(X), space.at(X, Y))
        if (tl.length == 2 &&
            (tl[0] instanceof TermTermAttribute) &&
            (tl[1] instanceof TermTermAttribute)) {
            var term1 = tl[0].term;
            var term2 = tl[1].term;
            if (term1.attributes.length == 1 &&
                term2.attributes.length == 2 &&
                term2.functor.name == "space.at" &&
                (term1.attributes[0] instanceof VariableTermAttribute) &&
                term1.attributes[0] == term2.attributes[0]) {
                // it's a "there is"!
                return this.termToEnglish_Inform_ThereIs(term1.functor, term2.attributes[1], speakerID, context);
            }
        }
        //		console.log("termToEnglish_Inform: " + t);
        if (POSParser.sortIsConsideredForTypes(t.functor, this.o) &&
            !(t.functor.is_a(this.nlg_cache_sort_propertywithvalue)) &&
            !(t.functor.is_a(this.nlg_cache_sort_relationwithvalue)) &&
            !(t.functor.is_a(this.nlg_cache_sort_haveableproperty)) &&
            !(t.functor.is_a(context.ai.o.getSort("time.subsequently")))) {
            //console.log("termToEnglish_Inform object, space_location, process, or role");
            return this.renderTypeStatement(pt.attributes[1], speakerID, context);
        }
        else if (t.functor.is_a(this.nlg_cache_sort_haveableproperty) && t.attributes.length == 1) {
            //console.log("termToEnglish_Inform nlg_cache_sort_haveableproperty");
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var verbStr = this.pos.getVerbString(ai.o.getSort("verb.have"), subjectStr[3], subjectStr[1], 3);
            var propertyStr = this.pos.getPropertyString(t.functor);
            if (verbStr != null && propertyStr != null) {
                if (adverb_str != null)
                    propertyStr = propertyStr + " " + adverb_str;
                if (negated_t) {
                    verbStr = this.pos.getVerbString(ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 3);
                    return subjectStr[0] + " " + verbStr + " not have " + propertyStr;
                }
                else {
                    return subjectStr[0] + " " + verbStr + " " + propertyStr;
                }
            }
        }
        else if (t.functor.is_a(this.nlg_cache_sort_property) && t.attributes.length == 1) {
            //			console.log("termToEnglish_Inform property");
            var complements = "";
            var time = this.nlg_cache_sort_present;
            var time_term = null;
            for (var _b = 0, tl_3 = tl; _b < tl_3.length; _b++) {
                var tmp_t2 = tl_3[_b];
                var t2 = tmp_t2.term;
                if (t2 == t)
                    continue;
                if (t2.functor.is_a(this.nlg_cache_sort_past) ||
                    t2.functor.is_a(this.nlg_cache_sort_present) ||
                    t2.functor.is_a(this.nlg_cache_sort_future)) {
                    if (t2.functor.is_a(this.nlg_cache_sort_past)) {
                        time = t2.functor;
                        time_term = t2;
                    }
                    if (t2.functor.is_a(this.nlg_cache_sort_present)) {
                        time = t2.functor;
                        time_term = t2;
                    }
                    if (t2.functor.is_a(this.nlg_cache_sort_future)) {
                        time = t2.functor;
                        time_term = t2;
                    }
                    // find if there is any other verb complement
                    if (t2.functor.name == "time.later")
                        complements += " later";
                    if (t2.functor.name == "time.first")
                        complements += " first";
                    if (t2.functor.name == "time.now")
                        complements += " now";
                    if (t2.functor.name == "time.subsequently")
                        complements += " after that";
                    if (t2.attributes.length == 2 &&
                        (t2.attributes[1] instanceof TermTermAttribute) &&
                        t2.attributes[1].sort.is_a_string("time.date")) {
                        var dateTerm = t2.attributes[1].term;
                        if (dateTerm.attributes.length >= 2) {
                            var time_date = this.termToEnglish_Date(dateTerm.attributes[0], dateTerm.attributes[1].sort);
                            if (time_date != null)
                                complements += " on " + time_date;
                        }
                    }
                }
            }
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var verbStr = this.verbStringWithTime(ai.o.getSort("verb.be"), subjectStr[3], subjectStr[1], time, false);
            var propertyStr = this.pos.getPropertyString(t.functor);
            if (propertyStr == null) {
                propertyStr = this.pos.getNounString(t.functor, 0, false); // without trying ancestors
                if (propertyStr != null)
                    propertyStr = "a " + propertyStr;
            }
            if (propertyStr == null) {
                propertyStr = this.pos.getNounString(t.functor, 0, true); // we are despearte, try ancestors
                if (propertyStr != null)
                    propertyStr = "a " + propertyStr;
            }
            if (adverb_str != null && adverb_str_term != time_term)
                complements = complements + " " + adverb_str;
            if (verbStr != null && propertyStr != null)
                return subjectStr[0] + " " + verbStr + " " + (negated_t ? "not " : "") + propertyStr + complements;
        }
        else if (t.functor.is_a(this.nlg_cache_sort_haveablepropertywithvalue) && t.attributes.length == 2) {
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, true, (t.attributes[0] instanceof ConstantTermAttribute ? (t.attributes[0]).value : null), false);
            var verbStr = this.pos.getVerbString(ai.o.getSort("verb.have"), subjectStr[3], subjectStr[1], 3);
            var propertyStr = this.pos.getPropertyString(t.functor);
            if (verbStr != null && propertyStr != null && objectStr != null) {
                if (propertyStr.substring(propertyStr.length - 3) == " to" &&
                    objectStr[0].substring(0, 3) == "to ") {
                    objectStr[0] = objectStr[0].substring(3);
                }
                if (adverb_str != null)
                    objectStr[0] = objectStr[0] + " " + adverb_str;
                if (negated_t) {
                    verbStr = this.pos.getVerbString(ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 3);
                    return subjectStr[0] + " " + verbStr + " not have " + propertyStr + " " + objectStr[0];
                }
                else {
                    return subjectStr[0] + " " + verbStr + " " + propertyStr + " " + objectStr[0];
                }
            }
        }
        else if (t.functor.is_a(this.nlg_cache_sort_role)) {
            var determiner = "a";
            var preComplementsStr = "";
            var time = this.nlg_cache_sort_present;
            var complementsStr = "";
            var typeFunctor = null;
            if (t.attributes.length == 3) {
                // role with a location, generate an initial complementsStr:
                //					console.log("role with a location, generate an initial complementsStr:!!!!");
                var roleLocationStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                    (t.attributes[0]).value : null), true);
                if (roleLocationStr != null) {
                    complementsStr = " in " + roleLocationStr[0];
                }
                typeFunctor = t.attributes[2].sort; // since this is of the form role(subject, [location], role))
            }
            else {
                typeFunctor = t.attributes[1].sort; // since this is of the form role(subject, role))
            }
            if (typeFunctor.is_a(this.nlg_cache_sort_unique_role))
                determiner = "the";
            // let relationStr:string = this.pos.getRelationString(typeFunctor, true);
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var verbStr = this.verbStringWithTime(ai.o.getSort("verb.be"), subjectStr[3], subjectStr[1], time, negated_t);
            var typeStr = this.pos.getTypeString(typeFunctor, subjectStr[3]);
            if (verbStr != null && typeStr != null) {
                if (determiner == "a")
                    determiner = this.aVSanArticle(preComplementsStr + typeStr);
                return subjectStr[0] + " " + verbStr + " " + determiner + " " + preComplementsStr + typeStr + complementsStr;
            }
            else {
                console.warn("(termToEnglish_Inform.nlg_cache_sort_role) subjectStr: " + subjectStr[0] + ", verbStr: " + verbStr + ", typeStr: " + typeStr + ", complementsStr: " + complementsStr);
                return null;
            }
        }
        else if (t.functor.is_a(this.nlg_cache_sort_propertywithvalue) && t.attributes.length == 2) {
            var useNameIfAvailable = true;
            if (t.functor.is_a_string("name"))
                useNameIfAvailable = false;
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, useNameIfAvailable);
            var verbStr = this.pos.getVerbString(ai.o.getSort("verb.be"), 0, 2, 3);
            var propertyStr = this.pos.getNounString(t.functor, 0, true);
            var propertyStr2 = null;
            if (t.attributes[1] instanceof ConstantTermAttribute &&
                t.attributes[1].sort.name != "symbol") {
                if (t.attributes[1].sort.is_a(this.nlg_cache_sort_measuringunit)) {
                    var unitStr = this.termToEnglish_MeasuringUnit((t.attributes[1]).value, t.attributes[1].sort);
                    if (unitStr != null)
                        propertyStr2 = unitStr[0];
                }
                else {
                    propertyStr2 = this.pos.getPropertyString(t.attributes[1].sort);
                }
            }
            else {
                var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                    (t.attributes[0]).value : null), true);
                if (objectStr != null)
                    propertyStr2 = objectStr[0];
            }
            if (subjectStr != null && verbStr != null && propertyStr != null && propertyStr2 != null) {
                if (adverb_str != null)
                    propertyStr2 = propertyStr2 + " " + adverb_str;
                if (subjectStr[0] == "I") {
                    return "my " + propertyStr + " " + verbStr + " " + (negated_t ? "not " : "") + propertyStr2;
                }
                else if (subjectStr[0] == "you") {
                    return "your " + propertyStr + " " + verbStr + " " + (negated_t ? "not " : "") + propertyStr2;
                }
                else {
                    return subjectStr[0] + "'s " + propertyStr + " " + verbStr + " " + (negated_t ? "not " : "") + propertyStr2;
                }
            }
            else {
                return t.toString();
            }
        }
        else if (t.functor.is_a(this.nlg_cache_sort_relationwithvalue)) {
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, false, null, true);
            var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, null, true);
            var valueStr = null;
            var relationStr = this.pos.getTypeString(t.functor, 0);
            if (t.attributes[2] instanceof ConstantTermAttribute &&
                t.attributes[2].sort.name != "symbol") {
                if (t.attributes[2].sort.is_a(this.nlg_cache_sort_measuringunit)) {
                    var unitStr = this.termToEnglish_MeasuringUnit((t.attributes[2]).value, t.attributes[2].sort);
                    if (unitStr != null)
                        valueStr = unitStr[0];
                }
                else {
                    valueStr = this.pos.getPropertyString(t.attributes[2].sort);
                }
            }
            else {
                var valueStrTmp = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                    (t.attributes[0]).value : null), true);
                if (valueStrTmp != null)
                    valueStr = valueStrTmp[0];
            }
            return "the " + relationStr + " between " + subjectStr[0] + " and " + objectStr[0] + " is " + valueStr;
        }
        else if (t.functor.is_a(context.ai.o.getSort("time.subsequently"))) {
            return this.termToEnglish_RequestAction(pt, speakerID, context, true, false);
        }
        else if (t.functor.is_a(this.nlg_cache_sort_relation) &&
            !t.functor.is_a(this.nlg_cache_sort_verb)) {
            // console.log("termToEnglish_Inform: relation " + t.functor.name);
            var subjectStr = void 0;
            if (t.functor.is_a(this.o.getSort("relation.howto")) /* ||
                t.functor.is_a(this.o.getSort("relation.cause"))*/) {
                // enable the use of variable names:
                if (this.renderingSentenceVariables == null)
                    this.renderingSentenceVariables = [];
                subjectStr = this.termToEnglish_RelationOrVerbArgument(t.attributes[0], speakerID, true, context, true, null, true, true);
            }
            else {
                subjectStr = this.termToEnglish_RelationArgument(t.attributes[0], speakerID, true, context, true, null, true);
            }
            // console.log("subjectStr: " + subjectStr);
            var time = this.nlg_cache_sort_present;
            if (subjectStr != null) {
                var relationsAggregateStr = "";
                var complementsStr = "";
                for (var _c = 0, tl_4 = tl; _c < tl_4.length; _c++) {
                    var tmp_t2 = tl_4[_c];
                    var t2 = tmp_t2.term;
                    var negated_t2 = false;
                    if (t2.functor.name == "#not" && t2.attributes.length == 1 &&
                        t2.attributes[0] instanceof TermTermAttribute) {
                        negated_t2 = true;
                        t2 = t2.attributes[0].term;
                    }
                    if (t2.attributes.length == 1 &&
                        (t2.attributes[0] instanceof TermTermAttribute) &&
                        (t2.attributes[0]).term == t) {
                        // find if there is any term that determines tense, otherwise, assume present:
                        if (t2.functor.is_a(this.nlg_cache_sort_past)) {
                            time = t2.functor;
                        }
                        if (t2.functor.is_a(this.nlg_cache_sort_future)) {
                            time = t2.functor;
                        }
                    }
                    if (t2.functor.is_a(this.nlg_cache_sort_relation) &&
                        !t2.functor.is_a(this.nlg_cache_sort_verb)) {
                        var typeFunctor = t2.functor;
                        var relationStr = void 0;
                        if (t.functor.is_a(this.o.getSort("relation.howto"))) {
                            relationStr = "";
                        }
                        else {
                            relationStr = this.pos.getRelationString(typeFunctor, true);
                        }
                        var objectStr = this.termToEnglish_RelationArgument(t2.attributes[1], speakerID, true, context, false, ((t2.attributes[0] instanceof ConstantTermAttribute) ?
                            (t2.attributes[0]).value : null), true);
                        //				console.log("termToEnglish_Inform: relationStr: " + relationStr);
                        //				console.log("termToEnglish_Inform: objectStr: " + objectStr);
                        if (objectStr == null)
                            return null;
                        if (relationStr == null) {
                            var reverseSortName = this.pos.reverseRelations[t2.functor.name];
                            if (reverseSortName != null) {
                                // we cannot render the original relation, but we can render the opposite, so reverse subject and object:
                                var reverseSort = ai.o.getSort(reverseSortName);
                                relationStr = this.pos.getRelationString(reverseSort, true);
                                var tmp2 = subjectStr;
                                subjectStr = objectStr;
                                objectStr = tmp2;
                            }
                        }
                        if (relationStr == "because" &&
                            !this.argumentIsVerb(t2.attributes[1], context))
                            relationStr = "because of";
                        if (relationStr + " " + objectStr[0] == "of I" ||
                            relationStr + " " + objectStr[0] == "of me") {
                            relationsAggregateStr += " " + (negated_t2 ? "not " : "") + "mine" + complementsStr;
                        }
                        else if (relationStr + " " + objectStr[0] == "of you") {
                            relationsAggregateStr += " " + (negated_t2 ? "not " : "") + "yours" + complementsStr;
                        }
                        else {
                            relationsAggregateStr += " " + (negated_t2 ? "not " : "") + relationStr + " " + objectStr[0] + complementsStr;
                        }
                    }
                }
                relationsAggregateStr = relationsAggregateStr.trim();
                if (t.functor.is_a(this.o.getSort("relation.cause"))) {
                    // special case:
                    return subjectStr[0] + " " + relationsAggregateStr;
                }
                else if (t.functor.is_a(this.o.getSort("relation.howto"))) {
                    // special case:
                    return "in order for " + subjectStr[0] + ", " + relationsAggregateStr;
                }
                else {
                    var verbStr = this.verbStringWithTime(ai.o.getSort("verb.be"), subjectStr[3], subjectStr[1], time, false);
                    if (verbStr != null && relationsAggregateStr != "")
                        return subjectStr[0] + " " + verbStr + " " + relationsAggregateStr;
                }
            }
        }
        else if (t.functor.is_a(this.nlg_cache_sort_verb)) {
            return this.termToEnglish_Inform_Verb(t, tl, negated_t, speakerID, context, false);
        }
        else if (t.functor.is_a(this.nlg_cache_sort_timedate)) {
            return this.termToEnglish_Date(t.attributes[0], t.attributes[1].sort);
        }
        console.error("termToEnglish_Inform: could not render " + pt);
        return null;
    };
    NLGenerator.prototype.termToEnglish_Inform_ThereIs = function (sort, location, speakerID, context) {
        var entityStr = this.termToEnglish_ConceptEntity(new VariableTermAttribute(sort, null), speakerID, context);
        if (location instanceof ConstantTermAttribute) {
            var locationStr = this.termToEnglish_Entity(location, speakerID, true, context, false, false);
            if (entityStr != null && locationStr != null) {
                return "there is " + entityStr[0] + " in " + locationStr[0];
            }
        }
        else if (location instanceof VariableTermAttribute) {
            var locationStr = this.pos.getAdverbString(location.sort);
            if (entityStr != null && locationStr != null) {
                return "there is " + entityStr[0] + " " + locationStr;
            }
        }
        return null;
    };
    NLGenerator.prototype.termToEnglish_Inform_Verb = function (t, tl, negated_t, speakerID, context, dropSubject) {
        var ai = context.ai;
        var verbPreComplements = "";
        var verbComplements = "";
        var time = null;
        for (var _i = 0, tl_5 = tl; _i < tl_5.length; _i++) {
            var tmp_t2 = tl_5[_i];
            var t2 = tmp_t2.term;
            if (t2 == t)
                continue;
            if (t2.functor.is_a(this.nlg_cache_sort_past) ||
                t2.functor.is_a(this.nlg_cache_sort_present) ||
                t2.functor.is_a(this.nlg_cache_sort_future)) {
                if (t2.functor.is_a(this.nlg_cache_sort_past)) {
                    time = t2.functor;
                }
                if (t2.functor.is_a(this.nlg_cache_sort_present)) {
                    time = t2.functor;
                }
                if (t2.functor.is_a(this.nlg_cache_sort_future)) {
                    time = t2.functor;
                }
                // find if there is any other verb complement
                if (t2.functor.name == "time.later")
                    verbComplements += " later";
                if (t2.functor.name == "time.first")
                    verbComplements += " first";
                if (t2.functor.name == "time.now")
                    verbComplements += " now";
                if (t2.functor.name == "time.subsequently")
                    verbComplements += " after that";
                if (t2.functor.name == "conjunction-contrast")
                    verbPreComplements += "however, ";
                if (t2.attributes.length == 2 &&
                    (t2.attributes[1] instanceof TermTermAttribute) &&
                    t2.attributes[1].sort.is_a_string("time.date")) {
                    var dateTerm = t2.attributes[1].term;
                    if (dateTerm.attributes.length >= 2) {
                        var time_date = this.termToEnglish_Date(dateTerm.attributes[0], dateTerm.attributes[1].sort);
                        if (time_date != null)
                            verbComplements += " on " + time_date;
                    }
                }
            }
            else if (t2.attributes.length == 1 &&
                (t2.attributes[0] instanceof TermTermAttribute) &&
                (t2.attributes[0]).term == t) {
                // find if there is any other verb complement
                if (t2.functor.name == "time.later")
                    verbComplements += " later";
                if (t2.functor.name == "time.first")
                    verbComplements += " first";
                if (t2.functor.name == "time.now")
                    verbComplements += " now";
                if (t2.functor.name == "conjunction-contrast")
                    verbPreComplements += "however, ";
            }
            else if (t2.attributes.length == 2 &&
                t2.functor.is_a(this.nlg_cache_sort_relation) &&
                (t2.attributes[0] instanceof TermTermAttribute) &&
                (t2.attributes[0]).term == t) {
                // render the relation as a verb complement:
                var relationStr = this.pos.getRelationString(t2.functor, true);
                var objectStr = this.termToEnglish_VerbArgument(t2.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                    (t.attributes[0]).value : null), true);
                if (relationStr == null &&
                    this.pos.reverseRelations[t2.functor.name] != null) {
                    relationStr = this.pos.getRelationString(ai.o.getSort(this.pos.reverseRelations[t2.functor.name]), true);
                }
                if (relationStr == "because" && !this.argumentIsVerb(t2.attributes[1], context))
                    relationStr = "because of";
                if (objectStr == null) {
                    console.warn("termToEnglish_Inform (verb): cannot render verb complement (objectStr == null) " + t2);
                }
                else if (relationStr == null) {
                    console.warn("termToEnglish_Inform (verb): cannot render verb complement (relationStr == null) " + t2);
                }
                else {
                    if (relationStr == "to" && objectStr[0].indexOf("to") == 0) {
                        verbComplements += " " + objectStr[0];
                    }
                    else {
                        verbComplements += " " + relationStr + " " + objectStr[0];
                    }
                }
            }
            else {
                console.warn("termToEnglish_Inform (verb): cannot render verb complement " + t2);
            }
        }
        if (time == null)
            time = this.nlg_cache_sort_present;
        if (t.attributes.length == 1) {
            if ((t.attributes[0] instanceof VariableTermAttribute) &&
                t.attributes[0].sort.name == "any") {
                // 0, 2 -> singular, 3rd person
                var verbStr = this.verbStringWithTime(t.functor, 0, 2, time, negated_t);
                return verbPreComplements + verbStr + verbComplements;
            }
            else {
                var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
                if (subjectStr == null) {
                    console.error("termToEnglish_Inform: subjectStr == null!");
                    return t.toString();
                }
                if (dropSubject) {
                    subjectStr[0] = "";
                }
                else {
                    subjectStr[0] += " ";
                }
                var verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], time, negated_t);
                return verbPreComplements + subjectStr[0] + verbStr + verbComplements;
            }
        }
        else if (t.attributes.length == 2) {
            if (t.attributes[0] instanceof VariableTermAttribute &&
                t.attributes[0].sort.name == "any") {
                // the subject of the verb is a variable, so, we are just going to render the verb in infinitive and the object
                var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                    (t.attributes[0]).value : null), true);
                if (objectStr == null || objectStr[0] == null) {
                    console.error("termToEnglish_Inform: objectStr == null or objectStr[0] == null! for " + t.attributes[1]);
                    return null;
                }
                if (objectStr[0][0] == 't' && objectStr[0][1] == 'o' && objectStr[0][2] == ' ' &&
                    t.functor.is_a(this.nlg_cache_sort_modal_verb))
                    objectStr[0] = objectStr[0].substring(3);
                //				console.log("Inform objectStr[0]: " + objectStr[0]);
                var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                return verbPreComplements + verbStr + " " + objectStr[0] + verbComplements;
            }
            else {
                var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
                var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                    (t.attributes[0]).value : null), true);
                if (subjectStr == null || objectStr == null) {
                    console.error("termToEnglish_Inform: subjectStr == null or objectStr == null!");
                    return null;
                }
                if (objectStr[0] == null) {
                    console.error("termToEnglish_Inform: objectStr[0] == null! for " + t.attributes[1]);
                    return null;
                }
                if (objectStr[0][0] == 't' && objectStr[0][1] == 'o' && objectStr[0][2] == ' ' &&
                    t.functor.is_a(this.nlg_cache_sort_modal_verb))
                    objectStr[0] = objectStr[0].substring(3);
                //				console.log("Inform objectStr[0]: " + objectStr[0]);
                var verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], time, negated_t);
                if (dropSubject) {
                    subjectStr[0] = "";
                }
                else {
                    subjectStr[0] += " ";
                }
                if (t.functor.name == "verb.happen") {
                    var verbStr_1 = this.verbStringWithTime(t.functor, subjectStr[3], 2, time, negated_t);
                    return (verbPreComplements + objectStr[0] + " " + verbStr_1 + " to " + subjectStr[0] + verbComplements).trim();
                }
                else {
                    return verbPreComplements + subjectStr[0] + verbStr + " " + objectStr[0] + verbComplements;
                }
            }
        }
        else if (t.attributes.length == 3 &&
            (t.functor.name == "verb.tell" ||
                t.functor.name == "action.talk" ||
                t.functor.name == "action.give" ||
                t.functor.name == "verb.go" ||
                t.functor.name == "verb.guide" ||
                t.functor.name == "verb.take-to" ||
                t.functor.name == "action.put-in" ||
                t.functor.name == "verb.bring" ||
                t.functor.name == "verb.help")) {
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var object1Str = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                (t.attributes[0]).value : null), true);
            var object2Str = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                (t.attributes[0]).value : null), true);
            var verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], time, negated_t);
            if (subjectStr != null && object1Str != null && object2Str != null) {
                if (dropSubject) {
                    subjectStr[0] = "";
                }
                else {
                    subjectStr[0] += " ";
                }
                if (t.functor.name == "verb.help" &&
                    (t.attributes[1] instanceof ConstantTermAttribute) &&
                    (t.attributes[2] instanceof TermTermAttribute) &&
                    t.attributes[2].term.attributes.length >= 1 &&
                    (t.attributes[2].term.attributes[0] instanceof ConstantTermAttribute)) {
                    var obj1 = t.attributes[1].value;
                    var obj2 = t.attributes[2].term.attributes[0].value;
                    if (obj1 == obj2) {
                        object2Str = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, obj1, true);
                    }
                    return subjectStr[0] + verbStr + " " + object1Str[0] + " " + object2Str[0] + verbComplements;
                }
                else if (t.functor.name == "verb.tell" ||
                    t.functor.name == "action.talk") {
                    return subjectStr[0] + verbStr + " " + object2Str[0] + " " + object1Str[0] + verbComplements;
                }
                else if (t.functor.name == "verb.take-to" ||
                    t.functor.name == "verb.bring") {
                    // verbStr = this.verbStringWithTime(this.o.getSort("action.take"), subjectStr[3], subjectStr[1], time, negated_t);
                    verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], time, negated_t);
                    return subjectStr[0] + verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0];
                }
                else if (t.functor.name == "action.put-in") {
                    verbStr = this.verbStringWithTime(ai.o.getSort("verb.put"), subjectStr[3], subjectStr[1], time, negated_t);
                    return subjectStr[0] + verbStr + " " + object1Str[0] + verbComplements + " in " + object2Str[0];
                }
                else {
                    return subjectStr[0] + verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0];
                }
            }
        }
        console.error("termToEnglish_Inform_verb: could not render " + t);
        return null;
    };
    NLGenerator.prototype.termToEnglish_Q_Predicate = function (pt, speakerID, context) {
        //		let listenerID:string = null;
        var ai = context.ai;
        //		if (pt.attributes[0] instanceof ConstantTermAttribute) listenerID = (<ConstantTermAttribute>pt.attributes[0]).value;
        if (!(pt.attributes[1] instanceof TermTermAttribute)) {
            console.error("termToEnglish_Q_Predicate: could not render " + pt);
            return pt.toString();
        }
        var tl = Term.elementsInList(pt.attributes[1].term, "#and");
        for (var _i = 0, tl_6 = tl; _i < tl_6.length; _i++) {
            var tmp_t = tl_6[_i];
            if (!(tmp_t instanceof TermTermAttribute)) {
                console.error("termToEnglish_Q_Predicate: could not render (one of the elements in the list is not a term) " + pt);
                return null;
            }
        }
        var t = tl[0].term; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
        if (t.functor.is_a(ai.o.getSort("verb"))) {
            // find if there is any term that determines tense, otherwise, assume present:
            var time = this.nlg_cache_sort_present;
            for (var _a = 0, tl_7 = tl; _a < tl_7.length; _a++) {
                var tmp_t2 = tl_7[_a];
                var t2 = tmp_t2.term;
                if (t2.attributes.length == 1 &&
                    (t2.attributes[0] instanceof TermTermAttribute) &&
                    (t2.attributes[0]).term == t) {
                    if (t2.functor.is_a(this.nlg_cache_sort_past))
                        time = t2.functor;
                    if (t2.functor.is_a(this.nlg_cache_sort_present))
                        time = t2.functor;
                    if (t2.functor.is_a(this.nlg_cache_sort_future))
                        time = t2.functor;
                }
            }
            if (t.functor.is_a(ai.o.getSort("verb.be")) && t.attributes.length == 2) {
                // ...
            }
            else if (t.attributes.length == 1) {
                var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
                var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                if (time.is_a(this.nlg_cache_sort_past)) {
                    var doVerbStr = this.pos.getVerbString(ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 4);
                    return doVerbStr + " " + subjectStr[0] + " " + verbStr + "?";
                }
                else if (time.is_a(this.nlg_cache_sort_future)) {
                    return "Will " + subjectStr[0] + " " + verbStr + "?";
                }
                else {
                    var doVerbStr = this.pos.getVerbString(ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 3);
                    return doVerbStr + " " + subjectStr[0] + " " + verbStr + "?";
                }
            }
            else if (t.attributes.length == 2) {
                var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
                var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                    (t.attributes[0]).value : null), true);
                var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                if (time.is_a(this.nlg_cache_sort_past)) {
                    var doVerbStr = this.pos.getVerbString(ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 4);
                    return doVerbStr + " " + subjectStr[0] + " " + verbStr + " " + objectStr[0] + "?";
                }
                else if (time.is_a(this.nlg_cache_sort_future)) {
                    return "Will " + subjectStr[0] + " " + verbStr + " " + objectStr[0] + "?";
                }
                else {
                    var doVerbStr = this.pos.getVerbString(ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 3);
                    return doVerbStr + " " + subjectStr[0] + " " + verbStr + " " + objectStr[0] + "?";
                }
            }
            else {
                // ...
            }
        }
        else if (t.functor.is_a(ai.o.getSort("haveable-property-with-value")) && t.attributes.length == 2) {
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, true, (t.attributes[0] instanceof ConstantTermAttribute ? (t.attributes[0]).value : null), false);
            var verbStr = this.pos.getVerbString(ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 3);
            var propertyStr = this.pos.getPropertyString(t.functor);
            if (verbStr != null && propertyStr != null) {
                //console.log("haveable-property-with-value:")
                //console.log("    subjectStr: " + subjectStr[0])
                //console.log("    objectStr: " + objectStr[0])
                if (propertyStr.substring(propertyStr.length - 3) == " to" &&
                    objectStr[0].substring(0, 3) == "to ") {
                    objectStr[0] = objectStr[0].substring(3);
                }
                return verbStr + " " + subjectStr[0] + " have " + propertyStr + " " + objectStr[0] + "?";
            }
        }
        else if (t.functor.is_a(ai.o.getSort("relation")) && t.attributes.length == 2) {
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, true, null, false);
            var verbStr = this.pos.getVerbString(ai.o.getSort("verb.be"), subjectStr[3], subjectStr[1], 3);
            var relationStr = this.pos.getRelationString(t.functor, true);
            if (verbStr != null && relationStr != null) {
                return verbStr + " " + subjectStr[0] + " " + relationStr + " " + objectStr[0] + "?";
            }
        }
        console.error("termToEnglish_Q_Predicate: could not render " + pt);
        return t.toString();
    };
    NLGenerator.prototype.termToEnglish_RequestAction = function (pt, speakerID, context, rootPerformative, addPlease) {
        var listenerID = null;
        //		let ai:RuleBasedAI = context.ai;
        if (pt.attributes[0] instanceof ConstantTermAttribute) {
            listenerID = pt.attributes[0].value;
        }
        if (!(pt.attributes[1] instanceof TermTermAttribute)) {
            console.error("termToEnglish_RequestAction: could not render " + pt);
            return pt.toString();
        }
        var actionSequence = Term.elementsInList(pt.attributes[1].term, "time.subsequently");
        var actionSequenceStrings = [];
        for (var _i = 0, actionSequence_1 = actionSequence; _i < actionSequence_1.length; _i++) {
            var actionAtt = actionSequence_1[_i];
            if (!(actionAtt instanceof TermTermAttribute)) {
                console.error("termToEnglish_RequestAction: could not render (one of the actions in the sequence is not a term) " + pt);
                return null;
            }
            var action = actionAtt.term;
            var actionString = null;
            var tl = Term.elementsInList(action, "#and");
            for (var _a = 0, tl_8 = tl; _a < tl_8.length; _a++) {
                var tmp_t = tl_8[_a];
                if (!(tmp_t instanceof TermTermAttribute)) {
                    console.error("termToEnglish_RequestAction: could not render (one of the elements in the list is not a term) " + pt);
                    return null;
                }
            }
            var t = tl[0].term; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
            var negated = false;
            if (t.functor.name == "#not") {
                negated = true;
                t = (t.attributes[0]).term;
            }
            if (!t.functor.is_a(this.nlg_cache_sort_verb)) {
                console.error("termToEnglish_RequestAction: could not render (main clause's functor is not a verb) " + t);
                return null;
            }
            if (!(t.attributes[0] instanceof ConstantTermAttribute) ||
                (t.attributes[0]).value != listenerID) {
                console.error("termToEnglish_RequestAction: could not render (subject of request (" + (t.attributes[0]).value + ") is different from listener (" + listenerID + ")) " + pt);
                return null;
            }
            var verbComplements = "";
            for (var _b = 0, tl_9 = tl; _b < tl_9.length; _b++) {
                var tmp_t2 = tl_9[_b];
                var t2 = tmp_t2.term;
                if (t2 == t)
                    continue;
                if (t2.attributes.length == 1 &&
                    (t2.attributes[0] instanceof TermTermAttribute) &&
                    (t2.attributes[0]).term == t) {
                    // find if there is any other verb complement
                    if (t2.functor.name == "time.later")
                        verbComplements += " later";
                    if (t2.functor.name == "time.first")
                        verbComplements += " first";
                    if (t2.functor.name == "time.now")
                        verbComplements += " now";
                    if (t2.functor.name == "time.subsequently")
                        verbComplements += " after that";
                }
                else if (t2.attributes.length == 2 &&
                    t2.functor.is_a(this.nlg_cache_sort_relation) &&
                    (t2.attributes[0] instanceof TermTermAttribute) &&
                    (t2.attributes[0]).term == t) {
                    // render the relation as a verb complement:
                    var relationStr = this.pos.getRelationString(t2.functor, true);
                    var objectStr = this.termToEnglish_VerbArgument(t2.attributes[1], speakerID, true, context, false, listenerID, true);
                    if (relationStr == "to" && objectStr[0].indexOf("to") == 0) {
                        verbComplements += " " + objectStr[0];
                    }
                    else {
                        verbComplements += " " + relationStr + " " + objectStr[0];
                    }
                }
                else {
                    console.warn("termToEnglish_RequestAction (verb): cannot render verb complement " + t2);
                }
            }
            if (t.attributes.length == 1) {
                var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                if (negated) {
                    if (rootPerformative) {
                        actionString = "do not " + verbStr + verbComplements;
                    }
                    else {
                        actionString = "not to " + verbStr + verbComplements;
                    }
                }
                else {
                    if (rootPerformative) {
                        actionString = verbStr + verbComplements;
                    }
                    else {
                        actionString = "to " + verbStr + verbComplements;
                    }
                }
            }
            else if (t.attributes.length == 2) {
                if (t.functor.is_a(context.ai.o.getSort("action.talk"))) {
                    var whatToSay = t.attributes[1];
                    if (whatToSay instanceof TermTermAttribute &&
                        whatToSay.term.functor.is_a(context.ai.o.getSort("performative"))) {
                        var perf = whatToSay.term;
                        var targetStr = this.termToEnglish_VerbArgument(perf.attributes[0], speakerID, true, context, false, listenerID, true);
                        var perfText = this.termToEnglishInternal(perf, speakerID, context, false);
                        if (targetStr != null && perfText != null) {
                            if (negated) {
                                if (rootPerformative) {
                                    actionString = "do not tell " + targetStr[0] + " " + perfText + verbComplements;
                                }
                                else {
                                    actionString = "not to tell " + targetStr[0] + " " + perfText + verbComplements;
                                }
                            }
                            else {
                                if (rootPerformative) {
                                    actionString = "tell " + targetStr[0] + " " + perfText + verbComplements;
                                }
                                else {
                                    actionString = "to tell " + targetStr[0] + " " + perfText + verbComplements;
                                }
                            }
                        }
                    }
                    else {
                        var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, listenerID, true);
                        if (objectStr != null) {
                            if (negated) {
                                if (rootPerformative) {
                                    actionString = "do not say " + objectStr[0] + verbComplements;
                                }
                                else {
                                    actionString = "not to say " + objectStr[0] + verbComplements;
                                }
                            }
                            else {
                                if (rootPerformative) {
                                    actionString = "say " + objectStr[0] + verbComplements;
                                }
                                else {
                                    actionString = "to say " + objectStr[0] + verbComplements;
                                }
                            }
                        }
                    }
                }
                else {
                    var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                    var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, listenerID, true);
                    if (objectStr != null) {
                        if (negated) {
                            if (rootPerformative) {
                                actionString = "do not " + verbStr + " " + objectStr[0] + verbComplements;
                            }
                            else {
                                actionString = "not to " + verbStr + " " + objectStr[0] + verbComplements;
                            }
                        }
                        else {
                            if (rootPerformative) {
                                actionString = verbStr + " " + objectStr[0] + verbComplements;
                            }
                            else {
                                actionString = "to " + verbStr + " " + objectStr[0] + verbComplements;
                            }
                        }
                    }
                }
            }
            else if (t.attributes.length == 3 &&
                (t.functor.name == "verb.tell" ||
                    t.functor.name == "action.talk" ||
                    t.functor.name == "action.give" ||
                    t.functor.name == "verb.go" ||
                    t.functor.name == "verb.take-to" ||
                    t.functor.name == "verb.bring")) {
                var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                var object1Str = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, listenerID, true);
                var object2Str = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, listenerID, true);
                if (object1Str != null && object2Str != null) {
                    if (negated) {
                        if (rootPerformative) {
                            actionString = "do not " + verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0];
                        }
                        else {
                            actionString = "not to " + verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0];
                        }
                    }
                    else {
                        if (rootPerformative) {
                            actionString = verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0];
                        }
                        else {
                            actionString = "to " + verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0];
                        }
                    }
                }
            }
            else {
                // ...
            }
            if (actionString == null) {
                console.error("termToEnglish_RequestAction: could not render " + pt);
                return null;
            }
            actionSequenceStrings.push(actionString);
        }
        var str = "";
        var first = true;
        for (var _c = 0, actionSequenceStrings_1 = actionSequenceStrings; _c < actionSequenceStrings_1.length; _c++) {
            var as = actionSequenceStrings_1[_c];
            if (first) {
                first = false;
                if (rootPerformative && addPlease) {
                    str = "please, " + as;
                }
                else {
                    str = as;
                }
            }
            else {
                str += ", then " + as;
            }
        }
        return str;
    };
    NLGenerator.prototype.termToEnglish_QuestionAction = function (pt, speakerID, context) {
        var listenerID = null;
        //		let ai:RuleBasedAI = context.ai;
        if (pt.attributes[0] instanceof ConstantTermAttribute) {
            listenerID = pt.attributes[0].value;
        }
        if (!(pt.attributes[1] instanceof TermTermAttribute)) {
            console.error("termToEnglish_QuestionAction: could not render " + pt);
            return pt.toString();
        }
        var tl = Term.elementsInList(pt.attributes[1].term, "#and");
        for (var _i = 0, tl_10 = tl; _i < tl_10.length; _i++) {
            var tmp_t = tl_10[_i];
            if (!(tmp_t instanceof TermTermAttribute)) {
                console.error("termToEnglish_QuestionAction: could not render (one of the elements in the list is not a term) " + pt);
                return null;
            }
        }
        var t = tl[0].term; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
        //		let negated:boolean = false;
        if (t.functor.name == "#not") {
            //			negated = true;
            t = (t.attributes[0]).term;
        }
        if (!t.functor.is_a(this.nlg_cache_sort_verb)) {
            console.error("termToEnglish_QuestionAction: could not render (main clause's functor is not a verb) " + pt);
            return pt.toString();
        }
        if (!(t.attributes[0] instanceof ConstantTermAttribute) ||
            (t.attributes[0]).value != listenerID) {
            console.error("termToEnglish_QuestionAction: could not render (subject of request (" + (t.attributes[0]).value + ") is different from listener (" + listenerID + ")) " + pt);
            return pt.toString();
        }
        var verbComplements = "";
        for (var _a = 0, tl_11 = tl; _a < tl_11.length; _a++) {
            var tmp_t2 = tl_11[_a];
            var t2 = tmp_t2.term;
            if (t2 == t)
                continue;
            if (t2.attributes.length == 1 &&
                (t2.attributes[0] instanceof TermTermAttribute) &&
                (t2.attributes[0]).term == t) {
                // find if there is any other verb complement
                if (t2.functor.name == "time.later")
                    verbComplements += " later";
                if (t2.functor.name == "time.first")
                    verbComplements += " first";
                if (t2.functor.name == "time.now")
                    verbComplements += " now";
                if (t2.functor.name == "time.subsequently")
                    verbComplements += " after that";
            }
            else if (t2.attributes.length == 2 &&
                t2.functor.is_a(this.nlg_cache_sort_relation) &&
                (t2.attributes[0] instanceof TermTermAttribute) &&
                (t2.attributes[0]).term == t) {
                // render the relation as a verb complement:
                var relationStr = this.pos.getRelationString(t2.functor, true);
                var objectStr = this.termToEnglish_VerbArgument(t2.attributes[1], speakerID, true, context, false, null, true);
                verbComplements += " " + relationStr + " " + objectStr[0];
            }
            else {
                console.warn("termToEnglish_QuestionAction (verb): cannot render verb complement " + t2);
            }
        }
        if (t.attributes.length == 1) {
            var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
            return "would you please " + verbStr + verbComplements + "?";
        }
        else if (t.attributes.length == 2) {
            var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
            var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, listenerID, true);
            if (objectStr != null) {
                return "would you please " + verbStr + " " + objectStr[0] + verbComplements + "?";
            }
        }
        else if (t.attributes.length == 3) {
            var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
            if (t.functor.name == "verb.help" &&
                (t.attributes[1] instanceof ConstantTermAttribute)) {
                // help special case (we assume the second argument is the subject of the 3rd, which is a verb):
                var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, listenerID, true);
                var objectStr2 = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, t.attributes[1].value, true);
                if (objectStr != null && objectStr2 != null) {
                    return "would you please " + verbStr + " " + objectStr[0] + " " + objectStr2[0] + verbComplements + "?";
                }
            }
            else {
                var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, listenerID, true);
                var objectStr2 = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, listenerID, true);
                if (objectStr != null && objectStr2 != null) {
                    return "would you please " + verbStr + " " + objectStr[0] + " " + objectStr2[0] + verbComplements + "?";
                }
            }
        }
        console.error("termToEnglish_QuestionAction: could not render " + pt);
        return t.toString();
    };
    NLGenerator.prototype.renderTypeStatement = function (pt, speakerID, context) {
        var ai = context.ai;
        //		if (pt.attributes[0] instanceof ConstantTermAttribute) listenerID = (<ConstantTermAttribute>pt.attributes[0]).value;
        if (!(pt instanceof TermTermAttribute)) {
            console.error("renderTypeStatement: could not render " + pt);
            return pt.toString();
        }
        var tl = Term.elementsInList(pt.term, "#and");
        for (var _i = 0, tl_12 = tl; _i < tl_12.length; _i++) {
            var tmp_t = tl_12[_i];
            if (!(tmp_t instanceof TermTermAttribute)) {
                console.error("renderTypeStatement: could not render (one of the elements in the list is not a term) " + pt);
                return null;
            }
        }
        var t = tl[0].term; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
        var original_t = t;
        var negated_t = false;
        if (t.functor.name == "#not" && t.attributes.length == 1 &&
            t.attributes[0] instanceof TermTermAttribute) {
            negated_t = true;
            t = t.attributes[0].term;
        }
        if (POSParser.sortIsConsideredForTypes(t.functor, this.o) &&
            !(t.functor.is_a(this.nlg_cache_sort_propertywithvalue)) &&
            !(t.functor.is_a(this.nlg_cache_sort_relationwithvalue)) &&
            !(t.functor.is_a(this.nlg_cache_sort_haveableproperty))) {
            // find complements:
            var typeFunctor = t.functor;
            var determiner = "a";
            var preComplementsStr = "";
            var complementsStr = "";
            var subjectPreComplementsStr = "";
            var subjectPostComplementsStr = "";
            var time = this.nlg_cache_sort_present;
            //			if (negated_t) subjectPreComplementsStr = "not";
            for (var _a = 0, tl_13 = tl; _a < tl_13.length; _a++) {
                var tmp_t2 = tl_13[_a];
                var t2 = tmp_t2.term;
                if (t2 != original_t) {
                    if (t2.attributes.length >= 1 &&
                        t2.attributes[0] instanceof TermTermAttribute &&
                        (t2.attributes[0]).term == t) {
                        if (t2.functor.is_a(this.nlg_cache_sort_past) ||
                            t2.functor.is_a(this.nlg_cache_sort_present) ||
                            t2.functor.is_a(this.nlg_cache_sort_future)) {
                            time = t2.functor;
                        }
                        else if (t2.functor.is_a(this.nlg_cache_sort_propertywithvalue) &&
                            t2.attributes.length == 2 &&
                            t2.attributes[1] instanceof ConstantTermAttribute) {
                            // let propertyStr:string = this.pos.getPropertyString(t2.attributes[1].sort);
                            // ???
                        }
                        else if (t2.functor.is_a(this.nlg_cache_sort_property) &&
                            t2.attributes.length == 1) {
                            var propertyStr = this.pos.getPropertyString(t2.functor);
                            preComplementsStr += propertyStr + " ";
                        }
                        else if (t2.functor.is_a(this.nlg_cache_sort_relation)) {
                            var relationStr = this.pos.getRelationString(t2.functor, true);
                            var objectStr = this.termToEnglish_RelationArgument(t2.attributes[1], speakerID, true, context, false, ((t2.attributes[0] instanceof ConstantTermAttribute) ?
                                (t2.attributes[0]).value : null), true);
                            if (relationStr != null) {
                                // console.log("subjectStr: " + subjectStr +"\nverbStr: " + verbStr + "\nrelationStr: " + relationStr + "\nobjectStr: " + objectStr);
                                complementsStr += " " + relationStr + " " + objectStr[0];
                            }
                            else {
                                console.error("renderTypeStatement: cannot render relation " + t2 + " as a complement");
                            }
                        }
                        else {
                            console.error("renderTypeStatement: cannot render term " + t2 + " as a complement");
                        }
                    }
                    else if (t2.attributes.length >= 1 &&
                        t2.attributes[0] == t.attributes[0]) {
                        if (t2.functor.is_a(this.nlg_cache_sort_propertywithvalue) &&
                            t2.attributes.length == 2 &&
                            t2.attributes[1] instanceof ConstantTermAttribute) {
                            var propertyStr = this.pos.getPropertyString(t2.attributes[1].sort);
                            subjectPreComplementsStr += propertyStr + " ";
                        }
                        else if (t2.functor.is_a(this.nlg_cache_sort_property) &&
                            t2.attributes.length == 1) {
                            var propertyStr = this.pos.getPropertyString(t2.functor);
                            subjectPreComplementsStr += propertyStr + " ";
                        }
                        else {
                            console.error("renderTypeStatement: cannot render term " + t2 + " as a subject complement");
                        }
                    }
                    else {
                        console.error("renderTypeStatement: cannot render term(2) " + t2 + " as a complement");
                    }
                }
            }
            var subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, null, true);
            var verbStr = this.verbStringWithTime(ai.o.getSort("verb.be"), subjectStr[3], subjectStr[1], time, negated_t);
            var typeStr = this.pos.getTypeString(typeFunctor, subjectStr[3]);
            if (typeStr == null)
                typeStr = this.pos.getNounString(typeFunctor, 0, false); // without trying ancestors
            if (typeStr == null)
                typeStr = this.pos.getPropertyString(typeFunctor);
            if (typeStr == null)
                typeStr = this.pos.getNounString(typeFunctor, 0, true); // we are despearte, try ancestors
            if (verbStr != null && typeStr != null) {
                if (determiner == "a")
                    determiner = this.aVSanArticle(preComplementsStr + typeStr);
                return subjectPreComplementsStr + subjectStr[0] + subjectPostComplementsStr + " " + verbStr + " " + determiner + " " + preComplementsStr + typeStr + complementsStr;
            }
            else {
                console.log("subjectStr: " + subjectStr[0] + ", verbStr: " + verbStr + ", typeStr: " + typeStr + ", complementsStr: " + complementsStr);
            }
        }
        return null;
    };
    NLGenerator.prototype.argumentIsVerb = function (entityRaw, context) {
        var tl = [entityRaw];
        if (entityRaw instanceof TermTermAttribute) {
            if (entityRaw.term.functor.name == "#and") {
                tl = Term.elementsInList(entityRaw.term, "#and");
            }
        }
        var entity = null;
        // first search for a "noun":
        for (var i = 0; i < tl.length; i++) {
            var entity_tmp = tl[i];
            if (entity_tmp instanceof TermTermAttribute) {
                var entityTerm = entity_tmp.term;
                if (entityTerm.functor.name == "#not" &&
                    entityTerm.attributes.length == 1 &&
                    (entityTerm.attributes[0] instanceof TermTermAttribute)) {
                    entity_tmp = entityTerm.attributes[0];
                    entityTerm = entityTerm.attributes[0].term;
                }
                if (entityTerm.functor.is_a(context.ai.o.getSort("noun"))) {
                    entity = entity_tmp;
                    break;
                }
            }
        }
        if (entity == null) {
            for (var i = 0; i < tl.length; i++) {
                entity = tl[i];
                var next = false;
                if (entity instanceof TermTermAttribute) {
                    var entityTerm = entity.term;
                    if (entityTerm.functor.name == "#not" &&
                        entityTerm.attributes.length == 1 &&
                        (entityTerm.attributes[0] instanceof TermTermAttribute)) {
                        entity = entityTerm.attributes[0];
                        entityTerm = entityTerm.attributes[0].term;
                    }
                    if (entityTerm.functor.is_a(context.ai.o.getSort("determiner")))
                        next = true;
                    if (entityTerm.functor.is_a(context.ai.o.getSort("adjective")))
                        next = true;
                }
                if (!next)
                    break; // we found it!
            }
        }
        if (entity instanceof TermTermAttribute) {
            var entity_term = entity.term; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
            //console.log("argumentIsVerb: " + entity_term.functor.name);
            if (entity_term.functor.is_a(this.nlg_cache_sort_verb))
                return true;
        }
        return false;
    };
    NLGenerator.prototype.termToEnglish_VerbArgument = function (entityRaw, speakerID, considerRelations, context, subject, mainVerbSubjectID, useNameIfAvailable) {
        return this.termToEnglish_RelationOrVerbArgument(entityRaw, speakerID, considerRelations, context, subject, mainVerbSubjectID, useNameIfAvailable, true);
    };
    NLGenerator.prototype.termToEnglish_RelationArgument = function (entityRaw, speakerID, considerRelations, context, subject, mainVerbSubjectID, useNameIfAvailable) {
        return this.termToEnglish_RelationOrVerbArgument(entityRaw, speakerID, considerRelations, context, subject, mainVerbSubjectID, useNameIfAvailable, false);
    };
    /*
    - This returns: [rendered string, person (0 = first, 1 = second, 2 = third), gender ('male', 'female'), and number (0 = singular, 1 = plural)]
    */
    NLGenerator.prototype.termToEnglish_RelationOrVerbArgument = function (entityRaw, speakerID, considerRelations, context, subject, mainVerbSubjectID, useNameIfAvailable, useInfinitives) {
        var tl = [entityRaw];
        if (entityRaw instanceof TermTermAttribute) {
            if (entityRaw.term.functor.name == "#or") {
                tl = Term.elementsInList(entityRaw.term, "#or");
                if (tl.length > 1) {
                    // we are trying to render a sentence, not just a term:
                    return [this.termToEnglish_Inform_ComplexSentence(tl, speakerID, context), 2, null, 0];
                }
            }
            if (tl[0].term.functor.name == "#and") {
                tl = Term.elementsInList(tl[0].term, "#and");
            }
        }
        var entity = null;
        var entityTerm = null;
        // first search for a "noun":
        for (var i = 0; i < tl.length; i++) {
            var entity_tmp = tl[i];
            if (entity_tmp instanceof TermTermAttribute) {
                entityTerm = entity_tmp.term;
                if (entityTerm.functor.is_a(context.ai.o.getSort("noun"))) {
                    entity = entity_tmp;
                    break;
                }
            }
        }
        if (entity == null) {
            for (var i = 0; i < tl.length; i++) {
                entity = tl[i];
                var next = false;
                if (entity instanceof TermTermAttribute) {
                    entityTerm = entity.term;
                    if (entityTerm.functor.is_a(context.ai.o.getSort("determiner")))
                        next = true;
                    if (entityTerm.functor.is_a(context.ai.o.getSort("adjective")))
                        next = true;
                }
                if (!next)
                    break; // we found it!
            }
        }
        if (entity instanceof ConstantTermAttribute) {
            if (entity.sort == this.nlg_cache_sort_id) {
                return this.termToEnglish_Entity(entity, speakerID, considerRelations, context, subject, useNameIfAvailable);
            }
            else if (entity.sort.is_a(this.nlg_cache_sort_measuringunit)) {
                return this.termToEnglish_MeasuringUnit(entity.value, entity.sort);
            }
            else if (entity.sort.is_a(this.nlg_cache_sort_verb)) {
                return [this.pos.getVerbString(entity.sort, 0, 0, 0), 2, null, 0];
            }
            else if (entity.sort.name == "pronoun.anything") {
                return ["anything", 2, undefined, 0];
            }
            else if (entity.sort.name == "pronoun.something") {
                return ["something", 2, undefined, 0];
            }
            else if (entity.sort.name == "symbol") {
                return [entity.value, 2, null, 0];
            }
            else {
                var num = 0;
                for (var _i = 0, tl_14 = tl; _i < tl_14.length; _i++) {
                    var t = tl_14[_i];
                    if (t == entity)
                        continue;
                    if (t instanceof TermTermAttribute) {
                        var t_term = t.term;
                        if (t_term.functor.is_a(context.ai.o.getSort("plural")))
                            num = 1;
                        else if (t_term.functor.is_a(context.ai.o.getSort("singular")))
                            num = 0;
                    }
                }
                var nounStr = this.pos.getNounString(entity.sort, num, false);
                if (nounStr != null) {
                    return [nounStr, 2, null, 0];
                }
                else {
                    return [entity.value, 2, null, 0];
                }
            }
        }
        if ((entity instanceof VariableTermAttribute) ||
            (entity instanceof TermTermAttribute &&
                entity.term.functor.name == "noun"))
            return this.termToEnglish_ConceptEntity(entityRaw, speakerID, context);
        if ((entity instanceof TermTermAttribute &&
            entity.term.functor.name == "proper-noun")) {
            var properNounTerm = entity.term;
            if (properNounTerm.attributes[0] instanceof ConstantTermAttribute) {
                return [(properNounTerm.attributes[0]).value, 0, null, 2];
            }
        }
        if (entity instanceof TermTermAttribute) {
            var negated_t = false;
            //			console.log("termToEnglish_RelationOrVerbArgument: TermTermAttribute -> " + entity);
            for (var _a = 0, tl_15 = tl; _a < tl_15.length; _a++) {
                var tmp_t = tl_15[_a];
                if (!(tmp_t instanceof TermTermAttribute)) {
                    console.error("termToEnglish_RelationArgument: could not render (one of the elements in the list is not a term) " + entity);
                    return null;
                }
            }
            //		 	entityTerm = (<TermTermAttribute>tl[0]).term;	// ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
            if (entityTerm.functor.name == "#not") {
                entityTerm = entityTerm.attributes[0].term;
                negated_t = true;
            }
            if (entityTerm.functor.name == "#query" && entityTerm.attributes.length == 1)
                return this.termToEnglish_QueryInternal(entityTerm.attributes[0], tl.slice(1), speakerID, context);
            if (entityTerm.functor.is_a(this.nlg_cache_sort_verb))
                return this.termToEnglish_NestedVerb(entityRaw.term, speakerID, mainVerbSubjectID, useInfinitives, context);
            if (entityTerm.functor.is_a(this.nlg_cache_sort_timedate))
                return [this.termToEnglish_Date(entityTerm.attributes[0], entityTerm.attributes[1].sort), 2, undefined, 0];
            if (entityTerm.functor.is_a(this.nlg_cache_sort_pronoun) &&
                entityTerm.attributes.length >= 2 &&
                entityTerm.attributes[0] instanceof ConstantTermAttribute) {
                var pronoun = entityTerm.attributes[0].value;
                var num = (entityTerm.attributes[1].sort.name == 'plural' ? 1 : 0);
                var gender = 0;
                if (entityTerm.attributes[2].sort.name == 'gender-femenine')
                    gender = 1;
                if (entityTerm.attributes[2].sort.name == 'gender-neutral')
                    gender = 2;
                return [this.pos.getPronounStringString(pronoun, num, gender), 2, undefined, num];
            }
            if (entityTerm.functor.is_a(this.nlg_cache_sort_property) && entityTerm.attributes.length == 1) {
                return this.termToEnglish_Property(entityRaw, speakerID, !subject, context);
            }
            if (entityTerm.functor.is_a(this.nlg_cache_sort_relation) && entityTerm.attributes.length == 2) {
                return this.termToEnglish_Relation(entityRaw, speakerID, context);
            }
            if (entityTerm.functor.is_a(this.nlg_cache_sort_haveablepropertywithvalue) && entityTerm.attributes.length == 2) {
                var infinitive = false;
                var subjectStr = null;
                var subjectId = null;
                if ((entityTerm.attributes[0] instanceof ConstantTermAttribute)) {
                    subjectId = (entityTerm.attributes[0]).value;
                    if (entityTerm.attributes[0].value == mainVerbSubjectID) {
                        infinitive = true;
                    }
                    else {
                        subjectStr = this.termToEnglish_VerbArgument(entityTerm.attributes[0], speakerID, true, context, true, null, true);
                    }
                }
                else {
                    subjectStr = this.termToEnglish_VerbArgument(entityTerm.attributes[0], speakerID, true, context, true, null, true);
                }
                var objectStr = this.termToEnglish_VerbArgument(entityTerm.attributes[1], speakerID, true, context, true, subjectId, false);
                var verbStr = null;
                if (infinitive) {
                    verbStr = this.pos.getVerbString(context.ai.o.getSort("verb.have"), 0, 0, 0);
                }
                else {
                    verbStr = this.pos.getVerbString(context.ai.o.getSort("verb.have"), subjectStr[3], subjectStr[1], 3);
                }
                var propertyStr = this.pos.getPropertyString(entityTerm.functor);
                if (verbStr != null && propertyStr != null) {
                    if (propertyStr.substring(propertyStr.length - 3) == " to" &&
                        objectStr[0].substring(0, 3) == "to ") {
                        objectStr[0] = objectStr[0].substring(3);
                    }
                    if (infinitive) {
                        if (negated_t) {
                            return ["not to " + verbStr + " " + propertyStr + " " + objectStr[0], 2, undefined, 0];
                        }
                        else {
                            return ["to " + verbStr + " " + propertyStr + " " + objectStr[0], 2, undefined, 0];
                        }
                    }
                    else {
                        if (negated_t) {
                            verbStr = this.pos.getVerbString(context.ai.o.getSort("verb.do"), subjectStr[3], subjectStr[1], 3);
                            return [subjectStr[0] + " " + verbStr + " not have " + propertyStr + " " + objectStr[0], 2, undefined, 0];
                        }
                        else {
                            return [subjectStr[0] + " " + verbStr + " " + propertyStr + " " + objectStr[0], 2, undefined, 0];
                        }
                    }
                }
                console.warn("termToEnglish_RelationOrVerbArgument: cannot render haveable property with value: " + entityTerm);
                return null;
            }
            if (entityTerm.functor.is_a(this.nlg_cache_sort_propertywithvalue) && entityTerm.attributes.length == 2) {
                var subjectStr = this.termToEnglish_RelationArgument(entityTerm.attributes[0], speakerID, true, context, true, null, true);
                var verbStr = this.pos.getVerbString(context.ai.o.getSort("verb.be"), 0, 2, 3);
                var propertyStr = this.pos.getNounString(entityTerm.functor, 0, true);
                var propertyStr2 = null;
                if (entityTerm.attributes[1] instanceof ConstantTermAttribute &&
                    entityTerm.attributes[1].sort.name != "symbol") {
                    if (entityTerm.attributes[1].sort.is_a(this.nlg_cache_sort_measuringunit)) {
                        var unitStr = this.termToEnglish_MeasuringUnit((entityTerm.attributes[1]).value, entityTerm.attributes[1].sort);
                        if (unitStr != null)
                            propertyStr2 = unitStr[0];
                    }
                    else {
                        propertyStr2 = this.pos.getPropertyString(entityTerm.attributes[1].sort);
                        if (propertyStr2 == null) {
                            propertyStr2 = this.pos.getNounString(entityTerm.functor, 0, true);
                        }
                    }
                }
                else {
                    var objectStr = this.termToEnglish_RelationArgument(entityTerm.attributes[1], speakerID, true, context, false, ((entityTerm.attributes[0] instanceof ConstantTermAttribute) ?
                        (entityTerm.attributes[0]).value : null), true);
                    if (objectStr != null)
                        propertyStr2 = objectStr[0];
                }
                if (subjectStr != null && verbStr != null && propertyStr != null && propertyStr2 != null) {
                    var negated_t_1 = false; // TODO: find the proper value for this
                    if (subjectStr[0] == "I") {
                        return ["my " + propertyStr + " " + verbStr + " " + (negated_t_1 ? "not " : "") + propertyStr2, 2, undefined, 0];
                    }
                    else if (subjectStr[0] == "you") {
                        return ["your " + propertyStr + " " + verbStr + " " + (negated_t_1 ? "not " : "") + propertyStr2, 2, undefined, 0];
                    }
                    else {
                        return [subjectStr[0] + "'s " + propertyStr + " " + verbStr + " " + (negated_t_1 ? "not " : "") + propertyStr2, 2, undefined, 0];
                    }
                }
                else {
                    console.error("termToEnglish_RelationOrVerbArgument: could not render property with value!");
                }
                return [this.termToEnglish_Inform(new Term(context.ai.o.getSort("perf.inform"), [new ConstantTermAttribute(speakerID, context.ai.o.getSort("#id")), entityRaw]), speakerID, context), 2, undefined, 0];
            }
            if ((entityTerm.functor.is_a_string("role") ||
                entityTerm.functor.is_a_string("relative-direction")) &&
                entityTerm.attributes.length == 3) {
                // special case for "role":
                var subjectStr = this.termToEnglish_RelationArgument(entityTerm.attributes[0], speakerID, true, context, true, null, true);
                var verbStr = this.pos.getVerbString(context.ai.o.getSort("verb.be"), 0, 2, 3);
                var propertyStr = this.pos.getNounString(entityTerm.functor, 0, true);
                var propertyStr2 = null;
                var propertyStr3 = null;
                if (entityTerm.attributes[1] instanceof ConstantTermAttribute &&
                    entityTerm.attributes[1].sort.name != "symbol") {
                    if (entityTerm.attributes[1].sort.is_a(this.nlg_cache_sort_measuringunit)) {
                        var unitStr = this.termToEnglish_MeasuringUnit((entityTerm.attributes[1]).value, entityTerm.attributes[1].sort);
                        if (unitStr != null)
                            propertyStr2 = unitStr[0];
                    }
                    else {
                        propertyStr2 = this.pos.getPropertyString(entityTerm.attributes[1].sort);
                    }
                }
                else {
                    var objectStr = this.termToEnglish_RelationArgument(entityTerm.attributes[1], speakerID, true, context, false, ((entityTerm.attributes[0] instanceof ConstantTermAttribute) ?
                        (entityTerm.attributes[0]).value : null), true);
                    if (objectStr != null)
                        propertyStr2 = objectStr[0];
                }
                var objectStr2 = this.termToEnglish_RelationArgument(entityTerm.attributes[2], speakerID, true, context, false, ((entityTerm.attributes[0] instanceof ConstantTermAttribute) ?
                    (entityTerm.attributes[0]).value : null), true);
                if (objectStr2 != null)
                    propertyStr3 = objectStr2[0];
                if (subjectStr != null && verbStr != null && propertyStr != null && propertyStr2 != null && propertyStr3 != null) {
                    var negated_t_2 = false; // TODO: find the proper value for this
                    if (entityTerm.functor.is_a_string("relative-direction")) {
                        return ["the " + propertyStr + " of " + propertyStr2 + " with respect to " + subjectStr[0] + " " + verbStr + " " + (negated_t_2 ? "not " : "") + propertyStr3, 2, undefined, 0];
                    }
                    else {
                        if (subjectStr[0] == "I") {
                            return ["my " + propertyStr + " " + verbStr + " " + (negated_t_2 ? "not " : "") + propertyStr2 + " in " + propertyStr3, 2, undefined, 0];
                        }
                        else if (subjectStr[0] == "you") {
                            return ["your " + propertyStr + " " + verbStr + " " + (negated_t_2 ? "not " : "") + propertyStr2 + " in " + propertyStr3, 2, undefined, 0];
                        }
                        else {
                            return [subjectStr[0] + "'s " + propertyStr + " " + verbStr + " " + (negated_t_2 ? "not " : "") + propertyStr2 + " in " + propertyStr3, 2, undefined, 0];
                        }
                    }
                }
                return [this.termToEnglish_Inform(new Term(context.ai.o.getSort("perf.inform"), [new ConstantTermAttribute(speakerID, context.ai.o.getSort("#id")), entityRaw]), speakerID, context), 2, undefined, 0];
            }
            if (entityTerm.functor.is_a(context.ai.o.getSort("perf.request.action"))) {
                return [this.termToEnglish_RequestAction(entityTerm, speakerID, context, false, false), 2, undefined, 0];
            }
            if (POSParser.sortIsConsideredForTypes(entityTerm.functor, this.o)) {
                //console.log("termToEnglish_Inform object, space_location, process, or role");
                return [this.renderTypeStatement(entityRaw, speakerID, context), 2, undefined, 0];
            }
        }
        console.warn("termToEnglish_RelationArgument: could not render " + entityRaw);
        return null;
    };
    NLGenerator.prototype.termToEnglish_Property = function (propertyRaw, speakerID, insertThat, context) {
        var tl = [propertyRaw];
        if (propertyRaw instanceof TermTermAttribute) {
            if (propertyRaw.term.functor.name == "#and") {
                tl = Term.elementsInList(propertyRaw.term, "#and");
            }
        }
        var property = tl[0]; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
        if (property instanceof TermTermAttribute) {
            var num = undefined;
            var time = this.nlg_cache_sort_present;
            for (var _i = 0, tl_16 = tl; _i < tl_16.length; _i++) {
                var t = tl_16[_i];
                if (t == property)
                    continue;
                if (t instanceof TermTermAttribute) {
                    var t_term = t.term;
                    if (t_term.functor.is_a(context.ai.o.getSort("plural")))
                        num = 1;
                    else if (t_term.functor.is_a(context.ai.o.getSort("singular")))
                        num = 0;
                    else if (t_term.functor.is_a(this.nlg_cache_sort_past) ||
                        t_term.functor.is_a(this.nlg_cache_sort_present) ||
                        t_term.functor.is_a(this.nlg_cache_sort_future)) {
                        time = t_term.functor;
                    }
                }
            }
            var property_term = property.term;
            var subjectStr = this.termToEnglish_VerbArgument(property_term.attributes[0], speakerID, true, context, true, null, true);
            if (num == undefined)
                num = subjectStr[3];
            var verbStr = this.verbStringWithTime(context.ai.o.getSort("verb.be"), num, subjectStr[1], time, false);
            var propertyStr = this.pos.getPropertyString(property_term.functor);
            if (propertyStr == null) {
                propertyStr = this.pos.getNounString(property_term.functor, 0, false); // without trying ancestors
                if (propertyStr != null)
                    propertyStr = "a " + propertyStr;
            }
            if (propertyStr == null) {
                propertyStr = this.pos.getNounString(property_term.functor, 0, true); // we are despearte, try ancestors
                if (propertyStr != null)
                    propertyStr = "a " + propertyStr;
            }
            if (verbStr != null && propertyStr != null)
                return [(insertThat ? "that " : "") + subjectStr[0] + " " + verbStr + " " + propertyStr, 2, undefined, 0];
        }
        console.error("termToEnglish_Property: could not render " + propertyRaw);
        return null;
    };
    NLGenerator.prototype.termToEnglish_Relation = function (propertyRaw, speakerID, context) {
        var not = false;
        if (propertyRaw instanceof TermTermAttribute) {
            if (propertyRaw.term.functor.name == "#not" &&
                propertyRaw.term.attributes.length == 1) {
                not = true;
                propertyRaw = propertyRaw.term.attributes[0];
            }
        }
        var tl = [propertyRaw];
        if (propertyRaw instanceof TermTermAttribute) {
            if (propertyRaw.term.functor.name == "#and") {
                tl = Term.elementsInList(propertyRaw.term, "#and");
            }
        }
        var relation = tl[0]; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
        if (relation instanceof TermTermAttribute) {
            var att1 = relation.term.attributes[0];
            var att2 = relation.term.attributes[1];
            var objectStr1 = this.termToEnglish_VerbArgument(att1, speakerID, true, context, false, null, true);
            var objectStr2 = this.termToEnglish_VerbArgument(att2, speakerID, true, context, false, null, true);
            var relationStr = this.pos.getRelationString(relation.term.functor, true);
            if (objectStr1 == null)
                return null;
            if (objectStr2 == null)
                return null;
            if (relation.term.functor.name == "relation.howto") {
                // special case for how to:
                if (not) {
                    return ["the way " + objectStr1[0] + " is not " + objectStr2[0], 2, null, objectStr1[3]];
                }
                else {
                    return ["the way " + objectStr1[0] + " is " + objectStr2[0], 2, null, objectStr1[3]];
                }
            }
            if (relationStr == null)
                return null;
            var verbStr = this.pos.getVerbString(context.ai.o.getSort("verb.be"), objectStr1[3], objectStr1[1], 3);
            if (not) {
                return [objectStr1[0] + " " + verbStr + " not " + relationStr + " " + objectStr2[0], 2, null, objectStr1[3]];
            }
            else {
                return [objectStr1[0] + " " + verbStr + " " + relationStr + " " + objectStr2[0], 2, null, objectStr1[3]];
            }
        }
        console.error("termToEnglish_Property: could not render " + propertyRaw);
        return null;
    };
    /*
    - This returns: [rendered string, person (0 = first, 1 = second, 2 = third), gender ('male', 'female'), and number (0 = singular, 1 = plural)]
    - definite == true, makes the returning string have "the", otherwise it has a/an
    */
    NLGenerator.prototype.termToEnglish_ConceptEntity = function (entityRaw, speakerID, context) {
        if (this.renderingSentenceVariables != null &&
            (entityRaw instanceof VariableTermAttribute)) {
            if (entityRaw.sort.name == "any" ||
                entityRaw.sort.name == "number" ||
                entityRaw.sort.name == "#id") {
                var idx = this.renderingSentenceVariables.indexOf(entityRaw);
                if (idx == -1) {
                    idx = this.renderingSentenceVariables.length;
                    this.renderingSentenceVariables.push(entityRaw);
                }
                return ["X" + (idx + 1), 2, null, 0];
            }
        }
        if ((entityRaw instanceof VariableTermAttribute) &&
            entityRaw.sort.name == "any" ||
            entityRaw.sort.name == "number" ||
            entityRaw.sort.name == "#id") {
            return ["something", 2, null, 0];
        }
        var tl = [entityRaw];
        if (entityRaw instanceof TermTermAttribute) {
            if (entityRaw.term.functor.name == "#and") {
                tl = Term.elementsInList(entityRaw.term, "#and");
            }
        }
        //		console.log("termToEnglish_ConceptEntity: " + entityRaw);
        // the_article, number, preComplementsStr, postComplementsStr, entity
        var entity_properties = [];
        var entities = [];
        for (var _i = 0, tl_17 = tl; _i < tl_17.length; _i++) {
            var t = tl_17[_i];
            //			console.log("considering: " + t + " (entities: " + entities + ")");
            if (t instanceof VariableTermAttribute) {
                if (entities.indexOf(t) == -1) {
                    entities.push(t);
                    entity_properties.push([false, 0, "", ""]);
                }
            }
            else if ((t instanceof TermTermAttribute) &&
                t.term.functor.name == "noun" &&
                t.term.attributes.length == 2) {
                var t_entity_idx = entities.indexOf(t);
                if (t_entity_idx == -1) {
                    // check if we need to replace the previous entry in the list by the "noun" structure
                    // This is because some elements like adjectives refer to the entity by its first parameter, rather
                    // than by the whole structure.
                    for (var i = 0; i < entities.length; i++) {
                        if (t.term.attributes[0] == entities[i]) {
                            t_entity_idx = i;
                            break;
                        }
                    }
                    if (t_entity_idx == -1) {
                        entities.push(t);
                        entity_properties.push([false, 0, "", ""]);
                    }
                    else {
                        entities[t_entity_idx] = t;
                    }
                }
            }
            else if (t instanceof TermTermAttribute) {
                var t_term = t.term;
                if (t_term.functor.is_a(context.ai.o.getSort("plural"))) {
                    //					number = 1;
                    var t_entity = t_term.attributes[0];
                    var t_entity_idx = entities.indexOf(t_entity);
                    if (t_entity_idx == -1) {
                        for (var i = 0; i < entities.length; i++) {
                            if (entities[i] instanceof TermTermAttribute &&
                                entities[i].term.functor.name == "noun" &&
                                entities[i].term.attributes.length == 2) {
                                if (entities[i].term.attributes[0] == t_entity) {
                                    t_entity_idx = i;
                                    break;
                                }
                            }
                        }
                    }
                    if (t_entity_idx == -1) {
                        t_entity_idx = entities.length;
                        entities.push(t_entity);
                        entity_properties.push([false, 0, "", ""]);
                    }
                    entity_properties[t_entity_idx][1] = 1;
                }
                else if (t_term.functor.is_a(context.ai.o.getSort("the"))) {
                    //the_article = true;
                    var t_entity = t_term.attributes[0];
                    var t_entity_idx = entities.indexOf(t_entity);
                    if (t_entity_idx == -1) {
                        for (var i = 0; i < entities.length; i++) {
                            if (entities[i] instanceof TermTermAttribute &&
                                entities[i].term.functor.name == "noun" &&
                                entities[i].term.attributes.length == 2) {
                                if (entities[i].term.attributes[0] == t_entity) {
                                    t_entity_idx = i;
                                    break;
                                }
                            }
                        }
                    }
                    if (t_entity_idx == -1) {
                        t_entity_idx = entities.length;
                        entities.push(t_entity);
                        entity_properties.push([false, 0, "", ""]);
                    }
                    entity_properties[t_entity_idx][0] = true;
                }
                else if (t_term.functor.is_a(context.ai.o.getSort("determiner"))) {
                    var num = 0;
                    var t_entity = t_term.attributes[0];
                    var t_entity_idx = entities.indexOf(t_entity);
                    if (t_entity_idx == -1) {
                        for (var i = 0; i < entities.length; i++) {
                            if (entities[i] instanceof TermTermAttribute &&
                                entities[i].term.functor.name == "noun" &&
                                entities[i].term.attributes.length == 2) {
                                if (entities[i].term.attributes[0] == t_entity) {
                                    t_entity_idx = i;
                                    break;
                                }
                            }
                        }
                    }
                    if (t_entity_idx == -1) {
                        t_entity_idx = entities.length;
                        entities.push(t_entity);
                        entity_properties.push([false, 0, "", ""]);
                    }
                    if (t_term.attributes.length >= 2) {
                        if (t_term.attributes[1].sort.name == 'plural')
                            num = 1;
                    }
                    var detStr = this.pos.getDeterminerString(t_term.functor, num);
                    if (detStr != null) {
                        //preComplementsStr = detStr + " " + preComplementsStr;
                        entity_properties[t_entity_idx][2] = detStr + " " + entity_properties[t_entity_idx][2];
                    }
                    else {
                        console.error("termToEnglish_ConceptEntity: getDeterminerString is null for " + t_term);
                    }
                }
                else if (t_term.functor.name == "adjective" &&
                    t_term.attributes.length == 2 &&
                    t_term.attributes[1] instanceof ConstantTermAttribute) {
                    var t_entity = t_term.attributes[0];
                    var t_entity_idx = entities.indexOf(t_entity);
                    if (t_entity_idx == -1) {
                        for (var i = 0; i < entities.length; i++) {
                            if (entities[i] instanceof TermTermAttribute &&
                                entities[i].term.functor.name == "noun" &&
                                entities[i].term.attributes.length == 2) {
                                if (entities[i].term.attributes[0] == t_entity) {
                                    t_entity_idx = i;
                                    break;
                                }
                            }
                        }
                    }
                    if (t_entity_idx == -1) {
                        t_entity_idx = entities.length;
                        entities.push(t_entity);
                        entity_properties.push([false, 0, "", ""]);
                    }
                    var adjSort = this.o.getSort(t_term.attributes[1].value);
                    var adjStr = this.pos.getPropertyString(adjSort);
                    if (adjStr != null) {
                        //preComplementsStr = detStr + " " + preComplementsStr;
                        entity_properties[t_entity_idx][2] = adjStr + " " + entity_properties[t_entity_idx][2];
                    }
                    else {
                        console.error("termToEnglish_ConceptEntity: getPropertyString is null for " + t_term);
                    }
                }
                else if (t_term.functor.is_a(context.ai.o.getSort("relation"))) {
                    var relationString = null;
                    var otherEntity = null;
                    //let t_entity:TermAttribute = null;
                    var t0_entity_idx = -1;
                    var t1_entity_idx = -1;
                    for (var i = 0; i < entities.length; i++) {
                        if (entities[i] == t_term.attributes[0]) {
                            t0_entity_idx = i;
                            break;
                        }
                        if (entities[i] instanceof TermTermAttribute &&
                            entities[i].term.functor.name == "noun" &&
                            entities[i].term.attributes.length == 2) {
                            if (entities[i].term.attributes[0] == t_term.attributes[0]) {
                                t0_entity_idx = i;
                                break;
                            }
                        }
                    }
                    for (var i = 0; i < entities.length; i++) {
                        if (entities[i] == t_term.attributes[1]) {
                            t1_entity_idx = i;
                            break;
                        }
                        if (entities[i] instanceof TermTermAttribute &&
                            entities[i].term.functor.name == "noun" &&
                            entities[i].term.attributes.length == 2) {
                            if (entities[i].term.attributes[0] == t_term.attributes[1]) {
                                t1_entity_idx = i;
                                break;
                            }
                        }
                    }
                    var t_entity_idx = -1;
                    if (t0_entity_idx != -1) {
                        //t_entity = t_term.attributes[0];
                        otherEntity = (t_term.attributes[1]);
                        relationString = this.pos.getRelationString(t_term.functor, true);
                        t_entity_idx = t0_entity_idx;
                    }
                    else if (t1_entity_idx != -1) {
                        //t_entity = t_term.attributes[1];
                        otherEntity = (t_term.attributes[0]);
                        var reverseSortName = this.pos.reverseRelations[t_term.functor.name];
                        if (reverseSortName != null) {
                            var reverseSort = context.ai.o.getSort(reverseSortName);
                            relationString = this.pos.getRelationString(reverseSort, true);
                        }
                        t_entity_idx = t1_entity_idx;
                    }
                    //					console.log("entities: " + entities);
                    //					console.log("otherEntity: " + otherEntity);
                    if (otherEntity != null) {
                        var tmp = this.termToEnglish_VerbArgument(otherEntity, speakerID, true, context, false, null, true);
                        //						console.log("tmp: " + tmp);
                        if (tmp != null) {
                            var otherEntityString = tmp[0];
                            if (otherEntityString != null) {
                                var tmpStr = relationString + " " + otherEntityString;
                                if (tmpStr == "of you") {
                                    //preComplementsStr = "your " + preComplementsStr;
                                    entity_properties[t_entity_idx][2] = "your " + entity_properties[t_entity_idx][2];
                                }
                                else if (tmpStr == "of I" ||
                                    tmpStr == "of me") {
                                    //preComplementsStr = "my " + preComplementsStr;
                                    entity_properties[t_entity_idx][2] = "my " + entity_properties[t_entity_idx][2];
                                }
                                else {
                                    //postComplementsStr += " " + tmpStr;
                                    entity_properties[t_entity_idx][3] += " " + tmpStr;
                                }
                            }
                        }
                        else {
                            console.error("termToEnglish_ConceptEntity: cannot render other entity ID " + otherEntity);
                        }
                    }
                    //					console.log("relationString: " + relationString);
                }
                else {
                    console.warn("ignoring term: " + t_term);
                }
            }
        }
        //console.log("entities: " + entities);
        //console.log("entity_properties: " + entity_properties);
        var outputNumber = 0;
        var outputText = null;
        for (var entity_idx = 0; entity_idx < entities.length; entity_idx++) {
            var entityText = null;
            var entity = entities[entity_idx];
            var the_article = entity_properties[entity_idx][0];
            var number = entity_properties[entity_idx][1];
            var preComplementsStr = entity_properties[entity_idx][2];
            var postComplementsStr = entity_properties[entity_idx][3];
            var sort = entity.sort;
            var needsArticle = true;
            if (entity instanceof TermTermAttribute &&
                entity.term.functor.name == "noun") {
                var entity_t = entity.term;
                if (entity_t.attributes.length >= 1 &&
                    entity_t.attributes[0] instanceof ConstantTermAttribute) {
                    sort = context.ai.o.getSort(entity_t.attributes[0].value);
                }
                if (entity_t.attributes.length >= 2 &&
                    entity_t.attributes[1] instanceof VariableTermAttribute) {
                    if (entity_t.attributes[1].sort.name == "plural")
                        number = 1;
                }
            }
            var word = this.pos.getAdverbString(sort);
            if (word != null)
                needsArticle = false;
            if (word == null) {
                word = this.pos.getTypeString(sort, number);
            }
            if (word == null) {
                var sort_l = sort.getAncestors();
                for (var _a = 0, sort_l_1 = sort_l; _a < sort_l_1.length; _a++) {
                    var ts = sort_l_1[_a];
                    word = this.pos.getTypeString(ts, 0);
                    if (word != null) {
                        sort = ts;
                        break;
                    }
                }
            }
            if (word == null) {
                word = this.pos.getPropertyString(sort);
                if (word != null) {
                    preComplementsStr = "something that is " + preComplementsStr;
                }
            }
            if (word == null &&
                (sort.name == "property" || sort.name == "property-with-value")) {
                word = "property";
            }
            if (word == null && sort.is_a_string("number")) {
                word = this.pos.getPOSString(sort, number);
            }
            //console.log("rendering entity ("+entity_idx+"): " + entities[entity_idx] + " --- " + entity_properties[entity_idx]);
            //console.log("word: " + word);
            if (word == null) {
                continue;
                //				console.error("termToEnglish_ConceptEntity NLPos could not generate a type string for " + sort + " for " + entityRaw);
                //				return null;
            }
            if (needsArticle) {
                if (number == 0) {
                    if (the_article)
                        preComplementsStr = "the " + preComplementsStr;
                    if (preComplementsStr == "" && this.pos.isCountable(word)) {
                        preComplementsStr = this.aVSanArticle(word) + " ";
                    }
                    else if (preComplementsStr == "a " ||
                        preComplementsStr == "an ") {
                        preComplementsStr = this.aVSanArticle(word) + " ";
                    }
                    entityText = preComplementsStr + word + postComplementsStr;
                }
                else {
                    if (the_article)
                        preComplementsStr = "the " + preComplementsStr;
                    //return [preComplementsStr + word + postComplementsStr, 2, undefined, number];
                    entityText = preComplementsStr + word + postComplementsStr;
                }
            }
            else {
                entityText = preComplementsStr + word + postComplementsStr;
            }
            if (outputText == null) {
                outputText = entityText;
            }
            else {
                outputText += " and " + entityText;
            }
            outputNumber = Math.max(outputNumber, number);
        }
        return [outputText, 2, undefined, outputNumber];
    };
    /*
    - This returns: [rendered string, person (0 = first, 1 = second, 2 = third), gender ('male', 'female'), and number (0 = singular, 1 = plural)]
    - Note:
        - This case is not handled: if a relation "r(X1,X2)" is present (X1 = entity), and there is another object X3, such that
          "r(X1,X3)" is also present, and X2 and X3 are of the same type, this will return a description that is not specific enough.
    */
    NLGenerator.prototype.termToEnglish_Entity = function (entity, speakerID, considerRelations, context, subject, useNameIfAvailable) {
        return this.termToEnglish_EntityInternal(entity, speakerID, considerRelations, context, subject, useNameIfAvailable, true);
    };
    NLGenerator.prototype.termToEnglish_EntityInternal = function (entity, speakerID, considerRelations, context, subject, useNameIfAvailable, userPronounsIfpossible) {
        if (!(entity instanceof ConstantTermAttribute))
            return null;
        var ai = context.ai;
        var entityID = entity.value;
        //		console.log("termToEnglish_Entity, entity: " + entity + ", speakerID: " + speakerID + ", selfID: " + ai.selfID);
        if (userPronounsIfpossible) {
            if (entityID == speakerID) {
                if (subject)
                    return ["I", 0, undefined, 0];
                else
                    return ["me", 0, undefined, 0];
            }
            if (entityID == context.speaker)
                return ["you", 1, undefined, 0];
        }
        // get all the properties of the entity:
        var ce = context.newContextEntity(entity, undefined, undefined, ai.o, false);
        var nameTerms = [];
        var typeTerms = [];
        var PRTerms = [];
        if (ce == null) {
            console.error("termToEnglish_Entity: could not render " + entity + " (" + nameTerms.length + " nameTerms, " + typeTerms.length + " typeTerms)");
            console.error("termToEnglish_Entity: ce is null!");
            return [entityID, 2, undefined, 0];
        }
        for (var _i = 0, _a = ce.terms; _i < _a.length; _i++) {
            var t = _a[_i];
            //			console.log("for " + entity + ": " + t);
            if (t.functor.is_a(this.nlg_cache_sort_name))
                nameTerms.push(t);
            if (t.attributes.length == 1 && POSParser.sortIsConsideredForTypes(t.functor, this.o))
                typeTerms.push(t);
            if (t.functor.is_a(this.nlg_cache_sort_property))
                PRTerms.push(t);
            if (considerRelations) {
                if (t.functor.is_a(this.nlg_cache_sort_relation)) {
                    PRTerms.push(t);
                    //					console.log("PRTerm.push( " + t + " )");
                }
            }
        }
        if (useNameIfAvailable && nameTerms.length > 0) {
            var nameTerm = nameTerms[0];
            if (nameTerm.attributes[1] instanceof ConstantTermAttribute) {
                var name_1 = nameTerm.attributes[1].value;
                if (name_1.indexOf(" room") != -1 ||
                    name_1.indexOf(" key") != -1)
                    return ["the " + name_1, 2, undefined, 0];
                return [name_1, 2, undefined, 0];
            }
        }
        //		console.log("termToEnglish_Entity, typeTerms: " + typeTerms.length + ", nameTerms: " + nameTerms.length + ", PRTerms: " + PRTerms.length);
        if (typeTerms.length > 0) {
            var typeTerm = typeTerms[0];
            var typeSort = typeTerm.functor;
            var typeString = this.pos.getTypeString(typeSort, 0);
            if (typeString == null) {
                var typeSort_l = typeSort.getAncestors();
                for (var _b = 0, typeSort_l_1 = typeSort_l; _b < typeSort_l_1.length; _b++) {
                    var ts = typeSort_l_1[_b];
                    typeString = this.pos.getTypeString(ts, 0);
                    if (typeString != null) {
                        typeSort = ts;
                        break;
                    }
                }
            }
            if (typeString == null) {
                console.error("termToEnglish_Entity: could not render type " + typeTerm);
                return [entityID, 2, undefined, 0];
            }
            // determine if it's enough:
            var msl = context.findEntitiesOfSort(typeSort, ai.o);
            var candidates = context.applySingularTheDeterminer(msl);
            if (candidates.length == 1 && candidates[0] == ce) {
                return ["the " + typeString, 2, undefined, 0];
            }
            // we need to be more precise!
            var selectedPRPair = this.selectEntityDifferentiatingPropertiesAndRelations(PRTerms, msl, ce, context, ai.o);
            var selectedPRs = selectedPRPair[0];
            //			console.log("selectedPRs: " + selectedPRs);
            if (selectedPRs == null) {
                return [this.aVSanArticle(typeString) + " " + typeString, 2, undefined, 0];
            }
            else {
                var defaultDeterminer = "the";
                if (selectedPRPair[1] > 1)
                    defaultDeterminer = "a";
                var preoutput = "";
                var postoutput = "";
                var determiner = false;
                for (var _c = 0, selectedPRs_1 = selectedPRs; _c < selectedPRs_1.length; _c++) {
                    var pr = selectedPRs_1[_c];
                    if (pr.attributes.length == 1) {
                        // property:
                        var propertyString = this.pos.getPropertyString(pr.functor);
                        if (propertyString != null) {
                            preoutput = preoutput.trim() + " " + propertyString + " ";
                        }
                        else {
                            //							console.error("Cannot render property: " + pr)
                        }
                    }
                    else if (pr.attributes.length == 2 &&
                        pr.functor.is_a(this.nlg_cache_sort_propertywithvalue) &&
                        pr.attributes[1] instanceof ConstantTermAttribute) {
                        // property with value:
                        var propertyString = this.pos.getPropertyString(pr.attributes[1].sort);
                        if (propertyString != null) {
                            preoutput = preoutput.trim() + " " + propertyString + " ";
                        }
                        else {
                            //							console.error("Cannot render property: " + pr)
                        }
                    }
                    else {
                        // relations:
                        //						console.log("trying to add: " + pr);
                        var relationString = null;
                        var otherEntityID = null;
                        if ((pr.attributes[0] instanceof ConstantTermAttribute) &&
                            pr.attributes[0].value == entityID) {
                            if (pr.attributes[1] instanceof ConstantTermAttribute) {
                                otherEntityID = (pr.attributes[1]);
                            }
                            relationString = this.pos.getRelationString(pr.functor, true);
                        }
                        else {
                            if (pr.attributes[0] instanceof ConstantTermAttribute) {
                                otherEntityID = (pr.attributes[0]);
                            }
                            var reverseSortName = this.pos.reverseRelations[pr.functor.name];
                            if (reverseSortName != null) {
                                var reverseSort = ai.o.getSort(reverseSortName);
                                relationString = this.pos.getRelationString(reverseSort, true);
                            }
                        }
                        //						console.log("entityID: " + entityID);
                        //						console.log("otherEntityID: " + otherEntityID);
                        if (otherEntityID != null && relationString != null) {
                            var otherEntityString = this.termToEnglish_Entity(otherEntityID, speakerID, false, context, false, true)[0];
                            if (otherEntityString != null) {
                                var tmpStr = relationString + " " + otherEntityString;
                                if (tmpStr == "of you") {
                                    determiner = true;
                                    preoutput = "your " + preoutput.trim();
                                }
                                else if (tmpStr == "of I" ||
                                    tmpStr == "of me") {
                                    determiner = true;
                                    preoutput = "my " + preoutput.trim();
                                }
                                else {
                                    postoutput += " " + tmpStr;
                                }
                            }
                        }
                    }
                }
                if (!determiner) {
                    if (defaultDeterminer == "a") {
                        if (preoutput == "")
                            defaultDeterminer = this.aVSanArticle(typeString);
                        else
                            defaultDeterminer = this.aVSanArticle(preoutput.trim());
                    }
                    preoutput = defaultDeterminer + " " + preoutput.trim();
                }
                // console.log("termToEnglish_Entity, entity: " + entity + ", preoutput: " + preoutput + ", typeString: " + typeString + ", postoutput: " + postoutput);				
                return [preoutput.trim() + " " + typeString + postoutput, 2, undefined, 0];
            }
        }
        console.error("termToEnglish_Entity: could not render " + entity + " (" + nameTerms.length + " nameTerms, " + typeTerms.length + " typeTerms)");
        console.error("termToEnglish_Entity: terms: " + ce.terms);
        return [entityID, 2, undefined, 0];
    };
    NLGenerator.prototype.termToEnglish_EntityName = function (entity, context) {
        var ai = context.ai;
        var targetID = entity.value;
        // first, check if we know the name:
        var name = ai.noInferenceQueryValue(Term.fromString("name('" + targetID + "'[#id],NAME:[symbol])", ai.o), ai.o, "NAME");
        if (name != null &&
            name instanceof ConstantTermAttribute)
            return name.value;
        // otherwise, just see what do we know about this entity:
        var sortsToConsider = ["object", "space.location"];
        var candidateSorts = [];
        for (var _i = 0, sortsToConsider_1 = sortsToConsider; _i < sortsToConsider_1.length; _i++) {
            var sortName = sortsToConsider_1[_i];
            var results = ai.longTermMemory.allSingleTermMatches(ai.o.getSort(sortName), 1, ai.o);
            for (var _a = 0, results_1 = results; _a < results_1.length; _a++) {
                var result = results_1[_a];
                //			console.log("result: " + result);
                if ((result.terms[0].attributes[0] instanceof ConstantTermAttribute) &&
                    (result.terms[0].attributes[0]).value == targetID) {
                    if (candidateSorts.indexOf(result.terms[0].functor) == -1) {
                        candidateSorts.push(result.terms[0].functor);
                        var ancestors = result.terms[0].functor.getAncestors();
                        for (var _b = 0, ancestors_1 = ancestors; _b < ancestors_1.length; _b++) {
                            var s = ancestors_1[_b];
                            if (s.is_a(ai.o.getSort(sortName)) &&
                                candidateSorts.indexOf(s) == -1)
                                candidateSorts.push(s);
                        }
                    }
                }
            }
            var results2 = ai.shortTermMemory.allMatches(Term.fromString(sortName + "('" + targetID + "'[#id])", ai.o));
            //		console.log(results2.length + " out of " + ai.shortTermMemory.plainTermList.length + " match");
            for (var _c = 0, results2_1 = results2; _c < results2_1.length; _c++) {
                var tmp = results2_1[_c];
                var term = tmp[0];
                //			console.log("result2: " + term);
                if ((term.attributes[0] instanceof ConstantTermAttribute) &&
                    (term.attributes[0]).value == targetID) {
                    if (candidateSorts.indexOf(term.functor) == -1) {
                        candidateSorts.push(term.functor);
                        var ancestors = term.functor.getAncestors();
                        for (var _d = 0, ancestors_2 = ancestors; _d < ancestors_2.length; _d++) {
                            var s = ancestors_2[_d];
                            if (s.is_a(ai.o.getSort("object")) &&
                                candidateSorts.indexOf(s) == -1)
                                candidateSorts.push(s);
                        }
                    }
                }
            }
            if (candidateSorts.length > 0)
                break;
        }
        var currentName = null;
        var currentSort = null;
        for (var _e = 0, candidateSorts_1 = candidateSorts; _e < candidateSorts_1.length; _e++) {
            var s = candidateSorts_1[_e];
            var tmp = this.pos.getTypeString(s, 0);
            if (tmp != null) {
                if (currentSort == null ||
                    s.is_a(currentSort)) {
                    currentName = tmp;
                    currentSort = s;
                }
            }
        }
        return currentName;
    };
    /*
    Assumes the first element of "tl" is #query(VARIABLE), and that the rest of terms in tl represent the query, for which VARIABLE is the answer
    */
    NLGenerator.prototype.termToEnglish_Query = function (variable, rest, speakerID, context) {
        var tl = Term.elementsInList(rest.term, "#and");
        for (var _i = 0, tl_18 = tl; _i < tl_18.length; _i++) {
            var tmp_t = tl_18[_i];
            if (!(tmp_t instanceof TermTermAttribute)) {
                console.error("termToEnglish_Query: could not render (one of the elements in the list is not a term) " + rest);
                return "[NLG ERROR: termToEnglish_Query]";
            }
        }
        return this.termToEnglish_QueryInternal(variable, tl, speakerID, context)[0] + "?";
    };
    /* examples:
        X, name('listener',X)  ->  what is your name?
        X, name('speaker',X)   ->  what is my name?
        X, color('chairID',X)  ->  what is the color of the chair?
        X, color(X,[white])    ->  what is colored white?
        X, in(X, 'crateID')    ->  what is in the crate?
       patterns:
        X, R(X, [SORT])	-> what is R SORT?
        X, R(X, ID)	    -> what is R ID?
        X, R([SORT], X) -> what is the R of a SORT? (if R is a property-with-value)
                        -> what is a SORT R? (otherwise)
        X, R(ID, X) 	-> what is the R of ID?	(if R is a property-with-value)
                        -> what is your/my ID? 	(if R is a property-with-value)
        X, R(ID, X)		-> what is ID R? (otherwise)
    */
    NLGenerator.prototype.termToEnglish_QueryInternal = function (variable, tl, speakerID, context) {
        if (tl.length == 1 &&
            (tl[0] instanceof TermTermAttribute)) {
            // this is the only case we know how to consider fow now:
            var constraint = (tl[0]).term;
            if (constraint.functor.is_a(this.nlg_cache_sort_relation) && constraint.attributes.length == 2) {
                var att1 = constraint.attributes[0];
                var att2 = constraint.attributes[1];
                if (att1 == variable) {
                    console.error("termToEnglish_QueryInternal: could not render (constraint not yet considered) " + tl);
                    // ...
                }
                else if (att2 == variable) {
                    var objectStr = this.termToEnglish_VerbArgument(att1, speakerID, true, context, false, null, true);
                    if (objectStr == null)
                        return null;
                    if (objectStr[0] == "you") {
                        return ["what are you " + this.pos.getRelationString(constraint.functor, true), 2, undefined, 0];
                    }
                    else if (objectStr[0] == "me") {
                        return ["what am I " + this.pos.getRelationString(constraint.functor, true), 2, undefined, 0];
                    }
                    else {
                        return ["what is " + objectStr[0] + this.pos.getRelationString(constraint.functor, true), 2, undefined, 0];
                    }
                }
                else {
                    console.error("termToEnglish_QueryInternal: could not render (variable not found in constraint) " + tl);
                }
            }
            else if (constraint.functor.is_a(this.nlg_cache_sort_propertywithvalue) && constraint.attributes.length == 2) {
                var att1 = constraint.attributes[0];
                var att2 = constraint.attributes[1];
                if (att1 == variable) {
                    console.error("termToEnglish_QueryInternal: could not render (constraint not yet considered) " + tl);
                    // ...
                }
                else if (att2 == variable) {
                    var objectStr = this.termToEnglish_VerbArgument(att1, speakerID, true, context, false, null, true);
                    if (objectStr == null)
                        return null;
                    if (objectStr[0] == "you") {
                        return ["what is your " + this.pos.getNounString(constraint.functor, 0, true), 2, undefined, 0];
                    }
                    else if (objectStr[0] == "me") {
                        return ["what is my " + this.pos.getNounString(constraint.functor, 0, true), 2, undefined, 0];
                    }
                    else {
                        return ["what is the " + this.pos.getNounString(constraint.functor, 0, true) + " of " + objectStr[0], 2, undefined, 0];
                    }
                }
            }
            else {
                console.error("termToEnglish_QueryInternal: could not render (constraint not yet considered) " + tl);
            }
        }
        console.error("termToEnglish_QueryInternal: could not render " + tl);
        return null;
    };
    NLGenerator.prototype.termToEnglish_Where = function (target, speakerID, context) {
        if (target instanceof ConstantTermAttribute) {
            var targetStr = this.termToEnglish_VerbArgument(target, speakerID, true, context, true, null, true);
            if (targetStr != null) {
                var verbStr = this.pos.getVerbString(context.ai.o.getSort("verb.be"), targetStr[3], targetStr[1], 3);
                if (verbStr != null) {
                    return "where " + verbStr + " " + targetStr[0] + "?";
                }
                else {
                    console.error("termToEnglish_Where: could not render verb");
                }
            }
            else {
                console.error("termToEnglish_Where: could not render target: " + target);
            }
        }
        else {
            console.error("termToEnglish_Where: could not render (target is not a constant) " + target);
        }
        console.error("termToEnglish_Where: could not render " + target);
        return target.toString();
    };
    NLGenerator.prototype.termToEnglish_How = function (performative, speakerID, context) {
        if (!(performative.attributes[1] instanceof TermTermAttribute))
            return null;
        var verbStr = this.termToEnglish_NestedVerb(performative.attributes[1].term, speakerID, null, false, context);
        if (verbStr == null)
            return null;
        return "How can " + verbStr[0] + "?";
    };
    NLGenerator.prototype.termToEnglish_Date = function (date, resolution) {
        var months = ["January", "February", "March", "April",
            "May", "June", "July", "August",
            "September", "October", "November", "December"];
        var days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"];
        if (!(date instanceof ConstantTermAttribute))
            return null;
        if (!(date.sort.name == "number"))
            return null;
        var dateNumber = Number(date.value);
        if (resolution.name == "time.second") {
            return "" + (getCurrentDaySecond(dateNumber) % 60);
        }
        else if (resolution.name == "time.minute") {
            var h = getCurrentDayHour(dateNumber);
            if (h == 0) {
                return "12:" + numberToStringTwoDigits(getCurrentDayMinute(dateNumber) % 60) + "am";
            }
            else if (h < 12) {
                return getCurrentDayHour(dateNumber) + ":" + numberToStringTwoDigits(getCurrentDayMinute(dateNumber) % 60) + "am";
            }
            else {
                return getCurrentDayHour(dateNumber) + ":" + numberToStringTwoDigits(getCurrentDayMinute(dateNumber) % 60) + "pm";
            }
        }
        else if (resolution.name == "time.hour") {
            var h = getCurrentDayHour(dateNumber);
            if (h == 0) {
                return "12am";
            }
            else if (h < 12) {
                return getCurrentDayHour(dateNumber) + "am";
            }
            else {
                return getCurrentDayHour(dateNumber) + "pm";
            }
        }
        else if (resolution.name == "time.day") {
            var monthDay = getCurrentDayOfTheMonth(dateNumber) + 1;
            var monthDayStr = monthDay + "th";
            if ((monthDay % 10) == 1 && (monthDay % 100) != 11) {
                monthDayStr = monthDay + "st";
            }
            else if ((monthDay % 10) == 2 && (monthDay % 100) != 12) {
                monthDayStr = monthDay + "nd";
            }
            else if ((monthDay % 10) == 3 && (monthDay % 100) != 13) {
                monthDayStr = monthDay + "rd";
            }
            return days[getCurrentDayOfTheWeek(dateNumber)] + ", " +
                months[getCurrentMonth(dateNumber)] + " " + monthDayStr + ", year " + getCurrentYear(dateNumber);
        }
        else if (resolution.name == "time.week") {
            return "week " + (1 + Math.floor(getCurrentYearDay(dateNumber) / 7)) + ", year " + getCurrentYear(dateNumber);
        }
        else if (resolution.name == "time.month") {
            return months[getCurrentMonth(dateNumber)] + ", year " + getCurrentYear(dateNumber);
        }
        else if (resolution.name == "time.year") {
            return "year " + getCurrentYear(dateNumber);
        }
        else if (resolution.name == "time.century") {
            var century = (1 + Math.floor(getCurrentYear(dateNumber) / 100));
            if ((century % 10) == 1 && (century % 100) != 11) {
                return century + "st century";
            }
            else if ((century % 10) == 2 && (century % 100) != 12) {
                return century + "nd century";
            }
            else if ((century % 10) == 3 && (century % 100) != 13) {
                return century + "rd century";
            }
            else {
                return century + "th century";
            }
        }
        else if (resolution.name == "time.millenium") {
            var millenium = (1 + Math.floor(getCurrentYear(dateNumber) / 1000));
            if ((millenium % 10) == 1 && (millenium % 100) != 11) {
                return millenium + "st millenium";
            }
            else if ((millenium % 10) == 2 && (millenium % 100) != 12) {
                return millenium + "nd millenium";
            }
            else if ((millenium % 10) == 3 && (millenium % 100) != 13) {
                return millenium + "rd millenium";
            }
            else {
                return millenium + "th millenium";
            }
        }
        return null;
    };
    // 	- This returns: [rendered string, person (0 = first, 1 = second, 2 = third), gender ('male', 'female'), and number (0 = singular, 1 = plural)]
    NLGenerator.prototype.termToEnglish_MeasuringUnit = function (amount, unit) {
        var value = Number(amount);
        if (isNaN(value))
            return null;
        var number = 1; // plural
        if (Number(value) == 1)
            number = 0; // singular
        var unitStr = this.pos.getTypeString(unit, number);
        return [amount + " " + unitStr, 2, null, number];
    };
    /*
    Renders a verb clause that is the argument of another verb
    - This returns: [rendered string, person (0 = first, 1 = second, 2 = third), gender ('male', 'female'), and number (0 = singular, 1 = plural)]
    */
    NLGenerator.prototype.termToEnglish_NestedVerb = function (t_raw, speakerID, mainVerbSubjectID, verbsInInfinitive, context) {
        //		console.log("termToEnglish_NestedVerb: " + t_raw);
        //		console.log("termToEnglish_NestedVerb: speakerID " + speakerID);
        //		console.log("termToEnglish_NestedVerb: mainVerbSubjectID " + mainVerbSubjectID);
        var tl;
        if (t_raw.functor.name == "#and") {
            tl = Term.elementsInList(t_raw, "#and");
            for (var _i = 0, tl_19 = tl; _i < tl_19.length; _i++) {
                var tmp_t = tl_19[_i];
                if (!(tmp_t instanceof TermTermAttribute)) {
                    console.error("termToEnglish_NestedVerb: could not render (one of the elements in the list is not a term) " + t_raw);
                    return null;
                }
            }
        }
        else {
            tl = [new TermTermAttribute(t_raw)];
        }
        var negated_t = false;
        var t = tl[0].term; // ASSUMPTION!!!: the main term is the first in case there is a list, and the rest should be qualifiers
        if (t.functor.name == "#not") {
            negated_t = true;
            t = (t.attributes[0]).term;
        }
        var verbComplements = "";
        for (var _a = 0, tl_20 = tl; _a < tl_20.length; _a++) {
            var tmp_t2 = tl_20[_a];
            var t2 = tmp_t2.term;
            if (t2 == t)
                continue;
            if (t2.attributes.length == 1 &&
                (t2.attributes[0] instanceof TermTermAttribute) &&
                (t2.attributes[0]).term == t) {
                if (t2.functor.name == "time.later")
                    verbComplements += " later";
                if (t2.functor.name == "time.first")
                    verbComplements += " first";
                if (t2.functor.name == "time.now")
                    verbComplements += " now";
                if (t2.functor.name == "time.subsequently")
                    verbComplements += " after that";
            }
            else if (t2.attributes.length == 2 &&
                t2.functor.is_a(this.nlg_cache_sort_relation) &&
                (t2.attributes[0] instanceof TermTermAttribute) &&
                (t2.attributes[0]).term == t) {
                // render the relation as a verb complement:
                var relationStr = this.pos.getRelationString(t2.functor, true);
                var objectStr = this.termToEnglish_VerbArgument(t2.attributes[1], speakerID, true, context, false, null, true);
                verbComplements += " " + relationStr + " " + objectStr[0];
            }
            else {
                console.warn("termToEnglish_NestedVerb: cannot render verb complement " + t2);
            }
        }
        if (t.functor.is_a(context.ai.o.getSort("verb.be")) && t.attributes.length == 2) {
            // ...
        }
        else if (t.attributes.length == 1) {
            var subjectStr = void 0;
            if ((t.attributes[0] instanceof ConstantTermAttribute) &&
                (t.attributes[0]).value == mainVerbSubjectID) {
                // when the subject in a nested verb is the same as the parent verb, it should not be rendered:
                subjectStr = ["", 0, undefined, 0];
            }
            else {
                subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, mainVerbSubjectID, true);
                if (subjectStr == null)
                    return null;
            }
            if (verbsInInfinitive) {
                var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                return [(subjectStr[0] + (negated_t ? " not" : "") + " to " + verbStr + verbComplements).trim(), 0, undefined, 0];
            }
            else {
                var verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], this.nlg_cache_sort_present, negated_t);
                //let verbStr:string = this.pos.getVerbString(t.functor, subjectStr[3], subjectStr[1], 3);
                return [(subjectStr[0] + " " + verbStr + verbComplements).trim(), 0, undefined, 0];
            }
        }
        else if (t.attributes.length == 2) {
            var subjectStr = void 0;
            if ((t.attributes[0] instanceof ConstantTermAttribute)) {
                if ((t.attributes[0]).value == mainVerbSubjectID) {
                    // when the subject in a nested verb is the same as the parnet ver, it should not be rendered:
                    subjectStr = ["", 0, undefined, 0];
                }
                else {
                    subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, mainVerbSubjectID, true);
                    if (subjectStr == null)
                        return null;
                    mainVerbSubjectID = (t.attributes[0]).value;
                }
            }
            else {
                subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, mainVerbSubjectID, true);
                if (subjectStr == null)
                    return null;
            }
            var objectStr = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, mainVerbSubjectID, true);
            if (objectStr == null)
                return null;
            if (objectStr[0] != null &&
                objectStr[0][0] == 't' && objectStr[0][1] == 'o' && objectStr[0][2] == ' ' &&
                t.functor.is_a(this.nlg_cache_sort_modal_verb))
                objectStr[0] = objectStr[0].substring(3);
            if (t.functor.name == "verb.happen") {
                var verbStr = this.verbStringWithTime(t.functor, subjectStr[3], 2, this.nlg_cache_sort_present, negated_t);
                return [(objectStr[0] + " " + verbStr + " to " + subjectStr[0] + verbComplements).trim(), 0, undefined, 0];
            }
            else {
                if (verbsInInfinitive) {
                    var verbStr = this.pos.getVerbString(t.functor, 0, 0, 0);
                    return [(subjectStr[0] + (negated_t ? " not" : "") + " to " + verbStr + " " + objectStr[0] + verbComplements).trim(), 0, undefined, 0];
                }
                else {
                    var verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], this.nlg_cache_sort_present, negated_t);
                    //				let verbStr:string = this.pos.getVerbString(t.functor, subjectStr[3], subjectStr[1], 3);
                    return [(subjectStr[0] + " " + verbStr + " " + objectStr[0] + verbComplements).trim(), 0, undefined, 0];
                }
            }
        }
        else if (t.attributes.length == 3 &&
            (t.functor.name == "verb.tell" ||
                t.functor.name == "action.talk" ||
                t.functor.name == "action.give" ||
                t.functor.name == "verb.go" ||
                t.functor.name == "verb.guide" ||
                t.functor.name == "verb.take-to" ||
                t.functor.name == "action.put-in" ||
                t.functor.name == "verb.bring" ||
                t.functor.name == "verb.find" ||
                t.functor.name == "verb.help")) {
            var subjectStr = void 0;
            if ((t.attributes[0] instanceof ConstantTermAttribute)) {
                if ((t.attributes[0]).value == mainVerbSubjectID) {
                    // when the subject in a nested verb is the same as the parnet ver, it should not be rendered:
                    subjectStr = ["", 0, undefined, 0];
                }
                else {
                    subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, mainVerbSubjectID, true);
                    if (subjectStr == null)
                        return null;
                    mainVerbSubjectID = (t.attributes[0]).value;
                }
            }
            else {
                subjectStr = this.termToEnglish_VerbArgument(t.attributes[0], speakerID, true, context, true, mainVerbSubjectID, true);
                if (subjectStr == null)
                    return null;
            }
            var object1Str = this.termToEnglish_VerbArgument(t.attributes[1], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                (t.attributes[0]).value : null), true);
            var object2Str = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, ((t.attributes[0] instanceof ConstantTermAttribute) ?
                (t.attributes[0]).value : null), true);
            var verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], this.nlg_cache_sort_present, negated_t);
            if (subjectStr != null && object1Str != null && object2Str != null) {
                if (t.functor.name == "verb.help" &&
                    (t.attributes[1] instanceof ConstantTermAttribute) &&
                    (t.attributes[2] instanceof TermTermAttribute) &&
                    t.attributes[2].term.attributes.length >= 1 &&
                    (t.attributes[2].term.attributes[0] instanceof ConstantTermAttribute)) {
                    var obj1 = t.attributes[1].value;
                    var obj2 = t.attributes[2].term.attributes[0].value;
                    if (obj1 == obj2) {
                        object2Str = this.termToEnglish_VerbArgument(t.attributes[2], speakerID, true, context, false, obj1, true);
                    }
                    return [subjectStr[0] + " " + verbStr + " " + object1Str[0] + " " + object2Str[0] + verbComplements, 0, undefined, 0];
                }
                else if (t.functor.name == "verb.tell" ||
                    t.functor.name == "action.talk") {
                    return [subjectStr[0] + " " + verbStr + " " + object2Str[0] + " " + object1Str[0] + verbComplements, 0, undefined, 0];
                }
                else if (t.functor.name == "verb.find") {
                    return [subjectStr[0] + " " + verbStr + " " + object1Str[0] + verbComplements + " in " + object2Str[0], 0, undefined, 0];
                }
                else if (t.functor.name == "verb.take-to" ||
                    t.functor.name == "verb.bring") {
                    // verbStr = this.verbStringWithTime(this.o.getSort("action.take"), subjectStr[3], subjectStr[1], time, negated_t);
                    verbStr = this.verbStringWithTime(t.functor, subjectStr[3], subjectStr[1], this.nlg_cache_sort_present, negated_t);
                    return [subjectStr[0] + " " + verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0], 0, undefined, 0];
                }
                else if (t.functor.name == "action.put-in") {
                    var ai = context.ai;
                    verbStr = this.verbStringWithTime(ai.o.getSort("verb.put"), subjectStr[3], subjectStr[1], this.nlg_cache_sort_present, negated_t);
                    return [subjectStr[0] + " " + verbStr + " " + object1Str[0] + verbComplements + " in " + object2Str[0], 0, undefined, 0];
                }
                else {
                    return [subjectStr[0] + verbStr + " " + object1Str[0] + verbComplements + " to " + object2Str[0], 0, undefined, 0];
                }
            }
        }
        return null;
    };
    NLGenerator.prototype.verbStringWithTime = function (verb, number, person, time, negation) {
        if (time.is_a(this.nlg_cache_sort_past)) {
            var verbStr = this.pos.getVerbString(verb, number, person, 4);
            if (negation) {
                if (verb.name == "verb.be") {
                    verbStr = verbStr + " not";
                }
                else if (verb.name == "verb.can") {
                    verbStr = verbStr + " not";
                }
                else {
                    var auxiliaryVerbStr = this.pos.getVerbString(this.nlg_cache_sort_verb_to_do, number, person, 4);
                    verbStr = auxiliaryVerbStr + " not " + this.pos.getVerbString(verb, 0, 0, 0);
                }
            }
            return verbStr;
        }
        else if (time.is_a(this.nlg_cache_sort_future)) {
            var verbStr = this.pos.getVerbString(verb, 0, 0, 0);
            return "will " + (negation ? "not " : "") + verbStr;
        }
        else {
            var verbStr = this.pos.getVerbString(verb, number, person, 3);
            if (negation) {
                if (verb.name == "verb.be") {
                    verbStr = verbStr + " not";
                }
                else if (verb.name == "verb.can") {
                    verbStr = verbStr + " not";
                }
                else {
                    var auxiliaryVerbStr = this.pos.getVerbString(this.nlg_cache_sort_verb_to_do, number, person, 3);
                    verbStr = auxiliaryVerbStr + " not " + this.pos.getVerbString(verb, 0, 0, 0);
                }
            }
            return verbStr;
        }
    };
    NLGenerator.prototype.selectEntityDifferentiatingPropertiesAndRelations = function (PRTerms, msl, target, context, o) {
        var selected = [];
        var remaining = [].concat(PRTerms); // we make a copy of the list
        var best_n = msl[0].length + msl[1].length + msl[2].length;
        while (remaining.length > 0) {
            var best_msl = null;
            var best_term = null;
            best_n = msl[0].length + msl[1].length + msl[2].length;
            //			console.log("selectEntityDifferentiatingPropertiesAndRelations: " + msl[0].length + ", " + msl[1].length + ", " + msl[2].length + " -> PRTerms: " + PRTerms);
            for (var _i = 0, remaining_1 = remaining; _i < remaining_1.length; _i++) {
                var PRTerm = remaining_1[_i];
                //				console.log("about to evaluate (for "+target.objectID+"): " + PRTerm);
                if (PRTerm.attributes.length == 1) {
                    var msl_included = context.filterByAdjective(PRTerm.functor, msl, o);
                    var n = msl_included[0].length + msl_included[1].length + msl_included[2].length;
                    var found = false;
                    for (var i = 0; i < 3; i++) {
                        for (var _a = 0, _b = msl_included[i]; _a < _b.length; _a++) {
                            var e = _b[_a];
                            if (e.objectID != null && target.objectID != null &&
                                e.objectID.value == target.objectID.value) {
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                    if (found &&
                        n < best_n) {
                        best_n = n;
                        best_term = PRTerm;
                        best_msl = msl_included;
                    }
                }
                else if (PRTerm.attributes.length == 2 &&
                    PRTerm.functor.is_a(this.nlg_cache_sort_propertywithvalue) &&
                    (PRTerm.attributes[1] instanceof ConstantTermAttribute) &&
                    (PRTerm.attributes[0] instanceof ConstantTermAttribute) &&
                    PRTerm.attributes[0].value == target.objectID.value) {
                    var msl_included = context.filterByAdjective(PRTerm.attributes[1].sort, msl, o);
                    var n = msl_included[0].length + msl_included[1].length + msl_included[2].length;
                    var found = false;
                    for (var i = 0; i < 3; i++) {
                        for (var _c = 0, _d = msl_included[i]; _c < _d.length; _c++) {
                            var e = _d[_c];
                            if (e.objectID != null && target.objectID != null &&
                                e.objectID.value == target.objectID.value) {
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                    if (found &&
                        n < best_n) {
                        best_n = n;
                        best_term = PRTerm;
                        best_msl = msl_included;
                    }
                }
                else if (PRTerm.attributes.length == 2 &&
                    (PRTerm.attributes[0] instanceof ConstantTermAttribute) &&
                    PRTerm.attributes[0].value == target.objectID.value) {
                    var msl_included = context.filterByRelation1(PRTerm, msl, o, null);
                    var n = msl_included[0].length + msl_included[1].length + msl_included[2].length;
                    var found = false;
                    for (var i = 0; i < 3; i++) {
                        for (var _e = 0, _f = msl_included[i]; _e < _f.length; _e++) {
                            var e = _f[_e];
                            if (e.objectID != null && target.objectID != null &&
                                e.objectID.value == target.objectID.value) {
                                found = true;
                                break;
                            }
                        }
                        if (found)
                            break;
                    }
                    if (found &&
                        n < best_n) {
                        best_n = n;
                        best_term = PRTerm;
                        best_msl = msl_included;
                    }
                    //					console.log("evaluating(1) " + PRTerm + " -> " + n);
                }
                else if (PRTerm.attributes.length == 2 &&
                    (PRTerm.attributes[1] instanceof ConstantTermAttribute) &&
                    PRTerm.attributes[1].value == target.objectID.value) {
                    // consider it only if we can render it as text, i.e., if it's a symmetric relation, or if
                    // there is a known way to render the opposite relation:
                    //					console.log("considering(2) " + PRTerm);
                    if (PRTerm.functor.is_a(this.nlg_cache_sort_symmetric_relation) ||
                        this.pos.reverseRelations[PRTerm.functor.name] != null) {
                        var msl_included = context.filterByRelation2(PRTerm, msl, o, null);
                        var n = msl_included[0].length + msl_included[1].length + msl_included[2].length;
                        //						console.log("evaluating(2) " + PRTerm + " -> " + n);
                        var found = false;
                        for (var i = 0; i < 3; i++) {
                            for (var _g = 0, _h = msl_included[i]; _g < _h.length; _g++) {
                                var e = _h[_g];
                                if (e.objectID != null && target.objectID != null &&
                                    e.objectID.value == target.objectID.value) {
                                    found = true;
                                    break;
                                }
                            }
                            if (found)
                                break;
                        }
                        if (found &&
                            n < best_n) {
                            best_n = n;
                            best_term = PRTerm;
                            best_msl = msl_included;
                        }
                    }
                }
            }
            if (best_term != null) {
                //				console.log("best_term " + best_term);
                selected.push(best_term);
                if (best_n == 1)
                    return [selected, best_n];
            }
            if (best_msl != null)
                msl = best_msl;
            remaining.splice(remaining.indexOf(best_term), 1);
        }
        return [selected, best_n];
    };
    // returns the proper article "a" or "an" to be used in front of a word
    NLGenerator.prototype.aVSanArticle = function (word) {
        var trimmedWord = word.trim();
        var firstLetter = trimmedWord[0];
        if (firstLetter == undefined) {
            console.error("aVSanArticle: empty word!");
        }
        //		console.log("aVSanArticle of '"+word+"', first letter is '"+firstLetter+"'");
        if (this.consonants_for_a_vs_an.indexOf(firstLetter) != -1) {
            return "a";
        }
        if (trimmedWord.length > 2 &&
            trimmedWord[0] == "u" &&
            (trimmedWord[1] == "s" || trimmedWord[1] == "n")) {
            if (trimmedWord == "utensil" ||
                trimmedWord == "utensils")
                return "an";
            return "a";
        }
        return "an";
    };
    NLGenerator.prototype.termToEnglish_Inform_ComplexSentence = function (terms, speakerID, context) {
        var negatedTerms = [];
        var positiveTerms = [];
        this.renderingSentenceVariables = []; // mark that we are actually rendering a sentence
        for (var _i = 0, terms_1 = terms; _i < terms_1.length; _i++) {
            var term_a = terms_1[_i];
            if (term_a instanceof TermTermAttribute) {
                var term = term_a.term;
                if (term.functor.name == "#not") {
                    if (term.attributes.length == 1 &&
                        term.attributes[0] instanceof TermTermAttribute) {
                        negatedTerms.push(term.attributes[0]);
                    }
                    else {
                        // we should never have this
                        console.error("malformed #not term in termToEnglish_Inform_ComplexSentence: " + term);
                    }
                }
                else {
                    positiveTerms.push(term_a);
                }
            }
            else {
                console.error("malformed term in termToEnglish_Inform_ComplexSentence: " + term_a);
                return null;
            }
        }
        // pattern 1: if X then Y1 and ... and Yn
        if (negatedTerms.length > 0 && positiveTerms.length > 0) {
            var output = "if ";
            for (var i = 0; i < negatedTerms.length; i++) {
                var tmp = this.termToEnglish_RelationOrVerbArgument(negatedTerms[i], speakerID, true, context, true, null, true, false);
                if (tmp == null || tmp[0] == null)
                    return null;
                if (i == 0) {
                    output += tmp[0];
                }
                else {
                    if (i == negatedTerms.length - 1) {
                        output += " and " + tmp[0];
                    }
                    else {
                        output += ", " + tmp[0];
                    }
                }
            }
            output += " then ";
            for (var i = 0; i < positiveTerms.length; i++) {
                var tmp = this.termToEnglish_RelationOrVerbArgument(positiveTerms[i], speakerID, true, context, true, null, true, false);
                if (tmp == null || tmp[0] == null)
                    return null;
                if (i == 0) {
                    output += tmp[0];
                }
                else {
                    if (i == positiveTerms.length - 1) {
                        output += " and " + tmp[0];
                    }
                    else {
                        output += ", " + tmp[0];
                    }
                }
            }
            return output;
        }
        // pattern 2: all positive
        if (negatedTerms.length == 0 && positiveTerms.length > 0) {
            var output = "or ";
            for (var i = 0; i < positiveTerms.length; i++) {
                var tmp = this.termToEnglish_RelationOrVerbArgument(positiveTerms[i], speakerID, true, context, true, null, true, false);
                if (tmp == null || tmp[0] == null)
                    return null;
                if (i == 0) {
                    output += tmp[0];
                }
                else {
                    output += " or " + tmp[0];
                }
            }
            return output;
        }
        // pattern 2: all positive
        if (negatedTerms.length > 0 && positiveTerms.length == 0) {
            var output = "either ";
            for (var i = 0; i < negatedTerms.length; i++) {
                var tmp = this.termToEnglish_RelationOrVerbArgument(negatedTerms[i], speakerID, true, context, true, null, true, false);
                if (tmp == null || tmp[0] == null)
                    return null;
                if (i == 0) {
                    output += tmp[0] + " is not true";
                }
                else {
                    output += " or " + tmp[0] + " is not true";
                }
            }
            return output;
        }
        console.error("termToEnglish_Inform_ComplexSentence: cannot render sentence with " + positiveTerms.length + " positive terms and " + negatedTerms.length + " negated terms! ");
        return null;
    };
    return NLGenerator;
}());
