/*

Note (santi):
- This is the core AI class for SHRDLU. It implements all the different elements of the NPC AI.
- This class is generic, however, and not tied to this specific project, nor to the A4Engine, so it can be separated from the
  game. Everything that is specific to the game engine is implemented in A4RuleBasedAI, which extends from this class. Then,
  two other classes (EtaoinAI and RobotAI) implement additional functionality used by the two types of NPCs in the game
  (EtaoinAI is used for ETAOIN which is a disembodied AI, and RobotAI is used for the two robots, QWERTY and SHRDLU).
- These classes implement all the AI functionalities except for pathfinding. For that, I reused all the pathfinding code
  originally implemented in the A4Engine, which is still in the A4PathFinding class inside of the A4Engine code.

*/
var ONTOLOGY_PROVENANCE = "ontology";
var BACKGROUND_PROVENANCE = "background";
var BACKGROUND_ADDITIONAL_PROVENANCE = "background-additional"; // this is additional background knowledge that is loaded after 
// some events, and thus needs to be saved in savegames.
var PERCEPTION_PROVENANCE = "perception";
var REACTION_PROVENANCE = "reaction";
var MEMORIZE_PROVENANCE = "memorize";
var LOCATIONS_PROVENANCE = "locations";
var ACTION_REQUEST_CANNOT_BE_SATISFIED = 0;
var ACTION_REQUEST_CAN_BE_SATISFIED = 1;
var ACTION_REQUEST_WILL_BE_HANDLED_EXTERNALLY = 2;
var MENTION_MEMORY_SIZE = 10;
var DEFAULT_QUESTION_PATIENCE_TIMER = 1200;
var CONVERSATION_TIMEOUT = 120 * 60; // 2 minute of real time, which is 2 hour of in-game time
var OCCURS_CHECK = false;
var AIGoal = /** @class */ (function () {
    function AIGoal(a_term, a_actions, a_priority, a_time, a_periocidity) {
        // execution variables:
        this.remainingActions = null;
        this.currentActionIR = null; // the intention record for the first action in "remainingActions"
        this.term = a_term;
        this.actions = a_actions;
        this.priority = a_priority;
        this.nextTimeToTrigger = a_time;
        this.periodicity = a_periocidity;
    }
    AIGoal.prototype.restart = function (ai) {
        if (this.term != null) {
            ai.addLongTermTerm(new Term(ai.o.getSort("verb.do"), [new ConstantTermAttribute(ai.selfID, ai.cache_sort_id),
                new TermTermAttribute(this.term)]), PERCEPTION_PROVENANCE);
        }
        // restart the remainingActions list:
        this.remainingActions = [];
        for (var _i = 0, _a = this.actions; _i < _a.length; _i++) {
            var action = _a[_i];
            this.remainingActions.push(action);
        }
        this.currentActionIR = null;
    };
    AIGoal.prototype.execute = function (ai) {
        if (this.remainingActions.length == 0) {
            this.remainingActions = null;
            return true;
        }
        if (this.currentActionIR == null) {
            // start executing the next action:
            this.currentActionIR = new IntentionRecord(this.remainingActions[0], null, null, null, ai.timeStamp);
            ai.queueIntentionRecord(this.currentActionIR);
        }
        else {
            // keep checking if it was executed:
            if (!ai.IRpendingCompletion(this.currentActionIR)) {
                // the intention has been either canceled or succeeded, check if it was succeeded:
                if (this.currentActionIR.succeeded) {
                    // action succeeded:
                    this.currentActionIR = null;
                    this.remainingActions.splice(0, 1);
                }
                else {
                    // something failed, so, restart!
                    this.restart(ai);
                }
            }
        }
        return false;
    };
    AIGoal.fromXML = function (xml, o, ai) {
        var term = Term.fromString(xml.getAttribute("term"), o);
        var priority = Number(xml.getAttribute("priority"));
        var nextTimeToTrigger = Number(xml.getAttribute("nextTimeToTrigger"));
        var periodicity = Number(xml.getAttribute("periodicity"));
        var remainingActions = null;
        if (xml.getAttribute("remainingActions") != null)
            remainingActions = Number(xml.getAttribute("remainingActions"));
        var actions = [];
        for (var _i = 0, _a = getElementChildrenByTag(xml, "action"); _i < _a.length; _i++) {
            var goalxml = _a[_i];
            actions.push(Term.fromString(goalxml.getAttribute("term"), o));
        }
        var goal = new AIGoal(term, actions, priority, nextTimeToTrigger, periodicity);
        if (remainingActions != null) {
            goal.remainingActions = [];
            for (var i = actions.length - remainingActions; i < actions.length; i++) {
                goal.remainingActions.push(actions[i]);
            }
        }
        return goal;
    };
    AIGoal.prototype.saveToXML = function (ai) {
        var str = "<AIGoal term=\"" + this.term + "\"" +
            " priority=\"" + this.priority + "\"" +
            " nextTimeToTrigger = \"" + this.nextTimeToTrigger + "\"" +
            " periodicity = \"" + this.periodicity + "\"";
        if (this.remainingActions != null) {
            str += " remainingActions=\"" + this.remainingActions.length + "\"";
        }
        str += ">\n";
        for (var _i = 0, _a = this.actions; _i < _a.length; _i++) {
            var action = _a[_i];
            str += "<action term=\"" + action + "\"/>\n";
        }
        str += "</AIGoal>";
        return str;
    };
    return AIGoal;
}());
var InferenceRecord = /** @class */ (function () {
    function InferenceRecord(ai, additionalSentences_arg, targets, p, a, findAllAnswers, timeTerm, e) {
        this.targets = [];
        this.inferences = [];
        this.completedInferences = [];
        this.additionalSentences = [];
        this.priority = 1;
        this.anxiety = 0;
        this.findAllAnswers = false;
        this.timeTerm = null;
        this.effect = null;
        this.triggeredBy = null;
        this.triggeredBySpeaker = null;
        this.targets = targets;
        this.priority = p;
        this.anxiety = a;
        this.additionalSentences = additionalSentences_arg;
        this.findAllAnswers = findAllAnswers;
        this.timeTerm = timeTerm;
        this.effect = e;
        this.inferences = [];
    }
    InferenceRecord.fromXML = function (xml, o, ai) {
        var variables = [];
        var variableNames = [];
        var p = Number(xml.getAttribute("priority"));
        var a = Number(xml.getAttribute("anxiety"));
        var findAllAnswers = xml.getAttribute("findAllAnswers") == "true";
        var e = null;
        var tt = null;
        var tb = null;
        var tbs = null;
        if (xml.getAttribute("timeTerm") != null)
            tt = Term.fromStringInternal(xml.getAttribute("timeTerm"), o, variableNames, variables).term;
        if (xml.getAttribute("triggeredBy") != null)
            tb = Term.fromStringInternal(xml.getAttribute("triggeredBy"), o, variableNames, variables).term;
        if (xml.getAttribute("triggeredBySpeaker") != null)
            tbs = xml.getAttribute("triggeredBySpeaker");
        var effect_xml = getFirstElementChildByTag(xml, "InferenceEffect");
        if (effect_xml != null)
            e = ai.inferenceEffectFactory.loadFromXML(effect_xml, ai, o, variables, variableNames);
        var additionalSentences = [];
        var additionalSentences_xml = getFirstElementChildByTag(xml, "additionalSentences");
        if (additionalSentences_xml != null) {
            for (var _i = 0, _a = getElementChildrenByTag(additionalSentences_xml, "sentence"); _i < _a.length; _i++) {
                var s_xml = _a[_i];
                var s = Sentence.fromStringInternal(s_xml.firstChild.nodeValue, o, variableNames, variables);
                if (s != null)
                    additionalSentences.push(s);
            }
        }
        var targets = [];
        for (var _b = 0, _c = getElementChildrenByTag(xml, "target"); _b < _c.length; _b++) {
            var s_l_xml = _c[_b];
            var t = [];
            for (var _d = 0, _e = getElementChildrenByTag(s_l_xml, "sentence"); _d < _e.length; _d++) {
                var s_xml = _e[_d];
                var s = Sentence.fromStringInternal(s_xml.firstChild.nodeValue, o, variableNames, variables);
                if (s != null)
                    t.push(s);
            }
            targets.push(t);
        }
        var ir = new InferenceRecord(ai, additionalSentences, targets, p, a, findAllAnswers, tt, e);
        ir.triggeredBy = tb;
        ir.triggeredBySpeaker = tbs;
        return ir;
    };
    InferenceRecord.prototype.init = function (ai, o) {
        // Knowledge base is all the long term knowledge, plus the perception:
        var additionalSentences = [];
        for (var _i = 0, _a = this.additionalSentences; _i < _a.length; _i++) {
            var s = _a[_i];
            additionalSentences.push(s);
        }
        for (var _b = 0, _c = ai.shortTermMemory.plainTermList; _b < _c.length; _b++) {
            var te = _c[_b];
            additionalSentences.push(new Sentence([te.term], [true]));
        }
        var ltm = ai.longTermMemory;
        if (this.timeTerm != null) {
            // edit the long term memory to match the time of the query:
            if (this.timeTerm.functor.name == "time.past") {
                ltm = TimeInference.applyTimePast(ai.longTermMemory);
            }
            else if (this.timeTerm.functor.is_a(o.getSort("time.present"))) {
                // do nothing
            }
            else {
                console.error("InferenceRecord timeTerm not supported: " + this.timeTerm);
            }
        }
        for (var _d = 0, _e = this.targets; _d < _e.length; _d++) {
            var target = _e[_d];
            // let occurs_check:boolean = OCCURS_CHECK;
            var occurs_check = true;
            this.inferences.push(new InterruptibleResolution(ltm, additionalSentences, target, occurs_check, this.timeTerm == null, ai));
        }
    };
    InferenceRecord.prototype.saveToXML = function (ai) {
        // We do not save the state of the interruptible resolution process, sicne that'd be complex. 
        // The inference will just be restarted when the AI is loaded again:
        var variables = [];
        var variableNames = [];
        var str = "<InferenceRecord priority=\"" + this.priority + "\" " +
            "anxiety=\"" + this.anxiety + "\" " +
            "findAllAnswers = \"" + this.findAllAnswers + "\" " +
            (this.timeTerm != null ? "timeTerm = \"" + this.timeTerm.toStringXMLInternal(variables, variableNames) + "\" " : "") +
            (this.triggeredBy != null ? "triggeredBy = \"" + this.triggeredBy.toStringXMLInternal(variables, variableNames) + "\" " : "") +
            (this.triggeredBySpeaker != null ? "triggeredBySpeaker = \"" + this.triggeredBySpeaker + "\" " : "") +
            ">\n";
        if (this.effect != null)
            str += this.effect.saveToXMLInternal(ai, variables, variableNames) + "\n";
        console.error("InferenceRecord saving to XML not yet supported (figure out a way to save InferenceEffects)");
        if (this.additionalSentences.length > 0) {
            str += "<additionalSentences>\n";
            for (var _i = 0, _a = this.additionalSentences; _i < _a.length; _i++) {
                var s = _a[_i];
                str += "<sentence>" + s.toStringXMLInternal(variables, variableNames) + "</sentence>\n";
            }
            str += "</additionalSentences>\n";
        }
        for (var _b = 0, _c = this.targets; _b < _c.length; _b++) {
            var sl = _c[_b];
            str += "<target>\n";
            for (var _d = 0, sl_1 = sl; _d < sl_1.length; _d++) {
                var s = sl_1[_d];
                str += "<sentence>" + s.toStringXMLInternal(variables, variableNames) + "</sentence>\n";
            }
            str += "</target>\n";
        }
        str += "</InferenceRecord>";
        return str;
    };
    return InferenceRecord;
}());
var CauseRecord = /** @class */ (function () {
    function CauseRecord(t, c, time) {
        this.term = null;
        this.cause = null;
        this.timeStamp = null;
        this.causesComeFromInference = false;
        this.term = t;
        this.cause = c;
        this.timeStamp = time;
    }
    CauseRecord.fromXML = function (xml, o) {
        var cause = null;
        var p_xml = getFirstElementChildByTag(xml, "cause");
        if (p_xml != null) {
            cause = CauseRecord.fromXML(p_xml, o);
        }
        return new CauseRecord(Term.fromString(xml.getAttribute("term"), o), cause, Number(xml.getAttribute("timeStamp")));
    };
    CauseRecord.prototype.saveToXML = function () {
        if (this.cause == null) {
            var tmp = "<CauseRecord term=\"" + this.term.toStringXML() + "\" " +
                "timeStamp=\"" + this.timeStamp + "\"/>";
            return tmp;
        }
        else {
            var tmp = "<CauseRecord term=\"" + this.term.toStringXML() + "\" " +
                "timeStamp=\"" + this.timeStamp + "\">";
            tmp += this.cause.saveToXML();
            tmp += "</CauseRecord>";
            return tmp;
        }
    };
    return CauseRecord;
}());
var IntentionRecord = /** @class */ (function () {
    function IntentionRecord(a, r, rp, c, time) {
        this.action = null;
        this.requester = null;
        this.requestingPerformative = null;
        this.cause = null; // if it had a cause, other than being requested by "requester", we specify it here
        this.timeStamp = null;
        this.succeeded = null; // when this action is done, this will contain true/false
        this.alternative_actions = null; // some times, the "action" field comes from having run an inference process and some
        // variables might have multiple alternative values. This list contains all the possible
        // values, in case the "action" field (which should be = to alternative_actions[0]) cannot
        // be executed, but another can! This is used, for example for requests such as:
        // "take a green block" (where there might be more than one green block, and some might not
        // be takeable).
        this.numberConstraint = null; // If a list of alternative_actions is specified, this field specifies how many of
        this.action = a;
        this.requester = r;
        this.requestingPerformative = rp;
        this.cause = c;
        this.timeStamp = time;
    }
    IntentionRecord.fromXML = function (xml, ai, o) {
        var variables = [];
        var variableNames = [];
        var action = Term.fromStringInternal(xml.getAttribute("action"), o, variableNames, variables).term;
        var requester = null;
        var rps = xml.getAttribute("requestingPerformativeSpeaker");
        var requestingPerformative = null;
        var cause = null;
        var timeStamp = Number(xml.getAttribute("timeStamp"));
        if (xml.getAttribute("requester") != null)
            requester = Term.parseAttribute(xml.getAttribute("requester"), o, variableNames, variables);
        if (rps != null) {
            var context = ai.contextForSpeaker(rps);
            if (context != null) {
                requestingPerformative = context.performatives[Number(xml.getAttribute("requestingPerformativeSpeaker"))];
            }
        }
        var cause_xml = getFirstElementChildByTag(xml, "CauseRecord");
        if (cause_xml != null) {
            cause = CauseRecord.fromXML(cause_xml, o);
        }
        var ir = new IntentionRecord(action, requester, requestingPerformative, cause, timeStamp);
        var alternative_actions_xml_l = getElementChildrenByTag(xml, "alternative_action");
        if (alternative_actions_xml_l.length > 0) {
            ir.alternative_actions = [];
            for (var _i = 0, alternative_actions_xml_l_1 = alternative_actions_xml_l; _i < alternative_actions_xml_l_1.length; _i++) {
                var alternative_actions_xml = alternative_actions_xml_l_1[_i];
                ir.alternative_actions.push(Term.fromStringInternal(alternative_actions_xml.getAttribute("action"), o, variableNames, variables).term);
            }
        }
        if (xml.getAttribute("numberConstraint") != null)
            ir.numberConstraint = Term.parseAttribute(xml.getAttribute("numberConstraint"), o, variableNames, variables);
        return ir;
    };
    IntentionRecord.prototype.saveToXML = function (ai) {
        var variables = [];
        var variableNames = [];
        var context = null;
        if (this.requestingPerformative != null) {
            context = ai.contextForSpeaker(this.requestingPerformative.speaker);
        }
        var xml = "<IntentionRecord action=\"" + this.action.toStringXMLInternal(variables, variableNames) + "\"" +
            (this.requester == null ? "" :
                " requester=\"" + this.requester.toStringXMLInternal(variables, variableNames) + "\"") +
            (context == null ? "" :
                " requestingPerformativeSpeaker=\"" + this.requestingPerformative.speaker + "\"" +
                    " requestingPerformative=\"" + context.performatives.indexOf(this.requestingPerformative) + "\"") +
            " timeStamp=\"" + this.timeStamp + "\"" +
            (this.numberConstraint == null ? "" :
                " numberConstraint=\"" + this.numberConstraint.toStringXMLInternal(variables, variableNames) + "\"");
        if (this.cause == null && this.alternative_actions == null) {
            xml += "/>";
        }
        else {
            xml += ">\n";
            if (this.cause != null)
                xml += this.cause.saveToXML();
            if (this.alternative_actions != null) {
                for (var _i = 0, _a = this.alternative_actions; _i < _a.length; _i++) {
                    var alternative_action = _a[_i];
                    xml += "<alternative_action action=\"" + alternative_action.toStringXMLInternal(variables, variableNames) + "\"/>";
                }
            }
            xml += "</IntentionRecord>";
        }
        return xml;
    };
    IntentionRecord.prototype.resolveNumberConstraint = function (numberConstraint, max) {
        if (numberConstraint != null) {
            if (numberConstraint.sort.is_a_string("all"))
                return max;
            if (numberConstraint.sort.is_a_string("number.1"))
                return 1;
            if (numberConstraint.sort.is_a_string("number.2"))
                return 2;
            if (numberConstraint.sort.is_a_string("number.3"))
                return 3;
            if (numberConstraint.sort.is_a_string("number.4"))
                return 4;
            if (numberConstraint.sort.is_a_string("number.5"))
                return 5;
            if (numberConstraint.sort.is_a_string("number.6"))
                return 6;
            if (numberConstraint.sort.is_a_string("number.7"))
                return 7;
            if (numberConstraint.sort.is_a_string("number.8"))
                return 8;
            if (numberConstraint.sort.is_a_string("number.9"))
                return 9;
            if (numberConstraint.sort.is_a_string("number.10"))
                return 10;
            if (numberConstraint.sort.is_a_string("number") &&
                numberConstraint instanceof ConstantTermAttribute) {
                var value = numberConstraint.value;
                return Number(value);
            }
        }
        return 1;
    };
    return IntentionRecord;
}());
var IntentionAction = /** @class */ (function () {
    function IntentionAction() {
        this.needsContinuousExecution = false;
        this.ir = null;
    }
    // This is what will be executed all the other times except the first. When it returns "true", action is over
    IntentionAction.prototype.executeContinuous = function (ai) {
        return true;
    };
    // Some requests require inference, however, an action  might be able to handle the inference internally.
    // If that's the case, then this function has to be redefined, and return "true" for those actions that the action handler
    // can handle inference internally.
    IntentionAction.prototype.canHandleWithoutInference = function (perf) {
        return false;
    };
    // This will be called if the script associated with the action fails:
    IntentionAction.prototype.actionScriptsFailed = function (ai, requester) {
    };
    return IntentionAction;
}());
var InferenceEffect = /** @class */ (function () {
    function InferenceEffect() {
    }
    InferenceEffect.prototype.saveToXML = function (ai) {
        return this.saveToXMLInternal(ai, [], []);
    };
    InferenceEffect.prototype.generateCauseRecord = function (target, result, ai) {
        var baseSentences = result.getBaseSentences(target);
        if (baseSentences.length == 0) {
            return null;
        }
        else {
            var cause = null;
            for (var _i = 0, baseSentences_1 = baseSentences; _i < baseSentences_1.length; _i++) {
                var s = baseSentences_1[_i];
                var s_term = Term.sentenceToTerm(s, ai.o);
                if (cause == null) {
                    cause = s_term;
                }
                else {
                    cause = new Term(ai.o.getSort("#and"), [new TermTermAttribute(cause), new TermTermAttribute(s_term)]);
                }
            }
            var cr = new CauseRecord(cause, null, ai.timeStamp);
            cr.causesComeFromInference = true;
            return cr;
        }
    };
    return InferenceEffect;
}());
var RuleBasedAI = /** @class */ (function () {
    function RuleBasedAI(o, nlp, pf, pfoffset, qpt) {
        this.timeStamp = 0;
        this.questionPatienceTimmer = 1200;
        this.maximum_answers_to_give_at_once_for_a_query = 3;
        this.predicatesToStoreInLongTermMemory = [];
        this.o = null;
        this.naturalLanguageParser = null;
        this.inferenceEffectFactory = null;
        this.perceptionFrequency = 10;
        this.perceptionFrequencyOffset = 0;
        this.perceptionMemoryTime = 120;
        this.selfID = "self";
        this.intentionHandlers = [];
        // (in a BDI model, this would be "B"):
        this.perceptionBuffer = [];
        this.shortTermMemory = new TermContainer();
        this.longTermMemory = new SentenceContainer();
        // AI current goals (in a BDI model, this would be "D"):
        this.goals = []; // list of goals that the AI currently has (not necessarily triggered)
        this.currentGoal = null; // current goal (only one at a time, and if a higher priority one triggers,
        // the current one will go back to the "goals" list)    	
        // (in a BDI model, this would be "I"):
        this.intentions = [];
        this.queuedIntentions = []; // these will become intentions only when intentions == [], currentInferenceProcess == null and queuedInferenceProcesses == []
        // the use of this is to queue things to do after the AI has finished doing the current set of things
        this.intentionsCausedByRequest = []; // we store the intention records for which there is a cause, for answering later "why" questions
        this.currentInferenceProcess = null;
        this.queuedInferenceProcesses = []; // list of the current inferences the AI wants to perform after currentInferenceProcess is done
        this.contexts = []; // contexts for natural language processing (one per entity we speak to)
        this.terminateConversationAfterThisPerformative = false;
        this.queuedParsedPerformatives = [];
        this.currentEpisodeTerms = []; // Terms that are to be remembered or the current "episode" (i.e., while the AI is executing an action),
        // but that will be erased when a new action is started.
        // This is a hack, but it is to avoid having to have the concept of "immediate past" and "far past",
        // since parsing those from text would be hard. 
        // To illustrate the problem, consider this interaction:
        // - Shrdlu, go north
        // - There is an obstacle here.
        // - go south.
        // - Ok.
        // - did you collide with something?
        // The expected answer is "no", but it would say "yes", as it collided with something in the first command.
        // So, we add the "collided-with" type of knowledge to the episode terms, and we clear them after each episode.
        // if this is != null, each time the AI executes an action, it will be logged here
        this.debugActionLog = null;
        // Sort cache for perception:
        this.cache_sort_name = null;
        this.cache_sort_space_at = null;
        this.cache_sort_time_current = null;
        this.cache_sort_number = null;
        this.cache_sort_symbol = null;
        this.cache_sort_id = null;
        this.cache_sort_map = null;
        this.cache_sort_intention = null;
        this.cache_sort_action_talk = null;
        this.cache_sort_performative = null;
        this.cache_sort_property = null;
        this.cache_sort_property_with_value = null;
        this.cache_sort_relation_with_value = null;
        this.cache_sort_object = null;
        this.cache_sort_space_location = null;
        this.cache_sort_relation = null;
        this.cache_sort_verb_have = null;
        this.cache_sort_verb_contains = null;
        this.cache_sort_stateSort = null;
        this.cache_sort_action_follow = null;
        this.cache_sort_action_think = null;
        this.o = o;
        this.naturalLanguageParser = nlp;
        this.perceptionFrequency = pf;
        this.perceptionFrequencyOffset = pfoffset;
        this.questionPatienceTimmer = qpt;
        this.cache_sort_name = this.o.getSort("name");
        this.cache_sort_space_at = this.o.getSort("space.at");
        this.cache_sort_time_current = this.o.getSort("time.current");
        this.cache_sort_number = this.o.getSort("number");
        this.cache_sort_symbol = this.o.getSort("symbol");
        this.cache_sort_id = this.o.getSort("#id");
        this.cache_sort_map = this.o.getSort("map");
        this.cache_sort_intention = this.o.getSort("intention");
        this.cache_sort_performative = this.o.getSort("performative");
        this.cache_sort_property = this.o.getSort("property");
        this.cache_sort_property_with_value = this.o.getSort("property-with-value");
        this.cache_sort_relation_with_value = this.o.getSort("relation-with-value");
        this.cache_sort_object = this.o.getSort("object");
        this.cache_sort_space_location = this.o.getSort("space.location");
        this.cache_sort_relation = this.o.getSort("relation");
        this.cache_sort_verb_have = this.o.getSort("verb.have");
        this.cache_sort_verb_contains = this.o.getSort("verb.contains");
        this.cache_sort_stateSort = this.o.getSort("#stateSort");
        this.cache_sort_action_talk = this.o.getSort("action.talk");
        this.cache_sort_action_follow = this.o.getSort("verb.follow");
        this.cache_sort_action_think = this.o.getSort("verb.think");
        this.inferenceEffectFactory = new InferenceEffectFactory();
        var ontologySentences = RuleBasedAI.translateOntologyToSentences(o);
        console.log("Ontology converted to " + ontologySentences.length + " sentences:");
        for (var _i = 0, ontologySentences_1 = ontologySentences; _i < ontologySentences_1.length; _i++) {
            var s = ontologySentences_1[_i];
            this.longTermMemory.addSentence(s, ONTOLOGY_PROVENANCE, 1, 0);
        }
    }
    RuleBasedAI.prototype.update = function (timeStamp) {
        this.timeStamp = timeStamp;
        // 1) Attention & Perception:
        if (this.currentInferenceProcess == null) {
            if (this.perceptionFrequency == 0 || (timeStamp % this.perceptionFrequency) == this.perceptionFrequencyOffset) {
                this.attentionAndPerception();
            }
        }
        else {
            // during inference, do less perception to free up some CPU:
            if (this.perceptionFrequency == 0 || (timeStamp % (this.perceptionFrequency * 2)) == this.perceptionFrequencyOffset) {
                this.attentionAndPerception();
            }
        }
        // 2) Short-term memory loop:
        this.shortTermMemory.activationUpdate();
        // 3) Rule execution:
        this.inferenceUpdate();
        // 4) Conversation context update (see if we need to reask questions):
        this.conversationUpdate();
        // 5) Check for active goals:
        if (this.considerGoals())
            this.goalsUpdate(timeStamp);
        // 6) Intention execution:
        this.executeIntentions();
        // 7) Performatives that have already been parsed, but are queued:
        if (this.queuedParsedPerformatives.length > 0 && this.isIdle()) {
            var tmp = this.queuedParsedPerformatives[0];
            this.reactToParsedPerformativeInternal(tmp[0], tmp[1], tmp[2], tmp[3]);
            this.queuedParsedPerformatives.splice(0, 1);
        }
    };
    RuleBasedAI.prototype.isIdle = function () {
        var expectingAnswerToQuestion = false;
        for (var _i = 0, _a = this.contexts; _i < _a.length; _i++) {
            var context = _a[_i];
            if (context.expectingAnswerToQuestion_stack.length > 0) {
                expectingAnswerToQuestion = true;
                break;
            }
        }
        return this.intentions.length == 0 && this.queuedIntentions.length == 0 &&
            !expectingAnswerToQuestion &&
            this.currentInferenceProcess == null && this.queuedInferenceProcesses.length == 0;
    };
    RuleBasedAI.prototype.addLongTermTerm = function (t, provenance) {
        this.addLongTermTermWithTimeAndSign(t, provenance, this.timeStamp, true);
    };
    RuleBasedAI.prototype.addLongTermTermWithTime = function (t, provenance, time) {
        this.addLongTermTermWithTimeAndSign(t, provenance, time, true);
    };
    RuleBasedAI.prototype.addLongTermTermWithSign = function (t, provenance, sign) {
        this.addLongTermTermWithTimeAndSign(t, provenance, this.timeStamp, sign);
    };
    RuleBasedAI.prototype.addLongTermTermWithTimeAndSign = function (t, provenance, time, sign) {
        // intentions:
        if (t.functor == this.cache_sort_intention && sign) {
            this.intentions.push(new IntentionRecord(t.attributes[0].term, t.attributes.length > 0 ? t.attributes[1] : null, null, null, time));
            return;
        }
        if (t.functor.is_a(this.cache_sort_stateSort)) {
            if (this.longTermMemory.addStateSentenceIfNew(new Sentence([t], [sign]), provenance, 1, time)) {
                // term added
                for (var _i = 0, _a = this.contexts; _i < _a.length; _i++) {
                    var context = _a[_i];
                    context.newLongTermStateTerm(t);
                }
                this.reactiveBehaviorUpdate(t);
            }
        }
        else {
            if (this.longTermMemory.addSentenceIfNew(new Sentence([t], [sign]), provenance, 1, time)) {
                // term added
                for (var _b = 0, _c = this.contexts; _b < _c.length; _b++) {
                    var context = _c[_b];
                    context.newLongTermTerm(t);
                }
                this.reactiveBehaviorUpdate(t);
            }
        }
    };
    RuleBasedAI.prototype.removeLongTermTermMatchingWith = function (t) {
        var se = this.longTermMemory.containsUnifyingTerm(t);
        if (se != null)
            this.longTermMemory.removeInternal(se);
    };
    RuleBasedAI.prototype.addShortTermTerm = function (t, provenance) {
        if (!this.shortMemoryToLongTermMemoryFilter(t, provenance)) {
            // intentions:
            if (t.functor == this.cache_sort_intention) {
                this.intentions.push(new IntentionRecord(t.attributes[0].term, t.attributes.length > 0 ? t.attributes[1] : null, null, null, this.timeStamp));
                return;
            }
            // we add 1 since "this.shortTermMemory.activationUpdate()" will be executed immediately
            // afterwards, decreasing it by 1 right away.
            if (t.functor.is_a(this.cache_sort_stateSort)) {
                if (this.shortTermMemory.addStateTermIfNew(t, provenance, this.perceptionMemoryTime + 1, this.timeStamp)) {
                    // new term was added:
                    this.reactiveBehaviorUpdate(t);
                }
            }
            else {
                if (this.shortTermMemory.addTermIfNew(t, provenance, this.perceptionMemoryTime + 1, this.timeStamp)) {
                    // new term was added:
                    this.reactiveBehaviorUpdate(t);
                }
            }
        }
    };
    RuleBasedAI.prototype.addLongTermRuleNow = function (s, provenance) {
        this.longTermMemory.addSentence(s, provenance, 1, this.timeStamp);
    };
    RuleBasedAI.prototype.addLongTermRule = function (s, provenance, time) {
        this.longTermMemory.addSentence(s, provenance, 1, time);
    };
    RuleBasedAI.prototype.removeLongTermRule = function (s) {
        this.longTermMemory.removeSentence(s);
    };
    RuleBasedAI.prototype.addEpisodeTerm = function (t, provenance) {
        this.currentEpisodeTerms.push(t);
        var term = Term.fromString(t, this.o);
        this.longTermMemory.addSentence(new Sentence([term], [true]), provenance, 1, this.timeStamp);
    };
    RuleBasedAI.prototype.clearEpisodeTerms = function () {
        for (var _i = 0, _a = this.currentEpisodeTerms; _i < _a.length; _i++) {
            var t = _a[_i];
            var term = Term.fromString(t, this.o);
            this.longTermMemory.removeSentence(new Sentence([term], [true]));
        }
        this.currentEpisodeTerms = [];
    };
    RuleBasedAI.prototype.loadLongTermRulesFromFile = function (rulesFileName) {
        var xmlhttp = new XMLHttpRequest();
        xmlhttp.overrideMimeType("text/xml");
        xmlhttp.open("GET", rulesFileName, false);
        xmlhttp.send();
        this.loadLongTermRulesFromXML(xmlhttp.responseXML.documentElement);
    };
    RuleBasedAI.prototype.loadLongTermRulesFromXML = function (xml) {
        for (var _i = 0, _a = getElementChildrenByTag(xml, "sentence"); _i < _a.length; _i++) {
            var sentence_xml = _a[_i];
            var rule = Sentence.fromString(sentence_xml.getAttribute("sentence"), this.o);
            if (rule.terms.length == 1 &&
                rule.terms[0].functor.name == "name" &&
                rule.terms[0].attributes.length == 2 &&
                (rule.terms[0].attributes[1] instanceof ConstantTermAttribute)) {
                var name_1 = rule.terms[0].attributes[1].value;
                if (name_1.indexOf(" ") != -1 &&
                    this.naturalLanguageParser.posParser.multitokens_plainlist.indexOf(name_1) == -1) {
                    console.error("Missing multitoken: " + name_1);
                }
            }
            var provenance = sentence_xml.getAttribute("provenance");
            var time = this.timeStamp;
            if (sentence_xml.getAttribute("time") != null)
                time = Number(sentence_xml.getAttribute("time"));
            var history_2 = [rule];
            var hasPrevious = false;
            sentence_xml = getFirstElementChildByTag(sentence_xml, "previousSentence");
            while (sentence_xml != null) {
                var rule2 = Sentence.fromString(sentence_xml.getAttribute("sentence"), this.o);
                var provenance_1 = sentence_xml.getAttribute("provenance");
                //let timeEnd:number = this.timeStamp;
                hasPrevious = true;
                if (sentence_xml.getAttribute("time") != null)
                    time = Number(sentence_xml.getAttribute("time"));
                //if (sentence_xml.getAttribute("timeEnd") != null) timeEnd = Number(sentence_xml.getAttribute("timeEnd"));
                if (provenance_1 == BACKGROUND_PROVENANCE ||
                    provenance_1 == ONTOLOGY_PROVENANCE) {
                    // this was a sentence that already was in the BK, so no need to add it
                    sentence_xml = null; // end of recursion
                }
                else {
                    history_2.push(rule2);
                    sentence_xml = getFirstElementChildByTag(sentence_xml, "previousSentence");
                }
            }
            history_2.reverse();
            // we add the sentences in reverse order, to reconstruct the "previousSentence" structure:
            for (var _b = 0, history_1 = history_2; _b < history_1.length; _b++) {
                var s = history_1[_b];
                if (hasPrevious) {
                    this.longTermMemory.addStateSentenceIfNew(s, provenance, 1, time);
                }
                else {
                    this.longTermMemory.addSentence(s, provenance, 1, time);
                }
            }
        }
        for (var _c = 0, _d = getElementChildrenByTag(xml, "previousSentence"); _c < _d.length; _c++) {
            var sentence_xml = _d[_c];
            var rule = Sentence.fromString(sentence_xml.getAttribute("sentence"), this.o);
            var provenance = sentence_xml.getAttribute("provenance");
            var time = this.timeStamp;
            var timeEnd = this.timeStamp;
            if (sentence_xml.getAttribute("time") != null)
                time = Number(sentence_xml.getAttribute("time"));
            if (sentence_xml.getAttribute("timeEnd") != null)
                timeEnd = Number(sentence_xml.getAttribute("timeEnd"));
            this.longTermMemory.addPreviousSentence(rule, provenance, 1, time, timeEnd, null);
        }
    };
    RuleBasedAI.prototype.attentionAndPerception = function () {
    };
    RuleBasedAI.prototype.clearPerception = function () {
        this.perceptionBuffer = [];
    };
    RuleBasedAI.prototype.addTermToPerception = function (term) {
        // console.log("addTermToPerception: " + term.toString());
        this.perceptionBuffer.push(term);
        this.perceptionToShortMemoryFilter(term);
    };
    RuleBasedAI.prototype.perceptionToShortMemoryFilter = function (term) {
        /*
        let action:Sort = this.o.getSort("actionverb");
        if (action.subsumes(term.functor)) {
            this.addShortTermTerm(term);
            return true;
        }
        return false;
        */
        // only filter time:
        if (term.functor == this.cache_sort_time_current)
            return false;
        this.addShortTermTerm(term, PERCEPTION_PROVENANCE);
        return true;
    };
    RuleBasedAI.prototype.shortMemoryToLongTermMemoryFilter = function (term, provenance) {
        var storeInLongTerm = false;
        for (var _i = 0, _a = this.predicatesToStoreInLongTermMemory; _i < _a.length; _i++) {
            var sort = _a[_i];
            if (sort.subsumes(term.functor)) {
                storeInLongTerm = true;
                break;
            }
        }
        if (storeInLongTerm) {
            this.addLongTermTerm(term, provenance);
            return true;
        }
        else if (term.functor.is_a(this.cache_sort_stateSort)) {
            // check if we have any contradiction with long term memory, and remove the contradiction:
            var s_l = this.longTermMemory.previousStateSentencesToReplace(term, true);
            for (var _b = 0, s_l_1 = s_l; _b < s_l_1.length; _b++) {
                var s = s_l_1[_b];
                if (term.equalsNoBindings(s.terms[0]) != 1) {
                    // only if the new term is different from the previous one, we need to do any update:
                    this.longTermMemory.removeSentence(s);
                }
            }
        }
        return false;
    };
    RuleBasedAI.prototype.reactiveBehaviorUpdate = function (t) {
    };
    RuleBasedAI.prototype.parsePerceivedText = function (text, speaker, context, actionTerms) {
        var parses = this.naturalLanguageParser.parse(text, this.cache_sort_performative, context, this);
        if (parses == null || parses.length == 0 && this.naturalLanguageParser.error_semantic.length > 0) {
            // if we cannot parse sentences in any other way, at least consider the semantic errors as the parses:
            parses = this.naturalLanguageParser.error_semantic;
        }
        if (parses != null && parses.length > 0) {
            var HPparse = this.naturalLanguageParser.chooseHighestPriorityParse(parses);
            console.log("AIRuleBasedAI(" + this.selfID + "): parsed sentence '" + text + "'\n  " + HPparse.result);
            // the parse might contain several performatives combined with a "#list" construct
            var parsePerformatives = Term.elementsInList(HPparse.result, "#list");
            var actionTerms2 = [];
            for (var _i = 0, actionTerms_1 = actionTerms; _i < actionTerms_1.length; _i++) {
                var actionTerm = actionTerms_1[_i];
                for (var _a = 0, parsePerformatives_1 = parsePerformatives; _a < parsePerformatives_1.length; _a++) {
                    var parsePerformative = parsePerformatives_1[_a];
                    var tmp = actionTerm.clone([]);
                    tmp.addAttribute(parsePerformative);
                    actionTerms2.push(tmp);
                }
            }
            actionTerms.splice(0, actionTerms.length);
            for (var _b = 0, actionTerms2_1 = actionTerms2; _b < actionTerms2_1.length; _b++) {
                var actionTerm = actionTerms2_1[_b];
                actionTerms.push(actionTerm);
            }
            this.reactToParsedPerformatives(parsePerformatives, text, speaker, HPparse);
        }
        else {
            console.warn("A4RuleBasedAI (" + this.selfID + "): cannot parse sentence: " + text);
            if (this.naturalLanguageParser.error_semantic.length > 0)
                console.warn("    semantic error!");
            if (this.naturalLanguageParser.error_deref.length > 0)
                console.warn("    (" + this.selfID + ") could not deref expressions: " + this.naturalLanguageParser.error_deref);
            if (this.naturalLanguageParser.error_unrecognizedTokens.length > 0)
                console.warn("    unrecognized tokens: " + this.naturalLanguageParser.error_unrecognizedTokens);
            if (this.naturalLanguageParser.error_grammatical)
                console.warn("    grammatical error!");
            this.reactToParseError(speaker, text);
        }
    };
    RuleBasedAI.prototype.reactToParsedPerformatives = function (performatives, text, speaker, parse) {
        if (speaker != this.selfID) {
            // When an utterance results in more than one performative, we just process the first, and queue the
            // rest. This is because requests consisting of several actions in a row would fail otherwise. 
            // For example: "open the crate and take a cable from it". The "take a cable from it" would fail until
            // the crate is open, since the crate is innitially closed.
            var first = true;
            for (var _i = 0, performatives_1 = performatives; _i < performatives_1.length; _i++) {
                var performative_att = performatives_1[_i];
                if (first) {
                    var performative = performative_att.term;
                    // is it talking to us?
                    var context = this.contextForSpeaker(speaker);
                    if (this.talkingToUs(context, speaker, performative)) {
                        this.reactToParsedPerformativeInternal(performative, text, speaker, parse);
                        first = false;
                    }
                    else {
                        // not talking to us, ignore the rest
                        break;
                    }
                }
                else {
                    var performative = performative_att.term;
                    this.queuedParsedPerformatives.push([performative, text, speaker, parse]);
                }
            }
        }
    };
    RuleBasedAI.prototype.reactToParsedPerformativeInternal = function (performative, text, speaker, parse) {
        var toAdd = [];
        var context = this.contextForSpeaker(speaker);
        // Since now we know they are talking to us, we can unify the LISTENER with ourselves:
        this.terminateConversationAfterThisPerformative = false;
        var perf2 = this.naturalLanguageParser.unifyListener(performative, this.selfID);
        if (perf2 == null)
            perf2 = performative;
        var nIntentions = this.intentions.length;
        var nQueuedIntentions = this.queuedIntentions.length;
        var tmp = this.reactToPerformative(perf2, new ConstantTermAttribute(speaker, this.cache_sort_id), context);
        if (tmp != null)
            toAdd = toAdd.concat(tmp);
        var nlcp = context.newPerformative(speaker, text, perf2, parse, null, null, this.o, this.timeStamp);
        // add this performative to all the new intentions:
        if (nlcp.length > 0) {
            for (var i = nIntentions; i < this.intentions.length; i++) {
                if (this.intentions[i].requestingPerformative == null) {
                    this.intentions[i].requestingPerformative = nlcp[0];
                }
            }
            for (var i = nQueuedIntentions; i < this.queuedIntentions.length; i++) {
                if (this.queuedIntentions[i].requestingPerformative == null) {
                    this.queuedIntentions[i].requestingPerformative = nlcp[0];
                }
            }
        }
        if (this.terminateConversationAfterThisPerformative)
            context.endConversation();
        for (var _i = 0, toAdd_1 = toAdd; _i < toAdd_1.length; _i++) {
            var t2 = toAdd_1[_i];
            console.log("reactToParsedPerformatives.toAdd: " + t2);
            this.addShortTermTerm(t2, REACTION_PROVENANCE);
        }
    };
    RuleBasedAI.prototype.reactToParseError = function (speakerID, sentence) {
        var context = this.contextForSpeakerWithoutCreatingANewOne(speakerID);
        if (context != null) {
            if (this.talkingToUs(context, speakerID, null)) {
                // respond!
                if (this.naturalLanguageParser.error_semantic.length > 0) {
                    console.log(this.selfID + ": semantic error when parsing a performative from " + speakerID);
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + this.selfID + "'[#id], #and(S:[sentence],the(S, [singular]))))))", this.o), null, null, null, this.timeStamp));
                }
                else if (this.naturalLanguageParser.error_deref.length > 0) {
                    var error = null;
                    var tmp = null;
                    var errorType = 0;
                    var tokensLeftToParse = null;
                    for (var _i = 0, _a = this.naturalLanguageParser.error_deref; _i < _a.length; _i++) {
                        var e = _a[_i];
                        if (e.derefFromContextError != null) {
                            if (tokensLeftToParse == null || e.tokensLeftToParse < tokensLeftToParse) {
                                error = e;
                                tmp = e.derefFromContextError;
                                errorType = e.derefErrorType;
                                tokensLeftToParse = e.tokensLeftToParse;
                                console.log("reporting derefFromContextError:" + tmp);
                            }
                        }
                        else if (e.derefUniversalError != null) {
                            if (tokensLeftToParse == null || e.tokensLeftToParse < tokensLeftToParse) {
                                error = e;
                                tmp = e.derefUniversalError;
                                errorType = e.derefErrorType;
                                tokensLeftToParse = e.tokensLeftToParse;
                                console.log("reporting derefUniversalError: " + tmp);
                            }
                        }
                        else if (e.derefHypotheticalError != null) {
                            if (tokensLeftToParse == null || e.tokensLeftToParse < tokensLeftToParse) {
                                error = e;
                                tmp = e.derefHypotheticalError;
                                errorType = e.derefErrorType;
                                tokensLeftToParse = e.tokensLeftToParse;
                                console.log("reporting derefHypotheticalError: " + tmp);
                            }
                        }
                        else if (e.derefQueryError != null) {
                            if (tokensLeftToParse == null || e.tokensLeftToParse < tokensLeftToParse) {
                                error = e;
                                tmp = e.derefQueryError;
                                errorType = e.derefErrorType;
                                console.log("reporting derefQueryError: " + tmp);
                                tokensLeftToParse = e.tokensLeftToParse;
                            }
                        }
                        // some times there are many entries with error of type "DEREF_ERROR_CANNOT_PROCESS_EXPRESSION",
                        // if there are entries with another type of error, prioritize those:
                        //if (errorType == DEREF_ERROR_NO_REFERENTS ||
                        //	errorType == DEREF_ERROR_CANNOT_DISAMBIGUATE) break;
                    }
                    // record the performative, even if we could not parse it (just in case there is a "perf.rephrase.entity" afterwards):
                    context.newPerformative(context.speaker, sentence, null, null, [error], null, this.o, this.timeStamp);
                    if (errorType == DEREF_ERROR_NO_REFERENTS) {
                        this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.see('" + this.selfID + "'[#id], " + tmp + "))))", this.o), null, null, null, this.timeStamp));
                    }
                    else if (errorType == DEREF_ERROR_CANNOT_DISAMBIGUATE) {
                        this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.disambiguate('" + this.selfID + "'[#id], " + tmp + ")))))", this.o), null, null, null, this.timeStamp));
                    }
                    else {
                        //		    			this.intentions.push(new IntentionRecord(Term.fromString("action.talk('"+this.selfID+"'[#id], perf.inform.parseerror('"+context.speaker+"'[#id], #not(verb.understand('"+this.selfID+"'[#id], "+tmp+"))))", this.o), null, null, null, this.timeStamp));
                        this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.parse('" + this.selfID + "'[#id], #and(S:[sentence],the(S, [singular])))))))", this.o), null, null, null, this.timeStamp));
                    }
                }
                else if (this.naturalLanguageParser.error_unrecognizedTokens.length > 0) {
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.understand('" + this.selfID + "'[#id], '" + this.naturalLanguageParser.error_unrecognizedTokens[0] + "'[symbol]))))", this.o), null, null, null, this.timeStamp));
                }
                else if (this.naturalLanguageParser.error_grammatical) {
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.parse('" + this.selfID + "'[#id], #and(S:[sentence],the(S, [singular])))))))", this.o), null, null, null, this.timeStamp));
                }
            }
            else {
                console.log("reactiveBehaviorUpdateToParseError(" + this.selfID + "): no need to react, since we are not currently talking to " + speakerID);
            }
        }
        else {
            console.log("reactiveBehaviorUpdateToParseError(" + this.selfID + "): no need to react, since we don't have a context for " + speakerID);
        }
    };
    RuleBasedAI.prototype.reactToPerformative = function (perf2, speaker, context) {
        var reaction = [];
        var performativeHandled = false;
        var newExpectingThankyou = false;
        if (context.expectingAnswerToQuestion_stack.length > 0) {
            if (perf2.functor.name == "perf.inform") {
                // determine if it's a proper answer:
                reaction = this.reactToAnswerPerformative(perf2, speaker, context);
                if (reaction == null) {
                    var t2 = Term.fromString("action.memorize('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                    t2.addAttribute(perf2.attributes[1]);
                    this.intentions.push(new IntentionRecord(t2, null, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.invalidanswer('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], " + context.expectingAnswerToQuestion_stack[context.expectingAnswerToQuestion_stack.length - 1].performative + ")", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    context.popLastQuestion(); // remove the question, since we will ask it again
                }
                performativeHandled = true;
            }
            else if (perf2.functor.is_a(this.o.getSort("perf.inform.answer")) ||
                perf2.functor.is_a(this.o.getSort("perf.ack.ok"))) {
                // determine if it's a proper answer:
                reaction = this.reactToAnswerPerformative(perf2, speaker, context);
                if (reaction == null) {
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.invalidanswer('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], " + context.expectingAnswerToQuestion_stack[context.expectingAnswerToQuestion_stack.length - 1].performative + ")", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    context.popLastQuestion(); // remove the question, since we will ask it again
                }
                performativeHandled = true;
            }
            else if (perf2.functor.is_a(this.o.getSort("perf.question"))) {
                // in this case, we accept the performative. It will be handled below
            }
            else if (perf2.functor.is_a(this.o.getSort("perf.request.action")) ||
                perf2.functor.is_a(this.o.getSort("perf.request.stopaction"))) {
                // in this case, we accept the performative. It will be handled below
            }
            else {
                this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.invalidanswer('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], " + context.expectingAnswerToQuestion_stack[context.expectingAnswerToQuestion_stack.length - 1].performative + ")", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                context.popLastQuestion(); // remove the question, since we will ask it again
                performativeHandled = true;
            }
        }
        else if (context.expectingConfirmationToRequest_stack.length > 0) {
            if (perf2.functor.is_a(this.o.getSort("perf.ack.ok"))) {
                // ok, clear requests:
                context.expectingConfirmationToRequest_stack = [];
                context.expectingConfirmationToRequestTimeStamp_stack = [];
                performativeHandled = true;
            }
            else if (perf2.functor.is_a(this.o.getSort("perf.ack.denyrequest"))) {
                context.expectingConfirmationToRequest_stack = [];
                context.expectingConfirmationToRequestTimeStamp_stack = [];
                performativeHandled = true;
                var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.ok('" + context.speaker + "'[#id]))", this.o);
                this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            if (perf2.functor.is_a(this.o.getSort("perf.inform.answer"))) {
                // TODO: we should probably check if it's a valid answer, but for now just clear the queues
                context.expectingConfirmationToRequest_stack = [];
                context.expectingConfirmationToRequestTimeStamp_stack = [];
                performativeHandled = true;
                if (perf2.attributes.length >= 2 &&
                    perf2.attributes[1] instanceof ConstantTermAttribute) {
                    var answer = perf2.attributes[1].value;
                    if (answer == "no") {
                        var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.ok('" + context.speaker + "'[#id]))", this.o);
                        this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    }
                }
            }
        }
        if (!performativeHandled) {
            if (perf2.functor.name == "perf.callattention") {
                if (context.speaker != "qwerty" && context.speaker != "shrdlu" && context.speaker != "etaoin") {
                    // we only confirm to the player, since otherwise, the AIs get all confused in loops some times
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.answer('" + context.speaker + "'[#id],'yes'[symbol]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                }
            }
            else if (perf2.functor.name == "perf.greet") {
                if (!context.expectingGreet) {
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.greet('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                }
            }
            else if (perf2.functor.name == "perf.farewell") {
                if (!context.expectingFarewell) {
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.farewell('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                }
                context.inConversation = false;
            }
            else if (perf2.functor.name == "perf.nicetomeetyou") {
                this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.nicetomeetyoutoo('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.nicetomeetyoutoo") {
                // just ignore ...
            }
            else if (perf2.functor.name == "perf.thankyou") {
                // If the "thank you" was necessary, then respond with a "you are welcome":
                if (context.expectingThankYou) {
                    newExpectingThankyou = false;
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.youarewelcome('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                }
            }
            else if (perf2.functor.name == "perf.youarewelcome") {
                // Do nothing
            }
            else if (perf2.functor.name == "perf.q.howareyou") {
                this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.answer('" + context.speaker + "'[#id],'fine'[symbol]))", this.o), speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.ack.ok") {
                // Do nothing
            }
            else if (perf2.functor.name == "perf.ackresponse") {
                // Do nothing
            }
            else if (perf2.functor.name == "perf.ack.contradict") {
                console.error("RuleBasedAI.reactToPerformative: not sure how to react to " + perf2);
            }
            else if (perf2.functor.name == "perf.inform") {
                this.handlePerfInform(perf2, speaker, context);
            }
            else if (perf2.functor.name == "perf.inform.answer") {
                if (!this.reportDereferenceErrorIfNoTokensLeftToParse(context)) {
                    var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform('" + context.speaker + "'[#id], #and(#not(X:verb.ask('" + this.selfID + "'[#id], 'pronoun.anything'[pronoun.anything])), time.past(X))))", this.o);
                    this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                }
            }
            else if (perf2.functor.name == "perf.q.predicate") {
                this.handlePerfQPredicate(perf2, speaker, context);
            }
            else if (perf2.functor.name == "perf.q.predicate-negated") {
                var t2 = Term.fromString("action.answer.predicate-negated('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.whereis") {
                var t2 = Term.fromString("action.answer.whereis('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.whereto") {
                var t2 = Term.fromString("action.answer.whereto('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.whois.name") {
                var t2 = Term.fromString("action.answer.whois.name('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.whois.noname") {
                var t2 = Term.fromString("action.answer.whois.noname('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.whatis.name") {
                var t2 = Term.fromString("action.answer.whatis.name('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                t2.addAttribute(perf2.attributes[1]);
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.whatis.noname") {
                var t2 = Term.fromString("action.answer.whatis.noname('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                t2.addAttribute(perf2.attributes[1]);
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.query") {
                var t2 = Term.fromString("action.answer.query('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                //				(<TermTermAttribute>t2.attributes[0]).term.addAttribute(perf2.attributes[1]);
                //				(<TermTermAttribute>t2.attributes[0]).term.addAttribute(perf2.attributes[2]);
                t2.addAttribute(new TermTermAttribute(perf2));
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.query-followup") {
                var t2 = Term.fromString("action.answer.query-followup('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                t2.addAttribute(perf2.attributes[1]);
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.howmany") {
                var t2 = Term.fromString("action.answer.howmany('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                //				(<TermTermAttribute>t2.attributes[0]).term.addAttribute(perf2.attributes[1]);
                //				(<TermTermAttribute>t2.attributes[0]).term.addAttribute(perf2.attributes[2]);
                t2.addAttribute(new TermTermAttribute(perf2));
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.when") {
                var t2 = Term.fromString("action.answer.when('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.why") {
                var t2 = Term.fromString("action.answer.why('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.how") {
                var t2 = Term.fromString("action.answer.how('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.q.distance") {
                var t2 = Term.fromString("action.answer.distance('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                for (var i = 1; i < perf2.attributes.length; i++) {
                    t2.addAttribute(perf2.attributes[i]);
                }
                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.request.action" ||
                perf2.functor.name == "perf.q.action") {
                this.reactToRequestActionPerformative(perf2, speaker, context);
            }
            else if (perf2.functor.name == "perf.request.stopaction") {
                if (perf2.attributes[1] instanceof TermTermAttribute) {
                    var action = (perf2.attributes[1]).term;
                    if (perf2.attributes.length >= 3 &&
                        perf2.attributes[2] instanceof TermTermAttribute) {
                        // this means that the action request has a variable and we need to start an inference process:
                        /*
                        let intention_l:Term[] = NLParser.termsInList((<TermTermAttribute>perf2.attributes[2]).term, "#and");;
                        let target1Terms:Term[] = [];
                        let target1Signs:boolean[] = [];
                        for(let i:number = 0;i<intention_l.length;i++) {
                            if (intention_l[i].functor.name == "#not") {
                                target1Terms.push((<TermTermAttribute>(intention_l[i].attributes[0])).term);
                                target1Signs.push(true);
                            } else {
                                target1Terms.push(intention_l[i]);
                                target1Signs.push(false);
                            }
                        }

                        // 2) start the inference process:
                        let target1:Sentence[] = [];
                        target1.push(new Sentence(target1Terms, target1Signs));*/
                        var targets = [];
                        var negatedExpression = new Term(this.o.getSort("#not"), [new TermTermAttribute(perf2.attributes[2].term)]);
                        var target = Term.termToSentences(negatedExpression, this.o);
                        targets.push(target);
                        var ir = new InferenceRecord(this, [], targets, 1, 0, false, null, new StopAction_InferenceEffect(action));
                        // let ir:InferenceRecord = new InferenceRecord(this, [], [target1], 1, 0, false, null, new StopAction_InferenceEffect(action));
                        ir.triggeredBy = perf2;
                        ir.triggeredBySpeaker = context.speaker;
                        this.queuedInferenceProcesses.push(ir);
                    }
                    else {
                        if (this.stopAction(action, context.speaker)) {
                            // account for the fact that maybe the request was to stop talking
                            if (!this.terminateConversationAfterThisPerformative) {
                                var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.ok('" + context.speaker + "'[#id]))", this.o);
                                this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                            }
                        }
                        else {
                            var tmp = "action.talk('" + this.selfID + "'[#id], perf.ack.denyrequest('" + context.speaker + "'[#id]))";
                            var term = Term.fromString(tmp, this.o);
                            var cause = Term.fromString("#not(" + action + ")", this.o);
                            this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), new CauseRecord(cause, null, this.timeStamp), this.timeStamp));
                        }
                    }
                }
                else {
                    var tmp = "action.talk('" + this.selfID + "'[#id], perf.ack.denyrequest('" + context.speaker + "'[#id]))";
                    var term = Term.fromString(tmp, this.o);
                    this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                }
            }
            else if (perf2.functor.name == "perf.moreresults") {
                if (context.lastEnumeratedQuestion_answered != null) {
                    this.reactToMoreResultsPerformative(perf2, speaker, context);
                    newExpectingThankyou = true;
                }
                else {
                    // we don't understand this question:
                    var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform('" + context.speaker + "'[#id],#not(verb.understand('" + this.selfID + "'[#id]))))", this.o);
                    this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                }
            }
            else if (perf2.functor.name == "perf.request.repeataction") {
                if (context.lastEnumeratedQuestion_answered != null) {
                    this.reactToMoreResultsPerformative(perf2, speaker, context);
                    newExpectingThankyou = true;
                }
                else {
                    if (!this.reactToRepeatActionPerformative(perf2, speaker, context)) {
                        var tmp = "action.talk('" + this.selfID + "'[#id], perf.ack.denyrequest('" + context.speaker + "'[#id]))";
                        var term = Term.fromString(tmp, this.o);
                        this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    }
                }
            }
            else if (perf2.functor.name == "perf.ack.denyrequest") {
                var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.ok('" + context.speaker + "'[#id]))", this.o);
                this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
            else if (perf2.functor.name == "perf.rephrase.entity") {
                this.reactToRephraseEntityPerformative(perf2, speaker, context);
            }
            else if (perf2.functor.name == "perf.changemind") {
                console.log("RuleBasedAI.reactToPerformative: nothing to do for " + perf2);
            }
            else {
                console.error("RuleBasedAI.reactToPerformative: unknown performative " + perf2);
            }
        }
        // update conversation state:
        context.expectingThankYou = newExpectingThankyou;
        context.expectingYouAreWelcome = false;
        context.expectingGreet = false;
        context.expectingFarewell = false;
        return reaction;
    };
    RuleBasedAI.prototype.handlePerfInform = function (perf2, speaker, context) {
        var t2 = Term.fromString("action.memorize('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
        t2.addAttribute(perf2.attributes[1]);
        this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
    };
    RuleBasedAI.prototype.handlePerfQPredicate = function (perf2, speaker, context) {
        var t2 = Term.fromString("action.answer.predicate('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
        for (var i = 1; i < perf2.attributes.length; i++) {
            t2.addAttribute(perf2.attributes[i]);
        }
        this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
    };
    RuleBasedAI.prototype.replaceID = function (t, id, replacement) {
        for (var i = 0; i < t.attributes.length; i++) {
            if (t.attributes[i] instanceof ConstantTermAttribute) {
                if (t.attributes[i] == id) {
                    t.attributes[i] = replacement;
                }
            }
            else if (t.attributes[i] instanceof TermTermAttribute) {
                this.replaceID(t.attributes[i].term, id, replacement);
            }
        }
    };
    RuleBasedAI.prototype.reactToRequestActionPerformative = function (perf2, speaker, context) {
        this.clearEpisodeTerms();
        if (perf2.attributes[1] instanceof TermTermAttribute) {
            var action = (perf2.attributes[1]).term;
            var needsInference = false;
            if (perf2.attributes.length >= 4 &&
                perf2.attributes[2] instanceof TermTermAttribute) {
                needsInference = true;
                for (var _i = 0, _a = this.intentionHandlers; _i < _a.length; _i++) {
                    var ih = _a[_i];
                    if (ih.canHandle(action, this)) {
                        if (ih.canHandleWithoutInference(perf2)) {
                            needsInference = false;
                            break;
                        }
                    }
                }
                if (needsInference) {
                    // Extract the time terms:
                    // ...
                    var forAlls = [];
                    if (perf2.attributes.length >= 5) {
                        forAlls = NLParser.termsInList((perf2.attributes[4]).term, "#and");
                    }
                    console.log("reactToRequestActionPerformative, forAlls: " + forAlls);
                    // this means that the action request has a variable and we need to start an inference process:
                    var targets = [];
                    var negatedExpression = new Term(this.o.getSort("#not"), [new TermTermAttribute(perf2.attributes[2].term)]);
                    var target = Term.termToSentences(negatedExpression, this.o);
                    targets.push(target);
                    // negate the forAlls:
                    for (var _b = 0, forAlls_1 = forAlls; _b < forAlls_1.length; _b++) {
                        var forAll = forAlls_1[_b];
                        if (forAll.attributes.length >= 2 &&
                            forAll.attributes[1] instanceof TermTermAttribute) {
                            var forAllTerm = (forAll.attributes[1]).term;
                            var negatedForAll = Term.termToSentences(new Term(this.o.getSort("#not"), [new TermTermAttribute(forAllTerm)]), this.o);
                            targets.push(negatedForAll);
                        }
                    }
                    // 2) start the inference process:
                    var ir = new InferenceRecord(this, [], targets, 1, 0, false, null, new ExecuteAction_InferenceEffect(perf2));
                    ir.triggeredBy = perf2;
                    ir.triggeredBySpeaker = context.speaker;
                    this.queuedInferenceProcesses.push(ir);
                    return;
                }
            }
            if (perf2.attributes.length == 2 ||
                (perf2.attributes.length >= 4 && !needsInference)) {
                // First check if the actor is us:
                var ir = new IntentionRecord(action, new ConstantTermAttribute(context.speaker, this.cache_sort_id), context.getNLContextPerformative(perf2), null, this.timeStamp);
                var tmp = this.canSatisfyActionRequest(ir);
                if (tmp == ACTION_REQUEST_CAN_BE_SATISFIED) {
                    this.queuedIntentions.push(ir);
                    // Request for executing an action that can be satisfied. However, maybe we might need to plan for it.
                    // So, invoke the planner (if it exists), before executing the action:
                    // this.planForAction(ir);
                }
                else if (tmp == ACTION_REQUEST_CANNOT_BE_SATISFIED) {
                    if (action.attributes.length >= 1 &&
                        (action.attributes[0] instanceof ConstantTermAttribute) &&
                        action.attributes[0].value == this.selfID) {
                        var tmp_1 = "action.talk('" + this.selfID + "'[#id], perf.ack.denyrequest('" + context.speaker + "'[#id]))";
                        var term = Term.fromString(tmp_1, this.o);
                        this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                    }
                    else {
                        // The action is not for us, just say "ok" :)
                        var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.ok('" + context.speaker + "'[#id]))", this.o);
                        this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                        // If the speaker is requesting an action for herself, then record it in the memory as a "verb.want":
                        var speakerID = null;
                        if (speaker instanceof ConstantTermAttribute)
                            speakerID = speaker.value;
                        if (action.attributes.length >= 1 &&
                            (action.attributes[0] instanceof ConstantTermAttribute) &&
                            action.attributes[0].value == speakerID) {
                            var term2 = new Term(this.o.getSort("verb.want"), [speaker, new TermTermAttribute(action)]);
                            var term3 = Term.fromString("action.memorize('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                            term3.attributes.push(new TermTermAttribute(term2));
                            this.intentions.push(new IntentionRecord(term3, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
                        }
                    }
                }
            }
            else {
                var tmp = "action.talk('" + this.selfID + "'[#id], perf.ack.denyrequest('" + context.speaker + "'[#id]))";
                var term = Term.fromString(tmp, this.o);
                this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
            }
        }
        else {
            var tmp = "action.talk('" + this.selfID + "'[#id], perf.ack.denyrequest('" + context.speaker + "'[#id]))";
            var term = Term.fromString(tmp, this.o);
            this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf2), null, this.timeStamp));
        }
    };
    RuleBasedAI.prototype.reactToRepeatActionPerformative = function (perf, speaker, context) {
        this.clearEpisodeTerms();
        // to be handled by the classes that inherit from this one
        return false;
    };
    RuleBasedAI.prototype.reactToMoreResultsPerformative = function (perf, speaker, context) {
        if (context.lastEnumeratedQuestion_next_answer_index < context.lastEnumeratedQuestion_answers.length) {
            var resultsTA = null;
            if (context.lastEnumeratedQuestion_answers.length >
                context.lastEnumeratedQuestion_next_answer_index + this.maximum_answers_to_give_at_once_for_a_query) {
                resultsTA = new ConstantTermAttribute("etcetera", this.o.getSort("etcetera"));
                for (var i = 0; i < this.maximum_answers_to_give_at_once_for_a_query; i++) {
                    resultsTA = new TermTermAttribute(new Term(this.o.getSort("#and"), [context.lastEnumeratedQuestion_answers[context.lastEnumeratedQuestion_next_answer_index], resultsTA]));
                    context.lastEnumeratedQuestion_next_answer_index++;
                }
            }
            else {
                for (; context.lastEnumeratedQuestion_next_answer_index < context.lastEnumeratedQuestion_answers.length; context.lastEnumeratedQuestion_next_answer_index++) {
                    if (resultsTA == null) {
                        resultsTA = context.lastEnumeratedQuestion_answers[context.lastEnumeratedQuestion_next_answer_index];
                    }
                    else {
                        resultsTA = new TermTermAttribute(new Term(this.o.getSort("#and"), [context.lastEnumeratedQuestion_answers[context.lastEnumeratedQuestion_next_answer_index], resultsTA]));
                    }
                }
            }
            var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.answer('" + context.speaker + "'[#id]," + resultsTA + "))", this.o);
            // give more answers:
            this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
            //						context.lastEnumeratedQuestion_next_answer_index++;
        }
        else {
            // no more answers to be given:
            var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.answer('" + context.speaker + "'[#id],'no-matches-found'[symbol]))", this.o);
            this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
        }
    };
    RuleBasedAI.prototype.reactToRephraseEntityPerformative = function (perf, speaker, context) {
        // Find the previous performative:
        var previous = context.lastPerformativeByExcept(context.speaker, perf);
        if (previous == null) {
            // do not understand:
            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.understand('" + this.selfID + "'[#id], #and(S:[sentence],the(S, [singular])))))))", this.o), null, null, null, this.timeStamp));
        }
        if (previous.performative != null) {
            if (perf.attributes.length == 2 &&
                (perf.attributes[1] instanceof ConstantTermAttribute)) {
                // see if it has a single mention to an entity (other than attribute 0, which is the listener):
                var previousPerf = previous.performative;
                var IDs = [];
                for (var i = 1; i < previousPerf.attributes.length; i++) {
                    if (previousPerf.attributes[i] instanceof ConstantTermAttribute) {
                        IDs.push((previousPerf.attributes[i]));
                    }
                    else if (previousPerf.attributes[i] instanceof TermTermAttribute) {
                        NLContext.searchForIDsInClause(previousPerf.attributes[i].term, IDs, this.o);
                    }
                }
                console.log("IDs: " + IDs);
                if (IDs.length == 1) {
                    // repeat the previous performative, but replacing the previous ID by the new ID:
                    var newPerf = previousPerf.clone([]);
                    this.replaceID(newPerf, IDs[0], perf.attributes[1]);
                    this.reactToPerformative(newPerf, speaker, context);
                }
                else {
                    // do not understand:
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.understand('" + this.selfID + "'[#id], #and(S:[sentence],the(S, [singular])))))))", this.o), null, null, null, this.timeStamp));
                }
            }
            else {
                // TODO: Case not handled:
                // do not understand:
                this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.understand('" + this.selfID + "'[#id], #and(S:[sentence],the(S, [singular])))))))", this.o), null, null, null, this.timeStamp));
            }
        }
        else if (previous.derefErrors != null && previous.derefErrors.length > 0 &&
            previous.derefErrors[0].derefFromContextError != null &&
            (previous.derefErrors[0].derefFromContextError instanceof TermTermAttribute)) {
            var error = (previous.derefErrors[0].derefFromContextError);
            if (perf.attributes.length == 2 &&
                (perf.attributes[1] instanceof ConstantTermAttribute)) {
                // The previous performative was a parse error:
                console.log("reactToRephraseEntityPerformative: and previous performative was a deref error: " + error + " -> " + perf.attributes[1]);
                HandleRephrasing_InferenceEffect.handleRephrasing(previous.text, perf.attributes[1], error, context.speaker, this);
            }
            else if (perf.attributes.length >= 3 &&
                (perf.attributes[1] instanceof VariableTermAttribute)) {
                // Needs inference:
                var intention = new Term(this.o.getSort("action.answer.rephrase"), [new ConstantTermAttribute(this.selfID, this.o.getSort("#id")),
                    speaker,
                    new TermTermAttribute(perf),
                    new ConstantTermAttribute(previous.text, this.o.getSort("symbol")),
                    error]);
                AnswerQuery_IntentionAction.launchQueryInference(intention, null, this, "HandleRephrasing_InferenceEffect");
            }
        }
        else {
            // do not understand:
            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.understand('" + this.selfID + "'[#id], #and(S:[sentence],the(S, [singular])))))))", this.o), null, null, null, this.timeStamp));
        }
    };
    RuleBasedAI.prototype.canSatisfyActionRequest = function (ir) {
        var actionRequest = ir.action;
        var functor = actionRequest.functor;
        if (functor.name == "#and") {
            var actionRequest_l = NLParser.termsInList(actionRequest, "#and");
            actionRequest = actionRequest_l[0];
        }
        for (var _i = 0, _a = this.intentionHandlers; _i < _a.length; _i++) {
            var ih = _a[_i];
            if (ih.canHandle(actionRequest, this))
                return ACTION_REQUEST_CAN_BE_SATISFIED;
        }
        return ACTION_REQUEST_CANNOT_BE_SATISFIED;
    };
    RuleBasedAI.prototype.stopAction = function (actionRequest, requester) {
        if (actionRequest.functor.is_a(this.cache_sort_action_talk)) {
            if (actionRequest.attributes.length == 3 &&
                (actionRequest.attributes[1] instanceof VariableTermAttribute) &&
                (actionRequest.attributes[2] instanceof ConstantTermAttribute)) {
                var target = actionRequest.attributes[2].value;
                var context = this.contextForSpeaker(target);
                if (context != null)
                    this.terminateConversationAfterThisPerformative = true;
                return true;
            }
            else if (actionRequest.attributes.length == 1) {
                var context = this.contextForSpeaker(requester);
                if (context != null)
                    this.terminateConversationAfterThisPerformative = true;
                return true;
            }
        }
        else if (actionRequest.functor.is_a(this.cache_sort_action_think)) {
            if (actionRequest.attributes.length == 1) {
                // stop inference processes:
                this.currentInferenceProcess = null;
                this.queuedInferenceProcesses = [];
                return true;
            }
            else {
                return false;
            }
        }
        return false;
    };
    RuleBasedAI.prototype.reportDereferenceErrorIfNoTokensLeftToParse = function (context) {
        // If there were any dereference errors, report those instead:
        var tmp = null;
        var errorType = 0;
        for (var _i = 0, _a = this.naturalLanguageParser.error_deref; _i < _a.length; _i++) {
            var e = _a[_i];
            if (e.tokensLeftToParse > 0)
                continue;
            if (e.derefFromContextError != null) {
                if (errorType > e.derefErrorType)
                    continue;
                tmp = e.derefFromContextError;
                errorType = e.derefErrorType;
            }
            else if (e.derefUniversalError != null) {
                if (errorType > e.derefErrorType)
                    continue;
                tmp = e.derefUniversalError;
                errorType = e.derefErrorType;
            }
            else if (e.derefHypotheticalError != null) {
                if (errorType > e.derefErrorType)
                    continue;
                tmp = e.derefHypotheticalError;
                errorType = e.derefErrorType;
            }
            else if (e.derefQueryError != null) {
                if (errorType > e.derefErrorType)
                    continue;
                tmp = e.derefQueryError;
                errorType = e.derefErrorType;
            }
        }
        if (errorType == DEREF_ERROR_NO_REFERENTS) {
            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.see('" + this.selfID + "'[#id], " + tmp + "))))", this.o), null, null, null, this.timeStamp));
            return true;
        }
        else if (errorType == DEREF_ERROR_CANNOT_DISAMBIGUATE) {
            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform.parseerror('" + context.speaker + "'[#id], #not(verb.can('" + this.selfID + "'[#id], verb.disambiguate('" + this.selfID + "'[#id], " + tmp + ")))))", this.o), null, null, null, this.timeStamp));
            return true;
        }
        return false;
    };
    RuleBasedAI.prototype.reactToAnswerPerformative = function (perf, speaker, context) {
        var reaction = [];
        if (context.expectingAnswerToQuestion_stack.length == 0 ||
            context.expectingAnswerToQuestion_stack[context.expectingAnswerToQuestion_stack.length - 1] == null) {
            if (!this.reportDereferenceErrorIfNoTokensLeftToParse(context)) {
                // Otherwise, just say that we did not ask anything:
                var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.inform('" + context.speaker + "'[#id], #and(#not(X:verb.ask('" + this.selfID + "'[#id], 'pronoun.anything'[pronoun.anything])), time.past(X))))", this.o);
                this.intentions.push(new IntentionRecord(term, speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
            }
            return reaction;
        }
        var lastQuestion = context.expectingAnswerToQuestion_stack[context.expectingAnswerToQuestion_stack.length - 1];
        console.log("Checking if " + perf + " is a proper answer to " + lastQuestion.performative);
        if (lastQuestion.performative.functor.is_a(this.o.getSort("perf.q.predicate"))) {
            // perf.inform.answer(LISTENER, 'yes'[#id])
            if ((perf.functor.is_a(this.o.getSort("perf.inform")) && perf.attributes.length == 2)) {
                if ((perf.attributes[1] instanceof ConstantTermAttribute)) {
                    if ((perf.attributes[1]).value == "yes") {
                        var toMemorize = this.sentenceToMemorizeFromPredicateQuestion(lastQuestion.performative, true);
                        if (toMemorize == null) {
                            // not a proper answer to the question
                            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.invalidanswer('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                            context.popLastQuestion(); // remove the question, since we will ask it again
                            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], " + lastQuestion.performative + ")", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                            return reaction;
                        }
                        else {
                            for (var _i = 0, toMemorize_1 = toMemorize; _i < toMemorize_1.length; _i++) {
                                var t = toMemorize_1[_i];
                                var t2 = Term.fromString("action.memorize('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                                t2.addAttribute(new TermTermAttribute(t));
                                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                            }
                            context.popLastQuestion(); // remove the question, it's been answered
                            return reaction;
                        }
                    }
                    else if ((perf.attributes[1]).value == "no") {
                        var toMemorize = this.sentenceToMemorizeFromPredicateQuestion(lastQuestion.performative, false);
                        if (toMemorize == null) {
                            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.invalidanswer('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                            context.popLastQuestion(); // remove the question, since we will ask it again
                            this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], " + lastQuestion.performative + ")", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                            return reaction;
                        }
                        else {
                            for (var _a = 0, toMemorize_2 = toMemorize; _a < toMemorize_2.length; _a++) {
                                var t = toMemorize_2[_a];
                                var t2 = Term.fromString("action.memorize('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                                t2.addAttribute(new TermTermAttribute(t));
                                this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                            }
                            context.popLastQuestion(); // remove the question, it's been answered
                            return reaction;
                        }
                    }
                    else if ((perf.attributes[1]).value == "unknown") {
                        // nothing to do
                        context.popLastQuestion(); // remove the question, it's been answered
                        return [];
                    }
                    else {
                        console.error("unsuported answer to perf.q.predicate " + perf);
                        return null;
                    }
                }
                else {
                    var toMemorize = this.sentenceToMemorizeFromPredicateQuestionWithInformAnswer(lastQuestion.performative, perf);
                    if (toMemorize == null) {
                        this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.invalidanswer('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                        context.popLastQuestion(); // remove the question, since we will ask it again
                        this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], " + lastQuestion.performative + ")", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                        return reaction;
                    }
                    else {
                        for (var _b = 0, toMemorize_3 = toMemorize; _b < toMemorize_3.length; _b++) {
                            var t = toMemorize_3[_b];
                            var t2 = Term.fromString("action.memorize('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                            t2.addAttribute(new TermTermAttribute(t));
                            this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                        }
                        context.popLastQuestion(); // remove the question, it's been answered
                        return reaction;
                    }
                }
            }
            else {
                console.error("unsuported answer to perf.q.predicate " + perf);
                return null;
            }
        }
        else if (lastQuestion.performative.functor.is_a(this.o.getSort("perf.q.query"))) {
            if (perf.functor.is_a(this.o.getSort("perf.inform"))) {
                var toMemorize = this.sentenceToMemorizeFromQueryQuestion(lastQuestion.performative, perf);
                console.log("toMemorize: " + toMemorize);
                if (toMemorize == null) {
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], perf.ack.invalidanswer('" + context.speaker + "'[#id]))", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                    context.popLastQuestion(); // remove the question, since we will ask it again
                    this.intentions.push(new IntentionRecord(Term.fromString("action.talk('" + this.selfID + "'[#id], " + lastQuestion.performative + ")", this.o), speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                    return reaction;
                }
                else {
                    for (var _c = 0, toMemorize_4 = toMemorize; _c < toMemorize_4.length; _c++) {
                        var t = toMemorize_4[_c];
                        var t2 = Term.fromString("action.memorize('" + this.selfID + "'[#id], '" + context.speaker + "'[#id])", this.o);
                        t2.addAttribute(new TermTermAttribute(t));
                        this.intentions.push(new IntentionRecord(t2, speaker, context.getNLContextPerformative(perf), null, this.timeStamp));
                    }
                    context.popLastQuestion(); // remove the question, it's been answered
                    return reaction;
                }
            }
            else {
                console.error("unsuported answer to perf.q.query " + perf);
                return null;
            }
        }
        else if (lastQuestion.performative.functor.is_a(this.o.getSort("perf.q.action"))) {
            if ((perf.functor.is_a(this.o.getSort("perf.inform")) && perf.attributes.length == 2) ||
                (perf.functor.is_a(this.o.getSort("perf.inform.answer")) && perf.attributes.length == 3)) {
                if (perf.attributes.length == 3) {
                    var answerPredicate = perf.attributes[2];
                    var questionPredicate = lastQuestion.performative.attributes[1];
                    console.log("  - answerPredicate: " + answerPredicate);
                    console.log("  - questionPredicate: " + questionPredicate);
                    if (!(answerPredicate instanceof TermTermAttribute) ||
                        !(questionPredicate instanceof TermTermAttribute)) {
                        console.log("predicates are not terms!!");
                        return null;
                    }
                    var ap_term = answerPredicate.term;
                    var qp_term = questionPredicate.term;
                    if (ap_term.equalsNoBindings(qp_term) != 1) {
                        console.log("predicates do not match!!");
                        return null;
                    }
                }
                if ((perf.attributes[1] instanceof ConstantTermAttribute)) {
                    if ((perf.attributes[1]).value == "yes") {
                        // ...
                        context.popLastQuestion(); // remove the question, it's been answered
                        return [];
                    }
                    else if ((perf.attributes[1]).value == "no") {
                        // ...
                        context.popLastQuestion(); // remove the question, it's been answered
                        return [];
                    }
                    else if ((perf.attributes[1]).value == "unknown") {
                        // ...
                        context.popLastQuestion(); // remove the question, it's been answered
                        return [];
                    }
                    else {
                        console.error("unsuported answer to perf.q.action " + perf);
                        return null;
                    }
                }
                else {
                    console.error("unsuported answer to perf.q.action " + perf);
                    return null;
                }
            }
            else if (perf.functor.is_a(this.o.getSort("perf.ack.ok"))) {
                // ...
                context.popLastQuestion(); // remove the question, it's been answered
                return [];
            }
            else {
                console.error("unsuported answer to perf.q.action " + perf);
                return null;
            }
        }
        else {
            console.error("answers to questions of type " + lastQuestion.performative.functor + " not yet supported...");
            return null;
        }
    };
    RuleBasedAI.prototype.sentenceToMemorizeFromPredicateQuestion = function (predicateQuestion, answer) {
        //		console.log("sentenceToMemorizeFromPredicateQuestion.predicateQuestion: " + predicateQuestion);
        //		console.log("sentenceToMemorizeFromPredicateQuestion.answer: " + answer);
        if (!(predicateQuestion.attributes[1] instanceof TermTermAttribute))
            return [];
        var queryTerm = (predicateQuestion.attributes[1]).term;
        // if there are variables, that means there was a query involved, so, we don't know how to do it:
        if (queryTerm.getAllVariables().length != 0)
            return [];
        var queryTerms = Term.elementsInList(queryTerm, "#and");
        if (answer) {
            // we need to memorize each term:
            var toMemorize_l = [];
            for (var _i = 0, queryTerms_1 = queryTerms; _i < queryTerms_1.length; _i++) {
                var qt = queryTerms_1[_i];
                if (qt instanceof TermTermAttribute) {
                    toMemorize_l.push(qt.term);
                }
            }
            return toMemorize_l;
        }
        else {
            // one of them is wrong!
            var toMemorize = new Term(this.o.getSort("#not"), [queryTerms[0]]);
            for (var i = 1; i < queryTerms.length; i++) {
                toMemorize = new Term(this.o.getSort("#and"), [new TermTermAttribute(toMemorize),
                    new TermTermAttribute(new Term(this.o.getSort("#not"), [queryTerms[i]]))]);
            }
            return [toMemorize];
        }
        return [];
    };
    RuleBasedAI.prototype.sentenceToMemorizeFromPredicateQuestionWithInformAnswer = function (predicateQuestion, answerPerformative) {
        var answerTerm = answerPerformative.attributes[1];
        if ((answerTerm instanceof TermTermAttribute) &&
            answerTerm.term.functor.name == "proper-noun") {
            answerTerm = answerTerm.term.attributes[0];
        }
        if (!(predicateQuestion.attributes[1] instanceof TermTermAttribute))
            return [];
        var queryTerm = predicateQuestion.attributes[1];
        var queryTerms = Term.elementsInList(queryTerm.term, "#and");
        if (!(queryTerms[0] instanceof TermTermAttribute))
            return [];
        var mainQueryTerm = (queryTerms[0]).term;
        if (mainQueryTerm.functor.name == "verb.remember" ||
            mainQueryTerm.functor.name == "verb.know") {
            // in this case, it's basically a query in disguise:
            //			console.error("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: predicateQuestion = " + predicateQuestion);
            //			console.error("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: answerPerformative = " + answerPerformative);
            // replace the query term by the hidden one inside:
            if (!(mainQueryTerm.attributes[1] instanceof TermTermAttribute))
                return null;
            queryTerm = mainQueryTerm.attributes[1];
            queryTerms = Term.elementsInList(queryTerm.term, "#and");
            if (!(queryTerms[0] instanceof TermTermAttribute))
                return null;
            if (!(queryTerms[1] instanceof TermTermAttribute))
                return null;
            if (queryTerms.length != 2)
                return null;
            var queryVariable = queryTerms[0];
            if (!(queryVariable instanceof TermTermAttribute) ||
                queryVariable.term.functor.name != "#query")
                return null;
            queryVariable = queryVariable.term.attributes[0];
            queryTerm = queryTerms[1];
            if (answerTerm instanceof VariableTermAttribute) {
                if (answerTerm.sort.name == "unknown")
                    return [];
                return null;
            }
            else if (answerTerm instanceof ConstantTermAttribute) {
                // direct answer:
                //				console.log("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: direct answer!");
                //				console.log("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: unify term 1: " + queryVariable);
                //				console.log("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: unify term 2: " + answerTerm);
                var bindings2 = new Bindings();
                if (Term.unifyAttribute(queryVariable, answerTerm, true, bindings2)) {
                    var tmp = queryTerm.applyBindings(bindings2);
                    if (!(tmp instanceof TermTermAttribute))
                        return null;
                    return [tmp.term];
                }
                return null;
            }
            else {
                // indirect answer:
                //				console.log("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: indirect answer!");
                //				console.log("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: unify term 1: " + queryTerm);
                //				console.log("sentenceToMemorizeFromPredicateQuestionWithInformAnswer: unify term 2: " + answerTerm);
                var bindings2 = new Bindings();
                if (Term.unifyAttribute(queryTerm, answerTerm, true, bindings2)) {
                    var tmp = queryTerm.applyBindings(bindings2);
                    if (!(tmp instanceof TermTermAttribute))
                        return null;
                    return [tmp.term];
                }
                return null;
            }
        }
        else {
            return null;
        }
    };
    RuleBasedAI.prototype.sentenceToMemorizeFromQueryQuestion = function (queryPerformative, answerPerformative) {
        var queryVariable = queryPerformative.attributes[1];
        var queryTerm = queryPerformative.attributes[2];
        var answerTerm = answerPerformative.attributes[1];
        if ((answerTerm instanceof TermTermAttribute) &&
            answerTerm.term.functor.name == "proper-noun") {
            answerTerm = answerTerm.term.attributes[0];
        }
        if (answerTerm instanceof VariableTermAttribute) {
            if (answerTerm.sort.name == "unknown")
                return [];
            return null;
        }
        else if (answerTerm instanceof ConstantTermAttribute) {
            // direct answer:
            //			console.log("sentenceToMemorizeFromQueryQuestion: direct answer!");
            //			console.log("sentenceToMemorizeFromQueryQuestion: unify term 1: " + queryVariable);
            //			console.log("sentenceToMemorizeFromQueryQuestion: unify term 2: " + answerTerm);
            var bindings2 = new Bindings();
            if (Term.unifyAttribute(queryVariable, answerTerm, true, bindings2)) {
                var tmp = queryTerm.applyBindings(bindings2);
                if (!(tmp instanceof TermTermAttribute))
                    return null;
                return [tmp.term];
            }
            return null;
        }
        else {
            // indirect answer:
            //			console.log("sentenceToMemorizeFromQueryQuestion: indirect answer!");
            //			console.log("sentenceToMemorizeFromQueryQuestion: unify term 1: " + queryTerm);
            //			console.log("sentenceToMemorizeFromQueryQuestion: unify term 2: " + answerTerm);
            var bindings2 = new Bindings();
            if (Term.unifyAttribute(queryTerm, answerTerm, true, bindings2)) {
                var tmp = queryTerm.applyBindings(bindings2);
                if (!(tmp instanceof TermTermAttribute))
                    return null;
                return [tmp.term];
            }
            return null;
        }
    };
    RuleBasedAI.prototype.talkingToUs = function (context, speaker, performative) {
        // the "targetList" is a structure of the form #and(T1, #and(t2, ... #and(Tn-1,Tn)...) if there is more than one target
        var targetList = null;
        var targetIDList = [];
        if (performative != null) {
            targetList = performative.attributes[0];
            while (targetList instanceof TermTermAttribute) {
                if (targetList.term.functor.name == "#and" &&
                    targetList.term.attributes[0] instanceof ConstantTermAttribute) {
                    targetIDList.push(targetList.term.attributes[0].value);
                    targetList = targetList.term.attributes[1];
                }
                else {
                    break;
                }
            }
            if (targetList instanceof ConstantTermAttribute)
                targetIDList.push(targetList.value);
            for (var _i = 0, targetIDList_1 = targetIDList; _i < targetIDList_1.length; _i++) {
                var targetID = targetIDList_1[_i];
                if (targetID == this.selfID) {
                    context.lastPerformativeInvolvingThisCharacterWasToUs = true;
                    return true;
                }
                else {
                    // talking to someone else, so we are now not talking to that someone else:
                    var context2 = this.contextForSpeakerWithoutCreatingANewOne(targetID);
                    if (context2 != null) {
                        context2.lastPerformativeInvolvingThisCharacterWasToUs = false;
                        context2.inConversation = false;
                    }
                }
            }
            if (targetIDList.length > 0) {
                // not talking to us!
                context.lastPerformativeInvolvingThisCharacterWasToUs = false;
                context.inConversation = false;
                for (var _a = 0, targetIDList_2 = targetIDList; _a < targetIDList_2.length; _a++) {
                    var targetID = targetIDList_2[_a];
                    var context2 = this.contextForSpeakerWithoutCreatingANewOne(targetID);
                    if (context2 != null) {
                        context2.inConversation = false;
                        context2.lastPerformativeInvolvingThisCharacterWasToUs = false;
                    }
                }
                return false;
            }
        }
        if (context.performatives.length > 0 &&
            (this.timeStamp - context.performatives[0].timeStamp) >= CONVERSATION_TIMEOUT) {
            console.log("Conversation has timed out, time sinde last performative: " + (this.timeStamp - context.performatives[0].timeStamp));
            return false;
        }
        if (context.lastPerformativeInvolvingThisCharacterWasToUs)
            return true;
        if (context.inConversation)
            return true;
        return false;
    };
    RuleBasedAI.prototype.inferenceUpdate = function () {
        //	    DEBUG_resolution = true;
        // select which inference process to continue in this cycle:
        // pick the inference that generates the maximum anxiety:
        var max_anxiety_inference = null;
        // for(let i:number = 0;i<this.inferenceProcesses.length;i++) {
        // 	// increment anxiety of inferences:
        // 	this.inferenceProcesses[i].anxiety += this.inferenceProcesses[i].priority;
        // 	if (max_anxiety_inference == null ||
        // 		this.inferenceProcesses[i].anxiety > max_anxiety_inference.anxiety) {
        // 		max_anxiety_inference = this.inferenceProcesses[i];
        // 	}
        // }
        if (this.currentInferenceProcess == null && this.queuedInferenceProcesses.length > 0) {
            this.currentInferenceProcess = this.queuedInferenceProcesses[0];
            this.currentInferenceProcess.init(this, this.o);
            this.queuedInferenceProcesses.splice(0, 1);
        }
        max_anxiety_inference = this.currentInferenceProcess;
        if (max_anxiety_inference != null) {
            var idx = max_anxiety_inference.completedInferences.length;
            if (idx >= max_anxiety_inference.inferences.length) {
                // inference is over!
                // this.inferenceProcesses.splice(this.inferenceProcesses.indexOf(max_anxiety_inference),1);
                this.currentInferenceProcess = null;
                if (max_anxiety_inference.effect != null) {
                    max_anxiety_inference.effect.execute(max_anxiety_inference, this);
                }
                // after we have answered everything the player wanted, check to see if we had any questions in the stack:
                if (this.currentInferenceProcess == null && this.queuedInferenceProcesses.length == 0) {
                    for (var _i = 0, _a = this.contexts; _i < _a.length; _i++) {
                        var context = _a[_i];
                        if (context.expectingAnswerToQuestionTimeStamp_stack.length > 0) {
                            var idx_1 = context.expectingAnswerToQuestionTimeStamp_stack.length - 1;
                            if (this.timeStamp - context.expectingAnswerToQuestionTimeStamp_stack[idx_1] > this.questionPatienceTimmer) {
                                // We have waited for an answer too long, ask the question again:
                                if (this.canHear(context.speaker))
                                    this.reaskTheLastQuestion(context);
                            }
                        }
                    }
                }
            }
            else {
                if (max_anxiety_inference.findAllAnswers) {
                    if (max_anxiety_inference.inferences[idx].stepAccumulatingResults(true)) {
                        max_anxiety_inference.completedInferences.push(max_anxiety_inference.inferences[idx]);
                    }
                }
                else {
                    if (max_anxiety_inference.inferences[idx].step()) {
                        max_anxiety_inference.completedInferences.push(max_anxiety_inference.inferences[idx]);
                    }
                }
            }
        }
    };
    RuleBasedAI.prototype.contextForSpeaker = function (speaker) {
        if (this.selfID == speaker)
            console.error("trying to get a context to talk to self!!");
        for (var _i = 0, _a = this.contexts; _i < _a.length; _i++) {
            var c = _a[_i];
            if (c.speaker == speaker)
                return c;
        }
        var context = new NLContext(speaker, this, MENTION_MEMORY_SIZE);
        this.contexts.push(context);
        return context;
    };
    RuleBasedAI.prototype.contextForSpeakerWithoutCreatingANewOne = function (speaker) {
        for (var _i = 0, _a = this.contexts; _i < _a.length; _i++) {
            var c = _a[_i];
            if (c.speaker == speaker)
                return c;
        }
        return null;
    };
    RuleBasedAI.prototype.considerGoals = function () {
        // if we are talking to someone, ignore goals:
        for (var _i = 0, _a = this.contexts; _i < _a.length; _i++) {
            var context = _a[_i];
            if (context.inConversation)
                return false;
        }
        return true;
    };
    RuleBasedAI.prototype.goalsUpdate = function (time) {
        var newCurrentGoal = this.currentGoal;
        for (var _i = 0, _a = this.goals; _i < _a.length; _i++) {
            var goal = _a[_i];
            if (goal.nextTimeToTrigger <= time) {
                if (newCurrentGoal == null || goal.priority > newCurrentGoal.priority) {
                    newCurrentGoal = goal;
                }
            }
        }
        if (newCurrentGoal != this.currentGoal) {
            this.currentGoal = newCurrentGoal;
            this.currentGoal.restart(this);
        }
        if (this.currentGoal != null) {
            if (this.currentGoal.execute(this)) {
                // goal finished!
                if (this.currentGoal.periodicity == -1) {
                    if (this.goals.indexOf(this.currentGoal) != -1) {
                        this.goals.splice(this.goals.indexOf(this.currentGoal), 1);
                    }
                }
                else {
                    this.currentGoal.nextTimeToTrigger += this.currentGoal.periodicity;
                }
                this.currentGoal = null;
                this.addLongTermTerm(Term.fromString("verb.do('" + this.selfID + "'[#id], 'nothing'[nothing])", this.o), PERCEPTION_PROVENANCE);
            }
        }
    };
    RuleBasedAI.prototype.addCurrentActionLongTermTerm = function (intention) {
        // if we are executing some goal, then we don't want to overwrite what we are currently doing:
        if (this.currentGoal == null) {
            this.addLongTermTerm(new Term(this.o.getSort("verb.do"), [new ConstantTermAttribute(this.selfID, this.cache_sort_id),
                new TermTermAttribute(intention)]), PERCEPTION_PROVENANCE);
        }
    };
    RuleBasedAI.prototype.executeIntentions = function () {
        if (this.intentions.length == 0 &&
            this.currentInferenceProcess == null && this.queuedInferenceProcesses.length == 0 &&
            this.queuedIntentions.length > 0) {
            var ir = this.queuedIntentions[0];
            this.queuedIntentions.splice(0, 1);
            this.planForAction(ir);
        }
        var toDelete = [];
        for (var _i = 0, _a = this.intentions; _i < _a.length; _i++) {
            var intention = _a[_i];
            var ret = this.executeIntention(intention);
            if (ret == null) {
                // this means that although we can execute the intetion, it cannot be executed right now, so, we need to wait:
                continue;
            }
            if (ret) {
                if (this.debugActionLog != null) {
                    this.debugActionLog.push(intention);
                }
            }
            else {
                console.error("Unsuported intention (" + this.selfID + "): " + intention.action);
                intention.succeeded = false;
            }
            // if (intention.succeeded == null) {
            // 	throw new Error("action handler for " + intention.action + "  did not set succeeded!");
            // }
            if (!intention.succeeded) {
                this.removeQueuedPerformativesDependingOnIntentionSuccess(intention);
            }
            toDelete.push(intention);
        }
        for (var _b = 0, toDelete_1 = toDelete; _b < toDelete_1.length; _b++) {
            var t = toDelete_1[_b];
            this.intentions.splice(this.intentions.indexOf(t), 1);
        }
    };
    RuleBasedAI.prototype.executeIntention = function (ir) {
        var intention = ir.action;
        for (var _i = 0, _a = this.intentionHandlers; _i < _a.length; _i++) {
            var ih = _a[_i];
            if (ih.canHandle(intention, this)) {
                return ih.execute(ir, this);
            }
        }
        return false;
    };
    RuleBasedAI.prototype.queueIntention = function (intention, requester, reqperformative) {
        var ir = new IntentionRecord(intention, requester, reqperformative, null, this.timeStamp);
        this.queuedIntentions.push(ir);
        return ir;
    };
    RuleBasedAI.prototype.queueIntentionRecord = function (ir) {
        if (this.queuedIntentions.indexOf(ir) == -1) {
            this.queuedIntentions.push(ir);
        }
    };
    RuleBasedAI.prototype.removeQueuedPerformativesDependingOnIntentionSuccess = function (ir) {
        // remove all other pending queued performatives from the same utterance:
        var toDelete = [];
        for (var _i = 0, _a = this.queuedParsedPerformatives; _i < _a.length; _i++) {
            var tmp = _a[_i];
            if (ir.requestingPerformative != null &&
                ir.requestingPerformative.parse == tmp[3]) {
                toDelete.push(tmp);
            }
        }
        for (var _b = 0, toDelete_2 = toDelete; _b < toDelete_2.length; _b++) {
            var tmp = toDelete_2[_b];
            this.queuedParsedPerformatives.splice(this.queuedParsedPerformatives.indexOf(tmp));
        }
    };
    // Some AIs extending from this class, might implement a planner, which will be called by redefining this function
    // In this default one, we just queue the action for execution.
    RuleBasedAI.prototype.planForAction = function (ir) {
        this.intentions.push(ir);
    };
    RuleBasedAI.prototype.canSee = function (characterID) {
        return true;
    };
    RuleBasedAI.prototype.canHear = function (characterID) {
        return true;
    };
    RuleBasedAI.prototype.conversationUpdate = function () {
        for (var _i = 0, _a = this.contexts; _i < _a.length; _i++) {
            var context = _a[_i];
            if (context.expectingAnswerToQuestionTimeStamp_stack.length > 0) {
                //				console.log("context.expectingAnswerToQuestion_stack.length: " + context.expectingAnswerToQuestion_stack.length + 
                //						    "\ncontext.expectingAnswerToQuestionTimeStamp_stack: " + context.expectingAnswerToQuestionTimeStamp_stack);
                var idx = context.expectingAnswerToQuestionTimeStamp_stack.length - 1;
                if (this.timeStamp - context.expectingAnswerToQuestionTimeStamp_stack[idx] > this.questionPatienceTimmer) {
                    // We have waited for an answer too long, ask the question again:
                    if (this.canHear(context.speaker))
                        this.reaskTheLastQuestion(context);
                }
            }
        }
    };
    RuleBasedAI.prototype.reaskTheLastQuestion = function (context) {
        var idx = context.expectingAnswerToQuestionTimeStamp_stack.length - 1;
        var performative = context.expectingAnswerToQuestion_stack[idx];
        //		console.log("context.expectingAnswerToQuestionTimeStamp_stack (before): " + context.expectingAnswerToQuestionTimeStamp_stack);
        context.popLastQuestion();
        //		console.log("context.expectingAnswerToQuestionTimeStamp_stack (after): " + context.expectingAnswerToQuestionTimeStamp_stack);
        // re-add the intention:
        if (!context.inConversation) {
            // we are not having a conversation at this point, so, we need to restart it:
            var term = Term.fromString("action.talk('" + this.selfID + "'[#id], perf.callattention('" + context.speaker + "'[#id]))", this.o);
            this.intentions.push(new IntentionRecord(term, null, null, null, this.timeStamp));
        }
        var term2 = new Term(this.o.getSort("action.talk"), [new ConstantTermAttribute(this.selfID, this.o.getSort("#id")),
            new TermTermAttribute(performative.performative)]);
        this.intentions.push(new IntentionRecord(term2, null, null, null, this.timeStamp));
    };
    /*
    - checks if "q" unifies with any term in the short or long term memory, and returns the bindings
    */
    RuleBasedAI.prototype.noInferenceQuery = function (q, o) {
        // short term memory:
        var tmp = this.shortTermMemory.firstMatch(q);
        if (tmp != null)
            return tmp[1];
        // long term memory:
        var s = this.longTermMemory.firstSingleTermMatch(q.functor, q.attributes.length, o);
        while (s != null) {
            var b = new Bindings();
            if (q.unify(s.terms[0], OCCURS_CHECK, b)) {
                return b;
            }
            s = this.longTermMemory.nextSingleTermMatch();
        }
        return null;
    };
    RuleBasedAI.prototype.noInferenceQueryValue = function (q, o, variableName) {
        var b = this.noInferenceQuery(q, o);
        if (b == null)
            return null;
        for (var _i = 0, _a = b.l; _i < _a.length; _i++) {
            var tmp = _a[_i];
            if (tmp[0].name == variableName)
                return tmp[1];
        }
        return null;
    };
    RuleBasedAI.prototype.checkSpatialRelation = function (relation, o1ID, o2ID, referenceObject) {
        return null;
    };
    RuleBasedAI.prototype.processSuperlatives = function (results, superlative) {
        return results;
    };
    RuleBasedAI.prototype.spatialRelations = function (o1ID, o2ID) {
        return null;
    };
    RuleBasedAI.prototype.recalculateCharacterAges = function () {
        for (var _i = 0, _a = this.longTermMemory.plainSentenceList; _i < _a.length; _i++) {
            var se = _a[_i];
            var s = se.sentence;
            if (s.terms.length == 1 && s.sign[0] &&
                s.terms[0].functor.name == "property.born" &&
                s.terms[0].attributes[0] instanceof ConstantTermAttribute) {
                var birthday = se.time;
                var bd_year = getCurrentYear(birthday);
                var bd_month = getCurrentMonth(birthday);
                var bd_day = getCurrentDayOfTheMonth(birthday);
                var current_year = getCurrentYear(this.timeStamp);
                var current_month = getCurrentMonth(this.timeStamp);
                var current_day = getCurrentDayOfTheMonth(this.timeStamp);
                var age_in_years = current_year - bd_year;
                if (current_month < bd_month ||
                    (current_month == bd_month && current_day < bd_day))
                    age_in_years--;
                this.longTermMemory.addStateSentenceIfNew(new Sentence([Term.fromString("property.age(" + s.terms[0].attributes[0] + ",'" + age_in_years + "'[time.year])", this.o)], [true]), se.provenance, 1, se.time);
            }
        }
    };
    RuleBasedAI.prototype.mostSpecificMatchesFromShortOrLongTermMemoryThatCanBeRendered = function (query) {
        var mostSpecificTypes = [];
        for (var _i = 0, _a = this.shortTermMemory.allMatches(query); _i < _a.length; _i++) {
            var match_bindings = _a[_i];
            var t = match_bindings[0];
            // if we don't know how to render this, then ignore:
            var msType = this.mostSpecificTypeThatCanBeRendered(t.functor);
            if (msType == null)
                continue;
            t = t.clone([]);
            t.functor = msType;
            var isMoreSpecific = true;
            var toDelete = [];
            for (var _b = 0, mostSpecificTypes_1 = mostSpecificTypes; _b < mostSpecificTypes_1.length; _b++) {
                var previous = mostSpecificTypes_1[_b];
                if (t.functor.subsumes(previous.functor)) {
                    isMoreSpecific = false;
                }
                else if (previous.functor.subsumes(t.functor)) {
                    toDelete.push(previous);
                }
            }
            for (var _c = 0, toDelete_3 = toDelete; _c < toDelete_3.length; _c++) {
                var previous = toDelete_3[_c];
                mostSpecificTypes.splice(mostSpecificTypes.indexOf(previous), 1);
            }
            if (isMoreSpecific)
                mostSpecificTypes.push(t);
        }
        for (var _d = 0, _e = this.longTermMemory.allSingleTermMatches(query.functor, query.attributes.length, this.o); _d < _e.length; _d++) {
            var match = _e[_d];
            var t = match.terms[0];
            if (query.unify(t, OCCURS_CHECK, new Bindings())) {
                // if we don't know how to render this, then ignore:
                var msType = this.mostSpecificTypeThatCanBeRendered(t.functor);
                if (msType == null)
                    continue;
                t = t.clone([]);
                t.functor = msType;
                var isMoreSpecific = true;
                var toDelete = [];
                for (var _f = 0, mostSpecificTypes_2 = mostSpecificTypes; _f < mostSpecificTypes_2.length; _f++) {
                    var previous = mostSpecificTypes_2[_f];
                    if (t.functor.subsumes(previous.functor)) {
                        isMoreSpecific = false;
                    }
                    else if (previous.functor.subsumes(t.functor)) {
                        toDelete.push(previous);
                    }
                }
                for (var _g = 0, toDelete_4 = toDelete; _g < toDelete_4.length; _g++) {
                    var previous = toDelete_4[_g];
                    mostSpecificTypes.splice(mostSpecificTypes.indexOf(previous), 1);
                }
                if (isMoreSpecific)
                    mostSpecificTypes.push(t);
            }
        }
        return mostSpecificTypes;
    };
    RuleBasedAI.prototype.mostSpecificTypeThatCanBeRendered = function (typeSort) {
        var typeString = this.naturalLanguageParser.posParser.getTypeString(typeSort, 0);
        if (typeString == null) {
            var typeSort_l = typeSort.getAncestors();
            for (var _i = 0, typeSort_l_1 = typeSort_l; _i < typeSort_l_1.length; _i++) {
                var ts = typeSort_l_1[_i];
                typeString = this.naturalLanguageParser.posParser.getTypeString(ts, 0);
                if (typeString != null) {
                    typeSort = ts;
                    break;
                }
            }
        }
        return typeSort;
    };
    RuleBasedAI.prototype.distanceBetweenIds = function (source, target) {
        return null;
    };
    RuleBasedAI.prototype.applyBindingsToSubsequentActionsOrInferences = function (bindings) {
        for (var _i = 0, _a = this.queuedInferenceProcesses; _i < _a.length; _i++) {
            var ir = _a[_i];
            for (var i = 0; i < ir.targets.length; i++) {
                for (var j = 0; j < ir.targets[i].length; j++) {
                    ir.targets[i][j] = ir.targets[i][j].applyBindings(bindings);
                }
            }
            if (ir.effect instanceof ExecuteAction_InferenceEffect) {
                ir.effect.perf = ir.effect.perf.applyBindings(bindings);
            }
        }
        for (var _b = 0, _c = this.queuedIntentions; _b < _c.length; _b++) {
            var ir = _c[_b];
            ir.action = ir.action.applyBindings(bindings);
        }
        for (var _d = 0, _e = this.queuedParsedPerformatives; _d < _e.length; _d++) {
            var tmp = _e[_d];
            tmp[0] = tmp[0].applyBindings(bindings);
        }
    };
    // Returns "true" if the AI is still trying to execute "ir"
    RuleBasedAI.prototype.IRpendingCompletion = function (ir) {
        if (this.intentions.indexOf(ir) != -1)
            return true;
        if (this.queuedIntentions.indexOf(ir) != -1)
            return true;
        return false;
    };
    RuleBasedAI.prototype.restoreFromXML = function (xml) {
        this.timeStamp = Number(xml.getAttribute("timeInSeconds"));
        this.questionPatienceTimmer = Number(xml.getAttribute("questionPatienceTimmer"));
        var stm_xml = getFirstElementChildByTag(xml, "shortTermMemory");
        if (stm_xml != null) {
            this.shortTermMemory = new TermContainer();
            for (var _i = 0, _a = getElementChildrenByTag(stm_xml, "term"); _i < _a.length; _i++) {
                var term_xml = _a[_i];
                var a = Number(term_xml.getAttribute("activation"));
                var p = term_xml.getAttribute("provenance");
                var t = Term.fromString(term_xml.getAttribute("term"), this.o);
                var time = Number(term_xml.getAttribute("time"));
                if (a != null && t != null)
                    this.shortTermMemory.addTerm(t, p, a, time);
            }
            for (var _b = 0, _c = getElementChildrenByTag(stm_xml, "previousTerm"); _b < _c.length; _b++) {
                var term_xml = _c[_b];
                var a = Number(term_xml.getAttribute("activation"));
                var p = term_xml.getAttribute("provenance");
                var t = Term.fromString(term_xml.getAttribute("term"), this.o);
                var time = Number(term_xml.getAttribute("time"));
                if (a != null && t != null)
                    this.shortTermMemory.plainPreviousTermList.push(new TermEntry(t, p, a, time));
            }
        }
        var ltm_xml = getFirstElementChildByTag(xml, "longTermMemory");
        if (ltm_xml != null) {
            //			this.longTermMemory = new SentenceContainer();
            this.loadLongTermRulesFromXML(ltm_xml);
        }
        this.currentEpisodeTerms = [];
        var currentEpisodeTerm_xmls = getElementChildrenByTag(xml, "currentEpisodeTerm");
        for (var _d = 0, currentEpisodeTerm_xmls_1 = currentEpisodeTerm_xmls; _d < currentEpisodeTerm_xmls_1.length; _d++) {
            var currentEpisodeTerm_xml = currentEpisodeTerm_xmls_1[_d];
            this.currentEpisodeTerms.push(currentEpisodeTerm_xml.getAttribute("text"));
        }
        // context:
        var context_xmls = getElementChildrenByTag(xml, "context");
        for (var _e = 0, context_xmls_1 = context_xmls; _e < context_xmls_1.length; _e++) {
            var context_xml = context_xmls_1[_e];
            this.contexts.push(NLContext.fromXML(context_xml, this.o, this, MENTION_MEMORY_SIZE));
        }
        // intentions:
        this.intentions = [];
        for (var _f = 0, _g = getElementChildrenByTag(xml, "IntentionRecord"); _f < _g.length; _f++) {
            var intention_xml = _g[_f];
            var intention = IntentionRecord.fromXML(intention_xml, this, this.o);
            this.intentions.push(intention);
        }
        this.queuedIntentions = [];
        var queuedIntentions_xml = getFirstElementChildByTag(xml, "queuedIntentions");
        if (queuedIntentions_xml != null) {
            for (var _h = 0, _j = getElementChildrenByTag(queuedIntentions_xml, "IntentionRecord"); _h < _j.length; _h++) {
                var intention_xml = _j[_h];
                var intention = IntentionRecord.fromXML(intention_xml, this, this.o);
                this.queueIntentionRecord(intention);
            }
        }
        this.intentionsCausedByRequest = [];
        var intentionsCausedByRequest_xml = getFirstElementChildByTag(xml, "intentionsCausedByRequest");
        if (intentionsCausedByRequest_xml != null) {
            for (var _k = 0, _l = getElementChildrenByTag(intentionsCausedByRequest_xml, "IntentionRecord"); _k < _l.length; _k++) {
                var intention_xml = _l[_k];
                var intention = IntentionRecord.fromXML(intention_xml, this, this.o);
                this.intentionsCausedByRequest.push(intention);
            }
        }
        // inference:
        var inference_xml = getFirstElementChildByTag(xml, "inference");
        if (inference_xml != null) {
            this.currentInferenceProcess = null;
            this.queuedInferenceProcesses = [];
            for (var _m = 0, _o = getElementChildrenByTag(inference_xml, "InferenceRecord"); _m < _o.length; _m++) {
                var ir_xml = _o[_m];
                var ir = InferenceRecord.fromXML(ir_xml, this.o, this);
                if (ir != null)
                    this.queuedInferenceProcesses.push(ir);
            }
        }
        // goals:
        this.goals = [];
        for (var _p = 0, _q = getElementChildrenByTag(xml, "AIGoal"); _p < _q.length; _p++) {
            var goal_xml = _q[_p];
            var goal = AIGoal.fromXML(goal_xml, this.o, this);
            this.goals.push(goal);
            if (goal.remainingActions != null)
                this.currentGoal = goal;
        }
    };
    RuleBasedAI.prototype.saveToXML = function () {
        var str = "<RuleBasedAI timeInSeconds=\"" + this.timeStamp + "\" " +
            "questionPatienceTimmer=\"" + this.questionPatienceTimmer + "\">\n";
        str += "<shortTermMemory>\n";
        for (var _i = 0, _a = this.shortTermMemory.plainTermList; _i < _a.length; _i++) {
            var te = _a[_i];
            str += "<term activation=\"" + te.activation + "\" " +
                "provenance=\"" + te.provenance + "\" " +
                "term=\"" + te.term.toStringXML() + "\" " +
                "time=\"" + te.time + "\"/>\n";
        }
        for (var _b = 0, _c = this.shortTermMemory.plainPreviousTermList; _b < _c.length; _b++) {
            var te = _c[_b];
            str += "<previousTerm activation=\"" + te.activation + "\" " +
                "provenance=\"" + te.provenance + "\" " +
                "term=\"" + te.term.toStringXML() + "\" " +
                "time=\"" + te.time + "\"/>\n";
        }
        str += "</shortTermMemory>\n";
        str += "<longTermMemory>\n";
        for (var _d = 0, _e = this.longTermMemory.previousSentencesWithNoCurrentSentence; _d < _e.length; _d++) {
            var se = _e[_d];
            if (se.provenance != BACKGROUND_PROVENANCE &&
                se.provenance != ONTOLOGY_PROVENANCE &&
                se.provenance != LOCATIONS_PROVENANCE) {
                str += "<previousSentence activation=\"" + se.activation + "\" " +
                    "provenance=\"" + se.provenance + "\" " +
                    "sentence=\"" + se.sentence.toStringXML() + "\" " +
                    "time=\"" + se.time + "\" " +
                    "timeEnd=\"" + se.timeEnd + "\"/>\n";
            }
        }
        for (var _f = 0, _g = this.longTermMemory.plainSentenceList; _f < _g.length; _f++) {
            var se = _g[_f];
            if (se.provenance != BACKGROUND_PROVENANCE &&
                se.provenance != ONTOLOGY_PROVENANCE &&
                se.provenance != LOCATIONS_PROVENANCE) {
                str += this.saveSentenceEntryToXML(se, false);
            }
        }
        str += "</longTermMemory>\n";
        for (var _h = 0, _j = this.currentEpisodeTerms; _h < _j.length; _h++) {
            var et = _j[_h];
            str += "<currentEpisodeTerm str=\"" + et + "\"/>\n";
        }
        for (var _k = 0, _l = this.intentions; _k < _l.length; _k++) {
            var t = _l[_k];
            str += t.saveToXML(this);
        }
        if (this.queuedIntentions.length > 0) {
            str += "<queuedIntentions>\n";
            for (var _m = 0, _o = this.queuedIntentions; _m < _o.length; _m++) {
                var t = _o[_m];
                str += t.saveToXML(this);
            }
            str += "</queuedIntentions>\n";
        }
        if (this.intentionsCausedByRequest.length > 0) {
            str += "<intentionsCausedByRequest>\n";
            for (var _p = 0, _q = this.intentionsCausedByRequest; _p < _q.length; _p++) {
                var t = _q[_p];
                str += t.saveToXML(this);
            }
            str += "</intentionsCausedByRequest>\n";
        }
        str += "<inference>\n";
        if (this.currentInferenceProcess != null) {
            str += this.currentInferenceProcess.saveToXML(this) + "\n";
        }
        for (var _r = 0, _s = this.queuedInferenceProcesses; _r < _s.length; _r++) {
            var ip = _s[_r];
            str += ip.saveToXML(this) + "\n";
        }
        str += "</inference>\n";
        for (var _t = 0, _u = this.contexts; _t < _u.length; _t++) {
            var context = _u[_t];
            str += context.saveToXML() + "\n";
        }
        for (var _v = 0, _w = this.goals; _v < _w.length; _v++) {
            var goal = _w[_v];
            str += goal.saveToXML(this) + "\n";
        }
        str += this.savePropertiesToXML() + "\n";
        str += "</RuleBasedAI>";
        return str;
    };
    RuleBasedAI.prototype.saveSentenceEntryToXML = function (se, previous) {
        var str = "";
        if (se.previousInTime == null) {
            str += "<" + (previous ? "previousSentence" : "sentence") + " activation=\"" + se.activation + "\" " +
                "provenance=\"" + se.provenance + "\" " +
                "sentence=\"" + se.sentence.toStringXML() + "\" " +
                "time=\"" + se.time + "\"/>\n";
        }
        else {
            str += "<" + (previous ? "previousSentence" : "sentence") + " activation=\"" + se.activation + "\" " +
                "provenance=\"" + se.provenance + "\" " +
                "sentence=\"" + se.sentence.toStringXML() + "\" " +
                "time=\"" + se.time + "\">\n";
            str += this.saveSentenceEntryToXML(se.previousInTime, true);
            str += "</" + (previous ? "previousSentence" : "sentence") + ">\n";
        }
        return str;
    };
    // this function is the one that will be extended by the subclasses to add additional info
    RuleBasedAI.prototype.savePropertiesToXML = function () {
        return "";
    };
    RuleBasedAI.translateOntologyToSentences = function (o) {
        var sentences = [];
        // In principle, all of these are needed for having a complete infernece process, but they make things very slow.
        // So, instead, I have a special case where I use functor subsumption instead of equality in case one of the two
        // sentences in resolution just has one term, and only generate a few necessary ones (those for relations):
        /*
        for(let s of o.getAllSorts()) {
            if (s.name[0] == "#" || s.name[0] == "~" || s.name[0] == "=") continue;
            if (s.is_a_string("grammar-concept")) continue;
            if (s.is_a_string("performative")) continue;
            if (!s.is_a_string("relation")) continue;
            for(let parent of s.parents) {
                // This is a hack to filter out rules that I know cause troubles, in reality, I should just use regular FOL inference...
                if (parent.name == "any") continue;	// no need to go all the way there :)
                if (parent.name == "relation") continue;	// no need to go all the way there :)
                if (parent.name == "spatial-relation") continue;	// no need to go all the way there :)
                if (parent.name == "relation-with-value") continue;	// no need to go all the way there :)
                if (parent.name == "space") continue;	// no need to go all the way there :)
                if (parent.name == "time") continue;	// no need to go all the way there :)
                if (parent.name == "distance") continue;	// no need to go all the way there :)
                if (parent.name == "abstract-entity") continue;	// no need to go all the way there :)
                if (parent.name == "#stateSort") continue;	// no need to go all the way there :)
                if (parent.name == "symmetric-relation") continue;	// no need to go all the way there :)
                if (parent.name == "measuring-unit") continue;	// no need to go all the way there :)
                
                if (s.is_a_string("relation")) {
                    let sentence:Sentence = Sentence.fromString("~" + s + "(X, Y);"+parent+"(X, Y)", o);
                    sentences.push(sentence);
                    console.log("    ontology sentence: " + sentence);
                } else {
                    let sentence:Sentence = Sentence.fromString("~" + s + "(X);"+parent+"(X)", o);
                    sentences.push(sentence);
                    console.log("    ontology sentence: " + sentence);
                }
            }
        }
        */
        return sentences;
    };
    return RuleBasedAI;
}());
