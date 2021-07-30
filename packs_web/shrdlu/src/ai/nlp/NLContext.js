var MAX_PERFORMATIVE_MEMORY = 10;
var MAXIMUM_DISTANCE_TO_BE_CONSIDERED_THIS = 128;
var NLDereferenceHint = /** @class */ (function () {
    function NLDereferenceHint(c, r) {
        this.clause = c;
        this.result = r;
    }
    return NLDereferenceHint;
}());
var NLContextEntity = /** @class */ (function () {
    function NLContextEntity(id, time, distance, tl) {
        this.objectID = null;
        this.mentionTime = null;
        this.distanceFromSpeaker = null;
        this.terms = [];
        this.lastUpdateTime = -1;
        this.objectID = id;
        this.mentionTime = time;
        this.distanceFromSpeaker = distance;
        this.terms = tl;
    }
    NLContextEntity.prototype.properNounMatch = function (name) {
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var term = _a[_i];
            if (term.functor.name == "name") {
                if (term.attributes[1] instanceof ConstantTermAttribute &&
                    (term.attributes[1]).value == name) {
                    return true;
                }
            }
        }
        return false;
    };
    NLContextEntity.prototype.sortMatch = function (sort) {
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var term = _a[_i];
            if (term.attributes.length == 1 &&
                term.functor.is_a(sort)) {
                return true;
            }
        }
        return false;
    };
    NLContextEntity.prototype.adjectiveMatch = function (sort, o) {
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var term = _a[_i];
            if (term.attributes.length == 1 &&
                term.functor.is_a(sort)) {
                return true;
            }
            else if (term.attributes.length == 2 &&
                term.functor.is_a(o.getSort("property-with-value")) &&
                (term.attributes[1] instanceof ConstantTermAttribute) &&
                term.attributes[1].sort.is_a(sort)) {
                return true;
            }
        }
        return false;
    };
    NLContextEntity.prototype.relationMatch = function (relation, o, pos) {
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var term = _a[_i];
            if (relation.subsumesNoBindings(term) == 1) {
                return true;
            }
        }
        if (pos != null &&
            pos.reverseRelations[relation.functor.name] != null) {
            var inverseRelation = o.getSort(pos.reverseRelations[relation.functor.name]);
            var relation_reverse = new Term(inverseRelation, [relation.attributes[1], relation.attributes[0]]);
            for (var _b = 0, _c = this.terms; _b < _c.length; _b++) {
                var term = _c[_b];
                if (relation_reverse.subsumesNoBindings(term) == 1) {
                    return true;
                }
            }
        }
        return false;
    };
    NLContextEntity.prototype.addTerm = function (t) {
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var t2 = _a[_i];
            if (t.equalsNoBindings(t2) == 1)
                return;
        }
        this.terms.push(t);
    };
    NLContextEntity.prototype.addStateTerm = function (t) {
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var t2 = _a[_i];
            if (TermContainer.termReplacesPreviousStateTerm(t, t2)) {
                if (t.equalsNoBindings(t2) == 1)
                    return;
                // replace!
                this.terms.splice(this.terms.indexOf(t2), 1);
                this.terms.push(t);
                return;
            }
        }
        this.terms.push(t);
    };
    NLContextEntity.fromXML = function (xml, o) {
        var id = new ConstantTermAttribute(xml.getAttribute("id"), o.getSort(xml.getAttribute("sort")));
        var time = 0;
        var distance = null;
        var tl = [];
        if (xml.getAttribute("mentionTime") != null)
            time = Number(xml.getAttribute("mentionTime"));
        if (xml.getAttribute("distanceFromSpeaker") != null)
            distance = Number(xml.getAttribute("distanceFromSpeaker"));
        for (var _i = 0, _a = getElementChildrenByTag(xml, "term"); _i < _a.length; _i++) {
            var t_xml = _a[_i];
            var t = Term.fromString(t_xml.getAttribute("term"), o);
            if (t != null)
                tl.push(t);
        }
        return new NLContextEntity(id, time, distance, tl);
    };
    NLContextEntity.prototype.outerHTML = function () { return this.saveToXML(); };
    NLContextEntity.prototype.saveToXML = function () {
        var str = "<NLContextEntity id=\"" + this.objectID.value + "\" sort=\"" + this.objectID.sort.name + "\"";
        if (this.mentionTime != null)
            str += " mentionTime=\"" + this.mentionTime + "\"";
        if (this.distanceFromSpeaker != null)
            str += " distanceFromSpeaker=\"" + this.distanceFromSpeaker + "\"";
        str += ">\n";
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var t = _a[_i];
            str += "<term term=\"" + t.toStringXML() + "\"/>\n";
        }
        str += "</NLContextEntity>";
        return str;
    };
    NLContextEntity.prototype.toString = function () {
        return "[NLCE: " + this.objectID + "," + this.mentionTime + "," + this.distanceFromSpeaker + "]";
    };
    return NLContextEntity;
}());
var NLContextPerformative = /** @class */ (function () {
    function NLContextPerformative(t, speaker, p, parse, derefErrors, c, context, timeStamp) {
        this.text = null;
        this.speaker = null;
        this.performative = null;
        this.parse = null;
        this.derefErrors = null; // for those performatives that cannot be parsed properly
        this.cause = null; // the cause of having said this performative
        this.context = null;
        this.timeStamp = 0; // cycle when it was recorded
        this.IDs = null; // the IDs of the objects mentioned in this performative
        this.text = t;
        this.speaker = speaker;
        this.performative = p;
        this.parse = parse;
        this.derefErrors = derefErrors;
        this.cause = c;
        this.context = context;
        this.timeStamp = timeStamp;
        this.IDs = null;
    }
    // find all the entities mentioned in the clause:
    NLContextPerformative.prototype.IDsInPerformative = function (o) {
        if (this.IDs != null)
            return this.IDs;
        this.IDs = [];
        var perf = this.performative;
        if (perf == null)
            return this.IDs;
        for (var i = 0; i < perf.attributes.length; i++) {
            if (perf.attributes[i] instanceof ConstantTermAttribute) {
                this.IDs.push((perf.attributes[i]));
            }
            else if (perf.attributes[i] instanceof TermTermAttribute) {
                NLContext.searchForIDsInClause(perf.attributes[i].term, this.IDs, o);
            }
        }
        return this.IDs;
    };
    NLContextPerformative.prototype.addMentionToPerformative = function (id, o) {
        if (id == null)
            return;
        this.IDsInPerformative(o); // make sure we have calculated the IDs in the performative
        for (var _i = 0, _a = this.IDs; _i < _a.length; _i++) {
            var id2 = _a[_i];
            if ((id2 instanceof ConstantTermAttribute) &&
                id2.value == id) {
                // already mentioned:
                return;
            }
        }
        var newID = new ConstantTermAttribute(id, o.getSort("#id"));
        this.IDs.push(newID);
        var ce = this.context.newContextEntity(newID, this.timeStamp, null, o, false);
        if (ce != null) {
            var idx = this.context.mentions.indexOf(ce);
            if (idx != -1)
                this.context.mentions.splice(idx, 1);
            this.context.mentions.unshift(ce);
        }
        else {
            console.error("addMentionToPerformative: could not create NLContextEntity!");
        }
    };
    NLContextPerformative.fromXML = function (xml, context, o) {
        var cause = null;
        var p_xml = getFirstElementChildByTag(xml, "cause");
        if (p_xml != null) {
            cause = CauseRecord.fromXML(p_xml, o);
        }
        var performative = null;
        if (xml.getAttribute("performative") != null)
            performative = Term.fromString(xml.getAttribute("performative"), o);
        return new NLContextPerformative(xml.getAttribute("text"), xml.getAttribute("speaker"), performative, null, // TODO: save/load NLParseRecord
        null, // TODO: save/load NLDerefErrorRecord[]
        cause, context, Number(xml.getAttribute("timeStamp")));
    };
    NLContextPerformative.prototype.outerHTML = function () { return this.saveToXML(); };
    NLContextPerformative.prototype.saveToXML = function () {
        if (this.cause == null) {
            var tmp = "<NLContextPerformative text=\"" + this.text + "\" " +
                "speaker=\"" + this.speaker + "\" " +
                (this.performative != null ? "performative=\"" + this.performative + "\" " : "") +
                "timeStamp=\"" + this.timeStamp + "\"/>";
            return tmp;
        }
        else {
            var tmp = "<NLContextPerformative text=\"" + this.text + "\" " +
                "speaker=\"" + this.speaker + "\" " +
                (this.performative != null ? "performative=\"" + this.performative + "\" " : "") +
                "timeStamp=\"" + this.timeStamp + "\">";
            tmp += this.cause.saveToXML();
            tmp += "</NLContextPerformative>";
            return tmp;
        }
    };
    return NLContextPerformative;
}());
var NLContext = /** @class */ (function () {
    function NLContext(speaker, ai, mentionMemorySize) {
        this.speaker = null;
        this.ai = null;
        this.shortTermMemory = [];
        this.longTermMemory = {}; // a cache for not having to create entities constantly during parsing...
        this.mentions = [];
        this.performatives = [];
        this.cache_sort_space_at = null;
        this.cache_sort_contains = null;
        // conversation state:
        this.inConversation = false;
        this.lastPerformativeInvolvingThisCharacterWasToUs = false;
        this.expectingYes = false;
        this.expectingThankYou = false;
        this.expectingYouAreWelcome = false;
        this.expectingGreet = false;
        this.expectingFarewell = false;
        this.expectingNicetomeetyoutoo = false;
        this.expectingAnswerToQuestion_stack = [];
        this.expectingAnswerToQuestionTimeStamp_stack = [];
        this.expectingConfirmationToRequest_stack = [];
        this.expectingConfirmationToRequestTimeStamp_stack = [];
        this.lastEnumeratedQuestion_answered = null;
        this.lastEnumeratedQuestion_answers = null;
        this.lastEnumeratedQuestion_next_answer_index = 0;
        // the "mentions" list can be at most this size: 
        this.mentionMemorySize = 10;
        // when hypothetical entities are mentioned in conversation, new IDs are given to them, this variable
        // determines which is the next ID to give them:
        this.nextHypotheticalID = 0;
        this.lastDerefErrorType = 0;
        this.lastTimeUpdated = -1;
        // Entity rephrasing: If the user's sentence could not be parsed due to a dereference error, and the user issued a clarification
        // such as "I meant the one to your right", this list will contain the clarification, so that when it comes time to parse the
        // previous sentence again, this can be used:
        this.dereference_hints = [];
        this.speaker = speaker;
        this.ai = ai;
        this.mentionMemorySize = mentionMemorySize;
        this.cache_sort_space_at = this.ai.o.getSort("space.at");
        this.cache_sort_contains = this.ai.o.getSort("verb.contains");
    }
    NLContext.prototype.reset = function () {
        this.endConversation();
        this.shortTermMemory = [];
        this.longTermMemory = {};
        this.mentions = [];
        this.performatives = [];
        this.lastDerefErrorType = 0;
        this.lastTimeUpdated = -1;
    };
    NLContext.prototype.endConversation = function () {
        this.inConversation = false;
        this.lastPerformativeInvolvingThisCharacterWasToUs = false;
        this.expectingYes = false;
        this.expectingThankYou = false;
        this.expectingYouAreWelcome = false;
        this.expectingGreet = false;
        this.expectingFarewell = false;
        this.expectingNicetomeetyoutoo = false;
        this.expectingAnswerToQuestion_stack = [];
        this.expectingAnswerToQuestionTimeStamp_stack = [];
        this.expectingConfirmationToRequest_stack = [];
        this.expectingConfirmationToRequestTimeStamp_stack = [];
        this.lastEnumeratedQuestion_answered = null;
        this.lastEnumeratedQuestion_answers = null;
        this.lastEnumeratedQuestion_next_answer_index = 0;
    };
    NLContext.prototype.newContextEntity = function (idAtt, time, distance, o, isFromLongTermMemory) {
        //console.log("newContextEntity: " + idAtt);
        var ID = idAtt.value;
        var e = this.findByID(ID);
        var itsAnExistingOne = false;
        if (e != null) {
            if (time != null) {
                if (e.mentionTime == null || e.mentionTime < time)
                    e.mentionTime = time;
            }
            if (distance != null) {
                e.distanceFromSpeaker = distance;
            }
            // return e;
            itsAnExistingOne = true;
        }
        else {
            if (isFromLongTermMemory &&
                ID in this.longTermMemory) {
                e = this.longTermMemory[ID];
            }
            //		console.log("newContextEntity: creating " + ID + " from scratch...");
            if (e == null) {
                e = new NLContextEntity(idAtt, time, distance, []);
                if (isFromLongTermMemory)
                    this.longTermMemory[ID] = e;
            }
        }
        if (this.ai.timeStamp <= e.lastUpdateTime) {
            // no need to reupdate it...
            return e;
        }
        // console.log("newContextEntity: "+this.ai.selfID+" updating " + ID + "(" + e.lastUpdateTime + " -> " + this.ai.timeStamp + ")");
        e.terms = []; // entities need to be updated every time, otherwise, info is outdated!
        e.lastUpdateTime = this.ai.timeStamp;
        // find everything we can about it:
        var typeSorts = [];
        var typeSortsWithArity = [];
        var pSort = o.getSort("property");
        var pwvSort = o.getSort("property-with-value");
        var rSort = o.getSort("relation");
        for (var _i = 0, _a = POSParser.sortsToConsiderForTypes; _i < _a.length; _i++) {
            var s = _a[_i];
            typeSorts.push(o.getSort(s));
            typeSortsWithArity.push([o.getSort(s), 1]);
        }
        typeSortsWithArity.push([pSort, 1]);
        typeSortsWithArity.push([pwvSort, 2]);
        typeSortsWithArity.push([rSort, 2]);
        /*
        for(let t of this.ai.perceptionBuffer) {
            if (t.functor.is_a(oSort) ||
                t.functor.is_a(pSort)) {
                if (t.attributes[0] instanceof ConstantTermAttribute &&
                    (<ConstantTermAttribute>t.attributes[0]).value == ID) {
                    e.terms.push(t);
                }
            }
        }
        */
        for (var _b = 0, _c = this.ai.shortTermMemory.plainTermList; _b < _c.length; _b++) {
            var te = _c[_b];
            if (POSParser.sortIsConsideredForTypes(te.term.functor, o) ||
                te.term.functor.is_a(pSort) ||
                te.term.functor.is_a(pwvSort) ||
                te.term.functor.is_a(rSort)) {
                for (var _d = 0, _e = te.term.attributes; _d < _e.length; _d++) {
                    var att = _e[_d];
                    if (att instanceof ConstantTermAttribute &&
                        att.value == ID) {
                        e.addTerm(te.term);
                    }
                }
            }
        }
        for (var _f = 0, typeSortsWithArity_1 = typeSortsWithArity; _f < typeSortsWithArity_1.length; _f++) {
            var tmp = typeSortsWithArity_1[_f];
            var s_l = this.ai.longTermMemory.allSingleTermMatches((tmp[0]), (tmp[1]), o);
            for (var _g = 0, s_l_1 = s_l; _g < s_l_1.length; _g++) {
                var s = s_l_1[_g];
                for (var _h = 0, _j = s.terms[0].attributes; _h < _j.length; _h++) {
                    var att = _j[_h];
                    if (att instanceof ConstantTermAttribute &&
                        att.value == ID) {
                        e.addTerm(s.terms[0]);
                    }
                }
            }
        }
        if (e.terms.length == 0) {
            if (itsAnExistingOne)
                this.deleteContextEntity(e);
            return null;
        }
        return e;
    };
    NLContext.prototype.deleteContextEntity = function (e) {
        var idx = this.shortTermMemory.indexOf(e);
        if (idx >= 0)
            this.shortTermMemory.splice(idx, 1);
        idx = this.mentions.indexOf(e);
        if (idx >= 0)
            this.mentions.splice(idx, 1);
    };
    NLContext.prototype.newLongTermTerm = function (term) {
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var ce = _a[_i];
            for (var _b = 0, _c = term.attributes; _b < _c.length; _b++) {
                var att = _c[_b];
                if (att instanceof ConstantTermAttribute &&
                    att.value == ce.objectID.value) {
                    if (ce.terms.indexOf(term) == -1)
                        ce.addTerm(term);
                }
            }
        }
        for (var _d = 0, _e = this.mentions; _d < _e.length; _d++) {
            var ce = _e[_d];
            for (var _f = 0, _g = term.attributes; _f < _g.length; _f++) {
                var att = _g[_f];
                if (att instanceof ConstantTermAttribute &&
                    att.value == ce.objectID.value) {
                    if (ce.terms.indexOf(term) == -1)
                        ce.addTerm(term);
                }
            }
        }
    };
    NLContext.prototype.newLongTermStateTerm = function (term) {
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var ce = _a[_i];
            for (var _b = 0, _c = term.attributes; _b < _c.length; _b++) {
                var att = _c[_b];
                if (att instanceof ConstantTermAttribute &&
                    att.value == ce.objectID.value) {
                    if (ce.terms.indexOf(term) == -1)
                        ce.addStateTerm(term);
                }
            }
        }
        for (var _d = 0, _e = this.mentions; _d < _e.length; _d++) {
            var ce = _e[_d];
            for (var _f = 0, _g = term.attributes; _f < _g.length; _f++) {
                var att = _g[_f];
                if (att instanceof ConstantTermAttribute &&
                    att.value == ce.objectID.value) {
                    if (ce.terms.indexOf(term) == -1)
                        ce.addStateTerm(term);
                }
            }
        }
    };
    NLContext.prototype.addMention = function (id, timeStamp, o) {
        if (id == null)
            return;
        var newID = new ConstantTermAttribute(id, o.getSort("#id"));
        var ce = this.newContextEntity(newID, timeStamp, null, o, false);
        if (ce != null) {
            var idx = this.mentions.indexOf(ce);
            if (idx != -1)
                this.mentions.splice(idx, 1);
            this.mentions.unshift(ce);
        }
        else {
            console.error("addMention: could not create NLContextEntity!");
        }
    };
    NLContext.prototype.newPerformative = function (speakerID, perfText, perf, parse, derefErrors, cause, o, timeStamp) {
        var newPerformatives = [];
        if (perf != null && perf.functor.name == "#list") {
            var parsePerformatives = Term.elementsInList(perf, "#list");
            for (var _i = 0, parsePerformatives_1 = parsePerformatives; _i < parsePerformatives_1.length; _i++) {
                var parsePerformative = parsePerformatives_1[_i];
                if (parsePerformative instanceof TermTermAttribute) {
                    newPerformatives = newPerformatives.concat(this.newPerformative(speakerID, perfText, parsePerformative.term, parse, derefErrors, cause, o, timeStamp));
                }
            }
            return newPerformatives;
        }
        var cp = new NLContextPerformative(perfText, speakerID, perf, parse, derefErrors, cause, this, timeStamp);
        var IDs = cp.IDsInPerformative(o);
        for (var _a = 0, IDs_1 = IDs; _a < IDs_1.length; _a++) {
            var id = IDs_1[_a];
            var ce = this.newContextEntity(id, timeStamp, null, o, false);
            if (ce != null) {
                var idx = this.mentions.indexOf(ce);
                if (idx != -1)
                    this.mentions.splice(idx, 1);
                this.mentions.unshift(ce);
            }
        }
        // add the clause:
        this.performatives.unshift(cp);
        while (this.performatives.length > MAX_PERFORMATIVE_MEMORY) {
            this.performatives.pop();
        }
        this.sortEntities();
        if (this.mentions.length > this.mentionMemorySize) {
            this.mentions = this.mentions.slice(0, this.mentionMemorySize);
        }
        // update conversation context:
        this.inConversation = true;
        if (perf != null &&
            speakerID == this.ai.selfID) {
            this.expectingThankYou = false;
            this.expectingYouAreWelcome = false;
            if (perf.functor.name == "perf.inform.answer") {
                this.expectingThankYou = true;
            }
            else if (perf.functor.name == "perf.thankyou") {
                this.expectingYouAreWelcome = true;
            }
            this.expectingGreet = false;
            this.expectingFarewell = false;
            this.expectingNicetomeetyoutoo = false;
            if (perf.functor.name == "perf.greet") {
                this.expectingGreet = true;
            }
            else if (perf.functor.name == "perf.farewell") {
                this.expectingFarewell = true;
                this.inConversation = false;
            }
            else if (perf.functor.name == "perf.nicetomeetyou") {
                this.expectingNicetomeetyoutoo = true;
            }
            if (perf.functor.is_a(this.ai.o.getSort("perf.question"))) {
                if (perf.functor.name == "perf.q.how" &&
                    perf.attributes.length >= 2 &&
                    (perf.attributes[1] instanceof TermTermAttribute) &&
                    perf.attributes[1].term.functor.name == "verb.help") {
                    // no need to push anything to the stack here
                }
                else {
                    this.expectingAnswerToQuestion_stack.push(cp);
                    this.expectingAnswerToQuestionTimeStamp_stack.push(timeStamp);
                }
            }
            if (perf.functor.is_a(this.ai.o.getSort("perf.request.action"))) {
                this.expectingConfirmationToRequest_stack.push(cp);
                this.expectingConfirmationToRequestTimeStamp_stack.push(timeStamp);
            }
            else {
                // For now, just clear these stacks if we move on in the conversation, since requests do not necessarily need an answer
                this.expectingConfirmationToRequest_stack = [];
                this.expectingConfirmationToRequestTimeStamp_stack = [];
            }
            this.expectingYes = false;
            if (perf.functor.is_a(this.ai.o.getSort("perf.callattention"))) {
                this.expectingYes = true;
            }
        }
        return [cp];
    };
    NLContext.prototype.popLastQuestion = function () {
        if (this.expectingAnswerToQuestion_stack.length > 0) {
            this.expectingAnswerToQuestion_stack.splice(this.expectingAnswerToQuestion_stack.length - 1, 1);
            this.expectingAnswerToQuestionTimeStamp_stack.splice(this.expectingAnswerToQuestionTimeStamp_stack.length - 1, 1);
        }
    };
    NLContext.prototype.lastPerformativeBy = function (speaker) {
        for (var i = 0; i < this.performatives.length; i++) {
            if (this.performatives[i].speaker == speaker)
                return this.performatives[i];
        }
        return null;
    };
    NLContext.prototype.lastPerformativeByExcept = function (speaker, perf) {
        for (var i = 0; i < this.performatives.length; i++) {
            if (this.performatives[i].speaker == speaker &&
                this.performatives[i].performative != perf)
                return this.performatives[i];
        }
        return null;
    };
    NLContext.searchForIDsInClause = function (clause, IDs, o) {
        for (var i = 0; i < clause.attributes.length; i++) {
            if (clause.attributes[i] instanceof ConstantTermAttribute &&
                clause.attributes[i].sort.name == "#id") {
                IDs.push((clause.attributes[i]));
            }
            else if (clause.attributes[i] instanceof TermTermAttribute) {
                NLContext.searchForIDsInClause(clause.attributes[i].term, IDs, o);
            }
        }
    };
    NLContext.prototype.sortEntities = function () {
        this.shortTermMemory.sort(function (e1, e2) {
            if (e1.distanceFromSpeaker == null &&
                e2.distanceFromSpeaker == null) {
                return 0;
            }
            else if (e1.distanceFromSpeaker == null) {
                return 1;
            }
            else if (e2.distanceFromSpeaker == null) {
                return -1;
            }
            else {
                return e1.distanceFromSpeaker - e2.distanceFromSpeaker;
            }
        });
        this.mentions.sort(function (e1, e2) {
            if (e1.mentionTime == null &&
                e2.mentionTime == null) {
                return 0;
            }
            else if (e1.mentionTime == null) {
                return 1;
            }
            else if (e2.mentionTime == null) {
                return -1;
            }
            else {
                return e2.mentionTime - e1.mentionTime;
            }
        });
    };
    NLContext.prototype.deref = function (clause, listenerVariable, nlpr, o, pos, AI) {
        // check if there are any deref hints:
        for (var _i = 0, _a = this.dereference_hints; _i < _a.length; _i++) {
            var hint = _a[_i];
            if (hint.clause.term.equalsNoBindings(clause) == 1) {
                return [new ConstantTermAttribute(hint.result, o.getSort("#id"))];
            }
        }
        return this.derefInternal(Term.elementsInList(clause, "#and"), listenerVariable, nlpr, o, pos, AI, false);
        ;
    };
    NLContext.prototype.derefInternal = function (clauseElements, listenerVariable, nlpr, o, pos, AI, returnAllCandidatesBeforeDisambiguation) {
        this.lastDerefErrorType = 0;
        var properNounSort = o.getSort("proper-noun");
        var nounSort = o.getSort("noun");
        var pronounSort = o.getSort("pronoun");
        var personalPronounSort = o.getSort("personal-pronoun");
        var reflexivePronounSort = o.getSort("reflexive-pronoun");
        var adjectiveSort = o.getSort("adjective");
        var determinerSort = o.getSort("determiner");
        var possessiveDeterminerSort = o.getSort("possessive-determiner");
        var firstPerson = o.getSort("first-person");
        var secondPerson = o.getSort("second-person");
        var thirdPerson = o.getSort("third-person");
        var relationSort = o.getSort("relation");
        var spatialRelationSort = o.getSort("spatial-relation");
        var properNounTerms = [];
        var nounTerms = [];
        var pronounTerms = [];
        var adjectiveTerms = [];
        var negatedAdjectiveTerms = [];
        var determinerTerms = [];
        var relationTerms = [];
        var otherTerms = [];
        var genitiveTerm = null;
        for (var _i = 0, clauseElements_1 = clauseElements; _i < clauseElements_1.length; _i++) {
            var tmp = clauseElements_1[_i];
            if (tmp instanceof TermTermAttribute) {
                var tmp2 = tmp.term;
                NLParser.resolveCons(tmp2, o);
                if (tmp2.functor.is_a(properNounSort)) {
                    properNounTerms.push(tmp2);
                }
                else if (tmp2.functor.name == "saxon-genitive") {
                    if (genitiveTerm != null) {
                        console.warn("two saxon genitives in a noun phrase, not supported!");
                        return null;
                    }
                    genitiveTerm = tmp2;
                }
                else if (tmp2.functor.is_a(nounSort)) {
                    nounTerms.push(tmp2);
                }
                else if (tmp2.functor.is_a(pronounSort)) {
                    pronounTerms.push(tmp2);
                }
                else if (tmp2.functor.is_a(adjectiveSort)) {
                    adjectiveTerms.push(tmp2);
                }
                else if (tmp2.functor.name == "#not" && tmp2.attributes.length == 1 &&
                    (tmp2.attributes[0] instanceof TermTermAttribute) &&
                    (tmp2.attributes[0].term.functor.is_a(adjectiveSort))) {
                    negatedAdjectiveTerms.push(tmp2.attributes[0].term);
                    // console.log("negated adjectives are not supported in #derefFromContext!");
                    return null;
                }
                else if (tmp2.functor.is_a(determinerSort)) {
                    determinerTerms.push(tmp2);
                }
                else if (tmp2.functor.is_a(relationSort)) {
                    relationTerms.push([tmp2]);
                }
                else {
                    otherTerms.push(tmp2);
                }
            }
            else {
                console.error("context.deref: clause contains an element that is not a term!");
            }
        }
        if (genitiveTerm != null)
            return null;
        var all_determiner = null;
        var other_determiner = null;
        for (var _a = 0, determinerTerms_1 = determinerTerms; _a < determinerTerms_1.length; _a++) {
            var t = determinerTerms_1[_a];
            if (t.functor.name == 'all') {
                if (determinerTerms.length == 1) {
                    // this clause refers to a hypothetical, and not to entities in the context!
                    this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                    return null;
                }
                else {
                    all_determiner = t;
                }
            }
            else if (t.functor.name == 'determiner.other') {
                other_determiner = t;
            }
        }
        // remove "all" and "other"
        if (all_determiner != null)
            determinerTerms.splice(determinerTerms.indexOf(all_determiner), 1);
        if (other_determiner != null)
            determinerTerms.splice(determinerTerms.indexOf(other_determiner), 1);
        // we should really only have one determiner other than "all" or "other":
        if (determinerTerms.length > 1) {
            console.warn("NLContext.derefInternal: too many determiners other than all/other" + determinerTerms);
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        if (properNounTerms.length > 0) {
            var output = [];
            for (var _b = 0, properNounTerms_1 = properNounTerms; _b < properNounTerms_1.length; _b++) {
                var properNounTerm = properNounTerms_1[_b];
                if (properNounTerm.attributes[0] instanceof ConstantTermAttribute) {
                    var name_1 = (properNounTerm.attributes[0]).value;
                    var entities = this.findAllProperNoun(name_1, o);
                    for (var _c = 0, entities_1 = entities; _c < entities_1.length; _c++) {
                        var entity = entities_1[_c];
                        if (output.indexOf(entity.objectID) == -1)
                            output.push(entity.objectID);
                    }
                }
            }
            return output;
        }
        else if (pronounTerms.length > 0) {
            // Special case for single pronouns ("it", "him", "her", etc.) that could deref to
            // an entity in the current NLParseRecord:
            if (pronounTerms.length == 1 &&
                clauseElements.length == 1 &&
                nlpr.derefs.length > 0) {
                var pronounTerm = pronounTerms[0];
                var deref = nlpr.derefs[nlpr.derefs.length - 1];
                var nlprResult = this.matchPronounToNLPRDeref(pronounTerm, deref, o);
                if (nlprResult != null)
                    return nlprResult;
            }
            var output = [];
            for (var _d = 0, pronounTerms_1 = pronounTerms; _d < pronounTerms_1.length; _d++) {
                var pronounTerm = pronounTerms_1[_d];
                if (pronounTerm.functor.is_a(personalPronounSort) ||
                    pronounTerm.functor.is_a(reflexivePronounSort)) {
                    if (pronounTerm.attributes[3].sort.is_a(firstPerson)) {
                        output.push(new ConstantTermAttribute(this.speaker, o.getSort("#id")));
                    }
                    else if (pronounTerm.attributes[3].sort.is_a(secondPerson)) {
                        output.push(listenerVariable);
                    }
                    else if (pronounTerm.attributes[3].sort.is_a(thirdPerson)) {
                        // find the most recent mention that could match with the pronoun
                        var entity = this.pronounMatch(pronounTerm, this.mentions, o);
                        if (entity != null)
                            output.push(entity.objectID);
                    }
                    else {
                        console.log("context.deref: unknown person dereferencing personal pronoun: " + clauseElements);
                    }
                }
                else {
                    if (pronounTerm.attributes[0] instanceof ConstantTermAttribute) {
                        var pronounType = pronounTerm.attributes[0].value;
                        if (pronounType == "pronoun.everybody") {
                            // match with all the known "character" entities:
                            var tmp = this.findEntitiesOfSort(o.getSort("character"), o);
                            var allCharacters = [];
                            for (var _e = 0, tmp_1 = tmp; _e < tmp_1.length; _e++) {
                                var tmpl = tmp_1[_e];
                                for (var _f = 0, tmpl_1 = tmpl; _f < tmpl_1.length; _f++) {
                                    var ce = tmpl_1[_f];
                                    if (allCharacters.indexOf(ce) == -1)
                                        allCharacters.push(ce);
                                }
                            }
                            for (var _g = 0, allCharacters_1 = allCharacters; _g < allCharacters_1.length; _g++) {
                                var ce = allCharacters_1[_g];
                                output.push(ce.objectID);
                            }
                        }
                        else if (pronounType == "pronoun.everybody.else") {
                            // match with all the known "character" entities, except speaker and listener:
                            var tmp = this.findEntitiesOfSort(o.getSort("character"), o);
                            var allCharacters = [];
                            for (var _h = 0, tmp_2 = tmp; _h < tmp_2.length; _h++) {
                                var tmpl = tmp_2[_h];
                                for (var _j = 0, tmpl_2 = tmpl; _j < tmpl_2.length; _j++) {
                                    var ce = tmpl_2[_j];
                                    if (allCharacters.indexOf(ce) == -1)
                                        allCharacters.push(ce);
                                }
                            }
                            for (var _k = 0, allCharacters_2 = allCharacters; _k < allCharacters_2.length; _k++) {
                                var ce = allCharacters_2[_k];
                                if (ce.objectID.value != this.speaker &&
                                    ce.objectID.value != this.ai.selfID)
                                    output.push(ce.objectID);
                            }
                        }
                    }
                    else {
                        console.log("context.deref: unsupported pronoun! " + pronounTerm);
                    }
                }
            }
            if (output.length == 0) {
                this.lastDerefErrorType = DEREF_ERROR_NO_REFERENTS;
                return null;
            }
            return output;
        }
        else if (nounTerms.length == 1) {
            var output = [];
            for (var _l = 0, nounTerms_1 = nounTerms; _l < nounTerms_1.length; _l++) {
                var nounTerm = nounTerms_1[_l];
                if (nounTerm.attributes.length == 2 &&
                    nounTerm.attributes[0] instanceof ConstantTermAttribute &&
                    nounTerm.attributes[0].value == "space.here") {
                    // find the location of the speaker:
                    var hereEntity = this.findLocationOfID(this.speaker);
                    if (hereEntity != null) {
                        return [hereEntity.objectID];
                    }
                }
                else if (nounTerm.attributes.length == 2 &&
                    nounTerm.attributes[0] instanceof ConstantTermAttribute &&
                    nounTerm.attributes[0].value == "space.there") {
                    // Find if there is a location we just talked about (and that is not the place where the speaker is):
                    var hereEntity = this.findLocationOfID(this.speaker);
                    var thereEntity = null;
                    var entities_mpl = this.findEntitiesOfSort(o.getSort("space.location"), o);
                    var candidateThereEntities = this.applySingularTheDeterminer(entities_mpl);
                    if (candidateThereEntities != null && candidateThereEntities.length == 1)
                        thereEntity = candidateThereEntities[0];
                    if (thereEntity != null && thereEntity != hereEntity) {
                        return [thereEntity.objectID];
                    }
                }
                else if (nounTerm.attributes.length == 2 &&
                    nounTerm.attributes[0] instanceof ConstantTermAttribute &&
                    nounTerm.attributes[0].value == "space.outside") {
                    // Find the most specific location that is "outdoers" and the player is in right now
                    var outsideEntity = this.findMostSpecificOutdoorLocationOfID(this.speaker, o);
                    if (outsideEntity != null) {
                        return [outsideEntity.objectID];
                    }
                }
                else if (nounTerm.attributes.length == 2 &&
                    nounTerm.attributes[0] instanceof ConstantTermAttribute) {
                    var name_2 = (nounTerm.attributes[0]).value;
                    var nameSort = o.getSortSilent(name_2);
                    var grammaticalNumberSort = nounTerm.attributes[1].sort;
                    var singular = false;
                    if (grammaticalNumberSort.name == "singular")
                        singular = true;
                    if (nameSort != null) {
                        // See if we have any determiners that will modify the search:
                        // - "the" -> resolve to the latest one mentioned if there is more than one (if it's from shortTermMemory, and there is more than one, then fail)
                        // - "every" -> change from singular to plural
                        // - possessive: -> look for "owned-by" relationship
                        // - "this" -> if it's a mention, consider the closest one, if it's in shortTermMemory, look for the closest one to the character
                        // - "these" -> (ignore)
                        // - "that" -> if it's a mention, consider the second one, if it's in shortTermMemory, consider the further one
                        // - "those" -> (ignore)
                        // - "other" -> removes all the matches from short term memory or from mentions
                        // "a"/"any" -> fail
                        // "some"/"much"/"many" -> ???
                        var the_determiner = false;
                        //						let a_determiner:boolean = false;
                        var this_determiner = false;
                        var that_determiner = false;
                        // console.log("context.derefInternal: nounTerm = " + nounTerm);
                        for (var _m = 0, determinerTerms_2 = determinerTerms; _m < determinerTerms_2.length; _m++) {
                            var determinerTerm = determinerTerms_2[_m];
                            // console.log("determinerTerm: " + determinerTerm);
                            var determinerNumberSort = determinerTerm.attributes[1].sort;
                            if (determinerTerm.functor.name == "the") {
                                //								if (singular) {
                                the_determiner = true;
                                //								} else {
                                // ignore for now ...
                                //								}
                            }
                            else if (determinerTerm.functor.name == "every") {
                                singular = false;
                            }
                            else if (determinerTerm.functor.name == "close-demonstrative-determiner") {
                                if (determinerNumberSort.name == "singular")
                                    this_determiner = true;
                                // if it's plural ("these"), ignore
                            }
                            else if (determinerTerm.functor.name == "far-demonstrative-determiner") {
                                if (determinerNumberSort.name == "singular")
                                    that_determiner = true;
                                // if it's plural ("those"), ignore
                            }
                            else if (determinerTerm.functor.is_a(possessiveDeterminerSort)) {
                                if (determinerTerm.functor.name == "determiner.my") {
                                    // find owner:
                                    //									let ownerRelation:Term = Term.fromString("verb.own('"+this.speaker+"'[#id])", o);
                                    //									ownerRelation.addAttribute(determinerTerm.attributes[0]);
                                    //									relationTerms.push(ownerRelation);
                                    var belongsRelation = new Term(o.getSort("verb.own"), [new ConstantTermAttribute(this.speaker, o.getSort("#id")),
                                        determinerTerm.attributes[0]]);
                                    var haveRelation = new Term(o.getSort("verb.have"), [new ConstantTermAttribute(this.speaker, o.getSort("#id")),
                                        determinerTerm.attributes[0]]);
                                    relationTerms.push([belongsRelation, haveRelation]);
                                }
                                else if (determinerTerm.functor.name == "determiner.your") {
                                    // find owner:
                                    //									let ownerRelation:Term = Term.fromString("verb.own('"+this.ai.selfID+"'[#id])", o);
                                    //									ownerRelation.addAttribute(determinerTerm.attributes[0]);
                                    //									relationTerms.push(ownerRelation);
                                    var belongsRelation = new Term(o.getSort("verb.own"), [new ConstantTermAttribute(this.ai.selfID, o.getSort("#id")),
                                        determinerTerm.attributes[0]]);
                                    var haveRelation = new Term(o.getSort("verb.have"), [new ConstantTermAttribute(this.ai.selfID, o.getSort("#id")),
                                        determinerTerm.attributes[0]]);
                                    relationTerms.push([belongsRelation, haveRelation]);
                                }
                                else {
                                    console.log("context.deref: determiner " + determinerTerm + " not yet supported!");
                                }
                            }
                            else if (determinerTerm.functor.name == "article.any") {
                                //								a_determiner = true;
                                //								console.log("context.deref: determiner " + determinerTerm + " is invalid in contex dereference!");
                                return null;
                            }
                            else if (determinerTerm.functor.name == "a") {
                                //								a_determiner = true;
                                //								console.log("context.deref: determiner " + determinerTerm + " is invalid in contex dereference!");
                                return null;
                            }
                            else {
                                console.log("context.deref: determiner " + determinerTerm + " not yet supported!");
                            }
                        }
                        //console.log("nameSort: '" + nameSort + "'");
                        //console.log("relationTerms: '" + relationTerms + "'");
                        var entities_mpl = this.findEntitiesOfSort(nameSort, o);
                        if (entities_mpl == null) {
                            console.log("No entities match name constraint '" + nameSort + "'");
                            this.lastDerefErrorType = DEREF_ERROR_NO_REFERENTS;
                            return null;
                        }
                        //console.log("entities_mpl: " + entities_mpl);
                        // adjectives:
                        for (var _o = 0, adjectiveTerms_1 = adjectiveTerms; _o < adjectiveTerms_1.length; _o++) {
                            var adjectiveTerm = adjectiveTerms_1[_o];
                            if (Term.equalsNoBindingsAttribute(nounTerm.attributes[0], adjectiveTerm.attributes[0]) == 1 &&
                                adjectiveTerm.attributes[1] instanceof ConstantTermAttribute) {
                                //								console.log("clauseElements: " + clauseElements);
                                //								console.log("nounTerm: " + nounTerm);
                                //								console.log("adjectiveTerm: " + adjectiveTerm);
                                var adjectiveStr = (adjectiveTerm.attributes[1].value);
                                var specificAdjectiveSort = o.getSortSilent(adjectiveStr);
                                if (specificAdjectiveSort != null) {
                                    //										console.log("adjective sort: " + specificAdjectiveSort.name);
                                    entities_mpl = this.filterByAdjective(specificAdjectiveSort, entities_mpl, o);
                                }
                                //								} else {
                                //									console.log("  discarded... (didn't match noun)");
                            }
                        }
                        //console.log("entities_mpl (after adjectives): " + entities_mpl);
                        // apply relations:
                        for (var _p = 0, relationTerms_1 = relationTerms; _p < relationTerms_1.length; _p++) {
                            var relationTermL = relationTerms_1[_p];
                            var relationTerm = relationTermL[0];
                            // check if it's a spatial relation (which are not in the logic representation, to save computation requirements):
                            if (relationTerm.functor.is_a(spatialRelationSort)) {
                                if (Term.equalsNoBindingsAttribute(nounTerm.attributes[0], relationTerm.attributes[0]) == 1 &&
                                    ((relationTerm.attributes[1] instanceof ConstantTermAttribute) ||
                                        relationTerm.attributes[1] == listenerVariable)) {
                                    var otherEntityID = void 0;
                                    if (relationTerm.attributes[1] == listenerVariable) {
                                        // in this case, we synthesize a relation with the variable replaced by a constant, since otherwise
                                        // the matching functions will fail:
                                        otherEntityID = this.ai.selfID;
                                        relationTerm = relationTerm.clone([]);
                                        relationTerm.attributes[1] = new ConstantTermAttribute(otherEntityID, this.ai.cache_sort_id);
                                    }
                                    else {
                                        otherEntityID = (relationTerm.attributes[1]).value;
                                    }
                                    var results_mpl = [];
                                    for (var _q = 0, entities_mpl_1 = entities_mpl; _q < entities_mpl_1.length; _q++) {
                                        var entities_5 = entities_mpl_1[_q];
                                        var results = [];
                                        for (var _r = 0, entities_2 = entities_5; _r < entities_2.length; _r++) {
                                            var entity = entities_2[_r];
                                            var spatialRelations = AI.spatialRelations(entity.objectID.value, otherEntityID);
                                            if (spatialRelations != null) {
                                                for (var _s = 0, spatialRelations_1 = spatialRelations; _s < spatialRelations_1.length; _s++) {
                                                    var sr = spatialRelations_1[_s];
                                                    if (relationTerm.functor.subsumes(sr)) {
                                                        results.push(entity);
                                                        break;
                                                    }
                                                }
                                            }
                                            else {
                                                var tmp = relationTerm.attributes[0];
                                                relationTerm.attributes[0] = entity.objectID;
                                                if (results.indexOf(entity) == -1 &&
                                                    entity.relationMatch(relationTerm, o, pos))
                                                    results.push(entity);
                                                relationTerm.attributes[0] = tmp;
                                            }
                                        }
                                        results_mpl.push(results);
                                    }
                                    entities_mpl = results_mpl;
                                }
                                else if (Term.equalsNoBindingsAttribute(nounTerm.attributes[0], relationTerm.attributes[1]) == 1 &&
                                    ((relationTerm.attributes[0] instanceof ConstantTermAttribute) ||
                                        relationTerm.attributes[0] == listenerVariable)) {
                                    var otherEntityID = void 0;
                                    if (relationTerm.attributes[1] == listenerVariable) {
                                        // in this case, we synthesize a relation with the variable replaced by a constant, since otherwise
                                        // the matching functions will fail:
                                        otherEntityID = this.ai.selfID;
                                        relationTerm = relationTerm.clone([]);
                                        relationTerm.attributes[0] = new ConstantTermAttribute(otherEntityID, this.ai.cache_sort_id);
                                    }
                                    else {
                                        otherEntityID = (relationTerm.attributes[0]).value;
                                    }
                                    var results_mpl = [];
                                    for (var _t = 0, entities_mpl_2 = entities_mpl; _t < entities_mpl_2.length; _t++) {
                                        var entities_6 = entities_mpl_2[_t];
                                        var results = [];
                                        for (var _u = 0, entities_3 = entities_6; _u < entities_3.length; _u++) {
                                            var entity = entities_3[_u];
                                            var spatialRelations = AI.spatialRelations(entity.objectID.value, otherEntityID);
                                            if (spatialRelations != null) {
                                                console.log("    spatialRelations != null");
                                                for (var _v = 0, spatialRelations_2 = spatialRelations; _v < spatialRelations_2.length; _v++) {
                                                    var sr = spatialRelations_2[_v];
                                                    if (relationTerm.functor.subsumes(sr)) {
                                                        results.push(entity);
                                                        break;
                                                    }
                                                }
                                            }
                                            else {
                                                var tmp = relationTerm.attributes[0];
                                                relationTerm.attributes[1] = entity.objectID;
                                                console.log("    spatialRelations == null, checking manually (2) for " + relationTerm);
                                                if (results.indexOf(entity) == -1 &&
                                                    entity.relationMatch(relationTerm, o, pos))
                                                    results.push(entity);
                                                relationTerm.attributes[1] = tmp;
                                            }
                                        }
                                        results_mpl.push(results);
                                    }
                                    entities_mpl = results_mpl;
                                }
                                //console.log("After " + relationTerm + ": " + entities_mpl[0].length + ", " + entities_mpl[1].length + ", " + entities_mpl[2].length);
                            }
                            else {
                                if (Term.equalsNoBindingsAttribute(nounTerm.attributes[0], relationTerm.attributes[0]) == 1 &&
                                    relationTerm.attributes[1] instanceof ConstantTermAttribute) {
                                    //								console.log("Considering relation (1): " + relationTerm);
                                    entities_mpl = this.filterByAtLeastOneRelation1(relationTermL, entities_mpl, o, pos);
                                }
                                else if (Term.equalsNoBindingsAttribute(nounTerm.attributes[0], relationTerm.attributes[1]) == 1 &&
                                    relationTerm.attributes[0] instanceof ConstantTermAttribute) {
                                    //								console.log("Considering relation (2): " + relationTerm);
                                    entities_mpl = this.filterByAtLeastOneRelation2(relationTermL, entities_mpl, o, pos);
                                }
                            }
                        }
                        //console.log("entities_mpl (after relations): " + entities_mpl);
                        if (entities_mpl[0].length == 0 &&
                            entities_mpl[1].length == 0 &&
                            entities_mpl[2].length == 0) {
                            this.lastDerefErrorType = DEREF_ERROR_NO_REFERENTS;
                            return null;
                        }
                        if (this_determiner || that_determiner) {
                            // remove both the speaker and listener:
                            var toDelete = [];
                            for (var _w = 0, _x = entities_mpl[1]; _w < _x.length; _w++) {
                                var e = _x[_w];
                                if (e.objectID.value == this.speaker ||
                                    e.objectID.value == this.ai.selfID) {
                                    toDelete.push(e);
                                }
                            }
                            for (var _y = 0, toDelete_1 = toDelete; _y < toDelete_1.length; _y++) {
                                var e = toDelete_1[_y];
                                entities_mpl[1].splice(entities_mpl[1].indexOf(e), 1);
                            }
                            // sort!
                            entities_mpl[1].sort(function (e1, e2) {
                                if (e1.distanceFromSpeaker == null &&
                                    e2.distanceFromSpeaker == null) {
                                    return 0;
                                }
                                else if (e1.distanceFromSpeaker == null) {
                                    return 1;
                                }
                                else if (e2.distanceFromSpeaker == null) {
                                    return -1;
                                }
                                else {
                                    return e1.distanceFromSpeaker - e2.distanceFromSpeaker;
                                }
                            });
                        }
                        if (returnAllCandidatesBeforeDisambiguation) {
                            var pool = null;
                            if (entities_mpl[0].length > 0) {
                                pool = entities_mpl[0];
                            }
                            else if (entities_mpl[1].length > 0) {
                                pool = entities_mpl[1];
                            }
                            else {
                                pool = entities_mpl[2];
                            }
                            pool = removeListDuplicates(pool);
                            var output_1 = [];
                            for (var _z = 0, pool_1 = pool; _z < pool_1.length; _z++) {
                                var e = pool_1[_z];
                                output_1.push(e.objectID);
                            }
                            return output_1;
                        }
                        // console.log("Before determiners: \nM: " + entities_mpl[0] + "\nP: " + entities_mpl[1] + "\nL: " + entities_mpl[2]);
                        // apply determiners:
                        // If there is no determiners, assume a "the":
                        if (determinerTerms.length == 0)
                            the_determiner = true;
                        var entities = null;
                        if (other_determiner) {
                            if (the_determiner) {
                                // first easy case: discard the speaker and listener, and see if that leaves already just 1:
                                var candidatesEliminated = false;
                                var entitiesButListenerAndSpeaker = [];
                                for (var _0 = 0, entities_mpl_3 = entities_mpl; _0 < entities_mpl_3.length; _0++) {
                                    var e_l = entities_mpl_3[_0];
                                    for (var _1 = 0, e_l_1 = e_l; _1 < e_l_1.length; _1++) {
                                        var e = e_l_1[_1];
                                        if (e.objectID.value == this.speaker ||
                                            e.objectID.value == this.ai.selfID) {
                                            candidatesEliminated = true;
                                        }
                                        else {
                                            entitiesButListenerAndSpeaker.push(e);
                                        }
                                    }
                                }
                                if (candidatesEliminated && entitiesButListenerAndSpeaker.length == 1) {
                                    entities = entitiesButListenerAndSpeaker;
                                }
                                else {
                                    var toDiscard = this.applySingularTheDeterminer(entities_mpl);
                                    if (toDiscard.length > 0) {
                                        var e = toDiscard[0];
                                        if (entities_mpl[0].indexOf(e) != -1)
                                            entities_mpl[0].splice(entities_mpl[0].indexOf(e), 1);
                                        if (entities_mpl[1].indexOf(e) != -1)
                                            entities_mpl[1].splice(entities_mpl[1].indexOf(e), 1);
                                        if (entities_mpl[2].indexOf(e) != -1)
                                            entities_mpl[2].splice(entities_mpl[2].indexOf(e), 1);
                                        if (singular) {
                                            entities = this.applySingularTheDeterminer(entities_mpl);
                                        }
                                        else {
                                            entities = entities_mpl[0].concat(entities_mpl[1]).concat(entities_mpl[2]);
                                        }
                                    }
                                }
                            }
                            else {
                                for (var _2 = 0, _3 = entities_mpl[0]; _2 < _3.length; _2++) {
                                    var e = _3[_2];
                                    if (entities_mpl[2].indexOf(e) != -1)
                                        entities_mpl[2].splice(entities_mpl[2].indexOf(e), 1);
                                }
                                for (var _4 = 0, _5 = entities_mpl[1]; _4 < _5.length; _4++) {
                                    var e = _5[_4];
                                    if (entities_mpl[2].indexOf(e) != -1)
                                        entities_mpl[2].splice(entities_mpl[2].indexOf(e), 1);
                                }
                                entities_mpl[0] = [];
                                entities_mpl[1] = [];
                                if (singular) {
                                    entities = this.applySingularTheDeterminer(entities_mpl);
                                }
                                else {
                                    entities = entities_mpl[2];
                                }
                            }
                        }
                        else if (the_determiner) {
                            if (singular) {
                                entities = this.applySingularTheDeterminer(entities_mpl);
                            }
                            else {
                                entities = entities_mpl[0].concat(entities_mpl[1]).concat(entities_mpl[2]);
                            }
                            //							console.log("the determiner with entities_mpl: " + entities_mpl + "\nResult: " + entities);
                        }
                        else if (this_determiner) {
                            // remove the speaker, and the content of the inventory from the list of candidates:
                            // since it's very weird to use "this" to refer to the inventory... (but
                            // we save it in ``toConsiderIfNoneLeft" just in case...)
                            var toDelete = [];
                            var toConsiderIfNoneLeft = [];
                            for (var idx = 0; idx < entities_mpl[1].length; idx++) {
                                if (entities_mpl[1][idx].objectID.value == this.speaker) {
                                    toDelete.push(entities_mpl[1][idx]);
                                }
                                else if (entities_mpl[1][idx].distanceFromSpeaker == 0 &&
                                    entities_mpl[1][idx].relationMatch(new Term(o.getSort("verb.have"), [new ConstantTermAttribute(this.speaker, o.getSort("#id")),
                                        entities_mpl[1][idx].objectID]), o, pos)) {
                                    toDelete.push(entities_mpl[1][idx]);
                                    toConsiderIfNoneLeft.push(entities_mpl[1][idx]);
                                }
                                else {
                                    // see if the object is contained in any other object we can see, and also remove:
                                    for (var _6 = 0, _7 = entities_mpl[1][idx].terms; _6 < _7.length; _6++) {
                                        var t = _7[_6];
                                        if (t.functor.is_a(this.cache_sort_contains) &&
                                            t.attributes.length >= 2 &&
                                            Term.equalsAttribute(t.attributes[1], entities_mpl[1][idx].objectID, new Bindings())) {
                                            toDelete.push(entities_mpl[1][idx]);
                                        }
                                    }
                                }
                            }
                            for (var _8 = 0, toDelete_2 = toDelete; _8 < toDelete_2.length; _8++) {
                                var e = toDelete_2[_8];
                                entities_mpl[1].splice(entities_mpl[1].indexOf(e), 1);
                            }
                            if (entities_mpl[1].length > 0) {
                                // get the one that is closest to the speaker:
                                if (entities_mpl[1].length > 1) {
                                    if ((entities_mpl[1][0].distanceFromSpeaker <
                                        entities_mpl[1][1].distanceFromSpeaker) ||
                                        (entities_mpl[1][0].distanceFromSpeaker != null &&
                                            entities_mpl[1][1].distanceFromSpeaker == null)) {
                                        if (entities_mpl[1][0].distanceFromSpeaker < MAXIMUM_DISTANCE_TO_BE_CONSIDERED_THIS) {
                                            entities = [entities_mpl[1][0]];
                                        }
                                    }
                                    else {
                                        // several locations are equally close (TODO: think to see if there is a way to disambiguate further)
                                        // ...
                                        // the two closest entities are at the same distance, we cannot disambiguate!
                                    }
                                }
                                else {
                                    if (entities_mpl[1][0].distanceFromSpeaker < MAXIMUM_DISTANCE_TO_BE_CONSIDERED_THIS) {
                                        entities = [entities_mpl[1][0]];
                                    }
                                }
                            }
                            if (entities == null) {
                                if (entities_mpl[0].length == 1) {
                                    entities = [entities_mpl[0][0]];
                                }
                                else if (entities_mpl[2].length == 1) {
                                    entities = [entities_mpl[2][0]];
                                }
                                else {
                                    entities = [];
                                }
                            }
                            // If we cannot find the entity we are looking for but one entity in the inventory matches, then maybe
                            // the speaker refers to that one:
                            if (entities.length == 0 && toConsiderIfNoneLeft.length == 1) {
                                entities = toConsiderIfNoneLeft;
                            }
                            //							console.log("\"this\" determiner with entities_mpl: " + entities_mpl + "\nResult: " + entities);
                        }
                        else if (that_determiner) {
                            if (entities_mpl[0].length == 1) {
                                entities = [entities_mpl[0][0]];
                            }
                            else if (entities_mpl[1].length == 1) {
                                entities = [entities_mpl[1][0]];
                            }
                            else if (entities_mpl[1].length == 2) {
                                // get the one that is further to the speaker:
                                if (entities_mpl[1][0].distanceFromSpeaker <
                                    entities_mpl[1][1].distanceFromSpeaker) {
                                    entities = [entities_mpl[1][1]];
                                }
                                else {
                                    // the two closest entities are at the same distance, we cannot disambiguate!
                                    entities = [];
                                }
                            }
                            else if (entities_mpl[2].length == 1) {
                                entities = [entities_mpl[2][0]];
                            }
                            else {
                                entities = [];
                            }
                            //						} else if (a_determiner) {
                            //							// just get the first one if it exists:
                            //							entities = entities_mpl[0].concat(entities_mpl[1]).concat(entities_mpl[2]);
                            //							if (entities.length>0) entities = [entities[0]];
                        }
                        else {
                            // if there is no determiner, then this should not be a context dereference ...
                            entities = entities_mpl[0].concat(entities_mpl[1]).concat(entities_mpl[2]);
                        }
                        if (entities != null) {
                            entities = removeListDuplicates(entities);
                            // consider grammatical number:
                            if (entities.length == 1) {
                                output.push(entities[0].objectID);
                            }
                            else if (entities.length > 1) {
                                if (singular) {
                                    //								 console.log("NLContext.deref noun: more than one entity matches a singular noun constraint ("+nameSort.name+"): " + entities);
                                }
                                else {
                                    for (var _9 = 0, entities_4 = entities; _9 < entities_4.length; _9++) {
                                        var e = entities_4[_9];
                                        output.push(e.objectID);
                                    }
                                }
                            }
                            else {
                                //							console.log("NLContext.deref noun: no entity matches the noun constraint ("+nameSort.name+")");
                            }
                        }
                        /*
                        if (determinerTerms.length > 0) {
                            console.log("After determiners/number: " + output);
                        }
                        */
                    }
                }
            }
            if (output.length == 0)
                this.lastDerefErrorType = DEREF_ERROR_CANNOT_DISAMBIGUATE;
            //console.log("output: " + output);
            return output;
        }
        else if (nounTerms.length > 1) {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
        return null;
    };
    NLContext.prototype.pronounMatch = function (pronounTerm, candidates, o) {
        var entity;
        // get the gender, only characters can match with "he"/"she", and only live humans do not match with "it"
        var gender = -1;
        if (pronounTerm.attributes[2].sort.name == "gender-masculine")
            gender = 0;
        else if (pronounTerm.attributes[2].sort.name == "gender-femenine")
            gender = 1;
        else if (pronounTerm.attributes[2].sort.name == "gender-neutral")
            gender = 2;
        for (var _i = 0, candidates_1 = candidates; _i < candidates_1.length; _i++) {
            var e2 = candidates_1[_i];
            if (gender == 0 || gender == 1)
                if (!e2.sortMatch(o.getSort("character")))
                    continue;
            if (gender == 2)
                if (e2.sortMatch(o.getSort("human")) &&
                    !e2.sortMatch(o.getSort("corpse")))
                    continue;
            if (e2.objectID.value != this.speaker && e2.objectID.value != this.ai.selfID) {
                if (entity == null) {
                    entity = e2;
                }
                else {
                    if (entity.mentionTime > e2.mentionTime) {
                        // we found it!
                        return entity;
                    }
                    // there is ambiguity... abort!
                    this.lastDerefErrorType = DEREF_ERROR_CANNOT_DISAMBIGUATE;
                    return null;
                }
            }
        }
        return entity;
    };
    NLContext.prototype.matchPronounToNLPRDeref = function (pronounTerm, deref, o) {
        if (pronounTerm.attributes.length == 4 &&
            pronounTerm.attributes[1].sort.name == "singular" &&
            pronounTerm.attributes[3].sort.is_a(o.getSort("third-person"))) {
            // singular:
            // check if the last deref record would be a good match for this pronoun:			
            if (deref.functor.name == "#derefFromContext" &&
                deref.attributes.length == 2) {
                var id = deref.attributes[1];
                if (id instanceof ConstantTermAttribute) {
                    var idstr = id.value;
                    var entity = this.findByID(idstr);
                    if (entity != null) {
                        entity = this.pronounMatch(pronounTerm, [entity], o);
                        if (entity != null) {
                            // console.log("   matchPronounToNLPRDeref match (#derefFromContext): " + id);
                            return [id];
                        }
                    }
                }
            }
            else if (deref.functor.name == "#derefQuery" &&
                deref.attributes.length == 3 &&
                deref.attributes[2] instanceof TermTermAttribute) {
                // console.log("pronounTerm: " + pronounTerm)
                // console.log("deref: " + deref);
                // See if the query could match: 
                var gender = -1;
                if (pronounTerm.attributes[2].sort.name == "gender-masculine")
                    gender = 0;
                else if (pronounTerm.attributes[2].sort.name == "gender-femenine")
                    gender = 1;
                else if (pronounTerm.attributes[2].sort.name == "gender-neutral")
                    gender = 2;
                var match = true;
                var queryTerms = NLParser.termsInList(deref.attributes[2].term, "#and");
                for (var _i = 0, queryTerms_1 = queryTerms; _i < queryTerms_1.length; _i++) {
                    var queryTerm = queryTerms_1[_i];
                    if (gender == 0 || gender == 1)
                        if (!queryTerm.functor.is_a(o.getSort("character"))) {
                            match = false;
                            break;
                        }
                    if (gender == 2)
                        if (queryTerm.functor.is_a(o.getSort("human")) &&
                            !queryTerm.functor.is_a(o.getSort("corpse"))) {
                            match = false;
                            break;
                        }
                }
                if (match) {
                    // console.log("   matchPronounToNLPRDeref match (#derefQuery): " + deref.attributes[1]);
                    return [deref.attributes[1]];
                }
            }
            else {
                // TODO: handle universals/hypotheticals
                // ...
            }
        }
        else {
            // TODO: support the plural case
            // ...
        }
        return null;
    };
    /*
        - Given a list containing [entities in mentions, entities in short term memory, entities in long-term memory]
        - This function returns the list of entities that the deteminer "the" (singular) refers to
        - If no entity applies, then it will return an empty list
    */
    NLContext.prototype.applySingularTheDeterminer = function (msl) {
        var pool = null;
        if (msl[0].length > 0) {
            pool = msl[0];
        }
        else if (msl[1].length > 0) {
            pool = msl[1];
        }
        else {
            pool = msl[2];
        }
        // console.log("applySingularTheDeterminer (before), M: " + msl[0] + "\nP: " + msl[1] + "\nL: " + msl[2]);
        // console.log("pool: " + pool);
        if (pool.length == 1) {
            // mentions:
            return [pool[0]];
        }
        else if (pool.length > 1) {
            // mentions:
            // console.log("    msl[0][0].mentionTime: " + msl[0][0].mentionTime + " (" + msl[0][0].objectID + ")");
            // console.log("    msl[0][1].mentionTime: " + msl[0][1].mentionTime + " (" + msl[0][0].objectID + ")");
            if (pool[0].mentionTime >
                pool[1].mentionTime) {
                return [pool[0]];
            }
            else {
                // the two closest entities were mentioned at the same time.
                // Since we cannot disambiguate just by time, try to disambiguate by distance:
                var candidates = [];
                var bestTime = null;
                for (var _i = 0, pool_2 = pool; _i < pool_2.length; _i++) {
                    var e = pool_2[_i];
                    if (bestTime == null || e.mentionTime < bestTime) {
                        bestTime = e.mentionTime;
                        candidates = [e];
                    }
                    else if (bestTime == e.mentionTime) {
                        candidates.push(e);
                    }
                }
                if (candidates.length <= 1)
                    return candidates;
                // sort by distance:
                candidates.sort(function (e1, e2) {
                    if (e1.distanceFromSpeaker == null &&
                        e2.distanceFromSpeaker == null) {
                        return 0;
                    }
                    else if (e1.distanceFromSpeaker == null) {
                        return 1;
                    }
                    else if (e2.distanceFromSpeaker == null) {
                        return -1;
                    }
                    else {
                        return e1.distanceFromSpeaker - e2.distanceFromSpeaker;
                    }
                });
                // console.log("sorted candidates: " + candidates);
                // console.log("SPACE_NEAR_FAR_THRESHOLD: " + SPACE_NEAR_FAR_THRESHOLD);
                // if the closest one is clearly closer (and the others are far), disambiguate:
                if (candidates[0].distanceFromSpeaker < candidates[1].distanceFromSpeaker &&
                    candidates[0].distanceFromSpeaker < SPACE_NEAR_FAR_THRESHOLD &&
                    candidates[1].distanceFromSpeaker > SPACE_NEAR_FAR_THRESHOLD) {
                    return [candidates[0]];
                }
                return [];
            }
        }
        return [];
    };
    NLContext.prototype.findLocationOfID = function (id) {
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var entity = _a[_i];
            if (entity.objectID.value == id) {
                for (var _b = 0, _c = entity.terms; _b < _c.length; _b++) {
                    var t = _c[_b];
                    if (t.functor.is_a(this.cache_sort_space_at) &&
                        t.attributes.length == 2 &&
                        t.attributes[0] instanceof ConstantTermAttribute &&
                        t.attributes[1] instanceof ConstantTermAttribute &&
                        t.attributes[0].value == id) {
                        return this.findByID(t.attributes[1].value);
                    }
                }
            }
        }
        for (var _d = 0, _e = this.mentions; _d < _e.length; _d++) {
            var entity = _e[_d];
            if (entity.objectID.value == id) {
                for (var _f = 0, _g = entity.terms; _f < _g.length; _f++) {
                    var t = _g[_f];
                    if (t.functor.is_a(this.cache_sort_space_at) &&
                        t.attributes.length == 2 &&
                        t.attributes[0] instanceof ConstantTermAttribute &&
                        t.attributes[1] instanceof ConstantTermAttribute &&
                        t.attributes[0].value == id) {
                        return this.findByID(t.attributes[1].value);
                    }
                }
            }
        }
        return null;
    };
    NLContext.prototype.findMostSpecificOutdoorLocationOfID = function (id, o) {
        var open = [];
        var closed = [];
        var outdoors_sort = o.getSort("outdoor.location");
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var entity = _a[_i];
            if (entity.objectID.value == id) {
                for (var _b = 0, _c = entity.terms; _b < _c.length; _b++) {
                    var t = _c[_b];
                    if (t.functor.is_a(this.cache_sort_space_at) &&
                        t.attributes.length == 2 &&
                        t.attributes[0] instanceof ConstantTermAttribute &&
                        t.attributes[1] instanceof ConstantTermAttribute &&
                        t.attributes[0].value == id) {
                        open.push(entity);
                    }
                }
            }
        }
        while (open.length > 0) {
            var current = open[0];
            open.splice(0, 1);
            closed.push(current);
            for (var _d = 0, _e = current.terms; _d < _e.length; _d++) {
                var t = _e[_d];
                if (t.functor.is_a(outdoors_sort)) {
                    return current;
                }
                if (t.functor.is_a(this.cache_sort_space_at) &&
                    t.attributes.length == 2 &&
                    t.attributes[0] instanceof ConstantTermAttribute &&
                    t.attributes[1] instanceof ConstantTermAttribute &&
                    t.attributes[0].value == current.objectID.value) {
                    var next = this.findByID(t.attributes[1].value);
                    if (next != null && closed.indexOf(next) == -1 && open.indexOf(next) == -1)
                        open.push(next);
                }
            }
        }
        return null;
    };
    NLContext.prototype.findByID = function (id) {
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var entity = _a[_i];
            if (entity.objectID.value == id)
                return entity;
        }
        for (var _b = 0, _c = this.mentions; _b < _c.length; _b++) {
            var entity = _c[_b];
            if (entity.objectID.value == id)
                return entity;
        }
        return null;
    };
    NLContext.prototype.findClosestProperNoun = function (name, o) {
        //		console.log("NLContext.findClosestProperNoun: " + name);
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var entity = _a[_i];
            if (entity.properNounMatch(name))
                return entity;
        }
        for (var _b = 0, _c = this.mentions; _b < _c.length; _b++) {
            var entity = _c[_b];
            if (entity.properNounMatch(name))
                return entity;
        }
        var nameSort = o.getSort("name");
        for (var _d = 0, _e = this.ai.longTermMemory.allSingleTermMatches(nameSort, 2, o); _d < _e.length; _d++) {
            var s = _e[_d];
            if (s.terms[0].functor == nameSort &&
                s.terms[0].attributes[1] instanceof ConstantTermAttribute &&
                s.terms[0].attributes[1].value == name) {
                var e = this.newContextEntity((s.terms[0].attributes[0]), 0, null, o, true);
                if (e != null)
                    return e;
            }
        }
        return null;
    };
    NLContext.prototype.findAllProperNoun = function (name, o) {
        var matches = [];
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var entity = _a[_i];
            if (entity.properNounMatch(name))
                matches.push(entity);
        }
        for (var _b = 0, _c = this.mentions; _b < _c.length; _b++) {
            var entity = _c[_b];
            if (entity.properNounMatch(name))
                matches.push(entity);
        }
        var nameSort = o.getSort("name");
        for (var _d = 0, _e = this.ai.longTermMemory.allSingleTermMatches(nameSort, 2, o); _d < _e.length; _d++) {
            var s = _e[_d];
            if (s.terms[0].functor == nameSort &&
                (s.terms[0].attributes[0] instanceof ConstantTermAttribute) &&
                (s.terms[0].attributes[1] instanceof ConstantTermAttribute) &&
                s.terms[0].attributes[1].value == name) {
                //console.log("    findAllProperNoun: '"+(<ConstantTermAttribute>s.terms[0].attributes[0]).value+"'");
                var e = this.newContextEntity((s.terms[0].attributes[0]), 0, null, o, true);
                if (e != null) {
                    matches.push(e);
                }
                else {
                    // console.log("    findAllProperNoun: failed to create entity for name '"+name+"'");
                }
            }
        }
        return matches;
    };
    // returns 3 arrays, containins matches in mentions, shortTermMemory and long-term memory
    NLContext.prototype.findEntitiesOfSort = function (sort, o) {
        var results_mpl = [];
        var results = [];
        //		let any_match:boolean = false;
        for (var _i = 0, _a = this.mentions; _i < _a.length; _i++) {
            var entity = _a[_i];
            if (entity.sortMatch(sort)) {
                results.push(entity);
                //any_match = true;
            }
        }
        results_mpl.push(results);
        results = [];
        for (var _b = 0, _c = this.shortTermMemory; _b < _c.length; _b++) {
            var entity = _c[_b];
            if (entity.sortMatch(sort)) {
                results.push(entity);
                //				any_match = true;
            }
        }
        results_mpl.push(results);
        //		if (any_match) {
        // if we have found something on shortTermMemory or mentions, do not check for long-term memory:
        //			results_mpl.push([]);
        //			return results_mpl;
        //		}
        results = [];
        for (var _d = 0, _e = this.ai.longTermMemory.allSingleTermMatches(sort, 1, o); _d < _e.length; _d++) {
            var s = _e[_d];
            if (s.terms[0].functor.is_a(sort) &&
                s.terms[0].attributes[0] instanceof ConstantTermAttribute) {
                var e = this.newContextEntity((s.terms[0].attributes[0]), 0, null, o, true);
                if (e != null)
                    results.push(e);
            }
        }
        results_mpl.push(results);
        return results_mpl;
    };
    // returns 3 arrays, containins matches in mentions, shortTermMemory and long-term memory
    NLContext.prototype.filterByAdjective = function (adjective, entities_mpl, o) {
        //		console.log("filterByAdjective: " + adjective);
        var results_mpl = [];
        for (var _i = 0, entities_mpl_4 = entities_mpl; _i < entities_mpl_4.length; _i++) {
            var entities = entities_mpl_4[_i];
            var results = [];
            for (var _a = 0, entities_7 = entities; _a < entities_7.length; _a++) {
                var entity = entities_7[_a];
                if (entity.adjectiveMatch(adjective, o))
                    results.push(entity);
            }
            results_mpl.push(results);
        }
        return results_mpl;
    };
    // returns 3 arrays, containins matches in mentions, shortTermMemory and long-term memory
    NLContext.prototype.filterByRelation1 = function (relation, entities_mpl, o, pos) {
        var results_mpl = [];
        for (var _i = 0, entities_mpl_5 = entities_mpl; _i < entities_mpl_5.length; _i++) {
            var entities = entities_mpl_5[_i];
            var results = [];
            for (var _a = 0, entities_8 = entities; _a < entities_8.length; _a++) {
                var entity = entities_8[_a];
                var tmp = relation.attributes[0];
                relation.attributes[0] = entity.objectID;
                if (entity.relationMatch(relation, o, pos))
                    results.push(entity);
                relation.attributes[0] = tmp;
            }
            results_mpl.push(results);
        }
        return results_mpl;
    };
    // returns 3 arrays, containins matches in mentions, shortTermMemory and long-term memory
    NLContext.prototype.filterByAtLeastOneRelation1 = function (relationL, entities_mpl, o, pos) {
        var results_mpl = [];
        for (var _i = 0, entities_mpl_6 = entities_mpl; _i < entities_mpl_6.length; _i++) {
            var entities = entities_mpl_6[_i];
            var results = [];
            for (var _a = 0, entities_9 = entities; _a < entities_9.length; _a++) {
                var entity = entities_9[_a];
                for (var _b = 0, relationL_1 = relationL; _b < relationL_1.length; _b++) {
                    var relation = relationL_1[_b];
                    var tmp = relation.attributes[0];
                    relation.attributes[0] = entity.objectID;
                    if (results.indexOf(entity) == -1 &&
                        entity.relationMatch(relation, o, pos))
                        results.push(entity);
                    relation.attributes[0] = tmp;
                }
            }
            results_mpl.push(results);
        }
        return results_mpl;
    };
    // returns 3 arrays, containins matches in mentions, shortTermMemory and long-term memory
    NLContext.prototype.filterByRelation2 = function (relation, entities_mpl, o, pos) {
        var results_mpl = [];
        for (var _i = 0, entities_mpl_7 = entities_mpl; _i < entities_mpl_7.length; _i++) {
            var entities = entities_mpl_7[_i];
            var results = [];
            for (var _a = 0, entities_10 = entities; _a < entities_10.length; _a++) {
                var entity = entities_10[_a];
                var tmp = relation.attributes[1];
                relation.attributes[1] = entity.objectID;
                if (entity.relationMatch(relation, o, pos))
                    results.push(entity);
                relation.attributes[1] = tmp;
            }
            results_mpl.push(results);
        }
        return results_mpl;
    };
    // returns 3 arrays, containins matches in mentions, shortTermMemory and long-term memory
    NLContext.prototype.filterByAtLeastOneRelation2 = function (relationL, entities_mpl, o, pos) {
        var results_mpl = [];
        for (var _i = 0, entities_mpl_8 = entities_mpl; _i < entities_mpl_8.length; _i++) {
            var entities = entities_mpl_8[_i];
            var results = [];
            for (var _a = 0, entities_11 = entities; _a < entities_11.length; _a++) {
                var entity = entities_11[_a];
                for (var _b = 0, relationL_2 = relationL; _b < relationL_2.length; _b++) {
                    var relation = relationL_2[_b];
                    var tmp = relation.attributes[1];
                    relation.attributes[1] = entity.objectID;
                    if (results.indexOf(entity) == -1 &&
                        entity.relationMatch(relation, o, pos))
                        results.push(entity);
                    relation.attributes[1] = tmp;
                }
            }
            results_mpl.push(results);
        }
        return results_mpl;
    };
    // Attempts to complete verb arguments by looking at previous sentences in the context. For example, if some one says:
    // "Do you know my password?"
    // and then "Would qwerty know?" -> then the parameter "my password" should be transferred to the "qwerty know" verb
    NLContext.prototype.completeVerbArgumentsFromContext = function (verb, outputVariable, o) {
        if (verb.attributes.length == 0)
            return null;
        var verbSort = verb.functor;
        var firstAttribute = verb.attributes[0];
        var nAttributes = verb.attributes.length;
        if (verbSort.name == "#cons") {
            nAttributes = verb.attributes.length - 1;
            if (verb.attributes.length >= 2 &&
                verb.attributes[0] instanceof ConstantTermAttribute) {
                verbSort = o.getSort(verb.attributes[0].value);
                firstAttribute = verb.attributes[1];
            }
            else {
                return null;
            }
        }
        // get the last performative by each agent:
        var performativesToConsider = [];
        {
            var perf = this.lastPerformativeBy(this.ai.selfID);
            if (perf != null)
                performativesToConsider.push(perf);
            perf = this.lastPerformativeBy(this.speaker);
            if (perf != null)
                performativesToConsider.push(perf);
        }
        var possibleOutputs = [];
        for (var _i = 0, performativesToConsider_1 = performativesToConsider; _i < performativesToConsider_1.length; _i++) {
            var perf = performativesToConsider_1[_i];
            var verb2 = perf.performative.findSubtermWithFunctorSort(verbSort);
            if (verb2 != null) {
                // found it! reconstruct a possible verb:
                var output = new Term(verbSort, [firstAttribute]);
                for (var i = 1; i < verb2.attributes.length; i++) {
                    output.attributes.push(verb2.attributes[i]);
                }
                possibleOutputs.push(new TermTermAttribute(output));
            }
        }
        if (possibleOutputs.length == 0) {
            if (verbSort.is_a(o.getSort("verb.know"))) {
                for (var _a = 0, performativesToConsider_2 = performativesToConsider; _a < performativesToConsider_2.length; _a++) {
                    var perf = performativesToConsider_2[_a];
                    if (perf.performative.functor.name == "perf.q.query" &&
                        perf.performative.attributes.length == 3) {
                        var output = new Term(verbSort, [firstAttribute,
                            perf.performative.attributes[2]]);
                        possibleOutputs.push(new TermTermAttribute(output));
                    }
                    else {
                        /*
                                                <sort name="perf.question" super="perf.request"/>
                                                <sort name="perf.q.predicate" super="perf.question"/>
                                                <sort name="perf.q.predicate-negated" super="perf.question"/>
                                                <sort name="perf.q.whereis" super="perf.question"/>
                                                <sort name="perf.q.whereto" super="perf.question"/>
                                                <sort name="perf.q.query-followup" super="perf.q.query"/>
                                                <sort name="perf.q.whois.name" super="perf.question"/>
                                                <sort name="perf.q.whois.noname" super="perf.question"/>
                                                <sort name="perf.q.whatis.name" super="perf.question"/>
                                                <sort name="perf.q.whatis.noname" super="perf.question"/>
                                                <sort name="perf.q.action" super="perf.question,perf.request.action"/> <!-- this is similar to perf.request.action, except that it is a question, rather than a command -->
                                                <sort name="perf.q.howareyou" super="perf.question"/>
                                                <sort name="perf.q.when" super="perf.question"/>
                                                <sort name="perf.q.howmany" super="perf.question"/>
                                                <sort name="perf.q.why" super="perf.question"/>
                                                <sort name="perf.q.how" super="perf.question"/>
                        */
                    }
                }
            }
            else if (verbSort.is_a(o.getSort("verb.go"))) {
                if (nAttributes == 1) {
                    // we are missing the destination, see if in the last performative, we mentioned a location:
                    for (var _b = 0, performativesToConsider_3 = performativesToConsider; _b < performativesToConsider_3.length; _b++) {
                        var perf = performativesToConsider_3[_b];
                        var IDs = perf.IDsInPerformative(o);
                        for (var _c = 0, IDs_2 = IDs; _c < IDs_2.length; _c++) {
                            var ID = IDs_2[_c];
                            var e = this.findByID(ID.value);
                            if (e != null && e.sortMatch(o.getSort("space.location"))) {
                                var output = new Term(verbSort, [firstAttribute,
                                    ID]);
                                possibleOutputs.push(new TermTermAttribute(output));
                            }
                        }
                    }
                }
            }
        }
        return possibleOutputs;
    };
    NLContext.prototype.newHypotheticalID = function () {
        this.nextHypotheticalID++;
        return "H-" + this.speaker + "-" + this.nextHypotheticalID;
    };
    NLContext.prototype.getNLContextPerformative = function (perf) {
        for (var _i = 0, _a = this.performatives; _i < _a.length; _i++) {
            var p = _a[_i];
            if (p.performative == perf)
                return p;
        }
        return null;
    };
    NLContext.fromXML = function (xml, o, ai, mentionMemorySize) {
        var c = new NLContext(xml.getAttribute("speaker"), ai, mentionMemorySize);
        var p_xml = getFirstElementChildByTag(xml, "shortTermMemory");
        if (p_xml != null) {
            for (var _i = 0, _a = getElementChildrenByTag(p_xml, "NLContextEntity"); _i < _a.length; _i++) {
                var ce_xml = _a[_i];
                var ce = NLContextEntity.fromXML(ce_xml, o);
                if (ce != null)
                    c.shortTermMemory.push(ce);
            }
        }
        var m_xml = getFirstElementChildByTag(xml, "mentions");
        if (m_xml != null) {
            for (var _b = 0, _c = getElementChildrenByTag(m_xml, "NLContextEntity"); _b < _c.length; _b++) {
                var ce_xml = _c[_b];
                var ce = NLContextEntity.fromXML(ce_xml, o);
                if (ce != null)
                    c.mentions.push(ce);
            }
        }
        var pf_xml = getFirstElementChildByTag(xml, "performatives");
        if (pf_xml != null) {
            for (var _d = 0, _e = getElementChildrenByTag(pf_xml, "NLContextPerformative"); _d < _e.length; _d++) {
                var cp_xml = _e[_d];
                var cp = NLContextPerformative.fromXML(cp_xml, c, o);
                if (cp != null)
                    c.performatives.push(cp);
            }
        }
        var tmp_xml = getFirstElementChildByTag(xml, "inConversation");
        if (tmp_xml != null)
            c.inConversation = tmp_xml.getAttribute("value") == "true";
        tmp_xml = getFirstElementChildByTag(xml, "lastPerformativeInvolvingThisCharacterWasToUs");
        if (tmp_xml != null)
            c.lastPerformativeInvolvingThisCharacterWasToUs = tmp_xml.getAttribute("value") == "true";
        tmp_xml = getFirstElementChildByTag(xml, "expectingYes");
        if (tmp_xml != null)
            c.expectingYes = tmp_xml.getAttribute("value") == "true";
        tmp_xml = getFirstElementChildByTag(xml, "expectingThankYou");
        if (tmp_xml != null)
            c.expectingThankYou = tmp_xml.getAttribute("value") == "true";
        tmp_xml = getFirstElementChildByTag(xml, "expectingYouAreWelcome");
        if (tmp_xml != null)
            c.expectingYouAreWelcome = tmp_xml.getAttribute("value") == "true";
        tmp_xml = getFirstElementChildByTag(xml, "expectingGreet");
        if (tmp_xml != null)
            c.expectingGreet = tmp_xml.getAttribute("value") == "true";
        tmp_xml = getFirstElementChildByTag(xml, "expectingFarewell");
        if (tmp_xml != null)
            c.expectingFarewell = tmp_xml.getAttribute("value") == "true";
        tmp_xml = getFirstElementChildByTag(xml, "expectingNicetomeetyoutoo");
        if (tmp_xml != null)
            c.expectingNicetomeetyoutoo = tmp_xml.getAttribute("value") == "true";
        for (var _f = 0, _g = getElementChildrenByTag(xml, "expectingAnswerToQuestion_stack"); _f < _g.length; _f++) {
            var tmp_xml2 = _g[_f];
            c.expectingAnswerToQuestion_stack.push(c.performatives[Number(tmp_xml2.getAttribute("value"))]);
            c.expectingAnswerToQuestionTimeStamp_stack.push(Number(tmp_xml2.getAttribute("time")));
        }
        for (var _h = 0, _j = getElementChildrenByTag(xml, "expectingConfirmationToRequest_stack"); _h < _j.length; _h++) {
            var tmp_xml2 = _j[_h];
            c.expectingConfirmationToRequest_stack.push(c.performatives[Number(tmp_xml2.getAttribute("value"))]);
            c.expectingConfirmationToRequestTimeStamp_stack.push(Number(tmp_xml2.getAttribute("time")));
        }
        tmp_xml = getFirstElementChildByTag(xml, "lastEnumeratedQuestion_answered");
        if (tmp_xml != null)
            c.lastEnumeratedQuestion_answered = c.performatives[Number(tmp_xml.getAttribute("value"))];
        c.lastEnumeratedQuestion_answers = null;
        for (var _k = 0, _l = getElementChildrenByTag(xml, "lastEnumeratedQuestion_answers"); _k < _l.length; _k++) {
            var tmp_xml2 = _l[_k];
            if (c.lastEnumeratedQuestion_answers == null)
                c.lastEnumeratedQuestion_answers = [];
            //			c.lastEnumeratedQuestion_answers.push(Term.fromString(tmp_xml2.getAttribute("value"), o));
            c.lastEnumeratedQuestion_answers.push(Term.parseAttribute(tmp_xml2.getAttribute("value"), o, [], []));
        }
        tmp_xml = getFirstElementChildByTag(xml, "lastEnumeratedQuestion_next_answer_index");
        if (tmp_xml != null)
            c.lastEnumeratedQuestion_next_answer_index = Number(tmp_xml.getAttribute("value"));
        tmp_xml = getFirstElementChildByTag(xml, "nextHypotheticalID");
        if (tmp_xml != null)
            c.nextHypotheticalID = Number(tmp_xml.getAttribute("value"));
        tmp_xml = getFirstElementChildByTag(xml, "lastDerefErrorType");
        if (tmp_xml != null)
            c.lastDerefErrorType = Number(tmp_xml.getAttribute("value"));
        return c;
    };
    NLContext.prototype.outerHTML = function () { return this.saveToXML(); };
    NLContext.prototype.saveToXML = function () {
        var str = "<context speaker=\"" + this.speaker + "\">\n";
        str += "<shortTermMemory>\n";
        for (var _i = 0, _a = this.shortTermMemory; _i < _a.length; _i++) {
            var ce = _a[_i];
            str += ce.saveToXML() + "\n";
        }
        str += "</shortTermMemory>\n";
        str += "<mentions>\n";
        for (var _b = 0, _c = this.mentions; _b < _c.length; _b++) {
            var ce = _c[_b];
            str += ce.saveToXML() + "\n";
        }
        str += "</mentions>\n";
        str += "<performatives>\n";
        for (var _d = 0, _e = this.performatives; _d < _e.length; _d++) {
            var cp = _e[_d];
            str += cp.saveToXML() + "\n";
        }
        str += "</performatives>\n";
        str += "<inConversation value=\"" + this.inConversation + "\"/>\n";
        str += "<lastPerformativeInvolvingThisCharacterWasToUs value=\"" + this.lastPerformativeInvolvingThisCharacterWasToUs + "\"/>\n";
        str += "<expectingYes value=\"" + this.expectingYes + "\"/>\n";
        str += "<expectingThankYou value=\"" + this.expectingThankYou + "\"/>\n";
        str += "<expectingYouAreWelcome value=\"" + this.expectingYouAreWelcome + "\"/>\n";
        str += "<expectingGreet value=\"" + this.expectingGreet + "\"/>\n";
        str += "<expectingFarewell value=\"" + this.expectingFarewell + "\"/>\n";
        str += "<expectingNicetomeetyoutoo value=\"" + this.expectingNicetomeetyoutoo + "\"/>\n";
        for (var i = 0; i < this.expectingAnswerToQuestion_stack.length; i++) {
            str += "<expectingAnswerToQuestion_stack value=\"" + this.performatives.indexOf(this.expectingAnswerToQuestion_stack[i]) + "\" time=\"" + this.expectingAnswerToQuestionTimeStamp_stack[i] + "\"/>\n";
        }
        for (var i = 0; i < this.expectingConfirmationToRequest_stack.length; i++) {
            str += "<expectingConfirmationToRequest_stack value=\"" + this.performatives.indexOf(this.expectingConfirmationToRequest_stack[i]) + "\" time=\"" + this.expectingConfirmationToRequestTimeStamp_stack[i] + "\"/>\n";
        }
        if (this.lastEnumeratedQuestion_answered != null) {
            str += "<lastEnumeratedQuestion_answered value=\"" + this.performatives.indexOf(this.lastEnumeratedQuestion_answered) + "\"/>\n";
        }
        if (this.lastEnumeratedQuestion_answers != null) {
            for (var i = 0; i < this.lastEnumeratedQuestion_answers.length; i++) {
                str += "<lastEnumeratedQuestion_answers value=\"" + this.lastEnumeratedQuestion_answers[i].toStringXML() + "\"/>\n";
            }
        }
        str += "<lastEnumeratedQuestion_next_answer_index value=\"" + this.lastEnumeratedQuestion_next_answer_index + "\"/>\n";
        str += "<nextHypotheticalID value=\"" + this.nextHypotheticalID + "\"/>\n";
        str += "<lastDerefErrorType value=\"" + this.lastDerefErrorType + "\"/>\n";
        str += "</context>";
        return str;
    };
    return NLContext;
}());
