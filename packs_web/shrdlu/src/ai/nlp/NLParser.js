var NLParser = /** @class */ (function () {
    function NLParser(o, dp) {
        this.o = null;
        this.defaultPriority = 100;
        this.posParser = null;
        this.rules = [];
        this.compiledRules = {};
        this.talkingTargets = null; // If this is != null, it restricts the possible objects we can talk to (for easier disambiguation)
        // errors from the last parse:
        this.error_semantic = []; // stores the parses that were properly parsed, but failed semantic checks
        this.error_deref = [];
        this.error_unrecognizedTokens = [];
        this.error_grammatical = false;
        this.o = o;
        this.defaultPriority = dp;
    }
    NLParser.fromXML = function (xml, o) {
        var parser = new NLParser(o, Number(xml.getAttribute("defaultPriority")));
        parser.posParser = new POSParser(o);
        var speakerVariable = new VariableTermAttribute(o.getSort("any"), "SPEAKER");
        var listenerVariable = new VariableTermAttribute(o.getSort("any"), "LISTENER");
        for (var _i = 0, _a = getElementChildrenByTag(xml, "NLPattern"); _i < _a.length; _i++) {
            var rulexml = _a[_i];
            var priority = parser.defaultPriority;
            if (rulexml.getAttribute("priority") != null)
                priority = Number(rulexml.getAttribute("priority"));
            var rule = NLPatternRule.fromString(rulexml.getAttribute("name"), rulexml.getAttribute("head"), rulexml.getAttribute("body"), priority, o, speakerVariable, listenerVariable);
            //			console.log("rule loaded with head: " + rule.head);
            // check for repeated names:
            for (var _b = 0, _c = parser.rules; _b < _c.length; _b++) {
                var rule2 = _c[_b];
                if (rule.name == rule2.name) {
                    console.error("parse rule has a repeated name: " + rule.name);
                }
            }
            parser.rules.push(rule);
        }
        var total_nstates = 0;
        for (var _d = 0, _e = ["nounOrOne",
            "nounPhraseListFromContext",
            "maybeNegatedPrepositionalPhrase",
            "maybeNegatedAdjective",
            "nounPhrase",
            "nounPhraseNoDeterminer",
            "nounPhraseNoDeterminerNoProperNoun",
            "properNounCompound",
            "performative",
            "perf.request.action.internal",
            "perf.inform.internal"]; _d < _e.length; _d++) {
            var sortName = _e[_d];
            var compiled = new CompiledNLPatternRules("compiled-" + sortName, o, speakerVariable, listenerVariable);
            compiled.populate(o.getSort(sortName), parser);
            var nstates = compiled.root.getAllStatesForDOTString().length;
            console.log("compiled parse graph for " + sortName + " has " + nstates + " nodes");
            total_nstates += nstates;
            // To generate the pdf type: dot -Tpdf grammar-2.4.dot -o grammar-2.4.pdf
            // if (sortName == "perf.inform.internal") {
            // 	console.log(compiled.root.convertToDOTString());
            // }
            parser.compiledRules[sortName] = compiled;
        }
        console.log("compiled parse graph total size: " + total_nstates);
        /*
                // list of sorts for which there are rules:
                let sortList:string[] = [];
                for(let rule of parser.rules) {
                    if (sortList.indexOf(rule.head.functor.name) == -1) sortList.push(rule.head.functor.name);
                }
                console.log(sortList);
        */
        return parser;
    };
    //  Parses a natural language sentence considering only rules that produce a resulting clause of sort "s"
    NLParser.prototype.parse = function (sentence, s, context, AI) {
        // STEP 1: Tokenization
        var tokens = this.posParser.tokenize(sentence);
        //console.log("Tokenization:\n" + tokens);
        if (tokens == null || tokens.length == 0) {
            return [];
        }
        // STEP 2: Dictionary-based multi-token word detection (merge tokens)
        var tokens2 = this.posParser.identifyMultiTokenWords(tokens);
        //console.log("Multi-token word identification:\n" + tokens2.toString());
        // STEP 3: Part of Speech Tagging
        this.posParser.unrecognizedTokens = [];
        this.posParser.POSTagging(tokens2, this.o);
        // console.log("POS Tagging:\n" + tokens2.toString());
        this.error_semantic = [];
        this.error_deref = [];
        this.error_unrecognizedTokens = [];
        this.error_grammatical = false;
        var results = [];
        var derefErrors = [];
        var bestPriorityOfFirstRule = 0;
        var semanticalErrors = [];
        for (var sort in this.compiledRules) {
            this.compiledRules[sort].lastDerefErrors = [];
        }
        var compiled = this.compiledRules[s.name];
        if (compiled != null) {
            // if we have a compiled tree, use it!
            var results2 = compiled.parse(tokens2, true, context, this, AI);
            if (results2 != null && results2.length > 0) {
                for (var _i = 0, results2_1 = results2; _i < results2_1.length; _i++) {
                    var r = results2_1[_i];
                    // console.log("(1) result before resolving the lists:" + r.result);
                    r.result = this.resolveLists(r.result);
                    // console.log("(2) result after resolving the lists:" + r.result);
                    // properly resolve the "listener" variable:
                    if (s.name == "performative" && r.result.attributes.length > 0) {
                        var performativeHead = r.result;
                        while (performativeHead.functor.name == "#list" ||
                            performativeHead.functor.name == "#and") {
                            if (performativeHead.attributes.length > 0 &&
                                performativeHead.attributes[0] instanceof TermTermAttribute) {
                                performativeHead = performativeHead.attributes[0].term;
                            }
                            else {
                                break;
                            }
                        }
                        // performative
                        if (performativeHead.functor.is_a_string("performative")) {
                            if (compiled.listenerVariable != performativeHead.attributes[0]) {
                                var b2 = new Bindings();
                                b2.l.push([compiled.listenerVariable, performativeHead.attributes[0]]);
                                r.result = r.result.applyBindings(b2);
                            }
                        }
                    }
                    if (this.semanticallyCorrect(r.result, context)) {
                        if (r.priorities[0] > bestPriorityOfFirstRule)
                            bestPriorityOfFirstRule = r.priorities[0];
                        results.push(r);
                    }
                    else {
                        semanticalErrors.push(r);
                        // for(let e of compiled.lastDerefErrors) derefErrors.push(e);
                    }
                }
            }
            if (results.length == 0) {
                for (var sort in this.compiledRules) {
                    for (var _a = 0, _b = this.compiledRules[sort].lastDerefErrors; _a < _b.length; _a++) {
                        var e = _b[_a];
                        derefErrors.push(e);
                    }
                }
            }
        }
        else {
            // we don't have a compiled tree, just use the rules...
            for (var _c = 0, _d = this.rules; _c < _d.length; _c++) {
                var rawRule = _d[_c];
                if (rawRule.priority <= bestPriorityOfFirstRule)
                    continue;
                if (!rawRule.head.functor.is_a(s))
                    continue;
                var rule = rawRule.clone();
                var results2 = rule.parse(tokens2, true, context, this, AI);
                if (results2 != null && results2.length > 0) {
                    for (var _e = 0, results2_2 = results2; _e < results2_2.length; _e++) {
                        var r = results2_2[_e];
                        r.result = this.resolveLists(r.result);
                        // properly resolve the "listener" variable:
                        if (s.name == "performative" && r.result.attributes.length > 0) {
                            if (compiled.listenerVariable != r.result.attributes[0]) {
                                var b2 = new Bindings();
                                b2.l.push([compiled.listenerVariable, r.result.attributes[0]]);
                                r.result = r.result.applyBindings(b2);
                            }
                        }
                        if (this.semanticallyCorrect(r.result, context)) {
                            if (r.priorities[0] > bestPriorityOfFirstRule)
                                bestPriorityOfFirstRule = r.priorities[0];
                            results.push(r);
                        }
                        else {
                            semanticalErrors.push(r);
                            for (var _f = 0, _g = compiled.lastDerefErrors; _f < _g.length; _f++) {
                                var e = _g[_f];
                                derefErrors.push(e);
                            }
                        }
                    }
                }
                else {
                    for (var _h = 0, _j = rule.lastDerefErrors; _h < _j.length; _h++) {
                        var e = _j[_h];
                        derefErrors.push(e);
                    }
                }
            }
        }
        /*
                // resolve the #list constructs, and ensure parses are semantically sound:
                let results2:NLParseRecord[] = [];
                for(let result of results) {
                    result.result = this.resolveLists(result.result);
                    if (this.semanticallyCorrect(result.result, context)) results2.push(result);
                }
        */
        if (results.length == 0) {
            // record why couldn't we parse the sentence:
            if (semanticalErrors.length > 0) {
                // parse error was due to a semantical error!
                // only if we cannot parse the sentence in any other way, we return the parses with semantic errors.
                // However, we still report the deref errors just in case:
                this.error_semantic = semanticalErrors;
                if (derefErrors.length > 0) {
                    this.error_deref = derefErrors;
                }
            }
            else if (this.posParser.unrecognizedTokens.length > 0) {
                // parse error was due to unrecognized words:
                this.error_unrecognizedTokens = this.posParser.unrecognizedTokens;
            }
            else if (derefErrors.length > 0) {
                // parse error was due to a deref error!
                this.error_deref = derefErrors;
            }
            else {
                // just grammatically not correct:
                this.error_grammatical = true;
            }
            this.removeDuplicateDerefErrors();
        }
        // for debugging purposes:
        //		for(let r of results) {
        //			console.log("parse: " + r.ruleNames + ", " + r.priorities);
        //		}
        return results;
    };
    NLParser.prototype.removeDuplicateDerefErrors = function () {
        var l = [];
        for (var _i = 0, _a = this.error_deref; _i < _a.length; _i++) {
            var e = _a[_i];
            var found = false;
            for (var _b = 0, l_1 = l; _b < l_1.length; _b++) {
                var e2 = l_1[_b];
                if (e.equals(e2)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                l.push(e);
            }
        }
        // console.log("removeDuplicateDerefErrors: from " + this.error_deref.length + " to " + l.length);
        this.error_deref = l;
    };
    NLParser.prototype.chooseHighestPriorityParse = function (parses) {
        var bestParse = null;
        for (var _i = 0, parses_1 = parses; _i < parses_1.length; _i++) {
            var parse = parses_1[_i];
            if (bestParse == null) {
                bestParse = parse;
            }
            else {
                if (parse.higherPriorityThan(bestParse) == 1) {
                    bestParse = parse;
                }
            }
        }
        return bestParse;
    };
    NLParser.prototype.chooseHighestPriorityParseWithListener = function (parses, listener) {
        var bestParse = null;
        for (var _i = 0, parses_2 = parses; _i < parses_2.length; _i++) {
            var parse = parses_2[_i];
            var result = this.unifyListener(parse.result, listener);
            if (result != null) {
                var unifiedParse = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities);
                unifiedParse.result = result;
                if (bestParse == null) {
                    bestParse = unifiedParse;
                }
                else {
                    if (unifiedParse.higherPriorityThan(bestParse) == 1) {
                        bestParse = unifiedParse;
                    }
                }
            }
        }
        return bestParse;
    };
    // If "parse" is a performative, unifies the first attribute (which should be the "LISTENER"), with listener
    NLParser.prototype.unifyListener = function (parse, listener) {
        if (parse.functor.is_a(this.o.getSort("performative"))) {
            // create a pattern for unification:
            if (parse.attributes.length > 0) {
                var bindings = new Bindings();
                if (Term.unifyAttribute(new ConstantTermAttribute(listener, this.o.getSort("#id")), parse.attributes[0], OCCURS_CHECK, bindings)) {
                    return parse.applyBindings(bindings);
                }
                else {
                    console.warn("NLParser.unifyListener: cannot unify listener for parse " + parse);
                    return null;
                }
            }
            else {
                console.warn("NLParser.unifyListener: cannot unify listener for parse " + parse);
                return null;
            }
            // let pattern:Term = new Term(parse.functor, []);
            // pattern.addAttribute(new ConstantTermAttribute(listener, this.o.getSort("#id")));
            // for(let i:number = 1;i<parse.attributes.length;i++) {
            // 	pattern.addAttribute(new VariableTermAttribute(this.o.getSort("any"), null));
            // }
            // let bindings:Bindings = new Bindings();
            // if (parse.unify(pattern, OCCURS_CHECK, bindings)) {
            // 	return parse.applyBindings(bindings);
            // } else {
            // 	console.warn("NLParser.unifyListener: parse " + parse + " does not unify with pattern " + pattern);
            // 	return null;
            // }
        }
        else if (parse.functor.name == "#list") {
            var result = null;
            // we go through them in reverse, since this function reverses their order:
            for (var _i = 0, _a = Term.elementsInList(parse, "#list").reverse(); _i < _a.length; _i++) {
                var perf = _a[_i];
                if (!(perf instanceof TermTermAttribute))
                    return null;
                var perf2 = this.unifyListener(perf.term, listener);
                if (perf2 != null) {
                    if (result == null) {
                        result = perf2;
                    }
                    else {
                        result = new Term(this.o.getSort("#list"), [new TermTermAttribute(perf2),
                            new TermTermAttribute(result)]);
                    }
                }
            }
            return result;
        }
        return parse;
    };
    // This function is used to filter out parses that do not make sense from a semantic point of view
    // for example: space.at(X, Y) only makes sense if Y is a location
    NLParser.prototype.semanticallyCorrect = function (parse, context) {
        return this.semanticallyCorrectInternal(parse, context, []);
    };
    NLParser.prototype.semanticallyCorrectInternal = function (parse, context, closed) {
        if (closed.indexOf(parse) != -1)
            return true;
        closed.push(parse);
        if (parse.functor.is_a(this.o.getSort("time.at"))) {
            if (parse.attributes[1].sort.is_a(this.o.getSort("number")))
                return true;
            console.log("semanticallyCorrect: fail! " + parse);
            return false;
        }
        if (parse.functor.is_a(this.o.getSort("space.at"))) {
            if (parse.attributes[1] instanceof ConstantTermAttribute) {
                var e = context.findByID(parse.attributes[1].value);
                if (e != null) {
                    if (e.sortMatch(this.o.getSort("space.location")))
                        return true;
                    if (e.sortMatch(this.o.getSort("container")))
                        return true;
                    return true;
                }
                else {
                    // we don't know, so, for now assume it's fine
                    return true;
                }
            }
            else if (parse.attributes[1] instanceof VariableTermAttribute) {
                if (parse.attributes[1].sort.is_a(this.o.getSort("space.location")) ||
                    this.o.getSort("space.location").is_a(parse.attributes[1].sort))
                    return true;
            }
            console.log("semanticallyCorrect: fail! " + parse);
            return false;
        }
        for (var _i = 0, _a = parse.attributes; _i < _a.length; _i++) {
            var ta = _a[_i];
            if (ta instanceof TermTermAttribute) {
                if (!this.semanticallyCorrectInternal(ta.term, context, closed))
                    return false;
            }
        }
        if (parse.functor.is_a(this.o.getSort("perf.inform.answer"))) {
            if (context.expectingAnswerToQuestion_stack.length > 0)
                return true;
            if (context.expectingYes &&
                parse.attributes.length >= 2 &&
                (parse.attributes[1] instanceof ConstantTermAttribute) &&
                parse.attributes[1].value == "yes")
                return true;
            if (context.expectingConfirmationToRequest_stack.length > 0 &&
                parse.attributes.length >= 2 &&
                (parse.attributes[1] instanceof ConstantTermAttribute) &&
                (parse.attributes[1].value == "yes" ||
                    parse.attributes[1].value == "no"))
                return true;
            // check if the previous sentence was a parse error with a problem in dereference,
            // and convert this to a 'perf.rephrase.entity':
            var lastPerformative = context.lastPerformativeBy(context.speaker);
            if (lastPerformative != null && lastPerformative.parse == null &&
                lastPerformative.derefErrors != null && lastPerformative.derefErrors.length > 0) {
                parse.functor = this.o.getSort("perf.rephrase.entity");
                return true;
            }
            return false;
        }
        if (parse.functor.is_a(this.o.getSort("perf.callattention"))) {
            if (parse.attributes.length >= 1 &&
                (parse.attributes[0] instanceof ConstantTermAttribute)) {
                var target = parse.attributes[0].value;
                // call attention cannot be to oneself!
                if (target == context.speaker)
                    return false;
            }
        }
        if (parse.functor.is_a(this.o.getSort("performative")) &&
            parse.attributes.length >= 1 &&
            parse.attributes[0] instanceof ConstantTermAttribute) {
            var target = parse.attributes[0].value;
            // we should really be only talking to the robots:
            if (this.talkingTargets != null && this.talkingTargets.indexOf(target) == -1) {
                console.log("semanticallyCorrectInternal: " + target + " is not in talkingTargets");
                return false;
            }
        }
        return true;
    };
    NLParser.resolveCons = function (parse, o) {
        if (parse.functor.name == "#cons" &&
            parse.attributes.length > 0 &&
            parse.attributes[0] instanceof ConstantTermAttribute) {
            var sortName = parse.attributes[0].value;
            if (sortName[0] == '~') {
                // negated sort! insert '#not'
                var sort = o.getSort(sortName.substring(1));
                if (sort != null) {
                    var tmp = new Term(sort, parse.attributes.slice(1));
                    parse.functor = o.getSort("#not");
                    parse.attributes = [new TermTermAttribute(tmp)];
                }
            }
            else {
                var sort = o.getSort(sortName);
                if (sort != null) {
                    parse.functor = sort;
                    parse.attributes.splice(0, 1);
                }
            }
        }
        for (var i = 0; i < parse.attributes.length; i++) {
            if (parse.attributes[i] instanceof TermTermAttribute) {
                NLParser.resolveCons(parse.attributes[i].term, o);
                parse.attributes[i].sort = parse.attributes[i].term.functor;
            }
        }
    };
    NLParser.prototype.resolveLists = function (parse) {
        //		console.log("resolveLists: " + parse);
        return this.resolveListsInternal(parse, []);
    };
    NLParser.prototype.resolveListsInternal = function (parse, alreadyProcessed) {
        if (alreadyProcessed.indexOf(parse) != -1)
            return parse;
        alreadyProcessed.push(parse);
        // start by resolving the inner #lists
        //		console.log("resolveLists: " + parse + " (" + alreadyProcessed.length + ")");
        for (var i = 0; i < parse.attributes.length; i++) {
            if (parse.attributes[i] instanceof TermTermAttribute) {
                parse.attributes[i].term = this.resolveListsInternal(parse.attributes[i].term, alreadyProcessed);
            }
        }
        //		console.log("resolveLists (outer): " + parse + " (" + alreadyProcessed.length + ")");
        // now resolve the outer lists:
        for (var ii = 0; ii < parse.attributes.length; ii++) {
            if (parse.attributes[ii] instanceof TermTermAttribute) {
                var childTerm = parse.attributes[ii].term;
                if (childTerm.functor.name == "#list" && parse.functor.name != "#list") {
                    var l = Term.elementsInList(childTerm, "#list");
                    var output = null;
                    //					console.log("resolveListsInternal (elements in list): " + l);
                    //					console.log("resolveListsInternal (before resolving the list): " + parse);
                    for (var j = l.length - 1; j >= 0; j--) {
                        if (output == null) {
                            output = this.cloneTermReplacingIthAttribute(parse, ii, l[j]);
                        }
                        else {
                            output = new Term(this.o.getSort("#list"), [new TermTermAttribute(this.cloneTermReplacingIthAttribute(parse, ii, l[j])),
                                new TermTermAttribute(output)]);
                        }
                    }
                    //					console.log("resolveListsInternal (after resolving the list): " + output);
                    // call the function again, just in case there are more lists:
                    //					console.log("resolveLists recursive call with: " + output);
                    return this.resolveListsInternal(output, alreadyProcessed);
                    //					return output;
                }
            }
        }
        return parse;
    };
    NLParser.constructList = function (elements, listFunctor) {
        var result = null;
        for (var _i = 0, elements_1 = elements; _i < elements_1.length; _i++) {
            var e = elements_1[_i];
            if (result == null) {
                result = e;
            }
            else {
                result = new TermTermAttribute(new Term(listFunctor, [e, result]));
            }
            //			console.log("after adding '" + e + "' the list is now: " + result);
        }
        return result;
    };
    NLParser.termsInList = function (list, listFunctor) {
        var output = [];
        for (var _i = 0, _a = Term.elementsInList(list, listFunctor); _i < _a.length; _i++) {
            var element = _a[_i];
            if (element instanceof TermTermAttribute) {
                output.push(element.term);
            }
        }
        return output;
    };
    NLParser.prototype.cloneTermReplacingIthAttribute = function (term, i, replacement) {
        var map = [];
        var output = new Term(term.functor, []);
        for (var j = 0; j < term.attributes.length; j++) {
            if (j == i) {
                output.addAttribute(replacement);
            }
            else {
                if (term.attributes[j] instanceof TermTermAttribute) {
                    output.addAttribute(new TermTermAttribute(term.attributes[j].term.cloneKeepingVariables(map)));
                }
                else {
                    output.addAttribute(term.attributes[j]);
                }
            }
        }
        return output;
    };
    return NLParser;
}());
