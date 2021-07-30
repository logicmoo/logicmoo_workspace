var NLPATTERN_NONE = 0;
var NLPATTERN_STRING = 1;
var NLPATTERN_SEQUENCE = 2;
var NLPATTERN_ALTERNATIVE = 3;
var NLPATTERN_OPTIONAL = 4;
var NLPATTERN_REPEAT = 5;
var NLPATTERN_POS = 6;
var NLPATTERN_PATTERN = 7;
var NLPATTERN_FUNCTION = 8;
var NLPattern = /** @class */ (function () {
    function NLPattern(t) {
        this.type = NLPATTERN_NONE;
        this.string = null;
        this.term = null; // this is used for POS, recursive patterns and special functions
        this.children = null; // this is used for sequence. alternative, optional or repeat
        this.lastDerefErrorType = 0;
        this.type = t;
    }
    NLPattern.prototype.parse = function (parse, context, rule, parser, AI) {
        switch (this.type) {
            case NLPATTERN_SEQUENCE:
                var parses2 = [parse];
                for (var _i = 0, _a = this.children; _i < _a.length; _i++) {
                    var pattern = _a[_i];
                    var newParses_1 = [];
                    for (var _b = 0, parses2_1 = parses2; _b < parses2_1.length; _b++) {
                        var parse2 = parses2_1[_b];
                        var parses3 = pattern.parse(parse2, context, rule, parser, AI);
                        if (parses3 != null)
                            newParses_1 = newParses_1.concat(parses3);
                    }
                    parses2 = newParses_1;
                    if (parses2.length == 0)
                        return null;
                }
                return parses2;
            case NLPATTERN_ALTERNATIVE:
                var parses_a = [];
                //console.log("alternative with " + this.children.length + " children");
                for (var _c = 0, _d = this.children; _c < _d.length; _c++) {
                    var pattern = _d[_c];
                    var parses2_a = pattern.parse(parse, context, rule, parser, AI);
                    //console.log("    " + parses2_a.length + " parses");
                    if (parses2_a != null)
                        parses_a = parses_a.concat(parses2_a);
                }
                if (parses_a.length == 0)
                    return null;
                return parses_a;
            case NLPATTERN_OPTIONAL:
                // concat the current parse (which ignores the optional part), with the pares resulting from forcing it
                var parses = this.children[0].parse(parse, context, rule, parser, AI);
                //				console.log("Results of optional: " + parses);
                if (parses == null)
                    return [parse];
                return [parse].concat(parses);
            case NLPATTERN_REPEAT:
                var parses2_r = [parse]; // 0 repetitions is also allowed, that's why
                // we initialize the list of results with the current parse.
                var parses2_r_last = [parse];
                var newParses = void 0;
                do {
                    newParses = [];
                    for (var _e = 0, parses2_r_last_1 = parses2_r_last; _e < parses2_r_last_1.length; _e++) {
                        var parse2_r = parses2_r_last_1[_e];
                        var parses3_r = this.children[0].parse(parse2_r, context, rule, parser, AI);
                        if (parses3_r != null)
                            newParses = newParses.concat(parses3_r);
                    }
                    parses2_r_last = newParses;
                    parses2_r = parses2_r.concat(newParses);
                } while (newParses.length > 0);
                return parses2_r;
            case NLPATTERN_STRING:
                return this.parseString(parse, context, rule, parser, AI);
            case NLPATTERN_POS:
                return this.parsePOS(parse, context, rule, parser, AI);
            case NLPATTERN_PATTERN:
                return this.parsePattern(parse, context, rule, parser, AI);
            case NLPATTERN_FUNCTION:
                return this.parseFunction(parse, context, rule, parser, AI);
            default:
                console.error("NLPattern.parse: pattern type not supported " + this.type);
                return null;
        }
    };
    NLPattern.prototype.parseString = function (parse, context, rule, parser, AI) {
        if (parse.nextTokens == null)
            return null;
        var parses = [];
        for (var _i = 0, _a = parse.nextTokens; _i < _a.length; _i++) {
            var nextToken = _a[_i];
            if (nextToken.token == null) {
                var parses2 = this.parseString(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities), context, rule, parser, AI);
                if (parses2 != null)
                    parses = parses.concat(parses2);
            }
            else if (nextToken.token == this.string) {
                // match!
                parses.push(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities));
            }
        }
        if (parses.length == 0)
            return null;
        return parses;
    };
    NLPattern.prototype.parsePOS = function (parse, context, rule, parser, AI) {
        var parses = [];
        var term2 = this.term.applyBindings(parse.bindings);
        //				console.log("Matching POS, before: " + this.term.toString() + "\n  bindings: " + parse.bindings + "\n  Matching POS, after: " + term2.toString());
        for (var _i = 0, _a = parse.nextTokens; _i < _a.length; _i++) {
            var nextToken = _a[_i];
            if (nextToken.token == null) {
                var parses2 = this.parsePOS(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities), context, rule, parser, AI);
                if (parses2 != null)
                    parses = parses.concat(parses2);
            }
            else {
                //						console.log("Matching POS "+term2.toString()+" with: " + nextToken.token);
                for (var _b = 0, _c = nextToken.POS; _b < _c.length; _b++) {
                    var POS = _c[_b];
                    var bindings = new Bindings();
                    if (POS.term.unify(term2, OCCURS_CHECK, bindings)) {
                        var newParse = new NLParseRecord(nextToken.next, parse.previousPOS.concat([POS]), parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities);
                        //								console.log("POS match with: " + POS.term + "\nBindings: " + newParse.bindings);
                        parses.push(newParse);
                    }
                }
            }
        }
        if (parses.length == 0)
            return null;
        return parses;
    };
    NLPattern.prototype.parsePattern = function (parse, context, rule, parser, AI) {
        var parses_p = [];
        var term2 = this.term.applyBindings(parse.bindings);
        //				console.log("Matching pattern: " + term2.toString());
        var compiled = parser.compiledRules[term2.functor.name];
        if (compiled != null) {
            // if we have a compiled tree, use it!
            //			console.log("NLPattern.parsePattern: Found a compiled tree for " + term2.functor.name + " ...");
            var results = compiled.parseMatchingWithTerm(new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities), false, context, parser, AI, term2);
            for (var _i = 0, results_1 = results; _i < results_1.length; _i++) {
                var pr = results_1[_i];
                var bindings2 = new Bindings();
                if (!pr.result.unify(term2, OCCURS_CHECK, bindings2)) {
                    console.error("NLPattern.parsePattern: something went wrong when parsing pattern " + term2.toString() + "\n  It does not unify with: " + pr.result);
                    return null;
                }
                // we continue from "pr", but using the bindings from "parse", since the bindings
                // generated during the parsing of the sub-pattern are not relevant
                // console.log("ruleNames for " + term2.functor.name + ": " + parse.ruleNames);
                // console.log("    concatenated to: " + pr.ruleNames);
                var pr2 = new NLParseRecord(pr.nextTokens, pr.previousPOS, parse.bindings.concat(bindings2), parse.derefs, pr.ruleNames.concat(parse.ruleNames), pr.priorities.concat(parse.priorities));
                pr2.result = pr.result;
                parses_p.push(pr2);
            }
        }
        else {
            console.log("NLPattern.parsePattern: Using the raw rules for " + term2.functor.name + " ...");
            for (var _a = 0, _b = parser.rules; _a < _b.length; _a++) {
                var rawRule2 = _b[_a];
                var rule2 = rawRule2.clone();
                var bindings = new Bindings();
                if (rule2.head.unify(term2, OCCURS_CHECK, bindings)) {
                    // rule to consider!!
                    //						console.log("  considering rule with head: " + rule2.head.toString() + "\n  new bindings: " + bindings);
                    var results = rule2.parseWithBindings(new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities), false, context, parser, AI);
                    for (var _c = 0, results_2 = results; _c < results_2.length; _c++) {
                        var pr = results_2[_c];
                        //							console.log("Pattern matched successfully with result: " + t.toString());
                        var bindings2 = new Bindings();
                        if (!pr.result.unify(term2, OCCURS_CHECK, bindings2)) {
                            console.error("NLPattern.parsePattern: something went wrong when parsing pattern " + term2.toString() + "\n  It does not unify with: " + pr.result);
                            return null;
                        }
                        // we continue from "pr", but using the bdingins from "parse", since the bindings
                        // generated during the parsing of the sub-pattern are not relevant
                        parses_p.push(new NLParseRecord(pr.nextTokens, pr.previousPOS, parse.bindings.concat(bindings2), parse.derefs, pr.ruleNames.concat(parse.ruleNames), pr.priorities.concat(parse.priorities)));
                    }
                    //					} else {
                    //						console.log("  head: " + rule2.head + "\n. did not unify with: " + term2);
                }
            }
        }
        if (parses_p.length == 0)
            return null;
        //				console.log("Result of pattern: " + parses_p);
        return parses_p;
    };
    NLPattern.prototype.parseFunction = function (parse, context, rule, parser, AI) {
        if (this.term.functor.name == "#derefFromContext") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_derefFromContext(parse, term2.attributes[0], term2.attributes[1], context, rule.listenerVariable, parser.o, parser.posParser, AI);
            if (nlprl == null) {
                var nlper = new NLDerefErrorRecord(context.lastDerefErrorType, parse.tokensLeftToParse(), parse.previousPOS);
                nlper.derefFromContextError = term2.attributes[0];
                rule.lastDerefErrors.push(nlper);
            }
            return nlprl;
        }
        else if (this.term.functor.name == "#derefUniversal") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_derefUniversal(parse, term2.attributes[0], term2.attributes[1], term2.attributes[2], parser.o);
            if (nlprl == null) {
                var nlper = new NLDerefErrorRecord(this.lastDerefErrorType, parse.tokensLeftToParse(), parse.previousPOS);
                nlper.derefUniversalError = term2.attributes[0];
                rule.lastDerefErrors.push(nlper);
            }
            return nlprl;
        }
        else if (this.term.functor.name == "#derefHypothetical") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_derefHypothetical(parse, term2.attributes[0], term2.attributes[1], term2.attributes[2], context, rule.listenerVariable, parser.o, parser.posParser, AI);
            if (nlprl == null) {
                var nlper = new NLDerefErrorRecord(this.lastDerefErrorType, parse.tokensLeftToParse(), parse.previousPOS);
                nlper.derefHypotheticalError = term2.attributes[0];
                rule.lastDerefErrors.push(nlper);
            }
            return nlprl;
        }
        else if (this.term.functor.name == "#derefQuery") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_derefQuery(parse, term2.attributes[0], term2.attributes[1], term2.attributes[2], context, rule.listenerVariable, parser.o, parser.posParser, AI);
            if (nlprl == null) {
                var nlper = new NLDerefErrorRecord(this.lastDerefErrorType, parse.tokensLeftToParse(), parse.previousPOS);
                nlper.derefQueryError = term2.attributes[0];
                rule.lastDerefErrors.push(nlper);
            }
            return nlprl;
        }
        else if (this.term.functor.name == "#symbolToSort") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_symbolToSort(parse, term2.attributes[0], term2.attributes[1], parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#subsumes") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_subsumes(parse, term2.attributes[0], term2.attributes[1], parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#doesnotsubsume") {
            var term2 = this.term.applyBindings(parse.bindings);
            var listenerBindings = new Bindings();
            listenerBindings.l.push([rule.listenerVariable, new ConstantTermAttribute(context.ai.selfID, parser.o.getSort("#id"))]);
            term2 = term2.applyBindings(listenerBindings);
            var nlprl = this.specialfunction_doesnotsubsume(parse, term2.attributes[0], term2.attributes[1], parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#equal") {
            var term2 = this.term.applyBindings(parse.bindings);
            var listenerBindings = new Bindings();
            listenerBindings.l.push([rule.listenerVariable, new ConstantTermAttribute(context.ai.selfID, parser.o.getSort("#id"))]);
            term2 = term2.applyBindings(listenerBindings);
            var nlprl = this.specialfunction_equal(parse, term2.attributes[0], term2.attributes[1], parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#notequal") {
            var term2 = this.term.applyBindings(parse.bindings);
            var listenerBindings = new Bindings();
            listenerBindings.l.push([rule.listenerVariable, new ConstantTermAttribute(context.ai.selfID, parser.o.getSort("#id"))]);
            term2 = term2.applyBindings(listenerBindings);
            var nlprl = this.specialfunction_notequal(parse, term2.attributes[0], term2.attributes[1], parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#sortParent") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_sortParent(parse, term2.attributes[0], term2.attributes[1], parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#concatenateSymbols") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_concatenateSymbols(parse, term2.attributes, parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#completeVerbArgumentsFromContext" && this.term.attributes.length == 2) {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_completeVerbArgumentsFromContext(parse, term2.attributes[0], term2.attributes[1], context, parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#changeConstantSort") {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_changeConstantSort(parse, term2.attributes, parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#token" && this.term.attributes.length == 1) {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_token(parse, term2.attributes[0], parser.o);
            return nlprl;
        }
        else if (this.term.functor.name == "#findLastNoun" && this.term.attributes.length == 1) {
            var term2 = this.term.applyBindings(parse.bindings);
            var nlprl = this.specialfunction_findLastNoun(parse, term2.attributes[0], context, parser.o);
            return nlprl;
        }
        else {
            console.error("NLPattern.parse: special function " + this.term.functor + " not supported!");
            return null;
        }
    };
    NLPattern.prototype.specialfunction_derefFromContext = function (parse, clause, output, context, listenerVariable, o, pos, AI) {
        this.lastDerefErrorType = 0;
        if (!(clause instanceof TermTermAttribute)) {
            console.log("specialfunction_derefFromContext: trying to dereference a clause that is not a term! " + clause);
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        var result_l = context.deref(clause.term, listenerVariable, parse, o, pos, AI);
        //		console.log("specialfunction_derefFromContext result: " + result);
        if (result_l == null || result_l.length == 0) {
            this.lastDerefErrorType = context.lastDerefErrorType;
            return null;
        }
        var result = result_l[0];
        for (var i = 1; i < result_l.length; i++) {
            var tmp = new Term(o.getSort("#list"), [result_l[i], result]);
            result = new TermTermAttribute(tmp);
        }
        var bindings2 = new Bindings();
        if (Term.unifyAttribute(output, result, true, bindings2)) {
            var bindings3 = parse.bindings.concat(bindings2);
            var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, bindings3, parse.derefs.concat([new Term(o.getSort("#derefFromContext"), [clause, output.applyBindings(bindings3)])]), parse.ruleNames, parse.priorities);
            return [parse2];
        }
        else {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
    };
    NLPattern.prototype.specialfunction_derefUniversal = function (parse, clause, outputVariable, output, o) {
        this.lastDerefErrorType = 0;
        var nounSort = o.getSort("noun");
        var properNounSort = o.getSort("proper-noun");
        var personalPronounSort = o.getSort("personal-pronoun");
        var determinerSort = o.getSort("determiner");
        var adjectiveSort = o.getSort("adjective");
        var result = null;
        if (!(clause instanceof TermTermAttribute)) {
            console.log("specialfunction_derefUniversal: trying to dereference a clause that is not a term! " + clause);
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        var all_found = false;
        for (var _i = 0, _a = Term.elementsInList(clause.term, "#and"); _i < _a.length; _i++) {
            var tmp = _a[_i];
            if (tmp instanceof TermTermAttribute) {
                var tmp2 = tmp.term;
                if (tmp2.functor.name == "all") {
                    all_found = true;
                }
                else if (tmp2.functor.is_a(determinerSort)) {
                    // if a determiner is used that is not "all", then this is not a universal
                    this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                    return null;
                }
                else if (tmp2.functor.is_a(properNounSort)) {
                    // if a proper noun is used, then this is not a universal
                    this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                    return null;
                }
                else if (tmp2.functor.is_a(personalPronounSort)) {
                    // if a personal pronoun is used, then this is not a universal
                    this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                    return null;
                }
                else if (tmp2.functor.is_a(nounSort) &&
                    tmp2.attributes[0] instanceof ConstantTermAttribute) {
                    var resultTmp = new Term(o.getSort(tmp2.attributes[0].value), [outputVariable]);
                    if (result == null) {
                        result = resultTmp;
                    }
                    else {
                        result = new Term(o.getSort("#and"), [new TermTermAttribute(resultTmp),
                            new TermTermAttribute(result)]);
                    }
                }
                else if (tmp2.functor.is_a(adjectiveSort) &&
                    tmp2.attributes[1] instanceof ConstantTermAttribute) {
                    var propertySort = o.getSort(tmp2.attributes[1].value);
                    var resultTmp = null;
                    if (propertySort.is_a(o.getSort("property-with-value"))) {
                        resultTmp = new Term(propertySort, [outputVariable, new ConstantTermAttribute(propertySort.name, propertySort)]);
                    }
                    else {
                        resultTmp = new Term(propertySort, [outputVariable]);
                    }
                    if (result == null) {
                        result = resultTmp;
                    }
                    else {
                        result = new Term(o.getSort("#and"), [new TermTermAttribute(resultTmp),
                            new TermTermAttribute(result)]);
                    }
                }
                else {
                    console.log("specialfunction_derefUniversal: clause contains an element that is not yet supported! " + tmp2);
                }
            }
            else {
                console.log("specialfunction_derefUniversal: clause contains an element that is not a term!");
            }
        }
        if (!all_found) {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        //		console.log("specialfunction_derefUniversal result: " + result);
        var bindings2 = new Bindings();
        var resultAtt = new TermTermAttribute(result);
        if (Term.unifyAttribute(output, resultAtt, true, bindings2)) {
            var bindings3 = parse.bindings.concat(bindings2);
            var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, bindings3, parse.derefs.concat([new Term(o.getSort("#derefUniversal"), [clause, outputVariable.applyBindings(bindings3), output.applyBindings(bindings3)])]), parse.ruleNames, parse.priorities);
            return [parse2];
        }
        else {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
    };
    NLPattern.prototype.specialfunction_derefHypothetical = function (parse, clause, outputVariable, output, context, listenerVariable, o, pos, AI) {
        this.lastDerefErrorType = 0;
        var properNounSort = o.getSort("proper-noun");
        var nounSort = o.getSort("noun");
        var determinerSort = o.getSort("determiner");
        var relationSort = o.getSort("relation");
        var indefiniteArticleSort = o.getSort("indefinite-article");
        //		let adjectiveSort:Sort = o.getSort("adjective");
        var result = null;
        var foundIndefiniteArticle = false;
        var hadName = false; // if there is a name for the new hypothetical, then we need to generate an ID for it
        var outputVariableSort = null;
        if (!(clause instanceof TermTermAttribute)) {
            console.log("specialfunction_derefHypothetical: trying to dereference a clause that is not a term! " + clause);
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        //		console.log("specialfunction_derefHypothetical: " + clause);
        // if we can dereference the expression in the context, then this function should not apply:
        var contextDerefResult = context.deref(clause.term, listenerVariable, parse, o, pos, AI);
        if (contextDerefResult != null && contextDerefResult.length > 0) {
            //			console.log("specialfunction_derefHypothetical: context.deref succeeded! " + contextDerefResult);
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        //		console.log("specialfunction_derefHypothetical: context.deref failed");
        var clauses = Term.elementsInList(clause.term, "#and");
        for (var _i = 0, clauses_1 = clauses; _i < clauses_1.length; _i++) {
            var tmp = clauses_1[_i];
            if (tmp instanceof TermTermAttribute) {
                var tmp2 = tmp.term;
                if (tmp2.functor.is_a(determinerSort)) {
                    if (tmp2.functor.is_a(indefiniteArticleSort)) {
                        foundIndefiniteArticle = true;
                        // an indefinite article is fine
                    }
                    else {
                        // if a determiner is used, then this is not a hypothetical
                        //						console.log("specialfunction_derefHypothetical: a determiner is used, this is not a hypothetical!");
                        this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                        return null;
                    }
                }
            }
        }
        for (var _a = 0, clauses_2 = clauses; _a < clauses_2.length; _a++) {
            var tmp = clauses_2[_a];
            if (tmp instanceof TermTermAttribute) {
                var tmp2 = tmp.term;
                if (tmp2.functor.is_a(properNounSort)) {
                    hadName = true;
                    var resultTmp = new Term(o.getSort("name"), [outputVariable, tmp2.attributes[0]]);
                    if (result == null) {
                        result = resultTmp;
                    }
                    else {
                        result = new Term(o.getSort("#and"), [new TermTermAttribute(resultTmp),
                            new TermTermAttribute(result)]);
                    }
                }
                else if (tmp2.functor.is_a(nounSort) &&
                    tmp2.attributes.length >= 1 &&
                    tmp2.attributes[0] instanceof ConstantTermAttribute) {
                    if (foundIndefiniteArticle) {
                        var resultTmp = new Term(o.getSort((tmp2.attributes[0]).value), [outputVariable]);
                        outputVariableSort = resultTmp.functor;
                        if (result == null) {
                            result = resultTmp;
                        }
                        else {
                            result = new Term(o.getSort("#and"), [new TermTermAttribute(resultTmp),
                                new TermTermAttribute(result)]);
                        }
                    }
                    else {
                        console.log("specialfunction_derefHypothetical: noun found without an indefinite article!");
                        this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                        return null;
                    }
                }
                else if (tmp2.functor.is_a(relationSort)) {
                    var resultTmp = new Term(relationSort, [outputVariable, tmp2.attributes[0]]);
                    if (result == null) {
                        result = resultTmp;
                    }
                    else {
                        result = new Term(o.getSort("#and"), [new TermTermAttribute(tmp2),
                            new TermTermAttribute(result)]);
                    }
                }
                else if (tmp2.functor.is_a(determinerSort)) {
                    // ignore, handled before
                }
                else {
                    console.log("specialfunction_derefHypothetical: clause contains an element that is not yet supported! " + tmp2);
                }
            }
            else {
                console.log("specialfunction_derefHypothetical: clause contains an element that is not a term!");
            }
        }
        if (result == null) {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        //		console.log("specialfunction_derefHypothetical result: " + result);
        var bindings2 = new Bindings();
        if (hadName) {
            var outputVariableID = new ConstantTermAttribute(context.newHypotheticalID(), o.getSort("#id"));
            //		console.log("outputVariable: " + outputVariable);
            //		console.log("outputVariableID: " + outputVariableID);
            if (!Term.unifyAttribute(outputVariable, outputVariableID, true, bindings2))
                return null;
        }
        else if (outputVariableSort != null) {
            var outputVariableID = new ConstantTermAttribute(outputVariableSort.name, outputVariableSort);
            //		console.log("outputVariable: " + outputVariable);
            //		console.log("outputVariableID: " + outputVariableID);
            if (!Term.unifyAttribute(outputVariable, outputVariableID, true, bindings2))
                return null;
        }
        var resultAtt = new TermTermAttribute(result);
        if (!Term.unifyAttribute(output, resultAtt, true, bindings2))
            return null;
        var bindings3 = parse.bindings.concat(bindings2);
        var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, bindings3, parse.derefs.concat([new Term(o.getSort("#derefHypothetical"), [clause, outputVariable.applyBindings(bindings3), output.applyBindings(bindings3)])]), parse.ruleNames, parse.priorities);
        return [parse2];
    };
    /*
    For example, for the sort "white", it will return the sort "color". But if we pass the sort "color",
    if should return the same "color" sort.
    */
    NLPattern.prototype.getPropertyWithValueFunctorSort = function (sort, o) {
        var pwv = o.getSort("property-with-value");
        if (sort.parents.indexOf(pwv) >= -0)
            return sort;
        for (var _i = 0, _a = sort.parents; _i < _a.length; _i++) {
            var parent_1 = _a[_i];
            if (parent_1.is_a(pwv))
                return parent_1;
        }
        return sort;
    };
    /*
    Examples:
        #and(V0:determiner.my(V1:'name'[#id], V2:[singular]), V3:noun(V1, V2)		->		name(context.selfID, queryVariable)
        #and(V0:determiner.your(V1:'name'[#id], V2:[singular]), V3:noun(V1, V2))		->		name(listenerVariable, queryVariable)
        #and(V0:verb.own(V1:'ship'[#id], V2:'name'[#id]), V3:#and(V4:noun(V2, V5:[singular]), V6:#and(V7:the(V1, V8:[singular]), V9:noun(V1, V8))))
                -> if there is a "verb.own", separate in two:
                        owner V1) V7:the(V1:'ship'[#id], V8:[singular]), V9:noun(V1, V8)
                        ownee V2) V4:noun(V2:'name'[#id], V5:[singular])
                    -> owner should just be a single noun, potentially with adjectives
                    -> then try to deref "owner" -> OWNER
                -> the result will be name(OWNER, queryVariable) [+ whatever adjectives applied to the queryVariable]

        Sentences with "this/these/that/those" should not be queries, and thus this function will return null for them
    */
    NLPattern.prototype.specialfunction_derefQuery = function (parse, clause, queryVariable, query, context, listenerVariable, o, pos, AI) {
        this.lastDerefErrorType = 0;
        if (!(clause instanceof TermTermAttribute)) {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        NLParser.resolveCons(clause.term, o);
        clause.sort = clause.term.functor;
        //		console.log("specialfunction_derefQuery: clause = " + clause);
        //		console.log("specialfunction_derefQuery: queryVariable = " + queryVariable);
        //		console.log("specialfunction_derefQuery: query = " + query);
        //		console.log("specialfunction_derefQuery: context.ai.selfID = " + context.ai.selfID);
        var queryTerm = null;
        var terms = Term.elementsInList(clause.term, "#and");
        // console.log("specialfunction_derefQuery: terms = " + terms);
        var myDeterminer = null;
        var ourDeterminer = null;
        var yourDeterminer = null;
        var definiteArticle = null;
        var aDeterminer = null;
        var indefinitePronoun = null;
        var ownsRelation = null;
        var otherRelations = [];
        var nounTerm = null;
        //let properNounTerm:Term = null;	
        var adverbs = [];
        var adjectives = []; // adjectives are filled later, since we neeed queryFunctor
        var negatedAdjectives = []; // adjectives are filled later, since we neeed queryFunctor
        var demonstrativeDeterminer = null;
        var demonstrativePronoun = null;
        var otherTerms = [];
        var elsePresent = false;
        var otherDeterminerPresent = false;
        for (var _i = 0, terms_1 = terms; _i < terms_1.length; _i++) {
            var t = terms_1[_i];
            if (t instanceof TermTermAttribute) {
                if (t.term.functor.is_a(o.getSort("determiner.my"))) {
                    myDeterminer = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("determiner.your"))) {
                    yourDeterminer = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("determiner.our"))) {
                    ourDeterminer = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("determiner.other")) ||
                    t.term.functor.is_a(o.getSort("determiner.another"))) {
                    otherDeterminerPresent = true;
                }
                else if (t.term.functor.is_a(o.getSort("a")) ||
                    t.term.functor.is_a(o.getSort("article.any"))) {
                    aDeterminer = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("definite-article"))) {
                    definiteArticle = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("demonstrative-determiner"))) {
                    demonstrativeDeterminer = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("verb.own"))) {
                    ownsRelation = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("#not")) &&
                    t.term.attributes.length == 1 &&
                    (t.term.attributes[0] instanceof TermTermAttribute) &&
                    (t.term.attributes[0]).term.functor.is_a(o.getSort("verb.own"))) {
                    //ownsRelation = (<TermTermAttribute>t).term;
                    console.error("negated owns relation not yet supported!");
                }
                else if (t.term.functor.is_a(o.getSort("indefinite-pronoun"))) {
                    if (t.term.attributes.length > 0 &&
                        (t.term.attributes[0] instanceof ConstantTermAttribute)) {
                        indefinitePronoun = t.term;
                        if ((t.term.attributes[0]).value == "pronoun.anyone.else") {
                            elsePresent = true;
                        }
                    }
                }
                else if (t.term.functor.is_a(o.getSort("adverb"))) {
                    adverbs.push(t.term);
                }
                else if (t.term.functor.is_a(o.getSort("noun"))) {
                    nounTerm = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("proper-noun"))) {
                    //properNounTerm = (<TermTermAttribute>t).term;
                    // if there is a proper noun, this is probably not a query...
                    this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                    return null;
                }
                else if (t.term.functor.is_a(o.getSort("demonstrative-pronoun"))) {
                    demonstrativePronoun = t.term;
                }
                else if (t.term.functor.is_a(o.getSort("relation"))) {
                    otherRelations.push(t.term);
                }
                else if (t.term.functor.is_a(o.getSort("#not")) &&
                    t.term.attributes.length == 1 &&
                    (t.term.attributes[0] instanceof TermTermAttribute) &&
                    (t.term.attributes[0]).term.functor.is_a(o.getSort("relation"))) {
                    otherRelations.push(t.term);
                }
                else {
                    otherTerms.push(t.term);
                }
            }
        }
        // console.log("#derefQuery.otherRelations: " + otherRelations);
        // console.log("#derefQuery.otherTerms: " + otherTerms);
        var potentialQueryFunctor = null;
        var queryFunctor = null;
        var queryFunctorSort = null;
        var querySubjectID_l = null;
        var queryLocationID = null;
        var TermUsedForQueryLocationID = null;
        var queryTerms = [];
        if (myDeterminer != null) {
            queryFunctor = myDeterminer.attributes[0];
            if (queryFunctor instanceof ConstantTermAttribute) {
                queryFunctorSort = o.getSortSilent(queryFunctor.value);
            }
            querySubjectID_l = [new ConstantTermAttribute(context.speaker, o.getSort("#id"))];
        }
        else if (ourDeterminer != null) {
            queryFunctor = ourDeterminer.attributes[0];
            if (queryFunctor instanceof ConstantTermAttribute) {
                queryFunctorSort = o.getSortSilent(queryFunctor.value);
            }
            querySubjectID_l = [new ConstantTermAttribute(context.ai.selfID, o.getSort("#id")),
                new ConstantTermAttribute(context.speaker, o.getSort("#id"))];
        }
        else if (yourDeterminer != null) {
            queryFunctor = yourDeterminer.attributes[0];
            if (queryFunctor instanceof ConstantTermAttribute) {
                queryFunctorSort = o.getSortSilent(queryFunctor.value);
            }
            querySubjectID_l = [new ConstantTermAttribute(context.ai.selfID, o.getSort("#id"))];
        }
        else if (ownsRelation != null) {
            // case 3: if there is a "verb.own":
            var ownerVariable = ownsRelation.attributes[0];
            // let ownerTerms:TermAttribute[] = [];
            var ownerClause = null;
            queryFunctor = ownsRelation.attributes[1];
            for (var _a = 0, terms_2 = terms; _a < terms_2.length; _a++) {
                var t = terms_2[_a];
                if (t instanceof TermTermAttribute) {
                    var t2 = t.term;
                    if (t2 == ownsRelation)
                        continue;
                    for (var i = 0; i < t2.attributes.length; i++) {
                        if (t2.attributes[i] == ownerVariable) {
                            // ownerTerms.push(t);
                            if (ownerClause == null) {
                                ownerClause = t.term;
                            }
                            else {
                                ownerClause = new Term(o.getSort("#and"), [new TermTermAttribute(ownerClause), t]);
                            }
                        }
                    }
                }
            }
            // let dereffedOwner:TermAttribute[] = context.derefInternal(ownerTerms, listenerVariable, parse, o, pos, AI, false);
            var dereffedOwner = context.deref(ownerClause, listenerVariable, parse, o, pos, AI);
            //			console.log("ownerTerms: " + ownerTerms);
            //			console.log("dereffedOwner: " + dereffedOwner);
            if (dereffedOwner == null) {
                this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                return null;
            }
            if (dereffedOwner.length != 1) {
                this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
                return null;
            }
            querySubjectID_l = [dereffedOwner[0]];
        }
        else if (aDeterminer != null) {
            queryFunctor = aDeterminer.attributes[0];
        }
        else if (demonstrativeDeterminer != null ||
            demonstrativePronoun != null) {
            return null;
        }
        else if (nounTerm != null) {
            potentialQueryFunctor = nounTerm.attributes[0];
            queryFunctorSort = o.getSort(nounTerm.attributes[0].value);
        }
        else if (indefinitePronoun != null) {
            potentialQueryFunctor = indefinitePronoun.attributes[0];
            if ((indefinitePronoun.attributes[0]).value == 'pronoun.anyone' ||
                (indefinitePronoun.attributes[0]).value == 'pronoun.anyone.else' ||
                (indefinitePronoun.attributes[0]).value == 'pronoun.someone') {
                queryFunctorSort = o.getSort("character");
            }
            else if ((indefinitePronoun.attributes[0]).value == 'pronoun.anything' ||
                (indefinitePronoun.attributes[0]).value == 'pronoun.something') {
                queryFunctorSort = o.getSort("any");
            }
        }
        if (queryFunctor != null &&
            queryFunctor instanceof ConstantTermAttribute) {
            queryFunctorSort = o.getSortSilent(queryFunctor.value);
        }
        if (queryFunctor == null && potentialQueryFunctor != null) {
            queryFunctor = potentialQueryFunctor;
        }
        for (var _b = 0, terms_3 = terms; _b < terms_3.length; _b++) {
            var t = terms_3[_b];
            if (t instanceof TermTermAttribute) {
                if (t.term.functor.is_a(o.getSort("adjective"))) {
                    var t2 = t.term;
                    if (t2.attributes[0] == queryFunctor &&
                        t2.attributes[1] instanceof ConstantTermAttribute &&
                        t2.attributes[1].sort.name != "#id") {
                        var adjectiveSort = o.getSort((t2.attributes[1]).value);
                        if (adjectiveSort != null)
                            adjectives.push(adjectiveSort);
                    }
                }
                else if (t.term.functor.is_a(o.getSort("#not")) &&
                    t.term.attributes.length == 1 &&
                    (t.term.attributes[0] instanceof TermTermAttribute) &&
                    (t.term.attributes[0]).term.functor.is_a(o.getSort("adjective"))) {
                    var t2 = (t.term.attributes[0]).term;
                    if (t2.attributes[0] == queryFunctor &&
                        t2.attributes[1] instanceof ConstantTermAttribute) {
                        var adjectiveSort = o.getSort((t2.attributes[1]).value);
                        if (adjectiveSort != null)
                            negatedAdjectives.push(adjectiveSort);
                    }
                }
                else if (t.term.functor.is_a(o.getSort("space.at")) &&
                    queryFunctorSort.is_a(o.getSort("role"))) {
                    queryLocationID = t.term.attributes[1];
                    TermUsedForQueryLocationID = t.term;
                }
                else {
                    //console.log("Not considered: " + t);
                }
            }
        }
        if (definiteArticle != null && negatedAdjectives.length == 0) {
            // Notice the exception for when we have a definite article, but negated articles,
            // this allows parsing expressions like "the non blue key" as queries
            return null;
        }
        var verbOrRelationTerm = null; // this is so that if we have an adverb of time, we know which term to qualify
        if (queryFunctorSort != null) {
            if (queryLocationID == null) {
                if (querySubjectID_l == null ||
                    !queryFunctorSort.is_a(o.getSort("property-with-value"))) {
                    if (queryFunctorSort.name != "any") {
                        verbOrRelationTerm = new TermTermAttribute(new Term(queryFunctorSort, [queryVariable]));
                        queryTerms.push(verbOrRelationTerm);
                    }
                }
                else {
                    // Property-with-value:
                    var property_with_value_functor_sort = this.getPropertyWithValueFunctorSort(queryFunctorSort, o);
                    for (var _c = 0, querySubjectID_l_1 = querySubjectID_l; _c < querySubjectID_l_1.length; _c++) {
                        var querySubjectID = querySubjectID_l_1[_c];
                        verbOrRelationTerm = new TermTermAttribute(new Term(property_with_value_functor_sort, [querySubjectID, queryVariable]));
                        queryTerms.push(verbOrRelationTerm);
                    }
                }
            }
            else {
                if (querySubjectID_l == null) {
                    console.warn("specialfunction_derefQuery: case not considered, querySubjectID == null and queryLocationID != null!");
                    return null;
                }
                else {
                    for (var _d = 0, querySubjectID_l_2 = querySubjectID_l; _d < querySubjectID_l_2.length; _d++) {
                        var querySubjectID = querySubjectID_l_2[_d];
                        verbOrRelationTerm = new TermTermAttribute(new Term(queryFunctorSort, [querySubjectID, queryLocationID, queryVariable]));
                        queryTerms.push(verbOrRelationTerm);
                    }
                    if (otherRelations.indexOf(TermUsedForQueryLocationID) != -1) {
                        otherRelations.splice(otherRelations.indexOf(TermUsedForQueryLocationID), 1);
                    }
                }
            }
        }
        if ((myDeterminer != null || ourDeterminer != null) && queryFunctorSort != null &&
            !queryFunctorSort.is_a(o.getSort("property-with-value"))) {
            for (var _e = 0, querySubjectID_l_3 = querySubjectID_l; _e < querySubjectID_l_3.length; _e++) {
                var querySubjectID = querySubjectID_l_3[_e];
                queryTerms.push(new TermTermAttribute(new Term(o.getSort("verb.own"), [querySubjectID, queryVariable])));
            }
        }
        if (yourDeterminer != null && queryFunctorSort != null &&
            !queryFunctorSort.is_a(o.getSort("property-with-value"))) {
            queryTerms.push(new TermTermAttribute(new Term(o.getSort("verb.own"), [listenerVariable, queryVariable])));
        }
        for (var _f = 0, adjectives_1 = adjectives; _f < adjectives_1.length; _f++) {
            var adjective = adjectives_1[_f];
            if (adjective.is_a(o.getSort("property-with-value"))) {
                var property_with_value_functor_sort = this.getPropertyWithValueFunctorSort(adjective, o);
                if (adjective.is_a(o.getSort("role"))) {
                    queryTerms.push(new TermTermAttribute(new Term(property_with_value_functor_sort, [queryVariable, queryLocationID, new ConstantTermAttribute(adjective.name, adjective)])));
                }
                else {
                    queryTerms.push(new TermTermAttribute(new Term(property_with_value_functor_sort, [queryVariable, new ConstantTermAttribute(adjective.name, adjective)])));
                }
            }
            else {
                queryTerms.push(new TermTermAttribute(new Term(adjective, [queryVariable])));
            }
        }
        for (var _g = 0, negatedAdjectives_1 = negatedAdjectives; _g < negatedAdjectives_1.length; _g++) {
            var adjective = negatedAdjectives_1[_g];
            if (adjective.is_a(o.getSort("property-with-value"))) {
                var property_with_value_functor_sort = this.getPropertyWithValueFunctorSort(adjective, o);
                if (adjective.is_a(o.getSort("role"))) {
                    queryTerms.push(new TermTermAttribute(new Term(o.getSort("#not"), [new TermTermAttribute(new Term(property_with_value_functor_sort, [queryVariable, queryLocationID, new ConstantTermAttribute(adjective.name, adjective)]))])));
                }
                else {
                    queryTerms.push(new TermTermAttribute(new Term(o.getSort("#not"), [new TermTermAttribute(new Term(property_with_value_functor_sort, [queryVariable, new ConstantTermAttribute(adjective.name, adjective)]))])));
                }
            }
            else {
                queryTerms.push(new TermTermAttribute(new Term(o.getSort("#not"), [new TermTermAttribute(new Term(adjective, [queryVariable]))])));
            }
        }
        for (var _h = 0, otherRelations_1 = otherRelations; _h < otherRelations_1.length; _h++) {
            var relation = otherRelations_1[_h];
            //			console.log("considering relation: " + relation);
            var atts = [];
            var negated = false;
            var found = false;
            if (relation.functor.name == "#not") {
                negated = true;
                relation = (relation.attributes[0]).term;
            }
            for (var i = 0; i < relation.attributes.length; i++) {
                if (relation.attributes[i] == queryFunctor) {
                    atts.push(queryVariable);
                    found = true;
                }
                else {
                    atts.push(relation.attributes[i]);
                }
            }
            if (found) {
                verbOrRelationTerm = new TermTermAttribute(new Term(relation.functor, atts));
                if (negated) {
                    verbOrRelationTerm = new TermTermAttribute(new Term(o.getSort("#not"), [verbOrRelationTerm]));
                }
                queryTerms.push(verbOrRelationTerm);
            }
            else {
                console.log("specialfunction_derefQuery: otherRelation does not have queryFunctor (" + queryFunctor + ") as a parameter: " + relation);
            }
        }
        for (var _j = 0, adverbs_1 = adverbs; _j < adverbs_1.length; _j++) {
            var adverb = adverbs_1[_j];
            if (adverb.attributes.length > 0 &&
                adverb.attributes[0] instanceof ConstantTermAttribute) {
                var adverbString = adverb.attributes[0].value;
                if (adverbString == "space.here") {
                    // find the location of the speaker:
                    var hereEntity = context.findLocationOfID(context.speaker);
                    if (hereEntity != null) {
                        verbOrRelationTerm = new TermTermAttribute(new Term(o.getSort("space.at"), [queryVariable, hereEntity.objectID]));
                        queryTerms.push(verbOrRelationTerm);
                    }
                }
                else if (adverbString == "space.there") {
                    // Find if there is a location we just talked about (and that is not the place where the speaker is):
                    var hereEntity = context.findLocationOfID(context.speaker);
                    var thereEntity = null;
                    var entities_mpl = context.findEntitiesOfSort(o.getSort("space.location"), o);
                    var candidateThereEntities = context.applySingularTheDeterminer(entities_mpl);
                    if (candidateThereEntities != null && candidateThereEntities.length == 1)
                        thereEntity = candidateThereEntities[0];
                    if (thereEntity != null && thereEntity != hereEntity) {
                        verbOrRelationTerm = new TermTermAttribute(new Term(o.getSort("space.at"), [queryVariable, thereEntity.objectID]));
                        queryTerms.push(verbOrRelationTerm);
                    }
                }
            }
        }
        // we iterate twice over the adverbs, since the time-related ones have to be processed last:
        for (var _k = 0, adverbs_2 = adverbs; _k < adverbs_2.length; _k++) {
            var adverb = adverbs_2[_k];
            if (adverb.attributes.length > 0 &&
                adverb.attributes[0] instanceof ConstantTermAttribute) {
                var adverbString = adverb.attributes[0].value;
                if (adverbString == "time.past") {
                    if (verbOrRelationTerm != null) {
                        queryTerms.push(new TermTermAttribute(new Term(o.getSort("time.past"), [verbOrRelationTerm])));
                    }
                    else {
                        // we have a time adverb, but we don't know what to qualify with it!
                        return null;
                    }
                }
                else if (adverbString == "time.future") {
                    if (verbOrRelationTerm != null) {
                        queryTerms.push(new TermTermAttribute(new Term(o.getSort("time.future"), [verbOrRelationTerm])));
                    }
                    else {
                        // we have a time adverb, but we don't know what to qualify with it!
                        return null;
                    }
                }
            }
        }
        /*
        if (properNounTerm != null && o.getSort("#id").is_a(queryVariable.sort)) {
            queryTerms.push(new TermTermAttribute(new Term(o.getSort("name"),[queryVariable,properNounTerm.attributes[0]])));
        }
        */
        // list of entities for which we have added a not(queryVariable == entity) term:
        var nottedEntities = [];
        if (elsePresent) {
            queryTerms.push(new TermTermAttribute(new Term(o.getSort("!="), [queryVariable, new ConstantTermAttribute(context.speaker, o.getSort("#id"))])));
            queryTerms.push(new TermTermAttribute(new Term(o.getSort("!="), [queryVariable, new ConstantTermAttribute(context.ai.selfID, o.getSort("#id"))])));
            nottedEntities.push(context.speaker);
            nottedEntities.push(context.ai.selfID);
        }
        if (otherDeterminerPresent && queryFunctorSort != null) {
            // Find the entities that the expression could be referring to, and exclude them ("other"):
            var entities_mpl = context.findEntitiesOfSort(queryFunctorSort, o);
            if (entities_mpl != null) {
                // exclude the ones in mentions and in short term memory:
                for (var _l = 0, _m = entities_mpl[0]; _l < _m.length; _l++) {
                    var e = _m[_l];
                    var stringID = (e.objectID).value;
                    if (nottedEntities.indexOf(stringID) == -1) {
                        queryTerms.push(new TermTermAttribute(new Term(o.getSort("!="), [queryVariable, e.objectID])));
                        nottedEntities.push(stringID);
                    }
                }
                for (var _o = 0, _p = entities_mpl[1]; _o < _p.length; _o++) {
                    var e = _p[_o];
                    var stringID = (e.objectID).value;
                    if (nottedEntities.indexOf(stringID) == -1) {
                        queryTerms.push(new TermTermAttribute(new Term(o.getSort("!="), [queryVariable, e.objectID])));
                        nottedEntities.push(stringID);
                    }
                }
            }
        }
        if (queryTerms.length == 0) {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        queryTerm = NLParser.constructList(queryTerms, o.getSort("#and"));
        //		console.log("queryFunctorSort: " + queryFunctorSort);
        //		console.log("queryTerms: " + queryTerms);
        //		console.log("queryTerm: " + queryTerm);
        if (queryTerm == null) {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        var bindings2 = new Bindings();
        if (!Term.unifyAttribute(query, queryTerm, true, bindings2)) {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
        var bindings3 = parse.bindings.concat(bindings2);
        var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, bindings3, parse.derefs.concat([new Term(o.getSort("#derefQuery"), [clause, queryVariable.applyBindings(bindings3), query.applyBindings(bindings3)])]), parse.ruleNames, parse.priorities);
        return [parse2];
    };
    NLPattern.prototype.specialfunction_symbolToSort = function (parse, symbol, sort, o) {
        if (symbol instanceof ConstantTermAttribute) {
            var symbol_v = symbol.value;
            //			let symbol_s:Sort = symbol.sort;
            //			if (symbol_s.is_a(o.getSort("symbol"))) {
            var s = o.getSort(symbol_v);
            if (s == null)
                return null;
            var sAtt = new VariableTermAttribute(s, null);
            var bindings = new Bindings();
            if (!Term.unifyAttribute(sort, sAtt, true, bindings)) {
                return null;
            }
            var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities);
            return [parse2];
            //			} else {
            //				return null;
            //			}
        }
        else {
            return null;
        }
    };
    NLPattern.prototype.specialfunction_subsumes = function (parse, sortAtt, att, o) {
        if (sortAtt instanceof VariableTermAttribute) {
            var s = sortAtt.sort;
            if (att.sort.is_a(s))
                return [parse];
        }
        return null;
    };
    NLPattern.prototype.specialfunction_doesnotsubsume = function (parse, sortAtt, att, o) {
        if (sortAtt instanceof VariableTermAttribute) {
            var s = sortAtt.sort;
            if (!att.sort.is_a(s))
                return [parse];
        }
        return null;
    };
    NLPattern.prototype.specialfunction_equal = function (parse, att1, att2, o) {
        var bindings = new Bindings();
        if (Term.unifyAttribute(att1, att2, true, bindings)) {
            var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities);
            return [parse2];
        }
        return null;
    };
    NLPattern.prototype.specialfunction_notequal = function (parse, att1, att2, o) {
        if (att1 instanceof VariableTermAttribute) {
            if (att1.sort != att2.sort)
                return [parse];
            if (att1.name != att2.name)
                return [parse];
        }
        else if (att1 instanceof ConstantTermAttribute) {
            if (att1.sort != att2.sort)
                return [parse];
            if (att1.value != att2.value)
                return [parse];
        }
        else {
            console.error("#notequal among terms not yet supported!");
            return null;
        }
        return null;
    };
    NLPattern.prototype.specialfunction_sortParent = function (parse, sortAtt, att, o) {
        var sort = sortAtt.sort;
        var parses = [];
        if (sortAtt instanceof ConstantTermAttribute) {
            for (var _i = 0, _a = sort.parents; _i < _a.length; _i++) {
                var parent_2 = _a[_i];
                if (parent_2.is_a(att.sort)) {
                    var output = new ConstantTermAttribute(parent_2.name, parent_2);
                    var bindings = new Bindings();
                    if (Term.unifyAttribute(att, output, true, bindings)) {
                        var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities);
                        parses.push(parse2);
                    }
                }
            }
        }
        else if (sortAtt instanceof VariableTermAttribute) {
            for (var _b = 0, _c = sort.parents; _b < _c.length; _b++) {
                var parent_3 = _c[_b];
                if (parent_3.is_a(att.sort)) {
                    var output = new VariableTermAttribute(parent_3, null);
                    var bindings = new Bindings();
                    if (Term.unifyAttribute(att, output, true, bindings)) {
                        var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities);
                        parses.push(parse2);
                    }
                }
            }
        }
        else {
            return null;
        }
        return parses;
    };
    NLPattern.prototype.specialfunction_concatenateSymbols = function (parse, args, o) {
        var concatenation = "";
        for (var i = 0; i < args.length - 1; i++) {
            if (args[i] instanceof ConstantTermAttribute) {
                concatenation += (args[i]).value;
            }
            else {
                return null;
            }
        }
        var bindings = new Bindings();
        if (!Term.unifyAttribute(args[args.length - 1], new ConstantTermAttribute(concatenation, o.getSort("symbol")), true, bindings)) {
            return null;
        }
        var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities);
        return [parse2];
    };
    NLPattern.prototype.specialfunction_completeVerbArgumentsFromContext = function (parse, verb, output, context, o) {
        this.lastDerefErrorType = 0;
        if (!(verb instanceof TermTermAttribute)) {
            console.log("specialfunction_completeVerbArgumentsFromContext: trying to complete a verb that is not a term! " + verb);
            this.lastDerefErrorType = DEREF_ERROR_VERB_COMPLETION;
            return null;
        }
        var term2 = verb.term.applyBindings(parse.bindings);
        var result_l = context.completeVerbArgumentsFromContext(term2, output, o);
        //		console.log("specialfunction_derefFromContext result: " + result);
        if (result_l == null || result_l.length == 0) {
            this.lastDerefErrorType = context.lastDerefErrorType;
            return null;
        }
        var result = result_l[0];
        for (var i = 1; i < result_l.length; i++) {
            var tmp = new Term(o.getSort("#list"), [result_l[i], result]);
            result = new TermTermAttribute(tmp);
        }
        var bindings2 = new Bindings();
        if (Term.unifyAttribute(output, result, true, bindings2)) {
            var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings2), parse.derefs, parse.ruleNames, parse.priorities);
            return [parse2];
        }
        else {
            this.lastDerefErrorType = DEREF_ERROR_CANNOT_PROCESS_EXPRESSION;
            return null;
        }
    };
    NLPattern.prototype.specialfunction_changeConstantSort = function (parse, args, o) {
        if (args.length != 3)
            return null;
        if (!(args[0] instanceof ConstantTermAttribute))
            return null;
        var newValue = new ConstantTermAttribute(args[0].value, args[1].sort);
        var bindings = new Bindings();
        if (!Term.unifyAttribute(args[2], newValue, true, bindings))
            return null;
        var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities);
        return [parse2];
    };
    NLPattern.prototype.specialfunction_token = function (parse, arg, o) {
        if (parse.nextTokens == null)
            return null;
        var parses = [];
        for (var _i = 0, _a = parse.nextTokens; _i < _a.length; _i++) {
            var nextToken = _a[_i];
            if (nextToken.token == null) {
                var parses2 = this.specialfunction_token(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities), arg, o);
                if (parses2 != null)
                    parses = parses.concat(parses2);
            }
            else {
                var newValue = new ConstantTermAttribute(nextToken.token, o.getSort("symbol"));
                var bindings = new Bindings();
                if (!Term.unifyAttribute(arg, newValue, true, bindings))
                    return null;
                parses.push(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities));
            }
        }
        if (parses.length == 0)
            return null;
        return parses;
    };
    NLPattern.prototype.specialfunction_findLastNoun = function (parse, arg, context, o) {
        var noun = null;
        for (var _i = 0, _a = parse.previousPOS; _i < _a.length; _i++) {
            var pos = _a[_i];
            if (pos.term.functor.name == "noun") {
                noun = pos;
            }
        }
        if (noun == null) {
            var n_past_performatives = 2;
            for (var i = 0; i < context.performatives.length && i < n_past_performatives; i++) {
                var p = context.performatives[i];
                if (p.parse != null) {
                    for (var _b = 0, _c = p.parse.previousPOS; _b < _c.length; _b++) {
                        var pos = _c[_b];
                        if (pos.term.functor.name == "noun") {
                            noun = pos;
                        }
                    }
                }
                if (p.derefErrors != null) {
                    for (var _d = 0, _e = p.derefErrors; _d < _e.length; _d++) {
                        var error = _e[_d];
                        for (var _f = 0, _g = error.previousPOS; _f < _g.length; _f++) {
                            var pos = _g[_f];
                            if (pos.term.functor.name == "noun") {
                                noun = pos;
                            }
                        }
                        if (noun != null)
                            break;
                    }
                }
                if (noun != null)
                    break;
            }
        }
        if (noun != null) {
            var bindings = new Bindings();
            if (!Term.unifyAttribute(arg, new TermTermAttribute(noun.term), true, bindings))
                return null;
            return [new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities)];
        }
        return null;
    };
    NLPattern.prototype.clone = function (map) {
        switch (this.type) {
            case NLPATTERN_STRING:
                return this; // this is a constant, no need to clone
            case NLPATTERN_SEQUENCE:
            case NLPATTERN_ALTERNATIVE:
            case NLPATTERN_OPTIONAL:
            case NLPATTERN_REPEAT:
                {
                    var children = [];
                    for (var _i = 0, _a = this.children; _i < _a.length; _i++) {
                        var c = _a[_i];
                        children.push(c.clone(map));
                    }
                    var p = new NLPattern(this.type);
                    p.children = children;
                    return p;
                }
            case NLPATTERN_POS:
            case NLPATTERN_PATTERN:
            case NLPATTERN_FUNCTION:
                {
                    var p = new NLPattern(this.type);
                    p.term = this.term.clone(map);
                    return p;
                }
            default:
                console.error("NLPattern.clone: pattern type not supported " + this.type);
                return null;
        }
    };
    /*
    applyBindings(bindings:Bindings) : NLPattern
    {
        console.log("NLPattern.applyBindings: " + bindings);
        let pattern2:NLPattern = new NLPattern(this.type);
        pattern2.string = this.string;
        pattern2.term = (this.term == null ? null : this.term.applyBindings(bindings));
        if (this.children != null) {
            pattern2.children = [];
            for(let child of this.children) {
                pattern2.children.push(child.applyBindings(bindings));
            }
        }
        pattern2.lastDerefErrorType = this.lastDerefErrorType;

        return pattern2;
    }
    */
    NLPattern.fromString = function (str, o, variableNames, variableValues) {
        // start assuming it is a sequence. Tokenize the string into the elements of the sequence, and then parse each of them:
        var parentheses = 0;
        var squareBrackets = 0;
        var inquote = false;
        var elementStrings = [];
        var tmp = "";
        for (var i = 0; i < str.length; i++) {
            var c = str.charAt(i);
            if (inquote) {
                if (c == '\'') {
                    inquote = false;
                    if (tmp.length > 0 && tmp[tmp.length - 1] == '\\')
                        inquote = true;
                }
                tmp += c;
            }
            else {
                if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                    if (!inquote && parentheses == 0 && squareBrackets == 0 && tmp.length > 0) {
                        // we have an element!
                        tmp = tmp.trim();
                        if (tmp.length > 0) {
                            //							tmp = tmp.replace("\\","");	// just for this case "\'s"
                            elementStrings.push(tmp);
                            //							console.log("Element: " + tmp + " (length: " + tmp.length + ")");
                            tmp = "";
                        }
                    }
                    else {
                        tmp += c;
                    }
                }
                else {
                    if (c == '(')
                        parentheses++;
                    if (c == ')')
                        parentheses--;
                    if (c == '[')
                        squareBrackets++;
                    if (c == ']')
                        squareBrackets--;
                    if (c == '\'')
                        inquote = true;
                    tmp += c;
                }
            }
        }
        if (inquote) {
            console.error("Unclosed quotation while parsing NLPattern: " + str);
            return null;
        }
        if (parentheses != 0) {
            console.error("Unclosed parenthesis while parsing NLPattern: " + str);
            return null;
        }
        if (squareBrackets != 0) {
            console.error("Unclosed square brackets while parsing NLPattern: " + str);
            return null;
        }
        if (!inquote && parentheses == 0 && squareBrackets == 0 && tmp.length > 0) {
            // we have an element!
            tmp = tmp.trim();
            if (tmp.length > 0) {
                //				tmp = tmp.replace("\\","");	// just for this case "\'s"
                elementStrings.push(tmp);
                //				console.log("Element: " + tmp);
            }
        }
        //		console.log("str: " + str + "  -->>  " + elementStrings);
        if (elementStrings.length > 1) {
            // it is a sequence:
            var patterns = [];
            for (var _i = 0, elementStrings_1 = elementStrings; _i < elementStrings_1.length; _i++) {
                var patternString = elementStrings_1[_i];
                var pattern = NLPattern.fromString(patternString, o, variableNames, variableValues);
                if (pattern == null) {
                    console.error("NLPattern.fromString: cannot parse string " + patternString);
                    return null;
                }
                patterns.push(pattern);
            }
            var p = new NLPattern(NLPATTERN_SEQUENCE);
            p.children = patterns;
            return p;
        }
        else {
            var patternString = elementStrings[0];
            var repeat = false;
            var pattern = null;
            if (patternString.charAt(patternString.length - 1) == '*') {
                repeat = true;
                patternString = patternString.substring(0, patternString.length - 1);
            }
            var c = patternString.charAt(0);
            if (c == '(')
                pattern = NLPattern.fromStringAlternative(patternString, o, variableNames, variableValues);
            if (c == '[')
                pattern = NLPattern.fromStringOptional(patternString, o, variableNames, variableValues);
            if (c == '\'')
                pattern = NLPattern.fromStringString(patternString, o, variableNames, variableValues);
            if ((c >= 'a' && c < 'z') ||
                (c >= 'A' && c < 'Z') ||
                c == '#')
                pattern = NLPattern.fromStringTerm(patternString, o, variableNames, variableValues);
            if (pattern != null) {
                if (repeat) {
                    var pattern2 = new NLPattern(NLPATTERN_REPEAT);
                    pattern2.children = [pattern];
                    return pattern2;
                }
                else {
                    return pattern;
                }
            }
            console.error("NLPattern.fromString: cannor parse string " + patternString);
            return null;
        }
    };
    NLPattern.fromStringAlternative = function (str, o, variableNames, variableValues) {
        // console.log("fromStringAlternative: " + str);
        if (str.charAt(0) != '(' ||
            str.charAt(str.length - 1) != ')') {
            console.error("NLPattern.fromStringAlternative: string does not start and end with parentheses!");
            return null;
        }
        str = str.substring(1, str.length - 1);
        var parentheses = 0;
        var squareBrackets = 0;
        var inquote = false;
        var elementStrings = [];
        var tmp = "";
        for (var i = 0; i < str.length; i++) {
            var c = str.charAt(i);
            if (inquote) {
                if (c == '\'') {
                    inquote = false;
                    if (tmp.length > 0 && tmp[tmp.length - 1] == '\\')
                        inquote = true;
                }
                tmp += c;
            }
            else {
                if (c == '|' && !inquote && parentheses == 0 && squareBrackets == 0) {
                    // we have an element!
                    tmp = tmp.trim();
                    if (tmp.length > 0) {
                        elementStrings.push(tmp);
                        //						console.log("alternative element: " + tmp);
                        tmp = "";
                    }
                    else {
                        console.error("NLPattern.fromStringAlternative: empty alternative parsing " + str);
                        return null;
                    }
                }
                else {
                    if (c == '(')
                        parentheses++;
                    if (c == ')')
                        parentheses--;
                    if (c == '[')
                        squareBrackets++;
                    if (c == ']')
                        squareBrackets--;
                    if (c == '\'')
                        inquote = true;
                    tmp += c;
                }
            }
        }
        if (!inquote && parentheses == 0 && squareBrackets == 0 && tmp.length > 0) {
            // we have an element!
            tmp = tmp.trim();
            if (tmp.length > 0) {
                elementStrings.push(tmp);
                //				console.log("alternative element: " + tmp);
            }
            else {
                console.error("NLPattern.fromStringAlternative: empty last alternative parsing " + str);
                return null;
            }
        }
        else {
            console.error("NLPattern.fromStringAlternative: empty last alternative parsing " + str);
            return null;
        }
        if (elementStrings.length > 1) {
            var patterns = [];
            for (var _i = 0, elementStrings_2 = elementStrings; _i < elementStrings_2.length; _i++) {
                var patternString = elementStrings_2[_i];
                var pattern = NLPattern.fromString(patternString, o, variableNames, variableValues);
                if (pattern == null) {
                    console.error("NLPattern.fromString: cannot parse string " + patternString);
                    return null;
                }
                patterns.push(pattern);
            }
            var p = new NLPattern(NLPATTERN_ALTERNATIVE);
            p.children = patterns;
            return p;
        }
        else {
            // this is an alternative with only one option (a parenthesis), so, just do a recursive call:
            return NLPattern.fromString(str, o, variableNames, variableValues);
        }
    };
    NLPattern.fromStringOptional = function (str, o, variableNames, variableValues) {
        if (str.charAt(0) != '[' ||
            str.charAt(str.length - 1) != ']') {
            console.error("NLPattern.fromStringOptional: string does not start and end with square brackets!");
            return null;
        }
        str = str.substring(1, str.length - 1);
        var pattern = NLPattern.fromString(str, o, variableNames, variableValues);
        if (pattern == null)
            return null;
        var p = new NLPattern(NLPATTERN_OPTIONAL);
        p.children = [pattern];
        return p;
    };
    NLPattern.fromStringString = function (str, o, variableNames, variableValues) {
        if (str.charAt(0) != '\'' ||
            str.charAt(str.length - 1) != '\'') {
            console.error("NLPattern.fromStringString: string does not start and end with quotes!");
            return null;
        }
        str = str.replace("\\", ""); // just for this case "\'s"
        str = str.substring(1, str.length - 1);
        var p = new NLPattern(NLPATTERN_STRING);
        p.string = str;
        return p;
    };
    NLPattern.fromStringTerm = function (str, o, variableNames, variableValues) {
        var term = Term.fromStringInternal(str, o, variableNames, variableValues).term;
        if (term == null)
            return null;
        // check if it's a POS, a pattern or a special function:
        var posSort = o.getSort("part-of-speech");
        var specialFunctionSort = o.getSort("parser-function");
        if (posSort.subsumes(term.functor)) {
            // POS:
            var p = new NLPattern(NLPATTERN_POS);
            p.term = term;
            return p;
        }
        else if (specialFunctionSort.subsumes(term.functor)) {
            // Special function:
            var p = new NLPattern(NLPATTERN_FUNCTION);
            p.term = term;
            return p;
        }
        else {
            // sub pattern
            var p = new NLPattern(NLPATTERN_PATTERN);
            p.term = term;
            return p;
        }
    };
    NLPattern.prototype.toStringWithoutChildren = function () {
        if (this.string != null)
            return "'" + this.string + "'";
        if (this.term != null)
            return this.term.toString();
        return null;
    };
    NLPattern.prototype.toString = function () {
        return this.toStringInternal([], []);
    };
    NLPattern.prototype.toStringInternal = function (variables, variableNames) {
        switch (this.type) {
            case NLPATTERN_SEQUENCE:
                {
                    var out = "";
                    for (var i = 0; i < this.children.length; i++) {
                        var child = this.children[i];
                        if (i != 0)
                            out += " ";
                        out += child.toStringInternal(variables, variableNames);
                    }
                    return out;
                }
            case NLPATTERN_ALTERNATIVE:
                {
                    var out = "(";
                    for (var i = 0; i < this.children.length; i++) {
                        var child = this.children[i];
                        if (i != 0)
                            out += "|";
                        out += child.toStringInternal(variables, variableNames);
                    }
                    return out + ")";
                }
            case NLPATTERN_OPTIONAL:
                return "[" + this.children[0].toStringInternal(variables, variableNames) + "]";
            case NLPATTERN_REPEAT:
                return this.children[0].toStringInternal(variables, variableNames) + "*";
            case NLPATTERN_STRING:
                return "'" + this.string + "'";
            case NLPATTERN_POS:
                return this.term.toStringInternal(variables, variableNames);
            case NLPATTERN_PATTERN:
            case NLPATTERN_FUNCTION:
                return this.term.toStringInternal(variables, variableNames);
            //			case NLPATTERN_NONE:
            default:
                console.error("A pattern has type NLPATTERN_NONE!");
                return "";
        }
    };
    return NLPattern;
}());
