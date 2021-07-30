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
var CompiledNLPatternRules = /** @class */ (function (_super) {
    __extends(CompiledNLPatternRules, _super);
    function CompiledNLPatternRules(name, o, sv, lv) {
        var _this = _super.call(this, name, sv, lv) || this;
        _this.root = null;
        return _this;
        //		if (o!=null) {
        //			this.speakerVariable = new VariableTermAttribute(o.getSort("any"),"SPEAKER");
        //			this.listenerVariable = new VariableTermAttribute(o.getSort("any"),"LISTENER");
        //		}
    }
    CompiledNLPatternRules.prototype.ruleHeadMatchesSort = function (sort, rule) {
        if (rule.head.functor.is_a(sort)) {
            return true;
        }
        else if (rule.head.functor.name == "#list" &&
            rule.head.attributes.length > 0 &&
            rule.head.attributes[0] instanceof TermTermAttribute) {
            if (rule.head.attributes[0].term.functor.is_a(sort)) {
                return true;
            }
        }
        return false;
    };
    CompiledNLPatternRules.prototype.populate = function (sort, parser) {
        this.root = new CompiledNLPatternState();
        for (var _i = 0, _a = parser.rules; _i < _a.length; _i++) {
            var rule = _a[_i];
            if (this.ruleHeadMatchesSort(sort, rule)) {
                //			if (rule.head.functor.is_a(sort)) {
                //				console.log("rule head: " + rule.head);
                this.root.addRule(rule, this.speakerVariable, this.listenerVariable);
            }
        }
    };
    CompiledNLPatternRules.prototype.cloneForParsing = function () {
        //		let map:[TermAttribute,TermAttribute][] = [];
        var c = new CompiledNLPatternRules(this.name, null, this.speakerVariable, this.listenerVariable);
        c.root = this.root;
        //		this.speakerVariable = new VariableTermAttribute(o.getSort("any"),"SPEAKER");
        //		this.listenerVariable = new VariableTermAttribute(o.getSort("any"),"LISTENER");
        //		c.speakerVariable = this.speakerVariable;
        //		c.listenerVariable = this.listenerVariable;
        return c;
    };
    CompiledNLPatternRules.prototype.parse = function (tokenization, filterPartialParses, context, parser, AI) {
        // parse the sentence:
        //		console.log("NLPatternRule.parse");
        var bindings = new Bindings();
        if (this.speakerVariable != null) {
            //			console.log("Speaker: " + this.speakerVariable);
            bindings.l.push([this.speakerVariable,
                new ConstantTermAttribute(context.speaker, parser.o.getSort("#id"))]);
        }
        var parses = this.root.parse(new NLParseRecord([tokenization], [], bindings, [], [], []), context, this, parser, AI, filterPartialParses);
        if (parses == null)
            return null;
        //		console.log("CompiledNLPatternRules.parse, n parses: " + parses.length);
        // if there is any valid parse, generate the corresponding terms:
        var results = [];
        for (var _i = 0, parses_1 = parses; _i < parses_1.length; _i++) {
            var parse = parses_1[_i];
            if (filterPartialParses &&
                parse.nextTokens != null && parse.nextTokens.length > 0)
                continue;
            //			parse.ruleNames = [this.name].concat(parse.ruleNames);
            //			parse.priorities = [this.priority].concat(parse.priorities);
            //			parse.result = this.head.applyBindings(parse.bindings);
            // console.log("Parse completed, result (before resolvecons): " + parse.result);
            NLParser.resolveCons(parse.result, parser.o);
            // console.log("CompiledNLPatternRules.parse: Parse completed, result: " + parse.result);
            results.push(parse);
        }
        return results;
    };
    CompiledNLPatternRules.prototype.parseMatchingWithTerm = function (parse, filterPartialParses, context, parser, AI, term) {
        var results = [];
        var parses = this.root.parse(parse, context, this, parser, AI, filterPartialParses);
        if (parses != null) {
            //			console.log("parseMatchingWithTerm completed with parses.length = " + parses.length);
            for (var _i = 0, parses_2 = parses; _i < parses_2.length; _i++) {
                var parse2 = parses_2[_i];
                if (filterPartialParses &&
                    parse2.nextTokens != null && parse2.nextTokens.length > 0)
                    continue;
                //				parse2.ruleNames = [this.name].concat(parse.ruleNames);
                //				parse2.priorities = [this.priority].concat(parse.priorities);
                //				parse2.result = this.head.applyBindings(parse2.bindings);
                // console.log("parseMatchingWithTerm completed, result (before resolvecons): " + parse2.result);
                NLParser.resolveCons(parse2.result, parser.o);
                // console.log("parseMatchingWithTerm.parse: Parse completed, result: " + parse2.result);
                var bindings = new Bindings();
                // if (parse2.result.unify(term, OCCURS_CHECK, bindings)) {
                if (term.unify(parse2.result, OCCURS_CHECK, bindings)) {
                    parse2.result = parse2.result.applyBindings(bindings);
                    //					console.log("parseMatchingWithTerm completed, result: " + parse2.result);
                    results.push(parse2);
                }
            }
        }
        return results;
    };
    return CompiledNLPatternRules;
}(NLPatternContainer));
var CompiledNLPatternState = /** @class */ (function () {
    function CompiledNLPatternState() {
        this.priorities = []; // if parsing ends at this node, what would be the priority of the parse
        this.heads = []; // if parsing can end at this node, this is the term that will be returned
        this.transitions = [];
        this.ruleNames = [];
    }
    // this function adds a rule to the current parsing graph, and returns the set of terminal states where this rule ends
    CompiledNLPatternState.prototype.addRule = function (rule, speakerVariable, listenerVariable) {
        var tmp = this.addRuleInternal(rule.priority, rule.body, new Bindings(), speakerVariable, listenerVariable);
        for (var i = 0; i < tmp.length; i++) {
            var s = tmp[i][0];
            var b = tmp[i][1];
            // set the proper LISTENER and SPEAKER variables:
            var rule2 = rule.head.applyBindings(b);
            //			console.log("rule.head: " + rule.head);
            //			console.log("rule2: " + rule2);
            var variables = rule2.getAllVariables();
            var b2 = new Bindings();
            for (var _i = 0, variables_1 = variables; _i < variables_1.length; _i++) {
                var v = variables_1[_i];
                //				if (v.name == "LISTENER" && v != listenerVariable) b2.l.push([v, listenerVariable]);
                if (v.name == "SPEAKER" && v != speakerVariable)
                    b2.l.push([v, speakerVariable]);
            }
            if (b2.l.length > 0)
                rule2 = rule2.applyBindings(b2);
            s.priorities.push(rule.priority);
            s.heads.push(rule2);
            s.ruleNames.push(rule.name);
        }
    };
    CompiledNLPatternState.prototype.addRuleInternal = function (priority, current, bindings, speakerVariable, listenerVariable) {
        switch (current.type) {
            case NLPATTERN_SEQUENCE:
                var currentStates = [[this, bindings]];
                for (var i = 0; i < current.children.length; i++) {
                    var nextStates = [];
                    for (var _i = 0, currentStates_1 = currentStates; _i < currentStates_1.length; _i++) {
                        var tmp_1 = currentStates_1[_i];
                        var s = tmp_1[0];
                        var b = tmp_1[1];
                        var nextStates2 = s.addRuleInternal(priority, current.children[i], b, speakerVariable, listenerVariable);
                        for (var _a = 0, nextStates2_1 = nextStates2; _a < nextStates2_1.length; _a++) {
                            var ns = nextStates2_1[_a];
                            nextStates.push(ns);
                        }
                    }
                    currentStates = nextStates;
                }
                return currentStates;
            case NLPATTERN_ALTERNATIVE:
                {
                    var nextStates = [];
                    var accumBindings = bindings;
                    for (var i = 0; i < current.children.length; i++) {
                        var nextStates2 = this.addRuleInternal(priority, current.children[i], accumBindings, speakerVariable, listenerVariable);
                        if (nextStates2.length != 1) {
                            console.error("!!!");
                        }
                        for (var _b = 0, nextStates2_2 = nextStates2; _b < nextStates2_2.length; _b++) {
                            var ns2 = nextStates2_2[_b];
                            nextStates.push(ns2);
                            accumBindings = ns2[1];
                        }
                    }
                    if (nextStates.length > 1) {
                        var s = new CompiledNLPatternState();
                        for (var _c = 0, nextStates_1 = nextStates; _c < nextStates_1.length; _c++) {
                            var ns = nextStates_1[_c];
                            var t = new CompiledNLPatternTransition();
                            t.type = NLPATTERN_NONE;
                            t.maxPriority = priority;
                            t.destinationState = s;
                            ns[0].transitions.push(t);
                        }
                        return [[s, accumBindings]];
                    }
                    else {
                        return nextStates;
                    }
                }
            case NLPATTERN_OPTIONAL:
                {
                    var nextStates = this.addRuleInternal(priority, current.children[0], bindings, speakerVariable, listenerVariable);
                    if (nextStates.length == 1) {
                        var found = false;
                        for (var _d = 0, _e = this.transitions; _d < _e.length; _d++) {
                            var t2 = _e[_d];
                            if (t2.type == NLPATTERN_NONE && t2.destinationState == nextStates[0][0]) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            // create a new transition (if needed):
                            var t = new CompiledNLPatternTransition();
                            t.type = NLPATTERN_NONE;
                            t.maxPriority = priority;
                            t.destinationState = nextStates[0][0];
                            this.transitions.push(t);
                        }
                        return nextStates;
                    }
                    else {
                        nextStates.push([this, bindings]);
                        return nextStates;
                    }
                }
            case NLPATTERN_REPEAT:
                {
                    var nextStates = this.addRuleInternal(priority, current.children[0], bindings, speakerVariable, listenerVariable);
                    // replace the next states with this:
                    var open_1 = [];
                    var closed_1 = [];
                    open_1.push([this, null]);
                    while (open_1.length > 0) {
                        //					console.log("open: " + open.length + ", closed: " + closed.length);
                        var _f = open_1[0], current_s = _f[0], current_t = _f[1];
                        open_1.splice(0, 1);
                        closed_1.push(current_s);
                        var found = false;
                        for (var _g = 0, nextStates_2 = nextStates; _g < nextStates_2.length; _g++) {
                            var sb = nextStates_2[_g];
                            if (sb[0] == current_s && current_t != null) {
                                current_t.destinationState = this;
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            for (var _h = 0, _j = current_s.transitions; _h < _j.length; _h++) {
                                var t = _j[_h];
                                if (closed_1.indexOf(t.destinationState) == -1) {
                                    open_1.push([t.destinationState, t]);
                                }
                            }
                        }
                    }
                }
                return [[this, bindings]];
            case NLPATTERN_STRING:
            case NLPATTERN_POS:
            case NLPATTERN_PATTERN:
            case NLPATTERN_FUNCTION:
                var tmp = this.findMatchingTransition(current, bindings);
                if (tmp == null) {
                    // create a new transition:
                    var t = new CompiledNLPatternTransition();
                    var s = new CompiledNLPatternState();
                    t.type = current.type;
                    t.string = current.string;
                    t.maxPriority = priority;
                    t.destinationState = s;
                    if (current.term != null) {
                        //						let v:TermAttribute[] = [];
                        //						let vn:string[] = [];
                        //						console.log("  adding transition: " + current.term.toStringInternal(v, vn));
                        //						console.log("  with bindings: " + bindings.toStringWithMappings(v, vn));
                        var term2 = current.term.applyBindings(bindings);
                        // set the proper LISTENER and SPEAKER variables:
                        var variables = term2.getAllVariables();
                        var b2 = new Bindings();
                        for (var _k = 0, variables_2 = variables; _k < variables_2.length; _k++) {
                            var v = variables_2[_k];
                            //							if (v.name == "LISTENER" && v != listenerVariable) b2.l.push([v, listenerVariable]);
                            if (v.name == "SPEAKER" && v != speakerVariable)
                                b2.l.push([v, speakerVariable]);
                        }
                        if (b2.l.length > 0) {
                            t.term = term2.applyBindings(b2);
                        }
                        else {
                            t.term = term2;
                        }
                    }
                    this.transitions.push(t);
                    return [[s, bindings]];
                }
                else {
                    var t = tmp[0];
                    var b = tmp[1];
                    t.maxPriority = Math.max(t.maxPriority, priority);
                    return [[t.destinationState, b]];
                }
            case NLPATTERN_NONE:
                console.error("Unsuported rule type NLPATTERN_NONE!");
                return null;
            default:
                console.error("Unsuported rule type!");
                return null;
        }
        return [];
    };
    CompiledNLPatternState.prototype.findMatchingTransition = function (p, b) {
        for (var i = 0; i < this.transitions.length; i++) {
            if (this.transitions[i].type == p.type) {
                if (p.type == NLPATTERN_STRING) {
                    if (this.transitions[i].string == p.string)
                        return [this.transitions[i], b];
                }
                else {
                    var t2 = p.term.applyBindings(b);
                    var b2 = new Bindings();
                    //					if (this.transitions[i].term.equalsInternal(t2, b2)) return [this.transitions[i], b.concat(b2)];
                    if (t2.equalsInternal(this.transitions[i].term, b2))
                        return [this.transitions[i], b.concat(b2)];
                }
            }
        }
        return null;
    };
    CompiledNLPatternState.prototype.addParseIfNew = function (parses, parse) {
        for (var _i = 0, parses_3 = parses; _i < parses_3.length; _i++) {
            var parse2 = parses_3[_i];
            if (parse.result != null && parse2.result != null &&
                parse.nextTokens.length == parse2.nextTokens.length &&
                parse.higherPriorityThan(parse2) <= 0 &&
                parse.result.equalsNoBindings(parse2.result) == 1) {
                if (parse.nextTokens.length == 0 ||
                    parse.nextTokens[0].token == parse2.nextTokens[0].token) {
                    // console.log("Filtering out parse: ");
                    //          console.log("  result: " + parse.result);
                    //          console.log("  ruleNames: " + parse.ruleNames);
                    //          console.log("  bindings: " + parse.bindings);
                    //          console.log("  nextTokens: " + parse.nextTokens);
                    //          console.log("  priorities: " + parse.priorities);
                    // console.log("Because it's the same as: ");
                    //          console.log("  result: " + parse2.result);
                    //          console.log("  ruleNames: " + parse2.ruleNames);
                    //          console.log("  bindings: " + parse2.bindings);
                    //          console.log("  nextTokens: " + parse2.nextTokens);
                    //          console.log("  priorities: " + parse2.priorities);
                    return false;
                }
            }
        }
        parses.push(parse);
        return true;
    };
    CompiledNLPatternState.prototype.parse = function (parse, context, rule, parser, AI, filterPartialParses) {
        //		console.log("CompiledNLPatternState.parse... (heads: " + this.heads.length + "), parsing: " + 
        //					(parse.nextTokens != null && parse.nextTokens.length>0 ? parse.nextTokens[0].token:"-"));
        var parses = [];
        //		let bestPriority:number = 0;
        for (var i = 0; i < this.heads.length; i++) {
            // found a parse!
            //			console.log("parse found with bindings: " + parse.bindings);
            var parse2 = new NLParseRecord(parse.nextTokens, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities);
            parse2.result = this.heads[i].applyBindings(parse2.bindings);
            parse2.ruleNames = [this.ruleNames[i]].concat(parse2.ruleNames);
            parse2.priorities = [this.priorities[i]].concat(parse2.priorities);
            this.addParseIfNew(parses, parse2);
            // console.log("HEAD reached with rule names: " + parse.ruleNames);
            // if (parse2.nextTokens != null && parse2.nextTokens.length > 0) {
            // console.log("    tokens left: " + parse.nextTokens[0].toStringSimple());
            // } else {
            // 	console.log("HEAD reached with rule names: " + parse2.ruleNames);
            // console.log("    bindings: " + parse.bindings);
            // 	console.log("    result: " + parse2.result);
            // }
            //			console.log("result: " + parse.result);
        }
        for (var _i = 0, _a = this.transitions; _i < _a.length; _i++) {
            var t = _a[_i];
            // if we do not have hopes of finding a parse with higher priority, then we can already abandon:
            //			if (t.maxPriority < bestPriority) {
            //				console.log("t.maxPriority = " + t.maxPriority + " < bestPriority = " + bestPriority);
            //				continue;
            //			}
            var parses2 = [];
            switch (t.type) {
                case NLPATTERN_NONE:
                    parses2 = t.destinationState.parse(parse, context, rule, parser, AI, filterPartialParses);
                    break;
                case NLPATTERN_STRING:
                    parses2 = t.parseString(parse, context, rule, parser, AI, filterPartialParses);
                    break;
                case NLPATTERN_POS:
                    parses2 = t.parsePOS(parse, context, rule, parser, AI, filterPartialParses);
                    break;
                case NLPATTERN_PATTERN:
                    parses2 = t.parsePattern(parse, context, rule, parser, AI, filterPartialParses);
                    break;
                case NLPATTERN_FUNCTION:
                    parses2 = t.parseFunction(parse, context, rule, parser, AI, filterPartialParses);
                    break;
                default:
                    console.error("CompiledNLPatternState.parse: pattern type not supported " + t.type);
            }
            if (parses2 != null) {
                for (var _b = 0, parses2_1 = parses2; _b < parses2_1.length; _b++) {
                    var p = parses2_1[_b];
                    if (filterPartialParses &&
                        p.nextTokens != null && p.nextTokens.length > 0)
                        continue;
                    //					if (p.priorities[0] >= bestPriority) {
                    this.addParseIfNew(parses, p);
                    //					console.log("passing along result ("+t.type+", priority = " + p.priorities[0] + "): " + p.result);
                    //					bestPriority = p.priorities[0];
                    //					}
                }
            }
        }
        return parses;
    };
    CompiledNLPatternState.prototype.getAllStatesForDOTString = function () {
        // find all the states:
        var open = [];
        var closed = [];
        open.push(this);
        while (open.length != 0) {
            var current = open[0];
            open.splice(0, 1);
            closed.push(current);
            for (var _i = 0, _a = current.transitions; _i < _a.length; _i++) {
                var t = _a[_i];
                if (closed.indexOf(t.destinationState) == -1 &&
                    open.indexOf(t.destinationState) == -1) {
                    open.push(t.destinationState);
                }
            }
        }
        return closed;
    };
    CompiledNLPatternState.prototype.convertToDOTString = function () {
        var str = "digraph compiledgrammar {\n";
        str += "graph[rankdir=LR];\n";
        var variables = [];
        var variableNames = [];
        // find all the states:
        var closed = this.getAllStatesForDOTString();
        for (var i = 0; i < closed.length; i++) {
            var slabel = "";
            for (var j = 0; j < closed[i].heads.length; j++) {
                if (j != 0)
                    slabel += "\n";
                slabel += closed[i].heads[j].toStringInternal(variables, variableNames) + "  (" + closed[i].priorities[j] + ")";
            }
            str += "s" + i + "[shape=box label=\"" + slabel + "\"];\n";
        }
        for (var i = 0; i < closed.length; i++) {
            for (var j = 0; j < closed[i].transitions.length; j++) {
                var tlabel = "";
                if (closed[i].transitions[j].string != null) {
                    tlabel = "'" + closed[i].transitions[j].string + "'";
                }
                else if (closed[i].transitions[j].term != null) {
                    tlabel = closed[i].transitions[j].term.toStringInternal(variables, variableNames);
                }
                else {
                    "-";
                }
                str += "s" + i + " -> s" + closed.indexOf(closed[i].transitions[j].destinationState) + "[label=\"" + tlabel + "  (" + closed[i].transitions[j].maxPriority + ")\"];\n";
            }
        }
        str += "}\n";
        return str;
    };
    return CompiledNLPatternState;
}());
var CompiledNLPatternTransition = /** @class */ (function () {
    function CompiledNLPatternTransition() {
        this.type = NLPATTERN_NONE;
        this.string = null;
        this.term = null; // this is used for POS, recursive patterns and special functions
        this.destinationState = null;
        this.maxPriority = 0;
    }
    CompiledNLPatternTransition.prototype.parseString = function (parse, context, rule, parser, AI, filterPartialParses) {
        if (parse.nextTokens == null)
            return null;
        var parses = [];
        for (var _i = 0, _a = parse.nextTokens; _i < _a.length; _i++) {
            var nextToken = _a[_i];
            if (nextToken.token == null) {
                var parses2 = this.parseString(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities), context, rule, parser, AI, filterPartialParses);
                if (parses2 != null)
                    parses = parses.concat(parses2);
            }
            else if (nextToken.token == this.string) {
                // match!
                var parses2 = this.destinationState.parse(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities), context, rule, parser, AI, filterPartialParses);
                if (parses2 != null)
                    parses = parses.concat(parses2);
            }
        }
        if (parses.length == 0)
            return null;
        return parses;
    };
    CompiledNLPatternTransition.prototype.parsePOS = function (parse, context, rule, parser, AI, filterPartialParses) {
        var parses = [];
        var term2 = this.term.applyBindings(parse.bindings);
        // console.log("Matching POS, before: " + this.term.toString() + "\n  bindings: " + parse.bindings + "\n  Matching POS, after: " + term2.toString());
        for (var _i = 0, _a = parse.nextTokens; _i < _a.length; _i++) {
            var nextToken = _a[_i];
            if (nextToken.token == null) {
                var parses2 = this.parsePOS(new NLParseRecord(nextToken.next, parse.previousPOS, parse.bindings, parse.derefs, parse.ruleNames, parse.priorities), context, rule, parser, AI, filterPartialParses);
                if (parses2 != null)
                    parses = parses.concat(parses2);
            }
            else {
                for (var _b = 0, _c = nextToken.POS; _b < _c.length; _b++) {
                    var POS = _c[_b];
                    var bindings = new Bindings();
                    if (POS.term.unify(term2, OCCURS_CHECK, bindings)) {
                        var parses2 = this.destinationState.parse(new NLParseRecord(nextToken.next, parse.previousPOS.concat([POS]), parse.bindings.concat(bindings), parse.derefs, parse.ruleNames, parse.priorities), context, rule, parser, AI, filterPartialParses);
                        if (parses2 != null)
                            parses = parses.concat(parses2);
                    }
                }
            }
        }
        if (parses.length == 0)
            return null;
        return parses;
    };
    CompiledNLPatternTransition.prototype.parsePattern = function (parse, context, rule, parser, AI, filterPartialParses) {
        var parses_p = [];
        var term2 = this.term.applyBindings(parse.bindings);
        //		let term2:Term = this.term;
        //				console.log("Matching pattern: " + term2.toString());
        var compiled = parser.compiledRules[term2.functor.name];
        if (compiled != null) {
            // if we have a compiled tree, use it!
            //			console.log("CompiledNLPatternTransition.parsePattern: Found a compiled tree for " + term2.functor.name + " ...");
            var results = compiled.parseMatchingWithTerm(new NLParseRecord(parse.nextTokens, parse.previousPOS, new Bindings(), parse.derefs, parse.ruleNames, parse.priorities), false, context, parser, AI, term2);
            for (var _i = 0, results_1 = results; _i < results_1.length; _i++) {
                var pr = results_1[_i];
                var bindings2 = new Bindings();
                // if (!pr.result.unify(term2, OCCURS_CHECK, bindings2)) {
                if (!term2.unify(pr.result, OCCURS_CHECK, bindings2)) {
                    console.error("CompiledNLPatternTransition.parsePattern: something went wrong when parsing pattern " + term2.toString() + "\n  It does not unify with: " + pr.result);
                    return null;
                }
                // console.log("Succesful unification of:");
                // console.log("        " + pr.result);
                // console.log("        " + term2);
                // console.log("        bindings: " + bindings2);
                // we continue from "pr", but using the bdingins from "parse", since the bindings
                // generated during the parsing of the sub-pattern are not relevant
                var parses2 = this.destinationState.parse(new NLParseRecord(pr.nextTokens, pr.previousPOS, parse.bindings.concat(bindings2), pr.derefs, pr.ruleNames.concat(parse.ruleNames), pr.priorities.concat(parse.priorities)), context, rule, parser, AI, filterPartialParses);
                if (parses2 != null)
                    parses_p = parses_p.concat(parses2);
            }
        }
        else {
            //			console.log("CompiledNLPatternTransition.parsePattern: Using the raw rules for " + term2.functor.name + " ...");
            for (var _a = 0, _b = parser.rules; _a < _b.length; _a++) {
                var rawRule2 = _b[_a];
                var rule2 = rawRule2.clone();
                var bindings = new Bindings();
                if (rule2.head.unify(term2, OCCURS_CHECK, bindings)) {
                    // rule to consider!!
                    //						console.log("  considering rule with head: " + rule2.head.toString() + "\n  new bindings: " + bindings);
                    var results = rule2.parseWithBindings(new NLParseRecord(parse.nextTokens, parse.previousPOS, bindings, parse.derefs, parse.ruleNames, parse.priorities), false, context, parser, AI);
                    for (var _c = 0, results_2 = results; _c < results_2.length; _c++) {
                        var pr = results_2[_c];
                        var bindings2 = new Bindings();
                        if (!pr.result.unify(term2, OCCURS_CHECK, bindings2)) {
                            console.error("CompiledNLPatternTransition.parse: something went wrong when parsing pattern " + term2.toString() + "\n  It does not unify with: " + pr.result);
                            return null;
                        }
                        //						console.log("    Pattern matched successfully with result: " + pr.result.toString());
                        //						console.log("    and bindings: " + bindings2);
                        // we continue from "pr", but using the bdingins from "parse", since the bindings
                        // generated during the parsing of the sub-pattern are not relevant
                        var parses2 = this.destinationState.parse(new NLParseRecord(pr.nextTokens, pr.previousPOS, parse.bindings.concat(bindings2), pr.derefs, pr.ruleNames.concat(parse.ruleNames), pr.priorities.concat(parse.priorities)), context, rule, parser, AI, filterPartialParses);
                        if (parses2 != null)
                            parses_p = parses_p.concat(parses2);
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
    CompiledNLPatternTransition.prototype.parseFunction = function (parse, context, rule, parser, AI, filterPartialParses) {
        var parses_p = [];
        var pattern = new NLPattern(this.type);
        pattern.term = this.term;
        var results = pattern.parseFunction(parse, context, rule, parser, AI);
        if (results != null) {
            for (var _i = 0, results_3 = results; _i < results_3.length; _i++) {
                var pr = results_3[_i];
                //			console.log("parseFunction, parse.bindings: " + parse.bindings);
                //			console.log("parseFunction, pr.bindings: " + pr.bindings);
                var parses2 = this.destinationState.parse(new NLParseRecord(pr.nextTokens, pr.previousPOS, pr.bindings, pr.derefs, pr.ruleNames, pr.priorities), context, rule, parser, AI, filterPartialParses);
                if (parses2 != null)
                    parses_p = parses_p.concat(parses2);
            }
        }
        return parses_p;
    };
    return CompiledNLPatternTransition;
}());
