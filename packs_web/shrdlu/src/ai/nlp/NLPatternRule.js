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
// Sorted in order or preference of reporting (we prefer to report the ones with a higher number):
var DEREF_ERROR_CANNOT_PROCESS_EXPRESSION = 0;
var DEREF_ERROR_VERB_COMPLETION = 1;
var DEREF_ERROR_NO_REFERENTS = 2;
var DEREF_ERROR_CANNOT_DISAMBIGUATE = 3;
var NLParseRecord = /** @class */ (function () {
    function NLParseRecord(nt, pp, b, d, rn, p) {
        this.nextTokens = null; // this is not a flat list, but all the possible "immediate next" tokens, according to different parses
        this.previousPOS = null; // the list of POS tags of the tokens already parsed (those that have the direct token in the pattern are ignored)
        this.bindings = null;
        this.derefs = null; // this list accumulates all the successful deref operations completed during parsing up to this point
        this.ruleNames = null;
        this.priorities = null; // the priorities of all the rules used up to this point
        this.result = null;
        this.nextTokens = nt;
        this.previousPOS = pp;
        this.bindings = b;
        this.derefs = d;
        this.ruleNames = rn;
        this.priorities = p;
    }
    NLParseRecord.prototype.higherPriorityThan = function (pr2) {
        var idx = 0;
        while (true) {
            var p1 = null;
            var p2 = null;
            if (this.priorities.length >= idx + 1)
                p1 = this.priorities[idx];
            if (pr2.priorities.length >= idx + 1)
                p2 = pr2.priorities[idx];
            if (p1 == null) {
                if (p2 == null)
                    return 0;
                return 1; // p1 has higher priority
            }
            else {
                if (p2 == null)
                    return -1;
                if (p1 > p2)
                    return 1;
                if (p1 < p2)
                    return -1;
            }
            idx++;
        }
    };
    // throught the first path
    NLParseRecord.prototype.tokensLeftToParse = function () {
        if (this.nextTokens == null || this.nextTokens.length == 0)
            return 0;
        return this.nextTokens[0].tokensLeftToParse();
    };
    return NLParseRecord;
}());
var NLDerefErrorRecord = /** @class */ (function () {
    function NLDerefErrorRecord(errorType, tokensLeftToParse, previousPOS) {
        this.derefFromContextError = null;
        this.derefUniversalError = null;
        this.derefHypotheticalError = null;
        this.derefQueryError = null;
        this.previousPOS = [];
        this.derefErrorType = -1;
        this.tokensLeftToParse = -1;
        this.derefErrorType = errorType;
        this.tokensLeftToParse = tokensLeftToParse;
        this.previousPOS = previousPOS;
    }
    NLDerefErrorRecord.prototype.toString = function () {
        return "NLDerefErrorRecord(" +
            (this.derefFromContextError != null ? "context: " + this.derefFromContextError : "") +
            (this.derefUniversalError != null ? "universal: " + this.derefUniversalError : "") +
            (this.derefHypotheticalError != null ? "hypothetical: " + this.derefHypotheticalError : "") +
            (this.derefQueryError != null ? "query: " + this.derefQueryError : "") +
            ")";
    };
    NLDerefErrorRecord.prototype.equals = function (e) {
        if (this.derefErrorType != e.derefErrorType)
            return false;
        if (this.tokensLeftToParse != e.tokensLeftToParse)
            return false;
        if ((this.derefFromContextError == null) != (e.derefFromContextError == null))
            return false;
        if (this.derefFromContextError != null &&
            Term.equalsNoBindingsAttribute(this.derefFromContextError, e.derefFromContextError) != 1) {
            return false;
        }
        if ((this.derefUniversalError == null) != (e.derefUniversalError == null))
            return false;
        if (this.derefUniversalError != null &&
            Term.equalsNoBindingsAttribute(this.derefUniversalError, e.derefUniversalError) != 1) {
            return false;
        }
        if ((this.derefHypotheticalError == null) != (e.derefHypotheticalError == null))
            return false;
        if (this.derefHypotheticalError != null &&
            Term.equalsNoBindingsAttribute(this.derefHypotheticalError, e.derefHypotheticalError) != 1) {
            return false;
        }
        if ((this.derefQueryError == null) != (e.derefQueryError == null))
            return false;
        if (this.derefQueryError != null &&
            Term.equalsNoBindingsAttribute(this.derefQueryError, e.derefQueryError) != 1) {
            return false;
        }
        if (this.previousPOS.length != e.previousPOS.length)
            return false;
        for (var i = 0; i < this.previousPOS.length; i++) {
            if (!this.previousPOS[i].equals(e.previousPOS[i]))
                return false;
        }
        return true;
    };
    return NLDerefErrorRecord;
}());
var NLPatternContainer = /** @class */ (function () {
    function NLPatternContainer(name, sv, lv) {
        this.name = "";
        this.speakerVariable = null;
        this.listenerVariable = null;
        this.lastDerefErrors = [];
        this.name = name;
        this.speakerVariable = sv;
        this.listenerVariable = lv;
    }
    return NLPatternContainer;
}());
var NLPatternRule = /** @class */ (function (_super) {
    __extends(NLPatternRule, _super);
    function NLPatternRule(name, h, b, p, sv, lv) {
        var _this = _super.call(this, name, sv, lv) || this;
        _this.head = null;
        _this.body = null;
        _this.priority = 100;
        _this.head = h;
        _this.body = b;
        _this.priority = p;
        return _this;
    }
    NLPatternRule.prototype.parse = function (tokenization, filterPartialParses, context, parser, AI) {
        // parse the sentence:
        this.lastDerefErrors = [];
        var bindings = new Bindings();
        if (this.speakerVariable != null) {
            bindings.l.push([this.speakerVariable,
                new ConstantTermAttribute(context.speaker, parser.o.getSort("#id"))]);
        }
        var parses = this.body.parse(new NLParseRecord([tokenization], [], bindings, [], [], []), context, this, parser, AI);
        if (parses == null)
            return null;
        // if there is any valid parse, generate the corresponding terms:
        var results = [];
        for (var _i = 0, parses_1 = parses; _i < parses_1.length; _i++) {
            var parse = parses_1[_i];
            if (filterPartialParses &&
                parse.nextTokens != null && parse.nextTokens.length > 0)
                continue;
            parse.ruleNames = [this.name].concat(parse.ruleNames);
            parse.priorities = [this.priority].concat(parse.priorities);
            parse.result = this.head.applyBindings(parse.bindings);
            NLParser.resolveCons(parse.result, parser.o);
            results.push(parse);
        }
        return results;
    };
    NLPatternRule.prototype.parseWithBindings = function (parse, filterPartialParses, context, parser, AI) {
        var results = [];
        var parses = this.body.parse(parse, context, this, parser, AI);
        if (parses != null) {
            for (var _i = 0, parses_2 = parses; _i < parses_2.length; _i++) {
                var parse2 = parses_2[_i];
                if (filterPartialParses &&
                    parse2.nextTokens != null && parse2.nextTokens.length > 0)
                    continue;
                parse2.ruleNames = [this.name].concat(parse.ruleNames);
                parse2.priorities = [this.priority].concat(parse.priorities);
                parse2.result = this.head.applyBindings(parse2.bindings);
                NLParser.resolveCons(parse2.result, parser.o);
                results.push(parse2);
            }
        }
        return results;
    };
    NLPatternRule.prototype.clone = function () {
        var map = [];
        var head = this.head.clone(map);
        var body = this.body.clone(map);
        var rule = new NLPatternRule(this.name, head, body, this.priority, this.speakerVariable, this.listenerVariable);
        for (var i = 0; i < map.length; i++) {
            if (map[i][0] instanceof VariableTermAttribute &&
                map[i][0].name == "SPEAKER") {
                rule.speakerVariable = map[i][1];
            }
        }
        return rule;
    };
    NLPatternRule.fromString = function (name, head, body, p, o, sv, lv) {
        var variableNames = [];
        var variableValues = [];
        var h = Term.fromStringInternal(head, o, variableNames, variableValues).term;
        if (h == null) {
            console.error("NLPatternRule.fromString: cannot parse head: " + head);
            return null;
        }
        var b = NLPattern.fromString(body, o, variableNames, variableValues);
        if (b == null) {
            console.error("NLPatternRule.fromString: cannot parse body: " + body);
            return null;
        }
        if (name == null) {
            console.error("Rule without a name!");
        }
        var rule = new NLPatternRule(name, h, b, p, sv, lv);
        for (var i = 0; i < variableNames.length; i++) {
            if (variableNames[i] == "SPEAKER") {
                rule.speakerVariable = variableValues[i];
            }
        }
        return rule;
    };
    return NLPatternRule;
}(NLPatternContainer));
