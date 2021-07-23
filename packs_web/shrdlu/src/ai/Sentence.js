/*

Sentences in my sorted-logic formalism in Disjunctive Normal Form
(so that the whole knowledge base is in Conjunctive Normal Form)

*/
var Sentence = /** @class */ (function () {
    function Sentence(terms, sign) {
        this.terms = [];
        this.sign = []; // whether each term is positive or negated
        this.terms = terms;
        this.sign = sign;
    }
    /*
    // Simple inference procedure that would only work for rules that are of the form ~Term; Term
    // - If "t" matches with the negated, it returns the positive term after applying the resulting bindings
    // - Otherwise, it returns null
    singleTermForwardChaining(t:Term) : Term
    {
        let negatedTerm:Term = null;
        let positiveTerm:Term = null;
        if (this.terms.length != 2) return null;

        for(let i:number = 0;i<this.terms.length;i++) {
            if (!this.sign[i]) {
                if (negatedTerm != null) return null;	// more than one negated term in this rule
                negatedTerm = this.terms[i];
            } else {
                if (positiveTerm != null) return null;	// more than one positive term in this rule
                positiveTerm = this.terms[i];
            }
        }
        let bindings:Bindings = new Bindings();
        if (t.unify(negatedTerm, OCCURS_CHECK, bindings)) {
            let t2:Term = positiveTerm.applyBindings(bindings);
            return t2;
        }
        return null;
    }
    */
    Sentence.prototype.applyBindings = function (b) {
        var s = new Sentence(this.terms, this.sign);
        for (var i = 0; i < s.terms.length; i++) {
            s.terms[i] = s.terms[i].applyBindings(b);
        }
        return s;
    };
    Sentence.prototype.getAllVariables = function () {
        var vs = [];
        for (var _i = 0, _a = this.terms; _i < _a.length; _i++) {
            var t = _a[_i];
            var tvs = t.getAllVariables();
            //			console.log("Sentence.getAllVariables(t): " + tvs);
            for (var _b = 0, tvs_1 = tvs; _b < tvs_1.length; _b++) {
                var v = tvs_1[_b];
                if (vs.indexOf(v) == -1)
                    vs.push(v);
            }
        }
        return vs;
    };
    Sentence.prototype.negate = function () {
        var sentences = [];
        for (var i = 0; i < this.terms.length; i++) {
            var t = this.terms[i];
            var s = this.sign[i];
            sentences.push(new Sentence([t], [!s]));
        }
        return sentences;
    };
    // checks if "this" is a subset of "s":
    Sentence.prototype.subsetNoBindings = function (s) {
        if (this.terms.length > s.terms.length)
            return false;
        for (var i = 0; i < this.terms.length; i++) {
            var found = false;
            for (var j = 0; j < s.terms.length; j++) {
                if (this.sign[i] == s.sign[j] &&
                    this.terms[i].equalsNoBindings(s.terms[j]) == 1) {
                    found = true;
                    break;
                }
            }
            if (!found)
                return false;
        }
        return true;
    };
    Sentence.prototype.equalsNoBindings = function (s) {
        if (this.terms.length != s.terms.length)
            return false;
        for (var i = 0; i < this.terms.length; i++) {
            if (this.sign[i] != s.sign[i])
                return false;
            if (this.terms[i].equalsNoBindings(s.terms[i]) != 1)
                return false;
        }
        return true;
    };
    Sentence.prototype.toString = function () {
        var variables = [];
        var variableNames = [];
        var str = "";
        var first = true;
        for (var i = 0; i < this.terms.length; i++) {
            var t = this.terms[i];
            if (first) {
                if (!this.sign[i])
                    str += "~";
                str += t.toStringInternal(variables, variableNames);
                first = false;
            }
            else {
                str += "; ";
                if (!this.sign[i])
                    str += "~";
                str += t.toStringInternal(variables, variableNames);
            }
        }
        return str;
    };
    Sentence.prototype.toStringXML = function () {
        return this.toStringXMLInternal([], []);
    };
    Sentence.prototype.toStringXMLInternal = function (variables, variableNames) {
        var str = "";
        var first = true;
        for (var i = 0; i < this.terms.length; i++) {
            var t = this.terms[i];
            if (first) {
                if (!this.sign[i])
                    str += "~";
                str += t.toStringXMLInternal(variables, variableNames);
                first = false;
            }
            else {
                str += "; ";
                if (!this.sign[i])
                    str += "~";
                str += t.toStringXMLInternal(variables, variableNames);
            }
        }
        return str;
    };
    Sentence.fromString = function (str, o) {
        return Sentence.fromStringInternal(str, o, [], []);
    };
    Sentence.fromStringInternal = function (str, o, variableNames, variableValues) {
        var tokens = [];
        var token = "";
        var c;
        var state = 0; // 0: no token character yet, 1: inside a token
        var parenthesis = 0;
        var squareBrackets = 0;
        var quotation = false;
        // separate the string in tokens:
        // each token can be: semicolon, ~, or a term
        for (var i = 0; i < str.length; i++) {
            c = str.charAt(i);
            if (c == ';' || c == '~') {
                if (state == 0) {
                    tokens.push(c);
                    token = "";
                }
                else if (state == 1) {
                    if (parenthesis == 0 && squareBrackets == 0 && !quotation) {
                        // end of token!
                        tokens.push(token.trim());
                        tokens.push(c);
                        token = "";
                        state = 0;
                    }
                    else {
                        token += c;
                    }
                }
            }
            else if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                if (state == 0) {
                    // ignore
                }
                else if (state == 1) {
                    if (quotation) {
                        token += c;
                    }
                    else if (parenthesis == 0 && squareBrackets == 0 && !quotation) {
                        // end of token!
                        tokens.push(token.trim());
                        //						console.log("token: " + token);
                        token = "";
                        state = 0;
                    }
                }
            }
            else {
                if (c == "\'")
                    quotation = !quotation;
                if (!quotation) {
                    if (c == '(')
                        parenthesis++;
                    if (c == ')')
                        parenthesis--;
                    if (c == '[')
                        squareBrackets++;
                    if (c == ']')
                        squareBrackets--;
                }
                token += c;
                state = 1;
            }
        }
        if (state == 1) {
            if (parenthesis == 0 && squareBrackets == 0 && !quotation) {
                tokens.push(token.trim());
                //console.log("token: " + token);
            }
            else {
                console.error("Sentence.fromString: unfinished sentence! " + str);
                return null;
            }
        }
        //		for(let t of tokens) {
        //			console.log("token: " + t);
        //		}
        // check that the sequence is correct: term [[~]term [; [~]term]*]
        var s = new Sentence([], []);
        var sign = true;
        state = 0;
        for (var i = 0; i < tokens.length; i++) {
            if (state == 0) {
                if (tokens[i] == "~") {
                    sign = false;
                    state = 1;
                    continue;
                }
            }
            if (state == 0 || state == 1) {
                if (tokens[i] == "~") {
                    console.error("Sentence.fromString: two negations in a row!!");
                    return null;
                }
                if (tokens[i] == ";") {
                    console.error("Sentence.fromString: semicolon found too early!");
                    return null;
                }
                var ta = Term.fromStringInternal(tokens[i], o, variableNames, variableValues);
                if (ta == null) {
                    console.error("Error parsing sentence: " + str);
                    return null;
                }
                var t = ta.term;
                if (t == null)
                    return null;
                s.terms.push(t);
                s.sign.push(sign);
                state = 2;
                sign = true;
                continue;
            }
            if (state == 2) {
                if (tokens[i] != ';') {
                    console.error("Sentence.fromString: expected semicolon after term and found: " + tokens[i]);
                    return null;
                }
                state = 0;
            }
        }
        return s;
    };
    return Sentence;
}());
