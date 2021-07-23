var Expression = /** @class */ (function () {
    function Expression() {
        this.head = null;
        this.quoted = false;
        this.parameters = [];
    }
    Expression.fromString = function (str) {
        //        console.log("Expression.fromString " + str);
        return Expression.fromStringOffset(str, 0)[0];
    };
    Expression.fromStringOffset = function (str, pos) {
        var pos0 = pos;
        var tmp = Expression.nextToken(str, pos);
        if (tmp == null)
            return null;
        var token = tmp[0];
        pos = tmp[1];
        var stack = [];
        var last = null;
        var current = null;
        while (token != null) {
            //            console.log("next token: " + token + ", offset " + pos);
            if (Expression.isSymbol(token)) {
                last = new Expression();
                last.head = token;
                if (current != null)
                    current.parameters.push(last);
                if (token == "new") {
                    var tmp2 = Expression.fromStringOffset(str, pos);
                    if (tmp2 != null) {
                        pos = tmp2[1];
                        last.parameters.push(tmp2[0]);
                    }
                    else {
                        console.error("Expression.from_string: illegal expression after 'new': " + str.substring(pos0));
                        break;
                    }
                    if (stack.length == 0)
                        return [last, pos];
                }
            }
            else if (token.charAt(0) == '(') {
                if (last != null) {
                    stack.push(last);
                    current = last;
                }
                else {
                    console.error("Expression::from_string: illegal expression '(': " + str.substring(pos0));
                    break;
                }
            }
            else if (token.charAt(0) == ')') {
                stack.splice(stack.length - 1, 1);
                if (stack.length > 0) {
                    current = stack[stack.length - 1];
                }
                else {
                    return [current, pos];
                }
            }
            else if (token.charAt(0) == ',') {
                // ... (ignore for now)
            }
            else if (token.charCodeAt(0) >= '0'.charCodeAt(0) && token.charCodeAt(0) <= '9'.charCodeAt(0)) {
                last = new Expression();
                last.head = token;
                if (current != null)
                    current.parameters.push(last);
            }
            else if (token.charAt(0) == '?') {
                last = new Expression();
                last.head = token;
                if (current != null)
                    current.parameters.push(last);
            }
            else if (token.charAt(0) == '\'' || token.charAt(0) == '\"') {
                token = token.substring(1, token.length - 1); // remove the quotes
                last = new Expression();
                last.head = token;
                last.quoted = true;
                if (current != null)
                    current.parameters.push(last);
            }
            else {
                console.error("Expression::from_string: unrecognized token: " + token);
                break;
            }
            tmp = Expression.nextToken(str, pos);
            if (tmp == null)
                break;
            token = tmp[0];
            pos = tmp[1];
        }
        console.error("Expression.from_string: illegal expression start: " + str.substring(pos0));
        return [null, pos];
    };
    Expression.listFromString = function (str) {
        return Expression.listFromStringOffset(str, 0)[0];
    };
    Expression.listFromStringOffset = function (str, pos) {
        var l = [];
        while (pos < str.length) {
            var tmp = Expression.fromStringOffset(str, pos);
            if (tmp != null) {
                l.push(tmp[0]);
                pos = tmp[1];
            }
            else {
                return [l, pos];
            }
            while (str.charAt(pos) == ' ' || str.charAt(pos) == ',')
                pos++;
        }
        return [l, pos];
    };
    Expression.nextToken = function (str, pos) {
        var buffer = "";
        // skip spaces:
        var c = str.charAt(pos);
        pos++;
        while (c == ' ' || c == '\t') {
            c = str.charAt(pos);
            pos++;
        }
        // determine token type:
        if (c == '') {
            return null;
        }
        else if (c == ',' || c == '(' || c == ')') {
            return [c, pos];
        }
        else if (c == '\"') {
            buffer += c;
            c = str.charAt(pos);
            pos++;
            while (c != '\"') {
                buffer += c;
                c = str.charAt(pos);
                pos++;
            }
            buffer += '\"';
            return [buffer, pos];
        }
        else if (c == '\'') {
            buffer += c;
            c = str.charAt(pos);
            pos++;
            while (c != '\'') {
                buffer += c;
                c = str.charAt(pos);
                pos++;
            }
            buffer += '\"';
            return [buffer, pos];
        }
        else if (Expression.isSymbolStartCharacter(c.charCodeAt(0))) {
            while (Expression.isSymbolCharacter(c.charCodeAt(0))) {
                buffer += c;
                c = str.charAt(pos);
                pos++;
            }
            pos--;
            return [buffer, pos];
        }
        else if (c.charCodeAt(0) >= '0'.charCodeAt(0) && c.charCodeAt(0) <= '9'.charCodeAt(0)) {
            while ((c.charCodeAt(0) >= '0'.charCodeAt(0) && c.charCodeAt(0) <= '9'.charCodeAt(0)) || c.charCodeAt(0) == '.'.charCodeAt(0)) {
                buffer += c;
                c = str.charAt(pos);
                pos++;
            }
            pos--;
            return [buffer, pos];
        }
        else if (c == '&' &&
            str.charAt(pos) == 'q' &&
            str.charAt(pos + 1) == 'u' &&
            str.charAt(pos + 2) == 'o' &&
            str.charAt(pos + 3) == 't' &&
            str.charAt(pos + 4) == ';') {
            pos += 5;
            buffer += '\"';
            c = str.charAt(pos);
            pos++;
            while (c != '&') {
                buffer += c;
                c = str.charAt(pos);
                pos++;
            }
            buffer += '\"';
            pos += 5; // skip the "quot;"
            return [buffer, pos];
        }
        else if (c == '?') {
            buffer += c;
            c = str.charAt(pos);
            pos++;
            while (c.charCodeAt(0) >= '0'.charCodeAt(0) && c.charCodeAt(0) <= '9'.charCodeAt(0)) {
                buffer += c;
                c = str.charAt(pos);
                pos++;
            }
            pos--;
            return [buffer, pos];
        }
        else {
            console.error("Expression.next_token: Token starts with illegal character '" + c + "'");
            return null;
        }
    };
    Expression.isSymbol = function (token) {
        return Expression.isSymbolStartCharacter(token.charCodeAt(0));
    };
    Expression.isSymbolCharacter = function (c) {
        return (c >= 'a'.charCodeAt(0) && c <= 'z'.charCodeAt(0)) ||
            (c >= 'A'.charCodeAt(0) && c <= 'Z'.charCodeAt(0)) ||
            c == '.'.charCodeAt(0) ||
            c == '_'.charCodeAt(0) ||
            c == '-'.charCodeAt(0) ||
            (c >= '0'.charCodeAt(0) && c <= '9'.charCodeAt(0)) ||
            c == ':'.charCodeAt(0);
    };
    Expression.isSymbolStartCharacter = function (c) {
        return (c >= 'a'.charCodeAt(0) && c <= 'z'.charCodeAt(0)) ||
            (c >= 'A'.charCodeAt(0) && c <= 'Z'.charCodeAt(0)) ||
            c == '_'.charCodeAt(0);
    };
    Expression.prototype.toString = function () {
        var s = (this.quoted ? "\"" + this.head + "\"" : this.head);
        if (this.parameters.length > 0) {
            s += "(";
            for (var i = 0; i < this.parameters.length; i++) {
                if (i > 0)
                    s += ",";
                s += this.parameters.toString();
            }
            s += ")";
        }
        return s;
    };
    return Expression;
}());
