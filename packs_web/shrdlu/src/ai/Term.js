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
var Bindings = /** @class */ (function () {
    function Bindings() {
        this.l = [];
    }
    Bindings.prototype.concat = function (b) {
        if (b == null)
            return null;
        var result = new Bindings();
        //result.l = this.l.concat(b.l);
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var _b = _a[_i], variable = _b[0], value = _b[1];
            result.l.push([variable, value.applyBindings(b)]);
        }
        for (var _c = 0, _d = b.l; _c < _d.length; _c++) {
            var binding = _d[_c];
            var found = false;
            for (var _e = 0, _f = result.l; _e < _f.length; _e++) {
                var tmp = _f[_e];
                var variable2 = tmp[0];
                var value2 = tmp[1];
                if (binding[0] == variable2) {
                    var cmp = Term.equalsNoBindingsAttribute(binding[1], value2);
                    if (cmp == 1) {
                        found = true;
                    }
                    else if (cmp == -1) {
                        return null;
                    }
                    else {
                        found = true;
                        tmp[1] = value2;
                    }
                }
            }
            if (!found)
                result.l.push(binding);
        }
        return result;
    };
    Bindings.prototype.removeUselessBindings = function (variables) {
        var new_l = [];
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var _b = _a[_i], variable = _b[0], value = _b[1];
            if (variables.indexOf(variable) != -1) {
                new_l.push([variable, value.applyBindings(this)]);
            }
        }
        this.l = new_l;
    };
    Bindings.prototype.removeUselessBindingsSentence = function (s, variables) {
        var s_variables = s.getAllVariables();
        var new_l = [];
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var _b = _a[_i], variable = _b[0], value = _b[1];
            if (s_variables.indexOf(variable) != -1 ||
                variables.indexOf(variable) != -1) {
                new_l.push([variable, value.applyBindings(this)]);
            }
        }
        this.l = new_l;
    };
    Bindings.prototype.getValueForVariableName = function (vName) {
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var b = _a[_i];
            if (b[0].name == vName)
                return b[1];
        }
        return null;
    };
    Bindings.prototype.toString = function () {
        var str = "[ ";
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var b = _a[_i];
            var str2 = "(";
            if (b[0].name != null) {
                str2 += b[0].name;
            }
            else {
                str2 += "_";
            }
            str2 += " = " + b[1].toString() + ")";
            str += str2 + " ";
        }
        return str + "]";
    };
    Bindings.prototype.toStringWithMappings = function (variables, variableNames) {
        var str = "[ ";
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var b = _a[_i];
            var str2 = "(";
            str2 += Term.variableNameForPrinting(b[0], variables, variableNames);
            str2 += " = " + Term.variableNameForPrinting(b[1], variables, variableNames) + ":" + b[1].toStringInternal(variables, variableNames) + ")";
            str += str2 + " ";
        }
        return str + "]";
    };
    Bindings.prototype.equals = function (b) {
        if (this.l.length != b.l.length)
            return false;
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var _b = _a[_i], variable = _b[0], value = _b[1];
            var found = false;
            for (var _c = 0, _d = b.l; _c < _d.length; _c++) {
                var _e = _d[_c], variable2 = _e[0], value2 = _e[1];
                if (variable == variable2) {
                    found = true;
                    if (Term.equalsNoBindingsAttribute(value, value2) != 1)
                        return false;
                }
            }
            if (!found)
                return false;
        }
        return true;
    };
    // if "this" a subset of "b"?
    Bindings.prototype.subset = function (b) {
        if (this.l.length > b.l.length)
            return false;
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var _b = _a[_i], variable = _b[0], value = _b[1];
            var found = false;
            for (var _c = 0, _d = b.l; _c < _d.length; _c++) {
                var _e = _d[_c], variable2 = _e[0], value2 = _e[1];
                if (variable == variable2) {
                    found = true;
                    if (Term.equalsNoBindingsAttribute(value, value2) != 1)
                        return false;
                }
            }
            if (!found)
                return false;
        }
        return true;
    };
    Bindings.prototype.clone = function () {
        var b = new Bindings();
        for (var _i = 0, _a = this.l; _i < _a.length; _i++) {
            var pair = _a[_i];
            b.l.push(pair);
        }
        return b;
    };
    return Bindings;
}());
var TermAttribute = /** @class */ (function () {
    function TermAttribute(sort) {
        this.sort = null;
        // for debugging purposes, remove after I'm done:
        this.ID = TermAttribute.next_ID++;
        this.sort = sort;
    }
    TermAttribute.prototype.toString = function () {
        return this.toStringInternal([], []);
    };
    TermAttribute.prototype.toStringXML = function () {
        return this.toStringXMLInternal([], []);
    };
    TermAttribute.next_ID = 0;
    return TermAttribute;
}());
var ConstantTermAttribute = /** @class */ (function (_super) {
    __extends(ConstantTermAttribute, _super);
    function ConstantTermAttribute(value, sort) {
        var _this = _super.call(this, sort) || this;
        _this.value = null;
        _this.value = value;
        return _this;
        // debug:
        // if (sort.name == "#id") {
        //     if (typeof value === 'string') {
        //         // ...
        //     } else {
        //         console.error("ID is not a string!");
        //     }
        // }
    }
    ConstantTermAttribute.prototype.occursCheck = function (v, bindings) {
        return false;
    };
    ConstantTermAttribute.prototype.applyBindings = function (bindings) {
        return this;
    };
    ConstantTermAttribute.prototype.applyBindings_internal = function (bindings, map) {
        return this;
    };
    ConstantTermAttribute.prototype.clone = function (map) {
        return this; // this is a constant, no need to clone
    };
    ConstantTermAttribute.prototype.cloneKeepingVariables = function (map) {
        return this; // this is a constant, no need to clone
    };
    ConstantTermAttribute.prototype.toStringInternal = function (variables, variableNames) {
        return "'" + this.value + "'[" + this.sort.name + "]";
    };
    ConstantTermAttribute.prototype.toStringXMLInternal = function (variables, variableNames) {
        if ((typeof this.value) == "string") {
            return "'" + this.value.replace("'", "\\039") + "'[" + this.sort.name + "]";
        }
        else {
            return "'" + this.value + "'[" + this.sort.name + "]";
        }
    };
    return ConstantTermAttribute;
}(TermAttribute));
var VariableTermAttribute = /** @class */ (function (_super) {
    __extends(VariableTermAttribute, _super);
    function VariableTermAttribute(sort, name) {
        var _this = _super.call(this, sort) || this;
        _this.name = null;
        _this.name = name;
        return _this;
    }
    VariableTermAttribute.prototype.occursCheck = function (v, bindings) {
        if (this == v)
            return true;
        for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
            var binding = _a[_i];
            if (this == binding[0]) {
                return binding[1].occursCheck(v, bindings);
            }
        }
        return false;
    };
    VariableTermAttribute.prototype.applyBindings = function (bindings) {
        for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
            var b = _a[_i];
            if (b[0] == this) {
                return b[1].applyBindings(bindings);
            }
        }
        return this;
    };
    VariableTermAttribute.prototype.applyBindings_internal = function (bindings, map) {
        for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
            var b = _a[_i];
            if (b[0] == this) {
                return b[1].applyBindings_internal(bindings, map);
            }
        }
        return this;
    };
    VariableTermAttribute.prototype.clone = function (map) {
        for (var _i = 0, map_1 = map; _i < map_1.length; _i++) {
            var _a = map_1[_i], v1 = _a[0], v2 = _a[1];
            if (v1 == this)
                return v2;
        }
        var v = new VariableTermAttribute(this.sort, this.name);
        map.push([this, v]);
        return v;
    };
    VariableTermAttribute.prototype.cloneKeepingVariables = function (map) {
        return this;
    };
    VariableTermAttribute.prototype.toStringInternal = function (variables, variableNames) {
        //        if (this.name != null) return this.name + ":[" + this.sort.name + "]";
        return "[" + this.sort.name + "]";
    };
    VariableTermAttribute.prototype.toStringXMLInternal = function (variables, variableNames) {
        //        if (this.name != null) return this.name + ":[" + this.sort.name + "]";
        return "[" + this.sort.name + "]";
    };
    return VariableTermAttribute;
}(TermAttribute));
var TermTermAttribute = /** @class */ (function (_super) {
    __extends(TermTermAttribute, _super);
    function TermTermAttribute(term) {
        var _this = _super.call(this, term.functor) || this;
        _this.term = null;
        _this.term = term;
        return _this;
    }
    TermTermAttribute.prototype.occursCheck = function (v, bindings) {
        for (var _i = 0, _a = this.term.attributes; _i < _a.length; _i++) {
            var a = _a[_i];
            if (a.occursCheck(v, bindings))
                return true;
        }
        return false;
    };
    TermTermAttribute.prototype.applyBindings = function (bindings) {
        return new TermTermAttribute(this.term.applyBindings(bindings));
    };
    TermTermAttribute.prototype.applyBindings_internal = function (bindings, map) {
        return this.term.applyBindings_internal(bindings, map);
    };
    TermTermAttribute.prototype.clone = function (map) {
        for (var _i = 0, map_2 = map; _i < map_2.length; _i++) {
            var _a = map_2[_i], v1 = _a[0], v2 = _a[1];
            if (v1 == this)
                return v2;
        }
        var v = new TermTermAttribute(this.term.clone(map));
        map.push([this, v]);
        return v;
    };
    TermTermAttribute.prototype.cloneKeepingVariables = function (map) {
        for (var _i = 0, map_3 = map; _i < map_3.length; _i++) {
            var _a = map_3[_i], v1 = _a[0], v2 = _a[1];
            if (v1 == this)
                return v2;
        }
        var v = new TermTermAttribute(this.term.cloneKeepingVariables(map));
        map.push([this, v]);
        return v;
    };
    TermTermAttribute.prototype.toStringInternal = function (variables, variableNames) {
        return this.term.toStringInternal(variables, variableNames);
    };
    TermTermAttribute.prototype.toStringXMLInternal = function (variables, variableNames) {
        return this.term.toStringXMLInternal(variables, variableNames);
    };
    return TermTermAttribute;
}(TermAttribute));
var Term = /** @class */ (function () {
    function Term(functor, attributes) {
        this.functor = null;
        this.attributes = [];
        this.functor = functor;
        this.attributes = attributes;
    }
    Term.prototype.addAttribute = function (p) {
        this.attributes.push(p);
    };
    Term.prototype.unify = function (t, occursCheck, bindings) {
        // if they have a different number of attribetus -> return false
        if (this.attributes.length != t.attributes.length)
            return false;
        // if functors do not match (one should subsume the other) -> return false
        if (!this.functor.is_a(t.functor) && !t.functor.is_a(this.functor))
            return false;
        // for each attribute:
        for (var i = 0; i < this.attributes.length; i++) {
            var att1 = this.attributes[i];
            var att2 = t.attributes[i];
            if (!Term.unifyAttribute(att1, att2, occursCheck, bindings))
                return false;
        }
        return true;
    };
    Term.prototype.unifySameFunctor = function (t, occursCheck, bindings) {
        // if they have a different number of attribetus -> return false
        if (this.attributes.length != t.attributes.length)
            return false;
        if (this.functor != t.functor)
            return false;
        // for each attribute:
        for (var i = 0; i < this.attributes.length; i++) {
            var att1 = this.attributes[i];
            var att2 = t.attributes[i];
            if (!Term.unifyAttributeSameFunctor(att1, att2, occursCheck, bindings))
                return false;
        }
        return true;
    };
    Term.prototype.unifyIgnoringFirstfunctor = function (t, occursCheck, bindings) {
        // if they have a different number of attribetus -> return false
        if (this.attributes.length != t.attributes.length)
            return false;
        // for each attribute:
        for (var i = 0; i < this.attributes.length; i++) {
            var att1 = this.attributes[i];
            var att2 = t.attributes[i];
            if (!Term.unifyAttributeSameFunctor(att1, att2, occursCheck, bindings))
                return false;
        }
        return true;
    };
    Term.unifyAttribute = function (att1, att2, occursCheck, bindings) {
        if (att1 == att2)
            return true;
        // - first of all, apply bindings (in order) if any is a variable to construct temporary terms
        if (att1 instanceof VariableTermAttribute) {
            for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
                var pair = _a[_i];
                if (pair[0] == att1) {
                    return Term.unifyAttribute(pair[1], att2, occursCheck, bindings);
                }
            }
        }
        if (att2 instanceof VariableTermAttribute) {
            for (var _b = 0, _c = bindings.l; _b < _c.length; _b++) {
                var pair = _c[_b];
                if (pair[0] == att2) {
                    return this.unifyAttribute(att1, pair[1], occursCheck, bindings);
                }
            }
        }
        // - if they are both constants, and are different -> return false
        if (att1 instanceof ConstantTermAttribute) {
            if (att2 instanceof ConstantTermAttribute) {
                if (att1.value != att2.value)
                    return false;
                if (!att1.sort.is_a(att2.sort) && !att2.sort.is_a(att1.sort))
                    return false;
            }
            else if (att2 instanceof TermTermAttribute) {
                return false;
            }
        }
        // - if they are both terms -> recursive call
        if (att1 instanceof TermTermAttribute) {
            if (att2 instanceof TermTermAttribute) {
                if (!att1.term.unify(att2.term, occursCheck, bindings))
                    return false;
            }
            else if (att2 instanceof ConstantTermAttribute) {
                return false;
            }
        }
        // - if one of them is a variable of a more general or equal sort than the other and that 
        //   does not occur inside the other (occurs check) -> add binding
        if ((att1 instanceof VariableTermAttribute) ||
            (att2 instanceof VariableTermAttribute)) {
            if ((att1 instanceof VariableTermAttribute) && att1.sort.subsumes(att2.sort)) {
                if (occursCheck && att2.occursCheck(att1, bindings))
                    return false;
                bindings.l.push([att1, att2]);
                return true;
            }
            if ((att2 instanceof VariableTermAttribute) && att2.sort.subsumes(att1.sort)) {
                if (occursCheck && att1.occursCheck(att2, bindings))
                    return false;
                bindings.l.push([att2, att1]);
                return true;
            }
            return false;
        }
        return true;
    };
    Term.unifyAttributeSameFunctor = function (att1, att2, occursCheck, bindings) {
        if (att1 == att2)
            return true;
        // - first of all, apply bindings (in order) if any is a variable to construct temporary terms
        if (att1 instanceof VariableTermAttribute) {
            for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
                var pair = _a[_i];
                if (pair[0] == att1) {
                    return Term.unifyAttributeSameFunctor(pair[1], att2, occursCheck, bindings);
                }
            }
        }
        if (att2 instanceof VariableTermAttribute) {
            for (var _b = 0, _c = bindings.l; _b < _c.length; _b++) {
                var pair = _c[_b];
                if (pair[0] == att2) {
                    return this.unifyAttributeSameFunctor(att1, pair[1], occursCheck, bindings);
                }
            }
        }
        // - if they are both constants, and are different -> return false
        if (att1 instanceof ConstantTermAttribute) {
            if (att2 instanceof ConstantTermAttribute) {
                if (att1.value != att2.value)
                    return false;
                if (!att1.sort.is_a(att2.sort) && !att2.sort.is_a(att1.sort))
                    return false;
            }
            else if (att2 instanceof TermTermAttribute) {
                return false;
            }
        }
        // - if they are both terms -> recursive call
        if (att1 instanceof TermTermAttribute) {
            if (att2 instanceof TermTermAttribute) {
                if (!att1.term.unifySameFunctor(att2.term, occursCheck, bindings))
                    return false;
            }
            else if (att2 instanceof ConstantTermAttribute) {
                return false;
            }
        }
        // - if one of them is a variable of a more general or equal sort than the other and that 
        //   does not occur inside the other (occurs check) -> add binding
        if ((att1 instanceof VariableTermAttribute) ||
            (att2 instanceof VariableTermAttribute)) {
            if ((att1 instanceof VariableTermAttribute) && att1.sort.subsumes(att2.sort)) {
                if (occursCheck && att2.occursCheck(att1, bindings))
                    return false;
                bindings.l.push([att1, att2]);
                return true;
            }
            if ((att2 instanceof VariableTermAttribute) && att2.sort.subsumes(att1.sort)) {
                if (occursCheck && att1.occursCheck(att2, bindings))
                    return false;
                bindings.l.push([att2, att1]);
                return true;
            }
            return false;
        }
        return true;
    };
    Term.prototype.subsumes = function (t, occursCheck, bindings) {
        if (this.functor.name == "$and" ||
            t.functor.name == "#and") {
            var tl1 = void 0;
            var tl2 = void 0;
            if (this.functor.name == "#and") {
                tl1 = Term.elementsInAndList(this);
            }
            else {
                tl1 = [new TermTermAttribute(this)];
            }
            if (t.functor.name == "#and") {
                tl2 = Term.elementsInAndList(t);
            }
            else {
                tl2 = [new TermTermAttribute(t)];
            }
            for (var _i = 0, tl1_1 = tl1; _i < tl1_1.length; _i++) {
                var t1 = tl1_1[_i];
                var found = false;
                for (var _a = 0, tl2_1 = tl2; _a < tl2_1.length; _a++) {
                    var t2 = tl2_1[_a];
                    var bl = bindings.l.length;
                    if ((t1 instanceof TermTermAttribute) &&
                        (t2 instanceof TermTermAttribute)) {
                        if (t1.term.subsumesInternal(t2.term, occursCheck, bindings)) {
                            found = true;
                            break;
                        }
                    }
                    else {
                        if (Term.subsumesAttribute(t1, t2, occursCheck, bindings)) {
                            found = true;
                            break;
                        }
                    }
                    bindings.l.length = bl; // remove all the bindings that were created in the failed subsumption attempt
                }
                if (!found)
                    return false;
            }
            return true;
        }
        else {
            return this.subsumesInternal(t, occursCheck, bindings);
        }
    };
    Term.prototype.subsumesInternal = function (t, occursCheck, bindings) {
        // if they have a different number of attribetus -> return false
        if (this.attributes.length != t.attributes.length)
            return false;
        // if functors do not match (one should subsume the other) -> return false
        if (!t.functor.is_a(this.functor))
            return false;
        // for each attribute:
        for (var i = 0; i < this.attributes.length; i++) {
            var att1 = this.attributes[i];
            var att2 = t.attributes[i];
            if (!Term.subsumesAttribute(att1, att2, occursCheck, bindings))
                return false;
        }
        return true;
    };
    Term.subsumesAttribute = function (att1, att2, occursCheck, bindings) {
        if (att1 == att2)
            return true;
        // - first of all, apply bindings (in order) if any is a variable to construct temporary terms
        if (att1 instanceof VariableTermAttribute) {
            for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
                var pair = _a[_i];
                if (pair[0] == att1) {
                    return Term.subsumesAttribute(pair[1], att2, occursCheck, bindings);
                }
            }
        }
        if (att2 instanceof VariableTermAttribute) {
            for (var _b = 0, _c = bindings.l; _b < _c.length; _b++) {
                var pair = _c[_b];
                if (pair[0] == att2) {
                    return this.subsumesAttribute(att1, pair[1], occursCheck, bindings);
                }
            }
        }
        // - if they are both constants, and are different -> return false
        if (att1 instanceof ConstantTermAttribute) {
            if (att2 instanceof ConstantTermAttribute) {
                if (att1.value != att2.value)
                    return false;
                if (!att1.sort.is_a(att2.sort) && !att2.sort.is_a(att1.sort))
                    return false;
            }
            else if (att2 instanceof TermTermAttribute) {
                return false;
            }
        }
        // - if they are both terms -> recursive call
        if (att1 instanceof TermTermAttribute) {
            if (att2 instanceof TermTermAttribute) {
                if (!att1.term.subsumes(att2.term, occursCheck, bindings))
                    return false;
            }
            else if (att2 instanceof ConstantTermAttribute) {
                return false;
            }
        }
        // - if one of them is a variable of a more general or equal sort than the other and that 
        //   does not occur inside the other (occurs check) -> add binding
        if ((att1 instanceof VariableTermAttribute) ||
            (att2 instanceof VariableTermAttribute)) {
            if ((att1 instanceof VariableTermAttribute) && att1.sort.subsumes(att2.sort)) {
                if (occursCheck && att2.occursCheck(att1, bindings))
                    return false;
                bindings.l.push([att1, att2]);
                return true;
            }
            return false;
        }
        return true;
    };
    Term.prototype.equalsBindings = function (t) {
        var b = new Bindings();
        if (this.equalsInternal(t, b))
            return b;
        return null;
    };
    Term.prototype.equals = function (t) {
        return this.equalsInternal(t, new Bindings());
    };
    Term.prototype.equalsInternal = function (t, bindings) {
        // if they have a different number of attributes -> return false
        if (this.attributes.length != t.attributes.length)
            return false;
        // if functors do not match -> return false
        if (this.functor != t.functor)
            return false;
        // for each attribute:
        for (var i = 0; i < this.attributes.length; i++) {
            var att1 = this.attributes[i];
            var att2 = t.attributes[i];
            if (!Term.equalsAttribute(att1, att2, bindings))
                return false;
        }
        return true;
    };
    // same as equals, but considers the #and functor in a spceial way, so that #and(X, #and(Y,Z)) is the same as #and(#and(X, Y), Z)
    Term.prototype.equalsConsideringAndList = function (t) {
        return this.equalsConsideringAndListInternal(t, new Bindings());
    };
    Term.prototype.equalsConsideringAndListInternal = function (t, bindings) {
        // if they have a different number of attributes -> return false
        if (this.attributes.length != t.attributes.length)
            return false;
        // if functors do not match -> return false
        if (this.functor != t.functor)
            return false;
        if (this.functor.name == "#and" ||
            this.functor.name == "#list") {
            // special case!
            var tl1 = Term.elementsInList(this, this.functor.name);
            var tl2 = Term.elementsInList(t, this.functor.name);
            if (tl1.length != tl2.length)
                return false;
            for (var _i = 0, tl1_2 = tl1; _i < tl1_2.length; _i++) {
                var t1 = tl1_2[_i];
                var found = null;
                for (var _a = 0, tl2_2 = tl2; _a < tl2_2.length; _a++) {
                    var t2 = tl2_2[_a];
                    var bl = bindings.l.length;
                    if (Term.equalsAttributeConsideringAndList(t1, t2, bindings)) {
                        found = t2;
                        break;
                    }
                    bindings.l.length = bl; // remove all the bindings that were created 
                }
                if (found == null)
                    return false;
                tl2.splice(tl2.indexOf(found), 1);
            }
        }
        else {
            // for each attribute:
            for (var i = 0; i < this.attributes.length; i++) {
                var att1 = this.attributes[i];
                var att2 = t.attributes[i];
                if (!Term.equalsAttributeConsideringAndList(att1, att2, bindings))
                    return false;
            }
        }
        return true;
    };
    Term.elementsInAndList = function (list) {
        return Term.elementsInList(list, "#and");
    };
    Term.elementsInListList = function (list) {
        return Term.elementsInList(list, "#list");
    };
    Term.elementsInList = function (list, listFunctor) {
        var output = [];
        if (list.functor.name == listFunctor) {
            for (var i = 0; i < list.attributes.length; i++) {
                if ((list.attributes[i] instanceof TermTermAttribute) &&
                    list.attributes[i].term.functor.name == listFunctor) {
                    output = output.concat(Term.elementsInList((list.attributes[i]).term, listFunctor));
                }
                else {
                    output.push(list.attributes[i]);
                }
            }
        }
        else {
            output.push(new TermTermAttribute(list));
        }
        return output;
    };
    Term.equalsAttribute = function (att1, att2, bindings) {
        // - if they are both constants, and are different -> return false
        if ((att1 instanceof ConstantTermAttribute) &&
            (att2 instanceof ConstantTermAttribute)) {
            if (att1.value != att2.value)
                return false;
            if (att1.sort != att2.sort)
                return false;
            return true;
        }
        // - if they are both terms -> recursive call
        if ((att1 instanceof TermTermAttribute) &&
            (att2 instanceof TermTermAttribute)) {
            return att1.term.equalsInternal(att2.term, bindings);
        }
        // - if one of them is a variable that does not occur inside the other (occurs check) -> add binding
        if (att1 instanceof VariableTermAttribute &&
            att2 instanceof VariableTermAttribute) {
            if (att1.sort != att2.sort)
                return false;
            var found = false;
            for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
                var _b = _a[_i], v1 = _b[0], v2 = _b[1];
                if (v1 == att1) {
                    if (v2 != att2)
                        return false;
                    found = true;
                }
            }
            if (!found && att1 != att2)
                bindings.l.push([att1, att2]);
            return true;
        }
        return false;
    };
    Term.equalsAttributeConsideringAndList = function (att1, att2, bindings) {
        // - if they are both constants, and are different -> return false
        if ((att1 instanceof ConstantTermAttribute) &&
            (att2 instanceof ConstantTermAttribute)) {
            if (att1.value != att2.value)
                return false;
            if (att1.sort != att2.sort)
                return false;
            return true;
        }
        // - if they are both terms -> recursive call
        if ((att1 instanceof TermTermAttribute) &&
            (att2 instanceof TermTermAttribute)) {
            return att1.term.equalsConsideringAndListInternal(att2.term, bindings);
        }
        // - if one of them is a variable that does not occur inside the other (occurs check) -> add binding
        if (att1 instanceof VariableTermAttribute &&
            att2 instanceof VariableTermAttribute) {
            if (att1.sort != att2.sort)
                return false;
            var found = false;
            for (var _i = 0, _a = bindings.l; _i < _a.length; _i++) {
                var _b = _a[_i], v1 = _b[0], v2 = _b[1];
                if (v1 == att1) {
                    if (v2 != att2)
                        return false;
                    found = true;
                }
            }
            if (!found)
                bindings.l.push([att1, att2]);
            return true;
        }
        return false;
    };
    /*
    1: equals
    0: cannot decide
    -1: different
    */
    Term.prototype.equalsNoBindings = function (t) {
        // if they have a different number of attribetus -> return false
        if (this.attributes.length != t.attributes.length)
            return -1;
        // if functors do not match -> return false
        if (this.functor != t.functor)
            return -1;
        // for each attribute:
        var result = 1;
        var tmp;
        for (var i = 0; i < this.attributes.length; i++) {
            tmp = Term.equalsNoBindingsAttribute(this.attributes[i], t.attributes[i]);
            if (tmp == -1)
                return -1;
            if (tmp == 0)
                result = 0;
        }
        return result;
    };
    // return values:
    // 1: true
    // -1: false
    // 0: could unify, but they are not identical
    Term.equalsNoBindingsAttribute = function (att1, att2) {
        if (att1 instanceof ConstantTermAttribute) {
            if (att2 instanceof ConstantTermAttribute) {
                if (att1.value != att2.value)
                    return -1;
                if (att1.sort != att2.sort)
                    return -1;
                return 1;
            }
            else if (att2 instanceof TermTermAttribute) {
                return -1;
            }
            else {
                return 0;
            }
        }
        else if (att1 instanceof TermTermAttribute) {
            if (att2 instanceof TermTermAttribute) {
                return att1.term.equalsNoBindings(att2.term);
            }
            else if (att2 instanceof ConstantTermAttribute) {
                return -1;
            }
            else {
                return 0;
            }
        }
        else /*if (att1 instanceof VariableTermAttribute)*/ {
            if (att2 instanceof VariableTermAttribute) {
                //if (att1 == att2) return 1;
                if (att1.sort == att2.sort)
                    return 1;
                return 0;
            }
            else {
                return 0;
            }
        }
    };
    /*
    1: equals
    0: cannot decide
    -1: different
    */
    Term.prototype.subsumesNoBindings = function (t) {
        // if they have a different number of attribetus -> return false
        if (this.attributes.length != t.attributes.length)
            return -1;
        // if functors do not match -> return false
        if (!this.functor.subsumes(t.functor))
            return -1;
        // for each attribute:
        var result = 1;
        for (var i = 0; i < this.attributes.length; i++) {
            var att1 = this.attributes[i];
            var att2 = t.attributes[i];
            var tmp = Term.subsumesNoBindingsAttribute(att1, att2);
            if (tmp == -1)
                return -1;
            if (tmp == 0 && result == 1)
                result = 0;
        }
        return result;
    };
    // return values:
    // 1: true
    // -1: false
    // 0: could unify, but they are not identical
    Term.subsumesNoBindingsAttribute = function (att1, att2) {
        if (att1 instanceof ConstantTermAttribute) {
            if (att2 instanceof ConstantTermAttribute) {
                if (att1.value != att2.value)
                    return -1;
                if (att1.sort != att2.sort)
                    return -1;
                return 1;
            }
            else if (att2 instanceof TermTermAttribute) {
                return -1;
            }
            else {
                return 0;
            }
        }
        if (att1 instanceof TermTermAttribute) {
            if (att2 instanceof TermTermAttribute) {
                return att1.term.subsumesNoBindings(att2.term);
            }
            else if (att2 instanceof ConstantTermAttribute) {
                return -1;
            }
            else {
                return 0;
            }
        }
        if (att1 instanceof VariableTermAttribute) {
            if (att2 instanceof VariableTermAttribute) {
                if (att1 == att2)
                    return 1;
                if (att1.sort == att2.sort)
                    return 1;
                return 0;
            }
            else {
                return 0;
            }
        }
        // we should never reach here anyway
        return -1;
    };
    Term.prototype.applyBindings = function (bindings) {
        if (bindings.l.length == 0)
            return this;
        return this.applyBindings_internal(bindings, []).term;
    };
    Term.prototype.applyBindings_internal = function (bindings, map) {
        for (var _i = 0, map_4 = map; _i < map_4.length; _i++) {
            var _a = map_4[_i], v1 = _a[0], v2 = _a[1];
            if (v1 == this)
                return v2;
        }
        var t = new Term(this.functor, []);
        var t_a = new TermTermAttribute(t);
        map.push([this, t_a]);
        for (var _b = 0, _c = this.attributes; _b < _c.length; _b++) {
            var att = _c[_b];
            var att2 = att.applyBindings_internal(bindings, map);
            t.attributes.push(att2);
        }
        t_a.sort = t_a.term.functor;
        return t_a;
    };
    Term.prototype.getAllVariables = function () {
        var vs = [];
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var att = _a[_i];
            if (att instanceof VariableTermAttribute) {
                vs.push(att);
            }
            else if (att instanceof TermTermAttribute) {
                var vs2 = att.term.getAllVariables();
                for (var _b = 0, vs2_1 = vs2; _b < vs2_1.length; _b++) {
                    var v = vs2_1[_b];
                    if (vs.indexOf(v) == -1)
                        vs.push(v);
                }
            }
        }
        return vs;
    };
    Term.prototype.containsVariable = function (v) {
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var att = _a[_i];
            if (att == v) {
                return true;
            }
            else if (att instanceof TermTermAttribute) {
                if (att.term.containsVariable(v))
                    return true;
            }
        }
        return false;
    };
    Term.prototype.containsAnyVariable = function () {
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var att = _a[_i];
            if (att instanceof VariableTermAttribute) {
                return true;
            }
            else if (att instanceof TermTermAttribute) {
                if (att.term.containsAnyVariable())
                    return true;
            }
        }
        return false;
    };
    Term.prototype.findSubtermWithFunctorSort = function (s) {
        if (this.functor.is_a(s))
            return this;
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var att = _a[_i];
            if (att instanceof TermTermAttribute) {
                var tmp = att.term.findSubtermWithFunctorSort(s);
                if (tmp != null)
                    return tmp;
            }
        }
        return null;
    };
    Term.prototype.clone = function (map) {
        var attributes = [];
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var a = _a[_i];
            attributes.push(a.clone(map));
        }
        return new Term(this.functor, attributes);
    };
    Term.prototype.cloneKeepingVariables = function (map) {
        var attributes = [];
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var a = _a[_i];
            attributes.push(a.cloneKeepingVariables(map));
        }
        return new Term(this.functor, attributes);
    };
    // Sentences or sets of sentences can be represented by a single term using the 
    // #and, #or and #not functors. This function decodes such notation into a set of sentences:
    Term.termToSentences = function (term, o) {
        var sentences = [];
        // First thing is to convert to CNF, which has 2 steps:
        term = Term.convertToCNF(term, o);
        var sentenceTermAs = Term.elementsInList(term, "#and");
        for (var _i = 0, sentenceTermAs_1 = sentenceTermAs; _i < sentenceTermAs_1.length; _i++) {
            var sentenceTermA = sentenceTermAs_1[_i];
            if (!(sentenceTermA instanceof TermTermAttribute))
                return null;
            var sentenceTerm = sentenceTermA.term;
            var termTermAs = Term.elementsInList(sentenceTerm, "#or");
            var terms = [];
            var signs = [];
            for (var _a = 0, termTermAs_1 = termTermAs; _a < termTermAs_1.length; _a++) {
                var termTermA = termTermAs_1[_a];
                if (!(termTermA instanceof TermTermAttribute))
                    return null;
                var termTerm = termTermA.term;
                var sign = true;
                if (termTerm.functor.name == "#not" &&
                    termTerm.attributes.length == 1 &&
                    termTerm.attributes[0] instanceof TermTermAttribute) {
                    termTerm = termTerm.attributes[0].term;
                    sign = false;
                }
                // turn "!=" into a negated "="
                if (termTerm.functor.name == "!=") {
                    termTerm = new Term(o.getSort("="), termTerm.attributes);
                    sign = !sign;
                }
                terms.push(termTerm);
                signs.push(sign);
            }
            sentences.push(new Sentence(terms, signs));
        }
        //        console.log("termToSentences: " + sentences);
        return sentences;
    };
    Term.convertToCNF = function (term, o) {
        // Step 1: bring the #not inwards:
        term = Term.bringNotInwards(term, o);
        // Step 2: apply distributive property to turn the expression into a conjunction of disjunctions:
        term = Term.applyDistributive(term, o);
        return term;
    };
    Term.bringNotInwards = function (term, o) {
        if (term.functor.name == "#not" &&
            term.attributes.length == 1 &&
            term.attributes[0] instanceof TermTermAttribute) {
            var subterm = term.attributes[0].term;
            if (subterm.functor.name == "#or") {
                var term2 = new Term(o.getSort("#and"), []);
                for (var _i = 0, _a = subterm.attributes; _i < _a.length; _i++) {
                    var att = _a[_i];
                    if (att instanceof TermTermAttribute) {
                        term2.attributes.push(new TermTermAttribute(Term.bringNotInwards(new Term(term.functor, [new TermTermAttribute(att.term)]), o)));
                    }
                    else {
                        term2.attributes.push(new TermTermAttribute(new Term(term.functor, [att])));
                    }
                }
                return term2;
            }
            else if (subterm.functor.name == "#and") {
                var term2 = new Term(o.getSort("#or"), []);
                for (var _b = 0, _c = subterm.attributes; _b < _c.length; _b++) {
                    var att = _c[_b];
                    if (att instanceof TermTermAttribute) {
                        term2.attributes.push(new TermTermAttribute(Term.bringNotInwards(new Term(term.functor, [new TermTermAttribute(att.term)]), o)));
                    }
                    else {
                        term2.attributes.push(new TermTermAttribute(new Term(term.functor, [att])));
                    }
                }
                return term2;
            }
            else if (subterm.functor.name == "#not" &&
                term.attributes.length == 1 &&
                term.attributes[0] instanceof TermTermAttribute) {
                // Two nots in a row, eliminate them!
                return Term.bringNotInwards((subterm.attributes[0]).term, o);
            }
            else {
                return term;
            }
        }
        else {
            var term2 = new Term(term.functor, []);
            for (var _d = 0, _e = term.attributes; _d < _e.length; _d++) {
                var att = _e[_d];
                if (att instanceof TermTermAttribute) {
                    term2.attributes.push(new TermTermAttribute(Term.bringNotInwards(att.term, o)));
                }
                else {
                    term2.attributes.push(att);
                }
            }
            return term2;
        }
    };
    // Checks if we need to apply the distributive property to turn a term into CNF:
    Term.applyDistributive = function (term, o) {
        // first make sure all the children are fixed:
        if (term.functor.name == "#and" || term.functor.name == "#or") {
            for (var _i = 0, _a = term.attributes; _i < _a.length; _i++) {
                var attribute = _a[_i];
                if (attribute instanceof TermTermAttribute) {
                    attribute.term = Term.applyDistributive(attribute.term, o);
                }
            }
        }
        // See if we can apply the distribution pattern:  P v (Q ^ R)  -->  (P v Q) ^ (P v R)
        if (term.functor.name == "#or" &&
            term.attributes.length == 2 &&
            (term.attributes[0] instanceof TermTermAttribute) &&
            (term.attributes[1] instanceof TermTermAttribute)) {
            var terma1 = term.attributes[0].term;
            var terma2 = term.attributes[1].term;
            if (terma1.functor.name == "#and" &&
                terma1.attributes.length == 2) {
                // apply pattern! (Q ^ R) v P  -->  (Q v P) ^ (R v P)
                term.functor = o.getSort("#and");
                term.attributes = [
                    new TermTermAttribute(new Term(o.getSort("#or"), [terma1.attributes[0],
                        term.attributes[1]])),
                    new TermTermAttribute(new Term(o.getSort("#or"), [terma1.attributes[1],
                        term.attributes[1]]))
                ];
                return Term.applyDistributive(term, o);
            }
            else if (terma2.functor.name == "#and") {
                // apply pattern! P v (Q ^ R)  -->  (P v Q) ^ (P v R)
                term.functor = o.getSort("#and");
                term.attributes = [
                    new TermTermAttribute(new Term(o.getSort("#or"), [term.attributes[0],
                        terma2.attributes[0]])),
                    new TermTermAttribute(new Term(o.getSort("#or"), [term.attributes[0],
                        terma2.attributes[1]]))
                ];
                return Term.applyDistributive(term, o);
            }
        }
        return term;
    };
    Term.sentencesToTerm = function (s_l, o) {
        var term = null;
        for (var i = 0; i < s_l.length; i++) {
            var s = s_l[i];
            var term2 = Term.sentenceToTerm(s, o);
            if (term == null) {
                term = term2;
            }
            else {
                term = new Term(o.getSort("#and"), [new TermTermAttribute(term), new TermTermAttribute(term2)]);
            }
        }
        return term;
    };
    Term.sentenceToTerm = function (sentence, o) {
        var term = null;
        for (var i = 0; i < sentence.terms.length; i++) {
            var term2 = sentence.terms[i];
            if (!sentence.sign[i]) {
                term2 = new Term(o.getSort("#not"), [new TermTermAttribute(term2)]);
            }
            if (term == null) {
                term = term2;
            }
            else {
                term = new Term(o.getSort("#or"), [new TermTermAttribute(term), new TermTermAttribute(term2)]);
            }
        }
        return term;
    };
    Term.prototype.toString = function () {
        return this.toStringInternal([], []);
    };
    Term.prototype.toStringXML = function () {
        return this.toStringXMLInternal([], []);
    };
    Term.prototype.toStringInternal = function (variables, variableNames) {
        var str = this.functor.name + "(";
        var first = true;
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var a = _a[_i];
            if (first) {
                first = false;
            }
            else {
                str += ", ";
            }
            if (variables.indexOf(a) == -1) {
                str += Term.variableNameForPrinting(a, variables, variableNames);
                str += ":" + a.toStringInternal(variables, variableNames);
            }
            else {
                str += Term.variableNameForPrinting(a, variables, variableNames);
            }
        }
        return str + ")";
    };
    Term.prototype.toStringXMLInternal = function (variables, variableNames) {
        var str = this.functor.name + "(";
        var first = true;
        for (var _i = 0, _a = this.attributes; _i < _a.length; _i++) {
            var a = _a[_i];
            if (first) {
                first = false;
            }
            else {
                str += ", ";
            }
            if (variables.indexOf(a) == -1) {
                str += Term.variableNameForPrinting(a, variables, variableNames);
                str += ":" + a.toStringXMLInternal(variables, variableNames);
            }
            else {
                str += Term.variableNameForPrinting(a, variables, variableNames);
            }
        }
        return str + ")";
    };
    Term.variableNameForPrinting = function (a, variables, variableNames) {
        var idx = variables.indexOf(a);
        if (idx >= 0)
            return variableNames[idx];
        variables.push(a);
        var basevname = null;
        var vname = null;
        if (a instanceof VariableTermAttribute)
            basevname = a.name;
        if (basevname == null)
            basevname = "V" + (variables.length - 1);
        vname = basevname;
        var idx2 = 0;
        var nameAllowed = true;
        do {
            nameAllowed = true;
            if (variableNames.lastIndexOf(vname) != -1) {
                nameAllowed = false;
            }
            else {
                for (var _i = 0, variables_1 = variables; _i < variables_1.length; _i++) {
                    var v2 = variables_1[_i];
                    var vname2 = null;
                    if (!(v2 instanceof VariableTermAttribute))
                        continue;
                    vname2 = v2.name;
                    if (vname == vname2) {
                        nameAllowed = false;
                    }
                }
            }
            if (!nameAllowed) {
                vname = basevname + "_" + idx2;
                idx2++;
            }
        } while (!nameAllowed);
        variableNames.push(vname);
        return vname;
    };
    Term.fromString = function (str, o) {
        var ta = Term.fromStringInternal(str, o, [], []);
        if (ta == null) {
            console.error("Error parsing term: " + str);
            return null;
        }
        return ta.term;
    };
    Term.tokenizeStringByAttribute = function (str) {
        // separate the strings corresponding to each attribute:
        var idx = 0;
        var len = str.length;
        var c;
        var attributeStrings = [];
        var parenthesis = 0;
        var squareBrackets = 0;
        var quotation = false;
        var expectingAttribute = false;
        var tmp = "";
        while (idx < len) {
            c = str.charAt(idx);
            idx++;
            if (parenthesis == 0 &&
                squareBrackets == 0 &&
                !quotation &&
                c == ',') {
                tmp = tmp.trim();
                if (tmp == "") {
                    console.error("Term.fromString: empty attribute string parsing: " + str);
                    return null;
                }
                attributeStrings.push(tmp);
                tmp = "";
                expectingAttribute = true;
            }
            else if (parenthesis == 0 &&
                squareBrackets == 0 &&
                !quotation &&
                c == ')') {
                tmp = tmp.trim();
                if (tmp == "" && expectingAttribute) {
                    console.error("Term.fromString: empty attribute string parsing: " + str);
                    return null;
                }
                if (tmp != "") {
                    attributeStrings.push(tmp);
                    tmp = "";
                }
                break;
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
                tmp += c;
            }
        }
        tmp = tmp.trim();
        //        console.log("str: " + str + "\np: " + parenthesis + ", sb: " + squareBrackets + ", q: " + quotation);
        //        console.log("attributeStrings: " + attributeStrings);
        //        console.log("tmp: " + tmp);
        if (tmp != "") {
            console.error("Term.fromString: missing closing parenthesis! " + str);
            return null;
        }
        if (quotation) {
            console.error("Term.fromString: term ends inside of a quotation!");
            return null;
        }
        if (parenthesis > 0) {
            console.error("Term.fromString: missing closing parenthesis!");
            return null;
        }
        if (squareBrackets > 0) {
            console.error("Term.fromString: missing closing square bracket!");
            return null;
        }
        if (str.substring(idx).trim() != "") {
            console.error("Term.fromString: extra characters found after term! " + str.substring(idx) + " in " + str);
            return null;
        }
        //        console.log("attribute strings("+attributeStrings.length+"): " + attributeStrings);
        return attributeStrings;
    };
    Term.fromStringInternal = function (str, o, variableNames, variableValues) {
        var tmp = "";
        var len = str.length;
        var idx = 0;
        var c = null;
        var term = new Term(null, []);
        var term_a = new TermTermAttribute(term);
        var state = 0;
        // if (str.indexOf("location2") >= 0) console.log("str: " + str);
        //        console.log("Term.fromStringInternal: " + str);
        // parse sort string (potentially with variable name):
        while (idx < len) {
            c = str.charAt(idx);
            idx++;
            if (c == ':') {
                if (state == 1) {
                    console.error("Term.fromString: two variable names!");
                    return null;
                }
                // there was a variable name!
                if (tmp == "") {
                    console.error("Term.fromString: empty variable name!");
                    return null;
                }
                variableNames.push(tmp);
                variableValues.push(term_a);
                tmp = "";
                state = 1;
            }
            else if (c == '(') {
                term.functor = o.getSort(tmp);
                term_a.sort = term.functor;
                if (term.functor == null) {
                    console.error("Term.fromString: unknown sort " + tmp + "!");
                    return null;
                }
                break;
            }
            else {
                tmp += c;
            }
        }
        if (idx == len) {
            console.error("Term.fromString: reached end of string and found no parenthesis!");
            return null;
        }
        var attributeStrings = Term.tokenizeStringByAttribute(str.substring(idx));
        if (attributeStrings == null) {
            console.error("Term.parseFromString: attributeStrings is null!");
            return null;
        }
        for (var _i = 0, attributeStrings_1 = attributeStrings; _i < attributeStrings_1.length; _i++) {
            var attributeString = attributeStrings_1[_i];
            var att = Term.parseAttribute(attributeString, o, variableNames, variableValues);
            if (att == null) {
                console.error("Term.parseFromString: Term.parseAttribute returned null!");
                return null;
            }
            term.attributes.push(att);
        }
        return term_a;
    };
    Term.parseAttribute = function (attributeString, o, variableNames, variableValues) {
        // if (attributeString.indexOf("location2") >= 0) console.log("attributeString: " + attributeString);
        var tmp = "";
        var idx = 0;
        var len = attributeString.length;
        while (idx < len) {
            var c = attributeString.charAt(idx);
            idx++;
            if (c == '[') {
                if (tmp == "") {
                    // [sort]
                    var idx2 = attributeString.indexOf("]");
                    tmp = attributeString.substring(idx, idx2);
                    if (attributeString.substring(idx2 + 1).trim() != "") {
                        console.error("Term.parseAttribute: extra characters found after VariableTermAttribute! parsing " + attributeString);
                        return null;
                    }
                    if (tmp.indexOf("'") >= 0) {
                        console.error("Term.parseAttribute: unexpected character ' in sort name: " + tmp);
                        return null;
                    }
                    var a_sort = o.getSort(tmp);
                    if (a_sort == null) {
                        console.error("Term.parseAttribute: unknown sort " + tmp);
                        return null;
                    }
                    return new VariableTermAttribute(a_sort, null);
                }
                else {
                    console.error("Term.parseAttribute: square bracket directly after a variable name! " + tmp);
                    return null;
                }
            }
            else if (c == "\'") {
                if (tmp == "") {
                    // 'constant'[sort]
                    var foundQuote = false;
                    var idx2 = idx;
                    while (idx2 < len) {
                        if (attributeString.charAt(idx2) == "\'") {
                            foundQuote = true;
                            break;
                        }
                        idx2++;
                    }
                    if (!foundQuote) {
                        console.error("Term.parseAttribute: unclosed quote in attribute!" + attributeString);
                        return null;
                    }
                    var idx3 = attributeString.substring(idx2).indexOf("[");
                    var idx4 = attributeString.substring(idx2).indexOf("]");
                    tmp = attributeString.substring(idx, idx2);
                    var tmp2 = attributeString.substring(idx2).substring(idx3 + 1, idx4);
                    if (tmp2.indexOf("'") >= 0) {
                        console.error("Term.parseAttribute: unexpected character ' in sort name: " + tmp2);
                        return null;
                    }
                    var a_sort = o.getSort(tmp2);
                    if (a_sort == null) {
                        console.error("Term.parseAttribute: unknown sort " + tmp2);
                        return null;
                    }
                    if (tmp.trim() != "" && a_sort.name != "#id" && !isNaN(Number(tmp))) {
                        return new ConstantTermAttribute(Number(tmp), a_sort);
                    }
                    else {
                        return new ConstantTermAttribute(tmp.replace("\\039", "'"), a_sort);
                    }
                }
                else {
                    console.error("Term.parseAttribute: quote starts in the middle of a name! parsing " + attributeString);
                    return null;
                }
            }
            else if (c == ":") {
                if (tmp == "") {
                    console.error("Term.parseAttribute: empty variable name!");
                    return null;
                }
                else {
                    if (attributeString.charAt(idx) == "[") {
                        // VariableName:[sort]
                        var idx2 = attributeString.indexOf("]");
                        var tmp2 = attributeString.substring(idx + 1, idx2);
                        if (attributeString.substring(idx2 + 1).trim() != "") {
                            console.error("Term.parseAttribute: extra characters found after VariableTermAttribute! parsing " + attributeString);
                            return null;
                        }
                        var a_sort = o.getSort(tmp2);
                        if (a_sort == null) {
                            console.error("Term.parseAttribute: unknown sort " + tmp2);
                            return null;
                        }
                        if (tmp.charAt(tmp.length - 1) == "]") {
                            console.error("Variable name ends in ], something went wrong!: " + tmp);
                        }
                        // console.log("   variableName(1): " + tmp);
                        var a_term = new VariableTermAttribute(a_sort, tmp);
                        if (variableNames.indexOf(tmp) == -1) {
                            variableNames.push(tmp);
                            variableValues.push(a_term);
                        }
                        else {
                            console.error("Repeated definition of variable sort for variable '" + tmp + "' in: " + attributeString);
                            return null;
                        }
                        return a_term;
                    }
                    else if (attributeString.charAt(idx) == "\'") {
                        // VariableName:'constant'[sort]
                        if (variableNames.indexOf(tmp) != -1) {
                            console.error("Repeated definition of variable sort for variable '" + tmp + "' in: " + attributeString);
                            return null;
                        }
                        variableNames.push(tmp);
                        attributeString = attributeString.substring(idx);
                        idx = 1;
                        var foundQuote = false;
                        var idx2 = idx;
                        len = attributeString.length;
                        while (idx2 < len) {
                            if (attributeString.charAt(idx2) == "\'") {
                                foundQuote = true;
                                break;
                            }
                            idx2++;
                        }
                        if (!foundQuote) {
                            console.error("Term.parseAttribute: unclosed quote in attribute!");
                            return null;
                        }
                        var idx3 = attributeString.substring(idx2).indexOf("[");
                        var idx4 = attributeString.substring(idx2).indexOf("]");
                        tmp = attributeString.substring(idx, idx2);
                        var tmp2 = attributeString.substring(idx2).substring(idx3 + 1, idx4);
                        var a_sort = o.getSort(tmp2);
                        if (a_sort == null) {
                            console.error("Term.parseAttribute: unknown sort " + tmp2);
                            return null;
                        }
                        var a_term = null;
                        if (tmp.trim() != "" && a_sort.name != "#id" && !isNaN(Number(tmp))) {
                            a_term = new ConstantTermAttribute(Number(tmp), a_sort);
                        }
                        else {
                            a_term = new ConstantTermAttribute(tmp.replace("\\039", "'"), a_sort);
                        }
                        variableValues.push(a_term);
                        return a_term;
                    }
                    else {
                        // VariableName:functor( ... )
                        var a_term = Term.fromStringInternal(attributeString, o, variableNames, variableValues);
                        if (a_term == null)
                            return null;
                        return a_term;
                    }
                }
            }
            else if (c == "(") {
                // functor( ... )
                var a_term = Term.fromStringInternal(attributeString, o, variableNames, variableValues);
                if (a_term == null) {
                    console.error("Term.parseAttribute: Term.parseFromString returned null!");
                    return null;
                }
                return a_term;
            }
            tmp += c;
        }
        if (tmp.charAt(tmp.length - 1) == "]") {
            console.error("Variable name ends in ], something went wrong!: " + tmp);
        }
        // VariableName
        //        console.log("variableNames: " + variableNames);
        // console.log("   variableName(2): " + tmp);
        idx = variableNames.indexOf(tmp);
        if (idx == -1) {
            //            console.log("   it's a new one");
            var a_sort = o.getSort("any");
            var a_term = new VariableTermAttribute(a_sort, tmp);
            variableNames.push(tmp);
            variableValues.push(a_term);
            return a_term;
        }
        else {
            //            console.log("   we had it before");
            return variableValues[idx];
        }
    };
    return Term;
}());
