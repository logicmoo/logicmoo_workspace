var SORT_HASH_SIZE = 2048;
var Ontology = /** @class */ (function () {
    function Ontology() {
        this.sorts = new Array(SORT_HASH_SIZE);
    }
    Ontology.prototype.loadSortsFromXML = function (xml) {
        var sort_xml_l = getElementChildrenByTag(xml, "sort");
        for (var _i = 0, sort_xml_l_1 = sort_xml_l; _i < sort_xml_l_1.length; _i++) {
            var sort_xml = sort_xml_l_1[_i];
            var name_1 = sort_xml.getAttribute("name");
            var super_raw = sort_xml.getAttribute("super");
            var super_l = [];
            if (super_raw != null) {
                for (var _a = 0, _b = super_raw.split(","); _a < _b.length; _a++) {
                    var super_s = _b[_a];
                    super_l.push(this.getSort(super_s));
                }
            }
            //console.log("loaded sort " + name + " supers: " + super_l);
            this.newSort(name_1, super_l);
        }
    };
    Ontology.prototype.newSort = function (name, parents) {
        var s = new Sort(name, parents);
        var bin = stringHashFunction(name) % SORT_HASH_SIZE;
        if (this.sorts[bin] == null)
            this.sorts[bin] = [];
        this.sorts[bin].push(s);
        return s;
    };
    Ontology.prototype.newSortStrings = function (name, parentsStr) {
        var parents = [];
        for (var _i = 0, parentsStr_1 = parentsStr; _i < parentsStr_1.length; _i++) {
            var str = parentsStr_1[_i];
            parents.push(this.getSort(str));
        }
        var s = new Sort(name, parents);
        var bin = stringHashFunction(name) % SORT_HASH_SIZE;
        if (this.sorts[bin] == null)
            this.sorts[bin] = [];
        this.sorts[bin].push(s);
        return s;
    };
    Ontology.prototype.getSort = function (name) {
        var bin = stringHashFunction(name) % SORT_HASH_SIZE;
        if (this.sorts[bin] == null) {
            console.error("Sort " + name + " does not exist!");
            return null;
        }
        for (var _i = 0, _a = this.sorts[bin]; _i < _a.length; _i++) {
            var s = _a[_i];
            if (s.name == name)
                return s;
        }
        console.error("Sort " + name + " does not exist!");
        return null;
    };
    // same as getSort, but does not print errors if sort do not exist
    Ontology.prototype.getSortSilent = function (name) {
        var bin = stringHashFunction(name) % SORT_HASH_SIZE;
        if (this.sorts[bin] == null) {
            return null;
        }
        for (var _i = 0, _a = this.sorts[bin]; _i < _a.length; _i++) {
            var s = _a[_i];
            if (s.name == name)
                return s;
        }
        return null;
    };
    Ontology.prototype.getAllSorts = function () {
        var allSorts = [];
        for (var i = 0; i < SORT_HASH_SIZE; i++) {
            if (this.sorts[i] != null) {
                for (var _i = 0, _a = this.sorts[i]; _i < _a.length; _i++) {
                    var s = _a[_i];
                    allSorts.push(s);
                }
            }
        }
        return allSorts;
    };
    Ontology.prototype.outerHTML = function () {
        var xmlString = "";
        xmlString += "<ontology>\n";
        for (var _i = 0, _a = this.getAllSorts(); _i < _a.length; _i++) {
            var text = _a[_i];
            xmlString += text.outerHTML() + "\n";
        }
        xmlString += "</ontology>\n";
        return xmlString;
    };
    return Ontology;
}());
