var A4ObjectFactory = /** @class */ (function () {
    function A4ObjectFactory() {
        this.objectTypes = [];
        this.baseClasses = [];
        this.baseClasses = ["item", "container", "vehicle", "character", "obstacle", "obstacle-container", "door", "key", "pushable-wall", "food", "spade", "object"];
    }
    A4ObjectFactory.prototype.addDefinitions = function (xml, game, baseClassName) {
        var o = game.ontology;
        var classes_xml = getElementChildrenByTag(xml, baseClassName);
        for (var i = 0; i < classes_xml.length; i++) {
            var class_xml = classes_xml[i];
            this.objectTypes.push(class_xml);
            var sortName = class_xml.getAttribute("class");
            var s = o.getSortSilent(sortName);
            if (s == null)
                s = o.newSort(sortName, []);
            var superClasses = class_xml.getAttribute("super").split(',');
            console.log("added class " + s.name + " to A4ObjectFactory.objectTypes");
            for (var _i = 0, superClasses_1 = superClasses; _i < superClasses_1.length; _i++) {
                var className = superClasses_1[_i];
                if (className.charAt(0) == '*') {
                    var sortName_1 = className.substring(1);
                    var s2 = o.getSortSilent(sortName_1);
                    if (s2 == null)
                        s2 = o.newSort(sortName_1, []);
                    s.addParent(s2);
                }
                else {
                    var sortName_2 = className;
                    var s2 = o.getSort(sortName_2);
                    if (s2 == null)
                        s2 = o.newSort(sortName_2, []);
                    s.addParent(s2);
                }
            }
        }
    };
    A4ObjectFactory.prototype.createObject = function (className, game, isPlayer, completeRedefinition) {
        var xml = this.getObjectType(className);
        if (xml != null)
            return this.createObjectFromXML(xml, game, isPlayer, completeRedefinition);
        return this.createObjectFromBaseClass(className, game.ontology.getSort("obstacle"), null, isPlayer, false);
    };
    A4ObjectFactory.prototype.createObjectFromBaseClass = function (baseClassName, s, o_name, isPlayer, dead) {
        if (baseClassName == "item") {
            return new A4Item(o_name, s);
        }
        else if (baseClassName == "vehicle") {
            return new A4Vehicle(o_name, s);
        }
        else if (baseClassName == "character") {
            if (dead) {
                return new A4Character(o_name, s);
            }
            else {
                if (isPlayer) {
                    return new A4PlayerCharacter(o_name, s);
                }
                else {
                    return new A4AICharacter(o_name, s);
                }
            }
        }
        else if (baseClassName == "obstacle") {
            return new A4Obstacle(o_name, s);
        }
        else if (baseClassName == "obstacle-container") {
            return new A4ObstacleContainer(o_name, s, null, true, false, null, null);
        }
        else if (baseClassName == "door") {
            return new A4Door(s, null, true, false, null, null);
        }
        else if (baseClassName == "key") {
            return new A4Key(o_name, s, null, null);
        }
        else if (baseClassName == "lever") {
            return new A4Lever(s, null, false, null, null);
        }
        else if (baseClassName == "pressure-plate") {
            return new A4PressurePlate(s, null, null, TRIGGER_PRESSURE_ITEM);
        }
        else if (baseClassName == "container") {
            return new A4Container(o_name, s, null);
        }
        else if (baseClassName == "pushable-wall") {
            return new A4PushableWall(o_name, s, null);
        }
        else if (baseClassName == "food") {
            return new A4Food(o_name, s, null, 0);
        }
        else if (baseClassName == "spade") {
            return new A4Spade(s, null, 0);
        }
        else if (baseClassName == "trigger") {
            return new A4Trigger(s, 0, 0);
        }
        else if (baseClassName == "object") {
            return new A4Object(o_name, s);
        }
        else {
            return null;
        }
    };
    A4ObjectFactory.prototype.getObjectType = function (type) {
        for (var _i = 0, _a = this.objectTypes; _i < _a.length; _i++) {
            var xml = _a[_i];
            if (xml.getAttribute("class") == type)
                return xml;
        }
        return null;
    };
    A4ObjectFactory.prototype.createObjectFromXML = function (xml, game, isPlayer, completeRedefinition) {
        var o;
        var ontology = game.ontology;
        if (xml.getAttribute("completeRedefinition") == "true")
            completeRedefinition = true;
        // find base class, and additional definitions to add:
        var classes = [];
        var baseClassName = null;
        var dead = false;
        {
            var open_1 = [xml.getAttribute("class")];
            //let closed:string[] = [];
            while (open_1.length > 0) {
                var current = open_1[0];
                //console.log("considering: " + current);
                open_1.splice(0, 1);
                var loadContent = true;
                var current2 = current;
                if (current.charAt(0) == '*') {
                    // loadContent = false;
                    current2 = current.substring(1);
                    if (ontology.getSort(current2).is_a_string("dead"))
                        dead = true;
                    //closed.push(current2);
                    continue;
                }
                else {
                    if (ontology.getSort(current2).is_a_string("dead"))
                        dead = true;
                    //closed.push(current2);
                }
                var current_xml = this.getObjectType(current2);
                var found = this.baseClasses.indexOf(current2);
                if (current_xml == null && found >= 0) {
                    if (baseClassName == null) {
                        baseClassName = current;
                    }
                    else if (baseClassName != current) {
                        console.error("A4ObjectFactory::createObject: baseClassName was '" + baseClassName + "' and now it's attempted to set to '" + current + "'!!!");
                        return null;
                    }
                }
                else {
                    if (current_xml != null) {
                        if (loadContent)
                            classes.unshift(current_xml); // push at the front of the array
                        var superString = current_xml.getAttribute("super");
                        var superClasses = superString.split(',');
                        for (var _i = 0, superClasses_2 = superClasses; _i < superClasses_2.length; _i++) {
                            var className = superClasses_2[_i];
                            open_1.push(className);
                        }
                    }
                }
            }
        }
        if (baseClassName == null) {
            console.error("A4ObjectFactory::createObject: baseClassName null to create '" + xml.getAttribute("class") + "'!!!");
            return null;
        }
        var o_ID = xml.getAttribute("id");
        var o_name = xml.getAttribute("name");
        if (o_name == null) {
            for (var _a = 0, classes_1 = classes; _a < classes_1.length; _a++) {
                var xml2 = classes_1[_a];
                o_name = xml2.getAttribute("name");
                if (o_name != null)
                    break;
            }
        }
        var classStr = xml.getAttribute("class");
        if (classStr == null)
            classStr = xml.getAttribute("type");
        o = this.createObjectFromBaseClass(baseClassName, ontology.getSort(classStr), o_name, isPlayer, dead);
        if (o_ID != null) {
            o.ID = o_ID;
            if (!isNaN(Number(o_ID)) &&
                Number(o_ID) >= A4Object.s_nextID)
                A4Object.s_nextID = Number(o_ID) + 1;
        }
        if (!completeRedefinition) {
            for (var _b = 0, classes_2 = classes; _b < classes_2.length; _b++) {
                var xml2 = classes_2[_b];
                o.loadObjectAdditionalContent(xml2, game, this, [], []);
            }
        }
        return o;
    };
    return A4ObjectFactory;
}());
;
