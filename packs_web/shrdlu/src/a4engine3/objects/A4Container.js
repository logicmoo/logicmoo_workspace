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
var A4Container = /** @class */ (function (_super) {
    __extends(A4Container, _super);
    function A4Container(name, sort, a) {
        var _this = _super.call(this, name, sort) || this;
        _this.content = [];
        _this.animations[A4_ANIMATION_IDLE] = a;
        _this.usable = true;
        return _this;
    }
    A4Container.prototype.loadObjectAdditionalContent = function (xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object) {
        _super.prototype.loadObjectAdditionalContent.call(this, xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object);
        var items_xml = getFirstElementChildByTag(xml, "items");
        if (items_xml != null) {
            //            let item_xml_l:NodeListOf<Element> = items_xml.children;
            var item_xml_l = items_xml.children;
            for (var i = 0; i < item_xml_l.length; i++) {
                var item_xml = item_xml_l[i];
                var tmp = item_xml.getAttribute("probability");
                if (tmp != null) {
                    if (Math.random() >= Number(tmp))
                        continue;
                }
                var completeRedefinition = false;
                if (item_xml.getAttribute("completeRedefinition") == "true")
                    completeRedefinition = true;
                var item = null;
                if (item_xml.getAttribute("class") != null) {
                    item = of.createObject(item_xml.getAttribute("class"), game, false, completeRedefinition);
                }
                else {
                    // for compatibility with previous formats
                    item = of.createObject(item_xml.getAttribute("type"), game, false, completeRedefinition);
                }
                var id = item_xml.getAttribute("id");
                if (id != null) {
                    item.ID = id;
                    if (!isNaN(Number(id)) &&
                        Number(id) >= A4Object.s_nextID)
                        A4Object.s_nextID = Number(id) + 1;
                }
                item.loadObjectAdditionalContent(item_xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object);
                this.addContent(item);
            }
        }
    };
    A4Container.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        if (this.content.length > 0) {
            xmlString += "<items>\n";
            for (var _i = 0, _a = this.content; _i < _a.length; _i++) {
                var o = _a[_i];
                xmlString += o.saveToXML(game, 0, false) + "\n";
            }
            xmlString += "</items>\n";
        }
        return xmlString;
    };
    A4Container.prototype.event = function (a_event, otherCharacter, map, game) {
        var retval = _super.prototype.event.call(this, a_event, otherCharacter, map, game);
        if (a_event == A4_EVENT_USE) {
            this.event(A4_EVENT_OPEN, otherCharacter, map, game);
            otherCharacter.removeFromInventory(this);
            for (var _i = 0, _a = this.content; _i < _a.length; _i++) {
                var o = _a[_i];
                otherCharacter.addObjectToInventory(o, game);
            }
            this.content = [];
            game.requestDeletion(this);
            return true;
        }
        return retval;
    };
    A4Container.prototype.addContent = function (o) {
        this.content.push(o);
        if (this.currentAnimation == A4_ANIMATION_OPEN_EMPTY) {
            if (this.animations[A4_ANIMATION_OPEN_FULL] != null)
                this.currentAnimation = A4_ANIMATION_OPEN_FULL;
        }
    };
    A4Container.prototype.objectRemoved = function (o) {
        _super.prototype.objectRemoved.call(this, o);
        for (var _i = 0, _a = this.content; _i < _a.length; _i++) {
            var o2 = _a[_i];
            o2.objectRemoved(o);
        }
        if (this.content.length == 0) {
            if (this.currentAnimation == A4_ANIMATION_OPEN_FULL) {
                if (this.animations[A4_ANIMATION_OPEN_EMPTY] != null)
                    this.currentAnimation = A4_ANIMATION_OPEN_EMPTY;
            }
        }
    };
    A4Container.prototype.findObjectByName = function (name) {
        for (var _i = 0, _a = this.content; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.name == name)
                return [o];
            var o2 = o.findObjectByName(name);
            if (o2 != null)
                return [o].concat(o2);
        }
        return null;
    };
    A4Container.prototype.findObjectByID = function (ID) {
        for (var _i = 0, _a = this.content; _i < _a.length; _i++) {
            var o = _a[_i];
            if (o.ID == ID)
                return [o];
            var o2 = o.findObjectByID(ID);
            if (o2 != null)
                return [o].concat(o2);
        }
        return null;
    };
    return A4Container;
}(A4Item));
;
