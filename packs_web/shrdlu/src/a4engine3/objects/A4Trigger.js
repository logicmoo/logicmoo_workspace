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
var A4Trigger = /** @class */ (function (_super) {
    __extends(A4Trigger, _super);
    function A4Trigger(sort, w, h) {
        var _this = _super.call(this, "trigger", sort) || this;
        _this.triggerState = false;
        _this.width = w;
        _this.height = h;
        _this.currentAnimation = A4_ANIMATION_OPEN;
        return _this;
    }
    A4Trigger.prototype.loadObjectAttribute = function (xml) {
        if (_super.prototype.loadObjectAttribute.call(this, xml))
            return true;
        var name = xml.getAttribute("name");
        if (name == "triggerState") {
            this.triggerState = false;
            if (xml.getAttribute("value") == "true")
                this.triggerState = true;
            return true;
        }
        return false;
    };
    A4Trigger.prototype.loadObjectAdditionalContent = function (xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object) {
        _super.prototype.loadObjectAdditionalContent.call(this, xml, game, of, objectsToRevisit_xml, objsctsToRevisit_object);
        this.width = Number(xml.getAttribute("width"));
        this.height = Number(xml.getAttribute("height"));
    };
    A4Trigger.prototype.savePropertiesToXML = function (game) {
        var xmlString = _super.prototype.savePropertiesToXML.call(this, game);
        xmlString += this.saveObjectAttributeToXML("triggerState", this.triggerState) + "\n";
        return xmlString;
    };
    A4Trigger.prototype.getPixelWidth = function () {
        return this.width;
    };
    A4Trigger.prototype.getPixelHeight = function () {
        return this.height;
    };
    A4Trigger.prototype.isTrigger = function () {
        return true;
    };
    A4Trigger.prototype.update = function (game) {
        _super.prototype.update.call(this, game);
        var l = this.map.getAllObjectCollisions(this);
        var triggered_by = null;
        var playerOver = false;
        for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
            var o = l_1[_i];
            if (o.isPlayer()) {
                playerOver = true;
                triggered_by = o;
            }
        }
        if (this.triggerState) {
            if (playerOver) {
                // nothing to do, keep pressed
            }
            else {
                // release
                this.triggerState = false;
                this.event(A4_EVENT_DEACTIVATE, null, this.map, game);
                this.event(A4_EVENT_USE, null, this.map, game);
            }
        }
        else {
            if (triggered_by != null && playerOver) {
                // press!
                this.triggerState = true;
                this.event(A4_EVENT_ACTIVATE, triggered_by, this.map, game);
                this.event(A4_EVENT_USE, triggered_by, this.map, game);
            }
            else {
                // nothing to do, keep released
            }
        }
        return true;
    };
    return A4Trigger;
}(A4Object));
;
