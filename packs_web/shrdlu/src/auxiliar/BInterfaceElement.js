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
var BInterfaceElement = /** @class */ (function () {
    function BInterfaceElement(x, y, width, height) {
        this.modal = false; // If any element is modal, only him has the control until it is destroyed (the rest of the interface is faded) 
        this.enabled = true; // whether the element can b interacted with or not 
        this.active = true; // This indicates whether the component is active or passive (passive elements are only decorative) 
        // e.g.: BText and BFrame are passive                    
        this.to_be_deleted = false;
        this.children = [];
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }
    BInterfaceElement.prototype.mouseOver = function (mousex, mousey) {
        return mousex >= this.x && mousex < this.x + this.width &&
            mousey >= this.y && mousey < this.y + this.height;
    };
    BInterfaceElement.prototype.highlighted = function (mousex, mousey) {
        if (BInterface.highlightedByKeyboard == -1) {
            return this.mouseOver(mousex, mousey);
        }
        else {
            return BInterface.elements[BInterface.highlightedByKeyboard] == this;
        }
    };
    BInterfaceElement.prototype.update = function (mouse_x, mouse_y, k, arg) {
        return false;
    };
    BInterfaceElement.prototype.mouseClick = function (mouse_x, mouse_y, button, arg) {
    };
    BInterfaceElement.prototype.draw = function () {
        this.drawAlpha(1.0);
    };
    BInterfaceElement.prototype.drawAlpha = function (alpha) {
    };
    BInterfaceElement.prototype.getChildren = function () {
        return this.children;
    };
    BInterfaceElement.prototype.getEnabled = function () {
        return this.enabled;
    };
    BInterfaceElement.prototype.setEnabled = function (enabled) {
        this.enabled = enabled;
    };
    BInterfaceElement.prototype.getID = function () {
        return this.ID;
    };
    return BInterfaceElement;
}());
var BText = /** @class */ (function (_super) {
    __extends(BText, _super);
    function BText(text, font, textHeight, x, y, centered, ID) {
        var _this = _super.call(this, x, y, 0, 0) || this;
        ctx.font = font;
        _this.width = ctx.measureText(text).width;
        _this.height = textHeight;
        _this.centered = centered;
        _this.text = text;
        _this.font = font;
        _this.enabled = true;
        _this.active = false;
        return _this;
    }
    BText.prototype.drawAlpha = function (alpha) {
        var color = "white";
        if (!this.enabled) {
            color = generateRGBColor(80, 80, 80);
        }
        if (this.centered) {
            fillTextTopCenter(this.text, this.x, this.y, this.font, color);
        }
        else {
            fillTextTopLeft(this.text, this.x, this.y, this.font, color);
        }
    };
    return BText;
}(BInterfaceElement));
var BQuad = /** @class */ (function (_super) {
    __extends(BQuad, _super);
    function BQuad(x, y, dx, dy, color) {
        var _this = _super.call(this, x, y, dx, dy) || this;
        _this.color = color;
        _this.enabled = true;
        _this.active = false;
        return _this;
    }
    BQuad.prototype.drawAlpha = function (alpha) {
        if (this.enabled) {
            ctx.fillStyle = this.color;
            ctx.fillRect(this.x, this.y, this.width, this.height);
        }
        else {
            ctx.save();
            ctx.globalAlpha *= 0.5;
            ctx.fillStyle = this.color;
            ctx.fillRect(this.x, this.y, this.width, this.height);
            ctx.restore();
        }
    };
    return BQuad;
}(BInterfaceElement));
/*
class BInterfaceAnimation extends BInterfaceElement {
    constructor(a:Animation, x:number, y:number)
    {
        super(x, y, a.getPixelWidth(), a.getPixelHeight());
        this.animation = a;
    }
    
    drawAlpha(alpha:number)
    {
        if (this.enabled) {
            this.animation.drawWithAlpha(this.x,this.y, alpha);
        } else {
            this.animation.drawWithAlpha(this.x,this.y, alpha*0.5);
        }
    }
    
    animation:Animation
}
*/ 
