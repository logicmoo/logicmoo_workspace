var KEYBOARD_SIZE = 256;
var KEY_CODE_BACKSPACE = 8;
var KEY_CODE_TAB = 9;
var KEY_CODE_RETURN = 13;
var KEY_CODE_LSHIFT = 16;
var KEY_CODE_ALT = 18;
var KEY_CODE_ESCAPE = 27;
var KEY_CODE_SPACE = 32;
var KEY_CODE_PAGEUP = 33;
var KEY_CODE_PAGEDOWN = 34;
var KEY_CODE_END = 35;
var KEY_CODE_HOME = 36;
var KEY_CODE_LEFT = 37;
var KEY_CODE_UP = 38;
var KEY_CODE_RIGHT = 39;
var KEY_CODE_DOWN = 40;
var KEY_CODE_DELETE = 40;
var KEY_CODE_0 = 48;
var KEY_CODE_1 = 49;
var KEY_CODE_2 = 50;
var KEY_CODE_3 = 51;
var KEY_CODE_9 = 57;
var KEY_CODE_A = 65;
var KEY_CODE_B = 66;
var KEY_CODE_D = 68;
var KEY_CODE_F = 70;
var KEY_CODE_I = 73;
var KEY_CODE_J = 74;
var KEY_CODE_K = 75;
var KEY_CODE_L = 76;
var KEY_CODE_M = 77;
var KEY_CODE_N = 78;
var KEY_CODE_O = 79;
var KEY_CODE_P = 80;
var KEY_CODE_R = 82;
var KEY_CODE_S = 83;
var KEY_CODE_T = 84;
var KEY_CODE_U = 85;
var KEY_CODE_V = 86;
var KEY_CODE_W = 87;
var KEY_CODE_X = 88;
var KEY_CODE_Z = 90;
var KEY_CODE_COMMA = 188;
var KEY_CODE_DOT = 190;
var KEYCODE_NAMES = {
    8: "backspace",
    9: "tab",
    13: "enter",
    16: "shift",
    17: "control",
    18: "alt",
    19: "break",
    20: "caps lock",
    27: "escape",
    32: "space",
    33: "page up",
    34: "page down",
    35: "end",
    36: "home",
    37: "left",
    38: "up",
    39: "right",
    40: "down",
    45: "insert",
    46: "delete",
    48: "0",
    49: "1",
    50: "2",
    51: "3",
    52: "4",
    53: "5",
    54: "6",
    55: "7",
    56: "8",
    57: "9",
    65: "a",
    66: "b",
    67: "c",
    68: "d",
    69: "e",
    70: "f",
    71: "g",
    72: "h",
    73: "i",
    74: "j",
    75: "k",
    76: "l",
    77: "m",
    78: "n",
    79: "o",
    80: "p",
    81: "q",
    82: "r",
    83: "s",
    84: "t",
    85: "u",
    86: "v",
    87: "w",
    88: "x",
    89: "y",
    90: "z",
    91: "left command",
    92: "right windows",
    93: "right command",
    96: "num 0",
    97: "num 1",
    98: "num 2",
    99: "num 3",
    100: "num 4",
    101: "num 5",
    102: "num 6",
    103: "num 7",
    104: "num 8",
    105: "num 9",
    106: "*",
    107: "+",
    109: "-",
    110: ".",
    111: "/",
    112: "f1",
    113: "f2",
    114: "f3",
    115: "f4",
    116: "f5",
    117: "f6",
    118: "f7",
    119: "f8",
    120: "f9",
    121: "f10",
    122: "f11",
    123: "f12",
    144: "num lock",
    145: "scroll lock",
    186: ";",
    187: "=",
    188: ",",
    189: "-",
    190: ".",
    191: "/",
    192: "`",
    219: "[",
    220: "\\",
    221: "]",
    222: "'"
};
var KeyboardState = /** @class */ (function () {
    function KeyboardState(a_repeat_period) {
        this.repeat_period = a_repeat_period;
        this.keyevents = new Array();
        this.keyboard = new Array(KEYBOARD_SIZE);
        this.old_keyboard = new Array(KEYBOARD_SIZE);
        this.time_pressed = new Array(KEYBOARD_SIZE);
        for (var i = 0; i < KEYBOARD_SIZE; i++) {
            this.keyboard[i] = false;
            this.old_keyboard[i] = false;
            this.time_pressed[i] = 0;
        }
    }
    KeyboardState.prototype.keyDown = function (event) {
        this.keyboard[event.keyCode] = true;
        this.keyevents.push(event);
    };
    KeyboardState.prototype.keyUp = function (event) {
        this.keyboard[event.keyCode] = false;
    };
    KeyboardState.prototype.cycle = function () {
        for (var i = 0; i < KEYBOARD_SIZE; i++) {
            this.old_keyboard[i] = this.keyboard[i];
            if (!this.keyboard[i])
                this.time_pressed[i] = 0;
            else
                this.time_pressed[i]++;
        }
    };
    // returns if a key has been pressed                
    // it returns true, just when the key is pressed    
    // and keeps returning yes each 4 cycles after 40  
    KeyboardState.prototype.key_press = function (key) {
        if (!this.old_keyboard[key] && this.keyboard[key])
            return true;
        if (this.keyboard[key] && this.time_pressed[key] > 40 && (this.time_pressed[key] % 4) == 0)
            return true;
        return false;
    };
    KeyboardState.prototype.key_first_press = function (key) {
        if (!this.old_keyboard[key] && this.keyboard[key])
            return true;
        return false;
    };
    KeyboardState.prototype.key_release = function (key) {
        if (this.old_keyboard[key] && !this.keyboard[key])
            return true;
        return false;
    };
    KeyboardState.prototype.consume_key_press = function (key) {
        this.old_keyboard[key] = this.keyboard[key];
    };
    KeyboardState.prototype.clearEvents = function () {
        this.keyevents = new Array();
    };
    ;
    return KeyboardState;
}());
