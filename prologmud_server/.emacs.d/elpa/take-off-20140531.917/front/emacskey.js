_keycode_dictionary = {
    0: "\\",
    8: "<DEL>",
    9: "<tab>",
    12: "num",
    13: "<RET>",
    16: "shift",
    17: "ctrl",
    18: "alt",
    19: "pause",
    20: "caps",
    27: "ESC",
    32: "<SPC>",
    33: "pageup",
    34: "pagedown",
    35: "end",
    36: "home",
    37: "<left>",
    38: "<up>",
    39: "<right>",
    40: "<down>",
    44: "print",
    45: "insert",
    46: "<deletechar>",
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
    91: "cmd",
    92: "cmd",
    93: "cmd",
    96: "num_0",
    97: "num_1",
    98: "num_2",
    99: "num_3",
    100: "num_4",
    101: "num_5",
    102: "num_6",
    103: "num_7",
    104: "num_8",
    105: "num_9",
    106: "num_multiply",
    107: "num_add",
    108: "num_enter",
    109: "num_subtract",
    110: "num_decimal",
    111: "num_divide",
    124: "print",
    144: "num",
    145: "scroll",
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
    222: "\'",
    223: "`",
    224: "cmd",
    225: "alt",
    57392: "ctrl",
    63289: "num"
};

function convertKeyToReadable(k){
    return _keycode_dictionary[k];
};

//US keyboard
keycodeShiftedKeys = {
    "/": "?",
    ".": ">",
    ",": "<",
    "\'": "\"",
    ";": ":",
    "[": "{",
    "]": "}",
    "\\": "|",
    "`": "~",
    "=": "+",
    "-": "_",
    "1": "!",
    "2": "@",
    "3": "#",
    "4": "$",
    "5": "%",
    "6": "^",
    "7": "&",
    "8": "*",
    "9": "(",
    "0": ")"
  };

var lowerLetter = /^[a-z]$/
function convertToShiftedKey(key, e){
    if (!e.shiftKey)
      return key;

    var k = keycodeShiftedKeys[key];
    if (k != null) {
      return k;
    }else if(lowerLetter.test(key)){
	key = key.toUpperCase();
    }
    return key;
}

function convertKey(e){
    var key = convertKeyToReadable(e.keyCode);
    return convertToShiftedKey(key, e);
}


var emacsModKeys = {
    "ctrl": "C-",
    "option": "M-",
    "alt": "M-",
    "cmd": "M-",
};
function convertToEmacsKey(key, prefix){
    if(!prefix)
	return key;
    return emacsModKeys[prefix] + key;
}

var meta = /Mac|iPod|iPhone|iPad/.test(navigator.platform) ? 'cmd' : 'ctrl';
var mod = ["alt", "option", "ctrl", "cmd"];//no "shift"

function isModKey(key){
    return mod.some(function(val){return val==key});
}

//aggregate keys and send them grouped after a delay
//emacs has no API to send keys events separately
//e.g. C- then later 'a' won't be interpreted as C-a
var sendKeyTimer = null;
//start or restart sendKeyTimer
function startTimer(){
    var timerDuration = 800;
    if(hasFinishedCommand()) timerDuration = 0;
    //give extra time if running command by name because as is emacs must
    //receive the command in one go for it to be interpreted correctly. 
    //give the user the time to type the whole command
    else if(isTypingCommandByName()) timerDuration = 2000;

    if(sendKeyTimer) clearTimeout(sendKeyTimer);
    sendKeyTimer = setTimeout(function(){
	sendKeyTimer = null;
	onTimerElapsed();
    }, timerDuration);
}

//(wrongly) assumes only one modifier key can be active at a time
var active_modifier;
var key_buffer = null;

//M-x = run command by name aka execute-extended-command
function isTypingCommandByName(){
    return key_buffer && key_buffer[0] == "M-x";
}

//Finished typing command by typing <RET>
function hasFinishedCommand(){
    //ok if key_buffer = []
    return key_buffer && key_buffer[key_buffer.length - 1] == "<RET>";
}

function onTimerElapsed(){
    if(key_buffer){
	//TODO send correct values
	console.log(key_buffer.join(' '));
	sendKey(key_buffer.join(' '))
	key_buffer = null;
    }
}

function onKeyDown(e){
    var key = convertKey(e);
    if(key.toUpperCase() == "SHIFT")
	//shift is not a special key nor a key to send to emacs
	//nothing to do
	return;
    e.preventDefault(true);
    
    if(isModKey(key)){
	active_modifier = key;
	return;
    }

    if(!active_modifier && !sendKeyTimer && !key_buffer){
	console.log(key);
	sendKey(convertToEmacsKey(key));
    }else{
	if(!key_buffer) key_buffer = [];
	key_buffer.push(convertToEmacsKey(key, active_modifier));
	startTimer();
    }
}

function onKeyUp(e){
    var key = convertKey(e);
    e.preventDefault(true);
    
    if(key == active_modifier)
	active_modifier = null;
}

function onBlur(){
    active_modifier = null;
}

var keyEvents = [
    {name: 'keydown', handler: onKeyDown}, 
    {name: 'keyup', handler: onKeyUp}, 
    {name: 'blur', handler: onBlur}];
function initkey(){
    addKeyEvents();
}

function addKeyEvents(){
    keyEvents.forEach(function(event){
	$('body').on(event.name, event.handler);
    });
}

function removeKeyEvents(){
    keyEvents.forEach(function(event){
	$('body').off(event.name, event.handler);
    });
}

function sendKey(keyString){
    ws.send(JSON.stringify({key: keyString}));
}
