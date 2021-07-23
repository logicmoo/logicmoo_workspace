var 
  // RENDERING VARIABLES:
  width = 800,    
  height = 600,    
  FPS = 50,
  nextGameTick = (new Date).getTime(),
  skipTicks = 1000/FPS,
  maxFrameSkip = 10,

  // INPUT VARIABLES:
  keyboardState = new Array(256),
  oldKeyboardState = new Array(256),
  timePressed = new Array(256),
  lastKeyPressed = -1,
  mouseState = {x:0, y:0,lastX:0, lastY:0, button:0,oldButton:0},
  lastTimeMouseActive = -1;

// initialize the keyboard state as all the keys being released:
for (i = 0;i<256;i++) {
  keyboardState[i] = 0;
  oldKeyboardState[i] = 0;
  timePressed[i] = 0;
}

// get the context and set the size:
var canvas = document.getElementById('A4Engine');
var ctx = canvas.getContext('2d');  
var gamepath = canvas.getAttribute("gamepath");
var gamefile = canvas.getAttribute("gamefile");
canvas.width = width;  
canvas.height = height;
ctx.imageSmoothingEnabled = false;  
ctx.mozImageSmoothingEnabled = false;  
ctx.webkitImageSmoothingEnabled = false;  

Game.initialize(gamepath, gamefile);

// keyboard callbacks:
document.onkeydown = function(evt) {
  keyboardState[evt.keyCode] = 1;
  lastKeyPressed = evt.keyCode;

  // if CTRL/ALT/COMMAND are used, pass those events along to the browser:
  if (evt.keyCode == 17 ||
      evt.keyCode == 18 ||
      evt.keyCode == 91 ||
      evt.keyCode == 93 ||
      keyboardState[17]==true ||
      keyboardState[18]==true ||
      keyboardState[91]==true ||
      keyboardState[93]==true) return true;
  return false;
};

document.onkeyup = function(evt) {
  keyboardState[evt.keyCode] = 0;

  // if CTRL/ALT/COMMAND are used, pass those events along to the browser:
  if (evt.keyCode == 17 ||
      evt.keyCode == 18 ||
      evt.keyCode == 91 ||
      evt.keyCode == 93 ||
      keyboardState[17]==true ||
      keyboardState[18]==true ||
      keyboardState[91]==true ||
      keyboardState[93]==true) return true;
  return false;
}; 

canvas.onmousemove = function(evt){
  // get canvas position
  var obj = canvas;
  var top = 0;
  var left = 0;
  while (obj && obj.tagName != 'BODY') {
      top += obj.offsetTop;
      left += obj.offsetLeft;
      obj = obj.offsetParent;
  }

  // return relative mouse position
  var mouseX = evt.clientX - left + window.pageXOffset;
  var mouseY = evt.clientY - top + window.pageYOffset;
  if (mouseX != mouseState.lastX ||
      mouseY != mouseState.lastY) {
    if (mouseState.x!=-1) mouseState.lastX = mouseState.x;
    if (mouseState.y!=-1) mouseState.lastY = mouseState.y;
    mouseState.x = mouseX;
    mouseState.y = mouseY;
    lastTimeMouseActive = (new Date).getTime();
  }
  //console.log("mouse move at " + lastTimeMouseActive);
}

canvas.onmousedown = function(evt) {
  if (mouseState.x==-1) {
    // this is necessary, since after 1 second, the x, and y fields are cleared to -1, assuming that the mouse is inactive,
    // but the lastX, lastY variables are kept
    mouseState.x = mouseState.lastX;
    mouseState.y = mouseState.lastY;      
  }
  mouseState.button = 1;
  lastTimeMouseActive = (new Date).getTime();
}

canvas.onmouseup = function(evt) {
  if (mouseState.x==-1) {
    // this is necessary, since after 1 second, the x, and y fields are cleared to -1, assuming that the mouse is inactive,
    // but the lastX, lastY variables are kept
    mouseState.x = mouseState.lastX;
    mouseState.y = mouseState.lastY;      
  }
  mouseState.button = 0;
  lastTimeMouseActive = (new Date).getTime();
}  

keyPress = function(key) {
  if (keyboardState[key] == 1  &&  oldKeyboardState[key] == 0) return true;
  if (timePressed[key]>20 && (timePressed[key]%4)==0) return true;
  return false;
};

keyPressed = function(key) {
  if (keyboardState[key] == 1) return true;
  return false;
};


gameRun = function() {
  var loops = 0;
  if ( lastTimeMouseActive + 1000 < (new Date).getTime() ) {
    mouseState.x = -1;
    mouseState.y = -1;
  }
  while ((new Date).getTime() > nextGameTick && loops < maxFrameSkip) {
    // fast forward if "TAB" key is pressed:
    if (keyPressed(9)) {
      for(var i = 0;i<4;i++) Game.update();
    } else {
      Game.update();
    }
    for (i = 0;i<256;i++) {
      oldKeyboardState[i] = keyboardState[i];
      if (lastKeyPressed==i && keyboardState[i] == 1) timePressed[i]++;
                                                 else timePressed[i]=0;
    }
    mouseState.oldButton = mouseState.button;
    nextGameTick += skipTicks;
    loops++;
  }
  if (loops>=maxFrameSkip) nextGameTick = (new Date).getTime() + skipTicks;    
  if (loops>0) {
    canvas.width = width;  
    canvas.height = height;  
    Game.draw();
  }
};


// Start the game loop
_intervalId = setInterval(gameRun, 1000 / FPS);
