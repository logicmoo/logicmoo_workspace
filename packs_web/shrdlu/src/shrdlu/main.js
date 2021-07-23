var canvas;
var ctx; // current context
var app;
var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
var PIXEL_SIZE = 4;
var WINDOW_WIDTH = 256 * PIXEL_SIZE;
var WINDOW_HEIGHT = 192 * PIXEL_SIZE;
var d = new Date();
var k;
var game_time;
var init_time;
var fonts_loaded = false;
var global_mouse_x = 0;
var global_mouse_y = 0;
var show_fps = false;
var frames_per_sec = 0;
var fps_count_start_time = null;
var fps_count = null;
var fontFamily8px = "8px MSX68";
var fontFamily16px = "16px MSX68";
var fontFamily24px = "24px MSX68";
var fontFamily32px = "32px MSX68";
var fontFamily64px = "64px MSX68";
var MSX_COLOR_BLACK = generateRGBColor(0, 0, 0);
var MSX_COLOR_GREEN = generateRGBColor(43, 221, 81);
var MSX_COLOR_LIGHT_GREEN = generateRGBColor(81, 255, 118);
var MSX_COLOR_DARK_BLUE = generateRGBColor(81, 81, 255);
var MSX_COLOR_BLUE = generateRGBColor(118, 118, 255);
var MSX_COLOR_DARK_RED = generateRGBColor(221, 81, 81);
var MSX_COLOR_LIGHT_BLUE = generateRGBColor(81, 255, 255);
var MSX_COLOR_RED = generateRGBColor(255, 81, 81);
var MSX_COLOR_LIGHT_RED = generateRGBColor(255, 118, 118);
var MSX_COLOR_DARK_GREEN = generateRGBColor(43, 187, 43);
var MSX_COLOR_GREY = generateRGBColor(192, 192, 192);
var MSX_COLOR_WHITE = generateRGBColor(255, 255, 255);
window.onload = function () {
    // get the canvas and set double buffering:
    canvas = document.getElementById('cnvs');
    ctx = canvas.getContext("2d");
    // replaces deprecated: ctx.mozImageSmoothingEnabled and ctx.webkitImageSmoothingEnabled
    ctx.imageSmoothingEnabled = false;
    app = new ShrdluApp(WINDOW_WIDTH, WINDOW_HEIGHT);
    k = new KeyboardState(-1);
    document.addEventListener('keydown', keyboardInputDown);
    document.addEventListener('keyup', keyboardInputUp);
    document.addEventListener('mousedown', mouseInput);
    document.addEventListener('mousemove', mouseMove);
    game_time = init_time = d.getTime();
    waitForResourcesLoop(0);
};
function checkIfFontsAreLoaded() {
    // This is a trick, since in JavaScript, there is no way to know if a font is loaded
    // I know how wide these images should be once the fonts are loaded, so, I wait for that!
    var tmp0 = getTextTile("TEST64", fontFamily64px, 64, "white");
    var tmp1 = getTextTile("TEST32", fontFamily32px, 32, "white");
    var tmp2 = getTextTile("TEST16", fontFamily16px, 16, "white");
    var tmp3 = getTextTile("TEST8", fontFamily8px, 8, "white");
    //    console.log("test64: " + tmp0.width);
    //    console.log("test32: " + tmp1.width);
    //    console.log("test16: " + tmp2.width);
    //    console.log("test8: " + tmp3.width);
    //    if (tmp1.width == 192 && tmp2.width == 96 && tmp3.width == 40) {
    //    if (tmp1.width == 180 && tmp2.width == 90 && tmp3.width == 37) {
    //    if (tmp1.width == 162 && tmp2.width == 81 && tmp3.width == 33) {
    if (tmp0.width == 288 && tmp1.width == 144 && tmp2.width == 72 && tmp3.width == 30) {
        //    if (tmp1.width == 216 && tmp2.width == 108 && tmp3.width == 60) {
        return true;
    }
    return false;
}
function keyboardInputDown(event) {
    k.keyDown(event);
    if (event.keyCode == KEY_CODE_F && event.getModifierState("Alt")) {
        show_fps = !show_fps;
    }
    event.preventDefault();
}
function keyboardInputUp(event) {
    k.keyUp(event);
    event.preventDefault();
}
function mouseInput(event) {
    var rect = canvas.getBoundingClientRect();
    var x = Math.floor((event.x - rect.left) / (rect.right - rect.left) * canvas.width);
    var y = Math.floor((event.y - rect.top) / (rect.bottom - rect.top) * canvas.height);
    app.mouseClick(x, y, event.button, event);
    BInterface.mouseClick(x, y, event.button, app);
}
function mouseMove(event) {
    var rect = canvas.getBoundingClientRect();
    global_mouse_x = Math.floor((event.clientX - rect.left) / (rect.right - rect.left) * canvas.width);
    global_mouse_y = Math.floor((event.clientY - rect.top) / (rect.bottom - rect.top) * canvas.height);
    BInterface.mouseMove(global_mouse_x, global_mouse_y);
}
function waitForResourcesLoop(timestamp) {
    if (app.game.imagesLoaded()) {
        app.game.finishLoadingGame(null);
        requestAnimationFrame(gameLoop);
    }
    else {
        requestAnimationFrame(waitForResourcesLoop);
    }
}
function gameLoop(timestamp) {
    if (!fonts_loaded) {
        fonts_loaded = checkIfFontsAreLoaded();
    }
    else {
        app.cycle(global_mouse_x, global_mouse_y, k);
        k.cycle();
        k.clearEvents();
        app.clearEvents();
        app.draw(WINDOW_WIDTH, WINDOW_HEIGHT);
        // count FPS:
        if (fps_count_start_time == null) {
            fps_count_start_time = timestamp;
        }
        else {
            fps_count++;
            if (timestamp > fps_count_start_time + 1000) {
                frames_per_sec = fps_count;
                fps_count = 0;
                fps_count_start_time = timestamp;
            }
        }
        if (show_fps) {
            ctx.fillStyle = "white";
            ctx.textBaseline = "bottom";
            ctx.textAlign = "center";
            ctx.font = fontFamily8px;
            ctx.fillText("fps: " + frames_per_sec, WINDOW_WIDTH / 2, WINDOW_HEIGHT - 16);
        } // if
    }
    requestAnimationFrame(gameLoop);
}
