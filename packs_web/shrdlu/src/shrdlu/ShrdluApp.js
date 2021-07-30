/*

Note (santi):
- SHRDLU was built on top of the A4Engine (a little game engine I wrote to test some PCG algorithms,
  and that can be found here: https://github.com/santiontanon/A4Engine). I translated the engine to TypeScript and started
  building SHRDLU on top of that. After the game was working, I started then removing all the functionality from the
  A4Engine that SHRDLU doesn't need (such as magic, attacks, equiping items, etc.), and adding some SHRDLU-specific code.
- This class (ShrdluApp) implements the basic finite state machine for the control flow of the game (transitioning from the main
  menu, to the game, etc.). The ShrdluA4Game/A4Game class is the one that actually implements the game, and the one that stores the game state.

*/
var SHRDLU_VERSION = "v4.0";
var A4ENGINE_STATE_INTRO = 0;
var A4ENGINE_STATE_TITLESCREEN = 1;
var A4ENGINE_STATE_QUIT = 2;
var A4ENGINE_STATE_GAME = 3;
var A4ENGINE_STATE_GAMECOMPLETE = 4;
var A4ENGINE_STATE_ACT1INTRO = 5;
var A4ENGINE_STATE_ACT2INTRO = 6;
var A4ENGINE_STATE_ACT3INTRO = 7;
var A4ENGINE_STATE_GAMEOVER = 8;
var A4ENGINE_STATE_ACHIEVEMENTS_MENU = 9;
var A4ENGINE_STATE_ACHIEVEMENTS_INGAME = 10;
var MAX_VOLUME = 1.0;
var DEFAULT_game_path = "data";
var DEFAULT_game_file = "shrdlu.xml";
var SHRDLU_ONTOLOGY_PATH = "data/shrdluontology.xml";
var A4CONFIG_STORAGE_KEY = "SHRDLU-configuration";
var ACHIEVEMENTS_STORAGE_KEY = "SHRDLU-achievements";
var A4SAVEGAME_STORAGE_KEY = "SHRDLU-savegame";
var INGAME_MENU = 1;
var INGAME_INSTRUCTIONS = 1;
var INGAME_SAVE_MENU = 6;
var INGAME_LOAD_MENU = 7;
var INGAME_INSTRUCTIONS_MENU = 8;
var INGAME_TUTORIAL = 9;
var QUIT_REQUEST_ACTION_QUIT = 0;
var QUIT_REQUEST_ACTION_ACHIEVEMENTS = 1;
var QUIT_REQUEST_ACTION_LOAD1 = 2;
var QUIT_REQUEST_ACTION_LOAD2 = 3;
var QUIT_REQUEST_ACTION_LOAD3 = 4;
var QUIT_REQUEST_ACTION_LOAD4 = 5;
var ShrdluApp = /** @class */ (function () {
    function ShrdluApp(a_dx, a_dy) {
        this.mouse_click_last_frame = false;
        // intro:
        this.intro_logoaux = null;
        this.intro_logofader = null;
        this.titlescreen_requested_gender = null;
        this.quit_request = false;
        this.quit_request_cycle = 0;
        this.quit_request_action = 0;
        this.take_press_used_for_push = false;
        this.tutorialMessages = []; // if this list is non-empty, the game will be paused, to display a tutorial message
        // state introact:
        this.introact_state = 0;
        this.introact_state_timer = 0;
        // game complete state:
        this.gamecomplete_state = 0;
        this.gamecomplete_logoaux = null;
        this.gamecomplete_logofader = null;
        // game over state:
        this.gameover_type = 0;
        this.gameover_state = 0;
        this.gameover_state_timer = 0;
        // achievements data:
        this.achievements_state = 0;
        this.achievements_state_timer = 0;
        this.achievement_names = [
            "Complete Tutorial",
            "Complete Act 1",
            "Complete Act 2",
            "Complete Act 3",
            "See both endings",
            "Both genders",
            "Hungry and thirsty",
            "Maker",
            "Low on oxygen",
            "In a rush",
            "Natural language",
            "Asky",
            "Etaoin at your service",
            "Robots at your service",
            "Justification",
            "Three is not enough",
            "Posters",
            "Diaries",
            "Asimov",
            "Vintage computer",
            "The secret of Aurora",
            "Debugging",
        ];
        this.achievement_descriptions = [
            ["Obtained when you complete the game",
                "tutorial."],
            ["Obtained when you complete Act 1 of",
                "the game."],
            ["Obtained when you complete Act 2 of",
                "the game."],
            ["Obtained when you complete Act 3 of",
                "the game."],
            ["Obtained when you see the different",
                "endings of the game."],
            ["Obtained when you play at least once",
                "with a female character and once with",
                "a male character."],
            ["You just woke up form a long time in",
                "stasis. Find something to eat and to",
                "drink, and consume it."],
            ["Have you seen the 3d printers? Print",
                "an item of each of the types that",
                "they can produce."],
            ["Risk walking with oxygen level so low",
                "that Etaoin warns you about it."],
            ["Walking feels too slow? Check the",
                "instructions for how to walk faster!"],
            ["Say something the AI cannot parse."],
            ["The AI can understand many different",
                "types of question. Ask at least one",
                "question of each of the types it",
                "knows how to respond to."],
            ["Etaoin cannot just talk, it controls",
                "the station. Make it do one of each",
                "of the actions it can perform."],
            ["Qwerty and Shrdlu can be very handy,",
                "make them execute one of each of the",
                "actions they can perform."],
            ["Do you believe what the AI said?",
                "Manage to get it to justify one of",
                "their answers."],
            ["Some times the answer of the AI is",
                "an enumeration. The AI only gives",
                "the first three answers, followed by",
                "\"...\". But you can ask for more!"],
            ["Find all the science fiction posters",
                "in the game."],
            ["Find and read all the diaries and",
                "data pads in the game."],
            ["Do you know the three laws of",
                "robotics? Make the AI remind you."],
            ["Someone in the station was a retro-",
                "computer fan, find his most precious",
                "possession."],
            ["Aurora might not be as dead as it",
                "seems. Can you find evidence of it?"],
            ["Debugging the natural language side",
                "of this game is very hard! Help me",
                "by downloading a debug log (from",
                "the in-game menu)! The achievement",
                "is given for just downloading it.",
                "But e-mailing me a log generated",
                "after you've played the game for",
                "a while is the real way to help the",
                "game get better!"],
        ];
        this.achievement_completed_alerts_shown = [
            false, false, false, false, false,
            false, false, false,
            false, false, false, false, false, false,
            false, false, false, false, false,
            false, false, false
        ];
        this.achievement_complete_tutorial = false;
        this.achievement_complete_act1 = false;
        this.achievement_complete_act2 = false;
        this.achievement_complete_act3 = false;
        this.achievement_complete_see_all_endings = [false, false];
        this.achievement_both_genders = [false, false];
        this.achievement_interact_eat_drink = [false, false];
        this.achievement_interact_3d_printed_one_of_each_kind = [false, false, false, false, false, false, false, false, false, false, false];
        this.achievement_interact_low_oxygen = false;
        this.achievement_interact_in_a_rush = false;
        this.achievement_nlp_parse_error = false;
        this.achievement_nlp_all_types_of_questions = [false, false, false, false, false, false, false, false, false, false];
        this.achievement_nlp_all_etaoin_actions = [false, false, false, false, false, false];
        this.achievement_nlp_all_robot_actions = [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false];
        this.achievement_nlp_resolution_explanation = false;
        this.achievement_nlp_asked_for_more = false;
        this.achievement_secret_posters = [false, false];
        this.achievement_secret_diaries = [false, false, false, false, false];
        this.achievement_secret_life_in_aurora = false;
        this.achievement_secret_msx = false;
        this.achievement_secret_3_laws_of_robotics = false;
        this.achievement_debug_log = false;
        this.achievement_alert = -1;
        this.achievement_alert_timer = 0;
        // AI debugger:
        this.SHRDLU_AI_debugger = new RuleBasedAIDebugger();
        this.writeLogsToServer = false;
        this.lastServerWrite = 0;
        A4Script.registerCustomScript("cutScene", function (xml, id) {
            var s = new A4Script(id, null, null, 0, false, false);
            s.value = Number(xml.getAttribute("cutscene"));
            return s;
        }, function (s) {
            return " cutscene=\"" + s.value + "\"";
        }, function (script, o, map, game, otherCharacter) {
            game.cutSceneActivated = script.value;
            game.cutScenes.cutSceneState = 0;
            game.cutScenes.cutSceneStateTimer = 0;
            return SCRIPT_FINISHED;
        });
        A4Script.registerCustomScript("refillOxygen", function (xml, id) {
            var s = new A4Script(id, null, null, 0, false, false);
            return s;
        }, function (s) {
            return "";
        }, function (script, o, map, game, otherCharacter) {
            game.suit_oxygen = SHRDLU_MAX_SPACESUIT_OXYGEN;
            return SCRIPT_FINISHED;
        });
        A4Script.registerCustomScript("embarkOnGarage", function (xml, id) {
            var s = new A4Script(id, null, null, 0, false, false);
            return s;
        }, function (s) {
            return "";
        }, function (script, o, map, game, otherCharacter) {
            if (otherCharacter != game.currentPlayer)
                return SCRIPT_FAILED;
            if (o.sort.is_a_string("garage-rover")) {
                game.takeRoverOutOfTheGarage(o, otherCharacter);
            }
            else if (o.sort.is_a_string("garage-shuttle")) {
                game.takeShuttleToTrantorCrater(o, otherCharacter);
            }
            return SCRIPT_FINISHED;
        });
        this.loadConfiguration();
        this.loadAchievements();
        this.game = null;
        this.state = A4ENGINE_STATE_INTRO;
        this.previous_state = this.state;
        this.state_cycle = 0;
        this.screen_width = a_dx;
        this.screen_height = a_dy;
        this.GLTM = new GLTManager();
        this.SFXM = new SFXManager();
        if (this.game_path != null && this.game_filename != null) {
            var fullPath = this.game_path + "/" + this.game_filename;
            var xmlhttp = new XMLHttpRequest();
            xmlhttp.overrideMimeType("text/xml");
            xmlhttp.open("GET", fullPath, false);
            xmlhttp.send();
            this.gameDefinition = xmlhttp.responseXML.documentElement;
            console.log("Game definition loaded from '" + fullPath + "'");
        }
        else {
            this.gameDefinition = null;
            console.log("Game definition not loaded...");
        }
        this.ingame_menu = 0;
        this.trade_dialog_player = null;
        this.trade_dialog_other = null;
        this.trade_needs_update = false;
        console.log("ShrdluApp created.");
        if (this.gameDefinition != null) {
            this.game = new ShrdluA4Game(this.gameDefinition, this.game_path, SHRDLU_ONTOLOGY_PATH, this.GLTM, this.SFXM, this.SFX_volume, null, this);
        }
    }
    ShrdluApp.prototype.loadConfiguration = function () {
        var config_xml = null;
        var configString = localStorage.getItem(A4CONFIG_STORAGE_KEY);
        if (configString != null) {
            console.log("Found config stored in the browser, loading it...");
            // if we can find a configuration saved in the browser, load it:
            var oParser = new DOMParser();
            config_xml = oParser.parseFromString(configString, "text/xml").documentElement;
        }
        if (config_xml != null) {
            var defaultGame = getElementChildrenByTag(config_xml, "defaultGame");
            var volume_xml = getElementChildrenByTag(config_xml, "volume");
            var controls_xml = getElementChildrenByTag(config_xml, "controls");
            if (defaultGame != null && defaultGame.length > 0) {
                var path = defaultGame[0].getAttribute("path");
                var gameFile = defaultGame[0].getAttribute("gamefile");
                if (path != null) {
                    this.game_path = path;
                }
                else {
                    console.log("Cannot find 'path' tag in defaultGame in the config file!");
                    this.game_path = null;
                }
                if (gameFile != null) {
                    this.game_filename = gameFile;
                }
                else {
                    console.log("Cannot find 'gamefile' tag in defaultGame in the config file!");
                    this.game_filename = null;
                }
            }
            else {
                console.log("Cannot find 'defaultGame' tag in config file!");
                this.game_path = null;
                this.game_filename = null;
            }
            if (volume_xml != null && volume_xml.length > 0) {
                this.SFX_volume = Number(volume_xml[0].getAttribute("sfx"));
            }
            else {
                console.log("Cannot find 'volume' tag in config file!");
            }
            if (controls_xml != null && controls_xml.length > 0) {
                var key_xml = null;
                key_xml = getFirstElementChildByTag(controls_xml[0], "messageconsole_up");
                this.key_messageconsole_up = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "messageconsole_down");
                this.key_messageconsole_down = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "inventory_toggle");
                this.key_inventory_toggle = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "left");
                this.key_left = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "right");
                this.key_right = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "up");
                this.key_up = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "down");
                this.key_down = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "take");
                this.key_take = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "talk");
                this.key_talk = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "use_item");
                this.key_use_item = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "drop_item");
                this.key_drop_item = Number(key_xml.getAttribute("key"));
                key_xml = getFirstElementChildByTag(controls_xml[0], "fast_time");
                this.key_fast_time = Number(key_xml.getAttribute("key"));
            }
            else {
                this.setDefaultConfiguration();
            }
        }
        else {
            // default configuration:
            this.game_path = DEFAULT_game_path;
            this.game_filename = DEFAULT_game_file;
            this.setDefaultConfiguration();
        }
    };
    ShrdluApp.prototype.setDefaultConfiguration = function () {
        console.log("Setting default config!");
        this.SFX_volume = MAX_VOLUME;
        this.key_messageconsole_up = KEY_CODE_PAGEUP;
        this.key_messageconsole_down = KEY_CODE_PAGEDOWN;
        this.key_inventory_toggle = KEY_CODE_TAB;
        this.key_left = KEY_CODE_LEFT;
        this.key_right = KEY_CODE_RIGHT;
        this.key_up = KEY_CODE_UP;
        this.key_down = KEY_CODE_DOWN;
        this.key_take = KEY_CODE_SPACE;
        this.key_talk = KEY_CODE_RETURN;
        this.key_use_item = KEY_CODE_U;
        this.key_drop_item = KEY_CODE_O;
        this.key_fast_time = KEY_CODE_LSHIFT;
    };
    ShrdluApp.prototype.saveConfiguration = function () {
        var configString = "<A4Configuration>";
        configString += "<volume sfx=\"" + this.SFX_volume + "\"/>";
        configString += "<defaultGame path=\"" + this.game_path + "\" gamefile=\"" + this.game_filename + "\"/>";
        configString += "<controls>";
        configString += "<messageconsole_up key=\"" + this.key_messageconsole_up + "\"/>";
        configString += "<messageconsole_down key=\"" + this.key_messageconsole_down + "\"/>";
        configString += "<inventory_toggle key=\"" + this.key_inventory_toggle + "\"/>";
        configString += "<left key=\"" + this.key_left + "\"/>";
        configString += "<right key=\"" + this.key_right + "\"/>";
        configString += "<up key=\"" + this.key_up + "\"/>";
        configString += "<down key=\"" + this.key_down + "\"/>";
        configString += "<take key=\"" + this.key_take + "\"/>";
        configString += "<talk key=\"" + this.key_talk + "\"/>";
        configString += "<use_item key=\"" + this.key_use_item + "\"/>";
        configString += "<drop_item key=\"" + this.key_drop_item + "\"/>";
        configString += "<fast_time key=\"" + this.key_fast_time + "\"/>";
        configString += "</controls>";
        configString += "</A4Configuration>";
        localStorage.setItem(A4CONFIG_STORAGE_KEY, configString);
        console.log("Config saved to the browser ... (key: " + A4CONFIG_STORAGE_KEY + ")");
    };
    ShrdluApp.prototype.cycle = function (mouse_x, mouse_y, k) {
        try {
            var old_state = this.state;
            if (this.state_cycle == 0)
                console.log("First Cycle started for state " + this.state + "...");
            switch (this.state) {
                case A4ENGINE_STATE_INTRO:
                    this.state = this.intro_cycle(k);
                    break;
                case A4ENGINE_STATE_TITLESCREEN:
                    this.state = this.titlescreen_cycle(k);
                    break;
                case A4ENGINE_STATE_GAME:
                    this.state = this.game_cycle(k);
                    break;
                case A4ENGINE_STATE_GAMECOMPLETE:
                    this.state = this.gamecomplete_cycle(k);
                    break;
                case A4ENGINE_STATE_ACT1INTRO:
                    this.state = this.actintro_cycle(k, 1);
                    break;
                case A4ENGINE_STATE_ACT2INTRO:
                    this.state = this.actintro_cycle(k, 2);
                    break;
                case A4ENGINE_STATE_ACT3INTRO:
                    this.state = this.actintro_cycle(k, 3);
                    break;
                case A4ENGINE_STATE_GAMEOVER:
                    this.state = this.gameover_cycle(k);
                    break;
                case A4ENGINE_STATE_ACHIEVEMENTS_MENU:
                    this.state = this.achievements_cycle(k, false);
                    break;
                case A4ENGINE_STATE_ACHIEVEMENTS_INGAME:
                    this.state = this.achievements_cycle(k, true);
                    break;
                default:
                    return false;
            }
            if (old_state == this.state) {
                this.state_cycle++;
            }
            else {
                this.state_cycle = 0;
                console.log("State change: " + old_state + " -> " + this.state);
            }
            this.SFXM.next_cycle();
            this.previous_state = old_state;
            BInterface.update(global_mouse_x, global_mouse_y, k, app);
        }
        catch (e) {
            console.error(e);
            this.game.errorMessagesForLog.push(["" + e + "%0a    " + e.stack.split("\n").join("%0a    "), "" + this.game.in_game_seconds]);
            this.game.addMessageWithColor("[ERROR: an internal game error occurred, please press ESC and send a debug log to the developer]", "red");
            this.game.etaoinAI.intentions = []; // clear the intentions to prevent the error from happening again and again if possible
            this.game.qwertyAI.intentions = [];
            this.game.shrdluAI.intentions = [];
        }
        this.mouse_click_last_frame = false;
        if (this.achievement_alert >= 0) {
            this.achievement_alert_timer++;
            if (this.achievement_alert_timer > 256)
                this.achievement_alert = -1;
        }
        return true;
    };
    ShrdluApp.prototype.draw = function (SCREEN_WIDTH, SCREEEN_HEIGHT) {
        this.screen_width = SCREEN_WIDTH;
        this.screen_height = SCREEEN_HEIGHT;
        // If no CYCLE has been executed for this state, do not redraw:
        if (this.state_cycle == 0)
            return;
        try {
            ctx.fillStyle = "black";
            ctx.fillRect(0, 0, this.screen_width, this.screen_height);
            switch (this.state) {
                case A4ENGINE_STATE_INTRO:
                    this.intro_draw();
                    break;
                case A4ENGINE_STATE_TITLESCREEN:
                    this.titlescreen_draw();
                    break;
                case A4ENGINE_STATE_GAME:
                    this.game_draw();
                    break;
                case A4ENGINE_STATE_GAMECOMPLETE:
                    this.gamecomplete_draw();
                    break;
                case A4ENGINE_STATE_ACT1INTRO:
                    this.actintro_draw(1);
                    break;
                case A4ENGINE_STATE_ACT2INTRO:
                    this.actintro_draw(2);
                    break;
                case A4ENGINE_STATE_ACT3INTRO:
                    this.actintro_draw(3);
                    break;
                case A4ENGINE_STATE_GAMEOVER:
                    this.gameover_draw();
                    break;
                case A4ENGINE_STATE_ACHIEVEMENTS_MENU:
                case A4ENGINE_STATE_ACHIEVEMENTS_INGAME:
                    this.achievements_draw();
                    break;
            }
        }
        catch (e) {
            console.error(e);
            this.game.errorMessagesForLog.push(["" + e + "%0a    " + e.stack.split("\n").join("%0a    "), "" + this.game.in_game_seconds]);
            this.game.addMessageWithColor("[ERROR: an internal game error occurred, please press ESC and send a debug log to the developer]", "red");
            this.game.etaoinAI.intentions = []; // clear the intentions to prevent the error from happening again and again
            this.game.qwertyAI.intentions = [];
            this.game.shrdluAI.intentions = [];
        }
        if (this.achievement_alert >= 0) {
            var x = 0;
            var y = 184;
            var text = "Achievement: " + this.achievement_names[this.achievement_alert];
            if (this.achievement_alert_timer < 64) {
                x = 4 * (this.achievement_alert_timer - 64);
            }
            else if (this.achievement_alert_timer > 196) {
                x = 4 * (196 - this.achievement_alert_timer);
            }
            ctx.fillStyle = MSX_COLOR_DARK_BLUE;
            ctx.fillRect(x * PIXEL_SIZE, (y - 1) * PIXEL_SIZE, (text.length * 6 + 1) * PIXEL_SIZE, 9 * PIXEL_SIZE);
            fillTextTopLeft(text, x * PIXEL_SIZE, y * PIXEL_SIZE, fontFamily32px, MSX_COLOR_WHITE);
        }
    };
    ShrdluApp.prototype.mouseClick = function (mouse_x, mouse_y, button, event) {
        this.mouse_click_last_frame = true;
        if (this.state == A4ENGINE_STATE_GAME) {
            if (this.ingame_menu != INGAME_MENU &&
                this.ingame_menu != INGAME_LOAD_MENU &&
                this.ingame_menu != INGAME_SAVE_MENU) {
                if (this.SHRDLU_AI_debugger.AI != null) {
                    this.SHRDLU_AI_debugger.mouseClick(mouse_x, mouse_y, button);
                }
                this.game.mouseClick(mouse_x, mouse_y, button);
            }
        }
    };
    ShrdluApp.prototype.clearEvents = function () {
    };
    ShrdluApp.prototype.intro_cycle = function (k) {
        var textShowTime = 600;
        var sceneDuration = SHRDLU_FADEIN_TIME * 2 + textShowTime;
        var currentScene = Math.floor(this.state_cycle / sceneDuration);
        if (k.key_press(KEY_CODE_ESCAPE)) {
            if (this.state_cycle < 350) {
                this.state_cycle = 350;
            }
            else {
                return A4ENGINE_STATE_TITLESCREEN;
            }
        }
        if (k.key_press(KEY_CODE_SPACE) ||
            k.key_press(KEY_CODE_RETURN) ||
            this.mouse_click_last_frame) {
            if (this.state_cycle < 11 * sceneDuration) {
                this.state_cycle = (currentScene + 1) * sceneDuration;
            }
            else {
                return A4ENGINE_STATE_TITLESCREEN;
            }
        }
        // the title appearing scene takes 800 cycles
        if (this.state_cycle > 11 * sceneDuration + 800)
            return A4ENGINE_STATE_TITLESCREEN;
        return A4ENGINE_STATE_INTRO;
    };
    ShrdluApp.prototype.intro_draw = function () {
        // each of these parts is in screen for SHRDLU_FADEIN_TIME*2+300 cycles
        var images = ["data/braingames.png",
            "data/cutscene-intro1.png",
            "data/cutscene-intro1.png",
            "data/cutscene-intro1.png",
            "data/cutscene-intro2.png",
            "data/cutscene-intro2.png",
            "data/cutscene-intro2.png",
            "data/cutscene-intro3.png",
            "data/cutscene-intro3.png",
            "data/cutscene-intro3.png",
            null];
        var text = [null,
            "Planet Earth, Sol system, year 2304.",
            "Despite significant technological\nadvancements, planet Earth was still\nhumanity's only home.",
            "But that was about to change...",
            "Due to unknown reasons, the amount of\nradiation coming from Earth's star,\nthe Sun, increased to lethal levels.",
            "Earth was becoming sterile, and very few\nregions of the planet remained habitable.",
            "It was time to find a new home...",
            "Large colony ships were built, holding\nmost of humanity's remaining population.",
            "The fleet consisted of 16 ships, each\nheaded for a different nearby star,\nwith the hope of finding a new home...",
            "One of them was sent on a 48 year\njourney to nearby Tau Ceti, where a\nplanet called Aurora was one of the\nmost promising alternatives.",
            "However, not everything unfolded\naccording to plan..."];
        var textShowTime = 600;
        var sceneDuration = SHRDLU_FADEIN_TIME * 2 + textShowTime;
        var currentScene = Math.floor(this.state_cycle / sceneDuration);
        var sceneTime = this.state_cycle - currentScene * sceneDuration;
        var f1 = 1.0;
        if (currentScene < 11) {
            ctx.save();
            ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
            if (images[currentScene] != null) {
                var img_bg = this.game.GLTM.get(images[currentScene]);
                if (img_bg != null) {
                    if (currentScene == 0)
                        img_bg.draw(40, 72);
                    else
                        img_bg.draw(0, 0);
                }
            }
            if (text[currentScene] != null) {
                var lines = text[currentScene].split("\n");
                var y = 192 - lines.length * 10;
                for (var _i = 0, lines_1 = lines; _i < lines_1.length; _i++) {
                    var line = lines_1[_i];
                    fillTextTopLeft(line, 8, y, fontFamily8px, MSX_COLOR_WHITE);
                    y += 10;
                }
            }
            ctx.restore();
            if (images[currentScene] != null) {
                f1 = 1;
                if (sceneTime < SHRDLU_FADEIN_TIME) {
                    if (currentScene > 0 && images[currentScene - 1] == images[currentScene]) {
                        f1 = 1;
                    }
                    else {
                        f1 = sceneTime / SHRDLU_FADEIN_TIME;
                    }
                }
                if (sceneTime > textShowTime + SHRDLU_FADEIN_TIME) {
                    if (currentScene < 10 && images[currentScene + 1] == images[currentScene]) {
                        f1 = 1;
                    }
                    else {
                        f1 = 1 - ((sceneTime - (textShowTime + SHRDLU_FADEIN_TIME)) / SHRDLU_FADEIN_TIME);
                    }
                }
                drawFadeInOverlay(1 - f1);
            }
        }
        else {
            // Game Title:
            //        if (this.state_cycle>=350) {
            ctx.save();
            ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
            sceneTime = this.state_cycle - 11 * sceneDuration;
            // background:
            f1 = sceneTime / 800.0;
            if (f1 > 1)
                f1 = 1;
            var img_bg = this.game.GLTM.get("data/shrdlu-title-bg.png");
            if (img_bg != null)
                img_bg.draw(0, Math.floor((1 - f1) * 64));
            // title:
            if (sceneTime >= 250) {
                if (sceneTime == 250) {
                    this.intro_logoaux = document.createElement('canvas');
                    this.intro_logoaux.width = 192;
                    this.intro_logoaux.height = 60;
                    this.intro_logofader = new FizzleFade(192, 60);
                }
                var img_logo = this.game.GLTM.get("data/shrdlu-title-logo.png");
                if (img_logo != null) {
                    for (var i = 0; i < 25; i++) {
                        var xy = this.intro_logofader.nextPixelToFizzle();
                        if (xy != null) {
                            this.intro_logoaux.getContext("2d").drawImage(img_logo.src, xy[0], xy[1], 1, 1, xy[0], xy[1], 1, 1);
                        }
                    }
                }
                ctx.drawImage(this.intro_logoaux, 32, 32);
            }
            // title:
            if (sceneTime >= 550) {
                var img_credits = this.game.GLTM.get("data/shrdlu-title-credits.png");
                if (img_credits != null)
                    img_credits.draw(32, 176);
            }
            ctx.restore();
        }
    };
    ShrdluApp.prototype.titlescreen_cycle = function (k) {
        if (this.state_cycle == 0) {
            // create the menus:
            this.titlescreen_state = 0;
            this.titlescreen_timer = 0;
            BInterface.reset();
            var menuItems = [];
            var menuCallbacks = [];
            menuItems.push("Play");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                if (app.titlescreen_state == 2)
                    return;
                BInterface.push();
                BInterface.addElement(new BShrdluTextFrame(["Play as a..."], false, fontFamily32px, 32, 48 * PIXEL_SIZE, 40 * PIXEL_SIZE, 160 * PIXEL_SIZE, 72 * PIXEL_SIZE, app.GLTM));
                BInterface.addElement(new BShrdluButton("female character", fontFamily32px, app.screen_width / 2 - 80, 64 * PIXEL_SIZE, 160, 8 * PIXEL_SIZE, 41, "white", true, function (arg, ID) {
                    app.titlescreen_state = 2;
                    app.titlescreen_timer = 0;
                    app.titlescreen_requested_gender = "female";
                    app.ingame_menu = 0;
                    app.quit_request = false;
                    app.achievement_both_genders[0] = true;
                    app.trigger_achievement_complete_alert();
                }));
                BInterface.addElement(new BShrdluButton("male character", fontFamily32px, app.screen_width / 2 - 80, 72 * PIXEL_SIZE, 160, 8 * PIXEL_SIZE, 42, "white", true, function (arg, ID) {
                    app.titlescreen_state = 2;
                    app.titlescreen_timer = 0;
                    app.titlescreen_requested_gender = "male";
                    app.ingame_menu = 0;
                    app.quit_request = false;
                    app.achievement_both_genders[1] = true;
                    app.trigger_achievement_complete_alert();
                }));
                BInterface.addElement(new BShrdluButton("Back", fontFamily32px, app.screen_width / 2 - 80, 88 * PIXEL_SIZE, 160, 64, 43, "white", true, function (arg, ID) {
                    BInterface.pop();
                }));
            });
            if (this.game.allowSaveGames) {
                menuItems.push("load");
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    if (app.titlescreen_state == 2)
                        return;
                    var menuItems = [];
                    var menuCallbacks = [];
                    for (var i = 0; i < 4; i++) {
                        var saveName = app.game.checkSaveGame("slot" + (i + 1));
                        if (saveName != null) {
                            menuItems.push("slot" + (i + 1) + ": " + saveName);
                            menuCallbacks.push(function (arg, ID) {
                                ID -= 9; // convert from menu item ID to slot ID
                                var app = arg;
                                var xmlString = LZString.decompressFromUTF16(localStorage.getItem(A4SAVEGAME_STORAGE_KEY + "-slot" + ID));
                                console.log("Decompresed string is: " + xmlString.length);
                                //                                            console.log(xmlString);
                                var dp = new DOMParser();
                                var xml = dp.parseFromString(xmlString, "text/xml");
                                var gamexml = getFirstElementChildByTag(xml.documentElement, "A4Game");
                                //                                            console.log(xml);
                                app.titlescreen_state = 3;
                                app.titlescreen_timer = 0;
                                app.ingame_menu = 0;
                                app.quit_request = false;
                                app.game = new ShrdluA4Game(gamexml, app.game_path, SHRDLU_ONTOLOGY_PATH, app.GLTM, app.SFXM, app.SFX_volume, null, app);
                                app.game.finishLoadingGame(xml.documentElement);
                            });
                        }
                        else {
                            menuItems.push("slot" + (i + 1));
                            menuCallbacks.push(null);
                        }
                    }
                    menuItems.push("back");
                    menuCallbacks.push(function (arg, ID) {
                        BInterface.pop();
                    });
                    BInterface.push();
                    createShrdluMenu(menuItems, menuCallbacks, fontFamily32px, 32, app.screen_width / 2 - 10 * 8 * PIXEL_SIZE, app.screen_height / 2 - 4 * 8 * PIXEL_SIZE, 20 * 8 * PIXEL_SIZE, 7 * 8 * PIXEL_SIZE, 0, 10, app.GLTM);
                    for (var i = 0; i < 4; i++) {
                        var saveName = app.game.checkSaveGame("slot" + (i + 1));
                        if (saveName == null) {
                            BInterface.disable(i + 10);
                        }
                    }
                });
            }
            menuItems.push("Instructions");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                if (app.titlescreen_state == 2)
                    return;
                BInterface.push();
                BInterface.addElement(new BShrdluTextFrame(getShrdluInstructionsString(), false, fontFamily32px, 32, 8 * PIXEL_SIZE, 8 * PIXEL_SIZE, 240 * PIXEL_SIZE, 164 * PIXEL_SIZE, app.GLTM));
                BInterface.addElement(new BShrdluButton("Back", fontFamily32px, app.screen_width / 2 - 80, app.screen_height - 206, 160, 64, 30, "white", true, function (arg, ID) {
                    BInterface.pop();
                }));
            });
            menuItems.push("Achievements");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                if (app.titlescreen_state == 2)
                    return;
                app.titlescreen_state = 4;
                app.titlescreen_timer = 0;
            });
            //            menuItems.push("configuration");
            //            menuCallbacks.push(createConfigurationMenu);
            createShrdluMenu(menuItems, menuCallbacks, fontFamily32px, 32, this.screen_width / 2 - 7 * 8 * PIXEL_SIZE, this.screen_height / 2 + 3 * 8 * PIXEL_SIZE, 14 * 8 * PIXEL_SIZE, (6 * 8) * PIXEL_SIZE, 0, 1, this.GLTM);
            // if there are no savegames, load is disabled:
            var anySaveGame = false;
            for (var i = 0; i < 4; i++) {
                var saveName = app.game.checkSaveGame("slot" + (i + 1));
                if (saveName != null) {
                    anySaveGame = true;
                    break;
                }
            }
            if (!anySaveGame)
                BInterface.disable(2);
            this.titlescreen_angle = 0;
            this.titlescreen_zoom = 1;
            this.titlescreen_x = 0;
            this.titlescreen_y = 0;
        }
        if (this.titlescreen_state == 0) {
            this.titlescreen_timer++;
            if (this.titlescreen_timer > SHRDLU_FADEIN_TIME)
                this.titlescreen_state = 1;
        }
        else if (this.titlescreen_state == 2) {
            this.titlescreen_timer++;
            if (this.titlescreen_timer > SHRDLU_FADEIN_TIME) {
                BInterface.reset();
                app.game = new ShrdluA4Game(app.gameDefinition, app.game_path, SHRDLU_ONTOLOGY_PATH, app.GLTM, app.SFXM, app.SFX_volume, this.titlescreen_requested_gender, app);
                app.game.finishLoadingGame(null);
                if (this.writeLogsToServer)
                    assignNewSessionID(app.game);
                return A4ENGINE_STATE_ACT1INTRO;
            }
        }
        else if (this.titlescreen_state == 3) {
            this.titlescreen_timer++;
            if (this.titlescreen_timer > SHRDLU_FADEIN_TIME) {
                BInterface.reset();
                return A4ENGINE_STATE_GAME;
            }
        }
        else if (this.titlescreen_state == 4) {
            this.titlescreen_timer++;
            if (this.titlescreen_timer > SHRDLU_FADEIN_TIME) {
                BInterface.reset();
                return A4ENGINE_STATE_ACHIEVEMENTS_MENU;
            }
        }
        return A4ENGINE_STATE_TITLESCREEN;
    };
    ShrdluApp.prototype.titlescreen_draw = function () {
        var f1 = 1;
        if (this.titlescreen_state == 0) {
        }
        else if (this.titlescreen_state == 2 || this.titlescreen_state == 3 || this.titlescreen_state == 4) {
            f1 = 1 - this.titlescreen_timer / SHRDLU_FADEIN_TIME;
            if (f1 < 0)
                f1 = 0;
            if (f1 > 1)
                f1 = 1;
        }
        ctx.save();
        ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
        var img_bg = this.game.GLTM.get("data/shrdlu-title-bg.png");
        if (img_bg != null)
            img_bg.draw(0, 0);
        var img_logo = this.game.GLTM.get("data/shrdlu-title-logo.png");
        if (img_logo != null)
            img_logo.draw(32, 32);
        var img_credits = this.game.GLTM.get("data/shrdlu-title-credits.png");
        if (img_credits != null)
            img_credits.draw(32, 176);
        fillTextTopLeft(SHRDLU_VERSION, 1, 1, fontFamily8px, MSX_COLOR_WHITE);
        ctx.restore();
        BInterface.drawAlpha(1);
        drawFadeInOverlay(1 - f1);
    };
    ShrdluApp.prototype.createInGameMenu = function (menu, target) {
        this.ingame_menu = menu;
        if (menu == INGAME_MENU) {
            var menuItems = [];
            var menuCallbacks = [];
            menuItems.push("Play");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                BInterface.pop();
                app.ingame_menu = 0;
            });
            if (this.game.allowSaveGames) {
                menuItems.push("Save");
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    app.createInGameMenu(INGAME_SAVE_MENU, null);
                });
                menuItems.push("Load");
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    app.createInGameMenu(INGAME_LOAD_MENU, null);
                });
            }
            menuItems.push("Instructions");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                BInterface.push();
                BInterface.addElement(new BShrdluTextFrame(getShrdluInstructionsString(), false, fontFamily32px, 32, 8 * PIXEL_SIZE, 8 * PIXEL_SIZE, 240 * PIXEL_SIZE, 144 * PIXEL_SIZE, app.GLTM));
                //                       BInterface.addElement(new BButton("Back", fontFamily16px, app.screen_width/2-40, app.screen_height-48, 80, 32, 30, 
                //                                                         function(arg:any, ID:number) {
                //                                                            BInterface.pop();
                //                                                         }));
                app.ingame_menu = INGAME_INSTRUCTIONS_MENU;
            });
            menuItems.push("Achievements");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                app.quit_request = true;
                app.quit_request_cycle = app.state_cycle;
                app.quit_request_action = QUIT_REQUEST_ACTION_ACHIEVEMENTS;
            });
            menuItems.push("Generate Debug Log");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                generateDebugLogForDownload(arg.game);
                app.achievement_debug_log = true;
                app.trigger_achievement_complete_alert();
            });
            if (this.game.allowSaveGames) {
                menuItems.push("Quit");
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    app.quit_request = true;
                    app.quit_request_cycle = app.state_cycle;
                    app.quit_request_action = QUIT_REQUEST_ACTION_QUIT;
                });
            }
            else {
                menuItems.push("Save & Quit");
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    app.game.saveGame("slot1");
                    app.quit_request = true;
                });
            }
            BInterface.push();
            //            createShrdluMenu(menuItems,menuCallbacks,  
            //                             fontFamily32px,32,this.screen_width/2-8*8*PIXEL_SIZE,this.screen_height/2-4*8*PIXEL_SIZE,16*8*PIXEL_SIZE,7*8*PIXEL_SIZE,0,1,this.GLTM);
            createShrdluMenu(menuItems, menuCallbacks, fontFamily32px, 32, this.screen_width / 2 - 8 * 8 * PIXEL_SIZE, this.screen_height / 2 - 4 * 8 * PIXEL_SIZE, 16 * 8 * PIXEL_SIZE, 9 * 8 * PIXEL_SIZE, 0, 1, this.GLTM);
            // if there are no savegames, load is disabled:
            var anySaveGame = false;
            for (var i = 0; i < 4; i++) {
                var saveName = app.game.checkSaveGame("slot" + (i + 1));
                if (saveName != null) {
                    anySaveGame = true;
                    break;
                }
            }
            if (!anySaveGame)
                BInterface.disable(3);
            //            BInterface.getElementByID(2).setEnabled(false);
            //            BInterface.getElementByID(3).setEnabled(false);
        }
        else if (menu == INGAME_SAVE_MENU) {
            var menuItems = [];
            var menuCallbacks = [];
            for (var i = 1; i <= 4; i++) {
                var saveName = app.game.checkSaveGame("slot" + i);
                if (saveName != null) {
                    menuItems.push("Slot" + i + ": " + saveName);
                }
                else {
                    menuItems.push("Slot" + i);
                }
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    app.game.saveGame("slot" + ID);
                    BInterface.pop();
                    app.ingame_menu = INGAME_MENU;
                });
            }
            menuItems.push("Back");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                BInterface.pop();
                app.ingame_menu = INGAME_MENU;
            });
            BInterface.push();
            createShrdluMenu(menuItems, menuCallbacks, fontFamily32px, 32, this.screen_width / 2 - 10 * 8 * PIXEL_SIZE, this.screen_height / 2 - 4 * 8 * PIXEL_SIZE, 20 * 8 * PIXEL_SIZE, 7 * 8 * PIXEL_SIZE, 0, 1, this.GLTM);
        }
        else if (menu == INGAME_LOAD_MENU) {
            var menuItems = [];
            var menuCallbacks = [];
            for (var i = 1; i <= 4; i++) {
                var saveName = app.game.checkSaveGame("slot" + i);
                if (saveName != null) {
                    menuItems.push("Slot" + i + ": " + saveName);
                    menuCallbacks.push(function (arg, ID) {
                        var app = arg;
                        app.quit_request = true;
                        app.quit_request_cycle = app.state_cycle;
                        app.quit_request_action = QUIT_REQUEST_ACTION_LOAD1 + (ID - 1);
                    });
                }
                else {
                    menuItems.push("Slot" + i);
                    menuCallbacks.push(null);
                }
            }
            menuItems.push("Back");
            menuCallbacks.push(function (arg, ID) {
                var app = arg;
                BInterface.pop();
                app.ingame_menu = INGAME_MENU;
            });
            BInterface.push();
            createShrdluMenu(menuItems, menuCallbacks, fontFamily32px, 32, this.screen_width / 2 - 10 * 8 * PIXEL_SIZE, this.screen_height / 2 - 4 * 8 * PIXEL_SIZE, 20 * 8 * PIXEL_SIZE, 7 * 8 * PIXEL_SIZE, 0, 1, this.GLTM);
            for (var i = 1; i <= 4; i++) {
                var saveName = app.game.checkSaveGame("slot" + i);
                if (saveName == null) {
                    BInterface.disable(i);
                }
            }
        }
    };
    ShrdluApp.prototype.game_cycle = function (k) {
        var fast_time = false;
        var fast_time_direction_command = A4_DIRECTION_NONE;
        if (this.game.introact_request == 1) {
            this.game.introact_request = 0;
            return A4ENGINE_STATE_ACT1INTRO;
        }
        if (this.game.introact_request == 2) {
            this.game.introact_request = 0;
            return A4ENGINE_STATE_ACT2INTRO;
        }
        if (this.game.introact_request == 3) {
            this.game.introact_request = 0;
            return A4ENGINE_STATE_ACT3INTRO;
        }
        if (this.game.gameover_request != 0) {
            this.gameover_type = this.game.gameover_request;
            this.game.gameover_request = 0;
            return A4ENGINE_STATE_GAMEOVER;
        }
        if (this.ingame_menu == 0) {
            if (this.game.HUD_state == SHRDLU_HUD_STATE_MESSAGES_INPUT) {
                //<SHRDLU-specific>
                // currently in text input state:
                // look for text input:
                for (var _i = 0, _a = k.keyevents; _i < _a.length; _i++) {
                    var ke = _a[_i];
                    this.game.textInputEvent(ke, this.SFXM);
                }
                if (k.key_press(KEY_CODE_RETURN)) {
                    if (!this.game.skipSpeechBubble()) {
                        this.game.textInputSubmit(this.SFXM);
                    }
                }
                if (k.key_press(KEY_CODE_ESCAPE))
                    this.game.textInputExit();
                //</SHRDLU-specific>
            }
            else if (this.game.cutSceneActivated >= 0) {
                if (k.key_press(KEY_CODE_ESCAPE))
                    this.game.skipSpeechBubble();
                if (k.key_press(KEY_CODE_SPACE))
                    this.game.skipSpeechBubble();
                if (k.key_press(KEY_CODE_RETURN))
                    this.game.skipSpeechBubble();
            }
            else {
                // playing:
                //<SHRDLU-specific>
                // write log to server periodically while game is being played
                if (this.writeLogsToServer && this.game.serverToken) {
                    var now = (new Date).getTime();
                    if (now - this.lastServerWrite > 600000) { // 10 min
                        writeLogToServer(this.game);
                        this.lastServerWrite = now;
                    }
                }
                if (k.key_press(this.key_drop_item))
                    this.game.playerInput_DropItem();
                if (k.key_press(this.key_use_item))
                    this.game.playerInput_UseItem();
                if (k.key_press(KEY_CODE_RETURN)) {
                    if (!this.game.skipSpeechBubble()) {
                        //this.game.playerInput_RequestMessageMode();
                        //this.game.textInputRequest();
                    }
                }
                // start text entering mode with any letter key:
                if (this.game.HUD_state != SHRDLU_HUD_STATE_INVENTORY &&
                    this.game.HUD_state != SHRDLU_HUD_STATE_SPLIT_INVENTORY) {
                    for (var i = KEY_CODE_A; i <= KEY_CODE_Z; i++) {
                        if (k.key_press(i)) {
                            this.game.textInputRequest();
                            for (var _b = 0, _c = k.keyevents; _b < _c.length; _b++) {
                                var ke = _c[_b];
                                this.game.textInputEvent(ke, this.SFXM);
                            }
                            break;
                        }
                    }
                    for (var i = KEY_CODE_0; i <= KEY_CODE_9; i++) {
                        if (k.key_press(i)) {
                            this.game.textInputRequest();
                            for (var _d = 0, _e = k.keyevents; _d < _e.length; _d++) {
                                var ke = _e[_d];
                                this.game.textInputEvent(ke, this.SFXM);
                            }
                            break;
                        }
                    }
                }
                //</SHRDLU-specific>
                if (k.key_press(KEY_CODE_ESCAPE)) {
                    if (!this.game.skipSpeechBubble())
                        this.createInGameMenu(INGAME_MENU, null);
                }
                // HUD:
                if (k.key_press(this.key_inventory_toggle))
                    this.game.playerInput_ToogleInventory();
                // console control:
                if (k.key_press(this.key_messageconsole_up))
                    this.game.messageConsoleUp();
                if (k.key_press(this.key_messageconsole_down))
                    this.game.messageConsoleDown();
                // issuing player commands (only if not sleeping):
                if (this.game.eyesClosedState == 2) {
                    if (k.keyboard[this.key_take]) {
                        //if (this.game.currentPlayer.isIdle()) {
                        if (k.key_press(this.key_left)) {
                            this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_PUSH, A4_DIRECTION_LEFT, this.game.currentPlayer.direction);
                            this.take_press_used_for_push = true;
                        }
                        if (k.key_press(this.key_up)) {
                            this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_PUSH, A4_DIRECTION_UP, this.game.currentPlayer.direction);
                            this.take_press_used_for_push = true;
                        }
                        if (k.key_press(this.key_right)) {
                            this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_PUSH, A4_DIRECTION_RIGHT, this.game.currentPlayer.direction);
                            this.take_press_used_for_push = true;
                        }
                        if (k.key_press(this.key_down)) {
                            this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_PUSH, A4_DIRECTION_DOWN, this.game.currentPlayer.direction);
                            this.take_press_used_for_push = true;
                        }
                        //}
                    }
                    else {
                        var direction = A4_DIRECTION_NONE;
                        if (k.keyboard[this.key_left])
                            direction = A4_DIRECTION_LEFT;
                        if (k.keyboard[this.key_up])
                            direction = A4_DIRECTION_UP;
                        if (k.keyboard[this.key_right])
                            direction = A4_DIRECTION_RIGHT;
                        if (k.keyboard[this.key_down])
                            direction = A4_DIRECTION_DOWN;
                        if (direction != A4_DIRECTION_NONE) {
                            if (k.keyboard[this.key_fast_time]) {
                                fast_time = true;
                                fast_time_direction_command = direction;
                                if (!this.achievement_interact_in_a_rush) {
                                    this.achievement_interact_in_a_rush = true;
                                    this.trigger_achievement_complete_alert();
                                }
                            }
                            this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_WALK, 0, direction);
                        }
                    }
                    //                    if (k.key_press(this.key_take)) this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_TAKE,0,A4_DIRECTION_NONE);
                    if (k.key_first_press(this.key_take)) {
                        this.take_press_used_for_push = false;
                    }
                    if (k.key_release(this.key_take) && !this.take_press_used_for_push) {
                        this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_TAKE, A4_DIRECTION_NONE, this.game.currentPlayer.direction);
                    }
                }
                if (this.game.currentPlayer.inventory[this.game.currentPlayer.selectedItem] == null) {
                    this.game.currentPlayer.selectedItem = -1;
                }
                // check if the AI debugger has to be activated or removed:
                if (k.key_press(KEY_CODE_RETURN) &&
                    k.keyboard[KEY_CODE_ALT]) {
                    if (this.SHRDLU_AI_debugger.AI == null) {
                        this.SHRDLU_AI_debugger.AI = this.game.etaoinAI;
                    }
                    else if (this.SHRDLU_AI_debugger.AI == this.game.etaoinAI) {
                        this.SHRDLU_AI_debugger.AI = this.game.qwertyAI;
                    }
                    else if (this.SHRDLU_AI_debugger.AI == this.game.qwertyAI) {
                        this.SHRDLU_AI_debugger.AI = this.game.shrdluAI;
                    }
                    else {
                        this.SHRDLU_AI_debugger.AI = null;
                    }
                }
            }
        }
        else if (this.ingame_menu == INGAME_MENU) {
            if (k.key_press(KEY_CODE_ESCAPE)) {
                BInterface.pop();
                app.ingame_menu = 0;
            }
        }
        else if (this.ingame_menu == INGAME_INSTRUCTIONS_MENU ||
            this.ingame_menu == INGAME_LOAD_MENU ||
            this.ingame_menu == INGAME_SAVE_MENU) {
            if (k.key_press(KEY_CODE_ESCAPE)) {
                BInterface.pop();
                app.ingame_menu = INGAME_MENU;
            }
        }
        else if (this.ingame_menu == INGAME_TUTORIAL) {
            if (k.key_press(KEY_CODE_ESCAPE)) {
                BInterface.pop();
                app.ingame_menu = 0;
                this.tutorialMessages.splice(0, 1);
            }
        }
        if (this.ingame_menu != INGAME_MENU && this.ingame_menu != INGAME_LOAD_MENU && this.ingame_menu != INGAME_SAVE_MENU &&
            this.ingame_menu != INGAME_INSTRUCTIONS_MENU && this.ingame_menu != INGAME_TUTORIAL) {
            var cycles_to_execute = 1;
            if (fast_time)
                cycles_to_execute = 2;
            for (var cycle = 0; cycle < cycles_to_execute; cycle++) {
                if (fast_time &&
                    cycle > 0 &&
                    fast_time_direction_command != A4_DIRECTION_NONE) {
                    this.game.playerInput_issueCommand(A4CHARACTER_COMMAND_WALK, 0, fast_time_direction_command);
                }
                if (!this.game.update(k)) {
                    if (!this.game.allowSaveGames)
                        this.game.deleteSaveGame("slot1");
                    return A4ENGINE_STATE_INTRO;
                }
                if (this.game.gameComplete) {
                    if (!this.game.allowSaveGames)
                        this.game.deleteSaveGame("slot1");
                    return A4ENGINE_STATE_GAMECOMPLETE;
                }
                if (this.tutorialMessages.length > 0) {
                    // there is a new tutorial message to display!
                    BInterface.push();
                    BInterface.addElement(new BShrdluTextFrame(this.tutorialMessages[0], false, fontFamily32px, 32, 8 * PIXEL_SIZE, 8 * PIXEL_SIZE, 240 * PIXEL_SIZE, 96 * PIXEL_SIZE, this.GLTM));
                    this.ingame_menu = INGAME_TUTORIAL;
                    break; // if there is a tutorial message, we stop executing cycles
                }
            }
        }
        if (this.quit_request && this.state_cycle > this.quit_request_cycle + SHRDLU_FADEIN_TIME) {
            this.quit_request = false;
            if (this.quit_request_action == QUIT_REQUEST_ACTION_QUIT) {
                return A4ENGINE_STATE_INTRO;
            }
            else if (this.quit_request_action == QUIT_REQUEST_ACTION_ACHIEVEMENTS) {
                return A4ENGINE_STATE_ACHIEVEMENTS_INGAME;
            }
            else {
                var ID = (this.quit_request_action - QUIT_REQUEST_ACTION_LOAD1) + 1;
                var xmlString = LZString.decompressFromUTF16(localStorage.getItem(A4SAVEGAME_STORAGE_KEY + "-slot" + ID));
                console.log("Decompresed string is: " + xmlString.length);
                var dp = new DOMParser();
                var xml = dp.parseFromString(xmlString, "text/xml");
                //                            console.log(xml);
                var gamexml = getFirstElementChildByTag(xml.documentElement, "A4Game");
                this.game = new ShrdluA4Game(gamexml, this.game_path, SHRDLU_ONTOLOGY_PATH, this.GLTM, this.SFXM, this.SFX_volume, null, this);
                this.game.finishLoadingGame(xml.documentElement);
                BInterface.reset();
                this.ingame_menu = 0;
                this.state_cycle = 0;
            }
        }
        return A4ENGINE_STATE_GAME;
    };
    ShrdluApp.prototype.game_draw = function () {
        this.game.draw(this.screen_width, this.screen_height);
        if (this.SHRDLU_AI_debugger.AI != null)
            this.SHRDLU_AI_debugger.draw();
        BInterface.draw();
        if (this.state_cycle < 16) {
            // some delay to prevent the player from seeing the door close/open animation at the beginning
            drawFadeInOverlay(1);
        }
        else if (this.state_cycle < 16 + SHRDLU_FADEIN_TIME) {
            drawFadeInOverlay(1 - ((this.state_cycle - 16) / SHRDLU_FADEIN_TIME));
        }
        if (this.quit_request) {
            drawFadeInOverlay((this.quit_request_cycle - this.state_cycle) / SHRDLU_FADEIN_TIME);
        }
    };
    ShrdluApp.prototype.gamecomplete_cycle = function (k) {
        // title:
        if (this.state_cycle == 0) {
            this.gamecomplete_logoaux = document.createElement('canvas');
            this.gamecomplete_logoaux.width = 208;
            this.gamecomplete_logoaux.height = 60;
            this.gamecomplete_logofader = new FizzleFade(208, 60);
            this.gamecomplete_state = 0;
        }
        switch (this.gamecomplete_state) {
            case 0:
                if (k.key_press(KEY_CODE_SPACE) ||
                    k.key_press(KEY_CODE_RETURN) ||
                    k.key_press(KEY_CODE_ESCAPE)) {
                    this.state_cycle = 600 + 800;
                }
                // the end appearninng takes 800 cycles, plus 600 cycles of wait time
                if (this.state_cycle > 600 + 800) {
                    this.gamecomplete_state = 1;
                }
                break;
            case 1:
                this.gamecomplete_state = 2;
                var menuItems = [];
                var menuCallbacks = [];
                menuItems.push("Thanks for playing");
                menuCallbacks.push(null);
                menuItems.push("SHRDLU!");
                menuCallbacks.push(null);
                menuItems.push("Generate Debug Log");
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    generateDebugLogForDownload(arg.game);
                    app.achievement_debug_log = true;
                    app.trigger_achievement_complete_alert();
                });
                menuItems.push("No thanks");
                menuCallbacks.push(function (arg, ID) {
                    var app = arg;
                    app.gamecomplete_state = 3;
                });
                BInterface.push();
                createShrdluMenu(menuItems, menuCallbacks, fontFamily32px, 32, this.screen_width / 2 - 10 * 8 * PIXEL_SIZE, 96 * PIXEL_SIZE, 20 * 8 * PIXEL_SIZE, 6 * 8 * PIXEL_SIZE, 0, 1, this.GLTM);
                BInterface.getElementByID(1).setEnabled(false);
                BInterface.getElementByID(2).setEnabled(false);
                break;
            case 2:
                break;
            case 3:
                return A4ENGINE_STATE_INTRO;
                break;
        }
        return A4ENGINE_STATE_GAMECOMPLETE;
    };
    ShrdluApp.prototype.gamecomplete_draw = function () {
        switch (this.gamecomplete_state) {
            case 0:
                // THE END:
                ctx.save();
                ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
                var img_logo = this.game.GLTM.get("data/theend.png");
                if (img_logo != null && this.gamecomplete_logofader != null) {
                    for (var i = 0; i < 25; i++) {
                        var xy = this.gamecomplete_logofader.nextPixelToFizzle();
                        if (xy != null) {
                            this.gamecomplete_logoaux.getContext("2d").drawImage(img_logo.src, xy[0], xy[1], 1, 1, xy[0], xy[1], 1, 1);
                        }
                    }
                }
                if (this.gamecomplete_logoaux != null)
                    ctx.drawImage(this.gamecomplete_logoaux, 24, 24);
                ctx.restore();
                break;
            default:
                ctx.save();
                ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
                var img = this.game.GLTM.get("data/theend.png");
                if (img != null)
                    img.draw(24, 24);
                ctx.restore();
                break;
        }
        BInterface.draw();
    };
    ShrdluApp.prototype.actintro_cycle = function (k, act) {
        switch (this.introact_state) {
            case 0:
                this.introact_state_timer++;
                if (this.introact_state_timer >= SHRDLU_FADEIN_TIME &&
                    (k.key_press(KEY_CODE_ESCAPE) || k.key_press(KEY_CODE_SPACE) || k.key_press(KEY_CODE_RETURN) ||
                        this.mouse_click_last_frame || this.introact_state_timer >= 600)) {
                    this.introact_state_timer = 0;
                    this.introact_state = 1;
                }
                break;
            case 1:
                this.introact_state_timer++;
                if (this.introact_state_timer > SHRDLU_FADEIN_TIME) {
                    this.game.cutSceneActivated = 0;
                    this.introact_state = 0;
                    this.introact_state_timer = 0;
                    if (act >= 2)
                        this.game.gameScript.act = "" + act;
                    return A4ENGINE_STATE_GAME;
                }
                break;
            case 2:
                // showing menu
                break;
            case 3:
                // request quit
                BInterface.reset();
                this.introact_state = 0;
                this.introact_state_timer = 0;
                return A4ENGINE_STATE_INTRO;
        }
        return this.state;
    };
    ShrdluApp.prototype.actintro_draw = function (act) {
        ctx.save();
        ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
        var img;
        if (act == 1)
            img = this.game.GLTM.get("data/act1.png");
        if (act == 2)
            img = this.game.GLTM.get("data/act2.png");
        if (act == 3)
            img = this.game.GLTM.get("data/act3.png");
        if (img != null)
            img.draw(0, 0);
        ctx.restore();
        if (this.introact_state == 0 && this.introact_state_timer < SHRDLU_FADEIN_TIME) {
            drawFadeInOverlay(1 - ((this.introact_state_timer) / SHRDLU_FADEIN_TIME));
        }
        if (this.introact_state == 1) {
            if (this.introact_state_timer < SHRDLU_FADEIN_TIME) {
                drawFadeInOverlay(((this.introact_state_timer) / SHRDLU_FADEIN_TIME));
            }
            else {
                drawFadeInOverlay(1);
            }
        }
        if (this.introact_state == 2)
            BInterface.draw();
    };
    ShrdluApp.prototype.gameover_cycle = function (k) {
        switch (this.gameover_state) {
            case 0:
                this.gameover_state_timer++;
                if (this.gameover_state_timer >= SHRDLU_FADEIN_TIME &&
                    (k.key_press(KEY_CODE_ESCAPE) || k.key_press(KEY_CODE_SPACE) || k.key_press(KEY_CODE_RETURN) ||
                        this.mouse_click_last_frame || this.gameover_state_timer >= 600)) {
                    this.gameover_state_timer = 0;
                    this.gameover_state = 1;
                    var menuItems = [];
                    var menuCallbacks = [];
                    menuItems.push("Generate Debug Log");
                    menuCallbacks.push(function (arg, ID) {
                        var app = arg;
                        generateDebugLogForDownload(arg.game);
                        app.achievement_debug_log = true;
                        app.trigger_achievement_complete_alert();
                    });
                    menuItems.push("Title Screen");
                    menuCallbacks.push(function (arg, ID) {
                        var app = arg;
                        app.gameover_state = 2;
                    });
                    BInterface.push();
                    createShrdluMenu(menuItems, menuCallbacks, fontFamily32px, 32, this.screen_width / 2 - 10 * 8 * PIXEL_SIZE, this.screen_height / 2 - 4 * 8 * PIXEL_SIZE, 20 * 8 * PIXEL_SIZE, 4 * 8 * PIXEL_SIZE, 0, 1, this.GLTM);
                }
                break;
            case 1:
                // showing menu
                break;
            case 2:
                // request quit
                BInterface.reset();
                this.gameover_state = 0;
                this.gameover_state_timer = 0;
                return A4ENGINE_STATE_INTRO;
        }
        return this.state;
    };
    ShrdluApp.prototype.gameover_draw = function () {
        ctx.save();
        ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
        var img;
        if (this.gameover_type == 1)
            img = this.game.GLTM.get("data/cutscene-death-oxygen.png");
        if (img != null)
            img.draw(0, 0);
        ctx.restore();
        if (this.gameover_state == 0 && this.gameover_state_timer < SHRDLU_FADEIN_TIME) {
            drawFadeInOverlay(1 - ((this.gameover_state_timer) / SHRDLU_FADEIN_TIME));
        }
        if (this.gameover_state == 1)
            BInterface.draw();
    };
    ShrdluApp.prototype.achievements_cycle = function (k, inGame) {
        switch (this.achievements_state) {
            case 0:
                if (this.achievements_state_timer == 0) {
                    BInterface.pushIgnoringCurrent();
                    BInterface.addElement(new BShrdluButton("Back", fontFamily32px, this.screen_width / 2 - 80, this.screen_height - 56, 160, 64, 100, "white", true, function (arg, ID) {
                        var app = arg;
                        app.achievements_state = 2;
                    }));
                    for (var i = 0; i < this.achievement_names.length; i++) {
                        BInterface.addElement(new BShrdluButton(this.achievement_names[i], fontFamily32px, 8 * PIXEL_SIZE, 8 + 8 * i * PIXEL_SIZE, 240 * PIXEL_SIZE, 8 * PIXEL_SIZE, 101 + i, "white", false, function (arg, ID) {
                            var app = arg;
                            BInterface.push();
                            BInterface.addElement(new BShrdluTextFrame(app.achievement_descriptions[ID - 101].concat([""]).concat(app.achievement_progress_string(ID - 101)), false, fontFamily32px, 32, 8 * PIXEL_SIZE, 8 * PIXEL_SIZE, 240 * PIXEL_SIZE, 128 * PIXEL_SIZE, app.GLTM));
                            BInterface.addElement(new BShrdluButton("Back", fontFamily32px, app.screen_width / 2 - 80, app.screen_height - 300, 160, 64, 200, "white", true, function (arg, ID) {
                                BInterface.pop();
                            }));
                        }));
                    }
                }
                this.achievements_state_timer++;
                if (this.achievements_state_timer >= SHRDLU_FADEIN_TIME ||
                    k.key_press(KEY_CODE_ESCAPE) || k.key_press(KEY_CODE_SPACE) || k.key_press(KEY_CODE_RETURN) ||
                    this.mouse_click_last_frame) {
                    this.achievements_state_timer = 0;
                    this.achievements_state = 1;
                }
                break;
            case 1:
                // showing achievements
                break;
            case 2:
                // request exit
                BInterface.pop();
                this.achievements_state = 0;
                this.achievements_state_timer = 0;
                if (inGame) {
                    return A4ENGINE_STATE_GAME;
                }
                else {
                    return A4ENGINE_STATE_TITLESCREEN;
                }
        }
        return this.state;
    };
    ShrdluApp.prototype.achievements_draw = function () {
        ctx.save();
        ctx.scale(PIXEL_SIZE, PIXEL_SIZE);
        for (var i = 0; i < this.achievement_names.length; i++) {
            if (this.achievement_complete(i)) {
                fillTextTopLeft("COMPLETE!", 196, 2 + i * 8, fontFamily8px, MSX_COLOR_LIGHT_GREEN);
            }
            else {
                fillTextTopLeft(" Pending", 196, 2 + i * 8, fontFamily8px, MSX_COLOR_GREY);
            }
        }
        ctx.restore();
        BInterface.draw();
        if (this.achievements_state == 0 && this.achievements_state_timer < SHRDLU_FADEIN_TIME) {
            drawFadeInOverlay(1 - ((this.achievements_state_timer) / SHRDLU_FADEIN_TIME));
        }
    };
    ShrdluApp.prototype.achievement_complete = function (idx) {
        switch (this.achievement_names[idx]) {
            case "Complete Tutorial":
                return this.achievement_complete_tutorial;
                break;
            case "Complete Act 1":
                return this.achievement_complete_act1;
                break;
            case "Complete Act 2":
                return this.achievement_complete_act2;
                break;
            case "Complete Act 3":
                return this.achievement_complete_act3;
                break;
            case "See both endings":
                return allArrayElementsTrue(this.achievement_complete_see_all_endings);
                break;
            case "Both genders":
                return allArrayElementsTrue(this.achievement_both_genders);
                break;
            case "Hungry and thirsty":
                return allArrayElementsTrue(this.achievement_interact_eat_drink);
                break;
            case "Maker":
                return allArrayElementsTrue(this.achievement_interact_3d_printed_one_of_each_kind);
                break;
            case "Low on oxygen":
                return this.achievement_interact_low_oxygen;
                break;
            case "In a rush":
                return this.achievement_interact_in_a_rush;
                break;
            case "Natural language":
                return this.achievement_nlp_parse_error;
                break;
            case "Asky":
                return allArrayElementsTrue(this.achievement_nlp_all_types_of_questions);
                break;
            case "Etaoin at your service":
                return allArrayElementsTrue(this.achievement_nlp_all_etaoin_actions);
                break;
            case "Robots at your service":
                return allArrayElementsTrue(this.achievement_nlp_all_robot_actions);
                break;
            case "Justification":
                return this.achievement_nlp_resolution_explanation;
                break;
            case "Three is not enough":
                return this.achievement_nlp_asked_for_more;
            case "Posters":
                return allArrayElementsTrue(this.achievement_secret_posters);
                break;
            case "Diaries":
                return allArrayElementsTrue(this.achievement_secret_diaries);
                break;
            case "Asimov":
                return this.achievement_secret_3_laws_of_robotics;
                break;
            case "Vintage computer":
                return this.achievement_secret_msx;
                break;
            case "The secret of Aurora":
                return this.achievement_secret_life_in_aurora;
                break;
            case "Debugging":
                return this.achievement_debug_log;
                break;
        }
        return false;
    };
    ShrdluApp.prototype.achievement_progress_string = function (idx) {
        var lines = [];
        switch (this.achievement_names[idx]) {
            case "See Both Endings":
                lines = ["Progress: " + nTruesInArray(this.achievement_complete_see_all_endings) + "/" + this.achievement_complete_see_all_endings.length];
                break;
            case "Hungry and Thirsty":
                {
                    var first = [true, true];
                    var tags = ["eating", "drinking"];
                    lines = ["Done: ", "Missing: "];
                    for (var i = 0; i < first.length; i++) {
                        if (this.achievement_interact_eat_drink[i]) {
                            if (!first[0])
                                lines[0] += ", ";
                            lines[0] += tags[i];
                            first[0] = false;
                        }
                        else {
                            if (!first[1])
                                lines[1] += ", ";
                            lines[1] += tags[i];
                            first[1] = false;
                        }
                    }
                }
                break;
            case "Maker":
                {
                    var n = 0;
                    for (var _i = 0, _a = this.achievement_interact_3d_printed_one_of_each_kind; _i < _a.length; _i++) {
                        var made = _a[_i];
                        if (made)
                            n++;
                    }
                    lines = ["Progress: " + n + "/" + this.achievement_interact_3d_printed_one_of_each_kind.length];
                }
                break;
            case "Asky":
                {
                    var first = [true, true, true, true, true, true, true, true, true, true];
                    var tags = ["predicate", "where", "who is", "what is", "query", "how many", "when", "why", "how", "define"];
                    lines = ["Done: ", "Missing: "];
                    for (var i = 0; i < first.length; i++) {
                        if (this.achievement_nlp_all_types_of_questions[i]) {
                            if (!first[0])
                                lines[0] += ", ";
                            lines[0] += tags[i];
                            first[0] = false;
                        }
                        else {
                            if (!first[1])
                                lines[1] += ", ";
                            lines[1] += tags[i];
                            first[1] = false;
                        }
                    }
                }
                break;
            case "Etaoin at your service":
                {
                    var first = [true, true, true, true, true, true, true];
                    var tags = ["connect to", "open", "close", "switch on", "switch off", "3d print", "reboot"];
                    lines = ["Done: ", "Missing: "];
                    for (var i = 0; i < first.length; i++) {
                        if (this.achievement_nlp_all_etaoin_actions[i]) {
                            if (!first[0])
                                lines[0] += ", ";
                            lines[0] += tags[i];
                            first[0] = false;
                        }
                        else {
                            if (!first[1])
                                lines[1] += ", ";
                            lines[1] += tags[i];
                            first[1] = false;
                        }
                    }
                }
                break;
            case "Robots at your service":
                {
                    var first = [true, true, true, true, true, true, true, true, true, true, true, true, true, true, true];
                    var tags = ["follow", "move", "guide", "stop", "put in", "drop", "give", "open", "close", "turn", "push", "repair", "take", "stay", "reboot"];
                    lines = ["Done: ", "Missing: "];
                    for (var i = 0; i < first.length; i++) {
                        if (this.achievement_nlp_all_robot_actions[i]) {
                            if (!first[0])
                                lines[0] += ", ";
                            lines[0] += tags[i];
                            first[0] = false;
                        }
                        else {
                            if (!first[1])
                                lines[1] += ", ";
                            lines[1] += tags[i];
                            first[1] = false;
                        }
                    }
                }
                break;
            case "Posters":
                lines = ["Progress: " + nTruesInArray(this.achievement_secret_posters) + "/" + this.achievement_secret_posters.length];
                break;
            case "Diaries":
                lines = ["Progress: " + nTruesInArray(this.achievement_secret_diaries) + "/" + this.achievement_secret_diaries.length];
                break;
            case "Both genders":
                {
                    var first = [true, true];
                    var tags = ["female", "male"];
                    lines = ["Done: ", "Missing: "];
                    for (var i = 0; i < first.length; i++) {
                        if (this.achievement_both_genders[i]) {
                            if (!first[0])
                                lines[0] += ", ";
                            lines[0] += tags[i];
                            first[0] = false;
                        }
                        else {
                            if (!first[1])
                                lines[1] += ", ";
                            lines[1] += tags[i];
                            first[1] = false;
                        }
                    }
                }
                break;
            default:
        }
        // split lines that are too long:
        var lines2 = [];
        for (var _b = 0, lines_2 = lines; _b < lines_2.length; _b++) {
            var line = lines_2[_b];
            if (line.length > 37) {
                for (var _c = 0, _d = splitStringBySpaces(line, 37); _c < _d.length; _c++) {
                    var line2 = _d[_c];
                    lines2.push(line2);
                }
            }
            else {
                lines2.push(line);
            }
        }
        return lines2;
    };
    ShrdluApp.prototype.trigger_achievement_complete_alert = function () {
        for (var i = 0; i < this.achievement_completed_alerts_shown.length; i++) {
            if (!this.achievement_completed_alerts_shown[i] &&
                this.achievement_complete(i)) {
                this.achievement_completed_alerts_shown[i] = true;
                this.achievement_alert = i;
                this.achievement_alert_timer = 0;
            }
        }
        this.saveAchievements();
    };
    ShrdluApp.prototype.loadAchievements = function () {
        var ach_xml = null;
        var achString = localStorage.getItem(ACHIEVEMENTS_STORAGE_KEY);
        if (achString != null) {
            console.log("Found config stored in the browser, loading it...");
            // if we can find a configuration saved in the browser, load it:
            var oParser = new DOMParser();
            ach_xml = oParser.parseFromString(achString, "text/xml").documentElement;
        }
        if (ach_xml != null) {
            var slot_xml = getFirstElementChildByTag(ach_xml, "achievement_completed_alerts_shown");
            if (slot_xml != null)
                this.achievement_completed_alerts_shown = eval(slot_xml.getAttribute("value"));
            while (this.achievement_completed_alerts_shown.length < this.achievement_names.length) {
                this.achievement_completed_alerts_shown.push(false);
            }
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_complete_tutorial");
            if (slot_xml != null)
                this.achievement_complete_tutorial = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_complete_act1");
            if (slot_xml != null)
                this.achievement_complete_act1 = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_complete_act2");
            if (slot_xml != null)
                this.achievement_complete_act2 = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_complete_act3");
            if (slot_xml != null)
                this.achievement_complete_act3 = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_complete_see_all_endings");
            if (slot_xml != null)
                this.achievement_complete_see_all_endings = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_both_genders");
            if (slot_xml != null)
                this.achievement_both_genders = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_interact_eat_drink");
            if (slot_xml != null)
                this.achievement_interact_eat_drink = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_interact_3d_printed_one_of_each_kind");
            if (slot_xml != null)
                this.achievement_interact_3d_printed_one_of_each_kind = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_interact_low_oxygen");
            if (slot_xml != null)
                this.achievement_interact_low_oxygen = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_interact_in_a_rush");
            if (slot_xml != null)
                this.achievement_interact_in_a_rush = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_nlp_parse_error");
            if (slot_xml != null)
                this.achievement_nlp_parse_error = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_nlp_all_types_of_questions");
            if (slot_xml != null)
                this.achievement_nlp_all_types_of_questions = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_nlp_all_etaoin_actions");
            if (slot_xml != null)
                this.achievement_nlp_all_etaoin_actions = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_nlp_all_robot_actions");
            if (slot_xml != null)
                this.achievement_nlp_all_robot_actions = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_nlp_resolution_explanation");
            if (slot_xml != null)
                this.achievement_nlp_resolution_explanation = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_nlp_asked_for_more");
            if (slot_xml != null)
                this.achievement_nlp_asked_for_more = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_secret_posters");
            if (slot_xml != null)
                this.achievement_secret_posters = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_secret_diaries");
            if (slot_xml != null)
                this.achievement_secret_diaries = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_secret_life_in_aurora");
            if (slot_xml != null)
                this.achievement_secret_life_in_aurora = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_secret_msx");
            if (slot_xml != null)
                this.achievement_secret_msx = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_secret_3_laws_of_robotics");
            if (slot_xml != null)
                this.achievement_secret_3_laws_of_robotics = eval(slot_xml.getAttribute("value"));
            slot_xml = getFirstElementChildByTag(ach_xml, "achievement_debug_log");
            if (slot_xml != null)
                this.achievement_debug_log = eval(slot_xml.getAttribute("value"));
        }
        else {
            console.log("No achievement data found!");
        }
    };
    ShrdluApp.prototype.saveAchievements = function () {
        var achString = "<achievements>";
        achString += "<achievement_completed_alerts_shown value=\"[" + this.achievement_completed_alerts_shown.toString() + "]\"/>";
        achString += "<achievement_complete_tutorial value=\"" + this.achievement_complete_tutorial + "\"/>";
        achString += "<achievement_complete_act1 value=\"" + this.achievement_complete_act1 + "\"/>";
        achString += "<achievement_complete_act2 value=\"" + this.achievement_complete_act2 + "\"/>";
        achString += "<achievement_complete_act3 value=\"" + this.achievement_complete_act3 + "\"/>";
        achString += "<achievement_complete_see_all_endings value=\"[" + this.achievement_complete_see_all_endings.toString() + "]\"/>";
        achString += "<achievement_both_genders value=\"[" + this.achievement_both_genders.toString() + "]\"/>";
        achString += "<achievement_interact_eat_drink value=\"[" + this.achievement_interact_eat_drink.toString() + "]\"/>";
        achString += "<achievement_interact_3d_printed_one_of_each_kind value=\"[" + this.achievement_interact_3d_printed_one_of_each_kind.toString() + "]\"/>";
        achString += "<achievement_interact_low_oxygen value=\"" + this.achievement_interact_low_oxygen + "\"/>";
        achString += "<achievement_interact_in_a_rush value=\"" + this.achievement_interact_in_a_rush + "\"/>";
        achString += "<achievement_nlp_parse_error value=\"" + this.achievement_nlp_parse_error + "\"/>";
        achString += "<achievement_nlp_all_types_of_questions value=\"[" + this.achievement_nlp_all_types_of_questions.toString() + "]\"/>";
        achString += "<achievement_nlp_all_etaoin_actions value=\"[" + this.achievement_nlp_all_etaoin_actions.toString() + "]\"/>";
        achString += "<achievement_nlp_all_robot_actions value=\"[" + this.achievement_nlp_all_robot_actions.toString() + "]\"/>";
        achString += "<achievement_nlp_resolution_explanation value=\"" + this.achievement_nlp_resolution_explanation + "\"/>";
        achString += "<achievement_nlp_asked_for_more value=\"" + this.achievement_nlp_asked_for_more + "\"/>";
        achString += "<achievement_secret_posters value=\"[" + this.achievement_secret_posters.toString() + "]\"/>";
        achString += "<achievement_secret_diaries value=\"[" + this.achievement_secret_diaries.toString() + "]\"/>";
        achString += "<achievement_secret_life_in_aurora value=\"" + this.achievement_secret_life_in_aurora + "\"/>";
        achString += "<achievement_secret_msx value=\"" + this.achievement_secret_msx + "\"/>";
        achString += "<achievement_secret_3_laws_of_robotics value=\"" + this.achievement_secret_3_laws_of_robotics + "\"/>";
        achString += "<achievement_debug_log value=\"" + this.achievement_debug_log + "\"/>";
        achString += "</achievements>";
        localStorage.setItem(ACHIEVEMENTS_STORAGE_KEY, achString);
        console.log("Achievements saved to the browser ... (key: " + ACHIEVEMENTS_STORAGE_KEY + ")");
        // console.log("string:\n" + achString);
    };
    return ShrdluApp;
}());
