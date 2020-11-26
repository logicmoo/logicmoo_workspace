var HMUD_UI = {
    output: null,
    scrollLock: false,
    echo: true,
    focus: true,

    /* current line in output window
       for counting and limiting lines in the output window) */
    lineCount: 1,

    icons: null,
    numIconsVisible: 3,
    iconWidth: 32,
    iConnOn: null,
    iConnOff: null,
    connState: null,

    init: function () {
        var that = this;

        /* Set font (based on Cookie, Browser or OS) */
        var f;
        if (f = readCookie("font"))
            this.selectFont(f, true);
        else if (BrowserDetect.OS == "Linux")
            this.selectFont("font-monospace", false);
        else if (BrowserDetect.browser == "Chrome")
            /* Chrome does not support Fixedsys natively */
            this.selectFont("font-courier-b", false);
        else
            this.selectFont("font-fixed", false);

        this.output = document.getElementById("output");
        this.output.onscroll = function() { that.outputOnScroll(this); };

        this.icons = [["i-save", "Treasure-icon.gif", function(){HMUD_Menu.saveLog();}],
                      ["i-clear", "Rune-icon.gif", function(){HMUD_Menu.options();}],
                      ["i-conn-on", "Button-icon.gif", function(){HMUD_Menu.connection();}],
                      ["i-conn-off", "Button-icon-off.gif", function(){HMUD_Menu.connection();}]];
        this.initCmdLine();

        document.body.onclick = function(e) { that.bodyOnClick(e); };
        document.title = m.windowTitle;

        /* Default icons */
        var ib = document.getElementById("iconsBar");
        var getIconFn = function(i) {
            return icon.onclick = function(e) {
                if (!e) var e = window.event;
                that.icons[i][2](this);
                e.cancelBubble = true;
                if (e.stopPropagation)
                    e.stopPropagation();
            };
        };
        for (var i = 0; i < this.icons.length; ++i) {
            var icon = document.createElement("img");
            icon.id = this.icons[i][0];
            icon.src = this.icons[i][1];
            icon.onclick = getIconFn(i);
            if (icon.id == "i-conn-on")
                icon.style.display = "none";
            ib.appendChild(icon);
        }

        document.getElementById("logSave").style.display = "none";
        document.getElementById("optionsMenu").style.display = "none";
        document.getElementById("connectionMenu").style.display = "none";

        this.iConnOn  = document.getElementById("i-conn-on");
        this.iConnOff = document.getElementById("i-conn-off");

        /*
         * Display guide (cookie, or display by default)
         */
        var show_guide;
        if (show_guide = readCookie("showGuide")) {
            if (show_guide == "yes")
                HMUD_Menu.showGuide(true);
            else
                HMUD_Menu.hideGuide(true);
        } else {
            HMUD_Menu.showGuide(false);
        }

        /*
         * When the window loses or gains focus.
         */
        if (BrowserDetect.browser == "Explorer") {
            document.onfocusin =  function() { that.stopTitleAlert(); that.setFocus(true); };
            document.onfocusout = function() { that.setFocus(false); };
        } else {
            window.onfocus = function() { that.stopTitleAlert(); that.setFocus(true); };
            window.onblur = function() { that.setFocus(false); };
        }

        window.onresize =  function() { that.rearrange(); };
        this.rearrange();

        this.screenWrite(m.loadingClient);
        /* try to connect when all the interface is ready */
        HMUD_Client.tryToConnect();
    },

    initCmdLine: function () {
        var that = this;
        this.cmdline = document.getElementById("cmdline");
        this.cmdline.onfocus = function() {
            that.setFocus(true);
            /* place the cursor at the end of the text */
            this.value = this.value;
        };
        this.cmdline.onkeydown = function(e) {
            e = e || window.event;
        
            if (e.keyCode == 38) { // 38 = Up
                this.value = HMUD_History.previous();
            }
            else if (e.keyCode == 40) { // 40 = Down
                this.value = HMUD_History.next();
            }
        };
        this.cmdline.onkeyup = function(e) {
            /* place the cursor at the end of the text */
            e = e || window.event;

            if (e.keyCode == 38) { // 38 = Up
                this.value += "";
            }
            else if (e.keyCode == 40) { // 40 = Down
                this.value += "";
            }
        };
        this.cmdline.focus();

        document.getElementById("cmdForm").onsubmit = function() {
            that.command(that.cmdline.value);
            that.cmdline.form.reset();
            that.cmdline.value="";
            return false;
        };
    },

    /* send a command to MUD */
    command: function (cmd) {
        if (this.echo) {
            this.screenWrite(cmd.replace(/ /g, BrowserDetect.browser == "Explorer" ? "\xA0" : " ") + "<br>");
            HMUD_History.add(cmd);
        }
        HMUD_Client.command(cmd);
        if (c.forceScrollOnCmd)
            this.scrollBottom();
    },

    scrollBottom: function () {
        this.output.scrollTop = Math.max(this.output.clientHeight, this.output.scrollHeight);
    },

    outputOnScroll: function (e) {
        if (e.scrollHeight - e.scrollTop > e.clientHeight) {
            this.scrollLock = true;
            if (e.className != "scrollLock")
                e.className = "scrollLock";
        } else {
            this.scrollLock = false;
            if (e.className != "")
                e.className = "";
        }
    },

    /* add <str> to output window (HTML format) */
    screenWrite: function (str) {
        var lastIndex = -1;
        var i;

        while ((i = str.indexOf("<br>", lastIndex + 1)) != -1) {
            this.updateLineCount(this.lineCount + 1);
            lastIndex = i + 4; /* 4 = <BR> length */
        }

        var dummy = document.createElement("span");
        dummy.innerHTML = str;
        var l = dummy.childNodes.length;

        for (i = 0; i < l; ++i)
            this.output.appendChild(dummy.childNodes[0]);

        if (!this.focus)
            this.startTitleAlert();
        if (!this.scrollLock)
            this.scrollBottom();
    },

    clearScreen: function(l) {
        if (l == "all") {
            var cl = this.output.childNodes.length;
            for (var i = 0; i < cl; i++)
                /* childNodes is "live", so always index 0 for the next element */
                this.output.removeChild(this.output.childNodes[0]);
            this.updateLineCount(1);
            return;
        }

        var cLines;
        if (l == "half")
            cLines = (this.lineCount / 2) | 0; // (int) cast
        else if (this.lineCount >= l)
            cLines = this.lineCount - l;
        else
            return;

        cLines = this.removeLines(this.output, cLines);
        this.updateLineCount(this.lineCount - cLines);
    },

    removeLines: function(root, num) {
        var linesRemoved = 0;
        var cl = root.childNodes.length;

        for (var i = 0; i < cl; ++i) {
            if (linesRemoved == num)
                return linesRemoved;

            var n = root.childNodes[0];

            if (n.nodeType == 1) { /* it's an Element */
                if (n.nodeName.toLowerCase() == "br") { /* line break found */
                    root.removeChild(n);
                    ++linesRemoved;
                } else {
                    linesRemoved += this.removeLines(n, num - linesRemoved);

                    if (n.childNodes.length == 0) {
                        root.removeChild(n); /* the element is empty, get rid of it */
                    }
                }
            } else {
                root.removeChild(n);
            }
        }

        return linesRemoved;
    },

    updateLineCount: function(l) { 
        this.lineCount = l | 0; // yes, this casts to an integer...
        var nl = document.getElementById("numLinesInScreen");
        if (nl)
            nl.innerHTML = this.lineCount;
        return this.lineCount;
    },

    /* Rearrange UI elements.
       Elements that are dynamically positioned should be treated here */
    rearrange: function() {
        var d = getViewportDimensions();
        var p;

        /* commandline */
        var c = this.cmdline;
        c.style.width = (d.width - 20 - (this.numIconsVisible * this.iconWidth)) + "px";

        /* icons bar */
        var ib = document.getElementById("iconsBar");
        p = findPos(c);
        ib.style.top = (p[1]) + "px";

        /* output window */
        var o = this.output;
        o.style.height = (d.height - (c.offsetHeight + 1) - 40) + "px";
        o.style.width = (d.width - 40) + "px";

        /* optionsMenu */
        var ic = document.getElementById("i-clear");
        var cs = document.getElementById("optionsMenu");
        p = findPos(ic);
        cs.style.left = (p[0] - cs.offsetWidth + ic.offsetWidth) + "px";
        cs.style.top = (p[1] + 1 - cs.offsetHeight) + "px";

        /* connectionMenu */
        var icn = this.getActiveConnectionIcon();
        var cm = document.getElementById("connectionMenu");
        p = findPos(icn);
        cm.style.left = (p[0] - cm.offsetWidth + icn.offsetWidth) + "px";
        cm.style.top = (p[1] + 1 - cm.offsetHeight) + "px";

        /* fixing fieldset elements inside menus */
        var fields = document.getElementsByTagName("fieldset");
        for (var i = 0; i < fields.length; ++i) {
            if (fields[i].lastChild.className != "fsSpacer") {
                var sp = document.createElement("div");
                sp.className = "fsSpacer";
                sp.style.height = "0px";
                sp.style.width = (fields[i].parentNode.offsetWidth - 45) + "px";
                fields[i].appendChild(sp);
            }
        }

        /* command guide */
        var guide = document.getElementById("cmdGuide");
        if (guide.style.display != "none") {
            p = findPos(c);
            guide.style.top = (p[1] - guide.offsetHeight - 20) + "px";
        }
    },

    selectFont: function(name, save) {
        document.body.className = name;
        if (save)
            createCookie("font", name, 30);
    },

    bodyOnClick: function(e) {
        var rightclick;
        if (!e) e = window.event;
    
        if (e.which) rightclick = (e.which == 3);
        else if (e.button) rightclick = (e.button == 2);
    
        /* If there's a selection, let's not change the focus */
        /* http://www.quirksmode.org/dom/range_intro.html */
        var userSelection;
        if (window.getSelection)
            userSelection = window.getSelection();
        else if (document.selection) // should come last; Opera!
            userSelection = document.selection.createRange();
    
        var selectedText = userSelection;
        if (userSelection.text)
            selectedText = userSelection.text;
        else if (selectedText.toString)
            selectedText = selectedText.toString();
        else
            selectedText = "";
    
        /* If it's not a right click and there's no text selected,
           let's automatically focus the command line */
        if (!rightclick && (!selectedText || selectedText.length < 1)) {
            if (!window.disableCmdFocus)
                this.cmdline.focus();
    
            var o;
            o = document.getElementById("optionsMenu");
            if (o.style.display != "none")
                o.style.display = "none";
            o = document.getElementById("connectionMenu");
            if (o.style.display != "none")
                o.style.display = "none";
            o = document.getElementById("logSave");
            if (o.style.display != "none") {
                o.style.display = "none";
                window.disableCmdFocus = false;
            }
        }
    },

    /*
     * Title alert
     */
    stopTitleAlert: function() {
        if (window.titleIId) {
            clearInterval(window.titleIId);
            window.titleIId = null;
            document.title = m.windowTitle;
        }
    },

    startTitleAlert: function() {
        document.title = document.title == m.windowTitleAlert ? m.windowTitle : m.windowTitleAlert;

        if (window.titleIId == null) {
            var that = this;
            window.titleIId = setInterval(function(){that.startTitleAlert();}, 1000);

            document.body.onmousemove = function() {
                that.stopTitleAlert();
                document.body.onmousemove = function(){};
            };
        }
    },

    /*
     * UI State
     */

    setEcho: function (b) {
        b = b ? true : false;

        if (this.echo == b)
            return;

        this.echo = b;

        // Replacing input text by password or password by text
        var newc = document.getElementById("cmdline-swap");
        newc.setAttribute("id", "cmdline");
        newc.style.display = "inline";
        this.cmdline.setAttribute("id", "cmdline-swap");
        this.cmdline.style.display = "none";
        this.cmdline = newc;
        this.initCmdLine();

        this.rearrange(); // it will resize the cmdline properly
    },

    setFocus: function (b) {
        this.focus = b ? true : false;
    },

    setConnectionState: function(state) {
        if (state == "connecting") {
            this.connState = "connecting";
            this.setIconConnecting();
            this.screenWrite(m.connecting);
        } else if (state == "connected") {
            this.connState = "connected";
            this.setIconConnected();
            this.screenWrite(m.connected);
        } else if (state == "disconnected") {
            this.connState = "disconnected";
            this.setIconDisconnected();
            this.screenWrite(m.disconnected);
        } else {
            alert("Invalid connection state!");
            return;
        }

        this.rearrange();
    },

    setIconConnecting: function () {
        clearInterval(this.connectingIId);
        var that = this;
        this.connectingIId = setInterval(function(){that.swapIconConnected}, 300);
    },

    /* helper for setIconConnecting */
    swapIconConnected: function () {
        if (this.iConnOn.style.display == "none") {
            this.iConnOn.style.display = "inline";
            this.iConnOff.style.display = "none";
        } else {
            this.iConnOn.style.display = "none";
            this.iConnOff.style.display = "inline";
        }
    },

    setIconConnected: function () {
        clearInterval(this.connectingIId);
        this.iConnOn.style.display = "inline";
        this.iConnOff.style.display = "none";
    },

    setIconDisconnected: function () {
        clearInterval(this.connectingIId);
        this.iConnOn.style.display = "none";
        this.iConnOff.style.display = "inline";
    },

    getActiveConnectionIcon: function() {
        if (this.iConnOn.style.display == "none")
            return this.iConnOff;
        else
            return this.iConnOn;
    },

    /* Messages from HMUD_Client */
    handleMessage: function(msg, info) {
        switch (msg) {
        case "loaded":
            this.screenWrite(m.clientLoaded);
            break;
        case "connecting":
            this.setConnectionState("connecting");
            break;
        case "connected":
            this.setConnectionState("connected");
            break;
        case "disconnected":
            this.setConnectionState("disconnected");
            break;
        case "ioError":
            this.screenWrite(m.ioError); /* no change in state, I guess */
            break;
        case "securityError":
            this.screenWrite(m.securityError);
            this.setConnectionState("disconnected");
            break;
        case "receive":
            this.screenWrite(info);
            break;
        case "echoOn":
            this.setEcho(true);
            break;
        case "echoOff":
            this.setEcho(false);
            break;
        case "usingIE":
            return BrowserDetect.browser == "Explorer";
        default:
            return false;
        }

        return true;
    }
};

/*
 * History navigation
 */
var HMUD_History = {
    commands: [],
    pointer: -1, // -1 means the user is out of the history navigation

    /* previous = true, next = false */
    get: function(previous) {
        if (this.commands.length < 1)
            return "";

        if (previous) {
            if (this.pointer == -1)
                return this.commands[this.pointer = this.commands.length - 1];
            else if (this.pointer == 0)
                return this.commands[0];
            else
                return this.commands[--this.pointer];
        } else {
            if (this.pointer == -1)
                return "";
            else if (this.pointer == this.commands.length - 1) { // out of range
                this.pointer = -1;
                return "";
            } else
                return this.commands[++this.pointer];
        }
    },

    previous: function() { return this.get(true); },
    next: function() { return this.get(false); },

    add: function(cmd) {
        // adding to history always clears the pointer
        this.pointer = -1;

        if (cmd.length < c.historyMinLength)
            return;

        /* if the command is the same as the previous one, let's not repeat it in the history */
        if (cmd == this.commands[this.commands.length - 1]) {
            return;
        }

        if (this.commands.push(cmd) > c.maxHistorySize)
            this.commands.shift();
    }
};

var HMUD_Menu = {
    connection: function () {
        var d = document;
        var cm = d.getElementById("connectionMenu");
    
        if (cm.style.display == "none") {
            cm.innerHTML = "";
    
            var ul = d.createElement("ul");
            var li = d.createElement("li");
    
            if (HMUD_UI.connState == "disconnected") {
                li.appendChild(d.createTextNode(m.connect));
                li.onclick = function(){HMUD_Client.connect();};
                ul.appendChild(li);
            } else {
                li.appendChild(d.createTextNode(m.reconnect));
                li.onclick = function(){HMUD_Client.disconnect();HMUD_Client.connect();};
                ul.appendChild(li);
    
                var li2 = d.createElement("li");
                li2.appendChild(d.createTextNode(m.disconnect));
                li2.onclick = function(){HMUD_Client.disconnect();};
                ul.appendChild(li2);
            }
    
            cm.appendChild(ul);
            cm.style.display = "block";
            HMUD_UI.rearrange();
        } else {
            cm.style.display = "none";
        }
    },

    options: function () {
        var om = document.getElementById("optionsMenu");
    
        if (om.style.display == "none") {
            this.updateOptionsMenu();
            om.style.display = "block";
            HMUD_UI.rearrange();
        } else {
            om.style.display = "none";
        }
    },

    /* will create menu if it still does not exist */
    updateOptionsMenu: function () {
        var d = document;
        var om = d.getElementById("optionsMenu");
    
        if (om.alreadyCreated) {
            return;
        }
    
        /* Command guide */
        if (HMUD_Guide.length > 0) {
            var g_fs = d.createElement("fieldset");
            var g_lg = d.createElement("legend");
            g_lg.appendChild(d.createTextNode("?"));
            g_fs.appendChild(g_lg);
            var g_ul = d.createElement("ul");
            g_fs.appendChild(g_ul);

            var g_li1 = d.createElement("li");
            g_li1.onclick = function(){HMUD_Menu.showGuide(true);};
            g_li1.appendChild(d.createTextNode(m.cmdGuide));
            g_ul.appendChild(g_li1);
            om.appendChild(g_fs);
        }
    
        /* font fieldset */
        var f_fs = d.createElement("fieldset");
        var f_lg = d.createElement("legend");
        f_lg.appendChild(d.createTextNode(m.selectFont));
        f_fs.appendChild(f_lg);
    
        var f_ul = d.createElement("ul");
    
        var f_li1 = d.createElement("li");
        f_li1.onclick = function(){HMUD_UI.selectFont('font-fixed', true);};
        f_li1.appendChild(d.createTextNode("Fixedsys"));
        f_ul.appendChild(f_li1);
        var f_li2 = d.createElement("li");
        f_li2.onclick = function(){HMUD_UI.selectFont('font-courier', true);};
        f_li2.appendChild(d.createTextNode("Courier New"));
        f_ul.appendChild(f_li2);
        var f_li3 = d.createElement("li");
        f_li3.onclick = function(){HMUD_UI.selectFont('font-courier-b', true);};
        f_li3.appendChild(d.createTextNode("Courier New Bold"));
        f_ul.appendChild(f_li3);
        var f_li4 = d.createElement("li");
        f_li4.onclick = function(){HMUD_UI.selectFont('font-monospace', true);};
        f_li4.appendChild(d.createTextNode("Monospace"));
        f_ul.appendChild(f_li4);
        var f_li5 = d.createElement("li");
        f_li5.onclick = function(){HMUD_UI.selectFont('font-terminal', true);};
        f_li5.appendChild(d.createTextNode("Terminal"));
        f_ul.appendChild(f_li5);
        f_fs.appendChild(f_ul);
    
        /* clear screen fieldset */
        var c_fs = d.createElement("fieldset");
        var c_lg = d.createElement("legend");
        c_lg.appendChild(d.createTextNode(m.clearOutput + " ("));
        var c_st = d.createElement("strong");
        var nlis = d.getElementById("numLinesInScreen");
        nlis.style.display = "inline";
        c_st.appendChild(nlis);
        c_lg.appendChild(c_st);
        c_lg.appendChild(d.createTextNode(" " + m.lines + ")"));
        c_fs.appendChild(c_lg);
    
        var c_ul = d.createElement("ul");
    
        var c_li1 = d.createElement("li");
        c_li1.onclick = function(){HMUD_UI.clearScreen(1000);};
        c_li1.appendChild(d.createTextNode(m.preserve1000));
        c_ul.appendChild(c_li1);
        var c_li2 = d.createElement("li");
        c_li2.onclick = function(){HMUD_UI.clearScreen("half");};
        c_li2.appendChild(d.createTextNode(m.half));
        c_ul.appendChild(c_li2);
        var c_li3 = d.createElement("li");
        c_li3.onclick = function(){HMUD_UI.clearScreen("all");};
        c_li3.appendChild(d.createTextNode(m.all));
        c_ul.appendChild(c_li3);
        c_fs.appendChild(c_ul);
    
        om.appendChild(f_fs);
        om.appendChild(c_fs);
    
        om.alreadyCreated = true;
    },

    saveLog: function () {
    
        var ls = document.getElementById("logSave");
        var lt = document.getElementById("logSaveTA");
    
        if (ls.style.display == "none") {
            var bodyClass = document.body.className.length
                          ? " class=\"" + document.body.className + "\"" : "";
            window.disableCmdFocus = true;
            lt.value = "<html><head><title>hMUD Log</title><style type=\"text/css\">"
                + document.getElementById("logStyle").innerHTML
                + "</style><body" + bodyClass + "><pre id=\"output\">"
                + HMUD_UI.output.innerHTML + "</pre></body></html>";
            ls.style.display = "block";
        } else {
            window.disableCmdFocus = false;
            lt.value = "";
            ls.style.display = "none";
        }
    },

    showGuide: function (setCookie) {
        if (HMUD_Guide.length < 1)
            return;

        if (setCookie)
            createCookie("showGuide", "yes", 30);

        var d = document;
        var g = d.getElementById("cmdGuide");

        if (g.guideAlreadyCreated) {
            g.style.display = "block";
            return;
        }

        /* creating table */
        var cap = d.createElement("div");
        cap.className = "caption";
        cap.appendChild(d.createTextNode(m.cmdGuide));
        g.appendChild(cap);
        var table = d.createElement("table");
        g.appendChild(table);
        var tbody = d.createElement("tbody");
        table.appendChild(tbody);

        for (var i = 0; i < HMUD_Guide.length; ++i) {
            var tr1 = d.createElement("tr");
            tbody.appendChild(tr1);
            var th = d.createElement("th");
            tr1.appendChild(th);

            /* section */
            th.appendChild(d.createTextNode(HMUD_Guide[i][0]));

            /* commands in this section */
            var commands = HMUD_Guide[i][1];
            var tr2 = d.createElement("tr");
            tbody.appendChild(tr2);
            var td = d.createElement("td");
            tr2.appendChild(td);

            for (var j = 0; j < commands.length; ++j) {
                var sp1 = d.createElement("span");
                sp1.appendChild(d.createTextNode(commands[j][0]));
                sp1.id = "cmd-name-" + i + "-" + j;
                sp1.className = "command-name";
                sp1.onmouseover = function() {
                    var e = document.getElementById(this.id.replace("cmd-name", "cmd-desc"));
                    e.style.display = "block";

                    var p = findPos(this, g);
                    var top = p[1] + this.offsetHeight;
                    e.style.top = (p[1] + this.offsetHeight) + "px";
                    if ((findPos(e)[1] + e.offsetHeight) > (findPos(HMUD_UI.output)[1] + HMUD_UI.output.offsetHeight))
                        e.style.top = (p[1] - e.offsetHeight) + "px";
                };
                sp1.onmouseout = function() {
                    var e = document.getElementById(this.id.replace("cmd-name", "cmd-desc"));
                    e.style.display = "none";
                };
                sp1.onclick = function() { HMUD_UI.cmdline.value += this.firstChild.nodeValue + " "; };
                td.appendChild(sp1);
                td.appendChild(d.createTextNode(" "));

                var sp2 = d.createElement("span");
                sp2.appendChild(d.createTextNode(commands[j][1]));
                sp2.id = "cmd-desc-" + i + "-" + j;
                sp2.style.display = "none";
                sp2.className = "command-description";
                td.appendChild(sp2);
                td.appendChild(d.createTextNode(" "));
            }
        }

        g.guideAlreadyCreated = true;
        g.style.display = "block";
        HMUD_UI.rearrange();
    },

    hideGuide: function(setCookie) {
        document.getElementById("cmdGuide").style.display = "none";
        if (setCookie)
            createCookie("showGuide", "no", 30);
    }
};

function getViewportDimensions() {
    var intH = 0, intW = 0;
    
    if(self.innerHeight) {
       intH = window.innerHeight;
       intW = window.innerWidth;
    } 
    else {
        if(document.documentElement && document.documentElement.clientHeight) {
            intH = document.documentElement.clientHeight;
            intW = document.documentElement.clientWidth;
        }
        else {
            if(document.body) {
                intH = document.body.clientHeight;
                intW = document.body.clientWidth;
            }
        }
    }

    return {
        height: parseInt(intH, 10),
        width: parseInt(intW, 10)
    };
}

function findPos(obj, stopAt)
{
    var curleft = 0;
    var curtop = 0;

    if (obj.offsetParent)
        do
        {
            if (obj == stopAt)
                break;
            curleft += obj.offsetLeft;
            curtop += obj.offsetTop;
        }
        while (obj = obj.offsetParent);

    return [curleft,curtop];
}

