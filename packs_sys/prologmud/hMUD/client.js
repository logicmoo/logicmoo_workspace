var m = HMUD_Messages;
var c = HMUD_Config;
var HMUD_Client = {

    canConnect: false,

    connectIId: null,
    connectingIId: null,
    bridge: null,

    connect: function() {
        this.bridge.connect(c.host, c.port, c.policyPort);
    },

    disconnect: function() {
        this.bridge.close();
        // the close event is only dispatched when the server closes the
        // connection, in this case, the client is closing the connection, so
        // we should do it here
        this.handleMessage("disconnected");
    },

    tryToConnect: function () {
        if (this.connectIId == null) {
            var that = this;
            this.loadFlashBridge();
            this.connectIId = setInterval(function(){that.tryToConnect();}, 100);
        }

        if (this.canConnect) {
            this.connect();
            clearInterval(this.connectIId);
            this.connectIId = null;
        }
    },

    loadFlashBridge: function() {
        swfobject.embedSWF(
            "bridge/hmud-bridge.swf",
            "hMUDBridgePlugin",
            "1",
            "1",
            "9.0.0",
            "expressInstall.swf",
            false,
            { allowscriptaccess: "always", wmode: "transparent" },
            false
        );
    },

    /* send a command to MUD */
    command: function (cmd) {
        this.bridge.command(cmd);
    },

    /*
     * Handle messages sent from the bridge (Flash plugin external interface)
     */
    handleMessage: function(msg, info) {
        switch (msg) {
        case "loaded":
            this.bridge = document.getElementById("hMUDBridgePlugin");
            this.canConnect = true;
            break;
        default:
            break;
        }

        return HMUD_UI.handleMessage(msg, info);
    }
};

