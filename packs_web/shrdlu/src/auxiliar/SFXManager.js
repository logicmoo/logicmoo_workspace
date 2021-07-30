var SFXManagerNode = /** @class */ (function () {
    function SFXManagerNode(b, n) {
        this.buffer = b;
        this.name = n;
    }
    return SFXManagerNode;
}());
var SFXManager = /** @class */ (function () {
    function SFXManager() {
        this.hash = {};
        this.already_played = [];
        console.log("SFXManager created.");
    }
    SFXManager.prototype.play = function (sfxName) {
        var sfx = this.hash[sfxName];
        if (sfx == null) {
            // load it:
            var SFXM_1 = this;
            var request_1 = new XMLHttpRequest();
            request_1.open('GET', sfxName, true);
            request_1.responseType = 'arraybuffer';
            // Decode asynchronously
            request_1.onload = function () {
                audioCtx.decodeAudioData(request_1.response, function (buffer) {
                    sfx = new SFXManagerNode(buffer, sfxName);
                    SFXM_1.hash[sfxName] = sfx;
                    SFXM_1.playInternal(sfx);
                });
            };
            request_1.send();
        }
        else {
            this.playInternal(sfx);
        }
    };
    SFXManager.prototype.playInternal = function (sfx) {
        // do not play the same SFX more than one during the same game cycle (to avoid volume issues)
        if (this.already_played.indexOf(sfx) == -1) {
            this.already_played.push(sfx);
            var source = audioCtx.createBufferSource();
            source.buffer = sfx.buffer;
            source.connect(audioCtx.destination);
            source.start();
        }
    };
    // clears the list of already played SFX
    SFXManager.prototype.next_cycle = function () {
        this.already_played = [];
    };
    return SFXManager;
}());
