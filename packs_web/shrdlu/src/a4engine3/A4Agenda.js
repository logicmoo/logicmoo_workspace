var AgendaEntry = /** @class */ (function () {
    function AgendaEntry() {
        this.scripts = [];
    }
    AgendaEntry.fromXML = function (xml) {
        var ae = new AgendaEntry();
        ae.time = Number(xml.getAttribute("time"));
        for (var i = 0; i < xml.children.length; i++) {
            var script_xml = xml.children[i];
            ae.scripts.push(A4Script.fromXML(script_xml));
        }
        return ae;
    };
    AgendaEntry.fromAgendaEntry = function (ae) {
        var ae2 = new AgendaEntry();
        ae2.time = ae.time;
        for (var _i = 0, _a = ae.scripts; _i < _a.length; _i++) {
            var s = _a[_i];
            ae2.scripts.push(A4Script.fromA4Script(s));
        }
        return ae2;
    };
    AgendaEntry.prototype.execute = function (o, map, game, otherCharacter) {
        var retValue = SCRIPT_FINISHED;
        var seq = null;
        for (var _i = 0, _a = this.scripts; _i < _a.length; _i++) {
            var s = _a[_i];
            if (seq == null) {
                s.reset();
                retValue = s.execute(o, map, game, otherCharacter);
                if (retValue == SCRIPT_FINISHED) {
                    // good, do nothing
                }
                else if (retValue == SCRIPT_NOT_FINISHED) {
                    // script needs more time, create an script queue
                    seq = new A4ScriptExecutionQueue(o, map, game, otherCharacter);
                    seq.scripts.push(A4Script.fromA4Script(s));
                    if (o != null) {
                        o.addScriptQueue(seq);
                    }
                    else if (map != null) {
                        map.addScriptQueue(seq);
                    }
                    else {
                        game.addScriptQueue(seq);
                    }
                }
                else {
                    // failed, stop the script
                    break;
                }
            }
            else {
                s.reset();
                seq.scripts.push(A4Script.fromA4Script(s));
            }
        }
        return retValue;
    };
    return AgendaEntry;
}());
var Agenda = /** @class */ (function () {
    function Agenda() {
        this.entries = [];
    }
    Agenda.fromXML = function (xml) {
        var a = new Agenda();
        a.name = xml.getAttribute("agenda");
        a.duration = Number(xml.getAttribute("duration"));
        a.cycle = 0;
        if (xml.getAttribute("cycle") != null)
            a.cycle = Number(xml.getAttribute("cycle"));
        a.loop = false;
        if (xml.getAttribute("loop") == "true")
            a.loop = true;
        a.absoluteTime = false;
        if (xml.getAttribute("absoluteTime") == "true")
            a.absoluteTime = true;
        for (var i = 0; i < xml.children.length; i++) {
            a.entries.push(AgendaEntry.fromXML(xml.children[i]));
        }
        a.absoluteStartCycle = -1;
        return a;
    };
    Agenda.fromAgenda = function (a) {
        var a2 = new Agenda();
        a2.name = a.name;
        a2.duration = a.duration;
        a2.loop = a.loop;
        a2.absoluteTime = a.absoluteTime;
        a2.cycle = 0;
        for (var _i = 0, _a = a.entries; _i < _a.length; _i++) {
            var e = _a[_i];
            a2.entries.push(AgendaEntry.fromAgendaEntry(e));
        }
        a2.absoluteStartCycle = -1;
        return a2;
    };
    Agenda.prototype.execute = function (o, map, game, otherCharacter) {
        if (this.absoluteTime) {
            if (this.absoluteStartCycle < 0)
                this.absoluteStartCycle = game.cycle;
            this.cycle = game.cycle - this.absoluteStartCycle;
        }
        for (var _i = 0, _a = this.entries; _i < _a.length; _i++) {
            var ae = _a[_i];
            if (ae.time == (this.cycle % this.duration)) {
                // execute entry!
                ae.execute(o, map, game, otherCharacter);
            }
        }
        if (!this.absoluteTime)
            this.cycle++;
        if (this.cycle >= this.duration && !this.loop) {
            if (!this.loop)
                return true;
        }
        return false;
    };
    return Agenda;
}());
