var RuleBasedAIDebugger = /** @class */ (function () {
    function RuleBasedAIDebugger() {
        this.AI = null;
        this.leftTab = 0;
        this.righttab = 1;
        this.leftScroll = 0;
        this.rightScroll = 0;
    }
    RuleBasedAIDebugger.prototype.setAI = function (AI) {
        this.AI = AI;
    };
    RuleBasedAIDebugger.prototype.mouseClick = function (mouse_x, mouse_y, button) {
        if (mouse_y < 4 + 16) {
            if (mouse_x < WINDOW_WIDTH / 2) {
                this.leftTab++;
                this.leftTab = this.leftTab % 6;
                this.leftScroll = 0;
            }
            else {
                this.righttab++;
                this.righttab = this.righttab % 6;
                this.rightScroll = 0;
            }
        }
        else if (mouse_y < WINDOW_HEIGHT / 2) {
            if (mouse_x < WINDOW_WIDTH / 2) {
                this.leftScroll -= 20;
                if (this.leftScroll <= 0)
                    this.leftScroll = 0;
            }
            else {
                this.rightScroll -= 20;
                if (this.rightScroll <= 0)
                    this.rightScroll = 0;
            }
        }
        else {
            if (mouse_x < WINDOW_WIDTH / 2) {
                this.leftScroll += 20;
            }
            else {
                this.rightScroll += 20;
            }
        }
    };
    RuleBasedAIDebugger.prototype.draw = function () {
        ctx.globalAlpha = 0.6;
        ctx.fillStyle = "black";
        ctx.fillRect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);
        ctx.globalAlpha = 1;
        this.drawTab(this.leftTab, 0, 0, WINDOW_WIDTH / 2, WINDOW_HEIGHT, this.leftScroll);
        this.drawTab(this.righttab, WINDOW_WIDTH / 2, 0, WINDOW_WIDTH, WINDOW_HEIGHT, this.rightScroll);
    };
    RuleBasedAIDebugger.prototype.drawTab = function (type, x0, y0, x1, y1, scroll) {
        if (type == 0)
            this.drawPerceptionTab(x0, y0, x1, y1);
        else if (type == 1)
            this.drawShortTermMemoryTab(x0, y0, x1, y1);
        else if (type == 2)
            this.drawLongTermMemoryTab(x0, y0, x1, y1, scroll);
        else if (type == 3)
            this.drawIntentionsTab(x0, y0, x1, y1);
        else if (type == 4)
            this.drawOntologyTab(x0, y0, x1, y1);
        else if (type == 5)
            this.drawContextTab(x0, y0, x1, y1);
    };
    RuleBasedAIDebugger.prototype.drawPerceptionTab = function (x0, y0, x1, y1) {
        this.drawTermListTab("Perception", this.AI.perceptionBuffer, x0, y0, x1, y1);
    };
    RuleBasedAIDebugger.prototype.drawShortTermMemoryTab = function (x0, y0, x1, y1) {
        var fontWidth = 6;
        var maxTextLength = Math.floor(((x1 - x0) - 8) / fontWidth);
        fillTextTopLeft("Short-term Memory", x0 + 4, y0 + 4, fontFamily16px, "white");
        var y = y0 + 4 + 16 + 4;
        for (var _i = 0, _a = this.AI.shortTermMemory.plainTermList; _i < _a.length; _i++) {
            var te = _a[_i];
            var str = te.activation + " - " + te.term.toString();
            while (str.length > maxTextLength) {
                var tmp = str.substring(0, maxTextLength);
                fillTextTopLeft(tmp, x0 + 4, y, fontFamily8px, "white");
                str = "  " + str.substring(maxTextLength);
                y += 8;
            }
            fillTextTopLeft(str, x0 + 4, y, fontFamily8px, "white");
            y += 8;
        }
    };
    RuleBasedAIDebugger.prototype.drawTermListTab = function (name, l, x0, y0, x1, y1) {
        var fontWidth = 6;
        var maxTextLength = Math.floor(((x1 - x0) - 8) / fontWidth);
        fillTextTopLeft(name, x0 + 4, y0 + 4, fontFamily16px, "white");
        var y = y0 + 4 + 16 + 4;
        for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
            var t = l_1[_i];
            var str = "- " + t.toString();
            while (str.length > maxTextLength) {
                var tmp = str.substring(0, maxTextLength);
                fillTextTopLeft(tmp, x0 + 4, y, fontFamily8px, "white");
                str = "  " + str.substring(maxTextLength);
                y += 8;
            }
            fillTextTopLeft(str, x0 + 4, y, fontFamily8px, "white");
            y += 8;
        }
    };
    RuleBasedAIDebugger.prototype.drawLongTermMemoryTab = function (x0, y0, x1, y1, scroll) {
        var fontWidth = 6;
        var maxTextLength = Math.floor(((x1 - x0) - 8) / fontWidth);
        fillTextTopLeft("Long-term Memory:", x0 + 4, y0 + 4, fontFamily16px, "white");
        var y = y0 + 4 + 16 + 4;
        var scroll_skip = 0;
        for (var _i = 0, _a = this.AI.longTermMemory.plainSentenceList; _i < _a.length; _i++) {
            var se = _a[_i];
            if (scroll_skip < scroll) {
                scroll_skip++;
                continue;
            }
            var r = se.sentence;
            var str = "- [" + se.provenance + "] " + r.toString();
            while (str.length > maxTextLength) {
                var tmp = str.substring(0, maxTextLength);
                fillTextTopLeft(tmp, x0 + 4, y, fontFamily8px, "white");
                str = "  " + str.substring(maxTextLength);
                y += 8;
            }
            fillTextTopLeft(str, x0 + 4, y, fontFamily8px, "white");
            y += 8;
        }
    };
    RuleBasedAIDebugger.prototype.drawIntentionsTab = function (x0, y0, x1, y1) {
        var intentions_l = [];
        for (var _i = 0, _a = this.AI.intentions; _i < _a.length; _i++) {
            var tmp = _a[_i];
            intentions_l.push(tmp[0]);
        }
        this.drawTermListTab("Intentions:", intentions_l, x0, y0, x1, y1);
    };
    RuleBasedAIDebugger.prototype.drawOntologyTab = function (x0, y0, x1, y1) {
        var fontWidth = 6;
        var maxTextLength = Math.floor(((x1 - x0) - 8) / fontWidth);
        fillTextTopLeft("Ontology:", x0 + 4, y0 + 4, fontFamily16px, "white");
        var y = y0 + 4 + 16 + 4;
        var x = x0 + 4;
        for (var _i = 0, _a = this.AI.o.getAllSorts(); _i < _a.length; _i++) {
            var s = _a[_i];
            var str = "- " + s.name + ": [ ";
            for (var _b = 0, _c = s.parents; _b < _c.length; _b++) {
                var s2 = _c[_b];
                str += s2.name + " ";
            }
            str += "]";
            while (str.length > maxTextLength) {
                var tmp = str.substring(0, maxTextLength);
                fillTextTopLeft(tmp, x, y, fontFamily8px, "white");
                str = "  " + str.substring(maxTextLength);
                y += 8;
            }
            fillTextTopLeft(str, x, y, fontFamily8px, "white");
            y += 8;
            if (y >= WINDOW_HEIGHT) {
                y = y0 + 4 + 16 + 4;
                x += 200;
            }
        }
    };
    RuleBasedAIDebugger.prototype.drawContextTab = function (x0, y0, x1, y1) {
        var y = y0 + 4;
        for (var _i = 0, _a = this.AI.contexts; _i < _a.length; _i++) {
            var context = _a[_i];
            fillTextTopLeft("Natural Language Context (" + context.ai.selfID + " -> " + context.speaker + "):", x0 + 4, y, fontFamily16px, "white");
            y += 20;
            fillTextTopLeft("Short Term Memory Entities:", x0 + 4, y, fontFamily8px, "white");
            y += 8;
            for (var _b = 0, _c = context.shortTermMemory; _b < _c.length; _b++) {
                var pe = _c[_b];
                var str = "- " + pe.objectID + ": [ ";
                for (var _d = 0, _e = pe.terms; _d < _e.length; _d++) {
                    var t = _e[_d];
                    str += t + " ";
                }
                str += "]";
                fillTextTopLeft(str, x0 + 4, y, fontFamily8px, "white");
                y += 8;
            }
            y += 8;
            fillTextTopLeft("Mentioned Entities:", x0 + 4, y, fontFamily8px, "white");
            y += 8;
            for (var _f = 0, _g = context.mentions; _f < _g.length; _f++) {
                var pe = _g[_f];
                var str = "- " + pe.objectID + " : [ ";
                for (var _h = 0, _j = pe.terms; _h < _j.length; _h++) {
                    var t = _j[_h];
                    str += t + " ";
                }
                str += "]";
                fillTextTopLeft(str, x0 + 4, y, fontFamily8px, "white");
                y += 8;
            }
            y += 8;
            fillTextTopLeft("Performatives:", x0 + 4, y, fontFamily8px, "white");
            y += 8;
            for (var _k = 0, _l = context.performatives; _k < _l.length; _k++) {
                var pe = _l[_k];
                var str = "- '" + pe.text + "'";
                fillTextTopLeft(str, x0 + 4, y, fontFamily8px, "white");
                y += 8;
                str = "  " + pe.performative;
                fillTextTopLeft(str, x0 + 4, y, fontFamily8px, "white");
                y += 8;
            }
            /*
                expectingAnswerToQuestion_stack:NLContextPerformative[] = [];
                expectingAnswerToQuestionTimeStamp_stack:number[] = [];
            
            */
            fillTextTopLeft("inConversation:" + context.inConversation, x0 + 4, y, fontFamily8px, "white");
            y += 8;
            fillTextTopLeft("lastPerformativeInvolvingThisCharacterWasToUs:" + context.lastPerformativeInvolvingThisCharacterWasToUs, x0 + 4, y, fontFamily8px, "white");
            y += 8;
            fillTextTopLeft("expectingThankYou:" + context.expectingThankYou, x0 + 4, y, fontFamily8px, "white");
            y += 8;
            fillTextTopLeft("expectingYouAreWelcome:" + context.expectingYouAreWelcome, x0 + 4, y, fontFamily8px, "white");
            y += 8;
            fillTextTopLeft("expectingGreet:" + context.expectingGreet, x0 + 4, y, fontFamily8px, "white");
            y += 8;
            fillTextTopLeft("expectingFarewell:" + context.expectingFarewell, x0 + 4, y, fontFamily8px, "white");
            y += 8;
            fillTextTopLeft("expectingAnswerToQuestionTimeStamp_stack:" + context.expectingAnswerToQuestionTimeStamp_stack, x0 + 4, y, fontFamily8px, "white");
            y += 8;
            y += 8;
        }
    };
    return RuleBasedAIDebugger;
}());
