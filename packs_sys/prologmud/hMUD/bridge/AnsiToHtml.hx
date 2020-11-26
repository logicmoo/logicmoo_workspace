/*
 * The MIT License
 *
 * Copyright (c) 2009 Alonso Andres
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
class AnsiToHtml
{
    static var curr_ansi_sequence : {
        reset:Bool,
        bold:Bool,
        underline:Bool,
        blink:Bool,
        reverse:Bool,
        fg:Int,
        bg:Int
    } = {
        reset:     true,
        bold:      false,
        underline: false,
        blink:     false,
        reverse:   false,
        fg:        0,
        bg:        0
    };

    static var inansi:Bool = false;
    static var lastcd:String = "";
    static var lastst:String = null;

    public static var spaceString:String = " ";

    /* Update the current ANSI state with a new ANSI code */
    static function update_sequence(code:Int):Void
    {
        switch (code) {
        case 0:
            curr_ansi_sequence.reset = true;
            curr_ansi_sequence.bold = false;
            curr_ansi_sequence.underline = false;
            curr_ansi_sequence.blink = false;
            curr_ansi_sequence.reverse = false;
            curr_ansi_sequence.fg = 0;
            curr_ansi_sequence.bg = 0;
        case 1:
            curr_ansi_sequence.reset = false;
            curr_ansi_sequence.bold = true;
        case 4:
            curr_ansi_sequence.reset = false;
            curr_ansi_sequence.underline = true;
        case 5:
            curr_ansi_sequence.reset = false;
            curr_ansi_sequence.blink = true;
        case 7:
            curr_ansi_sequence.reset = false;
            if (!curr_ansi_sequence.reverse) {
                if (curr_ansi_sequence.fg == 0)
                    curr_ansi_sequence.fg = 37;
                if (curr_ansi_sequence.bg == 0)
                    curr_ansi_sequence.bg = 40;

                var tmp = curr_ansi_sequence.fg;
                curr_ansi_sequence.fg = (curr_ansi_sequence.bg - 10);
                curr_ansi_sequence.bg = (tmp + 10);
                curr_ansi_sequence.reverse = true;
            }
        case 22:
            curr_ansi_sequence.reset = false;
            curr_ansi_sequence.bold = false;
        case 30, 31, 32, 33, 34, 35, 36, 37:
            curr_ansi_sequence.reset = false;
            curr_ansi_sequence.fg = code;
        case 40, 41, 42, 43, 44, 45, 46, 47:
            curr_ansi_sequence.reset = false;
            curr_ansi_sequence.bg = code;
        }
    }

    /* Classes representing the current ANSI state */
    static function get_current_html_classes():List<String>
    {
        var classes = new List<String>();

        if (curr_ansi_sequence.reset) {
            classes.add("r");
            return classes;
        } else {
            if (curr_ansi_sequence.underline) {
                classes.add("u");
            }
            if (curr_ansi_sequence.blink) {
                classes.add("l");
            }
            /*if (curr_ansi_sequence.reverse) {
                classes.add("v"); // FIXME: Reverse is being handled internaly now.
            }*/
            if (curr_ansi_sequence.fg != 0) {
                classes.add((if (curr_ansi_sequence.bold) "b" else "") + "c" + curr_ansi_sequence.fg);
            }
            if (curr_ansi_sequence.bg != 0) {
                classes.add("c" + curr_ansi_sequence.bg);
            }
    
            return classes;
        }
    }

    public static function parse(str:String):String
    {
        var new_str = new StringBuf();
        var c = "";
        var i = 0;

        var isdigit = ~/^[0-9]+$/;

        var write_last_seq = function () {
            var classes = get_current_html_classes();
            var seq = classes.join(" ");

            if (lastst != seq && seq.length > 0) {
                if (lastst != null) {
                    new_str.add("</b>");
                }

                new_str.add("<b class=\"" + seq + "\">");
                lastst = seq;
            }
        }

        if (lastst != null) {
            new_str.add("<b class=\"" + lastst + "\">");
        }

        while ((c = str.charAt(i++)) != "")
        {
            if (!inansi && c == "\x1b") {
                inansi = true;
            } else if (inansi && isdigit.match(c)) {
                lastcd += c;
            } else if (inansi && c == "m") {
                inansi = false;
                update_sequence(Std.parseInt(lastcd));
                lastcd = "";
                write_last_seq();
            } else if (inansi) {
                if (lastcd != "") {
                    update_sequence(Std.parseInt(lastcd));
                    lastcd = "";
                    write_last_seq();
                }
            } else {
                if (c == " ")
                    new_str.add(spaceString);
                else if (c == "\r") {
                    if (str.charAt(i) != "\n")
                        new_str.add(spaceString + "<br>");
                } else if (c == "\n")
                    new_str.add(spaceString + "<br>");
                else
                    new_str.add(StringTools.htmlEscape(c));
            }
        }

        if (lastst != null) {
            new_str.add("</b>");
        }

        return new_str.toString();
    }
}
