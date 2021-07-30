function getElementChildrenByTag(xml, tag) {
    var l = [];
    for (var i = 0; i < xml.children.length; i++) {
        var xml2 = xml.children[i];
        if (xml2.tagName == tag)
            l.push(xml2);
    }
    return l;
}
function getFirstElementChildByTag(xml, tag) {
    for (var i = 0; i < xml.children.length; i++) {
        var xml2 = xml.children[i];
        if (xml2.tagName == tag)
            return xml2;
    }
    return null;
}
// source: https://stackoverflow.com/questions/7616461/generate-a-hash-from-string-in-javascript-jquery
function stringHashFunction(name) {
    var hash = 0;
    var chr;
    if (name.length === 0)
        return hash;
    for (var i = 0; i < name.length; i++) {
        chr = name.charCodeAt(i);
        hash = ((hash << 5) - hash) + chr;
    }
    hash |= 0; // Convert to 32bit integer
    if (hash < 0)
        return -hash;
    return hash;
}
function stringToHTMLString(s) {
    return s.split("\"").join("&quot;");
}
function removeListDuplicates(l) {
    var l2 = [];
    for (var _i = 0, l_1 = l; _i < l_1.length; _i++) {
        var element = l_1[_i];
        if (l2.indexOf(element) == -1)
            l2.push(element);
    }
    return l2;
}
function numberToStringTwoDigits(n) {
    if (n < 10)
        return "0" + n;
    return "" + n;
}
function splitStringBySpaces(text, maxWidth) {
    var buffer = "";
    var last_space = 0;
    var longestLine = 0;
    var lines = [];
    for (var i = 0; i < text.length; i++) {
        buffer += text.charAt(i);
        if (text.charAt(i) == ' ')
            last_space = i;
        if (buffer.length >= maxWidth) {
            if (last_space == 0) {
                // a single word doesn't fit, just split it!
                lines.push(buffer);
                if (buffer.length > longestLine)
                    longestLine = buffer.length;
                buffer = "";
            }
            else {
                var backspaces = i - last_space;
                buffer = buffer.substring(0, buffer.length - backspaces);
                i -= backspaces;
                lines.push(buffer);
                if (buffer.length > longestLine)
                    longestLine = buffer.length;
                buffer = "";
                last_space = 0;
            }
        }
    }
    if (buffer != "") {
        lines.push(buffer);
        if (buffer.length > longestLine)
            longestLine = buffer.length;
    }
    return lines;
}
function allArrayElementsTrue(booleanArray) {
    for (var i = 0; i < booleanArray.length; i++) {
        if (booleanArray[i] == false)
            return false;
    }
    return true;
}
function nTruesInArray(booleanArray) {
    var n = 0;
    for (var i = 0; i < booleanArray.length; i++) {
        if (booleanArray[i])
            n++;
    }
    return n;
}
function shuffleList(list) {
    for (var i = list.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var tmp = list[i];
        list[i] = list[j];
        list[j] = tmp;
    }
}
function startsWith(str, prefix) {
    return str.substring(0, prefix.length) == prefix;
}
