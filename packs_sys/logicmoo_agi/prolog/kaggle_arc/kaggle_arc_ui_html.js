//$(function() {

function dragElement(elmnt) {
    var pos1 = 0,
        pos2 = 0,
        pos3 = 0,
        pos4 = 0;
    if (document.getElementById(elmnt.id + "header")) {
        /* if present, the header is where you move the DIV from:*/
        document.getElementById(elmnt.id + "header").onmousedown = dragMouseDown;
    } else {
        /* otherwise, move the DIV from anywhere inside the DIV:*/
        elmnt.onmousedown = dragMouseDown;
    }

    function dragMouseDown(e) {
        e = e || window.event;
        e.preventDefault();
        // get the mouse cursor position at startup:
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        // call a function whenever the cursor moves:
        document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
        e = e || window.event;
        e.preventDefault();
        // calculate the new cursor position:
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        // set the element's new position:
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
    }

    function closeDragElement() {
        /* stop moving when mouse button is released:*/
        document.onmouseup = null;
        document.onmousemove = null;
    }
}
/*
	function showPos(event, text) {
		var el, x, y;
		
		el = document.getElementById('PopUp');
		if (window.event) {
			x = window.event.clientX + document.documentElement.scrollLeft + document.body.scrollLeft;
			y = window.event.clientY + document.documentElement.scrollTop + + document.body.scrollTop;
		} else {
			x = event.clientX + window.scrollX;
			y = event.clientY + window.scrollY;
		}
		x -= 2; y -= 2;
		y = y+15
		el.style.left = x + "px";
		el.style.top = y + "px";
		el.style.display = "block";
		document.getElementById('PopUpText').innerHTML = text;
	}
	*/

function addAccordian(target_name, depth) {
    return addAccordian4("navbar_items", target_name, target_name.firstChild.textContent, depth);
}

function clickGrid(name) {
    // in case we are passed the object and no the name
    var node = toEle(name);
    if (node != null)
        name = node.id;
    console.log("clickGrid(" + name + ")");
    var val = getComponent("grid", "");
    if (val != name) {
        if (true)
            return;
        navToParams({
            grid: name,
            webcmd: "click_grid"
        });
    }
}

function toEle(name) {
    var node = null;
    if (name == null)
        return node;
	if( name.jquery ) {
		node = name.get(0);
		if (isElement(node))
			return node;
	}
	if( name.nodeType ) {
		return name;
	}
	if (isElement(name))
		return name;
    if (typeof name !== 'string') return name;
    if (typeof name === "string") {
        node = document.getElementById(name);
        if (node != null)
            return node;
        var nav = getNavWindow();
        node = nav.document.getElementById(name);
        if (node != null)
            return node;
        var topw = getTopWindow();
        node = topw.document.getElementById(name);
        if (node != null)
            return node;
        node = top.document.getElementById(name);
        if (node != null)
            return node;
        console.warn(name + " not found in " + topw);
        console.warn(name + " not found in " + nav);
        debugger;
        return null;
    }
    return node;
}

function getSomeElementById(Name) {
    var nameE = getSomeElementById(Name, window);
    if (nameE != null)
        return nameE;
    var P2 = window.parent;
    if (P2 != null && P2 != this) {
        nameE = getSomeElementById(Name, P2);
        if (nameE != null)
            return nameE;
    }
    var P3 = window.top;
    if (P3 != null && P3 != this && P3 != P2) {
        nameE = getSomeElementById(Name, P3);
        if (nameE != null)
            return nameE;
    }
    if (Name != "lm_xref") {
        var iframeP = getSomeElementById("lm_xref");
        if (iframeP != null) {
            var P4 = iframe.contentWindow;
            if (P4 != null && P4 != this && P4 != P2) {
                nameE = getSomeElementById(Name, P4);
                if (nameE != null)
                    return nameE;
            }
        }
    } else {
        var iframeP = document.getElementById("lm_xref");
        if (iframeP != null) {
            var P4 = iframe.contentWindow;
            if (P4 != null && P4 != this && P4 != P2) {
                nameE = getSomeElementById(Name, P4);
                if (nameE != null)
                    return nameE;
            }
        }
    }
    return null;
}

function getSomeElementById(Name, P) {
    if (P == null) {
        return null;
    }
    nameE = P.document.getElementById(Name);
    if (nameE != null)
        return nameE;
    var iframeP = P.document.getElementById("lm_xref");
    if (iframeP != null) {
        var P2 = iframe.contentWindow;
        if (P2 != null && P2 != P) {
            nameE = P2.document.getElementById(Name);
            if (nameE != null)
                return nameE;
        }
    }
}

var TwoFiftyPx = "250px";

function toggleNavL(nameE) {
	var nameE = top.document.getElementById(nameE);
	var mainE = top.document.getElementById('main');
    if (nameE.style.width != "0px") {
        nameE.style.width = "0px";
    } else {
        nameE.style.width = TwoFiftyPx;
    }
	
    try {everyoneScrollLeft();} catch (err) {}
	mainE.style.left = nameE.style.width;
}


function toggleNavR(Name) {
	var nameE = top.document.getElementById(Name);
	if (nameE.style.width != "0px") {
		nameE.style.width = "0px";
		//mainE.style.width = "200vh";
	} else {
		nameE.style.width = TwoFiftyPx;
	}
    try {everyoneScrollLeft();} catch (err) {};
}

function likeMain(mainE) {
	var mySideNavL = top.document.getElementById('mySideNavL');
	var mySideNavR = top.document.getElementById('mySideNavR');
	//mainE.style.marginLeft = mySideNavL.style.width;
	mainE.style.left = mySideNavL.style.width; mainE.style.marginLeft = "0px";
	mainE.style.marginRight = mySideNavR.style.width;  mainE.style.right = "0px";
	//mainE.style.right = mySideNavR.style.width; mainE.style.marginRight ="0px";
	mainE.style.width = `calc(100% - ${mySideNavL.style.width} - ${mySideNavR.style.width})`
    mainE.style.height = "calc(100%- 1px)";
}
function likeIFrame(mainE) {
	var mySideNavL = top.document.getElementById('mySideNavL');
	var mySideNavR = top.document.getElementById('mySideNavR');
	//mainE.style.marginLeft = mySideNavL.style.width;
	mainE.style.left = mySideNavL.style.width; mainE.style.marginLeft = "0px";
	mainE.style.marginRight = mySideNavR.style.width;  mainE.style.right = "0px";
	//mainE.style.right = mySideNavR.style.width; mainE.style.marginRight ="0px";
	mainE.style.width = `calc(100% - ${mySideNavL.style.width} - ${mySideNavR.style.width})`
    mainE.style.height = "calc(100%- 1px)";
}

function everyoneScrollLeft() {

	if (top == window) {

		$("body").scrollLeft(0);
		$("#main").scrollLeft(0);

		var mainE = top.document.getElementById("main");
		if (mainE != null) {
			likeMain(mainE);
			$(mainE).scrollLeft(0);
		}
		var iframeE = top.document.getElementById('lm_xref');
		if (iframeE != null) { 
			likeIFrame(iframeE);
			$(iframeE).scrollLeft(0);
		}

        if (iframeE != null) {
            var contentWindow = iframeE.contentWindow;
            //contentWindow.everyoneScrollLeft = window.everyoneScrollLeft;
            try {
                contentWindow.everyoneScrollLeftFrame();
            } catch (err) {};
        }
    } else {
    }
}
function everyoneScrollLeftFrame() {
	$("body").scrollLeft(0);
	$(".panel").scrollLeft(0);
	$("pre").scrollLeft(0);
	$(".wrappable").scrollLeft(0);
	$(".scrollable").scrollLeft(0);
	$("iframe").scrollLeft(0);
}

function navCmd(target) {
    console.log("navCmd(" + target + ")");
    navToParams({
        webcmd: target
    });

    return false;
}


function getNavWindow() {
    if (top.navWindow != null)
        return top.navWindow;
    var iframe = document.getElementById("lm_xref");
    if (iframe == null) {
        iframe = window.parent.document.getElementById("lm_xref");
    }
    if (iframe == null) {
        iframe = top.document.getElementById("lm_xref");
    }
    if (iframe != null) {
        var cw = iframe.contentWindow;
        if (cw != null) {
            top.navWindow = cw;
            return cw;
        }
    }
    return window;
}

function getTopWindow() {
    var nav = getNavWindow();
    if (nav != null)
        return nav.parent;
    return window.top;
    //parent;
}

function setFormFields(srch) {
    var newSrch = new URLSearchParams(srch);
    for (const [key, value] of newSrch.entries()) {
        console.log(`newFF '${key}:' '${value}'`);
        if (ignoredData(key))
            continue;
        setComponent(key, value);
        //  debugger;
    }
}

if (window.var_edits === undefined) {
    window.var_edits = 0;
}

function setMainQueryParams(srch) {
    //return;
    window.var_edits = 0;
    var topw = getTopWindow();
    var oldUrl = topw.location.href;
    if (!oldUrl.endsWith(srch)) {
        setFormFields(srch);
        if (window.var_edits == 0) {
            let url = new URL(oldUrl);
            url.search = srch;
            topw.history.pushState({}, null, url.href);
        }
    }
    var iframe = topw.document.getElementById("lm_xref");
    if (iframe != null) {
        var oldUrl2 = iframe.contentWindow.location.href;
        if (!oldUrl2.endsWith(srch)) {
            let url2 = new URL(oldUrl2);
            url2.search = srch;
            iframe.setAttribute("href", url2.href);
            // debugger;
        }
    }
}

function isElement(element) {
    if (typeof element !== 'object') {
        return false
    }
    return element instanceof Element || element instanceof HTMLDocument || element instanceof Node;
}

function setUrlParam(name, value) {
    console.log("setUrlParam(" + name + "," + value + ")");
    var topw = getTopWindow();
    let srch = new URLSearchParams(topw.location.search);
    if (!srch.has(name) || srch.get(name) != value) {
        srch.set(name, value);
        url = new URL(topw.location.href);
        url.search = srch;
        window.var_edits++;
        topw.history.pushState({}, null, url.href);
    }
}

function toComponent(name) {
    if (isElement(name))
        return name;
    if (ignoredData(name))
        return null;
    var nav = getNavWindow();
    var ele = nav.document.getElementById("param_" + name);
    if (ele != null)
        return ele;
    ele = nav.document.getElementById(name);
    if (ele != null)
        return ele;
    ele = document.getElementById("param_" + name);
    if (ele != null)
        return ele;
    ele = toEle(name);
    if (ele != null)
        return ele;
    return null;
}

function setComponent(name, value) {
    if (ignoredData(name))
        return false;

    if (typeof name === "string") {
        setUrlParam(name, value);
        ele = toComponent(name);
        var retval = false;
        if (ele != null)
            retval = setComponent(ele, value);
        var topw = getTopWindow();
        var tele = topw.document.getElementById("param_" + name);
        if (tele == null)
            tele = topw.document.getElementById(name);
        if (tele != null)
            if (setComponent(tele, value))
                return true;
        return retval;
    }
    var bool_value = true;
    var svalue = ("" + value).trim().toLowerCase;
    if (svalue == "false" || svalue == "no" || svalue == "off" || svalue == "unchecked" || value == "0" || value == "" || value == false || value == null || value == undefined || value == 0) {
        bool_value = false;
    }
    if (isElement(name)) {
        console.log("setComponent(" + name.id + "," + value + ")");
        if (name.getAttribute('type') == 'checkbox') {
            name.checked = bool_value;
            return true;
        } else if (name.tagName.toLowerCase == 'input') {
            name.value = value;
            return true;
        } else if (name.tagName.toLowerCase == 'textarea') {
            name.value = value;
            return true;
        } else {
            if (value.indexOf("<") == -1) {
                name.innerText = value;
                name.textContent = value;
            } else {
                name.innerHtml = value;
            }
        }
    }
    return false;
}

function add_tool_tips(name, text) {
    var node = toEle(name);
    if (node == null) {
        console.error("add_tool_tips missing: " + name);
        return null;
    }
    if (node != null) {
        var wasTitle = node.getAttribute('title');
        if (wasTitle == null || wasTitle === undefined || wasTitle == "") {
            node.setAttribute('title', text);
        } else if (!wasTitle.include(text)) {
            if (text.startsWith("#10;")) {
                node.setAttribute('title', wasTitle + text);
            } else if (text.endsWith("#10;")) {
                node.setAttribute('title', text + wasTitle);
            } else if (wasTitle.endsWith("#10;")) {
                node.setAttribute('title', wasTitle + text);
            } else if (wasTitle.startsWith("#10;")) {
                node.setAttribute('title', text + wasTitle);
            } else {
                wasTitle += "#10;";
                node.setAttribute('title', wasTitle + text);
            }
        }
        return node;
    }
}

function getComponent(name, elsev) {
    if (ignoredData(name))
        return elsev;
    if (typeof name === "string") {
        var topw = getTopWindow();
        var srch = new URLSearchParams(topw.location.search);
        debugger;
        if (srch.has(name)) {
            return srch.get(name);
        }
        var nav = getNavWindow();
        var ele = nav.document.getElementById("param_" + name);
        if (ele == null)
            ele = nav.document.getElementById(name);
        if (ele != null)
            return getComponent(ele, value);
        var tele = topw.document.getElementById("param_" + name);
        if (tele == null)
            tele = topw.document.getElementById(name);
        if (tele != null)
            return getComponent(tele, value);
        return elsev;
    }
    if (isElement(name)) {
        if (name.getAttribute('type') == 'checkbox') {
            return name.checked;
        } else if (name.tagName.toLowerCase == 'input') {
            return name.value;
        } else if (name.tagName.toLowerCase == 'textarea') {
            return name.value;
        }
        var it = name.innerText;
        if (it != null)
            it = it.trim();
        if (it == null || it == "")
            it = name.textContent;
        if (it != null)
            it = it.trim();
        if (it == null || it == "")
            it = name.innerHtml;
        if (it != null)
            it = it.trim();
        if (it != null)
            return it;
    }
    return elsev;
}

function getFormDicts() {
    var nav = getNavWindow();
    var topw = getTopWindow();

    var newSrch = new URLSearchParams({});

    for (const ele of nav.document.querySelectorAll('input[type="checkbox"]')) {
        var key = ele.id;
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.checked;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    for (const ele of topw.document.querySelectorAll('input[type="checkbox"]')) {
        var key = ele.id;
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.checked;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }

    for (const ele of nav.document.querySelectorAll('input[type]:not([type="checkbox"])')) {
        var key = ele.id;
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.checked;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    for (const ele of topw.document.querySelectorAll('input[type]:not([type="checkbox"])')) {
        var key = ele.id;
        if (ignoredData(key))
            continue;
        if (key.indexOf("param_") == 0) {
            if (ele.name)
                key = ele.name;
            else {
                key = key.substring(6);
            }
        }
        var value = ele.value;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    return newSrch;
}

function ignoredData(key) {
    if (key == null || key == undefined || key == [] || key == {} || key == 'nothing') return true;
    if (key == 'undefined' || key === "" || key == " " || key === "icmd" || key === "webcmd") return true;
    if ((typeof key === 'string') && key.includes("accord")) return true;
    return false;
}

{
    top.winObj = null;
    if (top.milledImages === undefined) top.milledImages = {};
    top.navWindow = null;
}

function clearMenu() {
    top.navWindow = null;
    top.milledImages = {};
    var e = top.document.getElementById("navbar_items");
    if (e == null)
        return;
    var child = e.lastElementChild;
    while (child) {
        e.removeChild(child);
        child = e.lastElementChild;
    }
}

function opener() {
    if (top.winObj == null || top.winObj.closed) {
        top.winObj = top.open("", "tips", "width=500, height=300");
        top.winObj.realTop = window.top;
        popup = top.winObj;
        popup.window.addEventListener('load', () => {
            popup.window.addEventListener('unload', () => {
                console.log('> Popup Closed');
                top.winObj = null;
                // window.location.reload();
            });
        });

        popup.document.open();
        popup.document.write(`<!DOCTYPE html>
<html lang="en">
<head>
   <meta charset="UTF-8" />
   <meta http-equiv="X-UA-Compatible" content="IE=edge" />
   <meta name="viewport" content="width=device-width, initial-scale=1.0" />
   <title>ARC Objects</title>
</head>
<body>
   <input type="button" value="click here to clear" onclick="document.getElementById('toolTips').innerHTML = '' "/>
   <div id='toolTips' name='toolTips'/>
</body>
</html>`);
        popup.document.close();

        //top.winObj.onblur = () => top.winObj.focus();
        /*top.winObj.onload = function() {
			let html = `<div style="font-size:30px">Welcome!</div>`;
			top.winObj.document.body.insertAdjacentHTML('afterbegin', html);
        };*/
    }
}

function easyToMatch(text) {
    if (text == null || text === undefined) return "";
    return text.replace(/[^0-9a-zA-Z_()]/gi, '');
}

function writeTips(info) {

    if (ignoredData(info)) return;

    var jqinfo = null;

    if (isElement(info)) {
        jqinfo = $(info);
    } else {
        var infotext = "" + info;
        if ( // skip simple one liners
            (!(infotext.includes("\n"))) &&
            (!(infotext.includes("\r"))) &&
            (!(infotext.includes("<")))) return false;

        if (!infotext.includes("<")) {
            //if (infotext.includes("\n"))
            infotext = `<pre>${infotext}</pre>`;
        }
        jqinfo = $(infotext);
    }

    opener();

    var tipsListDiv = top.winObj.document.getElementById('toolTips');
    var e2m = easyToMatch(jqinfo.prop("innerText"));
    var tip = tipsListDiv.firstElementChild;
    if (tip != null) {
        do {
            if (easyToMatch(tip.innerText).includes(e2m) ||
                easyToMatch(tip.innerHtml).includes(e2m) ||
                easyToMatch(tip.textContent).includes(e2m)) {
                jqinfo = $(tip);
                jqinfo.detach();
                break;
            }
        } while (tip = tip.nextElementSibling);
    }

    $(tipsListDiv).prepend(jqinfo);
    scrollToVertically(tipsListDiv);
    top.winObj.focus();
}


function resizer() {
    opener();
    top.winObj.moveTo(0, 0);
    top.winObj.resizeTo(screen.availWidth, screen.availHeight);
}

function navToParams(jsonDict) {
    console.log("navToParams=" + jsonDict);
    var newSrch = new URLSearchParams(jsonDict);


    var nav = getNavWindow();
    var topw = getTopWindow();
    var ooldSrch = new URLSearchParams(nav.location.search);
    ooldSrch.delete("");
    var topSrch = new URLSearchParams(topw.location.search);
    topSrch.delete("");
    console.log("nav=" + ooldSrch);
    console.log("topw=" + topSrch);
    console.log("newSrch=" + newSrch);

    var oldSrch = new URLSearchParams(nav.location.search);
    oldSrch.delete("");
    // Display the key/value pairs
    for (const [key, value] of oldSrch.entries()) {
        if (ignoredData(key))
            continue;
        console.log(`old ${key}: ${value}`);
    }
    oldSrch.delete("webcmd");
    oldSrch.delete("grid");
    for (const [key, value] of oldSrch.entries()) {
        if (ignoredData(key))
            continue;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        }
    }
    var formdicts = getFormDicts();
    for (const [key, value] of formdicts.entries()) {
        if (ignoredData(key))
            continue;
        if (!newSrch.has(key)) {
            newSrch.set(key, value);
        } else {
            setComponent(key, value);
        }
    }
    newSrch.delete("");
    for (const [key, value] of newSrch.entries()) {
        console.log(`new '${key}:' '${value}'`);
    }
    var newSrchStr = "?" + newSrch;
    var nsrc = topw.location.origin + "/arcproc_main" + newSrchStr;
    var tsrc = topw.location.origin + topw.location.pathname + newSrchStr;
    topw.history.pushState({}, null, tsrc);
    var iframe = document.getElementById("lm_xref");
    if (iframe == null) {
        iframe = window.parent.document.getElementById("lm_xref");
    }
    if (iframe.src == "about:blank" || !sameSrch(ooldSrch, newSrch)) {

        // setMainQueryParams(newSrchStr);
        //	debugger;
        //iframe.style.width = document.getElementById("main").style.width;
        if (iframe.src == "about:blank") {
            iframe.setAttribute("src", nsrc);
        } else {
            nav.location.assign(nsrc);
        }
    }
}

function sameSrch(oldSrch, newSrch) {
    for (const [key, value] of newSrch.entries()) {
        if (oldSrch.get(key) != newSrch.get(key))
            return false;
    }
    for (const [key, value] of oldSrch.entries()) {
        if (oldSrch.get(key) != newSrch.get(key))
            return false;
    }
    return true;
}

function addAccordian4(NBI, target_name, target_textContent, depth) {
    var target_id = target_name.id;
    // console.log(`addAccordian(${target_id},${target_textContent},${depth})`);
    var nav = top.document.getElementById(NBI);
    var spaces = "";
    for (i = 0; i < depth; i++) {
        spaces = spaces + "&nbsp;";
    }
    var href = `#${target_id}`;
    href = "javascript:void(0)";
    let html = `<li class="nav-item" id="${target_id}_menu" >${spaces}<a class="nav-link" href="${href}" id="${target_id}_link" target="lm_xref" onclick="clickAccordian('${target_id}',true);">${target_textContent}</a></li>`;
    nav.insertAdjacentHTML('beforeend', html);
    e = nav.lastElementChild;
    return e;
    //e.scrollIntoView(false);
    // target_name.scrollIntoView(false);
}

function clickAccordian(target_name, scrollTo) {
    console.log(`clickAccordian('${target_name}',${scrollTo})`);
    if (typeof target_name === "string") {
        target_name = accordianStem(target_name);
    }
    var target = toEle(target_name);
    target.classList.toggle("active");
    var panel = toEle(target_name + "_panel");
    var keepGoing = (panel == null || !(panel.classList.contains("panel")));
    if (panel == null) {
        panel = target.nextElementSibling;
        togglePanel(panel, keepGoing, scrollTo);
    } else {
        togglePanel(panel, keepGoing, scrollTo);
    }

    if (scrollTo) {
        try {everyoneScrollLeft();} catch (err) {}
    }

}

function getCurrentFrameAbsolutePosition() {
    let currentWindow = window;
    let currentParentWindow;
    let positions = [];
    let rect;

    while (currentWindow !== window.top) {
        currentParentWindow = currentWindow.parent;
        for (let idx = 0; idx < currentParentWindow.frames.length; idx++)
            if (currentParentWindow.frames[idx] === currentWindow) {
                for (let frameElement of currentParentWindow.document.getElementsByTagName('iframe')) {
                    if (frameElement.contentWindow === currentWindow) {
                        rect = frameElement.getBoundingClientRect();
                        positions.push({
                            x: rect.x,
                            y: rect.y
                        });
                    }
                }
                currentWindow = currentParentWindow;
                break;
            }
    }
    return positions.reduce((accumulator, currentValue) => {
        return {
            x: accumulator.x + currentValue.x,
            y: accumulator.y + currentValue.y
        };
    }, {
        x: 0,
        y: 0
    });
}

function isInViewport_when_IFrame(el) {

    var currentFramePosition = getCurrentFrameAbsolutePosition();

    var rect = el.getBoundingClientRect();
    var elemTop = rect.top + currentFramePosition.y;
    var elemBottom = rect.bottom + currentFramePosition.y;
    var elemLeft = rect.left + currentFramePosition.x;
    var elemRight = rect.right + currentFramePosition.x;

    var viewportWidth = window.innerWidth || document.documentElement.clientWidth;
    var viewportHeight = window.innerHeight || document.documentElement.clientHeight;
    var viewportTopHeight = window.top.innerHeight || window.top.document.documentElement.clientHeight;
    var viewportTopWidth = window.top.innerWidth || window.top.document.documentElement.clientWidth;
    var windowInnerHeight = (viewportHeight + viewportTopHeight * 0.5);
    var windowInnerWidth = (viewportWidth + viewportTopWidth * 0.5);

    console.log("eleTop " + elemTop);
    console.log("elemBottom " + elemBottom);

    console.log("windowInnerHeight " + (viewportHeight + (viewportTopHeight * 0.5)))

    var isVisibleH = (elemLeft >= 0) && (elemRight <= windowInnerWidth);
    var isVisibleV = (elemTop >= 0) && (elemBottom <= windowInnerHeight);
    return isVisibleH && isVisibleV;
}

function isInViewport(element) {
    if (window !== window.top) {
        return isInViewport_when_IFrame(element)
    }
    const rect = element.getBoundingClientRect();
    const viewportWidth = window.innerWidth || document.documentElement.clientWidth;
    const viewportHeight = window.innerHeight || document.documentElement.clientHeight;

    for (const key in rect) {
        if (typeof rect[key] !== 'function') {
            console.warn(`isInViewport ${key} : ${rect[key]}`);
        }
    }
    console.warn(`viewportWidth/Height=${viewportWidth}/${viewportWidth}`);

    var elemTop = rect.top;
    var elemBottom = rect.bottom;

    return (rect.top >= 0 && rect.left >= 0 && rect.bottom <= viewportHeight && rect.right <= viewportWidth);
}

function togglePanel(name, keepGoing, scrollTo) {
    var panel = intoPanel(name);
    if (panel == null)
        return;

    var notHidden = panel.classList.contains("panel_shown");
    var inViewport = isInViewport_when_IFrame(panel);
    console.warn("inViewport=" + inViewport);
    var wasLastPanel = (top.lastPanelShown == panel);

    if (!scrollTo) {
        wasLastPanel = true;
        top.lastPanelShown = panel;
    }

    if (wasLastPanel) {
        inViewport = true;
    }

    if (notHidden && !inViewport) {
        scrollToPanel(panel);
        lastPanelShown = panel;
        return;
    }

    if (notHidden) {
        hidePanel(panel, keepGoing);
    } else {
        showPanel(panel, keepGoing, scrollTo);
    }

    if (scrollTo) {
        scrollToPanel(panel);
    }
}

function setVisible(panel, tf) {
    if (panel == null) return;
    //var block = (tf?"block":"none");
    var block = tf;
    panel.style.display = block;
    //panel.nextElementSibling.style.display=block;
    panel.scrollIntoView(false);
    scrollToVertically(panel)
}

function scrollToPanel(panel) {
    if (panel == null) return;
    panel.scrollIntoView(false);
    var scrl = panel.previousElementSibling;
    if (scrl == null)
        scrl = panel;
    scrollToVertically(scrl);
}

function scrollToVertically(scrl) {
  scrl.scrollIntoView({ behavior: 'smooth', block: 'center', inline: 'start' });
}


{
    top.lastPanelShown = null;
}

function intoPanel(name) {
    if (name == null)
        return null;
    var panel = name;
    if (typeof name === "string") {
        panel = toEle(name);
        if (panel == null) {
            console.warn(name + " not found in " + nav);
            return null;
        }
    }
    if (isElement(panel)) {
        if (panel.classList.contains("panel")) return panel;
        var p = findParentWithClass(panel, "panel");
        if (isElement(p) && p.classList.contains("panel")) return p;
    }
    if (!panel.classList.contains("panel")) {
        console.warn(panel + "not a panel: " + pp(panel));
        // return null;
    }
    return panel;
}

function showPanel(name, keepGoing, scrollTo) {
    var panel = intoPanel(name);
    if (panel == null)
        return;

    /*
		if(scrollTo) {
			// gets us the show hide toggles
			if(top.lastPanelShown == panel) {
				if(panel.classList.contains("panel_shown")) {
				   hidePanel(panel);
				   top.lastPanelShown = null;
				   return;
				}
			} 
		}*/
    //panel.scrollIntoView(false);

    var PE = panel.parentElement;
    if (PE != null) {
        if (!(PE.classList.contains("panel"))) {
            var GPE = PE.parentElement;
            if (GPE != null) {
                if (GPE.classList.contains("panel")) {
                    PE = GPE;
                } else {
                    PE = GPE.parentElement;
                }
            }
        }
        if (PE.classList.contains("panel_hidden")) {
            var sibling = PE.firstElementChild;
            do {
                if (sibling != panel) {
                    if (sibling.classList.contains("panel_shown")) {
                        hidePanel(sibling);
                    }
                }
            } while (sibling = sibling.nextElementSibling);
            showPanel(PE, false, false);
        }
    }
    panel.classList.add("panel_shown");
    panel.classList.remove("panel_hidden");
    var menu_id = panel.id;
    openCloseMenu(menu_id, true);
    //panel.style.display = "inline-block";
    //panel.style.minHeight = "10px";
    //panel.style.maxHeight = "none";
    //panel.style.overflow = "auto";
    if (scrollTo) {
        var scrl = panel.previousElementSibling;
        if (scrl == null)
            scrl = panel;
        scrollToVertically(scrl);
        top.lastPanelShown = panel;
    }

}

function hidePanel(name, keepGoing) {
    var panel = intoPanel(name);
    if (panel == null)
        return;
    panel.classList.add("panel_hidden");
    panel.classList.remove("panel_shown");
    var menu_id = panel.id;
    openCloseMenu(menu_id, false);

    /*
		panel.style.display = "none";
		panel.style.minHeight = "0px";  
		panel.style.maxHeight = "0px";
		panel.style.overflow = "none";*/
}

function correctCmd(sessCmd) {
    var newSrch;
    if (sessCmd.startsWith("?")) {
        newSrch = new URLSearchParams(sessCmd);
        sessCmd = top.window.location.origin + "/arcproc_iframe";
    } else if (sessCmd.includes("/") || sessCmd.includes("?")) {
        var url = new URL(sessCmd, top.window.location);
        newSrch = new URLSearchParams(url.search);
        sessCmd = top.window.location.origin + url.pathname;
    } else if (sessCmd.indexOf("=") > 0) {
        newSrch = new URLSearchParams("?" + sessCmd);
        sessCmd = top.window.location.origin + "/arcproc_iframe";
    } else {
        newSrch = new URLSearchParams("?icmd=" + sessCmd);
        sessCmd = top.window.location.origin + "/arcproc_iframe";
    }

    let oldSrch = new URLSearchParams(top.window.location.search);
    oldSrch.delete("icmd");
    oldSrch.delete("webcmd");
    for (const [key, value] of oldSrch.entries()) {
        if (ignoredData(key))
            continue;
        if (!newSrch.has(key)) { // newSrch.set(key, value);
        }
    }
    var s = "" + newSrch;
    if (s !== "" && s !== "?") {
        if (!s.startsWith("?")) {
            s = "?" + s;
        }
        sessCmd = sessCmd + s;
    }
    return sessCmd;
}

function set_delayed_accordian_cmd(accordian_in, someCmd) {

    var accordian = ensure_delayed_accordian(accordian_in, "Cmd: " + someCmd);

    sessCmd = correctCmd(someCmd);
    var accordian_id = accordian.id;

    document.getElementById(accordian_id + '_iframe_div').innerHTML = `<p><a href="${sessCmd}" target="${accordian_id}_iframe">Refresh</a><br></p><p><iframe  
			src="${sessCmd}" width="1024" height="70%" frameborder="0" id="${accordian_id}_iframe" 
			name="${accordian_id}_iframe" style="width: 100%" scrolling="auto"></p></iframe></p>`;
    //  document.getElementById("sess_accordian").click();
}

function add_delayed_accordian(accordian_in, title, sessCmd) {
    var accordian = ensure_delayed_accordian(accordian_in, title);
    addAccordian4("global_command_items", accordian, title, 1);
    set_delayed_accordian_cmd(accordian, sessCmd);
}

function ensure_delayed_accordian(accordian_in, title) {
    if (isElement(accordian_in)) {
        return accordian_in;
    }
    debugger;
    var accordian_id = accordian_in;
    var accordian = top.document.getElementById(accordian_id);
    if (accordian == null) {
        var p = top.document.createElement('p');
        p.innerHTML = `<input type="checkbox" id="${accordian_id}" class="hidecontent" style="top: 0; left: 0; width: 100%; margin-top: 2px;"/>
				<label for="${accordian_id}" style="top: 0; left: 0; width: 100%; margin-top: 0px;">${title}</label>
				<div id="${accordian_id}_panel" class="content hidecontent" style="top: 0; left: 0; width: 100%; height=100vh;">
					<div id="${accordian_id}_iframe_div"></div>
					<label id="${accordian_id}_x" for="${accordian_id}" style="right: 0; position: relative; width: 8px">x</label>
				</div>`
        var sess_iframe_div = top.document.getElementById("sess_iframe_div");
        sess_iframe_div.appendChild(p);
        accordian = top.document.getElementById(accordian_id);
    }
    return accordian;
}


function removeLastInstance(badtext, str) {
    if (!str.endsWith(badtext)) return str;
    var charpos = str.lastIndexOf(badtext);
    if (charpos < 0) return str;
    ptone = str.substring(0, charpos);
    pttwo = str.substring(charpos + (badtext.length));
    return (ptone + pttwo);
}

function accordianStem(target_name) {
    target_name = removeLastInstance('_x', target_name);
    target_name = removeLastInstance('_div', target_name);
    target_name = removeLastInstance('_iframe', target_name);
    target_name = removeLastInstance('_panel', target_name);
    target_name = removeLastInstance('_link', target_name);
    target_name = removeLastInstance('_menu', target_name);
    return target_name;
}



/** Function that count occurrences of a substring in a string;
 * @param {String} string               The string
 * @param {String} subString            The sub string to search for
 * @param {Boolean} [allowOverlapping]  Optional. (Default:false)
 *
 * @author Vitim.us https://gist.github.com/victornpb/7736865
 * @see Unit Test https://jsfiddle.net/Victornpb/5axuh96u/
 * @see https://stackoverflow.com/a/7924240/938822
 */
function occurrences(string, subString, allowOverlapping) {

    string += "";
    subString += "";
    if (subString.length <= 0) return (string.length + 1);

    var n = 0,
        pos = 0,
        step = allowOverlapping ? 1 : subString.length;

    while (true) {
        pos = string.indexOf(subString, pos);
        if (pos >= 0) {
            ++n;
            pos += step;
        } else break;
    }
    return n;
}

function openCloseMenu(target_name, isExpanded) {
    var elem = target_name;
    if (typeof target_name === "string") {
        target_name = accordianStem(target_name);
        elem = top.document.getElementById(target_name + '_menu');
    }
    if (!isElement(elem)) return;

    if (isExpanded) {
        elem.classList.remove("collapsed_menu");
        elem.classList.add("expanded_menu");
    } else {
        elem.classList.remove("expanded_menu");
        elem.classList.add("collapsed_menu");
    }
    //if (elem.classList.contains("active")) { return; }
    var count = occurrences(elem.innerHTML, "&nbsp;");
    sibling = elem;
    while (sibling = sibling.nextElementSibling) {
        var scount = occurrences(sibling.innerHTML, "&nbsp;");
        if (scount <= count) {
            // if(isExpanded) {
            scrollToVertically(sibling.previousElementSibling);
            //}
            break;
        }
        if (isExpanded) {
            if (sibling.classList.contains("collapsed_menu")) {
                ///hidePanel(sibling.id, undefined);
            }
            sibling.classList.remove("collapsed_menu");
            sibling.classList.add("expanded_menu");
        } else {
            if (sibling.classList.contains("collapsed_menu")) {
                ///showPanel(sibling.id, undefined, false);
            }
            sibling.classList.remove("expanded_menu");
            sibling.classList.add("collapsed_menu");
        }
    }


}

function activateMenu(target_name) {
    var elem = target_name;
    if (typeof target_name === "string") {
        target_name = accordianStem(target_name);
        elem = top.document.getElementById(target_name + '_menu');
    }

    if (!isElement(elem)) return;

    if (elem.classList.contains("active")) { return; }

    var PWC = findParentWithClass(elem, "nav-pills");
    var sibling = PWC.firstElementChild;
    do {
        if (sibling != elem) {
            sibling.classList.remove("active");
        }
    } while (sibling = sibling.nextElementSibling);
    elem.classList.add("active");
    elem.scrollIntoView(false);
}


function isBlank(v) {
    return (v == null || v === undefined || v === "");
}


function cvtToIMG(name) {
    return intoNamedImg(false, true, name);
}

function intoNamedImg(useCache, replace, nodespec, nodeid, h, w) {
    if (isBlank(nodespec)) {
        if (isBlank(nodeid)) {
            var me = document.currentScript;
            nodespec = me.previousElementSibling;
        }
    }
    if (isBlank(nodespec)) nodespec = nodeid;
    var node = toEle(nodespec || nodeid);
    if (isBlank(node)) node = document.getElementById(nodespec || nodeid);
    if (isBlank(node)) {
        var me = document.currentScript;
        nodespec = me.previousElementSibling;
    }
    if (isBlank(nodeid) &&
        !isBlank(node)) nodeid = node.id;
    if (isBlank(h) && !isBlank(node)) {
        h = node.height;
        if (isBlank(h)) h = node.clientHeight;
    }
    if (isBlank(w) && !isBlank(node)) {
        w = node.width;
        if (isBlank(w)) w = node.clientWidth;
    }
    var waz = window.top.milledImages[nodeid];
    if (!isBlank(waz)) {
        return replaceNode(replace, node, nodeid, waz, h, w);
    }
    if (isBlank(waz) || useCache == false) {
		return htmlToImage.
            //domtoimage.       
		toJpeg(node, {
            quality: 1.0
        }).then(function(dataUrl) {
			waz = dataUrl;
            if(false) if (!isBlank(nodeid) && !isBlank(dataUrl)) { window.top.milledImages[nodeid] = dataUrl; }
            return replaceNode(replace, node, nodeid, dataUrl, h, w);
        }).catch((error) => {
            console.error(`intoNamedImg(${pp(top.milledImages.count)},
               ${pp(top.milledImages)},
               ${pp(node)},${pp(nodeid)}`, error);
            //   debugger;
			return null;
        });
    }
}


function replaceNode(replace, node, nodeid, dataUrl, h, w) {
    if (isBlank(dataUrl)) {
        console.warn(`replaceNode(${pp(top.milledImages.count)},
        ${pp(top.milledImages)},
        ${pp(img)},${pp(node)},
                   ${pp(nodeid)}, ${pp(h)}, ${pp(w)});`);
        return null;
    }
    var img = new Image();
    img.src = dataUrl;

    if (false) downloadImage(nodeid, dataUrl);
    if (true) {
        if (!isBlank(w)) {
            img.style.width = w + "px";
            img.setAttribute("width", w);
        }
        if (!isBlank(h)) {
            img.style.height = h + "px";
            img.setAttribute("height", h);
        }
    }
    if (node != null) {
		copyStylesInline(img,node);
        img.title = node.title;
        img.name = node.name;
        if (replace) img.id = nodeid || node.id;
        img.onclick = node.onclick;
        if (replace) node.replaceWith(img);
    }
    if (replace) img.id = nodeid;
    return img;
}

function copyStylesInline(destinationNode, sourceNode) {
   var containerElements = ["svg","g"];
   for (var cd = 0; cd < destinationNode.childNodes.length; cd++) {
       var child = destinationNode.childNodes[cd];
       if (containerElements.indexOf(child.tagName) != -1) {
            copyStylesInline(child, sourceNode.childNodes[cd]);
            continue;
       }
       var style = sourceNode.childNodes[cd].currentStyle || window.getComputedStyle(sourceNode.childNodes[cd]);
       if (style == "undefined" || style == null) continue;
       for (var st = 0; st < style.length; st++){
            child.style.setProperty(style[st], style.getPropertyValue(style[st]));
       }
   }
}

function alreadyRan(currentScript,prev) {
    var tagName = "MISSING";
	if (prev != null) tagName = prev.tagName;
    if (tagName !== 'TABLE') {
        currentScript.remove();
    } else {
       /* var ss = "<!-- " +
            tagName + " " + currentScript.outerHTML + " -->";
        currentScript.replaceWith($(ss)[0]);*/
    }
}




function downloadImage(name, dataUrl) {
    var link = document.createElement('a');
    link.download = name + '.png';
    link.href = dataUrl;
    link.click();
}

/*
const waitForImage = imgElem => new Promise(resolve => imgElem.complete ? resolve() : imgElem.onload = imgElem.onerror = resolve);

const svgToImgDownload = ext => {
  if (!['png', 'jpg', 'webp'].includes(ext))
    return;
  const _svg = document.querySelector("#svg_container").querySelector('svg');
  const xmlSerializer = new XMLSerializer();
  let _svgStr = xmlSerializer.serializeToString(_svg);
  const img = document.createElement('img');
  img.src = 'data:image/svg+xml;base64,' + window.btoa(_svgStr);
  waitForImage(img)
    .then(_ => {
      const canvas = document.createElement('canvas');
      canvas.width = _svg.clientWidth;
      canvas.height = _svg.clientHeight;
      canvas.getContext('2d').drawImage(img, 0, 0, _svg.clientWidth, _svg.clientHeight);
      return canvas.toDataURL('image/' + (ext == 'jpg' ? 'jpeg' : ext), 1.0);
    })
    .then(dataURL => {
      console.log(dataURL);
      document.querySelector("#img_download_btn").innerHTML = `<a href="${dataURL}" download="img.${ext}">Download</a>`;
    })
    .catch(console.error);
};

document.querySelector('#map2Png').addEventListener('click', _ => svgToImgDownload('png'));
document.querySelector('#map2Jpg').addEventListener('click', _ => svgToImgDownload('jpg'));
document.querySelector('#map2Webp').addEventListener('click', _ => svgToImgDownload('webp'));*/

/* window.addEventListener("click", (e) => {
	   e = e || window.event;
	   console.log(e.target);  // to get the element
	   console.log(e.target.tagName);  // to get the element tag name alone
	   console.log(e.target.id);  // to get the element tag name alone
	   var target = e.target|| e.srcElement;
	   while(target.id=="") {
		 if (target.onclick != undefined) {
		   target.click();
		 }
		 if(target.classList.contains("accordian")) {
		   clickAccordian(target);
		 }
		 target = target.parentElement;
	   }
	   console.log(target);  // to get the element
	   console.log(target.tagName);  // to get the element tag name alone
	   if (target.tagName!="TABLE") {
		e.stopPropagation();
	   }
	   console.log(target.id);  // to get the element tag name alone
	 });   */



function interceptClickEvent(e) {
    var href;
    var target = e.target || e.srcElement;
    if (target.tagName !== 'A') {
        var PE = target.parentElement;
        if (PE != null && PE.tagName === 'A') {
            target = PE;
        }
    }
    if (target.tagName === 'A') {
        href = target.getAttribute('href');
        if (href != null) {

            if (href.startsWith("?")) {
                e.preventDefault();
                if (href.includes("icmd=")) {
                    add_delayed_accordian("" + href, target.textContent, href);
                    return;
                }
                navToParams(href);
                return;
            } else if (href.includes("webcmd=")) {
                var i = href.indexOf('?');
                if (i > 0) {
                    href = href.substring(i);
                    e.preventDefault();
                    navToParams(href);
                    return;
                }
            }
        }
    }

    maybeShowTips(target);
    var selectable = findParentWithClass(target, "selectable");
    if (selectable != null && selectable != target) {
        maybeShowTips(selectable);
        target = selectable;
    }
    //if (target.tagName == 'DIV') {
    //    top.setScollers(target);
    //}
    //if (panel == null) panel = findParentWithClass(target, "dtopscroller2");
    var dtopscroller2 = findParentWithClass(target, "dtopscroller2");
    if (dtopscroller2 !== null) {
        top.setScollers(dtopscroller2);
        console.log(`top.setScollers(${pp(dtopscroller2)});`)
    }
    console.log(`interceptClickEvent('${pp(target)}',${pp(e)})`);
    return;
}

function pp(term) {
    if (term != null) {
        if (typeof term === "object") {
            return `<${term.tagName} id='${term.id}' class="${term.classList}" ... >`
        }
    }
    return "" + term;
}

function maybeShowTips(target) {
    var TE = target;
    while (TE != null && ignoredData(TE.getAttribute('title'))) {
        TE = TE.parentElement;
    }
    if (TE != null) {
        var title = TE.getAttribute('title');
        console.log(`writeTips('${TE}',${title})`);
        if (!ignoredData(title)) {
            writeTips(title);
        }
    }
}

function findParentWithClass(target, clasz) {
    var TE = target;
    while (TE != null) {
        if (TE.classList.contains(clasz)) return TE;
        TE = TE.parentElement;
    }
    //if (TE == null) return target;
    return null;
}


function mouseOverEvent(e) {

    var target = e.target || e.srcElement;
    if (false) {
        $(target).children(".description").show();
        $(target).mouseout(
            function() { $(this).children(".description").hide(); }
        );
    }

    var panel = findParentWithClass(target, "panel");
    if (panel != null) {
        activateMenu(panel.id);
        //console.log("mouseOverPanel(" + panel.id + "," + panel.tagName + ")");
    }

    var selectable = findParentWithClass(target, "selectable");
    if (selectable != null) {
        //activateMenu(selectable.id);
        console.log(`mouseOverSelectable(${pp(selectable)})`);
    }

}

function hideDesc(target) {
    $(target).children(".description").hide();
}

function showDesc(target) {
    maybeShowTips(target);
}

//var top.seInd;

{
    top.sortAscnd = false;
    top.seInd = null;
}

function sortDesc(a, b) {
    return a - b;
}

function sortAsc(a, b) {
    return b - a;
}

function sortMatrixDesc(a, b) {
    if (a[top.seInd] === b[top.seInd]) {
        return 0;
    } else {
        return (a[top.seInd] < b[top.seInd]) ? -1 : 1;
    }
}

function sortMatrixAsc(a, b) {
    if (a[top.seInd] === b[top.seInd]) {
        return 0;
    } else {
        return (a[top.seInd] > b[top.seInd]) ? -1 : 1;
    }
}

function getTblCont(tb) {
    var cols = [];
    $("tr:first td", tb).each(function(i, el) {
        cols[i] = [];
    });
    for (var c = 0; c < cols.length; c++) {
        $("tr", tb).each(function(i, el) {
            cols[c].push(parseInt($("td", el).eq(c).text().trim()));
        });
    }
    return cols;
}

function sortRow(rObj, desc) {
    var tblObj = getTblCont(rObj.parent());
    var rowInd = rObj.index();
    return sortTableRowIndex(tblObj, rowInd, desc);
}

function sortTableRowIndex(tableSpec, rowInd, desc) {
    var tblObj = upToTable(tableSpec);
    console.log("Pre-Sort", tblObj, top.seInd);
    if (desc == undefined || desc == true) {
        tblObj.sort(sortMatrixDesc);
    } else {
        tblObj.sort(sortMatrixAsc);
    }
    top.sortAscnd = (!(desc));
    console.log("Post Sort", tblObj, top.seInd);
    rObj.parent().find("tr").each(function(r, tr) {
        $("td", tr).each(function(i, el) {
            $(el).html(tblObj[i][r]);
        });
    });
}

function sortTableCols(tableSpec, rowIndex) {

    var tableE = upToTable(tableSpec);
    if (false) $(tableE).DataTable();
    if (false) {
        return sortTableRowIndex(tableE, rowIndex, top.sortAscnd);
    }
    return sortTableColsCur(tableE, rowIndex);
}

function sortTableColsCur(tableSpec, rowIndex) {

    var tableE = upToTable(tableSpec);
    if (tableE == null) {
        return;
    }

    let table = $(tableE);
    let tr = table.find('tr');
    let selectedRow = $(tr[rowIndex]);
    let selectedRowTd = selectedRow.find('td');
    let selectedRowSorting = [];

    // find and get clicked tr and it formats it in index and value of the cells
    selectedRowTd.each(function(tdIndex) {
        let td = $(selectedRowTd[tdIndex]);
        selectedRowSorting.push({
            tdIndex: tdIndex,
            value: parseInt(Math.ceil(td.text().trim()))
        })
    })

    // it will compare values and sort
    selectedRowSorting = selectedRowSorting.sort(function(a, b) {

        if (a.value == 0) {
            return -1;
        }

        if (b.value == 0) {
            return -1;
        }

        return b.value - a.value
    });

    console.log(selectedRowSorting)

    // it will only return indexs of sorted list of cells
    var sortedIndexs = selectedRowSorting.map(function(rowSorting) {
        return rowSorting.tdIndex
    })

    console.log(sortedIndexs)
    table.find('tr').each(function() {
        let tr = $(this);
        let modifiedTr = [];

        tr.children().each(function(tdIndex, td) {

            if (tdIndex == 0) {
                console.log(td)
                modifiedTr[0] = td;

            } else {
                for (let i = 0; i < sortedIndexs.length; i++) {
                    console.log(sortedIndexs[i])
                        // it gives me index of sorted column.
                    if (tdIndex == i) {
                        let sortedIndex = sortedIndexs[i];

                        if (sortedIndex == undefined) {
                            console.log('i', i, sortedIndex)
                            sortedIndex = sortedIndexs.length + 1
                        }

                        modifiedTr[sortedIndex] = td;
                    }
                }
            }

        })

        tr.append(modifiedTr)
    })
}

function toNumStr(x) {
    var x1;
    if (isElement(x)) {
        x1 = x.innerText;
    } else {
        x1 = "" + x;
    }
    var x2 = x1.toLowerCase().trim();
    var number = parseFloat(x2);
    if (!isNaN(number)) {
        number = number * 100;
        number = Math.round(number);
        let padToFour = number => number <= 99999999 ? `0000000${number}`.slice(-8) : number;
        return padToFour;
    }
    return x2;
}

function isGreater(y1, x1) {
    if (x1 === y1)
        return 0;
    var x = toNumStr(x1);
    var y = toNumStr(y1);
    if (x === y)
        return 0;
    if (x < y)
        return 1;
    return -1;
}

function upToTable(table) {
    if (typeof table === 'object') {
        while (table != null && (table.tagName !== 'TABLE' || !table.classList.contains("sortable"))) {
            table = table.parentElement;
        }
    }
    return table;
}

function sortTableColz(tableSpec, n) {
    var cols, switching, i, x, y, shouldSwitch, dir, switchcount = 0;

    var table = upToTable(tableSpec);

    if (table == null) {
        return;
    }

    switching = true;
    //Set the sorting direction to ascending:
    dir = "asc";
    /*Make a loop that will continue until
	  no switching has been done:*/
    while (switching) {
        //start by saying: no switching is done:
        switching = false;
        cols = table.cols;
        /*Loop through all table columns (except the
		first, which contains table headers):*/
        for (i = 1; i < (cols.length - 1); i++) {
            //start by saying there should be no switching:
            shouldSwitch = false;
            /*Get the two elements you want to compare,
		  one from current col and one from the next:*/
            x = cols[i].getElementsByTagName("TD")[n];
            y = cols[i + 1].getElementsByTagName("TD")[n];
            /*check if the two columns should switch place,
		  based on the direction, asc or desc:*/
            if (dir == "asc") {
                if (isGreater(x, y) > 0) {
                    //if so, mark as a switch and break the loop:
                    shouldSwitch = true;
                    break;
                }
            } else if (dir == "desc") {
                if (isGreater(y, x) > 0) {
                    //if so, mark as a switch and break the loop:
                    shouldSwitch = true;
                    break;
                }
            }
        }
        if (shouldSwitch) {
            /*If a switch has been marked, make the switch
		  and mark that a switch has been done:*/
            cols[i].parentNode.insertBefore(cols[i + 1], cols[i]);
            switching = true;
            //Each time a switch is done, increase this count by 1:
            switchcount++;
        } else {
            /*If no switching has been done AND the direction is "asc",
		  set the direction to "desc" and run the while loop again.*/
            if (switchcount == 0 && dir == "asc") {
                dir = "desc";
                switching = true;
            }
        }
    }
}



function sortTableRows(tableSpec, n) {
    var rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;

    var table = upToTable(tableSpec);

    if (table == null) {
        return;
    }

    switching = true;
    //Set the sorting direction to ascending:
    dir = "asc";
    /*Make a loop that will continue until
	  no switching has been done:*/
    while (switching) {
        //start by saying: no switching is done:
        switching = false;
        rows = table.rows;
        /*Loop through all table rows (except the
		first, which contains table headers):*/
        for (i = 1; i < (rows.length - 1); i++) {
            //start by saying there should be no switching:
            shouldSwitch = false;
            /*Get the two elements you want to compare,
		  one from current row and one from the next:*/
            x = rows[i].getElementsByTagName("TD")[n];
            y = rows[i + 1].getElementsByTagName("TD")[n];
            /*check if the two rows should switch place,
		  based on the direction, asc or desc:*/
            if (dir == "asc") {
                if (isGreater(x, y) > 0) {
                    //if so, mark as a switch and break the loop:
                    shouldSwitch = true;
                    break;
                }
            } else if (dir == "desc") {
                if (isGreater(y, x) > 0) {
                    //if so, mark as a switch and break the loop:
                    shouldSwitch = true;
                    break;
                }
            }
        }
        if (shouldSwitch) {
            /*If a switch has been marked, make the switch
		  and mark that a switch has been done:*/
            rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);
            switching = true;
            //Each time a switch is done, increase this count by 1:
            switchcount++;
        } else {
            /*If no switching has been done AND the direction is "asc",
		  set the direction to "desc" and run the while loop again.*/
            if (switchcount == 0 && dir == "asc") {
                dir = "desc";
                switching = true;
            }
        }
    }
}



function intoDataTable(target) {
    $(target).DataTable();
}


function windowCreation(id) {
    var isOut;
    document.getElementById("closeButton" + id).onclick = function() {
        fadeOut(document.getElementById("mydiv" + id), 50);
        isOut = true;
    };
    document.getElementById("button" + id).onclick = function() {
        if (document.getElementById("mydiv" + id).style.display === "initial") {
            isOut = false;
        }
        if (isOut) {
            document.getElementById("mydiv" + id).style = "position: absolute;";
            document.getElementById("mydiv" + id).style = "top: 80px;";
            fadeIn(document.getElementById("mydiv" + id), 50);
        }
        isOut = false;
    };
    dragElement(document.getElementById("mydiv" + id));
    isOut = true;
}

function dragElement(elmnt) {
    var pos1 = 0,
        pos2 = 0,
        pos3 = 0,
        pos4 = 0;
    if (document.getElementById(elmnt.id + "header")) {
        document.getElementById(elmnt.id + "header").onmousedown = dragMouseDown;
    } else {
        elmnt.onmousedown = dragMouseDown;
    }

    function dragMouseDown(e) {
        e = e || window.event;
        e.preventDefault();
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        document.onmousemove = elementDrag;
        var active = document.getElementsByClassName("mydiv");
        for (var i = active.length - 1; i > -1; i--) {
            active[i].classList.remove("mydivActive");
        }
        document.getElementById(elmnt.id).className += " mydivActive";
    }

    function elementDrag(e) {
        e = e || window.event;
        e.preventDefault();
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
    }

    function closeDragElement() {
        document.onmouseup = null;
        document.onmousemove = null;
    }
}

function fadeIn(elem, ms) {
    elem.style.opacity = 0;
    elem.style.filter = "alpha(opacity=0)";
    elem.style.display = "inline-block";
    elem.style.visibility = "visible";

    if (ms) {
        var opacity = 0;
        var timer = setInterval(function() {
            opacity += 50 / ms;
            if (opacity >= 1) {
                clearInterval(timer);
                opacity = 0.9;
            }
            elem.style.opacity = opacity;
            elem.style.filter = "alpha(opacity=" + opacity * 100 + ")";
            var active = document.getElementsByClassName("mydiv");
            for (var i = active.length - 1; i > -1; i--) {
                active[i].classList.remove("mydivActive");
            }
            elem.className += " mydivActive";
        }, 50);
    } else {
        elem.style.opacity = 1;
        elem.style.filter = "alpha(opacity=1)";
    }
}

function fadeOut(elem, ms) {
    if (ms) {
        var opacity = 1;
        var timer = setInterval(function() {
            opacity -= 50 / ms;
            if (opacity <= 0) {
                clearInterval(timer);
                opacity = 0;
                elem.style.display = "none";
                elem.style.visibility = "hidden";
            }
            elem.style.opacity = opacity;
            elem.style.filter = "alpha(opacity=" + opacity * 100 + ")";
        }, 50);
    } else {
        elem.style.opacity = 0;
        elem.style.filter = "alpha(opacity=0)";
        elem.style.display = "none";
        elem.style.visibility = "hidden";
    }
}

function scroll_top_and_bottom(ele) {
    if (true) return;
    if (typeof ele === string) {
        if (ele.startsWith(".")) {
            var className = ele.substring(1);
            const resizers = document.getElementsByClassName(clazzname)
            for (let i = 0; i < resizers.length; i++) {
                const currentResizer = resizers[i];
                scroll_top_and_bottom(currentResizer);
            }
            return;
        }

        var node = toEle(ele);
        if (node != null) {
            scroll_top_and_bottom(node);
            return;
        }
        return;
    }

    if (!ele.parentElement.classList.contains("dtopscroller2")) {
        NEW = $(`<div><div class="wtopscroller1"><div class="dtopscroller1"></div></div>
			   <div class="wtopscroller2"><div class="dtopscroller2"><div id="rm1"></div></div></div>`);
        ele.replaceWith(NEW);
        NEW.$("#rm1").replaceWith(ele);
    }
}


/*Make resizable div by Hung Nguyen*/
function makeResizableDiv(clazzname) {
    if (true) return;
    if (clazzname.startsWith(".")) className = clazzname.substring(1);
    const resizers = document.getElementsByClassName(clazzname)
    for (let i = 0; i < resizers.length; i++) {
        const currentResizer = resizers[i];
        makeResizable1Div(currentResizer);
    }
}


function makeResizable1Div(elin) {
    if (true) return;
    const element = $(elin);
    const resizers = element.querySelectorAll('.resizer');
    const minimum_size = 20;
    let original_width = 0;
    let original_height = 0;
    let original_x = 0;
    let original_y = 0;
    let original_mouse_x = 0;
    let original_mouse_y = 0;
    for (let i = 0; i < resizers.length; i++) {
        const currentResizer = resizers[i];
        currentResizer.addEventListener('mousedown', function(e) {
            e.preventDefault()
            original_width = parseFloat(getComputedStyle(element, null).getPropertyValue('width').replace('px', ''));
            original_height = parseFloat(getComputedStyle(element, null).getPropertyValue('height').replace('px', ''));
            original_x = element.getBoundingClientRect().left;
            original_y = element.getBoundingClientRect().top;
            original_mouse_x = e.pageX;
            original_mouse_y = e.pageY;
            window.addEventListener('mousemove', resize)
            window.addEventListener('mouseup', stopResize)
        })

        function resize(e) {
            if (currentResizer.classList.contains('bottom-right')) {
                const width = original_width + (e.pageX - original_mouse_x);
                const height = original_height + (e.pageY - original_mouse_y)
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                }
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                }
            } else if (currentResizer.classList.contains('bottom-left')) {
                const height = original_height + (e.pageY - original_mouse_y)
                const width = original_width - (e.pageX - original_mouse_x)
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                }
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                    element.style.left = original_x + (e.pageX - original_mouse_x) + 'px'
                }
            } else if (currentResizer.classList.contains('top-right')) {
                const width = original_width + (e.pageX - original_mouse_x)
                const height = original_height - (e.pageY - original_mouse_y)
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                }
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                    element.style.top = original_y + (e.pageY - original_mouse_y) + 'px'
                }
            } else {
                const width = original_width - (e.pageX - original_mouse_x)
                const height = original_height - (e.pageY - original_mouse_y)
                if (width > minimum_size) {
                    element.style.width = width + 'px'
                    element.style.left = original_x + (e.pageX - original_mouse_x) + 'px'
                }
                if (height > minimum_size) {
                    element.style.height = height + 'px'
                    element.style.top = original_y + (e.pageY - original_mouse_y) + 'px'
                }
            }
        }

        function stopResize() {
            window.removeEventListener('mousemove', resize)
        }
    }
}
//initilize svg or grab svg

// const { boolean } = require("webidl-conversions");


function testRunD3Sim(htmlEle) {
    var nodes = [
        { color: "red", size: 5 },
        { color: "orange", size: 10 },
        { color: "yellow", size: 15 },
        { color: "green", size: 20 },
        { color: "blue", size: 25 },
        { color: "purple", size: 30 }
    ];

    var links = [
        { source: "red", target: "orange" },
        { source: "orange", target: "yellow" },
        { source: "yellow", target: "green" },
        { source: "green", target: "blue" },
        { source: "blue", target: "purple" },
        { source: "purple", target: "red" },
        { source: "green", target: "red" }
    ];
    runD3Sim(htmlEle, nodes, links);
}

function tableToGraph(table) {

    var IvO = table.row[0].getElementsByTagName('TH')[0].innerText;
    var iioroo = IvO.slice(-1) === IvO.charAt(0);

    function adduniquenodes(nodes, value) {
        if (nodes.indexOf(value) === -1) {
            nodes.push(value);
        }
    }

    graph = { "nodes": [], "links": [] };

    /*Loop through all table rows (except the first, which contains table headers):*/
    for (i = 1; i < (table.columns.length - 1); i++) {
        var d = table.row[0].getElementsByTagName('TH')[i];
        adduniquenodes(graph.nodes, {
            "object": d.id,
            "image-src": imageFromID(d.id),
        });
    }
    if (!iioroo) {
        for (i = 1; i < (table.rows.length - 1); i++) {
            d = table.row[i].getElementsByTagName('TH')[0];
            adduniquenodes(graph.nodes, {
                "object": d.id,
                "image-src": imageFromID(d.id),
            });
        }
    }

    for (i = 1; i < (table.row.length - 1); i++) {
        for (j = 1; i < (table.col.length - 1); j++) {
            var to = table.row[0].getElementsByTagName('TH')[j]
            var from = table.row[j].getElementsByTagName('TH')[0]
            var str = table.row[i].getElementsByTagName('TD')[j];
            adduniquenodes(graph.links, {
                "source": from.id,
                "target": to.id,
                "value": str
            });
        }
    }
    console.log(graph);
    return graph;
}

function onBeforeUnload(leavingTo) {
    return;
}

function runD3Sim(htmlEleSpec, nodes, links) {
    var htmlEle = toEle(htmlEleSpec);
    var width = htmlEle.clientWidth; //svg.attr("width");
    var height = htmlEle.clientHeight; //svg.attr("height");

    if (height < 300) {
        height = 300;
    }
    var svgEle;
    if (htmlEle.tagName == 'SVG') {
        svgEle = htmlEle;
    } else {
        let svgml = `<svg width="${width}" height="${height}" style="background-color: #dfd; min-height: 200"></svg>`;
        htmlEle.insertAdjacentHTML('afterbegin', svgml);
        svgEle = htmlEle.firstElementChild;
    }

    var margin = { top: 20, right: 10, bottom: 20, left: 10 };

    var svg = d3
        .select(svgEle)
        .attr("width", width)
        .attr("height", height)

    svg.append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var linkSelection = svg
        .selectAll("line")
        .data(links)
        .enter()
        .append("line")
        .attr("stroke", "black")
        .attr("stroke-width", 2);

    var imgurl = "http://wallpapers.androlib.com/wallicons/wallpaper.big-pqC.cs.png"

    var defs = svg.append("defs").attr("id", "imgdefs")

    var catpattern = defs.append("pattern")
        .attr("id", "catpattern")
        .attr("height", 1)
        .attr("width", 1)
        .attr("x", "0")
        .attr("y", "0")

    svg.append("circle")
        .attr("r", 100)
        .attr("cy", 80)
        .attr("cx", 120)
        .attr("fill", "url(#catpattern)");

    var nodeSelection = svg
        .selectAll("circle")
        .data(nodes)
        .enter()
        //.attr("style", "color: white")
        .append("circle")
        .attr("r", d => d.size)
        .attr("fill", d => d.oid)
        .call(
            d3.drag()
            .on("start", dragStart)
            .on("drag", drag)
            .on("end", dragEnd)
        );

    var simulation = d3.forceSimulation(nodes);

    simulation
        .force("center", d3.forceCenter(width / 2, height / 2))
        .force("nodes", d3.forceManyBody())
        .force(
            "links",
            d3
            .forceLink(links)
            .id(d => d.oid)
            .distance(d => 3 * (100 - d.value))
        )
        .on("tick", ticked);

    function ticked() {
        // console.log(simulation.alpha());

        nodeSelection.attr("cx", d => d.x).attr("cy", d => d.y);

        linkSelection
            .attr("x1", d => d.source.x)
            .attr("y1", d => d.source.y)
            .attr("x2", d => d.target.x)
            .attr("y2", d => d.target.y);
    }

    function tick(node, path) {
        path.attr("d", function(d) {

            var dx = d.target.x - d.source.x,
                dy = d.target.y - d.source.y,
                dr = Math.sqrt(dx * dx + dy * dy);
            return "M" + d.source.x + "," +
                d.source.y +
                "A" + dr + "," +
                dr + " 0 0,1 " +
                d.target.x + "," +
                d.target.y;
        });
        node.attr("transform", nodeTransform);
    }


    /**
     * Gives the coordinates of the border for keeping the nodes inside a frame
     * http://bl.ocks.org/mbostock/1129492
     */
    function nodeTransform(d) {
        d.x = Math.max(maxNodeSize, Math.min(w - (d.imgwidth / 2 || 16), d.x));
        d.y = Math.max(maxNodeSize, Math.min(h - (d.imgheight / 2 || 16), d.y));
        return "translate(" + d.x + "," + d.y + ")";
    }

    function dragStart(d) {
        // console.log('drag start');
        simulation.alphaTarget(0.5).restart();
        d.fx = d.x;
        d.fy = d.y;
    }

    function drag(d) {
        // console.log('dragging');
        // simulation.alpha(0.5).restart()
        d.fx = d3.event.x;
        d.fy = d3.event.y;
    }

    function dragEnd(d) {
        // console.log('drag end');
        simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;

    }
}

function makeMarval() {
    var marvelJson = {
        "name": "marvel",
        "img": "https://dl.dropboxusercontent.com/u/19954023/marvel_force_chart_img/marvel.png",
        "children": [{
                "name": "Heroes",
                "children": [{
                        "oid": "Spider-Man",
                        "name": "Peter Benjamin Parker",
                        "link": "http://marvel.com/characters/54/spider-man",
                        "img": "https://dl.dropboxusercontent.com/u/19954023/marvel_force_chart_img/top_spiderman.png",
                        "size": 40000
                    },
                    {
                        "oid": "CAPTAIN MARVEL",
                        "name": "Carol Danvers",
                        "link": "http://marvel.com/characters/9/captain_marvel",
                        "img": "https://dl.dropboxusercontent.com/u/19954023/marvel_force_chart_img/top_captainmarvel.png",
                        "size": 40000
                    }
                ]
            },
            {
                "name": "Villains",
                "children": [{
                        "oid": "Dr. Doom",
                        "name": "Victor von Doom",
                        "link": "http://marvel.com/characters/13/dr_doom",
                        "img": "https://dl.dropboxusercontent.com/u/19954023/marvel_force_chart_img/drdoom.png",
                        "size": 40000
                    },
                    {
                        "oid": "Mystique",
                        "name": "Unrevealed",
                        "link": "http://marvel.com/characters/1552/mystique",
                        "img": "https://dl.dropboxusercontent.com/u/19954023/marvel_force_chart_img/mystique.png",
                        "size": 40000
                    }
                ]
            },
            {
                "name": "Teams",
                "children": [{
                        "oid": "Avengers",
                        "name": "",
                        "link": "http://marvel.com/characters/68/avengers",
                        "img": "https://dl.dropboxusercontent.com/u/19954023/marvel_force_chart_img/avengers.png",
                        "size": 40000
                    },
                    {
                        "oid": "Guardians of the Galaxy",
                        "name": "",
                        "link": "http://marvel.com/characters/70/guardians_of_the_galaxy",
                        "img": "https://dl.dropboxusercontent.com/u/19954023/marvel_force_chart_img/gofgalaxy.png",
                        "size": 40000
                    }
                ]
            }
        ]
    }

    /*
                var imgurl = "http://wallpapers.androlib.com/wallicons/wallpaper.big-pqC.cs.png"
    
                var margin = {top: 20, right: 10, bottom: 20, left: 10};
    
    
                var width = 960 - margin.left - margin.right,
                    height = 500 - margin.top - margin.bottom;
    
    
                var svg = d3.select("body").append("svg")
                    .attr("width", width + margin.left + margin.right)
                    .attr("height", height + margin.top + margin.bottom)
                  .append("g")
                    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
    
                var defs = svg.append("defs").attr("id", "imgdefs")
    
                var catpattern = defs.append("pattern")
                                        .attr("id", "catpattern")
                                        .attr("height", 1)
                                        .attr("width", 1)
                                        .attr("x", "0")
                                        .attr("y", "0")
    
    
                catpattern.append("image")
                     .attr("x", -130)
                     .attr("y", -220)
                     .attr("height", 640)
                     .attr("width", 480)
                     .attr("xlink:href", imgurl)
    
                svg.append("circle")
                    .attr("r", 100)
                    .attr("cy", 80)
                    .attr("cx", 120)
                    .attr("fill", "url(#catpattern)")
                    */


    // some colour variables
    var tcBlack = "#130C0E";

    // rest of vars
    var w = 960,
        h = 800,
        maxNodeSize = 50,
        x_browser = 20,
        y_browser = 25,
        root;

    var vis;
    var force = d3.layout.force();

    vis = d3.select("#vis").append("svg").attr("width", w).attr("height", h);

    //d3.json("marvel.json", function(json) {
    var json = marvelJson;

    root = json;
    root.fixed = true;
    root.x = w / 2;
    root.y = h / 4;


    // Build the path
    var defs = vis.insert("svg:defs")
        .data(["end"]);


    defs.enter().append("svg:path")
        .attr("d", "M0,-5L10,0L0,5");
    try {
        update();
    } catch (e) {
        console.log(e);
    }
    //});


    /**
     *   
     */
    function update() {
        var nodes = flatten(root),
            links = d3.layout.tree().links(nodes);

        // Restart the force layout.
        force.nodes(nodes)
            .links(links)
            .gravity(0.05)
            .charge(-1500)
            .linkDistance(100)
            .friction(0.5)
            .linkStrength(function(l, i) { return 1; })
            .size([w, h])
            .on("tick", tick)
            .start();

        var path = vis.selectAll("path.link")
            .data(links, function(d) { return d.target.id; });

        path.enter().insert("svg:path")
            .attr("class", "link")
            // .attr("marker-end", "url(#end)")
            .style("stroke", "#eee");


        // Exit any old paths.
        path.exit().remove();



        // Update the nodesÃ¯Â¿Â½
        var node = vis.selectAll("g.node")
            .data(nodes, function(d) { return d.id; });


        // Enter any new nodes.
        var nodeEnter = node.enter().append("svg:g")
            .attr("class", "node")
            .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; })
            .on("click", click)
            .call(force.drag);


        /*Dinesh Code*/
        /*
        
        var defs = svg.append("defs").attr("id", "imgdefs")
       
       var catpattern = defs.append("pattern")
                               .attr("id", "catpattern")
                               .attr("height", 1)
                               .attr("width", 1)
                               .attr("x", "0")
                               .attr("y", "0")
       
       
       catpattern.append("image")
            .attr("x", -130)
            .attr("y", -220)
            .attr("height", 640)
            .attr("width", 480)
            .attr("xlink:href", imgurl)
       
       svg.append("circle")
           .attr("r", 100)
           .attr("cy", 80)
           .attr("cx", 120)
           .attr("fill", "url(#catpattern)")
           */

        /******* Dinesh Code End******/

        nodeEnter.append("defs").attr("id", "imgdefs")
            .append("pattern")
            .attr("id", function(d) {
                if (d.img) return d.img.replace(/[/|.|:]/g, "");
                else null;
            })
            .attr("height", 1)
            .attr("width", 1)
            .attr("x", "0")
            .attr("y", "0")
            .append("image")
            .attr("height", function(d) { return 2 * Math.sqrt(d.size) / 10 || 4.5; })
            .attr("width", function(d) { return 2 * Math.sqrt(d.size) / 10 || 4.5; })
            .attr("xlink:href", function(d) {
                if (d.img) return d.img + "";
                else null;
            })

        // Append a circle
        var images = nodeEnter.append("svg:circle")
            .attr("r", function(d) { return Math.sqrt(d.size) / 10 || 4.5; })
            .attr("fill", function(d) {
                if (d.img) return "url(#" + d.img.replace(/[/|.|:]/g, "") + ")";
                else null;
            });


        // Append images
        /* var images = nodeEnter.append("svg:image")
               .attr("xlink:href",  function(d) { return d.img;})
               .attr("x", function(d) { return -25;})
               .attr("y", function(d) { return -25;})
               .attr("height", 50)
               .attr("width", 50);*/

        // make the image grow a little on mouse over and add the text details on click
        var setEvents = images
            // Append oid text
            .on('click', function(d) {
                d3.select("h1").html(d.oid);
                d3.select("h2").html(d.name);
                d3.select("h3").html("Take me to " + "<a href='" + d.link + "' >" + d.oid + " web page ?" + "</a>");
            })

        .on('mouseenter', function(d) {
                if (d.img) {
                    //console.log(d3.select(this).parent);
                    d3.select("pattern#" + d.img.replace(/[/|.|:]/g, "")).select("image").attr("width", "100").attr("height", "100");
                    // select element in current context
                    d3.select(this)
                        .transition()
                        .attr("x", function(d) { return -60; })
                        .attr("y", function(d) { return -60; })
                        .attr("r", "50")
                        //.attr("height", 100)
                        //.attr("width", 100);
                }
            })
            // set back
            .on('mouseleave', function(d) {
                if (d.img) {
                    d3.select("pattern#" + d.img.replace(/[/|.|:]/g, "")).select("image").attr("width", "40").attr("height", "40");

                    d3.select(this)
                        .transition()
                        .attr("x", function(d) { return -25; })
                        .attr("y", function(d) { return -25; })
                        .attr("r", "20")
                        //.attr("height", 50)
                        //.attr("width", 50);
                }
            });

        // Append oid name on roll over next to the node as well
        nodeEnter.append("text")
            .attr("class", "nodetext")
            .attr("x", x_browser)
            .attr("y", y_browser + 15)
            .attr("fill", tcBlack)
            .text(function(d) { return d.oid; });


        // Exit any old nodes.
        node.exit().remove();


        // Re-select for update.
        path = vis.selectAll("path.link");
        node = vis.selectAll("g.node");

        function tick() {


            path.attr("d", function(d) {

                var dx = d.target.x - d.source.x,
                    dy = d.target.y - d.source.y,
                    dr = Math.sqrt(dx * dx + dy * dy);
                return "M" + d.source.x + "," +
                    d.source.y +
                    "A" + dr + "," +
                    dr + " 0 0,1 " +
                    d.target.x + "," +
                    d.target.y;
            });
            node.attr("transform", nodeTransform);
        }
    }


    /**
     * Gives the coordinates of the border for keeping the nodes inside a frame
     * http://bl.ocks.org/mbostock/1129492
     */
    function nodeTransform(d) {
        d.x = Math.max(maxNodeSize, Math.min(w - (d.imgwidth / 2 || 16), d.x));
        d.y = Math.max(maxNodeSize, Math.min(h - (d.imgheight / 2 || 16), d.y));
        return "translate(" + d.x + "," + d.y + ")";
    }

    /**
     * Toggle children on click.
     */
    function click(d) {
        if (d.children) {
            d._children = d.children;
            d.children = null;
        } else {
            d.children = d._children;
            d._children = null;
        }

        update();
    }


    /**
     * Returns a list of all nodes under the root.
     */
    function flatten(root) {
        var nodes = [];
        var i = 0;

        function recurse(node) {
            if (node.children)
                node.children.forEach(recurse);
            if (!node.id)
                node.id = ++i;
            nodes.push(node);
        }

        recurse(root);
        return nodes;
    }


}

function ignoreScollerOrPlane(target) {
    if (!isElement(target)) return true;
    if (target.classList.contains("ignore-scroll")) return true;
    if (target.classList.contains("ignorable")) return true;
    if (target.classList.contains("ignore")) return true;
    if (target.classList.contains("wtopscroller11")) return true;
    if (target.classList.contains("wtopscroller1")) return true;
    return false;
}

function setScollers(target) {
    if (window != top) {
        top.setScollers(target);
        return;
    }
    if (!ignoreScollerOrPlane(target)) {

        console.log(`setScollers(${pp(target)},${e})`);
        var wasT = target;
        $(target).scroll(function() {
            if (!ignoreScollerOrPlane(wasT)) {
                top.divToScroll = wasT;
                var sw = wasT.width;
                var sl = $(this).scrollLeft();
                top.syncScroll(wasT, sl, sw);
            }
        });
    }
}

function syncScroll(target, sl, sw) {
    if (window != top) {
        top.syncScroll(target, sl, sw);
        return;
    }

    if (!top.ticking) {
        window.requestAnimationFrame(() => {
            top.syncScrollReal(target, sl, sw);
            top.ticking = false;
        });
        top.ticking = true;
    }
}


function syncScrollReal(target, sl, sw) {

    if (target == null || target == undefined) {
        target = top.divToScroll;
    }

    if (ignoreScollerOrPlane(target)) {
        target = top.divToScroll;
    } else {
        // top.divToScroll = target;
    }
    if (sw === undefined && isElement(target)) {
        sw = target.width;
    }
    if (sw !== undefined) {
        console.log(`min-width=(${sw}px)`);
        $('#top_scoller > div').css('min-width', sw + 'px');
        ///$('#bottem_scoller > div').css('min-width', sw + 'px');
    }
    if (sl !== undefined) {
        console.log(`syncScroll(${sl},${pp(target)})`);
        $('#top_scoller').scrollLeft(sl);
        //$('#bottem_scoller').scrollLeft(sl);
        if (isElement(target)) {
            $(target.parentElement).scrollLeft(sl);
            $(target).scrollLeft(sl);
        }
    }

}

function add_top_scroller(script) {

    var PEDiv2 = script.parentElement.parentElement;
    var PEDiv1 = PEDiv2.previousElementSibling;

    $(PEDiv2).scroll(function() {
        var sl = $(PEDiv2).scrollLeft();
        check_top_scroller(PEDiv2, sl);
        $(PEDiv1).scrollLeft(sl);
    });

    $(PEDiv1).scroll(function() {
        var sl = $(PEDiv1).scrollLeft();
        check_top_scroller(PEDiv2, sl);
        $(PEDiv2).scrollLeft(sl);
    });
}

function doSomething(e, ignored) {
    var target = e.target || e.srcElement;
    if (!ignoreScollerOrPlane(target)) {
        top.lastScoller = target;
        $("#top_scroller").scrollLeft($(target).scrollLeft());
    }

}


function check_top_scroller(PEDiv2, sl) {
    if (top.lastScoller && sl !== undefined) $(top.lastScoller).scrollLeft(sl);
    var PEDiv1 = PEDiv2.previousElementSibling;
    var paddDiv2 = PEDiv2.firstElementChild;
    var paddDiv1 = PEDiv1.firstElementChild;
    var panel = PEDiv2.parentElement;

    if (panel.classList.contains("dtopscroller1")) {
        PEDiv1 = panel.parentElement;
        PEDiv2 = PEDiv1.nextElementSibling;
        check_top_scroller(PEDiv2, sl);
        return;
    }
    if (panel.classList.contains("dtopscroller2")) {
        PEDiv2 = panel.parentElement;
        check_top_scroller(PEDiv2, sl);
        return;
    }
    if (paddDiv1 !== null) {
        console.log(`check_top_scroller(${pp(PEDiv2)}, ${sl})`);
        $(top.lastScoller).scrollLeft(sl);
        panel.style.minWidth = "fit-content";
        paddDiv2.style.minWidth = "fit-content";
        top.divToScroll = paddDiv2;
        var sw = paddDiv2.width;
        var PEDiv1 = PEDiv2.previousElementSibling;
        if (sl) $(PEDiv1).scrollLeft(sl);
        top.syncScroll(paddDiv2, sl, sw);
        panel.style.width = sw + "px";
        paddDiv1.style.width = sw + "px";
        paddDiv1.style.minWidth = sw + "px";
        PEDiv1.style.scrollY = "auto";
        PEDiv2.style.scrollY = "auto";
        panel.style.scrollY = "auto";
    }

}



function xframeLoading() {
    console.log("XFrame Loading Start: " + window.location.href);
    commonLoading();
}

function topReady() {
    $('#tableMySideNavL').DataTable();
    makeResizableDiv('.resizable');
    for (i = 1; i < 3; i++) {
        try {
            windowCreation(i);
        } catch (err) {}

    }
    $('#top_scoller').scroll(function() {
        var sl = $('#top_scoller').scrollLeft();
        if (top.lastScoller != null) {
            $(top.lastScoller).scrollLeft(sl);
        }
		window.top.$("#lm_xref").contents().find("#main_in_iframe").scrollLeft(sl);
        syncScroll(top.divToScroll, sl, undefined);
    });
    if (false) {
        $('#bottem_scoller').scroll(function() {
            var sl = $('#bottem_scoller').scrollLeft();
            syncScroll(top.divToScroll, sl, undefined);
        });
    }
    commonLoading();
}

{
    top.ticking = false;
    top.lastKnownScrollYPosition = 0;
}


function interceptScrollEvent(event) {
    top.lastKnownScrollYPosition = window.scrollY;
    if (!top.ticking) {
        window.requestAnimationFrame(() => {
            top.doSomething(event, top.lastKnownScrollYPosition);
            top.ticking = false;
        });
        top.ticking = true;
    }
}

function xframeReady() {
    commonLoading();
    if (document.addEventListener && false) {
        document.removeEventListener('scroll', interceptScrollEvent);
        document.addEventListener('scroll', interceptScrollEvent);
    }
	window.top.$('#top_scoller').scroll(function() {
        var sl = window.top.$('#top_scoller').scrollLeft();
		window.top.$("#lm_xref").contents().find("#main_in_iframe").scrollLeft(sl);
    });
    console.log("XFrame Ready End: " + window.location.href);
}

function commonLoading() {
    if (top == window) {
        var navWindow = getNavWindow();
        navWindow.commonLoadingImpl = top.commonLoadingImpl;
        navWindow.commonLoadingImpl();
        commonLoadingImpl();
    } else {
        // debugger;
        top.commonLoadingImpl();
        commonLoadingImpl();
    }
}

function commonLoadingImpl() {
    console.log("commonLoadingImpl: " + window.location.href);
    //listen for link click events at the document level
    if (document.addEventListener) {
        document.removeEventListener('click', interceptClickEvent);
        document.addEventListener('click', interceptClickEvent);
        document.removeEventListener('mouseover', mouseOverEvent);
        document.addEventListener('mouseover', mouseOverEvent);
    } else if (document.attachEvent) {
        document.detachEvent('onclick', interceptClickEvent);
        document.attachEvent('onclick', interceptClickEvent);
    }
}

function commonLoadingImplicitlyImplemented() {

    // we are already iomplicitly imlemented above
    if (true) {
        return;
    }
    $(".wtopscroller1").scroll(function(e) {
        var target = e.target || e.srcElement;
        var PE = target.parentElement;
        var w1 = PE.querySelector(".wtopscroller1");
        if (w1 === undefined) w1 = PE.closest(".wtopscroller1");
        var w2 = PE.querySelector(".wtopscroller2");
        if (w2 === undefined) w2 = PE.closest(".wtopscroller2");
        console.log(PE + "-> " + target + " w1= " + w1 + " w2= " + w2);
        $(w2).scrollLeft($(w1).scrollLeft());
    });

    $(".wtopscroller2").scroll(function(e) {
        var target = e.target || e.srcElement;
        var PE = target.parentElement;
        var w1 = PE.querySelector(".wtopscroller1");
        if (w1 === undefined) w1 = PE.closest(".wtopscroller1");
        var w2 = PE.querySelector(".wtopscroller2");
        if (w2 === undefined) w2 = PE.closest(".wtopscroller2");
        console.log(PE + "-> " + target + " w1= " + w1 + " w2= " + w2);
        $(w1).scrollLeft($(w2).scrollLeft());
    });
    $(".tiptext").mouseover(function() {
        $(this).children(".description").show();
    }).mouseout(function() {
        $(this).children(".description").hide();
    });
    $(".sortable tbody th").on("click", function(e) {
        var r = $(this).parent();
        top.seInd = r.index();
        if ($(this).data("sort") == undefined) {
            $(this).data("sort", true);
        }
        sortRow(r, $(this).data("sort"));
        $(this).data("sort", $(this).data("sort") ? false : true);
    });
    makeResizableDiv('.resizable');
    scroll_top_and_bottom('.scroll_top_and_bottom');
}


$(document).ready(function() {
    console.log("Document Ready Start: " + window.location.href);
    if (window == top) topReady();
    if (window !== top) xframeReady();
    console.log("Document Ready End: " + window.location.href);
});

{
    commonLoading();
}
//});
//});