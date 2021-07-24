/*  Part of Logicmoo's SWISH

    Author:        Douglas Miles
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.logicmoo.org
    Copyright (C): 2014-2016, VU University Amsterdam
			      CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/
function mouseoverHilite(class_action, class_id, anchors, debug_item) {
    for (const anchor of anchors || []) {
        const sig = document.getElementById(signature_prefix + anchor);
        if (sig) {
            sig.classList[class_action](class_id); // sig.classList.{add,remove}(class_id)
        } else {
            if (class_action === 'add') {
                console.trace('No edge for anchor', anchor, debug_item);
                // alert('No edge for ' + debug_item);
            }
        }
    }
}
// butterfly_term.js
// Callback for a click on a token (anchor) in the source display
async function clickAnchor(mouse_target, source_item) {
    console.assert(mouse_target.id.startsWith(signature_prefix), 'Invalid signature_prefix', mouse_target.id, 'should start with:', signature_prefix);
    const signature = mouse_target.id.substr(signature_prefix.length);
    await fetchFromServer({
            anchor_xref: {
                signature: signature,
                corpus: source_item.corpus,
                root: source_item.root,
                path: source_item.path,
                language: 'python'
            }
        }, // TODO: don't hard-code language
        data => setXref(source_item, mouse_target.id, data));
}


function loadcssfile(filename) {
    var fileref = document.createElement("link")
    fileref.setAttribute("rel", "stylesheet")
    fileref.setAttribute("type", "text/css")
    fileref.setAttribute("href", filename)
    if (typeof fileref != "undefined")
        document.getElementsByTagName("head")[0].appendChild(fileref)
}

function loadjsfile(filename) {
    /*$(jQuery).getScript( filename )
      .done(function( script, textStatus ) {
        console.log( textStatus + " " + filename  );
      })
      .fail(function( jqxhr, settings, exception ) {
    	debugger;    
    });*/
    var fileref = document.createElement('script')
    fileref.setAttribute("type", "text/javascript")
    fileref.setAttribute("src", filename)
    if (typeof fileref != "undefined")
        document.getElementsByTagName("head")[0].appendChild(fileref);
}


var mySwish = `<div id="hidden_swish_app" style="display:none; visibility:hidden">
		<header class="navbar navbar-default">
			<div class="container pull-left">
				<div class="navbar-header">
					<a href="/" class="pengine-logo">&nbsp;</a>
					<a href="/" class="swish-logo">&nbsp;</a>
				</div>
				<nav id="navbar"></nav>
			</div>
		</header>
		<div id="content" class="container">
		  <div class="tile horizontal" data-split="60%">
			<div class="prolog-editor"></div>
			<div class="tile vertical" data-split="70%">
			  <div class="prolog-runners"></div>
			  <div class="prolog-query"></div>
			</div>
		  </div>
		</div>
   </div>`;


$('body').each(function() {
    if (!$(".prolog-editor:first").hasClass("prolog-editor")) {
        $(this).append(mySwish);
        console.log("append mySwish");
    }

    // $(function() { $("body").swish(config.swish || {});

    loadjsfile("https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js")
    loadjsfile("https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js")
    loadcssfile("/swish/css/menu.css")
    loadcssfile("/swish/css/cliopatria.css")
    loadcssfile("/www/yui/2.7.0/build/autocomplete/assets/skins/sam/autocomplete.css")
    loadjsfile("/www/yui/2.7.0/build/utilities/utilities.js")
    // Use either font-awesome icons or Google icons with these links. Other icons could also be used if preferred
    loadcssfile("https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
    loadcssfile("https://fonts.googleapis.com/icon?family=Material+Icons")
    loadjsfile("/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js")
    loadcssfile("/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css")

    //loadjsfile("/www/yui/2.7.0/build/datasource/datasource.js")
    //loadjsfile("/www/yui/2.7.0/build/autocomplete/autocomplete.js")
    // loadjsfile("https://code.jquery.com/jquery-1.12.4.min.js")
    loadjsfile("/swish/lm_xref/pixmapx/selected/js/social.selection.js")
    loadcssfile("/swish/lm_xref/pixmapx/selected/css/social.selection.css")
    // loadcssfile("/swish/lm_xref/pixmapx/selected/css/example.css")
    loadjsfile("/swish/js/cliopatria.js")
    loadcssfile("/swish/css/butterfly_term.css")
    loadcssfile("/swish/css/term.css")
    console.log("loadjsfile");
    //debugger;

});

// necessary for the "draggable" ui  



/*

loadjsfile("/swish/lm_xref/pixmapx/popupmenu/scripts/Example.js")

// debugger;

// loadjsfile("/swish/js/jquery-2.1.3.min.js")
loadcssfile("/swish/css/term.css")
loadcssfile("/swish/css/butterfly_term.css")

// loadcssfile("/swish/lm_xref/pixmapx/popupmenu/styles/Example.css")

*/

if (!window.x) {
    window.x = {};
}

window.x.Selector = {};
window.x.Selector.getSelected = function() {
    var t = "";
    if (window.getSelection) {
        t = window.getSelection();
    } else if (document.getSelection) {
        t = document.getSelection();
    } else if (document.selection) {
        t = document.selection.createRange().text;
    }
    return t;
}

var pageX;
var pageY;
var toolElement;
var lastSelectedText;
var ctrlPressed = false;
var shiftPressed = false;
$(window).keydown(function(evt) {
    if (evt.which == 17) {
        ctrlPressed = true;
    }
    if (evt.which == 16) {
        shiftPressed = true;
    }
}).keyup(function(evt) {
    if (evt.which == 17) { // ctrl
        ctrlPressed = false;
    }
    if (evt.which == 16) {
        shiftPressed = false;
    }
});

function init_butterfly_term() {


    function setSelected(isButtonUp) {
        var selectedText = "" + x.Selector.getSelected();
        selectedText = selectedText.trim();
        if (selectedText != "") {
            // button up we hope
            if (isButtonUp) document.execCommand('copy');

            /*
              const el = document.createElement('textarea');
			  el.value = selectedText;
			  document.body.appendChild(el);
			  el.select();
			  document.execCommand('copy');
			  document.body.removeChild(el);*/

            lastSelectedText = selectedText;
            if (shiftPressed || ctrlPressed) showSelected();
        } else {
            hideToolElement();
        }

    }

    var shouldBeVisible = false;

    function hideToolElement() {
        if (shouldBeVisible) {
            shouldBeVisible = false;
            ensureToolElement();
            toolElement = $("div.selectionTooltip:first");
            toolElement.delay(2000).fadeOut(300);
        }
    }

    function showSelected() {
        shouldBeVisible = true;
        ensureToolElement();
        toolElement = $("div.selectionTooltip:first")
        //if(toolElement.hasClass("selectionTooltip")) {
        var st = toolElement.find("#selectedText");
        //debugger;
        if (lastSelectedText.indexOf('\n') > 0) {
            $(st).replaceWith($('<textarea id="selectedText">  ' + lastSelectedText + "  </textarea>"))
        } else {
            $(st).replaceWith($('<pre id="selectedText">  ' + lastSelectedText + "  </pre>"))
        }
        //}
        reposition();
    }

    function reposition() {
        if (!shouldBeVisible) {
            return;
        }
        ensureToolElement();
        var toolElement = $("div.selectionTooltip:first")
        if (toolElement.hasClass("selectionTooltip")) {
            var x = pageX - 125;
            if (x < 10) {
                x = 10;
            }
            var y = pageY - 180;
            if (y < 20) {
                y = 190;
            }
            toolElement.css({
                "left": x,
                "top": y
            }).fadeIn(200);
        }
    }


    function setXY(element) {
        pageY = $(element).position().top;
        pageX = $(element).position().left;
        //pageY = $(element).offset().top;
        //pageX = $(element).offset().left;
    }


    $("textarea").focus(function(e) {
        var text = $(this).val();
        if (text == "") {
            document.execCommand("paste");
            return;
        }
        return;
        $(this).select();
        setXY($(this));
        setSelected(false)
    });

    $("input:text").focus(function(e) {
        var text = $(this).val();
        if (text == "") {
            document.execCommand("paste");
            return;
        }
        return;
        $(this).select();
        setXY($(this));
        setSelected(false)
    });

    $(document).bind("mouseup", function(e) {
        if (contains($("div.selectionTooltip:first"), e.target)) return;

        setSelected(true);
    });

    { // remove wierd comma that shows up
        var v = document.querySelector("body > p");
        if (v) {
            var waz = v.outerHTML;
            if (waz == "<p>,\n     </p>") v.remove();
        }
    }


    function contains(p, target) {
        if ($(target).parents("div.selectionTooltip:first").length) {
            return true;
        }
        return false;
    }

    function ensureToolElement() {
        toolElement = $("div.selectionTooltip:first")
        if (!toolElement.hasClass("selectionTooltip")) {
            var x = pageX - 115;
            var y = pageY - 185;
            toolElementSrc = `
			<div class="selectionTooltip selectionTooltip752708 center" style="background-color: white; color: black;" top: ` + y + `px; left: ` + x + `px; max-width: 360px">
			  <textarea id="selectedText"> Selected </textarea>
   		      <span style="font-size: 32px;">`;
            toolElementSrc += "<p/>";

            var obj = {
                search: "find",
                copy: "copy",
                paste: "replace",
                bath: "Assertion",
                'quote-left': "English"
            };
            jQuery.each(obj, function(i, val) {
                toolElementSrc += '<button title="' + val + '"><i class="fa fa-' + i + '"/></button>';
            });
            toolElementSrc += "<p/>";
            var foo = "" + $("#table5 > tbody > tr:nth-child(1) > td:nth-child(3) > label").clone().html();
            if (foo != "undefined") {
                toolElementSrc += foo;
            } else {
                toolElementSrc += `<label><select name="action_above"><option value="Find">Find $item</option><option value="Forward">Forward Direction</option><option value="Backward">Backward Direction</option><option value="query" selected="yes">Query Item</option><option value="repropagate">Repropagate $item (ReAssert)</option><option value="remove">Remove $item(Unassert)</option><option value="Code">Assume Theorem (Disable $item)</option><option value="prologSingleValued">Make $item Single Valued</option><option value="prologBuiltin">Impl $item in Prolog</option><option value="prologPTTP">Impl $item in PTTP</option><option value="prologDRA">Impl $item in DRA</option><option value="prologPfc">Impl $item in PFC</option><option value="Monotonic">Treat $item Monotonic</option><option value="NonMonotonic">Treat $item NonMonotonic</option></select>&nbsp;&nbsp;&nbsp;<input type="submit" value="Now" name="Apply"></label>`;
            }

            toolElementSrc += `</span></div>`;
            toolElement = $(toolElementSrc);
            $("body").append(toolElement);
            $(toolElement).mousedown(function(event) {
                event.stopPropagation();
            });
            $(toolElement).mouseup(function(event) {
                event.stopPropagation();
            });
        }
    }

    $(document).on("mousedown", function(e) {
        pageX = e.pageX;
        pageY = e.pageY;
        if (contains($("div.selectionTooltip:first"), e.target)) return;
        if (e.buttons == 2) {
            if (lastSelectedText != null) {
                document.execCommand("paste")
            }
        }
    });


    function getClipboard() {
        var pasteTarget = document.createElement("div");
        pasteTarget.contentEditable = true;
        var actElem = document.activeElement.appendChild(pasteTarget).parentNode;
        pasteTarget.focus();
        document.execCommand("Paste", null, null);
        var paste = pasteTarget.innerText;
        actElem.removeChild(pasteTarget);
        return paste;
    };

    console.info("init_butterfly_term");
}

$(document).ready(function() {
    init_butterfly_term();
});



var MIN_RECONNECT_DELAY = 10000;
var MAX_RECONNECT_DELAY = 300000;

(function($) {
    var pluginName = 'eval_socket';
    var reconnect_delay = MIN_RECONNECT_DELAY;
    var last_open = null;

    /** @lends $.fn.ctrlEval */
    var methods = {
        _init: function(options) {
            return this.each(function() {
                var elem = $(this);
                $.fn.ctrlEval = function(method) {
                    if (methods[method]) {
                        return methods[method]
                            .apply(this, Array.prototype.slice.call(arguments, 1));
                    } else if (typeof method === 'object' || !method) {
                        return methods._init.apply(this, arguments);
                    } else {
                        $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
                    }
                };

                debugger;
                var data = {}; /* private data */

                elem.data(pluginName, data); /* store with element */

                /* add event handling */
                elem.on("click", function(ev) {
                    var li = $(ev.target).closest("li.user");

                    if (li.length == 1)
                        elem.ctrlEval('unnotify', li.attr("id"));
                });
                elem.on("send", function(ev, msg) {
                    elem.ctrlEval('send', msg);
                });
                $(window).bind("beforeunload", function() {
                    elem.ctrlEval('disconnect');
                });

                /* setup websocket */
                if (true) {
                    elem.ctrlEval('connect');
                }
            });
        },

        /*******************************
         *	      WEBSOCKET		*
         *******************************/

        /**
         * Create a websocket connection to /ctrlEval on the SWISH server.
         */
        connect: function() {
            var elem = this;
            var data = this.data(pluginName);
            var url = window.location.host + config.http.locations.swish_chat;
            var lead = "?";
            var ws = window.location.protocol.replace("http", "ws");

            if (data.connection && data.connection.readyState != 3)
                return this; /* already connecting, open or closing */

            function add_pref_param(name, pname) {
                var value = preferences.getVal(pname);

                if (value) {
                    if (pname == "anon-avatar") {
                        /* hack to deal with possibly rebased server */
                        if (value.indexOf("#") == -1) {
                            value = config.http.locations.avatar + value.split("/").pop();
                        } else {
                            value = config.http.locations.swish + "icons/" + value.split("/").pop();
                        }
                    }

                    url += lead + name + "=" + encodeURIComponent(value);
                    lead = "&";
                }
            }

            add_pref_param("avatar", "anon-avatar");
            add_pref_param("nickname", "nick-name");

            if (data.reconnect) {
                /* reconnecting */
                url += lead + "reconnect" + "=" + encodeURIComponent(data.reconnect);
                lead = "&";
            } else {
                add_pref_param("wsid-token", "reconnect");
            }

            try {
                data.connection = new WebSocket(ws + "//" + url,
                    ['v1.eval.shrdlurn.logicmoo.org']);
            } catch (err) {
                elem.ctrlEval('userCount', undefined);
                return;
            }

            data.connection.onerror = function(error) {
                elem.ctrlEval('userCount', undefined);
            };
            data.connection.onclose = function(ev) {
                if (last_open == null) {
                    reconnect_delay *= 2;
                    if (reconnect_delay > MAX_RECONNECT_DELAY)
                        reconnect_delay = MAX_RECONNECT_DELAY;
                } else {
                    if (getTime() - last_open > 300000) {
                        reconnect_delay = MIN_RECONNECT_DELAY;
                    } else {
                        reconnect_delay *= 2;
                        if (reconnect_delay > MAX_RECONNECT_DELAY)
                            reconnect_delay = MAX_RECONNECT_DELAY;
                    }
                }
                setTimeout(function() {
                    elem.ctrlEval('connect');
                }, reconnect_delay);
            };
            data.connection.onmessage = function(e) {
                var msg = JSON.parse(e.data);
                msg.origin = e.origin;
                if (msg.type)
                    elem.ctrlEval(msg.type, msg);
                else
                    console.log(e);
            };
            data.connection.onopen = function() {};
        },

        empty_queue: function() {
            var data = this.data(pluginName);

            while (data.queue &&
                data.queue.length > 0 &&
                data.connection.readyState == 1) {
                var str = data.queue.shift();
                data.connection.send(str);
            }
        },

        disconnect: function() {
            var data = this.data(pluginName);

            if (data.connection) {
                this.ctrlEval('send', {
                    type: "unload"
                });
                data.connection.onclose = function() {};
                data.connection.close();
                data.connection = undefined;
                preferences.setVal("wsid-token", data.reconnect);
            }

            return this;
        },


        /*******************************
         *	   BASIC MESSAGES	*
         *******************************/

        /**
         * @param {Object} msg is the JSON object to broadcast
         */
        send: function(msg) {
            var data = this.data(pluginName);

            if (data && data.connection) {
                var str = JSON.stringify(msg);

                if (data.connection.readyState != 1) {
                    if (!data.queue)
                        data.queue = [str];
                    else
                        data.queue.push(str);
                    this.ctrlEval('connect');
                } else {
                    data.connection.send(str);
                }
            }

            return this;
        },

        subscribe: function(channel, sub_channel) {
            var msg = {
                type: "subscribe",
                channel: channel
            };

            if (sub_channel)
                msg.sub_channel = sub_channel;

            this.ctrlEval('send', msg);
        },

        unsubscribe: function(channel, subchannel) {
            var msg = {
                type: "unsubscribe",
                channel: channel
            };

            if (sub_channel)
                msg.sub_channel = sub_channel;

            this.ctrlEval('send', msg);
        }
    } // methods

    // 'use strict'

    var CntrlSocket = function(url) {
        var elem = this;
        var defaultURL = window.location.protocol.replace("http", "ws") + // gets 'ws' or 'wss:'
            "//" + window.location.host + ":14302/swish/jseval_ws";
        this.url = url || defaultURL;		
        this.methods = methods;
    }

	CntrlSocket.prototype.scheduleRetry = function() {
				if(reconnectTries==0) {
				   reconnectTries++;

				   setTimeout(function() {
			 		    setTimeout(function() {
							console.warn("Reconnection to remote REPL on " + theCntrlSocket.url)
							theCntrlSocket.connectEval();
						}, 1000);
					}, 30000);
				}
	}

	var reconnectTries = 0;
    CntrlSocket.prototype.connectEval = function() {

        try {
			//this.url = "wss://echo.websocket.org";
			//this.url = "wss://logicmoo.org:14302/swish/jseval_ws";
            var socket = new WebSocket(this.url);
            socket.onopen = function(e) {
				reconnectTries ==0;
                console.log("[open] Connection established");

                //socket.send("true");
            };
			socket.onmessage = function(message) {
				console.log(`[message] Data received from server: ${message.data}`);
				try {
					//debugger;
					socket.send(JSON.stringify(eval(message.data)))
				} catch (e) {
					socket.send(JSON.stringify({
						"error": {
							"message": e.message,
							"trace": e.trace
						}
					}))
				}
			}

            socket.onclose = function(event) {
				console.warn(event);
                if (event.wasClean) {
                    console.log(`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
					reconnectTries = 0;
                } else {
                    // e.g. server process killed or network down
                    // event.code is usually 1006 in this case
                    console.log(`[close] Connection died, code=${event.code} reason=${event.reason}`);
                }
				theCntrlSocket.scheduleRetry();
            };

            socket.onerror = function(error) {    				
				console.warn(error);
                if(error !=null && error.message !=undefined) {
					console.log(`[error] ${error.message}`);
				}
				theCntrlSocket.scheduleRetry();
            };

            this.socket = socket;

        } catch (e) {
			theCntrlSocket.scheduleRetry();
        }
		
	}

        // return this;


    window.CntrlSocket = CntrlSocket;
    var theCntrlSocket = new CntrlSocket();
    window.theCntrlSocket = theCntrlSocket;
    theCntrlSocket.connectEval();
    // debugger;


})(window)
