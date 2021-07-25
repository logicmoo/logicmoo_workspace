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
                var sessionId =  /SESS\w*ID=([^;]+)/i.test(document.cookie) ? RegExp.$1 : false;
                socket.send("sessionId="+sessionId);
            };
			socket.onmessage = function(message) {
				console.log(`[message] Data received from server: ${message.data}`);
				try {
					//debugger;
					var messageData = message.data;
					if(messageData.startsWith("+")) {
						messageData = messageData.substring(1);
						var res = eval(messageData);
						var reply = JSON.stringifyWithCircularRefs(res);
						if (typeof res.outerHTML != 'undefined') {					
							reply = "+" + res.outerHTML;
						}
						console.log(`[reply] Replying with: ${reply}`);
						socket.send(reply);
					}
				} catch (e) {
					socket.send(JSON.stringify({
						"error": {							
							"message": e.message,
							"trace": e.trace,
						    "original": message
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

    JSON.stringifyWithCircularRefs = (function() {
    const refs = new Map();
    const parents = [];
    const path = ["this"];

    function clear() {
      refs.clear();
      parents.length = 0;
      path.length = 1;
    }

    function updateParents(key, value) {
      var idx = parents.length - 1;
      var prev = parents[idx];
      if (prev[key] === value || idx === 0) {
        path.push(key);
        parents.push(value);
      } else {
        while (idx-- >= 0) {
          prev = parents[idx];
          if (prev[key] === value) {
            idx += 2;
            parents.length = idx;
            path.length = idx;
            --idx;
            parents[idx] = value;
            path[idx] = key;
            break;
          }
        }
      }
    }

    function checkCircular(key, value) {
      if (value != null) {
        if (typeof value === "object") {
          if (key) { updateParents(key, value); }

          let other = refs.get(value);
          if (other) {
            return '[Circular Reference]' + other;
          } else {
            refs.set(value, path.join('.'));
          }
        }
      }
      return value;
    }

    return function stringifyWithCircularRefs(obj, space) {
	  try {
        parents.push(obj);
        return JSON.stringify(obj);
      } catch(e) {
      }
      try {
        parents.push(obj);
        return JSON.stringify(obj, checkCircular, space);
      } finally {
        clear();
      }
    }
  })();
        // return this;


    window.CntrlSocket = CntrlSocket;
    var theCntrlSocket = new CntrlSocket();
    window.theCntrlSocket = theCntrlSocket;
    theCntrlSocket.connectEval();
    // debugger;


})(window)
