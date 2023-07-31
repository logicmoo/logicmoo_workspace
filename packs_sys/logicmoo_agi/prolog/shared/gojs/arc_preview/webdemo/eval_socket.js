(function($) {

var MIN_RECONNECT_DELAY = 9000;
var MAX_RECONNECT_DELAY = 30000;
var RECONNECTS_AVAIL = 20;

  classNameKey = "_className";

	// Call by $(jQuery).getScript("eval_socket.js");
	console.log("(Re)loading Script: eval_socket.js");


	window.JSCtrl = function(url) {
		var defaultURL = window.location.protocol.replace("http", "ws") + // gets 'ws' or 'wss:'
			"//" + window.location.host + ":11766/swish_arc/jseval_ws";
		this.url = url || defaultURL;
        console.log("connect: " + this.url);
		DEBUGGING = true;
		reconnectsAvail = RECONNECTS_AVAIL;
		reconnectScheduled = false;
		objToPath = new Map();
		pathToObj = new Map();
		simpleMode = true;
		simpleMode2 = true;
	}

	window.game_props = new Map();

	// object to flat
	o2prox = new Map();
	prox2o = new Map();

	pathTypeS = function(low, high) {
		return Object.fromEntries(new Map(Array.from(window.game_props).slice(low, high)));
	}

	function loadjsfile(filename, datamain) {
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
		if (typeof datamain != "undefined") fileref.setAttribute("data-main", datamain)
		if (typeof fileref != "undefined")
			document.getElementsByTagName("head")[0].appendChild(fileref);
	}

	Object.prototype = {

		toDeepJSON: function() {
			if (this instanceof Map || this instanceof Set) {
				return Object.fromEntries(value);
			}
			var tmp = {};
			for (var key in this) {
				if (typeof this[key] !== 'function')
					tmp[key] = this[key];
			}
			return tmp;
		}
	};

	//loadjsfile("https://code.jquery.com/jquery-3.6.0.min.js")// crossorigin="anonymous">
	//loadjsfile("/node_modules/requirejs/require.js","/swish/js/swish");
	//loadjsfile("/swish/node_modules/reflect-metadata/Reflect.js");
	// loadjsfile("/swish/node_modules/class-transformer/cjs/index.js");
	//loadjsfile("eval_socket_hydrate.js");



	JSCtrl.prototype.scheduleReconnect = function() {
		if (reconnectScheduled) return;
		reconnectScheduled = true;
		if (reconnectsAvail > 0) {
			reconnectsAvail--;
			setTimeout(function() {
				setTimeout(function() {
					console.warn("Reconnection to remote JSCtrl on " + jsev.url);
					reconnectScheduled = false;
					jsev.connect();
				}, MIN_RECONNECT_DELAY);
			}, MAX_RECONNECT_DELAY);
		}
	}

	JSCtrl.prototype.classOf = function(obj) {
		if (obj == null) return "null";
		var typeOf = (typeof obj);
		if (typeOf !== "object") return typeOf;
		//var cn = obj[classNameKey]; 
		//if (cn && cn.length>0) return cn;
		var funcNameRegex = /function (.{1,})\(/;
		var constructor = (obj).constructor;
		
		if (constructor == null || typeof constructor === "undefined") return typeOf;
		var results = (funcNameRegex).exec(constructor.toString());
		return (results && results.length > 1) ? results[1] : "";
	};

	window.reload = function() {
	     eval('""+$.getScript("eval_socket.js");');
	}



	var workerFn = function() {
		console.log("I was run");
		//debugger;
	};
	// create a Blob object with a worker code
	var blob = (new Blob(["(" + workerFn.toString() + ")"], {
		type: "text/javascript"
	}));
	// Obtain a blob URL reference to our worker 'file'.
	var blobURL = window.URL.createObjectURL(blob);

	// create a Worker
	var worker = new Worker(blobURL);
	worker.onmessage = function(e) {
		console.log(e.data);
	};
	worker.postMessage("Send some Data");



	JSCtrl.prototype.connect = function() {
              // if (true) return;

		try {
			//this.url = "wss://echo.websocket.org";
			//this.url = "ws://org:14302/swish/jseval_ws";
			var socket = new WebSocket(this.url);
			this.socket = socket;

			socket.onopen = function(e) {
				reconnectsAvail = RECONNECTS_AVAIL;
				if (!(window.theA4Game)) {
					window.theA4Game = this;
				}
				window.game = window.theA4Game;
				console.log("JSCtrl: " + "[open] Connection established");
				var sessionId = /SESS\w*ID=([^;]+)/i.test(document.cookie) ? RegExp.$1 : false;
				socket.send("sessionId=" + sessionId);
			};

			socket.onmessage = function(message) {
				setTimeout(function() {
					console.log("JSCtrl: " + `[message] Data received from server: ${message.data}`);
					try {
						//debugger;
						var messageData = message.data;
						var evalThis = true;

						if (messageData.startsWith("+")) {
							messageData = messageData.substring(1);
							evalThis = true;
						}
						if (messageData.startsWith("-")) {
							messageData = messageData.substring(1);
							evalThis = false;
						}
						if (evalThis) {
							var t0 = performance.now()
							var res = window.eval(messageData);
							var reply = null;
							var html = jsev.maybeHtml(res, 0);
							if (jsev.isHtmlish(html)) {
								reply = html;
							} else {
								// reply = forestify_aka_decycle(res);
								var other = jsev.typeIfy(res);
								if (jsev.classOf(res) != jsev.classOf(other)) {
									try {
										if (reply == null) 
										//reply = 
										JSON.stringify(other);
										res = other;

									} catch (ignore) {
										// console.log(ignore);
									}
								}
								if (reply == null) {
									try {
										reply = JSON.stringify(res);
									} catch (ignore) {
										//  console.log(ignore);
									}
								}
								if (reply == null) {
									try {
										reply = jsev.stringifyAsJson("#REF:" + messageData, res);
									} catch (ignore) {
										console.log(ignore);
										//debugger;
										throw ignore
									}
								}


							}

							if (DEBUGGING) { // for debugging
								if (typeof reply === 'undefined') {} else {
									if (typeof reply.length != 'undefined' && reply.length < 3000) {
										console.log("JSCtrl: " + `[reply] Replying with: ${reply}`);
									} else if (typeof reply.substring != 'undefined') {
										var some = reply.substring(0, 100);
										console.log("JSCtrl: " + `[reply] Replying.length with: ${some}...${reply.length}`);
									} else {
										console.log("JSCtrl: " + `[reply] Replying with: ${reply}`);
									}
								}
							}

							if (typeof reply === "string") {
								socket.send(reply);
							} else {
								socket.send(JSON.stringify(reply));
							}
							var t1 = performance.now()
							console.log("eval/reply took " + (t1 - t0) + " milliseconds for: " + messageData)
						}
					} catch (e) {
						DEBUGGING = true;
						console.error(e);
						//debugger;
						socket.send(JSON.stringify({
							"error": {
								"message": e.message,
								"trace": e.trace,
								"original": message
							}
						}))
					}
				}, 0);
			}

			socket.onclose = function(event) {
				console.warn(event);
				if (event.wasClean) {
					console.log("JSCtrl: " + `[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
					reconnectsAvail = RECONNECTS_AVAIL;
				} else {
					// e.g. server process killed or network down
					// event.code is usually 1006 in this case
					console.log("JSCtrl: " + `[close] Connection died, code=${event.code} reason=${event.reason}`);
				}
				jsev.scheduleReconnect();
			};

			socket.onerror = function(error) {
				console.warn(error);
				if (error != null && error.message != undefined) {
					console.log("JSCtrl: " + `[error] ${error.message}`);
				}
				jsev.scheduleReconnect();
			};




		} catch (e) {
			jsev.scheduleReconnect();
		}


	}

	// run with node --experimental-worker index.js on Node.js 10.x
	//const request = require(['request']);
	//const fs1 = require(['fs']);
	//var fs = Promise.promisifyAll(require(["fs"]), {suffix: "MySuffix"});
	/*
								  const { Worker } = require(['worker_threads'])
								  function runService(workerData) {
									return new Promise((resolve, reject) => {
									  const worker = new Worker('./service.js', { workerData });
									  worker.on('message', resolve);
									  worker.on('error', reject);
									  worker.on('exit', (code) => {
										if (code !== 0)
										  reject(new Error(`Worker stopped with exit code ${code}`));
									  })
									})
								  }
						  
								  async function run() {
									const result = await runService('world')
									console.log(result);
								  }
						  
								  if(false)run().catch(err => console.error(err))
								  */
	JSCtrl.prototype.loadDocument = function(from) {
		var xmlhttp = new XMLHttpRequest();
		xmlhttp.overrideMimeType("text/xml");
		xmlhttp.open("GET", from, false);
		xmlhttp.send();
		return xmlhttp.responseXML.documentElement;
	}

	JSCtrl.prototype.parseRefJSON = function(json) {
		// use outer scope for now
		//var objToPath = new Map();
		//var pathToObj = new Map();
		var o = JSON.parse(json);

		var traverse = (parent, field) => {
			var obj = parent;
			var path = '#REF:';

			if (field !== undefined) {
				obj = parent[field];
				path = objToPath.get(parent) + (Array.isArray(parent) ? `[${field}]` : `${field?'.'+field:''}`);
			}

			objToPath.set(obj, path);
			pathToObj.set(path, obj);

			var ref = pathToObj.get(obj);
			if (ref) parent[field] = ref;

			for (var f in obj)
				if (obj === Object(obj)) traverse(obj, f);
		}

		traverse(o);
		return o;
	}
	// inspect the return result of maybeHtml
	JSCtrl.prototype.isHtmlish = function(value, depth) {
		if (typeof value === "string") return true;
		if (typeof value === "undefined") return false;
		//if (typeof value === "DefinedRef") return true;
		if (value == false) return false;
		//if (typeof value.toJSON === "function") return true;
		return false;
	}

	// return +<some>html</some> if value contains .outerHTML
	JSCtrl.prototype.maybeHtml = function(value0, depth) {
		if (!(value0 != null)) {
			return false;
		}
		if (typeof value0 === "undefined") {
			return false;
		}
		var value = value0;

		try {

			//if (typeof value.outerHTML === 'function') {
			return "+" + value.outerHTML().trim();
			//}
		} catch (e) {
			//debugger;
			// ignored
		}

		try {

			//if (typeof value.outerHTML !== 'undefined') {
			return "+" + value.outerHTML.trim();
			//}
		} catch (e) {
			//debugger;
			// ignored
		}

		// only use the above
		if (true || simpleMode2) return false;

		if (depth > 2 && typeof value.saveToXML === 'function') {
			return "+" + value.saveToXML().trim();
		}

		if (depth > 1 && typeof value.savePropertiesToXML === 'function') {
			return "+" + value.savePropertiesToXML(window.theA4Game).trim();
		}

		if (simpleMode) return false;
		try {
			if (depth < 3) return JSON.stringify(value);
			return "<@ " + JSON.stringify(value) + " @>";
		} catch (e) {
			// ignore
		}
		return false;
	}

	JSON.stringifyWithCircularRefs = (function() {
		const refToPath = new Map();
		const parents = [];
		const path = [""];
		var thisPrefix = "$";
		var savePath = true;

		function clear() {
			refToPath.clear();
			parents.length = 0;
			path.length = 1;
		}

		function updateParents(skey, key, value) {
			var idx = parents.length - 1;
			var prev = parents[idx];
			if (prev[key] === value || idx === 0) {
				path.push(skey);
				parents.push(value);
			} else {
				while (idx-- >= 0) {
					prev = parents[idx];
					if (prev && prev[key] === value) {
						idx += 2;
						parents.length = idx;
						path.length = idx;
						--idx;
						parents[idx] = value;
						path[idx] = skey;
						break;
					}
				}
			}
		}

		function checkCircular(key, value) {

			var isComplex = value && (value === Object(value));
			if (isComplex && key)
				updateParents(isNaN(key) ? ("." + key) : ("[" + key + "]"), key, value);

			var pathname = path.join('');

			if(savePath) {
				const regex = RegExp('\[[0-9]+\]','g');
				var cpathname = pathname.replaceAll(regex, '[_]');
				var oc = window.game_props.get(cpathname);
				if (!(oc && oc != null && oc !== 'null')) {
					var cn = jsev.classOf(value);
					window.game_props.set(cpathname, cn);
				}
			}

			var obj = pathToObj.get(pathname);
			if (savePath && obj != value) {
				pathToObj.set(pathname, value);
				objToPath.set(value, pathname);
			}
			if (isComplex) {
				var other = refToPath.get(value);
				if (other) { //var html = jsev.maybeHtml(value, 2); //if (jsev.isHtmlish(html)) return html;
					return thisPrefix + other;
				}
				refToPath.set(value, pathname);
				var html = jsev.maybeHtml(value, 1);
				if (jsev.isHtmlish(html)) return html;
			}
			return jsev.typeIfy(value);
		}

		return function stringifyWithCircularRefs(prefix, isThis, obj, space) {
			try {
				parents.push(obj);
			    savePath = isThis
				thisPrefix = prefix
				return JSON.stringify(obj, checkCircular, space);
			} finally {
				savePath = true;
				clear();
			}
		}
	})();

	JSCtrl.prototype.stringifyAsJson = function(prefix, value, space) {
		//value==window.theA4Game;
		return JSON.stringifyWithCircularRefs(prefix, true, value, space);
	}

	JSCtrl.prototype.typeIfy = function(obj) {

	
		if (obj == null || (!(typeof obj === 'object'))) return obj;

		var proxy = o2prox.get(obj);
		if (proxy) return proxy;
		if (prox2o.get(obj)) return obj; // was a proxy
		if (true) {
			var html = jsev.maybeHtml(obj, 0);
			if (jsev.isHtmlish(html)) return obj;
		}
		if (false && !obj.hasOwnProperty(classNameKey)) {
			try {
				var cn = jsev.classOf(obj);
				if (cn) obj[classNameKey] = cn;
			} catch (ee) {
				console.log(`${clasNameKey}: ` + obj);
				console.error(ee);
				debugger;
			}
		}
		try {
			if (obj instanceof Map) {
				proxy = Object.fromEntries(obj);
				//if (false) proxy[classNameKey] = "Map";
				return proxy;
			}

			if (Array.isArray(obj)) {
				proxy = obj;
				/*
				proxy = [];
				prox2o.set(proxy, obj);
				o2prox.set(obj, proxy);
				for (var i of obj) {
					//var t = jsev.typeIfy(i);
					proxy.push(i);
				}*/
				return proxy;
			
			}

			if (obj instanceof Set) {			   
                proxy = new Set(obj);
				// if (false) proxy[classNameKey] = "Set";
				return proxy;
			}

			proxy = {}; // proxy = Object.create(obj); //{};

			{
				try {
					var cn = jsev.classOf(obj);
					if (cn) proxy[classNameKey] = cn;
				} catch (ee) {
					console.log(`${classNameKey}: ` + obj);
					console.error(ee);
					debugger;
				}
			}
			prox2o.set(proxy, obj);
			try {
				for (var key in obj) {
					try {
						if (key == 'game') continue;
						if (key == 'window') continue;
		                if (key == '_window') continue;
						if (key == 'app') continue;
						var v = obj[key];
						if (v == obj) continue;
						if (typeof v === 'function') {
							v = Object.getOwnPropertyDescriptor(obj, key);
						} else {
							//var v = jsev.typeIfy(v);
						}
						try {
							proxy[key] = v;
						} catch (ee3) {
							console.log("setKey: " + key);
							console.error(ee3);
							debugger;
						}
					} catch (ee2) {
						console.log("typeIfy: " + key);
						console.error(ee2);
						debugger;
					}
				}
			} catch (e) {
				console.log(e);
				debugger;
			}
			return proxy;

		} finally {
			if (proxy != null) {
				o2prox.set(obj, proxy);
				prox2o.set(proxy, obj);
			}
		}
	}




	if (!(window.jsev)) {
		window.jsev = new JSCtrl();
		window.jsev.connect();
		jsev = window.jsev;
	}

   console.log("Done loading Script: eval_socket.js");

})(window)

