var MIN_RECONNECT_DELAY = 10000;
var MAX_RECONNECT_DELAY = 30000;

(function($) {

	if (typeof window.JSCtrl === "undefined") {

		function JSCtrl(url) {
			var defaultURL = window.location.protocol.replace("http", "ws") + // gets 'ws' or 'wss:'
				"//" + window.location.host + ":14302/swish/jseval_ws";
			this.url = url || defaultURL;
			DEBUGGING = false;
			reconnectsAvail = 10;
			reconnectScheduled = false;
			objToName = new Map();
			nameToObj = new Map();
			simpleMode = true;
			simpleMode2 = true;
		}

		window.JSCtrl = JSCtrl;
		jsev = new JSCtrl();

		// object to flat
		objToProxy = new Map();
		objFromProxy = new Map();
		pathValues = new Map();

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

		JSCtrl.prototype.getClassName = function(obj) {
			if (obj == null) return "@null";
			if (typeof obj === "undefined") return "@undefined";
			var funcNameRegex = /function (.{1,})\(/;
			var results = (funcNameRegex).exec((obj).constructor.toString());
			return (results && results.length > 1) ? results[1] : "";
		};


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

			try {
				//this.url = "wss://echo.websocket.org";
				//this.url = "wss://logicmoo.org:14302/swish/jseval_ws";
				var socket = new WebSocket(this.url);
				this.socket = socket;

				socket.onopen = function(e) {
					reconnectsAvail = 10;
					window.theA4Game;
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
								var res = window.eval(messageData);

								var t0 = performance.now()
								var reply = null;
								//res = jsev.typeIfy(res);
								var html = jsev.maybeHtml(res, 0);
								if (jsev.isHtmlish(html)) {
									reply = html;
								} else {
									// reply = forestify_aka_decycle(res);
									reply = jsev.stringfyAsJson(res);
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
						reconnectsAvail = 10;
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
			let objToPath = new Map();
			let pathToObj = new Map();
			let o = JSON.parse(json);

			let traverse = (parent, field) => {
				let obj = parent;
				let path = '#REF:$';

				if (field !== undefined) {
					obj = parent[field];
					path = objToPath.get(parent) + (Array.isArray(parent) ? `[${field}]` : `${field?'.'+field:''}`);
				}

				objToPath.set(obj, path);
				pathToObj.set(path, obj);

				let ref = pathToObj.get(obj);
				if (ref) parent[field] = ref;

				for (let f in obj)
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

				//if (typeof value.outerHTML === 'function') {
				return "+" + value.outerHTML.trim();
				//}
			} catch (e) {
				//debugger;
				// ignored
			}

			if (simpleMode2) return false;

			if (depth > 2 && typeof value.saveToXML === 'function') {
				return "+" + value.saveToXML().trim();
			}

			if (depth > 1 && typeof value.savePropertiesToXML === 'function') {
				return "+" + value.savePropertiesToXML(theA4Game).trim();

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

		JSCtrl.prototype.stringfyAsJson = function(value) {
			// value = jsev.typeIfy(value);
			return JSON.stringify(value, jsev.refReplacer(value));
		}

		Object.prototype = {

			toJSON2: function() {
				var tmp = {};

				for (var key in this) {
					if (typeof this[key] !== 'function')
						tmp[key] = this[key];
				}

				return tmp;
			}
		};

		JSCtrl.prototype.refReplacer = function(root) {
			let m = new Map(),
				v = new Map(),
				parents = [],
				init = null;


			return function(field, value) {
				var rep = replace(field, value);
				return rep;
			}

			function replace(field, value) {

				var proxy = value;
				try {
					proxy = jsev.typeIfy(value);
				} catch ( ee ) {
					console.log(ee);
					debugger;
				}

				var isProxy = (proxy != value);

				let p = m.get(this) + (Array.isArray(this) ? `[${field}]` : '.' + field);
				let isComplex = (value === Object(value));

				if (isComplex) m.set(value, p);


				//if(!isComplex) debugger;

				let pp = v.get(value) || '';
				let path = p.replace(/undefined\.\.?/, '');
				var refName = `#REF:${pp[0]=='[' ? '$':'$.'}${pp}`
				let val = pp ? refName : value;


				!init ? (init = value) : (val === init ? val = "#REF:$" : 0);

				var isRef = val != value;

				if(!isRef) {
					console.log(refName) ;					
				}

				try {
					if (!pp && isComplex) v.set(value, path);

					pathValues.set(path, jsev.getClassName(value));
					//console.log(path);

					if ((value == null) || (typeof value === "undefined") ||
						(typeof value === "string") ||
						(typeof value !== "object")) {
						return value;
					}

					if (isRef) return val;

					var decendantLevel = parents.indexOf(value);

					parents.push(value);

					if(decendantLevel> -1) {
						debugger;
					}


					if (true) {
						var html = jsev.maybeHtml(val, 0);
						if (jsev.isHtmlish(html)) {
							if (isRef) return val;
							return html;
						}
					}

					if (isRef) return val;

					if (isProxy) {
						debugger;
						return proxy;
					} else {
						return val;
					}

				} finally {
					parents.pop();
				}

			}
		}

		JSCtrl.prototype.typeIfy = function(obj) {
			if (obj === null) {
				return null;
			}

			if (!(typeof obj === 'object')) {
				return obj;
			}

			if ((typeof obj === 'function')) {
				return obj;
			}

			if (Array.isArray(obj)) {
				return obj;
			}
			
			if (obj instanceof Map) {
			    return obj;
			}
			if (obj instanceof Set) {
			    return obj;
			}

			var ofp = objFromProxy.get(obj);

			if (ofp) {
				// was a proxy
				return obj;
			}
			var proxy = objToProxy.get(obj);
			if (proxy == true) return obj;
			if (proxy) return proxy;

			if (true) {
				var html = jsev.maybeHtml(obj, 0);
				if (jsev.isHtmlish(html)) {
					return obj;
				}
			}

			// proxy = Object.create(obj); //{};
			proxy = {};
			
			objToProxy.set(obj, proxy);
			objFromProxy.set(proxy, obj);
			try {
				var cn = jsev.getClassName(obj);
				proxy["_className"] = cn;
			} catch (ee) {
			    console.log("_className: " + obj);
				console.log(ee);
				debugger;
			}
			try {
				for (var key in obj) {
					try {
						var v = obj[key];
						try {
							//if(!(typeof obj[key] === 'function')) {							 
							var r = jsev.typeIfy(v);
							try {
								proxy[key] = r;
							} catch (ee3) {
								console.log("setKey: " + key);
								console.log(ee3);
								debugger;
							}
							//}
						} catch (ee2) {
							console.log("typeIfy: " + key);
							console.log(ee2);
							debugger;
						}
						//}
					} catch (ee) {
						//console.log("getKey: " + key);
						//console.log(ee);
						//debugger;
					}
				}
			} catch (e) {
				console.log(e);
				debugger;
			}
			return proxy;
		}

	}

	jsev.connect();


})(window)
