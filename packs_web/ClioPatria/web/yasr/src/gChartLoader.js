var EventEmitter = require('events').EventEmitter,
	$ = require('jquery');
//cannot package google loader via browserify....
var loadingMain = false;
var loadingFailed = false;
var loader = function() {
	EventEmitter.call(this);
	var mod = this;
	this.init = function() {
		if (!loadingFailed && !require('google') && !loadingMain) { //not initiated yet, not currently loading, and has not failed the previous time
			loadingMain = true;
			/**
			 * It is extremely difficult to catch script loader errors (see http://www.html5rocks.com/en/tutorials/speed/script-loading/)
			 * Existing libraries either ignore several browsers (e.g. jquery 2.x), or use ugly hacks (timeouts or something)
			 * So, we use our own custom ugly hack (yes, timeouts)
			 */
			loadScript('http://google.com/jsapi', function() {
				loadingMain = false;
				mod.emit('initDone');
			});

			var timeout = 100; //ms
			var maxTimeout = 6000; //so 6 sec max
			var startTime = +new Date();
			var checkAndWait = function() {
				if (!require('google')) {
					if ((+new Date() - startTime) > maxTimeout) {
						//ok, we've waited long enough. Obviously we could not load the googleloader...
						loadingFailed = true;
						loadingMain = false;
						mod.emit('initError');

						//TODO: clear initDone callbacks. they won't fire anymore anyway

					} else {
						setTimeout(checkAndWait, timeout);
					}
				} else {
					//TODO: clear initFailed callbacks. they won't fire anymore anyway
				}
			}
			checkAndWait();
		} else {
			if (require('google')) {
				//already loaded! everything is fine
				mod.emit('initDone');
			} else if (loadingFailed) {
				mod.emit('initError')
			} else {
				//hmmm, should never get here
			}

		}
	}
	this.googleLoad = function() {

		var load = function() {
			require('google').load("visualization", "1", {
				packages: ["corechart", "charteditor"],
				callback: function() {
					mod.emit('done')
				}
			})
		}
		if (loadingMain) {
			mod.once('initDone', load);
			mod.once('initError', function() {
				mod.emit('error', 'Could not load google loader')
			});
		} else if (require('google')) {
			//google loader is there. use it
			load();
		} else if (loadingFailed) {
			mod.emit('error', 'Could not load google loader');
		} else {
			//not loading, no loading error, and not loaded. it must not have been initialized yet. Do that
			mod.once('initDone', load);
			mod.once('initError', function() {
				mod.emit('error', 'Could not load google loader')
			});
		}
	};
}


var loadScript = function(url, callback) {
	var script = document.createElement("script")
	script.type = "text/javascript";

	if (script.readyState) { //IE
		script.onreadystatechange = function() {
			if (script.readyState == "loaded" ||
				script.readyState == "complete") {
				script.onreadystatechange = null;
				callback();
			}
		};
	} else { //Others
		script.onload = function() {
			callback();
		};
	}

	script.src = url;
	document.body.appendChild(script);
}
loader.prototype = new EventEmitter;
module.exports = new loader();