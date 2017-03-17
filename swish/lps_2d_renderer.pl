:- module(lps_2d,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module('../../swish/lib/render').

:- register_renderer(lps_2d, "2d world display of a LPS execution").

term_rendering(lps_visualization(_T,TwoD), _Vars, _Options) --> 
	{TwoD \= []},
	% TODO: add export
	html(
		div([ 'data-render'('As LPS 2D world')],[
		canvas([id(lps_2dworld)], []),
		\js_script({|javascript(TwoD)||

// TODO: miggrate the bulk of the following to another file, to avoid Prolog JS parsing

(function() {
	
	function killAllObjects(){
		console.log("killAllObjects");
		$.each(paperEvents,function(id,po){
			console.log("removing event "+id);
			removePaperObject(po);
			delete paperEvents[id];
		});
		$.each(paperFluents,function(id,po){
			console.log("removing fluent "+id);
			removePaperObject(po);
			delete paperFluents[id];
		});
		fluentProps = {};
	}
	
	function killEvents(cycle){ 
		// console.log("-- killEvents for cycle "+cycle);
		var cycleOps = cycles[cycle];
		for (var c=0; c<cycleOps.length; c++){
			var op = cycleOps[c];
			var id = op.kill;
			if (id && paperEvents[id]) {
				// console.log("removing EVENT "+id);
				removePaperObject(paperEvents[id]);
				delete paperEvents[id];
			}
		}
	}

	function createEvents(cycle){
		//console.log("-- createEvents for cycle "+cycle);
		var cycleOps = cycles[cycle];
		for (var c=0; c<cycleOps.length; c++){
			var op = cycleOps[c];
			var props = op.create;
			// console.log("--- op "+c+": "+JSON.stringify(op));
			if (props && getProp(props,'event')) {
				var id = getProp(props,'id');
				// console.log("creating EVENT "+id);
				paperEvents[id] = newPaperObject(props);
			}
		}
	}
	
	// if a composite object, returns only if found in first part
	function getProp(props,p){
		if (Array.isArray(props))
			return props[0][p];
		else return props[p];
	}
	
	function removePaperObject(po){
		if (Array.isArray(po))
			$.each(po,function(i,o){
				o.remove();
			});
		else po.remove();
	}

	function displayFluents(cycle){
		//console.log("-- displayFluents for cycle "+cycle);
		if (cycle>=cycles.length)
			console.log("ERROR: displayFluents for cycle "+cycle+ "...??");
		var cycleOps = cycles[cycle];
		for (var c=0; c<cycleOps.length; c++){
			var op = cycleOps[c];
			var props = op.create;
			//console.log("--- op "+c+": "+JSON.stringify(op));
			if (props){
				if (getProp(props,'event')) 
					continue;
				// fluent creation:
				var id = getProp(props,'id');
				// console.log("creating FLUENT "+id);
				paperFluents[id] = newPaperObject(props);
				fluentProps[id] = props;
			} else if (props=op.kill) {
				// props is id
				if (paperEvents[props]) // events are killed elsewhere
					continue;
				if (removeFluent(props)){
					delete paperFluents[props];
					delete fluentProps[props];
				}
			} else if (props=op.update){  // never happens for events
				// As we can't easily update incrementally props as specified on the logic side, we need to 
				// "brute force": update = kill + creation ; otherwise we'd need to ket the logic programmer 
				// know too much about scaling, translation etc.
				var ID = props;
				// console.log("Updating "+ID);
				/* For now we're not updating incrementally, commenting this out:
				var allProps = fluentProps[ID];
				Object.assign(allProps,op.newProps);*/
				var allProps = op.newProps;
				removeFluent(ID);
				paperFluents[ID] = newPaperObject(allProps);
				
			} else console.log("Unexpected op:"+JSON.stringify(props));
		}
	}
	
	function removeFluent(ID){
		var paperOb = paperFluents[ID]; // actually, may be an ARRAY of objects (for composites)
		if (!paperOb) {
			console.log("Unexpected non-fluent deletion:"+JSON.stringify(ID));
			return;
		}
		// console.log("removing "+paperOb);
		// console.log(JSON.stringify(paperOb));
		removePaperObject(paperOb);
		return true;
	}
	
	
	// returns a new Paper object or an array thereof
	// beware, props IS changed: some properties are removed locally to avoid conflict with PaperJS
	function newPaperObject(props){ 
		// mylog("Entering newPaperObject");
		if (Array.isArray(props)) {
			//console.log("COMPOSITE with "+props.length+" parts");
			var result = [];
			$.each(props,function(i,p){
				result.push(newPaperObject(p));
			});
			return result;
		} //else console.log("SIMPLE");
		
		var po = null;
		var type = props.type.toLowerCase();
		var id = props.id;
		var label = props.label;
		console.log("creating "+type+" from "+JSON.stringify(props));
		var postConfig = prepareProps(props);
		//console.log("hacked:"+JSON.stringify(props));
		if (type==="rectangle"){
			po = new paper.Path.Rectangle(props);
		} else if (type==="circle") {
			po = new paper.Path.Circle(props);
		} else if (type==="line") {
			po = new paper.Path.Line(props);
		} else if (type==="ellipse") {
			po = new paper.Path.Ellipse(props);
		} else if (type==="arc") {
			po = new paper.Path.Arc(props);
		} else if (type==="raster" || type==="image") {
			po = new paper.Raster(props);
			po.matrix.d = -1; // compensate for the view matrix
			rastersToLoad ++;
		} else if (type==="regularpolygon") {
			po = new paper.Path.RegularPolygon(props);
		} else if (type==="star") {
			po = new paper.Path.Star(props);
		} else if (type==="path") {
			po = new paper.Path(props);
		} else if (type==="pointtext" || type==="text") {
			po = new paper.PointText(props);
			po.matrix.d = -1; // compensate for the view matrix
		} // now custom objects:
		  else if (type==="arrow") {
			po = createArrow(props,id);
		} else {
			console.log("Don't know how to draw "+type);
			return po;
		}
		if (label && po.position){ // we'll wrap object and its label as a third object
			var textProps = {content:label, point: po.position };
			Object.assign(textProps,props);
			var text = new paper.PointText(textProps);
			text.position.x -= text.bounds.width/2; // could probably use justification:true... ;-)
			text.matrix.d = -1; // compensate for the view matrix
			if (po.bounds && text.bounds.height>=po.bounds.height) // compensate for narrow horizontal objects
				text.position.y += po.bounds.height;
			else text.position.y -= text.bounds.height/4;
			var background = po.fillColor;
			if (background){
				// http://stackoverflow.com/questions/12043187/how-to-check-if-hex-color-is-too-black
				var lum = 0.2126 * background.red + 0.7152 * background.green + 0.0722 * background.blue;
				if (lum < 0.16) // invert label color
					text.strokeColor = new paper.Color(1.0-background.red,1.0-background.green,1.0-background.blue);
				else text.strokeColor = 'black';
			} else text.strokeColor = 'black';
			var parts = [po, text];
			parts[1].bringToFront();
			var groupProps = {children:parts};
			Object.assign(groupProps,props);
			po = new paper.Group(groupProps);
		}
				
		if (po instanceof paper.Raster)
			po.onLoad = function(){
				postConfig(po);
				rastersToLoad --;
			}
		else postConfig(po); // set props that cannot be set on construction, e.g. via method calls
		// mylog("Leaving newPaperObject");
		return po;
	}
	
	// Destroys temporarily some props (for one simple object) that collide with PaperJS, 
	// and returns a function to invoke methods as needed on the new object
	// (and restore hacked properties)
	function prepareProps(props){
		var id = props.id;
		var type = props.type;
		delete props.id; // Hacky steps! Avoid conflict with paperjs
		delete props.type; 
		var sendToBack = props.sendToBack;
		delete props.sendToBack;
		var bringToFront = props.bringToFront;
		delete props.bringToFront;
		var scale = props.scale;
		delete props.scale;
		var rotate = props.rotate;
		delete props.rotate;
		var transform = props.transform;
		delete props.transform;
		return function(po){ // the Paper object created
			// console.log("Configuring object:"+JSON.stringify(po));
			// call configuration methods, and restore props
			props.id = id; props.type = type;
			if (po!==null){ 
				if (sendToBack) {
					po.sendToBack();
					props.sendToBack = true;
				}
				if (bringToFront) {
					po.bringToFront();
					props.bringToFront = true;
				}
				if (rotate) {
					if (props.rotationCenter) po.rotate(rotate,props.rotationCenter);
					else po.rotate(rotate);
					props.rotate = rotate;
				}
				if (scale) {
					po.scale(scale);
					props.scale = scale;
				}
				if (transform){
					if (scale){
						console.log("Can't specify both scale and transform, ignoring transform for "+id);
						return
					} else {
						po.transform(new paper.Matrix(transform));
						props.transform = transform;
					}
				}
				//console.log("Configured object:"+JSON.stringify(po));
				//console.log("Object named "+po+", area "+po.area+", visible:"+po.visible);
			}
		}
	}

	/** Create an arrow, with mandatory props from/to; other specific props:
		headLength, headAngle, biDirectional; strokeColor assumed black by default
	@param {Object} props for a simple object
	@param {String} id passed because it is not in props now
	*/
	function createArrow(props,id){ 
		// based on http://stackoverflow.com/questions/16991895/paperjs-to-draw-line-with-arrow

		var headAngle = props.headAngle || 150;
		var start = props.from, end = props.to;
		
		if (!start && !end){
			console.log("Could not create arrow "+id+", missing from: or to: ");
			return null;
		}
		
		var startPoint = new paper.Point(start), endPoint = new paper.Point(end);

		var tailVector = endPoint.subtract(startPoint);
		var length = tailVector.length;
		var headLength = props.headLength || Math.abs(length/10);
		var headLine = tailVector.normalize(headLength);
		var tailLine = headLine.multiply(-1);

		var parts = [
			new paper.Path([start, end]),
			new paper.Path([endPoint.add(headLine.rotate(headAngle)), endPoint, endPoint.add(headLine.rotate(-headAngle))])
		];
		if (props.biDirectional)
			parts.push(new paper.Path([startPoint.add(tailLine.rotate(headAngle)), startPoint, startPoint.add(tailLine.rotate(-headAngle))]));

		var groupProps = {children:parts};
		Object.assign(groupProps,props);
		if (!groupProps.strokeColor)
			groupProps.strokeColor = 'black';
		if (!groupProps.strokeWidth)
			groupProps.strokeWidth = (length<40 ? 1 : Math.round(tailVector.length/40));
		return new paper.Group(groupProps);
	}
	
	function buildControlPanel(){
		var al = paper.project.activeLayer;
		var nextX = 41+12+17-1;
		var cycleCaption = new paper.PointText({point:[nextX+36,10+4], content:"Cycle ...", strokeColor:'white', fontSize:16});
		cycleCaption.matrix.d = -1; // compensate for the view matrix
		var pauseButton = new paper.Group({children:[
			new paper.Path.Rectangle({point:[12+17,10],size:[4,16], fillColor:'white'}),
			new paper.Path.Rectangle({point:[12+17+4,10],size:[5,16], opacity:0.01, fillColor:'white'}), // dummy area to get mouse events
			new paper.Path.Rectangle({point:[12+17+9,10],size:[4,16], fillColor:'white'})
		]});
		var playButton = new paper.Group({children:[
			new paper.Path.Rectangle({point:[12+17,10],size:[13,16], opacity:0.01, fillColor:'white'}), // dummy area to get mouse events
			new paper.Path({fillColor:'white',segments:[[12+17,10],[12+17,26],[12+17+13,(26+10)/2]]}) //  13*16  bounds
		]});
		playButton.visible=false;
		var nextButton = new paper.Group({fillColor:'white', children:[
			new paper.Path({fillColor:'white',segments:[[nextX,10],[nextX,26],[nextX+13,(10+26)/2]]}), 
			new paper.Path.Rectangle({fillColor:'white',point:[nextX+13,10],size:[2,16]}) 
			 
		]});
		nextButton.visible=false;
		var progressWidth = paper.view.viewSize.width-12*2;
		var progressRect = new paper.Path.Rectangle({point:[12,36], size:[1,6], fillColor:'red'});
		var layer = new paper.Layer({
			children:[
				new paper.Path.Rectangle({fillColor:'black', opacity:0.4, from:[0,0],to:[paper.view.viewSize.width,36+6+10]}),
				new paper.Path.Rectangle({fillColor:'grey', point:[12,36], size:[progressWidth,6]}),
				cycleCaption, pauseButton, playButton, nextButton, progressRect
				]
		});
		layer.bringToFront(); // not really necessary, but...		
		setTimeout(function(){
			if (!userEntered) 
			layer.visible = false;
		}, 2000);
		al.activate();

		var userEntered = false;
		paper.view.onMouseEnter = function(e){
			layer.visible = true;
			userEntered = true;
		}
		paper.view.onMouseLeave = function(e){
			layer.visible = false;
			userEntered = false;
		}
		
		var self = {
			refresh:function(cycle,progress){ // progress: 0..1
				cycleCaption.content = "Cycle " + cycle + " / " + (cycles.length-1);
				var pw = progress*progressWidth;
				progressRect.bounds.size.width = (pw>0?pw:1);
			},
			pause:function(){
				pauseButton.visible=false; playButton.visible=true; nextButton.visible=true;
				paused = true;
				paper.view.onFrame = null;
			},
			resume:function(){
				if (cycle>=cycles.length) {
					killAllObjects();
					zeroCount = null;
				}
				playButton.visible=false; pauseButton.visible=true; nextButton.visible=false;
				paused = false;
				paper.view.onFrame = onFrame;
			},
			resumeUntilNext:function(){
				nextStop = (cycle+1) * framesPerCycle + zeroCount;
				playButton.visible=false; pauseButton.visible=true; nextButton.visible=false;
				paused = false;
				paper.view.onFrame = onFrame;
			},
			theEnd:function(){
				paper.view.onFrame = null; // stops the animation
				pauseButton.visible=false; playButton.visible=true; nextButton.visible=false;
			}
		};
		
		pauseButton.onMouseDown = function(e){
			self.pause();
		}
		playButton.onMouseDown = function(e){
			self.resume();
		}
		nextButton.onMouseDown = function(e){
			self.resumeUntilNext();
		}
		
		return self;
	}
	

	function onFrame(event) {
		if (zeroCount===null) {
			zeroCount = event.count;
			endOfTime = zeroCount + framesPerCycle * cycles.length;
			// console.log(cycles.length+" cycles, zeroCount:"+zeroCount+",endOfTime:"+endOfTime);
		}
		cycle = Math.floor ((event.count-zeroCount) / framesPerCycle);
		if (nextStop && event.count>=nextStop){
			console.log("event.count:"+event.count+", nextStop:"+nextStop);
			nextStop = null;
			controlPanel.pause();
		}
		if (paused) return;
		if (event.count>=endOfTime){
			// paper.view.pause(); pauses...
			// paper.view.onFrame = onFrame; // resumes it...
			controlPanel.theEnd();
			return;
		}
		
		var cycleStart = zeroCount + cycle * framesPerCycle;
		var lastAnimationEnd = cycleStart + animationHalfSlice;
		var nextAnimationStart = cycleStart + framesPerCycle - animationHalfSlice;
		
		if (controlPanel) 
			controlPanel.refresh(cycle, (event.count-zeroCount)/(endOfTime-zeroCount));
			// controlPanel.refresh(cycle, (cycle+1)/(cycles.length)); // visually refresh only on cycle change
		
		// console.log("event.count:"+event.count+", cycle:"+cycle+ ", cycleStart:"+cycleStart+", nextAnimationStart:"+nextAnimationStart);
		
		if (event.count===cycleStart && (EXECUTE_LAST_CYCLE||cycle<cycles.length-1)){
			displayFluents(cycle);
		} else if (event.count===lastAnimationEnd) {
			killEvents(cycle);
		} else if (event.count===nextAnimationStart) {
			createEvents(cycle);
		}
		
		if (!didResize) {
			// will resize our canvas after the first cycle with all objects displayed
			if (rastersToLoad>0) return;
			var bounds = new paper.Rectangle(0,0,0,0);
			$.each(paperFluents,function(id,po){
				if (Array.isArray(po)){
					$.each(po,function(i,o){
						bounds = bounds.unite(o.bounds);
					});
				}
				else bounds = bounds.unite(po.bounds);
			});
			$.each(paperEvents,function(id,po){
				if (Array.isArray(po)){
					$.each(po,function(i,o){
						bounds = bounds.unite(o.bounds);
					});
				}
				else bounds = bounds.unite(po.bounds);
			});
			//console.log("BOUNDS "+JSON.stringify(bounds));
			if (bounds.width === 0)
				return; // no displayed objects yet
			if (bounds.width<200)
				bounds.width = 200; // Minimum space for the video controls
			console.log("Found LPS 2d world bounds "+JSON.stringify(bounds));
			paper.view.viewSize.width = bounds.width+10;
			paper.view.viewSize.height = bounds.height+10;
			// make y ordinates grow from the bottom:
			paper.view.matrix.d = -1; paper.view.matrix.ty=bounds.height+10;
			didResize = true;
			controlPanel= buildControlPanel();
		}
		// the number of times the frame event was fired:
		// console.log(event.count);
		// The total amount of time passed since
		// the first frame event in seconds:
		// console.log(event.time);
		// The time passed in seconds since the last frame event:
		// console.log(event.delta);
	
	}
	
	var startOfMyLog = Date.now();
	function mylog(m){
    	var prefix = (Date.now()-startOfMyLog)+"mS: ";
    	console.log(prefix+m);
	}
	
	var zeroCount = null; // as obtained from event.count
	var didResize = false;
	var endOfTime; // ibidem
	var rastersToLoad = 0; // Raster objects not yet initialized/loaded
	var paperEvents = {}; // ID -> Paper object
	var paperFluents = {}; // ID -> Paper object
	var fluentProps = {}; // ID -> original creation props
	var EXECUTE_LAST_CYCLE = false; // Last cycle is a massive removal, may want to stop just before that
	var WHITE_COLOR;
	var ANIMATION_HALF_SLICE = 0.3; // percentage of cycle time at beginning and end of cycle (so animation slice is twice this)
	var controlPanel;
	var paused = false;
	var nextStop = null; // in frame (event) counts
	var cycle = 0; // current cycle, as determined by onFrame
	
	if (!( $.ajaxScript )) 
	return;
	mylog("Starting Javascript");
	var div  = $.ajaxScript.parent();
	var container = div.find('#lps_2dworld');
	if (!container) alert('Could not find 2d world HTML element!');
	var DOMcontainer = container.get(0);

	// array of arrays, one for each cycle; each cycle array has an operation object
	// The object properties may be a Javascript object (for a simple display object)
	// or an array of display object props, for composite objects
	var cycles = TwoD.cycles; 
	if (!cycles || cycles.length==0){
		console.log("No LPS cycles to display!");
		return;
	}
	if (cycles.length<2){
		console.log("Weird LPS cycles, less than 2!");
		return;
	}
	//console.log(JSON.stringify(cycles));
	for(var i=0;i<cycles.length;i++) console.log(JSON.stringify(cycles[i]));

	var framesPerCycle; // (LPS cycles)
	if (cycles.length < 15) framesPerCycle = 60;
	else if (cycles.length <50) framesPerCycle = 30;
	else framesPerCycle = 15;
	framesPerCycle = 60;
	console.log("framesPerCycle:"+framesPerCycle);
	var animationHalfSlice = Math.floor(ANIMATION_HALF_SLICE*framesPerCycle); // frames before and after a cycle transition
		
	// TODO? preprocess properties: replace Point, Size; scale view, with zoom or scale; possibly at cycle 0
	// This should probably be included differently...
    $.ajax({url:"/lps/bower_components/paper/dist/paper-core.js", dataType:"script", cache: true, success:function() {  
		mylog("Loaded paperjs");
    	WHITE_COLOR = new paper.Color(255,255,255);
		var canvas = DOMcontainer;
		// Create an empty project and a view for the canvas:
		paper.setup(canvas);
    	paper.view.onFrame = onFrame;
    } });
    
  })();
		|})
	])).

