// Used by lps_timeline_renderer.pl
var lpsTimelineCSSloaded; // global scope
function drawTimeline(T) {
	if (!( $.ajaxScript )) 
	return;
	// element where the Timeline will be attached
	var div  = $.ajaxScript.parent();
	var container = div.find('#lps_timeline');
	if (!container) alert('Could not create timeline HTML element!');
	var DOMcontainer = container.get(0);
    var tmo;

	// Times (cycles, states) are milliseconds since the epoch; vis accepts numbers as precisely that
	// If the above overflow policy gets ugly, use title:'tooltip' in items
	// For better selection you might combine colors below with .vis-item.vis-selected (?)
	// background items may be used to show time ranges!
	// id necessary for handling events, keep it strictly sync'ed with array order
	var json_payload = T ;
	var items = json_payload['items'];
	var groups =  json_payload['groups'];
	var simulatedRealTime = json_payload['simulatedRealTime']; // in seconds
	
	function displayedToLPStime(TT){
		if (!simulatedRealTime) return TT;
		return Math.round((TT-simulatedRealTime.begin*1000)/(simulatedRealTime.cycle*1000));
	}

	var minTime = Number.MAX_VALUE;
	var maxTime = Number.MIN_VALUE;

	for(i=0; i<items.length; i++){
		if(items[i].start<minTime)
			minTime = items[i].start;
		if(items[i].start>maxTime)
			maxTime = items[i].start;
		if(items[i].end && items[i].end>maxTime)
			maxTime = items[i].end;
	}
	// Configuration for the Timeline
	var options;
	
	
	/* fluents groups are their predicate names, and subgroups are the first argument values (see visual/2 in visualizer.P)
	Subgroups are currently not stacked, appearing on the same vertical level, but I was unable to order them:
	somehow subgroupOrderer didn't make an effect
	Ordering items per se, eg. http://visjs.org/examples/timeline/items/itemOrdering.html, was not useful */
		
	if (simulatedRealTime){
		// we need to change all items times to simulated times
		// note that visjs requires ms, not seconds!
		var SRTB = simulatedRealTime.begin*1000;
		var SRTC = simulatedRealTime.cycle*1000;
		maxTime = SRTB + maxTime*SRTC;
		minTime = SRTB;
		var duration = maxTime-minTime;
		options = {
			//autoResize : true,     seems to cause loop
			//width: Math.max(0.85*div.parents("div.answer").innerWidth(),100),   useless?
			min:minTime, max: minTime+Math.round(duration*1.15),
			showMajorLabels : false
			,orientation : {axis:'both'} // 'top' looks fine too
			//,timeAxis : {scale:'millisecond',step:1}
			//,format : {minorLabels:{millisecond:'x'}}
		};
		for(i=0; i<items.length; i++){
			items[i].start = SRTB + SRTC*items[i].start;
			var dateTip = "<br/>"+new Date(items[i].start).toISOString();
			if (items[i].end){
				items[i].end = SRTB + SRTC*items[i].end;
				dateTip += " to " + new Date(items[i].end).toISOString();
			}
			items[i].title += dateTip;
		}
	} else 
		options = {
			//autoResize : true,     seems to cause loop
			//width: Math.max(0.85*div.parents("div.answer").innerWidth(),100),   useless?
			min:minTime, max:maxTime+1,
			showMajorLabels : false
			,orientation : {axis:'both'} // 'top' looks fine too
			,timeAxis : {scale:'millisecond',step:1}
			,format : {minorLabels:{millisecond:'x'}}
		};
	

	var timeline;
	
	/* div.on("reactive-resize", function() {
	  console.log("resizing!");
	  if ( timeline ) {
	  	if (tmo) clearTimeout(tmo);
		tmo = setTimeout(function(){timeline.redraw();}, 1000);	
	  }
	}); */
  
  if (!lpsTimelineCSSloaded){
  	lpsTimelineCSSloaded = true;
    var link = document.createElement("link");
    link.type = "text/css";
    link.rel = "stylesheet";
    link.href = "/lps/bower_components/vis/dist/vis.min.css";
    document.getElementsByTagName("head")[0].appendChild(link);
  }
  require.config({waitSeconds : 180}); // for very low bandwidth clients
  require(["/lps/bower_components/vis/dist/vis.min.js"], function(vis) {

      // ... updateSize();
	  // Create a Timeline
	  timeline = new vis.Timeline(DOMcontainer, items, groups, options);
	  timeline.on('select', function (properties) {
			id = properties['items'];
			p = items[id].p;
			if (typeof p != 'undefined'){
				// select precedents
				p.push(id);
				timeline.setSelection(p, {focus: focus.checked});
			}
			// document.getElementById('log').innerHTML=""+id+" "+items[id].content+" "+items[id].p;
		});
		timeline.on('contextmenu', function (properties) {
			// show internal syntax for the clicked item:
			if (!properties.what || properties.what!=="item")
				return;
			id = properties['item'];
			var item = null;
			for(i=0; i<items.length; i++){
				if (items[i].id==id){
					item = items[i];
				}
			}
			if (!item) return;
			var F = item.group.indexOf("/");
			var literal;
			if (F!==-1){ // fluent
				literal = "holds("+item.group.substr(0,F)+"("+item.content+"),"+ displayedToLPStime(new Date(properties.time).getTime())+")";
			} else { // event or action
				var T2 = displayedToLPStime(item.start);
				literal = "happens("+item.content+","+(T2-1)+","+ T2 +")";
			}
			$(".prolog-query").queryEditor("setQuery","why("+literal+",Explanation).");
			properties.event.preventDefault();
	  	});
	  });
	  setTimeout(function(){
	  		$(".vis-minor",DOMcontainer).css('text-align', 'center'); // hack axis labels to be centered; working on visjs 4.17.0
	  		$(".prolog-runners").prologRunners('scrollToBottom');
	  }, 2000);
    
}
