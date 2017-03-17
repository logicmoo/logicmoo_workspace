:- module(lps_timeline,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module('../../swish/lib/render').

:- register_renderer(lps_timeline, "Timeline of a LPS execution").

% adapted from lib/render/c3.pl and com/declarativa/fiji/reporting/timelineTemplate.html
term_rendering(lps_visualization(T,_TwoD), _Vars, _Options) --> 
	{T=_}, % validate T?
	% to add export...:
	html(
		%TODO: add resizing! timeline widget dependent!!!!
		div([ /*class([ 'render-C3','export-dom', 'reactive-size']),*/ 'data-render'('As LPS timeline')],[
		div(id(lps_timeline), []),
		% And now our script:
		\js_script({|javascript(T)||
var lpsTimelineCSSloaded; // global scope
(function() {
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

	var minTime = Number.MAX_VALUE;
	var maxTime = Number.MIN_VALUE;

	for(i=0; i<items.length; i++){
		if(items[i].start<minTime)
			minTime = items[i].start;
		if(items[i].start>maxTime)
			maxTime = items[i].start;
		if(items[i].end>maxTime)
			maxTime = items[i].end;
	}
	// console.log("minTime:"+minTime+", maxTime:"+maxTime);
	// Configuration for the Timeline
	var options = {
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
	  });
	  setTimeout(function(){
	  		$(".vis-minor",DOMcontainer).css('text-align', 'center'); // hack axis labels to be centered; working on visjs 4.17.0
	  		$(".prolog-runners").prologRunners('scrollToBottom');
	  }, 2000);
    
})();
		|})
		
	])).

