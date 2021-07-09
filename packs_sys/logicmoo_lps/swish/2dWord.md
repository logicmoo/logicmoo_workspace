# 2d World Visualization of LPS Executions #
After executing a LPS program with go(X), X is bound to a (Prolog) term representing the full history of fluents and events over time; SWISH then displays that as a graphical timeline, using a [Javascript framework](http://visjs.org/docs/timeline/).

However, in some cases (mostly) fluents and (sometimes) events can have a "natural" representation in a two-dimensional space. In that case, by providing additional information - via a `display(Term,DisplaySpecification)` predicate - the LPS programmer can also obtain an animation of fluents and events over time.

For example:

* In the [game of life](https://bitbucket.org/lpsmasters/lps_corner/src/HEAD/examples/CLOUT_workshop/life.pl) cells are defined as part of a two-dimensional matrix
* In [bubble sort](https://bitbucket.org/lpsmasters/lps_corner/src/HEAD/examples/CLOUT_workshop/bubbleSort.pl) the array can be represented as an horizontal row of cells with the values being sorted, and the swap events can be depicted as bidirectional arrows
* In the [banking example](https://bitbucket.org/lpsmasters/lps_corner/src/HEAD/examples/CLOUT_workshop/bankTransfer.pl) the two balances may be depicted as rectangles that grow proportionally to the money value, and transfers as arrows between them.
* ...etc.

The examples above can be tried and seen on 2D at <http://lpsdemo.interprolog.com/example/lpsExamples.swinb>. The source code links above contains the display/2 predicate definitions, in some cases commented out.

# Principles for 2d world visualization #

After LPS execution finishes, fluent states in each cycle and simple events in cycle transitions are collected, "postmortem". All these terms are then matched to the display/2 predicate, to obtain their "display properties", a declarative representation which the Javascript side later renders  on the the screen, cycle after cycle, by using another [Javascript framework](http://paperjs.org) for vector oriented graphics.

Consider the fluent `balance(Person,Amount)`. The following display/2 clause makes a fluent value appear as a green rectangle, with position depending on the person and height equal to the value:

	display( balance(Person,V), 
		[ from:[X,0], to:[RightX,V], label:(Person:V), type:rectangle, 
		  fontSize:13, fillColor:green ]
	) 
	:- (Person=bob,X=50;Person=fariba,X=200),RightX is X+70.

You can see this at work by trying [banking example](http://lpsdemo.interprolog.com/example/bankTransfer.pl) ; make sure you uncomment the `display(...)` clauses if necessary. 

Use the video controls to pause or step through the animation. Clicking the top left corner of the animation lets you see the timeline as usual; LPS timelines and 2d worlds are implemented as SWISH Prolog answer [renderers](http://lpsdemo.interprolog.com/example/rendering.swinb).

In order to see your LPS program running in 2D, you need to write a few clauses for the display/2 predicate, specifying "display properties" for each fluent and/or event you wish to see. 

**NOTE: display/2 clauses must NOT use Prolog cuts nor if-then-elses, nor unbound arithmetic instructions - clauses should be calleable with all unbound head variables. Furthermore, display/2 must be deterministic when called with the first argument bound.**

Next we look at what "props" (for short) are available.


# About display/2 props #

So what can go into the second argument of `display(Term,DisplayProps)` ?
It can be a list of lists (each list defining one graphic object), or a simple list of the following properties:


Property  | Possible values | Comment / Example
------------- | -------------| ----
type | rectangle circle line ellipse arc raster regularPolygon star path pointtext text arrow| Mandatory. All provided by PaperJS directly, except arrow
id | Prolog term | optional unique identifier of the (displayed) term; the whole term will be taken as id by default
label  | Prolog term | the label is rendered in an arbitrary position; for precise positioning use a pointText/text object instead
sendToBack | none| forces the object to be behind others
bringToFront | none | draws the object in front
scale | float number | scale:0.1   reduce 10x
from | [X,Y] | a point, in pixels; the origin of coordinates is bottom left
to | [X,Y] |
point | [X,Y] |
size | [X,Y] |
radius | number | for type:circle, but also for type:rectangle (defines radius of round corners)
radius1 | number | for type:star
radius2 | number | for type:star
points | integer | for type:star
biDirectional | none | for type:arrow
fillColor|any HTML 5 style color| fillColor:'#85bb65'
strokeColor|ib.|strokeColor:black
strokeWidth|integer|width of pen
shadowColor|ib.|
opacity|0..1.0|opacity:1.0 is the default; lower values increase transparency
Others in paperjs|See <http://paperjs.org/reference/path/>|In general, any property accepted in a Path constructor will work

`display(...)` clauses can of course use Prolog predicates. The following is extracted from the [burning example](http://lpsdemo.interprolog.com/example/burning.pl) :

	% Center a translucid star on the spot:
	display(ignite(X,Y), [
		type:star,fillColor:red,center:[CX,CY],points:6,radius1:10,radius2:6,opacity:0.5
		]) :- pixels(X,Y,CX,CY).

	% convert our model coordinates to screen pixels
	pixels(X,Y,CX,CY) :- CX is X*20+10, CY is Y*20+10.

Some more notes: 

* all prop lists must become ground after any fluent or event matches them
* only the first display specification found for a fluent/event is considered
* if you don't see what you expect displayed, make sure you got no warnings on the SWISH window, and eventually take a look at your browser JavaScript console

# Adding time independent scenery #

Up to now we've been assuming that fluent and event values are situated in time, and display/2 transforms them into "renderable" props lists. But what if you want to have some constant background objects, which do not depend on time at all? You could simply create "dummy", constant fluents, and display these; but given how frequent this need is there's a simpler way: the `timeless` term. 

The following defines the (unchanging) background for the banking example:

	display(timeless,[ 
    	% a display spec can be a list of properties (for one object) or a list of lists (4 objects here:)
    	[type:star, center:[250,150], points:9, radius1:20, radius2:25, fillColor:yellow, sendToBack],
    	[type:rectangle, from:[0,0], to:[320,200], sendToBack, fillColor:[0,0.746,1]], % R,G,B
    	[type:ellipse, shadowOffset:5, shadowColor:darkGray , point:[50,150], size:[110, 40],fillColor: white],
    	[type:ellipse,  point:[20,130], size:[90, 30],fillColor: white ]
	]). 

Timeless component objects can have different ids, to allow for GUI event detection.

# Realtime display and input events #

Alternatively, the LPS program can be executed in background with the ```server(S)``` predicate. If the program contains display/2 clauses, a 2d view will appear in the query answers panel, displaying fluents and events incrementally (*or "lazily" as referred in the source code*) and in real time. Add a ```maxRealTime(Seconds)``` if you want to specify the time the program has to run (20 seconds being the default).

User clicks onto fluents and other objects (identified by their 'id' property - the whole object term by default) originate events:

```
lps_mouseup(ID,X,Y)
lps_mousedown(ID,X,Y)
lps_mousedrag(ID,X,Y)
```

You need to declare these as ```events``` as usual. See examples ```life\_lazyGUI.pl``` and ```badlight\_user.pl```. X and Y are pixel coordinates in the whole scene. 

Alt-clicking the scene pauses/resumes the LPS program execution, allowing the user (for example) to perform other clicks to appear as events in the next cycle. Notice that LPS cycle time is suspended, but not real time.

