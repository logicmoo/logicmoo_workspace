
:- expects_dialect(lps).

/*
This LPS program simulates a matrix of burning cells. 
A burning cell ignites neighbours towards one of several wind directions, 
given as a Prolog list of pairs (thus illustrating how we can use Prolog from LPS) */
maxTime(8).
fluents burning(_X,_Y).  actions ignite(_X,_Y).
ignite(X,Y) initiates burning(X,Y).
if burning(X,Y), member((DX,DY),[(1,1),(1,0),(0,1)]) /* wind vectors */
then 
	NX is X+DX, NY is Y+DY,
	ignite(NX,NY).
false ignite(X,Y), burning(X,Y).
observe ignite(5,5) from 1 to 2. % events are not admissible earlier

/* When a LPS program defines the display/2 ("display") predicate, a 2D world animation is show
display(T,Props) specifies the appearance of fluent or event T; Props is a list of 
display properties (or a list of such lists, thus defining a composite display object)
A props list must contain a type property (e.g. line, circle, and all other 
shaped paths referred in http://paperjs.org/reference/path/ ), plus enough properties
to specify the PaperJS object
More examples with 2d displaying: 
  /example/bankTransfer.pl, /example/bubbleSort.pl, /example/life.pl, /example/d_test.pl 

Use the video controls to pause or step through the animation. 
Clicking the top left corner of the animation lets you still see the timeline as usual */

% Visualize a burning spot, converting our model coordinates to screen pixels:
display(burning(X,Y), [type:circle,center:[CX,CY],radius:10,fillColor:yellow] ):- 
	pixels(X,Y,CX,CY).

% Events are displayed only briefly during state transitions
% Center a translucid star on the spot:
display(ignite(X,Y), [type:star,fillColor:red,center:[CX,CY],points:6,radius1:10,radius2:6,opacity:0.5]) :-
    pixels(X,Y,CX,CY).

% Screen size is based on fluents visible in the first cycle, so let's display a visual
% boundary from the start, for all time (ergo 'timeless'):
display(timeless, [
	[type:rectangle,from:[0,0],to:[200,200],strokeColor:green],
	[type:pointText, fillColor:black, point:[10,10], content:'Burning example', fontSize:14] 
	]).

% convert our model coordinates tp screen pixels
pixels(X,Y,CX,CY) :- CX is X*20+10, CY is Y*20+10.


/** <examples>
?- godc(Timeline). % 'godc' is an experimental variant of the main LPS interpreter (go)
*/