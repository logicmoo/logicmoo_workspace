
:- expects_dialect(lps).

% A few examples of display specifications
% Now there's no need for dummy fluents anymore, just use display(timeless,...).

maxTime(5).

fluents background. initially background.
%display(background,[type:rectangle, fillColor:black, from:[0,0], to:[500,200], sendToBack]).

fluents line. initially line.
display(line,[type:line,  from: [0, 0], to: [300, 300], strokeColor: blue, strokeWidth:8]).

fluents path. initially path.
display(path,[type:path,  segments: [[20, 20], [80, 80], [140, 20]], strokeColor: red, strokeWidth:10, strokeCap:round]).

fluents circle. initially circle.
display(circle,[type:circle, fillColor:green, center:[50,200], radius:10, strokeColor:black]).

fluents foobar3. initially foobar3.
display(foobar3,[
    [type:circle, fillColor:orange, center:[20,20], radius:5, strokeColor:black],
    [type:circle, fillColor:pink, center:[200,20], radius:5, strokeColor:black]
  ]).

fluents rect. initially rect.
display(rect,[type:rectangle, fillColor:blue, from:[300,300], to:[320,320]]).

fluents text. initially text. % a little hack to avoid the need of specific "timeless" support:
display(text,[type:pointText, fillColor:orange, point:[50,50], content:'Hello world!', fontSize:25]).

fluents star. initially star.
display(star,[type:star, center:[100,100], points:7, radius1:25, radius2:40, fillColor:red]).

fluents hex. initially hex.
display(hex,[type:regularPolygon, center:[400,100], sides:6, radius:40, fillColor:gray, scale:3]).

fluents arc. initially arc.
display(arc,[type:arc,  from: [30, 20], through: [60, 20],to: [80, 80],strokeColor: black]).

fluents ellipse. initially ellipse.
display(ellipse,[type:ellipse,  point:[300,250], size:[120, 80],strokeColor: black]).

fluents arrow. initially arrow.
display(arrow,[type:arrow,  from:[10,200], to:[100, 250],strokeColor: green, strokeWidth:1, biDirectional]).

/** <examples>
?- go(Timeline).
*/

