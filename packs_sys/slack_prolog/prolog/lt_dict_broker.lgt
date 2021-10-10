

:- use_module(library(logtalk)).

% "shape" abstract class

:- object(shape,
	instantiates(abstract_class),
	specializes(object)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Generic geometric shape.']).

	:- public(color/1).
	:- mode(color(?atom), zero_or_one).
	:- info(color/1, [
		comment is 'Shape color.',
		argnames is ['Color']]).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Shape position.',
		argnames is ['X', 'Y']]).

	color(red).      % default shape color

	position(0, 0).  % default shape position

:- end_object.


% "polygon" abstract class

:- object(polygon,
    instantiates(abstract_class),
    specializes(shape)).

	:- info([
		author is 'Paulo Moura',
		version is 1.1,
		date is 2004/1/8,
		comment is 'Generic polygon.']).

	:- public(nsides/1).
	:- mode(nsides(?integer), zero_or_one).
	:- info(nsides/1, [
		comment is 'Polygon number of sides.',
		argnames is ['Number']]).

	:- public(area/1).
	:- mode(area(-float), zero_or_one).
	:- info(area/1, [
		comment is 'Polygon area.',
		argnames is ['Area']]).

	:- public(perimeter/1).
	:- mode(perimeter(?atom), zero_or_one).
	:- info(perimeter/1, [
		comment is 'Polygon perimeter.',
		argnames is ['Perimeter']]).

:- end_object.


% "regular_polygon" abstract class

:- object(regular_polygon,
    instantiates(abstract_class),
    specializes(polygon)).

	:- info([
		author is 'Paulo Moura',
		version is 1.1,
		date is 2004/1/8,
		comment is 'Generic regular polygon.']).

	:- public(side/1).
	:- mode(side(?atom), zero_or_one).
	:- info(side/1, [
		comment is 'Regular polygon side length.',
		argnames is ['Length']]).

	side(1).         % default side length

	perimeter(Perimeter) :-
		::nsides(Number),
		::side(Side),
		Perimeter is Number*Side.

:- end_object.


% "square" instantiable class

:- object(square,
    instantiates(class),
    specializes(regular_polygon)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Geometric square.']).

	nsides(4).

	area(Area) :-
		::side(Side),
		Area is Side*Side.

:- end_object.


:- object(q1,
    instantiates(square)).

	% inherits default values for position/2, color/1, and side/1

:- end_object.


:- object(q2,
    instantiates(square)).

	position(2, 3).

	color(blue).

	side(3).

:- end_object.

:- category(attributes).

    :- public(attribute/2).
    :- public(set_attribute/2).
    :- public(del_attribute/2).

    :- private(attribute_/2).
    :- dynamic(attribute_/2).

    attribute(Attribute, Value) :-
        ::attribute_(Attribute, Value).          % called in the context of "self"

    set_attribute(Attribute, Value) :-
        ::retractall(attribute_(Attribute, _)),  % retracts clauses in "self"
        ::assertz(attribute_(Attribute, Value)). % asserts clause in "self"

    del_attribute(Attribute, Value) :-
        ::retract(attribute_(Attribute, Value)). % retracts clause in "self"

:- end_category.


:- category(attributes).

    :- public(attribute/2).
    :- public(set_attribute/2).
    :- public(del_attribute/2).

    :- private(attribute_/2).
    :- dynamic(attribute_/2).

    attribute(Attribute, Value) :-
        attribute_(Attribute, Value).            % called in the context of "this"

    set_attribute(Attribute, Value) :-
        retractall(attribute_(Attribute, _)),    % retracts clauses in "this"
        assertz(attribute_(Attribute, Value)).   % asserts clause in "this"

    del_attribute(Attribute, Value) :-
        retract(attribute_(Attribute, Value)).   % retracts clause in "this"

:- end_category.

% An interpreter for object-oriented programs provided by
% Ivan Brakto in 'Prolog, Programming For Artificial Intelligence'
% with some minor patches to enable it to run on SWI-Prolog

:- op(600, xfy, ::).                            % send message to object

% use '::' operator as syntax for send message
Object::Message :-
   send(Object, Message).

% send(Message, Object): find Object's method and execute
send(Object, Message) :-
   get_methods(Object, Methods),                % Find Object's methods
   process(Message, Methods).                   % Execute corresponding method

% get the defined methods for the class
get_methods(Object, Methods) :-                 % Private methods
   object(Object, Methods).

% get the defined methods inherited from the superclass
get_methods(Object, Methods) :-                 % Inherited methods
   isa(Object, SuperObject),
   get_methods(SuperObject, Methods).

% process the method if it is defined as a fact
process(Message, [Message | _]) :-              % Use a fact
   process(Message, [Message | _]).

% process the method if it is defined as a rule
process(Message, [(Message :- Body) | _]) :-    % Use a rule
   call(Body).

process(Message, [_ | Methods]) :-              % break the message up
   process(Message, Methods).

/*
[ Return to language index ] { |one, step, back| }
The Shape Example in Prolog

Contributed by Chris Rathman
Code for Prolog
File: shape.prolog
*/

object(


   shape(X, Y), [
      (getx(X) :- X is X),

      (gety(Y) :- Y is Y),

      (moveto(_Shape, _X, _Y) :- fail),

      (rmoveto(_Shape, _X, _Y) :- fail),

      (draw :- fail)
   ]
).

/*
File: rectangle.prolog
*/
object(
   rectangle(X, Y, Width, Height), [
      (getwidth(W) :-
         W is Width),

      (getheight(H) :-
         H is Height),

      (setwidth(NewRectangle, NewWidth) :-
         NewRectangle = rectangle(X, Y, NewWidth, Height)),

      (setheight(NewRectangle, NewHeight) :-
         NewRectangle = rectangle(X, Y, Width, NewHeight)),

      (moveto(NewRectangle, NewX, NewY) :-
         NewRectangle = rectangle(NewX, NewY, Width, Height)),

      (rmoveto(NewRectangle, DeltaX, DeltaY) :-
         A is X + DeltaX,
         B is Y + DeltaY,
         NewRectangle = rectangle(A, B, Width, Height)),

      (draw :-
         write('Drawing a Rectangle at:('),
         write(X),
         write(','),
         write(Y),
         write('), width '),
         write(Width),
         write(', height '),
         write(Height),
         nl)
   ]
).

/*
% set rectangle to inherit from shape class
*/
isa(rectangle(X, Y, _WIDTH, _HEIGHT), shape(X, Y)).
% File: circle.prolog
object(
   circle(X, Y, Radius), [
      (getradius(R) :-
         R is Radius),

      (setradius(NewCircle, NewRadius) :-
         NewCircle = circle(X, Y, NewRadius)),

      (moveto(NewCircle, NewX, NewY) :-
         NewCircle = circle(NewX, NewY, Radius)),

      (rmoveto(NewCircle, DeltaX, DeltaY) :-
         A is X + DeltaX,
         B is Y + DeltaY,
         NewCircle = circle(A, B, Radius)),

      (draw :-
         write('Drawing a Circle at:('),
         write(X),
         write(','),
         write(Y),
         write('), radius '),
         write(Radius),
         nl)
   ]
).

% set circle to inherit from shape class
isa(circle(X, Y, _RADIUS), shape(X, Y)).
% File: polymorph.prolog
% iterate through a list and send message
drawloop([]) :- true.
drawloop([Shape|Tail]) :-
   Shape::draw,
   Shape::rmoveto(ShapeMoved, 100, 100),
   ShapeMoved::draw,
   drawloop(Tail).

polymorph :-
   % create a list containing various shape instances
   Scribble = [
      rectangle(10, 20, 5, 6),
      circle(15, 25, 8)],

   % iterate through the list and handle shapes polymorphically
   drawloop(Scribble),

   % handle rectangle and instance
   ARectangle = rectangle(0, 0, 15, 15),
   ARectangle::draw,
   ARectangle::setwidth(BRectangle, 30),
   BRectangle::draw.
/*
File: Main.prolog
?- consult('/articles/poly/oop.html').
?- consult('/articles/poly/polymorph.html').
?- polymorph
Output
Drawing a Rectangle at:(10,20), width 5, height 6
Drawing a Rectangle at:(110,120), width 5, height 6
Drawing a Circle at:(15,25), radius 8
Drawing a Circle at:(115,125), radius 8
Drawing a Rectangle at:(0,0), width 15, height 15
Drawing a Rectangle at:(0,0), width 30, height 15

Yes
[ Polyglot Polymorphism Index ]
Jim Weirich / jim@weirichhouse.org
*/


url,  %   A WebSocket Message Server URL.
self, %	The authenticated bot user.
team, %	Details on the authenticated user's team.
users, %	A hash of user objects by user ID.
channels, %	A hash of channel objects, one for every channel visible to the authenticated user.
groups, %	A hash of group objects, one for every group the authenticated user is in.
ims, %	A hash of IM objects, one for every direct message channel visible to the authenticated user.
bots, %	Details of the integrations set up on this team.
text, %	textual utils.
debug, %	Debugger fidling.
events, %	Registered callbacks.
files  %	registered storage.

object(
   shape(X, Y), [
      (getx(X) :- X is X),

      (gety(Y) :- Y is Y),

      (moveto(_Shape, _X, _Y) :- fail),

      (rmoveto(_Shape, _X, _Y) :- fail),

      (draw :- fail)
   ]
).
