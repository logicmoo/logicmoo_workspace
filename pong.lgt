:- object(xpce_o).

   :- include('xpce_includes.lgt').

   :- public(init/0).

   :- public(id/1).
   id(@ID) :-
       self(Self),
       functor(Self, ID, _).

   :- public(size/2).
   size(W, H) :-
       ::get(size, size(W, H)).

   :- public(new/1).
   new(O) :-
       ::id(ID),
       xnew(ID, O).

   :- public(get/2).
   get(P, O) :-
       ::id(ID),
       xget(ID, P, O).

   :- public(send/1).
   send(M) :-
       ::id(ID),
       xsend(ID, M).

   :- public(send/2).
   send(P, O) :-
       ::id(ID),
       xsend(ID, P, O).

   :- public(send/3).
   send(P, O, M) :-
       ::id(ID),
       xsend(ID, P, O, M).

   :- public(free/0).
   free :-
       ::id(ID),
       xfree(ID).

:- end_object.

:- object(window,
    extends(xpce_o)).

   init :-
       ^^new(picture('Pong')).

   :- public(open/0).
   open :-
       ^^send(open).

   :- public(display/1).
   display(O) :-
       O::position(X, Y),
       display(O, point(X, Y)).

   :- public(display/2).
   display(O, Point) :-
       O::id(ID),
       ^^send(display, ID, Point),
       O::send(flush).

   /*
   make_window :-
       ::xpce_id(W),
       ::bg_id(B),
       ::width(Width),
       ::height(Height),
       new(W, picture('Pong')),
       send(W, display, new(B, box(Width, Height))),
       send(B, fill_pattern, colour(black)),
       send(W, recogniser, handler(k, logtalk(bat, up))),
       send(W, recogniser, handler(j, logtalk(bat, down))).
   */


:- end_object.

:- object(graphical,
    extends(xpce_o)).

   :- public(position/2).
   position(X, Y) :-
       ^^get(position, point(X, Y)).

   :- public(draw/0).
   draw :-
       self(Self),
       window::display(Self).

:- end_object.

:- object(background,
    extends(graphical)).

    :- private(colour/1).
    colour(black).

    init :-
        ::colour(Colour),
        window::size(W, H),
        ^^new(box(W, H)),
        ^^send(fill_pattern, colour(Colour)),
        ^^draw.

:- end_object.

:- protocol(animated).
    :- public(update/1).
:- end_protocol.

:- object(ball(_XV_, _YV_),
    extends(graphical),
    implements(animated)).

   :- public([diameter/1, x_velocity/1, y_velocity/1, colour/1]).
   diameter(15).
   x_velocity(_XV_).
   y_velocity(_YV_).
   colour(white).

   init :-
       ::diameter(Diameter),
       ^^new(circle(Diameter)),
       ::colour(Colour),
       ^^send(fill_pattern, colour(Colour)),
       window::size(WW, WH),
       X is WW//2 - Diameter//2, Y is WH//2 - Diameter//2,
       self(Self),
       window::display(Self, point(X, Y)).

   :- public(update/1).
   update(ball(XV, YV)):-
       ::diameter(D),
       ^^position(X, Y),
       bounce(X, Y, XV, YV, D),
       NX is X + XV, NY is Y + YV,
       self(Self),
       window::display(Self, point(NX, NY)).

   /*
    *bounce(X, Y, XV, _YV_, _D) :-
    *    ( bat::rect(point(BTX, BTY), point(BBX, BBY))
    *    ; opponent::rect(point(BTX, BTY), point(BBX, BBY))
    *    ),
    *    between(BTX, BBX, X), between(BTY, BBY, Y),
    *    XV is -_XV_, !.
    */
   bounce(X, _Y, XV, _YV_, D) :-
       window::size(Width, _),
       (X < D ; X > Width - D),
       XV is -_XV_, !.
   bounce(_X, Y, _XV_, YV, D) :-
       window::size(_, Height),
       (Y < D ; Y > Height - D),
       YV is -_YV_, !.
   bounce(_X, _Y, _XV_, _YV_, _D).

:- end_object.

:- object(game).

   :- public(play/0).
   play :-
       window::init,
       window::open,
       sleep(0.1),
       background::init,
       ball(_, _)::init,
       %bat::make_bat,
       %opponent::make_bat,
       game_loop(ball(3, 1)).

   game_loop(Ball) :-
       Ball::update(NewBall),
       %opponent::update(NewBall),
       sleep(0.01),
       game_loop(NewBall).

:- end_object.

% EOF, preserving experimental code for features not yet implemented.

/*
:- object(bat).
   :- include('xpce_includes.lgt').

   :- public([xpce_id/1, x/1, y/1, width/1, height/1, rect/2, start_pos/2]).
   xpce_id(@bat).
   x(X) :- ::xpce_id(ID), get(ID, position, point(X, _)).
   y(Y) :- ::xpce_id(ID), get(ID, position, point(_, Y)).
   rect(point(TX, TY), point(BX, BY)) :-
       ::x(TX), ::y(TY), ::width(W), ::height(H),
       BX is TX + W,
       BY is TY + H.
   width(10).
   height(80).
   start_pos(40, 200).

   :- public(make_bat/0).
   make_bat :-
       window::xpce_id(W),
       ::xpce_id(B),
       ::width(Width), ::height(Height), ::start_pos(X, Y),
       send(W, display, new(B, box(Width, Height)), point(X, Y)),
       send(B, fill_pattern, colour(white)).

   :- public(up/0).
   up :-
      window::xpce_id(W),
      ::xpce_id(B),
      ::y(Y), ::x(X),
      NY is Y - 5,
      send(W, display, B, point(X, NY)),
      send(B, flush).
   :- public(down/0).
   down :-
      window::xpce_id(W),
      ::xpce_id(B),
      ::y(Y), ::x(X),
      NY is Y + 5,
      send(W, display, B, point(X, NY)),
      send(B, flush).

:- end_object.

:- object(opponent,
    extends(bat)).
   :- include('xpce_includes.lgt').

   xpce_id(@opp).
   start_pos(X, 200) :-
       window::width(W), X is W - 40.

   :- public(update/1).
   update(Ball) :-
       window::width(Width), Margin is Width//2,
       Ball::x(BX), BX > Margin,
       Ball::y(BY), ::y(Y), ::x(X),
       window::xpce_id(W), ::xpce_id(O),
       ( Y > BY-20, NY is Y - 2, send(W, display, O, point(X, NY))
       ; Y < BY, NY is Y + 2, send(W, display, O, point(X, NY))
       ), send(O, flush).
   update(_).

:- end_object.

*/

