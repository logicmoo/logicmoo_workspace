
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
       ^^new(picture('Pong')),
       ^^send(recogniser, handler(k, logtalk(player, up))),
       ^^send(recogniser, handler(j, logtalk(player, down))).

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

:- end_object.


:- object(graphical,
    extends(xpce_o)).

   :- public(colour/1).

   :- public(position/2).
   position(X, Y) :-
       ^^get(position, point(X, Y)).

   :- public(draw/0).
   draw :-
       self(Self),
       window::display(Self).

   :- public(draw/1).
   draw(Point) :-
       self(Self),
       window::display(Self, Point).

:- end_object.


:- object(background,
    extends(graphical)).

    colour(white).

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

   :- public([diameter/1, x_velocity/1, y_velocity/1]).
   diameter(20). % 15
   x_velocity(_XV_).
   y_velocity(_YV_).
   colour(black).

   init :-
       ::diameter(Diameter),
       ^^new(circle(Diameter)),
       ::colour(Colour),
       ^^send(fill_pattern, colour(Colour)),
       window::size(WW, WH),
       X is WW//2 - Diameter//2, Y is WH//2 - Diameter//2,
       ^^draw(point(X, Y)).

   :- public(update/1).
   update(ball(XV, YV)):-
       ::diameter(D),
       ^^position(X, Y),
       bounce(X, Y, XV, YV, D),
       NX is X + XV, NY is Y + YV,
       ^^draw(point(NX, NY)).

   bounce(X, Y, XV, _YV_, _D) :-
       extends_object(O, bat),
       O::point_collides(point(X, Y)),
       XV is -_XV_, !.
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


:- object(bat,
    extends(graphical)).

    :- private(move_step/1).
    :- public([start_size/2, start_pos/2]).
    start_size(40, 160).
    colour(blue).

    init :-
        ::colour(Colour),
        ::start_size(W, H),
        ::start_pos(X, Y),
        ^^new(box(W, H)),
        ^^send(fill_pattern, colour(Colour)),
        self(Self),
        window::display(Self, point(X, Y)).

    :- public(up/0).
    up :- 
        ^^position(X, Y),
        ::move_step(S),
        NY is Y - S,
        ^^send(position, point(X, NY)),
        ^^draw.

    :- public(down/0).
    down :-
        ^^position(X, Y),
        ::move_step(S),
        NY is Y + S,
        ^^send(position, point(X, NY)),
        ^^draw.

    :- public(point_collides/1).
    point_collides(point(X, Y)) :-
        ^^position(TX, TY),
        ^^size(W, H),
        BX is TX + W, BY is TY + H,
        between(TX, BX, X), between(TY, BY, Y).

:- end_object.


:- object(player,
    extends(bat)).
   start_pos(40, Y) :-
       background::size(_, H), Y is H//2.
   move_step(5).
   colour(green).

   :- public(update/1).
   update(Ball) :-
        window::size(Width, _), Margin is Width//2,
        Ball::position(BX, BY), BX > Margin,
        ^^position(_, Y),
	% format(user_error, "~q ", [BY]),
	1 is floor(BY) rem 3,
        ( Y > BY-60, ::up
        ; Y < BY+20, ::down
        ),
	^^draw. 
    update(_).

:- end_object.


:- object(computer,
    extends(bat),
    implements(animated)).
    start_pos(X, Y) :-
        background::size(W, H),
        X is W - 60,
        Y is H//2.
    move_step(4).

    update(Ball) :-
        window::size(Width, _), Margin is Width//2,
        Ball::position(BX, BY), BX > Margin,
        ^^position(_, Y),
        ( Y > BY-30, ::up
        ; Y < BY, ::down
        ).
    update(_).

:- end_object.


:- object(game).

   :- public(play/0).
   play :-
       window::init,
       window::open,
       sleep(0.2),
       background::init,
       ball(_, _)::init,
       player::init,
       computer::init,
       game_loop(ball(3, 1)).

   game_loop(Ball) :-
       Ball::update(NewBall),
       player::update(NewBall),
       computer::update(NewBall),
       sleep(0.001),
       game_loop(NewBall).

:- end_object.

