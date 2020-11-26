/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(plotter, []).
:- use_module(axis, []).
:- use_module(library(pce)).
:- require([ default/3
           ]).

:- pce_begin_class(plotter, device,
                   "Diagram for graphs and barcharts").

axis(P, Axe:plot_axis) :->
    "Associate an axis with the plotter"::
    get(Axe, type, Type),
    (   get(P, member, Type, Old)
    ->  free(Old)
    ;   true
    ),
    send(P, display, Axe),
    send(Axe, name, Type).

axis(P, Which:name, Axis:plot_axis) :<-
    "Get named axis"::
    get(P, member, Which, Axis).

x_axis(P, Axe:plot_axis) :<-
    "Find the X-axis (compatibility)"::
    get(P, member, x, Axe).

y_axis(P, Axe:plot_axis) :<-
    "Find the Y-axis (compatibility)"::
    get(P, member, y, Axe).

pixel_range(P, Dir:{x,y}, Range:tuple) :<-
    "Pixels covered in a direction"::
    (   Dir == x
    ->  get(P, x_axis, Axis)
    ;   get(P, y_axis, Axis)
    ),
    get(Axis, pixel_range, Range).

translate(P, X:'int|real', Y:'int|real', Point) :<-
    "Translate a coordinate"::
    get(P, member, x, XAxe),
    get(P, member, y, YAxe),
    get(XAxe, location, X, PX),
    get(YAxe, location, Y, PY),
    new(Point, point(PX, PY)).


translate_x(P, X:'int|real', Xpoint:int) :<-
    "Translate an X- coordinate"::
    get(P, member, x, XAxe),
    get(XAxe, location, X, Xpoint).
translate_y(P, Y:'int|real', Ypoint:int) :<-
    "Translate an Y- coordinate"::
    get(P, member, y, YAxe),
    get(YAxe, location, Y, Ypoint).

value_from_x(P, X:int, Value:'int|real') :<-
    "Translate X- coordinate to value"::
    get(P, member, x, XAxe),
    get(XAxe, value_from_coordinate, X, Value).
value_from_y(P, Y:int, Value:'int|real') :<-
    "Translate Y- coordinate to value"::
    get(P, member, y, YAxe),
    get(YAxe, value_from_coordinate, Y, Value).


translate_plot_point(P, PP:plot_point, Pt:point) :<-
    "Translate plot-point to physical point"::
    get(PP, x, X),
    get(PP, y, Y),
    get(P, translate, X, Y, Pt).

graph(P, Gr:plot_graph) :->
    "Display a graph on the plotter"::
    send(P, display, Gr).

clear(P) :->                            % TBD: how to decide what to remove?
    "Remove all graphs"::
    send(P?graphicals, for_all,
         if(or(message(@arg1, instance_of, plot_graph),
               message(@arg1, instance_of, plot_mark)),
            message(@arg1, free))).

modified_plot_axis(P, _A:[plot_axis]) :->
    "Trap changed axis parameters"::
    send(P?graphicals, for_all,
         if(message(@arg1, instance_of, plot_graph),
            message(@arg1, request_compute),
            if(message(@arg1, instance_of, plot_mark),
               message(@arg1, modified)))),
    send(P, expose_member, x),
    send(P, expose_member, y).

expose_member(P, Name:name) :->
    "Expose named member"::
    (   get(P, member, Name, Gr)
    ->  send(Gr, expose)
    ;   true
    ).

:- pce_end_class.

                 /*******************************
                 *          PLOT-POINT          *
                 *******************************/

:- pce_begin_class(plot_point(x_value, y_value), point, "Plotter point").

variable(modified,      bool := @on,    both,   "X/Y value is modified").
variable(x_value,       'int|real',     get,    "X-value").
variable(y_value,       'int|real',     get,    "Y-value").
variable(curve,         plot_graph,     get,    "Curve I'm associated with").

initialise(P, C:plot_graph, X:'x=int|real', Y:'y=int|real') :->
    "Create from X and Y"::
    send_super(P, initialise),
    send(P, slot, curve, C),
    send(P, slot, x_value, X),
    send(P, slot, y_value, Y),
    send(C, request_compute).

modified(P) :->
    "Indicate point and curve of modification"::
    (   get(P, slot, modified, @on)
    ->  true
    ;   send(P, slot, modified, @on),
        get(P, curve, Curve),
        send(Curve, request_compute)
    ).

compute(P) :->
    "Update the represented point"::
    (   get(P, modified, @on)
    ->  send(P, slot, modified, @off),
        get(P, curve, Curve),
        get(P, x_value, XVal),
        get(P, y_value, YVal),
        get(Curve, device, Plotter),
        get(Plotter, translate_x, XVal, X),
        get(Plotter, translate_y, YVal, Y),
        send(Curve, set_point, P, X, Y)
    ;   true
    ).

x(P, X:'int|real') :->
    send(P, slot, x_value, X),
    send(P, modified).
y(P, Y:'int|real') :->
    send(P, slot, y_value, Y),
    send(P, modified).

x(P, X:'int|real') :<-
    get(P, slot, x_value, X).
y(P, Y:'int|real') :<-
    get(P, slot, y_value, Y).

redundant(P) :->
    "Succeed if I'm right between my neighbours"::
    get(P, curve, Curve),
    get(Curve, kind, poly),
    get(Curve, points, Points),
    get(Points, previous, P, P1),
    get(Points, next, P, P2),
    get(P1, x, X1),
    get(P1, y, Y1),
    get(P2, x, X2),
    get(P2, y, Y2),
    get(P, x, X),
    get(P, y, Y),
    catch(abs(((Y-Y1)/(X-X1)) / ((Y2-Y1)/(X2-X1)) - 1) < 0.1,
          _,
          fail).

:- pce_end_class.

                 /*******************************
                 *          PLOT-GRAPH          *
                 *******************************/

:- pce_begin_class(plot_graph, path, "A graph for the plotter").

initialise(PG,
           Kind:'type=[{poly,smooth,points_only}]',
           Mark:'mark=[image]*') :->
    "Create from visualisation and mark"::
    default(Kind, poly, K),
    default(Mark, @nil, M),
    send_super(PG, initialise),
    send(PG, kind, K),
    send(PG, mark, M).


values(PG, Values:chain) :<-
    "Same as <-points"::
    get(PG, points, Values).


kind(PG, T:{poly,smooth,points_only}) :->
    (   T == points_only
    ->  send(PG, pen, 0)
    ;   send(PG, pen, 1),
        send_super(PG, kind, T)
    ).


append(PG, X:'x=int|real', Y:'y=int|real') :->
    "Append a plot_point to <-values"::
    send_super(PG, append, plot_point(PG, X, Y)).


compute(PG) :->
    "Update points"::
    send(PG?points, for_all, message(@arg1, compute)),
    send_super(PG, compute).

:- pce_end_class.


                 /*******************************
                 *           PLOT-MARK          *
                 *******************************/

:- pce_begin_class(plot_mark, device,
                   "Graphical mark on a graph").

variable(modified,      bool := @on,    none,   "X/Y value is modified").
variable(x_value,       'int|real',     get,    "X-value").
variable(y_value,       'int|real',     get,    "Y-value").

initialise(PM, X:x='int|real', Y:y='int|real', Img:image=[graphical]) :->
    send_super(PM, initialise),
    (   Img \== @default
    ->  send(PM, display, Img),
        send(Img, center, point(0,0))
    ;   true
    ),
    send(PM, slot, x_value, X),
    send(PM, slot, y_value, Y),
    send(PM, slot, modified, @on),
    send(PM, request_compute).

compute(PM) :->
    (   get(PM, slot, modified, @on),
        get(PM, device, Plotter),
        Plotter \== @nil
    ->  send(PM, slot, modified, @off),
        get(PM, x_value, XVal),
        get(PM, y_value, YVal),
        get(Plotter, translate_x, XVal, X),
        get(Plotter, translate_y, YVal, Y),
        send(PM, position, point(X,Y))
    ;   true
    ),
    send_super(PM, compute).

modified(P) :->
    "Indicate mark curve of modification"::
    (   get(P, slot, modified, @on)
    ->  true
    ;   send(P, slot, modified, @on),
        send(P, request_compute)
    ).

x_value(PM, X:'int|real') :->
    send(PM, slot, x_value, X),
    send(PM, modified).

y_value(PM, Y:'int|real') :->
    send(PM, slot, y_value, Y),
    send(PM, modified).

:- pce_end_class(plot_mark).
