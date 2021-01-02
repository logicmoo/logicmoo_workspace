# Episodic Memory 


# Douglas TODOs
```
true.

baseKB:  ?- mu:srv_mu.

% x(floyd,i1) @ somewhere: already about todo: look(x(floyd,i1))

% x(player,i1) @ somewhere: already about todo: look(x(player,i1))

% aXiom(look(x(floyd,i1))).

% aXiom(examine(
%          x(floyd,i1),
%          see,
%          in,
%          x(pantry,i1))).

% aXiom(sub__examine(x(floyd,i1),see,in,x(pantry,i1),3)).

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(1,16.7) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            look(x(player,i1))) ])).

% aXiom(look(x(player,i1))).

% aXiom(examine(
%          x(player,i1),
%          see,
%          in,
%          x(kitchen,i1))).

% aXiom(sub__examine(x(player,i1),see,in,x(kitchen,i1),3)).

episodic_mem(
   x(player,i1),
   timestamp(1,16.7)).
examine(
   x(player,i1),
   see,
   in,
   x(kitchen,i1)).
episodic_mem(
   x(player,i1),
   timestamp(1,16.7)).
sub__examine(x(player,i1),see,in,x(kitchen,i1),3).
episodic_mem(
   x(player,i1),
   timestamp(1,16.7)).
percept(
   x(player,i1),
   know,
   3,
   props(
      x(kitchen,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits(here),
        inherited(here),
        has_rel(exit(Exit8),t),
        inherited(place),
        inherited(kitchen) ])).
episodic_mem(
   x(player,i1),
   timestamp(1,16.7)).
percept(
   x(player,i1),
   see,
   3,
   props(
      x(kitchen,i1),
      [ shape=kitchen,
        volume_capacity=10000,
        desc="this is a place",
        has_rel(in,t),
        has_rel(exit(Exit8),t) ])).
episodic_mem(
   x(player,i1),
   timestamp(1,16.7)).
percept(
   x(player,i1),
   see,
   3,
   child_list(
      x(kitchen,i1),
      in,
      [ x(cabinate,i1),
        x(sink,i1),
        x(table,i1),
        x(screendoor,i1),
        x(player,i1),
        x(crate,i1) ])).
episodic_mem(
   x(player,i1),
   timestamp(1,16.7)).
percept(
   x(player,i1),
   see,
   3,
   exit_list(in,
      x(kitchen,i1),
      [west,north,south,east])).
timestamp is 1 ( 0s ago )
player does examine see in kitchen
timestamp is 1 ( 0s ago )
player does sub examine see in kitchen 3
timestamp is 1 ( 0s ago )
timestamp is 1 ( 0s ago )
(...verbose...: player sees the kitchen kitchen shaped , is large ,  "this is a place", thus, has an interior and can have exits. )
timestamp is 1 ( 0s ago )
Player sees in kitchen: {{ x(cabinate,i1) }} ,  {{ x(sink,i1) }} ,  {{ x(table,i1) }} ,  {{ x(screendoor,i1) }} and {{ x(crate,i1) }} .
timestamp is 1 ( 0s ago )
Exits in kitchen are: west , north , south and east.


% x(floyd,i1) @ somewhere: already about todo: sub__examine(x(floyd,i1),see,child,x(shelf,i1),2)

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(cabinate,i1),2)

% aXiom(sub__examine(x(floyd,i1),see,child,x(shelf,i1),2)).

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(2,16.7) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(cabinate,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(cabinate,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(2,16.7)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(cabinate,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits([untakeable,fully_corporial,thinkable,physical,furnature]),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(2,16.7)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(cabinate,i1),
      [ shape=cabinate,
        opened=f,
        has_rel(in,t),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(2,16.7)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(cabinate,i1),
      in,
      '<mystery>'(closed, in,
         x(cabinate,i1)))).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(cabinate,i1),
      on,
      [])).
timestamp is 2 ( 0s ago )
timestamp is 2 ( 0s ago )
(...verbose...: player sees the cabinate cabinate shaped , currently not opened , thus, has an interior and has a surface. )
timestamp is 2 ( 0.1s ago )
(...verbose...: cabinate is closed from seeing In )
(...verbose...: nothing on cabinate )

% x(floyd,i1) @ somewhere: already about todo: sub__examine(x(floyd,i1),see,child,x(locker,i1),2)

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(sink,i1),2)

% aXiom(sub__examine(x(floyd,i1),see,child,x(locker,i1),2)).

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(3,16.8) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(sink,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(sink,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(3,16.8)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(sink,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits([object,moveable,untakeable,fully_corporial,thinkable,physical,furnature]),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(3,16.8)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(sink,i1),
      [ shape=sink,
        opened=t,
        has_rel(in,t),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(3,16.8)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(sink,i1),
      in,
      [ x(plate,i1) ])).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(sink,i1),
      on,
      [])).
timestamp is 3 ( 0s ago )
timestamp is 3 ( 0s ago )
(...verbose...: player sees the sink sink shaped , opened , thus, has an interior and has a surface. )
timestamp is 3 ( 0s ago )
Player see in sink: {{ x(plate,i1) }} .
(...verbose...: nothing on sink )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(table,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(4,16.8) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(table,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(table,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(4,16.8)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(table,i1),
      [ has_rel(on,t),
        traits(physical),
        default_rel=on ])).
episodic_mem(
   x(player,i1),
   timestamp(4,16.8)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(table,i1),
      [ shape=(table),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(4,16.8)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(table,i1),
      on,
      [ x(box,i1),
        x(lamp,i1) ])).
timestamp is 4 ( 0s ago )
timestamp is 4 ( 0s ago )
(...verbose...: player sees the table table shaped and has a surface. )
timestamp is 4 ( 0s ago )
Player see on table: {{ x(box,i1) }} and {{ x(lamp,i1) }} .

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(screendoor,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(5,16.8) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(screendoor,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(screendoor,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(5,16.8)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(screendoor,i1),
      [ traits([untakeable,physical,furnature,thinkable,fully_corporial,door]),
        has_rel(on,t),
        default_rel=on ])).
episodic_mem(
   x(player,i1),
   timestamp(5,16.8)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(screendoor,i1),
      [ shape=screendoor,
        has_rel(on,t),
        opened=f ])).
episodic_mem(
   x(player,i1),
   timestamp(5,16.8)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(screendoor,i1),
      on,
      '<mystery>'(closed, on,
         x(screendoor,i1)))).
timestamp is 5 ( 0s ago )
timestamp is 5 ( 0s ago )
(...verbose...: player sees the screendoor screendoor shaped , has a surface and currently not opened. )
timestamp is 5 ( 0s ago )
(...verbose...: screendoor is closed from seeing On )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(crate,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(6,16.8) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(crate,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(crate,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(6,16.9)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(crate,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits([physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(6,16.9)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(crate,i1),
      [ shape=crate,
        opened=f,
        has_rel(in,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(6,16.9)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(crate,i1),
      in,
      '<mystery>'(closed, in,
         x(crate,i1)))).
timestamp is 6 ( 0s ago )
timestamp is 6 ( 0s ago )
(...verbose...: player sees the crate crate shaped , currently not opened and thus, has an interior. )
timestamp is 6 ( 0s ago )
(...verbose...: crate is closed from seeing In )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(plate,i1),1)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(7,16.9) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(plate,i1),1)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(plate,i1),1)).

episodic_mem(
   x(player,i1),
   timestamp(7,16.9)).
percept(
   x(player,i1),
   know,
   1,
   props(
      x(plate,i1),
      [ default_rel=on,
        traits([physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(7,16.9)).
percept(
   x(player,i1),
   see,
   1,
   props(
      x(plate,i1),
      [ shape=plate ])).
episodic_mem(
   x(player,i1),
   timestamp(7,16.9)).
percept(
   x(player,i1),
   see,
   1,
   child_list(
      x(plate,i1),
      on,
      [])).
timestamp is 7 ( 0s ago )
timestamp is 7 ( 0s ago )
( Extra verbose logic: extra_verbose_logic(notices(x(player,i1),see,1,[the(x(plate,i1)),'has shape is plate','.'])) )
timestamp is 7 ( 0s ago )
Player see on plate: <nothing>

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(box,i1),1)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(8,16.9) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(box,i1),1)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(box,i1),1)).

episodic_mem(
   x(player,i1),
   timestamp(8,16.9)).
percept(
   x(player,i1),
   know,
   1,
   props(
      x(box,i1),
      [ default_rel=in,
        traits([physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(8,16.9)).
percept(
   x(player,i1),
   see,
   1,
   props(
      x(box,i1),
      [ shape=box ])).
episodic_mem(
   x(player,i1),
   timestamp(8,16.9)).
percept(
   x(player,i1),
   see,
   1,
   child_list(
      x(box,i1),
      in,
      '<mystery>'(closed, in,
         x(box,i1)))).
timestamp is 8 ( 0s ago )
timestamp is 8 ( 0s ago )
( Extra verbose logic: extra_verbose_logic(notices(x(player,i1),see,1,[the(x(box,i1)),'has shape is box','.'])) )
timestamp is 8 ( 0s ago )
(...verbose...: box is closed from seeing In )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(lamp,i1),1)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(9,16.9) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(lamp,i1),1)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(lamp,i1),1)).

episodic_mem(
   x(player,i1),
   timestamp(9,16.9)).
percept(
   x(player,i1),
   know,
   1,
   props(
      x(lamp,i1),
      [ name="shiny brass lamp",
        traits([light,brass,shiny,physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(9,16.9)).
percept(
   x(player,i1),
   see,
   1,
   props(
      x(lamp,i1),
      [ shape=lamp ])).
timestamp is 9 ( 0s ago )
timestamp is 9 ( 0s ago )
( Extra verbose logic: extra_verbose_logic(notices(x(player,i1),see,1,[the(x(lamp,i1)),'has shape is lamp','.'])) )
x(player,i1)@[does]> s
s
episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(10,20.1) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            go_dir(
                               x(player,i1),
                               walk,
                               south)) ])).

% aXiom(go_dir(
%          x(player,i1),
%          walk,
%          south)).

episodic_mem(
   x(player,i1),
   timestamp(10,20.1)).
departing(
   x(player,i1),
   in,
   x(kitchen,i1),
   walk,
   south).
episodic_mem(
   x(player,i1),
   timestamp(10,20.1)).
arriving(
   x(player,i1),
   in,
   x(garden,i1),
   walk,
   north).
episodic_mem(
   x(player,i1),
   timestamp(10,20.1)).
episodic_mem(
   x(player,i1),
   success(go_dir(
              x(player,i1),
              walk,
              south))).
timestamp is 10 ( 0s ago )
player was in kitchen but left walking south
timestamp is 10 ( 0s ago )
player came walking north in garden
timestamp is 10 ( 0s ago )
( Success: walk south )
x(player,i1)@[does]> mem
mem
memories(
   x(player,i1),
   [ todo(
        x(player,i1),
        []),
     success(go_dir(
                x(player,i1),
                walk,
                south)),
     timestamp(10,20.1),
     h(in,x(player,i1),x(garden,i1)),
     h(exit(north),x(garden,i1),x(kitchen,i1)),
     h(exit(south),x(kitchen,i1),x(garden,i1)),
     h(exit(north),x(garden,i1),'<mystery>'(exit,north,x(garden,i1))),
     arriving(
        x(player,i1),
        in,
        x(garden,i1),
        walk,
        north),
     timestamp(10,20.1),
     departing(
        x(player,i1),
        in,
        x(kitchen,i1),
        walk,
        south),
     timestamp(10,20.1),
     attempts(
        x(player,i1),
        go_dir(
           x(player,i1),
           walk,
           south)),
     timestamp(10,20.1),
     props(
        x(lamp,i1),
        [ shape=lamp,
          traits([light,brass,shiny,physical,object,fully_corporial,thinkable,moveable]),
          name="shiny brass lamp" ]),
     percept(
        x(player,i1),
        see,
        1,
        props(
           x(lamp,i1),
           [ shape=lamp ])),
     timestamp(9,16.9),
     percept(
        x(player,i1),
        know,
        1,
        props(
           x(lamp,i1),
           [ name="shiny brass lamp",
             traits([light,brass,shiny,physical,object,fully_corporial,thinkable,moveable]) ])),
     timestamp(9,16.9),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(lamp,i1),1)),
     timestamp(9,16.9),
     h(in,'<mystery>'(closed,in,x(box,i1)),x(box,i1)),
     percept(
        x(player,i1),
        see,
        1,
        child_list(
           x(box,i1),
           in,
           '<mystery>'(closed, in,
              x(box,i1)))),
     timestamp(8,16.9),
     props(
        x(box,i1),
        [ shape=box,
          traits([physical,object,fully_corporial,thinkable,moveable]),
          default_rel=in ]),
     percept(
        x(player,i1),
        see,
        1,
        props(
           x(box,i1),
           [ shape=box ])),
     timestamp(8,16.9),
     percept(
        x(player,i1),
        know,
        1,
        props(
           x(box,i1),
           [ default_rel=in,
             traits([physical,object,fully_corporial,thinkable,moveable]) ])),
     timestamp(8,16.9),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(box,i1),1)),
     timestamp(8,16.9),
     percept(
        x(player,i1),
        see,
        1,
        child_list(
           x(plate,i1),
           on,
           [])),
     timestamp(7,16.9),
     props(
        x(plate,i1),
        [ shape=plate,
          traits([physical,object,fully_corporial,thinkable,moveable]),
          default_rel=on ]),
     percept(
        x(player,i1),
        see,
        1,
        props(
           x(plate,i1),
           [ shape=plate ])),
     timestamp(7,16.9),
     percept(
        x(player,i1),
        know,
        1,
        props(
           x(plate,i1),
           [ default_rel=on,
             traits([physical,object,fully_corporial,thinkable,moveable]) ])),
     timestamp(7,16.9),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(plate,i1),1)),
     timestamp(7,16.9),
     h(in,'<mystery>'(closed,in,x(crate,i1)),x(crate,i1)),
     percept(
        x(player,i1),
        see,
        2,
        child_list(
           x(crate,i1),
           in,
           '<mystery>'(closed, in,
              x(crate,i1)))),
     timestamp(6,16.9),
     props(
        x(crate,i1),
        [ opened=f,
          shape=crate,
          traits([physical,object,fully_corporial,thinkable,moveable]),
          has_rel(in,t),
          default_rel=in ]),
     percept(
        x(player,i1),
        see,
        2,
        props(
           x(crate,i1),
           [ shape=crate,
             opened=f,
             has_rel(in,t) ])),
     timestamp(6,16.9),
     percept(
        x(player,i1),
        know,
        2,
        props(
           x(crate,i1),
           [ default_rel=in,
             has_rel(in,t),
             traits([physical,object,fully_corporial,thinkable,moveable]) ])),
     timestamp(6,16.9),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(crate,i1),2)),
     timestamp(6,16.8),
     h(on,'<mystery>'(closed,on,x(screendoor,i1)),x(screendoor,i1)),
     percept(
        x(player,i1),
        see,
        2,
        child_list(
           x(screendoor,i1),
           on,
           '<mystery>'(closed, on,
              x(screendoor,i1)))),
     timestamp(5,16.8),
     props(
        x(screendoor,i1),
        [ opened=f,
          shape=screendoor,
          default_rel=on,
          has_rel(on,t),
          traits([untakeable,physical,furnature,thinkable,fully_corporial,door]) ]),
     percept(
        x(player,i1),
        see,
        2,
        props(
           x(screendoor,i1),
           [ shape=screendoor,
             has_rel(on,t),
             opened=f ])),
     timestamp(5,16.8),
     percept(
        x(player,i1),
        know,
        2,
        props(
           x(screendoor,i1),
           [ traits([untakeable,physical,furnature,thinkable,fully_corporial,door]),
             has_rel(on,t),
             default_rel=on ])),
     timestamp(5,16.8),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(screendoor,i1),2)),
     timestamp(5,16.8),
     h(on,x(lamp,i1),x(table,i1)),
     h(on,x(box,i1),x(table,i1)),
     percept(
        x(player,i1),
        see,
        2,
        child_list(
           x(table,i1),
           on,
           [ x(box,i1),
             x(lamp,i1) ])),
     timestamp(4,16.8),
     props(
        x(table,i1),
        [ shape=(table),
          default_rel=on,
          traits(physical),
          has_rel(on,t) ]),
     percept(
        x(player,i1),
        see,
        2,
        props(
           x(table,i1),
           [ shape=(table),
             has_rel(on,t) ])),
     timestamp(4,16.8),
     percept(
        x(player,i1),
        know,
        2,
        props(
           x(table,i1),
           [ has_rel(on,t),
             traits(physical),
             default_rel=on ])),
     timestamp(4,16.8),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(table,i1),2)),
     timestamp(4,16.8),
     percept(
        x(player,i1),
        see,
        2,
        child_list(
           x(sink,i1),
           on,
           [])),
     h(in,x(plate,i1),x(sink,i1)),
     percept(
        x(player,i1),
        see,
        2,
        child_list(
           x(sink,i1),
           in,
           [ x(plate,i1) ])),
     timestamp(3,16.8),
     props(
        x(sink,i1),
        [ opened=t,
          shape=sink,
          has_rel(on,t),
          traits([object,moveable,untakeable,fully_corporial,thinkable,physical,furnature]),
          has_rel(in,t),
          default_rel=in ]),
     percept(
        x(player,i1),
        see,
        2,
        props(
           x(sink,i1),
           [ shape=sink,
             opened=t,
             has_rel(in,t),
             has_rel(on,t) ])),
     timestamp(3,16.8),
     percept(
        x(player,i1),
        know,
        2,
        props(
           x(sink,i1),
           [ default_rel=in,
             has_rel(in,t),
             traits([object,moveable,untakeable,fully_corporial,thinkable,physical,furnature]),
             has_rel(on,t) ])),
     timestamp(3,16.8),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(sink,i1),2)),
     timestamp(3,16.8),
     percept(
        x(player,i1),
        see,
        2,
        child_list(
           x(cabinate,i1),
           on,
           [])),
     h(in,'<mystery>'(closed,in,x(cabinate,i1)),x(cabinate,i1)),
     percept(
        x(player,i1),
        see,
        2,
        child_list(
           x(cabinate,i1),
           in,
           '<mystery>'(closed, in,
              x(cabinate,i1)))),
     timestamp(2,16.7),
     props(
        x(cabinate,i1),
        [ opened=f,
          shape=cabinate,
          has_rel(on,t),
          traits([untakeable,fully_corporial,thinkable,physical,furnature]),
          has_rel(in,t),
          default_rel=in ]),
     percept(
        x(player,i1),
        see,
        2,
        props(
           x(cabinate,i1),
           [ shape=cabinate,
             opened=f,
             has_rel(in,t),
             has_rel(on,t) ])),
     timestamp(2,16.7),
     percept(
        x(player,i1),
        know,
        2,
        props(
           x(cabinate,i1),
           [ default_rel=in,
             has_rel(in,t),
             traits([untakeable,fully_corporial,thinkable,physical,furnature]),
             has_rel(on,t) ])),
     timestamp(2,16.7),
     attempts(
        x(player,i1),
        sub__examine(x(player,i1),see,child,x(cabinate,i1),2)),
     timestamp(2,16.7),
     h(exit(east),x(kitchen,i1),'<mystery>'(exit,east,x(kitchen,i1))),
     h(exit(south),x(kitchen,i1),'<mystery>'(exit,south,x(kitchen,i1))),
     h(exit(north),x(kitchen,i1),'<mystery>'(exit,north,x(kitchen,i1))),
     h(exit(west),x(kitchen,i1),'<mystery>'(exit,west,x(kitchen,i1))),
     percept(
        x(player,i1),
        see,
        3,
        exit_list(in,
           x(kitchen,i1),
           [west,north,south,east])),
     timestamp(1,16.7),
     h(in,x(crate,i1),x(kitchen,i1)),
     h(in,x(screendoor,i1),x(kitchen,i1)),
     h(in,x(table,i1),x(kitchen,i1)),
     h(in,x(sink,i1),x(kitchen,i1)),
     h(in,x(cabinate,i1),x(kitchen,i1)),
     percept(
        x(player,i1),
        see,
        3,
        child_list(
           x(kitchen,i1),
           in,
           [ x(cabinate,i1),
             x(sink,i1),
             x(table,i1),
             x(screendoor,i1),
             x(player,i1),
             x(crate,i1) ])),
     timestamp(1,16.7),
     props(
        x(kitchen,i1),
        [ desc="this is a place",
          volume_capacity=10000,
          shape=kitchen,
          inherited(kitchen),
          inherited(place),
          has_rel(exit(Exit8),t),
          inherited(here),
          traits(here),
          has_rel(in,t),
          default_rel=in ]),
     percept(
        x(player,i1),
        see,
        3,
        props(
           x(kitchen,i1),
           [ shape=kitchen,
             volume_capacity=10000,
             desc="this is a place",
             has_rel(in,t),
             has_rel(exit(Exit9),t) ])),
     timestamp(1,16.7),
     percept(
        x(player,i1),
        know,
        3,
        props(
           x(kitchen,i1),
           [ default_rel=in,
             has_rel(in,t),
             traits(here),
             inherited(here),
             has_rel(exit(Exit10),t),
             inherited(place),
             inherited(kitchen) ])),
     timestamp(1,16.7),
     sub__examine(x(player,i1),see,in,x(kitchen,i1),3),
     timestamp(1,16.7),
     examine(
        x(player,i1),
        see,
        in,
        x(kitchen,i1)),
     timestamp(1,16.7),
     attempts(
        x(player,i1),
        look(x(player,i1))),
     timestamp(1,16.7),
     propOf(memories,
        x(player,i1)),
     structure_label(mem(x(player,i1))),
     timestamp(0,16.5),
     current_goals(
        x(player,i1),
        []),
     goals_skipped(
        x(player,i1),
        []),
     goals_satisfied(
        x(player,i1),
        []),
     inst(x(player,i1)) ]).
x(player,i1)@[does]>
% aXiom(open(
%          x(floyd,i1),
%          x(locker,i1))).

% aXiom(change_state(
%          x(floyd,i1),
%          open(
%             x(floyd,i1),
%             x(locker,i1)),
%          x(locker,i1),
%          opened=t)).

% x(floyd,i1) @ somewhere: already about todo: examine(x(floyd,i1),see,x(locker,i1))

x(player,i1)@[does]>
% aXiom(examine(
%          x(floyd,i1),
%          see,
%          x(locker,i1))).

% aXiom(sub__examine(x(floyd,i1),see,at,x(locker,i1),3)).

x(player,i1)@[does]>
% aXiom(go_dir(
%          x(floyd,i1),
%          walk,
%          south)).

x(player,i1)@[does]>
% aXiom(go_dir(
%          x(floyd,i1),
%          walk,
%          north)).

% x(floyd,i1) @ somewhere: already about todo: go_dir(x(floyd,i1),walk,south)

x(player,i1)@[does]>
% aXiom(go_dir(
%          x(floyd,i1),
%          walk,
%          south)).

x(player,i1)@[does]> n
n
episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(11,52.2) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            go_dir(
                               x(player,i1),
                               walk,
                               north)) ])).

% aXiom(go_dir(
%          x(player,i1),
%          walk,
%          north)).

episodic_mem(
   x(player,i1),
   timestamp(11,52.2)).
departing(
   x(player,i1),
   in,
   x(garden,i1),
   walk,
   north).
episodic_mem(
   x(player,i1),
   timestamp(11,52.2)).
arriving(
   x(player,i1),
   in,
   x(kitchen,i1),
   walk,
   south).
episodic_mem(
   x(player,i1),
   timestamp(11,52.3)).
episodic_mem(
   x(player,i1),
   success(go_dir(
              x(player,i1),
              walk,
              north))).
timestamp is 11 ( 0.1s ago )
player was in garden but left walking north
timestamp is 11 ( 0.1s ago )
player came walking south in kitchen
timestamp is 11 ( 0s ago )
( Success: walk north )
x(player,i1)@[does]> look
look
episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(12,57) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            look(x(player,i1))) ])).

% aXiom(look(x(player,i1))).

% aXiom(examine(
%          x(player,i1),
%          see,
%          in,
%          x(kitchen,i1))).

% aXiom(sub__examine(x(player,i1),see,in,x(kitchen,i1),3)).

episodic_mem(
   x(player,i1),
   timestamp(12,57)).
examine(
   x(player,i1),
   see,
   in,
   x(kitchen,i1)).
episodic_mem(
   x(player,i1),
   timestamp(12,57)).
sub__examine(x(player,i1),see,in,x(kitchen,i1),3).
episodic_mem(
   x(player,i1),
   timestamp(12,57)).
percept(
   x(player,i1),
   know,
   3,
   props(
      x(kitchen,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits(here),
        inherited(here),
        has_rel(exit(Exit8),t),
        inherited(place),
        inherited(kitchen) ])).
episodic_mem(
   x(player,i1),
   timestamp(12,57)).
percept(
   x(player,i1),
   see,
   3,
   props(
      x(kitchen,i1),
      [ shape=kitchen,
        volume_capacity=10000,
        desc="this is a place",
        has_rel(in,t),
        has_rel(exit(Exit8),t) ])).
episodic_mem(
   x(player,i1),
   timestamp(12,57)).
percept(
   x(player,i1),
   see,
   3,
   child_list(
      x(kitchen,i1),
      in,
      [ x(player,i1),
        x(cabinate,i1),
        x(sink,i1),
        x(table,i1),
        x(screendoor,i1),
        x(crate,i1),
        x(floyd,i1) ])).
episodic_mem(
   x(player,i1),
   timestamp(12,57)).
percept(
   x(player,i1),
   see,
   3,
   exit_list(in,
      x(kitchen,i1),
      [west,north,south,east])).
timestamp is 12 ( 0s ago )
player does examine see in kitchen
timestamp is 12 ( 0s ago )
player does sub examine see in kitchen 3
timestamp is 12 ( 0s ago )
timestamp is 12 ( 0s ago )
(...verbose...: player sees the kitchen kitchen shaped , is large ,  "this is a place", thus, has an interior and can have exits. )
timestamp is 12 ( 0s ago )
Player sees in kitchen: {{ x(cabinate,i1) }} ,  {{ x(sink,i1) }} ,  {{ x(table,i1) }} ,  {{ x(screendoor,i1) }} ,  {{ x(crate,i1) }} and {{ x(floyd,i1) }} .
timestamp is 12 ( 0s ago )
Exits in kitchen are: west , north , south and east.


% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(cabinate,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(13,57.1) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(cabinate,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(cabinate,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(13,57.1)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(cabinate,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits([untakeable,fully_corporial,thinkable,physical,furnature]),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(13,57.1)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(cabinate,i1),
      [ shape=cabinate,
        opened=f,
        has_rel(in,t),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(13,57.1)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(cabinate,i1),
      in,
      '<mystery>'(closed, in,
         x(cabinate,i1)))).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(cabinate,i1),
      on,
      [])).
timestamp is 13 ( 0.1s ago )
timestamp is 13 ( 0.1s ago )
(...verbose...: player sees the cabinate cabinate shaped , currently not opened , thus, has an interior and has a surface. )
timestamp is 13 ( 0.1s ago )
(...verbose...: cabinate is closed from seeing In )
(...verbose...: nothing on cabinate )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(sink,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(14,57.3) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(sink,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(sink,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(14,57.3)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(sink,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits([object,moveable,untakeable,fully_corporial,thinkable,physical,furnature]),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(14,57.3)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(sink,i1),
      [ shape=sink,
        opened=t,
        has_rel(in,t),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(14,57.3)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(sink,i1),
      in,
      [ x(plate,i1) ])).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(sink,i1),
      on,
      [])).
timestamp is 14 ( 0.1s ago )
timestamp is 14 ( 0.1s ago )
(...verbose...: player sees the sink sink shaped , opened , thus, has an interior and has a surface. )
timestamp is 14 ( 0.1s ago )
Player see in sink: {{ x(plate,i1) }} .
(...verbose...: nothing on sink )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(table,i1),2)

% aXiom(emote(
%          x(floyd,i1),
%          act,
%          *,
%          [ cap(subj(x(floyd,i1))),
%            'hums quietly to themself.' ])).

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(15,57.5) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(table,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(table,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(15,57.4)).
episodic_mem(
   x(player,i1),
   emoted(
      x(floyd,i1),
      act,
      *,
      [ cap(subj(x(floyd,i1))),
        'hums quietly to themself.' ])).
episodic_mem(
   x(player,i1),
   timestamp(15,57.5)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(table,i1),
      [ has_rel(on,t),
        traits(physical),
        default_rel=on ])).
episodic_mem(
   x(player,i1),
   timestamp(15,57.5)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(table,i1),
      [ shape=(table),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(15,57.5)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(table,i1),
      on,
      [ x(box,i1),
        x(lamp,i1) ])).
timestamp is 15 ( 0.1s ago )
happened: floyd does emote act * Floyd hums quietly to themself.
timestamp is 15 ( 0s ago )
timestamp is 15 ( 0s ago )
(...verbose...: player sees the table table shaped and has a surface. )
timestamp is 15 ( 0s ago )
Player see on table: {{ x(box,i1) }} and {{ x(lamp,i1) }} .

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(screendoor,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(16,57.6) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(screendoor,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(screendoor,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(16,57.6)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(screendoor,i1),
      [ traits([untakeable,physical,furnature,thinkable,fully_corporial,door]),
        has_rel(on,t),
        default_rel=on ])).
episodic_mem(
   x(player,i1),
   timestamp(16,57.6)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(screendoor,i1),
      [ shape=screendoor,
        has_rel(on,t),
        opened=f ])).
episodic_mem(
   x(player,i1),
   timestamp(16,57.6)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(screendoor,i1),
      on,
      '<mystery>'(closed, on,
         x(screendoor,i1)))).
timestamp is 16 ( 0.1s ago )
timestamp is 16 ( 0.1s ago )
(...verbose...: player sees the screendoor screendoor shaped , has a surface and currently not opened. )
timestamp is 16 ( 0.1s ago )
(...verbose...: screendoor is closed from seeing On )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(crate,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(17,57.8) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(crate,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(crate,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(17,57.8)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(crate,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits([physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(17,57.8)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(crate,i1),
      [ shape=crate,
        opened=f,
        has_rel(in,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(17,57.8)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(crate,i1),
      in,
      '<mystery>'(closed, in,
         x(crate,i1)))).
timestamp is 17 ( 0s ago )
timestamp is 17 ( 0s ago )
(...verbose...: player sees the crate crate shaped , currently not opened and thus, has an interior. )
timestamp is 17 ( 0s ago )
(...verbose...: crate is closed from seeing In )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(floyd,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(18,57.9) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(floyd,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(floyd,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(18,57.9)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(floyd,i1),
      [ name="Floyd the robot",
        traits([metallic,physical,object,moveable,shiny,perceptive,autolook,fully_corporial,thinkable,noncorporial,partly_noncorporial,robot]),
        has_rel(worn_by,t),
        has_rel(held_by,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(18,57.9)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(floyd,i1),
      [ shape=floyd,
        emitting(see,light),
        inherited(shiny),
        has_rel(worn_by,t),
        has_rel(held_by,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(18,57.9)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(floyd,i1),
      worn_by,
      [])).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(floyd,i1),
      held_by,
      [ x(wrench,i1) ])).
timestamp is 18 ( 0.1s ago )
timestamp is 18 ( 0.1s ago )
(...verbose...: player sees the floyd floyd shaped , is glowing , inherits shiny! , can be dressed up and can hold objects. )
timestamp is 18 ( 0.1s ago )
(...verbose...: nothing worn by floyd )
Player see held by floyd: {{ x(wrench,i1) }} .

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(plate,i1),1)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(19,58.1) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(plate,i1),1)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(plate,i1),1)).

episodic_mem(
   x(player,i1),
   timestamp(19,58.1)).
percept(
   x(player,i1),
   know,
   1,
   props(
      x(plate,i1),
      [ default_rel=on,
        traits([physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(19,58.1)).
percept(
   x(player,i1),
   see,
   1,
   props(
      x(plate,i1),
      [ shape=plate ])).
episodic_mem(
   x(player,i1),
   timestamp(19,58.1)).
percept(
   x(player,i1),
   see,
   1,
   child_list(
      x(plate,i1),
      on,
      [])).
timestamp is 19 ( 0.1s ago )
timestamp is 19 ( 0.1s ago )
( Extra verbose logic: extra_verbose_logic(notices(x(player,i1),see,1,[the(x(plate,i1)),'has shape is plate','.'])) )
timestamp is 19 ( 0.1s ago )
Player see on plate: <nothing>

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(box,i1),1)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(20,58.2) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(box,i1),1)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(box,i1),1)).

episodic_mem(
   x(player,i1),
   timestamp(20,58.2)).
percept(
   x(player,i1),
   know,
   1,
   props(
      x(box,i1),
      [ default_rel=in,
        traits([physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(20,58.2)).
percept(
   x(player,i1),
   see,
   1,
   props(
      x(box,i1),
      [ shape=box ])).
episodic_mem(
   x(player,i1),
   timestamp(20,58.2)).
percept(
   x(player,i1),
   see,
   1,
   child_list(
      x(box,i1),
      in,
      '<mystery>'(closed, in,
         x(box,i1)))).
timestamp is 20 ( 0.1s ago )
timestamp is 20 ( 0.1s ago )
( Extra verbose logic: extra_verbose_logic(notices(x(player,i1),see,1,[the(x(box,i1)),'has shape is box','.'])) )
timestamp is 20 ( 0.1s ago )
(...verbose...: box is closed from seeing In )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(lamp,i1),1)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(21,58.4) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(lamp,i1),1)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(lamp,i1),1)).

episodic_mem(
   x(player,i1),
   timestamp(21,58.4)).
percept(
   x(player,i1),
   know,
   1,
   props(
      x(lamp,i1),
      [ name="shiny brass lamp",
        traits([light,brass,shiny,physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(21,58.4)).
percept(
   x(player,i1),
   see,
   1,
   props(
      x(lamp,i1),
      [ shape=lamp ])).
timestamp is 21 ( 0.1s ago )
timestamp is 21 ( 0.1s ago )
( Extra verbose logic: extra_verbose_logic(notices(x(player,i1),see,1,[the(x(lamp,i1)),'has shape is lamp','.'])) )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(wrench,i1),1)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(22,58.5) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(wrench,i1),1)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(wrench,i1),1)).

episodic_mem(
   x(player,i1),
   timestamp(22,58.5)).
percept(
   x(player,i1),
   know,
   1,
   props(
      x(wrench,i1),
      [ traits([physical,object,moveable,thinkable,fully_corporial,shiny]) ])).
episodic_mem(
   x(player,i1),
   timestamp(22,58.5)).
percept(
   x(player,i1),
   see,
   1,
   props(
      x(wrench,i1),
      [ shape=wrench ])).
timestamp is 22 ( 0.1s ago )
timestamp is 22 ( 0.1s ago )
( Extra verbose logic: extra_verbose_logic(notices(x(player,i1),see,1,[the(x(wrench,i1)),'has shape is wrench','.'])) )
x(player,i1)@[does]> s
s
episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(23,60) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            go_dir(
                               x(player,i1),
                               walk,
                               south)) ])).

% aXiom(go_dir(
%          x(player,i1),
%          walk,
%          south)).

episodic_mem(
   x(player,i1),
   timestamp(23,60)).
departing(
   x(player,i1),
   in,
   x(kitchen,i1),
   walk,
   south).
episodic_mem(
   x(player,i1),
   timestamp(23,60)).
arriving(
   x(player,i1),
   in,
   x(garden,i1),
   walk,
   north).
episodic_mem(
   x(player,i1),
   timestamp(23,60)).
episodic_mem(
   x(player,i1),
   success(go_dir(
              x(player,i1),
              walk,
              south))).
timestamp is 23 ( 0.1s ago )
player was in kitchen but left walking south
timestamp is 23 ( 0.1s ago )
player came walking north in garden
timestamp is 23 ( 0.1s ago )
( Success: walk south )
x(player,i1)@[does]> look
look
episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(24,61.9) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            look(x(player,i1))) ])).

% aXiom(look(x(player,i1))).

% aXiom(examine(
%          x(player,i1),
%          see,
%          in,
%          x(garden,i1))).

% aXiom(sub__examine(x(player,i1),see,in,x(garden,i1),3)).

episodic_mem(
   x(player,i1),
   timestamp(24,61.9)).
examine(
   x(player,i1),
   see,
   in,
   x(garden,i1)).
episodic_mem(
   x(player,i1),
   timestamp(24,61.9)).
sub__examine(x(player,i1),see,in,x(garden,i1),3).
episodic_mem(
   x(player,i1),
   timestamp(24,61.9)).
percept(
   x(player,i1),
   know,
   3,
   props(
      x(garden,i1),
      [ default_rel=in,
        has_rel(in,t),
        traits(here),
        inherited(here),
        has_rel(exit(Exit8),t),
        inherited(place),
        inherited(garden) ])).
episodic_mem(
   x(player,i1),
   timestamp(24,61.9)).
percept(
   x(player,i1),
   see,
   3,
   props(
      x(garden,i1),
      [ shape=garden,
        volume_capacity=10000,
        desc="this is a place",
        has_rel(in,t),
        has_rel(exit(Exit8),t) ])).
episodic_mem(
   x(player,i1),
   timestamp(24,61.9)).
percept(
   x(player,i1),
   see,
   3,
   child_list(
      x(garden,i1),
      in,
      [ x(player,i1),
        x(brklamp,i1),
        x(mushroom,i1),
        x(rock,2),
        x(rock,i1),
        x(fountain,i1) ])).
episodic_mem(
   x(player,i1),
   timestamp(24,61.9)).
percept(
   x(player,i1),
   see,
   3,
   exit_list(in,
      x(garden,i1),
      [north])).
timestamp is 24 ( 0s ago )
player does examine see in garden
timestamp is 24 ( 0s ago )
player does sub examine see in garden 3
timestamp is 24 ( 0s ago )
timestamp is 24 ( 0s ago )
(...verbose...: player sees the garden garden shaped , is large ,  "this is a place", thus, has an interior and can have exits. )
timestamp is 24 ( 0s ago )
Player sees in garden: {{ x(brklamp,i1) }} ,  {{ x(mushroom,i1) }} ,  {{ x(rock,2) }} ,  {{ x(rock,i1) }} and {{ x(fountain,i1) }} .
timestamp is 24 ( 0s ago )
Exits in garden are: north.


% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(brklamp,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(25,62) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(brklamp,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(brklamp,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(25,62)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(brklamp,i1),
      [ name="definately broken",
        traits([dented,broken,light,brass,shiny,physical,object,fully_corporial,thinkable,moveable]) ])).
episodic_mem(
   x(player,i1),
   timestamp(25,62)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(brklamp,i1),
      [ shape=brklamp,
        inherited(shiny),
        emitting(see,light) ])).
timestamp is 25 ( 0.1s ago )
timestamp is 25 ( 0.1s ago )
(...verbose...: player sees the brklamp brklamp shaped , inherits shiny! and is glowing. )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(mushroom,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(26,62.2) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(mushroom,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(mushroom,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(26,62.2)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(mushroom,i1),
      [ name="speckled mushroom",
        traits([physical,object,fully_corporial,thinkable,moveable,measurable,fungus,toadstool,speckled,mushroom]) ])).
episodic_mem(
   x(player,i1),
   timestamp(26,62.2)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(mushroom,i1),
      [ shape=mushroom ])).
timestamp is 26 ( 0.1s ago )
timestamp is 26 ( 0.1s ago )
( Extra verbose logic: player sees the mushroom has shape is mushroom. )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(rock,2),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(27,62.3) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(rock,2),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(rock,2),2)).

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(rock,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(28,62.5) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(rock,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(rock,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(28,62.5)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(rock,i1),
      [ shape=rock ])).
timestamp is 28 ( 0.1s ago )
( Extra verbose logic: player sees the rock has shape is rock. )

% x(player,i1) @ somewhere: already about todo: sub__examine(x(player,i1),see,child,x(fountain,i1),2)

episodic_mem(
   x(player,i1),
   memorize_prepending([ timestamp(29,62.6) ])).
episodic_mem(
   x(player,i1),
   memorize_prepending([ attempts(
                            x(player,i1),
                            sub__examine(x(player,i1),see,child,x(fountain,i1),2)) ])).

% aXiom(sub__examine(x(player,i1),see,child,x(fountain,i1),2)).

episodic_mem(
   x(player,i1),
   timestamp(29,62.6)).
percept(
   x(player,i1),
   know,
   2,
   props(
      x(fountain,i1),
      [ traits([here,object,moveable,untakeable,fully_corporial,thinkable,physical,furnature]),
        has_rel(exit(Exit8),t),
        default_rel=in,
        has_rel(in,t),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(29,62.6)).
percept(
   x(player,i1),
   see,
   2,
   props(
      x(fountain,i1),
      [ shape=fountain,
        has_rel(exit(Exit8),t),
        opened=t,
        has_rel(in,t),
        has_rel(on,t) ])).
episodic_mem(
   x(player,i1),
   timestamp(29,62.6)).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(fountain,i1),
      in,
      [])).
percept(
   x(player,i1),
   see,
   2,
   child_list(
      x(fountain,i1),
      on,
      [])).
timestamp is 29 ( 0.1s ago )
timestamp is 29 ( 0.1s ago )
(...verbose...: player sees the fountain fountain shaped , can have exits , opened , thus, has an interior and has a surface. )
timestamp is 29 ( 0.1s ago )
(...verbose...: nothing in fountain )
(...verbose...: nothing on fountain )
x(player,i1)@[does]>
% aXiom(emote(
%          x(floyd,i1),
%          act,
%          *,
%          [ cap(subj(x(floyd,i1))),
%            'hums quietly to themself.' ])).

x(player,i1)@[does]>

```



# Debug

```

% adv_server(4004).

Server is starting on port 4004
%         Thread Status       Time    Stack use    allocated
% ----------------------------------------------------------
%           main running     7.605      886,296    1,300,288
%             gc running     0.070        1,200       87,872
%            pce running     0.001        1,144      120,640
%         egg_go running     0.003       38,272      153,408
% 'httpd@3020_1' running     0.002        1,952      120,640
% 'httpd@3020_2' running     0.001        1,952      120,640
% 'httpd@3020_3' running     0.002        1,952      120,640
% 'httpd@3020_4' running     0.002        1,952      120,640
% 'httpd@3020_5' running     0.001        1,952      120,640
%    'http@3020' running     0.000        1,960      120,640
%    swish_stats running     0.005      198,832      350,016
%        mu_4004 running     0.000        1,584      120,640
% create_1obj(cup,i1).
% create_1obj(cabinate,i1).
% create_1obj(plate,i1).
% create_1obj(sink,i1).
% create_1obj(lamp,i1).
% create_1obj(table,i1).
% create_1obj(flour,i1).
% create_1obj(bowl,i1).
% create_1obj(box,i1).
% create_1obj(table,i1).
% create_1obj(crate,i1).
% create_1obj(kitchen,i1).
% create_1obj(fireplace,i1).
% create_1obj(living_room,i1).
% create_1obj(shovel,i1).
% create_1obj(basement,i1).
% create_1obj(fountain,i1).
% create_1obj(garden,i1).
% create_1obj(rock,i1).
% create_1obj(garden,i1).
% create_1obj(shelf,i1).
% create_1obj(pantry,i1).
% create_1obj(living_room,i1).
% create_1obj(kitchen,i1).
% create_1obj(dining_room,i1).
% create_1obj(living_room,i1).
% create_1obj(kitchen,i1).
% create_1obj(dining_room,i1).
% create_1obj(kitchen,i1).
% create_1obj(garden,i1).
% create_1obj(pantry,i1).
% create_1obj(basement,i1).
% create_1obj(pantry,i1).
% create_1obj(kitchen,i1).
% create_1obj(coins,i1).
% create_1obj(bag,i1).
% create_1obj(watch,i1).
% create_1obj(player,i1).
% create_1obj(floyd,i1).
% create_1obj(pantry,i1).
% create_1obj(player,i1).
% create_1obj(kitchen,i1).
% create_1obj(bag,i1).
% create_1obj(player,i1).
% create_1obj(wrench,i1).
% create_1obj(floyd,i1).
% create_1obj(kitchen,i1).
% create_1obj(pantry,i1).
% create_1obj(basement,i1).
% create_1obj(pantry,i1).
% create_1obj(garden,i1).
% create_1obj(kitchen,i1).
% create_1obj(dining_room,i1).
% create_1obj(kitchen,i1).
% create_1obj(living_room,i1).
% create_1obj(dining_room,i1).
% create_1obj(kitchen,i1).
% create_1obj(living_room,i1).
% create_1obj(locker,i1).
% create_1obj(pantry,i1).
% create_1obj(x(rock,2),i1).
% create_1obj(garden,i1).
% create_1obj(mushroom,i1).
% create_1obj(garden,i1).
% create_1obj(videocamera,i1).
% create_1obj(living_room,i1).
% create_1obj(screendoor,i1).
% create_1obj(kitchen,i1).
% create_1obj(apple,i1).
% create_1obj(crate,i1).
% create_1obj(brklamp,i1).
% create_1obj(garden,i1).
% create_1obj(table,i1).
% create_1obj(table_leg,i1).
% create_1obj(bowl,i1).
% create_1obj(box,i1).
% create_1obj(table,i1).
% create_1obj(kitchen,i1).
% create_1obj(sink,i1).
% create_1obj(kitchen,i1).
% create_1obj(cabinate,i1).
% create_1obj(kitchen,i1).

% oldObjectList=[].

% '='(newObjectList,
%    [ x(kitchen,i1),
%      x(cabinate,i1),
%      x(sink,i1),
%      x(table,i1),
%      x(box,i1),
%      x(bowl,i1),
%      x(table_leg,i1),
%      x(garden,i1),
%      x(brklamp,i1),
%      x(crate,i1),
%      x(apple,i1),
%      x(screendoor,i1),
%      x(living_room,i1),
%      x(videocamera,i1),
%      x(mushroom,i1),
%      x(pantry,i1),
%      x(locker,i1),
%      x(dining_room,i1),
%      x(basement,i1),
%      x(floyd,i1),
%      x(wrench,i1),
%      x(player,i1),
%      x(bag,i1),
%      x(watch,i1),
%      x(coins,i1),
%      x(shelf,i1),
%      x(rock,i1),
%      x(fountain,i1),
%      x(shovel,i1),
%      x(fireplace,i1),
%      x(flour,i1),
%      x(lamp,i1),
%      x(plate,i1),
%      x(cup,i1) ]).

% mu_create_object(
%    x(kitchen,i1),
%    [ shape=kitchen,
%      inherit(kitchen,t),
%      sp(nouns,
%         [kitchen]) ]).

% mu_create_object(
%    x(cabinate,i1),
%    [ shape=cabinate,
%      inherit(cabinate,t),
%      sp(nouns,
%         [cabinate]) ]).

% mu_create_object(
%    x(sink,i1),
%    [ shape=sink,
%      inherit(sink,t),
%      sp(nouns,
%         [sink]) ]).

% mu_create_object(
%    x(table,i1),
%    [ shape=(table),
%      inherit(table,t),
%      sp(nouns,
%         [table]) ]).

% mu_create_object(
%    x(box,i1),
%    [ shape=box,
%      inherit(box,t),
%      sp(nouns,
%         [box]) ]).

% mu_create_object(
%    x(bowl,i1),
%    [ shape=bowl,
%      inherit(bowl,t),
%      sp(nouns,
%         [bowl]) ]).

% mu_create_object(
%    x(table_leg,i1),
%    [ shape=table_leg,
%      inherit(table_leg,t),
%      sp(nouns,
%         [table_leg]) ]).

% mu_create_object(
%    x(garden,i1),
%    [ shape=garden,
%      inherit(garden,t),
%      sp(nouns,
%         [garden]) ]).

% mu_create_object(
%    x(brklamp,i1),
%    [ shape=brklamp,
%      inherit(brklamp,t),
%      sp(nouns,
%         [brklamp]) ]).

% mu_create_object(
%    x(crate,i1),
%    [ shape=crate,
%      inherit(crate,t),
%      sp(nouns,
%         [crate]) ]).

% mu_create_object(
%    x(apple,i1),
%    [ shape=apple,
%      inherit(apple,t),
%      sp(nouns,
%         [apple]) ]).

% mu_create_object(
%    x(screendoor,i1),
%    [ shape=screendoor,
%      inherit(screendoor,t),
%      sp(nouns,
%         [screendoor]) ]).

% mu_create_object(
%    x(living_room,i1),
%    [ shape=living_room,
%      inherit(living_room,t),
%      sp(nouns,
%         [living_room]) ]).

% mu_create_object(
%    x(videocamera,i1),
%    [ shape=videocamera,
%      inherit(videocamera,t),
%      sp(nouns,
%         [videocamera]) ]).

% mu_create_object(
%    x(mushroom,i1),
%    [ shape=mushroom,
%      inherit(mushroom,t),
%      sp(nouns,
%         [mushroom]) ]).

% mu_create_object(
%    x(pantry,i1),
%    [ shape=pantry,
%      inherit(pantry,t),
%      sp(nouns,
%         [pantry]) ]).

% mu_create_object(
%    x(locker,i1),
%    [ shape=locker,
%      inherit(locker,t),
%      sp(nouns,
%         [locker]) ]).

% mu_create_object(
%    x(dining_room,i1),
%    [ shape=dining_room,
%      inherit(dining_room,t),
%      sp(nouns,
%         [dining_room]) ]).

% mu_create_object(
%    x(basement,i1),
%    [ shape=basement,
%      inherit(basement,t),
%      sp(nouns,
%         [basement]) ]).

% mu_create_object(
%    x(floyd,i1),
%    [ shape=floyd,
%      inherit(floyd,t),
%      sp(nouns,
%         [floyd]) ]).

% mu_create_object(
%    x(wrench,i1),
%    [ shape=wrench,
%      inherit(wrench,t),
%      sp(nouns,
%         [wrench]) ]).

% mu_create_object(
%    x(player,i1),
%    [ shape=player,
%      inherit(player,t),
%      sp(nouns,
%         [player]) ]).

% mu_create_object(
%    x(bag,i1),
%    [ shape=bag,
%      inherit(bag,t),
%      sp(nouns,
%         [bag]) ]).

% mu_create_object(
%    x(watch,i1),
%    [ shape=watch,
%      inherit(watch,t),
%      sp(nouns,
%         [watch]) ]).

% mu_create_object(
%    x(coins,i1),
%    [ shape=coins,
%      inherit(coins,t),
%      sp(nouns,
%         [coins]) ]).

% mu_create_object(
%    x(shelf,i1),
%    [ shape=shelf,
%      inherit(shelf,t),
%      sp(nouns,
%         [shelf]) ]).

% mu_create_object(
%    x(rock,i1),
%    [ shape=rock,
%      inherit(rock,t),
%      sp(nouns,
%         [rock]) ]).

% mu_create_object(
%    x(fountain,i1),
%    [ shape=fountain,
%      inherit(fountain,t),
%      sp(nouns,
%         [fountain]) ]).

% mu_create_object(
%    x(shovel,i1),
%    [ shape=shovel,
%      inherit(shovel,t),
%      sp(nouns,
%         [shovel]) ]).

% mu_create_object(
%    x(fireplace,i1),
%    [ shape=fireplace,
%      inherit(fireplace,t),
%      sp(nouns,
%         [fireplace]) ]).

% mu_create_object(
%    x(flour,i1),
%    [ shape=flour,
%      inherit(flour,t),
%      sp(nouns,
%         [flour]) ]).

% mu_create_object(
%    x(lamp,i1),
%    [ shape=lamp,
%      inherit(lamp,t),
%      sp(nouns,
%         [lamp]) ]).

% mu_create_object(
%    x(plate,i1),
%    [ shape=plate,
%      inherit(plate,t),
%      sp(nouns,
%         [plate]) ]).

% mu_create_object(
%    x(cup,i1),
%    [ shape=cup,
%      inherit(cup,t),
%      sp(nouns,
%         [cup]) ]).
=============================================
INIT STATE
=============================================
[ structure_label(cur_state),
  type_props(videocamera,
     [ inherit(memorizer,t),
       inherit(perceptq,t),
       inherit(memorize_perceptq,t),
       can_be(switch,t),
       effect(
          switch(on),
          setprop(
             $self,
             powered=t)),
       effect(
          switch(off),
          setprop(
             $self,
             powered=f)),
       powered=t,
       has_sense(see),
       breaks_into=broken_videocam ]),
  type_props(table,
     [ inherit(surface,t),
       inherit(physical,t),
       default_rel=on ]),
  type_props(shelf,
     [ inherit(surface,t),
       inherit(physical,t),
       inherit(furnature,t) ]),
  type_props(surface,
     [ has_rel(on,t),
       default_rel=on,
       inherit(physical,t),
       cleanliness=clean ]),
  type_props(broken_lamp,
     [ name="dented brass lamp",
       inherit(light,t),
       traits(brass),
       inherit(dented,t),
       can_be(switch,t),
       effect(
          switch(on),
          true),
       effect(
          switch(off),
          true) ]),
  type_props(light,
     [ traits(light),
       sp=nouns ]),
  type_props(coins,
     [ inherit(shiny,t),
       inherit(measurable,t) ]),
  type_props(cabinate,
     [ inherit(container,t),
       inherit(furnature,t),
       volume_capacity=10 ]),
  type_props(sink,
     [ cleanliness=dirty,
       inherit(uncloseable,t),
       inherit(flask,t),
       inherit(furnature,t),
       volume_capacity=5 ]),
  type_props(cardboard,
     [ inherit(paper,t) ]),
  type_props(wooden,
     [ breaks_into=splinters,
       can_be(burn,t) ]),
  type_props(crate,
     [ inherit(container,t),
       inherit(moveable,t),
       volume_capacity=13,
       inherit(wooden,t),
       opened=t ]),
  type_props(fireplace,
     [ has_rel(on,f),
       has_rel(over,t),
       inherit(uncloseable,t),
       volume_capacity=20,
       inherit(furnature,t) ]),
  type_props(bowl,
     [ inherit(uncloseable,t),
       inherit(flask,t),
       volume_capacity=2,
       breaks_into=shards,
       cleanliness=dirty,
       name="porcelain bowl",
       desc="This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface." ]),
  type_props(bag,
     [ volume_capacity=10,
       inherit(container,t),
       inherit(moveable,t) ]),
  type_props(place,
     [ volume_capacity=10000,
       default_rel=in,
       desc="this is a place",
       has_rel(in,t),
       inherit(here,t),
       can_be(move,f),
       can_be(take,f),
       has_rel(exit(Exit),t) ]),
  type_props(perceptq,
     [ inherit(no_perceptq,f) ]),
  type_props(natural_force,
     [ knows_verbs(eat,f),
       can_be(touch,f),
       has_rel(held_by,f),
       has_rel(worn_by,f),
       has_sense(see),
       inherit(noncorporial,t),
       inherit(actor,t) ]),
  type_props(character,
     [ has_rel(worn_by,t),
       has_rel(held_by,t),
       model_depth=3,
       mass=50,
       volume=50,
       has_sense(see),
       inherit(perceptq,t),
       inherit(memorizer,t),
       inherit(actor,t),
       inherit(autoscan,t),
       inherit(partly_noncorporial,t) ]),
  type_props(perceptive,
     [ traits(perceptive),
       sp=adjs ]),
  type_props(nomicmu_plugin,
     [ inherit(plugin,t),
       inherit(unthinkable,t),
       '='(prefix,
          '$error'("required config var")),
       class_desc(["Nomicmu plugin"]) ]),
  type_props(plugin,
     [ traits(plugin),
       sp=nouns ]),
  type_props(autonomous,
     [ inherit(autoscan,t),
       inherit(impulsive,t),
       class_desc(["like Floyd the robot will, instances will automatically use its planner
        about planning to decide on what to do"]) ]),
  type_props(console,
     [ inherit(physical,t),
       traits([console]),
       inherit(player,t) ]),
  type_props(telnet,
     [ inherit(remote,t),
       inherit(player,t),
       inherit(player,t) ]),
  type_props(remote,
     [ traits(remote),
       sp=adjs ]),
  type_props(furnature,
     [ can_be(examine,t),
       inherit(furnature,t),
       inherit(untakeable,t),
       inherit(fully_corporial,t),
       inherit(surface,t),
       inherit(thinkable,t),
       inherit(physical,t),
       class_desc(["kind is furnature"]),
       traits(furnature),
       sp=nouns ]),
  type_props(fully_corporial,
     [ can_be(touch,t),
       can_be(examine,t),
       inherit(thinkable,t),
       cleanliness=clean,
       inherit(fully_corporial,t),
       class_desc(["kind is corporial"]),
       traits(fully_corporial),
       sp=adjs ]),
  type_props(partly_noncorporial,
     [ inherit(fully_corporial,t),
       inherit(partly_noncorporial,t),
       inherit(noncorporial,t),
       class_desc(["kind is both partly corporial and non-corporial"]),
       traits(partly_noncorporial),
       sp=adjs ]),
  type_props(only_conceptual,
     [ inherit(only_conceptual,t),
       inherit(noncorporial,t),
       inherit(thinkable,t),
       class_desc(["kind is only conceptual"]),
       traits(only_conceptual),
       sp=adjs ]),
  type_props(noncorporial,
     [ '='(
          can(examine),
          f),
       can_be(touch,f),
       inherit(thinkable,t),
       inherit(noncorporial,t),
       inherit(fully_corporial,f),
       class_desc(["direct inheriters are completely noncorporial"]),
       traits(noncorporial),
       sp=adjs ]),
  type_props(thinkable,
     [ can_be(examine,t),
       inherit(thinkable,t),
       class_desc(["kind is normally thinkable"]),
       traits(thinkable),
       sp=adjs ]),
  type_props(door,
     [ inherit(furnature,t),
       can_be(open,t),
       can_be(close,t),
       opened=f,
       inherit(door,t),
       inherit(fully_corporial,t),
       traits(door),
       sp=nouns,
       can_be(take,f) ]),
  type_props(mushroom,
     [ name="speckled mushroom",
       inherit(food,t),
       inherit(mushroom,t),
       inherit(fungus,t),
       inherit(toadstool,t),
       inherit(speckled,t),
       initial("A speckled mushroom grows out of the sodden earth, on a long stalk."),
       desc="The mushroom is capped with blotches, and you aren't at all sure it's not a toadstool.",
       can_be(eat,t),
       before(eat,
          ((random100=<30 ,
            die("It was poisoned!")) ;
           "yuck!")),
       after(take,
          initial,"You pick the mushroom, neatly cleaving its thin stalk."),
       traits(mushroom),
       sp=nouns ]),
  type_props(toadstool,
     [ traits(toadstool),
       sp=nouns ]),
  type_props(screendoor,
     [ door_to(kitchen),
       door_to(garden),
       opened=f,
       inherit(door,t) ]),
  type_props(pantry,
     [ volume_capacity=1000,
       inherit(closet,t),
       traits(kitchen),
       desc="You're in a dark kitchen pantry.",
       dark=t,
       inherit(place,t) ]),
  type_props(living_room,
     [ inherit(place,t) ]),
  type_props(garden,
     [ inherit(place,t),
       cant_go(
          $agent,
          up,
          "You lack the ability to fly."),
       desc="this is the garden",
       cant_go(
          $agent,
          _41604,
          "The fence surrounding the garden is too tall and solid to pass.") ]),
  type_props(basement,
     [ inherit(place,t),
       desc="This is a very dark basement.",
       dark=t ]),
  type_props(food,
     [ can_be(eat,t),
       inherit(moveable,t),
       inherit(measurable,t) ]),
  type_props(dining_room,
     [ inherit(place,t) ]),
  type_props(kitchen,
     [ inherit(place,t),
       desc="cooking happens here" ]),
  type_props(closet,
     [ traits(closet),
       sp=nouns ]),
  type_props(brklamp,
     [ inherit(broken,t),
       name="possibly broken lamp",
       effect(
          switch(on),
          print_(_41266,"Switch is flipped")),
       effect(hit,
          [ print_("Hit brklamp"),
            setprop(
               $self,
               inherit(broken)) ]),
       inherit(lamp,t) ]),
  type_props(broken,
     [ name="definately broken",
       effect(
          switch(on),
          true),
       effect(
          switch(off),
          true),
       can_be(switch,t),
       inherit(broken,t),
       inherit(dented,t),
       traits(broken),
       sp=adjs ]),
  type_props(fungus,
     [ traits(fungus),
       sp=nouns ]),
  type_props(speckled,
     [ traits(speckled),
       sp=adjs ]),
  type_props(moveable,
     [ can_be(examine,t),
       inherit(physical,t),
       inherit(moveable,t),
       traits(object),
       can_be(move,t),
       inherit(fully_corporial,t),
       inherit(thinkable,t),
       class_desc(["kind is an Movable Object"]),
       traits(moveable),
       sp=adjs ]),
  type_props(untakeable,
     [ inherit(untakeable,t),
       can_be(take,f),
       class_desc(["kind is an Immobile Object"]),
       traits(untakeable),
       sp=adjs ]),
  type_props(floyd,
     [ name="Floyd the robot",
       powered=t,
       inherit(autonomous,t),
       inherit(robot,t) ]),
  type_props(player,
     [ traits(player),
       sp=nouns,
       '='(name,
          $self),
       model_depth=3,
       inherit(autoscan,t),
       look_depth=2,
       user_mode=2,
       access_level=admin,
       inherit(console,t),
       inherit(humanoid,t) ]),
  type_props(humanoid,
     [ knows_verbs(eat,t),
       volume=50,
       mass=50,
       inherit(character,t),
       inherit(memorizer,t),
       can_be(
          switch(off),
          f),
       powered=t ]),
  type_props(decider,
     [ class_desc(["agents of this type/class call decide_action/3 hooks (and per plugin)"]) ]),
  type_props(unthinkable,
     [ traits(unthinkable),
       sp=nouns,
       can_be(examine,f),
       inherit(unthinkable,t),
       class_desc(["kind is normally unthinkable"]),
       sp=adjs ]),
  type_props(decider_plugin,
     [ traits(decider),
       inherit(nomicmu_plugin,t),
       class_desc(["plugins that contain decide_action hooks"]) ]),
  type_props(autoscan,
     [ inherit(perceptive,t),
       inherit(autolook,t),
       class_desc(["Sensory percepts that discover new objects request further details (notice dirty plates are in the sink)"]) ]),
  type_props(autolook,
     [ inherit(autolook,t),
       class_desc(["When entering a new area the Agent will automatically
            get an overview of the env (without purposeful looking)"]),
       traits(autolook),
       sp=adjs ]),
  type_props(actor,
     [ knows_verbs(examine,t),
       inherit(partly_noncorporial,t) ]),
  type_props(metallic,
     [ traits(metallic),
       sp=adjs ]),
  type_props(robot,
     [ knows_verbs(eat,f),
       inherit(autonomous,t),
       emitting(see,light),
       volume=50,
       mass=200,
       inherit(robot,t),
       inherit(metallic,t),
       desc="Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.",
       can_be(switch,t),
       iza(memorizer),
       inherit(shiny,t),
       inherit(character,t),
       powered=t,
       effect(
          switch(on),
          setprop(
             $self,
             powered=t)),
       effect(
          switch(off),
          setprop(
             $self,
             powered=f)),
       traits(robot),
       sp=nouns ]),
  type_props(no_perceptq,
     [ inherit(perceptq,f) ]),
  type_props(here,
     [ traits(here),
       sp=nouns ]),
  type_props(container,
     [ default_rel=in,
       opened=f,
       can_be(open,t),
       has_rel(in,t) ]),
  type_props(cup,
     [ inherit(flask,t) ]),
  type_props(flask,
     [ inherit(physical,t),
       inherit(container,t),
       opened=t,
       inherit(moveable,t) ]),
  type_props(plate,
     [ inherit(surface,t),
       inherit(moveable,t),
       volume_capacity=2,
       breaks_into=shards,
       cleanliness=dirty ]),
  type_props(box,
     [ opened=f,
       volume_capacity=11,
       inherit(container,t),
       inherit(moveable,t),
       inherit(cardboard,t) ]),
  type_props(locker,
     [ inherit(container,t),
       inherit(moveable,t),
       volume_capacity=13,
       inherit(metal,t),
       opened=f ]),
  type_props(metal,
     [ can_be(burn,f) ]),
  type_props(paper,
     [ can_be(burn,t) ]),
  type_props(uncloseable,
     [ opened=t,
       can_be(close,f),
       can_be(open,f),
       inherit(container,t) ]),
  type_props(fountain,
     [ volume_capacity=150,
       inherit(place,t),
       inherit(sink,t) ]),
  type_props(measurable,
     [ inherit(measurable,t),
       ammount=some,
       traits(measurable),
       sp=adjs ]),
  type_props(shiny,
     [ inherit(shiny,t),
       inherit(moveable,t),
       inherit(fully_corporial,t),
       traits(shiny),
       sp=adjs ]),
  type_props(flour,
     [ inherit(food,t),
       inherit(measurable,t) ]),
  type_props(lamp,
     [ name="shiny brass lamp",
       powered=t,
       can_be(switch,t),
       inherit(light,t),
       traits(brass),
       inherit(shiny,t),
       inherit(moveable,t),
       emitting(see,light),
       effect(
          switch(on),
          setprop(
             $self,
             emitting(see,light))),
       effect(
          switch(off),
          delprop(
             $self,
             emitting(see,light))),
       breaks_into=broken_lamp ]),
  type_props(dented,
     [ traits(dented),
       sp=adjs ]),
  type_props(physical,
     [ traits(physical),
       sp=adjs ]),
  type_props(wrench,
     [ inherit(shiny,t) ]),
  type_props(broken_videocam,
     [ can_be(switch,f),
       powered=f,
       inherit(videocamera,t) ]),
  h(in,x(cup,i1),x(cabinate,i1)),
  h(in,x(plate,i1),x(sink,i1)),
  h(on,x(lamp,i1),x(table,i1)),
  h(in,x(flour,i1),x(bowl,i1)),
  h(on,x(box,i1),x(table,i1)),
  h(in,x(crate,i1),x(kitchen,i1)),
  h(in,x(fireplace,i1),x(living_room,i1)),
  h(in,x(shovel,i1),x(basement,i1)),
  h(in,x(fountain,i1),x(garden,i1)),
  h(in,x(rock,i1),x(garden,i1)),
  h(in,x(shelf,i1),x(pantry,i1)),
  h(exit(south),x(living_room,i1),x(kitchen,i1)),
  h(exit(north),x(dining_room,i1),x(living_room,i1)),
  h(exit(east),x(kitchen,i1),x(dining_room,i1)),
  h(exit(south),x(kitchen,i1),x(garden,i1)),
  h(exit(down),x(pantry,i1),x(basement,i1)),
  h(exit(south),x(pantry,i1),x(kitchen,i1)),
  h(in,x(coins,i1),x(bag,i1)),
  h(worn_by,x(watch,i1),x(player,i1)),
  h(in,x(floyd,i1),x(pantry,i1)),
  h(in,x(player,i1),x(kitchen,i1)),
  h(held_by,x(bag,i1),x(player,i1)),
  h(held_by,x(wrench,i1),x(floyd,i1)),
  h(exit(north),x(kitchen,i1),x(pantry,i1)),
  h(exit(up),x(basement,i1),x(pantry,i1)),
  h(exit(north),x(garden,i1),x(kitchen,i1)),
  h(exit(west),x(dining_room,i1),x(kitchen,i1)),
  h(exit(east),x(living_room,i1),x(dining_room,i1)),
  h(exit(west),x(kitchen,i1),x(living_room,i1)),
  h(in,x(locker,i1),x(pantry,i1)),
  h(in,x(rock,2),x(garden,i1)),
  h(in,x(mushroom,i1),x(garden,i1)),
  h(in,x(videocamera,i1),x(living_room,i1)),
  h(in,x(screendoor,i1),x(kitchen,i1)),
  h(in,x(apple,i1),x(crate,i1)),
  h(in,x(brklamp,i1),x(garden,i1)),
  h(reverse(on),x(table,i1),x(table_leg,i1)),
  h(in,x(bowl,i1),x(box,i1)),
  h(in,x(table,i1),x(kitchen,i1)),
  h(in,x(sink,i1),x(kitchen,i1)),
  h(in,x(cabinate,i1),x(kitchen,i1)),
  props(
     x(kitchen,i1),
     [ shape=kitchen,
       volume_capacity=10000,
       default_rel=in,
       desc="this is a place",
       has_rel(in,t),
       traits(here),
       sp=nouns,
       inherited(here),
       can_be(move,f),
       can_be(take,f),
       has_rel(exit(Exit1),t),
       inherited(place),
       inherited(kitchen),
       co([ shape=kitchen,
            inherit(kitchen,t),
            sp(nouns,
               [kitchen]) ]) ]),
  props(
     x(cabinate,i1),
     [ shape=cabinate,
       default_rel=in,
       opened=f,
       can_be(open,t),
       has_rel(in,t),
       inherited(container),
       can_be(take,f),
       class_desc(["kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),
       traits([untakeable,fully_corporial,thinkable,physical,furnature]),
       inherited(untakeable),
       can_be(touch,t),
       inherited(fully_corporial),
       has_rel(on,t),
       cleanliness=clean,
       inherited(surface),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(physical),
       inherited(furnature),
       volume_capacity=10,
       inherited(cabinate),
       co([ shape=cabinate,
            inherit(cabinate,t),
            sp(nouns,
               [cabinate]) ]) ]),
  props(
     x(sink,i1),
     [ shape=sink,
       cleanliness=dirty,
       opened=t,
       can_be(close,f),
       can_be(open,f),
       inherited(uncloseable),
       default_rel=in,
       has_rel(in,t),
       inherited(container),
       traits([object,moveable,untakeable,fully_corporial,thinkable,physical,furnature]),
       can_be(move,t),
       class_desc(["kind is an Movable Object","kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),
       inherited(moveable),
       inherited(flask),
       can_be(take,f),
       inherited(untakeable),
       can_be(touch,t),
       inherited(fully_corporial),
       has_rel(on,t),
       inherited(surface),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(physical),
       inherited(furnature),
       volume_capacity=5,
       inherited(sink),
       co([ shape=sink,
            inherit(sink,t),
            sp(nouns,
               [sink]) ]) ]),
  props(
     x(table,i1),
     [ shape=(table),
       has_rel(on,t),
       cleanliness=clean,
       inherited(surface),
       traits(physical),
       sp=adjs,
       inherited(physical),
       default_rel=on,
       inherited(table),
       co([ shape=(table),
            inherit(table,t),
            sp(nouns,
               [table]) ]) ]),
  props(
     x(box,i1),
     [ shape=box,
       volume_capacity=11,
       default_rel=in,
       opened=f,
       can_be(open,t),
       has_rel(in,t),
       inherited(container),
       traits([physical,object,fully_corporial,thinkable,moveable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(moveable),
       can_be(burn,t),
       inherited(paper),
       inherited(cardboard),
       inherited(box),
       co([ shape=box,
            inherit(box,t),
            sp(nouns,
               [box]) ]) ]),
  props(
     x(bowl,i1),
     [ shape=bowl,
       opened=t,
       can_be(close,f),
       can_be(open,f),
       inherited(uncloseable),
       default_rel=in,
       has_rel(in,t),
       inherited(container),
       traits([physical,object,fully_corporial,thinkable,moveable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(moveable),
       inherited(flask),
       volume_capacity=2,
       breaks_into=shards,
       name="porcelain bowl",
       desc="This is a modest glass cooking bowl with a yellow flower motif glazed into the outside surface.",
       inherited(bowl),
       co([ shape=bowl,
            inherit(bowl,t),
            sp(nouns,
               [bowl]) ]) ]),
  props(
     x(table_leg,i1),
     [ shape=table_leg,
       inherited(table_leg),
       co([ shape=table_leg,
            inherit(table_leg,t),
            sp(nouns,
               [table_leg]) ]) ]),
  props(
     x(garden,i1),
     [ shape=garden,
       volume_capacity=10000,
       default_rel=in,
       desc="this is a place",
       has_rel(in,t),
       traits(here),
       sp=nouns,
       inherited(here),
       can_be(move,f),
       can_be(take,f),
       has_rel(exit(Exit2),t),
       inherited(place),
       cant_go(
          $agent,
          up,
          "You lack the ability to fly."),
       inherited(garden),
       co([ shape=garden,
            inherit(garden,t),
            sp(nouns,
               [garden]) ]) ]),
  props(
     x(brklamp,i1),
     [ shape=brklamp,
       name="definately broken",
       effect(
          switch(on),
          true),
       effect(
          switch(off),
          true),
       traits([dented,broken,light,brass,shiny,physical,object,fully_corporial,thinkable,moveable]),
       inherited(dented),
       sp=adjs,
       inherited(broken),
       effect(hit,
          [ print_("Hit brklamp"),
            setprop(
               x(brklamp,i1),
               inherit(broken)) ]),
       powered=t,
       can_be(switch,t),
       inherited(light),
       inherited(shiny),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       inherited(moveable),
       emitting(see,light),
       breaks_into=broken_lamp,
       inherited(lamp),
       inherited(brklamp),
       co([ shape=brklamp,
            inherit(brklamp,t),
            sp(nouns,
               [brklamp]) ]) ]),
  props(
     x(crate,i1),
     [ shape=crate,
       default_rel=in,
       opened=f,
       can_be(open,t),
       has_rel(in,t),
       inherited(container),
       traits([physical,object,fully_corporial,thinkable,moveable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(moveable),
       volume_capacity=13,
       breaks_into=splinters,
       can_be(burn,t),
       inherited(wooden),
       inherited(crate),
       co([ shape=crate,
            inherit(crate,t),
            sp(nouns,
               [crate]) ]) ]),
  props(
     x(apple,i1),
     [ shape=apple,
       inherited(apple),
       co([ shape=apple,
            inherit(apple,t),
            sp(nouns,
               [apple]) ]) ]),
  props(
     x(screendoor,i1),
     [ shape=screendoor,
       door_to(kitchen),
       door_to(garden),
       class_desc(["kind is an Immobile Object","kind is furnature","kind is normally thinkable","kind is corporial"]),
       traits([untakeable,physical,furnature,thinkable,fully_corporial,door]),
       inherited(untakeable),
       has_rel(on,t),
       default_rel=on,
       inherited(surface),
       sp=adjs,
       inherited(physical),
       inherited(furnature),
       can_be(open,t),
       can_be(close,t),
       opened=f,
       can_be(touch,t),
       can_be(examine,t),
       inherited(thinkable),
       cleanliness=clean,
       inherited(fully_corporial),
       can_be(take,f),
       inherited(door),
       inherited(screendoor),
       co([ shape=screendoor,
            inherit(screendoor,t),
            sp(nouns,
               [screendoor]) ]) ]),
  props(
     x(living_room,i1),
     [ shape=living_room,
       volume_capacity=10000,
       default_rel=in,
       desc="this is a place",
       has_rel(in,t),
       traits(here),
       sp=nouns,
       inherited(here),
       can_be(move,f),
       can_be(take,f),
       has_rel(exit(Exit3),t),
       inherited(place),
       inherited(living_room),
       co([ shape=living_room,
            inherit(living_room,t),
            sp(nouns,
               [living_room]) ]) ]),
  perceptq(
     x(videocamera,i1),
     []),
  memories(
     x(videocamera,i1),
     [ propOf(memories,
          x(videocamera,i1)),
       structure_label(mem(x(videocamera,i1))),
       timestamp(0,16.3),
       current_goals(_33800,[]),
       goals_skipped(_33800,[]),
       goals_satisfied(_33800,[]),
       todo(_33800,
          [ look(x(videocamera,i1)) ]),
       inst(x(videocamera,i1)) ]),
  props(
     x(videocamera,i1),
     [ shape=videocamera,
       inherited(memorize_perceptq),
       can_be(switch,t),
       effect(
          switch(on),
          setprop(
             x(videocamera,i1),
             powered=t)),
       effect(
          switch(off),
          setprop(
             x(videocamera,i1),
             powered=f)),
       powered=t,
       has_sense(see),
       breaks_into=broken_videocam,
       inherited(videocamera),
       co([ shape=videocamera,
            inherit(videocamera,t),
            sp(nouns,
               [videocamera]) ]) ]),
  props(
     x(mushroom,i1),
     [ shape=mushroom,
       name="speckled mushroom",
       traits([physical,object,fully_corporial,thinkable,moveable,measurable,fungus,toadstool,speckled,mushroom]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       inherited(moveable),
       ammount=some,
       sp=adjs,
       inherited(measurable),
       inherited(food),
       inherited(fungus),
       inherited(toadstool),
       inherited(speckled),
       initial("A speckled mushroom grows out of the sodden earth, on a long stalk."),
       desc="The mushroom is capped with blotches, and you aren't at all sure it's not a toadstool.",
       can_be(eat,t),
       before(eat,
          ((random100=<30 ,
            die("It was poisoned!")) ;
           "yuck!")),
       after(take,
          initial,"You pick the mushroom, neatly cleaving its thin stalk."),
       inherited(mushroom),
       co([ shape=mushroom,
            inherit(mushroom,t),
            sp(nouns,
               [mushroom]) ]) ]),
  props(
     x(pantry,i1),
     [ shape=pantry,
       volume_capacity=1000,
       traits([closet,kitchen,here]),
       inherited(closet),
       desc="You're in a dark kitchen pantry.",
       dark=t,
       default_rel=in,
       has_rel(in,t),
       sp=nouns,
       inherited(here),
       can_be(move,f),
       can_be(take,f),
       has_rel(exit(Exit4),t),
       inherited(place),
       inherited(pantry),
       co([ shape=pantry,
            inherit(pantry,t),
            sp(nouns,
               [pantry]) ]) ]),
  props(
     x(locker,i1),
     [ shape=locker,
       default_rel=in,
       can_be(open,t),
       has_rel(in,t),
       inherited(container),
       traits([physical,object,fully_corporial,thinkable,moveable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(moveable),
       volume_capacity=13,
       can_be(burn,f),
       inherited(metal),
       opened=f,
       inherited(locker),
       co([ shape=locker,
            inherit(locker,t),
            sp(nouns,
               [locker]) ]) ]),
  props(
     x(dining_room,i1),
     [ shape=dining_room,
       volume_capacity=10000,
       default_rel=in,
       desc="this is a place",
       has_rel(in,t),
       traits(here),
       sp=nouns,
       inherited(here),
       can_be(move,f),
       can_be(take,f),
       has_rel(exit(Exit5),t),
       inherited(place),
       inherited(dining_room),
       co([ shape=dining_room,
            inherit(dining_room,t),
            sp(nouns,
               [dining_room]) ]) ]),
  props(
     x(basement,i1),
     [ shape=basement,
       volume_capacity=10000,
       default_rel=in,
       desc="this is a place",
       has_rel(in,t),
       traits(here),
       sp=nouns,
       inherited(here),
       can_be(move,f),
       can_be(take,f),
       has_rel(exit(Exit6),t),
       inherited(place),
       dark=t,
       inherited(basement),
       co([ shape=basement,
            inherit(basement,t),
            sp(nouns,
               [basement]) ]) ]),
  memories(
     x(floyd,i1),
     [ propOf(memories,
          x(floyd,i1)),
       structure_label(mem(x(floyd,i1))),
       timestamp(0,16.4),
       current_goals(_31758,[]),
       goals_skipped(_31758,[]),
       goals_satisfied(_31758,[]),
       todo(_31758,
          [ look(x(floyd,i1)) ]),
       inst(x(floyd,i1)) ]),
  perceptq(
     x(floyd,i1),
     []),
  props(
     x(floyd,i1),
     [ shape=floyd,
       name="Floyd the robot",
       knows_verbs(eat,f),
       inherited(impulsive),
       class_desc(["like Floyd the robot will, instances will automatically use its planner
        about planning to decide on what to do","kind is an Movable Object","When entering a new area the Agent will automatically
            get an overview of the env (without purposeful looking)","Sensory percepts that discover new objects request further details (notice dirty plates are in the sink)","kind is corporial","kind is normally thinkable","direct inheriters are completely noncorporial","kind is both partly corporial and non-corporial"]),
       inherited(autonomous),
       emitting(see,light),
       mass=200,
       traits([metallic,physical,object,moveable,shiny,perceptive,autolook,fully_corporial,thinkable,noncorporial,partly_noncorporial,robot]),
       inherited(metallic),
       desc="Your classic robot: metallic with glowing red eyes, enthusiastic but not very clever.",
       can_be(switch,t),
       iza(memorizer),
       inherited(physical),
       can_be(move,t),
       inherited(moveable),
       inherited(shiny),
       has_rel(worn_by,t),
       has_rel(held_by,t),
       model_depth=3,
       volume=50,
       has_sense(see),
       knows_verbs(examine,t),
       inherited(actor),
       inherited(perceptive),
       inherited(autolook),
       inherited(autoscan),
       can_be(touch,t),
       cleanliness=clean,
       inherited(fully_corporial),
       inherit(fully_corporial,t),
       '='(
          can(examine),
          f),
       can_be(examine,t),
       inherited(thinkable),
       inherited(noncorporial),
       sp=adjs,
       inherited(partly_noncorporial),
       inherited(character),
       powered=t,
       effect(
          switch(on),
          setprop(
             x(floyd,i1),
             powered=t)),
       effect(
          switch(off),
          setprop(
             x(floyd,i1),
             powered=f)),
       inherited(robot),
       inherited(floyd),
       co([ shape=floyd,
            inherit(floyd,t),
            sp(nouns,
               [floyd]) ]) ]),
  props(
     x(wrench,i1),
     [ shape=wrench,
       traits([physical,object,moveable,thinkable,fully_corporial,shiny]),
       inherited(physical),
       can_be(move,t),
       class_desc(["kind is an Movable Object","kind is normally thinkable","kind is corporial"]),
       inherited(moveable),
       can_be(touch,t),
       can_be(examine,t),
       inherited(thinkable),
       cleanliness=clean,
       inherited(fully_corporial),
       sp=adjs,
       inherited(shiny),
       inherited(wrench),
       co([ shape=wrench,
            inherit(wrench,t),
            sp(nouns,
               [wrench]) ]) ]),
  memories(
     x(player,i1),
     [ propOf(memories,
          x(player,i1)),
       structure_label(mem(x(player,i1))),
       timestamp(0,16.5),
       current_goals(_30382,[]),
       goals_skipped(_30382,[]),
       goals_satisfied(_30382,[]),
       todo(_30382,
          [ look(x(player,i1)) ]),
       inst(x(player,i1)) ]),
  perceptq(
     x(player,i1),
     []),
  props(
     x(player,i1),
     [ shape=player,
       traits([player,physical,console,perceptive,autolook,fully_corporial,thinkable,noncorporial,partly_noncorporial]),
       sp=nouns,
       '='(name,
          x(player,i1)),
       look_depth=2,
       user_mode=2,
       access_level=admin,
       inherited(physical),
       inherited(console),
       knows_verbs(eat,t),
       has_rel(worn_by,t),
       has_rel(held_by,t),
       model_depth=3,
       mass=50,
       volume=50,
       has_sense(see),
       knows_verbs(examine,t),
       inherited(actor),
       inherited(perceptive),
       class_desc(["When entering a new area the Agent will automatically
            get an overview of the env (without purposeful looking)","Sensory percepts that discover new objects request further details (notice dirty plates are in the sink)","kind is corporial","kind is normally thinkable","direct inheriters are completely noncorporial","kind is both partly corporial and non-corporial"]),
       inherited(autolook),
       inherited(autoscan),
       can_be(touch,t),
       cleanliness=clean,
       inherited(fully_corporial),
       inherit(fully_corporial,t),
       '='(
          can(examine),
          f),
       can_be(examine,t),
       inherited(thinkable),
       inherited(noncorporial),
       inherited(partly_noncorporial),
       inherited(character),
       can_be(
          switch(off),
          f),
       powered=t,
       inherited(humanoid),
       inherited(player),
       co([ shape=player,
            inherit(player,t),
            sp(nouns,
               [player]) ]) ]),
  props(
     x(bag,i1),
     [ shape=bag,
       volume_capacity=10,
       default_rel=in,
       opened=f,
       can_be(open,t),
       has_rel(in,t),
       inherited(container),
       traits([physical,object,fully_corporial,thinkable,moveable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(moveable),
       inherited(bag),
       co([ shape=bag,
            inherit(bag,t),
            sp(nouns,
               [bag]) ]) ]),
  props(
     x(watch,i1),
     [ shape=watch,
       inherited(watch),
       co([ shape=watch,
            inherit(watch,t),
            sp(nouns,
               [watch]) ]) ]),
  props(
     x(coins,i1),
     [ shape=coins,
       traits([physical,object,moveable,thinkable,fully_corporial,shiny,measurable]),
       inherited(physical),
       can_be(move,t),
       class_desc(["kind is an Movable Object","kind is normally thinkable","kind is corporial"]),
       inherited(moveable),
       can_be(touch,t),
       can_be(examine,t),
       inherited(thinkable),
       cleanliness=clean,
       inherited(fully_corporial),
       inherited(shiny),
       ammount=some,
       sp=adjs,
       inherited(measurable),
       inherited(coins),
       co([ shape=coins,
            inherit(coins,t),
            sp(nouns,
               [coins]) ]) ]),
  props(
     x(shelf,i1),
     [ shape=shelf,
       can_be(take,f),
       class_desc(["kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),
       traits([untakeable,fully_corporial,thinkable,physical,furnature]),
       inherited(untakeable),
       can_be(touch,t),
       inherited(fully_corporial),
       has_rel(on,t),
       default_rel=on,
       cleanliness=clean,
       inherited(surface),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(physical),
       inherited(furnature),
       inherited(shelf),
       co([ shape=shelf,
            inherit(shelf,t),
            sp(nouns,
               [shelf]) ]) ]),
  props(
     x(rock,i1),
     [ shape=rock,
       inherited(rock),
       co([ shape=rock,
            inherit(rock,t),
            sp(nouns,
               [rock]) ]) ]),
  props(
     x(fountain,i1),
     [ shape=fountain,
       volume_capacity=150,
       desc="this is a place",
       traits([here,object,moveable,untakeable,fully_corporial,thinkable,physical,furnature]),
       sp=nouns,
       inherited(here),
       can_be(move,f),
       has_rel(exit(Exit7),t),
       inherited(place),
       cleanliness=dirty,
       opened=t,
       can_be(close,f),
       can_be(open,f),
       inherited(uncloseable),
       default_rel=in,
       has_rel(in,t),
       inherited(container),
       class_desc(["kind is an Movable Object","kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),
       inherited(moveable),
       inherited(flask),
       can_be(take,f),
       inherited(untakeable),
       can_be(touch,t),
       inherited(fully_corporial),
       has_rel(on,t),
       inherited(surface),
       can_be(examine,t),
       inherited(thinkable),
       inherited(physical),
       inherited(furnature),
       inherited(sink),
       inherited(fountain),
       co([ shape=fountain,
            inherit(fountain,t),
            sp(nouns,
               [fountain]) ]) ]),
  props(
     x(shovel,i1),
     [ shape=shovel,
       inherited(shovel),
       co([ shape=shovel,
            inherit(shovel,t),
            sp(nouns,
               [shovel]) ]) ]),
  props(
     x(fireplace,i1),
     [ shape=fireplace,
       has_rel(on,f),
       has_rel(over,t),
       opened=t,
       can_be(close,f),
       can_be(open,f),
       default_rel=in,
       has_rel(in,t),
       inherited(container),
       inherited(uncloseable),
       volume_capacity=20,
       can_be(take,f),
       class_desc(["kind is an Immobile Object","kind is corporial","kind is normally thinkable","kind is furnature"]),
       traits([untakeable,fully_corporial,thinkable,physical,furnature]),
       inherited(untakeable),
       can_be(touch,t),
       inherited(fully_corporial),
       cleanliness=clean,
       inherited(surface),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(physical),
       inherited(furnature),
       inherited(fireplace),
       co([ shape=fireplace,
            inherit(fireplace,t),
            sp(nouns,
               [fireplace]) ]) ]),
  props(
     x(flour,i1),
     [ shape=flour,
       can_be(eat,t),
       traits([physical,object,fully_corporial,thinkable,moveable,measurable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       inherited(moveable),
       inherited(food),
       ammount=some,
       sp=adjs,
       inherited(measurable),
       inherited(flour),
       co([ shape=flour,
            inherit(flour,t),
            sp(nouns,
               [flour]) ]) ]),
  props(
     x(lamp,i1),
     [ shape=lamp,
       name="shiny brass lamp",
       powered=t,
       can_be(switch,t),
       traits([light,brass,shiny,physical,object,fully_corporial,thinkable,moveable]),
       sp=nouns,
       inherited(light),
       inherited(shiny),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       inherited(moveable),
       emitting(see,light),
       effect(
          switch(on),
          setprop(
             x(lamp,i1),
             emitting(see,light))),
       effect(
          switch(off),
          delprop(
             x(lamp,i1),
             emitting(see,light))),
       breaks_into=broken_lamp,
       inherited(lamp),
       co([ shape=lamp,
            inherit(lamp,t),
            sp(nouns,
               [lamp]) ]) ]),
  props(
     x(plate,i1),
     [ shape=plate,
       has_rel(on,t),
       default_rel=on,
       inherited(surface),
       traits([physical,object,fully_corporial,thinkable,moveable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(moveable),
       volume_capacity=2,
       breaks_into=shards,
       inherited(plate),
       co([ shape=plate,
            inherit(plate,t),
            sp(nouns,
               [plate]) ]) ]),
  props(
     x(cup,i1),
     [ shape=cup,
       default_rel=in,
       opened=f,
       can_be(open,t),
       has_rel(in,t),
       inherited(container),
       traits([physical,object,fully_corporial,thinkable,moveable]),
       inherited(physical),
       can_be(move,t),
       can_be(touch,t),
       cleanliness=clean,
       class_desc(["kind is corporial","kind is normally thinkable","kind is an Movable Object"]),
       inherited(fully_corporial),
       can_be(examine,t),
       inherited(thinkable),
       sp=adjs,
       inherited(moveable),
       inherited(flask),
       inherited(cup),
       co([ shape=cup,
            inherit(cup,t),
            sp(nouns,
               [cup]) ]) ]),
  type_props(rock,
     [ traits(rock),
       sp=nouns ]),
  type_props(apple,
     [ traits(apple),
       sp=nouns ]),
  type_props(table_leg,
     [ traits(table_leg),
       sp=nouns ]),
  type_props(memorize_perceptq,
     [ traits(memorize_perceptq),
       sp=nouns ]),
  type_props(impulsive,
     [ traits(impulsive),
       sp=nouns ]),
  type_props(watch,
     [ traits(watch),
       sp=nouns ]),
  type_props(shovel,
     [ traits(shovel),
       sp=nouns ]) ].
```
