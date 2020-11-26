
lf_rewrite([null,B], true).
lf_rewrite([passive_agent,A], true).

lf_rewrite([tense,A,[present,continuous]], [tense,A,present]).
lf_rewrite([tense,A,[present,perfect]], [tense,A,past]).

lf_rewrite([you,X], [you,X]).
lf_rewrite([one,C], [one,C]).
lf_rewrite([it,C], [thing,C]).
lf_rewrite([physical_object,C], [thing,C]).

lf_rewrite([block,X], [block,X]).
lf_rewrite([pyramid,C], [pyramid,C]).
lf_rewrite([box,A], [box,A]).
lf_rewrite([table,A], [table,A]).
lf_rewrite([cube,D], [cube,D]).

lf_rewrite([size,X,big], [size,X,big]).
lf_rewrite([size,X,large], [size,X,big]).
lf_rewrite([size,C,small], [size,C,small]).

lf_rewrite([color,X,Color], [color,X,Color]).

lf_rewrite([stack_up,C,A,B,up], [stack_up,C,A,B]).

macro(be_or_sit_loc(LocPrepSense, LocPred),
      lf_rewrite([be, C, A, [LocPrepSense, B]],
		 [LocPred, C, A, B])).
macro(be_or_sit_loc(LocPrepSense, LocPred),
      lf_rewrite(([sit,C,A], [LocPrepSense,C,B]),
		 [LocPred, C, A, B])).
macro(be_or_sit_loc(LocPrepSense, LocPred),
      lf_rewrite(([there_is,C,A], [LocPrepSense,C,B]),
		 [LocPred, C, A, B])).

@be_or_sit_loc(in_loc, be_in_loc).
@be_or_sit_loc(inside_loc, be_in_loc).
@be_or_sit_loc(on_loc, be_on_loc).
@be_or_sit_loc(on_top_of_loc, be_on_loc).
@be_or_sit_loc(behind_loc, be_behind_loc).
@be_or_sit_loc(under_loc, be_under_loc).
@be_or_sit_loc(in_front_of_loc, be_in_front_of_loc).
@be_or_sit_loc(to_the_right_of_loc, be_to_the_right_of_loc).
@be_or_sit_loc(to_the_left_of_loc, be_to_the_left_of_loc).
@be_or_sit_loc(on_the_right_of_loc, be_to_the_right_of_loc).
@be_or_sit_loc(on_the_left_of_loc, be_to_the_left_of_loc).
@be_or_sit_loc(closest_to_loc, be_closest_to_loc).
@be_or_sit_loc(at_loc, be_at_loc).
@be_or_sit_loc(loc, be_at_loc).

macro(put_loc(LocPrepSense, LocPred),
      lf_rewrite(([ @put_or_similar, F, A, B], [LocPrepSense, F, C]),
		 [LocPred, F, A, B, C])).

macro(put_or_similar, put).
macro(put_or_similar, set).
macro(put_or_similar, place).
macro(put_or_similar, stack).

@put_loc(onto_loc, put_on_loc).
@put_loc(on_loc, put_on_loc).
@put_loc(on_top_of_loc, put_on_loc).
@put_loc(in_loc, put_in_loc).
@put_loc(inside_loc, put_in_loc).
@put_loc(into_loc, put_in_loc).
@put_loc(behind_loc, put_behind_loc).
@put_loc(under_loc, put_under_loc).
@put_loc(in_front_of_loc, put_in_front_of_loc).
@put_loc(to_the_right_of_loc, put_to_the_right_of_loc).
@put_loc(to_the_left_of_loc, put_to_the_left_of_loc).
@put_loc(on_the_right_of_loc, put_to_the_right_of_loc).
@put_loc(on_the_left_of_loc, put_to_the_left_of_loc).
@put_loc(closest_to_loc, put_closest_to_loc).
@put_loc(loc, put_at_loc).

lf_rewrite(([[bottom,C],[of,C,B]]), [bottom_of,C,B]).
lf_rewrite(([[top,C],[of,C,B]]), [top_of,C,B]).

lf_rewrite(([be,C,A,B], [color,B]), [has_color,C,A,B]).
lf_rewrite(([be,C,A,[[color,B]]]), [has_color,C,A,B]).

lf_rewrite(([duration, A, B], [now,B]), [is_now,A]).
lf_rewrite(([duration, A, B], [then,B]), [is_then,A]).

lf_rewrite([pick_up,C,A,B,up], [pick_up,C,A,B]).
lf_rewrite([stack_up,C,A,B,up], [stack_up,C,A,B]).
lf_rewrite([put_down,C,A,B,down], [put_down,C,A,B]).
lf_rewrite([put_back,C,A,B,back], [put_back,C,A,B]).
lf_rewrite([clear_off,C,A,B,off], [clear_off,C,A,B]).
lf_rewrite([take_off,C,A,B,off], [take_off,C,A,B]).
lf_rewrite([turn_over,C,A,B,over], [turn_over,C,A,B]).

lf_rewrite([grasp,C,A,B], [grasp,C,A,B]).
lf_rewrite([contain,C,A,B], [contain,C,A,B]).
lf_rewrite([support,C,A,B], [support,C,A,B]).
lf_rewrite([there_is,C,B], [exists,C,B]).

/*
lf_rewrite(, ).
*/


