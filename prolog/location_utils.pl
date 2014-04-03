:- module(location_utils, [property_location/3, predicate_location/2,
			   property_from/3, record_location_dynamic/2,
			   predicate_from/2, cleanup_locations/3,
			   from_location/2, from_to_file/2, in_set/2,
			   in_dir/2, r_true/1, record_location_meta/4,
			   record_location/3, option_filechk/2,
			   option_dirchk/2]).

:- use_module(library(database_fact)).

:- dynamic
    record_locations:declaration_location/3.
:- multifile
    record_locations:declaration_location/3.

from_location(From, Location) :-
	prolog:message_location(From, Location, []),
	!.
from_location(From, From).

from_to_file(clause_term_position(ClauseRef, _), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(clause(ClauseRef), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(file_term_position(File, _), File).
from_to_file(file(File, _, _, _), File).

in_set(FileL, File) :-
    memberchk(File, FileL).

in_dir(DirL, File) :-
    member(Dir, DirL),
    directory_file_path(Dir, _, File),
    !.

r_true(_).

option_files(AliasL, FileChk) :-
    findall(File,
	    ( member(Alias, AliasL),
	      absolute_file_name(Alias, Pattern, [file_type(prolog),
						  solutions(all)]),
	      expand_file_name(Pattern, FileL0 ),
	      member(File, FileL0 )),
	    FileL),
    FileChk = in_set(FileL).

option_dirs(AliasL, FileChk) :-
    findall(Dir,
	    ( member(Alias, AliasL),
	      absolute_file_name(Alias, Pattern, [file_type(directory),
						  solutions(all)]),
	      expand_file_name(Pattern, DirL),
	      member(Dir, DirL)),
	    DirL),
    FileChk = in_dir(DirL).

option_dirchk(OptionL, DirChk) :-
    ( memberchk(dirs(AliasL), OptionL)
    ->option_dirs(AliasL, DirChk)
    ; memberchk(dir(Alias), OptionL)
    ->option_dirs([Alias], DirChk)
    ; DirChk = r_true
    ).
    
option_filechk(OptionL, FileChk) :-
    ( memberchk(files(AliasL), OptionL)
    ->option_files(AliasL, FileChk)
    ; memberchk(file(Alias), OptionL)
    ->option_dirs([Alias], FileChk)
    ; memberchk(dirs(AliasL), OptionL)
    ->option_dirs(AliasL, FileChk)
    ; memberchk(dir(Alias), OptionL)
    ->option_dirs([Alias], FileChk)
    ; FileChk = r_true
    ).

property_location(Prop, Declaration, Location) :-
	property_from(Prop, Declaration, From),
	from_location(From, Location).

% non det
property_from(PI, Declaration, From) :-
	( ( record_locations:declaration_location(PI, Declaration, From)
	  ; definition_location(PI, Declaration, From)
	  ) *-> true
	; From = []
	).

definition_location(M:F/A, Declaration, From) :-
	functor(H, F, A),
	P = M:H,
	predicate_properties(P, List),
	Declaration = predicate(List),
	predicate_from(P, From).

predicate_location(P, Loc) :-
    predicate_from(P, From),
    from_location(From, Loc).

:- meta_predicate predicate_properties(:,-).
predicate_properties(P, List) :-
	findall(Prop,
		( predicate_property(P, Prop),
		  \+ memberchk(Prop, [interpreted,
				      visible,
				      number_of_rules(_),
				      imported_from(_),
				      file(_),
				      line_count(_)])
		), List).

:- meta_predicate predicate_from(:,-).

predicate_from(P, file(File, Line, -1, 0)) :-
	predicate_property(P, file(File)),
	predicate_property(P, line_count(Line)).

record_location(PI, Type, From) :-
    ( record_locations:declaration_location(PI, Type, From)
    ->true
    ; assertz(record_locations:declaration_location(PI, Type, From))
    ).

:- meta_predicate record_location_meta(+,+,3,3).
record_location_meta(MCall, From, FactBuilder, Recorder) :-
    (predicate_property(MCall, imported_from(IM)) -> true ; IM:_ = MCall),
    MCall = SM:Call,
    call(FactBuilder, Def, IM:Call, MFact),
    nonvar(MFact),
    ( MFact = M:Fact -> true
    ; (predicate_property(SM:MFact, imported_from(M)) -> true ; M = SM),
      Fact = MFact
    ),
    nonvar(M),
    nonvar(Fact),
    functor(Fact, F, A),
    call(Recorder, M:F/A, dynamic(Def, Call), From),
    fail.
record_location_meta(_, _, _, _).

record_location_dynamic(MCall, From) :-
    record_location_meta(MCall, From, database_fact_ort, record_location).

cleanup_locations(PI, Type, From) :-
    retractall(record_locations:declaration_location(PI, Type, From)).
