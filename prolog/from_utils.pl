:- module(from_utils, [from_to_file/2,
		       from_to_line/2,
		       from_to_file_line_pos/5,
		       subsumes_from/2]).

:- use_module(library(prolog_clause),   []).
:- use_module(library(prolog_codewalk), []).

from_to_file_line_pos(clause_term_position(ClauseRef, TermPos), File, CLine, TLine, Pos) :-
    clause_property(ClauseRef, file(File)),
    clause_property(ClauseRef, line_count(CLine)),
    ( compound(TermPos),
      arg(1, TermPos, CharCount),
      integer(CharCount)
    ->setup_call_cleanup('$push_input_context'(file_line),
			 prolog_codewalk:filepos_line(File, CharCount, TLine, Pos),
			 '$pop_input_context')
    ; true
    ).
from_to_file_line_pos(clause(ClauseRef), File, CLine, _, _) :-
    clause_property(ClauseRef, file(File)),
    clause_property(ClauseRef, line_count(CLine)).
from_to_file_line_pos(file_term_position(File, TermPos), File, _, Line, Pos) :-
    arg(1, TermPos, CharCount),
    setup_call_cleanup(
       '$push_input_context'(file_line),
       prolog_codewalk:filepos_line(File, CharCount, Line, Pos),
       '$pop_input_context').
from_to_file_line_pos(file(File, Line, _, _), File, Line, _, _).

subsumes_from(From1, From2) :-
    from_to_file_line_pos(From1, File1, CLine1, TLine1, Pos1),
    from_to_file_line_pos(From2, File2, CLine2, TLine2, Pos2),
    subsumes_term(flp(File1, CLine1, TLine1, Pos1),
		  flp(File2, CLine2, TLine2, Pos2)).

from_to_file(clause_term_position(ClauseRef, _), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(clause(ClauseRef), File) :-
    clause_property(ClauseRef, file(File)).
from_to_file(file_term_position(File, _), File).
from_to_file(file(File, _, _, _), File).

from_to_line(clause_term_position(ClauseRef, _), Line) :-
    clause_property(ClauseRef, line_count(Line)).
from_to_line(clause(ClauseRef), Line) :-
    clause_property(ClauseRef, line_count(Line)).
from_to_line(file_term_position(File, TermPos), Line) :-
    arg(1, TermPos, CharCount),
    setup_call_cleanup(
       '$push_input_context'(file_line),
       prolog_codewalk:filepos_line(File, CharCount, Line, _),
       '$pop_input_context').
from_to_line(file(_, Line, _, _), Line).
