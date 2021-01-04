/*  Logicmoo Debug Tools
% ===================================================================
% File 'instant_prolog_docs.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
% File: '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/instant_prolog_docs.pl'
:- module(prolog_refactor, [ ]).

:- set_module(class(library)).



:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).
% user:portray


:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(prolog_colour)).
:- use_module(library(pldoc/doc_colour)).
:- use_module(library(pldoc/doc_html)).
%:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_modes)).
%:- use_module(library(pldoc/doc_process)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
%:- use_module(library(prolog_xref)).

:- meta_predicate
    source_to_html(+, +, :).




/*  source pretty-printer

This module colourises Prolog  source  using   HTML+CSS  using  the same
cross-reference based technology as used by PceEmacs.

@tbd    Create hyper-links to documentation and definitions.
@author Jan Wielemaker
*/

:- predicate_options(source_to_html/3, 3,
                     [ format_comments(boolean),
                       header(boolean),
                       skin(callable),
                       stylesheets(list),
                       title(atom)
                     ]).


:- thread_local
    lineno/0,                       % print line-no on next output
    nonl/0,                         % previous tag implies nl (block level)
    id/1.                           % Emitted ids


%!  source_to_html(+In:filename, +Out, :Options) is det.
%
%   Colourise Prolog source as HTML. The idea   is to first create a
%   sequence of fragments and  then  to   apply  these  to the code.
%   Options are:
%
%     * format_comments(+Boolean)
%     If =true= (default), use PlDoc formatting for structured
%     comments.
%
%   Other options are passed to the following predicates:
%
%     * print_html_head/2
%     * print_html_footer/2.
%     * doc_fragments/6
%
%   @arg In         A filename.  Can also be an abstract name,
%                   which is subject to library(prolog_source)
%                   abstract file handling. See
%                   prolog_open_source/2.  Note that this cannot
%                   be a stream as we need to read the file three
%                   times: (1) xref, (2) assign colours and (3)
%                   generate HTML.
%   @arg Out        Term stream(Stream) or filename specification

source_to_html(Src, stream(Out), Options) :-
    !,
    retractall(lineno),             % play safe
    retractall(nonl),               % play safe
    retractall(id(_)),
    colour_fragments(Src, Fragments0),
    refactor_frags(Fragments0,Fragments),
    setup_call_cleanup(
        ( open_source(Src, In),
          asserta(user:thread_message_hook(_,_,_), Ref)
        ),
        ( 
          doc_fragments(Fragments, In, Out, [], State, Options),
          copy_rest(In, Out, State, State1),
          pop_state(State1, Out, In)
        ),
        ( erase(Ref),          
          finish_in(Out,In)
        )),!.

source_to_html(Src, FileSpec, Options) :-
    absolute_file_name(FileSpec, OutFile, [access(write)]),
    setup_call_cleanup(
        open(OutFile, write, Out, [encoding(utf8)]),
        source_to_html(Src, stream(Out), Options),
        close(Out)).

finish_in(_Out,In):- at_end_of_stream(In),close(In).
finish_in( Out,In):- get0(In,C),put_code(Out,C),finish_in( Out,In).

open_source(Id, Stream) :- prolog:xref_open_source(Id, Stream), !.
open_source(File, Stream) :- open(File, read, Stream).



%!  doc_fragments(+Fragments, +In, +Out, +State, +Options) is det.
%
%   Copy In to Out, inserting HTML elements using Fragments.

doc_fragments([], _, _, State, State, _):-!.
doc_fragments([H|T], In, Out, State0, State, Options) :-    
    start_doc_fragment(H, In, Out, State0, State1, Options),
    doc_fragments(T, In, Out, State1, State, Options),!.


%!  doc_fragment(+Fragment, +In, +Out,
%!                +StateIn, -StateOut, +Options) is det.
%
%   Print from current position upto the end of Fragment.  First
%   clause deals with structured comments.


start_doc_fragment(fragment(Start, End, Class, Sub), In, Out, StateP, State, Options):-    
    copy_to(In, Start, Out, StateP, State0), flush_output(Out),
    doc_fragment(fragment(Start, End, Class, Sub), In, Out, State0, State, Options).

doc_fragment(fragment(Start, End, Class, Sub), In, Out, State0, State, Options):-    
    peek_code(In, C), (member(C,[46,32,12,13])),     
    get0(In,_), put_code(Out, C), flush_output(Out), !, 
    Start2 is Start + 1,     
    doc_fragment(fragment(Start2, End, Class, Sub), In, Out, State0, State, Options).

doc_fragment(fragment(Start, End, Class, Sub), In, Out, State0, State, Options) :- 
  member(Class,[clause,directive,neck(directive)]),
  clause_fragment(fragment(Start, End, clause, Sub), In, Out, State0, State, Options).

doc_fragment(fragment(_, End, _, _Args), In, Out, State0, State, _Options) :- copy_to(In, End, Out, State0, State),!.

sub_clause_fragments([], _, _, State, State, _).
sub_clause_fragments([H|T], In, Out, State0, State, Options) :-
    sub_clause_fragment(H, In, Out, State0, State1, Options),
    sub_clause_fragments(T, In, Out, State1, State, Options).

clause_fragment(fragment(Start, End, Class,Sub), In, Out, State2, State, Options) :- 
    nl,nl,print_tree(fragment(Start, End, Class, Sub)),nl,
    %start_fragment(Class, In, Out, State0, State2),
    sub_clause_fragments(Sub, In, Out, State2, State3, Options),
    % copy_to(In, End, Out, State3, State4),  % TBD: pop-to?
    end_fragment(Out, In, State3, State),!.

sub_clause_fragment(fragment(Start, End, Class, Sub), In, Out, StateP, State, Options) :-
    copy_to(In, Start, Out, StateP, State1), flush_output(Out),
    member(Class,[neck(_),head(_,_), goal(_,_),singleton,fullstop,control,comment(_)]),        
    start_fragment(Class, In, Out, State1, State2),
    sub_clause_fragments(Sub, In, Out, State2, State3, Options),
    copy_to(In, End, Out, State3, State4),  % TBD: pop-to?
    end_fragment(Out, In, State4, State),!.

sub_clause_fragment(fragment(Start, End, Class, Sub), In, Out, State, State, Options) :-
    member(Class,[functor,goal_term(_,_),head_term(_,_)]),        
    % functor(Class,F,_),atom_codes(F,[C|_]),  format(Out,'/*~s*/ ',[[C]]),
    grab_term(In,Start,End, Term, _Source),
    transformed_term(Term,NewTerm), 
    write_trans_term(Out,NewTerm),!.

sub_clause_fragment(fragment(Start, End, Class, Sub), In, Out, State1, State, Options) :-
    member(Class,[functor,goal_term(_,_),head_term(_,_)]),        
    % functor(Class,F,_),atom_codes(F,[C|_]),  format(Out,'/*~s*/ ',[[C]]),
    start_fragment(Class, In, Out, State1, State2),
    sub_clause_fragments(Sub, In, Out, State2, State3, Options),
    copy_to(In, End, Out, State3, State4),  % TBD: pop-to?
    end_fragment(Out, In, State4, State),!.

sub_clause_fragment(fragment(_Start, End, Class, Sub), In, Out, State1, State, Options) :- 
    start_fragment(Class, In, Out, State1, State2),
    sub_clause_fragments(Sub, In, Out, State2, State3, Options),
    copy_to(In, End, Out, State3, State4),  % TBD: pop-to?
    end_fragment(Out, In, State4, State),!.

grab_term(In,Start,End, Term, _Source):- 
 seek(In, Start, +Method, -NewLocation).
start_fragment(atom, In, Out, State0, State) :-
    !,
    (   peek_code(In, C),
        C == 39
    ->  start_fragment(quoted_atom, In, Out, State0, State)
    ;   State = [nop|State0]
    ).
start_fragment(Class, _, Out, State, [Push|State]) :-
    element(Class, Tag, CSSClass),
    !,
    Push =.. [Tag,class(CSSClass)],
    (   anchor(Class, ID)
    ->  skip_format(Out, '<~w id="~w" class="~w">', [Tag, ID, CSSClass])
    ;   skip_format(Out, '<~w class="~w">', [Tag, CSSClass])
    ).
start_fragment(Class, _, Out, State, [span(class(SpanClass))|State]) :-
    functor(Class, SpanClass, _),
    skip_format(Out, '<span class="~w">', [SpanClass]).

end_fragment(_, _, [nop|State], State) :- !.
end_fragment(Out, In, [span(class(directive))|State], State) :-
    !,
    copy_full_stop(In, Out),
    skip_format(Out, '</span>', []),
    (   peek_code(In, 10),
        \+ nonl
    ->  assert(nonl)
    ;   true
    ).
end_fragment(Out, _, [Open|State], State) :-
    retractall(nonl),
    functor(Open, Element, _),
    skip_format(Out, '</~w>', [Element]).

pop_state([], _, _) :- !.
pop_state(State, Out, In) :-
    end_fragment(Out, In, State, State1),
    pop_state(State1, Out, In).


%!  anchor(+Class, -Label) is semidet.
%
%   True when Label is the =id= we   must  assign to the fragment of
%   class Class. This that  the  first   definition  of  a head with
%   the id _name/arity_.

anchor(head(_, Head), Id) :-
    callable(Head),
    functor(Head, Name, Arity),
    skip_format(atom(Id), '~w/~w', [Name, Arity]),
    (   id(Id)
    ->  fail
    ;   assertz(id(Id))
    ).

mode_anchor(Out, Mode) :-
    mode_anchor_name(Mode, Id),
    (   id(Id)
    ->  true
    ;   skip_format(Out, '<span id="~w"><span>', [Id]),
        assertz(id(Id))
    ).

assert_seen_mode(Mode) :-
    mode_anchor_name(Mode, Id),
    (   id(Id)
    ->  true
    ;   assertz(id(Id))
    ).

%!  copy_to(+In:stream, +End:int, +Out:stream, +State) is det.
%
%   Copy data from In to Out   upto  character-position End. Inserts
%   HTML entities for HTML the reserved characters =|<&>|=. If State
%   does not include a =pre= environment,   create  one and skip all
%   leading blank lines.

copy_to(In, End, Out, State, State) :-
    member(pre(_), State),
    !,
    copy_to(In, End, Out).
copy_to(In, End, Out, State, [pre(class(listing))|State]) :-
    skip_format(Out, '<pre class="listing">~n', []),
    line_count(In, Line0),
    read_to(In, End, Codes0),
    delete_leading_white_lines(Codes0, Codes, Line0, Line),
    assert(lineno),
    my_write_codes(Codes, Line, Out).

copy_codes(Codes, Line, Out, State, State) :-
    member(pre(_), State),
    !,
    my_write_codes(Codes, Line, Out).
copy_codes(Codes0, Line0, Out, State, State) :-
    skip_format(Out, '<pre class="listing">~n', []),
    delete_leading_white_lines(Codes0, Codes, Line0, Line),
    assert(lineno),
    my_write_codes(Codes, Line, Out).


%!  copy_full_stop(+In, +Out) is det.
%
%   Copy upto and including the .

copy_full_stop(In, Out) :-
    get_code(In, C0),
    copy_full_stop(C0, In, Out).

copy_full_stop(0'., _, Out) :- %'
    !,
    my_put_code(Out, 0'.). %'
copy_full_stop(C, In, Out) :-
    my_put_code(Out, C),
    get_code(In, C2),
    copy_full_stop(C2, In, Out).


%!  delete_leading_white_lines(+CodesIn, -CodesOut, +LineIn, -Line) is det.
%
%   Delete leading white lines. Used  after structured comments. The
%   last two arguments update the  start-line   number  of the <pre>
%   block that is normally created.

delete_leading_white_lines(Codes0, Codes, Line0, Line) :-
    append(LineCodes, [10|Rest], Codes0),
    all_spaces(LineCodes),
    !,
    Line1 is Line0 + 1,
    delete_leading_white_lines(Rest, Codes, Line1, Line).
delete_leading_white_lines(Codes, Codes, Line, Line).

%!  copy_without_trailing_white_lines(+In, +End, +StateIn, -StateOut) is det.
%
%   Copy input, but skip trailing white-lines. Used to copy the text
%   leading to a structured comment.

copy_without_trailing_white_lines(In, End, Out, State, State) :-
    member(pre(_), State),
    !,
    line_count(In, Line),
    read_to(In, End, Codes0),
    delete_trailing_white_lines(Codes0, Codes),
    my_write_codes(Codes, Line, Out).
copy_without_trailing_white_lines(In, End, Out, State0, State) :-
    copy_to(In, End, Out, State0, State).

delete_trailing_white_lines(Codes0, []) :-
    all_spaces(Codes0),
    !.
delete_trailing_white_lines(Codes0, Codes) :-
    append(Codes, Tail, [10|Rest], Codes0),
    !,
    delete_trailing_white_lines(Rest, Tail).
delete_trailing_white_lines(Codes, Codes).

%!  append(-First, -FirstTail, ?Rest, +List) is nondet.
%
%   Split List.  First part is the difference-list First-FirstTail.

append(T, T, L, L).
append([H|T0], Tail, L, [H|T]) :-
    append(T0, Tail, L, T).

all_spaces([]).
all_spaces([H|T]) :-
    code_type(H, space),
    all_spaces(T).

copy_to(In, End, Out) :-
    line_count(In, Line),
    read_to(In, End, Codes),
    (   debugging(htmlsrc)
    ->  length(Codes, Count),
        debug(htmlsrc, 'Copy ~D chars: ~s', [Count, Codes])
    ;   true
    ),
    my_write_codes(Codes, Line, Out).

read_to(In, End, Codes) :-
    character_count(In, Here),
    Len is End - Here,
    read_n_codes(In, Len, Codes).

%!  my_write_codes(+Codes, +Line, +Out) is det.
%
%   Write codes that have been read starting at Line.



%!  content_escape(+Code, +Out, +Line0, -Line) is det
%
%   Write Code to Out, while taking care of.
%
%           * Use HTML entities for =|<&>|=
%           * If a line-no-tag is requested, write it
%           * On \n, post a line-no request.  If nonl/0 is set,
%             do _not_ emit a newline as it is implied by the
%             closed environment.

content_escape(_, Out, L, _) :-
    (   lineno
    ->  retractall(lineno),
        write_line_no(L, Out),
        fail
    ;   fail
    ).
content_escape(0'\n, Out, L0, L) :- %'
    !,
    L is L0 + 1,
    (   retract(nonl)
    ->  true
    ;   my_nl(Out)
    ),
    assert(lineno).
content_escape(C, Out, L, L) :-
    my_put_code(Out, C).

write_line_no(LineNo, Out) :-
    nop(skip_format(Out, '<span class="line-no">~|~t~d~5+</span>', [LineNo])).

%!  copy_rest(+In, +Out, +StateIn, -StateOut) is det.
%
%   Copy upto the end of the input In.

copy_rest(In, Out, State0, State) :-
    copy_to(In, -1, Out, State0, State).

%!  read_n_codes(+In, +N, -Codes)
%
%   Read the next N codes from In as a list of codes. If N < 0, read
%   upto the end of stream In.

read_n_codes(_, N, Codes) :-
    N =< 0,
    !,
    Codes = [].
read_n_codes(In, N, Codes) :-
    get_code(In, C0),
    read_n_codes(N, C0, In, Codes).

read_n_codes(_, -1, _, []) :- !.
read_n_codes(1, C, _, [C]) :- !.
read_n_codes(N, C, In, [C|T]) :-
    get_code(In, C2),
    N2 is N - 1,
    read_n_codes(N2, C2, In, T).



%:- set_prolog_flag(verbose_load, full).
:- set_prolog_flag(verbose, normal).
%:- set_prolog_flag(verbose_autoload, true).

skip_format(A,B,C):- nop(format(A,B,C)).
not_skip_format(A,B,C):- format(A,B,C).
my_nl(Out):- nl(Out).
my_put_code(Out,C):- put_code(Out,C).

my_write_codes([], _, _).
% my_write_codes([H|T],_L0, Out):- format(Out,'"~s"\n',[[H|T]]),!.
my_write_codes([H|T], L0, Out) :-
    content_escape(H, Out, L0, L1),
    my_write_codes(T, L1, Out).

my_print_html(Out,Tokens):- print_html(Out,Tokens).
:- meta_predicate source_to_html(+,+,:).

%source_to_html:-  source_to_html('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_agi/prolog/episodic_memory/adv_axiom.pl').
source_to_html:-  source_to_html('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_agi/prolog/episodic_memory/knowledgeBaseCGI.pl').

source_to_html(Src):-
  source_to_html(Src, stream(user_output), []).

my_prolog_read_source_term(In,B,C,D):-  prolog_read_source_term(In,B,C,D).

refactor_frags(Term1,Term2):- map_tree_pred3(refactor_src,Term1,Term2),!.

map_tree_pred3(_,Arg1,Arg2):- var(Arg1),!,Arg2=Arg1,!.
map_tree_pred3(Pred,Arg1,Arg2):- call(Pred,Arg1,Arg2), Arg1\==Arg2,!.
map_tree_pred3(_ ,Arg1,Arg2):- \+ compound(Arg1), !, Arg2=Arg1.
map_tree_pred3(Pred,Arg1,Arg2):- 
  compound_name_arguments(Arg1,F1,ArgS1),
  maplist(map_tree_pred3(Pred),ArgS1,ArgS2),
  compound_name_arguments(Arg2,F1,ArgS2).

refactor_src(nathan,bob).
refactor_src(Term,E):- fail, is_list(Term),E=was_list,!.

