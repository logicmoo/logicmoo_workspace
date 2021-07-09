:-module(parser_chat80, [chat80/0, chat80/3, chat80/2, test_chat80_regressions/0, t80/0 /*, t10/0, t14/0, */ %t11/0, t12/0, t13/0, 
 ]).

:- set_module(class(library)).

:- set_how_virtualize_file(false).
/** <module>
% Imperitive Sentence Parser (using DCG)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren, |
|		SRI International, 333 Ravenswood Ave., Menlo Park, |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira, |
|		Dept. of Architecture, University of Edinburgh, |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/
% :- include(logicmoo('vworld/moo_header.pl')).

% :- use_module(library(pfc_lib)).

%:- use_module(library(with_thread_local)).
%:- load_library_system(library(logicmoo_plarkc)).
%:- kb_shared(baseKB:que/2).
%:- '$set_source_module'(baseKB).
%:- '$set_typein_module'(baseKB).

:- set_prolog_flag(expect_pfc_file, never).


%:- parser_chat80:export(parser_chat80:theText80/3).
%:- import(parser_chat80:theText80/3).
:- reexport(parser_e2c).
:- reexport(parser_tokenize).
%:- use_module(pldata(clex_iface)).
%:- use_module(parser_chat80, [plt/0, print_tree/1]).

:- use_module(library(logicmoo_nlu/parser_sharing)).
:- use_module(library(pfc_lib)).
:- set_prolog_flag(expect_pfc_file, never).

:- absolute_file_name('../../ext/', Dir, [file_type(directory)]),
   asserta_new(user:file_search_path(logicmoo_nlu_ext, Dir)).

:- thread_local(t_l:disable_px/0).
:- thread_local(t_l:usePlTalk/0).
:- thread_local(t_l:useAltPOS/0).
:- thread_local(t_l:tracing80/0).
:- thread_local(t_l:chat80_interactive/0).
:- thread_local(t_l:useOnlyExternalDBs/0).
:- dynamic(thglobal:use_cyc_database/0).

cycQuery80(Q):- current_predicate(_, Q), call(Q).

:- retractall(t_l:disable_px).
:- asserta(t_l:disable_px).

:- shared_parser_data(talkdb:talk_db/2).
:- shared_parser_data(talkdb:talk_db/3).
:- shared_parser_data(talkdb:talk_db/6).
:- shared_parser_data(installed_converter/1).

:- shared_parser_data(transitive_subclass/2).
:- shared_parser_data(tSet/1).
:- shared_parser_data(ttFormatType/1).
:- shared_parser_data(capitalized/5).
:- shared_parser_data(isa/2).
:- shared_parser_data(mpred_arity/2).
:- shared_parser_data(posName/1).


:- shared_parser_data(parser_chat80:longitude80/2).
:- shared_parser_data(parser_chat80:latitude80/2).

%:- share_mp(term_depth/2).

:- share_mp(common_logic_kb_hooks:cyckb_t/1).
:- share_mp(common_logic_kb_hooks:cyckb_t/2).
:- share_mp(common_logic_kb_hooks:cyckb_t/3).
:- share_mp(common_logic_kb_hooks:cyckb_t/4).
:- share_mp(common_logic_kb_hooks:cyckb_t/5).
:- share_mp(common_logic_kb_hooks:cyckb_t/6).
:- share_mp(common_logic_kb_hooks:cyckb_t/7).
:- share_mp(common_logic_kb_hooks:cyckb_t/8).
:- share_mp(pfc_lib:call_u/1).
:- share_mp(memoize_pos_to_db/4).
:- share_mp(must_test_801/3).

/*
:- meta_predicate holds_truthvalue(*, *). %  0, * = breaks it
:- meta_predicate satisfy(*). %  0 = breaks it

%:- meta_predicate theTextL(*, *, 0, *, *, *, *).
:- meta_predicate call_with_limits0(0).
:- meta_predicate exception(0).
:- meta_predicate call_in_banner(*, 0).
:- meta_predicate if_try(0, 0).
:- meta_predicate safely_call(0).
:- meta_predicate control80(4, *).
:- meta_predicate process_run_real(4, *, *, *, *).


%:- meta_predicate no_repeats_must(0).
%:- meta_predicate call_with_limits(0).
:- meta_predicate loop_check_chat80(0).
:- meta_predicate loop_check_chat80(0, 0).

:- meta_predicate hi80(4, *).
:- meta_predicate plt_call(*, *, 0).
:- meta_predicate plt2_call(*, *, 0).
:- meta_predicate process_run(4, *, *, *).
:- meta_predicate process_run_diff(4, *, *, *).
*/

:- op(600, xfy, --).
% :- op(450, xfy, ((:))).

:- if( \+ current_op(_, _, (user:'&'))).
:- op(400, xfy, ((user:'&'))).
:- endif.

:- op(300, fx, (('`'))).
:- op(200, xfx, ((--))).

:- baseKB:setup_mpred_ops.
% :- register_module_type(utility).


%:- shared_parser_data(parser_chat80:(contains0/2, country/8, city/3, borders/2, in_continent/2)).
:- shared_parser_data(parser_chat80:contains/2).
:- shared_parser_data(parser_chat80:trans_LF/9).
:- shared_parser_data(parser_chat80:det/7).
:- shared_parser_data(parser_chat80:sentence80/5).
:- shared_parser_data(parser_chat80:noun/6).

chat80_t(UIn):- chat80_t(UIn, O1, _O2), fmt(O1).
chat80_t(UIn, O1, O2):-
   convert_to_sel_string(fail, a, =, UIn, Mid), !,
   process_run_real(_Callback, _StartParse, Mid, O1, O2),
   flush_output_safe, !.

:- shared_parser_data(parser_chat80:chat80/1).
:- shared_parser_data(parser_chat80:chat80/2).
:- shared_parser_data(parser_chat80:chat80/3).

:- discontiguous(chat80/1).
:- discontiguous(chat80/2).
:- discontiguous(chat80/3).

contains_subterm(Term, Sub):- \+ \+ (sub_term(ST, Term), Sub==ST).
is_trait(X) :- ground(X), \+ string(X), \+ \+ (chat80(_XXX, _Ans, Traits), contains_subterm(Traits, X)).
with_traits(X):- forall((chat80(XX, Ans, Traits), contains_subterm(Traits, X)), chat80(XX, Ans, Traits)).

:- set_how_virtualize_file(part).

:- set_prolog_flag(expect_pfc_file, some_preds).
parser_chat80:chat80(X):- awc, ground(X), !, (is_trait(X)-> with_traits(X) ; test_chat80(X)).
parser_chat80:chat80(X, Ans):- awc, ground(X), !, chat80(X), ignore((nonvar(Ans), dmsg(answersShouldBe(Ans)))).
parser_chat80:chat80(X, Ans, Traits):- awc, ground(X), !, chat80(X, Ans), ignore((nonvar(Traits), dmsg(traitsShouldBe(Traits)))).
:- set_prolog_flag(expect_pfc_file, never).

:- set_how_virtualize_file(false).

% :- style_check(+discontiguous).
:- asserta((t_l:enable_src_loop_checking)).

numbervars80(Term, Start, End):- numbervars(Term, Start, End, [attvar(bind), functor_name('$VAR'), singletons(false)]).

copy_term80(Term, Copy):- copy_term(Term, Copy), !.

% ran on server start
must_test_80_sanity([what, rivers, are, there, ?], [sent([what, rivers, are, there, ?]), parse(whq(feature&river-B, s(np(3+plu, np_head(int_det(feature&river-B), [], river), []), verb(be, active, pres+fin, [], pos), [void], []))), sem((answer([A]):-river(A), A^true)), qplan((answer([B]):-river(B), B^true)), answers([amazon, amu_darya, amur, brahmaputra, colorado, congo_river, cubango, danube, don, elbe, euphrates, ganges, hwang_ho, indus, irrawaddy, lena, limpopo, mackenzie, mekong, mississippi, murray, niger_river, nile, ob, oder, orange, orinoco, parana, rhine, rhone, rio_grande, salween, senegal_river, tagus, vistula, volga, volta, yangtze, yenisei, yukon, zambesi])], [time(0.0)]).
must_test_80_sanity([what, countries, are, there, in, europe, ?], [sent([what, countries, are, there, in, europe, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(be, active, pres+fin, [], pos), [void], [prep_phrase(prep(in), np(3+sin, name(europe), []))]))), sem((answer([A]):-country(A), in(A, europe))), qplan((answer([A]):-in(A, europe), {country(A)})), answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])], [time(0.0010000000000000009)]).
must_test_80_sanity([which, country, '\'', s, capital, is, london, ?], [sent([which, country, '\'', s, capital, is, london, ?]), parse(whq(feature&place&country-B, s(np(3+sin, np_head(det(the(sin)), [], capital), [prep_phrase(poss, np(3+sin, np_head(int_det(feature&place&country-B), [], country), []))]), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(london), []))], []))), sem((answer([A]):-country(A), capital(A, london))), qplan((answer([A]):-capital(A, london), {country(A)})), answers([united_kingdom])], [time(0.0010000000000000009)]).
must_test_80_sanity([what, is, the, total, area, of, countries, south, of, the, equator, and, not, in, australasia, ?], [sent([what, is, the, total, area, of, countries, south, of, the, equator, and, not, in, australasia, ?]), parse(whq(A-B, s(np(3+sin, wh(A-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [adj(total)], area), [prep_phrase(prep(of), np(3+plu, np_head(generic, [], country), [conj(and, reduced_rel(feature&place&country-F, s(np(3+plu, wh(feature&place&country-F), []), verb(be, active, pres+fin, [], pos), [arg(pred, prep_phrase(prep(southof), np(3+sin, name(equator), [])))], [])), reduced_rel(feature&place&country-F, s(np(3+plu, wh(feature&place&country-F), []), verb(be, active, pres+fin, [], neg), [arg(pred, prep_phrase(prep(in), np(3+sin, name(australasia), [])))], [])))]))]))], []))), sem((answer([A]):-B^ (setof(C:[D], (area(D, C), country(D), southof(D, equator), \+in(D, australasia)), B), aggregate(total, B, A)))), qplan((answer([E]):-D^ (setof(C:[B], (southof(B, equator), area(B, C), {country(B)}, {\+in(B, australasia)}), D), aggregate(total, D, E)))), answers([10239--ksqmiles])], [time(0.0010000000000000009)]).

% ran on regression testing
must_test_80(U, R, O):- must_test_80_sanity(U, R, O).
must_test_80([does, afghanistan, border, china, ?], [sent([does, afghanistan, border, china, ?]), parse(q(s(np(3+sin, name(afghanistan), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(china), []))], []))), sem((answer([]):-borders(afghanistan, china))), qplan((answer([]):-{borders(afghanistan, china)})), answers([true])], [time(0.0)]).
must_test_80([what, is, the, capital, of, upper_volta, ?], [sent([what, is, the, capital, of, upper_volta, ?]), parse(whq(feature&city-B, s(np(3+sin, wh(feature&city-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], capital), [prep_phrase(prep(of), np(3+sin, name(upper_volta), []))]))], []))), sem((answer([A]):-capital(upper_volta, A))), qplan((answer([A]):-capital(upper_volta, A))), answers([ouagadougou])], [time(0.0010000000000000009)]).
must_test_80([where, is, the, largest, country, ?], [sent([where, is, the, largest, country, ?]), parse(whq(feature&place&A-B, s(np(3+sin, np_head(det(the(sin)), [sup(most, adj(large))], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, prep_phrase(prep(in), np(_, np_head(int_det(feature&place&A-B), [], place), [])))], []))), sem((answer([A]):-B^ (C^ (setof(D:E, (country(E), area(E, D)), C), aggregate(max, C, B)), place(A), in(B, A)))), qplan((answer([F]):-E^D^ (setof(C:B, (country(B), area(B, C)), D), aggregate(max, D, E), in(E, F), {place(F)}))), answers([asia, northern_asia])], [time(0.0009999999999999731)]).
must_test_80([which, countries, are, european, ?], [sent([which, countries, are, european, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, adj(european))], []))), sem((answer([A]):-country(A), european(A))), qplan((answer([A]):-european(A), {country(A)})), answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])], [time(0.0)]).
must_test_80([which, is, the, largest, african, country, ?], [sent([which, is, the, largest, african, country, ?]), parse(whq(feature&place&country-B, s(np(3+sin, wh(feature&place&country-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [sup(most, adj(large)), adj(african)], country), []))], []))), sem((answer([A]):-B^ (setof(C:D, (country(D), area(D, C), african(D)), B), aggregate(max, B, A)))), qplan((answer([D]):-C^ (setof(B:A, (african(A), {country(A)}, area(A, B)), C), aggregate(max, C, D)))), answers([sudan])], [time(0.0)]).
must_test_80([how, large, is, the, smallest, american, country, ?], [sent([how, large, is, the, smallest, american, country, ?]), parse(whq(measure&area-B, s(np(3+sin, np_head(det(the(sin)), [sup(most, adj(small)), adj(american)], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, value(adj(large), wh(measure&area-B)))], []))), sem((answer([A]):-B^ (C^ (setof(D:E, (country(E), area(E, D), american(E)), C), aggregate(min, C, B)), area(B, A)))), qplan((answer([E]):-D^C^ (setof(B:A, (american(A), {country(A)}, area(A, B)), C), aggregate(min, C, D), area(D, E)))), answers([0--ksqmiles])], [time(0.0)]).
must_test_80([what, is, the, ocean, that, borders, african, countries, and, that, borders, asian, countries, ?], [sent([what, is, the, ocean, that, borders, african, countries, and, that, borders, asian, countries, ?]), parse(whq(feature&place&seamass-B, s(np(3+sin, wh(feature&place&seamass-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], ocean), [conj(and, rel(feature&place&seamass-C, s(np(3+sin, wh(feature&place&seamass-C), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(generic, [adj(african)], country), []))], [])), rel(feature&place&seamass-C, s(np(3+sin, wh(feature&place&seamass-C), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(generic, [adj(asian)], country), []))], [])))]))], []))), sem((answer([A]):-ocean(A), B^ (country(B), african(B), borders(A, B)), C^ (country(C), asian(C), borders(A, C)))), qplan((answer([A]):-B^C^ (ocean(A), {borders(A, B), {african(B)}, {country(B)}}, {borders(A, C), {asian(C)}, {country(C)}}))), answers([indian_ocean])], [time(0.0020000000000000018)]).
must_test_80([what, are, the, capitals, of, the, countries, bordering, the, baltic, ?], [sent([what, are, the, capitals, of, the, countries, bordering, the, baltic, ?]), parse(whq(feature&city-B, s(np(3+plu, wh(feature&city-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], capital), [prep_phrase(prep(of), np(3+plu, np_head(det(the(plu)), [], country), [reduced_rel(feature&place&country-D, s(np(3+plu, wh(feature&place&country-D), []), verb(border, active, inf, [prog], pos), [arg(dir, np(3+sin, name(baltic), []))], []))]))]))], []))), sem((answer([D]):-setof([A]:C, (country(A), borders(A, baltic), setof(B, capital(A, B), C)), D))), qplan((answer([H]):-setof([E]:G, (country(E), borders(E, baltic), setof(F, capital(E, F), G)), H))), answers([[[denmark]:[copenhagen], [east_germany]:[east_berlin], [finland]:[helsinki], [poland]:[warsaw], [soviet_union]:[moscow], [sweden]:[stockholm], [west_germany]:[bonn]]])], [time(0.0010000000000000009)]).
must_test_80([which, countries, are, bordered, by, two, seas, ?], [sent([which, countries, are, bordered, by, two, seas, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(border, passive, pres+fin, [], pos), [], [prep_phrase(prep(by), np(3+plu, np_head(quant(same, (2)), [], sea), []))]))), sem((answer([A]):-country(A), numberof(B, (sea(B), borders(B, A)), 2))), qplan((answer([B]):-numberof(A, (sea(A), borders(A, B)), 2), {country(B)})), answers([egypt, iran, israel, saudi_arabia, turkey])], [time(0.0)]).
must_test_80([how, many, countries, does, the, danube, flow, through, ?], [sent([how, many, countries, does, the, danube, flow, through, ?]), parse(whq(feature&place&country-B, s(np(3+sin, name(danube), []), verb(flow, active, pres+fin, [], pos), [], [prep_phrase(prep(through), np(3+plu, np_head(quant(same, wh(feature&place&country-B)), [], country), []))]))), sem((answer([A]):-numberof(B, (country(B), flows(danube, B)), A))), qplan((answer([B]):-numberof(A, (flows(danube, A), {country(A)}), B))), answers([6])], [time(0.0010000000000000009)]).
must_test_80([what, is, the, average, area, of, the, countries, in, each, continent, ?], [sent([what, is, the, average, area, of, the, countries, in, each, continent, ?]), parse(whq(A-C, s(np(3+sin, wh(A-C), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [adj(average)], area), [prep_phrase(prep(of), np(3+plu, np_head(det(the(plu)), [], country), [prep_phrase(prep(in), np(3+sin, np_head(det(each), [], continent), []))]))]))], []))), sem((answer([B, E]):-continent(B), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]] ^  (setof(D:[C], (area(C, D), country(C), in(C, B)), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]]), aggregate(average, [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]], E)))), qplan((answer([F, J]):-continent(F), I^ (setof(H:[G], (area(G, H), country(G), in(G, F)), I), aggregate(average, I, J)))), answers([[europe, 58.84375--ksqmiles]])], [time(0.0040000000000000036)]).
must_test_80([is, there, more, than, one, country, in, each, continent, ?], [sent([is, there, more, than, one, country, in, each, continent, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(quant(more, (1)), [], country), [prep_phrase(prep(in), np(3+sin, np_head(det(each), [], continent), []))]))], []))), sem((answer([]):- \+A^ (continent(A), \+C^ (numberof(B, (country(B), in(B, A)), C), C>1)))), qplan((answer([]):- \+D^ (continent(D), \+F^ (numberof(E, (country(E), in(E, D)), F), F>1)))), answers([false])], [time(0.0010000000000000009)]).


must_test_80([is, there, some, ocean, that, does, not, border, any, country, ?], [sent([is, there, some, ocean, that, does, not, border, any, country, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(some), [], ocean), [rel(feature&place&seamass-B, s(np(3+sin, wh(feature&place&seamass-B), []), verb(border, active, pres+fin, [], neg), [arg(dir, np(3+sin, np_head(det(any), [], country), []))], []))]))], []))), sem((answer([]):-A^ (ocean(A), \+B^ (country(B), borders(A, B))))), qplan((answer([]):-A^{ocean(A), {\+B^ (borders(A, B), {country(B)})}})), answers([true])], [time(0.0010000000000000009)]).
must_test_80([what, are, the, countries, from, which, a, river, flows, into, the, black_sea, ?], [sent([what, are, the, countries, from, which, a, river, flows, into, the, black_sea, ?]), parse(whq(feature&place&country-B, s(np(3+plu, wh(feature&place&country-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], country), [rel(feature&place&country-D, s(np(3+sin, np_head(det(a), [], river), []), verb(flow, active, pres+fin, [], pos), [], [prep_phrase(prep(from), np(3+plu, wh(feature&place&country-D), [])), prep_phrase(prep(into), np(3+sin, name(black_sea), []))]))]))], []))), sem((answer([A]):-setof(B, (country(B), C^ (river(C), flows(C, B, black_sea))), A))), qplan((answer([C]):-setof(B, A^ (flows(A, B, black_sea), {country(B)}, {river(A)}), C))), answers([[romania]])], [time(0.0010000000000000009)]).
must_test_80([what, percentage, of, countries, border, each, ocean, ?], [sent([what, percentage, of, countries, border, each, ocean, ?]), parse(whq(A-C, s(np(3+plu, np_head(int_det(A-C), [], percentage), [prep_phrase(prep(of), np(3+plu, np_head(generic, [], country), []))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(each), [], ocean), []))], []))), sem((answer([B, E]):-ocean(B), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]^ (setof(C, country(C), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]), 4^ (numberof(D, (one_of([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], D), borders(D, B)), 4), 156^ (card([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], 156), ratio(4, 156, E)))))), qplan((answer([F, L]):-ocean(F), H^ (setof(G, country(G), H), J^ (numberof(I, (one_of(H, I), borders(I, F)), J), K^ (card(H, K), ratio(J, K, L)))))), answers([[arctic_ocean, 2.5641025641025643]])], [time(0.0020000000000000018)]).
must_test_80([which, countries, with, a, population, exceeding, (10), million, border, the, atlantic, ?], [sent([which, countries, with, a, population, exceeding, (10), million, border, the, atlantic, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), [prep_phrase(prep(with), np(3+sin, np_head(det(a), [], population), [reduced_rel(measure&heads-C, s(np(3+sin, wh(measure&heads-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+plu, np_head(quant(same, (10)), [], million), []))], []))]))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(atlantic), []))], []))), sem((answer([A]):-B^ (population(A, B), exceeds(B, 10--million), country(A)), borders(A, atlantic))), qplan((answer([A]):-B^ (borders(A, atlantic), {population(A, B), {exceeds(B, 10--million)}}, {country(A)}))), answers([venezuela])], [time(0.0010000000000000009)]).
must_test_80([which, countries, have, a, population, exceeding, (10), million, ?], [sent([which, countries, have, a, population, exceeding, (10), million, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(have, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(a), [], population), [reduced_rel(measure&heads-C, s(np(3+sin, wh(measure&heads-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+plu, np_head(quant(same, (10)), [], million), []))], []))]))], []))), sem((answer([A]):-country(A), B^ (exceeds(B, 10--million), population(A, B)))), qplan((answer([A]):-B^ (country(A), {population(A, B), {exceeds(B, 10--million)}}))), answers([malaysia, uganda])], [time(0.0010000000000000009)]).


must_test_80([which, country, bordering, the, mediterranean, borders, a, country, that, is, bordered, by, a, country, whose, population, exceeds, the, population, of, india, ?], [sent([which, country, bordering, the, mediterranean, borders, a, country, that, is, bordered, by, a, country, whose, population, exceeds, the, population, of, india, ?]), parse(whq(feature&place&country-B, s(np(3+sin, np_head(int_det(feature&place&country-B), [], country), [reduced_rel(feature&place&country-B, s(np(3+sin, wh(feature&place&country-B), []), verb(border, active, inf, [prog], pos), [arg(dir, np(3+sin, name(mediterranean), []))], []))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(a), [], country), [rel(feature&place&country-C, s(np(3+sin, wh(feature&place&country-C), []), verb(border, passive, pres+fin, [], pos), [], [prep_phrase(prep(by), np(3+sin, np_head(det(a), [], country), [rel(feature&place&country-D, s(np(3+sin, np_head(det(the(sin)), [], population), [prep_phrase(poss, np(3+sin, wh(feature&place&country-D), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], population), [prep_phrase(prep(of), np(3+sin, name(india), []))]))], []))]))]))]))], []))), sem((answer([A]):-country(A), borders(A, mediterranean), B^ (country(B), C^ (country(C), D^ (population(C, D), E^ (population(india, E), exceeds(D, E))), borders(C, B)), borders(A, B)))), qplan((answer([B]):-C^D^E^A^ (population(india, A), borders(B, mediterranean), {country(B)}, {borders(B, C), {country(C)}, {borders(D, C), {country(D)}, {population(D, E), {exceeds(E, A)}}}}))), answers([turkey])], [time(0.0020000000000000018)]).

% must_test_80([is, there, more, than, one, country, in, each, brokenness, ?], [sent([is, there, more, than, one, country, in, each, continent, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(quant(more, (1)), [], country), [prep_phrase(prep(in), np(3+sin, np_head(det(each), [], continent), []))]))], []))), sem((answer([]):- \+A^ (continent(A), \+C^ (numberof(B, (country(B), in(B, A)), C), C>1)))), qplan((answer([]):- \+D^ (continent(D), \+F^ (numberof(E, (country(E), in(E, D)), F), F>1)))), answers([false])], [time(0.0010000000000000009)]).

% Wrong!
must_test_80([what, are, the, continents, no, country, in, which, contains, more, than, two, cities, whose, population, exceeds, (1), million, ?], [sent([what, are, the, continents, no, country, in, which, contains, more, than, two, cities, whose, population, exceeds, (1), million, ?]), parse(whq(feature&place&continent-B, s(np(3+plu, wh(feature&place&continent-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], continent), [rel(feature&place&continent-D, s(np(3+sin, np_head(det(no), [], country), [prep_phrase(prep(in), np(3+plu, wh(feature&place&continent-D), []))]), verb(contain, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(quant(more, (2)), [], city), [rel(feature&city-G, s(np(3+sin, np_head(det(the(sin)), [], population), [prep_phrase(poss, np(3+plu, wh(feature&city-G), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(quant(same, (1)), [], million), []))], []))]))], []))]))], []))), sem((answer([F]):-setof(A, (continent(A), \+B^ (country(B), in(B, A), E^ (numberof(C, (city(C), D^ (population(C, D), exceeds(D, 1--million)), in(C, B)), E), E>2))), F))), qplan((answer([L]):-setof(G, (continent(G), \+H^ (country(H), in(H, G), K^ (numberof(I, (city(I), J^ (population(I, J), exceeds(J, 1--million)), in(I, H)), K), K>2))), L))), answers([[africa, antarctica, australasia]])], [time(0.05499999999999999)]).


chat80(X, Ans, notraits):- var(X), (chat80(X, Ans); chat80(X)).
%chat80(X, Ans, [regressions|Props]):- var(X), must_test_80(Tokens, Props, _Time), member(answers(Ans), Props), into_acetext(Tokens, Ace), any_to_string(Ace, X).
chat80(X, Ans):- var(X), must_test_80(Tokens, Props, _Time), once(( member(answers(Ans), Props), into_acetext(Tokens, Ace), any_to_string(Ace, X))).

chat80(X):- var(X), must_test_80(Tokens, _, _Time), once((into_acetext(Tokens, Ace), any_to_string(Ace, X))).
chat80(X):- var(X), clause(chat80([L|IST]), true), once((into_acetext([L|IST], Ace), any_to_string(Ace, X))).
chat80(X):- var(X), clause(chat80(X, _, _), true).
chat80(X):- var(X), clause(ed(_, X, _), true).
chat80(X):- var(X), clause(chat80(X, _), true).
chat80(X):- var(X), clause(chat80_janw(X, _), true).
%chat80(X, Ans, traits):- var(X), chat80(X, Ans, _Traits).
%chat80(X, Ans, notraits):- var(X), chat80(X, Ans).


chat80("which countries are bordered by two seas?", [[egypt, iran, israel, saudi_arabia, turkey]]).

chat80("Which countries have a population exceeding 10 million?").
chat80("Which countries contain a city?").
chat80("Which countries contain 2 cities?").
chat80("Which countries contain 3 cities?").
chat80("Which countries contain more than 3 cities?").
chat80("Which countries contain more than 2 cities?").
chat80("Which continents contain more than 4 cities?").
chat80("Which asian countries have a population exceeding 10 million?").
chat80("What is the average area of the countries in each continent?").
chat80("What is a river?").
chat80("What is a river that is in asia?").
chat80("Which rivers are not in asia?").
chat80("How many rivers are not in asia?" , 25).
chat80("What is a river that is asian?").
%chat80("What is a river that is not happy?").
chat80("How many rivers are in asia?", 16).
chat80("How many asian countries have a population exceeding 10 million?", 20).
chat80("How many countries have a population exceeding 10 million?", 50).
chat80("How many countries have a population greater than 10 million?", 50).
% These are too clow for the Unit Tests
%chat80("What are the continents in which no country contains more than 3 cities?", [africa, antarctica, australasia, europe], slow).
%chat80("What are the continents containing a country in which contains more than 3 cities?", [america, asia, europe], slow).
chat80("What are the continents not containing a country?", [antarctica]).
chat80("What are the continents containing no countries?", [antarctica]).
chat80("What are the continents containing no country?", [antarctica]).

chat80("What are the continents no country in which contains more than two cities whose population exceeds 1 million ?", [africa, antarctica, australasia]).
chat80("What are the continents in which no country contains more than two cities whose population exceeds 1 million?", [africa, antarctica, australasia]).
%TODO chat8("What are the continents in which no country contains more than two cities whose population each exceed 1 million?", [africa, antarctica, australasia], borked).


%TODO chat80("What are the continents with a country containing at least two cities whose population each exceeds 1 million ?", [africa, antarctica, australasia]).
chat80("What are the continents containing a country in which contains more than two cities whose population exceeds 1 million?", [america, asia, europe]).


chat80("does afghanistan border china?").
chat80("what is the capital of upper_volta?").
chat80("where is the largest country?").
chat80("which countries are european?").
chat80("which country's capital is london?").
chat80("which is the largest african country?").
chat80("how large is the smallest american country?").
chat80("what is the ocean that borders african countries and that borders asian countries?").
chat80("what are the capitals of the countries bordering the baltic?").
chat80("how many countries does the danube flow through?").
chat80("what is the total area of countries south of the equator and not in australasia?").
chat80("what is the average area of the countries in each continent?").
chat80("is there more than one country in each continent?").
chat80("is there some ocean that does not border any country? ").
chat80("what are the countries from which a river flows into the black_sea?").
chat80("what are the continents no country in which contains more than two cities whose population exceeds 1 million? ").
chat80("which country bordering the mediterranean borders a country that is bordered by a country whose population exceeds the population of india?").
chat80("which countries have a population exceeding 10 million?").
chat80("which countries with a population exceeding 10 million border the atlantic?").
chat80("what percentage of countries border each ocean?").
chat80("what countries are there in europe?").


chat80_tests:-
  forall(chat80(X, Y, Z), chat80(X, Y, Z)).

%:- import(clex_iface:clex_adj/3).


:-export(test_chat80_regressions/0).
test_chat80_regressions:- locally(tracing80, locally_hide(thglobal:use_cyc_database,
   forall(must_test_80(U, R, O), process_run_diff(report, U, R, O)))).

:-export(test_chat80_sanity/0).
test_chat80_sanity:- locally(tracing80, locally_hide(thglobal:use_cyc_database, forall(must_test_80_sanity(U, R, O),
  process_run_diff(report, U, R, O)))).

baseKB:regression_test:- test_chat80_regressions.
baseKB:sanity_test:- test_chat80_sanity.
baseKB:feature_test:- chat80_tests.

% ===========================================================
% CHAT80 command
% ===========================================================
:- set_prolog_flag(expect_pfc_file, some_preds).
==>type_action_info(tHumanControlled, actChat80(ftListFn(ftTerm)), "Development test CHAT-80 Text for a human.  Usage: CHAT80 Cant i see the blue backpack?").

==>agent_call_command(_Gent, actChat80([])):- chat80.
==>agent_call_command(_Gent, actChat80(StringM)):- chat80(StringM).
:- set_prolog_flag(expect_pfc_file, never).

% ===========================================================
% CHAT80 REPL
% ===========================================================
:-thread_local t_l:chat80_interactive/0.
:-export(chat80/0).
chat80 :- locally(tracing80,
           locally(t_l:chat80_interactive,
            locally_hide(t_l:useOnlyExternalDBs,
             locally_hide(thglobal:use_cyc_database,
                  locally(t_l:usePlTalk, (told, repeat, prompt_read('CHAT80> ', U),
                      into_text80_atoms(U, WL), ((WL==[bye];WL==[end, '_', of, '_', file];control80(WL))))))))).

:- thread_local(t_l:into_form_code/0).
:- asserta(t_l:into_form_code).



:- if( \+ current_predicate( chat80_impl_dir/1)).
chat80_impl_dir(chat80/original).
:- endif.

:- chat80_impl_dir(Where), ensure_loaded(logicmoo_nlu_ext(Where/load)).	% XG generator


:- retract(t_l:into_form_code).

:- retractall(t_l:enable_src_loop_checking).

baseKB:mud_test(chat80_regressions, test_chat80_regressions).

%:- context_module(CM), module_predicates_are_exported(CM).
%:- context_module(CM), all_module_predicates_are_transparent(CM).

:- fixup_exports.

% :- context_module(CM), module_property(CM, exports(List)), moo_hide_show_childs(List).

% :- include(logicmoo('vworld/moo_footer.pl')).


t80:- bchat80_impl_dir(Where), baseKB:hi80(logicmoo_nlu_ext(Where/demo)).

:- use_module(parser_e2c).


