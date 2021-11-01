% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
%
% The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later version.
%
% The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE. See the GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with the Attempto
% Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.

:- if( current_module(clex) ).
:- module(clex_ape, [
		clex_switch/1,     % ?Switch
		set_clex_switch/1  % +Switch
	]).
:- format(user_error,"Renaming CLEX interface module from 'clex' to 'clex_ape' ",[]).

:- else.
:- module(clex, [
		clex_switch/1,     % ?Switch
		set_clex_switch/1  % +Switch
	]).
:- format(user_error,"Keeping CLEX interface module 'clex' ",[]).
:- endif.

	
:- use_module(library(error)).

/** <module> Common Lexicon Interface

This module contains the predicates for the management of the common lexicon that is compiled into
the executable.

@author Tobias Kuhn
@version 2008-07-17
*/


:- if(exists_source(pldata(clex_iface))).
:- reexport(pldata(clex_iface)).

:- else.


%% clex_file(-ClexFile)
%
% This predicate defines the clex-file that is loaded and compiled into the executable. In order to
% change this, you have to edit the source code and recompile.

clex_file(pldata('clex_lexicon_user1.nldata')):- exists_source(pldata('clex_lexicon_user1.nldata')).
clex_file(clex_lexicon):- exists_source(clex_lexicon).
clex_file(clex_lexicon):- exists_source('./clex_lexicon').
clex_file(clex_lexicon):- exists_source(clex_lexicon_small).
%clex_file(clex_lexicon_small).
%clex_file('').


% The predicates for the lexicon entries are declared dynamic. In this way, they don't fail if
% no entry exists.

:- multifile(adj_itr/2).
:- multifile(adj_itr_comp/2).
:- multifile(adj_itr_sup/2).
:- multifile(adj_tr/3).
:- multifile(adj_tr_comp/3).
:- multifile(adj_tr_sup/3).
:- multifile(adv/2).
:- multifile(adv_comp/2).
:- multifile(adv_sup/2).
:- multifile(dv_finsg/3).
:- multifile(dv_infpl/3).
:- multifile(dv_pp/3).
:- multifile(iv_finsg/2).
:- multifile(iv_infpl/2).
:- multifile(mn_pl/2).
:- multifile(mn_sg/2).
:- multifile(noun_mass/3).
:- multifile(noun_pl/3).
:- multifile(noun_sg/3).
:- multifile(pn_pl/3).
:- multifile(pn_sg/3).
:- multifile(pndef_pl/3).
:- multifile(pndef_sg/3).
:- multifile(prep/2).
:- multifile(tv_finsg/2).
:- multifile(tv_infpl/2).
:- multifile(tv_pp/2).

:- dynamic adv/2.
:- dynamic adv_comp/2.
:- dynamic adv_sup/2.
:- dynamic adj_itr/2.
:- dynamic adj_itr_comp/2.
:- dynamic adj_itr_sup/2.
:- dynamic adj_tr/3.
:- dynamic adj_tr_comp/3.
:- dynamic adj_tr_sup/3.
:- dynamic noun_sg/3.
:- dynamic noun_pl/3.
:- dynamic noun_mass/3.
:- dynamic mn_sg/2.
:- dynamic mn_pl/2.
:- dynamic pn_sg/3.
:- dynamic pn_pl/3.
:- dynamic pndef_sg/3.
:- dynamic pndef_pl/3.
:- dynamic iv_finsg/2.
:- dynamic iv_infpl/2.
:- dynamic tv_finsg/2.
:- dynamic tv_infpl/2.
:- dynamic tv_pp/2.
:- dynamic dv_finsg/3.
:- dynamic dv_infpl/3.
:- dynamic dv_pp/3.
:- dynamic prep/2.


% Load the clex-file
:- style_check(-discontiguous).
%:- clex_file(ClexFile), ( ClexFile == '' ; load_files(ClexFile, [encoding(utf8)]) ).
:- forall(clex_file(ClexFile), ( ClexFile == '' ; load_files(ClexFile, [encoding(iso_latin_1)]) )).
%:-include(library(pldata/clex_lexicon_user1)).
:- style_check(+discontiguous).

:- endif.


:- include(clex_lexicon).

%% clex_switch(?Switch)
%
% This predicate returns 'on' if clex is switched on, or 'off' otherwise.

:- dynamic(clex_switch/1).

clex_switch(on).


%% set_clex_switch(+Switch)
%
% This predicate switches clex on (Switch='on') or off (Switch='off').

set_clex_switch(Switch) :-
    must_be(oneof([on,off]), Switch),
    retractall(clex_switch(_)),
    assert(clex_switch(Switch)).




:- '$set_source_module'(clex).

alc_s(L,S,A):- atomic_list_concat(L,S,A).
alc_j(L,A):- atomic_list_concat(L,A).
alc_jy(A,Act,Est,B,YR):- alc_j([A,Act,Est,B],YR).
% alc_jy(A,Act,Est,B,YR):- alc_j([A,Act,B,Est],YR).
x_good(Sep,X):-atom_chars(X,[C|_]), (Sep==C ; \+ char_type(C,alpha)),!.
x_good(X):- atom_chars(X,[C|_]),  \+ char_type(C,alpha),!.

learned_as_type(Act,X,Est,Y):-  quietly(some_names_two(Act,X,Est,Y)),!.
% clex:learned_as_type('action',X,'', 'eating.action').
% clex:learned_as_type('action',X,'', 'eating.action5').
% clex:learned_as_type('action',X,'', 'action5.eating').
% clex:learned_as_type('action',X,'', 'action.eating').
% clex:learned_as_type('action',X,'', 'actiona.eating'). FALSE
learned_as_type(Act,XD):-  quietly(some_name1(Act,XD)),!.
some_name1(_, XD):- compound(XD),!,fail.
some_name1(Act,XD):- var(XD),!,en_gen(any,N),atom_concat(Act,N,XD).
some_name1(Act,XD):- some_sep1(Sep),alc_s([L,R],Sep,XD),!,some_name1_l_r(Sep,Act,L,R).
some_name1(Act,XD):- alc_s([_,X|_],Act,XD), X\=='',some_sep1(Sep),x_good(Sep,X),!.

some_name1_l_r(Sep,Act,L,_):-  alc_s([_,B],Act,L),!,(B=='';x_good(Sep,B)),!.
some_name1_l_r(Sep,Act,_,L):-  alc_s([_,B],Act,L),!,(B=='';x_good(Sep,B)),!.

some2_a_v(Act,XD,Est,YD):- some_sep1(Sep),alc_s([L,R],Sep,XD),!,some2_a_v_l_r(Sep,Act,Est,YD,L,R).
some2_a_v(Act,XD,Est,YD):- alc_s([A,B],Act,XD), x_good(B), alc_jy(A,Act,Est,B,YD).
some2_a_v_l_r(Sep,Act,Est,YD,L,R):- alc_s([A,B],Act,L),x_good(B),!,
  alc_jy(A,Act,Est,B,YL), alc_j([YL,Sep,R,Est],YD).
some2_a_v_l_r(Sep,Act,Est,YD,L,R):- alc_s([A,B],Act,R),x_good(B),!,
  alc_jy(A,Act,Est,B,YR), alc_j([L,Est,Sep,YR],YD).

some2_v_a(Act,XD,Est,YD):- alc_s([A,EstB],Act,YD),alc_s([B1,B2|BN],Est,EstB),!,
  alc_j([A,Act,B1,B2|BN],XD).
some2_v_a(Act,XD,Est,YD):- some_sep1(Sep),alc_s([L,R],Sep,YD),!,some2_v_a_l_r(Sep,Act,Est,XD,L,R).

some2_v_a_l_r(Sep,Act,Est,XD,L,R):- alc_s([A,B],Act,L),
  alc_s([AA,BB],Est,B),
  some_chops(R,Est,RR),
  alc_j([A,Act,AA,BB,Sep,RR],XD).
some2_v_a_l_r(Sep,Act,Est,XD,L,R):- alc_s([A,B],Act,R),
  alc_s([AA,BB],Est,B),
  some_chops(L,Est,LL),
  alc_j([LL,Sep,A,Act,AA,BB],XD).
  
some_chops(L,Est,LL):- atom_concat(LL,Est,L),!.
some_chops(L,_,L).


some_names_two(_Act,XD,_Est,YD):- (compound(XD);compound(YD)),!,fail.
some_names_two(Act,XD,'',YD):-!,XD=YD,learned_as_type(Act,XD).
some_names_two(Act,XD,Est,YD):- atom(XD),var(YD),!,some2_a_v(Act,XD,Est,YD).
some_names_two(Act,XD,Est,YD):- var(XD),atom(YD),!,some2_v_a(Act,XD,Est,YD).
some_names_two(Act,XD,Est,YD):- atom(XD),atom(YD),!,
  alc_s([A,EstB],Act,YD),x_good(EstB),alc_s([B1,B2|BN],Est,EstB),!,
  alc_j([A,Act,B1,B2|BN],XD).
some_names_two(Act,XD,Est,YD):- some_names2(Act,XD,Est,YD).
/*
some_name1(Act,XD,Est,YD):- atom(XD),atom(YD),!,some_names2(Act,X,Est,Y),alc_s([A,B|Rest],X,XD),alc_s([A,B|Rest],Y,YD),!.
some_name1(Act,XD,Est,YD):- atom(YD),!,once((some_names2(Act,X,Est,Y),alc_s([A,B|Rest],Y,YD),alc_s([A,B|Rest],X,XD))).
some_name1(Act,XD,Est,YD):- atom(XD),!,once((some_names2(Act,X,Est,Y),alc_s([A,B|Rest],X,XD),alc_s([A,B|Rest],Y,YD))).
some_name1(Act,XD,Est,YD):- some_names2(Act,XD,Est,YD).
*/
some_names2(Act,X,Est,Y):- atom_concat(Act,Est,ActEst), en_gen(any,N),some_names4(N,Act,ActEst,X,Est,Y).

some_names4(N,Act,ActEst,X,Est,Y):- atom_concat(Act,N,X),
  (atom_concat(X,Est,Y);(N\=='',atom_concat(ActEst,N,Y));(nonvar(Y),Est\=='',atom_concat(Est,N,Y))).

%some_sep1('$').
some_sep1('000').
%some_sep1('-').
%some_sep1('.').
%some_sep1('_').
%en_gen(any,'*').
en_gen(any,Sep):-some_sep1(Sep).
en_gen(any,N):- between(1,4,N).

adj_itr(Y, X):- learned_as_type('adjective',X,'',Y).
adj_itr(X, Y):- learned_as_type('attrib',X,'',Y).
adj_itr(X, Y):- learned_as_type('type',X,'ish',Y).

adj_itr_comp(Y, X):- learned_as_type('attrib',X,'er',Y).
adj_itr_comp(Y, X):- learned_as_type('type',X,'isher',Y).

adj_itr_sup(Y, X):- learned_as_type('attrib',X,'est',Y).
adj_itr_sup(Y, X):- learned_as_type('type',X,'ishest',Y).

adv(Y, X):- learned_as_type('attrib',X,'',Y).
adv(Y, X):- learned_as_type('adverb',X,'',Y).
adv(Y, X):- learned_as_type('attrib',X,'ly',Y).
adv(Y, X):- learned_as_type('type',X,'ishly',Y).
adv(Y, X):- learned_as_type('type',X,'ly',Y).

adv_comp(Y, X):- learned_as_type('attrib',X,'er',Y).
adv_comp(Y, X):- learned_as_type('attrib',X,'lier',Y).
adv_comp(Y, X):- learned_as_type('type',X,'lier',Y).

adv_sup(Y, X):- learned_as_type('attrib',X,'est',Y).
adv_sup(Y, X):- learned_as_type('attrib',X,'liest',Y).
adv_sup(Y, X):- learned_as_type('type',X,'liest',Y).

dv_pp(Y, X, ''):- learned_as_type('action',X,'ed',Y).
dv_pp(Y, X, 'as'):- learned_as_type('action',X,'ed',Y).
dv_pp(Y, X, 'to'):- learned_as_type('action',X,'ed',Y).
%dv_pp(Y, X, 'of'):- learned_as_type('attrib',X,'s',Y).
dv_finsg(Y, X, ''):- learned_as_type('action',X,'s',Y).
dv_infpl(Y, X, ''):- learned_as_type('action',X,'',Y).

iv_finsg(Y, X):- learned_as_type('action',X,'s',Y).
iv_finsg(Y, X):- learned_as_type('action',X,'ing',Y).
iv_infpl(Y, X):- learned_as_type('action',X,'',Y).
iv_infpl(Y, X):- learned_as_type('action',X,'ing',Y).

tv_finsg(Y, X):- learned_as_type('action',X,'s',Y).
tv_finsg(Y, X):- learned_as_type('attrib',X,'s',Y).
tv_pp(Y, X):- learned_as_type('attrib',X,'ed',Y).
tv_pp(Y, X):- learned_as_type('action',X,'ed',Y).
tv_pp(Y, X):- learned_as_type('action',X,'',Y).


noun_mass(Y, X, human):- learned_as_type('agent',X,'',Y).
noun_mass(Y, X, neutr):- learned_as_type('object',X,'',Y).
noun_mass(Y, X, neutr):- learned_as_type('type',X,'',Y).
noun_mass(Y, X, neutr):- learned_as_type('attrib',X,'',Y).

noun_pl(Y, X, Human):- some_of_type(Human,Agent),learned_as_type(Agent,X,'s',Y).
noun_sg(Y, X, Human):- some_of_type(Human,Agent),learned_as_type(Agent,X,'',Y).

some_of_type(Human,Agent):- some_of_type1(Human,Agent).
some_of_type(human,'actioner').
some_of_type(human,'group').
some_of_type(neutr,'action').
some_of_type(human,'type').
some_of_type(neutr,'type').
some_of_type(neutr,'group').
some_of_type(neutr,'actioner'). % non base-form?
some_of_type1(human,'agent').
some_of_type1(neutr,'object').

pn_sg(Y, X, Type):-  some_of_type1(Type,Agent),learned_as_type(Agent,X,'',Y).
pn_defsg(Y, X, Type):-  some_of_type1(Type,Agent),learned_as_type(Agent,X,'',Y).
pn_pl(Y, X, Type):-  some_of_type1(Type,Agent),learned_as_type(Agent,X,'s',Y).
pn_defpl(Y, X, Type):-  some_of_type1(Type,Agent),learned_as_type(Agent,X,'s',Y).



:- '$set_source_module'(clex_ape).
adj_itr(Adj_itr,Itr):-clex:adj_itr(Adj_itr,Itr).
adj_itr_comp(Adj_itr_comp,Comp):-clex:adj_itr_comp(Adj_itr_comp,Comp).
adj_itr_sup(Adj_itr_sup,Sup):-clex:adj_itr_sup(Adj_itr_sup,Sup).
adj_tr(Adj_tr1,Adj_tr,Tr):-clex:adj_tr(Adj_tr1,Adj_tr,Tr).
adj_tr_comp(Adj_tr_comp1,Adj_tr_comp,Comp):-clex:adj_tr_comp(Adj_tr_comp1,Adj_tr_comp,Comp).
adj_tr_sup(Adj_tr_sup1,Adj_tr_sup,Sup):-clex:adj_tr_sup(Adj_tr_sup1,Adj_tr_sup,Sup).
adv(Adv,Adv1):-clex:adv(Adv,Adv1).
adv_comp(Adv_comp,Comp):-clex:adv_comp(Adv_comp,Comp).
adv_sup(Adv_sup,Sup):-clex:adv_sup(Adv_sup,Sup).
dv_finsg(Dv_finsg1,Dv_finsg,Finsg):-clex:dv_finsg(Dv_finsg1,Dv_finsg,Finsg).
dv_infpl(Dv_infpl1,Dv_infpl,Infpl):-clex:dv_infpl(Dv_infpl1,Dv_infpl,Infpl).
dv_pp(Dv_pp1,Dv_pp,Pp):-clex:dv_pp(Dv_pp1,Dv_pp,Pp).
iv_finsg(Iv_finsg,Finsg):-clex:iv_finsg(Iv_finsg,Finsg).
iv_infpl(Iv_infpl,Infpl):-clex:iv_infpl(Iv_infpl,Infpl).
mn_pl(Mn_pl,Pl):-clex:mn_pl(Mn_pl,Pl).
mn_sg(Mn_sg,Sg):-clex:mn_sg(Mn_sg,Sg).
noun_mass(Noun_mass1,Noun_mass,Mass):-clex:noun_mass(Noun_mass1,Noun_mass,Mass).
noun_pl(Noun_pl1,Noun_pl,Pl):-clex:noun_pl(Noun_pl1,Noun_pl,Pl).
noun_sg(Noun_sg1,Noun_sg,Sg):-clex:noun_sg(Noun_sg1,Noun_sg,Sg).
pn_pl(Pn_pl1,Pn_pl,Pl):-clex:pn_pl(Pn_pl1,Pn_pl,Pl).
pn_sg(Pn_sg1,Pn_sg,Sg):-clex:pn_sg(Pn_sg1,Pn_sg,Sg).
pndef_pl(Pndef_pl1,Pndef_pl,Pl):-clex:pndef_pl(Pndef_pl1,Pndef_pl,Pl).
pndef_sg(Pndef_sg1,Pndef_sg,Sg):-clex:pndef_sg(Pndef_sg1,Pndef_sg,Sg).
prep(Prep,Prep1):-clex:prep(Prep,Prep1).
tv_finsg(Tv_finsg,Finsg):-clex:tv_finsg(Tv_finsg,Finsg).
tv_infpl(Tv_infpl,Infpl):-clex:tv_infpl(Tv_infpl,Infpl).
tv_pp(Tv_pp,Pp):-clex:tv_pp(Tv_pp,Pp).

