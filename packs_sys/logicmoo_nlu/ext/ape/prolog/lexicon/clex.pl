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

some_of_name(_Act,XD,_Est,YD):- (compound(XD);compound(YD)),!,fail.
some_of_name(Act,XD,Est,YD):- atom(XD),atom(YD),!,some_of_name(Act,X,Est,Y),atomic_list_concat([A,B|Rest],X,XD),atomic_list_concat([A,B|Rest],Y,YD),!.
some_of_name(Act,XD,Est,YD):- atom(YD),!,once((some_of_name(Act,X,Est,Y),atomic_list_concat([A,B|Rest],Y,YD),atomic_list_concat([A,B|Rest],X,XD))).
some_of_name(Act,XD,Est,YD):- atom(XD),!,once((some_of_name(Act,X,Est,Y),atomic_list_concat([A,B|Rest],X,XD),atomic_list_concat([A,B|Rest],Y,YD))).
some_of_name(Act,X,Est,Y):- atom_concat(Act,Est,ActEst),en_gen(N),atom_concat(Act,N,X),
  (atom_concat(X,Est,Y);atom_concat(ActEst,N,Y);(nonvar(Y),Est\=='',atom_concat(Est,N,Y))).

en_gen('*').
en_gen(N):- between(1,8,N).
en_gen('').

adj_itr(X, Y):- some_of_name('attrib',X,'',Y).
adj_itr(X, Y):- some_of_name('type',X,'ish',Y).

adj_itr_comp(Y, X):- some_of_name('attrib',X,'er',Y).
adj_itr_comp(Y, X):- some_of_name('type',X,'isher',Y).

adj_itr_sup(Y, X):- some_of_name('attrib',X,'est',Y).
adj_itr_sup(Y, X):- some_of_name('type',X,'ishest',Y).

adv(Y, X):- some_of_name('attrib',X,'',Y).
adv(Y, X):- some_of_name('attrib',X,'ly',Y).
adv(Y, X):- some_of_name('type',X,'ishly',Y).
adv(Y, X):- some_of_name('type',X,'ly',Y).

adv_comp(Y, X):- some_of_name('attrib',X,'er',Y).
adv_comp(Y, X):- some_of_name('attrib',X,'lier',Y).
adv_comp(Y, X):- some_of_name('type',X,'lier',Y).

adv_sup(Y, X):- some_of_name('attrib',X,'est',Y).
adv_sup(Y, X):- some_of_name('attrib',X,'liest',Y).
adv_sup(Y, X):- some_of_name('type',X,'liest',Y).

dv_pp(Y, X, ''):- some_of_name('action',X,'ed',Y).
dv_pp(Y, X, 'as'):- some_of_name('action',X,'ed',Y).
dv_pp(Y, X, 'to'):- some_of_name('action',X,'ed',Y).


dv_finsg(Y, X, ''):- some_of_name('action',X,'s',Y).
dv_infpl(Y, X, ''):- some_of_name('action',X,'',Y).
iv_finsg(Y, X):- some_of_name('action',X,'s',Y).
iv_infpl(Y, X):- some_of_name('action',X,'',Y).
tv_finsg(Y, X):- some_of_name('action',X,'s',Y).
tv_pp(Y, X):- some_of_name('action',X,'',Y).
tv_pp(Y, X):- some_of_name('action',X,'ed',Y).

noun_mass(Y, X, human):- some_of_name('agent',X,'',Y).
noun_mass(Y, X, neutr):- some_of_name('object',X,'',Y).
noun_mass(Y, X, neutr):- some_of_name('type',X,'',Y).
noun_mass(Y, X, neutr):- some_of_name('attrib',X,'',Y).

noun_pl(Y, X, Human):- some_of_type(Human,Agent),some_of_name(Agent,X,'s',Y).
noun_sg(Y, X, Human):- some_of_type(Human,Agent),some_of_name(Agent,X,'',Y).

some_of_type(Human,Agent):- some_of_type1(Human,Agent).
some_of_type(human,'actioner').
some_of_type(human,'group').
some_of_type(neutr,'action').
some_of_type(human,'type').
some_of_type(neutr,'type').
some_of_type1(human,'agent').
some_of_type1(human,'actor').
some_of_type1(neutr,'object').

pn_sg(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'',Y).
pn_defsg(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'',Y).
pn_pl(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'s',Y).
pn_defpl(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'s',Y).





:- '$set_source_module'(clex_ape).

some_of_name(Act,X,Est,Y):- clex:some_of_name(Act,X,Est,Y).

adj_itr(X, Y):- some_of_name('attrib',X,'',Y).
adj_itr(X, Y):- some_of_name('type',X,'ish',Y).

adj_itr_comp(Y, X):- some_of_name('attrib',X,'er',Y).
adj_itr_comp(Y, X):- some_of_name('type',X,'isher',Y).

adj_itr_sup(Y, X):- some_of_name('attrib',X,'est',Y).
adj_itr_sup(Y, X):- some_of_name('type',X,'ishest',Y).

adv(Y, X):- some_of_name('attrib',X,'',Y).
adv(Y, X):- some_of_name('attrib',X,'ly',Y).
adv(Y, X):- some_of_name('type',X,'ishly',Y).
adv(Y, X):- some_of_name('type',X,'ly',Y).

adv_comp(Y, X):- some_of_name('attrib',X,'er',Y).
adv_comp(Y, X):- some_of_name('attrib',X,'lier',Y).
adv_comp(Y, X):- some_of_name('type',X,'lier',Y).

adv_sup(Y, X):- some_of_name('attrib',X,'est',Y).
adv_sup(Y, X):- some_of_name('attrib',X,'liest',Y).
adv_sup(Y, X):- some_of_name('type',X,'liest',Y).

dv_pp(Y, X, ''):- some_of_name('action',X,'ed',Y).
dv_pp(Y, X, 'as'):- some_of_name('action',X,'ed',Y).
dv_pp(Y, X, 'to'):- some_of_name('action',X,'ed',Y).


dv_finsg(Y, X, ''):- some_of_name('action',X,'s',Y).
dv_infpl(Y, X, ''):- some_of_name('action',X,'',Y).
iv_finsg(Y, X):- some_of_name('action',X,'s',Y).
iv_infpl(Y, X):- some_of_name('action',X,'',Y).
tv_finsg(Y, X):- some_of_name('action',X,'s',Y).
tv_pp(Y, X):- some_of_name('action',X,'',Y).
tv_pp(Y, X):- some_of_name('action',X,'ed',Y).

noun_mass(Y, X, human):- some_of_name('agent',X,'',Y).
noun_mass(Y, X, neutr):- some_of_name('object',X,'',Y).
noun_mass(Y, X, neutr):- some_of_name('type',X,'',Y).
noun_mass(Y, X, neutr):- some_of_name('attrib',X,'',Y).

noun_pl(Y, X, Human):- some_of_type(Human,Agent),some_of_name(Agent,X,'s',Y).
noun_sg(Y, X, Human):- some_of_type(Human,Agent),some_of_name(Agent,X,'',Y).

some_of_type(Human,Agent):- some_of_type1(Human,Agent).
some_of_type(human,'actioner').
some_of_type(human,'group').
some_of_type(neutr,'action').
some_of_type(human,'type').
some_of_type(neutr,'type').
some_of_type1(human,'agent').
some_of_type1(human,'actor').
some_of_type1(neutr,'object').

pn_sg(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'',Y).
pn_defsg(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'',Y).
pn_pl(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'s',Y).
pn_defpl(Y, X, Type):-  some_of_type1(Type,Agent),some_of_name(Agent,X,'s',Y).







