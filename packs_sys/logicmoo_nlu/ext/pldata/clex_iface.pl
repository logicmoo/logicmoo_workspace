:-module(clex_iface, [

   clex_call/1,clex_call/2,
   clex_call/3,clex_call/4,
   clex_call/5,clex_call/6,


         clex_verb/4,
/*           tv_pp/2,
           tv_infpl/2,
           tv_finsg/2,
           iv_infpl/2,
           iv_finsg/2,
           dv_pp/3,
           dv_infpl/3,
           dv_finsg/3,
*/
           % for now we use prep/2 from different (possibly more complete) lexicon
 %           prep/2,

         clex_adv/3,
  %          adv_sup/2,
   %         adv_comp/2,
    %        adv/2,
                               
         clex_adj_prep/4,
     %       adj_tr_sup/3,
      %      adj_tr_comp/3,
       %     adj_tr/3,

         clex_adj/3,
        %    adj_itr_sup/2,
         %   adj_itr_comp/2,
          %  adj_itr/2,

         clex_noun/5,

%             noun_sg/3,
 %            noun_pl/3,
  %           noun_mass/3,

         clex_mass_noun/3  ,
         % for now we use these from different (possibly more complete) lexicon
   /*
           mn_sg/2,
           mn_pl/2,

           pndef_sg/3,
           pndef_pl/3,
           pn_sg/3,
           pn_pl/3      */
   is_clex_pred/3
          ]).


when_chat80(_):- fail.

%when_chat80(G):- call(chat80:G).

%! clex_verb(?Atom, ?Verb, ?ITD, ?Info)
% 
% ?- clex_verb(jumped, Verb, ITD, Info).
% Verb = jump,
% ITD = tv,
% Info = pp .
%
% ?- clex_verb(jump, Verb, ITD, Info).
% Verb = jump,
% ITD = iv,
% Info = infpl ;
%
% Verb = jump,
% ITD = tv,
% Info = infpl ;

:- style_check(-(discontiguous)).

:- dynamic(is_clex_pred/3).

%clex_pred(F/A):-
clex_pred(M:F/A):- 
  dynamic(M:F/A),multifile(M:F/A),export(M:F/A),
  assert_if_new(clex_iface:is_clex_pred(M,F,A)).
clex_pred(F/A):- clex_pred(clex:F/A).

:- export(clex_call/1).
:- export(clex_call/2).
:- export(clex_call/3).
:- export(clex_call/4).
:- export(clex_call/5).
:- export(clex_call/6).
:- export(clex_call/7).
clex_call([F|Rest]):- !, is_clex_pred(M,F,N),/*var(Rest)->*/length(Rest,N),M:apply(F,Rest).
clex_call(P):- is_clex_pred(M,F,A),functor(P,F,A),M:call(P).
clex_call(F,A):- is_clex_pred(M,F,1),M:call(F,A).
clex_call(F,A,B):- is_clex_pred(M,F,2),M:call(F,A,B).
clex_call(F,A,B,C):- is_clex_pred(M,F,3),M:call(F,A,B,C).
clex_call(F,A,B,C,D):- is_clex_pred(M,F,4),M:call(F,A,B,C,D).
clex_call(F,A,B,C,D,E):- is_clex_pred(M,F,5),M:call(F,A,B,C,D,E).
clex_call(F,A,B,C,D,E,G):- is_clex_pred(M,F,5),M:call(F,A,B,C,D,E,G).

:- clex_pred(adj_itr/2).
:- clex_pred(adj_itr_comp/2).
:- clex_pred(adj_itr_sup/2).
:- clex_pred(adj_tr/3).
:- clex_pred(adj_tr_comp/3).
:- clex_pred(adj_tr_sup/3).
:- clex_pred(adv/2).
:- clex_pred(adv_comp/2).
:- clex_pred(adv_sup/2).

:- clex_pred(mn_pl/2).
:- clex_pred(mn_sg/2).
:- clex_pred(noun_mass/3).
:- clex_pred(noun_pl/3).
:- clex_pred(noun_sg/3).
:- clex_pred(pn_pl/3).
:- clex_pred(pn_sg/3).
:- clex_pred(pndef_pl/3).
:- clex_pred(pndef_sg/3).
:- clex_pred(prep/2).

:- clex_pred(tv_pp/2).
:- clex_pred(dv_pp/3).

:- clex_pred(iv_infpl/2).
:- clex_pred(tv_infpl/2).
:- clex_pred(dv_infpl/3).

:- clex_pred(iv_finsg/2).
:- clex_pred(tv_finsg/2).
:- clex_pred(dv_finsg/3).


:- clex_pred(clex_iface:clex_verb/4).
:- export(clex_verb/4).
clex_verb(Formed, Verb, iv, finsg):- clex_call(iv_finsg,Formed, Verb).
clex_verb(Formed, Verb, tv, finsg):- clex_call(tv_finsg,Formed, Verb).
clex_verb(Formed, Verb, dv(Prep), finsg):- clex_call(dv_finsg,Formed, Verb, Prep), nop(Prep\=='').
clex_verb(Formed, Verb, iv, pp):- clex_call(iv_pp,Formed, Verb). % iz none .. what about "jump to" ?
clex_verb(Formed, Verb, tv, pp):- clex_call(tv_pp,Formed, Verb).
clex_verb(Formed, Verb, dv(Prep), pp):- clex_call(dv_pp,Formed, Verb, Prep).
clex_verb(Formed, Verb, iv, infpl):- clex_call(iv_infpl,Formed, Verb).
clex_verb(Formed, Verb, tv, infpl):- clex_call(tv_infpl,Formed, Verb).
clex_verb(Formed, Verb, dv(Prep), infpl):- clex_call(dv_infpl,Formed, Verb, Prep).

   both_of(RootNoun, Noun, NounI):- var(RootNoun), nonvar(NounI), !, (Noun=NounI;RootNoun=NounI).
   both_of(_, Noun, Noun).
   both_of(RootNoun, Noun, NounI):- RootNoun\=Noun, Noun=NounI.

/*
pp = past+part
pres+fin
*/


:- clex_pred(clex_iface:clex_mass_noun/3).
clex_mass_noun(NounI, RootNoun, Type):- both_of(RootNoun, Noun, NounI), clex_call(noun_mass,Noun, RootNoun, Type), 
  both_of(RootNoun, Noun, NounI).

:- clex_pred(clex_iface:clex_noun/5).
clex_noun(Noun, RootNoun, Type, _PlOrSg, mass):- clex_mass_noun(Noun, RootNoun, Type).
clex_noun(Noun, RootNoun, Type, SG, count):- clex_noun0(Noun, RootNoun, Type, SG), \+ clex_mass_noun(Noun, _, _).
clex_noun(Noun, RootNoun, unkown, SG, count):- clex_noun1(Noun, RootNoun, SG), \+ clex_noun0(Noun, RootNoun, _, SG), \+ clex_mass_noun(Noun, _, _). 
clex_noun(Noun, RootNoun, Type, SG, pn):- clex_noun2(Noun, RootNoun, Type, SG), \+ clex_noun0(Noun, RootNoun, _, SG),  \+ clex_mass_noun(Noun, _, _).

   clex_noun0(Noun, RootNoun, Type, sg):- clex_call(noun_sg,Noun, RootNoun, Type).
   clex_noun0(Noun, RootNoun, Type, pl):- clex_call(noun_pl,Noun, RootNoun, Type).

   clex_noun0(Noun, RootNoun, mn, sg):- clex_call(mn_sg,Noun, RootNoun).
   clex_noun0(Noun, RootNoun, mn, pl):- clex_call(mn_pl,Noun, RootNoun).

   clex_noun1(Noun, RootNoun, pl):- when_chat80(clex_call(noun_plu_db,Noun, RootNoun)), Noun\==ares.
   clex_noun1(RootNoun, RootNoun, sg):- when_chat80(clex_call(noun_sin_db,RootNoun)).

   clex_noun2(Noun, RootNoun, Type, sg):- clex_call(pn_sg,Noun, RootNoun, Type).
   clex_noun2(Noun, RootNoun, Type, pl):- clex_call(pn_pl,Noun, RootNoun, Type).
   clex_noun2(Noun, RootNoun, Type, sg):- clex_call(pndef_sg,Noun, RootNoun, Type).
   clex_noun2(Noun, RootNoun, Type, pl):- clex_call(pndef_pl,Noun, RootNoun, Type).



:- clex_pred(clex_iface:clex_adj_prep/4).
clex_adj_prep(Biggest, Big, Prep, Type):- 
  ( \+ atom(Prep)-> true;
  (atom_concat('-',Prep,PrepH),
    (nonvar(Biggest)->atom_concat(Biggest,PrepH,BiggestH);true),
    (nonvar(Big)->atom_concat(Big,PrepH,BigH);true))),
  clex_adj_prep0(BiggestH, BigH, Prep, Type),
   atom_concat('-',Prep,PrepH),
   atom_concat(Biggest,PrepH,BiggestH),
   atom_concat(Big,PrepH,BigH).


:- clex_pred(clex_iface:clex_adj_prep0/4).
clex_adj_prep0(Biggest, Big, Prep, Type):- adj_prep0(Biggest, Big, Prep, Type).
clex_adj_prep0(Biggest, Big, Prep, unknown):- clex_call(adj_tr,Biggest, Big, Prep), \+ adj_prep0(Biggest, _, Prep, _).
      adj_prep0(Bigger, Big, Prep, comparitve):- clex_call(adj_tr_comp,Bigger, Big, Prep).
      adj_prep0(Biggest, Big, Prep, superlative):- clex_call(adj_tr_sup,Biggest, Big, Prep).


clex_word(adj_prep(Prep), Formed, Root, Type):- clex_adj_prep(Formed, Root, Prep, Type).
clex_word(verb, Formed, Root, Iv+Finsg):- clex_verb(Formed, Root, Iv, Finsg).
clex_word(noun, Formed, Root, Type+SG-Mass):- clex_noun(Formed, Root, Type, SG, Mass).
clex_word(adj, Formed, Root, Data):- clex_adj(Formed, Root,  Data).
clex_word(adv, Formed, Root, Data):- clex_adv(Formed, Root,  Data).


:- clex_pred(clex_iface:clex_adj/3).
clex_adj(Biggest, Big, Type):- adj_itr0(Biggest, Big, Type).
clex_adj(Biggest, Big, unknown):- clex_call(adj_itr,Biggest, Big), \+ adj_itr0(Biggest, _, _).
         adj_itr0(Bigger, Big, comparitve):- clex_call(adj_itr_comp,Bigger, Big).
         adj_itr0(Biggest, Big, superlative):- clex_call(adj_itr_sup,Biggest, Big).

:- clex_pred(clex_iface:clex_adv/3).
clex_adv(Biggest, Big, Type):- adv_itr0(Biggest, Big, Type).
clex_adv(Biggest, Big, unknown):- clex_call(adv,Biggest, Big), \+ adv_itr0(Biggest, _, _).
         adv_itr0(Bigger, Big, comparitve):- clex_call(adv_comp,Bigger, Big).
         adv_itr0(Biggest, Big, superlative):- clex_call(adv_sup,Biggest, Big).


/*
:- ensure_loaded(nl_iface).
:- if(\+ (exists_file('clex_nldata.qlf'))).
:- format(user_error,'~NLoading clex_nldata.qlf ... ~n',[]).
:- time(load_files(clex_nldata,[qcompile(auto)])).
:- endif.
*/

%:- style_check(-discontiguous).
%:- include(pldata('clex_lexicon_user1.nldata')).

clex_dynload(M,F):- absolute_file_name(F, File, [access(read),file_type(prolog)]),
   open(File, read, In),
   set_stream(In, encoding(iso_latin_1)),
   repeat,
   read(In, P),
   % DMiles: i am putting them in backwards (cuz, the hypens- confuse me if they pop out first in the debugger)
   (P= (:- (G)) -> M:call(G) ; asserta_new(M:P)),
   P==end_of_file, !,
   close(In).

:- if(exists_source(pldata('clex_lexicon_user1.nldata'))).
:- clex_dynload(clex,pldata('clex_lexicon_user1.nldata')).
:- else.
:- if(exists_source(('clex_lexicon_user1.nldata'))).
:- clex_dynload(clex,('clex_lexicon_user1.nldata')).
:- endif.
:- endif.


:- if(exists_source(ape(lexicon/clex_lexicon))).
:- clex_dynload(clex,ape(lexicon/clex_lexicon)).
:- endif.

:- fixup_exports. 

% apply_fixes:- clex_verb(Formed, Verb, dv(Prep), PP)

% :- re export(talk_db).                      


