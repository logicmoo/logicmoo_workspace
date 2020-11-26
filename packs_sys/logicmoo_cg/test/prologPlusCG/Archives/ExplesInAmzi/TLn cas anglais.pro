
%//-------------------------------------------------------------------------------------------------------------//
%//-------------------------------             programme Prologe          --------------------------------------//
%//----------------------------------                Cas Anglais        ---------------------------------------//
%//------------------------------------------------------------------------------------------------------------//


%----------------------------------------- ditionnaire & catégorie lexical ------------------------------------
nom_commun([customer|L],L).
nom_commun([employee|L],L).
nom_commun([year|L],L).
nom_commun([card|L],L).
nom_commun([train|L],L).
nom_commun([monday|L],L).
nom_commun([number|L],L).
nom_commun([station|L],L).
nom_commun([key|L],L).
nom_commun([machine|L],L).
nom_commun([button|L],L).
nom_commun([bank|L],L).
nom_commun([slot|L],L).
nom_commun([end|L],L).
nom_commun([bill|L],L).

nompropre([john|Suite], Suite). 
nompropre([simplemat|Suite], Suite). 

and_or([and|Suite],Suite).
and_or([or|Suite],Suite).

adverbe([correctly|Suite], Suite). 
adverbe([manually|Suite], Suite). 
adverbe([ponctually|Suite], Suite). 


be([is|Suite], Suite). 

not([not|Suite], Suite). 
not(L,L).

prep([to|Suite], Suite). 
prep([into|Suite], Suite). 
prep([of|Suite], Suite). 


more_less([more|Suite], Suite).
more_less([less|Suite], Suite).

than([than|Suite], Suite).

the([the|Suite], Suite).
the([a|Suite], Suite).

adj([invalid| L],L).
adj([valid| L],L).
adj([blue| L],L).
adj([fast| L],L).

qualificat([no|L], L).
qualificat([every|L], L).
qualificat([each|L], L).
qualificat([there-is_a|L], L).
qualificat([there_is_an|L], L).
qualificat([there_is_no|L], L).


verbe_intransf([arrive|Suite], Suite).

verbe_transf([enters|Suite], Suite).
verbe_transf([insetrs|Suite], Suite).
verbe_transf([rejects|Suite], Suite).
verbe_transf([has|Suite], Suite).
verbe_transf([retains|Suite], Suite).
verbe_transf([presses|Suite], Suite).
verbe_transf([enters|Suite], Suite).

verbe_2cpl([sends|L], L).


relative_prenom([that|L],L).
relative_prenom([wich|Suite], Suite) .
relative_prenom([who|L],L).

which([which|L],L).

most_least([most|L],L).
most_least([least|L],L).

if([if|L], L).
then([then|L], L).
for_every([for_every|L],L).

for_every1([for_every|L],L).
for_every1(L,L).

prenon_p_sujet([she|Suite],Suite).
prenon_p_sujet([he|Suite],Suite).
prenon_p_sujet([it|Suite],Suite).

%------------------------------------- grammaire ------------------------------------------------------
phrase(L, Suite):- coordphrases(L, Suite).
phrase(L, Suite):- phrasecond(L, Suite).

phrase(L,Suite):- for_every_ph(L, Suite),!.

phrasecond(L, Suite):- if(L,L1), for_every1(L1, L2), coordphrases(L2, L3), suite_cond(L3, Suite).
suite_cond(L, L).
suite_cond(L, Suite):- then(L,L1), coordphrases(L1, Suite).

coordphrases(L, Suite):- phrase_simple(L, L1), suite_ph_simple(L1, Suite).
suite_ph_simple(L, L).
suite_ph_simple(L, Suite):- and_or(L, L1), phrase_simple(L1, Suite).

for_every_ph(L, Suite):- for_every(L, L1),phrase(L1, Suite).

%---------------------------------------------------------------------------------------
phrase_simple(L, Suite):- sujet(L, L1), predicats(L1, Suite).
%---------------------------------------------------------------------------------------
sujet(L, Suite):- prenon_p_sujet(L, Suite).
sujet(L, suite):- gn(L, Suite).
%--------------------------------------------------------------------------------------

predicats(L,Suite):- predicat(L, Suite).
predicats(L,Suite):- predicat(L, L1), suite_predicats(L1,suite).

suite_predicats(L, Suite):- and_or(L, L1), predicat(L1, L2),
                            suite_predicat(L2, Suite),!.
suite_predicats(L, L).

predicat(L, Suite):- verbe_c(L, Suite).
predicat(L, Suite):- adverbe(L, L1), verbe_c(L1, L2), adjuncts(L2, Suite).
predicat(L, Suite):- be(L,L1),not(L1,L2),comparative(L2,L3),gadj_gnc_gp(L3,suite).

adverbes(L, Suite):- adverbe(L, L1), s_adverbe(L1, suite).
adverbes(L, L).
s_adverbe(L, L).
s_adverbe(L, Suite):- and_or(L, L1), adverbe(L1,L2), s_adverbe(L2, Suite),!.

comparative(L,Suite):-more_less(L,L2),adj(L2,L3), than(L3,Suite).
comparative(L,L).


verbe_c(L, Suite):- verbe_intransf(L, Suite).
verbe_c(L, Suite):- verbe_transf(L, L1), complement(L1, Suite).
verbe_c(L, Suite):- verbe_2cpl(L, L1), complement(L1, Suite).
verbe_c(L, Suite):- verbe_2cpl(L, L1), complement(L1, L2), 
                   complement(L2, suite).

the_nom(L,Suite) :- the(L,L1), nom_commun(L1,Suite).
complement(L,Suite) :- the_nom(L,Suite).
complement(L,Suite) :- the_nom(L,L1),nompropre(L1,Suite).
complement(L,Suite) :- the_nom(L,L1),relative_prenom(L1,L2), predicat(L2, Suite).
complement(L,Suite) :- the_nom(L,L1),of(L1,L2),the_nom(L1, Suite).



gadj_gnc_gp(L,suite):-g_adjs(L,suite).
gadj_gnc_gp(L,suite):- g_nom_commun(L,Suite).
gadj_gnc_gp(L,suite):-g_preps(L,Suite).



adjuncts(L, Suite):- adverbe(L, Suite).
adjuncts(L, Suite):- adverbe(L, L1), g_props(L1, Suite).
adjuncts(L, Suite):-g_preps(L, Suite).



g_props(L, Suite):-gprep(L, L1), suite_gprops(L1, Suite).
g_props(L, L).

suite_gprops(L, Suite):-gprep(L, L1), and_or(L1, L2), suite_gprep(L2, Suite),!.
suite_gprops(L, L).

gprep(L, Suite):- prep(L, L1), gn(L1, Suite).
gprep(L, L).

gns(L,Suite):-gn(L,L1),suite_gns(L1,Suite).

suite_gns(L,Suite):-and_or(L,L1),gn(L1,L2),suite_gns(L2,Suite),!.
suite_gns(L,L).

gn(L,Suite):-nompropre(L,Suite).
gn(L,Suite):-the(L,L1),nom_commun(L1,Suite).
gn(L,Suite):-g_nom_commun(L,Suite).

g_nom_commun(L,Suite):-	the(L,L1), superlative(L1, Suite).
g_nom_commun(L,Suite):- qualification(L, L1), possessive_noun(L2, L3), g_adjs(L3, L4),
                        nom_commun(L4, L5), apposition(L5, L6), of_gprep(L6, Suite),!.

possessive_noun([their|L], L).

qualification(L, Suite):- the(L, l1), superlative(L1, Suite).
qualification(L, Suite):-qualificat(L, Suite).
qualification(L, Suite):- det(L, Suite).




apposition(L, Suite):- gn(L, Suite).
apposition(L, Suite):- nom_propre(L, Suite).
apposition(L, Suite):- nom_dynamique(L, Suite).
apposition(L, L).
superlative(L, Suite):- most_least(L, L1), adj(L1, suite).

g_relatives(L, Suite):- g_relative(L, L1), suite_relative(L1, Suite).
suite_relative(L, Suite):- ond_or(L, L1), f_relalive(L1, L2), suite_relative(L2, Suite),!.
suite_relative(L, L).  
g_relative(L, Suite):- relative_prenom(L, L1), predicat(L1, Suite).
g_relative(L, Suite):- which(L, L1), phrase_simple(L1, Suite).



of_gprep(L, Suite):- of(L, L1), gn(L1, Suite),!.
of([of|L],L).

g_adjs(L,suite):- gadjs(L, L1), suite_gadj(L1, Suite).
g_adjs(L,L).
suite_gadj(L, Suite):- and_or(L, L1), gadj(L1, L2), suite_gadj(L2, Suite),!.
suite_gadj(L, L).

gadj(L, Suite):- adj(L, L1), gprep(L1, Suite).











