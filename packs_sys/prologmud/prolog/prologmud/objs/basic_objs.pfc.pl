/* * module * 
% This file contains the definitions for the objects in a household
% To create a new world, simply change the object definitions as
% described below (or in manual)
%
% *******Object definitions*******
%.
% Use the label_type_props(label,colcode,[property1,property2,etc.]]).
% label is whats used to refer to the object in the running world
% colcode is whats used to refer to the object for initialization (see world.pl)
%
%
*/


((vtActionTemplate(AT)/get_functor(AT,F)==>vtVerb(F))).
%(vtVerb(F)/name_text_now(F,Txt)==>mudKeyword(F,Txt)).
%(tCol(F)/name_text_now(F,Txt)==>mudKeyword(F,Txt)).




mudLabelTypeProps("tr",tTree,[mudHeight(3),mudWeight(4)]).
mudLabelTypeProps("rk",tRock,[mudHeight(2),mudWeight(4)]).
mudLabelTypeProps("pt",tPit,[mudHeight(-1),mudWeight(4)]).
 mudLabelTypeProps("ot",tPowerOutlet,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenStays),mudActAffect(actTake,mudEnergy(+50))]).
 mudLabelTypeProps("nt",tNut,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+40)),spawn_rate(10)]).
mudLabelTypeProps("lw",tLowWall,[mudHeight(2),mudWeight(4)]).
:- do_gc.

mudLabelTypeProps("lg",tLedge,[mudHeight(2),mudWeight(4)]).
mudLabelTypeProps("lb",tLowBox,[mudHeight(2),mudWeight(2)]).
:- do_gc.

mudLabelTypeProps("hw",tHighWall,[mudHeight(3),mudWeight(4)]).
mudLabelTypeProps("hb",tHighBox,[mudHeight(3),mudWeight(3)]).
mudLabelTypeProps("gd",tGold,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenDeletes),mudActAffect(actTake,mudScore(+10))]).
:- do_gc.

mudLabelTypeProps("fd",tFood,[mudHeight(0),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+80))]).
mudLabelTypeProps("el",tElixer,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudHealth(+80))]).
mudLabelTypeProps("ed",tEdgeOfWorld,[mudHeight(10),mudWeight(4)]).
:-garbage_collect_atoms.
 mudLabelTypeProps("dt",tDirt,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenDeletes),mudActAffect(actTake,mudScore(+1)),spawn_rate(5)]).
:-garbage_collect_atoms.
mudLabelTypeProps("dr",tDoor,[mudHeight(3),mudWeight(2)]).
:-garbage_collect_atoms.

% What an agent turns into upon death.
% Must be named iCorpseFn (or edit agent_to_corpse/1 in lib.pl.
 mudLabelTypeProps("cp",tCorpsea,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+80)),mudActAffect(actTake,mudScore(+10))]).
 mudLabelTypeProps("cp",tCorpseb,[mudHeight(1),mudWeight(1),mudPermanence(actTake,vTakenMoves),mudActAffect(actEat,mudEnergy(+120))]).
mudLabelTypeProps("da",tCorpsec,[mudHeight(2),mudWeight(2)]).
% This is used to make the monster roaming the maze a bit tougher
% It doesn't require any of the usual traits since the monster will never 
% leave the monster's possession.
mudLabelTypeProps("nk",tNastyKnife,[mudActAffect(actWield,mudAttack(2))]).
mudLabelTypeProps("th",tToughHide,[mudActAffect(actWear,mudArmor(2))]).



% Define the maximum charge and maximum damage an agent can have


% :- include(prologmud(mud_footer)).
