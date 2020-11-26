% :-swi_module(user). 
% :- module(modStats, []).
/* * module * A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% health(Agent,Dam) = damage
% wasSuccess(Agent,_What,Suc) = checks success of last action (actually checks the cmdfailure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% ====================================================
% show the stats system
% ====================================================
action_info(actStats(isOptional(tObj,isSelfAgent)), "Examine MUD stats of something").

tCol(rtStatPred).

==> 
rtStatPred(isEach(
         mudEnergy,
         mudStr,
         mudStm, % stamina
         mudScore,
         mudHealth,
         mudHeight)).


agent_call_command(Agent,actStats(What)):-
  findall(Pred, (rtStatPred(Stat),Pred=..[Stat,What,value]),Stats),
   sort(Stats,StatsS),
   show_kb_preds(Agent,What,StatsS),!.
   %xlisting(What),!.



/*
There are 7 aptitudes in Eclipse Phase:
* * Cognition (COG) is your aptitude for problemsolving,
logical analysis, and understanding. It
also includes memory and recall.

* * Coordination (COO) is your skill at integrating
the actions of different parts of your morph
to produce smooth, successful movements. It
includes manual dexterity, fine motor control,
nimbleness, and balance.
* * Intuition (INT) is your skill at following your
gut instincts and evaluating on the fly. It includes
physical awareness, cleverness, and cunning.
* * Reflexes (REF) is your skill at acting quickly. This
encompasses your reaction time, your gut-level
response, and your ability to think fast.
* * Savvy (SAV) is your mental adaptability, social
intuition, and proficiency for interacting
with others. It includes social awareness and
manipulation.
* * Somatics (SOM) is your skill at pushing your
morph to the best of its physical ability, including
the fundamental utilization of the morph�s strength,
endurance, and sustained positioning and motion.
* * Willpower (WIL) is your skill for self-control,
your ability to command your own destiny.
*/

action_info(actGrep(isOptional(ftTerm,isSelfAgent)), "Examine MUD listing of something").
agent_call_command(_Gent,actGrep(Obj)):- string(Obj),!,xlisting(Obj),atom_string(Atom,Obj),xlisting(Atom).
agent_call_command(_Gent,actGrep(Obj)):- xlisting(Obj).

:- include(prologmud(mud_footer)).
