% :-swi_module(user). 
:- swi_module(modions_conf, []).

end_of_file.

:- source_location(File,_Line),
    file_directory_name(File, RunDir),
    atom_concat(RunDir,'/*.pl',Exp),
     expand_file_name(Exp,X),
     forall(member(E,X),ensure_loaded(E)).

:- use_module('look.pl').   % simmudGetPrecepts(Agent,[list of internal traits],[list of percepts]).
:- use_module('move.pl').   % move(Dir). Dir is one of 8 cardinal aDirectionsFn
:- use_module('sit.pl').    % sit. Don't do anything.
:- use_module('take.pl').   % take(Object). take an item in the same atloc as agent
:- use_module('drop.pl').   % drop(Object). drop an item in agent's possession
:- use_module('climb.pl').  % climb(Dir). climb up on an item in direction Dir
:- use_module('push.pl').   % push(Dir). push an item in direction Dir
:- use_module('eat.pl').    % eat(Object). eat/destroy item in possesion
:- use_module('attack.pl'). % attack(Dir). attack another agent in direction Dir

% bugger:action_verb_useable(actWearUnused,wearsClothing,tWearAble,mudPossess).

