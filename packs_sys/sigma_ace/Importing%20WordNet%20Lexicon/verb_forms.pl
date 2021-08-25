:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading code to transform verb root forms to present indicatives'),nl.

% root_form_to_present_indicative performs morphological processing
% on the verb, changing it from its root form (e.g., 'enter')
% to its present indicative  form (e.g., 'enters').

% Irregular forms of auxiliaries:

root_form_to_present_indicative(be,is) :- !.
root_form_to_present_indicative(do,does) :- !.
root_form_to_present_indicative(go,goes) :- !.
root_form_to_present_indicative(have,has) :- !.

% Rule: multiple words in base form
% Example: 'go to bed' -> 'goes to bed'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  first_word_is(Verb_Root_Form,First_Word,Rest),!,
  root_form_to_present_indicative(First_Word,First_Word_Indicative),
  atom_concat(First_Word_Indicative,Rest,Verb_Present_Indicative).

first_word_is(Words,First_Word,Rest_Of_Words) :-
	atom_codes(Words,Words_String),
	find_space(Words_String,First_Word_String,Rest_Of_Words_String),!,
	atom_codes(First_Word,First_Word_String),
	atom_codes(Rest_Of_Words,Rest_Of_Words_String).

find_space([32|Rest],[],[32|Rest]) :- !.

find_space([Char|Chars],[Char|First_Word],Rest) :-
	Char \== 32,
	find_space(Chars,First_Word,Rest).

% Rule: s -> es
% Last letter is an 's'
% Change to 'es'
% Example: 'miss' -> 'misses'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 2,
  Length2 is Length - 1,
  sub_string(Verb_Root_Form,Length2,1,0,'s'),!,
  string_concat(Verb_Root_Form,'es',Verb_Present_Indicative).
  
% Rule: x -> xes
% Last letter is an 's'
% Change to 'xes'
% Example: 'box' -> 'boxes'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 2,
  Length2 is Length - 1,
  sub_string(Verb_Root_Form,Length2,1,0,'x'),!,
  string_concat(Verb_Root_Form,'es',Verb_Present_Indicative).
  
% Rule: tch -> tches
% Last letters are 'tch'
% Change to 'tches'
% Example: 'fetch' -> 'fetches'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 3,
  Length2 is Length - 3,
  sub_string(Verb_Root_Form,Length2,3,0,'tch'),!,
  string_concat(Verb_Root_Form,'es',Verb_Present_Indicative).
  
% Rule: sh -> shes
% Last letters are 'sh'
% Change to 'shes'
% Example: 'crash' -> 'crashes'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 2,
  Length2 is Length - 2,
  sub_string(Verb_Root_Form,Length2,2,0,'sh'),!,
  string_concat(Verb_Root_Form,'es',Verb_Present_Indicative).

% Rule: ay -> ays
% Last letters are 'ay'
% Change to 'ays'
% Example: 'bay' -> 'bays'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 2,
  Length2 is Length - 2,
  sub_string(Verb_Root_Form,Length2,2,0,'ay'),!,
  string_concat(Verb_Root_Form,'s',Verb_Present_Indicative).

% Rule: ey -> eys
% Last letters are 'ey'
% Change to 'eys'
% Example: 'obey' -> 'obeys'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 3,
  Length2 is Length - 2,
  sub_string(Verb_Root_Form,Length2,2,0,'ey'),!,
  string_concat(Verb_Root_Form,'s',Verb_Present_Indicative).

% Rule: iy -> iys
% Last letters are 'iy'
% Change 'iy' to 'ies'
% Example: 'reify' -> 'reifies', 'beautify' -> 'beautifies'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 3,
  Length2 is Length - 2,
  sub_string(Verb_Root_Form,Length2,2,0,'iy'),!,
  Length1 is Length - 1,
  sub_string(Verb_Root_Form,0,Length1,0,Root_Less_y),
  string_concat(Root_Less_y,'ies',Verb_Present_Indicative).

% Rule: oy -> oys
% Last letters are 'oy'
% Change to 'oys'
% Example: ?

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 2,
  Length2 is Length - 2,
  sub_string(Verb_Root_Form,Length2,2,0,'oy'),!,
  string_concat(Verb_Root_Form,'s',Verb_Present_Indicative).

% Rule: uy -> uys
% Last letters are 'uy'
% Change to 'uys'
% Example: 'buy' -> 'buys'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 2,
  Length2 is Length - 2,
  sub_string(Verb_Root_Form,Length2,2,0,'uy'),!,
  string_concat(Verb_Root_Form,'s',Verb_Present_Indicative).

% Rule: y -> ys
% Last letters are 'y'
% Change to 'ys'
% Example: 'cry' -> 'cries'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  Length > 2,
  Length2 is Length - 1,
  sub_string(Verb_Root_Form,Length2,1,0,'y'),!,
  sub_string(Verb_Root_Form,0,Length2,1,MinusTheY),
  string_concat(MinusTheY,'ies',Verb_Present_Indicative).

% All else, add 's'

root_form_to_present_indicative(Verb_Root_Form, Verb_Present_Indicative) :-
  string_length(Verb_Root_Form,Length),
  string_concat(Verb_Root_Form,'s',Verb_Present_Indicative).

%POS  Suffix  Ending     
%NOUN  "s"  ""    
%NOUN  "ses"  "s"    
%NOUN  "xes"  "x"    
%NOUN  "zes"  "z"    
%NOUN  "ches"  "ch"    
%NOUN  "shes"  "sh"    
%NOUN  "men"  "man"    
%VERB  "s"  ""    
%VERB  "ies"  "y"    
%VERB  "es"  "e"    
%VERB  "es"  ""    
%VERB  "ed"  "e"    
%VERB  "ed"  ""    
%VERB  "ing"  "e"    
%VERB  "ing"  ""    
%ADJ  "er"  ""    
%ADJ  "est"  ""    
%ADJ  "er"  "e"    
%ADJ  "est"  "e"

% verb_in_lexicon(move involuntarilies,move involuntarily,transitive,singular,event,simple,BodyMotion,200006642).
% verb_in_lexicon(act involuntarilies,act involuntarily,transitive,singular,event,simple,BodyMotion,200006829).
% verb_in_lexicon(go to beds,go to bed,transitive,singular,event,simple,BodyMotion,200012272).

% move involutarily -> moves involutarily

% be active -> is active
% have sex -> has sex
% do good -> does good

% determine_event_or_state(+WordNet_verb,+Synset_ID,-Event_or_State)
% sets Event_or_State to either 'event' or 'state' according to the
% type of verb.

% stative forms of 'be':
% s(201775973,1,'be',v,1,1).
% s(201784339,1,'be',v,2,1).
% s(201811792,1,'be',v,3,1).
% s(201775163,2,'be',v,4,1).
% s(201781222,1,'be',v,5,1).
% s(201817610,2,'be',v,6,1).
% s(201787769,5,'be',v,7,1).
% s(201666138,1,'be',v,8,1).
% s(201840295,2,'be',v,9,1).
% s(201552250,1,'be',v,10,1).
% s(201782836,1,'be',v,11,1).
% s(201843641,2,'be',v,12,0).

% Others:
%  --from hypernyms of 'have': 201508689, 201794357, 201443215, 201509295, 201857688, 201620370, 201509557, 201876679, 200045715, 201858069
%  --from hypernyms of 'stay': 201261882, 201371015, 201868387
%  --from hypernyms of 'own' : 201509295 (also in 'have')

determine_event_or_state(WordNet_verb,Synset_ID,state) :- % stative forms of be
	is_hypernym_in(Synset_ID,[201775973,201784339,201811792,201775163,201781222,201817610,
				  201787769,201666138,201840295,201552250,201782836,201843641]),!.

determine_event_or_state(WordNet_verb,Synset_ID,state) :- % hypernyms of 'have' and 'stay'
	is_hypernym_in(Synset_ID,[201508689, 201794357, 201443215, 201509295, 201857688,
				  201620370, 201509557, 201876679, 200045715, 201858069,
				  201261882, 201371015, 201868387]),!.

determine_event_or_state(WordNet_verb,Synset_ID,event).

% determine additional properties of verb entries required for lexicon,
% such as the kind of verb [simple, compound, phrasal, prepositional]
kind_of_verb(Base_Form_Words,prepositional) :-
	is_list(Base_Form_Words),
	last(Last_Word_In_Verb,Base_Form_Words),
	prepositional_verb_preposition(Last_Word_In_Verb),!.

kind_of_verb(Base_Form_Words,phrasal) :-
	is_list(Base_Form_Words),
	last(Last_Word_In_Verb,Base_Form_Words),
	phrasal_verb_ending(Last_Word_In_Verb),!.

kind_of_verb(Base_Form_Words,compound) :-
	is_list(Base_Form_Words),!.

kind_of_verb(Base_Form,simple).

prepositional_verb_preposition(to).                    % e.g., 'give to'
prepositional_verb_preposition(for).                   % e.g., 'call for'
prepositional_verb_preposition(with).                  % e.g., 'go with'
prepositional_verb_preposition(as).                    % e.g., 'go as'

phrasal_verb_ending(out).                              % e.g., 'hand out', 'throw out'
phrasal_verb_ending(up).                               % e.g., 'throw up', 'dress up'
phrasal_verb_ending(down).                             % e.g., 'dress down'
phrasal_verb_ending(off).                              % e.g., 'take off'
