/*************************************************************************

         name: lexicon_general_english.pl 
      version: 
  description: Common lexicon for godis-aod
       author: Staffan Larsson, David Hjelm
 
*************************************************************************/


%greet moves

input_form( greet,['hej']).
output_form( greet,['Hej']).

%quit moves

input_form( quit, [hej,'då'] ).
input_form( quit, [sluta] ).
output_form( quit, ['adjö'] ).

%usr request templates
%e.g. "byt kanal"
input_form( request(Action), Str ):-
	action_form(Action,imperative,Str).

input_form( request(Action), Str ):-
	usr_request_form(Str1),
	action_form(Action,infinitive,Str2),
	append(Str1,Str2,Str).

usr_request_form([jag,vill]).
usr_request_form([jag,skulle,vilja]).

%general system ask moves and templates

output_form( ask(X^(action(X))), ['Vad kan jag hjälpa dig med?'] ).

output_form( ask(action(Action)), Str ):-
	action_form(Action,infinitive, ActionStr ),
	append( ['Vill du'], ActionStr, Str0 ),
	append( Str0, ['?'], Str).

%
output_form( ask(set(Alts)), Output):-
	
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).