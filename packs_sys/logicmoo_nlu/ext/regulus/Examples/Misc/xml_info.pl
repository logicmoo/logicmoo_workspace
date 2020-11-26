:- module(xml_info,
          [beautify_prolog_xml/2,
	   test_access_functions/0]
         ).

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).



:- use_module(library(lists)).
:- use_module(library(xml)).
:- use_module('$REGULUS/PrologLib/utilities'). 

%Predicates to make the search result XML look ok


beautify_prolog_xml(XML1, XML4):-
	change_pcdata_into_atoms(XML1, XML2),
	change_strings_into_atoms(XML2, XML3),
	beautify_prolog_xml1(XML3, XML4).

beautify_prolog_xml1([F|Rest], [F1|Rest1]) :-
	beautify_prolog_xml1(F, F1),
	beautify_prolog_xml1(Rest,Rest1).

beautify_prolog_xml1(Stuff, Stuff).

%============================================================	
	
change_pcdata_into_atoms(pcdata(String), Output) :-
	atom_codes(Atom, String),
	(   atom_to_int(Atom, Number) ->
	    Output = Number ;
	    Output = Atom
	),
	!.
change_pcdata_into_atoms(Atom, Atom) :-
	( atomic(Atom) ; var(Atom) ),
	!.
change_pcdata_into_atoms(T, T1) :-
	functor(T, F, N),
	functor(T1, F, N),
	change_pcdata_into_atoms_args(N, T, T1),
	!.

change_pcdata_into_atoms_args(0, _T, _T1).
change_pcdata_into_atoms_args(I, T, T1) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	change_pcdata_into_atoms(Arg, Arg1),
	I1 is I - 1,
	!,
	change_pcdata_into_atoms_args(I1, T, T1).

%============================================================
change_strings_into_atoms(X=String, X=Output) :-
	atom_codes(Atom, String),
	(   atom_to_int(Atom, Number) ->
	    Output = Number ;
	    Output = Atom
	),
	!.
change_strings_into_atoms(Atom, Atom) :-
	( atomic(Atom) ; var(Atom) ),
	!.
change_strings_into_atoms(T, T1) :-
	functor(T, F, N),
	functor(T1, F, N),
	change_strings_into_atoms_args(N, T, T1),
	!.

change_strings_into_atoms_args(0, _T, _T1).
change_strings_into_atoms_args(I, T, T1) :-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	change_strings_into_atoms(Arg, Arg1),
	I1 is I - 1,
	!,
	change_strings_into_atoms_args(I1, T, T1).

%============================================================
/*
  Work down the XML until you get to the list of items under 
  "post". Then get the values. 
*/

/*
extract_xml_info(XML, InfoType, Value):-
	get_xml_content(XML, Content),
	get_message(Content, Message),
	get_result(Message, Result),
	get_task_posts(Result, TaskPosts),
	get_attribute_value(InfoType, TaskPosts, Value).

%the top level has predicate xml/2
get_xml_content(XML, Content):-
	XML = xml(_VersionEncoding, Content).

%most other levels have element items which have 3 arguments
get_message(XML_SubTerm, Message):-
	XML_SubTerm = [element(message, _, Message)].

get_result(XML_SubTerm, Result):-
	XML_SubTerm = [element(result, _, Result)].


% Since there are three kinds of "posts" under results, we take only the TaskPosts
% which are the only ones filled in the example. If you wanted Event or Note
% you could do something very similar

get_task_posts(XML_SubTerm, TaskPosts):-
	XML_SubTerm = [element(posts, [category='Task'|_], TaskPosts),_Event, _Note].
	
%There are 9 items in the post, here are matches for 7 of them.
get_attribute_value(id,         [element(post,_,[element('id',_,[ID]),_,_,_,_,_,_,_,_,_])], ID).
get_attribute_value(createtime, [element(post,_,[_,element('createtime',_,[CreateTime]),_,_,_,_,_,_,_,_])], CreateTime).
get_attribute_value(createtime_epoch, [element(post,_,[_,_,element('createtime_epoch',_,[CreateTimeEpoch]),_,_,_,_,_,_,_])], CreateTimeEpoch).
get_attribute_value(media_type, [element(post,_,[_,_,_,element('media_type',_,[MediaType]),_,_,_,_,_,_])], MediaType).
get_attribute_value(transcriptstatus, [element(post,_,[_,_,_,_,element('transcriptstatus',_,[TranscriptStatus]),_,_,_,_,_])], TranscriptStatus).
get_attribute_value(transcript, [element(post,_,[_,_,_,_,_,element('transcript',_,[Transcript]),_,_,_,_])], Transcript).
get_attribute_value(duration, [element(post,_,[_,_,_,_,_,_,element('duration',_,[Duration]),_,_,_])], Duration).
*/

%============================================================

test_access_functions :-
	prolog_xml(test2, XML),     %use the test xml
	format('~N~w~n~n', [XML]),  %print it out
	%InfoType = media_type,
	InfoType = createtime,
	extract_xml_info(XML, InfoType, Value),
	format('~NThe value of ~q is:~n~q~n', [InfoType, Value]).


%prologized xml for testing
prolog_xml(test2, 
xml([version='1.0',encoding='UTF-8'],
    [element(message, [],
             [element(result, [type=success],
                      [element(posts, [category='Task',howmany=1],
                               [element(post, [],
                                        [element(id,[],[149667]),
                                         element(createtime,[],['2008-05-21 20:07']),
                                         element(createtime_epoch,[],[1211400471]),
                                         element(media_type,[],['Text']),
                                         element(transcriptstatus,[],['DONE']),
                                         element(transcript, [],
                                                 ['This is a sample post from Sunil']),
                                         element(duration,[],[0]),
                                         element(property, [set=yes],
                                                 [element(name,[],[status]),
                                                  element(value,[],['PENDING']),
                                                  element(last_updated_at, [],
                                                          ['2008-05-21 20:07']),
                                                  element(updated_at_epoch,[],[1211400471])]),
                                         element(calendardate,[set=no],[]),
                                         element(delegation,[delegated=false],[])])]),
                       element(posts,[category='Event',howmany=0],[]),
                       element(posts,[category='Note',howmany=0],[])])])])).

/* A more general way to get information from the xml
   but it's more difficult to understand
*/
extract_xml_info_generic(PrologisedXML, InfoType, Values) :-
	extract_xml_info1(PrologisedXML, InfoType, Values-[]).  %calling a predicate that uses difference lists

% if you are looking at the right level just get the value
extract_xml_info1(element(InfoType, _Feats, [Value]), InfoType, [Value | Rest]-Rest) :-
	!.
% if you are looking at an atom, you aren't going to find a value
extract_xml_info1(Atom, _InfoType, Values-Values) :-
	atomic(Atom),
	!.
% if you are looking at a variable, you aren't going to find a value
extract_xml_info1(Var, _InfoType, Values-Values) :-
	var(Var),
	!.
%if it's a compound term figure out how many args there are and work through them
extract_xml_info1(Term, InfoType, ValuesIn-ValuesOut) :-
	compound(Term),
	functor(Term, _F, N),    %figure out the number of arguments
	extract_xml_info_args(N, Term, InfoType, ValuesIn-ValuesOut).  %use the number of args to know when you're done

%this predicate counts through the arguments of a predicate counting down until you're done

%no arguments left
extract_xml_info_args(I, _Term, _InfoType, Values-Values) :-
	I < 1,
	!.
%still have args to process
extract_xml_info_args(I, Term, InfoType, ValuesIn-ValuesOut) :-
	I >= 1,           %1 or more args
	arg(I, Term, Arg), %get the Ith arg 
	extract_xml_info1(Arg, InfoType, ValuesIn-ValuesNext),  %look for the value in the Ith arg
	I1 is I - 1,     %decrement
	!,
	extract_xml_info_args(I1, Term, InfoType, ValuesNext-ValuesOut).  %do it for the rest of the list
	


	
	