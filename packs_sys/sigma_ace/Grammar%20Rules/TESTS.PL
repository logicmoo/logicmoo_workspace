%-------------------------------
% CODE FOR TESTING
%-------------------------------

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% Test routines for ACE / CELT. Code to test various parts for regression testing.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

% CELT Version 2(b)--Simple Sentences, Queries, Syntax and Semantics. No quantifiers or compounds.

:-write('Loading CELT Parser Version 2(b) ...tests...syntactic and semantic translation for simple sentences'),nl.

:-write('test_parse_np_all/0 -- tests translation of noun phrases'),nl.
:-write('test_parse_sent_all/0 -- tests translation of complete sentences'),nl.
:-write('test_parse_query_all/0 -- tests translation of queries'),nl.

%-------------------------------
% THE GRAND TEST!
%-------------------------------

% test_CELT is intended for software regression testing
% of all new versions of CELT. It runs tests on the ability
% to parse different kinds of noun phrases, sentences, and
% queries to test different CELT capabilities.

test_CELT :- format("Testing noun phrase parses...~n"),
             test_parse_np_all('np_trace.txt'),
	     format("Testing sentence parses...~n"),
             test_parse_sent_all('sent_trace.txt'),
	     format("Testing query parses...~n"),
             test_parse_query_all('query_trace.txt'),
	     format("All testing completed and results saved.~n").

%-------------------------------
% CODE FOR TESTING PARSES
%-------------------------------

translate_also :- flag(translate,X,1). % call this to add translation
parse_only     :- flag(translate,X,0). % the default

% parse(+Nonterminal,+Words,+Expected,-Test_Result,-Features)
% conducts a parsing test: it attempts to parse the
% words Words with the grammar nonterminal Nonterminal
% which is assumed to have one arg, a set of features.
% If parse succeeds or fails as expected the test result
% is passed, otherwise it is failed.

% Test_Result is 'passed' or 'failed'

parse(Nonterminal,Words,Expected_result,Test_Result,Features) :- 
        functor(To_Parse,Nonterminal,1),       % e.g., functor(np(X),np,1).
        arg(1,To_Parse,Features),              % e.g., X = Features
        phrase(To_Parse,Words),                % e.g., phrase(np(Features),Words)
        !,                                     % If we parse OK, don't backtrack beyond here.
	nl,write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features),
	check_result(Expected_result,yes,Test_Result),
	nl.

parse(Nonterminal,Words,Expected_result,Test_Result,'empty') :- write('Failed to parse: '),write_sent(Words),
	check_result(Expected_result,no,Test_Result),
	nl.

% generate_all generates all NPs (or whatever Nonterminal is)
% that satisfy FEATURES. FEATURES can be used
% to specify initial features (e.g., gcat<->empty) or can be left
% unspecified (e.g., _).

generate_all(Nonterminal,Features) :-
        functor(To_Parse,Nonterminal,1),       % e.g., functor(np(X),np,1).
        arg(1,To_Parse,Features),              % e.g., X = Features
	phrase(To_Parse,Words), write_sent(Words),fail.
generate_all(Nonterminal).

% generate_some generates all NPs that have NUMBER of words in
% them, including each word in REQUIRED. FEATURES can be used
% to specify initial features (e.g., gcat<->empty) or can be left
% unspecified (e.g., _).

% e.g., generate_some(3,[card],gcat<->empty) generates all 3 word noun phrases
% with the word 'card' in them that would occur in normal noun phrases (i.e.,
% not inside of relative sentences where there may be a gap).
generate_some(Nonterminal,Number,Required,Features) :-
        functor(To_Parse,Nonterminal,1),       % e.g., functor(np(X),np,1).
        arg(1,To_Parse,Features),              % e.g., X = Features
	phrase(To_Parse,Words),                % e.g., phrase(np(Features),Words)
	length(Words,Number),
	subset(Required,Words),
	write_sent(Words),fail.
generate_some(Nonterminal,Number,Required,Features).

% check_result(+Expected_Results,+Actual_Results,-Passed_Or_Failed_Test).
% given an expected parse result of yes (parsed) or no (did not parse),
% compare this to the actual result. If they match return 'passed' in
% the last var, otherwise return (unify) it with 'failed'.
check_result(yes,yes,passed):-!, write('Test passed.'),nl.
check_result(no,no,passed)  :-!, write('Test passed.'),nl.
check_result(yes,no,failed) :- !, write('Test failed: these words should have parsed.'),nl.
check_result(no,yes,failed) :- !, write('Test failed: these words should NOT have parsed, but did.'),nl.

write_sent([]) :- nl.
write_sent([First|Rest]) :- format('~w ',First),write_sent(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_all is the main code called in     %
% test_parse_sent_all, test_parse_np_all, %
% and test_parse_query_all.               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% test_all(+Nonterminal,+File)
% retrieves all test/3 clauses, each of which specifies a test to try,
% it tries them all and saves test results which can be printed out
% after calling test_all by calling print_test_results (see below).

% If FILE is 'empty', 'stdio', or a variable then get_return is called to break
% up the output which should be being displayed on the terminal. It is
% skipped over if a filename has been given.

test_all(Nonterminal,File) :-
	test(Nonterminal,Words,Expected_result),
	flag(tried,Tried,Tried),
	Next is Tried + 1,
	format("~n~n****************~nTest ~w, parsing: ",[Next]),
	write_sent(Words),
	parse(Nonterminal,Words,Expected_result,Test_results,Features),
	describe_translation(Test_results,Expected_result,Nonterminal,Features,File),
	save_test_results(Words,Expected_result,Test_results),
	flag(tried,Tried,Next),
	fail.

test_all(Nonterminal,File).

% describe_translation(+Test_results,+Expected_result,+Nonterminal,+Features,+File)
% generates the SUMO forms from the parsed nonterminal
% (e.g., a parsed sentence, query, or np), then pretty
% prints those forms, describes the speech act involved,
% and then all the translation warnings.

describe_translation(Test_results,Expected_result,Nonterminal,Features,File) :-
        Test_results == passed,
	Expected_result = yes,
	get_return(File),
	write('Translation = '),nl,
	% traverse_features translates semantic features to logic, right now just for 'sentence', 'query', or 'np'
	traverse_features_and_return_existential(Nonterminal,Features,SUMO_FORMS,SPEECH_ACT), 
	pretty_print(SUMO_FORMS),nl,nl, 
	describe_speech_act(SPEECH_ACT),
	gather_translation_warnings(Nonterminal,Features,Translation_Warnings),
	write_numbered_list(Translation_Warnings),
	get_return(File),
	!.
	
describe_translation(Test_results,Expected_result,Nonterminal,Features,File) :- !.

% describe_speech_act(+SPEECH_ACT)
% describes a speech act which is either
% an assertion or a query (WH or YES_NO)
% with possible variables to find.

describe_speech_act(assertion) :-
	write('Speech Act = Assertion'),nl.

describe_speech_act(query(wh,VARS)) :-
	write('Speech Act = WH-Query, Find Variables: '),
	write(VARS),nl.

describe_speech_act(query(yes_no,[])) :-
	write('Speech Act = YES/NO-Query'),
	nl.

% describe_speech_act(+SPEECH_ACT,-TEXT)
% acts the same as describe_speech_act/1
% but returns its output as a string TEXT.

describe_speech_act(assertion,TEXT) :-
	sformat(TEXT,"Speech Act = Assertion~n").

describe_speech_act(query(wh,VARS),TEXT) :-
	sformat(TEXT,"Speech Act = WH-Query, Find Variables: ~w~n",VARS).

describe_speech_act(query(yes_no,[]),TEXT) :-
	sformat(TEXT,"Speech Act = YES/NO-Query~n").

% clear_test_results clears the assertions used to save test results.
clear_test_results :- flag(tried,_,0),flag(passed,_,0),flag(failed,_,0),retractall(failed_test(_,_)).

% save_test_results(Words,Expected_result,Test_results)
% ...Words is the list of words that were parsed or attempted,
% ...Expected_result is 'yes' or 'no' according to whether the parse should have succeeded or not,
% ...Test_results is 'passed' or 'failed' according to whether the particular test succeeded or not.
save_test_results(Words,Expected_results,Test_results) :-
	!,
	(Test_results == passed) ->  flag(passed,N,N+1);
	                            (flag(failed,N,N+1),assert(failed_test(Words,expected(Expected_results)))).

% print_test_results prints a summary of the saved test results.
print_test_results :-
	flag(tried,Tried,Tried),
	flag(passed,Passed,Passed),
	flag(failed,Failed,Failed),
	(Tried > 0 -> Ratio_Passed is (Passed / Tried) * 100; Ratio_Passed is 0),
	(Failed > 0 -> Ratio_Failed is (Failed / Tried) * 100; Ratio_Failed is 0),	
	format("~n~n Passed ~w tests (~2f %) and failed ~w tests (~2f %) tests out of ~w tried.~n~n",
	       [Passed,Ratio_Passed,Failed,Ratio_Failed,Tried]),
	not(Failed=0)->print_failed_tests;true.

% print_failed_results prints a summary of the tests failed.
print_failed_tests :-
	failed_test(Words,Expected),
	(Expected=expected(yes)->write('Should have parsed: ');write('Should NOT have parsed: ')),
	write_sent(Words),
	fail.
print_failed_tests.

%------------------------------------
% CODE FOR TESTING NOUN PHRASE PARSES
%------------------------------------

% test_parse_np_all runs all tests for NP parses.

test_parse_np_all :- test_parse_np_all(stdio). % shows results on TTY with pauses.

test_parse_np_all(File) :- (var(File);File=stdio;File=empty),!,clear_test_results,test_all(np,File),print_test_results.
	
test_parse_np_all(File) :- tell(File),clear_test_results,test_all(np,File),print_test_results,told.

% Generate all NPs.
test_gen_np_all :- generate_all(np,gcat<->empty..rel<->no). % Interrupt this with control-c when you've had enough.

% Particular tests to generate specific kinds of NPs.
test_gen_np1 :- generate_some(np,4,[card],gcat<->empty). % Generate all 4 word noun phrases with 'card' in them.
test_gen_np2 :- generate_some(np,2,[],gcat<->empty). % Generate all 2 word noun phrases
test_gen_np3 :- generate_some(np,_,[card,customer,'John'],gcat<->empty). % Generate all NPs with these words in them, any length

% Specific tests for gaps and relative sentences

% Strategy:
%
% 1. First test complete sentences
%    a.  Intransitive: The dog breathes.
%    b.  Transitive: The cat sees the mouse.
%    c.  Ditransitive: John gives a book to Mary.
% 2. Then test the same sentences with gaps
%    a.  Intransitive: ... breathes {e.g., 'that breathes'}
%    b.  Transitive: ... sees the mouse {e.g., 'that sees the mouse', gap fills in subject position}
%    c.              ... the cat sees {e.g., 'that the cat sees', gap fills in direct object position }
%    d.  Ditransitive: ...gives a book to Mary { e.g., 'who gives a book to Mary', gap fills in subject position}
%    e.                ...John gives to Mary { e.g., 'what John gives to Mary', gap fills in direct object position }
%    f.                ...John gives a book to { e.g., 'who John gives a book to', gap fills in indirect object position }
% 3. Then test the same relative sentences:
%    a.  Intransitive: ... that breathes
%    b.  Transitive: ... that sees the mouse {e.g., gap fills in subject position}
%    c.              ... that the cat sees {e.g.,  gap fills in direct object position }
%    d.  Ditransitive: ...who gives a book to Mary { e.g., gap fills in subject position}
%    e.                ...what John gives to Mary { e.g.,  gap fills in direct object position }
%    f.                ...who John gives a book to { e.g., gap fills in indirect object position }
% 4. Then test the complete noun phrases with the subordinate sentences added:
%    a.  Intransitive: ...the dog that breathes
%    b.  Transitive: ... the cat that sees the mouse {e.g., gap fills in subject position}
%    c.              ... the dog that the cat sees {e.g.,  gap fills in direct object position }
%    d.  Ditransitive: ...the man who gives a book to Mary { e.g., gap fills in subject position}
%    e.                ...the book that John gives to Mary { e.g.,  gap fills in direct object position }
%    f.                ...the person who John gives a book to { e.g., gap fills in indirect object position }

test_rel1a :-
	Words = [the,dog,breathes],
	phrase(sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel1b :-
	Words = [the,cat,sees,the,mouse],
	phrase(sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel1c :-
	Words = ['John',gives,a,book,to,'Mary'],
	phrase(sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel2a :-
	Words = [breathes],
	Features = syn<->(gcat<->object..gap<->that..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel2b :-
	Words = [sees,the,mouse],
	Features = syn<->(gcat<->object..gap<->that..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel2c :-
	Words = [the,cat,sees],
	Features = syn<->(gcat<->object..gap<->that..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel2d :-
	Words = [gives,a,book,to,'Mary'],
	Features = syn<->(gcat<->person..gap<->who..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel2e :-
	Words = ['John',gives,to,'Mary'],
	Features = syn<->(gcat<->object..gap<->that..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel2f :-
	Words = ['John',gives,a,book,to],
	Features = syn<->(gcat<->object..gap<->that..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel3a :-
	Words = [that,breathes],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel3b :-
	Words = [that,sees,the,mouse],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel3c :-
	Words = [that,the,cat,sees],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel3d :-
	Words = [who,gives,a,book,to,'Mary'],
	Features = syn<->(gcat<->person..gap<->who..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel3e :-
	Words = [that,'John',gives,to,'Mary'],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel3f :-
	Words = [that,'John',gives,a,book,to],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel4a :-
	Words = [the,dog,that,breathes],
	Features = syn<->(rel<->no)..sem<->SEMANTICS,
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel4b :-
	Words = [the,cat,that,sees,the,mouse],
	Features = syn<->(rel<->no)..sem<->SEMANTICS,
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel4c :-
	Words = [the,dog,that,the,cat,sees],
	Features = syn<->(rel<->no)..sem<->SEMANTICS,
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel4d :-
	Words = [the,man,who,gives,a,book,to,'Mary'],
	Features = syn<->(rel<->no)..sem<->SEMANTICS,
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel4e :-
	Words = [the,book,that,'John',gives,to,'Mary'],
	Features = syn<->(rel<->no)..sem<->SEMANTICS,
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_rel4f :-
	Words = [the,person,that,'John',gives,a,book,to],
	Features = syn<->(rel<->no)..sem<->SEMANTICS,
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_np :- Words = [the,bank],
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel0 :- Words = [the,bank,that,the,customer,enters],
	phrase(np(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel1 :- Words = [that,the,customer,inserts],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel2 :- Words = [that,the,customer,enters],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel3 :- Words = [who,is,'Swiss'],
	Features = syn<->(gcat<->person..gap<->who..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel4 :- Words = [who,enters,the,bank],
	Features = syn<->(gcat<->person..gap<->who..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel5 :- Words = [which,enters,the,code],
	Features = syn<->(gcat<->object..gap<->which..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).	

test_parse_rel6 :- Words = [that,is,'Swiss'],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel7 :- Words = [which,is,'Swiss'],
	Features = syn<->(gcat<->object..gap<->which..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel8 :- Words = [that,enters,the,code],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel9 :- Words = [that,'Mary',inserts],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel10 :- Words = [which,'Mary',inserts],
	Features = syn<->(gcat<->object..gap<->which..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel11 :- Words = [which,the,customer,gives,to,the,teller],
	Features = syn<->(gcat<->object..gap<->which..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel12 :- Words = [that,'John',gives,to,'Mary'],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).	

test_parse_rel13 :- Words = [that,he,gives,to,her],
	Features = syn<->(gcat<->object..gap<->that..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel14 :- Words = [who,the,customer,gives,the,code,to],
	Features = syn<->(gcat<->person..gap<->who..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_rel15 :- Words = [who,'John',gives,the,code,to],
	Features = syn<->(gcat<->person..gap<->who..rel<->no)..sem<->SEMANTICS,
	phrase(relative_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).	

% Specific tests for embedded sentences in relative clauses
test_parse_e1 :- Words = [is,'Swiss'],
	Features = syn<->(gcat<->person..gap<->who..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_e2 :- Words = [enters,the,bank],
	Features = syn<->(gcat<->person..gap<->who..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).	

%---------------------------------------
% CODE FOR TESTING SENTENCES AND QUERIES
%---------------------------------------

% Run all tests for SENT parses. Saves results to file FILE if called with a file name.
test_parse_sent_all :- test_parse_sent_all(stdio). % shows results on TTY with pauses.

test_parse_sent_all(File) :- (var(File);File=stdio;File=empty),!,clear_test_results,test_all(sentence,File),print_test_results.

test_parse_sent_all(File) :- tell(File),clear_test_results,test_all(sentence,File),print_test_results,told.

% Run all tests for QUERY parses. Saves results to file FILE if called with a file name.
test_parse_query_all :- test_parse_query_all(stdio).

test_parse_query_all(File) :- (var(File);File=stdio;File=empty),!,clear_test_results,test_all(query,File),print_test_results,told.

test_parse_query_all(File) :- tell(File),clear_test_results,test_all(query,File),print_test_results,told.

% Generate all SENTs.
test_gen_sent_all :- generate_all(sentence,gcat<->empty..rel<->no). % Interrupt this with control-c when you've had enough.

% Other tests...

% To test adjuncts
test_parse_adjuncts_all :- 
	test(adjuncts,Words,yes),
        phrase(adjuncts(3),Words),
	write_sent(Words),
	fail.

% To test generation...of predicates...
test_gen_predicate_all :- generate_all(predicate,gcat<->empty..rel<->no..max<->2). % Interrupt this with control-c...

%---------------------------------------
% CODE FOR TESTING VOCABULARY FROM WORDNET
%---------------------------------------

% To test vocabulary

% From Ian's first tests...
% 1.  This is the capital of Delaware.
% 2.  John is the president of the United States.
% 3.  Predators are large animals.
% 4.  Tom gave the card to Sarah.
% 5.  Who is the representative of the company?

ian1 :- eng2log('This city is the capital of Delaware.').       % OK
ian2 :- eng2log('John is the president of America.').           % OK (except translation)
ian3 :- eng2log('The predator is a large animal.').             % OK 
ian4 :- eng2log('Tom gives the card to Sarah.').                % OK (but slow)
ian5 :- eng2log('Who is the representative of the company?').   % OK

% From Ian's second tests...
% 1.  The boy ate the fish.
% 2.  The dog ran.
% 3.  The car is in the garage.
% 4.  The boy gave the girl a book.
% 5.  Trees are green.
% 6.  Apples are fruits.

ian6 :- eng2log('The boy eats the fish.').                      % OK
ian7 :- eng2log('The dog runs.').                               % OK
ian8 :- eng2log('The car is in the garage.').                   % OK!
ian9 :- eng2log('The boy gives a book to the girl.').           % OK
ian10 :- eng2log('The tree is green.').                         % OK
ian11 :- eng2log('An apple is a fruit.').                       % OK

ian_test :-
	ian1,
	ian2,
	ian3,
	ian4,
	ian5,
	ian6,
	ian7,
	ian8,
	ian9,
	ian10,
	ian11.

%---------------------------------------
% CODE FOR TESTING USING ACE TEST SENTENCES
%           IN TEST_SUITE.PL
%---------------------------------------

test_suite(v1) :-
	test_parse(v1, ID, Sentence, Description, Expected_Translation),
	nl,write('Test '),write(ID),write(' for CELT Version 1: '),
	eng2log(Sentence),
	nl,write('Expected Translation: '),nl,
	string_to_atom(Expected_Translation,Expected_Output),
	write(Expected_Output),nl,
	get_return,
	fail.

test_suite(v1) :- true.

%---------------------------------------------
% CODE FOR TESTING CONSISTENCY OF CELT LEXICON
%---------------------------------------------

% find_stative_verbs lists all stative verbs and whether they are mapped correctly to SUMO relations or not.
% list_stative_verb_warnings_to_file(+File) writes out all the warnings that find_stative_verbs would list but to a file.

list_stative_verb_warnings_to_file(Max) :-
	tell(stative_verb_warnings),
	flag(verbs_checked, _, 0),         % number of verbs checked
	find_stative_verbs(Max),
	told.

% find_stative_verbs lists all stative verbs and whether they are mapped correctly to SUMO relations or not.

find_stative_verbs(Max) :-
	verb_in_lexicon(Verb,Root,[Intransitive, Transitive, Ditransitive],Plurality,
			Event_or_State,Simple_or_Phrasal,SUMO_concept,WordNet_ID),
	check_stative(Verb,Root,Event_or_State,SUMO_concept,WordNet_ID),
	flag(verbs_checked, N, N+1),
	Max is N+1.           % this goal fails and we continue if we have not yet recorded Max entries.

find_stative_verbs(Max).      % we reach this goal if either enough verbs have been checked or there are no more verb lexicon entries.

% check_stative checks to see if Verb is a stative verb, according to WordNet,
% and if so, if it maps to SUMO relation.

check_stative(Verb,Root,Event_or_State,SUMO_concept,WordNet_ID) :-
	is_stative_verb_in_WordNet(WordNet_ID),
	is_stative_relation_in_SUMO(SUMO_concept),
	!,
	write('Check: The verb '),write(Verb),write('[WordNet Synset '),write(WordNet_ID),write(']'),
	write(' is a stative verb in WordNet and maps to the relation '),
	write(SUMO_concept),write(' in the SUMO.'),nl.

check_stative(Verb,Root,Event_or_State,SUMO_concept,WordNet_ID) :-
	not(is_stative_verb_in_WordNet(WordNet_ID)),
	not(is_stative_relation_in_SUMO(SUMO_concept)),
	!.
	
check_stative(Verb,Root,Event_or_State,SUMO_concept,WordNet_ID) :-
	is_stative_verb_in_WordNet(WordNet_ID),
	not(is_stative_relation_in_SUMO(SUMO_concept)),
	!,
	write('Warning: The verb '),write(Verb),write('[WordNet Synset '),write(WordNet_ID),write(']'),
	write(' is a stative verb in WordNet but does not map to a relation '),
	write('in SUMO, instead it maps to '),write(SUMO_concept),write('.'),nl.

check_stative(Verb,Root,Event_or_State,SUMO_concept,WordNet_ID) :-
	not(is_stative_verb_in_WordNet(WordNet_ID)),
	is_stative_relation_in_SUMO(SUMO_concept),
	!,
	write('Warning: The verb '),write(Verb),write('[WordNet Synset '),write(WordNet_ID),write(']'),
	write(' is not a stative verb in WordNet but does map to a SUMO relation: '),
	write(SUMO_concept),write('.'),nl.

check_stative(Verb,Root,Event_or_State,SUMO_concept,WordNet_ID) :-
	write('Error: The verb '),write(Verb),write(' fell thru the check_stative rules. This should not happen!'),nl.

% is_stative_verb_in_WordNet(+WordNet_ID) succeeds if the synset ID is a stative verb, fails otherwise.

% It checks to see if the synset ID descends from any of these (forms of 'be'):

% 201775973 - have the quality of being; (copula, used with an adjective or a predicate noun); "John is rich";
%             "This is not a good answer"
% 201811792 - occupy a certain position or area; be somewhere; "Where is my umbrella?" "The toolshed is in the back";
%	      also for abstract situations and relations: "What is behind this behavior?")	
% 201775163 - have an existence, be extant; "Is there a God?"
% 201781222 - happen or come to pass; "I lost my wallet; this was during the visit to my parents' house";
%  	      "There were two hundred people at his funeral"; "There was a lot of noise in the kitchen")
% 201817610 - be identical or equivalent to: "One dollar equals 1,000 rubles these days!"
% 201787769 - form or compose; "This money is my only income"; "The stone wall was the backdrop for the performance";
%             "These constitute my entire belonging"; "The children made up the chorus";
%             "This sum represents my entire income for a year"; "These few men comprise his entire army"
% 201666138 -  work in a specific place, with a specific subject, or in a specific function; "He is a herpetologist;
%              "She is our resident philosopher"; "She works as a waitress to put herself through law school"
% 201840295 - represent, as of a character on stage; "Derek Jacobi was Hamlet"
% 201843641 - be priced at; "These shoes cost $100"

% It also checks to see if the synset ID descends from any of these (forms of 'possess'):

% 201795751 - have as an attribute, knowledge, or skill; "he possesses great knowledge about the Middle East"
% 201509295 - have ownership or possession of; "He owns three houses in Florida"; "How many cars does she have?"
% 201807461 - enter into and control, as of emotions or ideas; "What possessed you to buy this house?" "A terrible rage possessed her"

% It also checks to see if the synset ID descends from any of these (forms of 'have'):
                                         
% 201508689 -- have or possess, either in a concrete or an abstract sense: "She has $1,000 in the bank";
%             "He has got two beautiful daughters"; "She holds a Master's degree from Harvard"
% 201794357 -- have as a feature; "This restaurant features the most famous chefs in France"
% 201443215 -- of mental or bodily states or experiences: "get an idea"; "experience vertigo";
%              "get nauseous"; "undergo a strange sensation"; "The fluid undergoes shear"; "receive injuries"; "have a feeling"
% 201509295 -- have ownership or possession of; "He owns three houses in Florida"; "How many cars does she have?"
% 201857688 -- be obliged, required, or forced to)
% 201620370 -- have a personal or business relationship with someone; "have a postdoc"; "have an assistant"; "have a lover"
% 201509557 -- have left; "I have two years left"; "I don't have any money left" "They had two more games left"
% 201876679 -- be confronted with: "What do we have here?"; "Now we have a fine mess"
% 200045715 -- suffer from; be ill with; "She has arthritis"
% 201530096 -- receive willingly something given or offered; "The only girl who would have him was the miller's daughter";
%              "I won't have this dog in my house!"; "Please accept my present"
% 200045966 -- of injuries and illnesses: "She suffered a fracture in the accident";
%           "He had an insulin shock after eating three candy bars"; "She got a bruise on her leg"; "He got his arm broken in the scuffle"
% 201608899 -- achieve a point or goal, as in a sport; "Nicklaus had a 70"; "The Brazilian team got 4 goals";
%            "She made 29 points that day"
% 201858069 -- be likely or probable; "They have to be kidding"


is_stative_verb_in_WordNet(WordNet_ID) :-
	is_hypernym_in_set_careful(WordNet_ID,[201775973, % have the quality of being
					       201811792, % occupy a certain position or area; be somewhere
					       201775163, % have an existence, be extant; "Is there a God?"
					       201781222, % happen or come to pass
					       201817610, % be identical or equivalent to: "One dollar equals 1,000 rubles these days!"
					       201787769, % form or compose; "This money is my only income"
					       201666138, % work in a specific place, with a specific subject, or in a specific function
					       201840295, % represent, as of a character on stage; "Derek Jacobi was Hamlet"
					       201843641, % be priced at; "These shoes cost $100"
                                                          % next are forms of 'possess'
                                               201795751, % have as an attribute, knowledge, or skill
					       201509295, % have ownership or possession of
					       201807461, % enter into and control, as of emotions or ideas
					                  % next are forms of 'have'
					       201508689, % have or possess, either in a concrete or an abstract sense
					       201794357, % have as a feature; "This restaurant features the most famous chefs in France"
					       201443215, % of mental or bodily states or experiences: "get an idea"; "experience vertigo"
					       201509295, % have ownership or possession of; "He owns three houses in Florida"
					       201857688, % be obliged, required, or forced to
					       201620370, % have a personal or business relationship with someone; "have a postdoc"
					       201509557, % have left; "I have two years left"
					       201876679, % be confronted with: "What do we have here?"; "Now we have a fine mess"
					       200045715, % suffer from; be ill with; "She has arthritis"
					       201530096, % receive willingly something given or offered
					       200045966, % of injuries and illnesses: "She suffered a fracture in the accident"
					       201608899, % achieve a point or goal, as in a sport; "Nicklaus had a 70"
					       201858069],% be likely or probable; "They have to be kidding"
				   []).

% is_stative_relation_in_SUMO(+SUMO_concept) succeeds if the SUMO concept is a SUMO relation, fails otherwise.

is_stative_relation_in_SUMO(SUMO_concept) :- instance_of(SUMO_concept,'Relation').

%---------------------------------------------
% CODE FOR TESTING MULTI-ARG SENTENCES
%---------------------------------------------

% Note 1: For now preposition should not be 'with' or 'of' and each preposition should be unique for a relation.
% Note 2: All parameter specs joined by 'and' at present.	
% Note 3: A noun phrase is parsed after a parameter specification so if the noun is a count noun it must be
%         preceded by a determiner, if a mass noun that can be omitted (e.g., 'law' is OK but not 'accounting',
%         instead use 'the accounting' as 'law' is a mass noun but 'accounting' is not at present.)
% Note 4: In test sentences numbers must be quoted, e.g., use '1' instead of just 1 in a test clause. This is
%         handled properly by the reader so don't worry about it if you are using top_level/0 or eng2log/2.

relation(employee_relation,[a,new,employee],employee,
	 [
	  spec(keywords([works,for]),attribute([supervisor]),type('Human')),              % first arg of employee_relation
	  spec(keywords([starts,on]),attribute([start,date]),type('Time')),               % second arg of employee_relation
	  spec(keywords([works,in]),attribute([office]),type('Place')),                   % third arg of employee_relation
	  spec(keywords([reports,to]),attribute([department]),type('Organization'))       % fourth arg of employee_relation
	 ]).

% Add test sentence...

% John is a new employee who works for Mary and starts on Tuesday and works in Los Angeles and reports to law.

test(sentence,['John',is,a,new,employee,who,works,for,'Mary',and,starts,on,'Tuesday',and,
	       works,in,'Los','Angeles',and,reports,to,law],yes).

relation(air_mission_relation,[a,new,air,mission],ato_mission,
	 [
	  spec(keywords([has,a,primary,target]),attribute([first,target]),type('ATOTarget')),    % first arg of air_mission_relation
	  spec(keywords([has,a,secondary,target]),attribute([second,target]),type('ATOTarget')), % second arg of air_mission_relation
	  spec(keywords([refuels,at]),attribute([refueling,point]),type('Place')),               % third arg of air_mission_relation
	  spec(keywords([has,a,waypoint,at]),attribute([waypoint]),type('Place')),               % fourth arg of air_mission_relation
	  spec(keywords([has,type]),attribute([mission,type]),type('ATOMissionType'))            % fifth arg of air_mission_relation
	 ]).

% Add test sentence...

% Air Mission 1 is a new air mission that has a primary target Target 1 and second target Target 2 and
% refuels at Location 5 and mission type combat air patrol.

test(sentence,['Mission','1',is,a,new,air,mission,that,has,a,primary,target,'Target','A',
	       and,second,target,'Target','B',and,refuels,at,'Location','5',and,mission,type,combat,air,patrol],yes).


test_relation1 :- phrase(sentence(Sentence),
			 ['Mary',is,a,new,employee,who,works,for,supervisor,'John',and,starts,on,'Tuesday']),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_relation2 :- phrase(sentence(Sentence),
			 ['Mary',is,a,new,employee,who,works,for,'John',and,has,start,date,'Tuesday',and,has,department,law]),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_relation3 :- phrase(sentence(Sentence),
			 ['Mission','1',is,a,new,air,mission,that,has,a,primary,target,first,target,'Location','5',
			  and,has,a,secondary,target,'Location','2']),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_relation4 :- phrase(sentence(Sentence),['Mission','2',is,a,new,air,mission,that,has,waypoint,'Location','3',
					     and,refuels,at,'Location','2']),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_relation5 :- phrase(sentence(Sentence),['Mission','1',is,a,new,air,mission,that,has,a,primary,target,'Target','A']),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_relation6 :- phrase(sentence(Sentence),['Mission','1',is,a,new,air,mission,that,has,a,primary,target,'Target','A',
					     and,second,target,'Target','B']),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_relation7 :- phrase(sentence(Sentence),['Mission','1',is,a,new,air,mission,that,has,a,primary,target,'Target','A',
					     and,second,target,'Target','B',and,refuels,at,'Location','5']),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_relation8 :- phrase(sentence(Sentence),['Mission','1',is,a,new,air,mission,that,has,a,primary,target,'Target','A',
					     and,second,target,'Target','B',and,refuels,at,'Location','5',
					     and,mission,type,combat,air,patrol]),
	write('Parameter values: '), display_feature_structure(Sentence),nl.

test_par_vals1 :- phrase(parameter_specifications(employee_relation,SPEC),
			 [works,for,supervisor,'John',and,starts,on,'Tuesday']),
	write('Parameter values: '), display_feature_structure(SPEC),nl.

test_par_vals2 :- phrase(parameter_specifications(employee_relation,SPEC),
			 [starts,on,'Tuesday',and,reports,to,'Los','Angeles']),
	write('Parameter values: '), display_feature_structure(SPEC),nl.

% Note: 'department law' parses but not 'department accounting' since
% accounting is not listed as a mass noun in CELT but law is. Thus the
% latter would require 'department the accounting' to parse.

test_par_vals3 :- phrase(parameter_specifications(employee_relation,SPEC),
			 [works,for,'John',and,has,start,date,'Tuesday',and,has,department,law]),
	write('Parameter values: '), display_feature_structure(SPEC),nl.

test_par_vals3a :- phrase(parameter_specifications(employee_relation,SPEC),
			 [works,for,'John']),
	write('Parameter values: '), display_feature_structure(SPEC),nl.

test_par_vals3b :- phrase(parameter_specifications(employee_relation,SPEC),
			 [works,for,'John',and,has,start,date,'Tuesday']),
	write('Parameter values: '), display_feature_structure(SPEC),nl.

test_par_vals3c :- phrase(parameter_specifications(employee_relation,SPEC),
			 [works,for,'John',and,has,department,law]),     % either 'law' (mass) or 'the payroll' (count noun) etc.
	write('Parameter values: '), display_feature_structure(SPEC),nl.

test_par_vals4 :- phrase(parameter_specifications(air_mission_relation,SPEC),
			 [has,a,primary,target,first,target,'Location','5',and,has,a,secondary,target,'Location','2']),
	write('Parameter values: '), display_feature_structure(SPEC),nl.

test_par_vals5 :- phrase(parameter_specifications(air_mission_relation,SPEC),
			 [waypoint,'Location','3',and,refuels,at,'Location','2']),
	write('Parameter values: '), display_feature_structure(SPEC),nl.

test_par_vals6 :- phrase(parameter_specifications(air_mission_relation,SPEC),
			     [has,a,primary,target,'Location','1',and,second,target,'Location','3']),
	write('Parameter values: '), display_feature_structure(SPEC),nl.	


test_par_val_spec1 :- phrase(parameter_value_specification(employee_relation,SPEC,NP),[works,for,supervisor,'John']),
	write('NP is '), display_feature_structure(NP),nl.
test_par_val_spec2 :- phrase(parameter_value_specification(employee_relation,SPEC,NP),[works,for,the,man]),
	write('NP is '), display_feature_structure(NP),nl.
test_par_val_spec3 :- phrase(parameter_value_specification(employee_relation,SPEC,NP),[supervisor,'Mary']),
	write('NP is '), display_feature_structure(NP),nl.
test_par_val_spec4 :- phrase(parameter_value_specification(employee_relation,SPEC,NP),[works,for,the,man,that,replaces,'John']),
	write('NP is '), display_feature_structure(NP),nl.
test_par_val_spec5 :- phrase(parameter_value_specification(employee_relation,SPEC,NP),[department,law]),
	write('NP is '), display_feature_structure(NP),nl.
test_par_val_spec6 :- phrase(parameter_value_specification(employee_relation,SPEC,NP),[reports,to,law]),
	write('NP is '), display_feature_structure(NP),nl.

test_np_aux1 :- phrase(np(NP),['Location','1']),  % Note: numbers must be quoted, '1' parses but not just 1.
	write('NP is '), display_feature_structure(NP),nl.

test_np_aux2 :- phrase(np(NP),['Location','2']),  % Note: numbers must be quoted, '2' parses but not just 2.
	write('NP is '), display_feature_structure(NP),nl.

test_par_val_spec7 :- phrase(parameter_value_specification(air_mission_relation,SPEC,NP),
			     [has,a,primary,target,first,target,'Location','5']),
	write('NP is '), display_feature_structure(NP),nl.

test_par_val_spec8 :- phrase(parameter_value_specification(air_mission_relation,SPEC,NP),
			     [refuels,at,'Location','2']),
	write('NP is '), display_feature_structure(NP),nl.

test_par_val_spec9 :- phrase(parameter_value_specification(air_mission_relation,SPEC,NP),
			     [waypoint,'Location','3']),
	write('NP is '), display_feature_structure(NP),nl.

test_par_val_spec10 :- phrase(parameter_value_specification(air_mission_relation,SPEC,NP),
			     [has,a,secondary,target,'Location','4']),
	write('NP is '), display_feature_structure(NP),nl.

test_par_spec1 :- phrase(parameter_specification(employee_relation,SPEC),[works,for,supervisor]).
test_par_spec2 :- phrase(parameter_specification(employee_relation,SPEC),[works,for]).
test_par_spec3 :- phrase(parameter_specification(employee_relation,SPEC),[supervisor]).

test_par_spec4 :- phrase(parameter_specification(air_mission_relation,SPEC),[has,a,primary,target,first,target]).
test_par_spec5 :- phrase(parameter_specification(air_mission_relation,SPEC),[refuels,at]).
test_par_spec6 :- phrase(parameter_specification(air_mission_relation,SPEC),[has,a,secondary,target]).
