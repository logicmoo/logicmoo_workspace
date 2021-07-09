/*  Copyright 1986-2020 David H. D. Warren, Fernando C. N. Pereira and
    Jan Wielemaker.

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

:- module(chat80,
	  [ chat_process/2,                     % +Question, -Answer

            chat_parse/2,                       % +Question, -Tree
            chat_semantics/2,                   % +Tree, -Query
            chat_optimize/2,			% +QueryIn, -Query
            chat_answer/2,                      % +Query, -Answer

            chat_example/3,                     % ?Nr, ?Sentence, ?Correct
            chat_print_tree/1,                  % +Tree

            test_chat80/0,
	    rtest_chats/1                       % +Times
	  ]).
:- require([ (mode)/1,
             display/1
           ]).
:- ensure_loaded(library(parser_sharing)).	% misc
:- ensure_loaded(chat).

:- system:abolish(system:time/1).
:- system:use_module(library(statistics)).
:- system:import(prolog_statistics:time/1).
%user:test_chat80:- trace,chat80:test.


/** <module> CHAT80 driver for SWI-Prolog
*/

%!  test_chat80
%
%   Run default demo suite, showing timing  and   warn  if the answer80 is
%   incorrect.

%!  rtest_chats(+Times)
%
%   Run the test  suite  Times  times   silently.  Used  for  timing and
%   profiling.

%!  chat_process(+Question, -Answer)
%
%   Process Question using chat80, resulting in Answer.  Question is a
%   list of atoms expressing words.  For exampple,
%
%   ```
%   ?- chat_process([what, is, the,capital, of, france, ?], A).
%   A = [paris].
%   ```
%
%   @see tokenize_atom/2.

chat_process(Question, Answer) :-
    process(Question, Answer, _Times).

%!  chat_parse(+Question, -Tree)
%
%   Perform the parsing phase of CHAT80.

chat_parse(Question, Tree) :-
    sentence80(Tree,Question,[],[],[]).

%!  chat_semantics(+Tree, -Query)
%
%   Translate the NLP parse tree into a Prolog query.

chat_semantics(Tree, Query) :-
    i_sentence(Tree,QT),
    clausify80(QT,UE),
    simplify80(UE,Query).

%!  chat_optimize(+QueryIn, -Query)
%
%   Optimize a query

chat_optimize(QueryIn, Query) :-
    qplan(QueryIn, Query).

%!  chat_answer(+Query, -Answer)
%
%   Find answers for the Query.

chat_answer(Query, Answer) :-
    answer80(Query, Answer).

%!  chat_print_tree(+Tree)
%
%   Print an NLP parse tree

chat_print_tree(Tree) :-
    print_tree(Tree).

%!  chat_example(?Nr, ?Sentence, ?Correct)
%
%   True when Nr is the (integer) Id of the tokenized Sentence and
%   Correct is the correct answer80.

chat_example(Nr, Sentence, Correct) :-
    ed(Nr, Sentence, Correct).
