/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(chat80,
	  [ chat_process/2,                     % +Question, -Answer

            chat_parse/2,                       % +Question, -Tree
            chat_semantics/2,                   % +Tree, -Query
            chat_optimize/2,			% +QueryIn, -Query
            chat_answer/2,                      % +Query, -Answer

            chat_example/3,                     % ?Nr, ?Sentence, ?Correct
            chat_print_tree/1,                  % +Tree

            test_chat/0,
	    rtest_chats/1                       % +Times
	  ]).
:- ensure_loaded(chat80/chat).

/** <module> CHAT80 driver for SWI-Prolog
*/

%!  test_chat
%
%   Run default demo suite, showing timing  and   warn  if the answer is
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
    sentence(Tree,Question,[],[],[]).

%!  chat_semantics(+Tree, -Query)
%
%   Translate the NLP parse tree into a Prolog query.

chat_semantics(Tree, Query) :-
    i_sentence(Tree,QT),
    clausify(QT,UE),
    simplify(UE,Query).

%!  chat_optimize(+QueryIn, -Query)
%
%   Optimize a query

chat_optimize(QueryIn, Query) :-
    qplan(QueryIn, Query).

%!  chat_answer(+Query, -Answer)
%
%   Find answers for the Query.

chat_answer(Query, Answer) :-
    answer(Query, Answer).

%!  chat_print_tree(+Tree)
%
%   Print an NLP parse tree

chat_print_tree(Tree) :-
    print_tree(Tree).

%!  chat_example(?Nr, ?Sentence, ?Correct)
%
%   True when Nr is the (integer) id of the tokenized Sentence and
%   Correct is the correct answer.

chat_example(Nr, Sentence, Correct) :-
    ed(Nr, Sentence, Correct).
