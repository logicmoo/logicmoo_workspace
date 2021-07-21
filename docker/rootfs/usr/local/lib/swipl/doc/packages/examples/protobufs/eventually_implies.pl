/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2013, Jeffrey Rosenwald
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

:- module(eventually_implies,
          [ (~>)/2,
            op(950, xfy, '~>')
          ]).

%!   ~>(:P, :Q)
%
%    Eventually_Implies(P, Q) is  semidet.   asserts  temporal  Liveness
%    (something good happens, eventually) and   Safety (nothing bad ever
%    happens) properties. Analogous to the "leads-to" operator of Owicki
%    and Lamport, 1982. Provides a sort   of  lazy implication described
%    informally as:
%
%      * Liveness: For all possible outcomes, P -> Q, eventually.
%      * Safety: For all possible outcomes, (\+P ; Q), is invariant.
%
%    Described practically:
%
%    P ~> Q, declares that if P is true, then Q must be true, now or at
%    some point in the future.

:- meta_predicate ~>(0,0).

~>(P, Q) :-
    setup_call_cleanup(P,
                       (true; fail),
                       Q -> true; throw(error(goal_failed(Q), context(~>, _)))).

