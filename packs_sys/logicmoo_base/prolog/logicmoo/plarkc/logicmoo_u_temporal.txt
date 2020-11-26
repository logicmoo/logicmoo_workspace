   % NOTICE: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %                                                                      %
   %  COPYRIGHT (2009) University of Dallas at Texas.                     %
   %                                                                      %
   %  Developed at the Applied Logic, Programming Languages and Systems   %
   %  (ALPS) Laboratory at UTD by Feliks Kluzniak.                        %
   %                                                                      %
   %  Permission is granted to modify this file, and to distribute its    %
   %  original or modified contents for non-commercial purposes, on the   %
   %  condition that this notice is included in all copies in its         %
   %  original form.                                                      %
   %                                                                      %
   %  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,     %
   %  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES     %
   %  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE AND     %
   %  NON-INFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR        %
   %  ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE FOR ANY DAMAGES OR       %
   %  OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, ARISING    %
   %  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR       %
   %  OTHER DEALINGS IN THE SOFTWARE.                                     %
   %                                                                      %
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Operators for expressing LTL formulae.

:- op( 10,  fy ,  -  ).   % not
:- op( 20, xfy , &   ).   % and
:- op( 30, xfy , v   ).   % or
:- op( 10,  fy , nextX   ).   % LTL: "next"
:- op( 10,  fy , eventuallyF   ).   % LTL: "eventually"
:- op( 10,  fy , alwaysG   ).   % LTL: "always"
:- op( 20, xfx , untilU   ).   % LTL: "until"
:- op( 20, xfx , releaseR   ).   % LTL: "release"

%%%%%

%--- Normalize an LTL formula by pushing negations to the level of propositions
%--- and applying some absorption and idempotency laws.


normalize( X, Normalized ) :-
        (
            var( X )
        ->
            write( '*** The formula is a Prolog variable!' ),
            nl,
            Normalized = ?
        ;
            once( norm( X, Normalized, OK ) )
        ->
            var( OK )
        ).


% If an error is found, OK will cease to be a variable.

norm(  - ( A v B ) , Normalized, OK ) :-
        once( norm(  -A &  -B , Normalized, OK ) ).

norm(  - ( A & B ) , Normalized, OK ) :-
        once( norm(  -A v  -B , Normalized, OK ) ).

norm(  - -A , Normalized, OK ) :-
        once( norm( A , Normalized, OK ) ).

norm(  -nextX A , Normalized, OK ) :-
        once( norm( nextX  -A , Normalized, OK ) ).

norm(  -eventuallyF A , Normalized, OK ) :-
        once( norm( alwaysG  -A , Normalized, OK ) ).

norm(  -alwaysG A , Normalized, OK ) :-
        once( norm( eventuallyF  -A , Normalized, OK ) ).

norm(  - ( A untilU B ) , Normalized, OK ) :-
        once( norm(  -A releaseR  -B , Normalized, OK ) ).

norm(  - ( A releaseR B ) , Normalized, OK ) :-
        once( norm(  -A untilU  -B , Normalized, OK ) ).

norm( A v B , NA v NB, OK ) :-
        once( norm( A, NA, OK ) ),
        once( norm( B, NB, OK ) ).

norm( A & B , NA & NB, OK ) :-
        once( norm( A, NA, OK ) ),
        once( norm( B, NB, OK ) ).

norm(  -A ,  -NA, OK ) :-
        once( norm( A, NA, OK ) ).

norm( nextX A , nextX NA, OK ) :-
        once( norm( A, NA, OK ) ).

norm( eventuallyF A , eventuallyF NA, OK ) :-
        once( norm( A, NA, OK ) ).

norm( alwaysG A , alwaysG NA, OK ) :-
        once( norm( A, NA, OK ) ).

norm( A untilU B , NA untilU NB, OK ) :-
        once( norm( A, NA, OK ) ),
        once( norm( B, NB, OK ) ).

norm( A releaseR B , NA releaseR NB, OK ) :-
        once( norm( A, NA, OK ) ),
        once( norm( B, NB, OK ) ).

norm( eventuallyF eventuallyF A , Normalized, OK ) :-
        once( norm( eventuallyF A , Normalized, OK ) ).

norm( alwaysG alwaysG A , Normalized, OK ) :-
        once( norm( alwaysG A , Normalized, OK ) ).

norm( A untilU (A untilU B) , Normalized, OK ) :-
        once( norm( A untilU B , Normalized, OK )  ).

norm( (A untilU B) untilU B , Normalized, OK ) :-
        once( norm( A untilU B , Normalized, OK )  ).

norm( eventuallyF alwaysG eventuallyF A , Normalized, OK ) :-
        once( norm( alwaysG eventuallyF A , Normalized, OK ) ).

norm( alwaysG eventuallyF alwaysG A , Normalized, OK ) :-
        once( norm( eventuallyF alwaysG A , Normalized, OK ) ).

norm( nextX A & nextX B , Normalized, OK ) :-
        once( norm( nextX (A & B) , Normalized, OK ) ).

norm( alwaysG A & alwaysG B , Normalized, OK ) :-
        once( norm( alwaysG (A & B) , Normalized, OK ) ).

norm( eventuallyF A v eventuallyF B , Normalized, OK ) :-
        once( norm( eventuallyF (A v B) , Normalized, OK ) ).

norm(  -P,  -P, _ ) :-
        proposition( P ).

norm( P, P, _ ) :-
        proposition( P ).

norm( X, ?, ? ) :-
        write( '*** This is not a well-formed (sub)formula: \"' ),
        write( X ),
        write( '\"' ),
        nl.


   % NOTICE: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %                                                                      %
   %  COPYRIGHT (2009) University of Dallas at Texas.                     %
   %                                                                      %
   %  Developed at the Applied Logic, Programming Languages and Systems   %
   %  (ALPS) Laboratory at UTD by Feliks Kluzniak.                        %
   %                                                                      %
   %  Permission is granted to modify this file, and to distribute its    %
   %  original or modified contents for non-commercial purposes, on the   %
   %  condition that this notice is included in all copies in its         %
   %  original form.                                                      %
   %                                                                      %
   %  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,     %
   %  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES     %
   %  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE AND     %
   %  NON-INFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR        %
   %  ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE FOR ANY DAMAGES OR       %
   %  OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, ARISING    %
   %  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR       %
   %  OTHER DEALINGS IN THE SOFTWARE.                                     %
   %                                                                      %
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% A consistency checker for automata.  See "verifier.tlp".

% Check the consistency of the automaton's description.

:- [ 'partition_graph.pl' ].



% NOTE: The dynamic declaration is necessary for Eclipse.

:- dynamic automaton_error/0.

automaton_error.  % Will be retracted: needed to suppress a warning from Sicstus

check_consistency :-
        retractall( automaton_error ),
        check_connectedness,
        check_propositions,
        check_transitions,
        (
            automaton_error
        ->
            fail
        ;
            true
        ).


% If the graph is not connected, print a warning.

check_connectedness :-
        partition( Components ),
        length( Components, NumberOfComponents ),
        (
            NumberOfComponents =:= 1                                % connected
        ->
            true
        ;
            write( 'WARNING: The graph is not connected!' ),
            nl,
            write( 'The partitions are: ' ),
            write( Components ),
            nl
        ).


% Make sure propositions don't clash with operators.

check_propositions :-
        proposition( P ),
        (
            \+ atom( P )
        ->
            write( 'A proposition must be an atom: ' ),
            write( '\"' ),
            write( P ),
            write( '\"' ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        (
            member( P, [ 'v', 'nextX', 'eventuallyF', 'alwaysG', 'untilU', 'releaseR' ] )
        ->
            write( '\"v\", \"nextX\", \"eventuallyF\", \"alwaysG\", \"untilU\" and \"releaseR\" ' ),
            write( 'cannot be propositions: ' ),
            write( '\"' ),
            write( P ),
            write( '\"' ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        fail.

check_propositions.


% Make sure that there is no state with no outgoing transitions, and that all
% transitions are between states.

check_transitions :-
        trans( S1, S2 ),
        (
            (var( S1 ) ;  var( S2 ) ; \+ state( S1 ) ; \+ state( S2 ))
        ->
            write( 'Transitions can only occur between states: ' ),
            write( S1 ),
            write( ' ---> ' ),
            write( S2 ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        fail.

check_transitions :-
        state( S ),
        (
            (\+ trans( S, _Set ) ; trans( S, [] ))
        ->
            write( 'No transition out of state ' ),
            write( S ),
            nl,
            assert( automaton_error )
        ;
            true
        ),
        fail.

check_transitions.

%-------------------------------------------------------------------------------
   % NOTICE: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %                                                                      %
   %  COPYRIGHT (2009) University of Dallas at Texas.                     %
   %                                                                      %
   %  Developed at the Applied Logic, Programming Languages and Systems   %
   %  (ALPS) Laboratory at UTD by Feliks Kluzniak.                        %
   %                                                                      %
   %  Permission is granted to modify this file, and to distribute its    %
   %  original or modified contents for non-commercial purposes, on the   %
   %  condition that this notice is included in all copies in its         %
   %  original form.                                                      %
   %                                                                      %
   %  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,     %
   %  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES     %
   %  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE AND     %
   %  NON-INFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR        %
   %  ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE FOR ANY DAMAGES OR       %
   %  OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, ARISING    %
   %  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR       %
   %  OTHER DEALINGS IN THE SOFTWARE.                                     %
   %                                                                      %
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  Some "higher order" predicates for Prolog.                              %%%
%%%  This particular version was                                             %%%
%%%  written by Feliks Kluzniak at UTD (February 2009).                      %%%
%%%                                                                          %%%
%%%  Last update: 11 June 2009.                                              %%%
%%%                                                                          %%%

%%% NOTE: Throughout the file "predicate name" will be used either for
%%        the name of a predicate, or for a predicate with an incomplete
%%        list of arguments (a partially applied predicate): see apply/2.


%%------------------------------------------------------------------------------
%% apply( + predicate name (possibly with arguments), + list of arguments ):
%% Extend the list of arguments of the first argument with the second argument
%% and invoke the result.
%% For example, if we have
%%     sum( A, B, C ) :-C is A + B.
%% then
%%     map( sum(5 ), [ 1, 2, 3 ], Result )
%% will bind Result to [ 6, 7, 8 ].

apply( PredNameArgs, Arguments ) :-
        PredNameArgs =.. [ PredName | Args ],
        append( Args, Arguments, AllArgs ),
        Literal =.. [ PredName | AllArgs ],
        call( Literal ).


%%------------------------------------------------------------------------------
%% map( + predicate name, + list, - mapped list ):
%% The predicate should implement a unary function, i.e.,
%%   - it should take two arguments, the first of which is an input argument,
%%     and the second of which is an output argument;
%%   - it should always succeed, and the first result should be "what we want".
%% The predicate is applied to input arguments from the list, and the
%% corresponding outputs are returned in the second list.
%%
%% Example:
%%          square( M, N ) :-N is M * M.
%%
%%          ?- map( square, [ 1, 2, 3 ], Ans ).
%%
%%          Ans = [ 1, 4, 9 ].

map( _, [], [] ).

map( PredName, [ H | T ], [ NH | NT ] ) :-
        apply( PredName, [ H, NH ] ),
        map( PredName, T, NT ).


%%------------------------------------------------------------------------------
%% filter( + predicate name, + list, - filtered list ):
%% The predicate should take one argument.
%% The output list will contain only those elements of the input list for which
%% the predicate succeeds.

filter( _, [], [] ).

filter( PredName, [ H | T ], NL ) :-
        (
            apply( PredName, [ H ] )
        ->
            NL = [ H | NT ]
        ;
            NL = NT
        ),
        filter( PredName, T, NT ).


%%------------------------------------------------------------------------------
%% filter2( + predicate name, + list, - yes list, - no list ):
%% The predicate should take one argument.
%% The first output list will contain only those elements of the input list for
%% which the predicate succeeds; the second output list will contain only those
%% elements of the input list for which the predicate fails.

filter2( _, [], [], [] ) :-!.

filter2( PredName, [ H | T ], [ H | Yes ], No ) :-
        apply( PredName, [ H ] ),
        !,
        filter2( PredName, T, Yes, No ).

filter2( PredName, [ H | T ], Yes, [ H | No ] ) :-
        % \+ apply( PredName, [ H ] ),
        filter2( PredName, T, Yes, No ).


%%------------------------------------------------------------------------------
%% fold( + predicate name,+ initial value, + list, - final value ):
%% The predicate should implement a binary function, i.e.,
%%   - it should take three arguments, the first two of which are input
%%     arguments, and the third of which is an output argument;
%%   - it should always succeed, and the first result should be "what we want".
%% If the list is empty, the initial value is returned; otherwise the predicate
%% is applied to the initial value and the first member of the list, and then
%% to the result and the third member, and so on.
%% For example, if "sum( A, B, C )" unifies "C" with the sum of "A" and "B",
%% then "fold( sum, 0, [1,2,3], S )" unifies "S" with "6".

fold( _, Initial, [], Initial ).

fold( PredName, Initial, [ H | T ], Result ) :-
        apply( PredName, [ Initial, H, R ] ),
        fold( PredName, R, T, Result ).

%%------------------------------------------------------------------------------


   % NOTICE: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %                                                                      %
   %  COPYRIGHT (2009) University of Dallas at Texas.                     %
   %                                                                      %
   %  Developed at the Applied Logic, Programming Languages and Systems   %
   %  (ALPS) Laboratory at UTD by Feliks Kluzniak.                        %
   %                                                                      %
   %  Permission is granted to modify this file, and to distribute its    %
   %  original or modified contents for non-commercial purposes, on the   %
   %  condition that this notice is included in all copies in its         %
   %  original form.                                                      %
   %                                                                      %
   %  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,     %
   %  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES     %
   %  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE AND     %
   %  NON-INFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR        %
   %  ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE FOR ANY DAMAGES OR       %
   %  OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE, ARISING    %
   %  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR       %
   %  OTHER DEALINGS IN THE SOFTWARE.                                     %
   %                                                                      %
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is an experimental version of a pure Prolog counterpart of verifier.tlp.
%%
%% The approach is to visit each node at most once, and rewrite the expression
%% to take into account the valuation of propositions in that node.
%% The cost is O( number of nodes ) * O( length of formula ).
%% We don't yet have a proof of correctness, but all the examples work.
%%
%% Written by Feliks Kluzniak at UTD (March 2009).
%% Last update: 24 April 2009.


:- [ 'operators.pl' ].
:- [ 'normalize.pl' ].
:- [ 'looping_prefix.pl' ].
:- [ 'consistency_checker.pl' ].
:- [ '../../higher_order.pl' ].

:- ensure_loaded( library( lists ) ).  % Sicstus, reverse/2.


%% Check whether the state satisfies the formula.
%% This is done by checking that it does not satisfy the formula's negation.
%% (We have to apply the conditional, because our tabling interpreter does not
%%  support the cut, and we don't yet support negation for coinduction.)

check( State, Formula ) :-
        check_consistency,
        (
            state( State )
        ->
            true
        ;
            write( '\"' ),
            write( State ),
            write( '\" is not a state' ),
            nl,
            fail
        ),
        write( 'Query for state ' ),
        write( State ),
        write( ': ' ),
        write( Formula ),
        nl,
        once( normalize(  -Formula, NormalizedNegationOfFormula ) ),
        write( '(Negated and normalized: ' ),
        write( NormalizedNegationOfFormula ),
        write( ')' ),
        nl,
        (
            once( verify( State, NormalizedNegationOfFormula, [] ) )
        ->
            fail
        ;
            true
        ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% verify( + state, + formula, + path ) :
%% Verify whether the formula holds for this state (which we reached by this
%% path).
%% (The formula is our negated thesis, so we are looking for one path.)

verify( S, F, Path ) :-
        rewrite( F, S, NF ),
        (
            NF = true
        ->
            show_path( Path )
        ;
            NF = false
        ->
            fail
        ;
            (
                member( pair( S, F ), Path )
            ->
                (
                    disjunct( alwaysG _, F )
                ->
                    show_path( Path )
                ;
                    fail
                )
            ;
                once( strip_off_x( NF, NNF ) ),
                trans( S, NS ),
                verify( NS, NNF, [ pair( S, F ) | Path ] )
            )
        ).


%
show_path( Path ) :-
        write( 'COUNTEREXAMPLE: ' ),
        reverse( Path, RevPath ),
        map( first, RevPath, TruePath ),
        write( TruePath ),
        nl.

%
first( pair( State, _ ), State ).


%% disjunct( +- disjunct, + formula ):
%% Like member, only of an outermost disjunction rather than a list.

disjunct( A, A v _ ).
disjunct( A, _ v B ) :-disjunct( A, B ).
disjunct( A, A     ).


%% strip_off_x( + formula, - formula ):
%% Strip off the "nextX" operator from every disjunct, raise an alarm and abort if
%% there are disjuncts that are not so wrapped.

strip_off_x( nextX A v B, A v NB ) :-strip_off_x( B, NB ).

strip_off_x( nextX F, F ).

strip_off_x( F, _ ) :-
        F \= nextX F,
        write( 'Formula not in X : ' ),
        write( F ),
        nl,
        abort.



%% rewrite( + formula, + state, - new formula ):
%% The formula has been normalized, so that negations are applied only to
%% propositions.

rewrite( F, S, NF ) :-
        once( releaseR( F, S, NF ) ).

%
releaseR( A v B, S, NF  ) :-releaseR( A, S, NA ),  releaseR( B, S, NB ),
                       simplify( NA v NB, NF ).

releaseR( A & B, S, NF  ) :-releaseR( A, S, NA ),  releaseR( B, S, NB ),
                       simplify( NA & NB, NF ).

releaseR( nextX A  , _, nextX A ).

releaseR( eventuallyF A  , S, NF  ) :-releaseR( A, S, NA ),
                       simplify( NA v nextX eventuallyF A, NF ).

releaseR( alwaysG A  , S, NF  ) :-releaseR( A, S, NA ),
                       simplify( NA & nextX alwaysG A, NF ).

releaseR( A untilU B, S, NF  ) :-releaseR( A, S, NA ),  releaseR( B, S, NB ),
                       simplify( NA & nextX (A untilU B), Conj ),
                       simplify( NB v Conj, NF ).

releaseR( A releaseR B, S, NF  ) :-releaseR( A, S, NA ),  releaseR( B, S, NB ),
                       simplify( NB & NA, Conj ),
                       simplify( Conj v nextX (A releaseR B), NF ).

releaseR(  -P  , S, NF  ) :-proposition( P ),
                       ( holds( S, P ) -> NF = false
                       ;                  NF = true
                       ).

releaseR( P    , S, NF  ) :-proposition( P ),
                       ( holds( S, P ) -> NF = true
                       ;                  NF = false
                       ).


%% simplify( + formula, + state,- new formula ):
%% The formula has now been rewritten: simplify the result.

simplify( F, NF ) :-
        once( s( F, NF ) ).

%
s( true    v _    , true  ).
s( _       v true , true  ).
s( false   v A    , NA    ) :-s( A        , NA ).
s( A       v false, NA    ) :-s( A        , NA ).
s(  A v A  v B    , NF    ) :-s( A v B    , NF ).
s( (A v B) v C    , NF    ) :-s( A v B v C, NF ).

s( false   & _    , false ).
s( _       & false, false ).
s( true    & A    , NA    ) :-s( A        , NA ).
s( A       & true , NA    ) :-s( A        , NA ).
s(  A & A  & B    , NF    ) :-s( A & B    , NF ).
s( (A & B) & C    , NF    ) :-s( A & B & C, NF ).
s( nextX A     & nextX B  , nextX NF  ) :-s( A & B    , NF ).

s( F              , F     ).

%-------------------------------------------------------------------------------

