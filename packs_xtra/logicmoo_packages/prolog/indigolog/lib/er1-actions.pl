%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: lib/er1-actions.pl
%
%    WRITTEN BY: Sebastian Sardina (ssardina@cs.toronto.edu)
%    Time-stamp: <03/05/04 12:47:48 ssardina>
%    TESTED    : ECLiPSe 5.4 on RedHat Linux 6.2-7.2
%    TYPE CODE : system independent predicates 
%
% DESCRIPTION: mapping between ER1 low-level actions and ER1 IndiGolog actions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             July 9, 2002
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following definition of constants are provided:
%
% -- actionNum(ER1-HighLevelAction, ER1-LowLevelAction)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

actionNum(clearEvents, clear).
actionNum(events, events).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GRIPPER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A = [auto, close, open, status, stop]
actionNum(gripper(A), X) :- 
        ground(A),
        concat_atom(['gripper ', A], X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MOVEMENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General Movement
actionNum(move(D,U), X) :- 
        ground(D), ground(U),
        concat_atom(['move ', D, ' ',U], X).
actionNum(move(D), X)   :- 
        ground(D),
        concat_atom(['move ', D, ' cm'], X).

% Move forward and backwards 
actionNum(moveFwd(D), X)   :- 
        ground(D),
        concat_atom(['move ', D, ' cm'], X).
actionNum(moveBack(D), X)  :- 
        ground(D),
        D2 is (-1*D),
        concat_atom(['move ', D2, ' cm'], X).

% Rotation 
actionNum(turnAround,   'move 360 degrees').
actionNum(turnComplete, 'move 180 degrees').
actionNum(turnLeft,     'move 90 degrees').
actionNum(turnRight,    'move -90 degrees').

actionNum(rotateTowardsObject(O), X) :- 
        ground(O),
        concat_atom(['move rotate toward object', O], X).
actionNum(rotateTowardsColor(R, G, B), X) :- 
        ground(R), ground(G), ground(B),
        concat_atom(['move rotate toward color', R, ' ', G, ' ', B], X).

actionNum(moveDriveObject(O), X) :- 
        ground(O),
        concat_atom(['move drive toward object', O], X).
actionNum(moveDriveColor(R, G, B), X) :- 
        ground(R), ground(G), ground(B),
        concat_atom(['move drive toward color', R, ' ', G, ' ', B], X).

actionNum(stop, stop).
actionNum(freeze, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOUND and SPEECH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actionNum(speakOn, 'speak on').
actionNum(speakOff, 'speak off').
actionNum(playFile(F), X) :- 
        ground(F),
        concat_atom(['play file ', '\'', F, '\''], X).
actionNum(say(P), X) :- 
        ground(P),
        (is_list(P) -> concat_atom(P, Phrase) ; Phrase=P),
        concat_atom(['play phrase ', '\"', Phrase, '\"'], X).
actionNum(say(P,V), X) :- 
        ground(P), ground(V),
        (is_list(P) -> concat_atom(P, Phrase) ; Phrase=P),
        concat_atom(['play phrase ', '\"', Phrase, '\"', 
                     ' voice \"Microsoft ',V,'\"'], X).

actionNum(selectVoice(V), X) :- 
        ground(V),
        concat_atom(['set voice ', '\'', V, '\''], X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SENSING ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actionNum(position, 'position').

% Turn sensor S on: S = [ir, objects, speech, sound level]
actionNum(senseOn(S), X) :- 
        ground(S),
        concat_atom(['sense ', S], X).
actionNum(senseOff(S), X) :- 
        ground(S),
        concat_atom(['sense ', S, ' off'], X).

actionNum(senseGripper    ,'sense gripper').

actionNum(setIR_oa(on), 'set ir oa on all 50').
actionNum(setIR_oa(off), 'set ir oa off').
actionNum(setIR_oa(N, D), X) :-
        ground(N), ground(D),
        concat_atom(['set ir oa on all ',N,' disable distance ', D], X).

% A = [all, 1, 2, 3]
actionNum(senseIR(A), X) :- 
        ground(A),
        concat_atom(['ir ', A], X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETTINGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actionNum(setLinearVelocity(V), X) :- 
        ground(V),
        concat_atom(['set linear velocity ', V], X).
actionNum(setAngularVelocity(V), X) :- 
        ground(V),
        concat_atom(['set angular velocity ', V], X).

actionNum(setPower(stopped, P), X) :- 
        ground(P),
        concat_atom(['set power stopped ', P], X).
actionNum(setPower(moving, P), X) :- 
        ground(P),
        concat_atom(['set power moving ', P], X).

actionNum(setObjectConfidence(P), X) :- 
        ground(P),
        concat_atom(['set confidence threshold ', P], X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXOGENOUS ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXOGENOUS ACTIONS
actionNum(arrive, 'move done').
actionNum(finishSaying, 'play done').
actionNum(finishSaying, X) :- sub_atom(X,_,_,_,'play error'), !.
actionNum(getStuck, X) :- sub_atom(X,_,_,_,'move error'), !.

actionNum(spotObject(Object), X) :- 
        get_object_event(X, Object, _, _, _, _).

% A = lostObject <object>, e.g., lostObject warning sign
actionNum(lostObject(Object), A) :-
        string_to_atom(S, A),
        emptyString(Empty),
        string_to_atom(Space,' '),
        split_string(S, Space, Empty, List),
        List=[Type|RList],
        string_to_atom(Type, lostObject),   % Check it's a lost-object event
        join_string(RList, Space, SObject),
        string_to_atom(SObject, Object).


% An object stop event has the following form:
%   "object <name> <no features matched> <total features> <x> <x> <distance>
% e.g., object "warning sign" 5 30 89 89 67.6
get_object_event(AString, Object, Rate, X, Y, Distance) :-
        string_to_atom(String, AString),
        string_to_atom(Quote,'\"'),
        string_to_atom(Space,' '),
        split_string(String, Space, Quote, List),
        List=[Type|RList],
        string_to_atom(Type, object), % Check it's an object spot event
        reverse(RList, [SDistance, SY, SX, SFT, SFM|LObject]),
        string_to_number(SX, X),        % Get coordinate X
        string_to_number(SY, Y),        % Get coordinate Y
        string_to_number(SFT, FT),      % Get total features
        string_to_number(SFM, FM),      % Get features matched
        Rate is (100*FM)/FT,            % Calculate rate
        string_to_number(SDistance, Distance), % Get distance to object
        reverse(LObject, LObject2),
        join_string(LObject2, Space, SObject),   % Build the object name
        string_to_atom(SObject, Object).
        
        
        

