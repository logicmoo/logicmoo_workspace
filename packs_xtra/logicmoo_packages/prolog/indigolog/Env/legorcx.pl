%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE      : Env/legorcx.pl
%  Time-stamp: <02/11/30 01:16:39 ssardina>
%
%  Author      : First, by Maurice Pagnucco and Hector J. Levesque
%                Then, by Sebastian Sardina 
%  DESCRIPTION : Prolog code to establish communication with 
%                LEGO Mindstorms RCX
%                contains sockets, strings, OS tools, others
%  email       : ssardina@cs.toronto.edu
%  WWW         : www.cs.toronto.edu/~ssardina
%  TYPE CODE   : system independent predicates
%  TESTED      : ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%                SWI Prolog 5.0.10 under RedHat Linux 6.2-8.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             June 15, 2000
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
% 
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
% NOTE: All communication between Prolog and the RCX is initiated by Prolog
%
% This file defines the following top level predicates
%
% -- initializeRcx 
%             prepare the RCX for reading
% -- finalizeRcx 
%             finished with RCX for reading and writing
% -- sendRcxActionNumber(+Num, -Result) 
%             send the action number Num to the RCX, and return 
%             the value Result from the RCX
% -- receiveRcxActionNumber(-Actions) 
%             receive 0 or 1 action numbers in list Actions from RCX. 
%             Fail if no reply from RCX
%
% The following *system dependent* predicates are assumed to be predefined:
%
% -- initRcx           : initialize serial port, baud, parity etc. for RCX
% -- openRcxRead       : open RCX serial port for reading
% -- openRcxWrite      : open RCX serial port for writing
% -- closeRcx          : close RCX serial port
% -- getRcxByte(-Ascii): get a character from RCX
% -- eofRcx            : succeed if there is no character from RCX to read
% -- putRcxByte(+Ascii): write a character to RCX
% -- currentTime(-Time): current system time in 100ths of seconds
% -- waitUntilRcx(+End): optionally wait (busy or suspend) until End 
%                        system time or until RCX input arrives. 
%                        Fail if End is passed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Msg numbers in RCX for send/receive
% rcxMessageRange(+MesgType, -MesgLow, -MesgHigh, -MesgBase): Send messages
%     using numbered RCX messages in the range MesgLow--MesgHigh in
%     base MesgBase (= 2^n where n is the number of bits per message)

rcxMessageRange(action, 32, 63, 16).  % Primitive action message range
rcxMessageRange(value, 64, 79, 8).    % Sensing value message range

% Special message numbers
rcxPanicMessage(80).     % Panic message - RCX is in a panicked state
rcxAbortMessage(81).     % Abort message - reset RCX
rcxContinueMessage(82).  % Used for synchronisation in multi-part messages
rcxExogReqMessage(83).   % Request whether RCX has exogenous action to report
rcxNoExogMessage(84).    % Response from RCX when no detected exogenous actions
rcxDelayMessage(85).     % Response from RCX to obtain more time for the
                         %   execution of a primitive action

% Delays in hundredths of seconds
rcxResponseDelay(200).    % Normal delay for RCX that is ready to answer
%rcxResponseDelay(30).    % Normal delay for RCX that is ready to answer
rcxPyramidDelay(600).    % Window of time while infrared Pyramid is active
rcxLongDelay(1500).      % Time for RCX to complete longest behaviour
rcxGetDelay(50).         % Minimum time between subsequent rcxGetMess

% We assume that we are open for read except during putRcxList below

% initializeRcx: Perform any necessary initialization of RCX
initializeRcx :-
    initRcx,             % Initialise serial port settings
    openRcxRead.         % Have RCX ready to read

% finalizeRcx: Shut down RCX
finalizeRcx :-
    closeRcx. % Close serial port

% rcxMess(?MesgNo, ?CharList): Verifies that CharList is the correct
%     sequence of 9 bytes for user message number MesgNo
rcxMess(MesgNo, CharList) :-
    CharList = [85, 255, 0, 247, 8, MesgNo, N1, CheckSum, C1],
    N1 is 255 - MesgNo,
    CheckSum is mod(247 + MesgNo, 256),
    C1 is 255 - CheckSum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOW LEVEL SENDING TO RCX -  put lists of ascii bytes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% putRcxMess(+MesgNo): Send message number MesgNo to RCX
putRcxMess(MesgNo) :- 
    rcxMess(MesgNo,CharList),          % Generate a list of 9 characters
    putRcxList(CharList).              % Send them out to the RCX

% putRcxList(+CharList): Open serial port for writing and transmit CharList
putRcxList(CharList) :-
    closeRcx,
    openRcxWrite,
    putRcxL(CharList),
    closeRcx,
    openRcxRead.

% putRcxL(+CharList): Send characters in CharList one at a time
putRcxL([]).
putRcxL([FirstChar|CharList]) :-
    putRcxByte(FirstChar),
    putRcxL(CharList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOW LEVEL RECEIVING FROM RCX  -  get lists of ascii bytes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% getRcxMess(+MesgNo, +End): Get message number MesgNo from RCX by End time.
%     Succeeds if message of correct format received within End time
getRcxMess(MesgNo, End) :- 
    getRcxList(9, CharList, End),      % Read a list of 9 bytes 
    rcxMess(MesgNo, CharList).         % Check the format of Mess

% getRcxList(+NumBytes, -CharList, +End): Returns NumBytes of data from
%     RCX before End time has passed. On backtracking, it uses maybeSkip to
%     obtain the next byte from the RCX, while discarding the first byte
getRcxList(NumBytes, CharList, End) :-
    getRcxCount(NumBytes, Z, E, End),
    maybeSkip(Z, E, CharList, End).

% getRcxCount(+NumBytes, -CharList, -ListEnd, +End): Reads NumBytes of
%     input from RCX and returns it in CharList before End. The end of the
%     list ListEnd is left open so that new bytes can be added as needed.
getRcxCount(0, L, L, _).
getRcxCount(NumBytes, [X|L], E, End) :-
    NumBytes > 0,
    waitRcxByte(X, End),
    M1 is NumBytes - 1,
    getRcxCount(M1, L, E, End).

% maybeSkip(+CurList, -NewEnd, -NewList, +End): Either return CurList
%     of bytes, or if time remains, drop a byte and read a new one at the end
maybeSkip(Z, [], Z, _).
maybeSkip([_|Z], [X|E], L, End) :-
    waitRcxByte(X, End),
    maybeSkip(Z, E, L, End).

% waitRcxByte(-Ascii, +End): Get a byte from RCX before End time or fail
waitRcxByte(Ascii, End) :- 
    eofRcx -> 
        (waitUntilRcx(End), waitRcxByte(Ascii, End)) ; 
        getRcxByte(Ascii).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HIGH LEVEL COMMUNICATION 
%   The high level primitives will try repeatedly to send a message or to 
%   receive a message in small chunks of time, up until a given End time.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sendRcxMessage(+MesgNo, +End, -Reply): Repeatedly send message MesgNo
%     to RCX until reply received from RCX. Fails if there is no reply
%     before End.

sendRcxMessage(Msg, End, Reply) :-
    singleGetTime(End, End2),
    putRcxMess(Msg),
    (getRcxMess(Reply, End2) -> true ; sendRcxMessageAgain(Msg, End, Reply)).

sendRcxMessageAgain(Msg, End, Reply) :-	% Here if getRcxMess above failed
    currentTime(Now),
    rcxResponseDelay(Delay),
    End > Now+Delay,  % Continue if there is still time
    sendRcxMessage(Msg, End, Reply).

% singleGetTime(+End, -End2): End2 is maximum time between Now and End
%     for a single getRcxMess. It must not exceed the time the infrared
%     pyramid is active
singleGetTime(End, End2) :-
    currentTime(Now),
    rcxPyramidDelay(Delay), 
    (Delay < End-Now ->
        End2 is Now + Delay;
        End2 is End).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In the actual communication to/from the RCX, to send or receive action N,
% we need to break N down into a sequence of messages N1, N2, ..., Nk.
% Similarly, to receive a sensing value N, we receive a sequence of messages.
% Action and sensing values are sent using different ranges.
% The predicate rcxMessageRange is used to control what the Ni should be.
% In between each message Ni, a message of rcxContinueMessage synchronizes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% decodeNumber(+N, +NumberType, -Nums): Decodes number N of type NumberType
%     into a list for sending to RCX
decodeNumber(N, NumberType, [First|Rest]) :-
    rcxMessageRange(NumberType, Low, _, Base),
    (N < Base -> (First is N+Low, Rest = []) ;
        (First is mod(N,Base) + Low + Base, 
         N1 is N // Base,
         decodeNumber(N1, NumberType, Rest))).

% getEncodedNumber(+First, +NumberType, +End, -N): Interprets First as the
%     first part of an encoding of NumberType and goes on to receive
%     messages from RCX as necessary to encode number N (a sensing value).
%     Sends rcxContinueMessages as needed. Fails if End time has passed
getEncodedNumber(First, NumberType, End, N) :-
    rcxDelayMessage(First), !,    % Received a request from RCX for more
                                  %   time to execute primitive action
    rcxContinueMessage(Num),      % Grant this by returning rcxContinueMessage
    sendRcxMessage(Num, End, Ans),
    getEncodedNumber(Ans, NumberType, End, N).
   

getEncodedNumber(First, NumberType, End, N) :-
    rcxMessageRange(NumberType, Low, High, Base),
    rcxContinueMessage(Ack),
    First >= Low, First =< High,          % Fail if not in Low-High range
    M is First - Low,                     % Assign M in 0--(Base-1) range
    (M < Base -> N is M ;                 % Return M if only one message needed
        (sendRcxMessage(Ack, End, Next),  % Else get next number
         getEncodedNumber(Next, NumberType, End, Temp),
         N is (M - Base) + Base * Temp)). % Decode final number

% sendRcxNumbers(+Nums, +ResultType, +End, -Result): Sends the numbers in
%     the list Nums to the RCX as a sequence of messages, ensuring it gets
%     continue messages between each one.  Fails if End time has passed.
%     After the last number, the message received is interpreted as the
%     number Result of ResultType (action or value), using getEncodedNumber.
sendRcxNumbers([First|Rest], ResultType, End, Result) :-
    sendRcxMessage(First, End, Answer),
    (Rest == [] -> 
        getEncodedNumber(Answer, ResultType, End, Result) ;
        (rcxContinueMessage(Answer), 
         sendRcxNumbers(Rest, ResultType, End, Result) )).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This handles communication between RCX and Prolog required by IndiGolog.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% sendRcxActionNumber(+Num, -Result): Send action number N and obtain
%     sensing value Result. Fails if RCX does not return Result
sendRcxActionNumber(Num, Result) :-
    rcxLongDelay(C),
    currentTime(Now),
    End is Now + C,
    decodeNumber(Num, action, Nums),          % Translate action number N
    sendRcxNumbers(Nums, value, End, Result). % Send Nums and obtain sensing
                                              %  value Result

receiveRcxActionNumber(Actions) :-
    currentTime(Now),
    rcxResponseDelay(Delay1),
    End1 is Now + Delay1,
    rcxExogReqMessage(Msg),
    rcxLongDelay(Delay2),
    End2 is Now+Delay2,
    sendRcxMessage(Msg, End1, Ans),           % Query RCX for exogenous actions
    ((rcxNoExogMessage(Ans), Actions = []) ;  % RCX reports none
     (getEncodedNumber(Ans, action, End2, N), 
                             Actions = [N])). % RCX returns action

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Env/legorcx.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





