%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE      : Env/lego_ecl.pl
%  Time-stamp: <02/11/30 01:33:51 ssardina>
%
%  Author      : First, by Maurice Pagnucco and Hector J. Levesque
%                Then, by Sebastian Sardina
%  DESCRIPTION : Prolog code to establish communication with 
%                LEGO Mindstorms RCX. This file contains the basic interface
%                to open, close, read and write for ECLIPSE Prolog.
%  email       : ssardina@cs.toronto.edu
%  WWW         : www.cs.toronto.edu/~ssardina
%  TYPE CODE   : system dependent predicates
%  TESTED      : ECLiPSe 5.5 on RedHat Linux 6.2-8.0
%
% It is written for ECLiPSe Prolog (http://www.icparc.ic.ac.uk/eclipse/)
% running under Linux
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
% The following system dependent predicates are provided in this file:
%
% -- initRcx           : initialize serial port, baud, parity etc. for RCX
% -- openRcxRead       : open RCX serial port for reading
% -- openRcxWrite      : open RCX serial port for writing
% -- closeRcx          : close RCX serial port
% -- getRcxByte(-Ascii): get a character from RCX
% -- eofRcx            : succeed if there is a character from RCX to read
% -- putRcxByte(+Ascii): write a character to RCX
% -- currentTime(-Time): current system time measured in seconds
% -- waitUntilRcx(+End): optionally wait (busy or suspend) until End
%                        system time or until RCX input arrives. 
%                        Fails if End is passed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rcxStream(-Rcx): Saves the file descriptor currently referring to the open
%     serial port Rcx.
:- dynamic(rcxStream/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For ECLiPSe Prolog you will need the following version of get/2
% to work around the problem that it converts ASCII 13 (carriage return) to
% ASCII 10 (newline).
% Thanks to Joachim Schimpf (J.Schimpf@icparc.ic.ac.uk) from IC PARC for fix
% Apparently this problem will be fixed in the next release (currently 4.2.2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fixed_get(+Stream, -Ascii): Obtain next Ascii character from Stream
%fixed_get(Stream, Ascii) :-
%    get_stream_info(Stream, line, L0),
%    get(Stream, X),
%    (X \== 10 ->
%        Ascii = X;
%        get_stream_info(Stream, line, L1),
%        (L0 == L1 -> Ascii = 13 ; Ascii = X)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Using default Linux serial port /dev/ttyS0
% If you change this, make sure to alter it below in initRcx/0 as well

% rcxPort(+Port): Location of serial port to which infrared tower is attached
%rcxPort('/dev/cua0').
rcxPort('/dev/ttyS0').

% immediateDelay(+Delay): Amount of time Delay (in seconds) to wait in between
%     fetching bytes Linux appears to be much faster than the RCX and this
%     delay is used to avoid "splitting" message packets
immediateDelay(0.03).

% currentTime(-Time): Returns the current system Time in hundredths of seconds
currentTime(Time) :-
    statistics(session_time, T),
    Time is fix(100 * T).

% initRcx: Uses stty to initialise the serial port, setting baud rate, etc.
%     Make sure that the serial port is set correctly. We use /dev/ttyS0 here.
initRcx :-
    system('stty -echo -icanon -iexten -isig -icrnl -inpck -istrip -ixon -cstopb cs8 parenb parodd -opost ispeed 2400 ospeed 2400 < /dev/ttyS0').

   
% openRcxRead: Opens infrared tower (connected to serial port) for reading
%     data from RCX
openRcxRead:-
    rcxPort(Port),
    open(Port, read, Rcx), % Open serial port for reading and writing
    retract_all(rcxStream(_)),
    assert(rcxStream(Rcx)).

% openRcxWrite: Opens infrared tower (connected to serial port) for sending
%     data to RCX
openRcxWrite:-
    rcxPort(Port),
    open(Port, write, Rcx), % Open serial port for reading and writing
    retract_all(rcxStream(_)),
    assert(rcxStream(Rcx)).


% closeRcx: Closes infrared tower (connected to serial port) if it is
%     currently open. This predicate always succeeds
closeRcx:-
    (rcxStream(Rcx) ->
        (close(Rcx), retract_all(rcxStream(Rcx))); true).

% eofRcx: Checks whether there is any input data waiting on infrared tower
eofRcx :-
    rcxStream(Rcx),
    select([Rcx], 0, ReadyList),
    ReadyList == [].

% waitUntilRcx(+End): Optionally wait (busy or suspend) until End system time
%     or until RCX input arrives. Fail if End passed
waitUntilRcx(End) :-
    currentTime(Now),
    TimeOut is (End - Now)/100,
    TimeOut > 0,
    rcxStream(Rcx),
    select([Rcx], TimeOut, _).

% getRcxByte(-Ascii): Returns the first Ascii character read from infrared
%     tower. Blocks if there is no input
getRcxByte(Ascii) :-
    rcxStream(Rcx),
    immediateDelay(Delay),
    sleep(Delay),
    get(Rcx, Ascii).

% putRcxByte(+Ascii): Sends Ascii character to Rcx using infrared tower
putRcxByte(Ascii) :-
    rcxStream(Rcx),
    put(Rcx, Ascii),
    flush(Rcx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Env/lego_ecl.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
