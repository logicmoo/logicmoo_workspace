%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Env/lego_swi.pl
%
%    WRITTEN BY: First, by Maurice Pagnucco and Hector J. Levesque
%                Then, by Sebastian Sardina (ssardina@cs.toronto.edu)
%    Time-stamp: <02/11/27 17:32:28 ssardina>
%    TESTED    : SWI Prolog 4.0.5 under RedHat Linux 6.2/7.2
%    TYPE CODE : system dependent predicates
%
% DESCRIPTION: Prolog code to establish communication with 
%              LEGO Mindstorms RCX. This file contains the basic interface
%              to open, close, read and write for SWI Prolog.
%
% It is written for SWI Prolog (http://www.swi-prolog.org/) running under Linux
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

% rcxStream(+Rcx): Saves the file descriptor currently referring to the open
%     serial port Rcx
:- dynamic(rcxStream/1).

% Using default Linux serial port /dev/ttyS0
% If you change this, make sure to alter it below in initRcx/0 as well

% rcxPort(+Port): Location of serial port to which infrared tower is attached
rcxPort('/dev/ttyS0').

% immediateDelay(+Delay): Amount of time Delay (in seconds) to wait in between
%     fetching bytes Linux appears to be much faster than the RCX and this
%     delay is used to avoid "splitting" message packets
immediateDelay(0.01).

% currentTime(-Time): Returns the current system Time in hundredths of seconds
currentTime(Time) :-
    get_time(T),
    convert_time(T,_,_,_,H,Min,S,Mil),
    Time is Mil//10+S*100+Min*60000+H*24*60000.

% initRcx: Uses stty to initialise the serial port, setting baud rate, etc.
%     Make sure that the serial port is set correctly. We use /dev/ttyS0 here
initRcx :-
    shell('stty -echo -icanon -iexten -isig -icrnl -inpck -istrip -ixon -cstopb cs8 parenb parodd -opost ispeed 2400 ospeed 2400 < /dev/ttyS0').

% openRcxRead: Opens infrared tower (connected to serial port) for reading
%     data from RCX
openRcxRead :-
    rcxPort(Port),
    open(Port, read, Rcx, [eof_action(eof_code), close_on_abort(false)]),
    retractall(rcxStream(_)),
    assert(rcxStream(Rcx)).

% openRcxWrite: Opens infrared tower (connected to serial port) for sending
%     data to RCX
openRcxWrite :-
    rcxPort(Port),
    open(Port, write, Rcx, [close_on_abort(false), buffer(false)]),
    retractall(rcxStream(_)),
    assert(rcxStream(Rcx)).

% closeRcx: Closes infrared tower (connected to serial port) if it is
%     currently open. This predicate always succeeds
closeRcx :-
    (rcxStream(Rcx) ->
        (close(Rcx), retractall(rcxStream(Rcx))); true).

% eofRcx: Checks whether there is any input data waiting on infrared tower
eofRcx :-
    rcxStream(Rcx),
    immediateDelay(Delay),
    wait_for_input([Rcx], ReadyList, Delay),
    ReadyList == [].

% waitUntilRcx(+End): Optionally wait (busy or suspend) until End system time
%     or until RCX input arrives. Fail if End passed
waitUntilRcx(End) :-
    currentTime(Now),
    TimeOut is (End - Now)/100,
    TimeOut > 0,
    rcxStream(Rcx),
    wait_for_input([Rcx], _, TimeOut).

% getRcxByte(-Ascii): Returns the first Ascii character read from infrared
%     tower. Blocks if there is no input
getRcxByte(Ascii) :-
    rcxStream(Rcx), 
    get0(Rcx, Ascii).

% putRcxByte(+Ascii): Sends Ascii character to Rcx using infrared tower
putRcxByte(Ascii) :-
    rcxStream(Rcx),
    put(Rcx, Ascii), flush_output(Rcx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Env/lego_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
