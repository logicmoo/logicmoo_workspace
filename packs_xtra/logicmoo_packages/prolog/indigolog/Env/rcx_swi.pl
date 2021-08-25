%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE      : Env/rcx_swi.pl
%  Time-stamp: <02/11/30 01:25:17 ssardina>
%
%  Author      : Sebastian Sardina
%  DESCRIPTION : Interface to communicate with the Lego RCX via SWI
%  email       : ssardina@cs.toronto.edu
%  WWW         : www.cs.toronto.edu/~ssardina
%  TYPE CODE   : system dependent predicates
%  TESTED      : SWI Prolog 5.0.10 under RedHat Linux 6.2-8.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             November 22, 2002
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
% The interface to Lego RCX is divided into two parts/files:
%
%   (a) a system independent part/file dealing with the protocol (lego_rcx.pl)
%   (b) a system dependent part/file dealing with the actual low level comm
%
% This file defines the following interface:
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(rcx_swi,
          [initializeRcx/0,
           finalizeRcx/0,
           sendRcxActionNumber/2,
           receiveRcxActionNumber/1]).

:- include(legorcx).  % System independent part
:- include(lego_swi). % System dependent part for SWI


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Env/rcx_swi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
