/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(area,
        [ side_pattern/3        % Side x Side x Pattern
        ]).

%%   side_pattern(+SideA, +SideB, -Pattern)
%
%   Pattern is the bit if SideA on area A corresponds to SideB on area B.

side_pattern(top,    top,    2'1).
side_pattern(top,    center, 2'10).
side_pattern(top,    bottom, 2'100).
side_pattern(center, top,    2'1000).
side_pattern(center, center, 2'10000).
side_pattern(center, bottom, 2'100000).
side_pattern(bottom, top,    2'1000000).
side_pattern(bottom, center, 2'10000000).
side_pattern(bottom, bottom, 2'100000000).
side_pattern(left,   left,   2'1000000000).
side_pattern(left,   middle, 2'10000000000).
side_pattern(left,   right,  2'100000000000).
side_pattern(middle, left,   2'1000000000000).
side_pattern(middle, middle, 2'10000000000000).
side_pattern(middle, right,  2'100000000000000).
side_pattern(right,  left,   2'1000000000000000).
side_pattern(right,  middle, 2'10000000000000000).
side_pattern(right,  right,  2'100000000000000000).
