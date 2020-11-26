/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1985-2011, University of Amsterdam
                              VU University Amsterdam
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

:- module(pce_manual,
          [ manpce/0,
            manpce/1
          ]).
:- use_module(library(pce)).
:- consult([ man/util                   % Common utilities
           , man/p_card                 % General card infra-structure
           , man/p_data                 % Manual specific infra-structure
           , man/v_manual               % Top level window
           ]).
:- require([ pce_warn/1
           , pce_to_method/2
           ]).

/** <module> Start XPCE manual
*/

:- pce_autoload(man_class_browser,      library('man/v_class')).
:- pce_autoload(man_editor,             library('man/v_editor')).
:- pce_autoload(man_card_editor,        library('man/v_card')).
:- pce_autoload(man_summary_browser,    library('man/v_summary')).
:- pce_autoload(man_class_hierarchy,    library('man/v_hierarchy')).
:- pce_autoload(man_search_tool,        library('man/v_search')).
:- pce_autoload(man_index_manager,      library('man/man_index')).
:- pce_autoload(man_topic_browser,      library('man/v_topic')).
:- pce_autoload(man_module_browser,     library('man/v_module')).
:- pce_autoload(man_statistics,         library('man/v_statistics')).
:- pce_autoload(isp_frame,              library('man/v_inspector')).
:- pce_autoload(vis_frame,              library('man/v_visual')).
:- pce_autoload(man_instance_browser,   library('man/v_instance')).
:- pce_autoload(man_global,             library('man/v_global')).
:- pce_autoload(man_object_browser,     library('man/v_global')).
:- pce_autoload(man_error_browser,      library('man/v_error')).
:- pce_autoload(man_group_browser,      library('man/v_group')).

:- pce_global(@manual, new(man_manual)).

%!  manpce is det.
%
%   Starts the XPCE manual tools by opening a small window.

manpce :-
    in_pce_thread(send(@manual, expose)).


%!  manpce(+Spec) is det.
%
%   Start the XPCE manual tools, opening   the manual page for Spec.
%   Spec is translated into an   XPCE  object using pce_to_method/2.
%   Examples:
%
%     ==
%     ?- manpce(window).
%     ?- manpce(point->x).
%     ==

manpce(Spec) :-
    in_pce_thread(manpce_(Spec)).

manpce_(Spec) :-
    (   method(Spec, Object)
    ->  send(@manual, manual, Object)
    ;   pce_warn(pce(no_help(Spec))),
        fail
    ).

method(Spec, Method) :-
    object(Spec),
    send(Spec, '_instance_of', var),
    !,
    Spec = @Ref,
    new(Method, man_global(Ref)).
method(Spec, Method) :-
    pce_to_method(Spec, Method),
    !.
method(Atom, Method) :-
    atom(Atom),
    catch(term_to_atom(Spec, Atom), _, fail),
    pce_to_method(Spec, Method).



