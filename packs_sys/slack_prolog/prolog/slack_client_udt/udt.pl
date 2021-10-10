/*  Part of SWI-Prolog
    Author:        Douglas R. Miles, Jan Wielemaker
    E-mail:        logicmoo@gmail.com, jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org http://www.prologmoo.com
    Copyright (C): 2015, University of Amsterdam
                                    VU University Amsterdam
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(udt, [oo/1,oo/2,is_oo/1,oo_call/3,jpl_call/3,oo_deref/2]).

% :- use_module(library(jpl),[jpl_set/3,jpl_get/3,jpl_call/4]).

:- use_module(atts).
:- use_module(multivar).

oo(O):-multivar(O).
oo(O,Value):-multivar(O),put_attr(O,oo,binding(O,Value)).
oo:attr_unify_hook(B,Value):- B = binding(_Var,Prev),Prev.equals(Value).


oo_set(UDT,Key, Value):- attvar(UDT),!,put_attr(UDT,Key, Value).
oo_set(UDT,Key, Value):- jpl_set(UDT,Key,Value).



put_oo(Key, UDT, Value, NewUDT):- is_dict(UDT),!,put_dict(Key, UDT, Value, NewUDT).
put_oo(Key, UDT, Value, NewUDT):- oo_copy_term(UDT,NewUDT),put_oo(NewUDT,Key, Value).


oo_copy_term(UDT,NewUDT):- copy_term(UDT,NewUDT).

put_oo(Key, UDT, Value):- is_dict(UDT),!,put_dict(Key, UDT, Value).
put_oo(Key, UDT, Value):- oo_set(UDT,Key, Value).


get_oo(Key, UDT, Value):- oo_call(UDT,Key, Value).


:- meta_predicate(fail_on_missing(0)).
fail_on_missing(G):-catch(G,error(existence_error(_,_),_),fail).

jpl_call(A,B,C):- B=..[H|L], fail_on_missing(jpl_call(A,H,L,C)),!.
jpl_call(A,B,C):- jpl_get(A,B,C).



is_oo(O):- (attvar(O);is_dict(O);jpl_is_ref(O)),!.
%SWICLI is_oo(O):- once(cli_is_object(O);cli_is_struct(O)).

%oo_call(Self,[Memb1|Memb2],Value):- !,oo_call(Self,Memb1,Value1),oo_call(Value1,Memb2,Value).
oo_call(Self,Memb,Value):- is_dict(Self) ,!, '$dict_dot3'(Self, Memb, Value).
oo_call(Self,Memb,Value):- attvar(Self),!,oo_call_av(Self,Memb,Value).
oo_call(Self,Memb,Value):- compound(Self),!,oo_call_cmp(Self,Memb,Value).
oo_call(Self,Memb,Value):- oo_deref(Self,NewSelf)->NewSelf\==Self,!,oo_call(NewSelf,Memb,Value).


oo_call(Self,Memb,Value):- nb_linkval(Self,construct(Self,Memb,Value)),!,oo_call(Self,Memb,Value).
oo_call(Self,Memb,Value):- to_member_path(Memb,[F|Path]),append(Path,[Value],PathWValue),
   Call =.. [F,Self|PathWValue],
   oo_call(Call).

to_member_path(C,[F|ARGS]):-compound(C),!,compound_name_args(C,F,ARGS).
to_member_path(C,[C]).


oo_call_av(Self,Memb,Value):- get_attr(Self, Memb, Value),!.
oo_call_av(Self,Memb,Value):- get_attr(Self, oo, NewSelf),!,oo_call(NewSelf,Memb,Value).

oo_call_cmp(Self,Memb,Value):- jpl_is_ref(Self),!, jpl_call(Self, Memb, Value).
%SWICLI oo_call_cmp(Self,Memb,Value):-  cli_is_object(Self),!,cli_call(Self, Memb, Value).
%SWICLI oo_call_cmp(Self,Memb,Value):-  cli_is_struct(Self),!,cli_call(Self, Memb, Value).
oo_call_cmp(Self,Memb,Value):-  oo_deref(Self,NewSelf),!, NewSelf\=Self, oo_call(NewSelf,Memb,Value).


oo_deref(Obj,RObj):- var(Obj),!,once(get_attr(Obj,oo,binding(_,RObj));Obj=RObj),!.
oo_deref(GVar,Value):- atom(GVar),nb_current(GVar,ValueM),!,oo_deref(ValueM,Value).
oo_deref(Value,Value):- \+ compound(Value),!.
oo_deref(cl_eval(Call),Result):-is_list(Call),!,cl_eval(Call,Result).
oo_deref(cl_eval(Call),Result):-!,nonvar(Call),oo_deref(Call,CallE),!,call(CallE,Result).
oo_deref(Value,Value):- jpl_is_ref(Value),!.
%%oo_deref([A|B],Result):-!, maplist(oo_deref,[A|B],Result).
%%oo_deref(Call,Result):- call(Call,Result),!.
oo_deref(Head,HeadE):- Head=..B,maplist(oo_deref,B,A),HeadE=..A,!.
oo_deref(Value,Value).


:- if(clause('$dicts':'.'(_,_,_),_)).

:- clause('$dicts':'.'(Data, Func, Value),BODY),
   asserta(('$dict_dot3'(Data, Func, Value):- '$dict':BODY)).


:- else.

'$dict_dot3'(Data, Func, Value) :-
    (   '$get_dict_ex'(Func, Data, V0)
    *-> Value = V0
    ;   is_dict(Data, Tag)
    ->  '$dicts':eval_dict_function(Func, Tag, Data, Value)
    ;   is_list(Data)
    ->  (   (atomic(Func) ; var(Func))
        ->  dict_create(Dict, _, Data),
            '$get_dict_ex'(Func, Dict, Value)
        ;   '$type_error'(atom, Func)
        )
    ;   '$type_error'(dict, Data)
    ).

:- endif.


:-redefine_system_predicate('system':'.'(_Data, _Func, _Value)).
:-'system':abolish('$dicts':'.'/3).

'system':'.'(Data, Func, Value) :- oo_call(Data,Func,Value).
% system:'.'(Data, Func):- oo_call(Data,Func,_).





get_oo(Key, Dict, Value, NewDict, NewDict) :- is_dict(Dict),!,
   get_dict(Key, Dict, Value, NewDict, NewDict).
get_oo(Key, Dict, Value, NewDict, NewDict) :-
        get_oo(Key, Dict, Value),
        put_oo(Key, Dict, NewDict, NewDict).






%!  eval_oo_function(+Func, +Tag, +UDT, -Value)
%
%   Test for predefined functions on Objects or evaluate a user-defined
%   function.

eval_oo_function(Func, Tag, UDT, Value) :- is_dict(Tag),!,
   '$dicts':eval_dict_function(Func, Tag, UDT, Value).

eval_oo_function(get(Key), _, UDT, Value) :-
    !,
    get_oo(Key, UDT, Value).
eval_oo_function(put(Key, Value), _, UDT, NewUDT) :-
    !,
    (   atomic(Key)
    ->  put_oo(Key, UDT, Value, NewUDT)
    ;   put_oo_path(Key, UDT, Value, NewUDT)
    ).
eval_oo_function(put(New), _, UDT, NewUDT) :-
    !,
    put_oo(New, UDT, NewUDT).
eval_oo_function(Func, Tag, UDT, Value) :-
    call(Tag:Func, UDT, Value).


%!  put_oo_path(+KeyPath, +UDT, +Value, -NewUDT)
%
%   Add/replace  a  value  according  to  a  path  definition.  Path
%   segments are separated using '/'.

put_oo_path(Key, UDT, Value, NewUDT) :-
    atom(Key),
    !,
    put_oo(Key, UDT, Value, NewUDT).
put_oo_path(Path, UDT, Value, NewUDT) :-
    get_oo_path(Path, UDT, _Old, NewUDT, Value).

get_oo_path(Path, _, _, _, _) :-
    var(Path),
    !,
    '$instantiation_error'(Path).
get_oo_path(Path/Key, UDT, Old, NewUDT, New) :-
    !,
    get_oo_path(Path, UDT, OldD, NewUDT, NewD),
    (   get_oo(Key, OldD, Old, NewD, New),
        is_oo(Old)
    ->  true
    ;   Old = _{},
        put_oo(Key, OldD, New, NewD)
    ).
get_oo_path(Key, UDT, Old, NewUDT, New) :-
    get_oo(Key, UDT, Old, NewUDT, New),
    is_oo(Old),
    !.
get_oo_path(Key, UDT, _{}, NewUDT, New) :-
    put_oo(Key, UDT, New, NewUDT).


oo_class(Name):-asserta(is_oo_class_impl(Name)).

oo_class_end:-retract(is_oo_class_impl(Name)),assertz(is_oo_class_impl(Name)).


oo_inner_class(Name,Inner):-asserta(is_oo_inner_class(Name,Inner)).
oo_inner_class(Inner):-is_oo_class_impl(Name),oo_inner_class(Name,Inner).
oo_inner_class_end:-retract(is_oo_inner_class(Name,Inner)),assertz(is_oo_inner_class(Name,Inner)).


