:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_uitl,[]).
:- endif.
% ===============================================
% Utility functions
% ===============================================
% convert snowflake timestamps
id_to_time(ID,UTC):- integer(ID), UTC is (( ID >> 22) / 1000) + 1420070400.

%int_to_date(S,V):- stamp_date_time(S,Date,local),!, format_time(string(V),'%a, %d %b %Y %T PST',Date,posix).
int_to_date(S,V):- stamp_date_time(S,Date,local),!, format_time(string(V),':%Y:%b:%d:%T',Date,posix).

test_string_to_dict:-
 string_to_dict("{\"type\":\"dnd_updated_user\",\"user\":\"U3T3R279S\",\"dnd_status\":{\"dnd_enabled\":false,\"next_dnd_start_ts\":1,\"next_dnd_end_ts\":1},\"event_ts\":\"1485012634.280271\"}",Dict),
  ddbg_always(Dict).


string_to_dict(Text,Dict):-
   text_to_string(Text,String),
   setup_call_cleanup(open_string(String,Stream),
    notrace_catch(json_read_dict(Stream,Dict)),
    close(Stream)),!.
string_to_dict(Text,Dict):- atom_json_dict(Text,Dict,[value_string_as(string),null(null),true(true),false(false)]).


from_string(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(from_string,As,AAs),!,compound_name_arguments(V,F,AAs).
from_string(S,V):- \+ string(S), \+ atom(S), !, V=S.
from_string(S,V):- atom_length(S,L),L>40,!,S=V.
from_string(S,V):- \+ atom(S), text_to_string(S,SS),string_to_atom(SS,A),notrace_catch(atom_number(A,V)),!.
from_string(S,V):- parse_time(S,_,V),!.
from_string(V,V).

maybe_into_string(Name,NameS):- nonvar(Name), \+ string(Name), notrace_catch(text_to_string(Name,NameS)), !.

add_to_dict(Dict,[],Dict):-!.
add_to_dict(Dict,[Add|Mentions],NewDict):-
  add_to_dict(Dict,Add,MDict),
  add_to_dict(MDict,Mentions,NewDict),!.
add_to_dict(Dict,ADict,NewDict):- is_dict(ADict),dict_pairs(ADict,_,List),!,add_to_dict(Dict,List,NewDict).
add_to_dict(Dict,KV,NewDict):- get_kvd(KV,K,V), get_dict(K,Dict,Old),is_dict(Old),add_to_dict(Old,V,New),
  put_dict(K,Dict,New,NewDict).
add_to_dict(Dict,KV,NewDict):- get_kvd(KV,K,V),put_dict(K,Dict,V,NewDict).

json_to_string(JSON,Str):- wots(Str,json_write(current_output,JSON,[])).

atom_or_string(S):- atom(S);string(S).

:- thread_local(t_l:hide_ddbg/0).

:- meta_predicate(notrace_catch(:)).
notrace_catch(G):- notrace(catch(G,_,fail)).
debug_console(G):- (tmp:discord_debug(X);X=user_error),!, with_output_to(X,G).

:- volatile(tmp:discord_debug/1).
:-  dynamic(tmp:discord_debug/1).
if_debug(_).


:- meta_predicate(without_ddbg(:)).
without_ddbg(G):- locally(t_l:hide_ddbg,call(G)).

discord_debug_cvt(T,T):- var(T),!.
discord_debug_cvt(tmp:T,S):- nonvar(T), !,discord_debug_cvt(T,S).
discord_debug_cvt(discord_info(A,B,C),discord_info(S,id,C)):- B==id, !, discord_debug_cvt(A,S).
discord_debug_cvt(T,S):- compound(T), compound_name_arguments(T,F,As),maplist(discord_debug_cvt,As,AAs),!,compound_name_arguments(S,F,AAs).
discord_debug_cvt(T,S):- number(T), !, int_to_name(T,S)-> true ; T=S.
discord_debug_cvt(T,S):- (atom(T);string(T)),discord_is_secret(T),!,(discord_ddd(V,hasValue,T)-> S= '$'(V) ; S="nZC-SECRET").
%discord_debug_cvt(T,S):- string(T), notrace_catch(atom_number(T,N)), !, discord_debug_cvt(N,S).
discord_debug_cvt(S,S).

%ddbg(O):- sub_var("PRESENCE_UPDATE",O),!.
%ddbg(_):- \+ thread_self(main),!.
ddbg(_):- t_l:hide_ddbg,!.
ddbg(discord_addd(s,hasValue,_)).
ddbg(O):- ddbg_always(O).
%ddbg_always(O):- discord_debug_cvt(O,OO), !, debug_console(in_cmt(fmt(OO))).
ddbg_always(O):- discord_debug_cvt(O,OO),!, ddbg_raw(OO).
ddbg_raw(OO):-
 locally(current_prolog_flag(debugger_write_options,[quoted(true), portray(true), 
   max_depth(500), attributes(portray), spacing(next_argument)]),
   debug_console(in_cmt(print_tree(OO)))).
%ddbg_always(F,Args):- if_debug((fresh_line, format(user_error,F,Args))).


dict_append_curls(Dict,Params,NewDict):-any_to_curls(Params,Curly), dict_append_curls3(Dict,Curly,NewDict).
dict_append_curls3(Dict,{},Dict):-!.
dict_append_curls3(Dict,{Curly},NewDict):-!,dict_append_curls3(Dict,Curly,NewDict).
dict_append_curls3(Dict,(A,B),NewDict):-!,dict_append_curls3(Dict,A,NewDictM),dict_append_curls3(NewDictM,B,NewDict).
dict_append_curls3(Dict,KS:V,NewDict):- string_to_atom(KS,K), put_dict(K,Dict,V,NewDict).


any_to_json_dict(D,D):- is_dict(D,_),!. 
any_to_json_dict(List,Dict):- is_list(List),dict_create(Dict,_,List),!.

any_to_json_dict({C},D):-  notrace_catch((sformat(S,'~q',[{C}]),atom_json_dict(S,D,[as(string)]))),!.
any_to_json_dict({C},D):- !, conjuncts_to_list(C,L),maplist(any_to_json_dict_arg,L,L2),dict_create(D,_,L2),!.

any_to_json_dict(S,V):- compound(S), compound_name_arguments(S,F,As),maplist(any_to_json_dict,As,AAs),!,compound_name_arguments(V,F,AAs).
any_to_json_dict(String,D):- notrace_catch(atom_json_dict(String,D,[as(string)])),!.
%any_to_json_dict(S,V):- number(S), int_to_name(S,V),!.
any_to_json_dict(V,V).

any_to_json_dict_arg(S:V,SS-VV):- any_to_atom(S,SS),any_to_json_dict(V,VV).
%dict_to_curly(Dict,{type:Type,Data}):- del_dict(type,Dict,Type,DictOut),dict_pairs(DictOut,_,Pairs),any_to_curls(Pairs,Data).
%dict_to_curly(Dict,{type:Type,Data}):- dict_pairs(Dict,Type,Pairs),nonvar(Type),any_to_curls(Pairs,Data).
dict_to_curly(Dict,{Data}):- dict_pairs(Dict,_,Pairs),list_to_curls(Pairs,Data).

list_to_curls([A],AA):-!,any_to_curls(A,AA).
list_to_curls([A|B],(AA,BB)):-!,any_to_curls(A,AA),list_to_curls(B,BB).

any_to_curls(Dict,Data):- is_dict(Dict),!,dict_to_curly(Dict,Data).
any_to_curls(Var,"var"):- \+ must(\+ var(Var)),!.
any_to_curls(null,null).
any_to_curls(true,true).
any_to_curls(false,false).
any_to_curls(KV,AA:BB):-get_kvd(KV,A,B),!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls('$'(Var),ValO):- discord_dd(Var,Val),!,any_to_curls(Val,ValO).
any_to_curls({DataI},{Data}):-!,any_to_curls(DataI,Data).
any_to_curls((A,B),(AA,BB)):-!,any_to_curls(A,AA),any_to_curls(B,BB).
any_to_curls(A,O):- string(A),!,from_string(A,O).
any_to_curls(A,O):- maybe_into_string(A,AA),!,any_to_curls(AA,O).
any_to_curls(A,O):- from_string(A,O).


string_appended(Appended,STRING_CREATE,STRINGS):- 
  string(STRING_CREATE),atom_concat(STRING,Appended,STRING_CREATE),
  downcase_atom(STRING,STRINGD),atom_concat(STRINGD,'s',STRINGS),!.

get_prop(O,_,_):- \+ compound(O),!,fail.
% _{k:v,...}
get_prop(O,K,V):- is_dict(O),!,O.Key=V,atom_string(K,Key).
% [k:v,...]
get_prop(O,K,V):- is_list(O),!,member(KV,O),get_kvd(KV,K,V).
% json([k-v,...])
get_prop(json(O),K,V):- !, member(KV,O),get_kvd(KV,K,V).
% {k:v,...}
get_prop({(KV,Rest)},K,V):- !, (get_kvd(KV,K,V); get_prop({Rest},K,V)).
% {k:v}
get_prop({KV},K,V):- !, get_kvd(KV,K,V).
get_prop(KV,K,V):-  get_kvd(KV,K,V).

get_kvd(KV,K,V):- \+ compound(KV),!,fail,throw(var_get_kvd(KV,K,V)).
get_kvd(K:V,N,V):- !, atom_string(N,K).
get_kvd(K-V,N,V):- !, atom_string(N,K).
get_kvd(K=V,N,V):- !, atom_string(N,K).
%get_kvd(KV,N,V):- compound_name_arguments(KV,_,[K,V]),atom_string(N,K).

same_strings(K,N):- K=N,!.
same_strings(K,N):- atom_string(K,KS),atom_string(N,NS),KS=NS.

toplevel_prop :- \+ t_l:prop_prepend(_).

% curl -H "Authorization: Bot $AUTH_TOKEN" -H "User-Agent: DiscordBot" https://discord.com/api/channels/892809238710222930/messages

from_dict(Dict,Var):- var(Var),!,Dict=Var.
from_dict(Dict,List):- is_list(List),!,maplist(from_dict(Dict),List).
from_dict(Dict,json(KV)):- !,from_dict(Dict,KV).
from_dict(Dict,{KV,List}):- !,from_dict(Dict,KV),from_dict(Dict,{List}).
from_dict(Dict,{KV}):- !,from_dict(Dict,KV).
from_dict(Dict,KV):- get_kvd(KV,K,V),get_prop(Dict,K,DV),from_dict(DV,V).


