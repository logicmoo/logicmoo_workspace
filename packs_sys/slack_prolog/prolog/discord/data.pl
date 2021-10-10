:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_data,[]).
:- endif.

:- multifile(tmp:discord_info/3).
:- volatile(tmp:discord_info/3).
:- dynamic(tmp:discord_info/3).


% ===========================
% Data Access
% ===========================

discord_dd(ID,Data):- discord_ddd(ID,hasValue,Data).
discord_dd(ID,Prop,Value):- discord_ddd(ID,Prop,Value)*->true;get_discord2(ID,Prop,Value).
get_discord2(Type,Prop,Value):- discord_ddd(Type,hasValue,ID),discord_ddd(ID,Prop,Value).
get_discord_info(ID,Prop,Data):- discord_ddd(ID,Prop,Data)*-> true;
  (\+ integer(ID), \+ var(ID), any_to_id(ID,ID2),!, discord_ddd(ID2,Prop,Data)).

discord_ddd(guilds, id, X):- default_guild(X).
discord_ddd(A,B,C):- tmp:discord_info(A,B,C).
discord_ddd(A,B,C):- integer(A), B == timestamp, !, id_to_time(A,C).

% ===========================
% Persistence
% ===========================
reload_discord_info:- reconsult(guild_info_file).

%load_discord_info:- !.
load_discord_info:- exists_file(guild_info_file)->consult(guild_info_file);true.

clear_discord_info:- retractall(tmp:discord_info/3).

save_discord_info:- 
 setup_call_cleanup(open(guild_info_file,write,O),
  save_discord_info(O),
  close(O)).

tmp_r(R,TmpR,TmpRG):- functor(R,discord_info,3), TmpR=tmp:R, TmpRG = (TmpR, \+ discord_is_secret(R)).

save_discord_info(O):-
  tmp_r(R,TmpR,TmpRG),
  functor(R,F,A), Dyn = tmp:F/A, 
  format(O,'~n~q.~n~q.~n~q.~n',
   [(:- multifile(Dyn)),
    (:- volatile(Dyn)),
    (:- dynamic(Dyn))]),      
  forall(TmpRG,format(O,'~q.~n',[TmpR])).

show_discord_info:-
  tmp_r(R,_TmpR,TmpRG),
  forall(TmpRG,ddbg_always(R)).
show_discord_info_raw:-
  tmp_r(R,_TmpR,TmpRG),
  forall(TmpRG,ddbg_raw(R)).
show_discord_info(Str):-
 tmp_r(R,_TmpR,TmpRG),
 forall(TmpRG,
 ignore((
   discord_debug_cvt(R,RD),sformat(S,'~w ~q',[RD,R]),
   matches_info(S,Str),
   ddbg_always(RD)))).

show_discord_info_raw(Str):-
 tmp_r(R,_TmpR,TmpRG),
 forall(TmpRG,
 ignore((
   discord_debug_cvt(R,RD),sformat(S,'~w ~q',[RD,R]),
   matches_info(S,Str),
   ddbg_raw(R)))).


:- if( \+ prolog_load_context(reloading, true)).
:- at_halt(save_discord_info).
:- load_discord_info.
:- endif.

% ===========================
% Database
% ===========================

:- thread_local( t_l:prop_prepend/1).

add_id_type(_,Type):- number(Type),!.
add_id_type(_,String):- string(String),!.
add_id_type(ID,Type):- 
  discord_set_info(ID,instanceOf,Type),
  discord_set_info(ID,id,ID).

%fixed_id("READY",'@me').
%fixed_id("READY",'members').
fixed_id(S,ID):- from_string(S,ID), number(ID).

discord_add("TYPING_START",Dict):- from_dict(Dict,[channel_id:ID,user_id:User,timestamp:Time]),!, 
  nop(discord_addd(ID,event,at(typing_start(User),Time))).

discord_add("MESSAGE_REACTION_ADD",Data):- is_dict(Data), 
  Data.message_id=MID, 
  ignore((Data.channel_id=CID,discord_addd(MID,channel_id,CID))),
  discord_addd(MID,reaction,Data).
discord_add("MESSAGE_REACTION_REMOVE",Data):- is_dict(Data), 
  Data.message_id=MID, 
  ignore((Data.channel_id=CID,discord_addd(MID,channel_id,CID))),
  discord_addd(MID,reaction_remove,Data).

discord_add(guild_member,Data):- toplevel_prop, 
  Data.user.id = ID, 
  add_id_type(ID,members),
  discord_add(ID,Data),!.
discord_add(member,Data):- toplevel_prop,
  Data.user.id = ID, 
  add_id_type(ID,memberz),
  discord_add(ID,Data),!.

%discord_add(Prop,List):- discord_grouping(Prop), is_list(List),!, maplist(discord_add(Prop),List).
discord_add(ID,Data):- fixed_id(ID,ID2),ID\==ID2,!,discord_add(ID2,Data).
discord_add(Type,Pairs):- 
  toplevel_prop,
  is_list(Pairs),select(KV,Pairs,Rest),
  once(get_kvd(KV,id,ID);get_kvd(KV,"id",ID)),
  once((add_id_type(ID,Type),discord_add(ID,Rest))),!.  

discord_add(Type,Pairs):- toplevel_prop, is_list(Pairs), Pairs\==[], !, maplist(discord_add(Type),Pairs),!.

discord_add(Type,KV):- get_kvd(KV,K,V), !,discord_addd(Type,K,V),!.
discord_add(Type,Data):- toplevel_prop, is_dict(Data),dict_pairs(Data,_Tag,Pairs),!,discord_add(Type,Pairs),!.
discord_add(CREATE,Data):- toplevel_prop, string_appended('_CREATE',CREATE,TypeS),default_guild(ID),discord_addd(ID,TypeS,Data),!.
discord_add(UPDATE,Data):- toplevel_prop, string_appended('_UPDATE',UPDATE,TypeS),default_guild(ID),discord_addd(ID,TypeS,Data),!.

%discord_add("GUILD_CREATE",{Data,Rest}):- !, discord_add("GUILD_CREATE",Data),discord_add("GUILD_CREATE",{Rest}),!.
%discord_add("GUILD_CREATE",{Data}):- !, discord_add("GUILD_CREATE",Data).


%discord_add(Type,{Data,Rest}):- !, discord_add(Type,Data),discord_add(Type,{Rest}).
discord_add(Type,Data):- %retractall(tmp:discord_info(Type,_,_)),
  discord_addd(Type,hasValue,Data),!.


discord_addd(reconnect,op,7):- discord_reconnect.

%discord_addd(user_settings,Prop, Data):- !, discord_add(Prop, Data).
discord_addd("READY",Prop, Data):- \+ retain_props(Prop),!, discord_add(Prop, Data).
discord_addd(ID,Prop,Data):- fixed_id(ID,ID2),ID\==ID2,!,discord_addd(ID2,Prop,Data).

discord_addd(ID,Prop,Data):- from_string(Data,Data2),Data\==Data2,!,discord_addd(ID,Prop,Data2).

discord_addd(Guild,presences,[H|List]):- default_guild(Guild), maplist(discord_addd(presence,hasMember),[H|List]),!.

discord_addd(ID,mentions,List):- !, discord_addd(ID,recipients,List).

discord_addd(ID,recipients,List):- is_list(List),maplist(discord_addd(ID,recipient),List),!.
discord_addd(ID,recipient,Dict):- is_dict(Dict),del_dict(id,Dict,UserID,Rest),!,discord_add(UserID,Rest),
   discord_addd(ID,recipient,UserID).

discord_addd(ID,recipient,UserID):- integer(UserID),
  forall(id_to_name(UserID,Name),discord_addd(ID,recipient_name,Name)),
  fail.

discord_addd(ID,attachments,List):- is_list(List),maplist(discord_addd(ID,attachment),List).
discord_addd(ID,attachment_url,URL):- notrace_catch(http_open:http_get(URL,Data,[])), 
  once(discord_addd(ID,attached_content,Data)),fail.

discord_addd(ID,content,Data):- Data \== "",
  add_discord_chat_event(ID,content(Data)),fail.

discord_addd(ID,embeds,Data):- Data \== [],
  add_discord_chat_event(ID,embeds(Data)),fail.

discord_addd(ID,attached_content,Data):- Data \== "",
  add_discord_chat_event(ID,content(Data)),fail.


discord_addd(ID,user,Data):- 
  is_dict(Data),
  once(discord_add(ID,Data)),fail.

discord_addd(MID,channel_id,CID):-
  add_id_type(CID,channels),
  add_id_type(MID,messages),
  discord_ddd(MID,author_username,Who),
  without_ddbg(discord_addd(CID,user_seen,Who)),
  fail.

discord_addd(_CID,last_message_id,MID):- MID\==null, add_id_type(MID,messages),fail.


discord_addd(Guild,hasValue,List):- default_guild(Guild),is_list(List),!, maplist(discord_add(Guild),List).


discord_addd(presence,hasMember,Dict):- Dict.user.id = ID,!,
  without_ddbg(discord_add(ID,Dict)).

discord_addd(ID,referenced_message,Dict):- is_dict(Dict), Dict.id = MID, !,
  discord_addd(ID,referenced_message,MID),
  discord_add(MID, Dict).



discord_addd(Guild,Prop,[H|List]):- default_guild(Guild), % discord_grouping(Prop), 
  is_list(List),
  \+ \+ has_id(H),!,
  maplist(discord_add(Prop),[H|List]),!.

discord_addd(Guild,members,[H|List]):- default_guild(Guild), % discord_grouping(Prop), 
  is_list(List), maplist(discord_add(guild_member),[H|List]),!.

discord_addd(_,op,10):- !, discord_identify.


discord_addd(ID,Prop,Dict):- atom(Prop), once(t_l:prop_prepend(Pre)), Pre\=='',
 atomic_list_concat([Pre,Prop],'_',PreProp),!,
 locally(t_l:prop_prepend(''), discord_addd(ID,PreProp,Dict)).

discord_addd(ID,Author,Dict):- maybe_prepend(Author), integer(ID), toplevel_prop, is_dict(Dict),!,
 dict_pairs(Dict,_,Pairs),
 locally(t_l:prop_prepend(Author),maplist(discord_add(ID),Pairs)).

%discord_addd(ID,user_username,Dict):- !, discord_set_info(ID,name,Dict).
%discord_addd(ID,username,Dict):- !, discord_set_info(ID,name,Dict).
discord_addd(ID,Prop,Dict):- discord_set_info(ID,Prop,Dict),!.

discord_set_info(ID,Prop,Data):- from_string(ID,ID2),ID\==ID2,!,discord_set_info(ID2,Prop,Data).
discord_set_info(Data,Prop,ID):- from_string(ID,ID2),number(ID2),ID2>10000,ID\==ID2,!,discord_set_info(Data,Prop,ID2).

discord_set_info(ID,Prop,Data):- 
 once(Prop==hasValue ; (number(Data),get_time(Time),Data<Time)),
 \+ tmp:discord_info(ID,Prop,Data),
 retractall(tmp:discord_info(ID,Prop,_)),fail.
discord_set_info(_,Prop,Data):- default_info(Prop,Data),!.
discord_set_info(ID,Prop,Data):-
  TmpR=tmp:R,
  R= discord_info(ID,Prop,Data),
  (\+ \+ call(TmpR) -> (retract(TmpR),asserta(TmpR),nop(ddbg(discord_keep_info(ID,Prop,Data)))) 
   ; (asserta(TmpR),on_new_ddd(ID,Prop,Data))).

on_new_ddd(ID,Prop,Data):-
  ddbg(discord_addd(ID,Prop,Data)),
  ignore((Data==threads->discord_join_subchannel(ID))).



maybe_prepend(author).
maybe_prepend(user).
maybe_prepend(recipients).
maybe_prepend(referenced_message):-!,fail.
maybe_prepend(message_reference).
%maybe_prepend(Atom):- atom(Atom).

default_info(hasValue,[]).
default_info(X,_):- nonvar(X), no_default_info(X),!,fail.
default_info(flags,0). % probably this doesnt belong
default_info(accent_color,null). default_info(attachments,[]). default_info(avatar,null).
default_info(banner,null). default_info(banner_color,null). default_info(components,[]).
default_info(edited_timestamp,null). default_info(embeds,[]). 
default_info(guild_id,GuildID):- default_guild(GuildID).
default_info(last_message_id,null). default_info(mention_everyone,false). default_info(mention_roles,[]).
default_info(mentions,[]). default_info(nsfw,false). default_info(permission_overwrites,[]).
default_info(pinned,false). default_info(rate_limit_per_user,0). default_info(rtc_region,null).
default_info(tts,false). default_info(type,0). default_info(user_limit,0). default_info(public_flags,0).
default_info(email,null). default_info(features,[]). default_info(messages,[]). default_info(owner,false).
default_info(X,Y):- default_info_value(Y),!, nop(dmsg(default_info(X,Y))).

default_info_value(null).
default_info_value(0).
default_info_value([]).
default_info_value(false).

no_default_info(bot). 
no_default_info(topic). no_default_info(position). no_default_info(parent_id). % no_default_info(s).
no_default_info(id). no_default_info(instanceOf). no_default_info(type). no_default_info(hasValue).

retain_props(guild_experiments).
retain_props(experiments).

