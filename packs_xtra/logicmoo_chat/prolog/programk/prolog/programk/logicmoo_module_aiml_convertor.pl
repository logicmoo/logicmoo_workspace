% ===================================================================
% File 'logicmoo_module_aiml_loader.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

ppfs:-ppfs('../aiml/chomskyAIML/chomsky04*.aiml').

% ppfs('../aiml/chomskyAIML/*.aiml').

ppfs:-ppfs('../aiml/chomskyAIML/chomsky001.aiml').
ppfs(FN):-expand_file_name(FN,Exp),member(F,Exp),ppfs1(F),fail.
ppfs(_).

ppfs1(File):-
    fileToLineInfoElements(_Ctx,File,Z),
    atom_concat(File,'.term',Elis),
    (file_newer(Elis,File) 
      -> 
       ('format'('%% skipping: ~q ~n',[Elis]),flush_output(user_output))
        ; 
         prolog_mustEach(('format'('%% writing: ~q ~n',[Elis]),flush_output(user_output),setup_call_cleanup(open(Elis,write,Str),writeEachTo(Str,Z),close(Str))))).

writeEachTo(_Str,[]):-!.
writeEachTo(Str,Atom):-atom(Atom),'format'(Str,'atom_load(~q).~n',[Atom]),'format'('atom_load(~q).~n',[Atom]),!.
writeEachTo(Str,[H|T]):- !, maplist_safe(writeEachTo(Str),[H|T]).
writeEachTo(Str,element(aiml,[],STUFF)):- !,writeEachTo(Str,STUFF).
writeEachTo(Str,element(Foo,S,STUFF)):- member(Foo,[category,topic]),!,'format'(Str,'~q.~n',[element(Foo,S,STUFF)]),!.
writeEachTo(Str,S):-'format'(Str,'~q.~n',[S]), 'format'('%% UNK ~q.~n',[S]).


file_newer(File1,File2):-
   time_file_safe(File1,Time1), % fails on non-existent
   time_file_safe(File2,Time2),Time1>Time2.



%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
/*
:- if((current_prolog_flag(version,MMmmPP),MMmmPP<70000)).
:- style_check(-atom).
:- style_check(-string).
:- endif.
*/

:-discontiguous(convert_ele/3).

translate_single_aiml_file(Ctx,F0):-global_pathname(F0,File),F0\==File,!,translate_single_aiml_file(Ctx,File).
translate_single_aiml_file(Ctx,F0):-
  prolog_mustEach((
   global_pathname(F0,File),!,
   cateForFile(Ctx,File,FileMatch),!,
   atom_concat_safe(File,'.tmp.pl',PLNAME),
   translate_single_aiml_file(Ctx,File,PLNAME,FileMatch))),!.

translate_aiml_structure(Ctx,Structure):- string(Structure),!,
   debugFmt(translate_aiml_structure_string(Ctx,Structure)),
   string_to_structure(Ctx,Structure,XMLStructures),
   load_aiml_structure(Ctx,XMLStructures),!.

translate_aiml_structure(Ctx,Structure):-not(atom(Structure)),!, trace,
   debugFmt(translate_aiml_structure_no_atom(Ctx,Structure)),!.

translate_aiml_structure(Ctx,Structure):- trace,
   debugFmt(translate_aiml_structure_atom(Ctx,Structure)),!,
   string_to_structure(Ctx,Structure,XMLStructures),
   load_aiml_structure(Ctx,XMLStructures),!.


cateForFile(_Ctx,SRCFILE,aimlCate(_GRAPH,_PRECALL,_TOPIC,_THAT,_INPUT,_PATTERN,_FLAGS,_CALL,_GUARD,_USERDICT,_TEMPLATE,_LINENO,SRCFILE:_-_,_RULESYM)):-useCateID,!.
cateForFile(_Ctx,SRCFILE,aimlCate(_GRAPH,_PRECALL,_TOPIC,_THAT,_INPUT,_PATTERN,_FLAGS,_CALL,_GUARD,_USERDICT,_TEMPLATE,_LINENO,SRCFILE:_-_)):-!.
cateForFile(Ctx,File,FileMatch):- atrace,withNamedValue(Ctx,[anonvarsFroCate=true], makeAimlCate(Ctx,[srcfile=File:_-_],FileMatch)),!.

withNamedValue(Ctx,[N=V],Call):-withAttributes(Ctx,[N=V],Call),!.

% =================================================================================
% AIML -> Prolog pretransating
% =================================================================================

:-dynamic(creating_aiml_file/2).
:-dynamic(loaded_aiml_file/3).
:-dynamic(pending_aiml_file/2).

do_pending_loads:-withCurrentContext(do_pending_loads).
do_pending_loads(Ctx):-forall(retract(pending_aiml_file(File,PLNAME)),load_pending_aiml_file(Ctx,File,PLNAME)).

load_pending_aiml_file(Ctx,File,PLNAME):- debugFmt(load_pending_aiml_file(Ctx,File,PLNAME)),
  error_catch(prolog_must(dynamic_load(File,PLNAME)),E,(debugFmt(E),assert(pending_aiml_file(File,PLNAME)))),!.

translate_single_aiml_file(_Ctx,File,PLNAME,FileMatch):- creating_aiml_file(File,PLNAME),!,
  throw_safe(already(creating_aiml_file(File,PLNAME),FileMatch)),!.

translate_single_aiml_file(_Ctx,File,PLNAME,_FileMatch):-   %% fail if want to always remake file
   file_newer(PLNAME,File), % fails on non-existent
   %not(aimlOption(rebuild_Aiml_Files,true)),
   debugFmt(up_to_date(create_aiml_file(File,PLNAME))),!,
   retractall(creating_aiml_file(File,PLNAME)),!.

%%translate_single_aiml_file(_Ctx,File,PLNAME,FileMatch):- loaded_aiml_file(File,PLNAME,Time),!, throw_safe(already(loaded_aiml_file(File,PLNAME,Time),FileMatch)).
translate_single_aiml_file(Ctx,File,PLNAME,FileMatch):- loaded_aiml_file(File,PLNAME,Time),!,
   debugFmt(translate_single_aiml_file(loaded_aiml_file(File,PLNAME,Time),FileMatch)),
   prolog_must(retract(loaded_aiml_file(File,PLNAME,Time))),
   retractall(pending_aiml_file(File,PLNAME)),
   assertz(pending_aiml_file(File,PLNAME)),!,
   translate_single_aiml_file(Ctx,File,PLNAME,FileMatch).

translate_single_aiml_file(Ctx,File,PLNAME,FileMatch):-
  call_cleanup(
     translate_single_aiml_file0(Ctx,File,PLNAME,FileMatch),
     translate_single_aiml_file1(File,PLNAME,FileMatch)).

translate_single_aiml_file0(Ctx,File,PLNAME,FileMatch):-
 prolog_mustEach((
        asserta(creating_aiml_file(File,PLNAME)),
        debugFmt(doing(create_aiml_file(File,PLNAME))),
        aimlCateSig(CateSig),!,
   (format('%-----------------------------------------~n')),
        printPredCount('Cleaning',FileMatch,_CP),
   (format('%-----------------------------------------~n')),
        unify_listing(FileMatch),
        retractall(FileMatch),
   (format('%-----------------------------------------~n')),
        flag(cateSigCount,PREV_cateSigCount,0),
   (format('%-----------------------------------------~n')),
      withAttributes(Ctx,[withCategory=[translate_cate]], %% asserta_cate = load it as well .. but interferes with timesrtamp
               ( fileToLineInfoElements(Ctx,File,AILSTRUCTURES),
                 tell(PLNAME),
                  load_aiml_structure(Ctx,AILSTRUCTURES))),

   (format('%-----------------------------------------~n')),
         unify_listing_header(FileMatch),
         %printAll(FileMatch),
   (format('%-----------------------------------------~n')),
        listing(xmlns),
   (format('%-----------------------------------------~n')),
        told,
        flag(cateSigCount,NEW_cateSigCount,PREV_cateSigCount),
        printPredCount('Errant Lines',lineInfoElement(File,_,_,_),_EL),
        printPredCount('Total Categories',CateSig,_TC),!,
        debugFmt('NEW_cateSigCount=~q~n',[NEW_cateSigCount]),!,
        statistics(global,Mem),MU is (Mem / 1024 / 1024),
        debugFmt(statistics(global,MU)),!,
        printPredCount('Loaded',FileMatch, _FM),
        retractall(creating_aiml_file(File,PLNAME)))),!.

stream_file(user,PLNAME):-!,atrace,stream_property(user,file_name(Name)),prolog_must(PLNAME=Name).
stream_file(PLNAMET,PLNAME):-atrace,is_stream(PLNAMET),stream_property(PLNAMET,file_name(Name)),prolog_must(PLNAME=Name).
stream_file(PLNAMET,PLNAME):-exists_file(PLNAMET),!,prolog_must(PLNAME=PLNAMET).


translate_single_aiml_file1(File,PLNAME,FileMatch):-
    catch((ignore((telling(PLNAMET),PLNAMET\==user,stream_file(PLNAMET,PLNAME),told))),_,true),
    catch((ignore((creating_aiml_file(File,PLNAME),retractall(creating_aiml_file(File,PLNAME)),delete_file(PLNAME)))),_,true),
    retractall(lineInfoElement(File,_,_,_)),
    retractall(FileMatch),
    retractall(xmlns(_,_,_)),   
    retractall(loaded_aiml_file(File,PLNAME,_Time)).

/*
translate_single_aiml_filexxx(Ctx,File,PLNAME):-
  prolog_must((
     Dofile = true,
     aimlCateSig(CateSig),
   ifThen(Dofile,tell(PLNAME)),
   (format(user_error,'%~w~n',[File])),
   load_structure(File,X,[dialect(xml),space(remove)]),!,
   ATTRIBS = [srcfile=File],!,
   pushAttributes(Ctx,filelevel,ATTRIBS),
   load_aiml_structure_list(Ctx,X),!,
   popAttributes(Ctx,filelevel,ATTRIBS),!,
   ifThen(Dofile,((listing(CateSig),retractall(CateSig)))),
   ifThen(Dofile,(told /*,[PLNAME]*/ )))),!.
*/

% ===================================================================
% ===================================================================

convert_text('',[]):-!.
convert_text([],[]):-!.
convert_text(C,D):-is_list(C),!,convert_text_list(C,D),!.
convert_text(A,L):-atom(A),!,convert_atom(A,O),convert_text_list(O,L).
convert_text(A,[]):-ignore_aiml(A),!.
convert_text(E,File):-aiml_error(convert_text(E,File)),!,E=File.


convert_text_list([],[]):-!.
convert_text_list([A],B):-!,convert_text_list(A,B).
convert_text_list(M,C):-delete(M,'',B), (M == B -> C=B ; convert_text_list(B,C)).
convert_text_list([A|AA],BBB):-convert_text(A,B),convert_text_list(AA,BB),!,flattem_append(B,BB,BBB0),!,BBB=BBB0.
convert_text_list(A,C):-atom(A),atomWSplit(A,M),([A]==M->C=M;convert_text(M,C)),!.
convert_text_list(A,AA):-listify(A,AA).

convert_atom(A,Z):-convert_atom0(A,Y),!,Y=Z.
convert_atom(E,File):-aiml_error(convert_atom(E,File)),!,E=File.
%convert_atom(A,C):-atom_to_number(A,C),!.
convert_atom0(A,C):-atomWSplit(A,M),!,convert_text(M,C),!.
convert_atom0(A,D):-literal_atom_safe(A,D),!.
convert_atom0(A,A):-concat_atom_safe([A],' ',A).
convert_atom0(A,A). %%:-!listify(A,AA).

flattem_append(A,B,BBB):-flatten([A],AA),!,flatten([B],BB),!,append(AA,BB,BBB),!.



% ===============================================================================================
%  PATTERN/TEMPLATE normalization
% ===============================================================================================
convert_template(_Ctx,X,_Y):-var(X),throw_safe(var(X)).
convert_template(_Ctx,_X,Y):-nonvar(Y),throw_safe(nonvar(Y)).
convert_template(_Ctx,[],[]):-!.
%%HIDE convert_template(Ctx,[I|P],L):-!,convert_template(I,IO),!,convert_template(Ctx,P,PO),append(IO,PO,L),!.
convert_template(_Ctx,I,[]):-ignore_aiml(I),!.

%%%HIDE            %%convert_template(_Ctx,[ATOM],O):-atom(ATOM),!,atomWSplit(ATOM,LIST),!,toAtomList(LIST,O),!.
convert_template(Ctx,I,GOOD):- atom(I),atomWSplit(I,LIST),toAtomList(LIST,O),[I] \== O,!, convert_template(Ctx,O,GOOD),!.
%%%HIDE            %%convert_template(Ctx,[I|P],GOOD):- is_list(I),!,append(I,P,IP),!,convert_template(Ctx,IP,GOOD),!.
%%%HIDE            %%convert_template(Ctx,[I|P],GOOD):- convert_template(Ctx,I,O), I \== O,!, convert_template(Ctx,[O|P],GOOD),!.
convert_template(Ctx,[I|P],GOOD):- convert_template(Ctx,I,O),!,convert_template(Ctx,P,L),!,append(O,L,GOOD),!.
%%%HIDE            %%convert_template(Ctx,[P],POL):-!,convert_template(Ctx,P,POL).
convert_template(Ctx,element(TAG,ATTRIBS,P),POL):-!, convert_element(Ctx,element(TAG,ATTRIBS,P),OUT),!,listify(OUT,POL).
convert_template(Ctx,P,POL):-convert_element(Ctx,P,PO),!,listify(PO,POL).

toAtomList(A,O):-delete(A,'',O),!.

convert_element(Ctx,element(Tag, A, B),Out):-!,convert_ele(Ctx,element(Tag, A, B),M),!,M=Out,!.
convert_element(_Ctx,Input,Out):-atomic(Input),convert_text_list(Input,Out),!.
convert_element(Ctx,Input,Out):-convert_ele(Ctx,Input,M),!,prolog_must(M=Out).

      
nameOrValue(ALIST, _VALUE, NORV, 0):-lastMember(name=NORV,ALIST),!.
nameOrValue(ALIST, _VALUE, NORV, 0):-lastMember(var=NORV,ALIST),!.
nameOrValue(_XATS, VALUE, NORV, 1):- NORV = VALUE.

convert_ele(_Ctx,_X,Y):-nonvar(Y),throw_safe(nonvar(Y)).
convert_ele(_Ctx,In,_In):-not(ground(In)),aiml_error(not(ground(In))),!,fail.

convert_ele(Ctx,li(A),li(AA)):-convert_template(Ctx,A,AA).
convert_ele(_Ctx,element(NSLocal,_A,_B),_Out):- var(NSLocal),!,throw_safe(not(atom(NSLocal))),!.
convert_ele(Ctx,element(_NS:Local,A,B),Out):- !,convert_ele(Ctx,element(Local,A,B),Out),!.
convert_ele(_Ctx,element(NSLocal,_A,_B),_Out):-not(atom(NSLocal)),!,throw_safe(not(atom(NSLocal))),!.
convert_ele(Ctx,element(NSLocal,A,B),Out):- concat_atom_safe([_NS,Local],':',NSLocal),!,convert_ele(Ctx,element(Local,A,B),Out),!.
convert_ele(Ctx,element(html:TAG,A,B),Out):-!,convert_ele(Ctx,element(TAG,A,B),Out),!.
convert_ele(_Ctx,element(br,[],[]),'\n').
convert_ele(_Ctx,element(p,[],[]),'\r\n').
convert_ele(Ctx,element(pre,[],B),BB):-!,convert_template(Ctx,B,BB).

convert_ele(Ctx,element(catagory, A, B),Out):-convert_ele(Ctx,element(category, A, B),Out).
%%convert_ele(Ctx,element(Tag, A, B),BB):- member(Tag,[category,srai]), convert_template(Ctx,element(Tag, A, B),BB).


botGetSet(bot,bot,_NAME,_NUM).
botGetSet(get,user,_NAME,_NUM).
botGetSet(set,user,_NAME,0).

% bot/get/set
convert_ele(Ctx,element(TAG, ALIST, VALUE),element(TAG,NEWLIST,VALUEO)):-
            botGetSet(TAG,TYPE,NAME,NUM),not(member(var=_,ALIST)),         
            append(ALIST,[type=TYPE,var=NAME],NEWLIST),
            nameOrValue(ALIST,VALUE,NORV,NUM), 
            convert_template(Ctx,NORV,NAME), 
            convert_template(Ctx,VALUE,VALUEO).

% get_xxx/set_xxx
convert_ele(Ctx,element(VAR_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat_safe('get_',N,VAR_ATOM),convert_template(Ctx,V,VV).
convert_ele(Ctx,element(VAR_ATOM, ALIST, V),element(set,[name=N|ALIST],VV)):-atom_concat_safe('set_',N,VAR_ATOM),convert_template(Ctx,V,VV).

% bot_xxx/botxxx
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat_safe('bot_',N,BOT_ATOM),convert_template(Ctx,V,VV).
convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=N|ALIST],VV)):-atom_concat_safe('bot',N,BOT_ATOM),lengthAtLeast(N,2),convert_template(Ctx,V,VV),!.

% getXXX
convert_ele(Ctx,element(VAR_ATOM, ALIST, V),element(get,[name=N|ALIST],VV)):-atom_concat_safe('get',N,VAR_ATOM),lengthAtLeast(N,2),convert_template(Ctx,V,VV),!.

% version/id/favfood/date/size
% HANDLE this in computeAnswer except for favfood maybe? for now favfood is still in computeAnswer
% convert_ele(Ctx,element(BOT_ATOM, ALIST, V),element(bot,[name=BOT_ATOM|ALIST],VV)):- globalAliceTagVar(BOT_ATOM),convert_template(Ctx,V,VV),!.

% ===================================================================
% ===================================================================

%DELAY convert_ele(Ctx,element(random, [], B),random(BB)):-convert_template(Ctx,B,BB).
%DELAY convert_ele(Ctx,element(li, Attrib, B),element(li, Attrib, BB)):-convert_template(Ctx,B,BB).
%DELAY convert_ele(Ctx,element(star, [], []),(*)).
convert_ele(_Ctx,element(a, [Target, Link], Name),A):-sformat(S,'<a ~q ~q>~w</a>',[Target, Link, Name]),string_to_atom(S,A).
convert_ele(_Ctx,element(a, [Link], Name),A):-sformat(S,'<a ~q>~w</a>',[Link, Name]),string_to_atom(S,A).

%DELAY convert_ele(Ctx,element(get, [name=Var], []),get(Var)):-!.
convert_ele(_Ctx,element(learn,Attrs,MORE),element(learn,NewAttrs,MORE)):- Become = srcfile, append(Left,[N=V|Right],Attrs),pathAttrib(N),N\=Become,append(Left,[Become=V|Right],NewAttrs),!.
convert_ele(_Ctx,element(load, Attrs,MORE),element(load ,NewAttrs,MORE)):- Become = srcfile, append(Left,[N=V|Right],Attrs),pathAttrib(N),N\=Become,append(Left,[Become=V|Right],NewAttrs),!.
convert_ele(_Ctx,element(sr,ALIST,MORE),element(srai,ALIST,[element(star,ALIST,MORE)])):-!.
convert_ele(_Ctx,element(star,ALIST,MORE),star(pattern,XLAT2,MORE2)):-!,starIndex(star,pattern,ALIST,MORE,XLAT2,MORE2).
  starIndex(_Tag,_Star,ALIST,MORE,XLAT2,MORE2):-convert_attributes(Ctx,ALIST,XLAT2),convert_template(Ctx,MORE,MORE2),!.

convert_ele(_Ctx,element(Tag,ALIST,MORE),star(Star,XLAT2,MORE2)):- starType(Tag,Star),!,starIndex(Tag,Star,ALIST,MORE,XLAT2,MORE2).
   starType(Tag,Star):-member(Tag=Star,[star=pattern,topicstar=topic,guardstar=guard,inputstar=pattern,thatstar=that,get_star=pattern]),!.
   starType(Tag,Star):-atom_concat_safe(Star,'_star',Tag),!.
   starType(Tag,Star):-atom_concat_safe(Star,'star',Tag),!.

convert_ele(Ctx,element(Tag, ALIST , INNER_XML), RESULT):-
      transform_aiml_structure(Tag,NewTag,ALIST,NewProps,INNER_XML,NEWPATTERN),
      convert_ele(Ctx,element(NewTag, NewProps, NEWPATTERN),RESULT),!.

convert_ele(Ctx,L,LO):-is_list(L),flatten(L,M),!,
	    (L==M -> LO=M ; convert_template(Ctx,M,LO)).

%convert_ele(Ctx,A,B):-atom(A),atom_to_number(A,B).

convert_ele(_Ctx,A,W):-atom(A),atomWSplit(A,B),!,convert_text(B,W),!.

convert_ele(Ctx,element(A, B, C),INNER_XML):-tagType(A, immediate),!,
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),!,
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=element(AA, BB, CC); convert_element(Ctx,element(AA, BB, CC),INNER_XML)),!.

convert_ele(Ctx,element(A, B, C),INNER_XML):-
      convert_name(A,AA),
      convert_attributes(Ctx,B,BB),
      convert_template(Ctx,C,CC),!, 
   (element(A, B, C) == element(AA, BB, CC) ->  INNER_XML=element(AA, BB, CC); convert_element(Ctx,element(AA, BB, CC),INNER_XML)),!.

convert_ele(Ctx,element(Tag, A, B),element(Tag, A, BB)):- member(Tag,[category]), convert_template(Ctx,B,BB).

convert_ele(Ctx,element(Tag, A, B),element(Tag, A, BB)):- member(Tag,[srai]),atrace,convert_template(Ctx,B,BB).

convert_ele(_Ctx,O,O).


convert_attributes(Ctx,A,AAA):- hotrace(prolog_must((convert_attributes0(Ctx,A,AA),list_to_set_safe(AA,AAA)))).
convert_attributes0(Ctx,[B|A],[BB|AA]):-convert_attribute(B,BB),convert_attributes0(Ctx,A,AA).
convert_attributes0(_Ctx,[],[]).

convert_attribute(A=B,AA=BB):-convert_name(A,AA),convert_template(_Ctx,B,BB).

convert_name(A,AAA):-convert_name0(A,AA), (A==AA -> AAA=AA ; convert_name(AA,AAA)),!.

convert_name0(A,AA):-literal_atom_safe(A,AA).
convert_name0(var,name).
convert_name0(Attrib,srcfile):-pathAttrib(Attrib),!.

% ===================================================================
% ===================================================================

% ===============================================================================================
%  refomat type transformations
% ===============================================================================================

isVerbatumTag(N):-memberchk(N,[call,precall,srcfile,srcdir,lineno,srcinfo]),!.
isVerbatumTag(N):-pathAttrib(N),!.


transformTagData(Ctx,[Name|S],Else,ValueI,ValueO):- member(N,[Name|S]),transformTagData0(Ctx,N,Else,ValueI,ValueO),prolog_must(N\==ValueO).
transformTagData(Ctx,[Name|S],Else,ValueI,ValueO):- member(N,[Name|S]),!,transformTagData1(Ctx,N,Else,ValueI,ValueO),prolog_must(N\==ValueO).
transformTagData(Ctx,Tag,Else,ValueI,ValueO):-transformTagData0(Ctx,Tag,Else,ValueI,ValueO),prolog_must(Tag\==ValueO).
transformTagData(Ctx,Tag,Else,ValueI,ValueO):-transformTagData1(Ctx,Tag,Else,ValueI,ValueO),prolog_must(Tag\==ValueO).

% this was _Tag.. very bad!
tagStar(_TAG,Star,Star):-!.

transformTagData0(_Ctx,TAG,_Default,[*],TAGSTAR):-tagStar(TAG,*,TAGSTAR),!.
transformTagData0(_Ctx,TAG,_Default,*,TAGSTAR):-tagStar(TAG,*,TAGSTAR),!.
transformTagData0(_Ctx,TAG,_Default,['_'],TAGSTAR):-tagStar(TAG,'_',TAGSTAR),!.
transformTagData0(_Ctx,TAG,_Default,'_',TAGSTAR):-tagStar(TAG,'_',TAGSTAR),!.
transformTagData0(Ctx,Tag,_Else,ValueI,ValueO):- ValueI=='$current_value', current_value(Ctx,Tag,ValueO),!.
transformTagData0(_Ctx,_N,_Else,gensym(Named),ValueO):-useCateID,atom(Named),gensym(Named,ValueO),!.
transformTagData0(_Ctx,N,Else,ValueO,ValueO):-isVerbatumTag(N),!, member(Else,['$current_value']),!.
transformTagData0(Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):-isPatternTag(TAG),convert_pattern(Ctx,PATTERN_IN,PATTERN_OUT),!.

transformTagData0(Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):-
  isOutputTag(TAG),convert_template_pred(Ctx,=,PATTERN_IN,PATTERN_OUT),!,
  nop((traceIf((    
    member(element(THAT,_,_),PATTERN_IN),
    not(member(element(THAT,_,_),PATTERN_OUT)),
    not(member(THAT,[br,star,pattern,thatstar,topicstar,think,srai,sr]))
    )))),
  convert_template_pred(Ctx,=,PATTERN_IN,_PATTERN_OUT_UNUSED).

transformTagData1(_Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):- member(TAG,[userdict,graph]),literal_atom_safe(PATTERN_IN,PATTERN_OUT),!.
transformTagData1(_Ctx,TAG,_Default,PATTERN_IN,PATTERN_OUT):-infoTagLikeLineNumber(TAG),!,PATTERN_IN=PATTERN_OUT.

transformTagData1(Ctx,_TAG,_Default,PATTERN_IN,PATTERN_OUT):- %%% debugFmt(transformTagData(TAG,Default,PATTERN_IN)), 
                 convert_template_pred(Ctx,literal_atom_safe,PATTERN_IN,PATTERN_OUT),!.
transformTagData1(Ctx,_N,_Default,R,RR):-convert_template(Ctx,R,RR),!. 
transformTagData1(_Ctx,_TAG,_Default,PATTERN,PATTERN):-!.

% ===============================================================================================
% ===============================================================================================

convert_pattern(Ctx,PATTERN_IN,PATTERN_OUT):- convert_template_pred(Ctx,matchable_literal_safe_non_special,PATTERN_IN,PATTERN_OUT),!.

matchable_literal_safe_non_special(A,A):-not(atom(A)),!.
matchable_literal_safe_non_special(Atom,Atom):-atom_prefix(Atom,'#$'),!.
matchable_literal_safe_non_special(A,U):-literal_atom_safe(A,U).

convert_template_pred(Ctx,Pred,PATTERN_IN,PATTERN_OUT):- convert_template(Ctx,PATTERN_IN,PATTERN_MID),!,
     prolog_must(map_tree_to_list(Pred,PATTERN_MID,PATTERN_OUT)),!.

transform_aiml_structure(catagory,category,OldProps,OldProps,NEWPATTERN,NEWPATTERN).
transform_aiml_structure(alice,aiml,OldProps,OldProps,NEWPATTERN,NEWPATTERN).
transform_aiml_structure('name','bot',OldProps,[name=['name']|OldProps],NEWPATTERN,NEWPATTERN).
transform_aiml_structure(OldName,NewName,OldProps,NewProps,NEWPATTERN,NEWPATTERN):-
      specialIndex(OldName,NewName,AddProps),append(AddProps,OldProps,NewProps).

specialIndex(justbeforethat,that,[index=(2:1)]).
specialIndex(justthat ,input,[index=2]).
specialIndex(beforethat,input,[index=3]).

%%specialIndex(load,learn,[]).
specialIndex(set_female,set,[name=gender,value=female]).

specialIndex(getname,name,[name=[name]]).
specialIndex(gettopic,name,[name=[name]]).

specialIndex(personf,formatter,[type=url_encode]).
specialIndex(Name,formatter,[type=Type]):-formatterType(Name,Type),!.


formatterProc(Dict):-member(Dict,[formal,uppercase,lowercase,sentence,gossip,think,(format)]).
formatterType(TagName,TagName):-formatterProc(TagName).
formatterTypeMethod(TagName,Type,Method):-formatterType(TagName,Type),atom_concat(format_,Type,Method),!.


evaluatorTag(Tag):-member(Tag,[system,javascript,eval,
                                     cycquery,cycsystem,cycassert,
                                     fortunecookie,substitute,learn,aiml,genlMt,think,
                                     substitute,srai,testsuite,testcase,template,set]).


substitutionDictsName(pattern,input).
substitutionDictsName(N,N):-substitutionDicts(N).

substitutionDicts(input).
substitutionDicts(output).
substitutionDicts(gender).
substitutionDicts(person).
substitutionDicts(person2).
substitutionDicts(person3).
%substitutionDicts(Dict):-evaluatorTag(Dict).


tagType(Tag,immediate):-evaluatorTag(Tag),!.
tagType(Tag,pushable):-cateFallback(LIST),member([Tag=_],LIST).
tagType(Tag,insideCate):-cateMember(Tag).

tagType(Tag,requiredCate):-member(Tag,[pattern,template]).
tagType(Tag,optionalCate):-cateMember(Tag),not(tagType(Tag,requiredCate)).

