% ===================================================================
% File 'parser_candc.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_candc.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================
% end_of_file.
:-module(parser_candc,[
            test_candc_server/0,
            test_candc_server/1, 
            test_candc_server/2,
            call_candc_server/2,
            call_candc_server/3                   
         ]).
:- set_module(class(library)).
:- set_module(base(system)).

:- user:ensure_loaded(library(logicmoo_utils_all)).


:- dynamic(input:(ccg/2)).
:- multifile(input:(ccg/2)).
:- discontiguous(input:(ccg/2)).
:- dynamic(input:(id/2)).
:- multifile(input:(id/2)).
:- discontiguous(input:(id/2)).

:- use_module(parser_lexical).
:- user:ensure_loaded(logicmoo_nlu_ext('candc/src/prolog/boxer/boxer.pl')).

:- use_module(boxer(ccg2drs),[ccg2drs/2]).
:- use_module(boxer(input),[openInput/0,identifyIDs/1,preferred/2]).
:- use_module(boxer(evaluation),[initEval/0,reportEval/0]).
:- use_module(boxer(version),[version/1]).
:- use_module(boxer(printCCG),[printCCG/2]).
:- use_module(boxer(transform),[preprocess/6]).
:- use_module(boxer(drs2fdrs),[eqDrs/2]).
:- use_module(boxer(output),[printHeader/4,printFooter/1,printSem/4,printRC/1,printRC/2,printRC/3]).

:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(semlib(options),[candc_option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).

print_reply_candc(L):- is_list(L),!,maplist(print_reply_candc,L).
print_reply_candc(S):- string(S),!,format('~s',[S]).
print_reply_candc(Drs):- print_reply_colored(Drs).

parse2sem(Text):- text2sem(Text,Drs),print_reply_candc(Drs).
text2sem(Text, Drs):- text2sem(Text, [drs,pdrs,fol,drg,amr,tacitus,der], Drs).

parse2sem(Text,Options):- text2sem(Text,Options,Drs),print_reply_candc(Drs).
text2sem(Text, Options, Drs):- 
  notrace((call_candc_server(Text,_Options,Reply),
  print_reply_candc(Reply))),
  must_or_rtrace(rply2drs(Reply,Options,Drs)).


set_memory_opts:- setDefaultOptions(boxer),
   setOption(boxer,'--box',false),setOption(boxer,'--ccg',false),setOption(boxer,'--copula',true),setOption(boxer,'--elimeq',false),
   setOption(boxer,'--format',prolog),setOption(boxer,'--instantiate',false),setOption(boxer,'--integrate',false),
   setOption(boxer,'--modal',false),setOption(boxer,'--nn',false),setOption(boxer,'--output',current_output),
   setOption(boxer,'--plural',false),
   %setOption(boxer,'--presup',max),
   setOption(boxer,'--resolve',false),
   setOption(boxer,'--roles',proto),   setOption(boxer,'--theory',drt), % was drt
   setOption(boxer,'--semantics',drs),setOption(boxer,'--tense',false),
   setOption(boxer,'--tokid',local),setOption(boxer,'--warnings',false),setOption(boxer,'--x',false),
   % setOption(boxer,'--input','/tmp/pl_boxer/logicmoo_33609_0.ccg'),
    %setOption(boxer,'--loaded',do),
    !.

make_ids(ccg(N,_)):- !, assert(input:id(_,[N])).
make_ids(_IDO).

make_ids(ccg(Id,_),IDO):- !, IDO=id(_,[Id]),assert(input:IDO).
make_ids(IDO,IDO).

setInput(Reply):- 
  retractall(input:inputtype(_)),
  assert(input:inputtype(ccg)),
  retractall(input:id(_,_)),
  retractall(input:ccg(_,_)),  
  maplist(input:asserta_new, Reply),
  initEval,
  ignore((
  \+ input:id(_,_),
  maplist(make_ids, Reply))),!.

each_candc_option(N,NV):- !, options:candc_option(boxer, N, _1, V, _), once(setOption(boxer,N,V)),NV=(N=V).
each_candc_option(N,V):- retractall(options:candc_option(N,_)), asserta(options:candc_option(N,V)).

rply2drs(Reply,Options,Drs):- \+ is_list(Reply),!, rply2drs([Reply],Options,Drs). % maplist(rply2drs,Reply,Drs).
rply2drs(Reply,Options,Drs):- 
   setInput(Reply),
   maplist(genSem2,Options,Drs).

%genSem2(Reply,Options,Drs):-

genSem2(Options,Drs):-  
  set_memory_opts,
  setDefaultOptions(boxer),  
  asserta_new(options:candc_option('--roles',framenet)),
  asserta_new(options:candc_option('--roles',verbnet)),
  asserta_new(options:candc_option('--roles',proto)),
  
  setOption(boxer,'--semantics',Options), % [drs,pdrs,fol,drg,amr,tacitus,der]
   setOption(boxer,'--box',true),
   setOption(boxer,'--warnings',true),
   setOption(boxer,'--tense',true),
   setOption(boxer,'--modal',true),
   setOption(boxer,'--integrate',false),  
   setOption(boxer,'--resolve',false),
   setOption(boxer,'--copula',false),
  
  findall(Str, 
       ( each_candc_option('--theory',A),
      
        %assert(options:candc_option('--theory',drt)), %:- member(V,[drt,sdrt]).
        initEval,runBoxer(Str),print_reply_candc([A,Options]),print_reply_candc(Str)),
  Drs).

runBoxer(Drs):-
  input:identifyIDs(NewReply),
    with_output_to(string(Str), 
     (boxer:buildList(NewReply, 1, current_output))),
     nop(read_term_list(Str, Drs)),Drs=Str,!.

% '--integrate'                                                                 
do_inputIDs(Reply,NewList):- maplist(make_ids,Reply,NewList), input:identifyIDs(NewList).



% :- user:ensure_loaded(logicmoo_nlu_ext('candc/src/prolog/nutcracker/nutcracker.pl')).



baseKB:sanity_test:- test_candc_server.

:- use_module(library(http/http_client)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

test_candc_server(English):- make, test_candc_server(English, _Options).

test_candc_server(English, Options):-
  call_candc_server(English, Options, OutF),!,
  maplist(print_reply_candc, OutF).

call_candc_server(English, OutS):-
  call_candc_server(English, [], OutS).

call_candc_server(English0, OptionsIn, OutS):-
  DefaultOpts = [ ],
   (OptionsIn=[]->Options=DefaultOpts;Options=OptionsIn),  
  % kbp,  % sentiment,
  ignore('.\nSome quick brown foxes jumped over the lazy dog. If we sang a song, X is Y.  Pee implies queue.'=English0), 
  any_to_string(English0,EnglishIn),
  ((false;atom_contains(EnglishIn,'<META>'))->English=EnglishIn;string_concat("<META>'Server'\n",EnglishIn,English)),
  into_canc_tokenized(English,PostData),
  %format('~N---->~w<----~n',[PostData]),
  % http_open([host(localhost), port(3090), post([PostData]), path(''), search([properties=For])], In, []),
  uri_encoded(query_value, PostData, Encoded), 
  atomic_list_concat(['https://logicmoo.org/doc2/logicmoocanc.php?candc_query=', Encoded,'&pos=0&boxer=0&showui=0'],URL),
  %format('~w',[URL]),
  http_get(URL, Reply, Options),
  format('~w',[Reply]),
  % print_reply_candc(Reply), print_reply_candc("==============================================================="),
  % maplist(wdmsg, Reply),
  must_or_rtrace(parse_reply([reply], Reply, Out)),
  flatten([Out], OutS),
  !.

zave_varname(N,V):- debug_var(N,V),!.
%zave_varname(N,V):- V = '$VAR'(N).

implode_varnames(Vs):- (var(Vs) ; Vs==[]),!.
implode_varnames([NV|Vs]) :- implode_varnames(Vs),
  (var(NV) -> ignore((variable_name(NV,Name),zave_varname(Name,NV))); 
  ignore((NV=(N=V),zave_varname(N,V)))).


read_term_list( NonStream, Out):- \+ is_stream(NonStream), !, % wdmsg(NonStream),
  must_or_rtrace((open_string(NonStream,Stream), read_term_list(Stream, Out))).

read_term_list(Stream, Out):-
 '$current_typein_module'(M),
  M\== input, !,
  setup_call_cleanup(
   '$set_typein_module'(input),
   read_term_list(Stream, Out),
   '$set_typein_module'(M)).

read_term_list(Stream, Out):-
 op(601, xfx, input:(/)),
 op(601, xfx, input:(\\)),
 (at_end_of_stream(Stream)-> Out=[]; 
  (catch(notrace(input:read_term(Stream, Term, [variable_names(Vs), module(input)])),E,(wdmsg(E),Term=error,throw(E))),
    (Term == end_of_file -> Out=[];
    ((notrace(implode_varnames(Vs)), % wdmsg(Term),
      (Term = (:- Exec) -> (input:call(Exec), Out=More) ; Out = [Term|More]),
       read_term_list(Stream, More)))))),!.

parse_reply(_Ctx, Stream, Out):- is_stream(Stream), !, must_or_rtrace(read_term_list(Stream, Out)).  

parse_reply(_Ctx, NonStream, Out):- \+ compound(NonStream), !, % wdmsg(NonStream),
  must_or_rtrace((open_string(NonStream,Stream), read_term_list(Stream, Out))).

parse_reply(Ctx, List, Out):- is_list(List),!, maplist(parse_reply(Ctx),List, Out).
parse_reply(Ctx, InnerCtx=json(List), Out):- !,  parse_reply([InnerCtx|Ctx], List, Out).
parse_reply(Ctx, List, Out):- 
   sub_term(Sub, List), nonvar(Sub), 
   parse_reply_replace(Ctx, Sub, NewSub),
   % ignore((NewSub=='$',wdmsg(parse_reply_replace(_Ctx, Sub, NewSub)))),
   nonvar(NewSub), Sub\==NewSub,
   subst(List, Sub, NewSub, NewList), 
   List\==NewList, !, 
   parse_reply(Ctx, NewList, Out).
parse_reply(_Ctx, List, Out):- flatten([List], Out),!.

parse_reply_replace(_Ctx, json(Replace), Replace):- nonvar(Replace),!.


join_atomics(Sep,List,Joined):- atomics_to_string(List,Sep,Joined).

into_canc_tokenized(Text,TokenizedText):- \+ string(Text),!, 
  any_to_string(Text,String), into_canc_tokenized(String,TokenizedText).
into_canc_tokenized(Text,TokenizedText):-
 split_string(Text, "", "\s\t\r\n", [L]), L\==Text,!,
 into_canc_tokenized(L,M), 
 %string_concat(M,"\n",TokenizedText).
 string_concat(M,"",TokenizedText).
into_canc_tokenized(Text,TokenizedText):-   L=[_S1,_S2|_SS],    
  member(Split,["\n'","'\n","<META>'","<META>","\n"]),  
  atomic_list_concat(L,Split,Text),  
  maplist(into_canc_tokenized,L,LO),
  atomics_to_string(LO,Split, TokenizedText).
into_canc_tokenized(Text,TokenizedText):-   
  split_string(Text, "\n", "\s\t\n\r",StringList),
  maplist(into_text80_atoms,StringList,SentenceList),
  maplist(join_atomics(' '),SentenceList,ListOfStrings),
  join_atomics('\n',ListOfStrings,TokenizedText),!.



test_candc_server:- call_candc_server("The Norwegian lives in the first house.").

test_candc_server:- call_candc_server(
'There are 5 houses with five different owners.
 These five owners drink a certain type of beverage, smoke a certain brand of cigar and keep a certain pet.
 No owners have the same pet, smoke the same brand of cigar or drink the same beverage.
 The man who smokes Blends has a neighbor who drinks water.
 A red cat fastly jumped onto the table which is in the kitchen of the house.
 After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau.
 Rydell was a big quiet Tennessean with a sad shy grin, cheap sunglasses, and a walkie-talkie screwed permanently into one ear.
 Concrete beams overhead had been hand-painted to vaguely resemble blond oak.
 The chairs, like the rest of the furniture in the Chateau\'s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale.
 Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.
 A book called, "A little tribute to Gibson".
 "You look like the cat that swallowed the canary, " he said, giving her a puzzled look.').


test_candc_server:- call_candc_server("Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize.").

test_candc_server:- call_candc_server(
".
The Brit lives in the red house.
The Swede keeps dogs as pets.
The Dane drinks tea.
The green house is on the immediate left of the white house.
The green house's owner drinks coffee.
The owner who smokes Pall Mall rears birds.
The owner of the yellow house smokes Dunhill.
The owner living in the center house drinks milk.
The Norwegian lives in the first house.
The owner who smokes Blends lives next to the one who keeps cats.
The owner who keeps the horse lives next to the one who smokes Dunhills.
The owner who smokes Bluemasters drinks beer.
The German smokes Prince.
The Norwegian lives next to the blue house.
The owner who smokes Blends lives next to the one who drinks water.").


:- system:import(test_candc_server/0).

:- user:call(op(0,xfx,'/')).
:- user:call(op(0,fx,'-')).
:- user:call(op(200,fy,'-')).
:- user:call(op(500,yfx,'-')).
:- user:call(op(600,xfy,':')).
:- user:call(op(400,yfx,'/')).




end_of_file.


andrewdo@ai2:/var/lib/myfrdcsa/codebases/internal/perllib/scripts$ ./test-system-candc-5.pl
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 7.03174 562 677
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   fa(np:nb,
    t(np:nb/n, "a", "a", "DT", "I-NP", "O"),
    fa(n,
     t(n/n, "red", "red", "JJ", "I-NP", "O"),
     t(n, "cat", "cat", "NN", "I-NP", "O"))),
   fa(s:dcl\\np,
    t((s:dcl\\np)/(s:dcl\\np), "fastly", "fastly", "RB", "I-ADVP", "O"),
    ba(s:dcl\\np,
     t(s:dcl\\np, "jumped", "jump", "VBD", "I-VP", "O"),
     fa((s:dcl\\np)\\(s:dcl\\np),
      t(((s:dcl\\np)\\(s:dcl\\np))/np, "onto", "onto", "IN", "I-PP", "O"),
      ba(np,
       fa(np:nb,
        t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
        t(n, "table", "table", "NN", "I-NP", "O")),
       fa(np\\np,
        t((np\\np)/(s:dcl\\np), "which", "which", "WDT", "B-NP", "O"),
        fa(s:dcl\\np,
         t((s:dcl\\np)/pp, "is", "be", "VBZ", "I-VP", "O"),
         fa(pp,
          t(pp/np, "in", "in", "IN", "I-PP", "O"),
          ba(np,
           fa(np:nb,
            t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
            t(n, "kitchen", "kitchen", "NN", "I-NP", "O")),
           fa(np\\np,
            t((np\\np)/np, "of", "of", "IN", "I-PP", "O"),
            fa(np:nb,
             t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
             t(n, "house", "house", "NN", "I-NP", "O")))))))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% a red cat fastly jumped onto the table which is in the kitchen of the house . 

sem(1,
    [word(1001, a), word(1002, red), word(1003, cat), word(1004, fastly), word(1005, jumped), word(1006, onto), word(1007, the), word(1008, table), word(1009, which), word(1010, is), word(1011, in), word(1012, the), word(1013, kitchen), word(1014, of), word(1015, the), word(1016, house), word(1017, ".")],
    [pos(1001, "DT"), pos(1002, "JJ"), pos(1003, "NN"), pos(1004, "RB"), pos(1005, "VBD"), pos(1006, "IN"), pos(1007, "DT"), pos(1008, "NN"), pos(1009, "WDT"), pos(1010, "VBZ"), pos(1011, "IN"), pos(1012, "DT"), pos(1013, "NN"), pos(1014, "IN"), pos(1015, "DT"), pos(1016, "NN"), pos(1017, ".")],
    [],
    smerge(drs([[1007]:A, [1012]:B, [1015]:C], [[1008]:pred(A, table, n, 0), [1013]:pred(B, kitchen, n, 0), [1016]:pred(C, house, n, 0)]), drs([[1001]:D, [1005]:E, [1010]:F], [[1002]:pred(D, red, a, 0), [1003]:pred(D, cat, n, 0), [1005]:pred(E, jump, v, 0), [1005]:rel(E, D, agent, 0), [1010]:pred(F, be, v, 0), [1010]:rel(F, A, agent, 0), [1014]:rel(B, C, of, 0), [1011]:rel(F, B, in, 0), []:pred(F, event, n, 1), [1004]:pred(E, fastly, a, 0), []:pred(E, event, n, 1), [1006]:rel(E, A, onto, 0)])) ).
%%%   _____________   ______________  
%%%  | x0 x1 x2    | | x3 x4 x5     | 
%%%  |-------------| |--------------| 
%%% (| table(x0)   |+| red(x3)      |)
%%%  | kitchen(x1) | | cat(x3)      | 
%%%  | house(x2)   | | jump(x4)     | 
%%%  |_____________| | agent(x4,x3) | 
%%%                  | be(x5)       | 
%%%                  | agent(x5,x0) | 
%%%                  | of(x1,x2)    | 
%%%                  | in(x5,x1)    | 
%%%                  | event(x5)    | 
%%%                  | fastly(x4)   | 
%%%                  | event(x4)    | 
%%%                  | onto(x4,x0)  | 
%%%                  |______________| 

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 10.1806 1444 2064
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 fa(s:dcl,
  fa(s:dcl/s:dcl,
   t((s:dcl/s:dcl)/np, "After", "after", "IN", "I-PP", "O"),
   lx(np, n,
    t(n, "Slitscan", "Slitscan", "NNP", "I-NP", "I-ORG"))),
  rp(s:dcl,
   lp(s:dcl,
    t(comma, ",", ",", ",", "O", "O"),
    ba(s:dcl,
     lx(np, n,
      t(n, "Laney", "Laney", "NNP", "I-NP", "I-PER")),
     fa(s:dcl\\np,
      t((s:dcl\\np)/pp, "heard", "hear", "VBD", "I-VP", "O"),
      fa(pp,
       t(pp/np, "about", "about", "IN", "I-PP", "O"),
       ba(np,
        ba(np,
         fa(np:nb,
          t(np:nb/n, "another", "another", "DT", "I-NP", "O"),
          t(n, "job", "job", "NN", "I-NP", "O")),
         fa(np\\np,
          t((np\\np)/np, "from", "from", "IN", "I-PP", "O"),
          lx(np, n,
           t(n, "Rydell", "Rydell", "NNP", "I-NP", "I-LOC")))),
        conj(np\\np, np,
         t(comma, ",", ",", ",", "O", "O"),
         ba(np,
          fa(np:nb,
           t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
           fa(n,
            t(n/n, "night", "night", "NN", "I-NP", "O"),
            fa(n,
             t(n/n, "security", "security", "NN", "I-NP", "O"),
             t(n, "man", "man", "NN", "I-NP", "O")))),
          fa(np\\np,
           t((np\\np)/np, "at", "at", "IN", "I-PP", "O"),
           fa(np:nb,
            t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
            t(n, "Chateau", "Chateau", "NNP", "I-NP", "I-LOC")))))))))),
   t(period, ".", ".", ".", "O", "O")))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% After Slitscan , Laney heard about another job from Rydell , the night security man at the Chateau . 

sem(1,
    [word(1001, "After"), word(1002, "Slitscan"), word(1003, (",")), word(1004, "Laney"), word(1005, heard), word(1006, about), word(1007, another), word(1008, job), word(1009, from), word(1010, "Rydell"), word(1011, (",")), word(1012, the), word(1013, night), word(1014, security), word(1015, man), word(1016, at), word(1017, the), word(1018, "Chateau"), word(1019, ".")],
    [pos(1001, "IN"), pos(1002, "NNP"), pos(1003, (",")), pos(1004, "NNP"), pos(1005, "VBD"), pos(1006, "IN"), pos(1007, "DT"), pos(1008, "NN"), pos(1009, "IN"), pos(1010, "NNP"), pos(1011, (",")), pos(1012, "DT"), pos(1013, "NN"), pos(1014, "NN"), pos(1015, "NN"), pos(1016, "IN"), pos(1017, "DT"), pos(1018, "NNP"), pos(1019, ".")],
    [ne(1002, "I-ORG"), ne(1004, "I-PER"), ne(1010, "I-LOC"), ne(1018, "I-LOC")],
    smerge(drs([[1002]:A, [1004]:B, []:C, [1010]:D, [1014]:E, [1013]:F, [1012]:G, [1017]:H], [[1002]:named(A, slitscan, org, 0), [1004]:named(B, laney, per, 0), [1008]:pred(C, job, n, 0), [1010]:named(D, rydell, loc, 0), [1014]:pred(E, security, n, 0), [1013]:pred(F, night, n, 0), [1015]:pred(G, man, n, 0), []:rel(E, G, nn, 0), []:rel(F, G, nn, 0), [1018]:named(H, chateau, loc, 0)]), drs([[1001]:I, []:J], [[1001]:pred(I, after, v, 0), [1001]:pred(J, proposition, n, 1), []:pred(I, event, n, 1), [1001]:rel(I, A, agent, 0), [1001]:rel(I, J, theme, 0), [1001]:prop(J, drs([[1005]:K, [1007]:L], [[1005]:pred(K, hear, v, 0), [1005]:rel(K, B, agent, 0), [1008]:pred(L, job, n, 0), [1007]:not(drs([], [[]:eq(L, C)])), [1009]:rel(L, D, from, 0), [1006]:rel(K, L, about, 0), [1016]:rel(G, H, at, 0), [1006]:rel(K, G, about, 0), []:pred(K, event, n, 1)]))])) ).
%%%   _________________________   ___________________________  
%%%  | x0 x1 x2 x3 x4 x5 x6 x7 | | x8 x9                     | 
%%%  |-------------------------| |---------------------------| 
%%% (| named(x0,slitscan,org)  |+| after(x8)                 |)
%%%  | named(x1,laney,per)     | | proposition(x9)           | 
%%%  | job(x2)                 | | event(x8)                 | 
%%%  | named(x3,rydell,loc)    | | agent(x8,x0)              | 
%%%  | security(x4)            | | theme(x8,x9)              | 
%%%  | night(x5)               | |       __________________  | 
%%%  | man(x6)                 | |      | x10 x11          | | 
%%%  | nn(x4,x6)               | |   x9:|------------------| | 
%%%  | nn(x5,x6)               | |      | hear(x10)        | | 
%%%  | named(x7,chateau,loc)   | |      | agent(x10,x1)    | | 
%%%  |_________________________| |      | job(x11)         | | 
%%%                              |      |      __________  | | 
%%%                              |      |     |          | | | 
%%%                              |      | __  |----------| | | 
%%%                              |      |   | | x11 = x2 | | | 
%%%                              |      |     |__________| | | 
%%%                              |      | from(x11,x3)     | | 
%%%                              |      | about(x10,x11)   | | 
%%%                              |      | at(x6,x7)        | | 
%%%                              |      | about(x10,x6)    | | 
%%%                              |      | event(x10)       | | 
%%%                              |      |__________________| | 
%%%                              |___________________________| 

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 13.801 3332 5053
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% Rydell was a big quiet Tennessean with a sad shy grin , cheap sunglasses , and a walkie-talkie screwed permanently into one ear . 

sem(1,
    [word(1001, "Rydell"), word(1002, was), word(1003, a), word(1004, big), word(1005, quiet), word(1006, "Tennessean"), word(1007, with), word(1008, a), word(1009, sad), word(1010, shy), word(1011, grin), word(1012, (",")), word(1013, cheap), word(1014, sunglasses), word(1015, (",")), word(1016, and), word(1017, a), word(1018, "walkie-talkie"), word(1019, screwed), word(1020, permanently), word(1021, into), word(1022, one), word(1023, ear), word(1024, ".")],
    [pos(1001, "NNP"), pos(1002, "VBD"), pos(1003, "DT"), pos(1004, "JJ"), pos(1005, "JJ"), pos(1006, "NN"), pos(1007, "IN"), pos(1008, "DT"), pos(1009, "JJ"), pos(1010, "JJ"), pos(1011, "NN"), pos(1012, (",")), pos(1013, "JJ"), pos(1014, "NNS"), pos(1015, (",")), pos(1016, "CC"), pos(1017, "DT"), pos(1018, "NN"), pos(1019, "VBD"), pos(1020, "RB"), pos(1021, "IN"), pos(1022, "CD"), pos(1023, "NN"), pos(1024, ".")],
    [ne(1001, "I-PER")],
    smerge(drs([[1001]:A, [1003]:B, [1008]:C, [1002]:D, [1013, 1014]:E, [1002]:F], [[1001]:named(A, rydell, per, 0), [1004]:pred(B, big, a, 0), [1005]:pred(B, quiet, a, 0), [1006]:pred(B, tennessean, n, 0), [1009]:pred(C, sad, a, 0), [1010]:pred(C, shy, a, 0), [1011]:pred(C, grin, n, 0), []:pred(D, event, n, 1), [1013]:pred(E, cheap, a, 0), [1014]:pred(E, sunglass, n, 0), []:pred(F, event, n, 1), [1007]:rel(B, C, with, 0), [1007]:rel(B, E, with, 0), [1002]:prop(D, drs([], [[1002]:eq(A, B)])), [1002]:prop(F, drs([], [[1002]:eq(A, B)]))]), drs([[1017]:G, [1019]:H, [1022, 1023]:I], [[1022]:card(I, 1, ge), [1018]:pred(G, "walkie-talkie", n, 0), [1019]:pred(H, screw, v, 0), [1023]:pred(I, ear, n, 0), [1020]:pred(H, permanently, a, 0), []:pred(H, event, n, 1), [1019]:rel(H, G, agent, 0), [1021]:rel(H, I, into, 0)])) ).
%%%   ______________________   ___________________  
%%%  | x0 x1 x2 x3 x4 x5    | | x6 x7 x8          | 
%%%  |----------------------| |-------------------| 
%%% (| named(x0,rydell,per) |+| |x8| >= 1         |)
%%%  | big(x1)              | | walkie-talkie(x6) | 
%%%  | quiet(x1)            | | screw(x7)         | 
%%%  | tennessean(x1)       | | ear(x8)           | 
%%%  | sad(x2)              | | permanently(x7)   | 
%%%  | shy(x2)              | | event(x7)         | 
%%%  | grin(x2)             | | agent(x7,x6)      | 
%%%  | event(x3)            | | into(x7,x8)       | 
%%%  | cheap(x4)            | |___________________| 
%%%  | sunglass(x4)         |                       
%%%  | event(x5)            |                       
%%%  | with(x1,x2)          |                       
%%%  | with(x1,x4)          |                       
%%%  |       _________      |                       
%%%  |      |         |     |                       
%%%  |   x3:|---------|     |                       
%%%  |      | x0 = x1 |     |                       
%%%  |      |_________|     |                       
%%%  |       _________      |                       
%%%  |      |         |     |                       
%%%  |   x5:|---------|     |                       
%%%  |      | x0 = x1 |     |                       
%%%  |      |_________|     |                       
%%%  |______________________|                       

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   ba(s:dcl,
    lx(np, n,
     t(n, "Rydell", "Rydell", "NNP", "I-NP", "I-PER")),
    fa(s:dcl\\np,
     t((s:dcl\\np)/np, "was", "be", "VBD", "I-VP", "O"),
     ba(np,
      fa(np:nb,
       t(np:nb/n, "a", "a", "DT", "I-NP", "O"),
       fa(n,
        t(n/n, "big", "big", "JJ", "I-NP", "O"),
        fa(n,
         t(n/n, "quiet", "quiet", "JJ", "I-NP", "O"),
         t(n, "Tennessean", "tennessean", "NN", "I-NP", "O")))),
      fa(np\\np,
       t((np\\np)/np, "with", "with", "IN", "I-PP", "O"),
       ba(np,
        fa(np:nb,
         t(np:nb/n, "a", "a", "DT", "I-NP", "O"),
         fa(n,
          t(n/n, "sad", "sad", "JJ", "I-NP", "O"),
          fa(n,
           t(n/n, "shy", "shy", "JJ", "I-NP", "O"),
           t(n, "grin", "grin", "NN", "I-NP", "O")))),
        conj(np\\np, np,
         t(comma, ",", ",", ",", "O", "O"),
         lx(np, n,
          fa(n,
           t(n/n, "cheap", "cheap", "JJ", "I-NP", "O"),
           t(n, "sunglasses", "sunglass", "NNS", "I-NP", "O"))))))))),
   lp(s:dcl\\s:dcl,
    t(comma, ",", ",", ",", "O", "O"),
    conj(s:dcl\\s:dcl, s:dcl,
     t(conj, "and", "and", "CC", "O", "O"),
     ba(s:dcl,
      fa(np:nb,
       t(np:nb/n, "a", "a", "DT", "I-NP", "O"),
       t(n, "walkie-talkie", "walkie-talkie", "NN", "I-NP", "O")),
      fa(s:dcl\\np,
       bxc((s:dcl\\np)/pp,
        t((s:dcl\\np)/pp, "screwed", "screw", "VBD", "I-VP", "O"),
        t((s:dcl\\np)\\(s:dcl\\np), "permanently", "permanently", "RB", "I-ADVP", "O")),
       fa(pp,
        t(pp/np, "into", "into", "IN", "I-PP", "O"),
        lx(np, n,
         fa(n,
          t(n/n, "one", "one", "CD", "I-NP", "O"),
          t(n, "ear", "ear", "NN", "I-NP", "O"))))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 9.201 336 460
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% Concrete beams overhead had been hand-painted to vaguely resemble blond oak . 

sem(1,
    [word(1001, "Concrete"), word(1002, beams), word(1003, overhead), word(1004, had), word(1005, been), word(1006, "hand-painted"), word(1007, to), word(1008, vaguely), word(1009, resemble), word(1010, blond), word(1011, oak), word(1012, ".")],
    [pos(1001, "NNP"), pos(1002, "VBZ"), pos(1003, "NN"), pos(1004, "VBD"), pos(1005, "VBN"), pos(1006, "JJ"), pos(1007, "TO"), pos(1008, "RB"), pos(1009, "VB"), pos(1010, "JJ"), pos(1011, "NN"), pos(1012, ".")],
    [],
    smerge(drs([[1001]:A], [[1001]:named(A, concrete, nam, 0)]), drs([[1002]:B, []:C], [[1002]:pred(B, beam, v, 0), [1002]:pred(C, proposition, n, 1), []:pred(B, event, n, 1), [1002]:rel(B, A, agent, 0), [1002]:rel(B, C, theme, 0), [1002]:prop(C, drs([[1003]:D, [1006]:E, []:F], [[1003]:pred(D, overhead, n, 0), [1006]:pred(F, proposition, n, 1), []:pred(E, event, n, 1), [1006]:rel(E, F, theme, 0), [1006]:prop(E, drs([], [[1006]:pred(D, "hand-painted", a, 0)])), [1006]:prop(F, drs([[1010, 1011]:G, [1009]:H], [[1010]:pred(G, blond, a, 0), [1011]:pred(G, oak, n, 0), [1009]:pred(H, resemble, v, 0), [1008]:pred(H, vaguely, a, 0), []:pred(H, event, n, 1), [1009]:rel(H, D, agent, 0), [1009]:rel(H, G, patient, 0)]))]))])) ).
%%%   ________________________   ____________________________________  
%%%  | x0                     | | x1 x2                              | 
%%%  |------------------------| |------------------------------------| 
%%% (| named(x0,concrete,nam) |+| beam(x1)                           |)
%%%  |________________________| | proposition(x2)                    | 
%%%                             | event(x1)                          | 
%%%                             | agent(x1,x0)                       | 
%%%                             | theme(x1,x2)                       | 
%%%                             |       ___________________________  | 
%%%                             |      | x3 x4 x5                  | | 
%%%                             |   x2:|---------------------------| | 
%%%                             |      | overhead(x3)              | | 
%%%                             |      | proposition(x5)           | | 
%%%                             |      | event(x4)                 | | 
%%%                             |      | theme(x4,x5)              | | 
%%%                             |      |       __________________  | | 
%%%                             |      |      |                  | | | 
%%%                             |      |   x4:|------------------| | | 
%%%                             |      |      | hand-painted(x3) | | | 
%%%                             |      |      |__________________| | | 
%%%                             |      |       ________________    | | 
%%%                             |      |      | x6 x7          |   | | 
%%%                             |      |   x5:|----------------|   | | 
%%%                             |      |      | blond(x6)      |   | | 
%%%                             |      |      | oak(x6)        |   | | 
%%%                             |      |      | resemble(x7)   |   | | 
%%%                             |      |      | vaguely(x7)    |   | | 
%%%                             |      |      | event(x7)      |   | | 
%%%                             |      |      | agent(x7,x3)   |   | | 
%%%                             |      |      | patient(x7,x6) |   | | 
%%%                             |      |      |________________|   | | 
%%%                             |      |___________________________| | 
%%%                             |____________________________________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   lx(np, n,
    t(n, "Concrete", "Concrete", "NNP", "I-NP", "O")),
   fa(s:dcl\\np,
    t((s:dcl\\np)/s:dcl, "beams", "beam", "VBZ", "I-VP", "O"),
    ba(s:dcl,
     lx(np, n,
      t(n, "overhead", "overhead", "NN", "I-NP", "O")),
     fa(s:dcl\\np,
      t((s:dcl\\np)/(s:pt\\np), "had", "have", "VBD", "I-VP", "O"),
      fa(s:pt\\np,
       t((s:pt\\np)/(s:adj\\np), "been", "be", "VBN", "I-VP", "O"),
       fa(s:adj\\np,
        t((s:adj\\np)/(s:to\\np), "hand-painted", "hand-painted", "JJ", "I-ADJP", "O"),
        fa(s:to\\np,
         bxc((s:to\\np)/(s:b\\np),
          t((s:to\\np)/(s:b\\np), "to", "to", "TO", "I-VP", "O"),
          t((s:to\\np)\\(s:to\\np), "vaguely", "vaguely", "RB", "I-VP", "O")),
         fa(s:b\\np,
          t((s:b\\np)/np, "resemble", "resemble", "VB", "I-VP", "O"),
          lx(np, n,
           fa(n,
            t(n/n, "blond", "blond", "JJ", "I-NP", "O"),
            t(n, "oak", "oak", "NN", "I-NP", "O"))))))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 15.1619 2902 4964
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 ba(s:dcl,
  fa(np:nb,
   t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
   t(n, "chairs", "chair", "NNS", "I-NP", "O")),
  lp(s:dcl\\np,
   t(comma, ",", ",", ",", "O", "O"),
   fa(s:dcl\\np,
    fa((s:dcl\\np)/(s:dcl\\np),
     t(((s:dcl\\np)/(s:dcl\\np))/np, "like", "like", "IN", "I-PP", "O"),
     ba(np,
      fa(np:nb,
       t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
       t(n, "rest", "rest", "NN", "I-NP", "O")),
      fa(np\\np,
       t((np\\np)/np, "of", "of", "IN", "I-PP", "O"),
       ba(np,
        fa(np:nb,
         t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
         t(n, "furniture", "furniture", "NN", "I-NP", "O")),
        fa(np\\np,
         t((np\\np)/np, "in", "in", "IN", "I-PP", "O"),
         fa(np:nb,
          ba(np:nb/n,
           fa(np:nb,
            t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
            t(n, "Chateau", "Chateau", "NNP", "I-NP", "I-LOC")),
           t((np:nb/n)\\np, "'s", "'s", "POS", "B-NP", "O")),
          t(n, "lobby", "lobby", "NN", "I-NP", "O"))))))),
    lp(s:dcl\\np,
     t(comma, ",", ",", ",", "O", "O"),
     fa(s:dcl\\np,
      t((s:dcl\\np)/(s:pss\\np), "were", "be", "VBD", "I-VP", "O"),
      ba(s:pss\\np,
       t(s:pss\\np, "oversized", "oversize", "VBN", "I-VP", "O"),
       fa((s:pss\\np)\\(s:pss\\np),
        t(((s:pss\\np)\\(s:pss\\np))/np, "to", "to", "TO", "I-PP", "O"),
        fa(np:nb,
         t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
         fa(n,
          t(n/s:em, "extent", "extent", "NN", "I-NP", "O"),
          fa(s:em,
           t(s:em/s:dcl, "that", "that", "IN", "I-SBAR", "O"),
           rp(s:dcl,
            ba(s:dcl,
             fa(np,
              t(np/(s:dcl\\np), "whoever", "whoever", "WP", "I-NP", "O"),
              ba(s:dcl\\np,
               t(s:dcl\\np, "sat", "sit", "VBD", "I-VP", "O"),
               fa((s:dcl\\np)\\(s:dcl\\np),
                t(((s:dcl\\np)\\(s:dcl\\np))/np, "in", "in", "IN", "I-PP", "O"),
                t(np, "them", "they", "PRP", "I-NP", "O")))),
             fa(s:dcl\\np,
              t((s:dcl\\np)/(s:pss\\np), "seemed", "seem", "VBD", "I-VP", "O"),
              fa(s:pss\\np,
               t((s:pss\\np)/pp, "built", "build", "VBN", "I-VP", "O"),
               fa(pp,
                t(pp/np, "to", "to", "TO", "I-PP", "O"),
                fa(np:nb,
                 t(np:nb/n, "a", "a", "DT", "I-NP", "O"),
                 fa(n,
                  t(n/n, "smaller", "smaller", "JJR", "I-NP", "O"),
                  t(n, "scale", "scale", "NN", "I-NP", "O"))))))),
            t(period, ".", ".", ".", "O", "O"))))))))))))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The chairs , like the rest of the furniture in the Chateau "s lobby , were oversized to the extent that whoever sat in them seemed built to a smaller scale . 

sem(1,
    [word(1001, "The"), word(1002, chairs), word(1003, (",")), word(1004, like), word(1005, the), word(1006, rest), word(1007, of), word(1008, the), word(1009, furniture), word(1010, in), word(1011, the), word(1012, "Chateau"), word(1013, "'s"), word(1014, lobby), word(1015, (",")), word(1016, were), word(1017, oversized), word(1018, to), word(1019, the), word(1020, extent), word(1021, that), word(1022, whoever), word(1023, sat), word(1024, in), word(1025, them), word(1026, seemed), word(1027, built), word(1028, to), word(1029, a), word(1030, smaller), word(1031, scale), word(1032, ".")],
    [pos(1001, "DT"), pos(1002, "NNS"), pos(1003, (",")), pos(1004, "IN"), pos(1005, "DT"), pos(1006, "NN"), pos(1007, "IN"), pos(1008, "DT"), pos(1009, "NN"), pos(1010, "IN"), pos(1011, "DT"), pos(1012, "NNP"), pos(1013, "POS"), pos(1014, "NN"), pos(1015, (",")), pos(1016, "VBD"), pos(1017, "VBN"), pos(1018, "TO"), pos(1019, "DT"), pos(1020, "NN"), pos(1021, "IN"), pos(1022, "WP"), pos(1023, "VBD"), pos(1024, "IN"), pos(1025, "PRP"), pos(1026, "VBD"), pos(1027, "VBN"), pos(1028, "TO"), pos(1029, "DT"), pos(1030, "JJR"), pos(1031, "NN"), pos(1032, ".")],
    [ne(1012, "I-LOC")],
    smerge(drs([[1001]:A, [1025]:B, [1019]:C, [1005]:D, [1008]:E, [1011]:F, [1013]:G], [[1002]:pred(A, chair, n, 0), [1025]:pred(B, thing, n, 12), [1020]:pred(C, extent, n, 0), [1022]:whq([], drs([[1022]:H], [[1022]:pred(H, thing, n, 12)]), H, drs([[1023]:I, [1027]:J, [1029]:K], [[1023]:pred(I, sit, v, 0), [1023]:rel(I, H, agent, 0), []:pred(I, event, n, 1), [1024]:rel(I, B, in, 0), [1027]:pred(J, build, v, 0), [1030]:pred(K, smaller, a, 0), [1031]:pred(K, scale, n, 0), [1027]:rel(J, H, patient, 0), [1028]:rel(J, K, to, 0), [1020]:rel(J, C, rel, 0)])), [1006]:pred(D, rest, n, 0), [1009]:pred(E, furniture, n, 0), [1012]:named(F, chateau, loc, 0), [1014]:pred(G, lobby, n, 0), [1013]:rel(G, F, of, 0)]), drs([[1017]:L], [[1017]:pred(L, oversize, v, 0), [1017]:rel(L, A, patient, 0), [1018]:rel(L, C, to, 0), []:pred(L, event, n, 1), [1010]:rel(E, G, in, 0), [1007]:rel(D, E, of, 0), [1004]:rel(L, D, like, 0)])) ).
%%%   ____________________________________   _________________  
%%%  | x0 x1 x2 x3 x4 x5 x6               | | x11             | 
%%%  |------------------------------------| |-----------------| 
%%% (| chair(x0)                          |+| oversize(x11)   |)
%%%  | thing(x1)                          | | patient(x11,x0) | 
%%%  | extent(x2)                         | | to(x11,x2)      | 
%%%  |  ___________     ________________  | | event(x11)      | 
%%%  | | x7        |   | x8 x9 x10      | | | in(x4,x6)       | 
%%%  | |-----------|   |----------------| | | of(x3,x4)       | 
%%%  | | thing(x7) | ? | sit(x8)        | | | like(x11,x3)    | 
%%%  | |___________|   | agent(x8,x7)   | | |_________________| 
%%%  |                 | event(x8)      | |                     
%%%  |                 | in(x8,x1)      | |                     
%%%  |                 | build(x9)      | |                     
%%%  |                 | smaller(x10)   | |                     
%%%  |                 | scale(x10)     | |                     
%%%  |                 | patient(x9,x7) | |                     
%%%  |                 | to(x9,x10)     | |                     
%%%  |                 | rel(x9,x2)     | |                     
%%%  |                 |________________| |                     
%%%  | rest(x3)                           |                     
%%%  | furniture(x4)                      |                     
%%%  | named(x5,chateau,loc)              |                     
%%%  | lobby(x6)                          |                     
%%%  | of(x6,x5)                          |                     
%%%  |____________________________________|                     

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 15.9775 2682 4412
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup , as though he were hoping to find a secret prize . 

sem(1,
    [word(1001, "Rydell"), word(1002, used), word(1003, his), word(1004, straw), word(1005, to), word(1006, stir), word(1007, the), word(1008, foam), word(1009, and), word(1010, ice), word(1011, remaining), word(1012, at), word(1013, the), word(1014, bottom), word(1015, of), word(1016, his), word(1017, tall), word(1018, plastic), word(1019, cup), word(1020, (",")), word(1021, as), word(1022, though), word(1023, he), word(1024, were), word(1025, hoping), word(1026, to), word(1027, find), word(1028, a), word(1029, secret), word(1030, prize), word(1031, ".")],
    [pos(1001, "NNP"), pos(1002, "VBD"), pos(1003, "PRP$"), pos(1004, "NN"), pos(1005, "TO"), pos(1006, "VB"), pos(1007, "DT"), pos(1008, "NN"), pos(1009, "CC"), pos(1010, "NN"), pos(1011, "VBG"), pos(1012, "IN"), pos(1013, "DT"), pos(1014, "NN"), pos(1015, "IN"), pos(1016, "PRP$"), pos(1017, "JJ"), pos(1018, "NN"), pos(1019, "NN"), pos(1020, (",")), pos(1021, "IN"), pos(1022, "IN"), pos(1023, "PRP"), pos(1024, "VBD"), pos(1025, "VBG"), pos(1026, "TO"), pos(1027, "VB"), pos(1028, "DT"), pos(1029, "JJ"), pos(1030, "NN"), pos(1031, ".")],
    [ne(1001, "I-PER")],
    smerge(drs([[1001]:A, [1003]:B, [1007]:C, [1013]:D, [1018]:E, [1016]:F], [[1003, 1016, 1023]:pred(A, male, a, 0), [1001]:named(A, rydell, per, 0), [1004]:pred(B, straw, n, 0), [1003]:rel(B, A, of, 0), [1008]:pred(C, foam, n, 0), [1010]:pred(C, ice, n, 0), [1014]:pred(D, bottom, n, 0), [1018]:pred(E, plastic, n, 0), [1017]:pred(F, tall, a, 0), [1019]:pred(F, cup, n, 0), []:rel(E, F, nn, 0), [1016]:rel(F, A, of, 0)]), drs([[1002]:G, []:H, [1021]:I], [[1002]:pred(G, use, v, 0), [1002]:pred(H, proposition, n, 1), [1021]:pred(I, proposition, n, 1), []:pred(G, event, n, 1), [1002]:rel(G, B, patient, 0), [1002]:rel(G, A, agent, 0), [1002]:rel(G, H, theme, 0), [1021]:rel(G, I, as, 0), [1002]:prop(H, drs([[1011]:J, [1006]:K], [[1011]:pred(J, remain, v, 0), [1011]:rel(J, C, agent, 0), []:pred(J, event, n, 1), [1015]:rel(D, F, of, 0), [1012]:rel(J, D, at, 0), [1006]:pred(K, stir, v, 0), []:pred(K, event, n, 1), [1006]:rel(K, B, agent, 0), [1006]:rel(K, C, patient, 0)])), [1021]:prop(I, drs([[1025]:L, []:M], [[1025]:pred(L, hope, v, 0), [1025]:pred(M, proposition, n, 1), []:pred(L, event, n, 1), [1025]:rel(L, A, agent, 0), [1025]:rel(L, M, theme, 0), [1025]:prop(M, drs([[1028]:N, [1027]:O], [[1029]:pred(N, secret, a, 0), [1030]:pred(N, prize, n, 0), [1027]:pred(O, find, v, 0), []:pred(O, event, n, 1), [1027]:rel(O, A, agent, 0), [1027]:rel(O, N, patient, 0)]))]))])) ).
%%%   ______________________   ____________________________________  
%%%  | x0 x1 x2 x3 x4 x5    | | x6 x7 x8                           | 
%%%  |----------------------| |------------------------------------| 
%%% (| male(x0)             |+| use(x6)                            |)
%%%  | named(x0,rydell,per) | | proposition(x7)                    | 
%%%  | straw(x1)            | | proposition(x8)                    | 
%%%  | of(x1,x0)            | | event(x6)                          | 
%%%  | foam(x2)             | | patient(x6,x1)                     | 
%%%  | ice(x2)              | | agent(x6,x0)                       | 
%%%  | bottom(x3)           | | theme(x6,x7)                       | 
%%%  | plastic(x4)          | | as(x6,x8)                          | 
%%%  | tall(x5)             | |       _________________            | 
%%%  | cup(x5)              | |      | x9 x10          |           | 
%%%  | nn(x4,x5)            | |   x7:|-----------------|           | 
%%%  | of(x5,x0)            | |      | remain(x9)      |           | 
%%%  |______________________| |      | agent(x9,x2)    |           | 
%%%                           |      | event(x9)       |           | 
%%%                           |      | of(x3,x5)       |           | 
%%%                           |      | at(x9,x3)       |           | 
%%%                           |      | stir(x10)       |           | 
%%%                           |      | event(x10)      |           | 
%%%                           |      | agent(x10,x1)   |           | 
%%%                           |      | patient(x10,x2) |           | 
%%%                           |      |_________________|           | 
%%%                           |       ___________________________  | 
%%%                           |      | x11 x12                   | | 
%%%                           |   x8:|---------------------------| | 
%%%                           |      | hope(x11)                 | | 
%%%                           |      | proposition(x12)          | | 
%%%                           |      | event(x11)                | | 
%%%                           |      | agent(x11,x0)             | | 
%%%                           |      | theme(x11,x12)            | | 
%%%                           |      |       __________________  | | 
%%%                           |      |      | x13 x14          | | | 
%%%                           |      |  x12:|------------------| | | 
%%%                           |      |      | secret(x13)      | | | 
%%%                           |      |      | prize(x13)       | | | 
%%%                           |      |      | find(x14)        | | | 
%%%                           |      |      | event(x14)       | | | 
%%%                           |      |      | agent(x14,x0)    | | | 
%%%                           |      |      | patient(x14,x13) | | | 
%%%                           |      |      |__________________| | | 
%%%                           |      |___________________________| | 
%%%                           |____________________________________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   lx(np, n,
    t(n, "Rydell", "Rydell", "NNP", "I-NP", "I-PER")),
   ba(s:dcl\\np,
    rp(s:dcl\\np,
     fa(s:dcl\\np,
      fa((s:dcl\\np)/(s:to\\np),
       t(((s:dcl\\np)/(s:to\\np))/np, "used", "use", "VBD", "I-VP", "O"),
       fa(np:nb,
        t(np:nb/n, "his", "his", "PRP$", "I-NP", "O"),
        t(n, "straw", "straw", "NN", "I-NP", "O"))),
      fa(s:to\\np,
       t((s:to\\np)/(s:b\\np), "to", "to", "TO", "I-VP", "O"),
       fa(s:b\\np,
        t((s:b\\np)/np, "stir", "stir", "VB", "I-VP", "O"),
        ba(np,
         fa(np:nb,
          t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
          ba(n,
           t(n, "foam", "foam", "NN", "I-NP", "O"),
           conj(n\\n, n,
            t(conj, "and", "and", "CC", "I-NP", "O"),
            t(n, "ice", "ice", "NN", "I-NP", "O")))),
         lx(np\\np, s:ng\\np,
          ba(s:ng\\np,
           t(s:ng\\np, "remaining", "remain", "VBG", "I-NP", "O"),
           fa((s:ng\\np)\\(s:ng\\np),
            t(((s:ng\\np)\\(s:ng\\np))/np, "at", "at", "IN", "I-PP", "O"),
            ba(np,
             fa(np:nb,
              t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
              t(n, "bottom", "bottom", "NN", "I-NP", "O")),
             fa(np\\np,
              t((np\\np)/np, "of", "of", "IN", "I-PP", "O"),
              fa(np:nb,
               t(np:nb/n, "his", "his", "PRP$", "I-NP", "O"),
               fa(n,
                t(n/n, "tall", "tall", "JJ", "I-NP", "O"),
                fa(n,
                 t(n/n, "plastic", "plastic", "NN", "I-NP", "O"),
                 t(n, "cup", "cup", "NN", "I-NP", "O"))))))))))))),
     t(comma, ",", ",", ",", "O", "O")),
    fa((s:dcl\\np)\\(s:dcl\\np),
     t(((s:dcl\\np)\\(s:dcl\\np))/s:poss, "as", "as", "IN", "I-SBAR", "O"),
     fa(s:poss,
      t(s:poss/s:dcl, "though", "though", "IN", "I-SBAR", "O"),
      ba(s:dcl,
       t(np, "he", "he", "PRP", "I-NP", "O"),
       fa(s:dcl\\np,
        t((s:dcl\\np)/(s:ng\\np), "were", "be", "VBD", "I-VP", "O"),
        fa(s:ng\\np,
         t((s:ng\\np)/(s:to\\np), "hoping", "hope", "VBG", "I-VP", "O"),
         fa(s:to\\np,
          t((s:to\\np)/(s:b\\np), "to", "to", "TO", "I-VP", "O"),
          fa(s:b\\np,
           t((s:b\\np)/np, "find", "find", "VB", "I-VP", "O"),
           fa(np:nb,
            t(np:nb/n, "a", "a", "DT", "I-NP", "O"),
            fa(n,
             t(n/n, "secret", "secret", "JJ", "I-NP", "O"),
             t(n, "prize", "prize", "NN", "I-NP", "O")))))))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 attempt nospan at B=0.075, K=20
1 parsed at B=0.03, K=20
1 coverage 100%
1 stats 18.65 7118 15807
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 ba(np,
  lp(np,
   t(lqu, "``", "``", "LQU", "O", "O"),
   ba(np,
    fa(np:nb,
     t(np:nb/n, "A", "a", "DT", "I-NP", "O"),
     fa(n,
      t(n/n, "little", "little", "JJ", "I-NP", "O"),
      t(n, "tribute", "tribute", "NN", "I-NP", "O"))),
    fa(np\\np,
     t((np\\np)/np, "to", "to", "TO", "I-PP", "O"),
     rp(np,
      lx(np, n,
       t(n, "Gibson", "Gibson", "NNP", "I-NP", "I-PER")),
      t(rqu, "\\"\\"", "\\"\\"", "RQU", "O", "O"))))),
  lx(np\\np, s:dcl,
   rp(s:dcl,
    ba(s:dcl,
     t(np, "You", "you", "PRP", "I-NP", "O"),
     ba(s:dcl\\np,
      fa(s:dcl\\np,
       t((s:dcl\\np)/pp, "look", "look", "VBP", "I-VP", "O"),
       fa(pp,
        t(pp/np, "like", "like", "IN", "I-PP", "O"),
        ba(np,
         fa(np:nb,
          t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
          t(n, "cat", "cat", "NN", "I-NP", "O")),
         fa(np\\np,
          t((np\\np)/(s:dcl\\np), "that", "that", "WDT", "B-NP", "O"),
          ba(s:dcl\\np,
           fa(s:dcl\\np,
            t((s:dcl\\np)/np, "swallowed", "swallow", "VBD", "I-VP", "O"),
            fa(np:nb,
             t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
             t(n, "canary", "canary", "JJ", "I-NP", "O"))),
           lp((s:dcl\\np)\\(s:dcl\\np),
            t(comma, ",", ",", ",", "O", "O"),
            rtc((s:dcl\\np)\\(s:dcl\\np),
             fc(s:dcl/s:dcl,
              tr(s:dcl/(s:dcl\\np),
               lp(np,
                t(lqu, "``", "``", "LQU", "O", "O"),
                t(np, "he", "he", "PRP", "I-NP", "O"))),
              t((s:dcl\\np)/s:dcl, "said", "say", "VBD", "I-VP", "O")),
             t(comma, ",", ",", ",", "O", "O")))))))),
      lx((s:dcl\\np)\\(s:dcl\\np), s:ng\\np,
       fa(s:ng\\np,
        fa((s:ng\\np)/np,
         t(((s:ng\\np)/np)/np, "giving", "give", "VBG", "I-VP", "O"),
         lx(np, n,
          t(n, "her", "her", "PRP$", "I-NP", "O"))),
        fa(np:nb,
         t(np:nb/n, "a", "a", "DT", "B-NP", "O"),
         fa(n,
          t(n/n, "puzzled", "puzzled", "JJ", "I-VP", "O"),
          t(n, "look", "look", "NN", "I-NP", "O"))))))),
    t(period, ".", ".", ".", "O", "O"))))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% `` A little tribute to Gibson "" You look like the cat that swallowed the canary , `` he said , giving her a puzzled look . 

sem(1,
    [word(1001, ``), word(1002, "A"), word(1003, little), word(1004, tribute), word(1005, to), word(1006, "Gibson"), word(1007, "\\"\\""), word(1008, "You"), word(1009, look), word(1010, like), word(1011, the), word(1012, cat), word(1013, that), word(1014, swallowed), word(1015, the), word(1016, canary), word(1017, (",")), word(1018, ``), word(1019, he), word(1020, said), word(1021, (",")), word(1022, giving), word(1023, her), word(1024, a), word(1025, puzzled), word(1026, look), word(1027, ".")],
    [pos(1001, "LQU"), pos(1002, "DT"), pos(1003, "JJ"), pos(1004, "NN"), pos(1005, "TO"), pos(1006, "NNP"), pos(1007, "RQU"), pos(1008, "PRP"), pos(1009, "VBP"), pos(1010, "IN"), pos(1011, "DT"), pos(1012, "NN"), pos(1013, "WDT"), pos(1014, "VBD"), pos(1015, "DT"), pos(1016, "JJ"), pos(1017, (",")), pos(1018, "LQU"), pos(1019, "PRP"), pos(1020, "VBD"), pos(1021, (",")), pos(1022, "VBG"), pos(1023, "PRP$"), pos(1024, "DT"), pos(1025, "JJ"), pos(1026, "NN"), pos(1027, ".")],
    [ne(1006, "I-PER")],
    smerge(drs([[1006]:A, [1008]:B, [1011]:C, [1015]:D], [[1019]:pred(A, male, a, 0), [1006]:named(A, gibson, per, 0), [1008]:pred(B, person, n, 1), [1012]:pred(C, cat, n, 0), [1016]:pred(D, canary, n, 0)]), drs([[1002]:E, [1009]:F, [1020]:G, []:H, [1022, 1024, 1025, 1026]:I, [1024]:J, [1023]:K, [1022]:L], [[1003]:pred(E, little, a, 0), [1004]:pred(E, tribute, n, 0), []:pred(E, topic, a, 1), [1005]:rel(E, A, to, 0), [1009]:pred(F, look, v, 0), [1009]:rel(F, B, agent, 0), [1020]:pred(G, say, v, 0), [1020]:pred(H, proposition, n, 1), []:pred(G, event, n, 1), [1020]:rel(G, A, agent, 0), [1020]:rel(G, H, theme, 0), [1020]:prop(H, drs([[1014]:M], [[1014]:pred(M, swallow, v, 0), []:pred(M, event, n, 1), [1014]:rel(M, C, agent, 0), [1014]:rel(M, D, patient, 0)])), [1010]:rel(F, C, like, 0), []:pred(F, event, n, 1), []:rel(F, E, rel, 0), [1025]:pred(J, puzzled, a, 0), [1026]:pred(J, look, n, 0), [1023]:pred(K, her, n, 0), [1022]:pred(L, give, v, 0), []:pred(L, event, n, 1), [1022]:rel(L, I, agent, 0), [1022]:rel(L, J, recipient, 0), [1022]:rel(L, K, theme, 0)])) ).
%%%   ______________________   ___________________________  
%%%  | x0 x1 x2 x3          | | x4 x5 x6 x7 x8 x9 x10 x11 | 
%%%  |----------------------| |---------------------------| 
%%% (| male(x0)             |+| little(x4)                |)
%%%  | named(x0,gibson,per) | | tribute(x4)               | 
%%%  | person(x1)           | | topic(x4)                 | 
%%%  | cat(x2)              | | to(x4,x0)                 | 
%%%  | canary(x3)           | | look(x5)                  | 
%%%  |______________________| | agent(x5,x1)              | 
%%%                           | say(x6)                   | 
%%%                           | proposition(x7)           | 
%%%                           | event(x6)                 | 
%%%                           | agent(x6,x0)              | 
%%%                           | theme(x6,x7)              | 
%%%                           |       _________________   | 
%%%                           |      | x12             |  | 
%%%                           |   x7:|-----------------|  | 
%%%                           |      | swallow(x12)    |  | 
%%%                           |      | event(x12)      |  | 
%%%                           |      | agent(x12,x2)   |  | 
%%%                           |      | patient(x12,x3) |  | 
%%%                           |      |_________________|  | 
%%%                           | like(x5,x2)               | 
%%%                           | event(x5)                 | 
%%%                           | rel(x5,x4)                | 
%%%                           | puzzled(x9)               | 
%%%                           | look(x9)                  | 
%%%                           | her(x10)                  | 
%%%                           | give(x11)                 | 
%%%                           | event(x11)                | 
%%%                           | agent(x11,x8)             | 
%%%                           | recipient(x11,x9)         | 
%%%                           | theme(x11,x10)            | 
%%%                           |___________________________| 

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 6.02587 406 481
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The monkey heard about the very next ship which is yellow and green . 

sem(1,
    [word(1001, "The"), word(1002, monkey), word(1003, heard), word(1004, about), word(1005, the), word(1006, very), word(1007, next), word(1008, ship), word(1009, which), word(1010, is), word(1011, yellow), word(1012, and), word(1013, green), word(1014, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "VBD"), pos(1004, "IN"), pos(1005, "DT"), pos(1006, "RB"), pos(1007, "JJ"), pos(1008, "NN"), pos(1009, "WDT"), pos(1010, "VBZ"), pos(1011, "JJ"), pos(1012, "CC"), pos(1013, "JJ"), pos(1014, ".")],
    [],
    smerge(drs([[1001]:A, [1005]:B], [[1002]:pred(A, monkey, n, 0), [1007]:pred(B, next, a, 0), [1008]:pred(B, ship, n, 0), [1006]:pred(B, very, a, 0)]), drs([[1003]:C, [1011]:D, [1013]:E], [[1003]:pred(C, hear, v, 0), [1003]:rel(C, A, agent, 0), []:pred(D, event, n, 1), []:pred(E, event, n, 1), [1004]:rel(C, B, about, 0), [1011]:prop(D, drs([], [[1011]:pred(B, yellow, a, 0)])), [1013]:prop(E, drs([], [[1013]:pred(B, green, a, 0)])), []:pred(C, event, n, 1)])) ).
%%%   ____________   _____________________  
%%%  | x0 x1      | | x2 x3 x4            | 
%%%  |------------| |---------------------| 
%%% (| monkey(x0) |+| hear(x2)            |)
%%%  | next(x1)   | | agent(x2,x0)        | 
%%%  | ship(x1)   | | event(x3)           | 
%%%  | very(x1)   | | event(x4)           | 
%%%  |____________| | about(x2,x1)        | 
%%%                 |       ____________  | 
%%%                 |      |            | | 
%%%                 |   x3:|------------| | 
%%%                 |      | yellow(x1) | | 
%%%                 |      |____________| | 
%%%                 |       ___________   | 
%%%                 |      |           |  | 
%%%                 |   x4:|-----------|  | 
%%%                 |      | green(x1) |  | 
%%%                 |      |___________|  | 
%%%                 | event(x2)           | 
%%%                 |_____________________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    t(n, "monkey", "monkey", "NN", "I-NP", "O")),
   fa(s:dcl\\np,
    t((s:dcl\\np)/pp, "heard", "hear", "VBD", "I-VP", "O"),
    fa(pp,
     t(pp/np, "about", "about", "IN", "I-PP", "O"),
     ba(np,
      fa(np:nb,
       t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
       fa(n,
        fa(n/n,
         t((n/n)/(n/n), "very", "very", "RB", "I-NP", "O"),
         t(n/n, "next", "next", "JJ", "I-NP", "O")),
        t(n, "ship", "ship", "NN", "I-NP", "O"))),
      fa(np\\np,
       t((np\\np)/(s:dcl\\np), "which", "which", "WDT", "B-NP", "O"),
       fa(s:dcl\\np,
        t((s:dcl\\np)/(s:adj\\np), "is", "be", "VBZ", "I-VP", "O"),
        ba(s:adj\\np,
         t(s:adj\\np, "yellow", "yellow", "JJ", "I-ADJP", "O"),
         conj((s:adj\\np)\\(s:adj\\np), s:adj\\np,
          t(conj, "and", "and", "CC", "I-ADJP", "O"),
          t(s:adj\\np, "green", "green", "JJ", "I-ADJP", "O"))))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 2.56495 166 188
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(np,
  ba(np,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    fa(n,
     t(n/n, "Brit", "Brit", "NNP", "I-NP", "O"),
     t(n, "lives", "life", "NNS", "I-NP", "O"))),
   fa(np\\np,
    t((np\\np)/np, "in", "in", "IN", "I-PP", "O"),
    fa(np:nb,
     t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
     fa(n,
      t(n/n, "red", "red", "JJ", "I-NP", "O"),
      t(n, "house", "house", "NN", "I-NP", "O"))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The Brit lives in the red house . 

sem(1,
    [word(1001, "The"), word(1002, "Brit"), word(1003, lives), word(1004, in), word(1005, the), word(1006, red), word(1007, house), word(1008, ".")],
    [pos(1001, "DT"), pos(1002, "NNP"), pos(1003, "NNS"), pos(1004, "IN"), pos(1005, "DT"), pos(1006, "JJ"), pos(1007, "NN"), pos(1008, ".")],
    [],
    smerge(drs([[1002]:A, [1001]:B, [1005]:C], [[1003]:pred(B, life, n, 0), []:rel(A, B, nn, 0), [1002]:named(A, brit, nam, 0), [1006]:pred(C, red, a, 0), [1007]:pred(C, house, n, 0)]), drs([], [[]:pred(B, topic, a, 1), [1004]:rel(B, C, in, 0)])) ).
%%%   ____________________   ___________  
%%%  | x0 x1 x2           | |           | 
%%%  |--------------------| |-----------| 
%%% (| life(x1)           |+| topic(x1) |)
%%%  | nn(x0,x1)          | | in(x1,x2) | 
%%%  | named(x0,brit,nam) | |___________| 
%%%  | red(x2)            |               
%%%  | house(x2)          |               
%%%  |____________________|               

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 4.78749 194 241
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    t(n, "Swede", "swede", "NN", "I-NP", "O")),
   fa(s:dcl\\np,
    fa((s:dcl\\np)/pp,
     t(((s:dcl\\np)/pp)/np, "keeps", "keep", "VBZ", "I-VP", "O"),
     lx(np, n,
      t(n, "dogs", "dog", "NNS", "I-NP", "O"))),
    fa(pp,
     t(pp/np, "as", "as", "IN", "I-PP", "O"),
     lx(np, n,
      t(n, "pets", "pet", "NNS", "I-NP", "O"))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The Swede keeps dogs as pets . 

sem(1,
    [word(1001, "The"), word(1002, "Swede"), word(1003, keeps), word(1004, dogs), word(1005, as), word(1006, pets), word(1007, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "VBZ"), pos(1004, "NNS"), pos(1005, "IN"), pos(1006, "NNS"), pos(1007, ".")],
    [],
    smerge(drs([[1001]:A], [[1002]:pred(A, swede, n, 0)]), drs([[1004]:B, [1003]:C, [1006]:D], [[1004]:pred(B, dog, n, 0), [1003]:pred(C, keep, v, 0), [1006]:pred(D, pet, n, 0), []:pred(C, event, n, 1), [1003]:rel(C, A, agent, 0), [1003]:rel(C, B, patient, 0), [1005]:rel(C, D, as, 0)])) ).
%%%   ___________   ________________  
%%%  | x0        | | x1 x2 x3       | 
%%%  |-----------| |----------------| 
%%% (| swede(x0) |+| dog(x1)        |)
%%%  |___________| | keep(x2)       | 
%%%                | pet(x3)        | 
%%%                | event(x2)      | 
%%%                | agent(x2,x0)   | 
%%%                | patient(x2,x1) | 
%%%                | as(x2,x3)      | 
%%%                |________________| 

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 1.38629 77 101
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(np:nb,
  fa(np:nb,
   t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
   fa(n,
    t(n/n, "Dane", "Dane", "NNP", "I-NP", "I-ORG"),
    fa(n,
     t(n/n, "drinks", "drink", "NNS", "I-NP", "O"),
     t(n, "tea", "tea", "NN", "I-NP", "O")))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The Dane drinks tea . 

sem(1,
    [word(1001, "The"), word(1002, "Dane"), word(1003, drinks), word(1004, tea), word(1005, ".")],
    [pos(1001, "DT"), pos(1002, "NNP"), pos(1003, "NNS"), pos(1004, "NN"), pos(1005, ".")],
    [ne(1002, "I-ORG")],
    smerge(drs([[1003]:A, [1002]:B, [1001]:C], [[1003]:pred(A, drink, n, 0), [1004]:pred(C, tea, n, 0), []:rel(A, C, nn, 0), []:rel(B, C, nn, 0), [1002]:named(B, dane, org, 0)]), drs([], [[]:pred(C, topic, a, 1)])) ).
%%%   ____________________   ___________  
%%%  | x0 x1 x2           | |           | 
%%%  |--------------------| |-----------| 
%%% (| drink(x0)          |+| topic(x2) |)
%%%  | tea(x2)            | |___________| 
%%%  | nn(x0,x2)          |               
%%%  | nn(x1,x2)          |               
%%%  | named(x1,dane,org) |               
%%%  |____________________|               

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
   1 stats 4.33073 334 378
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The green house is on the immediate left of the white house . 

sem(1,
    [word(1001, "The"), word(1002, green), word(1003, house), word(1004, is), word(1005, on), word(1006, the), word(1007, immediate), word(1008, left), word(1009, of), word(1010, the), word(1011, white), word(1012, house), word(1013, ".")],
    [pos(1001, "DT"), pos(1002, "JJ"), pos(1003, "NN"), pos(1004, "VBZ"), pos(1005, "IN"), pos(1006, "DT"), pos(1007, "JJ"), pos(1008, "NN"), pos(1009, "IN"), pos(1010, "DT"), pos(1011, "JJ"), pos(1012, "NN"), pos(1013, ".")],
    [],
    smerge(drs([[1001]:A, [1006]:B], [[1011]:pred(A, white, a, 0), [1003, 1012]:pred(A, house, n, 0), [1002]:pred(A, green, a, 0), [1007]:pred(B, immediate, a, 0), [1008]:pred(B, left, n, 0)]), drs([[1004]:C], [[1004]:pred(C, be, v, 0), [1004]:rel(C, A, agent, 0), [1009]:rel(B, A, of, 0), [1005]:rel(C, B, on, 0), []:pred(C, event, n, 1)])) ).
%%%   _______________   ______________  
%%%  | x0 x1         | | x2           | 
%%%  |---------------| |--------------| 
%%% (| white(x0)     |+| be(x2)       |)
%%%  | house(x0)     | | agent(x2,x0) | 
%%%  | green(x0)     | | of(x1,x0)    | 
%%%  | immediate(x1) | | on(x2,x1)    | 
%%%  | left(x1)      | | event(x2)    | 
%%%  |_______________| |______________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    fa(n,
     t(n/n, "green", "green", "JJ", "I-NP", "O"),
     t(n, "house", "house", "NN", "I-NP", "O"))),
   fa(s:dcl\\np,
    t((s:dcl\\np)/pp, "is", "be", "VBZ", "I-VP", "O"),
    fa(pp,
     t(pp/np, "on", "on", "IN", "I-PP", "O"),
     ba(np,
      fa(np:nb,
       t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
       fa(n,
        t(n/n, "immediate", "immediate", "JJ", "I-NP", "O"),
        t(n, "left", "left", "NN", "I-NP", "O"))),
      fa(np\\np,
       t((np\\np)/np, "of", "of", "IN", "I-PP", "O"),
       fa(np:nb,
        t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
        fa(n,
         t(n/n, "white", "white", "JJ", "I-NP", "O"),
         t(n, "house", "house", "NN", "I-NP", "O")))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 1.38629 150 176
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The green house "s owner drinks coffee . 

sem(1,
    [word(1001, "The"), word(1002, green), word(1003, house), word(1004, "'s"), word(1005, owner), word(1006, drinks), word(1007, coffee), word(1008, ".")],
    [pos(1001, "DT"), pos(1002, "JJ"), pos(1003, "NN"), pos(1004, "POS"), pos(1005, "NN"), pos(1006, "NNS"), pos(1007, "NN"), pos(1008, ".")],
    [],
    smerge(drs([[1001]:A, [1006]:B, [1005]:C, [1004]:D], [[1002]:pred(A, green, a, 0), [1003]:pred(A, house, n, 0), [1006]:pred(B, drink, n, 0), [1005]:pred(C, owner, n, 0), [1007]:pred(D, coffee, n, 0), []:rel(B, D, nn, 0), []:rel(C, D, nn, 0), [1004]:rel(D, A, of, 0)]), drs([], [[]:pred(D, topic, a, 1)])) ).
%%%   _____________   ___________  
%%%  | x0 x1 x2 x3 | |           | 
%%%  |-------------| |-----------| 
%%% (| green(x0)   |+| topic(x3) |)
%%%  | house(x0)   | |___________| 
%%%  | drink(x1)   |               
%%%  | owner(x2)   |               
%%%  | coffee(x3)  |               
%%%  | nn(x1,x3)   |               
%%%  | nn(x2,x3)   |               
%%%  | of(x3,x0)   |               
%%%  |_____________|               

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(np:nb,
  fa(np:nb,
   ba(np:nb/n,
    fa(np:nb,
     t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
     fa(n,
      t(n/n, "green", "green", "JJ", "I-NP", "O"),
      t(n, "house", "house", "NN", "I-NP", "O"))),
    t((np:nb/n)\\np, "'s", "'s", "POS", "B-NP", "O")),
   fa(n,
    t(n/n, "owner", "owner", "NN", "I-NP", "O"),
    fa(n,
     t(n/n, "drinks", "drink", "NNS", "I-NP", "O"),
     t(n, "coffee", "coffee", "NN", "I-NP", "O")))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 4.33073 200 232
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   ba(np,
    fa(np:nb,
     t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
     t(n, "owner", "owner", "NN", "I-NP", "O")),
    fa(np\\np,
     t((np\\np)/(s:dcl\\np), "who", "who", "WP", "B-NP", "O"),
     fa(s:dcl\\np,
      t((s:dcl\\np)/np, "smokes", "smoke", "VBZ", "I-VP", "O"),
      lx(np, n,
       fa(n,
        t(n/n, "Pall", "Pall", "NNP", "I-NP", "I-ORG"),
        t(n, "Mall", "Mall", "NNP", "I-NP", "I-ORG")))))),
   fa(s:dcl\\np,
    t((s:dcl\\np)/np, "rears", "rear", "VBZ", "I-VP", "O"),
    lx(np, n,
     t(n, "birds", "bird", "NNS", "I-NP", "O")))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The owner who smokes Pall Mall rears birds . 

sem(1,
    [word(1001, "The"), word(1002, owner), word(1003, who), word(1004, smokes), word(1005, "Pall"), word(1006, "Mall"), word(1007, rears), word(1008, birds), word(1009, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "WP"), pos(1004, "VBZ"), pos(1005, "NNP"), pos(1006, "NNP"), pos(1007, "VBZ"), pos(1008, "NNS"), pos(1009, ".")],
    [ne(1005, "I-ORG"), ne(1006, "I-ORG")],
    smerge(drs([[1001]:A, [1005, 1006]:B], [[1002]:pred(A, owner, n, 0), [1005, 1006]:named(B, pall_mall, org, 0)]), drs([[1004]:C, [1008]:D, [1007]:E], [[1004]:pred(C, smoke, v, 0), []:pred(C, event, n, 1), [1004]:rel(C, A, agent, 0), [1004]:rel(C, B, patient, 0), [1008]:pred(D, bird, n, 0), [1007]:pred(E, rear, v, 0), []:pred(E, event, n, 1), [1007]:rel(E, A, agent, 0), [1007]:rel(E, D, patient, 0)])) ).
%%%   _________________________   ________________  
%%%  | x0 x1                   | | x2 x3 x4       | 
%%%  |-------------------------| |----------------| 
%%% (| owner(x0)               |+| smoke(x2)      |)
%%%  | named(x1,pall_mall,org) | | event(x2)      | 
%%%  |_________________________| | agent(x2,x0)   | 
%%%                              | patient(x2,x1) | 
%%%                              | bird(x3)       | 
%%%                              | rear(x4)       | 
%%%                              | event(x4)      | 
%%%                              | agent(x4,x0)   | 
%%%                              | patient(x4,x3) | 
%%%                              |________________| 

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 4.02535 340 404
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The owner of the yellow house smokes Dunhill . 

sem(1,
    [word(1001, "The"), word(1002, owner), word(1003, of), word(1004, the), word(1005, yellow), word(1006, house), word(1007, smokes), word(1008, "Dunhill"), word(1009, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "IN"), pos(1004, "DT"), pos(1005, "JJ"), pos(1006, "NN"), pos(1007, "VBZ"), pos(1008, "NNP"), pos(1009, ".")],
    [ne(1008, "I-ORG")],
    smerge(drs([[1001]:A, [1004]:B, [1008]:C], [[1002]:pred(A, owner, n, 0), [1005]:pred(B, yellow, a, 0), [1006]:pred(B, house, n, 0), [1008]:named(C, dunhill, org, 0)]), drs([[1007]:D], [[1003]:rel(A, B, of, 0), [1007]:pred(D, smoke, v, 0), []:pred(D, event, n, 1), [1007]:rel(D, A, agent, 0), [1007]:rel(D, C, patient, 0)])) ).
%%%   _______________________   ________________  
%%%  | x0 x1 x2              | | x3             | 
%%%  |-----------------------| |----------------| 
%%% (| owner(x0)             |+| of(x0,x1)      |)
%%%  | yellow(x1)            | | smoke(x3)      | 
%%%  | house(x1)             | | event(x3)      | 
%%%  | named(x2,dunhill,org) | | agent(x3,x0)   | 
%%%  |_______________________| | patient(x3,x2) | 
%%%                            |________________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   ba(np,
    fa(np:nb,
     t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
     t(n, "owner", "owner", "NN", "I-NP", "O")),
    fa(np\\np,
     t((np\\np)/np, "of", "of", "IN", "I-PP", "O"),
     fa(np:nb,
      t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
      fa(n,
       t(n/n, "yellow", "yellow", "JJ", "I-NP", "O"),
       t(n, "house", "house", "NN", "I-NP", "O"))))),
   fa(s:dcl\\np,
    t((s:dcl\\np)/np, "smokes", "smoke", "VBZ", "I-VP", "O"),
    lx(np, n,
     t(n, "Dunhill", "Dunhill", "NNP", "I-NP", "I-ORG")))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 2.56495 202 239
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(np,
  ba(np,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    fa(n,
     t(n/n, "owner", "owner", "NN", "I-NP", "O"),
     t(n, "living", "living", "NN", "I-NP", "O"))),
   fa(np\\np,
    t((np\\np)/np, "in", "in", "IN", "I-PP", "O"),
    fa(np:nb,
     t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
     fa(n,
      t(n/n, "center", "center", "NN", "I-NP", "O"),
      fa(n,
       t(n/n, "house", "house", "NN", "I-NP", "O"),
       fa(n,
        t(n/n, "drinks", "drink", "NNS", "I-NP", "O"),
        t(n, "milk", "milk", "NN", "I-NP", "O"))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The owner living in the center house drinks milk . 

sem(1,
    [word(1001, "The"), word(1002, owner), word(1003, living), word(1004, in), word(1005, the), word(1006, center), word(1007, house), word(1008, drinks), word(1009, milk), word(1010, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "NN"), pos(1004, "IN"), pos(1005, "DT"), pos(1006, "NN"), pos(1007, "NN"), pos(1008, "NNS"), pos(1009, "NN"), pos(1010, ".")],
    [],
    smerge(drs([[1002]:A, [1001]:B, [1008]:C, [1007]:D, [1006]:E, [1005]:F], [[1002]:pred(A, owner, n, 0), [1003]:pred(B, living, n, 0), []:rel(A, B, nn, 0), [1008]:pred(C, drink, n, 0), [1007]:pred(D, house, n, 0), [1006]:pred(E, center, n, 0), [1009]:pred(F, milk, n, 0), []:rel(C, F, nn, 0), []:rel(D, F, nn, 0), []:rel(E, F, nn, 0)]), drs([], [[]:pred(B, topic, a, 1), [1004]:rel(B, F, in, 0)])) ).
%%%   ___________________   ___________  
%%%  | x0 x1 x2 x3 x4 x5 | |           | 
%%%  |-------------------| |-----------| 
%%% (| owner(x0)         |+| topic(x1) |)
%%%  | living(x1)        | | in(x1,x5) | 
%%%  | nn(x0,x1)         | |___________| 
%%%  | drink(x2)         |               
%%%  | house(x3)         |               
%%%  | center(x4)        |               
%%%  | milk(x5)          |               
%%%  | nn(x2,x5)         |               
%%%  | nn(x3,x5)         |               
%%%  | nn(x4,x5)         |               
%%%  |___________________|               

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 2.19722 155 176
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The Norwegian lives in the first house . 

sem(1,
    [word(1001, "The"), word(1002, "Norwegian"), word(1003, lives), word(1004, in), word(1005, the), word(1006, first), word(1007, house), word(1008, ".")],
    [pos(1001, "DT"), pos(1002, "JJ"), pos(1003, "NNS"), pos(1004, "IN"), pos(1005, "DT"), pos(1006, "JJ"), pos(1007, "NN"), pos(1008, ".")],
    [],
    smerge(drs([[1001]:A, [1005]:B], [[1002]:pred(A, norwegian, a, 0), [1003]:pred(A, life, n, 0), [1006]:pred(B, first, a, 0), [1007]:pred(B, house, n, 0)]), drs([], [[]:pred(A, topic, a, 1), [1004]:rel(A, B, in, 0)])) ).
%%%   _______________   ___________  
%%%  | x0 x1         | |           | 
%%%  |---------------| |-----------| 
%%% (| norwegian(x0) |+| topic(x0) |)
%%%  | life(x0)      | | in(x0,x1) | 
%%%  | first(x1)     | |___________| 
%%%  | house(x1)     |               
%%%  |_______________|               

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(np,
  ba(np,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    fa(n,
     t(n/n, "Norwegian", "norwegian", "JJ", "I-NP", "O"),
     t(n, "lives", "life", "NNS", "I-NP", "O"))),
   fa(np\\np,
    t((np\\np)/np, "in", "in", "IN", "I-PP", "O"),
    fa(np:nb,
     t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
     fa(n,
      t(n/n, "first", "first", "JJ", "I-NP", "O"),
      t(n, "house", "house", "NN", "I-NP", "O"))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 7.39388 457 578
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The owner who smokes Blends lives next to the one who keeps cats . 

sem(1,
    [word(1001, "The"), word(1002, owner), word(1003, who), word(1004, smokes), word(1005, "Blends"), word(1006, lives), word(1007, next), word(1008, to), word(1009, the), word(1010, one), word(1011, who), word(1012, keeps), word(1013, cats), word(1014, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "WP"), pos(1004, "VBZ"), pos(1005, "NNPS"), pos(1006, "VBZ"), pos(1007, "JJ"), pos(1008, "TO"), pos(1009, "DT"), pos(1010, "NN"), pos(1011, "WP"), pos(1012, "VBZ"), pos(1013, "NNS"), pos(1014, ".")],
    [ne(1005, "I-ORG")],
    smerge(drs([[1001]:A, [1005]:B, [1009]:C], [[1002]:pred(A, owner, n, 0), [1005]:named(B, blends, org, 0), [1010]:pred(C, one, n, 0)]), drs([[1004]:D, [1006]:E, [1013]:F, [1012]:G], [[1004]:pred(D, smoke, v, 0), []:pred(D, event, n, 1), [1004]:rel(D, A, agent, 0), [1004]:rel(D, B, patient, 0), [1006]:pred(E, live, v, 0), [1006]:rel(E, A, agent, 0), [1013]:pred(F, cat, n, 0), [1012]:pred(G, keep, v, 0), []:pred(G, event, n, 1), [1012]:rel(G, C, agent, 0), [1012]:rel(G, F, patient, 0), [1008]:rel(E, C, to, 0), []:pred(E, event, n, 1)])) ).
%%%   ______________________   ________________  
%%%  | x0 x1 x2             | | x3 x4 x5 x6    | 
%%%  |----------------------| |----------------| 
%%% (| owner(x0)            |+| smoke(x3)      |)
%%%  | named(x1,blends,org) | | event(x3)      | 
%%%  | one(x2)              | | agent(x3,x0)   | 
%%%  |______________________| | patient(x3,x1) | 
%%%                           | live(x4)       | 
%%%                           | agent(x4,x0)   | 
%%%                           | cat(x5)        | 
%%%                           | keep(x6)       | 
%%%                           | event(x6)      | 
%%%                           | agent(x6,x2)   | 
%%%                           | patient(x6,x5) | 
%%%                           | to(x4,x2)      | 
%%%                           | event(x4)      | 
%%%                           |________________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   ba(np,
    fa(np:nb,
     t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
     t(n, "owner", "owner", "NN", "I-NP", "O")),
    fa(np\\np,
     t((np\\np)/(s:dcl\\np), "who", "who", "WP", "B-NP", "O"),
     fa(s:dcl\\np,
      t((s:dcl\\np)/np, "smokes", "smoke", "VBZ", "I-VP", "O"),
      lx(np, n,
       t(n, "Blends", "Blends", "NNPS", "I-NP", "I-ORG"))))),
   ba(s:dcl\\np,
    t(s:dcl\\np, "lives", "live", "VBZ", "I-VP", "O"),
    fa((s:dcl\\np)\\(s:dcl\\np),
     t(((s:dcl\\np)\\(s:dcl\\np))/pp, "next", "next", "JJ", "I-ADVP", "O"),
     fa(pp,
      t(pp/np, "to", "to", "TO", "I-PP", "O"),
      ba(np,
       fa(np:nb,
        t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
        t(n, "one", "one", "NN", "I-NP", "O")),
       fa(np\\np,
        t((np\\np)/(s:dcl\\np), "who", "who", "WP", "B-NP", "O"),
        fa(s:dcl\\np,
         t((s:dcl\\np)/np, "keeps", "keep", "VBZ", "I-VP", "O"),
         lx(np, n,
          t(n, "cats", "cat", "NNS", "I-NP", "O"))))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 7.19143 468 574
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The owner who keeps the horse lives next to the one who smokes Dunhills . 

sem(1,
    [word(1001, "The"), word(1002, owner), word(1003, who), word(1004, keeps), word(1005, the), word(1006, horse), word(1007, lives), word(1008, next), word(1009, to), word(1010, the), word(1011, one), word(1012, who), word(1013, smokes), word(1014, "Dunhills"), word(1015, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "WP"), pos(1004, "VBZ"), pos(1005, "DT"), pos(1006, "NN"), pos(1007, "VBZ"), pos(1008, "JJ"), pos(1009, "TO"), pos(1010, "DT"), pos(1011, "CD"), pos(1012, "WP"), pos(1013, "VBZ"), pos(1014, "NNP"), pos(1015, ".")],
    [ne(1014, "I-ORG")],
    smerge(drs([[1001]:A, [1005]:B, [1010]:C, [1014]:D], [[1002]:pred(A, owner, n, 0), [1006]:pred(B, horse, n, 0), [1011]:card(C, 1, ge), [1011]:pred(C, thing, n, 12), [1014]:named(D, dunhills, org, 0)]), drs([[1004]:E, [1007]:F, [1013]:G], [[1004]:pred(E, keep, v, 0), []:pred(E, event, n, 1), [1004]:rel(E, A, agent, 0), [1004]:rel(E, B, patient, 0), [1007]:pred(F, live, v, 0), [1007]:rel(F, A, agent, 0), [1013]:pred(G, smoke, v, 0), []:pred(G, event, n, 1), [1013]:rel(G, C, agent, 0), [1013]:rel(G, D, patient, 0), [1009]:rel(F, C, to, 0), []:pred(F, event, n, 1)])) ).
%%%   ________________________   ________________  
%%%  | x0 x1 x2 x3            | | x4 x5 x6       | 
%%%  |------------------------| |----------------| 
%%% (| owner(x0)              |+| keep(x4)       |)
%%%  | horse(x1)              | | event(x4)      | 
%%%  | |x2| >= 1              | | agent(x4,x0)   | 
%%%  | thing(x2)              | | patient(x4,x1) | 
%%%  | named(x3,dunhills,org) | | live(x5)       | 
%%%  |________________________| | agent(x5,x0)   | 
%%%                             | smoke(x6)      | 
%%%                             | event(x6)      | 
%%%                             | agent(x6,x2)   | 
%%%                             | patient(x6,x3) | 
%%%                             | to(x5,x2)      | 
%%%                             | event(x5)      | 
%%%                             |________________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(s:dcl,
  ba(s:dcl,
   ba(np,
    fa(np:nb,
     t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
     t(n, "owner", "owner", "NN", "I-NP", "O")),
    fa(np\\np,
     t((np\\np)/(s:dcl\\np), "who", "who", "WP", "B-NP", "O"),
     fa(s:dcl\\np,
      t((s:dcl\\np)/np, "keeps", "keep", "VBZ", "I-VP", "O"),
      fa(np:nb,
       t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
       t(n, "horse", "horse", "NN", "I-NP", "O"))))),
   ba(s:dcl\\np,
    t(s:dcl\\np, "lives", "live", "VBZ", "I-VP", "O"),
    fa((s:dcl\\np)\\(s:dcl\\np),
     t(((s:dcl\\np)\\(s:dcl\\np))/pp, "next", "next", "JJ", "I-ADVP", "O"),
     fa(pp,
      t(pp/np, "to", "to", "TO", "I-PP", "O"),
      ba(np,
       fa(np:nb,
        t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
        t(n, "one", "one", "CD", "I-NP", "O")),
       fa(np\\np,
        t((np\\np)/(s:dcl\\np), "who", "who", "WP", "B-NP", "O"),
        fa(s:dcl\\np,
         t((s:dcl\\np)/np, "smokes", "smoke", "VBZ", "I-VP", "O"),
         lx(np, n,
          t(n, "Dunhills", "Dunhills", "NNP", "I-NP", "I-ORG"))))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 2.63906 151 182
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The owner who smokes Bluemasters drinks beer . 

sem(1,
    [word(1001, "The"), word(1002, owner), word(1003, who), word(1004, smokes), word(1005, "Bluemasters"), word(1006, drinks), word(1007, beer), word(1008, ".")],
    [pos(1001, "DT"), pos(1002, "NN"), pos(1003, "WP"), pos(1004, "VBZ"), pos(1005, "NNS"), pos(1006, "NNS"), pos(1007, "NN"), pos(1008, ".")],
    [],
    smerge(drs([[1001]:A], [[1002]:pred(A, owner, n, 0)]), drs([[1006]:B, [1005]:C, [1005, 1006, 1007]:D, [1004]:E], [[1006]:pred(B, drink, n, 0), [1005]:pred(C, bluemaster, n, 0), [1007]:pred(D, beer, n, 0), [1004]:pred(E, smoke, v, 0), []:pred(E, event, n, 1), []:pred(A, topic, a, 1), []:rel(B, D, nn, 0), []:rel(C, D, nn, 0), [1004]:rel(E, A, agent, 0), [1004]:rel(E, D, patient, 0)])) ).
%%%   ___________   ________________  
%%%  | x0        | | x1 x2 x3 x4    | 
%%%  |-----------| |----------------| 
%%% (| owner(x0) |+| drink(x1)      |)
%%%  |___________| | bluemaster(x2) | 
%%%                | beer(x3)       | 
%%%                | smoke(x4)      | 
%%%                | event(x4)      | 
%%%                | topic(x0)      | 
%%%                | nn(x1,x3)      | 
%%%                | nn(x2,x3)      | 
%%%                | agent(x4,x0)   | 
%%%                | patient(x4,x3) | 
%%%                |________________| 

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(np,
  ba(np,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    t(n, "owner", "owner", "NN", "I-NP", "O")),
   fa(np\\np,
    t((np\\np)/(s:dcl\\np), "who", "who", "WP", "B-NP", "O"),
    fa(s:dcl\\np,
     t((s:dcl\\np)/np, "smokes", "smoke", "VBZ", "I-VP", "O"),
     lx(np, n,
      fa(n,
       t(n/n, "Bluemasters", "bluemaster", "NNS", "I-NP", "O"),
       fa(n,
        t(n/n, "drinks", "drink", "NNS", "I-NP", "O"),
        t(n, "beer", "beer", "NN", "I-NP", "O"))))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 1.94591 162 189
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The German smokes Prince . 

sem(1,
    [word(1001, "The"), word(1002, "German"), word(1003, smokes), word(1004, "Prince"), word(1005, ".")],
    [pos(1001, "DT"), pos(1002, "JJ"), pos(1003, "NNS"), pos(1004, "NNP"), pos(1005, ".")],
    [],
    smerge(drs([[1003]:A, [1001]:B], [[1003]:pred(A, smoke, n, 0), [1002]:pred(B, german, a, 0), []:rel(A, B, nn, 0), [1004]:named(B, prince, nam, 0)]), drs([], [[]:pred(B, topic, a, 1)])) ).
%%%   ______________________   ___________  
%%%  | x0 x1                | |           | 
%%%  |----------------------| |-----------| 
%%% (| smoke(x0)            |+| topic(x1) |)
%%%  | german(x1)           | |___________| 
%%%  | nn(x0,x1)            |               
%%%  | named(x1,prince,nam) |               
%%%  |______________________|               

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 fa(np:nb,
  t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
  fa(n,
   t(n/n, "German", "german", "JJ", "I-NP", "O"),
   fa(n,
    t(n/n, "smokes", "smoke", "NNS", "I-NP", "O"),
    rp(n,
     t(n, "Prince", "Prince", "NNP", "I-NP", "O"),
     t(period, ".", ".", ".", "O", "O")))))).

id(rte, [1]).


        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 parsed at B=0.075, K=20
1 coverage 100%
1 stats 2.77259 173 197
Attempted: 1. Completed: 1 (100.00%).
$VAR1 = {
          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.

ccg(1,
 rp(np,
  ba(np,
   fa(np:nb,
    t(np:nb/n, "The", "the", "DT", "I-NP", "O"),
    fa(n,
     t(n/n, "Norwegian", "norwegian", "JJ", "I-NP", "O"),
     t(n, "lives", "life", "NNS", "I-NP", "O"))),
   fa(np\\np,
    t((np\\np)/pp, "next", "next", "IN", "B-NP", "O"),
    fa(pp,
     t(pp/np, "to", "to", "TO", "I-PP", "O"),
     fa(np:nb,
      t(np:nb/n, "the", "the", "DT", "I-NP", "O"),
      fa(n,
       t(n/n, "blue", "blue", "JJ", "I-NP", "O"),
       t(n, "house", "house", "NN", "I-NP", "O")))))),
  t(period, ".", ".", ".", "O", "O"))).

id(rte, [1]).


          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

id(rte,1).

%%% The Norwegian lives next to the blue house . 

sem(1,
    [word(1001, "The"), word(1002, "Norwegian"), word(1003, lives), word(1004, next), word(1005, to), word(1006, the), word(1007, blue), word(1008, house), word(1009, ".")],
    [pos(1001, "DT"), pos(1002, "JJ"), pos(1003, "NNS"), pos(1004, "IN"), pos(1005, "TO"), pos(1006, "DT"), pos(1007, "JJ"), pos(1008, "NN"), pos(1009, ".")],
    [],
    smerge(drs([[1001]:A, [1006]:B], [[1002]:pred(A, norwegian, a, 0), [1003]:pred(A, life, n, 0), [1007]:pred(B, blue, a, 0), [1008]:pred(B, house, n, 0)]), drs([], [[1004]:pred(A, next, n, 0), [1005]:rel(A, B, to, 0), []:pred(A, topic, a, 1)])) ).
%%%   _______________   ___________  
%%%  | x0 x1         | |           | 
%%%  |---------------| |-----------| 
%%% (| norwegian(x0) |+| next(x0)  |)
%%%  | life(x0)      | | to(x0,x1) | 
%%%  | blue(x1)      | | topic(x0) | 
%%%  | house(x1)     | |___________| 
%%%  |_______________|               

        };
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer
cd /var/lib/myfrdcsa/sandbox/nutcracker-1.0/nutcracker-1.0/candc && bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box
# this file was generated by the following command(s):
#   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

1 attempt nospan at B=0.075, K=20
1 attempt nospan at B=0.03, K=20
1 attempt nospan at B=0.01, K=20
1 attempt nospan at B=0.005, K=20
1 attempt nospan at B=0.001, K=150
1 failed no span at B=0.001, K=150
Attempted: 1. Completed: 0 (0.00%).
$VAR1 = {
          'DRS' => '%%% This file was generated by the following command:'
%%% bin/boxer --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.drs --resolve --vpe --box 

:- multifile     sem/5, id/2.
:- discontiguous sem/5, id/2.
:- dynamic       sem/5, id/2.

          'CCG' => '% this file was generated by the following command(s):'
%   bin/candc --input /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.met --output /var/lib/myfrdcsa/codebases/internal/perllib/data/candc/candc-text.ccg --models models/boxer --candc-printer boxer

:- op(601, xfx, (/)).
:- op(601, xfx, (\\)).
:- multifile ccg/2, id/2.
:- discontiguous ccg/2, id/2.


id(rte, [1]).


        };
andrewdo@ai2:/var/lib/myfrdcsa/codebases/internal/perllib/scripts$ 

