%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question rules
%
% Version: 1.0.1
% Last modified: 02/08/04
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.


% Best Parse Categories
best_parse_cats([s,sbar,sinv,q,relc,nfvp,fvp,whnp,whpp,howadjp,np,pp]).
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question Prefix
%   -- ensures relc rules not fired at beginning of question
%   -- note * question * can throw off taggers --  WP's may 
%      become NN as no longer at start of sentence, so 
%      capitalisation may imply  unknown words -- robotag has 
%      been modifed to avoid this
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule(q(sem: QSem),
    [sym(s_form:'*'),
     n(m_root:'question'),
     sym(s_form:'*'),
     q(sem: QSem)
]).

rule(q(sem: QSem),
    [n(m_root:'*'),
     n(m_root:'question'),
     n(m_root:'*'),
     q(sem: QSem)
]).

% REMOVED: causes weird ordering effects in embedded
% rule application 
%rule(q(sem: QVar^E^[QSem,ASem,[rule,q1]]),
%     [qbody(sem: QVar^E^QSem),
%      adjuncts(sem:ASem),
%      sym(s_form:'?')]).
     
%rule(q(sem: QVar^E^[QSem,ASem,PPSem,[rule,q2]]),
%     [qbody(sem: QVar^E^QSem), pp(sem:PPSem)]).
%      sym(s_form:'?')]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Questions
%   WHO questions
%   WHAT questions
%   WHNP questions
%   WHPP questions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% WHO QUESTIONS
%%%%%%%%%%%%%%%%

%  Who1
%% Who1: Q -> WP FVP(active|passive)
%  "Who built the Eiffel Tower?"
rule(q(sem: QVar^E^[[qvar,QVar],[qattr,QVar,name],[person,QVar],[lsubj,E,QVar],
	VPSem,[qcon,E,verb],[rule,who1a]]),
      [wp(m_root:'who'),
       fvp(voice:active,sem:E^VPSem)
]).
% "Who was eaten by the tiger?"
rule(q(sem: QVar^E^[[qvar,QVar],[qattr,QVar,name],[person,QVar],[lobj,E,QVar],
	VPSem,[qcon,E,verb],[rule,who1b]]),
      [wp(m_root:'who'),
       fvp(voice:passive,sem:E^VPSem)
]).


%  Who2
%% Who2: Q -> WP(who) VPCORE(be) NP 
%  "Who was the President of Turkmenistan in 1994?"
 rule(q(sem: QVar^E^[[qvar, QVar],[qattr,QVar,name],[person,QVar],[lsubj,E,QVar],
 		  VPSem, NPSem, ASem,[rule,who2]]),
      [wp(m_root:'who'),
       vpcore(m_root:'be',sem:E^VPSem),
       np(sem:QVar^NPSem)
 ]).

%  Who3
%% Who3: Q -> WP(who/whom) SINV
%  "Who(m) did John disobey?"
rule(q(sem: QVar^E^[[qvar, QVar],[qattr,QVar,name],[person,QVar], 
	[lobj, E, QVar], SinvSem,[qcon,E,verb],[rule,who3]]),
     [wp(m_root:'who'),
      sinv(voice:active, sem:E^SinvSem)
]).


rule(q(sem: QVar^E^[[qvar, QVar],[qattr,QVar,name],[person,QVar], 
	[lobj, E, QVar], SinvSem,[qcon,E,verb],[rule,who3]]),
     [wp(m_root:'whom'),
      sinv(voice:active, sem:E^SinvSem)
]).


%%%%%%%%%%%%%%%%%
%% WHAT QUESTIONS
%%%%%%%%%%%%%%%%%

%  WHAT1
%% What1: Q -> WP(what) FVP (active|passive)
%  "What was eating/was eaten by John?"
rule(q(sem: QVar^E^[[qvar, QVar],[lsubj, E, QVar], VPSem,[rule,what1a]]),
     [wp(m_root:'what'),
      fvp(voice:active, sem:E^VPSem)
]).
rule(q(sem: QVar^E^[[qvar, QVar],[lobj, E, QVar], VPSem,
	[qcon,E,verb],[rule,what1b]]),
     [wp(m_root:'what'),
      fvp(voice:passive, sem:E^VPSem)
]).


%  WHAT2
%% What2: Q -> WP(what) VPCORE(be) NP ?
%  "What is the capital of Uganda?"
rule(q(sem: QVar^E^[[qvar, QVar],[lsubj,E,QVar],
		    VPSem,NP2Sem,ASem,[rule,what2]]),
     [wp(m_root:'what'),
      vpcore(m_root:'be',sem:E^VPSem),
      np(sem:QVar^NP2Sem)
]).

%  WHAT3
%% What3: Q -> WP(what) SINV
%  "What did John eat?"
rule(q(sem: QVar^E^[[qvar, QVar],[lobj, E, QVar], SinvSem,
	[qcon,E,verb],[rule,what3]]),
     [wp(m_root:'what'),
      sinv(voice:active, sem:E^SinvSem)
]).

%  WHAT4
%% WHAT4: Q -> WP(what) VPCORE(be) DT N(name|term) IN(of|for) NP
%  What is the name of Whistler's mother?
rule(q(sem: QVar^E^[[qvar, QVar],[qattr,QVar,name],VPSem,NPSem,[rule,what4]]),
     [wp(m_root:'what'), 
      vpcore(m_root:'be',sem:E^VPSem), 
      dt(m_root:D),
      [n(m_root:'name'),  
       n(m_root:'term')],
      [in(m_root:'of'),
       in(m_root:'for')],
      np(sem:NPSem)
]). 

%  WHAT5
%% WHAT5: Q -> WP(what) VPCORE(be) DT N(capital) IN(of) NP
%  What is the capital of Uganda?
rule(q(sem: QVar^E^CapEnt^[[qvar, QVar],[qattr,QVar,name],
	[capital,CapEnt],[city,QVar],VPSem,NPSem,[rule,what5]]),
     [wp(m_root:'what'),
      vpcore(m_root:'be',sem:E^VPSem),
      dt(m_root:D),
      n(m_root:'capital'),
      in(m_root:'of'),
      np(sem:NPSem)  
]).


%  WHAT6
%% WHAT6: Q -> WP(what) VPCORE(be) NP VPCORE
%  What are pennies made of?
%  What are baby frogs called
rule(q(sem: QVar^E^NP^[[qvar, QVar],[lobj,E,NP],VSem,NPSem,[rule,what6]]),
     [wp(m_root:'what'),
      v(m_root:'be'),
      np(sem:NP^NPSem),
      vpcore(vform:nform,sem:E^VSem)
]).


%  WHAT6
%% WHAT6: Q -> WP(what) VPCORE(be) NP VPCORE P
%  What is tequila made from?
rule(q(sem: QVar^E^NP^[[qvar, QVar],[lobj,E,NP],[P,E,QVar],VSem,NPSem,[rule,what7]]),
     [wp(m_root:'what'),
      v(m_root:'be'),
      np(sem:NP^NPSem),
      vpcore(vform:nform,sem:E^VSem),
      in(m_root:P)
]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WHNP QUESTIONS: What/which/how many/how much/ X
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  WHNPQ0
%% WhNP0: Q -> WHNP
%  Default
rule(q(sem: [WHNPSem]),
      [whnp(sem:WHNPSem)
]).

%  WHNPQ1
%% WhHPQ1: Q -> WHNP FVP(active|passive)
%  "What|which car consumes the least petrol?"
% This also works for how much/how many, etc.
rule(q(sem: QVar^E^[[lsubj,E,QVar],NPSem,VPSem,[qcon,E,verb],[rule,whnpq1a]]),
     [whnp(sem:QVar^NPSem),
      fvp(sem:E^VPSem,voice:active)
]).
rule(q(sem: QVar^E^[[lobj,E,QVar],NPSem,VPSem,[qcon,E,verb],[rule,whnpq1b]]),
     [whnp(sem:QVar^NPSem),
      fvp(sem:E^VPSem,voice:passive)
]).

%  WHNPQ2
%% WhNPQ2: Q -> WHNP VPCORE(be) NP ?
%  "[What|which city] is the capital of Uganda?"
rule(q(sem: QVar^E^[[lsubj,E,QVar],
		    NP1Sem,VPSem,NP2Sem,[rule,whnpq2]]),
      [whnp(sem:QVar^NP1Sem),
      vpcore(m_root:'be',sem:E^VPSem),
      np(sem:QVar^NP2Sem)
]).

%  WHNPQ3
%% WhNPQ3: Q -> WHNP SINV
% "What food did John eat?"
rule(q(sem: QVar^E^[[lobj, E, QVar], NPSem, SinvSem,
	[qcon,E,verb],[rule,whnpq3]]),
     [whnp(sem:QVar^NPSem),
      sinv(voice:active, sem:E^SinvSem)
]).

%  WHNPQ4
%% WhNPQ4: Q -> WHNP(head="year"|"date") SINV(active)
% "What year did Columbus discover America?"
rule(q(sem: QVar^E^[NPSem, SinvSem,[qcon,E,verb],[rule,whnpq4]]),
     [[whnp(s_form:'year',sem:QVar^NPSem),whnp(s_form:'date',sem:QVar^NPSem)],
      sinv(voice:active, sem:E^SinvSem)
]).

%  WHNPQ5
%% WhNPQ5: Q -> WHNP(head="year") SINV(passive)
% "What year was the Magna Carta signed ?
rule(q(sem: QVar^E^[[in, E, QVar], NPSem, SinvSem,[qcon,E,verb],[rule,whnpq5]]),
     [whnp(s_form:'year',sem:QVar^NPSem),
      sinv(voice: passive,sem:E^SinvSem)
]).

%  WHNPQ6
%% WHNPPQ6: WHNP -> WP|WDT  POSS BNP_CORE
%  What fruit's stone does Laetrile come from?
rule(q(sem:QVar^N^E^[[qvar, QVar],[lobj,E,N],[realisation,E,Edge],
           PossNSem,NSem,SinvSem,[rule,whnpq6]]), 
    [[wp(m_root:'what'),wdt(m_root:'which')],
     poss(sem:N^QVar^PossNSem),
     bnp_core(sem:N^NSem,number:Num,s_form:S),
     sinv(voice:active,sem:E^SinvSem)
]).


%%%%%%%%%%%%%%%%%%
%% WHPP QUESTIONS
%%%%%%%%%%%%%%%%%%

%  WHPPQ0
%% WhPPQ0: Q -> WHPP
%  Default
rule(q(sem: [WHPPSem,[rule,whppq0]]),
      [whpp(sem:WHPPSem)
]).
 
%  WHPPQ1
%% WhPPQ1: Q -> WHPP SINV
%  At what age did Rossini stop writing opera?
rule(q(sem: QVar^E^Agent^[PPSem,SinvSem,[qcon,E,verb],[rule,whppq1]]),
     [whpp(sem:QVar^E^PPSem),
      sinv(sem:E^Agent^SinvSem)
]).

%%%%%%%%%%%%%%%%%%
%% WHERE QUESTIONS
%%%%%%%%%%%%%%%%%%

%  Where0
%% Where0: Q -> WRB(where)
%  Default
rule(se(sem:QVar^[[qvar,QVar],[qattr,QVar,name],[location,QVar],[rule,where0]]),
     [wrb(m_root:'where')
]).

%  Where1
%% Where1: Q -> WRB(where) VPCORE(be) NP
%  "Where is Bolivia?"
rule(q(sem: QVar^E^X^[[qvar, QVar],[qattr,QVar,name],[location, QVar],
		      [in, X, QVar], VPSem,NPSem,[rule,where1]]),
     [wrb(m_root:'where'),
      vpcore(m_root:'be',sem:E^VPSem),
      np(sem:X^NPSem)
]).

%  Where2
%% Where2: Q -> WRB(where) SINV
%  "Where did Dylan Thomas die?"
rule(q(sem: QVar^E^[[qvar, QVar],[qattr,QVar,name],[location, QVar], 
	[qcon,E,verb], SinvSem,[rule,where2]]),
     [wrb(m_root:'where'),
      sinv(sem:E^SinvSem)
]).


%%%%%%%%%%%%%%%%%
%% WHEN QUESTIONS
%%%%%%%%%%%%%%%%%

%  When0
%% When0: Q -> WRB(when)
%  Default
rule(q(sem:QVar^[[qvar,QVar],[qattr,QVar,name],[date,QVar],[rule,when0]]),
     [wrb(m_root:'when')
]).


%  When1
%% When1: Q -> WRB(when) VPCORE NP
%  "When is Christmas?
rule(q(sem: QVar^E^X^[[qvar, QVar],[qattr,QVar,name],[date, QVar],
		      [on, X, QVar],VPSem,NPSem,[rule,when1]]),
     [wrb(m_root:'when'),
      vpcore(m_root:'be',sem:E^VPSem),
      np(sem:X^NPSem)
]).

%  When2
%% When2: Q -> WRB(when) SINV
%  "When did Nelson Mandela become President?"
rule(q(sem: QVar^E^[[qvar, QVar],[qattr,QVar,name],[date, QVar],
		    [in, E, QVar],[qcon,E,verb],SinvSem,[rule,when2]]),
     [wrb(m_root:'when'),
      sinv(sem:E^SinvSem)
]).

%%%%%%%%%%%%%%%%%
%% HOW QUESTIONS
%%%%%%%%%%%%%%%%%

%  How1
%% How1: Q -> WRB(how) SINV
% "How did Socrates die" = Socrates died by ___
rule(q(sem: QVar^E^[[qvar,QVar],[qcon,E,verb],SinvSem,[rule,how1]]),
     [wrb(m_root:'how'),
      sinv(sem:E^SinvSem)
]).

%  How2a
%% How2a: Q -> WRB(how) RB SINV
% "How often is someone murdered in the US
%  default for adverbs
rule(q(sem: QVar^E^[[qvar,QVar],[qcon,E,verb],SinvSem,[rule,how2a]]),
     [wrb(m_root:'how'),
      rb(m_root: RB),
      sinv(sem:E^SinvSem)
]).


%  How2b
%% How2b: Q -> HOWADVP SINV
% "How fast/quickly can a nuclear submarine travel?
rule(q(sem: QVar^E^[[qvar,QVar],[qcon,E,verb],HowAdvpSem,SinvSem,[rule,how2b]]),
     [howadvp(sem:QVar^HowAdvpSem),
      sinv(sem:E^SinvSem)
]).

%  How2b
%% How2b: Q -> HOWADVP SINV
% "How fast/quickly can a nuclear submarine travel?
rule(q(sem: QVar^E^[[qvar,QVar],[qcon,E,verb],HowAdvpSem,SinvSem,[rule,how2b1]]),
     [howadjp(sem:QVar^HowAdvpSem),
      fvp(sem:E^SinvSem)
]).



%  How2c
%% How2c: Q -> HOWADJP SINV
% "How big does a pig get?
rule(q(sem: QVar^E^[[qvar,QVar],[qcon,E,verb],HowAdjpSem,SinvSem,[rule,how2c]]),
     [howadjp(sem:QVar^HowAdjpSem),
      sinv(sem:E^SinvSem)
]).

%  How3a
%% How3a: Q -> HOW JJ(miscellaneous -- not handled by HOWADJP) VPCORE(be) NP
%  "How accurate are HIV tests
%  NB: Last rule in source file which matches is preferred
rule(q(sem: QVar^X^[[qual,QVar,X],[adj,X,JJ],NPSem,[rule,how3a]]),
     [wrb(m_root:'how'),
      jj(m_root:JJ),
      vpcore(m_root:'be',sem:_),
      np(sem:X^NPSem)
]).


%  How3b
%% How3b: Q -> HOWADJP VPCORE(be) NP
%  "How tall is the Statue of Liberty?"
%  NB: Last rule in source file which matches is preferred
rule(q(sem: QVar^X^[[qvar,QVar],[qual,QVar,X],NPSem,HowAdjpSem,[rule,how3b]]),
     [howadjp(sem:QVar^HowAdjpSem),
      vpcore(m_root:'be',sem:_),
      np(sem:X^NPSem)
]).


%  How4
%% How4: Q -> HOWADVP VPCORE(be) NP
%  "How fast is an eye blink?"
rule(q(sem: QVar^X^[[qual,QVar,X],NPSem,HowAdvpSem,[rule,how4]]),
     [howadvp(sem:QVar^HowAdvpSem),
      vpcore(m_root:'be',sem:_),
      np(sem:X^NPSem)
]).

%  How5
%% How5: Q -> WRB(how) JJ(long) VPCORE(do) PRP(it) VPCORE(take) NFVP
% "How long does it take to X"
rule(q(sem: QVar^E^[[qvar, QVar],[time,QVar],
		    [duration_of, E,QVar], VPSem, ASem,[rule,how5]]),
     [wrb(m_root:'how'),
      jj(m_root:'long'),
      vpcore(m_root:'do',vform:sform),
      prp(m_root:'it'),
      vpcore(m_root:'take'),
      nfvp(sem:E^VPSem)
]).





%  How6a
%% How6a: Q -> HOWADJP(How far) VPCORE(be) NP IN(from) NP
%  "How far is Yaroslavl from Moscow?
rule(q(sem: QVar^X1^X2^[NPSem1,NPSem2,HowAdjpSem,[rule,how6a]]),
     [howadjp(sem:QVar^HowAdjpSem,s_form:'far'),
      vpcore(m_root:'be',sem:_),
      bnp(sem:X1^NPSem1),
      in(s_form:'from'),
      bnp(sem:X2^NPSem2)
]).

%  How6b
%% How6b: Q -> HOWADJP(How far) VPCORE(be) PPS(it) IN(from) NP TO(to) NP
%  "How far is it from Earth to Mars?
rule(q(sem: QVar^X1^X2^[NPSem1,NPSem2,HowAdjpSem,[rule,how6b]]),
     [howadjp(sem:QVar^HowAdjpSem,s_form:'far'),
      vpcore(m_root:'be',sem:_),
      pps(s_form:'it'),
      in(s_form:'from'),
      bnp(sem:X1^NPSem1),
      to(s_form:'to'),
      bnp(sem:X2^NPSem2)
]).


%  How7
%% How7: Q -> HOWADJP (how close) VPCORE(be) NP To(to) NP
%  "How close is Mercury to the Sun?
rule(q(sem: QVar^X1^X2^[NPSem1,NPSem2,HowAdjpSem,[rule,how7]]),
     [howadjp(sem:QVar^HowAdjpSem,s_form:'close'),
      vpcore(m_root:'be',sem:_),
      bnp(sem:X1^NPSem1),
      to(s_form:'to'),
      bnp(sem:X2^NPSem2)
]).


%  How8
%% How8: Q -> WRB(how) JJ(long) IN(before|...)  SBAR
% "How long before ...?
rule(q(sem: QVar^[[qvar, QVar],[measure,QVar],[measure_type,Qvar,time],
		     SSem, [rule,how8]]),
     [wrb(m_root:'How'),
      rb(m_root:'long'),
      sbar(sem:QVar^SSem)]
 ).


%%%%%%%%%%%%%%%%
%% WHY QUESTIONS
%%%%%%%%%%%%%%%%

%  Why0
%% Why0: Q -> WRB(why)
%  Default
rule(q(sem:QVar^[[qvar,QVar],[rule,why0]]),
    [wrb(m_root:'why')
]).

%  Why1
%% Why1: Q -> WRB(why) SINV
%  Why did David Koresh ask the FBI for a word processor?
rule(q(sem: QVar^E^[[qvar,QVar],SSem,[rule,why1]]),
    [wrb(m_root:'why'),
     sinv(sem:E^SSem)
]).

%%%%%%%%%%%%%%%%%%%%%%%%%
% Embedded WHPP QUESTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%

%  EWHPP1
%% EWHPP1: Q -> S WHPP
%  John died/ate quiche in which Northern European country?
rule(q(sem:QVar^E^[SSem,WHPPSem,[qcon,E,verb],[rule,ewhpp1]]),
     [s(sem:E^SSem),
      whpp(sem:E^QVar^WHPPSem)
]).

%  EWHPP2
%% EWHPP2: Q -> NP VPCORE(be) WHPP
%  The Faroes are in which Northern European country?
rule(q(sem:QVar^E^X^[NPSem,VPSem,WHPPSem,[rule,ewhpp2]]),
     [np(sem:X^NPSem),
      vpcore(m_root:'be',sem:E^VPSem),    
      whpp(sem:X^QVar^WHPPSem)
]).

%%%%%%%%%%%
%% COMMANDS
%%%%%%%%%%%

%  Name1
%% Name0: Q -> N(name) NP
%  Default

rule(q(sem: QVar^[[qvar, QVar],[qattr,QVar,name],NPSem,[rule,name0]]),
      [n(m_root:'name'),
       np(sem:NPSem)
]).

%  Name1
%% Name1: Q -> N(name) BNP RELC
%  Name the first man who walked on the moon
%  event marked as a type2 constraint
rule(q(sem: QVar^E^Agent^[[qvar,QVar],[qattr,QVar,name],NPSem,[qcon,E,verb],
	     RelcSem,[rule,name1]]),
      [n(m_root:'name'),
       bnp(sem:Agent^NPSem),
       relc(sem:Agent^E^RelcSem)
]).

%  Name2
%% Name2: Q -> N(name) BNP NFVP(base) 
%  Name the first man to walk on the moon
%  event marked as a type2 constraint
rule(q(sem: QVar^E^Agent^[[qvar,QVar],[qattr,QVar,name],NPSem,[qcon,E,verb],
	     InfSem,[rule,name2]]),
      [n(m_root:'name'),
       bnp(sem:Agent^NPSem),
       nfvp(vform:base,sem:E^InfSem)
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SINV Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%rule(q(sem:SinvSem),
%    [sinv(sem:SinvSem)
%]).

%% SINV -> V (do|will) NP AV ?
% Did/Will John die?
rule(sinv(voice:active,sem:Event^Agent^X^[NPSem,[R,Event],VSem,[time, Event,T],
			   [lsubj,Event,Agent],ASem,PPSem,[rule,sinv1]]),
     [ [v(m_root:'do',tense:T,vform:F,number:N,person:P),
       md(_)],
       np(person:P,number:N,sem:Agent^NPSem),
       vpcore(m_root:R,vform:base,sem:Event^VSem),
       {[adjuncts(sem:ASem),pp(sem:Event^X^PPSem)]}
%       sym(s_form:'?')
]).

%% SINV -> V (do|will) NP AV NP ?
% Did/Will John eat the elephant?
rule(sinv(voice:active, sem:Event^Agent^Obj^[NPSem,[R,Event],VSem,[time,Event,T],NP2Sem,
			       [lsubj,Event,Agent],[lobj,Event,Obj],ASem,[rule,sinv2]]),
     [ [v(m_root:'do',tense:T,vform:F, number:N,person:P),
       md(s_form:_)],
       np(person:P,number:N,sem:Agent^NPSem),
       vpcore(m_root:R,vform:base,sem:Event^VSem),
       np(sem:Obj^NP2Sem),
       {adjuncts(sem:ASem)}
%       sym(s_form:'?')
]).      

%% SINV -> V (do|will) NP AV ?
%  Was the Magna Carta signed ?
rule(sinv(voice:passive,sem:Event^Patient^X^[NPSem,[R,Event],ASem,[time, Event,T],
			   [lobj,Event,Patient],PPSem,[rule,sinv3]]),
     [ v(m_root:'be',tense:T,vform:F,number:N,person:P),
       np(person:P,number:N,sem:Patient^NPSem),
       [vpcore(m_root:R,vform:nform,sem:Event^ASem),
	vpcore(m_root:R,vform:dform,sem:Event^ASem)], % dform for bad tagging
       {[adjuncts(sem:ASem),pp(sem:Event^X^PPSem)]}
%       sym(s_form:'?')
]).



%% SINV -> V (be) NP?
%  is the River Seine?
rule(sinv(voice:passive,sem:Agent^Verb^[NPSem,[time,Verb,T],[lsubj,Verb,Agent],[be,Verb],
[rule,sinv3b]]),
     [ v(m_root:'be',tense:T,vform:F,number:N,person:P),
       np(person:P,number:N,sem:Agent^NPSem)
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHNP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  WHNP1
%% WHNP1: WHNP -> WP|WDT  BNP_CORE
%  what|which city
%  which city in Nevada
rule(whnp(sem:QVar^[H,[qvar, QVar],[realisation,E,Edge],
	PPSem,[rule,whnp1]],number:N,s_form:S,edge:Edge), 
    [[wp(m_root:'what'), wdt(m_root:'which')],
      bnp_core(sem:QVar^H,number:N,s_form:S),
      {pp(sem:QVar^PPSem)}
]).

%  WHNP2
%% WHNP2: WHNP -> WRB(how) JJ (many) BNP_CORE {PP}
%  how many inches 
%  how many cities in Nevada
rule(whnp(sem:QVar^[[qvar,QVar],[qattr,QVar,count],[realisation,QVar,Edge],
	H,PPSem,[rule,whnp2]]), 
    [wrb(m_root:'how'),
     jj(m_root:'many'),
     bnp_core(sem:QVar^H),
     {pp(sem:QVar^PPSem)}
]).

%  WHNP3
%% WHNP3: WHNP -> WRB(how) JJ (many) PP
%  how many of the gold medallists
rule(whnp(sem:QVar^[[qvar,QVar],[qattr,QVar,count],[realisation,QVar,Edge],
	H,E^Qvar^PPSem,[rule,whnp3]]), 
    [wrb(m_root:'how'),
     jj(m_root:'many'),
     pp(sem:E^QVar^PPSem)
]).

%  WHNP4
%% WHNP4: WHNP -> WRB(how) JJ(much) | RB(much) 
%  Ask for a quantity  
%  how much (assume "how much" on its own is money)
rule(whnp(sem:QVar^[[qvar, QVar],[money,QVar],[qattr,QVar,name],
	[realisation,QVar,Edge],[rule,whnp4]]),
    [wrb(m_root:'how'),
     [jj(m_root:'much'),rb(m_root:'much')]
]).

%  WHNP5
%% WHNP5: WHNP -> WRB(how) JJ(much) | RB(much) BNP_CORE {PP}
%  Ask for a quantity  
%  how much butter
rule(whnp(sem:QVar^[[qvar, QVar],[realisation,QVar,Edge],[qattr,QVar,count],
		H,PPSem,[rule,whnp5]]), 
    [wrb(m_root:'how'),
     [jj(m_root:'much'),rb(m_root:'much')],
     bnp_core(sem:QVar^H),
     {pp(sem:QVar^PPSem)}
]).

%  WHNP6
%% WHNP6: WHNP -> WRB(how) JJ(much) | RB(much) BNP_CORE {PP}
%  Ask for a quantity  
%  how much butter
rule(whnp(sem:QVar^[[qvar, QVar],[realisation,QVar,Edge],[qattr,QVar,count],
		H,PPSem,[rule,whnp6]]), 
    [wrb(m_root:'how'),
     [jj(m_root:'much'),rb(m_root:'much')],
     bnp_core(sem:QVar^H),
     {pp(sem:QVar^PPSem)}
]).

%  WHNP7
%% WHNP7: WHNP -> WRB(What) BNP_CORE(day|month|year) 
%  Ask for a date 
%  What day did Pearl Harbor occur?
rule(whnp(sem:QVar^[[qvar, QVar],[realisation,QVar,Edge],[qattr,QVar,name],
		[date,QVar],[rule,whnp7]]), 
    [[wp(m_root:'what'), wdt(m_root:'which')],
     [n(m_root:'day'),n(m_root:'month'),n(m_root:'year')]
]).


%  WHNP8
%% WHNP8: WHNP -> WRB(What) 
%  Ask for a date 
%  What day and month did Nixon resign
rule(whnp(sem:QVar^[[qvar, QVar],[realisation,QVar,Edge],[qattr,QVar,name],
		[date,QVar],[rule,whnp7b]]), 
    [[wp(m_root:'what'), wdt(m_root:'which')],
     [bnp_core(m_root:'day'),bnp_core(m_root:'month'),bnp_core(m_root:'year')],
     cc(_),     [bnp_core(m_root:'day'),bnp_core(m_root:'month'),bnp_core(m_root:'year')]
]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WHPP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  WHPP1
%% WHPP1: WHPP -> IN WHNP
%  At what age
%  In which city in Nevada
rule(whpp(sem:E^QVar^[[R,E,QVar],WHNPSem,[realisation,E,Edge],[rule,whpp1]]),
    [in(m_root:R),
     whnp(sem:QVar^WHNPSem)
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOWADJP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  HOWADJP1a
%% HOWADJP1a: HOWADJP -> WRB(how) JJ(far|wide|near|close|wide|tall|high|large|big|small|huge
%  How tall is the Statue of Liberty?
rule(howadjp(sem:QVar^[[qvar, QVar],[qattr,QVar,count],[qattr,QVar,unit],
	[measure,QVar],[measure_type,QVar,distance],[realisation,QVar,Edge],[rule,howadjp1a]],s_form:S),
    [wrb(m_root:'how'),
     [jj(m_root:'far',s_form:S),rb(m_root:'far',s_form:S),jj(m_root:'near',s_form:S),jj(m_root:'close',s_form:S),v(m_root:'close',s_form:S),
      jj(m_root:'long',s_form:S),jj(m_root:'wide',s_form:S),jj(m_root:'tall',s_form:S),jj(m_root:'high',s_form:S),n(m_root:'high',s_form:S),
      jj(m_root:'large',s_form:S),jj(m_root:'big',s_form:S),jj(m_root:'small',s_form:S),jj(m_root:'huge',s_form:S)]
]).

%  HOWADJP1b
%% HOWADJP1b: HOWADJP -> WRB(how) JJ(far|wide|near|close|wide|tall|high|large|big|small|huge
%  How tall in feet is the Statue of Liberty?
%  Units not handled properly
rule(howadjp(sem:QVar^X^[[qvar, QVar],[qattr,QVar,count],
	[measure,QVar],[measure_type,QVar,distance],PNPSem,[realisation,QVar,Edge],[rule,howadjp1b]],s_form:S),
    [wrb(m_root:'how'),
     [jj(m_root:'far',s_form:S),rb(m_root:'far',s_form:S),jj(m_root:'near',s_form:S),jj(m_root:'close',s_form:S),v(m_root:'close',s_form:S),
      jj(m_root:'long',s_form:S),jj(m_root:'wide',s_form:S),jj(m_root:'tall',s_form:S),jj(m_root:'high',s_form:S),n(m_root:'high',s_form:S),
      jj(m_root:'large',s_form:S),jj(m_root:'big',s_form:S),jj(m_root:'small',s_form:S),jj(m_root:'huge',s_form:S)],
      in(m_root:in),
      bnp(sem:X^PNPSem)
]).


%  HOWADJP2
%% HOWADJP2: HOWADJP -> WRB(how) JJ(heavy|massive)
%  How heavy is the Statue of Liberty?
rule(howadjp(sem:QVar^[[qvar, QVar],[qattr,QVar,count], [qattr,QVar,unit],
	[measure,QVar],[measure_type,QVar,mass],[realisation,QVar,Edge],[rule,howadjp2]]),
    [wrb(m_root:'how'),
     [jj(m_root:'heavy'),jj(m_root:'massive')]
]).

%  HOWADJP3
%% HOWADJP3: HOWADJP -> WRB(how) JJ(late)
%  How late is Disneyland open?
rule(howadjp(sem:QVar^[[qvar, QVar],
	[date,QVar],[realisation,QVar,Edge],[rule,howadjp3]]),
    [wrb(m_root:'how'),
     jj(m_root:'late')
]).

%  HOWADJP4
%% HOWADJP4: HOWADJP -> WRB(how) JJ(fast)
%  How fast can a nuclear submarine travel?
rule(howadjp(sem:QVar^[[qvar, QVar],[qattr,QVar,count],[qattr,QVar,unit],
	[measure,QVar],[measure_type,QVar,speed],[realisation,QVar,Edge],[rule,howadjp4]]),
    [wrb(m_root:'how'),
     jj(m_root:'fast')
]).


%  HOWADJP5
%% HOWADJP5: HOWADJP -> WRB(how) JJ(hot|cold)| N(cold)
%  How hot/cold/ is the sun?
rule(howadjp(sem:QVar^[[qvar, QVar],[qattr,QVar,count],[qattr,QVar,unit],
	[measure,QVar],[measure_type,QVar,temp],[realisation,QVar,Edge],[rule,howadjp5]]),
    [wrb(m_root:'how'),
     [jj(m_root:'hot'), jj(m_root:'cold'),n(m_root:'cold')]
]).

%  HOWADJP6
%% HOWADJP6: HOWADJP -> WRB(how) JJ(old)
%  How old is the Red Pyramid?
rule(howadjp(sem:QVar^[[qvar, QVar],[qattr,QVar,count],[qattr,QVar,unit],
	[measure,QVar],[measure_type,QVar,age],[realisation,QVar,Edge],[rule,howadjp6]]),
    [[wrb(m_root:'how'),wrb(m_root:'How')],
     jj(m_root:'old')
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOWADVP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  HOWADVP1
%% HOWADVP1: HOWADVP -> WRB(how) JJ(fast)|RB(quickly)
%  How fast can a nuclear submarine travel?
rule(howadvp(sem:QVar^[[qvar, QVar],[qattr,QVar,count],[qattr,QVar,unit],
	[measure,QVar],[measure_type,QVar,speed],[realisation,QVar,Edge],[rule,howadvp1]]),
    [wrb(m_root:'how'),
     [rb(m_root:'fast'),rb(m_root:'quickly')]
]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adjuncts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% to New York
rule(adjuncts(sem:[PPSem]),
    [pp(sem:PPSem)
]).

% To New York in a Concorde
rule(adjuncts(sem:[PP1Sem,PP2Sem]),
    [pp(sem:PP1Sem),
     pp(sem:PP2Sem)
]).

% when Columbus landed in 1492
rule(adjuncts(sem:[SBarSem]),
    [sbar(sem:SBarSem)
]).

% to set foot
%rule(adjuncts(sem:[NFVPSem]),
%    [nfvp(sem:NFVPSem)
%]).

% to set foot on the moon 
%rule(adjuncts(sem:[NFVPSem,PPSem]),
%    [nfvp(sem:NFVPSem),
%     pp(sem:PPSem)
%]).

