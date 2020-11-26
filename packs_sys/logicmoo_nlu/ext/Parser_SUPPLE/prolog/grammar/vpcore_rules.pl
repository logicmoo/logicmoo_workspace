/* ------------------------------------------------------------------------
 > FILENAME:	vpcore_rules.pl
 > PURPOSE:	A basic feature-based core VP grammar for English
 > AUTHORS:	R. Gaizauskas, adapted for buchart by Kevin Humphreys
 > DATE:	Jul 12 1996, adapted for buchart Aug 27 1996
 > LAST MOD:    29/07/00 RJG
 > NOTES:	Based on the treatment in Quirk + Greenbaum's 
                "A University Grammar of English" Chapter 3
 ------------------------------------------------------------------------ */

:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

%
% Best Parse Categories
best_parse_cats([vpcore,np,pp]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Handles:
%     nonmodal and modal auxiliaries
%     embedded adverbial phrases (negation is handled this way)
%
% Verb features used: 
%    number: sing | plural
%    person: 1 | 2 | 3 
%    tense: none | present | past | modal
%    vform: base | sform | dform | nform | gform 
%    voice: active | passive
%    aspect: simple | perf | prog | perfprog
%
% The basic analysis is:
% 
% VPCORE -> FVPCORE | NFVPCORE
% FVPCORE -> NONMODAL_VPCORE | MODAL_VPCORE
% NONMODAL_VPCORE -> NONMODAL_VPCORE1 | NONMODAL_VPCORE2
% NONMODAL_VPCORE1 -> VPCORE1 | VPCORE2 | VPCORE3 | VPCORE4
% NONMODAL_VPCORE2 -> VPCORE5
% MODAL_VPCORE -> MD NONMODAL_VPCORE1
% MODAL_VPCORE -> MD ADVP NONMODAL_VPCORE1
% MODAL_VPCORE -> MD NONMODAL_VPCORE1 ADVP
% VPCORE1(tense:present,voice:active) -> AV(vform:sform) 
% VPCORE1(tense:present,voice:active) -> AV(vform:base) 
% VPCORE1(tense:past,voice:active) -> AV(vform:dform)
% VPCORE1(voice:passive) -> V(mroot:be) AV(vform:nform) 
% VPCORE1(tense:none,aspect:simple,voice:active) -> V(vform:base)
% VPCORE2(aspect:prog,voice:active) -> V(mroot:be) AV(vform:gform)
% VPCORE2(aspect:prog,voice:passive) -> V(mroot:be) AV(mroot:be,vform:gform) 
%                                       AV(vform:nform)
% VPCORE3(aspect:perf,voice:active) -> V(mroot:have) AV(vform:nform)
% VPCORE3(aspect:perf,voice:active) -> V(mroot:have) AV(vform:dform) % for mistagging
% VPCORE3(aspect:perf,voice:passive) -> V(mroot:have) AV(mroot:be,vform:nform) 
%                                       AV(vform:nform)
% VPCORE4(aspect:perfprog,voice:active) -> V(mroot:have) AV(mroot:be,vform:nform) 
%                                          AV(vform:gform)
% VPCORE4(aspect:perfprog,voice:passive) -> V(mroot:have) AV(mroot:be,vform:nform)
%                                           AV(mroot:be,vform:gform) AV(vform:nform)
% VPCORE5(aspect:simple,voice:active) -> V(mroot:do),AV(vform:base)
% NFVPCORE(tense:none,aspect:simple,voice:passive) -> AV(vform:nform)
% NFVPCORE(tense:none,aspect:simple,voice:actsive) -> AV(vform:gform)
% NFVPCORE(tense:none,aspect:perfprog,voice:passive) -> V(mroot:have,vform:gform) 
%                                                       AV(mroot:be),vform:nform)
%                                                       AV(vform:nform)
% NFVPCORE(tense:none,aspect:simple) -> TO AV(vform:base)
% AV -> V | ADVP V  | V ADVP
%
% ADVP -> RB | RB RB
% where VPCORE = VP core
%       FVPCORE = finite VP core (tense /= none)
%       NFVPCORE - non-finite VP core (tense = none)
%       NONMODAL_VPCORE = non-modal VP core
%       MODAL_VPCORE = modal VP core
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 



% convert verb + particle into compound verb
% V -> V RP
rule(v(s_form:S,m_root:compound([VR,'_',PR]),person:P,number:N,tense:T,vform:F),
        [v(s_form:S,m_root:VR,person:P,number:N,tense:T,vform:F),
         rp(m_root:PR)]).

% Optional ADVP + V or V + ADVP
% - avoid duplicate vpcore rules for each possible ADVP position
rule(av(s_form:S,m_root:R,person:P,number:N,tense:T,vform:F),
	[
        v(s_form:S,m_root:R,person:P,number:N,tense:T,vform:F)
        ]).
rule(av(s_form:S,m_root:R,person:P,number:N,tense:T,vform:F,sem:E^ASem),
	[
        advp(sem:E^ASem),
        v(s_form:S,m_root:R,person:P,number:N,tense:T,vform:F)
        ]).
rule(av(s_form:S,m_root:R,person:P,number:N,tense:T,vform:F,sem:E^ASem),
	[
        v(s_form:S,m_root:R,person:P,number:N,tense:T,vform:F),
        advp(sem:E^ASem)
        ]).

%%% Verbs tagged as JJ's
rule(av(s_form:S,m_root:R,person:P,number:N,tense:past,vform:dform),
	[
        jj(s_form:S,m_root:R,m_affix:ed)
        ]).

% VP Cores are either finite or non-finite
% finite VPs must have tense = present | past | modal
rule(vpcore(m_root:R,tense:present,aspect:A,number:N,person:P,voice:V,
	sem:E^[Sem,[time,E,present],[aspect,E,A],[voice,E,V]]),
	[ 
        fvpcore(m_root:R,tense:present,aspect:A,number:N,person:P,voice:V,sem:E^Sem)
	]).
rule(vpcore(m_root:R,tense:past,aspect:A,number:N,person:P,voice:V,
	sem:E^[Sem,[time,E,past],[aspect,E,A],[voice,E,V]]),
	[ 
        fvpcore(m_root:R,tense:past,aspect:A,number:N,person:P,voice:V,sem:E^Sem)
	]).
rule(vpcore(m_root:R,tense:modal,aspect:A,number:N,person:P,voice:V,
	sem:E^[Sem,[time,E,modal],[aspect,E,A],[voice,E,V]]),
	[ 
        fvpcore(m_root:R,tense:modal,aspect:A,number:N,person:P,voice:V,sem:E^Sem)
	]).
% non-finite VPs must have tense = none
rule(vpcore(m_root:R,tense:none,aspect:A,number:N,person:P,voice:V,vform:F,
	sem:E^[Sem, [time,E,none],[aspect,E,A],[voice,E,V]]),
	[ 
        nfvpcore(m_root:R,tense:none,aspect:A,number:N,person:P,vform:F,voice:V,sem:E^Sem)
	]).


% Finite VP cores are either modal or nonmodal 
%    and must be tensed (i.e. tense /= none)
rule(fvpcore(m_root:R,tense:past,aspect:A,number:N,person:P,voice:V,sem:E^Sem),
	[ 
        nonmodal_vpcore(m_root:R,tense:past,aspect:A,number:N,person:P,voice:V,sem:E^Sem)
	]).
rule(fvpcore(m_root:R,tense:present,aspect:A,number:N,person:P,voice:V,sem:E^Sem),
	[
        nonmodal_vpcore(m_root:R,tense:present,aspect:A,number:N,person:P,voice:V,sem:E^Sem)
	]).
% Note tense shifted from none to modal
rule(fvpcore(m_root:R,tense:modal,aspect:A,number:N,person:P,voice:V,sem:E^Sem),
	[ 
        modal_vpcore(m_root:R,tense:none,aspect:A,number:N,person:P,voice:V,sem:E^Sem)
	]).
	 
% VP's with nonmodal auxiliaries
% Nonmodals may follow a modal (nonmodal_vpcore1) or
% may not (nonmodal_vpcore2) -- as with 'do'
rule(nonmodal_vpcore(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,sem:Sem),
	[
	nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem)
	]).
rule(nonmodal_vpcore(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,sem:Sem),
	[
	nonmodal_vpcore2(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem)
	]).

rule(nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem),
	[
	vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem)
	]).
rule(nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem),
	[
	vpcore2(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem)
	]).
rule(nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem),
	[
	vpcore3(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem)
	]).
rule(nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem),
	[
	vpcore4(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem)
	]).
	
rule(nonmodal_vpcore2(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem),
	[
	vpcore5(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:Sem)
	]).

% VP's with modal auxiliaries
%    I should write						
%    I should be writing
%    I should have written
%    I should have been writing
rule(modal_vpcore(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,
	sem:E^[Sem,[modal,E,M]]),
	[
	md(m_root:M), 
	nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:E^Sem)
	]).
rule(modal_vpcore(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,
	sem:E^[Sem,[modal,E,M],ASem]),
	[
	md(m_root:M), 
	advp(sem:E^ASem),
	nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:E^Sem)
	]).
rule(modal_vpcore(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,
	sem:E^[Sem,[modal,E,M],ASem]),
	[
	md(m_root:M), 
	nonmodal_vpcore1(m_root:R,tense:T,aspect:A,number:N,person:P,voice:V,vform:F,sem:E^Sem),
	advp(sem:E^ASem)
	]).

  
% Simple present/past and untensed
%  Active
%  	I write
%       He writes
%	I wrote 		
rule(vpcore1(m_root:R,tense:present,aspect:simple,number:N,person:P,voice:active,vform:sform,sem:E^[[R,E],ASem]),
	[ 
	av(m_root:R,vform:sform,sem:E^ASem)
	]).
rule(vpcore1(m_root:R,tense:present,aspect:simple,number:N,person:P,voice:active,vform:base,sem:E^[[R,E],ASem]),
	[ 
	av(m_root:R,vform:base,sem:E^ASem)
	]).
rule(vpcore1(m_root:R,tense:past,aspect:simple,number:N,person:P,voice:active,vform:dform,sem:E^[[R,E],ASem]),
	[ 
	av(m_root:R,vform:dform,sem:E^ASem)
	]).


%  Passive
%       I am written
%       I was written
rule(vpcore1(m_root:R,tense:T,aspect:simple,number:N,person:P,voice:passive,vform:F,sem:E^[[R,E],ASem]),
	[ 
	v(m_root:'be',tense:T,vform:F,number:N,person:P),
	av(m_root:R,vform:nform,sem:E^ASem)
	]).
%  Untensed
%       write (for use in e.g. modals -- I should write)
rule(vpcore1(m_root:R,tense:none,aspect:simple,number:N,person:P,voice:active,vform:base,sem:E^[R,E]),
	[ 
	v(m_root:R,vform:base)
	]).


% Progressive present/past
%  Active
%       I am writing
%       I was writing
rule(vpcore2(m_root:R,tense:T,aspect:prog,number:N,person:P,voice:active,vform:F,sem:E^[[R,E],ASem]),
	[ 
	v(m_root:'be',tense:T,vform:F,number:N,person:P),
	av(m_root:R,vform:gform,sem:E^ASem)
	]).	

%  Passive
%       I am being written
%       I was being written
rule(vpcore2(m_root:R,tense:T,aspect:prog,number:N,person:P,voice:passive,vform:F,sem:E^[[R,E],ASem1,ASem2]),
	[ 
	v(m_root:'be',tense:T,vform:F,number:N,person:P),
	av(m_root:'be',vform:gform,sem:E^ASem1),
	av(m_root:R,vform:nform,sem:E^ASem2)
	]).


% Perfective present/past
%  Active
%       I have written
%       I had written
rule(vpcore3(m_root:R,tense:T,aspect:perf,number:N,person:P,voice:active,vform:F,sem:E^[[R,E],ASem]),
	[ 
%	have_aux(tense:T,vform:F,number:N,person:P),
	v(m_root:'have',tense:T,vform:F,number:N,person:P),
	av(m_root:R,vform:nform,sem:E^ASem)
	]).

% Perfective past
%  Active
%       I have wrote
%  to cope with common mistagging
rule(vpcore3(m_root:R,tense:T,aspect:perf,number:N,person:P,voice:active,vform:F,sem:E^[[R,E],ASem]),
	[ 
%	have_aux(tense:T,vform:F,number:N,person:P),
	v(m_root:'have',tense:T,vform:F,number:N,person:P),
	av(m_root:R,vform:dform,sem:E^ASem)
	]).

%  Passive
%       I have been written
%       I had been  written
rule(vpcore3(m_root:R,tense:T,aspect:perf,number:N,person:P,voice:passive,vform:F,sem:E^[[R,E],ASem1,ASem2]),
	[ 
%	have_aux(tense:T,vform:F,number:N,person:P),
	v(m_root:'have',tense:T,vform:F,number:N,person:P),
	av(m_root:'be',vform:nform,sem:E^ASem1),
	av(m_root:R,vform:nform,sem:E^ASem2)
	]).


% Perfect progressive present/past
%  Active
%       I have been writing
%       I had been writing
rule(vpcore4(m_root:R,tense:T,aspect:perfprog,number:N,person:P,voice:active,vform:F,sem:E^[[R,E],ASem1,ASem2]),
	[ 
%	have_aux(tense:T,vform:F,number:N,person:P),	
        v(m_root:'have',tense:T,vform:F,number:N,person:P),
	av(m_root:'be',vform:nform,sem:E^ASem1),
	av(m_root:R,vform:gform,sem:E^ASem2)
	]).
%  Passive
%       I have been being written
%       I had been being written
rule(vpcore4(m_root:R,tense:T,aspect:perfprog,number:N,person:P,voice:passive,vform:F,sem:E^[[R,E],ASem1,ASem2,ASem3]),
	[ 
%	have_aux(tense:T,vform:F,number:N,person:P),
	v(m_root:'have',tense:T,vform:F,number:N,person:P),
	av(m_root:'be',vform:nform,sem:E^ASem1),
	av(m_root:'be',vform:gform,sem:E^ASem2),
	av(m_root:R,vform:nform,sem:E^ASem3)
	]).

% 'Do' auxiliaries
%       I do write 
%       I did write
rule(vpcore5(m_root:'do',tense:T,aspect:simple,number:N,person:P,voice:active,vform:F,sem:E^[[R,E],ASem]),
	[ 
	v(m_root:'do',tense:T,vform:F,number:N,person:P),
	av(m_root:R,vform:base,sem:E^ASem)
	]).


%%% Irrelevant comment -- next 2 rules were used to force incorrectly
%%% tagged vbn and vbg verbs to be finite -- changed for now ...
%%% catchall for strangely tagged verbs

%
% nfvpcore Nonfinite VP cores
%

% -ed/en participle
%    taken hostage
rule(nfvpcore(m_root:R,tense:none,aspect:simple,number:N,person:P,voice:passive,vform:nform,sem:E^[[R,E],ASem]),
	[ 
	av(m_root:R,vform:nform,sem:E^ASem)
	]).

% -ing participle
%    searching for treasure
rule(nfvpcore(m_root:R,tense:none,aspect:simple,number:N,person:P,voice:active,vform:gform,sem:E^[[R,E],ASem]),
	[ 
	av(m_root:R,vform:gform,sem:E^ASem)
	]).

%    having offended
% NB: this has another analysis under the grammar as an fvp via vpcore3 (have/has/had offended)
%     since having is assigned a tense (present) and there is no check for vform in vpcore3.
%     The present rule is preferred because it comes later in this source file -- so
%     DON'T MOVE THIS RULE
rule(nfvpcore(m_root:'have',tense:none,aspect:perfprog,number:N,person:P,voice:active,vform:F,sem:E^[[R,E],ASem1,ASem2]),
	[ 
	av(m_root:'have',tense:T,vform:gform,number:N,person:P),
	av(m_root:R,vform:nform,sem:E^ASem2)
	]).

%    having been offended
rule(nfvpcore(m_root:'have',tense:none,aspect:perfprog,number:N,person:P,voice:passive,vform:F,sem:E^[[R,E],ASem1,ASem2]),
	[ 
	av(m_root:'have',tense:T,vform:gform,number:N,person:P),
	av(m_root:'be',vform:nform,sem:E^ASem1),
	av(m_root:R,vform:nform,sem:E^ASem2)
	]).

% infinitive
%    to sleep
rule(nfvpcore(m_root:R,tense:none,aspect:simple,number:N,person:P,voice:_,vform:base,sem:E^[[R,E],ASem]),
	[
	to(m_root:'to'),
	av(m_root:R,vform:base,sem:E^ASem)
	]).
   

%% ADVP -> RB
rule(advp(sem:E^[adv,E,RB]),
    [rb(m_root:RB)]).

%% ADVP -> RB RB
rule(advp(sem:E^[[adv,E,RB1],[adv,E,RB2]]),
    [rb(m_root:RB1),
     rb(m_root:RB2)]).

%% HAVE_AUX -> V(mroot:have)
% These rules are necessary to cope with tagger unreliability in tagging
% auxiliary "have" as either VB or VBP -- i.e. as tense: none or tense:present
rule(have_aux(tense:present,vform:F,number:N,person:P),
    [v(m_root:'have',tense:none,vform:F,number:N,person:P)]).
rule(have_aux(tense:present,vform:F,number:N,person:P),
    [v(m_root:'have',tense:present,vform:F,number:N,person:P)]).
rule(have_aux(tense:past,vform:F,number:N,person:P),
    [v(m_root:'have',tense:past,vform:F,number:N,person:P)]).


