%Relative Clause rules
%

:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

% Best Parse Categories
best_parse_cats([relc,fvp,nfvp,np,pp]).
%


%% RELC -> WP VP % wh-pronoun
rule(relc(sem:E^V^[VPSem,[lsubj,V,E],[pronoun,E,R]]), [
    wp(m_root:R),
    vp(sem:V^VPSem)
]).

%% RELC -> WDT VP % wh-determiner
rule(relc(sem:E^V^[VPSem,[lsubj,V,E],[pronoun,E,R]]), [
    wdt(m_root:R),
    vp(sem:V^VPSem)
]).

%% SAM - new forms
%% RELC -> in which S

rule(relc(sem:E^Event^Agent^[SSem,[wherein,E,Event]]), [
    in(m_root:'in'),
    wdt(m_root:'which'),
    s(sem:Event^Agent^SSem)
]).

%% RELC -> wherein/where S

rule(relc(sem:E^Event^Agent^[SSem,[wherein,E,Event]]), [
    [wrb(m_root:'wherein'),wrb(m_root:'where')],
    s(sem:Event^Agent^SSem)
]). 


%% NP -> BNP RELC % minimally attach the rel clause to initial bnps
rule(np(sem:E^V^[NPSem,VPSem]), [
    top(_),
    bnp(sem:E^NPSem),
    relc(sem:E^V^VPSem)
]).

%% NP -> BNP COMMA RELC % minimally attach the rel clause to initial bnps
%% SAM -> Allow an optional second comma
rule(np(sem:E^V^[NPSem,VPSem]), [
    top(_),
    bnp(sem:E^NPSem),
    comma(_),
    relc(sem:E^V^VPSem)
]).

%% NP -> NP COMMA NP COMMA RELC
% apposition:
rule(np(sem:E1^E2^E3^[S1,S2,S3,[apposed,E1,E2]],
	      number:plural), [
    np(sem:E1^S1,number:N),
    comma(_),
    np(sem:E2^S2,number:N),
    comma(_),
    relc(sem:E1^E3^S3)
]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% retry PP, NP & VP rules which may include any new relative clauses

%% PP -> IN NP
rule(pp(sem:E^X^[[R,E,X],NPSem]),
    [in(m_root:R),
     np(sem:X^NPSem)]).
 
%% NP -> BNP PP(of) % minimally attach all 'of' PPs
rule(np(sem:E^X^[N1S,N2S,[of,E,X]],number:N), [
    bnp(sem:E^N1S,number:N),
    pp(sem:E^X^[[of,E,X],N2S])
]).
 
%% NP -> BNP
rule(np(sem:S,number:N), [
    bnp(sem:S,number:N)
]).
 
%% NP -> NP CC NP
% conjunction: the chairman and CEO
rule(np(sem:E1^E2^[S1,S2,[coord,E1,E2]],
              number:plural), [
    np(sem:E1^S1,number:N),
    cc(_),
    np(sem:E2^S2,number:N)
]).
 
%% VP -> VPCORE NP 
rule(vp(edge:Edge,person:P,number:N,tense:T,
    sem:E^X^[VSem,[lobj,E,X],NPSem,[realisation,E,Edge]]),
    [vpcore(m_root:R,person:P,number:N,tense:T,sem:E^VSem),
     np(sem:X^NPSem)]).

