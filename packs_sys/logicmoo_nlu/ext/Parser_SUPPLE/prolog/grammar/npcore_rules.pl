
:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% npcore_rules.pl -- A Basic NP grammar (head nouns with simple
%                 premodifiers -- i.e. no nested NP's)
%
% Author: R. Gaizauskas
%
% Features
%   number: sing | plural
%
% The basic analysis is
% BNP -> {PPS | DT} BNP_CORE | PRP
% where BNP_CORE -> {QUANT} {PREMODS} BNP_HEAD 
% and
% where BNP = basic noun phrase
%       BNP_CORE = part of BNP after PPS, DT or another possessive BNP
%       PRP = pronoun
%       EX = There
%       PPS = possessive pronoun
%       DT  = determiner
%       QUANT = numeric quantifier (CD_NP from NE grammar)
%       PREMODS = premodifiers
%       BNP_HEAD = head of basic noun phrase
% and
% PREMODS -> PREMOD | PREMOD PREMOD
% where
% PREMOD -> JJ | RB JJ | VBN | VBG | N
%       JJ = adjective
%       RB = adverb
%       VBN = past participle
%       VBG = gerundive
%       N = noun
%       NE_NP = proper names, etc. from NE grammar (may be unclassified)
%
% Known problems:
% - relations between premodifiers are not distinguished -- e.g. 
%   'the stained glass factory' and 'the disused glass factory' get
%   the same analysis
% - possessive NPs are premodifiers are not handled -- e.g. 
%   'the six boys' school' will not be analysed, though 'the six boys
%    schools' will be
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


best_parse_cats([bnp]).


% BNP -> BNP_CORE
% sugar
rule(bnp(edge:Edge,sem:E^[H,[realisation,E,Edge]],number:N), [
    bnp_core(sem:E^H,number:N)
]).

% BNP -> DT BNP_CORE
% the boss
rule(bnp(edge:Edge,sem:E^[H,[det,E,D],[realisation,E,Edge]],number:N), [
    dt(m_root:D),
    bnp_core(sem:E^H,number:N)
]).


% BNP -> POSS BNP_CORE
% his dog
rule(bnp(edge:Edge,sem:E1^E2^[PPS,H,[realisation,E1,Edge]],number:N), [
    poss(sem:E1^E2^PPS),
    bnp_core(sem:E1^H,number:N)
]).


% BNP_CORE -> BNP_HEAD
% sugar | Ford | Barry Diller
rule(bnp_core(sem:H,number:N), [
    bnp_head(sem:H,number:N)
]).

% BNP_HEAD -> N
% sugar
rule(bnp_head(sem:E^[[R,E],[number,E,N]],number:N), [
    n(m_root:R,number:N)
]).

% BNP_HEAD -> NE_NP
% Barry Diller -- from NE grammar
rule(bnp_head(sem:H,number:N), [
    ne_np(sem:H,number:N)
]).

% BNP_HEAD -> BNP_HEAD CC BNP_HEAD
rule(bnp_head(edge:Edge,sem:E3^E2^E1^[S1,S2,[coord,E2,E1],
                      [set,E3],[set_member,E3,E1],[set_member,E3,E2],[number,E3,plural],
                      [realisation,E3,Edge]],
              number:plural), [
    bnp_head(sem:E1^S1,number:N),
    cc(_),
    bnp_head(sem:E2^S2,number:N)
]).

% BNP_CORE -> PREMODS BNP_HEAD
% happy days
rule(bnp_core(sem:E^[H,P],number:N), [
    premods(sem:E^P),
    bnp_head(sem:E^H,number:N)
]).

% BNP_CORE -> QUANT BNP_HEAD
% three soldiers
rule(bnp_core(sem:E^[H,Q],number:N), [
    quant(sem:E^Q,number:N),
    bnp_head(sem:E^H,number:N)
]).

% BNP_CORE -> QUANT PREMODS BNP_HEAD
% three frozen soldiers
rule(bnp_core(sem:E^[H,P,Q],number:N), [
    quant(sem:E^Q,number:N), 
    premods(sem:E^P),
    bnp_head(sem:E^H,number:N)
]).

% QUANT -> CD_NP
% 
rule(quant(sem:E^[count,E,Q],number:N), [
    cd_np(s_form:Q,number:N)
]).

% QUANT -> JJ(degree:comp) IN(mroot:than) CD_NP
%
% more than 16 hounds -- semantics is naff
rule(quant(sem:E1^E2^[[count,E1,Q],[JJ,E2,Q]],number:N), [
    jj(m_root:JJ,degree:comp),
    in(m_root:than),
    cd_np(s_form:Q,number:N)					 
]).


% PREMODS -> PREMOD
rule(premods(sem:P), [
    premod(sem:P)
]).

% PREMODS -> PREMODS PREMOD
rule(premods(sem:E^[P1,P2]), [
    premods(sem:E^P1),
    premod(sem:E^P2)
]).

% PREMOD -> JJ
% brown
rule(premod(sem:E^[adj,E,JJ]), [
    jj(m_root:JJ)
]).

% PREMOD -> JJ CC JJ
% vital and complex
%rule(premod(sem:E^[[adj,E,JJ1],[adj,E,JJ2]]), [
%    jj(m_root:JJ1),
%    cc(_),
%    jj(m_root:JJ2)
%]).

% PREMOD -> PREMOD CC PREMOD
% vital and complex | singing and dancing
rule(premod(sem:E^[P1,P2]), [
    premod(sem:E^P1),
    cc(_),
    premod(sem:E^P2)
]).


% PREMOD -> RB JJ
% newly created
% NOTE: this could be ambiguous -- e.g. 'Recently painted walls have
% been causing problems'
rule(premod(sem:E^[[adj,E,RB],[adj,E,JJ]]), [
    rb(m_root:RB),
    jj(m_root:JJ)
]).

% PREMOD -> RB V
% newly registered
% NOTE: this could be ambiguous -- e.g. 'Recently painted walls have
% been causing problems'
rule(premod(sem:E^[[adj,E,RB],[adj,E,JJ]]), [
    rb(m_root:RB),
    v(s_form:JJ,vform:nform)
]).

% PREMOD -> N
rule(premod(edge:Edge,
    sem:E1^E2^[[R,E2],[qual,E1,E2],[number,E2,N],[realisation,E2,Edge]]), [
    n(m_root:R,number:N)
]).

% PREMOD -> NE_NP
% Washington
rule(premod(edge:Edge,sem:E1^E2^[N,[realisation,E2,Edge],[qual,E1,E2]]), [
    ne_np(sem:E2^N,number:_)
]).

% PREMOD -> N SYM(-) IN(like)
% cat-like
rule(premod(edge:Edge,
    sem:E1^E2^[[R,E2],[qual,E1,E2],[number,E2,N],[realisation,E2,Edge]]), [
    n(m_root:R,number:N),
    sym(s_form:'-'),
    in(m_root:'like')									   
]).

% PREMOD -> NE_NP SYM(-) IN(like)
% Scud-like
rule(premod(edge:Edge,sem:E1^E2^[N,[realisation,E2,Edge],[qual,E1,E2]]), [
    ne_np(sem:E2^N,number:_),
    sym(s_form:'-'),
    in(m_root:'like')		
]).


% PREMOD -> NE_NP V
% Washington registered
rule(premod(edge:Edge,sem:E1^E2^[N,[realisation,E2,Edge],[qual,E1,E2],[adj,E1,JJ]]), [
        ne_np(sem:E2^N),
	v(s_form:JJ,vform:nform)
]).


% PREMOD -> NE_NP SYM(-) V
% Washington-based
rule(premod(sem:E1^E2^[[location,E2],N,[realisation,E2,Edge],[qual,E1,E2],[adj,E1,JJ]]), [
        ne_np(edge:Edge,sem:E2^[[location, E2]|N]),
	sym(s_form:'-'),
	v(s_form:JJ,vform:nform)
]).


% PREMOD -> CD_NP SYM(-) N
% 14-day
rule(premod(edge:Edge,sem:E1^E2^[[R,E2],[count,E2,Q],[realisation,E2,Edge],[qual,E1,E2]]), [
        cd_np(s_form:Q),
	sym(s_form:'-'),
	n(s_form:R)
]).


% PREMOD -> VBN
% frozen
% Note: Brill usually tags these JJ when they are playing an
% adjectival role
% oh all right -- let's try it again for a while (cf 'a friend and retired military
% specialist') -- retired tagged as VBN
rule(premod(sem:E^[adj,E,VBN]), [
    v(s_form:VBN,vform:nform)
]).

% PREMOD -> VBN
% the launching site
rule(premod(sem:E^[adj,E,VBG]), [
    v(s_form:VBG,vform:gform)
]).

% POSS -> PPS
rule(poss(edge:Edge,
    sem:E1^E2^[[pronoun,E2,R],[of,E1,E2],[realisation,E2,Edge]]), [
    pps(m_root:R)
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% unused rules:

% PREMOD -> N POS
%rule(premod(sem:[N,POS]), [
%    n(m_root:N,number:_),
%    pos(m_root:POS)
%]).

% PREMOD -> VBN
% frozen
% Note: Brill usually tags these JJ when they are playing an
% adjectival role
%rule(premod(sem:E^[adj,E,VBN]), [
%    v(s_form:VBN,vform:nform)
%]).

% PREMOD -> VBG 
% laughing
% Note: Brill usually tags these JJ when they are playing an
% adjectival role
%rule(premod(sem:E^[adj,E,VBG]), [
%    v(s_form:VBG,vform:gform)
%]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% additional rules:

% BNP_CORE -> CD_NP
% three
rule(bnp_core(sem:E^[[count,E,Q]],number:N), [
    cd_np(s_form:Q,number:N)
]).

% BNP -> PRP
% she
rule(bnp(edge:Edge,sem:E^[[pronoun,E,R],[realisation,E,Edge]]),
    [prp(m_root:R)]).

% BNP -> EX
% there
rule(bnp(edge:Edge,sem:E^[[realisation,E,Edge]]),
    [ex(m_root:R)]).

% BNP -> PPS
% it
rule(bnp(edge:Edge,sem:E^[[pronoun,E,R],[realisation,E,Edge]]),
    [pps(m_root:R)]).

% POSS -> BNP POS
% EMI's
rule(poss(sem:E1^E2^[N,[of,E1,E2],[realisation,E2,Edge]]), [
    bnp(edge:Edge,sem:E2^N),
    pos(_)
]).

% POSS -> BNP VBZ('s)
% allow for bad tagging of pos
rule(poss(sem:E1^E2^[N,[of,E1,E2],[realisation,E2,Edge]]), [
    bnp(edge:Edge,sem:E2^N),
    v(s_form:'''s')
]).


%%%%%%%%%% THIS IS SO UGLY YOU DON'T WANT TO LOOK %%%%%%%%%%%

% Retag Brill's jj tagging of lift-off
rule(n(m_root:lift-off), [
    jj(m_root:lift-off)
]).	   


