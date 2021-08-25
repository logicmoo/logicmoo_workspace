/*  ASN.PL  */

/*  Shelved on the 14th of November 1989.  */


% PURPOSE:  Translators between EASN and EBNF, and parser generator
%       for ASN.
%
% AUTHORS:  C.D. Farris and P. Singleton.
%           Made portable by Jocelyn Paine
%
% INSTITUTION:  Dept. of Computer Science, University of Keele.
%
% DATE:     17/7/1989
%
% MADE PORTABLE by Jocelyn Paine, Oxford.             
%
% COMMENTS:
%
%   ASN is described in "The compilation of metalanguages
%   into Prolog", by C.D. Farris and P. Singleton, University
%   Computing, vol 11, no 2, pp 62-75.
%
%   'type', 'mode' and 'sysdep' comments constitute information which we
%   exploit in our own scheme and may safely be ignored.
%
% DEMONSTRATIONS:
%
%   1) Translate the EBNF for (a fragment of) Pam into EASN and
%      display it.

demo1 :-
    demo_rule( pam, ebnf, C),
    ebnfchars_to_ebnftokens( C, T),
    ebnftokens_to_ebnftree( T, P),
    ebnftree_to_easnrules( P, R),
    checklist( portray_clause, R).

%   2) Translate the EBNF for EBNF into EASN and display it.

demo2 :-
    demo_rule( ebnf, ebnf, C),
    ebnfchars_to_ebnftokens( C, T),
    ebnftokens_to_ebnftree( T, P),
    ebnftree_to_easnrules( P, R),
    checklist( portray_clause, R).

%   3) Compile the EASN rules for (a fragment of) Pam into Prolog
%      clauses with principal functor 'demo_parse' and portray them.

demo3 :-
    setof( R, demo_rule(pam,easn,R), Rs),
    easnrules_to_clauses( demo_parse, Rs, Cs),
    checklist( portray_clause, Cs).

%   4) Compile the EASN rules for EBNF into Prolog clauses with
%      principal functor 'demo_parse' and portray them.

demo4 :-
    setof( R, demo_rule(ebnf,easn,R), Rs),
    easnrules_to_clauses( demo_parse, Rs, Cs),
    checklist( portray_clause, Cs).
    
%   5) Translate the EASN rules for (a fragment of) Pam into EBNF
%      and display it.

demo5 :-
    setof( R, demo_rule(pam,easn,R), Rs),
    easnrules_to_ebnfrules( Rs, Es),
    checklist( ebnfrule_to_current_output, Es).

%   6) Translate the EASN rules for EBNF into EBNF and display it.

demo6 :-
    setof( R, demo_rule(ebnf,easn,R), Rs),
    easnrules_to_ebnfrules( Rs, Es),
    checklist( ebnfrule_to_current_output, Es).

% DEMONSTRATION RULES:

demo_rule( pam, easn, asn(program,is,[series])).
demo_rule( pam, easn, asn(series,all,[statement,optsome(all([token(;),statement]))])).
demo_rule( pam, easn, asn(statement,one,['input statement','output statement','assignment statement','conditional statement','definite loop','indefinite loop'])).
demo_rule( pam, easn, asn('input statement',all,[token(read),variable,optsome(all([token(','),variable]))])).
demo_rule( pam, easn, asn('output statement',all,[token(write),variable,optsome(all([token(','),variable]))])).
demo_rule( pam, easn, asn('assignment statement',all,[variable,token(:=),expression])).
demo_rule( pam, easn, asn('conditional statement',all,[token(if),comparison,token(then),series,opt(all([token(else),series])),token(fi)])).

demo_rule( ebnf, easn, asn(grammar,some,all([nonterminal,token('::='),chain([alternative,token('|')]),token(end)]))).
demo_rule( ebnf, easn, asn(alternative,some,element)).
demo_rule( ebnf, easn, asn(element,one,[poss_expr,repeat_expr,nonterminal,terminal])).
demo_rule( ebnf, easn, asn(poss_expr, all,[token('['),chain([alternative,token('|')]),token(']')])).
demo_rule( ebnf, easn, asn( repeat_expr,all,[token('{'),chain([alternative,token('|')]),token('}')])).

demo_rule( ebnf, ebnf,
"<grammar> ::= <nonterminal> '::=' <alternative> { '|' <alternative> } 'end' { <nonterminal> '::=' <alternative> { '|' <alternative> } 'end' }\
<alternative> ::= <element> { <element> }\
<element> ::= <poss_expr> | <repeat_expr> | <nonterminal> | <terminal>\
<poss_expr> ::= '[' <alternative> { '|' <alternative> } ']'\
<repeat_expr> ::= '{' <alternative> { '|' <alternative> } '}'\
").

demo_rule( pam, ebnf,
"<program> ::= <series>\
<series> ::= <statement> { ';' <statement> }\
<statement> ::= <input statement> | <output statement> | <assignment statement> | <conditional statement> | <definite loop> | <indefinite loop>\
<input statement> ::= 'read' <variable> { ',' <variable> }\
<output statement> ::= 'write' <variable> { ',' <variable> }\
<assignment statement> ::= <variable> ':=' <expression>\
<conditional statement> ::= 'if' <comparison> 'then' <series> [ 'else' <series> ] 'fi'\
").

% END OF DEMONSTRATION CODE.
%
% PORTABILITY:
%
%   I have replaced Quintus-specific calls by calls to Edinburgh standard
%   predicates, where necessary via the files IO.PL and USEFUL.PL consulted
%   below. The main remaining portability problems are
%   1)  These % comments.
%   2)  The long "..." strings above and the way they're continued over
%       lines.
:- reconsult( 'useful.pl' ).
:- reconsult( 'io.pl' ).


%type   easnfile_to_ebnffile( filename, filename)
%mode   easnfile_to_ebnffile( +, +)
%
% easnfile_to_ebnffile( EASNFile, EBNFFile) :-
%   Read EASN rules from the file EASNFile, translate them to
%   EBNF rules, and write them to the file EBNFFile.


easnfile_to_ebnffile( EASNFile, EBNFFile) :-
    easnfile_to_easnrules( EASNFile, EASNs),
    easnrules_to_ebnfrules( EASNs, EBNFs),
    ebnfrules_to_ebnffile( EBNFs, EBNFFile).




%type   easnfile_to_plfile( filename, atom, filename)
%mode   easnfile_to_plfile( +, +, +)
%
% easnfile_to_plfile( EASNFile, Functor, PlFile) :-
%   Read ASN rules from the file ASNFile, translate them
%   into Prolog clauses with principal functor name Functor
%   and write them to file PlFile.


easnfile_to_plfile( EASNFile, F, PlFile) :-
    easnfile_to_easnrules( EASNFile, EASNs),
    easnrules_to_clauses( F, EASNs, Cs),
    clauses_to_plfile( Cs, PlFile).




%type   ebnffile_to_easnfile( filename, filename)
%mode   ebnffile_to_easnfile( +, +)
%
% ebnffile_to_easnfile( EBNFFile, EASNFile) :-
%   Read EBNF grammar from the file EBNFFile, translate it to
%   an EASN grammar, and write it to the file EASNFile.


ebnffile_to_easnfile( EBNFFile, EASNFile) :-
    read_file_to_chars( EBNFFile, EBNFChars),
    ebnfchars_to_ebnftokens( EBNFChars, EBNFTokens),
    ebnftokens_to_ebnftree( EBNFTokens, EBNFTree),
    ebnftree_to_easnrules( EBNFTree, EASNs),
    easnrules_to_easnfile( EASNs, EASNFile).




%type   ebnfrule_to_ebnffile( list(atom), filename)
%mode   ebnfrule_to_ebnffile( +, +)
%
% ebnfrule_to_ebnffile( EBNFFile, EBNFRule) :-
%   Write an EBNF grammar rule, represented by a list EBNFRule
%   of rule components, to the file EBNFFile.


ebnfrule_to_ebnffile( EBNFRule, EBNFFile) :-
    current_output( C),
    open( EBNFFile, write, S),
    set_output( S),
    (   ebnfrule_to_current_output( EBNFRule)
    ->  set_output( C),
        close( S)
    ;   set_output( C),
        close( S),
        fail
    ).




%type   ptree_to_flattened_ptree( list(term), list(term))
%mode   ptree_to_flattened_ptree( +, -)
%
% ptree_to_flattened_ptree( PTree, -FlattenedPTree) :-
%   Delete generated nodes in PTree to give FlattenedPTree.


ptree_to_flattened_ptree( [], []).

ptree_to_flattened_ptree( [N1|PT1], [N2|PT2]) :-
    is_list( N1), !,
    ptree_to_flattened_ptree( N1, N2),
    ptree_to_flattened_ptree( PT1, PT2).

ptree_to_flattened_ptree( [N,PT1|PT2], PT) :-
    is_generated_mnemonic( N), !,
    append( PT1, PT2, PT3),
    ptree_to_flattened_ptree( PT3, PT).

ptree_to_flattened_ptree( [N|PT1], [N|PT2]) :-
    ptree_to_flattened_ptree( PT1, PT2).




%type   easnfile_to_easnrules( filename, list(easnrule))
%mode   easnfile_to_easnrules( +, -)
%
% easnfile_to_easnrules( EASNFile, EASNRules) :-
%   Read EASN rules from the file with absolute path name
%   EASNFile as a list EASNRules.


easnfile_to_easnrules( EASNFile, EASNs) :-
    read_file_to_terms( EASNFile, EASNs).




%type   easnrules_to_ebnfrules( list(easnrule), list(list(atom)))
%mode   easnrules_to_ebnfrules( +, -)
%
% easnrules_to_ebnfrules( EASNRules, EBNFRules) :-
%   Translate EASNRules to EBNFRules (see 'easnrule_to_ebnfrule'/2).

easnrules_to_ebnfrules( [], []).
easnrules_to_ebnfrules( [EASN|EASNs], [EBNF|EBNFs]) :-
    easnrule_to_ebnfrule( EASN, EBNF),
    EASN = asn(LHS,_,_),                % feedback -
    write( LHS), write( ' translated...'), nl,  % can be deleted
    easnrules_to_ebnfrules( EASNs, EBNFs).




%type   ebnfrules_to_ebnffile( list(list(atom)), filename)
%mode   ebnfrules_to_ebnffile( +, +)
%
% ebnfrules_to_ebnffile( EBNFRules, EBNFFile) :-
%   Write a list EBNFRules of EBNF grammar rules, each represented
%   by a list of rule components, to the file EBNFFile.


ebnfrules_to_ebnffile( EBNFRules, EBNFFile) :-
    current_output( C),
    open( EBNFFile, write, S),
    set_output( S),
    (   checklist( ebnfrule_to_current_output, EBNFRules)
    ->  set_output( C),
        close( S)
    ;   set_output( C),
        close( S),
        fail
    ).




%type   clauses_to_plfile( list(clause), filename)
%mode   clauses_to_plfile( +, +)
%
% clauses_to_plfile( Clauses, PlFile) :-
%   Portray Prolog Clauses to the file with absolute path
%   name PlFile.


clauses_to_plfile( Clauses, PlFile) :-
    current_output( C),
    open( PlFile, write, S),
    set_output( S),
    (   checklist( portray_clause, Clauses)
    ->  set_output( C),
        close( S)
    ;   set_output( C),
        close( S),
        fail
    ).




%type   easnrules_to_clauses( atom, list(easnrule), list(clause))
%mode   easnrules_to_clauses( +, +, -)
%
% easnrules_to_clauses( Functor, EASNRules, Clauses) :-
%   Translate EASNRules to Prolog Clauses with principal
%   functor name Functor.


easnrules_to_clauses( _, [], []).

easnrules_to_clauses( F, [EASN1|EASNs1], Cs) :-
    easnrule_to_normal_easnrule( EASN1, EASN2),
    easnrule_to_asnrule_and_easnrules( EASN2, ASN, EASNs2),
    append( EASNs2, EASNs1, EASNs),
    asnrule_to_clauses( F, ASN, Cs1),
    ASN = asn(LHS,_,_),             % feedback -
    write( LHS), write( ' translated...'), nl,  % can be deleted
    easnrules_to_clauses( F, EASNs, Cs2),
    append( Cs1, Cs2, Cs).




%type   easnrules_to_easnfile( list(easnrule), filename)
%mode   easnrules_to_easnfile( +, +)
%
% easnrules_to_easnfile( EASNRules, EASNFile) :-
%   Write a list of EASNRules to the file EASNFile.


easnrules_to_easnfile( EASNs, EASNFile) :-
    current_output( C),
    open( EASNFile, write, S),
    set_output( S),
    (   checklist( portray_clause, EASNs)
    ->  set_output( C),
        close( S)
    ;   set_output( C),
        close( S),
        fail
    ).




%type   ebnfchars_to_ebnftokens( list(char), list(term))
%mode   ebnfchars_to_ebnftokens( +, -)
%
% ebnfchars_to_ebnftokens( Chars, Tokens) :-
%   Tokenise the EBNF grammar ASCII list Chars to give Tokens.


ebnfchars_to_ebnftokens( [], []) :-
    !.

ebnfchars_to_ebnftokens( [C1,C2|Cs], Ts) :-
    is_char( nl, C1),
    (   is_char( tab, C2)
    ;   is_char( space, C2)
    ),
    !,
    ebnfchars_to_ebnftokens( Cs, Ts).

ebnfchars_to_ebnftokens( [C|Cs], Ts1) :-
    is_char( nl, C),
    !,
    ebnfchars_to_ebnftokens( Cs, Ts2),
    (   Ts2 = [end|_]   % to ensure we don't get an 'end' token
    ->  Ts1 = Ts2   % for each nl.   
    ;   Ts1 = [end|Ts2]
    ).

ebnfchars_to_ebnftokens( [C|Cs], Ts) :-
    is_char( white_space, C),
    !,
    ebnfchars_to_ebnftokens( Cs, Ts).

ebnfchars_to_ebnftokens( Cs1, [T|Ts]) :-
    ebnfchars_to_ebnftoken( T, Cs1, Cs2),
    ebnfchars_to_ebnftokens( Cs2, Ts).




%type   ebnftokens_to_ebnftree( list(term), list(term))
%mode   ebnftokens_to_ebnftree( +, -)
%
% ebnftokens_to_ebnftree( EBNFTokens, EBNFTree) :-
%   Link procedure to 'ebnf_parse'/4 (q.v.).


ebnftokens_to_ebnftree( EBNFTokens, EBNFTree) :-
    ebnf_parse( grammar, EBNFTree, EBNFTokens, []).




%type   ebnftree_to_easnrules( list(term), list(easnrule))
%mode   ebnftree_to_easnrules( +, -)
%
% ebnftree_to_easnrules( EBNFTree, EASNRules) :-
%   Link procedure to 'ebnf_to_easn'/3 (q.v.).


ebnftree_to_easnrules( EBNFTree, EASNs) :-
    ebnf_to_easn( grammar, EBNFTree, EASNs).




%type   ebnfrule_to_current_output( list(atom))
%mode   ebnfrule_to_current_output( +)
%
% ebnfrule_to_current_output( EBNFRule) :-
%   Write an EBNF grammar rule, represented by a list EBNFRule
%   of rule components, to current output.


ebnfrule_to_current_output( []) :-
    nl.

ebnfrule_to_current_output( [E|Es]) :-
    write( E),
    write( ' '),
    ebnfrule_to_current_output( Es).
    




%type   is_generated_mnemonic( atom)
%mode   is_generated_mnemonic( +)
%
% is_generated_mnemonic( Mnemonic) :-
%   Mnemonic is a generated mnemonic.


is_generated_mnemonic( Mnemonic) :-
    nonvar( Mnemonic),
    name( Mnemonic, [36,109|C]),
    is_digits( C ).




%type   easnrule_to_ebnfrule( easnrule, list(atom))
%mode   easnrule_to_ebnfrule( +, -)
%
% easnrule_to_ebnfrule( EASNRule, EBNFRule) :-
%   Translate EASNRule to EBNFRule (see 'easn_to_ebnf'/3).


easnrule_to_ebnfrule( EASN, EBNF) :-
    easnrule_to_normal_easnrule( EASN, NormalEASN),
    easn_to_ebnf( rule, NormalEASN, EBNF).




%type   asnrule_to_clauses( atom, asnrule, list(clause))
%mode   asnrule_to_clauses( +, +, -)

% asnrule_to_clauses( Functor, ASNRule, Clauses) :-
%   Compile ASNRule into Prolog DCG Clauses
%   with principal functor name Functor.

asnrule_to_clauses( F, asn(L,all,R), [(H:-B)]) :-
    H =.. [F,L,PT,S0,S],
    subgoals_to_body( F, R, PT, S0, S, B).

asnrule_to_clauses( _, asn(_,one,[]), []).

asnrule_to_clauses( F, asn(L,one,[SG|R]), [C|Cs]) :-
    asnrule_to_clauses( F, asn(L,all,[SG]), [C]),
    asnrule_to_clauses( F, asn(L,one,R), Cs).

asnrule_to_clauses( F, asn(L,is,[SG]), Cs) :-
    asnrule_to_clauses( F, asn(L,all,[SG]), Cs).

asnrule_to_clauses( F, asn(L,token,[Tn]), [C]) :-
    C =.. [F,L,[Tn],[Tn|S],S].

asnrule_to_clauses( F, asn(L,non,Tms), [(H:- \+(member(C,Tms)))]) :-
    H =.. [F,L,[C],[C|S],S].

asnrule_to_clauses( F, asn(L,opt,[SG]), [C1,C2]) :-
    asnrule_to_clauses( F, asn(L,all,[SG]), [C1]),
    C2 =.. [F,L,[],S,S].

asnrule_to_clauses( F, asn(L,some,[SG]), [(H1:-B1a,B1b),(H2:-B2),C3]) :-
    H1  =.. [F,L,[SG-PT1|PT2],S0,S],
    B1a =.. [F,SG,PT1,S0,S1],
    B1b =.. [F,more(L),PT2,S1,S],
    H2  =.. [F,more(L),PT,S0,S],
    B2  =.. [F,L,PT,S0,S],
    C3  =.. [F,more(L),[],S,S].

asnrule_to_clauses( F, asn(L,optsome,[SG]), [(H1:-B1a,B1b),C2]) :-
    H1  =.. [F,L,[SG-PT1|PT2],S0,S],
    B1a =.. [F,SG,PT1,S0,S1],
    B1b =.. [F,L,PT2,S1,S],
    C2  =.. [F,L,[],S,S].
    
asnrule_to_clauses( F, asn(L,fromto,[Fr,To]), [(H1:-B1a,B1b),(H2:-B2,!),(H3:-B3)]) :-
    H1  =.. [F,L,PT,S0,S],
    B1a =.. [F,Fr,_,S0,S1],
    B1b =.. [F,more(L),PT,S1,S],
    H2  =.. [F,more(L),[],S0,S],
    B2  =.. [F,To,_,S0,S],
    H3  =.. [F,more(L),[Tm|PT],[Tm|S0],S],
    B3  =.. [F,more(L),PT,S0,S].

asnrule_to_clauses( F, asn(L,chain,[SG,Lk]), [(H1:-B1a,B1b),(H2:-B2a,B2b),C3]) :-
    H1  =.. [F,L,[SG-PT1|PT2],S0,S],
    B1a =.. [F,SG,PT1,S0,S1],
    B1b =.. [F,more(L),PT2,S1,S],
    H2  =.. [F,more(L),PT,S0,S],
    B2a =.. [F,Lk,_,S0,S1],
    B2b =.. [F,L,PT,S1,S],
    C3  =.. [F,more(L),[],S,S].

asnrule_to_clauses( _, asn(_,term,[]), []).
asnrule_to_clauses( F, asn(L,term,[Tm|Tms]), [C|Cs]) :-
    C =.. [F,L,[Tm],[Tm|S],S],
    asnrule_to_clauses( F, asn(L,term,Tms), Cs).

asnrule_to_clauses( F, asn(L,allterm,Tms), [C]) :-
    append( Tms, S, S0),
    C =.. [F,L,Tms,S0,S].

asnrule_to_clauses( F, asn(L,max,[SG]), [(H1:-B1a,!,B1b),(H2:-B2),C3]) :-
    H1  =.. [F,L,[PT1|PT2],S0,S],
    B1a =.. [F,SG,PT1,S0,S1],
    B1b =.. [F,more(L),PT2,S1,S],
    H2  =.. [F,more(L),PT1,S0,S],
    B2  =.. [F,L,PT1,S0,S],
    C3  =.. [F,more(L),[],S,S].

asnrule_to_clauses( F, asn(L,butnot,[SG|Nots]), [(H:-Ba,Bb)]) :-
    H  =.. [F,L,PT,S0,S],
    Ba =.. [F,SG,PT,S0,S],
    notgoals_to_body( F, Nots, S0, S, Bb).




%type   easnrule_to_asnrule_and_easnrules( easnrule, asnrule, list(easnrule))
%mode   easnrule_to_asnrule_and_easnrules( +, -, -)
%
% easnrule_to_asnrule_and_easnrules( EASNRule, ASNRule, EASNRules) :-
%   Expand out EASNRule to give ASNRule, leaving EASNRules yet to
%   be expanded.


easnrule_to_asnrule_and_easnrules( asn(L,T,[]), asn(L,T,[]), []).

easnrule_to_asnrule_and_easnrules( asn(L,T,[S1|Ss1]), asn(L,T,[S2|Ss2]), EASNs1) :-
    easnrule_to_asnrule_and_easnrules( asn(L,T,Ss1), asn(L,T,Ss2), EASNs2),
    (   T \== token,
        S1 =.. [F,SSs],
        is_st( F)
    ->  generate_mnemonic( S2),
        EASNs1 = [asn(S2,F,SSs)|EASNs2]
    ;   S2 = S1,
        EASNs1 = EASNs2
    ).




%type   easnrule_to_normal_easnrule( easnrule, easnrule)
%mode   easnrule_to_normal_easnrule( +, -)
%
% easnrule_to_normal_easnrule( EASNRule, NormalEASNRule) :-
%   Translate EASNRule to its normal form NormalEASNRule.
%   At the moment this simply ensures that lists are
%   re-instated, but it should be used to re-instate
%   any convenient deviation, and to type-validate.


easnrule_to_normal_easnrule( asn(LHS,Type,RHS), asn(LHS,Type,NewRHS)) :-
    (   is_list( RHS)
    ->  NewRHS = RHS
    ;   NewRHS = [RHS]
    ).




%type   ebnfchars_to_ebnftoken( atom, list(char), list(char))
%mode   ebnfchars_to_ebnftoken( -, +, -)
%
% ebnfchars_to_ebnftoken( Token, Chars, RestChars) :-
%   Token is stripped from the front of Chars to leave RestChars.


ebnfchars_to_ebnftoken( Tk, [C|Cs1], Cs2) :-
    C =< 32,
    !,
    ebnfchars_to_ebnftoken( Tk, Cs1, Cs2).

ebnfchars_to_ebnftoken( '::=', [C1,C2,C3|Cs], Cs) :-
    is_char( colon, C1),
    is_char( colon, C2),
    is_char( equals, C3),
    !.

ebnfchars_to_ebnftoken( n(Tk), [C|Cs1], Cs2) :-
    is_char( l_chevron, C),
    !,
    ebnfchars_to_nonterminal( NT, Cs1, Cs2),
    name( Tk, NT).      % altered from constant_chars to
                % improve portability

ebnfchars_to_ebnftoken( t(Tk), [C|Cs1], Cs2) :-
    is_char( single_quote, C),
    !,
    ebnfchars_to_terminal( Tm, Cs1, Cs2),
    name( Tk, Tm).      % ditto

ebnfchars_to_ebnftoken( Tk, [C|Cs], Cs) :-
    name( Tk, [C]).     % ditto




%type   is_char( atom, char)
%mode   is_char( ?, ?)
%
% is_char( Name, Value)
%   The ASCII character with decimal value Value has name Name.


is_char( white_space, C) :-
    member( Name, [space,tab,nl]),
    is_char( Name, C).

is_char( alpha, C) :-
    member( Name, [lower_case,upper_case]),
    is_char( Name, C).

is_char( lower_case, C) :-
    C >= 97,
    C =< 122.

is_char( upper_case, C) :-
    C >= 65,
    C =< 90.

is_char( digit, C) :-
    C >= 48,
    C =< 57.

is_char( colon,     58).
is_char( double_quote,  34).
is_char( equals,    61).
is_char( l_paren,   40).
is_char( l_chevron, 60).
is_char( minus,     45).
is_char( nl,        10).
is_char( point,     46).
is_char( plus,      43).
is_char( r_paren,   41).
is_char( r_chevron, 62).
is_char( div,       47).
is_char( single_quote,  39).
is_char( space,     32).
is_char( sqrt,      94).
is_char( mult,      42).
is_char( tab,       9).
is_char( zero,      48).




%type   ebnf_parse( gtype, list(constant), list(term), list(term))
%mode   ebnf_parse( +, -, +, -)
%
% ebnf_parse( GType, ParseTree, S0, S) :-
%   The difference list S0-S of language tokens parse as
%   as an EBNF grammatical unit of type GType yielding ParseTree.
%
% This parser was generated from the following ASN grammar:
%
%  asn( grammar,    some,   phrase).
%  asn( phrase,     all,    [nonterminal,consists_of,expression,end]).
%  asn( expression, chain,  [alternative,or]).
%  asn( alternative,    some,   element).
%  asn( element,    one,    [poss_expr,repeat_expr,nonterminal,terminal]).
%  asn( poss_expr,  all,    [left_square,expression,right_square]).
%  asn( repeat_expr,    all,    [left_curly,expression,right_curly]).
%  
%  asn( end,        token,  end).
%  asn( consists_of,    token,  '::=').
%  asn( or,     token,  '|').
%  asn( left_square,    token,  '[').
%  asn( right_square,   token,  ']').
%  asn( left_curly, token,  '{').
%  asn( right_curly,    token,  '}').
%  
%  asn( nonterminal,    token,  n(_)).
%  asn( terminal,   token,  t(_)).


ebnf_parse(grammar,[phrase,A|B],C,D) :-
        ebnf_parse(phrase,A,C,E),
        ebnf_parse(more(grammar),B,E,D).
ebnf_parse(more(grammar),A,B,C) :-
        ebnf_parse(grammar,A,B,C).
ebnf_parse(more(grammar),[],A,A).
ebnf_parse(phrase,[nonterminal,A,consists_of,B,expression,C,end,D],E,F) :-
        ebnf_parse(nonterminal,A,E,G),
        ebnf_parse(consists_of,B,G,H),
        ebnf_parse(expression,C,H,I),
        ebnf_parse(end,D,I,F).
ebnf_parse(expression,[alternative,A|B],C,D) :-
        ebnf_parse(alternative,A,C,E),
        ebnf_parse(more(expression),B,E,D).
ebnf_parse(more(expression),A,B,C) :-
        ebnf_parse(or,D,B,E),
        ebnf_parse(expression,A,E,C).
ebnf_parse(more(expression),[],A,A).
ebnf_parse(alternative,[element,A|B],C,D) :-
        ebnf_parse(element,A,C,E),
        ebnf_parse(more(alternative),B,E,D).
ebnf_parse(more(alternative),A,B,C) :-
        ebnf_parse(alternative,A,B,C).
ebnf_parse(more(alternative),[],A,A).
ebnf_parse(element,[poss_expr,A],B,C) :-
        ebnf_parse(poss_expr,A,B,C).
ebnf_parse(element,[repeat_expr,A],B,C) :-
        ebnf_parse(repeat_expr,A,B,C).
ebnf_parse(element,[nonterminal,A],B,C) :-
        ebnf_parse(nonterminal,A,B,C).
ebnf_parse(element,[terminal,A],B,C) :-
        ebnf_parse(terminal,A,B,C).
ebnf_parse(poss_expr,[left_square,A,expression,B,right_square,C],D,E) :-
        ebnf_parse(left_square,A,D,F),
        ebnf_parse(expression,B,F,G),
        ebnf_parse(right_square,C,G,E).
ebnf_parse(repeat_expr,[left_curly,A,expression,B,right_curly,C],D,E) :-
        ebnf_parse(left_curly,A,D,F),
        ebnf_parse(expression,B,F,G),
        ebnf_parse(right_curly,C,G,E).
ebnf_parse(end,[end],[end|A],A).
ebnf_parse(consists_of,[::=],[::=|A],A).
ebnf_parse(or,['|'],['|'|A],A).
ebnf_parse(left_square,['['],['['|A],A).
ebnf_parse(right_square,[']'],[']'|A],A).
ebnf_parse(left_curly,['{'],['{'|A],A).
ebnf_parse(right_curly,['}'],['}'|A],A).
ebnf_parse(nonterminal,[n(A)],[n(A)|B],B).
ebnf_parse(terminal,[t(A)],[t(A)|B],B).




%type   ebnf_to_easn( gtype, list(term), list(asnrule))
%mode   ebnf_to_easn( +, +, -)
%
% ebnf_to_easn( GType, EBNFTree, ASNRules) :-
%   Translate the EBNF grammatical unit of type GType and with
%   parse tree EBNFTree - as yielded by 'ebnf_parse'/4 (q.v.) -
%   to an ASN grammar represented by a list ASNRules of ASN rules.


ebnf_to_easn( grammar, [], []).

ebnf_to_easn( grammar, [phrase,P|Ps], [ASN|ASNs]) :-
    ebnf_to_easn( phrase, P, ASN),
    ebnf_to_easn( grammar, Ps, ASNs).
    
ebnf_to_easn( phrase, [_,[n(NT)],_,_,_,Ex,_,_], asn(NT,ST,SG)) :-
    (   ebnf_to_easn( expression, Ex, asn(NT,ST,SG))
    ->  write( NT),         % THIS
        write( ' translated...'),   %
        nl              % FEEDBACK
    ;   write( NT),         %
        write( ' failed to translate'), % CAN BE
        nl,             %
        fail                % DELETED
    ).

ebnf_to_easn( expression, [alternative,Al], ASN) :-
    ebnf_to_easn( alternative, Al, ASN).

ebnf_to_easn( expression, [alternative,Al,alternative|Ex], asn(_,one,RHS)) :-
    ebnf_to_easn( alternative, Al, asn(_,AlST,AlRHS)),
    easn_rhs_to_normal_easn_rhs( AlST, AlRHS, [one,is], RHS1),
    ebnf_to_easn( expression, [alternative|Ex], asn(_,ExST,ExRHS)),
    easn_rhs_to_normal_easn_rhs( ExST, ExRHS, [one,is], RHS2),
    append( RHS1, RHS2, RHS).

ebnf_to_easn( alternative, [element,El], ASN) :-
    ebnf_to_easn( element, El, ASN).

ebnf_to_easn( alternative, [element,El,element|Al], asn(_,all,RHS)) :-
    ebnf_to_easn( element, El, asn(_,ElST,ElRHS)),
    easn_rhs_to_normal_easn_rhs( ElST, ElRHS, [all,is], RHS1),
    ebnf_to_easn( alternative, [element|Al], asn(_,AlST,AlRHS)),
    easn_rhs_to_normal_easn_rhs( AlST, AlRHS, [all,is], RHS2),
    append( RHS1, RHS2, RHS).

ebnf_to_easn( element, [poss_expr,Po], asn(_,opt,RHS)) :-
    ebnf_to_easn( poss_expr, Po, asn(_,PoST,PoRHS)),
    easn_rhs_to_normal_easn_rhs( PoST, PoRHS, [all], RHS).

ebnf_to_easn( element, [repeat_expr,Re], asn(_,optsome,RHS)) :-
    ebnf_to_easn( repeat_expr, Re, asn(_,ReST,ReRHS)),
    easn_rhs_to_normal_easn_rhs( ReST, ReRHS, [all], RHS).

ebnf_to_easn( element, [nonterminal,[n(SG)]], asn(_,is,[SG])).

ebnf_to_easn( element, [terminal,[t(SG)]], asn(_,token,SG)).

ebnf_to_easn( poss_expr, [_,_,_,Ex,_,_], ASN) :-
    ebnf_to_easn( expression, Ex, ASN).

ebnf_to_easn( repeat_expr, [_,_,_,Ex,_,_], ASN) :-
    ebnf_to_easn( expression, Ex, ASN).




%type   easn_to_ebnf( gtype, asn_fragment, ebnf_fragment)
%mode   easn_to_ebnf( +, +, -)
%
% easn_to_ebnf( GType, ASNFragment, EBNFFragment) :-
%   Translate the grammatical unit of type GType of
%   ASN grammar fragment ASNFragment to EBNF grammar
%   fragment EBNFFragment.


easn_to_ebnf( grammar, [], []).

easn_to_ebnf( grammar, [ASN|ASNs], [EBNF|EBNFs]) :-
    easn_to_ebnf( rule, ASN, EBNF),
    easn_to_ebnf( grammar, ASNs, EBNFs).

easn_to_ebnf( rule, asn(ANT,all,ANTs), [BNT,'::='|BNTs]) :-
    easn_to_ebnf( nonterminals, [ANT|ANTs], [BNT|BNTs]).

easn_to_ebnf( rule, asn(ANT,one,ANTs), [BNT,'::='|BRHS]) :-
    easn_to_ebnf( nonterminals, [ANT|ANTs], [BNT|BNTs]),
    list_to_chain( BNTs, '|', BRHS).

easn_to_ebnf( rule, asn(ANT1,is,[ANT2]), [BNT1,'::=',BNT2]) :-
    easn_to_ebnf( nonterminals, [ANT1,ANT2], [BNT1,BNT2]).

easn_to_ebnf( rule, asn(ANT,opt,ANTs), [BNT,'::='|BRHS]) :-
    easn_to_ebnf( nonterminals, [ANT|ANTs], [BNT|BNTs]),
    append( ['['|BNTs],[']'],BRHS).

easn_to_ebnf( rule, asn(ANT,some,ANTs), [BNT,'::='|BRHS]) :-
    easn_to_ebnf( nonterminals, [ANT|ANTs], [BNT|BNTs]),
    append( BNTs, ['{'|BNTs],BRHS1),
    append( BRHS1,['}'],BRHS).

easn_to_ebnf( rule, asn(ANT,optsome,ANTs), [BNT,'::='|BRHS]) :-
    easn_to_ebnf( nonterminals, [ANT|ANTs], [BNT|BNTs]),
    append( ['{'|BNTs],['}'],BRHS).

easn_to_ebnf( rule, asn(ANT,chain,ANTs), [BNT,'::=',BNT1,'{',BNT2,BNT1,'}']) :-
    easn_to_ebnf( nonterminals, [ANT|ANTs], [BNT,BNT1,BNT2]).

easn_to_ebnf( rule, asn(ANT,token,[AT]), [BNT,'::=',BT]) :-
    easn_to_ebnf( nonterminal, ANT, [BNT]),
    concat_atom( ['''',AT,''''], BT).

easn_to_ebnf( rule, asn(ANT,term,ATs), [BNT,'::='|BRHS]) :-
    easn_to_ebnf( nonterminal, ANT, [BNT]),
    easn_to_ebnf( terminals, ATs, BTs),
    list_to_chain( BTs, '|', BRHS).

easn_to_ebnf( rule, asn(ANT,allterm,ATs), [BNT,'::='|BTs]) :-
    easn_to_ebnf( nonterminal, ANT, [BNT]),
    easn_to_ebnf( terminals, ATs, BTs).
    
easn_to_ebnf( nonterminals, [], []).

easn_to_ebnf( nonterminals, [ANT|ANTs], BNTs) :-
    easn_to_ebnf( nonterminal, ANT, BNTs1),
    easn_to_ebnf( nonterminals, ANTs, BNTs2),
    append( BNTs1, BNTs2, BNTs).

easn_to_ebnf( nonterminal, ANT, BNTs) :-
    ANT =.. [F,SubNTs],
    is_st( F), !,
    generate_mnemonic( Mn),
    easnrule_to_ebnfrule( asn(Mn,F,SubNTs), [_,'::='|BNTs]).

easn_to_ebnf( nonterminal, ANT, [BNT]) :-
    concat_atom( ['<',ANT,'>'], BNT).
    
easn_to_ebnf( terminals, [], []).

easn_to_ebnf( terminals, [AT|ATs], [BT|BTs]) :-
    concat_atom( ['''',AT,''''], BT),
    easn_to_ebnf( terminals, ATs, BTs).




%type   notgoals_to_body( atom, asn_rhs, list(term), list(term), term)
%mode   notgoals_to_body( +, +, -, -, -)
%
% notgoals_to_body( Functor, Subgoals, StartDiff, EndDiff, Conjunction) :-
%   Similar to 'subgoals_to_body'/6 but with goals negated
%   (hence no parse tree).


notgoals_to_body( F, [NG], S0, S, \+(G)) :-
    G =.. [F,NG,_,S0,S].

notgoals_to_body( F, [NG1,NG2|NGs], S0, S, (\+(G),B)) :-
    G =.. [F,NG1,_,S0,S],
    notgoals_to_body( F, [NG2|NGs], S0, S, B).




%type   subgoals_to_body( atom, asn_rhs, asn_parsetree, list(term), list(term), term)
%mode   subgoals_to_body( +, +, -, -, -, -)
%
% subgoals_to_body( Functor, Subgoals, ParseTree, StartDiff, EndDiff, Conj) :-
%   Given Functor and list of Subgoals, generate a suitable conjunction 
%   Conj, complete with all the right parse trees and difference lists,
%   and return it, the parse tree ParseTree and difference lists StartDiff
%   and EndDiff for the head.


subgoals_to_body( F, [S], [S-SubPT], V1, V2, G) :-
    G =.. [F,S,SubPT,V1,V2].

subgoals_to_body( F, [S1,S2|Ss], [S1-SubPT1|PT], V1, V3, (G,Gs)) :-
    subgoals_to_body( F, [S2|Ss], PT, V2, V3, Gs),
    G =.. [F,S1,SubPT1,V1,V2].




%type   generate_mnemonic( atom)
%mode   generate_mnemonic( -)
%
% generate_mnemonic( Mnemonic) :-
%   Generate a distinct mnemonic Mnemonic of
%   the form '$mN' where N is the current count.


generate_mnemonic( Mnemonic) :-
    increment_count( N),
    aname( N, C),
    name( Mnemonic, [36,109|C]).  % [36,109] = "$m"




%type   is_st( st)
%mode   is_st( ?)
%
% is_st( ST) :-
%   ST is a recognised ASN structural type.


is_st( all).
is_st( one).
is_st( atom).
is_st( is).
is_st( token).
is_st( non).
is_st( opt).
is_st( some).
is_st( optsome).
is_st( fromto).
is_st( chain).
is_st( term).
is_st( allterm).
is_st( max).
is_st( butnot).




%type   ebnfchars_to_nonterminal( list(char), list(char), list(char))
%mode   ebnfchars_to_nonterminal( -, +, -)
%
% ebnfchars_to_nonterminal( NTChars, Chars, RestChars) :-
%   The difference lists Chars-RestChars yield chars NTChars
%   for a nonterminal symbol.


ebnfchars_to_nonterminal( [], [C|Cs], Cs) :-
    is_char( r_chevron, C),
    !.

ebnfchars_to_nonterminal( [C|NT], [C|Cs1], Cs2) :-
    ebnfchars_to_nonterminal( NT, Cs1, Cs2).




%type   ebnfchars_to_terminal( list(char), list(char), list(char))
%mode   ebnfchars_to_terminal( -, +, -)
%
% ebnfchars_to_terminal( TChars, Chars, RestChars) :-
%   The difference lists Chars-RestChars yield chars TChars
%   for a terminal symbol.


ebnfchars_to_terminal( [C|T], [C,C|Cs1], Cs2) :-
    is_char( single_quote, C),
    !,
    ebnfchars_to_terminal( T, Cs1, Cs2).

ebnfchars_to_terminal( [], [C|Cs], Cs) :-
    is_char( single_quote, C),
    !.

ebnfchars_to_terminal( [C|T], [C|Cs1], Cs2) :-
    ebnfchars_to_terminal( T, Cs1, Cs2).




%type   easn_rhs_to_normal_easn_rhs( st, list(term), list(st), list(term))
%mode   easn_rhs_to_normal_easn_rhs( +, +, +, -)
%
% easn_rhs_to_normal_easn_rhs( ST, RHS, Exceptions, NewRHS) :-
%   Construct NewRHS given the existing RHS by creating a function
%   structure, except where this would add no extra information, eg:
%
%   all([e1,all([e2,e3,e4]),e5,is([e6])]) == all([e1,e2,e3,e4,e5,e6])
%
%   This is determined by whether a structural type ST is a member
%   of a given set of Exceptions.


easn_rhs_to_normal_easn_rhs( ST, RHS, E, RHS) :-
    member( ST, E),
    !.

easn_rhs_to_normal_easn_rhs( ST, RHS, _, [F]) :-
    F =.. [ST,RHS].




%type   list_to_chain( list(atom), atom, list(atom))
%mode   list_to_chain( +, +, -)
%
% list_to_chain( List, Link, Chain) :-
%   Interpose the elements of List with Link to give Chain eg:
%
%   | ?- list_to_chain( [1,2,3], and, C).
%
%   C = [1,and,2,and,3]


list_to_chain( [E], _, [E]).

list_to_chain( [E1,E2|Es], L, [E1,L|ELs]) :-
    list_to_chain( [E2|Es], L, ELs).




%type   increment_count( natural)
%mode   increment_count( -)
%
% increment_count( Count):-
%
%   Return current Count and increment it.


increment_count( M) :-
    retract( current_count(M)),
    N is M+1,
    assert( current_count(N)).




%type   current_count( natural)
%mode   current_count( ?)
%
% current_count( Count) :-
%
%   Current count is Count - called by 'increment_count/1' (q.v.).


% :- dynamic current_count/1.

current_count( 1).




%type   read_file_to_chars( filename, list(char))
%mode   read_file_to_chars( +, -)
%sysdep QP

% read_file_to_chars( +FileName, -Chars) :-
%   the file called FileName (which must exist and be readable)
%   is found to contain exactly the sequence of characters denoted by Chars.

read_file_to_chars( FileName, Chars) :-
    open( FileName, read, Stream ),
    read_stream_to_chars( Stream, Chars),
    close( Stream).




%type   read_file_to_terms( filename, list(term))
%mode   read_file_to_terms( +, -)
%sysdep QP

% read_file_to_terms( +FileName, -Terms) :-
%   FileName must name a readable Prolog source file:
%   Terms is a list of the terms (sentences)
%   of which it contains representations.

read_file_to_terms( FileName, Terms) :-
    open( FileName, read, Stream),
    read_stream_to_terms( Stream, Terms),
    close( Stream).




%type   read_stream_to_chars( stream, list(char))
%mode   read_stream_to_chars( +, -)
%sysdep QP

%read_stream_to_chars( +Stream, -Chars) :-
%   from Stream, the sequence of character codes denoted by Chars
%   was (destructively) read.

read_stream_to_chars( S, Cs) :-
    get0_from( S, C),
    (   is_eof_char( C )
    ->  Cs = []
    ;   Cs = [C|Cs2],
        read_stream_to_chars( S, Cs2)
    ).




%type   read_stream_to_terms( stream, list(term))
%mode   read_stream_to_terms( +, -)
%sysdep QP

% read_stream_to_terms( +Stream, -Terms) :-
%   Stream (read to exhaustion)
%   contained representations of the successive members of Terms.
%   Since QP's 'read' doesn't return an error code, we hope for the best.

read_stream_to_terms( S, Ts) :-
    read_from( S, T),
    (   T == end_of_file
    ->  Ts = []
    ;   Ts = [T|Ts2],
        read_stream_to_terms( S, Ts2)
    ).
