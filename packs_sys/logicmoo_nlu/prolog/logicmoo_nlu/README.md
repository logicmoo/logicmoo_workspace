Initial starter Docs https://github.com/logicmoo/PrologMUD/wiki

Installation see.. https://docs.google.com/document/d/1fkOxnmI1LqxadvZuCRS-fGIEweIKyPn6AVGp5Yjse1I/edit




The MUD_PDDL repository is now both Planning and NLU/NLG code.. The NLTK of Prolog!



It is very bothersome that with all the cool NLU demos out there there is  that is like GATE or even a Blackboard system 


This NLU/NLG ToolKit uses the following projects into a usable pipeline

TALK UGOT WP3 task 3.1 implementation accompanying D3.1
Extended Information State Modelling  -- Stina Ericsson, 19th January, 2006

The Attempto Parsing Engine (APE) translates ACE texts unambiguously into discourse representation structures (DRS) that use a variant of the language of first-order logic.[2] A DRS can be further translated into other formal languages, for instance AceRules with various semantics,[3] OWL,[4] and SWRL. Translating an ACE text into (a fragment of) first-order logic allows users to reason about the text, for instance to verify, to validate, and to query it.

TALK

GULP 

CHAT80

````

% ============================================BEGIN=============================================================

%                       [what,rivers,are,there,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], river), []), verb(be, active, pres+fin, [], pos(B)), [void(there)], [])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], river), []),
          verb(be, active, pres+fin, [], pos(B)),
          [void(there)],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-river(B),B^true


Reply:(report4) 0.0030000000000143245 sec(s).
[[amazon,amu_darya,amur,brahmaputra,colorado,congo_river,cubango,danube,don,elbe,euphrates,ganges,hwang_ho,indus,irrawaddy,lena,limpopo,mackenzie,mekong,mississippi,murray,niger_river,nile,ob,oder,orange,orinoco,parana,rhine,rhone,rio_grande,salween,senegal_river,tagus,vistula,volga,volta,yangtze,yenisei,yukon,zambesi]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,rivers,are,there,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,countries,are,there,in,europe,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(be, active, pres+fin, [], pos(B)), [void(there)], [pp(prep(in), np(3+sg, nameOf(europe), []))])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(be, active, pres+fin, [], pos(B)),
          [void(there)],
          [pp(prep(in), np(3+sg, nameOf(europe), []))])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-country(B),in_ploc(B,europe)


Reply:(report4) 0.00899999999998613 sec(s).
[[albania,andorra,austria,belgium,bulgaria,cyprus,czechoslovakia,denmark,east_germany,eire,finland,france,greece,hungary,iceland,italy,liechtenstein,luxembourg,malta,monaco,netherlands,norway,poland,portugal,romania,san_marino,spain,sweden,switzerland,united_kingdom,west_germany,yugoslavia]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,countries,are,there,in,europe,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,country,'\'',s,capital,is,london,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, np_head(det(the(sg)), [], capital), [pp(poss, np(3+sg, np_head(int_det(A), [], country), []))]), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, nameOf(london), []))], [])) :-
    whq(A,
        s(np(3+sg,
             np_head(det(the(sg)), [], capital),
             [pp(poss, np(3+sg, np_head(int_det(A), [], country), []))]),
          verb(be, active, pres+fin, [], pos(B)),
          [varg(dir, np(3+sg, nameOf(london), []))],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-country(B),capital(B,london)


actPlanning:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-capital(B,london),{country(B)}


Reply:(report4) 0.0 sec(s).
[[united_kingdom]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,country,'\'',s,capital,is,london,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(D)), [varg(dir, np(3+sg, np_head(det(the(sg)), [adj(total)], area), [pp(prep(of), np(3+pl, np_head(generic, [], country), [conj(and, reduced_rel(B, s(np(3+pl, wh(B), []), verb(be, active, pres+fin, [], pos(E)), [varg(pred, pp(prep(southof), np(3+sg, nameOf(equator), [])))], [])), reduced_rel(C, s(np(3+pl, wh(C), []), verb(be, active, pres+fin, [], neg(not)), [varg(pred, pp(prep(in), np(3+sg, nameOf(australasia), [])))], [])))]))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(D)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [adj(total)], area),

                    [ pp(prep(of),
                         np(3+pl,
                            np_head(generic, [], country),

                            [ conj(and,
                                   reduced_rel(B,
                                               s(np(3+pl, wh(B), []),
                                                 verb(be,
                                                      active,
                                                      pres+fin,
                                                      [],
                                                      pos(E)),

                                                 [ varg(pred,
                                                        pp(prep(southof),
                                                           np(3+sg,
                                                              nameOf(equator),
                                                              [])))
                                                 ],
                                                 [])),
                                   reduced_rel(C,
                                               s(np(3+pl, wh(C), []),
                                                 verb(be,
                                                      active,
                                                      pres+fin,
                                                      [],
                                                      neg(not)),

                                                 [ varg(pred,
                                                        pp(prep(in),
                                                           np(3+sg,
                                                              nameOf(australasia),
                                                              [])))
                                                 ],
                                                 [])))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0009999999999763531 sec(s).
answer80([B]):-C^(setof(D:[E],(areaOf(E,D),country(E),southof(E,equator),\+in_ploc(E,australasia)),C),aggregate80(total,C,B))


Reply:(report4) 0.019000000000005457 sec(s).
[[--(10239.035000000003,ksqmiles)]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]

% ============================================END=============================================================
~n~n
true.

baseKB:  ?-   test_chat80_regressions.

% ============================================BEGIN=============================================================

%                       [what,rivers,are,there,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], river), []), verb(be, active, pres+fin, [], pos(B)), [void(there)], [])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], river), []),
          verb(be, active, pres+fin, [], pos(B)),
          [void(there)],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-river(B),B^true


Reply:(report4) 0.0020000000000095497 sec(s).
[[amazon,amu_darya,amur,brahmaputra,colorado,congo_river,cubango,danube,don,elbe,euphrates,ganges,hwang_ho,indus,irrawaddy,lena,limpopo,mackenzie,mekong,mississippi,murray,niger_river,nile,ob,oder,orange,orinoco,parana,rhine,rhone,rio_grande,salween,senegal_river,tagus,vistula,volga,volta,yangtze,yenisei,yukon,zambesi]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,rivers,are,there,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,countries,are,there,in,europe,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(be, active, pres+fin, [], pos(B)), [void(there)], [pp(prep(in), np(3+sg, nameOf(europe), []))])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(be, active, pres+fin, [], pos(B)),
          [void(there)],
          [pp(prep(in), np(3+sg, nameOf(europe), []))])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),in_ploc(B,europe)


Reply:(report4) 0.015999999999991132 sec(s).
[[albania,andorra,austria,belgium,bulgaria,cyprus,czechoslovakia,denmark,east_germany,eire,finland,france,greece,hungary,iceland,italy,liechtenstein,luxembourg,malta,monaco,netherlands,norway,poland,portugal,romania,san_marino,spain,sweden,switzerland,united_kingdom,west_germany,yugoslavia]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,countries,are,there,in,europe,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,country,'\'',s,capital,is,london,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+sg, np_head(det(the(sg)), [], capital), [pp(poss, np(3+sg, np_head(int_det(A), [], country), []))]), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, nameOf(london), []))], [])) :-
    whq(A,
        s(np(3+sg,
             np_head(det(the(sg)), [], capital),
             [pp(poss, np(3+sg, np_head(int_det(A), [], country), []))]),
          verb(be, active, pres+fin, [], pos(B)),
          [varg(dir, np(3+sg, nameOf(london), []))],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-country(B),capital(B,london)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-capital(B,london),{country(B)}


Reply:(report4) 0.0010000000000047748 sec(s).
[[united_kingdom]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,country,'\'',s,capital,is,london,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(D)), [varg(dir, np(3+sg, np_head(det(the(sg)), [adj(total)], area), [pp(prep(of), np(3+pl, np_head(generic, [], country), [conj(and, reduced_rel(B, s(np(3+pl, wh(B), []), verb(be, active, pres+fin, [], pos(E)), [varg(pred, pp(prep(southof), np(3+sg, nameOf(equator), [])))], [])), reduced_rel(C, s(np(3+pl, wh(C), []), verb(be, active, pres+fin, [], neg(not)), [varg(pred, pp(prep(in), np(3+sg, nameOf(australasia), [])))], [])))]))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(D)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [adj(total)], area),

                    [ pp(prep(of),
                         np(3+pl,
                            np_head(generic, [], country),

                            [ conj(and,
                                   reduced_rel(B,
                                               s(np(3+pl, wh(B), []),
                                                 verb(be,
                                                      active,
                                                      pres+fin,
                                                      [],
                                                      pos(E)),

                                                 [ varg(pred,
                                                        pp(prep(southof),
                                                           np(3+sg,
                                                              nameOf(equator),
                                                              [])))
                                                 ],
                                                 [])),
                                   reduced_rel(C,
                                               s(np(3+pl, wh(C), []),
                                                 verb(be,
                                                      active,
                                                      pres+fin,
                                                      [],
                                                      neg(not)),

                                                 [ varg(pred,
                                                        pp(prep(in),
                                                           np(3+sg,
                                                              nameOf(australasia),
                                                              [])))
                                                 ],
                                                 [])))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-C^(setof(D:[E],(areaOf(E,D),country(E),southof(E,equator),\+in_ploc(E,australasia)),C),aggregate80(total,C,B))


Reply:(report4) 0.019000000000005457 sec(s).
[[--(10239.035000000003,ksqmiles)]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [does,afghanistan,border,china,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
q(s(np(3+sg, nameOf(afghanistan), []), verb(border, active, pres+fin, [], pos(A)), [varg(dir, np(3+sg, nameOf(china), []))], [])) :-
    q(s(np(3+sg, nameOf(afghanistan), []),
        verb(border, active, pres+fin, [], pos(A)),
        [varg(dir, np(3+sg, nameOf(china), []))],
        [])).


iSemantics:(report4) 0.0 sec(s).
answer80([]):-borders(afghanistan,china)


actPlanning:(report4) 0.0 sec(s).
answer80([]):-{borders(afghanistan,china)}


Reply:(report4) 0.0010000000000047748 sec(s).
[[true]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [does,afghanistan,border,china,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,capital,of,upper_volta,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(the(sg)), [], capital), [pp(prep(of), np(3+sg, nameOf(upper_volta), []))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [], capital),
                    [pp(prep(of), np(3+sg, nameOf(upper_volta), []))]))
          ],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-capital(upper_volta,B)


Reply:(report4) 0.0 sec(s).
[[ouagadougou]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,capital,of,upper_volta,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [where,is,the,largest,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0 sec(s).
whq(A, s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(large))], country), []), verb(be, active, pres+fin, [], pos(B)), [varg(pred, pp(prep(in), np(C, np_head(int_det(A), [], place), [])))], [])) :-
    whq(A,
        s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(large))], country), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(pred,
                 pp(prep(in),
                    np(C, np_head(int_det(A), [], place), [])))
          ],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-C^(D^(setof(E:F,(country(F),areaOf(F,E)),D),aggregate80(max,D,C)),place(B),in_ploc(C,B))


Reply:(report4) 0.038000000000010914 sec(s).
[[asia,northern_asia]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [where,is,the,largest,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,are,european,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(be, active, pres+fin, [], pos(B)), [varg(pred, adj(european))], [])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(be, active, pres+fin, [], pos(B)),
          [varg(pred, adj(european))],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),european(B)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-european(B),{country(B)}


Reply:(report4) 0.006000000000000227 sec(s).
[[albania,andorra,austria,belgium,bulgaria,cyprus,czechoslovakia,denmark,east_germany,eire,finland,france,greece,hungary,iceland,italy,liechtenstein,luxembourg,malta,monaco,netherlands,norway,poland,portugal,romania,san_marino,spain,sweden,switzerland,united_kingdom,west_germany,yugoslavia]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,are,european,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,is,the,largest,african,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(the(sg)), [sup(most, adj(large)), adj(african)], country), []))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)),
                            [sup(most, adj(large)), adj(african)],
                            country),
                    []))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-C^(setof(D:E,(country(E),areaOf(E,D),african(E)),C),aggregate80(max,C,B))


Reply:(report4) 0.018000000000000682 sec(s).
[[sudan]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,is,the,largest,african,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [how,large,is,the,smallest,american,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0 sec(s).
whq(A, s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(small)), adj(american)], country), []), verb(be, active, pres+fin, [], pos(B)), [varg(pred, value(adj(large), wh(A)))], [])) :-
    whq(A,
        s(np(3+sg,
             np_head(det(the(sg)),
                     [sup(most, adj(small)), adj(american)],
                     country),
             []),
          verb(be, active, pres+fin, [], pos(B)),
          [varg(pred, value(adj(large), wh(A)))],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-C^(D^(setof(E:F,(country(F),areaOf(F,E),american(F)),D),aggregate80(min,D,C)),areaOf(C,B))


Reply:(report4) 0.018000000000000682 sec(s).
[[--(0.133,ksqmiles)]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [how,large,is,the,smallest,american,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,ocean,that,borders,african,countries,and,that,borders,asian,countries,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(D)), [varg(dir, np(3+sg, np_head(det(the(sg)), [], ocean), [conj(and, rel(B, s(np(3+sg, wh(B), []), verb(border, active, pres+fin, [], pos(E)), [varg(dir, np(3+pl, np_head(generic, [adj(african)], country), []))], [])), rel(C, s(np(3+sg, wh(C), []), verb(border, active, pres+fin, [], pos(F)), [varg(dir, np(3+pl, np_head(generic, [adj(asian)], country), []))], [])))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(D)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [], ocean),

                    [ conj(and,
                           rel(B,
                               s(np(3+sg, wh(B), []),
                                 verb(border, active, pres+fin, [], pos(E)),

                                 [ varg(dir,
                                        np(3+pl,
                                           np_head(generic,
                                                   [adj(african)],
                                                   country),
                                           []))
                                 ],
                                 [])),
                           rel(C,
                               s(np(3+sg, wh(C), []),
                                 verb(border, active, pres+fin, [], pos(F)),

                                 [ varg(dir,
                                        np(3+pl,
                                           np_head(generic,
                                                   [adj(asian)],
                                                   country),
                                           []))
                                 ],
                                 [])))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0020000000000095497 sec(s).
answer80([B]):-ocean(B),C^(country(C),african(C),borders(B,C)),D^(country(D),asian(D),borders(B,D))


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-C^D^(ocean(B),{borders(B,C),{african(C)},{country(C)}},{borders(B,D),{asian(D)},{country(D)}})


Reply:(report4) 0.012000000000000455 sec(s).
[[indian_ocean]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,ocean,that,borders,african,countries,and,that,borders,asian,countries,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,are,the,capitals,of,the,countries,bordering,the,baltic,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, wh(A), []), verb(be, active, pres+fin, [], pos(C)), [varg(dir, np(3+pl, np_head(det(the(pl)), [], capital), [pp(prep(of), np(3+pl, np_head(det(the(pl)), [], country), [reduced_rel(B, s(np(3+pl, wh(B), []), verb(border, active, inf, [prog], pos(D)), [varg(dir, np(3+sg, nameOf(baltic), []))], []))]))]))], [])) :-
    whq(A,
        s(np(3+pl, wh(A), []),
          verb(be, active, pres+fin, [], pos(C)),

          [ varg(dir,
                 np(3+pl,
                    np_head(det(the(pl)), [], capital),

                    [ pp(prep(of),
                         np(3+pl,
                            np_head(det(the(pl)), [], country),

                            [ reduced_rel(B,
                                          s(np(3+pl, wh(B), []),
                                            verb(border,
                                                 active,
                                                 inf,
                                                 [prog],
                                                 pos(D)),

                                            [ varg(dir,
                                                   np(3+sg, nameOf(baltic), []))
                                            ],
                                            []))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0020000000000095497 sec(s).
answer80([B]):-setof([C]:D,(country(C),borders(C,baltic),setof(E,capital(C,E),D)),B)


Reply:(report4) 0.010000000000019327 sec(s).
[[[[denmark]:[copenhagen],[east_germany]:[east_berlin],[finland]:[helsinki],[poland]:[warsaw],[soviet_union]:[moscow],[sweden]:[stockholm],[west_germany]:[bonn]]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,are,the,capitals,of,the,countries,bordering,the,baltic,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,are,bordered,by,two,seas,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(border, passive, pres+fin, [], pos(B)), [], [pp(prep(by), np(3+pl, np_head(quant(same, nquant(2)), [], sea), []))])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(border, passive, pres+fin, [], pos(B)),
          [],
          [pp(prep(by), np(3+pl, np_head(quant(same, nquant(2)), [], sea), []))])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),numberof(C,(sea(C),borders(C,B)),2)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-numberof(C,(sea(C),borders(C,B)),2),{country(B)}


Reply:(report4) 0.0010000000000047748 sec(s).
[[egypt,iran,israel,saudi_arabia,turkey]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,are,bordered,by,two,seas,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [how,many,countries,does,the,danube,flow,through,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, nameOf(danube), []), verb(flow, active, pres+fin, [], pos(B)), [], [pp(prep(through), np(3+pl, np_head(quant(same, wh(A)), [], country), []))])) :-
    whq(A,
        s(np(3+sg, nameOf(danube), []),
          verb(flow, active, pres+fin, [], pos(B)),
          [],

          [ pp(prep(through),
               np(3+pl, np_head(quant(same, wh(A)), [], country), []))
          ])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-numberof(C,(country(C),flows(danube,C)),B)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-numberof(C,(flows(danube,C),{country(C)}),B)


Reply:(report4) 0.0009999999999763531 sec(s).
[[6]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [how,many,countries,does,the,danube,flow,through,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,average,area,of,the,countries,in,each,continent,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(the(sg)), [adj(average)], area), [pp(prep(of), np(3+pl, np_head(det(the(pl)), [], country), [pp(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [adj(average)], area),

                    [ pp(prep(of),
                         np(3+pl,
                            np_head(det(the(pl)), [], country),

                            [ pp(prep(in),
                                 np(3+sg, np_head(det(each), [], continent), []))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B,C]):-continent(B),D^(setof(E:[F],(areaOf(F,E),country(F),in_ploc(F,B)),D),aggregate80(average,D,C))


Reply:(report4) 0.12899999999999068 sec(s).
[[[europe,--(58.808937500000006,ksqmiles)]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,average,area,of,the,countries,in,each,continent,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [is,there,more,than,one,country,in,each,continent,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
q(s(there, verb(be, active, pres+fin, [], pos(A)), [varg(dir, np(3+sg, np_head(quant(more, nquant(1)), [], country), [pp(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))], [])) :-
    q(s(there,
        verb(be, active, pres+fin, [], pos(A)),

        [ varg(dir,
               np(3+sg,
                  np_head(quant(more, nquant(1)), [], country),
                  [pp(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))
        ],
        [])).


iSemantics:(report4) 0.0 sec(s).
answer80([]):- \+B^(continent(B),\+C^(numberof(D,(country(D),in_ploc(D,B)),C),C>1))


Reply:(report4) 0.027000000000015234 sec(s).
[[false]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [is,there,more,than,one,country,in,each,continent,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [is,there,some,ocean,that,does,not,border,any,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
q(s(there, verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(some), [], ocean), [rel(A, s(np(3+sg, wh(A), []), verb(border, active, pres+fin, [], neg(not)), [varg(dir, np(3+sg, np_head(det(any), [], country), []))], []))]))], [])) :-
    q(s(there,
        verb(be, active, pres+fin, [], pos(B)),

        [ varg(dir,
               np(3+sg,
                  np_head(det(some), [], ocean),

                  [ rel(A,
                        s(np(3+sg, wh(A), []),
                          verb(border, active, pres+fin, [], neg(not)),
                          [varg(dir, np(3+sg, np_head(det(any), [], country), []))],
                          []))
                  ]))
        ],
        [])).


iSemantics:(report4) 0.0009999999999763531 sec(s).
answer80([]):-B^(ocean(B),\+C^(country(C),borders(B,C)))


actPlanning:(report4) 0.0 sec(s).
answer80([]):-B^{ocean(B),{\+C^(borders(B,C),{country(C)})}}


Reply:(report4) 0.0020000000000095497 sec(s).
[[true]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [is,there,some,ocean,that,does,not,border,any,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,are,the,countries,from,which,a,river,flows,into,the,black_sea,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, wh(A), []), verb(be, active, pres+fin, [], pos(C)), [varg(dir, np(3+pl, np_head(det(the(pl)), [], country), [rel(B, s(np(3+sg, np_head(det(a), [], river), []), verb(flow, active, pres+fin, [], pos(D)), [], [pp(prep(from), np(3+pl, wh(B), [])), pp(prep(into), np(3+sg, nameOf(black_sea), []))]))]))], [])) :-
    whq(A,
        s(np(3+pl, wh(A), []),
          verb(be, active, pres+fin, [], pos(C)),

          [ varg(dir,
                 np(3+pl,
                    np_head(det(the(pl)), [], country),

                    [ rel(B,
                          s(np(3+sg, np_head(det(a), [], river), []),
                            verb(flow, active, pres+fin, [], pos(D)),
                            [],

                            [ pp(prep(from), np(3+pl, wh(B), [])),
                              pp(prep(into), np(3+sg, nameOf(black_sea), []))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-setof(C,(country(C),D^(river(D),flows(D,C,black_sea))),B)


Reply:(report4) 0.38900000000001 sec(s).
[[[romania]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,are,the,countries,from,which,a,river,flows,into,the,black_sea,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,percentage,of,countries,border,each,ocean,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], percentage), [pp(prep(of), np(3+pl, np_head(generic, [], country), []))]), verb(border, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(each), [], ocean), []))], [])) :-
    whq(A,
        s(np(3+pl,
             np_head(int_det(A), [], percentage),
             [pp(prep(of), np(3+pl, np_head(generic, [], country), []))]),
          verb(border, active, pres+fin, [], pos(B)),
          [varg(dir, np(3+sg, np_head(det(each), [], ocean), []))],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B,C]):-ocean(B),D^(setof(E,country(E),D),F^(numberof(G,(one_of(D,G),borders(G,B)),F),H^(card(D,H),ratio(F,H,C))))


Reply:(report4) 0.044999999999987494 sec(s).
[[[arctic_ocean,2.5641025641025643]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,percentage,of,countries,border,each,ocean,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,are,the,continents,no,country,in,which,contains,more,than,two,cities,whose,population,exceeds,nquant(1),million,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0020000000000095497 sec(s).
whq(A, s(np(3+pl, wh(A), []), verb(be, active, pres+fin, [], pos(D)), [varg(dir, np(3+pl, np_head(det(the(pl)), [], continent), [rel(B, s(np(3+sg, np_head(det(no), [], country), [pp(prep(in), np(3+pl, wh(B), []))]), verb(contain, active, pres+fin, [], pos(E)), [varg(dir, np(3+pl, np_head(quant(more, nquant(2)), [], city), [rel(C, s(np(3+sg, np_head(det(the(sg)), [], population), [pp(poss, np(3+pl, wh(C), []))]), verb(exceed, active, pres+fin, [], pos(F)), [varg(dir, np(3+sg, np_head(quant(same, nquant(1)), [], million), []))], []))]))], []))]))], [])) :-
    whq(A,
        s(np(3+pl, wh(A), []),
          verb(be, active, pres+fin, [], pos(D)),

          [ varg(dir,
                 np(3+pl,
                    np_head(det(the(pl)), [], continent),

                    [ rel(B,
                          s(np(3+sg,
                               np_head(det(no), [], country),
                               [pp(prep(in), np(3+pl, wh(B), []))]),
                            verb(contain, active, pres+fin, [], pos(E)),

                            [ varg(dir,
                                   np(3+pl,
                                      np_head(quant(more, nquant(2)), [], city),

                                      [ rel(C,
                                            s(np(3+sg,
                                                 np_head(det(the(sg)),
                                                         [],
                                                         population),

                                                 [ pp(poss,
                                                      np(3+pl, wh(C), []))
                                                 ]),
                                              verb(exceed,
                                                   active,
                                                   pres+fin,
                                                   [],
                                                   pos(F)),

                                              [ varg(dir,
                                                     np(3+sg,
                                                        np_head(quant(same,
                                                                      nquant(1)),
                                                                [],
                                                                million),
                                                        []))
                                              ],
                                              []))
                                      ]))
                            ],
                            []))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-setof(C,(continent(C),\+D^(country(D),in_ploc(D,C),E^(numberof(F,(city(F),G^(population(F,G),exceeds(G,--(1,million))),in_ploc(F,D)),E),E>2))),B)


Reply:(report4) 2.2439999999999998 sec(s).
[[[africa,america,antarctica,asia,australasia,europe]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,are,the,continents,no,country,in,which,contains,more,than,two,cities,whose,population,exceeds,nquant(1),million,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,country,bordering,the,mediterranean,borders,a,country,that,is,bordered,by,a,country,whose,population,exceeds,the,population,of,india,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0020000000000095497 sec(s).
whq(A, s(np(3+sg, np_head(int_det(A), [], country), [reduced_rel(B, s(np(3+sg, wh(B), []), verb(border, active, inf, [prog], pos(E)), [varg(dir, np(3+sg, nameOf(mediterranean), []))], []))]), verb(border, active, pres+fin, [], pos(F)), [varg(dir, np(3+sg, np_head(det(a), [], country), [rel(C, s(np(3+sg, wh(C), []), verb(border, passive, pres+fin, [], pos(G)), [], [pp(prep(by), np(3+sg, np_head(det(a), [], country), [rel(D, s(np(3+sg, np_head(det(the(sg)), [], population), [pp(poss, np(3+sg, wh(D), []))]), verb(exceed, active, pres+fin, [], pos(H)), [varg(dir, np(3+sg, np_head(det(the(sg)), [], population), [pp(prep(of), np(3+sg, nameOf(india), []))]))], []))]))]))]))], [])) :-
    whq(A,
        s(np(3+sg,
             np_head(int_det(A), [], country),

             [ reduced_rel(B,
                           s(np(3+sg, wh(B), []),
                             verb(border, active, inf, [prog], pos(E)),
                             [varg(dir, np(3+sg, nameOf(mediterranean), []))],
                             []))
             ]),
          verb(border, active, pres+fin, [], pos(F)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(a), [], country),

                    [ rel(C,
                          s(np(3+sg, wh(C), []),
                            verb(border, passive, pres+fin, [], pos(G)),
                            [],

                            [ pp(prep(by),
                                 np(3+sg,
                                    np_head(det(a), [], country),

                                    [ rel(D,
                                          s(np(3+sg,
                                               np_head(det(the(sg)),
                                                       [],
                                                       population),

                                               [ pp(poss,
                                                    np(3+sg, wh(D), []))
                                               ]),
                                            verb(exceed,
                                                 active,
                                                 pres+fin,
                                                 [],
                                                 pos(H)),

                                            [ varg(dir,
                                                   np(3+sg,
                                                      np_head(det(the(sg)),
                                                              [],
                                                              population),

                                                      [ pp(prep(of),
                                                           np(3+sg,
                                                              nameOf(india),
                                                              []))
                                                      ]))
                                            ],
                                            []))
                                    ]))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),borders(B,mediterranean),C^(country(C),D^(country(D),E^(population(D,E),F^(population(india,F),exceeds(E,F))),borders(D,C)),borders(B,C))


actPlanning:(report4) 0.0009999999999763531 sec(s).
answer80([B]):-C^D^E^F^(population(india,F),borders(B,mediterranean),{country(B)},{borders(B,C),{country(C)},{borders(D,C),{country(D)},{population(D,E),{exceeds(E,F)}}}})


Reply:(report4) 0.07699999999999818 sec(s).
[[turkey]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,country,bordering,the,mediterranean,borders,a,country,that,is,bordered,by,a,country,whose,population,exceeds,the,population,of,india,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,have,a,population,exceeding,nquant(10),million,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(have, active, pres+fin, [], pos(C)), [varg(dir, np(3+sg, np_head(det(a), [], population), [reduced_rel(B, s(np(3+sg, wh(B), []), verb(exceed, active, inf, [prog], pos(D)), [varg(dir, np(3+pl, np_head(quant(same, nquant(10)), [], million), []))], []))]))], [])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(have, active, pres+fin, [], pos(C)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(a), [], population),

                    [ reduced_rel(B,
                                  s(np(3+sg, wh(B), []),
                                    verb(exceed,
                                         active,
                                         inf,
                                         [prog],
                                         pos(D)),

                                    [ varg(dir,
                                           np(3+pl,
                                              np_head(quant(same, nquant(10)),
                                                      [],
                                                      million),
                                              []))
                                    ],
                                    []))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-country(B),C^(exceeds(C,--(10,million)),population(B,C))


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-C^(country(B),{population(B,C),{exceeds(C,--(10,million))}})


Reply:(report4) 0.020000000000010232 sec(s).
[[malaysia,uganda]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,have,a,population,exceeding,nquant(10),million,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,with,a,population,exceeding,nquant(10),million,border,the,atlantic,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), [pp(prep(with), np(3+sg, np_head(det(a), [], population), [reduced_rel(B, s(np(3+sg, wh(B), []), verb(exceed, active, inf, [prog], pos(C)), [varg(dir, np(3+pl, np_head(quant(same, nquant(10)), [], million), []))], []))]))]), verb(border, active, pres+fin, [], pos(D)), [varg(dir, np(3+sg, nameOf(atlantic), []))], [])) :-
    whq(A,
        s(np(3+pl,
             np_head(int_det(A), [], country),

             [ pp(prep(with),
                  np(3+sg,
                     np_head(det(a), [], population),

                     [ reduced_rel(B,
                                   s(np(3+sg, wh(B), []),
                                     verb(exceed,
                                          active,
                                          inf,
                                          [prog],
                                          pos(C)),

                                     [ varg(dir,
                                            np(3+pl,
                                               np_head(quant(same, nquant(10)),
                                                       [],
                                                       million),
                                               []))
                                     ],
                                     []))
                     ]))
             ]),
          verb(border, active, pres+fin, [], pos(D)),
          [varg(dir, np(3+sg, nameOf(atlantic), []))],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-C^(population(B,C),exceeds(C,--(10,million)),country(B)),borders(B,atlantic)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-C^(borders(B,atlantic),{population(B,C),{exceeds(C,--(10,million))}},{country(B)})


Reply:(report4) 0.007000000000005002 sec(s).
[[venezuela]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,with,a,population,exceeding,nquant(10),million,border,the,atlantic,?]

% ============================================END=============================================================
~n~n
true.

baseKB:  ?- cls.
[Detaching after fork from child process 32196]
true.

baseKB:  ?- test_chat80_regressions.

% ============================================BEGIN=============================================================

%                       [what,rivers,are,there,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], river), []), verb(be, active, pres+fin, [], pos(B)), [void(there)], [])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], river), []),
          verb(be, active, pres+fin, [], pos(B)),
          [void(there)],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-river(B),B^true


Reply:(report4) 0.0020000000000095497 sec(s).
[[amazon,amu_darya,amur,brahmaputra,colorado,congo_river,cubango,danube,don,elbe,euphrates,ganges,hwang_ho,indus,irrawaddy,lena,limpopo,mackenzie,mekong,mississippi,murray,niger_river,nile,ob,oder,orange,orinoco,parana,rhine,rhone,rio_grande,salween,senegal_river,tagus,vistula,volga,volta,yangtze,yenisei,yukon,zambesi]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,rivers,are,there,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,countries,are,there,in,europe,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(be, active, pres+fin, [], pos(B)), [void(there)], [pp(prep(in), np(3+sg, nameOf(europe), []))])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(be, active, pres+fin, [], pos(B)),
          [void(there)],
          [pp(prep(in), np(3+sg, nameOf(europe), []))])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),in_ploc(B,europe)


Reply:(report4) 0.019000000000005457 sec(s).
[[albania,andorra,austria,belgium,bulgaria,cyprus,czechoslovakia,denmark,east_germany,eire,finland,france,greece,hungary,iceland,italy,liechtenstein,luxembourg,malta,monaco,netherlands,norway,poland,portugal,romania,san_marino,spain,sweden,switzerland,united_kingdom,west_germany,yugoslavia]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,countries,are,there,in,europe,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,country,'\'',s,capital,is,london,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, np_head(det(the(sg)), [], capital), [pp(poss, np(3+sg, np_head(int_det(A), [], country), []))]), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, nameOf(london), []))], [])) :-
    whq(A,
        s(np(3+sg,
             np_head(det(the(sg)), [], capital),
             [pp(poss, np(3+sg, np_head(int_det(A), [], country), []))]),
          verb(be, active, pres+fin, [], pos(B)),
          [varg(dir, np(3+sg, nameOf(london), []))],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),capital(B,london)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-capital(B,london),{country(B)}


Reply:(report4) 0.0 sec(s).
[[united_kingdom]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,country,'\'',s,capital,is,london,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(D)), [varg(dir, np(3+sg, np_head(det(the(sg)), [adj(total)], area), [pp(prep(of), np(3+pl, np_head(generic, [], country), [conj(and, reduced_rel(B, s(np(3+pl, wh(B), []), verb(be, active, pres+fin, [], pos(E)), [varg(pred, pp(prep(southof), np(3+sg, nameOf(equator), [])))], [])), reduced_rel(C, s(np(3+pl, wh(C), []), verb(be, active, pres+fin, [], neg(not)), [varg(pred, pp(prep(in), np(3+sg, nameOf(australasia), [])))], [])))]))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(D)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [adj(total)], area),

                    [ pp(prep(of),
                         np(3+pl,
                            np_head(generic, [], country),

                            [ conj(and,
                                   reduced_rel(B,
                                               s(np(3+pl, wh(B), []),
                                                 verb(be,
                                                      active,
                                                      pres+fin,
                                                      [],
                                                      pos(E)),

                                                 [ varg(pred,
                                                        pp(prep(southof),
                                                           np(3+sg,
                                                              nameOf(equator),
                                                              [])))
                                                 ],
                                                 [])),
                                   reduced_rel(C,
                                               s(np(3+pl, wh(C), []),
                                                 verb(be,
                                                      active,
                                                      pres+fin,
                                                      [],
                                                      neg(not)),

                                                 [ varg(pred,
                                                        pp(prep(in),
                                                           np(3+sg,
                                                              nameOf(australasia),
                                                              [])))
                                                 ],
                                                 [])))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0020000000000095497 sec(s).
answer80([B]):-C^(setof(D:[E],(areaOf(E,D),country(E),southof(E,equator),\+in_ploc(E,australasia)),C),aggregate80(total,C,B))


Reply:(report4) 0.036000000000001364 sec(s).
[[--(10239.035000000003,ksqmiles)]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,total,area,of,countries,south,of,the,equator,and,not,in,australasia,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [does,afghanistan,border,china,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
q(s(np(3+sg, nameOf(afghanistan), []), verb(border, active, pres+fin, [], pos(A)), [varg(dir, np(3+sg, nameOf(china), []))], [])) :-
    q(s(np(3+sg, nameOf(afghanistan), []),
        verb(border, active, pres+fin, [], pos(A)),
        [varg(dir, np(3+sg, nameOf(china), []))],
        [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([]):-borders(afghanistan,china)


actPlanning:(report4) 0.0 sec(s).
answer80([]):-{borders(afghanistan,china)}


Reply:(report4) 0.0 sec(s).
[[true]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [does,afghanistan,border,china,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,capital,of,upper_volta,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(the(sg)), [], capital), [pp(prep(of), np(3+sg, nameOf(upper_volta), []))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [], capital),
                    [pp(prep(of), np(3+sg, nameOf(upper_volta), []))]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-capital(upper_volta,B)


Reply:(report4) 0.0 sec(s).
[[ouagadougou]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,capital,of,upper_volta,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [where,is,the,largest,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(large))], country), []), verb(be, active, pres+fin, [], pos(B)), [varg(pred, pp(prep(in), np(C, np_head(int_det(A), [], place), [])))], [])) :-
    whq(A,
        s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(large))], country), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(pred,
                 pp(prep(in),
                    np(C, np_head(int_det(A), [], place), [])))
          ],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-C^(D^(setof(E:F,(country(F),areaOf(F,E)),D),aggregate80(max,D,C)),place(B),in_ploc(C,B))


Reply:(report4) 0.02199999999999136 sec(s).
[[asia,northern_asia]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [where,is,the,largest,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,are,european,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(be, active, pres+fin, [], pos(B)), [varg(pred, adj(european))], [])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(be, active, pres+fin, [], pos(B)),
          [varg(pred, adj(european))],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),european(B)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-european(B),{country(B)}


Reply:(report4) 0.003999999999990678 sec(s).
[[albania,andorra,austria,belgium,bulgaria,cyprus,czechoslovakia,denmark,east_germany,eire,finland,france,greece,hungary,iceland,italy,liechtenstein,luxembourg,malta,monaco,netherlands,norway,poland,portugal,romania,san_marino,spain,sweden,switzerland,united_kingdom,west_germany,yugoslavia]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,are,european,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,is,the,largest,african,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(the(sg)), [sup(most, adj(large)), adj(african)], country), []))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)),
                            [sup(most, adj(large)), adj(african)],
                            country),
                    []))
          ],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-C^(setof(D:E,(country(E),areaOf(E,D),african(E)),C),aggregate80(max,C,B))


Reply:(report4) 0.02200000000001978 sec(s).
[[sudan]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,is,the,largest,african,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [how,large,is,the,smallest,american,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, np_head(det(the(sg)), [sup(most, adj(small)), adj(american)], country), []), verb(be, active, pres+fin, [], pos(B)), [varg(pred, value(adj(large), wh(A)))], [])) :-
    whq(A,
        s(np(3+sg,
             np_head(det(the(sg)),
                     [sup(most, adj(small)), adj(american)],
                     country),
             []),
          verb(be, active, pres+fin, [], pos(B)),
          [varg(pred, value(adj(large), wh(A)))],
          [])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-C^(D^(setof(E:F,(country(F),areaOf(F,E),american(F)),D),aggregate80(min,D,C)),areaOf(C,B))


Reply:(report4) 0.018000000000000682 sec(s).
[[--(0.133,ksqmiles)]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [how,large,is,the,smallest,american,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,ocean,that,borders,african,countries,and,that,borders,asian,countries,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(D)), [varg(dir, np(3+sg, np_head(det(the(sg)), [], ocean), [conj(and, rel(B, s(np(3+sg, wh(B), []), verb(border, active, pres+fin, [], pos(E)), [varg(dir, np(3+pl, np_head(generic, [adj(african)], country), []))], [])), rel(C, s(np(3+sg, wh(C), []), verb(border, active, pres+fin, [], pos(F)), [varg(dir, np(3+pl, np_head(generic, [adj(asian)], country), []))], [])))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(D)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [], ocean),

                    [ conj(and,
                           rel(B,
                               s(np(3+sg, wh(B), []),
                                 verb(border, active, pres+fin, [], pos(E)),

                                 [ varg(dir,
                                        np(3+pl,
                                           np_head(generic,
                                                   [adj(african)],
                                                   country),
                                           []))
                                 ],
                                 [])),
                           rel(C,
                               s(np(3+sg, wh(C), []),
                                 verb(border, active, pres+fin, [], pos(F)),

                                 [ varg(dir,
                                        np(3+pl,
                                           np_head(generic,
                                                   [adj(asian)],
                                                   country),
                                           []))
                                 ],
                                 [])))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-ocean(B),C^(country(C),african(C),borders(B,C)),D^(country(D),asian(D),borders(B,D))


actPlanning:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-C^D^(ocean(B),{borders(B,C),{african(C)},{country(C)}},{borders(B,D),{asian(D)},{country(D)}})


Reply:(report4) 0.009999999999990905 sec(s).
[[indian_ocean]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,ocean,that,borders,african,countries,and,that,borders,asian,countries,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,are,the,capitals,of,the,countries,bordering,the,baltic,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, wh(A), []), verb(be, active, pres+fin, [], pos(C)), [varg(dir, np(3+pl, np_head(det(the(pl)), [], capital), [pp(prep(of), np(3+pl, np_head(det(the(pl)), [], country), [reduced_rel(B, s(np(3+pl, wh(B), []), verb(border, active, inf, [prog], pos(D)), [varg(dir, np(3+sg, nameOf(baltic), []))], []))]))]))], [])) :-
    whq(A,
        s(np(3+pl, wh(A), []),
          verb(be, active, pres+fin, [], pos(C)),

          [ varg(dir,
                 np(3+pl,
                    np_head(det(the(pl)), [], capital),

                    [ pp(prep(of),
                         np(3+pl,
                            np_head(det(the(pl)), [], country),

                            [ reduced_rel(B,
                                          s(np(3+pl, wh(B), []),
                                            verb(border,
                                                 active,
                                                 inf,
                                                 [prog],
                                                 pos(D)),

                                            [ varg(dir,
                                                   np(3+sg, nameOf(baltic), []))
                                            ],
                                            []))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-setof([C]:D,(country(C),borders(C,baltic),setof(E,capital(C,E),D)),B)


Reply:(report4) 0.00899999999998613 sec(s).
[[[[denmark]:[copenhagen],[east_germany]:[east_berlin],[finland]:[helsinki],[poland]:[warsaw],[soviet_union]:[moscow],[sweden]:[stockholm],[west_germany]:[bonn]]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,are,the,capitals,of,the,countries,bordering,the,baltic,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,are,bordered,by,two,seas,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(border, passive, pres+fin, [], pos(B)), [], [pp(prep(by), np(3+pl, np_head(quant(same, nquant(2)), [], sea), []))])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(border, passive, pres+fin, [], pos(B)),
          [],
          [pp(prep(by), np(3+pl, np_head(quant(same, nquant(2)), [], sea), []))])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-country(B),numberof(C,(sea(C),borders(C,B)),2)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-numberof(C,(sea(C),borders(C,B)),2),{country(B)}


Reply:(report4) 0.0010000000000047748 sec(s).
[[egypt,iran,israel,saudi_arabia,turkey]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,are,bordered,by,two,seas,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [how,many,countries,does,the,danube,flow,through,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, nameOf(danube), []), verb(flow, active, pres+fin, [], pos(B)), [], [pp(prep(through), np(3+pl, np_head(quant(same, wh(A)), [], country), []))])) :-
    whq(A,
        s(np(3+sg, nameOf(danube), []),
          verb(flow, active, pres+fin, [], pos(B)),
          [],

          [ pp(prep(through),
               np(3+pl, np_head(quant(same, wh(A)), [], country), []))
          ])).


iSemantics:(report4) 0.0 sec(s).
answer80([B]):-numberof(C,(country(C),flows(danube,C)),B)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-numberof(C,(flows(danube,C),{country(C)}),B)


Reply:(report4) 0.0009999999999763531 sec(s).
[[6]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [how,many,countries,does,the,danube,flow,through,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,is,the,average,area,of,the,countries,in,each,continent,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+sg, wh(A), []), verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(the(sg)), [adj(average)], area), [pp(prep(of), np(3+pl, np_head(det(the(pl)), [], country), [pp(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))]))], [])) :-
    whq(A,
        s(np(3+sg, wh(A), []),
          verb(be, active, pres+fin, [], pos(B)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(the(sg)), [adj(average)], area),

                    [ pp(prep(of),
                         np(3+pl,
                            np_head(det(the(pl)), [], country),

                            [ pp(prep(in),
                                 np(3+sg, np_head(det(each), [], continent), []))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B,C]):-continent(B),D^(setof(E:[F],(areaOf(F,E),country(F),in_ploc(F,B)),D),aggregate80(average,D,C))


Reply:(report4) 0.10300000000000864 sec(s).
[[[europe,--(58.808937500000006,ksqmiles)]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,is,the,average,area,of,the,countries,in,each,continent,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [is,there,more,than,one,country,in,each,continent,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
q(s(there, verb(be, active, pres+fin, [], pos(A)), [varg(dir, np(3+sg, np_head(quant(more, nquant(1)), [], country), [pp(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))], [])) :-
    q(s(there,
        verb(be, active, pres+fin, [], pos(A)),

        [ varg(dir,
               np(3+sg,
                  np_head(quant(more, nquant(1)), [], country),
                  [pp(prep(in), np(3+sg, np_head(det(each), [], continent), []))]))
        ],
        [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([]):- \+B^(continent(B),\+C^(numberof(D,(country(D),in_ploc(D,B)),C),C>1))


Reply:(report4) 0.027000000000015234 sec(s).
[[false]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [is,there,more,than,one,country,in,each,continent,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [is,there,some,ocean,that,does,not,border,any,country,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
q(s(there, verb(be, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(some), [], ocean), [rel(A, s(np(3+sg, wh(A), []), verb(border, active, pres+fin, [], neg(not)), [varg(dir, np(3+sg, np_head(det(any), [], country), []))], []))]))], [])) :-
    q(s(there,
        verb(be, active, pres+fin, [], pos(B)),

        [ varg(dir,
               np(3+sg,
                  np_head(det(some), [], ocean),

                  [ rel(A,
                        s(np(3+sg, wh(A), []),
                          verb(border, active, pres+fin, [], neg(not)),
                          [varg(dir, np(3+sg, np_head(det(any), [], country), []))],
                          []))
                  ]))
        ],
        [])).


iSemantics:(report4) 0.0 sec(s).
answer80([]):-B^(ocean(B),\+C^(country(C),borders(B,C)))


actPlanning:(report4) 0.0 sec(s).
answer80([]):-B^{ocean(B),{\+C^(borders(B,C),{country(C)})}}


Reply:(report4) 0.0009999999999763531 sec(s).
[[true]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [is,there,some,ocean,that,does,not,border,any,country,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,are,the,countries,from,which,a,river,flows,into,the,black_sea,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+pl, wh(A), []), verb(be, active, pres+fin, [], pos(C)), [varg(dir, np(3+pl, np_head(det(the(pl)), [], country), [rel(B, s(np(3+sg, np_head(det(a), [], river), []), verb(flow, active, pres+fin, [], pos(D)), [], [pp(prep(from), np(3+pl, wh(B), [])), pp(prep(into), np(3+sg, nameOf(black_sea), []))]))]))], [])) :-
    whq(A,
        s(np(3+pl, wh(A), []),
          verb(be, active, pres+fin, [], pos(C)),

          [ varg(dir,
                 np(3+pl,
                    np_head(det(the(pl)), [], country),

                    [ rel(B,
                          s(np(3+sg, np_head(det(a), [], river), []),
                            verb(flow, active, pres+fin, [], pos(D)),
                            [],

                            [ pp(prep(from), np(3+pl, wh(B), [])),
                              pp(prep(into), np(3+sg, nameOf(black_sea), []))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-setof(C,(country(C),D^(river(D),flows(D,C,black_sea))),B)


Reply:(report4) 0.35900000000000887 sec(s).
[[[romania]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,are,the,countries,from,which,a,river,flows,into,the,black_sea,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,percentage,of,countries,border,each,ocean,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], percentage), [pp(prep(of), np(3+pl, np_head(generic, [], country), []))]), verb(border, active, pres+fin, [], pos(B)), [varg(dir, np(3+sg, np_head(det(each), [], ocean), []))], [])) :-
    whq(A,
        s(np(3+pl,
             np_head(int_det(A), [], percentage),
             [pp(prep(of), np(3+pl, np_head(generic, [], country), []))]),
          verb(border, active, pres+fin, [], pos(B)),
          [varg(dir, np(3+sg, np_head(det(each), [], ocean), []))],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B,C]):-ocean(B),D^(setof(E,country(E),D),F^(numberof(G,(one_of(D,G),borders(G,B)),F),H^(card(D,H),ratio(F,H,C))))


Reply:(report4) 0.042999999999977945 sec(s).
[[[arctic_ocean,2.5641025641025643]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,percentage,of,countries,border,each,ocean,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [what,are,the,continents,no,country,in,which,contains,more,than,two,cities,whose,population,exceeds,nquant(1),million,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0020000000000095497 sec(s).
whq(A, s(np(3+pl, wh(A), []), verb(be, active, pres+fin, [], pos(D)), [varg(dir, np(3+pl, np_head(det(the(pl)), [], continent), [rel(B, s(np(3+sg, np_head(det(no), [], country), [pp(prep(in), np(3+pl, wh(B), []))]), verb(contain, active, pres+fin, [], pos(E)), [varg(dir, np(3+pl, np_head(quant(more, nquant(2)), [], city), [rel(C, s(np(3+sg, np_head(det(the(sg)), [], population), [pp(poss, np(3+pl, wh(C), []))]), verb(exceed, active, pres+fin, [], pos(F)), [varg(dir, np(3+sg, np_head(quant(same, nquant(1)), [], million), []))], []))]))], []))]))], [])) :-
    whq(A,
        s(np(3+pl, wh(A), []),
          verb(be, active, pres+fin, [], pos(D)),

          [ varg(dir,
                 np(3+pl,
                    np_head(det(the(pl)), [], continent),

                    [ rel(B,
                          s(np(3+sg,
                               np_head(det(no), [], country),
                               [pp(prep(in), np(3+pl, wh(B), []))]),
                            verb(contain, active, pres+fin, [], pos(E)),

                            [ varg(dir,
                                   np(3+pl,
                                      np_head(quant(more, nquant(2)), [], city),

                                      [ rel(C,
                                            s(np(3+sg,
                                                 np_head(det(the(sg)),
                                                         [],
                                                         population),

                                                 [ pp(poss,
                                                      np(3+pl, wh(C), []))
                                                 ]),
                                              verb(exceed,
                                                   active,
                                                   pres+fin,
                                                   [],
                                                   pos(F)),

                                              [ varg(dir,
                                                     np(3+sg,
                                                        np_head(quant(same,
                                                                      nquant(1)),
                                                                [],
                                                                million),
                                                        []))
                                              ],
                                              []))
                                      ]))
                            ],
                            []))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0020000000000095497 sec(s).
answer80([B]):-setof(C,(continent(C),\+D^(country(D),in_ploc(D,C),E^(numberof(F,(city(F),G^(population(F,G),exceeds(G,--(1,million))),in_ploc(F,D)),E),E>2))),B)


Reply:(report4) 2.194000000000017 sec(s).
[[[africa,america,antarctica,asia,australasia,europe]]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [what,are,the,continents,no,country,in,which,contains,more,than,two,cities,whose,population,exceeds,nquant(1),million,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,country,bordering,the,mediterranean,borders,a,country,that,is,bordered,by,a,country,whose,population,exceeds,the,population,of,india,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0020000000000095497 sec(s).
whq(A, s(np(3+sg, np_head(int_det(A), [], country), [reduced_rel(B, s(np(3+sg, wh(B), []), verb(border, active, inf, [prog], pos(E)), [varg(dir, np(3+sg, nameOf(mediterranean), []))], []))]), verb(border, active, pres+fin, [], pos(F)), [varg(dir, np(3+sg, np_head(det(a), [], country), [rel(C, s(np(3+sg, wh(C), []), verb(border, passive, pres+fin, [], pos(G)), [], [pp(prep(by), np(3+sg, np_head(det(a), [], country), [rel(D, s(np(3+sg, np_head(det(the(sg)), [], population), [pp(poss, np(3+sg, wh(D), []))]), verb(exceed, active, pres+fin, [], pos(H)), [varg(dir, np(3+sg, np_head(det(the(sg)), [], population), [pp(prep(of), np(3+sg, nameOf(india), []))]))], []))]))]))]))], [])) :-
    whq(A,
        s(np(3+sg,
             np_head(int_det(A), [], country),

             [ reduced_rel(B,
                           s(np(3+sg, wh(B), []),
                             verb(border, active, inf, [prog], pos(E)),
                             [varg(dir, np(3+sg, nameOf(mediterranean), []))],
                             []))
             ]),
          verb(border, active, pres+fin, [], pos(F)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(a), [], country),

                    [ rel(C,
                          s(np(3+sg, wh(C), []),
                            verb(border, passive, pres+fin, [], pos(G)),
                            [],

                            [ pp(prep(by),
                                 np(3+sg,
                                    np_head(det(a), [], country),

                                    [ rel(D,
                                          s(np(3+sg,
                                               np_head(det(the(sg)),
                                                       [],
                                                       population),

                                               [ pp(poss,
                                                    np(3+sg, wh(D), []))
                                               ]),
                                            verb(exceed,
                                                 active,
                                                 pres+fin,
                                                 [],
                                                 pos(H)),

                                            [ varg(dir,
                                                   np(3+sg,
                                                      np_head(det(the(sg)),
                                                              [],
                                                              population),

                                                      [ pp(prep(of),
                                                           np(3+sg,
                                                              nameOf(india),
                                                              []))
                                                      ]))
                                            ],
                                            []))
                                    ]))
                            ]))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),borders(B,mediterranean),C^(country(C),D^(country(D),E^(population(D,E),F^(population(india,F),exceeds(E,F))),borders(D,C)),borders(B,C))


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-C^D^E^F^(population(india,F),borders(B,mediterranean),{country(B)},{borders(B,C),{country(C)},{borders(D,C),{country(D)},{population(D,E),{exceeds(E,F)}}}})


Reply:(report4) 0.09899999999998954 sec(s).
[[turkey]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,country,bordering,the,mediterranean,borders,a,country,that,is,bordered,by,a,country,whose,population,exceeds,the,population,of,india,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,have,a,population,exceeding,nquant(10),million,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0009999999999763531 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), []), verb(have, active, pres+fin, [], pos(C)), [varg(dir, np(3+sg, np_head(det(a), [], population), [reduced_rel(B, s(np(3+sg, wh(B), []), verb(exceed, active, inf, [prog], pos(D)), [varg(dir, np(3+pl, np_head(quant(same, nquant(10)), [], million), []))], []))]))], [])) :-
    whq(A,
        s(np(3+pl, np_head(int_det(A), [], country), []),
          verb(have, active, pres+fin, [], pos(C)),

          [ varg(dir,
                 np(3+sg,
                    np_head(det(a), [], population),

                    [ reduced_rel(B,
                                  s(np(3+sg, wh(B), []),
                                    verb(exceed,
                                         active,
                                         inf,
                                         [prog],
                                         pos(D)),

                                    [ varg(dir,
                                           np(3+pl,
                                              np_head(quant(same, nquant(10)),
                                                      [],
                                                      million),
                                              []))
                                    ],
                                    []))
                    ]))
          ],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-country(B),C^(exceeds(C,--(10,million)),population(B,C))


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-C^(country(B),{population(B,C),{exceeds(C,--(10,million))}})


Reply:(report4) 0.016999999999995907 sec(s).
[[malaysia,uganda]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,have,a,population,exceeding,nquant(10),million,?]

% ============================================END=============================================================
~n~n

% ============================================BEGIN=============================================================

%                       [which,countries,with,a,population,exceeding,nquant(10),million,border,the,atlantic,?]

% ---------------------------------------------------------------------------------------------------

Parse:(report4) 0.0010000000000047748 sec(s).
whq(A, s(np(3+pl, np_head(int_det(A), [], country), [pp(prep(with), np(3+sg, np_head(det(a), [], population), [reduced_rel(B, s(np(3+sg, wh(B), []), verb(exceed, active, inf, [prog], pos(C)), [varg(dir, np(3+pl, np_head(quant(same, nquant(10)), [], million), []))], []))]))]), verb(border, active, pres+fin, [], pos(D)), [varg(dir, np(3+sg, nameOf(atlantic), []))], [])) :-
    whq(A,
        s(np(3+pl,
             np_head(int_det(A), [], country),

             [ pp(prep(with),
                  np(3+sg,
                     np_head(det(a), [], population),

                     [ reduced_rel(B,
                                   s(np(3+sg, wh(B), []),
                                     verb(exceed,
                                          active,
                                          inf,
                                          [prog],
                                          pos(C)),

                                     [ varg(dir,
                                            np(3+pl,
                                               np_head(quant(same, nquant(10)),
                                                       [],
                                                       million),
                                               []))
                                     ],
                                     []))
                     ]))
             ]),
          verb(border, active, pres+fin, [], pos(D)),
          [varg(dir, np(3+sg, nameOf(atlantic), []))],
          [])).


iSemantics:(report4) 0.0010000000000047748 sec(s).
answer80([B]):-C^(population(B,C),exceeds(C,--(10,million)),country(B)),borders(B,atlantic)


actPlanning:(report4) 0.0 sec(s).
answer80([B]):-C^(borders(B,atlantic),{population(B,C),{exceeds(C,--(10,million))}},{country(B)})


Reply:(report4) 0.008000000000009777 sec(s).
[[venezuela]]


% ---------------------------------------------------------------------------------------------------

% ---------------------------------------------------------------------------------------------------

%                       [which,countries,with,a,population,exceeding,nquant(10),million,border,the,atlantic,?]

% ============================================END=============================================================
~n~n
true.

baseKB:  ?-

````

SIRIDUS

TrindiKit

GoDuS

BOXER

GATE's SUPPLE

BuChart 98

and about 10 more!


