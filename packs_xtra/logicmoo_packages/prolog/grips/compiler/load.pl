/*  LOAD.PL  */


loadcode( Code ) <-
    loadcode1( Code, map, 0 ).


loadcode1( [], Map, Loc ) <-
    Map.

loadcode1( [ 'LABEL'(_) | Rest ], Map, Loc ) <-
    loadcode1( Rest, Map, Loc ).

loadcode1( [ Instr | Rest ], Map, Loc ) <-
    loadcode1( Rest, map_update(Map,Loc,Instr), Loc+width(Instr) ).
