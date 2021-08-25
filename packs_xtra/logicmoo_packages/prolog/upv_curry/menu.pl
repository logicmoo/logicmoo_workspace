:- expects_dialect(sicstus).
:- set_prolog_flag(double_quotes, codes).
:- style_check(-discontiguous).
% ---------------------------------------------------------------------------
% Menu 
% ---------------------------------------------------------------------------

:- thread_local 
     haltSystem/0.

main :- haltSystem,halt.
main :- 
        \+(haltSystem),
         must_det((prompt(_,'> '), prompt( InputS1 ),
          process_then(InputS1,main))).

process_then(InputS1, Main):-
      on_exception(_,
          (process( InputS1, InputS2 ), 
          (  InputS2=[], Input=[], Main
           ; sappend( Input, [10], InputS2 ),
             (  command_read( Input )
              ; command_debug( Input )
              ; command_deftree( Input )
              ; command_type( Input )
              ; command_help( Input )
              ; command_cd( Input )
              ; command_pwd( Input )
              ; command_exit( Input )
              ; (nopass,command_solve( Input ) )), 
             !,Main ))
        ,Main)
        .

prompt( Input ) :-
        get0(X),
        ( (X =:= 10 ; X =:= 13), !, Input = []
         ;Input = [X|Input2], prompt(Input2) ).

command_solve( `` ) :- !.
command_solve( [] ) :- !.
command_solve( Input ) :- %No known command accepted
        ( sappend( `:load `,  _, Input )
         ;sappend( `:l `,     _, Input )
         ;sappend( `:debug `, _, Input )
         ;sappend( `:d `,     _, Input )
         ;sappend( `:deftree `, _, Input )
         ;sappend( `:e `, _, Input )
         ;sappend( `:type `, _, Input )
         ;sappend( `:t `, _, Input )
         ;sappend( `:help `, _, Input )
         ;sappend( `:h `, _, Input )
         ;sappend( `:cd `, _, Input )
         ;sappend( `:c `, _, Input )
         ;sappend( `:type `, _, Input )
         ;sappend( `:t `, _, Input )
         ;sappend( `:quit `, _, Input )
         ;sappend( `:q `, _, Input )      ),
        !.
command_solve( Input ) :-
        cleanVars,
        exprIni( Exp, Input, []),
        sCheckExpr( Exp, _, [], _),!,
        ( solve( Exp )
         ;write('') ).
command_solve( Text ) :- 
        name( Atom, Text ),
        write('ERROR: Unknown (maybe Incorrect) expression "'), write(Atom), write('" found'),nl.

command_read( Input ) :-
        ( sappend( `:load `, Str, Input )
         ;sappend( `:l `,    Str, Input ) ), !,
        Str = [34|Rest],
        get_ToQuote( Rest, Name, [] ),
        sicstus_atom_chars( File, Name ),
        parser( File, Source ),
        checkType( Source, Rules ),
        deftrees(Rules).

command_debug( Input ) :-
        ( sappend( `:debug `, Expression, Input )
         ;sappend( `:d `,     Expression, Input ) ),!,
        ( pass,
          command_solve( Expression ),
          nopass
         ;nopass).

command_deftree( Input ) :-
        ( sappend( `:deftree `, Function, Input )
         ;sappend( `:e `,       Function, Input ) ),!,
        ( sappend( Name, [32], Function )
         ;Name=Function ),
        name(F,Name),
        (  dt(F,_,DT),!, format(`~a:\n`,[F]), write2(DT), nl
         ; (\+(dt(F,_,_ /*DT*/ )), format(`There is no definitional tree\n`,[]) )).

command_type( Input ) :-
        ( sappend( `:type `, ExpS, Input )
         ;sappend( `:t `,    ExpS, Input ) ),!,
        ( exprIni(Exp,ExpS,[])
        ; functionInfixID(F,_,_,ExpS,[]), Exp=f(F,2,[]) ),
        sCheckExpr( Exp, TypeExpr, [], Susts ),
        applySusts(Susts,TypeExpr,Type), simpType(Type,ShowType),
        write2(ShowType),nl.

command_help( Input ) :- 
        ( Input = `:help `
         ;Input = `:h `   ),!,
        nl,
        write('Commands:'),nl,
        write('   :load `<File>`      - Load Curry file in interpreter [:l]'),nl,
        write('   <Expression>        - Evaluate expression'),nl,
        write('   :debug <Expression> - Debug expression evaluation    [:d]'),nl,
        write('   :deftree <Function> - Show function deftree          [:e]'),nl,
        write('   :type <Expression>  - Show expression type           [:t]'),nl,
        write('   :cd `<Directory>`   - Change working directory       [:c]'),nl,
        write('   :pwd                - Show working directory         [:p]'),nl,
        write('   :help               - This help menu                 [:h]'),nl,
        write('   :quit               - Exit interpreter               [:q]'),nl,
        nl,
        write(' :? refers to the short form of each command (only first character)'),
        nl,
        nl.

command_cd( Input ) :-
        ( sappend( `:cd `, Str, Input )
         ;sappend( `:c `,  Str, Input ) ), !,
        Str = [34|Rest],
        get_ToQuote( Rest, Name, [] ),
        sicstus_atom_chars( Dir, Name ),
        working_directory(_,Dir).

command_pwd( Input ) :-
        ( Input = `:pwd `
         ;Input = `:p `   ),!,
        working_directory(Dir,Dir),
        write(Dir),nl.

command_exit( Input ) :-
        ( Input = `:quit `
         ;Input = `:q `   ),!,
        assert(haltSystem).

