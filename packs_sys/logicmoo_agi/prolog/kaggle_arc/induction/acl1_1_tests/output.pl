/* output.pl : output commands for acl1_1.pl */
title(File,Stream):-
        max_spec_steps(Spec),
        der_depth(Der),
        beamsize(B),
        ver(V),
        verbosity(Ver),
        verapp(Vapp),
	min_cov(MC),
        format(Stream,"~N ~N/*~NACL1 ver ~a AbdProofProc. ver ~a~NFile name: ~a~N",
                [V,Vapp,File]),
        format(Stream,"Max spec steps=~w, Beamsize=~w,~NDerivation depth=~w, Verbosity=~w
        , Minimum coverage=~w
        ~N*/~N",
                        [Spec,B,Der,Ver,MC]).

print_number_neg(Eminus):-
        length(Eminus,L),
        verbosity(V),
        V>0,
        format("Negative examples: ~w~N",[L]).

print_list([],_Stream):-!.

print_list([[Cl,Np,Npa,Nm,Nma,Epc,Emc,NewDelta]|T],Stream):-
        print_list(T,Stream),
        format(Stream," ~N",[]),
        print_clause(Stream,Cl,Np,Npa,Nm,Nma,Epc,Emc,NewDelta).

print_clause(Stream,rule(H,B),_Np,_Npa,_Nm,_Nma,
                Epc,Emc,NewDelta):-
        list2and(B,Ba),
        writevars(Stream,(H:-Ba)),write(Stream,'.'),nl(Stream),
        format(Stream,"/*Covered positive examples: ~p~N\c
  Covered negative examples: ~p~N\c
  Abduced literals: ~p*/~N~N",
                [Epc,Emc,NewDelta]).


print_new_clause(rule(H,B),C,Np,Npa,Nm,Nma,
                Epluscovered,Eminuscovered,NewDelta):-
        verbosity(V),
        V>0,
        list2and(B,Ba),
        format(" ~N ~NGenerated clause: ",[]),
        writevars(user_output,(H:-Ba)),write('.'),nl,
        format("correct: ~a, Np=~w, Npa=~w, Nm=~w, Nma=~w\c
                ~NPos ex cov: ~p~NNeg ex cov: ~p~NAbduced literals: ~p~N ~N",
                [C,Np,Npa,Nm,Nma,
                Epluscovered,Eminuscovered,NewDelta]),
        (V>3->
                get0(_)
        ;
                true
        ).


print_ex_rem(Eplus,Eminus):-
        verbosity(V),
        V>0,
        length(Eplus,Lp),
        format("Positive examples remaining: ~w~N~p~N~N",[Lp,Eplus]),
        length(Eminus,Lm),
        format("Negative examples remaining: ~w~N~p~N~N",[Lm,Eminus]).


print_agenda([]):-!,
        verbosity(V),
        (V>4->
                get0(_)
        ;
                true
        ).


print_agenda([[rule(H,B1),Value,C,Np1,Npa,Nm1,Nma,Kp1,Km1,_Var]|T]):-
        print_refinement([rule(H,B1),Value,C,Np1,Npa,Nm1,Nma,Kp1,Km1]),%
        print_agenda(T).

print_max_spec_steps(Nmax):-
        verbosity(V),
        V>1,
        format("Reached the max number ~w of specializing steps~N",[Nmax]).

print_refinements:-
        verbosity(V),
        V>1,
        format(" ~NRefinements added to agenda: ~N",[]).

print_spec_step(N):-
        verbosity(V),
        V>1,
        format(" ~NSpecializing step n.~w.",[N]).

print_current_agenda(Agenda):-
        verbosity(V),
        V>2,
        format(" ~NCurrent Agenda:~N",[]),
        print_agenda(Agenda).

print_refinement([rule(H,B1),Value,C,Np1,Npa,Nm1,Nma,Kp1,Km1]):-
        verbosity(V),
        V>1,
        format("[",[]),
        list2and(B1,Ba),
        writevars(user_output,((H:-Ba))),
        format(",~3f,~a,~w,~w,~w,~w,~3f,~3f]~N",
                        [Value,C,Np1,Npa,Nm1,Nma,Kp1,Km1]).

writevars(Stream,X) :-
  freshcopy(X,Y),
  numbervars(Y,0,_L),
  write(Stream,Y).

% freshcopy(+X,-Y)
% get a copy of term X with fresh variables
freshcopy(X,Y) :-
  assert(newcopy(X)),
  retract(newcopy(Y)),
  !.
