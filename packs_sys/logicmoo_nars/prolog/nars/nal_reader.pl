% nal_reader.pl
% Read Non_Axiomatic Logic from Prolog
:-module(nal_reader,[
            nal_tests/0,
            nars_tests/0,
            nal_test_files/0,
            nal_test/1,
            nal_read_clause/2,
           % nal_test/2,
          %  nal_call/2,
            nal_call/3                   
         ]).

:- set_module(class(library)).
:- set_module(base(system)).

:- use_module(library(logicmoo_common)).

:- use_module(library(logicmoo/dcg_meta)).
:- use_module(library(logicmoo/dcg_must)).
:- use_module(library(narsese)).



/*
                task ::= [budget] sentence                       (* task to be processed *)

         sentence ::= statement"." [tense] [truth]            (* judgement to be absorbed into beliefs *)
                    | statement"?" [tense] [truth]            (* question on thuth-value to be answered *)
                    | statement"!" [desire]                   (* goal to be realized by operations *)
                    | statement"@" [desire]                   (* question on desire-value to be answered *)

        statement ::= <"<">term copula term<">">              (* two terms related to each other *)
                    | <"(">term copula term<")">              (* two terms related to each other, new notation *)
                    | term                                    (* a term can name a statement *)
                    | "(^"word {","term} ")"                  (* an operation to be executed *)
                    | word"("term {","term} ")"               (* an operation to be executed, new notation *)

           copula ::= "-->"                                   (* inheritance *)
                    | "<->"                                   (* similarity *)
                    | "{--"                                   (* instance *)
                    | "--]"                                   (* property *)
                    | "{-]"                                   (* instance-property *)
                    | "==>"                                   (* implication *)
                    | "=/>"                                   (* predictive implication *)
                    | "=|>"                                   (* concurrent implication *)
                    | "=\\>"                                  (* =\> retrospective implication *)
                    | "<=>"                                   (* equivalence *)
                    | "</>"                                   (* predictive equivalence *)
                    | "<|>"                                   (* concurrent equivalence *)

             term ::= word                                    (* an atomic constant term *)
                    | variable                                (* an atomic variable term *)
                    | compound-term                           (* a term with internal structure *)
                    | statement                               (* a statement can serve as a term *)

    compound-term ::= op-ext-set term {"," term} "}"          (* extensional set *)
                    | op-int-set term {"," term} "]"          (* intensional set *)
                    | "("op-multi"," term {"," term} ")"      (* with prefix operator *)
                    | "("op-single"," term "," term ")"       (* with prefix operator *)
                    | "(" term {op-multi term} ")"            (* with infix operator *)
                    | "(" term op-single term ")"             (* with infix operator *)
                    | "(" term {","term} ")"                  (* product, new notation *)
                    | "(" op-ext-image "," term {"," term} ")"(* special case, extensional image *)
                    | "(" op-int-image "," term {"," term} ")"(* special case, \ intensional image *)
                    | "(" op-negation "," term ")"            (* negation *)
                    | op-negation term                        (* negation, new notation *)

        op-int-set::= "["                                     (* intensional set *)
        op-ext-set::= "{"                                     (* extensional set *)
       op-negation::= "--"                                    (* negation *)
      op-int-image::= "\\"                                    (* \ intensional image *)
      op-ext-image::= "/"                                     (* extensional image *)
         op-multi ::= "&&"                                    (* conjunction *)
                    | "*"                                     (* product *)
                    | "||"                                    (* disjunction *)
                    | "&|"                                    (* parallel events *)
                    | "&/"                                    (* sequential events *)
                    | "|"                                     (* intensional intersection *)
                    | "&"                                     (* extensional intersection *)
        op-single ::= "-"                                     (* extensional difference *)
                    | "~"                                     (* intensional difference *)

         variable ::= "$"word                                 (* independent variable *)
                    | "#"word                                 (* dependent variable *)
                    | "?"word                                 (* query variable in question *)

            tense ::= ":/:"                                   (* future event *)
                    | ":|:"                                   (* present event *)
                    | ":\\:"                                  (* :\: past event *)
          
           desire ::= truth                                   (* same format, different interpretations *)
            truth ::= <"%">frequency[<";">confidence]<"%">    (* two numbers in [0,1]x(0,1) *)
           budget ::= <"$">priority[<";">durability][<";">quality]<"$"> (* three numbers in [0,1]x(0,1)x[0,1] *)

               word : #"[^\ ]+"                               (* unicode string *)    
           priority : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
         durability : #"[0]?\.[0]*[1-9]{1}[0-9]*"             (* 0 <  x <  1 *)
            quality : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
          frequency : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
         confidence : #"[0]?\.[0]*[1-9]{1}[0-9]*"             (* 0 <  x <  1 *)
         occurrenceTime: 
*/



nal_task(S)--> chspace,!,nal_task(S),!.
nal_task(OUT)--> nal_task_0(TASK),
   (nal_three_vals(V3) -> {OUT = nal_in(TASK,V3)} ; {OUT = TASK}).

nal_task_0(nal_task(X,S,T,O,B)) --> optional(B, nal_budget),!, nal_sentence(X,S,T,O).  % task to be processed 


nal_sentence(X,S,T,O)--> nal_statement(S), nal_post_statement(X,T,O).

nal_post_statement(X,T,O)--> 
          /*nal_statement(S),*/ nal_o(`.` ,X, judgement)-> optional(T,nal_tense)-> optional(O,nal_truth),!      % judgement to be absorbed into beliefs 
        ; /*nal_statement(S),*/ nal_o(`?` ,X, question_truth)-> optional(T,nal_tense)-> optional(O,nal_truth),! % question on truth_value to be answered 
        ; /*nal_statement(S),*/ nal_o(`!` ,X, goal)-> optional(T,nal_tense)-> optional(O,nal_desire)            % goal to be realized by operations 
        ; /*nal_statement(S),*/ nal_o(`@` ,X, question_desire)-> optional(T,nal_tense)-> optional(O,nal_desire) % question on desire_value to be answered 
        .

% nal_statement(nal_word(S))--> nal_word(S). 
nal_statement(S)--> amw(nal_statement_0(S)),!.
nal_statement_0(S)--> cwhite,nal_statement_0(S).
nal_statement_0(S)--> 
        amw(`<`) ,!, nal_term(A), nal_copula(R), nal_term(B), amw(`>`) ,   {S=..[R,A,B]}   % two terms related to each other 
      ;  nal_l_paren, `^` , nal_term_list_comma(L), nal_paren_r,       {S= exec(L)}        % an operation to be executed 
      ;  nal_l_paren, nal_term(A), nal_copula(R), nal_term(B), nal_paren_r,  {S=..[R,A,B]} % two terms related to each other, new notation 
      ;  nal_word(A), nal_l_paren, nal_term_list_comma(L), nal_paren_r,    {S= exec([A|L])}% an operation to be executed, new notation 
      ;  nal_term_1(X),             {S= nal_named_statement(X)}                       % a term can name a statement(S) 
      .
%nal_statement_0(S)-->nal_rsymbol(S).

nal_copula(X) -->      
            nal_o(`-->` ,X,                          inheritance )
         ;  nal_o(`<->` ,X,                          similarity )
         ;  nal_o(`{--` ,X,                          instance )
         ;  nal_o(`--]` ,X,                          property )
         ;  nal_o(`{-]` ,X,                          inst_prop )
         ;  nal_o(`==>` ,X,                          implication )
         ;  nal_o(`=/>` ,X,                          predictive_impl )
         ;  nal_o(`=|>` ,X,                          concurrent_impl )
         ;  nal_o(`=\\>` ,X,                         retrospective_impl )
         ;  nal_o(`<=>` ,X,                          equiv )
         ;  nal_o(`</>` ,X,                          predictive_equiv )
         ;  nal_o(`<|>` ,X,                          concurrent_equiv )
         ;  nal_o(`=>` ,X,                           unknown_implication )% dmiles added
         ;  nal_o(`|-` ,X,                           prolog_implication ) % dmiles added
         .

nal_term(N) --> nal_term_old(O), {old_to_new(O,N)}.
nal_term_old(S)
       --> nal_word(S)                         % an atomic constant, term,         
        ;  nal_variable(S)                     % an atomic variable, term, 
        ;  nal_compound_term(S)                % a term with internal structure 
        ;  nal_statement(S)                    % a statement can serve as a term, 
        .


nal_term_0(N) --> nal_term_0_old(O), {old_to_new(O,N)}.
nal_term_0_old(S) 
        -->  nal_word_0(S)                       % an atomic constant, term,         
        ;  nal_variable_0(S)                     % an atomic variable, term, 
        ;  nal_compound_term_0(S)                % a, term, with internal structure 
        ;  nal_statement_0(S)                    % a statement can serve as a, term, 
        .

nal_term_1(N) --> nal_term_1_old(O), {old_to_new(O,N)},!.
nal_term_1_old(S)
        --> nal_word(S)                        % an atomic constant, term,         
        ;  nal_variable(S)                     % an atomic variable, term, 
        ;  nal_compound_term(S)                % a, term, with internal structure 
        .

old_to_new(rel([R, var(arg, L) | B]), ext_image(New)):- length(Left,L), append(Left,Right,[R|B]), append(Left,['_'|Right],New),!.
old_to_new(rel([R, var(int, L) | B]), int_image(New)):- length(Left,L), append(Left,Right,[R|B]), append(Left,['_'|Right],New),!.
old_to_new(X,X).
nal_compound_term(X)--> mw(nal_compound_term_0(X)),!.

nal_compound_term_0('exec'([S]))--> `^`,!,nal_term_1(S),!.
nal_compound_term_0(S)--> \+ dcg_peek(`<`),!,nal_compound_term_1(S),!.
nal_compound_term_1(S)-->
   (  nal_o(nal_op_ext_set,X,ext_set), nal_term_list_comma(L), `}`                % extensional set 
   ;  nal_o(nal_op_int_set,X,int_set), nal_term_list_comma(L), `]`                % intensional set 

   ;  nal_word_0(A), `[`, nal_term_list_comma(L), `]`,  {S= v(A,L)}            % @TODO notation 
   ;  nal_o(nal_op_negation,X,negation), nal_term(AB),{L=[AB]}              % negation, new notation 
   ;   nal_l_paren, nal_paren_compound_term(X,L), nal_paren_r
   ), {S=..[X,L]}.

nal_paren_compound_term(X,L) --> 
      nal_op_multi(X), nal_comma, nal_term_list_comma(L)                        % with prefix operator 
   ;  nal_op_single(X), nal_comma, nal_term(A), nal_comma, nal_term(B), {L=[A,B]} % with prefix operator 
   ;  nal_o(nal_op_ext_image,X,ext_image), nal_comma, nal_term_list_comma(L)        % special case, extensional image 
   ;  nal_o(nal_op_int_image,X,int_image), nal_comma, nal_term_list_comma(L)        % special case, \ intensional image 
   ;  nal_o(nal_op_negation,X,negation), nal_comma, nal_term(AB),{L=[AB]}     % negation 
   ;  nal_term(A), nal_op_multi(X), nal_term(B),{L=[A,B]}                 % with infix operator 
   ;  nal_term(A), nal_op_single(X), nal_term(B),{L=[A,B]}                % with infix operator 
   ;  nal_preserve_whitespace((nal_term_0(A), chspace,  {X=rel}, nal_term_list_white(SL, ` `))),{L=[A|SL]}
   ;  {X=product}, nal_term_list_comma(L)                               % product, new notation 
   .

nal_op_int_set-->`[`.                          % intensional set 
nal_op_ext_set-->`{`.                          % extensional set 
nal_op_negation-->`--`.                        % negation 
nal_op_int_image-->`\\`.                       % \ intensional image 
nal_op_ext_image-->`/`.                        % / extensional image 


nal_preserve_whitespace(DCG,S,E) :- locally(b_setval(whitespace,preserve),phrase(DCG,S,E)).
% nal_no_preserve_whitespace(DCG,S,E) :- phrase(DCG,S,E).


nal_op_multi(X)-->   
        nal_o(`&&` ,X, and)                          % conjunction 
      ; nal_o(`*` ,X, product)                       % product 
      ; nal_o(`||` ,X, or)                           % disjunction 
      ; nal_o(`#` ,X, sequence_spatial)              % patham9 "sequence", wasn't really useful for NLP, it was called PART
      ; nal_o(`&|` ,X, parallel_evnts)               % parallel events 
      ; nal_o(`&/` ,X, sequence_evnts)               % sequential events 
      ; nal_o(`|` ,X, int_intersection)              % intensional intersection 
      ; nal_o(`&` ,X, ext_intersection)              % extensional intersection 
      .
nal_op_single(X) --> 
          nal_o(`-`, X, ext_difference)              % extensional difference 
       ;  nal_o(`~`, X, int_difference)              % intensional difference 
       .

nal_variable(VA)--> nal_variable_0(V),!,maybe_plus_array2(V,VA).

nal_variable_0(var(X,W))
    -->nal_o(`$`, X, ind), nal_word_0(W)      % independent variable 
      ;nal_o(`#`, X, dep), nal_word_0(W)      % dependent variable 
      ;nal_o(`?`, X, query), nal_word_0(W)    % query variable in question 
      ;nal_o(`/`, X, arg), nal_word_0(W)      % query variable in params 
      ;nal_o(`\\`,X, int), nal_word_0(W)      % query variable in .... 
      .

nal_variable_0(('_')) --> `_`.
nal_variable_0(('#')) --> `#`.
nal_variable_0(('$')) --> `$`.


nal_tense(X) -->
      nal_o(`:/:`, X, future)                        % future event 
   ;  nal_o(`:|:`, X, present)                       % present event 
   ;  nal_o(`:\\:`, X, past)                         % :\: past event 
   .

nal_tense('t!'(X)) --> `:!`, number(X), `:`.
nal_tense('t'(X)) --> `:`, nal_term_1(X), `:`.

% Desire is same format of Truth, but different interpretations 
nal_desire(D)-->nal_truth(D).									
% Truth is two numbers in [0,1]x(0,1) 
nal_truth([F,C])--> `%`, !, nal_frequency(F), optional((`;`, nal_confidence(C))), optional(`%`),!.	                
nal_truth([F,C])--> `{`, dcg_basics:number(F),!,{is_float_0_1(F)}, chspace, dcg_basics:number(C),{is_float_0_1(F)},`}`,!.
nal_truth([F,C])--> `Truth:`,read_nal_expected_truth(F,C).
% Budget is three numbers in optional(O,0,1]x(0,1)x[0,1] 
nal_budget(nal_budget_pdq(P,D,Q))--> `$`,!, nal_priority(P), optional(( `;`, nal_durability(D))), optional((`;`, nal_quality(Q))), `$`.  

is_float_0_1(F):- 0.0 =< F, F =< 1.0.
nal_word(E) --> amw(nal_word_0(E)).

nal_word_0('+'(E)) --> `+`,dcg_basics:integer(E),!.
nal_word_0(E) --> dcg_basics:number(E),!.
nal_word_0(E) --> quoted_string(E),!.
nal_word_0(E) --> nal_word_str(E).

% nal_rsymbol(Chars,E) --> [C], {notrace(nal_sym_char(C))},!, nal_sym_continue(S), {append(Chars,[C|S],AChars),string_to_atom(AChars,E)},!.

nal_word_str(MaybeArray) --> dcg_peek([C]),{char_type(C,alpha)},!, nal_rsymbol(E),!,maybe_plus_array(E,MaybeArray).

maybe_plus_array2(E,var_holds(E,[])) --> mw(`(`),mw(`)`),!.
maybe_plus_array2(E,var_holds(E,Ar)) --> `(`,owhite,nal_term_list_comma(Ar),owhite,`)`.
maybe_plus_array2(E,E)-->[].

maybe_plus_array(E,E) --> \+ dcg_peek(`[`),!.
maybe_plus_array(E,idxOf(E,Ar)) --> `[`,owhite,nal_term_list_comma(Ar),owhite,`]`.


  nal_priority(F) --> nal_float_inclusive(0,1,F).           %  0 <= x <= 1       
nal_durability(F) --> nal_float_exclusive(0,1,F).           %  0 <  x <  1 
   nal_quality(F) --> nal_float_inclusive(0,1,F).           %  0 <= x <= 1 
 nal_frequency(F) --> nal_float_inclusive(0,1,F).           %  0 <= x <= 1 
nal_confidence(F) --> nal_float_exclusive(0,1,F).           %  0 <  x <  1 

nal_o(S,X,X) --> owhite,S,owhite.
nal_o(X,X) --> nal_o(X,X,X).

nal_float_inclusive(L,H,F)--> amw((dcg_basics:number(F) -> {nal_warn_if_strict((L=< F,F=< H))})).
nal_float_exclusive(L,H,F)--> amw((dcg_basics:number(F) -> {nal_warn_if_strict((L < F,F < H))})).

nal_warn_if_strict(G):- call(G),!.
nal_warn_if_strict(G):- nal_dmsg(nal_warn_if_strict(G)),!.

:- set_dcg_meta_reader_options(file_comment_reader, nal_comment_expr_unused).

nal_comment_expr_unused(_) --> {!,fail}.

nal_comment_expr(X) --> chspace,!,nal_comment_expr(X).
nal_comment_expr('$COMMENT'(Expr,I,CP)) --> nal_comment_expr_3(Expr,I,CP),!.

nal_comment_expr_3(T,N,CharPOS) --> `/*`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`*/`)),!,
  {text_to_string_safe(S,T)},!.
nal_comment_expr_3(T,N,CharPOS) -->  {nal_cmt_until_eoln(Text)},dcg_peek(Text),!, 
  my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
 {text_to_string_safe(S,T)},!.


nal_cmt_until_eoln((`//`, \+ dcg_peek(`expected:`))).
nal_cmt_until_eoln(((`'`, \+ dcg_peek(`'outputMustContain`), dcg_peek(\+ `' Answer `)))).
nal_cmt_until_eoln((`**`)).


nal_comma --> amw(`,`).
nal_l_paren --> amw(`(`).
nal_paren_r --> amw(`)`).

nal_term_list_white([H|T], Sep) --> nal_term_0(H), ( (Sep,owhite) ->  nal_term_list_white(T, Sep) ; ({T=[]},owhite)).
nal_term_list_comma([H|T]) --> nal_term(H), ( nal_comma ->  nal_term_list_comma(T) ; {T=[]} ).

builtin_symbol('_').
builtin_symbol('--').
builtin_symbol('~').
builtin_symbol('*').
builtin_symbol(key_101).


nal_rsymbol(S)--> {builtin_symbol(S),name(S,Str)},Str,!.
nal_rsymbol(E)--> nal_rsymbol([],E).
nal_rsymbol(Chars,E) --> [C], {notrace(nal_sym_char(C))},!, nal_sym_continue(S), {append(Chars,[C|S],AChars),string_to_atom(AChars,E)},!.
nal_sym_continue([]) --> nal_peek_symbol_breaker,!.
nal_sym_continue([H|T]) --> [H], {nal_sym_char(H)},!, nal_sym_continue(T).
nal_sym_continue([]) --> [].

nal_peek_symbol_breaker --> dcg_peek(`--`).
nal_peek_symbol_breaker --> dcg_peek(`-`),!,{fail}.
nal_peek_symbol_breaker --> dcg_peek(one_blank).
nal_peek_symbol_breaker --> dcg_peek([C]),{\+ nal_sym_char(C)},!.

nal_sym_char(C):- \+ integer(C),!,char_code(C,D),!,nal_sym_char(D).
nal_sym_char(C):- [C]=`_`,!.
nal_sym_char(C):- bx(C =<  32),!,fail.
%nal_sym_char(44). % allow comma in middle of symbol
% word is: #"[^\ ]+"   %  unicode string     
nal_sym_char(C):- nal_never_symbol_char(NeverSymbolList),memberchk(C,NeverSymbolList),!,fail.  % maybe 44 ? nal_comma
%nal_sym_char(C):- nb_current('$maybe_string',t),memberchk(C,`,.:;!%`),!,fail.
nal_sym_char(_):- !.

nal_never_symbol_char(`";()~'[]!<>``{},=.\\^`).


nal_rsymbol_cont(Prepend,E) --> nal_sym_continue(S), {append(Prepend,S,AChars),string_to_atom(AChars,E)},!.


nal_is_test_file(X):-filematch(library('../nal-tests/**/*'),X), \+ nal_non_file(X). 
nal_is_test_file(X):-filematch(library('../examples/**/*'),X),atom_contains(X,nars), \+ nal_non_file(X). 
nal_non_file(X):- downcase_atom(X,DC),X\==DC,!,nal_non_file(DC).
nal_non_file(X):- atom_contains(readme,X).
nal_non_file(X):- exists_directory(X).
nal_non_file(X):- atom_concat(_,'.pl',X).

nal_test_files:- 
 make,
 catch((
   forall(nal_is_test_file(X),((nal_dmsg(test_file_begin(X)),ignore(nal_do_test_file(X)),nal_dmsg(test_file_end(X)))))),
    '$aborted',true).

nal_do_test_file(File):- (\+ atom(File); \+ is_absolute_file_name(File); \+ exists_file(File)),
  filematch(File,Absolute), !, nal_do_test_file(Absolute).
nal_do_test_file(File):- 
  setup_call_cleanup(open(File,read,In),      
      nal_do_test_stream(In),
        close(In)).

% Whole Group
nal_do_test_stream(In):- nal_read_clauses(In,Expr),!,nars_exec_ex(Expr).
% One at a time
% nal_do_test_stream(In):- repeat, nal_read_clause(In,Expr), nars_exec_ex(Expr), Expr==end_of_file.

/*
  must_or_rtrace(nal_call(nal_do_test_file,Expr,OutL)),!,
  flatten([OutL],Out),
  maplist(nal_dmsg,Out),!.
*/

nal_dmsg(O):- is_list(O),!,in_cmt(maplist(print_tree_nl,O)).
nal_dmsg(O):- format('~N'), in_cmt(print_tree_nl(O)).

amw(A)--> cspace,!,amw(A).
amw(A)--> A,more_cspace(chspace).

%chspace--> `,`,dcg_peek(chspace),!.
chspace--> cspace.
/*
chspace--> `,`,!.
chspace--> [C],{char_type(C,white)}.
*/
aspaces--> chspace, more_aspace(chspace).
more_aspace(P)--> P,!, more_aspace(P).
more_aspace(_)--> [].

more_cspace(P)--> P,!, more_cspace(P).
more_cspace(_)--> [].

% NAL file reader
nal_file(CMT) --> {retract(t_l:'$last_comment'(CMT))},!.
nal_file(end_of_file) --> file_eof,!.
nal_file(O) --> eoln, !, nal_file(O).
nal_file(O) --> chspace, !, nal_file(O).
nal_file([]) -->  \+ dcg_peek([_]), !.
%nal_file(O) --> nal_file_element(O), !, owhite.
nal_file(O) --> read_string_until_no_esc(Str,eoln), {phrase((nal_file_element(O),owhite),Str),!}.
% fallback to english in a file
nal_file(unk_english(Text)) -->  read_string_until_no_esc(Str,eoln), 
  {atom_string(Str,TextStr),{format('~N%~~ '),ansifmt([red],(TextStr)),nl},split_string(TextStr, "", "\s\t\r\n", Text)}.

% nal_file(planStepLPG(Name,Expr,Value)) --> owhite,sym_or_num(Name),`:`,owhite, nal(Expr),owhite, `[`,sym_or_num(Value),`]`,owhite.  %   0.0003:   (PICK-UP ANDY IBM-R30 CS-LOUNGE) [0.1000]
% nal_file(Term,Left,Right):- eoln(EOL),append(LLeft,[46,EOL|Right],Left),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
% nal_file(Term,Left,Right):- append(LLeft,[46|Right],Left), ( \+ member(46,Right)),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.

% non-standard
nal_file_element(O) --> chspace, !, nal_file_element(O).
nal_file_element(expected(O) ) --> `//expected:`, read_string_until(Str,(eoln)),!,{phrase(read_nal_expected(O),Str)}. 
nal_file_element(outputMustContain(O)) --> `''outputMustContain('`, amw(nal_file_element(O)),`')`,!.
%nal_file_element(outputMustContain(O)) --> `''outputMustContain('`, !, read_string_until(Str,`')`),`')`,{fmt(Str),phrase(nal_file_element(O),Str,[])}. 
nal_file_element('oneAnswer'(O) ) --> `' Answer `, read_string_until(Str,(`{`,eoln)),{phrase(nal_task(O),Str,[])}. 

nal_file_element(Comment) --> nal_comment_expr(Comment),!.

nal_file_element(N=V          ) -->  `*`, nal_word(N), amw(`=`), nal_term(V).
nal_file_element(nal_in(H,V3))  -->  `IN:`,   nal_task_0(H), optional(nal_three_vals(V3)).
nal_file_element(nal_out(H,V3)) -->  `OUT:`,  nal_task_0(H), optional(nal_three_vals(V3)).
% standard
nal_file_element(do_steps(N)) --> dcg_basics:number(N),!.
nal_file_element(H) --> nal_task(H),!.
nal_file_element(nal_term(H)) --> nal_term(H).
% nal_read_clause("'the detective claims that tim lives in graz",A)

ospace --> chspace,!,ospace.
ospace --> [].

read_nal_expected(O) --> chspace, !, read_nal_expected(O).
read_nal_expected([H|T]) --> read_nal_expected_ele(H),read_nal_expected(T).
read_nal_expected([]) --> [].
read_nal_expected_ele(O) --> chspace, !, read_nal_expected_ele(O).
% ^say executed with args ({SELF} * bedroom)
read_nal_expected_ele(executed_with_args(T,A)) --> amw(nal_term(T)),`executed with args`,amw(nal_term(A)).
read_nal_expected_ele(answer(O)) --> `Answer:`,nal_file_element(O).
read_nal_expected_ele(occurrenceTime(T)) --> `occurrenceTime=`,ospace,dcg_basics:number(T).
read_nal_expected_ele(truth(F,C)) --> `Truth:`,read_nal_expected_truth(F,C).

read_nal_expected_truth(F,C) --> chspace, !, read_nal_expected_truth(F,C).
read_nal_expected_truth(_,_) --> `.`,!.
read_nal_expected_truth(_,_) -->  \+ dcg_peek([_]), !.
read_nal_expected_truth(F,C) --> read_nal_expected_truth_ele(F,C), !, read_nal_expected_truth(F,C).
read_nal_expected_truth_ele(F,C) --> (chspace;`,`), !, read_nal_expected_truth_ele(F,C).
read_nal_expected_truth_ele(F,_) --> `frequency=`,ospace,nal_frequency(F).
read_nal_expected_truth_ele(_,C) --> `confidence=`,ospace,nal_confidence(C).

% {1 : 4;3} 
nal_three_vals(V3)--> `{`, read_string_until_no_esc(Str,(`}`;eoln)), 
  {read_term_from_codes(Str,V3,[double_quotes(string),syntax_errors(fail)])},!.


%nal_file_with_comments(O,with_text(O,Txt),S,E):- copy_until_tail(S,Copy),text_to_string_safe(Copy,Txt),!.


:- thread_local(t_l:sreader_options/2).

nars_tests:-
  nal_tests,
  nal_test_files.


nal_tests:- make, fmt('\nNAL TEST'), nal_reader:forall(nal_is_test(_,Test),nal_test(Test)).


:- use_module(library(dcg/basics)).

% try_reader_test(Test):- is_stream(Test), !, \+ is_compound(Test), open_string(Test,Stream), try_reader_test(Stream).
nal_test(Test):- 
  in_cmt((fmt("\n-----------------------------\n"),
          fmt(Test), 
          fmt("-----------------------------\n"))),
  nal_call('nal_dmsg',Test,Out),nal_dmsg(Out).


nal_zave_varname(N,V):- debug_var(N,V),!.
%nal_zave_varname(N,V):- V = '$VAR'(N).

/*
implode_varnames(Vs):- (var(Vs) ; Vs==[]),!.
implode_varnames([NV|Vs]) :- implode_varnames(Vs),
  (var(NV) -> ignore((nal_variable_name(NV,Name),nal_zave_varname(Name,NV))); 
  ignore((NV=(N=V),nal_zave_varname(N,V)))).
*/

nal_read_clauses( Text, Out):-
 findall(Cl,nal_read_clause(Text, Cl), OutL),
 flatten([OutL],Out).

nal_read_clause( NonStream, Out):- \+ is_stream(NonStream), !, % nal_dmsg(NonStream),
  must_or_rtrace((open_string(NonStream,Stream), nal_read_clause(Stream, Out))).

nal_read_clause(Stream, Out):-
 '$current_typein_module'(M),
  M\== input, !,
  setup_call_cleanup(
   '$set_typein_module'(input),
   nal_read_clause(Stream, Out),
   '$set_typein_module'(M)).

nal_read_clause(Stream, Out):-
 op(601, xfx, input:(/)),
 op(601, xfx, input:(\\)),
 (at_end_of_stream(Stream)-> Out=[]; 
   (retractall(t_l:'$last_comment'(_)),
     nal_read_term(Stream, Term),
    (Term == end_of_file -> Out=[];
      (Term = (:- Exec) -> (input:call(Exec), Out=More) ; Out = [Term|More]),
       nal_read_clause(Stream, More)))).

nal_read_term(In,Expr):- 
 notrace(( is_stream(In), 
  remove_pending_buffer_codes(In,Codes), 
  read_codes_from_pending_input(In,Text), Text\==[])), !,
  call_cleanup(parse_meta_ascii_nal(nal_file, Text,Expr),
    append_buffer_codes(In,Codes)).
nal_read_term(Text,Expr):- 
 notrace(( =( ascii_,In),
  remove_pending_buffer_codes(In,Codes))),   
  call_cleanup(parse_meta_ascii_nal(nal_file, Text,Expr),
    append_buffer_codes(In,Codes)).

parse_meta_ascii_nal(nal_file, Text,Expr):-   
  parse_meta_ascii(nal_file, Text,Expr),
  retractall(t_l:'$last_comment'(_)).

nal_reader:{X}:- call(X).

% Expand Stream or String
nal_call(Ctx, Stream, Out):- \+ compound(Stream),
  must_or_rtrace(nal_read_clauses(Stream, List)), !,
  nal_call(Ctx, List, Out).

nal_call(Ctx, List, Out):- is_list(List),!, maplist(nal_call(Ctx),List, OutL),flatten(OutL,Out).
nal_call(Ctx, InnerCtx=json(List), Out):- !,  nal_call([InnerCtx|Ctx], List, Out).

nal_call(Ctx, List, Out):- 
   sub_term(Sub, List), nonvar(Sub), 
   nal_rule_rewrite(Ctx, Sub, NewSub),
   % ignore((NewSub=='$',nal_dmsg(nal_rule_rewrite(_Ctx, Sub, NewSub)))),
   nonvar(NewSub), Sub\==NewSub,
   subst(List, Sub, NewSub, NewList), 
   List\==NewList, !, 
   nal_call(Ctx, NewList, Out).

nal_call(_Ctx, List, Out):- flatten([List], Out),!.




nal_rule_rewrite(_Ctx, json(Replace), Replace):- nonvar(Replace),!.


nal_join_atomics(Sep,List,Joined):- atomics_to_string(List,Sep,Joined).

/*
nal_into_tokenized(Text,TokenizedText):- \+ string(Text),!, 
  any_to_string(Text,String), nal_into_tokenized(String,TokenizedText).
nal_into_tokenized(Text,TokenizedText):-
 split_string(Text, "", "\s\t\r\n", [L]), L\==Text,!,
 nal_into_tokenized(L,M), 
 %string_concat(M,"\n",TokenizedText).
 string_concat(M,"",TokenizedText).
nal_into_tokenized(Text,TokenizedText):-   L=[_S1,_S2|_SS],    
  member(Split,["\n'","'\n","<META>'","<META>","\n"]),  
  atomic_list_concat(L,Split,Text),  
  maplist(nal_into_tokenized,L,LO),
  atomics_to_string(LO,Split, TokenizedText).
nal_into_tokenized(Text,TokenizedText):-   
  split_string(Text, "\n", "\s\t\n\r",StringList),
  maplist(into_text80_atoms,StringList,SentenceList),
  maplist(nal_join_atomics(' '),SentenceList,ListOfStrings),
  nal_join_atomics('\n',ListOfStrings,TokenizedText),!.
*/

:- fixup_exports.

nal_is_test(read, "'the detective claims that tim lives in graz").
nal_is_test(read, "<{tim} --> (/,livingIn,_,{graz})>.").
nal_is_test(read, "<bird --> swimmer>. %0.87;0.91%").
nal_is_test(read, "''outputMustContain('<bird --> swimmer>. %0.87;0.91%')").
nal_is_test(read, "1").

nal_is_test(read, "$1").
nal_is_test(read, "#1").
nal_is_test(read, "?1").
nal_is_test(read, "/1").
nal_is_test(read, "\\1").
% to distinguish 
%         "eaten by tiger" vs. "eating tiger"  
% before:  (/,eat,tiger,_) vs. (/,eat,_,tiger)
% now:      (eat /2 tiger) vs. (eat /1 tiger)
nal_is_test(read, "'eating tiger").
nal_is_test(read, "(eat /1 tiger)").
nal_is_test(read, "(/,eat,_,tiger)").
nal_is_test(read, "'eaten by tiger").
nal_is_test(read, "(eat /2 tiger)").
nal_is_test(read, "(/,eat,tiger,_)").
nal_is_test(read, "'intensional eating").
nal_is_test(read, "(eat \\1 tiger)").
nal_is_test(read, "(\\,eat,_,tiger)").
nal_is_test(read, "(eat \\2 tiger)").
nal_is_test(read, "(\\,eat,tiger,_)").


% 
nal_is_test(exec, "'Revision ------

'Bird is a type of swimmer.
<bird --> swimmer>.

'Bird is probably not a type of swimmer.
<bird --> swimmer>. %0.10;0.60%

1

'Bird is very likely to be a type of swimmer.
''outputMustContain('<bird --> swimmer>. %0.87;0.91%')").

nal_is_test(exec, "
********** revision
  IN: <bird --> swimmer>. %1.00;0.90% {0 : 1} 
  IN: <bird --> swimmer>. %0.10;0.60% {0 : 2} 
1
 OUT: <bird --> swimmer>. %0.87;0.91% {1 : 1;2} 
").


nal_is_test(exec, "
********** abduction
  IN: <sport --> competition>. %1.00;0.90% {0 : 1} 
  IN: <chess --> competition>. %0.90;0.90% {0 : 2} 
1
 OUT: <sport --> chess>. %1.00;0.42% {1 : 2;1} 
 OUT: <chess --> sport>. %0.90;0.45% {1 : 2;1} 
 OUT: <chess <-> sport>. %0.90;0.45% {1 : 2;1} 
 OUT: <(&,chess,sport) --> competition>. %1.00;0.81% {1 : 2;1} 
 OUT: <(|,chess,sport) --> competition>. %0.90;0.81% {1 : 2;1} 
 OUT: <<sport --> $1> ==> <chess --> $1>>. %0.90;0.45% {1 : 2;1} 
 OUT: <<chess --> $1> ==> <sport --> $1>>. %1.00;0.42% {1 : 2;1} 
 OUT: <<chess --> $1> <=> <sport --> $1>>. %0.90;0.45% {1 : 2;1} 
 OUT: (&&,<chess --> #1>,<sport --> #1>). %0.90;0.81% {1 : 2;1} 
").

nal_is_test(exec, "
********* induction
  IN: <swan --> swimmer>. %0.90;0.90% {0 : 1} 
  IN: <swan --> bird>. %1.00;0.90% {0 : 2} 
1
 OUT: <bird --> swimmer>. %0.90;0.45% {1 : 2;1} 
 OUT: <swimmer --> bird>. %1.00;0.42% {1 : 2;1} 
 OUT: <bird <-> swimmer>. %0.90;0.45% {1 : 2;1} 
 OUT: <swan --> (|,bird,swimmer)>. %1.00;0.81% {1 : 2;1} 
 OUT: <swan --> (&,bird,swimmer)>. %0.90;0.81% {1 : 2;1} 
 OUT: <<$1 --> swimmer> ==> <$1 --> bird>>. %1.00;0.42% {1 : 2;1} 
 OUT: <<$1 --> bird> ==> <$1 --> swimmer>>. %0.90;0.45% {1 : 2;1} 
 OUT: <<$1 --> bird> <=> <$1 --> swimmer>>. %0.90;0.45% {1 : 2;1} 
 OUT: (&&,<#1 --> bird>,<#1 --> swimmer>). %0.90;0.81% {1 : 2;1} 
").

nal_is_test(exec, "
********** exemplification
  IN: <robin --> bird>. %1.00;0.90% {0 : 1} 
  IN: <bird --> animal>. %1.00;0.90% {0 : 2} 
1
 OUT: <robin --> animal>. %1.00;0.81% {1 : 2;1} 
 OUT: <animal --> robin>. %1.00;0.45% {1 : 2;1} 
").

nal_is_test(exec, "
********** conversion
  IN: <bird --> swimmer>. %1.00;0.90% {0 : 1} 
  IN: <swimmer --> bird>?  {0 : 2} 
2
 OUT: <swimmer --> bird>. %1.00;0.47% {2 : 1} 
").

nal_is_test(exec, "
********** y/n question
  IN: <bird --> swimmer>. %1.00;0.90% {0 : 1} 
  IN: <bird --> swimmer>?  {0 : 2} 
1
 OUT: <bird --> swimmer>. %1.00;0.90% {0 : 1} 
").

nal_is_test(exec, "
********** wh-question
  IN: <bird --> swimmer>. %1.00;0.80% {0 : 1} 
  IN: <?1 --> swimmer>?  {0 : 2} 
1
 OUT: <bird --> swimmer>. %1.00;0.80% {0 : 1} 
").

nal_is_test(exec, "

'the detective claims that tim lives in graz
'<{tim} --> (/,livingIn,_,{graz})>.
'and lawyer claims that this is not the case
<{tim} --> (/,livingIn,_,{graz})>. %0%
100
'the first deponent, a psychologist,
'claims that people with sunglasses are more aggressive
<<(*,$1,sunglasses) --> own> ==> <$1 --> [aggressive]>>.
'the third deponent claims, that he has seen tom with sunglasses on:
<(*,{tom},sunglasses) --> own>.
'the teacher claims, that people who are aggressive tend to be murders
<<$1 --> [aggressive]> ==> <$1 --> murder>>.
'the second deponent claims, that if the person lives in Graz, he is surely the murder
<<$1 --> (/,livingIn,_,{graz})> ==> <$1 --> murder>>.
'who is the murder?
<{?who} --> murder>?
''outputMustContain('<{tom} --> murder>. %1.00;0.73%')

").

nal_is_test(read, "
' Custom truth values These are added by appending {0.0 0.9} instead of %0.0;0.9% as we believe this increased the readability.

' Example

********** wh-question
  IN: <bird --> swimmer>. %1.00;0.80% {0 : 1} 
  IN: <?1 --> swimmer>?  {0 : 2} 
1
 OUT: <bird --> swimmer>. %1.00;0.80% {0 : 1} 

' can now be


********** wh-question
  IN: <bird --> swimmer>. {1.0 0.80} {0 : 1} 
  IN: <?1 --> swimmer>?  {0 : 2} 
1
 OUT: <bird --> swimmer>. {1.0 0.80} {0 : 1} 

").

nal_is_test(read, "
'Images

(/,rel,_,b) 

' has to be written as

(rel /1 b)

' and image as

(/,rel,a,_) 

' as

(rel /2 a)

' same for \\ with \\1 and \\2.

").

nal_is_test(read, "
'Intervals, to measure expected time distances between events, are always learned by ONA and stored as meta-data, they are not part of the Narsese I/O format anymore. Hence a sequence

(&/,a,+5,b)

' becomes

(&/,a,b)

' or

(a &/ b)

' and also the interval for implications is not used anymore.

").

nal_is_test(read, "
'Operators The syntactic sugar

(^op,arg_1,arg_2,arg_3,arg_n)

' is not supported anymore, instead the full notation has to be used which is supported by both systems:

<(*,arg_1,arg_2,arg_3,arg_n) --> ^op>

' though for operations without arguments, the following shortcut can be used:

^op

").

nal_is_test(read, "
'Restrictions

'1. Copulas in ONA are binary, since it's using an array-based heap for terms. 
' While there are ways to encode n-ary relations in a binary heap, the ONA philosophy, following KISS, 
' encourages the use of left-nesting, which is also used by the system itself to compose sequences of events:

((a &/ b) &/ c).

").

nal_is_test(read, "
'2. The parallel temporal copula &| is not implemented, please use &/ for now, again due to KISS. 
' If the order does not matter in some background knowledge we want to give the system, in addition to

<(a &/ b) =/> c>

' also give it

<(b &/ a) =/> c>

' to achieve the same as with &| for now.
").

nal_is_test(read, "
'Optional syntactic improvements
' The ONA parser does not require commas, and doesn't distinguish between < and (, also it supports infix format.

<(|,a,b) --> M>. 

' can be written as

<(a | b) --> M>. 

' or even as

((a | b) --> M).

' Note: Spaces cannot be omitted.
").

nal_is_test(read, "
'Tim is alive.

<{Tim} --> [alive]>.

'Tim is a human.

<{Tim} --> human>.
").

nal_is_test(read, "
'Humans are a lifeform.

<human --> lifeform>.

'Lifeforms are like machines.

<lifeform <-> machine>.
").

nal_is_test(read, "
'Tom eats chocolate.

<(*,{Tom},chocolate) --> eat>.

<{Tom} --> (/,eat,_,chocolate)>.

<chocolate --> (/,eat,{Tom},_)>.
").

nal_is_test(read, "
*volume=0
*motorbabbling=false
<corridor --> in>. :|:
<({SELF} * kitchen) --> ^go>. :|:
<({cat} * kitchen) --> in>. :|:
100
<corridor --> in>. :|:
<({SELF} * bedroom) --> ^go>. :|:
<({cat} * bedroom) --> in>. :|:
100
<corridor --> in>. :|:
<({SELF} * livingroom) --> ^go>. :|:
//no cat this time, it doesn't like the livingroom :)
100
<corridor --> in>. :|:
<({SELF} * bedroom) --> ^go>. :|:
<({cat} * bedroom) --> in>. :|:
100
//Ok you are in corridor now
<corridor --> in>. :|:
").

nal_is_test(read, "
*volume=0
G! :|:
").

nal_is_test(read, "//Jonas has asthma?
<{jonas} --> [asthma]>?
//expected: Answer: <{jonas} --> [asthma]>. Truth: frequency=1.000000, confidence=0.801900
//Angelika has asthma?
<{angelika} --> [asthma]>?
//expected: Answer: <{angelika} --> [asthma]>. Truth: frequency=1.000000, confidence=0.810000").


nal_is_test(exec, "
'********** [08 + 09 -> 10]:

'The robot is holding key001. 
<(*,Self,key001) --> hold>. :|: %1.00;0.81% 

1

'The robot is holding key001. 
<(*,Self,key001) --> hold>. :|:

5

'The robot is holding key001. 
''outputMustContain('<(*,Self,key001) --> hold>. :!1: %1.00;0.93%') 
").


nal_is_test(exec, "
'********** compound composition, two premises

'Sport is a type of competition. 
<sport --> competition>. %0.90% 

'Chess is a type of competition. 
<chess --> competition>. %0.80%  

16

'If something is either chess or sport, then it is a competition.
''outputMustContain('<(|,chess,sport) --> competition>. %0.72;0.81%')

'If something is both chess and sport, then it is a competition.
''outputMustContain('<(&,chess,sport) --> competition>. %0.98;0.81%')
").


nal_is_test(exec, "
********** [07 + 09 -> 11]:
  IN: <(*,key001) --> ^pick>. :|: %1.00;0.90% {0 : 0 : 1} 

1

  IN: <(*,Self,key001) --> hold>. :|: %1.00;0.90% {1 : 1 : 2} 

1

 OUT: <<(*,Self,key001) --> hold> =\\> <(*,key001) --> ^pick>>. :\\: %1.00;0.45% {2 : 1 : 1;2} 

 OUT: <<(*,key001) --> ^pick> =/> <(*,Self,key001) --> hold>>. :\\: %1.00;0.45% {2 : 1 : 1;2} 

 OUT: <<(*,key001) --> ^pick> </> <(*,Self,key001) --> hold>>. :\\: %1.00;0.45% {2 : 1 : 1;2} 

 OUT: <key001 --> (/,^pick,_)>. :\\: %1.00;0.90% {2 : 0 : 1} 
").

nal_is_test(exec, "********** deduction
  IN: <bird --> animal>. %1.00;0.90% {0 : 1} 
  IN: <robin --> bird>. %1.00;0.90% {0 : 2} 
1
 OUT: <robin --> animal>. %1.00;0.81% {1 : 2;1} 
 OUT: <animal --> robin>. %1.00;0.45% {1 : 2;1} ").

nal_is_test(exec, "********** deduction
'the detective claims that tim lives in graz
'<{tim} --> (/,livingIn,_,{graz})>.
'and lawyer claims that this is not the case
<{tim} --> (/,livingIn,_,{graz})>. %0%
100
'the first deponent, a psychologist,
'claims that people with sunglasses are more aggressive
<<(*,$1,sunglasses) --> own> ==> <$1 --> [aggressive]>>.
'the third deponent claims, that he has seen tom with sunglasses on:
<(*,{tom},sunglasses) --> own>.
'the teacher claims, that people who are aggressive tend to be murders
<<$1 --> [aggressive]> ==> <$1 --> murder>>.
'the second deponent claims, that if the person lives in Graz, he is surely the murder
<<$1 --> (/,livingIn,_,{graz})> ==> <$1 --> murder>>.
'who is the murder?
<{?who} --> murder>?
''outputMustContain('<{tom} --> murder>. %1.00;0.73%')").

nal_is_test(read, "
//First: Input diamond:
// |    ██    |
// |  ██  ██  |
// |██      ██|
// |  ██  ██  |
// |    ██    |:
<{M1[-1.0,0.0]} --> [BRIGHT]>.
<{M1[1.0,0.0]} --> [BRIGHT]>.
<{M1[0.0,1.0]} --> [BRIGHT]>.
<{M1[0.0,-1.0]} --> [BRIGHT]>.
<{M1[0.5,0.5]} --> [BRIGHT]>.
<{M1[-0.5,0.5]} --> [BRIGHT]>.
<{M1[0.5,-0.5]} --> [BRIGHT]>.
<{M1[-0.5,-0.5]} --> [BRIGHT]>.
<{M1} --> (/,called,_,circle)>.

//Re-observe imperfectly
// |▒▒  ██    |
// |  ██  ▒▒  |
// |▒▒      ██|
// |  ██  ▒▒▒▒|
// |          |:
<{M3[-1.0,1.0]} --> [BRIGHT]>. %0.5%
<{M3[0.0,1.0]} --> [BRIGHT]>.
<{M3[-0.5,0.5]} --> [BRIGHT]>.
<{M3[0.5,0.5]} --> [BRIGHT]>. %0.5%
<{M3[-1.0,0.0]} --> [BRIGHT]>. %0.5%
<{M3[1.0,0.0]} --> [BRIGHT]>.
<{M3[-0.5,-0.5]} --> [BRIGHT]>.
<{M3[0.5,-0.5]} --> [BRIGHT]>. %0.5%
<{M3[1.0,-0.5]} --> [BRIGHT]>. %0.5%

50000
//What was observed?
<{M3} --> (/,called,_,?what)>?
//A circle
''outputMustContain('<{M3} --> (/,called,_,circle)>. %0.83;0.36%')").

nal_is_test(exec, "
*volume=0
burgers are tasty food
the stranger eats a burger in the pub
100
who eats tasty food?
//expected: Answer: <stranger --> (eat /1 ([tasty] & food))>. :|: occurrenceTime=2 Truth: frequency=1.000000, confidence=0.278811
who is in the pub?
//expected: Answer: <stranger --> (in /1 pub)>. :|: occurrenceTime=3 Truth: frequency=1.000000, confidence=0.900000
").

nal_is_test(exec, "
//NARS, where is the cat?
//Passive question <({cat} * ?where) --> in>? :|: wouldn't trigger a decision
//Active question however does:
<(<({cat} * #where) --> in> &/ <({SELF} * #where) --> ^say>) =/> G>.
G! :|:
100
//expected: ^go executed with args ({SELF} * bedroom)
").

nal_is_test(read, "
//ok, feedback of NARS going to the bedroom, the cat is there!
<({cat} * bedroom) --> in>. :|:
G! :|:
10
//expected: ^say executed with args ({SELF} * bedroom)
").


nal_is_test(exec, "
'********** compound composition, two premises

'Sport is a type of competition. 
<sport --> competition>. %0.90% 

'Chess is a type of competition. 
<chess --> competition>. %0.80%  

16

'If something is either chess or sport, then it is a competition.
''outputMustContain('<(|,chess,sport) --> competition>. %0.72;0.81%')

'If something is both chess and sport, then it is a competition.
''outputMustContain('<(&,chess,sport) --> competition>. %0.98;0.81%')
").

nal_is_test(read, "<dog --> (/,REPRESENT,_,DOG)>. %1.00;0.90% {0 : 2} ").

nal_is_test(read, "
'********** induction on events 

'John is opening door_101
<John --> (/,open,_,door_101)>. :|: 

6

'John is entering room_101
<John --> (/,enter,_,room_101)>. :|: 

20

'If John enter room_101, he should open door_101 before
''outputMustContain('<<John --> (/,enter,_,room_101)> =\\> (&/,<John --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')

'new: variable introduction also in time:

'If someone enter room_101, he should open door_101 before
''outputMustContain('<<$1 --> (/,enter,_,room_101)> =\\> (&/,<$1 --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')

'adjusted +2 to +3 in both conditions

10
").

nal_is_test(read, "
''outputMustContain('<<John --> (/,enter,_,room_101)> =\\> (&/,<John --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')").
nal_is_test(read, "
''outputMustContain('<<$1 --> (/,enter,_,room_101)> =\\> (&/,<$1 --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%')").


nal_is_test(read, "
<<John --> (/,enter,_,room_101)> =\\> (&/,<John --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%").
nal_is_test(read, "<<$1 --> (/,enter,_,room_101)> =\\> (&/,<$1 --> (/,open,_,door_101)>,+6)>. :!6: %1.00;0.45%").
nal_is_test(read, "****************I am a comment").
nal_is_test(read, "****************").

nal_is_test(exec,"
*volume=0
//When a person picks an object in a place, the picked object is also in that place
<<#Person --> ((pick /1 $Object) & (in /1 $Place))> ==> <$Object --> (in /1 $Place)>>.
John is in the playground.
Bob is in the office.
John picked up the football.
Bob went to the kitchen.
50
The football is in what?
//expected: Answer: <football --> (in /1 playground)>. :|: occurrenceTime=4 Truth: frequency=1.000000, confidence=0.466560
").


nal_is_test(exec,"
********** conditional deduction
  IN: <(&&,a,b) ==> c>. %1.00;0.90% {0 : 1} 
  IN: a. %1.00;0.90% {0 : 2} 
1
 OUT: <b ==> c>. %1.00;0.81% {1 : 1;2} 
").

nal_is_test(read,X):-
 atomic_list_concat(List,'\n',
"IN: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\:
 IN: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\: %1.00;0.90% {0 : -1 : 1}
 IN: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\:
 IN: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\: %1.00;0.90% {0 : -1 : 1}
 IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)!
 IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)! %1.00;0.90% {0 : 2}
 IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :|:
 IN: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :|: %1.00;0.90% {0 : 0 : 1}
 IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>.
 IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>. %1.00;0.90% {0 : 1}
 IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>. %1.00;0.90% {0 : 2}
 IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>. 
 IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>) =|> <(*,Self,#1) --> reachable>>.
 IN: <(&|,<(*,#1,#2(#1)) --> on>,<(*,Self,#2(#1)) --> at>)=|><(*,Self,#1) --> reachable>>.
 OUT: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\: %1.00;0.90%
 OUT: (&|,<#1() --> (/,at,Self,_)>,<(*,{t002},#1()) --> on>). :\\: %1.00;0.90% {8 : -1 : 1}
 OUT: (&|,<#1() --> (/,at,Self,_)>,<{t002} --> (/,on,_,#1())>). :\\: %1.00;0.90% {6 : -1 : 1}
 OUT: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\: %1.00;0.81%
 OUT: (&|,<#1() --> (/,on,{t002},_)>,<#1() --> (/,at,Self,_)>). :\\: %1.00;0.81% {2 : -1 : 2;1}
 OUT: (&|,<(*,Self,#1()) --> at>,<{t002} --> (/,on,_,#1())>)! %1.00;0.90% {6 : 2}
 OUT: (&|,<(*,Self,#1()) --> at>,<{t002} --> (/,on,_,#1())>)? :\\:  {23 : 1 : 3}
 OUT: (&|,<(*,Self,#1()) --> at>,<{t002} --> (/,on,_,#1())>)? :|:  {7 : 7 : 5}
 OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)! %1.00;0.81%
 OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)! %1.00;0.81% {16 : 2;1}
 OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :\\: %1.00;0.90%
 OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>). :\\: %1.00;0.90% {4 : -1 : 1}
 OUT: (&|,<(*,{t002},#1()) --> on>,<(*,Self,#1()) --> at>)? :|:  {1 : 1 : 3}
 OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>)! %1.00;0.90% {1 : 2}
 OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>). :\\: %1.00;0.90% {3 : -1 : 1}
 OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>)? :\\:  {15 : 1 : 3}
 OUT: (&|,<(*,{t002},#1()) --> on>,<Self --> (/,at,_,#1())>)? :|:  {2 : 2 : 4}
 OUT: <(&|,<(*,#1,#2(#1)) --> on>,<Self --> (/,at,_,#2(#1))>) ==> <(*,Self,#1) --> reachable>>. %1.00;0.90% {4 : 1}"),
 member(X,List).



nal_is_test(read,X):-atomic_list_concat(List,'\n',"
  ''outputMustContain('')
  ''outputMustContain('(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001}))! %1.00;0.43%')
  ''outputMustContain('(^go-to,{t001})! %1.00;0.81%')
  ''outputMustContain('(^go-to,{t003})! %1.00;0.81%')
  ''outputMustContain('...')
  ''outputMustContain('<(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.81%')
  ''outputMustContain('<(&&,<$1 --> [with-wings]>,<(*,$1,worms) --> food>) ==> <$1 --> bird>>. %1.00;0.45%')
  ''outputMustContain('<(&&,<$1 --> flyer>,<(*,$1,worms) --> food>) ==> <$1 --> [with-wings]>>. %1.00;0.45%')
  ''outputMustContain('<(&&,<robin --> [chirping]>,<robin --> [with-beak]>) ==> <robin --> bird>>. %1.00;0.42%')
  ''outputMustContain('<(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.81%')
  ''outputMustContain('<(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.43%')
  ''outputMustContain('<(&/,<(*,Self,{t002}) --> reachable>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.81%')
  ''outputMustContain('<<$1 --> [with-wings]> ==> (&&,<$1 --> flyer>,<(*,$1,worms) --> food>)>. %1.00;0.45%')
  ''outputMustContain('<<robin --> [with-wings]> ==> <robin --> bird>>. %0.90;0.45%')
  ''outputMustContain('<<robin --> [with-wings]> ==> <robin --> bird>>. %1.00;0.81%')
  ''outputMustContain('<<robin --> bird> ==> <robin --> [with-wings]>>. %1.00;0.42%')
  ''outputMustContain('<bird <-> swan>. %0.10')
  (&&,<#x --> P >, <#x --> S>) ?
  (&&,<S1 --> P1>, <S2 --> P2>) .
  (&&,<S1 --> P1>, <S2 --> P2>)?
  (--, <David {-- (/, taller_than, {Tom}, _)>).
  (^go-to,{SELF},{t001}). :\\:
  < (&&,<(*,#manswer,#m) --> replyTo>, <(*,#manswer,U) --> sender>) <=> <#m --> uResponse> >.
  < <#x --> P > ==> <M --> (/,R, #x, _ ) > >.
  <(&&, <#y --> S>, <#x --> P> ) ==> <#y --> (/, R, #x, _ )> > ?
  <(&&,<$x --> [chirping]>,<$x --> [with-wings]>) ==> <$x --> bird>>.
  <(&&,<$x --> flyer>,<$x --> [chirping]>, <(*, $x, worms) --> food>) ==> <$x --> bird>>.
  <(&&,<$y --> [chirping]>,<$y --> [with-wings]>) ==> <$y --> bird>>.
  <(&&,<robin --> [chirping]>,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>.
  <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> [living]>>. %0.9%
  <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>.
  <(&&,<robin --> [flying]>,<robin --> [with-wings]>,<robin --> [chirping]>) ==> <robin --> bird>>.
  <(&&,<robin --> [with-wings]>,<robin --> [chirping]>) ==> <robin --> bird>>.
  <(&, [red], light) --> traffic_signal>?
  <(&/, (||, S, P), +5) =/> M>. %0.9%
  <(&/, a, +1) =/> b>.
  <(&/, b, +1) =/> c>.
  <(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:
  <(&/,<(*,Self,{t002}) --> hold>,(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>.
  <(&/,<(*,Self,{t003}) --> at>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:
  <(*, John, key_101) --> hold>. :\\:
  <(*,S1, S2 ) --> (*,P1, P2 )> ?
  <(*,S1, S2 ) --> (*,P1, P2 )>.
  <(*,{t003}) --> ^go-to>. :|:
  <(^go-to,$1) =/> <(*,Self,$1) --> at>>.
  <(^go-to,$1)=/><(*,SELF,$1) --> at>>.
  <(^go-to,{SELF},$1)=/><(*,{SELF},$1) --> at>>.
  <(|, boy, girl) --> youth>. %0.90%
  <(~, boy, girl) --> [strong]>. %0.90%
  <(~,swimmer, swan) --> bird>?
  <<$y --> [with-wings]> ==> <$y --> flyer>>.
  <<$y --> flyer> ==> <$y --> [with-wings]>>.
  <<(*,$1) --> ^go-to> =/> <(*,SELF,$1) --> at>>.
  <<(*,$1,$2) --> Friends> ==>     (||,    (&&,<$1 --> [Smokes]>,<$2 --> [Smokes]>),    (&&,(--,<$1 --> [Smokes]>),(--,<$2 --> [Smokes]>)))>. %0.6;0.9%
  <<robin --> [flying]> ==> <robin --> [with-beak]>>. %0.90%
  <John {-- (/, taller_than, {Tom}, _)>.
  <Karl {-- (/, taller_than, {Tom}, _)>.
  <light_1 {-- (&, [red], light)>.
  <light_2 {-- (&, [red], light)>.
  <Tom {-- (/, taller_than, _, boy)>?
  <{Tweety} --> [with-wings]>.
  [number]: # of cycles to process before continuing to next line
  IN: (^go-to,{t001})!
  IN: (^go-to,{t001}). :\\:
  IN: (^go-to,{t003})! %1.00;0.90% {0 : 1}
  IN: (^go-to,{t003}). :|:
  IN: <(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.90% {0 : 2}
  IN: <(&&,<robin --> [chirping]>,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 1}
  IN: <(&&,<robin --> [chirping]>,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 2}
  IN: <(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 1}
  IN: <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> [living]>>. %0.90;0.90% {0 : 1}
  IN: <(&&,<robin --> [flying]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.90% {0 : 1}
  IN: <(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:
  IN: <(&/,<(*,Self,{t002}) --> hold>,(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>.
  IN: <(&/,<(*,Self,{t002}) --> hold>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.90% {0 : 1}
  IN: <(&/,<(*,Self,{t002}) --> reachable>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.90% {0 : 1}
  IN: <(&/,<(*,Self,{t003}) --> at>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :|:
  IN: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :|: %1.00;0.90% {0 : 0 : 1}
  IN: <(&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :|: %1.00;0.90% {0 : 0 : 2}
  IN: <(*, John, key_101) --> hold>. :/:
  IN: <(*,{t001}) --> ^go-to>! %1.00;0.90% {0 : 1}
  IN: <(*,{t001}) --> ^go-to>. :\\: %1.00;0.90% {0 : -1 : 1}
  IN: <(*,{t003}) --> ^go-to>! %1.00;0.90% {0 : 1}
  IN: <(*,{t003}) --> ^go-to>. :|: %1.00;0.90% {0 : 0 : 1}
  IN: <(^go-to,#1) =/> <(*,Self,#1) --> at>>.
  IN: <(^go-to,#1)=/><(*,Self,#1) --> at>>.
  IN: <<$1 --> [with-wings]> ==> <$1 --> flyer>>. %1.00;0.90% {0 : 2}
  IN: <<$1 --> flyer> ==> <$1 --> [with-wings]>>. %1.00;0.90% {0 : 2}
  IN: <<(*,#1) --> ^go-to> =/> <(*,Self,#1) --> at>>. %1.00;0.90% {0 : 1}
  IN: <<(*,#1) --> ^go-to> =/> <(*,Self,#1) --> at>>. %1.00;0.90% {0 : 2}
  IN: <<robin --> [flying]> ==> <robin --> [with-beak]>>. %0.90;0.90% {0 : 2}
  IN: <{Tweety} --> [with-wings]>. %1.00;0.90% {0 : 1}
  out is a reference to the current output buffer, containing a list of strings; one for each output
  OUT: (&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001}))! %1.00;0.81%
  OUT: (&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>)! %1.00;0.81% {16 : 2;1}
  OUT: (&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>)? :\\:  {13 : 0 : 2;3}
  OUT: (&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<{t001} --> (/,^go-to,_)>,<(*,{t001}) --> ^open>)? :\\:  {14 : 0 : 2;3}
  OUT: (^go-to,{t001})! %1.00;0.81%
  OUT: (^go-to,{t001}). :|: %1.00;0.90%
  OUT: (^go-to,{t003})! %1.00;0.81%
  OUT: (^go-to,{t003}). :|: %1.00;0.90%
  OUT: <(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.81%
  OUT: <(&&,<$1 --> [chirping]>,<$1 --> [with-wings]>) ==> <$1 --> bird>>. %1.00;0.81% {1 : 2;1}
  OUT: <(&&,<$1 --> [with-wings]>,<(*,$1,worms) --> food>) ==> <$1 --> bird>>. %1.00;0.45%
  OUT: <(&&,<$1 --> [with-wings]>,<(*,$1,worms) --> food>) ==> <$1 --> bird>>. %1.00;0.45% {4 : 1;2}
  OUT: <(&&,<$1 --> flyer>,<(*,$1,worms) --> food>) ==> <$1 --> [with-wings]>>. %1.00;0.45%
  OUT: <(&&,<$1 --> flyer>,<(*,$1,worms) --> food>) ==> <$1 --> [with-wings]>>. %1.00;0.45% {4 : 1;2}
  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-beak]>) ==> <robin --> bird>>. %1.00;0.42%
  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-beak]>) ==> <robin --> bird>>. %1.00;0.42% {11 : 1;2}
  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.81%
  OUT: <(&&,<robin --> [chirping]>,<robin --> [with-wings]>) ==> <robin --> bird>>. %1.00;0.81% {5 : 1;2}
  OUT: <(&/,(^go-to,{t003}),(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. :\\: %1.00;0.81%
  OUT: <(&/,<(*,Self,{t002}) --> hold>,(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.81%
  OUT: <(&/,<(*,Self,{t002}) --> hold>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.81% {2 : 2;1}
  OUT: <(&/,<(*,Self,{t002}) --> reachable>,(^pick,{t002}),(^go-to,{t001}),(^open,{t001})) =/> <{t001} --> [opened]>>. %1.00;0.81%
  OUT: <(&/,<(*,Self,{t002}) --> reachable>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.81% {14 : 2;1}
  OUT: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.43% {5 : 2;1}
  OUT: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.43% {8 : 2;1}
  OUT: <(&/,<(*,Self,{t003}) --> at>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :\\: %1.00;0.81% {17 : 0 : 1;2}
  OUT: <(&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. %1.00;0.43% {11 : 1;2}
  OUT: <(&/,<(*,{t003}) --> ^go-to>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) =/> <{t001} --> [opened]>>. :\\: %1.00;0.81% {16 : 0 : 2;1}
  OUT: <(&/,<Self --> (/,at,_,{t003})>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. :\\: %1.00;0.90% {15 : 0 : 1}
  OUT: <(&/,<Self --> (/,hold,_,{t002})>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {10 : 1}
  OUT: <(&/,<Self --> (/,reachable,_,{t002})>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {16 : 1}
  OUT: <(&/,<{t002} --> (/,hold,Self,_)>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {11 : 1}
  OUT: <(&/,<{t002} --> (/,reachable,Self,_)>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. %1.00;0.90% {9 : 1}
  OUT: <(&/,<{t003} --> (/,^go-to,_)>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. :\\: %1.00;0.90% {11 : 0 : 2}
  OUT: <(&/,<{t003} --> (/,at,Self,_)>,<(*,{t002}) --> ^pick>,<(*,{t001}) --> ^go-to>,<(*,{t001}) --> ^open>) ==> <{t001} --> [opened]>>. :\\: %1.00;0.90% {7 : 0 : 1}
  OUT: <(*,{t001}) --> ^go-to>! %1.00;0.81% {19 : 2;1}
  OUT: <(*,{t001}) --> ^go-to>. :|: %1.00;0.90% {1 : 1 : 2}
  OUT: <(*,{t003}) --> ^go-to>! %1.00;0.81% {19 : 2;1}
  OUT: <(*,{t003}) --> ^go-to>. :|: %1.00;0.90% {1 : 1 : 2}
  OUT: <(/,(*,{t001}),_) --> (/,^go-to,_)>. :\\: %1.00;0.90% {7 : -1 : 1}
  OUT: <(/,(*,{t003}),_) --> (/,^go-to,_)>. :\\: %1.00;0.90% {7 : 0 : 1}
  OUT: <(~,swimmer, swan) --> bird>. %0.10;0.73%
  OUT: <<$1 --> [with-wings]> ==> (&&,<$1 --> flyer>,<(*,$1,worms) --> food>)>. %1.00;0.45%
  OUT: <<$1 --> [with-wings]> ==> (&&,<$1 --> flyer>,<(*,$1,worms) --> food>)>. %1.00;0.45% {4 : 1;2}
  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {1 : 1}
  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {1 : 2}
  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {10 : 2}
  OUT: <<(*,#1) --> ^go-to> =/> <Self --> (/,at,_,#1)>>. %1.00;0.90% {8 : 2}
  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %0.90;0.45%
  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %0.90;0.45% {8 : 2;1}
  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %1.00;0.81%
  OUT: <<robin --> [with-wings]> ==> <robin --> bird>>. %1.00;0.81% {3 : 2;1}
  OUT: <<robin --> bird> ==> <robin --> [with-wings]>>. %1.00;0.42%
  OUT: <<robin --> bird> ==> <robin --> [with-wings]>>. %1.00;0.42% {8 : 2;1}
  OUT: <{t001} --> (/,^go-to,_)>. :\\: %1.00;0.90% {2 : -1 : 1}
  OUT: <{t003} --> (/,^go-to,_)>. :\\: %1.00;0.90% {2 : 0 : 1}
  IN: <(*, John, key_101) --> hold>. :/:
<John {-- (/, taller_than, {Tom}, _)>.
<Tom {-- (/, taller_than, _, boy)>?
(--, <David {-- (/, taller_than, {Tom}, _)>).
<Karl {-- (/, taller_than, {Tom}, _)>.
<(~,swimmer, swan) --> bird>?
OUT: <(~,swimmer, swan) --> bird>. %0.10;0.73%
<(|, boy, girl) --> youth>. %0.90%
<(~, boy, girl) --> [strong]>. %0.90%
<(&&,<$x --> flyer>,<$x --> [chirping]>, <(*, $x, worms) --> food>) ==> <$x --> bird>>.
IN: <(*, John, key_101) --> hold>. :/:

''outputMustContain('<bird <-> swan>. %0.10')

nars hears a boom"),member(X,List),X\=="",X\==''.


nal_is_test(exec,"'********** conversions between inheritance and similarity

'Swan is a type of bird. 
<swan --> bird>. 

'Bird is not a type of swan. 
<bird --> swan>. %0.10% 

1

'Bird is different from swan.  
''outputMustContain('<bird <-> swan>.')
''outputMustContain('<bird <-> swan>. %0.10')
''outputMustContain('<bird <-> swan>. %0.10;0.81%')

").

nal_is_test(read,"
<{a, b} |- (&/,a,b)>.").

nal_is_test(read,"
<{a, b, after(a,b)} |- <a =/> b>>.").

nal_is_test(read,"
<{ b! , <a =/> b>} |- a! >").


% {Event a., Implication <a =/> b>.} |- Event b.
