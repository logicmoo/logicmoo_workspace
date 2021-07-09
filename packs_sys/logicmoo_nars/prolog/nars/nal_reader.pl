% nal_reader.pl
% Read Non_Axiomatic Logic from Prolog
:-module(nal_reader,[
            nal_test/0,
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
*/




nal_task(S)--> cwhite,!,nal_task(S),!.
nal_task(task(X,S,T,O,B)) --> nal_task(X,S,T,O,B),!.



nal_task(X,S,T,O,B) --> optional(B, nal_budget),!, nal_sentence(X,S,T,O).  % task to be processed 


nal_sentence(X,S,T,O)--> nal_statement(S), nal_post_statement(X,T,O).

nal_post_statement(X,T,O)--> 
          /*nal_statement(S),*/ nal_o(`.` ,X, judgement)-> optional(T,nal_tense)-> optional(O,nal_truth),!   % judgement to be absorbed into beliefs 
        ; /*nal_statement(S),*/ nal_o(`?` ,X, question_truth)-> optional(T,nal_tense)-> optional(O,nal_truth),! % question on truth_value to be answered 
        ; /*nal_statement(S),*/ nal_o(`!` ,X, goal), optional(O,nal_desire)                  % goal to be realized by operations 
        ; /*nal_statement(S),*/ nal_o(`@` ,X, question_desire), optional(O,nal_desire)       % question on desire_value to be answered 
        .

nal_statement(S)--> mw(nal_statement_0(S)),!.
nal_statement_0(S)--> 
        mw(`<`) ,!, nal_term(A), nal_copula(R), nal_term(B), mw(`>`) ,   {S=..[R,A,B]}   % two, terms related to each other 
      ;  nal_l_paren, `^` , nal_term_list_comma(L), nal_paren_r,       {S= exec(L)}            % an operation to be executed 
      ;  nal_l_paren, nal_term(A), nal_copula(R), nal_term(B), nal_paren_r,  {S=..[R,A,B]}       % two, terms related to each other, new notation 
      ;  nal_word(A), nal_l_paren, nal_term_list_comma(L), nal_paren_r,    {S= exec([A|L])}        % an operation to be executed, new notation 
      ;  nal_term_1(X),             {S= nal_named_statement(X)}                       % a, term, can name a statement(S) 
      .
         

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
         ;  nal_o(`=>` ,X,                           unknown_impl )
         .

nal_term(N) --> nal_term_old(O), {old_to_new(O,N)}.
nal_term_old(S)
       --> nal_word(S)                         % an atomic constant, term,         
        ;  nal_variable(S)                     % an atomic variable, term, 
        ;  nal_compound_term(S)                % a, term, with internal structure 
        ;  nal_statement(S)                    % a statement can serve as a, term, 
        .

nal_term_0(N) --> nal_term_0_old(O), {old_to_new(O,N)}.
nal_term_0_old(S) 
        -->  nal_word_0(S)                       % an atomic constant, term,         
        ;  nal_variable_0(S)                     % an atomic variable, term, 
        ;  nal_compound_term_0(S)                % a, term, with internal structure 
        ;  nal_statement_0(S)                    % a statement can serve as a, term, 
        .

nal_term_1(N) --> nal_term_1_old(O), {old_to_new(O,N)}.
nal_term_1_old(S)
        --> nal_word(S)                        % an atomic constant, term,         
        ;  nal_variable(S)                     % an atomic variable, term, 
        ;  nal_compound_term(S)                % a, term, with internal structure 
        .

old_to_new(rel([R, var(arg, L) | B]), ext_image(New)):- length(Left,L), append(Left,Right,[R|B]), append(Left,['_'|Right],New).
old_to_new(rel([R, var(int, L) | B]), int_image(New)):- length(Left,L), append(Left,Right,[R|B]), append(Left,['_'|Right],New).
old_to_new(X,X).
nal_compound_term(X)--> mw(nal_compound_term_0(X)).

nal_compound_term_0('exec'([S]))--> `^`,!,nal_term_1(S).
nal_compound_term_0(S)--> \+ dcg_peek(`<`),!,
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
   ;  nal_preserve_whitespace((nal_term_0(A), cspace,  {X=rel}, nal_term_list_white(SL, ` `))),{L=[A|SL]}
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

nal_variable(V)--> mw(nal_variable_0(V)).

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
nal_truth([F,C])--> `%`, !, nal_frequency(F), optional((`;`, nal_confidence(C))), `%`.	                
nal_truth([F,C])--> `{`, !, nal_frequency(F), nal_confidence(C), `}`.	                
% Budget is three numbers in optional(O,0,1]x(0,1)x[0,1] 
nal_budget(nal_budget_pdq(P,D,Q))--> `$`,!, nal_priority(P), optional(( `;`, nal_durability(D))), optional((`;`, nal_quality(Q))), `$`.  


nal_word(E) --> mw(nal_word_0(E)).

nal_word_0('+'(E)) --> `+`,dcg_basics:integer(E),!.
nal_word_0(E) --> dcg_basics:number(E),!.
nal_word_0(E) --> quoted_string(E),!.
nal_word_0(E) --> dcg_peek([C]),{char_type(C,alpha)},!, nal_rsymbol([],E),!.


  nal_priority(F) --> nal_float_inclusive(0,1,F).           %  0 <= x <= 1       
nal_durability(F) --> nal_float_exclusive(0,1,F).           %  0 <  x <  1 
   nal_quality(F) --> nal_float_inclusive(0,1,F).           %  0 <= x <= 1 
 nal_frequency(F) --> nal_float_inclusive(0,1,F).           %  0 <= x <= 1 
nal_confidence(F) --> nal_float_exclusive(0,1,F).           %  0 <  x <  1 

nal_o(S,X,X) --> owhite,S,owhite.
nal_o(X,X) --> nal_o(X,X,X).

nal_float_inclusive(L,H,F)--> mw((dcg_basics:number(F) -> {nal_warn_if_strict((L=< F,F=< H))})).
nal_float_exclusive(L,H,F)--> mw((dcg_basics:number(F) -> {nal_warn_if_strict((L < F,F < H))})).



nal_warn_if_strict(G):- call(G),!.
nal_warn_if_strict(G):- dmsg(nal_warn_if_strict(G)),!.

:- set_dcg_meta_reader_options(file_comment_reader, nal_comment_expr).


nal_comment_expr(X) --> cspace,!,nal_comment_expr(X).
nal_comment_expr('$COMMENT'(Expr,I,CP)) --> nal_comment_expr_3(Expr,I,CP),!.

nal_comment_expr_3(T,N,CharPOS) --> `/*`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`*/`)),!,
  {text_to_string_safe(S,T)},!.
nal_comment_expr_3(T,N,CharPOS) -->  {nal_cmt_until_eoln(Text)},Text,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
 {text_to_string_safe(S,T)},!.


nal_cmt_until_eoln(`//`).
nal_cmt_until_eoln(`'`).
nal_cmt_until_eoln(`**`).


nal_comma --> mw(`,`).
nal_l_paren --> mw(`(`).
nal_paren_r --> mw(`)`).

nal_term_list_white([H|T], Sep) --> nal_term_0(H), ( (Sep,owhite) ->  nal_term_list_white(T, Sep) ; ({T=[]},owhite)).
nal_term_list_comma([H|T]) --> nal_term(H), ( nal_comma ->  nal_term_list_comma(T) ; {T=[]} ).


nal_rsymbol(Chars,E) --> [C], {notrace(nal_sym_char(C))},!, nal_sym_continue(S), {append(Chars,[C|S],AChars),string_to_atom(AChars,E)},!.
nal_sym_continue([]) --> nal_peek_symbol_breaker,!.
nal_sym_continue([H|T]) --> [H], {nal_sym_char(H)},!, nal_sym_continue(T).
nal_sym_continue([]) --> [].

nal_peek_symbol_breaker --> dcg_peek(`--`).
nal_peek_symbol_breaker --> dcg_peek(`-`),!,{fail}.
nal_peek_symbol_breaker --> dcg_peek(one_blank).
nal_peek_symbol_breaker --> dcg_peek([C]),{\+ nal_sym_char(C)},!.

nal_sym_char(C):- \+ integer(C),!,char_code(C,D),!,nal_sym_char(D).
nal_sym_char(C):- bx(C =<  32),!,fail.
%nal_sym_char(44). % allow comma in middle of symbol
% word is: #"[^\ ]+"   %  unicode string     
nal_sym_char(C):- nal_never_symbol_char(NeverSymbolList),memberchk(C,NeverSymbolList),!,fail.  % maybe 44 ? nal_comma
%nal_sym_char(C):- nb_current('$maybe_string',t),memberchk(C,`,.:;!%`),!,fail.
nal_sym_char(_):- !.

nal_never_symbol_char(`";()~'[]<>``{},=\\^`).


nal_rsymbol_cont(Prepend,E) --> nal_sym_continue(S), {append(Prepend,S,AChars),string_to_atom(AChars,E)},!.


nal_is_test_file(X):-filematch('../../nal-tests/**/*',X), \+ nal_non_file(X). 
nal_is_test_file(X):-filematch('../../examples/**/*',X), \+ nal_non_file(X). 
nal_non_file(X):- downcase_atom(X,DC),X\==DC,!,nal_non_file(DC).
nal_non_file(X):- atom_concat(readme,_,X).
nal_non_file(X):- atom_concat(_,'.pl',X).

nal_test_file:- 
 make,
 catch((
   forall(nal_is_test_file(X),((dmsg(file_begin(X)),ignore(nal_test_file(X)),dmsg(file_end(X)))))),
    '$aborted',true).

nal_test_file(File):- (\+ atom(File); \+ is_absolute_file_name(File)),
  absolute_file_name(File,Absolute), !, nal_test_file(Absolute).
nal_test_file(File):- open(File,read,In),
  nal_read_clauses(In, Expr),!,
  must_or_rtrace(nal_call(nal_test_file,Expr,OutL)),!,
  flatten([OutL],Out),
  maplist(wdmsg,Out),!.

% NAL file reader
nal_file(end_of_file) --> file_eof,!.
nal_file(O) --> cspace, !, nal_file(O).
nal_file([]) -->  \+ dcg_peek([_]), !.
nal_file(Comment) --> nal_comment_expr(Comment).
nal_file(O) --> nal_file_element(O), !, owhite.
% fallback to english in a file
nal_file(unk_english(Text)) --> read_string_until_no_esc(Str,eoln), 
  {atom_string(Str,Text)},!. %split_string(Str, "", "\s\t\r\n", Text).

% nal_file(planStepLPG(Name,Expr,Value)) --> owhite,sym_or_num(Name),`:`,owhite, nal(Expr),owhite, `[`,sym_or_num(Value),`]`,owhite.  %   0.0003:   (PICK-UP ANDY IBM-R30 CS-LOUNGE) [0.1000]
% nal_file(Term,Left,Right):- eoln(EOL),append(LLeft,[46,EOL|Right],Left),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
% nal_file(Term,Left,Right):- append(LLeft,[46|Right],Left), ( \+ member(46,Right)),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.

% non-standard
nal_file_element(outputMustContain(O)) --> `''outputMustContain('`, !, trace, read_string_until(Str,`')`),{fmt(Str),phrase(nal_task(O),Str,[])}. 
nal_file_element('1Answer'(O) ) --> `' Answer `, read_string_until(Str,(`{`,read_string_until(_,eoln))),{phrase(nal_task(O),Str,[])}. 
nal_file_element(N=V          ) -->  `*`, nal_word(N), mw(`=`), nal_term(V).
nal_file_element(nal_in(H,V3))  -->  `IN:`,   nal_task(H), optional(nal_three_vals(V3)).
nal_file_element(nal_out(H,V3)) -->  `OUT:`,  nal_task(H), optional(nal_three_vals(V3)).
% standard
nal_file_element(do_steps(N)) --> dcg_basics:number(N),!.
nal_file_element(H) --> nal_task(H).
nal_file_element(nal_term(H)) --> nal_term(H).
% nal_read_clause("'the detective claims that tim lives in graz",A)


% {1 : 4;3} 
nal_three_vals(V3)--> `{`, read_string_until_no_esc(Str,(`}`;eoln)), 
  {read_term_from_codes(Str,V3,[double_quotes(string),syntax_errors(fail)])},!.


%nal_file_with_comments(O,with_text(O,Txt),S,E):- copy_until_tail(S,Copy),text_to_string_safe(Copy,Txt),!.


:- thread_local(t_l:sreader_options/2).



nal_test:- fmt('\nNAL TEST'), forall(nal_is_test(_,Test),nal_test(Test)).


:- use_module(library(dcg/basics)).

% try_reader_test(Test):- is_stream(Test), !, \+ is_compound(Test), open_string(Test,Stream), try_reader_test(Stream).
nal_test(Test):- 
  fmt("\n-----------------------------\n"),
  fmt(Test), 
  fmt("-----------------------------\n"),
  nal_call('dmsg',Test,Out),dmsg(Out).


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

nal_read_clause( NonStream, Out):- \+ is_stream(NonStream), !, % wdmsg(NonStream),
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
   (nal_read_term(Stream, Term),
    (Term == end_of_file -> Out=[];
      (Term = (:- Exec) -> (input:call(Exec), Out=More) ; Out = [Term|More]),
       nal_read_clause(Stream, More)))).

nal_read_term(In,Expr):- 
 notrace(( is_stream(In), 
  remove_pending_buffer_codes(In,Codes), 
  read_codes_from_pending_input(In,Text), Text\==[])), !,
  call_cleanup(parse_meta_ascii(nal_file, Text,Expr),
    append_buffer_codes(In,Codes)).
nal_read_term(Text,Expr):- 
 notrace(( =( ascii_,In),
  remove_pending_buffer_codes(In,Codes))),   
  call_cleanup(parse_meta_ascii(nal_file, Text,Expr),
    append_buffer_codes(In,Codes)).

% Expand Stream or String
nal_call(Ctx, Stream, Out):- \+ compound(Stream),
  must_or_rtrace(nal_read_clauses(Stream, List)), !,
  nal_call(Ctx, List, Out).

nal_call(Ctx, List, Out):- is_list(List),!, maplist(nal_call(Ctx),List, OutL),flatten(OutL,Out).
nal_call(Ctx, InnerCtx=json(List), Out):- !,  nal_call([InnerCtx|Ctx], List, Out).

nal_call(Ctx, List, Out):- 
   sub_term(Sub, List), nonvar(Sub), 
   nal_rule_rewrite(Ctx, Sub, NewSub),
   % ignore((NewSub=='$',wdmsg(nal_rule_rewrite(_Ctx, Sub, NewSub)))),
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
% like to distinguish 
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

nal_is_test(exec, "********** deduction
  IN: <bird --> animal>. %1.00;0.90% {0 : 1} 
  IN: <robin --> bird>. %1.00;0.90% {0 : 2} 
1
 OUT: <robin --> animal>. %1.00;0.81% {1 : 2;1} 
 OUT: <animal --> robin>. %1.00;0.45% {1 : 2;1} ").

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

(rel /1 b), 

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

