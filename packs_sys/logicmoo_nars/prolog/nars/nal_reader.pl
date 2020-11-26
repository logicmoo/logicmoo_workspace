% nal_reader.pl
% Read Non_Axiomatic Logic from Prolog
:-module(nal_reader,[
            test_nal/0,
            test_nal/1, 
           % test_nal/2,
          %  call_nal/2,
            call_nal/3                   
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
                    | "(^"nars_word {","term} ")"                  (* an operation to be executed *)
                    | nars_word"("term {","term} ")"               (* an operation to be executed, new notation *)

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

             term ::= nars_word                                    (* an atomic constant term *)
                    | nars_variable                                (* an atomic nars_variable term *)
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

         nars_variable ::= "$"nars_word                                 (* independent nars_variable *)
                    | "#"nars_word                                 (* dependent nars_variable *)
                    | "?"nars_word                                 (* query nars_variable in question *)

            tense ::= ":/:"                                   (* future event *)
                    | ":|:"                                   (* present event *)
                    | ":\\:"                                  (* :\: past event *)
          
           desire ::= truth                                   (* nars_same format, different interpretations *)
            truth ::= <"%">frequency[<";">confidence]<"%">    (* two numbers in [0,1]x(0,1) *)
           budget ::= <"$">priority[<";">durability][<";">quality]<"$"> (* three numbers in [0,1]x(0,1)x[0,1] *)

               nars_word : #"[^\ ]+"                               (* unicode string *)    
           priority : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
         durability : #"[0]?\.[0]*[1-9]{1}[0-9]*"             (* 0 <  x <  1 *)
            quality : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
          frequency : #"([0]?\.[0-9]+|1\.[0]*|1|0)"           (* 0 <= x <= 1 *)
         confidence : #"[0]?\.[0]*[1-9]{1}[0-9]*"             (* 0 <  x <  1 *)
*/

task(S)--> cwhite,!,task(S),!.
task(task(X,S,T,O,B)) --> task(X,S,T,O,B),!.



task(X,S,T,O,B) --> optional(B, budget),!, sentence(X,S,T,O).  % task to be processed 


sentence(X,S,T,O)--> statement(S), post_statement(X,T,O).

post_statement(X,T,O)--> 
          /*statement(S),*/ o(`.` ,X, judgement)-> optional(T,tense)-> optional(O,truth),!   % judgement to be absorbed into beliefs 
        ; /*statement(S),*/ o(`?` ,X, question_truth)-> optional(T,tense)-> optional(O,truth),! % question on truth_value to be answered 
        ; /*statement(S),*/ o(`!` ,X, goal), optional(O,desire)                  % goal to be realized by operations 
        ; /*statement(S),*/ o(`@` ,X, question_desire), optional(O,desire)       % question on desire_value to be answered 
        .

statement(S)--> mw(statement0(S)),!.
statement0(S)--> 
        mw(`<`) ,!, term(A), copula(R), term(B), mw(`>`) ,   {S=..[R,A,B]}   % two, terms related to each other 
      ;  l_paren, `^` , nars_term_list(L), paren_r,       {S= exec(L)}            % an operation to be executed 
      ;  l_paren, term(A), copula(R), term(B), paren_r,  {S=..[R,A,B]}       % two, terms related to each other, new notation 
      ;  nars_word(A), l_paren, nars_term_list(L), paren_r,    {S= exec([A|L])}        % an operation to be executed, new notation 
      ;  nal_term1(X),             {S= named_statement(X)}                       % a, term, can name a statement(S) 
      .
         

copula(X) -->      
            o(`-->` ,X,                          inheritance )
         ;  o(`<->` ,X,                          similarity )
         ;  o(`{--` ,X,                          instance )
         ;  o(`--]` ,X,                          property )
         ;  o(`{-]` ,X,                          inst_prop )
         ;  o(`==>` ,X,                          implication )
         ;  o(`=/>` ,X,                          predictive_impl )
         ;  o(`=|>` ,X,                          concurrent_impl )
         ;  o(`=\\>` ,X,                         retrospective_impl )
         ;  o(`<=>` ,X,                          equiv )
         ;  o(`</>` ,X,                          predictive_equiv )
         ;  o(`<|>` ,X,                          concurrent_equiv )
         ;  o(`=>` ,X,                           unknown_impl )
         .

term(S)--> nars_word(S)                         % an atomic constant, term,         
        ;  nars_variable(S)                     % an atomic nars_variable, term, 
        ;  compound_term(S)                % a, term, with internal structure 
        ;  statement(S)                    % a statement can serve as a, term, 
        .

term0(S)-->  word0(S)                       % an atomic constant, term,         
        ;  variable0(S)                     % an atomic nars_variable, term, 
        ;  compound_term0(S)                % a, term, with internal structure 
        ;  statement0(S)                    % a statement can serve as a, term, 
        .

nal_term1(S)--> nars_word(S)                        % an atomic constant, term,         
        ;  nars_variable(S)                     % an atomic nars_variable, term, 
        ;  compound_term(S)                % a, term, with internal structure 
        .

compound_term(X)--> mw(compound_term0(X)).

compound_term0('exec'([S]))--> `^`,!,nal_term1(S).
compound_term0(S)--> \+ dcg_peek(`<`),!,
   (  o(op_ext_set,X,ext_set), nars_term_list(L), `}`                % extensional set 
   ;  o(op_int_set,X,int_set), nars_term_list(L), `]`                % intensional set 

   ;  word0(A), `[`, nars_term_list(L), `]`,  {S= v(A,L)}            % @TODO notation 
   ;  o(op_negation,X,negation), term(AB),{L=[AB]}              % negation, new notation 
   ;   l_paren, paren_compound_term(X,L), paren_r 
   ), {S=..[X,L]}.

paren_compound_term(X,L) --> 
      op_multi(X), comma, nars_term_list(L)                        % with prefix operator 
   ;  op_single(X), comma, term(A), comma, term(B), {L=[A,B]} % with prefix operator 
   ;  o(op_ext_image,X,ext_image), comma, nars_term_list(L)        % special case, extensional image 
   ;  o(op_int_image,X,int_image), comma, nars_term_list(L)        % special case, \ intensional image 
   ;  o(op_negation,X,negation), comma, term(AB),{L=[AB]}     % negation 
   ;  term(A), op_multi(X), term(B),{L=[A,B]}                 % with infix operator 
   ;  term(A), op_single(X), term(B),{L=[A,B]}                % with infix operator 
   ;  preserve_whitespace((term0(A), cspace,  {X=rel}, term_list_sep(SL, ` `))),{L=[A|SL]}
   ;  {X=product}, nars_term_list(L)                               % product, new notation 
   .

op_int_set-->`[`.                          % intensional set 
op_ext_set-->`{`.                          % extensional set 
op_negation-->`--`.                        % negation 
op_int_image-->`\\`.                       % \ intensional image 
op_ext_image-->`/`.                        % / extensional image 


preserve_whitespace(DCG,S,E) :- locally(b_setval(whitespace,preserve),phrase(DCG,S,E)).
no_preserve_whitespace(DCG,S,E) :- phrase(DCG,S,E).


op_multi(X)-->   
        o(`&&` ,X, and)                          % conjunction 
      ; o(`*` ,X, product)                       % product 
      ; o(`||` ,X, or)                           % disjunction 
      ; o(`#` ,X, sequence_spatial)              % patham9 "sequence", wasn't really useful for NLP, it was called PART
      ; o(`&|` ,X, parallel_evnts)               % parallel events 
      ; o(`&/` ,X, sequence_evnts)               % sequential events 
      ; o(`|` ,X, int_intersection)              % intensional intersection 
      ; o(`&` ,X, ext_intersection)              % extensional intersection 
      .
op_single(X) --> 
          o(`-`, X, ext_difference)              % extensional difference 
       ;  o(`~`, X, int_difference)              % intensional difference 
       .

nars_variable(V)--> mw(variable0(V)).

variable0(var(X,W))
    -->o(`$`, X, ind), word0(W)      % independent nars_variable 
      ;o(`#`, X, dep), word0(W)      % dependent nars_variable 
      ;o(`?`, X, query), word0(W)    % query nars_variable in question 
      ;o(`/`, X, arg), word0(W)    % query nars_variable in params 
      .

variable0(('_')) --> `_`.
variable0(('#')) --> `#`.
variable0(('$')) --> `$`.


tense(X) -->
      o(`:/:`, X, future)                        % future event 
   ;  o(`:|:`, X, present)                       % present event 
   ;  o(`:\\:`, X, past)                         % :\: past event 
   .
tense('t!'(X)) --> `:!`, number(X), `:`.
tense('t'(X)) --> `:`, nal_term1(X), `:`.

% Desire is nars_same format of Truth, but different interpretations 
desire(D)-->truth(D).									
% Truth is two numbers in [0,1]x(0,1) 
truth([F,C])--> `%`, !, frequency(F), optional((`;`, confidence(C))), `%`.	                
truth([F,C])--> `{`, !, frequency(F), confidence(C), `}`.	                
% Budget is three numbers in optional(O,0,1]x(0,1)x[0,1] 
budget(budget_pdq(P,D,Q))--> `$`,!, priority(P), optional(( `;`, durability(D))), optional((`;`, quality(Q))), `$`.  


nars_word(E) --> mw(word0(E)).

word0(E) --> dcg_basics:number(E),!.
word0(E) --> quoted_string(E),!.
word0(E) --> dcg_peek([C]),{char_type(C,alpha)},!, nars_rsymbol([],E),!.


  priority(F) --> float_inclusive(0,1,F).           %  0 <= x <= 1       
durability(F) --> float_exclusive(0,1,F).           %  0 <  x <  1 
   quality(F) --> float_inclusive(0,1,F).           %  0 <= x <= 1 
 frequency(F) --> float_inclusive(0,1,F).           %  0 <= x <= 1 
confidence(F) --> float_exclusive(0,1,F).           %  0 <  x <  1 

o(S,X,X) --> owhite,S,owhite.
o(X,X) --> o(X,X,X).

float_inclusive(L,H,F)--> mw((dcg_basics:number(F) -> {warn_if_strict((L=< F,F=< H))})).
float_exclusive(L,H,F)--> mw((dcg_basics:number(F) -> {warn_if_strict((L < F,F < H))})).



warn_if_strict(G):- call(G),!.
warn_if_strict(G):- dmsg(warn_if_strict(G)),!.

:- set_dcg_meta_reader_options(file_comment_reader, nars_comment_expr).


nars_comment_expr(X) --> cspace,!,nars_comment_expr(X).
nars_comment_expr('$COMMENT'(Expr,I,CP)) --> nars_comment_expr_3(Expr,I,CP),!.

nars_comment_expr_3(T,N,CharPOS) --> `/*`, !, my_lazy_list_location(file(_,_,N,CharPOS)),!, zalwayz(read_string_until_no_esc(S,`*/`)),!,
  {text_to_string_safe(S,T)},!.
nars_comment_expr_3(T,N,CharPOS) -->  {cmt_until_eoln(Text)},Text,!, my_lazy_list_location(file(_,_,N,CharPOS)),!,zalwayz(read_string_until_no_esc(S,eoln)),!,
 {text_to_string_safe(S,T)},!.


cmt_until_eoln(`//`).
cmt_until_eoln(`'`).
cmt_until_eoln(`**`).


comma --> mw(`,`).
l_paren --> mw(`(`).
paren_r --> mw(`)`).

term_list_sep([H|T], Sep) --> term0(H), ( (Sep,owhite) ->  term_list_sep(T, Sep) ; ({T=[]},owhite)).
nars_term_list([H|T]) --> term(H), ( comma ->  nars_term_list(T) ; {T=[]} ).


nars_rsymbol(Chars,E) --> [C], {notrace(nars_sym_char(C))},!, nars_sym_continue(S), {append(Chars,[C|S],AChars),string_to_atom(AChars,E)},!.
nars_sym_continue([]) --> nars_peek_symbol_breaker,!.
nars_sym_continue([H|T]) --> [H], {nars_sym_char(H)},!, nars_sym_continue(T).
nars_sym_continue([]) --> [].

nars_peek_symbol_breaker --> dcg_peek(`--`).
nars_peek_symbol_breaker --> dcg_peek(`-`),!,{fail}.
nars_peek_symbol_breaker --> dcg_peek(one_blank).
nars_peek_symbol_breaker --> dcg_peek([C]),{\+ nars_sym_char(C)},!.

nars_sym_char(C):- \+ integer(C),!,char_code(C,D),!,nars_sym_char(D).
nars_sym_char(C):- bx(C =<  32),!,fail.
%nars_sym_char(44). % allow comma in middle of symbol
% nars_word is: #"[^\ ]+"   %  unicode string     
nars_sym_char(C):- never_symbol_char(NeverSymbolList),memberchk(C,NeverSymbolList),!,fail.  % maybe 44 ? comma
%nars_sym_char(C):- nb_current('$maybe_string',t),memberchk(C,`,.:;!%`),!,fail.
nars_sym_char(_):- !.

never_symbol_char(`";()~'[]<>``{},=\\^`).


nars_rsymbol_cont(Prepend,E) --> nars_sym_continue(S), {append(Prepend,S,AChars),string_to_atom(AChars,E)},!.


is_nal_test_file(X):-filematch('../../nal-tests/**/*',X), \+ non_nal_file(X). 
is_nal_test_file(X):-filematch('../../examples/**/*',X), \+ non_nal_file(X). 
non_nal_file(X):- downcase_atom(X,DC),X\==DC,!,non_nal_file(DC).
non_nal_file(X):- atom_concat(readme,_,X).
non_nal_file(X):- atom_concat(_,'.pl',X).

test_nal_file:- 
 make,
 catch((
   forall(is_nal_test_file(X),((dmsg(file_begin(X)),ignore(test_nal_file(X)),dmsg(file_end(X)))))),
    '$aborted',true).

test_nal_file(File):- (\+ atom(File); \+ is_absolute_file_name(File)),
  absolute_file_name(File,Absolute), !, test_nal_file(Absolute).
test_nal_file(File):- open(File,read,In),
  read_nal_clauses(In, Expr),!,
  must_or_rtrace(call_nal(test_nal_file,Expr,OutL)),!,
  flatten([OutL],Out),
  maplist(wdmsg,Out),!.


file_nal(end_of_file) --> file_eof,!.
% WANT? 
file_nal(O) --> cwhite,!,file_nal(O).
file_nal([]) -->  \+ dcg_peek([_]),!.
file_nal(outputMustContain(O)) --> `''outputMustContain('`, read_string_until(Str,`')`),{phrase(task(O),Str,[])}. 
file_nal('1Answer'(O)) --> `' Answer `, read_string_until(Str,(`{`,read_string_until(_,eoln))),{phrase(task(O),Str,[])}. 
% file_nal(planStepLPG(Name,Expr,Value)) --> owhite,sym_or_num(Name),`:`,owhite, nal(Expr),owhite, `[`,sym_or_num(Value),`]`,owhite.  %   0.0003:   (PICK-UP ANDY IBM-R30 CS-LOUNGE) [0.1000]
% file_nal(Term,Left,Right):- eoln(EOL),append(LLeft,[46,EOL|Right],Left),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
% file_nal(Term,Left,Right):- append(LLeft,[46|Right],Left), ( \+ member(46,Right)),read_term_from_codes(LLeft,Term,[double_quotes(string),syntax_errors(fail)]),!.
file_nal(do_steps(N)) --> dcg_basics:number(N),!.
file_nal(N=V) -->  mw(`*`), nars_word(N), mw(`=`), term(V).
file_nal(nal_in(H,V3)) -->  `IN:`,  task(H), optional(three_vals(V3)).
file_nal(nal_out(H,V3)) -->  `OUT:`,  task(H), optional(three_vals(V3)).
file_nal(H) --> task(H).
file_nal(term(H)) --> term(H).
file_nal(english(Text)) --> read_string_until_no_esc(Str,eoln), 
  {atom_string(Str,Text)},!. %split_string(Str, "", "\s\t\r\n", Text).

% {1 : 4;3} 
three_vals(V3)--> `{`, read_string_until_no_esc(Str,(`}`;eoln)), 
  {read_term_from_codes(Str,V3,[double_quotes(string),syntax_errors(fail)])},!.


%file_nal_with_comments(O,with_text(O,Txt),S,E):- copy_until_tail(S,Copy),text_to_string_safe(Copy,Txt),!.


:- thread_local(t_l:sreader_options/2).


a_nal_test("'the detective claims that tim lives in graz").

a_nal_test("'Revision ------

'Bird is a type of swimmer.
<bird --> swimmer>.

'Bird is probably not a type of swimmer.
<bird --> swimmer>. %0.10;0.60%

1

'Bird is very likely to be a type of swimmer.
''outputMustContain('<bird --> swimmer>. %0.87;0.91%')").

a_nal_test("

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



test_nal:- forall(a_nal_test(Test),test_nal(Test)).


:- use_module(library(dcg/basics)).

% try_reader_test(Test):- is_stream(Test), !, \+ is_compound(Test), open_string(Test,Stream), try_reader_test(Stream).
test_nal(Test):- call_nal('dmsg',Test,Out),dmsg(Out).


nars_zave_varname(N,V):- debug_var(N,V),!.
%nars_zave_varname(N,V):- V = '$VAR'(N).

/*
implode_varnames(Vs):- (var(Vs) ; Vs==[]),!.
implode_varnames([NV|Vs]) :- implode_varnames(Vs),
  (var(NV) -> ignore((variable_name(NV,Name),nars_zave_varname(Name,NV))); 
  ignore((NV=(N=V),nars_zave_varname(N,V)))).
*/

read_nal_clauses( Text, Out):-
 findall(Cl,read_nal_clause(Text, Cl), OutL),
 flatten([OutL],Out).

read_nal_clause( NonStream, Out):- \+ is_stream(NonStream), !, % wdmsg(NonStream),
  must_or_rtrace((open_string(NonStream,Stream), read_nal_clause(Stream, Out))).

read_nal_clause(Stream, Out):-
 '$current_typein_module'(M),
  M\== input, !,
  setup_call_cleanup(
   '$set_typein_module'(input),
   read_nal_clause(Stream, Out),
   '$set_typein_module'(M)).

read_nal_clause(Stream, Out):-
 op(601, xfx, input:(/)),
 op(601, xfx, input:(\\)),
 (at_end_of_stream(Stream)-> Out=[]; 
   (read_nal_term(Stream, Term),
    (Term == end_of_file -> Out=[];
      (Term = (:- Exec) -> (input:call(Exec), Out=More) ; Out = [Term|More]),
       read_nal_clause(Stream, More)))).

read_nal_term(In,Expr):- 
 notrace(( is_stream(In), 
  remove_pending_buffer_codes(In,Codes), 
  read_codes_from_pending_input(In,Text), Text\==[])), !,
  call_cleanup(parse_meta_ascii(file_nal, Text,Expr),
    append_buffer_codes(In,Codes)).
read_nal_term(Text,Expr):- 
 notrace(( =( ascii_,In),
  remove_pending_buffer_codes(In,Codes))),   
  call_cleanup(parse_meta_ascii(file_nal, Text,Expr),
    append_buffer_codes(In,Codes)).

% Expand Stream or String
call_nal(Ctx, Stream, Out):- \+ compound(Stream),
  must_or_rtrace(read_nal_clauses(Stream, List)), !,
  call_nal(Ctx, List, Out).

call_nal(Ctx, List, Out):- is_list(List),!, maplist(call_nal(Ctx),List, OutL),flatten(OutL,Out).
call_nal(Ctx, InnerCtx=json(List), Out):- !,  call_nal([InnerCtx|Ctx], List, Out).

call_nal(Ctx, List, Out):- 
   sub_term(Sub, List), nonvar(Sub), 
   rule_rewrite(Ctx, Sub, NewSub),
   % ignore((NewSub=='$',wdmsg(rule_rewrite(_Ctx, Sub, NewSub)))),
   nonvar(NewSub), Sub\==NewSub,
   subst(List, Sub, NewSub, NewList), 
   List\==NewList, !, 
   call_nal(Ctx, NewList, Out).

call_nal(_Ctx, List, Out):- flatten([List], Out),!.




rule_rewrite(_Ctx, json(Replace), Replace):- nonvar(Replace),!.


nars_join_atomics(Sep,List,Joined):- atomics_to_string(List,Sep,Joined).

/*
into_nal_tokenized(Text,TokenizedText):- \+ string(Text),!, 
  any_to_string(Text,String), into_nal_tokenized(String,TokenizedText).
into_nal_tokenized(Text,TokenizedText):-
 split_string(Text, "", "\s\t\r\n", [L]), L\==Text,!,
 into_nal_tokenized(L,M), 
 %string_concat(M,"\n",TokenizedText).
 string_concat(M,"",TokenizedText).
into_nal_tokenized(Text,TokenizedText):-   L=[_S1,_S2|_SS],    
  member(Split,["\n'","'\n","<META>'","<META>","\n"]),  
  atomic_list_concat(L,Split,Text),  
  maplist(into_nal_tokenized,L,LO),
  atomics_to_string(LO,Split, TokenizedText).
into_nal_tokenized(Text,TokenizedText):-   
  split_string(Text, "\n", "\s\t\n\r",StringList),
  maplist(into_text80_atoms,StringList,SentenceList),
  maplist(nars_join_atomics(' '),SentenceList,ListOfStrings),
  nars_join_atomics('\n',ListOfStrings,TokenizedText),!.
*/

:- fixup_exports.
