/** <module> swicffi - Use C/C++ Runtimes from SWI-Prolog using only headers
%
% Dec 13, 2035
% Douglas Miles
*/

:- module(swicffi,[install_cffi/2,cffi_tests/0,to_forms/2,cffi_eval/1,cffi_test/1]).
:- reexport(swicli). 


:- style_check(-singleton).
:- style_check(-discontiguous).
:- set_prolog_flag(double_quotes, codes). 

/*
% TODO


defctype(PrologName,CType,Comment)  http://common-lisp.net/project/cffi/manual/html_node/defctype.html


6.1 Built-In Types

— Foreign Type: :char
— Foreign Type: :unsigned-char
— Foreign Type: :short
— Foreign Type: :unsigned-short
— Foreign Type: :int
— Foreign Type: :unsigned-int
— Foreign Type: :long
— Foreign Type: :unsigned-long
— Foreign Type: :long-long
— Foreign Type: :unsigned-long-long
These types correspond to the native C integer types according to the ABI of the Lisp implementation's host system.

:long-long and :unsigned-long-long are not supported natively on all implementations. However, they are emulated by mem-ref and mem-set.

When those types are not available, the symbol cffi-sys::no-long-long is pushed into *features*.

— Foreign Type: :uchar
— Foreign Type: :ushort
— Foreign Type: :uint
— Foreign Type: :ulong
— Foreign Type: :llong
— Foreign Type: :ullong
For convenience, the above types are provided as shortcuts for unsigned-char, unsigned-short, unsigned-int, unsigned-long, long-long and unsigned-long-long, respectively.

— Foreign Type: :int8
— Foreign Type: :uint8
— Foreign Type: :int16
— Foreign Type: :uint16
— Foreign Type: :int32
— Foreign Type: :uint32
— Foreign Type: :int64
— Foreign Type: :uint64
Foreign integer types of specific sizes, corresponding to the C types defined in stdint.h.

— Foreign Type: :float
— Foreign Type: :double
On all systems, the :float and :double types represent a C float and double, respectively. On most but not all systems, :float and :double represent a Lisp single-float and double-float, respectively. It is not so useful to consider the relationship between Lisp types and C types as isomorphic, as simply to recognize the relationship, and relative precision, among each respective category.

— Foreign Type: :long-double
This type is only supported on SCL.

— Foreign Type: :pointer &optional type
A foreign pointer to an object of any type, corresponding to void *. You can optionally specify type of pointer (e.g. (:pointer :char)). Although CFFI won't do anything with that information yet, it is useful for documentation purposes.

— Foreign Type: :void
No type at all. Only valid as the return type of a function.



struct person { int number; char* reason; };

  The equivalent defcstruct form follows:

(defcstruct person (number :int) (reason :string))


Dictionary

convert-from-foreign
convert-to-foreign
defbitfield
defcstruct
defcunion
defctype
defcenum
define-foreign-type
define-parse-method
foreign-bitfield-symbols
foreign-bitfield-value
foreign-enum-keyword
foreign-enum-value
foreign-slot-names
foreign-slot-offset
foreign-slot-pointer
foreign-slot-value
foreign-type-alignment
foreign-type-size
free-converted-object
free-translated-object
translate-from-foreign
translate-to-foreign
translate-into-foreign-memory
with-foreign-slots

*/
install_cffi(_Module,File):-read_file_to_codes(File,Codes,[]),to_forms(Codes,Forms),cffi_eval(Forms).

:- meta_predicate debug_call(0).

char_atom(S):-atom(S),atom_length(S,1).

to_forms(Atom, Expr):- atom(Atom),!,atom_codes(Atom,String),!,cto_forms(String, Expr).
to_forms(String, Expr):- string(String),string_codes(String,Codes),!,cto_forms(Codes, Expr).
to_forms([S|Source],Expr):-char_atom(S),atom_chars(Atom,[S|Source]),!,to_forms(Atom,Expr).
to_forms([S|Source],Expr):-integer(S),cto_forms([S|Source],Expr).
to_forms(O,O).

cto_forms(Source,Expr):-white(Source,Start), sexprs(Expr,Start,[]),!.

cffi_test(Code):-to_forms(Code,Forms),cffi_eval(Forms).
echo_forms(Code):-to_forms(Code,Forms),portray(echo_forms:-Forms).

cffi_eval(Forms):-is_list(Forms),last(Forms,LL),is_list(LL),!,forall(member(F,Forms),cffi_eval1(F)).
cffi_eval(Forms):-is_list(Forms),to_forms(Forms,Next), Forms \=@= Next, !, cffi_eval(Next).
cffi_eval(Forms):-to_forms(Forms,Next), Forms \=@= Next, !, cffi_eval(Next).
cffi_eval(F):-cffi_eval1(F).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parsing (Using LISPy CFFI File format)
   Dont be confused when you are seeing S-Expressions in this library
   I am merely using Trisk;s S-Expresssion parser (from lisprolog.pl with no incentives yet to strip out unused parts)
   Why? There has been 20+ years developing the Lisp FFI design and tests to be leveraged and no incentive (see as it as a 
   distraction at least to msyelf) to copy the entire technology from S to P (prolog terms) syntax.
   Ammusingly the SWI-Prolog interface     is available in S format and not  P 
   https://github.com/logicmoo/swicli/blob/master/cffi-tests/swi-prolog.cffi
   So for those who think it needs to become P .. Please go ahead and get the processes started, I wil continue to
   develop the technology to enable either.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


eoln(10).

blank --> [C], {C =< 32}, white.
blank --> ";", comment, white.

white --> blank.
white --> [].

comment --> [C], {eoln(C)}, !.
comment --> [C], comment.

sexprs([H|T]) --> sexpr(H), !, sexprs(T).
sexprs([]) --> [].

sexpr(L)                      --> "(", !, white, sexpr_list(L), white.
sexpr(vec(V))                 --> "#(", !, sexpr_vector(V), white.
sexpr(boo(t))                 --> "#t", !, white.
sexpr(boo(f))                 --> "#f", !, white.
sexpr(chr(N))                 --> "#\\", [C], !, {N is C}, white.
sexpr(str(S))                 --> """", !, sexpr_string(S), white.
sexpr([quote,E])              --> "'", !, white, sexpr(E).
sexpr([quasiquote,E])         --> "`", !, white, sexpr(E).
sexpr(['unquote-splicing',E]) --> ",@", !, white, sexpr(E).
sexpr([unquote,E])            --> ",", !, white, sexpr(E).
sexpr(E)                      --> sym_or_num(E), white.

sexpr_list([]) --> ")", !.
sexpr_list(_) --> ".", [C], {\+ sym_char(C), !, fail}.
sexpr_list([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_rest([]) --> ")", !.
sexpr_rest(E) --> ".", [C], {\+ sym_char(C)}, !, sexpr(E,C), !, ")".
sexpr_rest([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_vector([]) --> ")", !.
sexpr_vector([First|Rest]) --> sexpr(First), !, sexpr_vector(Rest).

sexpr_string(Str) --> sexpr_ascii(Codes),{string_codes(Str,Codes)}.

sexpr_ascii([]) --> """", !.
sexpr_ascii([C|S]) --> chr(C), sexpr_ascii(S).

chr(92) --> "\\\\", !.
chr(34) --> "\\\"", !.
chr(N)  --> [C], {C >= 32, N is C}.

sym_or_num(E) --> [C], {sym_char(C)}, sym_string(S), {string_to_atom([C|S],E)}.

sym_string([H|T]) --> [H], {sym_char(H)}, sym_string(T).
sym_string([]) --> [].

number(N) --> unsigned_number(N).
number(N) --> "-", unsigned_number(M), {N is -M}.
number(N) --> "+", unsigned_number(N).

unsigned_number(N) --> digit(X), unsigned_number(X,N).
unsigned_number(N,M) --> digit(X), {Y is N*10+X}, unsigned_number(Y,M).
unsigned_number(N,N) --> [].

digit(N) --> [C], {C >= 48, C =<57, N is C-48}.

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

sexpr(E,C,X,Z) :- white([C|X],Y), sexpr(E,Y,Z).

sym_char(C) :- C > 32, \+ member(C,";()#""',`").

string_to_atom(S,N) :- number(N,S,[]), !.
string_to_atom(S,O) :- lowcase(S,L), name(I,L),trim_left(I,'cffi:',O).

trim_left(All,Left,Right):-atom_concat(Left,Right,All)->true;All=Right.

lowcase(C,C).
lowcase([],[]).
lowcase([C1|T1],[C2|T2]) :- lowercase(C1,C2), lowcase(T1,T2).

lowercase(C1,C2) :- C1 >= 65, C1 =< 90, !, C2 is C1+32.
lowercase(C,C).


reader_tests:- cffi_test("()").
% Append:
reader_tests:- 
   echo_forms("
      (  (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))

        (defun fib (n)
          (if (= 0 n) 0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)

        (defun fib (n) (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to) (if (= i to) f2 (fib1 f2 (+ f1 f2) (+ i 1) to)))
        (fib 250)
  ( (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)

        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ())))
 ;;
        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3)))
      "
      ).
/*
// int printf( const char *format [, argument]... )

[DllImport("msvcrt.dll", CharSet=CharSet.Ansi, CallingConvention=CallingConvention.Cdecl)]
public static extern int printf(String format, int i, double d); 

[DllImport("msvcrt.dll", CharSet=CharSet.Ansi, CallingConvention=CallingConvention.Cdecl)]
public static extern int printf(String format, int i, String s); 
}
*/

:-set_prolog_flag(double_quotes, string). 


debug_call(Call):- catch((Call,debug(swicffi,'SUCCEED: ~q.~n',[Call])),E,(debug(swicffi), debug(swicffi,'ERROR: ~q.~n',[E=Call]),throw(E))) *-> true; debug(swicffi,'FAILED: ~q.~n',[Call]) .
:-debug(swicffi).

cffi_eval1(F):-debug_call(cffi_eval2(F)).

cffi_eval2([F,Unmanaged0|Fields]):-once(cffi_to_keyword(Unmanaged0,Unmanaged)),Unmanaged0 \=@= Unmanaged,cffi_eval2([F,Unmanaged|Fields]).
cffi_eval2([defcstruct,Unmanaged0|Fields]):-cffi_to_keyword(Unmanaged0,Unmanaged),cffi_to_args(Fields,ParamTypes),!,
   cli_compile_cstruct(Unmanaged,ParamTypes,ResultCode).
cffi_eval2([defcfun,[Unmanaged0,Managed0]|ReturnTypeArgs]):-maplist(cffi_to_keyword,[Unmanaged0,Managed0],[Unmanaged,Managed]), cffi_to_args(ReturnTypeArgs,[ReturnType|ParamTypes]),!,
   cli_compile_cfun(Unmanaged,Managed,ReturnType,ParamTypes,ResultCode).
cffi_eval2(F):-is_list(F),C=..F,cffi_eval3(C).
cffi_eval2(F):-cffi_eval3(F).

cffi_eval3(F):-predicate_property(F,visible),format(':-~q. ~n',[F]),!,debug_call(F),cffi_db_assert(F),!.
cffi_eval3(F):-cffi_eval4(F),!.

cffi_eval4(F):-predicate_property(F,visible),format(':-~q. ~n',[F]),cffi_db_assert(F).
cffi_eval4(F):-format('delay:- ~q.~n',[F]),cffi_db_assert(F).

cffi_db_assert(C):-C=..[F|ARGS],functor(C,F,A),atom_concat('cdb_',F,DBF),asserta_new(cdb_definer(DBF,A,F)),DB=..[DBF|ARGS],asserta_new(DB).

asserta_new(DB):-functor(DB,F,A),dynamic(F/A),ignore(retract(DB)),asserta(DB).


:-dynamic(cdb_defctype/2).

cffi_to_args(List,Out):-maplist(cffi_to_param,List,Out).


cffi_to_keyword(str(S),A):-atom_string(A,S),!.
cffi_to_keyword(A,A).

cffi_to_param([N0,T],p(O,N)):-cffi_to_keyword(N0,N),cffi_to_param(T,O),!.
cffi_to_param(str(S),O):-atom_string(A,S),!,cffi_to_param(A,O),!.
cffi_to_param(T,T):-cdb_defctype(T,':pointer'),!.
cffi_to_param(T,T):-cdb_defctype(_,T),!.
cffi_to_param(T,O):-cdb_defctype(T,B),!,cffi_to_param(B,O),!.
cffi_to_param(T,O):-cffi_to_keyword(T,O),!.


cli_compile_cfun(Unmanaged,Managed,ReturnType,ParamTypes,ResultCode):-cdb_cli_get_dll(DLL,_),cffi_eval4(cfun(DLL,Unmanaged,Managed,ReturnType,ParamTypes)).
cli_compile_cstruct(Unmanaged,ParamTypes,ResultCode):-cffi_eval4(cli_compile_cstruct(Unmanaged,ParamTypes,ResultCode)).
'load-foreign-library'(Str):-cffi_eval4(cli_get_dll(Str,R)).
defctype(Managed,Unmanaged):-asserta(cdb_defctype(Managed,Unmanaged)).


display_class(O):- forall((cli_memb(O,PP),\+ contains_var(static(true),PP),cli_cast(PP,'System.Reflection.MemberInfo',MI),
  cli_get(MI,['DeclaringType','Namespace'],DT)   % DT\="System", DT\="System.Reflection",
  ),writeln(cli_memb(O,PP))).

cffi_tests :- forall(cffi_test,true).

cffi_test :- cli_compile_enum(int,'MyEnum',['Low'(0),'High'(100)],[],O),display_class(O).
cffi_test :- cli_compile_type_raw([],[],"MyType",[f('Low',int(0),[],[]),f('High',int(100),['Static'],[])],['FlagsAttribute'],O),display_class(O).
cffi_test_disabled :- cli_compile_type([int],[],"MyType",[f('Low'(0)),f('High'(100))],['FlagsAttribute'],_).

cffi_test_disabled :- cli_compile_type(class([f(intValue,int(3))],foo,[]),NewClass),cli_new(NewClass,[],Instance),cli_get(Instance,intValue,Out).  %  it will return Out = 3.

cffi_test:-cffi_test('

  (defcunion uint32-bytes
    (int-value :unsigned-int)
    (bytes :unsigned-char :count 4))

(defcenum my-boolean
    :no
    :yes)

 (ql:quickload :cffi)

(defcstruct person (number :int) (reason :string))

(cffi:load-foreign-library "user32.dll")
 
(cffi:defctype hwnd :unsigned-int)
 
(cffi:defcfun ("MessageBoxA" message-box) :int
  (wnd     hwnd)
  (text    :string)
  (caption :string)
  (type    :unsigned-int))

(message-box 0 "hello" "world" 0)  

   (define-foreign-type my-string-type ()
    ((encoding :reader string-type-encoding :initarg :encoding))
    (:actual-type :pointer))

 (foreign-funcall "getenv" :string "SHELL" :string)

 (with-foreign-string (str "abcdef")
          (foreign-funcall "strlen" :string str :int))
').


% cffi_test :- forall(reader_tests,true).
% works!
cffi_test :- \+ cli_is_windows, cli_get_dll('libc.so.6',DLL),cli_call(DLL,printf,["Linux I have been clicked %d times", 2],O).
cffi_test :- cli_is_windows, cli_get_dll('msvcrt',DLL), cli_cast(0,int,Zero), cli_call(DLL,printf,["Win32 I have been clicked %d times", 2],Zero).
% cffi_test :- cli_is_windows, cli_get_dll('msvcrt',DLL), cli_cast(0,int,Zero), cli_call(DLL,mspec(printf(':string',':int'),int),["Win32 I have been clicked %d times", 2],Zero).

% cffi_test :- install_cffi('snake-tail',pack('swicli/cffi-tests/swi-prolog.cffi')),module(swicffi),prolog.
% cffi_test:- cli_compile_enum(int,'MyEnum',['Low'(0),'High'(100)],[],O),cli_memb(O,M),writeq(M),nl,fail.
% cffi_test:- cli_memb(string,M),cli_compile_member(M,_Out),fail.
% cffi_test:- cli_memb(int,M),cli_compile_member(M,_Out),fail.

% cffi_test:- cli_compile_enum(int,'MyEnum',['Low'(0),'High'(100)],[],O),display_class(O).


:-dynamic(cdb_definer/3).
% cffi_test:-listing(cdb_definer/3).

end_of_file.

root@titan:/mnt/i7d/swicffi# swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.1.26)
Copyright (c) 1990-2014 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- use_module(library(swicffi)).
ERROR: No assembly found named Swicli.Library
Warning: /usr/lib/swi-prolog/library/swicffi.pl:86:
        Goal (directive) failed: swicffi:cli_load_lib('SWIProlog','Swicli.Library','Swicli.Library.Embedded',install)
ERROR: /usr/lib/swi-prolog/library/swicffi.pl:106:
        catch/3: Undefined procedure: swicffi:cli_load_assembly/1
Warning: /usr/lib/swi-prolog/library/swicffi.pl:106:
        Goal (directive) failed: swicffi:cli_load_assembly('Swicli.Library')
ERROR: /usr/lib/swi-prolog/library/swicffi.pl:8:
        Exported procedure swicffi:cli_new_delegate/3 is not defined
ERROR: /usr/lib/swi-prolog/library/swicffi.pl:8:
        Exported procedure swicffi:cli_add_event_handler/3 is not defined
ERROR: /usr/lib/swi-prolog/library/swicffi.pl:8:
        Exported procedure swicffi:cli_new_delegate_term/4 is not defined
true.

?-
% halt
root@titan:/mnt/i7d/swicffi# . ./
c/                   doc/                 .gitignore           install-linux.sh     lib/                 makeall.bat          make-linux.sh        pack.pl              README.txt
cffi-tests/          .git/                .ignore-on-commit    INSTALL-Windows.txt  local-test.sh        Makefile             mono_sysvars.sh      prolog/              TempAssembly.dll
root@titan:/mnt/i7d/swicffi# . ./mono_sysvars.sh
root@titan:/mnt/i7d/swicffi# swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.1.26)
Copyright (c) 1990-2014 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- use_module(library(swicffi)).
SetupProlog

Cannot install hook ThreadExit to Mono
Swicli.Library.Embedded.install suceeded
true.

?- cli_memb(string,M),cli_compile_member(M,_Out),fail.

