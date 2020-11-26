:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:-set_prolog_flag(double_quotes,string).
:- dynamic(assertion_holds/2),
   multifile(assertion_holds/2),
   dynamic(assertion_holds/3),
    multifile(assertion_holds/3),
   dynamic(assertion_holds/4),
    multifile(assertion_holds/4),
   dynamic(assertion_holds/5),
    multifile(assertion_holds/5),
   dynamic(assertion_holds/6),
    multifile(assertion_holds/6),
   dynamic(assertion_holds/7),
    multifile(assertion_holds/7),
    !.

