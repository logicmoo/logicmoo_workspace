
:- use_module(library(listing)).
:- use_module(library(debug)).
/*
:- listing(prolog_source:file_alias_path/2).
:- prolog_source:build_alias_cache.
*/
:- use_module(library(swicli)).
:- use_module(library(swicffi)).

:- cffi_tests.

% :- cli_compile_type([int],[],"MyType",['Low'(0),'High'(100)],['FlagsAttribute'],O).

end_of_file.

:- load_forms(['in-package',':swi-prolog-ffi']).
:- load_forms(['cffi:define-foreign-library',libpl,[t,[':default',str("libpl")]]]).
:- load_forms(['cffi:use-foreign-library',libswipl]).
:- load_forms([defctype,'atom-t',':unsigned-long',str("Prolog atom")]).
:- load_forms([defctype,'module-t',':pointer',str("Prolog module")]).
:- load_forms([defctype,'predicate-t',':pointer',str("Prolog procedure")]).
:- load_forms([defctype,'term-t',':unsigned-long',str("Opaque Prolog term handle")]).
:- load_forms([defctype,'qid-t',':unsigned-long',str("Opaque Prolog query handle")]).
:- load_forms([defctype,'pl-fid-t',':unsigned-long',str("Opaque foreign context handle")]).
:- load_forms([defctype,'functor-t',':unsigned-long',str("Name/arity pair")]).
:- load_forms([defctype,'foreign-t',':unsigned-long',str("Return type of foreign functions.")]).
:- load_forms([defcfun,[str("PL_new_module"),'pl-new-module'],'module-t',[name,'atom-t']]).
:- load_forms([defcfun,[str("PL_open_foreign_frame"),'pl-open-foreign-frame'],'pl-fid-t']).
:- load_forms([defcfun,[str("PL_close_foreign_frame"),'pl-close-foreign-frame'],':void',[cid,'pl-fid-t']]).
:- load_forms([defcfun,[str("PL_register_foreign"),'pl-register-foreign'],':int',[name,':string'],[arity,':int'],[function,':pointer'],[flag,':int']]).
:- load_forms([defcfun,[str("PL_pred"),'pl-pred'],'predicate-t',[f,'functor-t'],[m,'module-t']]).
:- load_forms([defcfun,[str("PL_predicate"),'pl-predicate'],'predicate-t',['predicate-name',':string'],[arity,':int'],['module-name',':string']]).
:- load_forms([defcfun,[str("PL_open_query"),'pl-open-query'],'qid-t',[m,'module-t'],[flags,':int'],[pred,'predicate-t'],[t0,'term-t']]).
:- load_forms([defcfun,[str("PL_next_solution"),'pl-next-solution'],':int',[qid,'qid-t']]).
:- load_forms([defcfun,[str("PL_close_query"),'pl-close-query'],':void',[qid,'qid-t']]).
:- load_forms([defcfun,[str("PL_cut_query"),'pl-cut-query'],':void',[qid,'qid-t']]).
:- load_forms([defcfun,[str("PL_call"),'pl-call'],':int',['arg-t','term-t'],[m,'module-t']]).
:- load_forms([defcfun,[str("PL_call_predicate"),'pl-call-predicate'],':int',[m,'module-t'],[debug,':int'],[pred,'predicate-t'],[t0,'term-t']]).
:- load_forms([defcfun,[str("PL_exception"),'pl-exception'],'term-t',[qid,'qid-t']]).
:- load_forms([defcfun,[str("PL_raise_exception"),'pl-raise-exception'],':int',[exception,'term-t']]).
:- load_forms([defcfun,[str("PL_throw"),'pl-throw'],':int',[exception,'term-t']]).
:- load_forms([defcfun,[str("PL_new_term_refs"),'pl-new-term-refs'],'term-t',[n,':int']]).
:- load_forms([defcfun,[str("PL_new_term_ref"),'pl-new-term-ref'],'term-t']).
:- load_forms([defcfun,[str("PL_copy_term_ref"),'pl-copy-term-ref'],'term-t',[from,'term-t']]).
:- load_forms([defcfun,[str("PL_new_atom"),'pl-new-atom'],'atom-t',[s,':string']]).
:- load_forms([defcfun,[str("PL_atom_chars"),'pl-atom-chars'],':string',[a,'atom-t']]).
:- load_forms([defcfun,[str("PL_new_functor"),'pl-new-functor'],'functor-t',[f,'atom-t'],[a,':int']]).
:- load_forms([defcfun,[str("PL_get_atom"),'pl-get-atom'],':int',['arg-t','term-t'],[a,':pointer']]).
:- load_forms([defcfun,[str("PL_get_bool"),'pl-get-bool'],':int',['arg-t','term-t'],[value,':pointer']]).
:- load_forms([defcfun,[str("PL_get_string"),'pl-get-string'],':int',['arg-t','term-t'],[s,':pointer'],[len,':pointer']]).
:- load_forms([defcfun,[str("PL_get_chars"),'pl-get-chars'],':int',['arg-t','term-t'],[s,':pointer'],[flags,':unsigned-int']]).
:- load_forms([defcfun,[str("PL_get_integer"),'pl-get-integer'],':int',['arg-t','term-t'],[i,':pointer']]).
:- load_forms([defcfun,[str("PL_get_float"),'pl-get-float'],':int',['arg-t','term-t'],[f,':pointer']]).
:- load_forms([defcfun,[str("PL_get_functor"),'pl-get-functor'],':int',['arg-t','term-t'],[f,':pointer']]).
:- load_forms([defcfun,[str("PL_get_name_arity"),'pl-get-name-arity'],':int',['arg-t','term-t'],[name,':pointer'],[arity,':pointer']]).
:- load_forms([defcfun,[str("PL_get_module"),'pl-get-module'],':int',['arg-t','term-t'],[module,':pointer']]).
:- load_forms([defcfun,[str("PL_get_arg"),'pl-get-arg'],':int',[index,':int'],['arg-t','term-t'],[a,'term-t']]).
:- load_forms([defcfun,[str("PL_get_list"),'pl-get-list'],':int',[l,'term-t'],[h,'term-t'],['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_get_head"),'pl-get-head'],':int',[l,'term-t'],[h,'term-t']]).
:- load_forms([defcfun,[str("PL_get_tail"),'pl-get-tail'],':int',[l,'term-t'],[tail,'term-t']]).
:- load_forms([defcfun,[str("PL_get_nil"),'pl-get-nil'],':int',[l,'term-t']]).
:- load_forms([defcfun,[str("PL_term_type"),'pl-term-type'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_variable"),'pl-is-variable'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_ground"),'pl-is-ground'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_atom"),'pl-is-atom'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_integer"),'pl-is-integer'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_string"),'pl-is-string'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_float"),'pl-is-float'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_compound"),'pl-is-compound'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_functor"),'pl-is-functor'],':int',['arg-t','term-t'],[f,'functor-t']]).
:- load_forms([defcfun,[str("PL_is_list"),'pl-is-list'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_atomic"),'pl-is-atomic'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_is_number"),'pl-is-number'],':int',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_put_variable"),'pl-put-variable'],':void',['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_put_atom"),'pl-put-atom'],':void',['arg-t','term-t'],[a,'atom-t']]).
:- load_forms([defcfun,[str("PL_put_atom_chars"),'pl-put-atom-chars'],':void',['arg-t','term-t'],[chars,':string']]).
:- load_forms([defcfun,[str("PL_put_string_chars"),'pl-put-string-chars'],':void',['arg-t','term-t'],[chars,':string']]).
:- load_forms([defcfun,[str("PL_put_integer"),'pl-put-integer'],':void',['arg-t','term-t'],[i,':long']]).
:- load_forms([defcfun,[str("PL_put_float"),'pl-put-float'],':void',['arg-t','term-t'],[f,':double']]).
:- load_forms([defcfun,[str("PL_put_functor"),'pl-put-functor'],':void',['arg-t','term-t'],[functor,'functor-t']]).
:- load_forms([defcfun,[str("PL_put_list"),'pl-put-list'],':void',[l,'term-t']]).
:- load_forms([defcfun,[str("PL_put_nil"),'pl-put-nil'],':void',[l,'term-t']]).
:- load_forms([defcfun,[str("PL_put_term"),'pl-put-term'],':void',[t1,'term-t'],[t2,'term-t']]).
:- load_forms([defcfun,[str("PL_cons_functor_v"),'pl-cons-functor-v'],':void',[h,'term-t'],[fd,'functor-t'],[a0,'term-t']]).
:- load_forms([defcfun,[str("PL_cons_list"),'pl-cons-list'],':void',[l,'term-t'],[h,'term-t'],['arg-t','term-t']]).
:- load_forms([defcfun,[str("PL_initialise"),'pl-initialise'],':int',[argc,':int'],[argv,':pointer']]).
:- load_forms([defcfun,[str("PL_is_initialised"),'pl-is-initialised'],':int',[argc,':pointer'],[argv,':pointer']]).
:- load_forms([defcfun,[str("PL_cleanup"),'pl-cleanup'],':int',[status,':int']]).
:- load_forms([defcfun,[str("PL_halt"),'pl-halt'],':int',[status,':int']]).
:- load_forms([defcfun,[str("PL_query"),'pl-query'],':long',['arg-1',':int']]).

