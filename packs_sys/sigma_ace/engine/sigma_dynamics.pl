% ;; ====================================================================================   
%  ;; As Kernel Predicates are Passed As they Are and only explored the surface-domains   
%  ;; ====================================================================================   
%          ( as-kernel prolog-equal 2 )  
'as-kernel'('prolog-equal', 2).

%   ( as-kernel identity-skolem 2 )  
'as-kernel'('identity-skolem', 2).

%   ( as-kernel resolve_skolem 2 )  
'as-kernel'(resolve_skolem, 2).

%   ( as-kernel true 0 )  
'as-kernel'(true, 0).

%   ( as-kernel false 0 )  
'as-kernel'(false, 0).

%   ( as-kernel fail 0 )  
'as-kernel'(fail, 0).

%   ( as-kernel succeed 0 )  
'as-kernel'(succeed, 0).

%   ( as-kernel trace 0 )  
'as-kernel'(trace, 0).

%   ( as-kernel atom 1 )  
'as-kernel'(atom, 1).

%   ( as-kernel integer 1 )  
'as-kernel'(integer, 1).

%   ( as-kernel number 1 )  
'as-kernel'(number, 1).

%   ( as-kernel atomic 1 )  
'as-kernel'(atomic, 1).

%   ( as-kernel constant 1 )  
'as-kernel'(constant, 1).

%   ( as-kernel functor 3 )  
'as-kernel'(functor, 3).

%   ( as-kernel arg 3 )  
'as-kernel'(arg, 3).

%   ( as-kernel var 1 )  
'as-kernel'(var, 1).

%   ( as-kernel nonvar 1 )  
'as-kernel'(nonvar, 1).

%   ( as-kernel call 1 )  
'as-kernel'(call, 1).

%   ( as-kernel = 2 )  
'as-kernel'(=, 2).

%   ( as-kernel \= 2 )  
%   ( as-kernel == 2 )  
'as-kernel'(==, 2).

%   ( as-kernel \== 2 )  
%   ( as-kernel =\= 2 )  
%   ( as-kernel > 2 )  
'as-kernel'(>, 2).

%   ( as-kernel < 2 )  
'as-kernel'(<, 2).

%   ( as-kernel >= 2 )  
'as-kernel'(>=, 2).

%   ( as-kernel =< 2 )  
'as-kernel'(=<, 2).

%   ( as-kernel dif 2 )  
'as-kernel'(dif, 2).

%   ( as-kernel is 2 )  
'as-kernel'(is, 2).

%   ( as-kernel max 3 )  
'as-kernel'(max, 3).

%   ( as-kernel min 3 )  
'as-kernel'(min, 3).

%   ( as-kernel display 1 )  
'as-kernel'(display, 1).

%   ( as-kernel write 1 )  
'as-kernel'(write, 1).

%   ( as-kernel nl 0 )  
'as-kernel'(nl, 0).

%   ( as-kernel infer_by ?UNK )  
'as-kernel'(infer_by, _G14243).

%   ( as-kernel search_cost ?UNK )  
'as-kernel'(search_cost, _G14315).

%   ( as-kernel test_and_decrement_search_cost ?UNK )  
'as-kernel'(test_and_decrement_search_cost, _G14771).

%   ( as-kernel equal ?UNK )  
'as-kernel'(equal, _G14171).

%   ( as-kernel identical_member ?UNK )  
'as-kernel'(identical_member, _G14435).

%   ( as-kernel unifiable_member ?UNK )  
'as-kernel'(unifiable_member, _G14435).

%   ( as-kernel inc_ncalls 0 )  
'as-kernel'(inc_ncalls, 0).

%   ( as-kernel axioms ?UNK )  
'as-kernel'(axioms, _G14195).

%   ( as-kernel anc_del ?UNK )  
'as-kernel'(anc_del, _G14219).

%    ( as-kernel anc_union ?UNK )  
'as-kernel'(anc_union, _G14279).

%    ( as-kernel anc_subset ?UNK )  
'as-kernel'(anc_subset, _G14303).

%       ;; ====================================================================================   
%  ;;  The compiler must know some taxonimic pairs   
%  ;; ====================================================================================   
%  (taxonomic-pair isa genls)  
'taxonomic-pair'(isa, genls).

%   (taxonomic-pair instance subclass)  
'taxonomic-pair'(instance, subclass).

%   (taxomonic-pair surface-instance surface-subclass)  
'taxomonic-pair'('surface-instance', 'surface-subclass').

%         ;; ========================================================   
%  ;; Compiler Macros   
%  ;; Some Reduction predicates to save Compiler Bandwidth    
%  ;; ========================================================   
%    ;;                                               ;;   
%  ;;                Declarations to Kernel Language         ;;   
%  ;;                                               ;;   
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;;;   KERNEL CORE   
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%    ;(Class context-atom)   
%  ;(Class resource-atom)   
%  ;(Class macro-atom)   
%  ;(Class inference-language-atom)   
%  ;(Class system-known-atom)   
%  ;(Class relational-atom)   
%                                                         ;(subclass context-atom resource-atom)   
%  ;(subclass resource-atom system-known-atom)   
%    ;; If Resource was an object   
%  ;; Resource.Name  as a Resource-atom   
%  ;; Resource.Location as a prolog_file, url_path, kif_file, etc   
%  ;; Resource.resource-state    
%  ;; Resource.availability    
%    ;(instance sigma-kernel resource-atom)   
%  ;(instance sigma-kernel kif-file-resource)   
%  ;(instance sigma-kernel prolog-file-resource)   
%  ;(instance kernel-kb context-atom)   
%  ;(instance context-links-kb context-atom)   
%  ;(instance complier-kb context-atom)   
%  ;(instance default-context context-atom)   
%       ;; If Context was an object:   
%  ;;   
%  ;; Context.Name  as a Context-atom   
%  ;; Context.Resource as a Resource-path   
%  ;; Context.context-state as loaded-unchanged or loaded-change   
%  ;; Context.plarents as ListOf Contexts   
%  ;; Context.Children as ListOf Contexts   
%        ;(instance assert inference-language-atom)   
%  ;(instance retract inference-language-atom)   
%  ;(instance retractAllProlog inference-language-atom)   
%  ;(instance update inference-language-atom)   
%    ;( =>   
%  ;        (instance ?Atom inference-language-atom)   
%  ;        (relation-located ?Atom sigma-kernel)   
%  ;   
%  ;)   
%                                                                                                           ;(instance macro-script relational-atom)   
%  ;(functsymbol macro-script 2)   
%  ;(domain macro-script 1 sigma-prototype-argument)   
%  ;(domain macro-script 2 sigma-expression)   
%       ;;(context-object ?Name ?Resource ?State ?Parents ?Children)   
%    ;;(instance Context.Name Context-atom)   
%  ;;(instance Context.FileName Resource-path)   
%    ;;(instance context-create ContextManagementMacro)   
%  ;;(subclass ContextManagementMacro ImmediateModeMacro)   
%                                                         ;( instance context-create macro-scripts)   
%  ;( instance use-kr macro-scripts)   
%  ;( instance assert macro-scripts)   
%  ;( instance query macro-scripts)   
%  ;( instance save-kr macro-scripts)   
%  ;( instance use-resource macro-scripts)   
%  ;( instance unuse-resource macro-scripts)   
%  ;( instance add-relation macro-scripts)   
%  ;( instance delete-relation macro-scripts)   
%  ;( instance info-relation macro-scripts)   
%  ;( instance list-kr macro-scripts)   
%    ;;;;;;;;;;;;;;;;;;;;;;RELATION LOCATED;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%    (relation-located prolog-equal 2 kernel  (SetFn ?h639  ?h641) prolog-equal  always )  
'relation-located'('prolog-equal', 2, kernel, 'SetFn'(_G16382, _G16388), 'prolog-equal', always).

%   (relation-located identity-skolem 2 kernel  (SetFn ?h639  ?h641) generic_skolem  always )  
'relation-located'('identity-skolem', 2, kernel, 'SetFn'(_G16490, _G16496), generic_skolem, always).

%   (relation-located prolog-eval 2 kernel  (SetFn ?h639  ?h641) prolog-eval  always )  
'relation-located'('prolog-eval', 2, kernel, 'SetFn'(_G16322, _G16328), 'prolog-eval', always).

%   (relation-located sigma-consultation 3 kernel  (SetFn ?h639  ?h641 ?h643) sigma-consultation always )  
'relation-located'('sigma-consultation', 3, kernel, 'SetFn'(_G17061, _G17067, _G17073), 'sigma-consultation', always).

%   (relation-located user-believes 1 kernel  (SetFn ?h639 ) user-believes always )  
'relation-located'('user-believes', 1, kernel, 'SetFn'(_G16003), 'user-believes', always).

%   (relation-located nnf 2 kernel  (SetFn ?h639 ?h639 ) nnf always )  
'relation-located'(nnf, 2, kernel, 'SetFn'(_G15922, _G15922), nnf, always).

%   (relation-located equal 2 kernel  (SetFn ?h639 ?h639 ) prolog-equal always )  
'relation-located'(equal, 2, kernel, 'SetFn'(_G16186, _G16186), 'prolog-equal', always).

%   ;(relation-located greaterThanOrEqualTo 2 kernel  (SetFn ?h639 ?h639 ) greaterThanOrEqualTo always )   
%  ;(relation-located lessThanOrEqualTo 2 kernel  (SetFn ?h639 ?h639 ) lessThanOrEqualTo always )   
%  ;(relation-located greaterThan 2 kernel  (SetFn ?h639 ?h639 ) greaterThan always )   
%  ;(relation-located lessThan 2 kernel  (SetFn ?h639 ?h639 ) lessThan always )   
%    ;(relation-located = 2 kernel  (SetFn ?h639 ?h639 ) prolog-equal always )   
%    ;(instance  the-user-name  consultation-predicate)   
%                                                             ;;(instance Context.State Context-state-atom)   
%  ;;(instance Context.State Context-state-atom)   
%       ;Rule.Context   
%  ;Rule.ExternalTrackingNumber   
%  ;Rule.CompliedForm   
%  ;Rule.Certainty   
%                                                   (  =>          (macro-script  (?Atom) ?Proposition)          (instance ?Atom macro-atom)  )  
'macro-script'(_G17106, _G17112)=>instance(_G17106, 'macro-atom').

%     (  =>          (macro-script  (?Atom ?1 ?2) ?Proposition)          (instance ?Atom macro-atom)  )  
'macro-script'(holds(_G17003, _G16991, _G16997), _G17009)=>instance(_G17003, 'macro-atom').

%     (  =>          (macro-script  (?Atom ?1 ?2 ?3) ?Proposition)          (instance ?Atom macro-atom)  )  
'macro-script'(holds(_G17247, _G17229, _G17235, _G17241), _G17253)=>instance(_G17247, 'macro-atom').

%     (  =>          (macro-script  (?Atom ?1 ?2 ?3 ?4) ?Proposition)          (instance ?Atom macro-atom)  )  
'macro-script'(holds(_G17491, _G17467, _G17473, _G17479, _G17485), _G17497)=>instance(_G17491, 'macro-atom').

%                                                                                                                                         (macro-script   (context-create ?Context-atom)          (and-then                  (assert  resources-kb (instance ?Context-atom  context-atom ) )                   (retractAllProlog  resources-kb  (context-state ?Context-atom  ?? ) )                   (assert  resources-kb  (context-state ?Context-atom  context-state-unloaded))           )  )  
'macro-script'('context-create'(_G24939), 'and-then'(assert('resources-kb', instance(_G24939, 'context-atom')), retractAllProlog('resources-kb', 'context-state'(_G24939, _G24951)), assert('resources-kb', 'context-state'(_G24939, 'context-state-unloaded')))).

%           (macro-script           (use-kr ?Context-atom ?Resource-Path)            (and-then                  (context-create ?Context-atom)                  (file-resource-create ?Resource-Path)          )  )  
'macro-script'('use-kr'(_G18925, _G18931), 'and-then'('context-create'(_G18925), 'file-resource-create'(_G18931))).

%                           (macro-script           (file-resource-create ?Resource-name ?Resource-path)                  (assert  resources-kb  (instance FileName  source-file-path))                   (assert  resources-kb  (context-exists-in-file NewContext-atom  FileName))   )  
'macro-script'('file-resource-create'(_G20745, _G20751), assert('resources-kb', instance('FileName', 'source-file-path')), assert('resources-kb', 'context-exists-in-file'('NewContext-atom', 'FileName'))).

%           
end_of_file.

%     (end-of-file)  
'end-of-file'.

%       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;;                                               ;;   
%  ;;                Bootstrap/INI file                        ;;   
%  ;;                                               ;;   
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%    ;(use-kr kernel-kb 'builtin\kernel-kb.kif')          ; That is here   
%  ;(use-kr compiler-kb 'builtin\compiler-kb.kif')   
%  ;(use-kr inference-kb 'builtin\inference-kb.kif')   
%  ;(use-kr context-links 'builtin\context-links.kif')   
%  ;;;(use-kr merged-ontology-kb 'builtin\merged-ontology-kb.kif')   
%  ;(use-kr universal-kb 'builtin\universal-kb.kif')   
%  ;(use-kr sample-kb 'builtin\sample-kb.kif')   
%  ;(use-kr resources-kb 'resources\resources-kb.kif')   
%  ;(use-resource sigma-user-assisted-inference)   
%                 ;; Context/Rule Management   
%    ;; Imagine a context as an object:   
%    ;; It has these properties:   
%                (=>          (relation-located ?RelationName ?Valence kernel  ?ModeList ?RelationName ?Availabily )          (instance ?RelationName  kernel-atom))  
'relation-located'(_G18247, _G18253, kernel, _G18241, _G18247, _G18235)=>instance(_G18247, 'kernel-atom').

%     (=>          (relation-located ?RelationName ?Valence sigma-user-assisted-inference  ?ModeList ?RelationName ?Availabily )          (instance ?RelationName  consultation-predicate))  
'relation-located'(_G18925, _G18931, 'sigma-user-assisted-inference', _G18919, _G18925, _G18913)=>instance(_G18925, 'consultation-predicate').

%     (=>          (relation-located ?RelationName ?Valence sigma-user-assisted-inference  ?ModeList ?RelationName always )          (instance ?RelationName  consultation-predicate-always))  
'relation-located'(_G18878, _G18884, 'sigma-user-assisted-inference', _G18872, _G18878, always)=>instance(_G18878, 'consultation-predicate-always').

%     (=>          (relation-located ?RelationName ?Valence sigma-user-assisted-inference  ?ModeList ?RelationName after-failure )          (instance ?RelationName  consultation-predicate-after-failure))  
'relation-located'(_G19214, _G19220, 'sigma-user-assisted-inference', _G19208, _G19214, 'after-failure')=>instance(_G19214, 'consultation-predicate-after-failure').

%     ;;;;; A consultation predicate Assertion   
%  (relation-located           user-name 1           sigma-user-assisted-inference (external-agent-must-bind)          get-user-name  once )  
'relation-located'('user-name', 1, 'sigma-user-assisted-inference', 'external-agent-must-bind', 'get-user-name', once).

%     ;;;;   This means if ( user-name ?X ) is being sought by the inference engine    
%  ;;;;   Call the sigma-user-assisted-inference is used with function call get-user-name and it must external-agent-must-bind  the argument   
%  ;;;;   This will only happen once per session   
%  ;;(functsymbol relation-located 5)   
%  ; ;;       ( domain relation-located 1 Relation-atom )   
%  ;  ;;      ( domain relation-located 2 Resource-Modules )   
%  ; ;       ( domainSubclass relation-located 3 Argument-Mode )   
%  ;  ;      ( domain relation-located 4 Relation-atom )   
%  ;        ( domain relation-located 5 Resource-Availabily )   
%      (subclass Argument-List Unmonitored-List)  
subclass('Argument-List', 'Unmonitored-List').

%           ;; Unmonitored lists are structures that only are understood by the predicate using them and cannot be optimally constrained (and therefore are never subject to backchaining or negation this is this is a simular to the understanding that integers are not entities as the same way these are ussualy 'flags' or 'properties' are never existencially quantifiable (DISPARAMS)   
%    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;;( domain relation-located 2 Resource-Modules )   
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;; Five Code Implimented Resource-Modules   
%  ;;(Class Resource-Modules)   
%         ;; hardwired into prolog and cannot be changed   
%    ;;      (instance kernel Resource-Modules)   
%         ;; GAFS and Proven Assertions   
%    ;;      (instance local-assertions Resource-Modules)   
%         ;; Subsumes the above two but has Infenrece as well   
%   ;;       (instance local-inference Resource-Modules)   
%         ;; Built in Consultation with User or Agent    
%   ;;       (instance sigma-user-assisted-inference Resource-Modules)   
%         ;; Built in Consultation with Resouces exported by a wrapped SIGMA Interface  (TODO)   
%   ;;       (instance sigma-resource-module Resource-Modules)   
%            ;; Further one adds RDBMS's, Web Agents and Full Foriegn Applications   
%  ;; To do this:   
%  ;;    
%  ;;   Create a Small .pl file    
%  ;;   In the XSB Prolog manual there are examples of linking the C/C++ or to an ODBC interface   
%  ;;   In the Prolog File Make only one Predicate that will access your resource   
%  ;;   
%  ;;   Put the file in the $InferenceEnginePath/resources the new file   
%  ;;   Assert into the Contexts that you would like to use the external resource:   
%  ;;   (functsymbol ?KRName-Predicate ?VALENCE)    ;;Valence assertions actually act as the only requried marker in a context   
%  ;;   (domain ?KRName-Predicate 1 ?Known-Class)   
%  ;;   (domain ?KRName-Prfedicate 2 ?Known-Class)   
%  ;;            ...etc...   
%  ;;   (relation-located ?KRName-Predicate ?VALENCE ?Name-of-resource ( ?Arg1Argument-Mode ?Arg2Argument-Mode ) ?'Predicate-Name-in-Resource' ?Resource-Availabily)   
%    ;;  Now into the $/resources/resources-kb.kif file add the lines:  (Or make sure SIGMA agent adds this assertion)   
%  ;;   (instance ?Name-of-resource Resource-Modules)   
%  ;;   (instance  ?KRName-Predicate Relation-atom)   
%  ;;   (resource-module ?Name-of-resource filesystem ?RelativePath) or (resource-module ?Name-of-resource sigma-system ?RequestName)   
%  ;;   
%  ;;  The inference Engine is now considering the predicate but cannot use it until the final assertion (actually a call that results in an assertion)   
%  ;;  ( use-resource ?Name-of-resource )   
%  ;;  this will result in this assertion ( resource-state ?Name-of-resource load-defered )   
%  ;;  or possibly ( resource-state ?Name-of-resource cb_error )   
%  ;;   
%  ;;  Try out a call to the predicate    
%  ;;  the resources-kb should now have  ( resource-state ?Name-of-resource loaded )   
%  ;;   
%  ;;   From that point on it will stay loaded until you call  ( unuse-resource ?Name-of-resource )   
%  ;;   Now you will see no assertion of  (resource-state ... ...)   
%  ;;   
%                                                           ;;;;(relation-located between 2 ( at-least-one-other-arg-bound at-least-one-other-arg-bound )    
%      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;;;       Before Calling a Resource Is Used each Argument is checked for Single_bindings     ;;;   
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;; Four code implimented modes   
%       ;(Class Argument-Mode)   
%          ;;( instance must-be-bound-before-call Argument-Mode)   
%          ;;( instance external-agent-must-bind Argument-Mode)   
%          ;;( instance not-monitored Argument-Mode)   
%          ;;( instance at-least-one-other-arg-bound Argument-Mode)   
%      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;;;       Resource Availablity Atoms                         ;;;;;;   
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
%  ;; Four code implimented modes   
%            ;;(Class Resource-Availabily)   
%          ;; Each Iteration Calls The Resource-Modules   
%          ;;( instance always Resource-Availabily)   
%                    ;; No Iteration Calls The Resource-Modules   
%          ;;( instance never Resource-Availabily)   
%                    ;; Only if the minimum Answers has not been reached call the Resource-Modules   
%          ;;( instance after-failure Resource-Availabily)   
%                    ;; Only if the minimum Answers has not been reached call the Resource-Modules (But Once you have received a disp_single_binding set do not ask resource again)   
%          ;; This is how to add a prolog style-cut into the real world!   
%          ;;( instance once Resource-Availabily)   
%                                                         ;;(=>   
%    ;;      (relation-located ?RelationName ?Valence kernel  ?ModeList ?RelationName ?Availabily )   
%   ;;       (functsymbol ?RelationName  ?Valence))   
%          (relation-located isa 2 inferSurfaceer (external-agent-must-bind external-agent-must-bind) inferSurface  always )  
'relation-located'(isa, 2, inferSurfaceer, 'external-agent-must-bind'('external-agent-must-bind'), inferSurface, always).

%   (relation-located genls 2 inferSurfaceer (external-agent-must-bind external-agent-must-bind) inferSurface  always )  
'relation-located'(genls, 2, inferSurfaceer, 'external-agent-must-bind'('external-agent-must-bind'), inferSurface, always).

%   (relation-located inferSurface 1 inferSurfaceer (external-agent-must-bind) inferSurface  always )  
'relation-located'(inferSurface, 1, inferSurfaceer, 'external-agent-must-bind', inferSurface, always).

%   (relation-located ?Pred ?Valence inferSurfaceer ??? inferSurface  always )  
'relation-located'(_G15901, _G15919, inferSurfaceer, _G15913, _G15925, always).

%   (relation-located backward_chain 2 backward_chainer (must-be-bound-before-call external-agent-must-bind) backward_chain  always )  
'relation-located'(backward_chain, 2, backward_chainer, 'must-be-bound-before-call'('external-agent-must-bind'), backward_chain, always).

%   (relation-located ?Pred ?Valence backward_chainer ??? backward_chain  always )  
'relation-located'(_G15949, _G15967, backward_chainer, _G15961, _G15973, always).

%                 (relation-located flush 0 kernel  (EmptySetFn) flush  always )  
'relation-located'(flush, 0, kernel, 'EmptySetFn', flush, always).

%   (relation-located \== 2 kernel  (SetFn ?h640  ?h642) \==  always )  
%   (relation-located =.. 2 kernel  (SetFn ?h640  ?h642) =..  always )  
'relation-located'(=.., 2, kernel, 'SetFn'(_G16150, _G16156), =.., always).

%   (relation-located meta-interpret 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) meta-interpret  always )  
'relation-located'('meta-interpret', 6, kernel, 'SetFn'(_G18174, _G18180, _G18186, _G18192, _G18198, _G18204), 'meta-interpret', always).

%   (relation-located socket-write 1 kernel  (SetFn ?h639) socket-write  always )  
'relation-located'('socket-write', 1, kernel, 'SetFn'(_G15943), 'socket-write', always).

%   (relation-located socket-write 2 kernel  (SetFn ?h640  ?h642) socket-write  always )  
'relation-located'('socket-write', 2, kernel, 'SetFn'(_G16370, _G16376), 'socket-write', always).

%   ;(relation-located - 0 kernel  (EmptySetFn) -  always )   
%  ;(relation-located   2 kernel  (SetFn ?h640  ?h642)    always )   
%  ;(relation-located . 2 kernel  (SetFn ?h639  ?h641) .  always )   
%  (relation-located seeing 1 kernel  (SetFn ?h639) seeing  always )  
'relation-located'(seeing, 1, kernel, 'SetFn'(_G15643), seeing, always).

%   ;(relation-located ; 2 kernel  (SetFn ?h640  ?h642) ;  always )   
%  (relation-located < 2 kernel  (SetFn ?h640  ?h642) <  always )  
'relation-located'(<, 2, kernel, 'SetFn'(_G15946, _G15952), <, always).

%   (relation-located = 2 kernel  (SetFn ?h640  ?h642) =  always )  
'relation-located'(=, 2, kernel, 'SetFn'(_G15958, _G15964), =, always).

%   (relation-located > 2 kernel  (SetFn ?h640  ?h642) >  always )  
'relation-located'(>, 2, kernel, 'SetFn'(_G15958, _G15964), >, always).

%   ;(relation-located same=.. 2 kernel  (SetFn ?h640  ?h642) same=..  always )   
%  (relation-located C 3 kernel  (SetFn ?h641  ?h643  ?h645) C  always )  
'relation-located'('C', 3, kernel, 'SetFn'(_G16257, _G16263, _G16269), 'C', always).

%   (relation-located module-property 2 kernel  (SetFn ?h640  ?h642) module-property  always )  
'relation-located'('module-property', 2, kernel, 'SetFn'(_G16514, _G16520), 'module-property', always).

%   (relation-located read-clause 1 kernel  (SetFn ?h639) read-clause  always )  
'relation-located'('read-clause', 1, kernel, 'SetFn'(_G15895), 'read-clause', always).

%   ;(relation-located same 2 kernel  (SetFn ?h640  ?h642) same  always )   
%  (relation-located a 1 kernel  (SetFn ?h639) a  always )  
'relation-located'(a, 1, kernel, 'SetFn'(_G15403), a, always).

%     (relation-located c 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) c  always )  
'relation-located'(c, 4, kernel, 'SetFn'(_G16726, _G16732, _G16738, _G16744), c, always).

%   (relation-located socket-accept 2 kernel  (SetFn ?h640  ?h642) socket-accept  always )  
'relation-located'('socket-accept', 2, kernel, 'SetFn'(_G16418, _G16424), 'socket-accept', always).

%   (relation-located socket-accept 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-accept  always )  
'relation-located'('socket-accept', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'socket-accept', always).

%   (relation-located close-list 1 kernel  (SetFn ?h639) close-list  always )  
'relation-located'('close-list', 1, kernel, 'SetFn'(_G15847), 'close-list', always).

%   (relation-located unrestricted 2 kernel  (SetFn ?h640  ?h642) unrestricted  always )  
'relation-located'(unrestricted, 2, kernel, 'SetFn'(_G16370, _G16376), unrestricted, always).

%   (relation-located current-functor 1 kernel  (SetFn ?h639) current-functor  always )  
'relation-located'('current-functor', 1, kernel, 'SetFn'(_G16087), 'current-functor', always).

%   (relation-located current-functor 2 kernel  (SetFn ?h640  ?h642) current-functor  always )  
'relation-located'('current-functor', 2, kernel, 'SetFn'(_G16514, _G16520), 'current-functor', always).

%   (relation-located invert 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) invert  always )  
'relation-located'(invert, 6, kernel, 'SetFn'(_G17790, _G17796, _G17802, _G17808, _G17814, _G17820), invert, always).

%   (relation-located get-word-chars 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) get-word-chars  always )  
'relation-located'('get-word-chars', 4, kernel, 'SetFn'(_G17320, _G17326, _G17332, _G17338), 'get-word-chars', always).

%   (relation-located create-legal-gafs 2 kernel  (SetFn ?h640  ?h642) create-legal-gafs  always )  
'relation-located'('create-legal-gafs', 2, kernel, 'SetFn'(_G16610, _G16616), 'create-legal-gafs', always).

%   (relation-located xsb-configuration 2 kernel  (SetFn ?h640  ?h642) xsb-configuration  always )  
'relation-located'('xsb-configuration', 2, kernel, 'SetFn'(_G16610, _G16616), 'xsb-configuration', always).

%   (relation-located nonvar-backward-chain 3 kernel  (SetFn ?h641  ?h643  ?h645) nonvar-backward-chain  always )  
'relation-located'('nonvar-backward-chain', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'nonvar-backward-chain', always).

%   (relation-located sigma-console 0 kernel  (EmptySetFn) sigma-console  always )  
'relation-located'('sigma-console', 0, kernel, 'EmptySetFn', 'sigma-console', always).

%   (relation-located =:= 2 kernel  (SetFn ?h640  ?h642) =:=  always )  
'relation-located'(=:=, 2, kernel, 'SetFn'(_G16150, _G16156), =:=, always).

%   (relation-located tsetof 3 kernel  (SetFn ?h641  ?h643  ?h645) tsetof  always )  
'relation-located'(tsetof, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), tsetof, always).

%   (relation-located hilog-arg 3 kernel  (SetFn ?h641  ?h643  ?h645) hilog-arg  always )  
'relation-located'('hilog-arg', 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), 'hilog-arg', always).

%   (relation-located sigma-A-do-retract-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) sigma-A-do-retract-list  always )  
'relation-located'('sigma-A-do-retract-list', 4, kernel, 'SetFn'(_G17752, _G17758, _G17764, _G17770), 'sigma-A-do-retract-list', always).

%   (relation-located replace-nth 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) replace-nth  always )  
'relation-located'('replace-nth', 4, kernel, 'SetFn'(_G17176, _G17182, _G17188, _G17194), 'replace-nth', always).

%   (relation-located hilog-symbol 1 kernel  (SetFn ?h639) hilog-symbol  always )  
'relation-located'('hilog-symbol', 1, kernel, 'SetFn'(_G15943), 'hilog-symbol', always).

%   (relation-located hex-digit 2 kernel  (SetFn ?h640  ?h642) hex-digit  always )  
'relation-located'('hex-digit', 2, kernel, 'SetFn'(_G16226, _G16232), 'hex-digit', always).

%   (relation-located file-read-line-atom 2 kernel  (SetFn ?h640  ?h642) file-read-line-atom  always )  
'relation-located'('file-read-line-atom', 2, kernel, 'SetFn'(_G16706, _G16712), 'file-read-line-atom', always).

%   (relation-located file-read-line-atom 3 kernel  (SetFn ?h641  ?h643  ?h645) file-read-line-atom  always )  
'relation-located'('file-read-line-atom', 3, kernel, 'SetFn'(_G17133, _G17139, _G17145), 'file-read-line-atom', always).

%   (relation-located ua-context-create 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-context-create  always )  
'relation-located'('ua-context-create', 6, kernel, 'SetFn'(_G18318, _G18324, _G18330, _G18336, _G18342, _G18348), 'ua-context-create', always).

%   (relation-located digit 1 kernel  (SetFn ?h639) digit  always )  
'relation-located'(digit, 1, kernel, 'SetFn'(_G15607), digit, always).

%   (relation-located ua-context-info 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) ua-context-info  always )  
'relation-located'('ua-context-info', 5, kernel, 'SetFn'(_G17795, _G17801, _G17807, _G17813, _G17819), 'ua-context-info', always).

%   (relation-located sigma-U-setup-confirmation 1 kernel  (SetFn ?h639) sigma-U-setup-confirmation  always )  
'relation-located'('sigma-U-setup-confirmation', 1, kernel, 'SetFn'(_G16615), 'sigma-U-setup-confirmation', always).

%   (relation-located tphrase-set-string 1 kernel  (SetFn ?h639) tphrase-set-string  always )  
'relation-located'('tphrase-set-string', 1, kernel, 'SetFn'(_G16231), 'tphrase-set-string', always).

%   (relation-located sigma-P-chars-to-term 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-P-chars-to-term  always )  
'relation-located'('sigma-P-chars-to-term', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'sigma-P-chars-to-term', always).

%   (relation-located file-read-line-list 2 kernel  (SetFn ?h640  ?h642) file-read-line-list  always )  
'relation-located'('file-read-line-list', 2, kernel, 'SetFn'(_G16706, _G16712), 'file-read-line-list', always).

%   (relation-located flag-list 3 kernel  (SetFn ?h641  ?h643  ?h645) flag-list  always )  
'relation-located'('flag-list', 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), 'flag-list', always).

%   (relation-located file-read-line-list 3 kernel  (SetFn ?h641  ?h643  ?h645) file-read-line-list  always )  
'relation-located'('file-read-line-list', 3, kernel, 'SetFn'(_G17133, _G17139, _G17145), 'file-read-line-list', always).

%   (relation-located bagof 3 kernel  (SetFn ?h641  ?h643  ?h645) bagof  always )  
'relation-located'(bagof, 3, kernel, 'SetFn'(_G16461, _G16467, _G16473), bagof, always).

%   (relation-located socket-set-option 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-set-option  always )  
'relation-located'('socket-set-option', 3, kernel, 'SetFn'(_G17037, _G17043, _G17049), 'socket-set-option', always).

%   (relation-located integer 1 kernel  (SetFn ?h639) integer  always )  
'relation-located'(integer, 1, kernel, 'SetFn'(_G15703), integer, always).

%   (relation-located ua-context-link 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-context-link  always )  
'relation-located'('ua-context-link', 6, kernel, 'SetFn'(_G18222, _G18228, _G18234, _G18240, _G18246, _G18252), 'ua-context-link', always).

%   (relation-located socket-set-select 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) socket-set-select  always )  
'relation-located'('socket-set-select', 4, kernel, 'SetFn'(_G17464, _G17470, _G17476, _G17482), 'socket-set-select', always).

%   (relation-located check-legal-modes 3 kernel  (SetFn ?h641  ?h643  ?h645) check-legal-modes  always )  
'relation-located'('check-legal-modes', 3, kernel, 'SetFn'(_G17037, _G17043, _G17049), 'check-legal-modes', always).

%   (relation-located instanc 2 kernel  (SetFn ?h640  ?h642) instanc  always )  
'relation-located'(instanc, 2, kernel, 'SetFn'(_G16130, _G16136), instanc, always).

%   (relation-located sigma 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma  always )  
'relation-located'(sigma, 3, kernel, 'SetFn'(_G16461, _G16467, _G16473), sigma, always).

%   (relation-located skip 1 kernel  (SetFn ?h639) skip  always )  
'relation-located'(skip, 1, kernel, 'SetFn'(_G15559), skip, always).

%   (relation-located compile-quantitatives-deterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-quantitatives-deterministicly  always )  
'relation-located'('compile-quantitatives-deterministicly', 3, kernel, 'SetFn'(_G17997, _G18003, _G18009), 'compile-quantitatives-deterministicly', always).

%   (relation-located nonvar-backwards-chain 3 kernel  (SetFn ?h641  ?h643  ?h645) nonvar-backwards-chain  always )  
'relation-located'('nonvar-backwards-chain', 3, kernel, 'SetFn'(_G17277, _G17283, _G17289), 'nonvar-backwards-chain', always).

%   (relation-located compile-quantitatives-nondeterministicly 2 kernel  (SetFn ?h640  ?h642) compile-quantitatives-nondeterministicly  always )  
'relation-located'('compile-quantitatives-nondeterministicly', 2, kernel, 'SetFn'(_G17714, _G17720), 'compile-quantitatives-nondeterministicly', always).

%   (relation-located compile-quantitatives-nondeterministicly-arg 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-quantitatives-nondeterministicly-arg  always )  
'relation-located'('compile-quantitatives-nondeterministicly-arg', 3, kernel, 'SetFn'(_G18333, _G18339, _G18345), 'compile-quantitatives-nondeterministicly-arg', always).

%   (relation-located compile-quantitatives-nondeterministicly-list 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-quantitatives-nondeterministicly-list  always )  
'relation-located'('compile-quantitatives-nondeterministicly-list', 3, kernel, 'SetFn'(_G18381, _G18387, _G18393), 'compile-quantitatives-nondeterministicly-list', always).

%   (relation-located client-terminate 0 kernel  (EmptySetFn) client-terminate  always )  
'relation-located'('client-terminate', 0, kernel, 'EmptySetFn', 'client-terminate', always).

%   (relation-located nodisp_debug 0 kernel  (EmptySetFn) nodisp_debug  always )  
'relation-located'(nodisp_debug, 0, kernel, 'EmptySetFn', nodisp_debug, always).

%   (relation-located statistics 0 kernel  (EmptySetFn) statistics  always )  
'relation-located'(statistics, 0, kernel, 'EmptySetFn', statistics, always).

%   (relation-located statistics 1 kernel  (SetFn ?h639) statistics  always )  
'relation-located'(statistics, 1, kernel, 'SetFn'(_G15847), statistics, always).

%   (relation-located inform-single_bindings 2 kernel  (SetFn ?h640  ?h642) inform-single_bindings  always )  
'relation-located'('inform-single_bindings', 2, kernel, 'SetFn'(_G16850, _G16856), 'inform-single_bindings', always).

%   (relation-located check-legal-types 3 kernel  (SetFn ?h641  ?h643  ?h645) check-legal-types  always )  
'relation-located'('check-legal-types', 3, kernel, 'SetFn'(_G17037, _G17043, _G17049), 'check-legal-types', always).

%   (relation-located check-result 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) check-result  always )  
'relation-located'('check-result', 6, kernel, 'SetFn'(_G18078, _G18084, _G18090, _G18096, _G18102, _G18108), 'check-result', always).

%   (relation-located ua-retractAllProlog 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-retractAllProlog  always )  
'relation-located'('ua-retractAllProlog', 6, kernel, 'SetFn'(_G18126, _G18132, _G18138, _G18144, _G18150, _G18156), 'ua-retractAllProlog', always).

%   (relation-located verify-assertion 2 kernel  (SetFn ?h640  ?h642) verify-assertion  always )  
'relation-located'('verify-assertion', 2, kernel, 'SetFn'(_G16562, _G16568), 'verify-assertion', always).

%   (relation-located callable 1 kernel  (SetFn ?h639) callable  always )  
'relation-located'(callable, 1, kernel, 'SetFn'(_G15751), callable, always).

%   (relation-located sigma-H-assert 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-assert  always )  
'relation-located'('sigma-H-assert', 3, kernel, 'SetFn'(_G16893, _G16899, _G16905), 'sigma-H-assert', always).

%   (relation-located sigma-X-assert 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-X-assert  always )  
'relation-located'('sigma-X-assert', 5, kernel, 'SetFn'(_G17747, _G17753, _G17759, _G17765, _G17771), 'sigma-X-assert', always).

%   (relation-located sigma-A-do-update-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) sigma-A-do-update-list  always )  
'relation-located'('sigma-A-do-update-list', 4, kernel, 'SetFn'(_G17704, _G17710, _G17716, _G17722), 'sigma-A-do-update-list', always).

%   (relation-located sigma-H-resource-info 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-resource-info  always )  
'relation-located'('sigma-H-resource-info', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'sigma-H-resource-info', always).

%   (relation-located sigma-X-retract-new-1 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-X-retract-new-1  always )  
'relation-located'('sigma-X-retract-new-1', 5, kernel, 'SetFn'(_G18179, _G18185, _G18191, _G18197, _G18203), 'sigma-X-retract-new-1', always).

%   (relation-located =\= 2 kernel  (SetFn ?h640  ?h642) =\=  always )  
%   (relation-located list-forall-clauses 1 kernel  (SetFn ?h639) list-forall-clauses  always )  
'relation-located'('list-forall-clauses', 1, kernel, 'SetFn'(_G16279), 'list-forall-clauses', always).

%   (relation-located ttywrite 1 kernel  (SetFn ?h639) ttywrite  always )  
'relation-located'(ttywrite, 1, kernel, 'SetFn'(_G15751), ttywrite, always).

%   ;(relation-located -> 2 kernel  (SetFn ?h640  ?h642) ->  always )   
%  (relation-located html-foot 0 kernel  (EmptySetFn) html-foot  always )  
'relation-located'('html-foot', 0, kernel, 'EmptySetFn', 'html-foot', always).

%   (relation-located socket-writeln 1 kernel  (SetFn ?h639) socket-writeln  always )  
'relation-located'('socket-writeln', 1, kernel, 'SetFn'(_G16039), 'socket-writeln', always).

%   (relation-located socket-writeln 2 kernel  (SetFn ?h640  ?h642) socket-writeln  always )  
'relation-located'('socket-writeln', 2, kernel, 'SetFn'(_G16466, _G16472), 'socket-writeln', always).

%   (relation-located generate-language 2 kernel  (SetFn ?h640  ?h642) generate-language  always )  
'relation-located'('generate-language', 2, kernel, 'SetFn'(_G16610, _G16616), 'generate-language', always).

%   (relation-located sigma-H-resource-delete 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-resource-delete  always )  
'relation-located'('sigma-H-resource-delete', 3, kernel, 'SetFn'(_G17325, _G17331, _G17337), 'sigma-H-resource-delete', always).

%   (relation-located sigma-H-resource-create 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-resource-create  always )  
'relation-located'('sigma-H-resource-create', 3, kernel, 'SetFn'(_G17325, _G17331, _G17337), 'sigma-H-resource-create', always).

%   (relation-located retractAllProlog 1 kernel  (SetFn ?h639) retractAllProlog  always )  
'relation-located'(retractAllProlog, 1, kernel, 'SetFn'(_G15847), retractAllProlog, always).

%   (relation-located sigma-H-optional-setting 2 kernel  (SetFn ?h640  ?h642) sigma-H-optional-setting  always )  
'relation-located'('sigma-H-optional-setting', 2, kernel, 'SetFn'(_G16946, _G16952), 'sigma-H-optional-setting', always).

%   (relation-located compile-predicates-deterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-predicates-deterministicly  always )  
'relation-located'('compile-predicates-deterministicly', 3, kernel, 'SetFn'(_G17853, _G17859, _G17865), 'compile-predicates-deterministicly', always).

%   (relation-located compile-predicates-nondeterministicly-list 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-predicates-nondeterministicly-list  always )  
'relation-located'('compile-predicates-nondeterministicly-list', 3, kernel, 'SetFn'(_G18237, _G18243, _G18249), 'compile-predicates-nondeterministicly-list', always).

%   (relation-located compile-predicates-nondeterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-predicates-nondeterministicly  always )  
'relation-located'('compile-predicates-nondeterministicly', 3, kernel, 'SetFn'(_G17997, _G18003, _G18009), 'compile-predicates-nondeterministicly', always).

%   (relation-located file-open 3 kernel  (SetFn ?h641  ?h643  ?h645) file-open  always )  
'relation-located'('file-open', 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), 'file-open', always).

%   (relation-located getCputime 1 kernel  (SetFn ?h639) getCputime  always )  
'relation-located'(getCputime, 1, kernel, 'SetFn'(_G15703), getCputime, always).

%   (relation-located =< 2 kernel  (SetFn ?h640  ?h642) =<  always )  
'relation-located'(=<, 2, kernel, 'SetFn'(_G16114, _G16120), =<, always).

%   (relation-located == 2 kernel  (SetFn ?h640  ?h642) ==  always )  
'relation-located'(==, 2, kernel, 'SetFn'(_G16114, _G16120), ==, always).

%   (relation-located @=< 2 kernel  (SetFn ?h640  ?h642) @=<  always )  
'relation-located'(@=<, 2, kernel, 'SetFn'(_G16150, _G16156), @=<, always).

%   (relation-located tokenize 2 kernel  (SetFn ?h640  ?h642) tokenize  always )  
'relation-located'(tokenize, 2, kernel, 'SetFn'(_G16178, _G16184), tokenize, always).

%   (relation-located number-chars 2 kernel  (SetFn ?h640  ?h642) number-chars  always )  
'relation-located'('number-chars', 2, kernel, 'SetFn'(_G16370, _G16376), 'number-chars', always).

%   (relation-located number-codes 2 kernel  (SetFn ?h640  ?h642) number-codes  always )  
'relation-located'('number-codes', 2, kernel, 'SetFn'(_G16370, _G16376), 'number-codes', always).

%   (relation-located >= 2 kernel  (SetFn ?h640  ?h642) >=  always )  
'relation-located'(>=, 2, kernel, 'SetFn'(_G16114, _G16120), >=, always).

%   (relation-located @>= 2 kernel  (SetFn ?h640  ?h642) @>=  always )  
'relation-located'(@>=, 2, kernel, 'SetFn'(_G16150, _G16156), @>=, always).

%   (relation-located socket-listen 2 kernel  (SetFn ?h640  ?h642) socket-listen  always )  
'relation-located'('socket-listen', 2, kernel, 'SetFn'(_G16418, _G16424), 'socket-listen', always).

%   (relation-located compile-clause-rewrites-deterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-clause-rewrites-deterministicly  always )  
'relation-located'('compile-clause-rewrites-deterministicly', 3, kernel, 'SetFn'(_G18093, _G18099, _G18105), 'compile-clause-rewrites-deterministicly', always).

%   (relation-located compile-clause-rewrites-deterministicly-q 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-clause-rewrites-deterministicly-q  always )  
'relation-located'('compile-clause-rewrites-deterministicly-q', 3, kernel, 'SetFn'(_G18189, _G18195, _G18201), 'compile-clause-rewrites-deterministicly-q', always).

%   (relation-located socket-listen 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-listen  always )  
'relation-located'('socket-listen', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'socket-listen', always).

%   (relation-located compile-clause-rewrites-nondeterministicly-q 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-clause-rewrites-nondeterministicly-q  always )  
'relation-located'('compile-clause-rewrites-nondeterministicly-q', 3, kernel, 'SetFn'(_G18333, _G18339, _G18345), 'compile-clause-rewrites-nondeterministicly-q', always).

%   (relation-located compile-clause-rewrites-nondeterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-clause-rewrites-nondeterministicly  always )  
'relation-located'('compile-clause-rewrites-nondeterministicly', 3, kernel, 'SetFn'(_G18237, _G18243, _G18249), 'compile-clause-rewrites-nondeterministicly', always).

%   (relation-located sigma-X-retractAllProlog-new-1 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-X-retractAllProlog-new-1  always )  
'relation-located'('sigma-X-retractAllProlog-new-1', 5, kernel, 'SetFn'(_G18323, _G18329, _G18335, _G18341, _G18347), 'sigma-X-retractAllProlog-new-1', always).

%   (relation-located @< 2 kernel  (SetFn ?h640  ?h642) @<  always )  
'relation-located'(@<, 2, kernel, 'SetFn'(_G16114, _G16120), @<, always).

%   (relation-located @> 2 kernel  (SetFn ?h640  ?h642) @>  always )  
'relation-located'(@>, 2, kernel, 'SetFn'(_G16114, _G16120), @>, always).

%   (relation-located atom-codes 2 kernel  (SetFn ?h640  ?h642) atom-codes  always )  
'relation-located'('atom-codes', 2, kernel, 'SetFn'(_G16274, _G16280), 'atom-codes', always).

%   (relation-located html-head 1 kernel  (SetFn ?h639) html-head  always )  
'relation-located'('html-head', 1, kernel, 'SetFn'(_G15799), 'html-head', always).

%   (relation-located sigma-H-restart-inference-kb 0 kernel  (EmptySetFn) sigma-H-restart-inference-kb  always )  
'relation-located'('sigma-H-restart-inference-kb', 0, kernel, 'EmptySetFn', 'sigma-H-restart-inference-kb', always).

%   (relation-located optional-bound 2 kernel  (SetFn ?h640  ?h642) optional-bound  always )  
'relation-located'('optional-bound', 2, kernel, 'SetFn'(_G16466, _G16472), 'optional-bound', always).

%   (relation-located ua-context-remove 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) ua-context-remove  always )  
'relation-located'('ua-context-remove', 5, kernel, 'SetFn'(_G17891, _G17897, _G17903, _G17909, _G17915), 'ua-context-remove', always).

%   (relation-located pterm-to-sterm-list 2 kernel  (SetFn ?h640  ?h642) pterm-to-sterm-list  always )  
'relation-located'('pterm-to-sterm-list', 2, kernel, 'SetFn'(_G16706, _G16712), 'pterm-to-sterm-list', always).

%   (relation-located instantiate 2 kernel  (SetFn ?h640  ?h642) instantiate  always )  
'relation-located'(instantiate, 2, kernel, 'SetFn'(_G16322, _G16328), instantiate, always).

%   (relation-located sterm-to-pterm-list 2 kernel  (SetFn ?h640  ?h642) sterm-to-pterm-list  always )  
'relation-located'('sterm-to-pterm-list', 2, kernel, 'SetFn'(_G16706, _G16712), 'sterm-to-pterm-list', always).

%   (relation-located once 1 kernel  (SetFn ?h639) once  always )  
'relation-located'(once, 1, kernel, 'SetFn'(_G15559), once, always).

%   (relation-located socket-select 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) socket-select  always )  
'relation-located'('socket-select', 6, kernel, 'SetFn'(_G18126, _G18132, _G18138, _G18144, _G18150, _G18156), 'socket-select', always).

%   (relation-located coll-white 2 kernel  (SetFn ?h640  ?h642) coll-white  always )  
'relation-located'('coll-white', 2, kernel, 'SetFn'(_G16274, _G16280), 'coll-white', always).

%   (relation-located log-ith 3 kernel  (SetFn ?h641  ?h643  ?h645) log-ith  always )  
'relation-located'('log-ith', 3, kernel, 'SetFn'(_G16557, _G16563, _G16569), 'log-ith', always).

%   (relation-located store-assertion 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) store-assertion  always )  
'relation-located'('store-assertion', 4, kernel, 'SetFn'(_G17368, _G17374, _G17380, _G17386), 'store-assertion', always).

%   (relation-located file-exists 1 kernel  (SetFn ?h639) file-exists  always )  
'relation-located'('file-exists', 1, kernel, 'SetFn'(_G15895), 'file-exists', always).

%   (relation-located constant 1 kernel  (SetFn ?h639) constant  always )  
'relation-located'(constant, 1, kernel, 'SetFn'(_G15751), constant, always).

%   (relation-located coll-white2 2 kernel  (SetFn ?h640  ?h642) coll-white2  always )  
'relation-located'('coll-white2', 2, kernel, 'SetFn'(_G16418, _G16424), 'coll-white2', always).

%   (relation-located coll-white3 2 kernel  (SetFn ?h640  ?h642) coll-white3  always )  
'relation-located'('coll-white3', 2, kernel, 'SetFn'(_G16418, _G16424), 'coll-white3', always).

%   (relation-located reconsult 1 kernel  (SetFn ?h639) reconsult  always )  
'relation-located'(reconsult, 1, kernel, 'SetFn'(_G15799), reconsult, always).

%   (relation-located reconsult 2 kernel  (SetFn ?h640  ?h642) reconsult  always )  
'relation-located'(reconsult, 2, kernel, 'SetFn'(_G16226, _G16232), reconsult, always).

%   (relation-located never-retractAllProlog 1 kernel  (SetFn ?h639) never-retractAllProlog  always )  
'relation-located'('never-retractAllProlog', 1, kernel, 'SetFn'(_G16135), 'never-retractAllProlog', always).

%   (relation-located load-dync 1 kernel  (SetFn ?h639) load-dync  always )  
'relation-located'('load-dync', 1, kernel, 'SetFn'(_G15799), 'load-dync', always).

%   (relation-located concat-list 3 kernel  (SetFn ?h641  ?h643  ?h645) concat-list  always )  
'relation-located'('concat-list', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'concat-list', always).

%   (relation-located never-assert 1 kernel  (SetFn ?h639) never-assert  always )  
'relation-located'('never-assert', 1, kernel, 'SetFn'(_G15943), 'never-assert', always).

%   (relation-located conv-det 2 kernel  (SetFn ?h640  ?h642) conv-det  always )  
'relation-located'('conv-det', 2, kernel, 'SetFn'(_G16178, _G16184), 'conv-det', always).

%   (relation-located ua-context-unlink 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-context-unlink  always )  
'relation-located'('ua-context-unlink', 6, kernel, 'SetFn'(_G18318, _G18324, _G18330, _G18336, _G18342, _G18348), 'ua-context-unlink', always).

%   (relation-located getPrologVars-splitter 3 kernel  (SetFn ?h641  ?h643  ?h645) getPrologVars-splitter  always )  
'relation-located'('getPrologVars-splitter', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'getPrologVars-splitter', always).

%   (relation-located clear-quit-query 0 kernel  (EmptySetFn) clear-quit-query  always )  
'relation-located'('clear-quit-query', 0, kernel, 'EmptySetFn', 'clear-quit-query', always).

%   (relation-located \+ 1 kernel  (SetFn ?h639) \+  always )  
%   (relation-located \= 2 kernel  (SetFn ?h640  ?h642) \=  always )  
%   (relation-located check-start 2 kernel  (SetFn ?h640  ?h642) check-start  always )  
'relation-located'('check-start', 2, kernel, 'SetFn'(_G16322, _G16328), 'check-start', always).

%   (relation-located clear-dcg-mode 0 kernel  (EmptySetFn) clear-dcg-mode  always )  
'relation-located'('clear-dcg-mode', 0, kernel, 'EmptySetFn', 'clear-dcg-mode', always).

%   (relation-located open-list 2 kernel  (SetFn ?h640  ?h642) open-list  always )  
'relation-located'('open-list', 2, kernel, 'SetFn'(_G16226, _G16232), 'open-list', always).

%   (relation-located infer-branch 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) infer-branch  always )  
'relation-located'('infer-branch', 6, kernel, 'SetFn'(_G18078, _G18084, _G18090, _G18096, _G18102, _G18108), 'infer-branch', always).

%   (relation-located delete 3 kernel  (SetFn ?h641  ?h643  ?h645) delete  always )  
'relation-located'(delete, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), delete, always).

%   (relation-located sigma-H-query 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-query  always )  
'relation-located'('sigma-H-query', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'sigma-H-query', always).

%   (relation-located conv 2 kernel  (SetFn ?h640  ?h642) conv  always )  
'relation-located'(conv, 2, kernel, 'SetFn'(_G15986, _G15992), conv, always).

%   (relation-located recollect-body 2 kernel  (SetFn ?h640  ?h642) recollect-body  always )  
'relation-located'('recollect-body', 2, kernel, 'SetFn'(_G16466, _G16472), 'recollect-body', always).

%   (relation-located hello 0 kernel  (EmptySetFn) hello  always )  
'relation-located'(hello, 0, kernel, 'EmptySetFn', hello, always).

%   (relation-located sigma-C-make-list-q 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-C-make-list-q  always )  
'relation-located'('sigma-C-make-list-q', 5, kernel, 'SetFn'(_G17987, _G17993, _G17999, _G18005, _G18011), 'sigma-C-make-list-q', always).

%   (relation-located copy 2 kernel  (SetFn ?h640  ?h642) copy  always )  
'relation-located'(copy, 2, kernel, 'SetFn'(_G15986, _G15992), copy, always).

%   (relation-located structure 1 kernel  (SetFn ?h639) structure  always )  
'relation-located'(structure, 1, kernel, 'SetFn'(_G15799), structure, always).

%   (relation-located file-read 2 kernel  (SetFn ?h640  ?h642) file-read  always )  
'relation-located'('file-read', 2, kernel, 'SetFn'(_G16226, _G16232), 'file-read', always).

%   (relation-located hilog 1 kernel  (SetFn ?h639) hilog  always )  
'relation-located'(hilog, 1, kernel, 'SetFn'(_G15607), hilog, always).

%   (relation-located sort 2 kernel  (SetFn ?h640  ?h642) sort  always )  
'relation-located'(sort, 2, kernel, 'SetFn'(_G15986, _G15992), sort, always).

%   (relation-located valid 1 kernel  (SetFn ?h639) valid  always )  
'relation-located'(valid, 1, kernel, 'SetFn'(_G15607), valid, always).

%   (relation-located quote-found 3 kernel  (SetFn ?h641  ?h643  ?h645) quote-found  always )  
'relation-located'('quote-found', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'quote-found', always).

%   (relation-located select 3 kernel  (SetFn ?h641  ?h643  ?h645) select  always )  
'relation-located'(select, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), select, always).

%   (relation-located open 3 kernel  (SetFn ?h641  ?h643  ?h645) open  always )  
'relation-located'(open, 3, kernel, 'SetFn'(_G16413, _G16419, _G16425), open, always).

%   (relation-located is 2 kernel  (SetFn ?h640  ?h642) is  always )  
'relation-located'(is, 2, kernel, 'SetFn'(_G15890, _G15896), is, always).

%   (relation-located atom-concat 3 kernel  (SetFn ?h641  ?h643  ?h645) atom-concat  always )  
'relation-located'('atom-concat', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'atom-concat', always).

%   (relation-located fail-if 1 kernel  (SetFn ?h639) fail-if  always )  
'relation-located'('fail-if', 1, kernel, 'SetFn'(_G15703), 'fail-if', always).

%   (relation-located mc 2 kernel  (SetFn ?h640  ?h642) mc  always )  
'relation-located'(mc, 2, kernel, 'SetFn'(_G15890, _G15896), mc, always).

%   (relation-located nl 0 kernel  (EmptySetFn) nl  always )  
'relation-located'(nl, 0, kernel, 'EmptySetFn', nl, always).

%   (relation-located nl 1 kernel  (SetFn ?h639) nl  always )  
'relation-located'(nl, 1, kernel, 'SetFn'(_G15463), nl, always).

%   (relation-located write-prolog 1 kernel  (SetFn ?h639) write-prolog  always )  
'relation-located'('write-prolog', 1, kernel, 'SetFn'(_G15943), 'write-prolog', always).

%   (relation-located write-prolog 2 kernel  (SetFn ?h640  ?h642) write-prolog  always )  
'relation-located'('write-prolog', 2, kernel, 'SetFn'(_G16370, _G16376), 'write-prolog', always).

%   (relation-located op 3 kernel  (SetFn ?h641  ?h643  ?h645) op  always )  
'relation-located'(op, 3, kernel, 'SetFn'(_G16317, _G16323, _G16329), op, always).

%   (relation-located filter-proof 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) filter-proof  always )  
'relation-located'('filter-proof', 5, kernel, 'SetFn'(_G17651, _G17657, _G17663, _G17669, _G17675), 'filter-proof', always).

%   (relation-located system-hash-301 2 kernel  (SetFn ?h640  ?h642) system-hash-301  always )  
'relation-located'('system-hash-301', 2, kernel, 'SetFn'(_G16802, _G16808), 'system-hash-301', always).

%   (relation-located good-response-to 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) good-response-to  always )  
'relation-located'('good-response-to', 4, kernel, 'SetFn'(_G17416, _G17422, _G17428, _G17434), 'good-response-to', always).

%   (relation-located qq 1 kernel  (SetFn ?h639) qq  always )  
'relation-located'(qq, 1, kernel, 'SetFn'(_G15463), qq, always).

%   (relation-located filter 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) filter  always )  
'relation-located'(filter, 5, kernel, 'SetFn'(_G17363, _G17369, _G17375, _G17381, _G17387), filter, always).

%   (relation-located inform-readln 3 kernel  (SetFn ?h641  ?h643  ?h645) inform-readln  always )  
'relation-located'('inform-readln', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'inform-readln', always).

%   (relation-located clean-true 2 kernel  (SetFn ?h640  ?h642) clean-true  always )  
'relation-located'('clean-true', 2, kernel, 'SetFn'(_G16274, _G16280), 'clean-true', always).

%   (relation-located functor0 3 kernel  (SetFn ?h641  ?h643  ?h645) functor0  always )  
'relation-located'(functor0, 3, kernel, 'SetFn'(_G16701, _G16707, _G16713), functor0, always).

%   (relation-located member 2 kernel  (SetFn ?h640  ?h642) member  always )  
'relation-located'(member, 2, kernel, 'SetFn'(_G16082, _G16088), member, always).

%   (relation-located memberchk 2 kernel  (SetFn ?h640  ?h642) memberchk  always )  
'relation-located'(memberchk, 2, kernel, 'SetFn'(_G16226, _G16232), memberchk, always).

%   (relation-located number 1 kernel  (SetFn ?h639) number  always )  
'relation-located'(number, 1, kernel, 'SetFn'(_G15655), number, always).

%   (relation-located tq 0 kernel  (EmptySetFn) tq  always )  
'relation-located'(tq, 0, kernel, 'EmptySetFn', tq, always).

%   (relation-located tq 1 kernel  (SetFn ?h639) tq  always )  
'relation-located'(tq, 1, kernel, 'SetFn'(_G15463), tq, always).

%   (relation-located handle-xml-java-request 1 kernel  (SetFn ?h639) handle-xml-java-request  always )  
'relation-located'('handle-xml-java-request', 1, kernel, 'SetFn'(_G16471), 'handle-xml-java-request', always).

%   (relation-located fail 0 kernel  (EmptySetFn) fail  always )  
'relation-located'(fail, 0, kernel, 'EmptySetFn', fail, always).

%   (relation-located concat 3 kernel  (SetFn ?h641  ?h643  ?h645) concat  always )  
'relation-located'(concat, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), concat, always).

%   (relation-located halt 0 kernel  (EmptySetFn) halt  always )  
'relation-located'(halt, 0, kernel, 'EmptySetFn', halt, always).

%   (relation-located ch-white 1 kernel  (SetFn ?h639) ch-white  always )  
'relation-located'('ch-white', 1, kernel, 'SetFn'(_G15751), 'ch-white', always).

%   (relation-located name 2 kernel  (SetFn ?h640  ?h642) name  always )  
'relation-located'(name, 2, kernel, 'SetFn'(_G15986, _G15992), name, always).

%   (relation-located arg 3 kernel  (SetFn ?h641  ?h643  ?h645) arg  always )  
'relation-located'(arg, 3, kernel, 'SetFn'(_G16365, _G16371, _G16377), arg, always).

%   (relation-located remove 1 kernel  (SetFn ?h639) remove  always )  
'relation-located'(remove, 1, kernel, 'SetFn'(_G15655), remove, always).

%   (relation-located list-to-set 2 kernel  (SetFn ?h640  ?h642) list-to-set  always )  
'relation-located'('list-to-set', 2, kernel, 'SetFn'(_G16322, _G16328), 'list-to-set', always).

%   (relation-located not-forward-chain 2 kernel  (SetFn ?h640  ?h642) not-forward-chain  always )  
'relation-located'('not-forward-chain', 2, kernel, 'SetFn'(_G16610, _G16616), 'not-forward-chain', always).

%   (relation-located e-o-f 1 kernel  (SetFn ?h639) e-o-f  always )  
'relation-located'('e-o-f', 1, kernel, 'SetFn'(_G15607), 'e-o-f', always).

%   (relation-located not-forward-chain 3 kernel  (SetFn ?h641  ?h643  ?h645) not-forward-chain  always )  
'relation-located'('not-forward-chain', 3, kernel, 'SetFn'(_G17037, _G17043, _G17049), 'not-forward-chain', always).

%   (relation-located getPrologVars 3 kernel  (SetFn ?h641  ?h643  ?h645) getPrologVars  always )  
'relation-located'(getPrologVars, 3, kernel, 'SetFn'(_G16413, _G16419, _G16425), getPrologVars, always).

%   (relation-located getPrologVars 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) getPrologVars  always )  
'relation-located'(getPrologVars, 4, kernel, 'SetFn'(_G16840, _G16846, _G16852, _G16858), getPrologVars, always).

%   (relation-located getPrologVars 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) getPrologVars  always )  
'relation-located'(getPrologVars, 5, kernel, 'SetFn'(_G17267, _G17273, _G17279, _G17285, _G17291), getPrologVars, always).

%   (relation-located arg0 3 kernel  (SetFn ?h641  ?h643  ?h645) arg0  always )  
'relation-located'(arg0, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), arg0, always).

%   (relation-located compound 1 kernel  (SetFn ?h639) compound  always )  
'relation-located'(compound, 1, kernel, 'SetFn'(_G15751), compound, always).

%   (relation-located predicate-type 2 kernel  (SetFn ?h640  ?h642) predicate-type  always )  
'relation-located'('predicate-type', 2, kernel, 'SetFn'(_G16466, _G16472), 'predicate-type', always).

%   (relation-located generate-external-tracking-number 2 kernel  (SetFn ?h640  ?h642) generate-external-tracking-number  always )  
'relation-located'('generate-external-tracking-number', 2, kernel, 'SetFn'(_G17378, _G17384), 'generate-external-tracking-number', always).

%   (relation-located bind-var 2 kernel  (SetFn ?h640  ?h642) bind-var  always )  
'relation-located'('bind-var', 2, kernel, 'SetFn'(_G16178, _G16184), 'bind-var', always).

%   (relation-located length 2 kernel  (SetFn ?h640  ?h642) length  always )  
'relation-located'(length, 2, kernel, 'SetFn'(_G16082, _G16088), length, always).

%   (relation-located nthmember 3 kernel  (SetFn ?h641  ?h643  ?h645) nthmember  always )  
'relation-located'(nthmember, 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), nthmember, always).

%   (relation-located argf 3 kernel  (SetFn ?h641  ?h643  ?h645) argf  always )  
'relation-located'(argf, 3, kernel, 'SetFn'(_G16413, _G16419, _G16425), argf, always).

%   (relation-located term-to-list 2 kernel  (SetFn ?h640  ?h642) term-to-list  always )  
'relation-located'('term-to-list', 2, kernel, 'SetFn'(_G16370, _G16376), 'term-to-list', always).

%   (relation-located xml-java-server 2 kernel  (SetFn ?h640  ?h642) xml-java-server  always )  
'relation-located'('xml-java-server', 2, kernel, 'SetFn'(_G16514, _G16520), 'xml-java-server', always).

%   (relation-located detect-cut 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) detect-cut  always )  
'relation-located'('detect-cut', 4, kernel, 'SetFn'(_G17128, _G17134, _G17140, _G17146), 'detect-cut', always).

%   (relation-located disp_debugging 0 kernel  (EmptySetFn) disp_debugging  always )  
'relation-located'(disp_debugging, 0, kernel, 'EmptySetFn', disp_debugging, always).

%   (relation-located short-circuit-TODO 2 kernel  (SetFn ?h640  ?h642) short-circuit-TODO  always )  
'relation-located'('short-circuit-TODO', 2, kernel, 'SetFn'(_G16658, _G16664), 'short-circuit-TODO', always).

%   (relation-located ignore 1 kernel  (SetFn ?h639) ignore  always )  
'relation-located'(ignore, 1, kernel, 'SetFn'(_G15655), ignore, always).

%   (relation-located sigma-P-file-line-writeFmt 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) sigma-P-file-line-writeFmt  always )  
'relation-located'('sigma-P-file-line-writeFmt', 6, kernel, 'SetFn'(_G18654, _G18660, _G18666, _G18672, _G18678, _G18684), 'sigma-P-file-line-writeFmt', always).

%   (relation-located global-increment 1 kernel  (SetFn ?h639) global-increment  always )  
'relation-located'('global-increment', 1, kernel, 'SetFn'(_G16135), 'global-increment', always).

%   (relation-located write-rule 1 kernel  (SetFn ?h639) write-rule  always )  
'relation-located'('write-rule', 1, kernel, 'SetFn'(_G15847), 'write-rule', always).

%   (relation-located nonvar 1 kernel  (SetFn ?h639) nonvar  always )  
'relation-located'(nonvar, 1, kernel, 'SetFn'(_G15655), nonvar, always).

%   (relation-located file-nl 1 kernel  (SetFn ?h639) file-nl  always )  
'relation-located'('file-nl', 1, kernel, 'SetFn'(_G15703), 'file-nl', always).

%   (relation-located never-retract 1 kernel  (SetFn ?h639) never-retract  always )  
'relation-located'('never-retract', 1, kernel, 'SetFn'(_G15991), 'never-retract', always).

%   (relation-located check-end 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) check-end  always )  
'relation-located'('check-end', 4, kernel, 'SetFn'(_G17080, _G17086, _G17092, _G17098), 'check-end', always).

%   (relation-located current-input-port 1 kernel  (SetFn ?h639) current-input-port  always )  
'relation-located'('current-input-port', 1, kernel, 'SetFn'(_G16231), 'current-input-port', always).

%   (relation-located suokif-xsb 2 kernel  (SetFn ?h640  ?h642) suokif-xsb  always )  
'relation-located'('suokif-xsb', 2, kernel, 'SetFn'(_G16274, _G16280), 'suokif-xsb', always).

%   (relation-located is-most-general-term 1 kernel  (SetFn ?h639) is-most-general-term  always )  
'relation-located'('is-most-general-term', 1, kernel, 'SetFn'(_G16327), 'is-most-general-term', always).

%   (relation-located get-token 3 kernel  (SetFn ?h641  ?h643  ?h645) get-token  always )  
'relation-located'('get-token', 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), 'get-token', always).

%   (relation-located cls 0 kernel  (EmptySetFn) cls  always )  
'relation-located'(cls, 0, kernel, 'EmptySetFn', cls, always).

%   (relation-located write-rulep 1 kernel  (SetFn ?h639) write-rulep  always )  
'relation-located'('write-rulep', 1, kernel, 'SetFn'(_G15895), 'write-rulep', always).

%   (relation-located suokif 3 kernel  (SetFn ?h641  ?h643  ?h645) suokif  always )  
'relation-located'(suokif, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), suokif, always).

%   (relation-located predicate-type-funct 2 kernel  (SetFn ?h640  ?h642) predicate-type-funct  always )  
'relation-located'('predicate-type-funct', 2, kernel, 'SetFn'(_G16754, _G16760), 'predicate-type-funct', always).

%   (relation-located assert-new 1 kernel  (SetFn ?h639) assert-new  always )  
'relation-located'('assert-new', 1, kernel, 'SetFn'(_G15847), 'assert-new', always).

%   (relation-located fmt-read 3 kernel  (SetFn ?h641  ?h643  ?h645) fmt-read  always )  
'relation-located'('fmt-read', 3, kernel, 'SetFn'(_G16605, _G16611, _G16617), 'fmt-read', always).

%   (relation-located fmt-read 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) fmt-read  always )  
'relation-located'('fmt-read', 4, kernel, 'SetFn'(_G17032, _G17038, _G17044, _G17050), 'fmt-read', always).

%   (relation-located var-list 2 kernel  (SetFn ?h640  ?h642) var-list  always )  
'relation-located'('var-list', 2, kernel, 'SetFn'(_G16178, _G16184), 'var-list', always).

%   (relation-located int-to-chars 3 kernel  (SetFn ?h641  ?h643  ?h645) int-to-chars  always )  
'relation-located'('int-to-chars', 3, kernel, 'SetFn'(_G16797, _G16803, _G16809), 'int-to-chars', always).

%   (relation-located telling 1 kernel  (SetFn ?h639) telling  always )  
'relation-located'(telling, 1, kernel, 'SetFn'(_G15703), telling, always).

%   (relation-located repeat 0 kernel  (EmptySetFn) repeat  always )  
'relation-located'(repeat, 0, kernel, 'EmptySetFn', repeat, always).

%   (relation-located number-digits 2 kernel  (SetFn ?h640  ?h642) number-digits  always )  
'relation-located'('number-digits', 2, kernel, 'SetFn'(_G16418, _G16424), 'number-digits', always).

%   (relation-located remap-keys 2 kernel  (SetFn ?h640  ?h642) remap-keys  always )  
'relation-located'('remap-keys', 2, kernel, 'SetFn'(_G16274, _G16280), 'remap-keys', always).

%   (relation-located get-term 3 kernel  (SetFn ?h641  ?h643  ?h645) get-term  always )  
'relation-located'('get-term', 3, kernel, 'SetFn'(_G16605, _G16611, _G16617), 'get-term', always).

%   (relation-located ua-relation-add 9 kernel  (SetFn ?h647  ?h649  ?h651  ?h653  ?h655  ?h657  ?h659  ?h661  ?h663) ua-relation-add  always )  
'relation-located'('ua-relation-add', 9, kernel, 'SetFn'(_G19503, _G19509, _G19515, _G19521, _G19527, _G19533, _G19539, _G19545, _G19551), 'ua-relation-add', always).

%   (relation-located parse-filename 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) parse-filename  always )  
'relation-located'('parse-filename', 4, kernel, 'SetFn'(_G17320, _G17326, _G17332, _G17338), 'parse-filename', always).

%   (relation-located var-found 3 kernel  (SetFn ?h641  ?h643  ?h645) var-found  always )  
'relation-located'('var-found', 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), 'var-found', always).

%   (relation-located import 1 kernel  (SetFn ?h639) import  always )  
'relation-located'((import), 1, kernel, 'SetFn'(_G15655), (import), always).

%   (relation-located read-sock-chars 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) read-sock-chars  always )  
'relation-located'('read-sock-chars', 4, kernel, 'SetFn'(_G17368, _G17374, _G17380, _G17386), 'read-sock-chars', always).

%   (relation-located notrace 0 kernel  (EmptySetFn) notrace  always )  
'relation-located'(notrace, 0, kernel, 'EmptySetFn', notrace, always).

%   (relation-located compile 1 kernel  (SetFn ?h639) compile  always )  
'relation-located'(compile, 1, kernel, 'SetFn'(_G15703), compile, always).

%   (relation-located compile 2 kernel  (SetFn ?h640  ?h642) compile  always )  
'relation-located'(compile, 2, kernel, 'SetFn'(_G16130, _G16136), compile, always).

%   (relation-located dynamic 1 kernel  (SetFn ?h639) dynamic  always )  
'relation-located'((dynamic), 1, kernel, 'SetFn'(_G15703), (dynamic), always).

%   (relation-located atom 1 kernel  (SetFn ?h639) atom  always )  
'relation-located'(atom, 1, kernel, 'SetFn'(_G15559), atom, always).

%   (relation-located compare 3 kernel  (SetFn ?h641  ?h643  ?h645) compare  always )  
'relation-located'(compare, 3, kernel, 'SetFn'(_G16557, _G16563, _G16569), compare, always).

%   (relation-located read 1 kernel  (SetFn ?h639) read  always )  
'relation-located'(read, 1, kernel, 'SetFn'(_G15559), read, always).

%   (relation-located read 2 kernel  (SetFn ?h640  ?h642) read  always )  
'relation-located'(read, 2, kernel, 'SetFn'(_G15986, _G15992), read, always).

%   (relation-located url-decode 2 kernel  (SetFn ?h640  ?h642) url-decode  always )  
'relation-located'('url-decode', 2, kernel, 'SetFn'(_G16274, _G16280), 'url-decode', always).

%   (relation-located real 1 kernel  (SetFn ?h639) real  always )  
'relation-located'(real, 1, kernel, 'SetFn'(_G15559), real, always).

%   (relation-located chars-to-term 2 kernel  (SetFn ?h640  ?h642) chars-to-term  always )  
'relation-located'('chars-to-term', 2, kernel, 'SetFn'(_G16418, _G16424), 'chars-to-term', always).

%   (relation-located chars-to-term 3 kernel  (SetFn ?h641  ?h643  ?h645) chars-to-term  always )  
'relation-located'('chars-to-term', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'chars-to-term', always).

%   (relation-located tphrase-string-length 1 kernel  (SetFn ?h639) tphrase-string-length  always )  
'relation-located'('tphrase-string-length', 1, kernel, 'SetFn'(_G16375), 'tphrase-string-length', always).

%   (relation-located flatten1-hash-301 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) flatten1-hash-301  always )  
'relation-located'('flatten1-hash-301', 4, kernel, 'SetFn'(_G17848, _G17854, _G17860, _G17866), 'flatten1-hash-301', always).

%   (relation-located findall 3 kernel  (SetFn ?h641  ?h643  ?h645) findall  always )  
'relation-located'(findall, 3, kernel, 'SetFn'(_G16557, _G16563, _G16569), findall, always).

%   (relation-located url-decode1 2 kernel  (SetFn ?h640  ?h642) url-decode1  always )  
'relation-located'('url-decode1', 2, kernel, 'SetFn'(_G16418, _G16424), 'url-decode1', always).

%   (relation-located length1 3 kernel  (SetFn ?h641  ?h643  ?h645) length1  always )  
'relation-located'(length1, 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), length1, always).

%   (relation-located functor 3 kernel  (SetFn ?h641  ?h643  ?h645) functor  always )  
'relation-located'(functor, 3, kernel, 'SetFn'(_G16557, _G16563, _G16569), functor, always).

%   (relation-located tbagof 3 kernel  (SetFn ?h641  ?h643  ?h645) tbagof  always )  
'relation-located'(tbagof, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), tbagof, always).

%   (relation-located write-xml-java-header 0 kernel  (EmptySetFn) write-xml-java-header  always )  
'relation-located'('write-xml-java-header', 0, kernel, 'EmptySetFn', 'write-xml-java-header', always).

%   (relation-located ua-relation-info 11 kernel  (SetFn ?h649  ?h651  ?h653  ?h655  ?h657  ?h659  ?h661  ?h663  ?h665  ?h667  ?h669) ua-relation-info  always )  
'relation-located'('ua-relation-info', 11, kernel, 'SetFn'(_G20477, _G20483, _G20489, _G20495, _G20501, _G20507, _G20513, _G20519, _G20525, _G20531, _G20537), 'ua-relation-info', always).

%   (relation-located name-value-pairs 3 kernel  (SetFn ?h641  ?h643  ?h645) name-value-pairs  always )  
'relation-located'('name-value-pairs', 3, kernel, 'SetFn'(_G16989, _G16995, _G17001), 'name-value-pairs', always).

%   (relation-located xsb-configuration-hash-301 1 kernel  (SetFn ?h639) xsb-configuration-hash-301  always )  
'relation-located'('xsb-configuration-hash-301', 1, kernel, 'SetFn'(_G16903), 'xsb-configuration-hash-301', always).

%   (relation-located xsb-configuration-hash-302 1 kernel  (SetFn ?h639) xsb-configuration-hash-302  always )  
'relation-located'('xsb-configuration-hash-302', 1, kernel, 'SetFn'(_G16903), 'xsb-configuration-hash-302', always).

%   (relation-located xsb-configuration-hash-303 1 kernel  (SetFn ?h639) xsb-configuration-hash-303  always )  
'relation-located'('xsb-configuration-hash-303', 1, kernel, 'SetFn'(_G16903), 'xsb-configuration-hash-303', always).

%   (relation-located xsb-configuration-hash-304 1 kernel  (SetFn ?h639) xsb-configuration-hash-304  always )  
'relation-located'('xsb-configuration-hash-304', 1, kernel, 'SetFn'(_G16903), 'xsb-configuration-hash-304', always).

%   (relation-located charst 2 kernel  (SetFn ?h640  ?h642) charst  always )  
'relation-located'(charst, 2, kernel, 'SetFn'(_G16082, _G16088), charst, always).

%   (relation-located charst 3 kernel  (SetFn ?h641  ?h643  ?h645) charst  always )  
'relation-located'(charst, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), charst, always).

%   (relation-located tell 1 kernel  (SetFn ?h639) tell  always )  
'relation-located'(tell, 1, kernel, 'SetFn'(_G15559), tell, always).

%   (relation-located clause 1 kernel  (SetFn ?h639) clause  always )  
'relation-located'(clause, 1, kernel, 'SetFn'(_G15655), clause, always).

%   (relation-located clause 2 kernel  (SetFn ?h640  ?h642) clause  always )  
'relation-located'(clause, 2, kernel, 'SetFn'(_G16082, _G16088), clause, always).

%   (relation-located is-charlist 1 kernel  (SetFn ?h639) is-charlist  always )  
'relation-located'('is-charlist', 1, kernel, 'SetFn'(_G15895), 'is-charlist', always).

%   (relation-located is-charlist 2 kernel  (SetFn ?h640  ?h642) is-charlist  always )  
'relation-located'('is-charlist', 2, kernel, 'SetFn'(_G16322, _G16328), 'is-charlist', always).

%   (relation-located eval 2 kernel  (SetFn ?h640  ?h642) eval  always )  
'relation-located'(eval, 2, kernel, 'SetFn'(_G15986, _G15992), eval, always).

%   (relation-located console 0 kernel  (EmptySetFn) console  always )  
'relation-located'(console, 0, kernel, 'EmptySetFn', console, always).

%   (relation-located compile-functions-deterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-functions-deterministicly  always )  
'relation-located'('compile-functions-deterministicly', 3, kernel, 'SetFn'(_G17805, _G17811, _G17817), 'compile-functions-deterministicly', always).

%   (relation-located compile-functions-deterministicly-for-query 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-functions-deterministicly-for-query  always )  
'relation-located'('compile-functions-deterministicly-for-query', 3, kernel, 'SetFn'(_G18285, _G18291, _G18297), 'compile-functions-deterministicly-for-query', always).

%   (relation-located consult-add 1 kernel  (SetFn ?h639) consult-add  always )  
'relation-located'('consult-add', 1, kernel, 'SetFn'(_G15895), 'consult-add', always).

%   (relation-located compile-functions-nondeterministicly-for-query-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) compile-functions-nondeterministicly-for-query-list  always )  
'relation-located'('compile-functions-nondeterministicly-for-query-list', 4, kernel, 'SetFn'(_G19096, _G19102, _G19108, _G19114), 'compile-functions-nondeterministicly-for-query-list', always).

%   (relation-located compile-functions-nondeterministicly-for-query 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) compile-functions-nondeterministicly-for-query  always )  
'relation-located'('compile-functions-nondeterministicly-for-query', 4, kernel, 'SetFn'(_G18856, _G18862, _G18868, _G18874), 'compile-functions-nondeterministicly-for-query', always).

%   (relation-located compile-functions-nondeterministicly-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) compile-functions-nondeterministicly-list  always )  
'relation-located'('compile-functions-nondeterministicly-list', 4, kernel, 'SetFn'(_G18616, _G18622, _G18628, _G18634), 'compile-functions-nondeterministicly-list', always).

%   (relation-located compile-functions-nondeterministicly 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) compile-functions-nondeterministicly  always )  
'relation-located'('compile-functions-nondeterministicly', 4, kernel, 'SetFn'(_G18376, _G18382, _G18388, _G18394), 'compile-functions-nondeterministicly', always).

%   (relation-located consult 1 kernel  (SetFn ?h639) consult  always )  
'relation-located'(consult, 1, kernel, 'SetFn'(_G15703), consult, always).

%   (relation-located consult 2 kernel  (SetFn ?h640  ?h642) consult  always )  
'relation-located'(consult, 2, kernel, 'SetFn'(_G16130, _G16136), consult, always).

%   (relation-located ua-post-confirmation-list 2 kernel  (SetFn ?h640  ?h642) ua-post-confirmation-list  always )  
'relation-located'('ua-post-confirmation-list', 2, kernel, 'SetFn'(_G16994, _G17000), 'ua-post-confirmation-list', always).

%   (relation-located collect-temp-getPrologVars 1 kernel  (SetFn ?h639) collect-temp-getPrologVars  always )  
'relation-located'('collect-temp-getPrologVars', 1, kernel, 'SetFn'(_G16183), 'collect-temp-getPrologVars', always).

%   (relation-located convert 2 kernel  (SetFn ?h640  ?h642) convert  always )  
'relation-located'(convert, 2, kernel, 'SetFn'(_G16130, _G16136), convert, always).

%   (relation-located sigma-console-loop 0 kernel  (EmptySetFn) sigma-console-loop  always )  
'relation-located'('sigma-console-loop', 0, kernel, 'EmptySetFn', 'sigma-console-loop', always).

%   (relation-located pterm-to-sterm 2 kernel  (SetFn ?h640  ?h642) pterm-to-sterm  always )  
'relation-located'('pterm-to-sterm', 2, kernel, 'SetFn'(_G16466, _G16472), 'pterm-to-sterm', always).

%   (relation-located basic 3 kernel  (SetFn ?h641  ?h643  ?h645) basic  always )  
'relation-located'(basic, 3, kernel, 'SetFn'(_G16461, _G16467, _G16473), basic, always).

%   (relation-located cross-reference-tracking 3 kernel  (SetFn ?h641  ?h643  ?h645) cross-reference-tracking  always )  
'relation-located'('cross-reference-tracking', 3, kernel, 'SetFn'(_G17373, _G17379, _G17385), 'cross-reference-tracking', always).

%   (relation-located separator 3 kernel  (SetFn ?h641  ?h643  ?h645) separator  always )  
'relation-located'(separator, 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), separator, always).

%   (relation-located sterm-to-pterm 2 kernel  (SetFn ?h640  ?h642) sterm-to-pterm  always )  
'relation-located'('sterm-to-pterm', 2, kernel, 'SetFn'(_G16466, _G16472), 'sterm-to-pterm', always).

%   (relation-located bootstrap-sitepackage 2 kernel  (SetFn ?h640  ?h642) bootstrap-sitepackage  always )  
'relation-located'('bootstrap-sitepackage', 2, kernel, 'SetFn'(_G16802, _G16808), 'bootstrap-sitepackage', always).

%   (relation-located sigma_X_kb-pred 1 kernel  (SetFn ?h639) sigma_X_kb-pred  always )  
'relation-located'('sigma_X_kb-pred', 1, kernel, 'SetFn'(_G16087), 'sigma_X_kb-pred', always).

%   (relation-located sigma-C-query-based-on-language-2 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) sigma-C-query-based-on-language-2  always )  
'relation-located'('sigma-C-query-based-on-language-2', 4, kernel, 'SetFn'(_G18328, _G18334, _G18340, _G18346), 'sigma-C-query-based-on-language-2', always).

%   (relation-located sigma-C-query-based-on-language 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-C-query-based-on-language  always )  
'relation-located'('sigma-C-query-based-on-language', 5, kernel, 'SetFn'(_G18563, _G18569, _G18575, _G18581, _G18587), 'sigma-C-query-based-on-language', always).

%   (relation-located list-var-symbols 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) list-var-symbols  always )  
'relation-located'('list-var-symbols', 4, kernel, 'SetFn'(_G17416, _G17422, _G17428, _G17434), 'list-var-symbols', always).

%   (relation-located assert-n 1 kernel  (SetFn ?h639) assert-n  always )  
'relation-located'('assert-n', 1, kernel, 'SetFn'(_G15751), 'assert-n', always).

%   (relation-located var-pure 1 kernel  (SetFn ?h639) var-pure  always )  
'relation-located'('var-pure', 1, kernel, 'SetFn'(_G15751), 'var-pure', always).

%   (relation-located assert 1 kernel  (SetFn ?h639) assert  always )  
'relation-located'(assert, 1, kernel, 'SetFn'(_G15655), assert, always).

%   (relation-located compile-ignored-clauses 2 kernel  (SetFn ?h640  ?h642) compile-ignored-clauses  always )  
'relation-located'('compile-ignored-clauses', 2, kernel, 'SetFn'(_G16898, _G16904), 'compile-ignored-clauses', always).

%   (relation-located hilog-functor 3 kernel  (SetFn ?h641  ?h643  ?h645) hilog-functor  always )  
'relation-located'('hilog-functor', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'hilog-functor', always).

%   (relation-located chars-to-term-s 3 kernel  (SetFn ?h641  ?h643  ?h645) chars-to-term-s  always )  
'relation-located'('chars-to-term-s', 3, kernel, 'SetFn'(_G16941, _G16947, _G16953), 'chars-to-term-s', always).

%   (relation-located get-word 3 kernel  (SetFn ?h641  ?h643  ?h645) get-word  always )  
'relation-located'('get-word', 3, kernel, 'SetFn'(_G16605, _G16611, _G16617), 'get-word', always).

%   (relation-located get 1 kernel  (SetFn ?h639) get  always )  
'relation-located'(get, 1, kernel, 'SetFn'(_G15511), get, always).

%   (relation-located catch 1 kernel  (SetFn ?h639) catch  always )  
'relation-located'(catch, 1, kernel, 'SetFn'(_G15607), catch, always).

%   (relation-located catch 3 kernel  (SetFn ?h641  ?h643  ?h645) catch  always )  
'relation-located'(catch, 3, kernel, 'SetFn'(_G16461, _G16467, _G16473), catch, always).

%   (relation-located file-clone 3 kernel  (SetFn ?h641  ?h643  ?h645) file-clone  always )  
'relation-located'('file-clone', 3, kernel, 'SetFn'(_G16701, _G16707, _G16713), 'file-clone', always).

%   (relation-located varsfo 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) varsfo  always )  
'relation-located'(varsfo, 4, kernel, 'SetFn'(_G16936, _G16942, _G16948, _G16954), varsfo, always).

%   (relation-located sigma-H-command 1 kernel  (SetFn ?h639) sigma-H-command  always )  
'relation-located'('sigma-H-command', 1, kernel, 'SetFn'(_G16087), 'sigma-H-command', always).

%   (relation-located sigma-H-command 2 kernel  (SetFn ?h640  ?h642) sigma-H-command  always )  
'relation-located'('sigma-H-command', 2, kernel, 'SetFn'(_G16514, _G16520), 'sigma-H-command', always).

%   (relation-located file-close 1 kernel  (SetFn ?h639) file-close  always )  
'relation-located'('file-close', 1, kernel, 'SetFn'(_G15847), 'file-close', always).

%   (relation-located nospy 1 kernel  (SetFn ?h639) nospy  always )  
'relation-located'(nospy, 1, kernel, 'SetFn'(_G15607), nospy, always).

%   (relation-located clean-wff 2 kernel  (SetFn ?h640  ?h642) clean-wff  always )  
'relation-located'('clean-wff', 2, kernel, 'SetFn'(_G16226, _G16232), 'clean-wff', always).

%   (relation-located ua-relation-delete 9 kernel  (SetFn ?h647  ?h649  ?h651  ?h653  ?h655  ?h657  ?h659  ?h661  ?h663) ua-relation-delete  always )  
'relation-located'('ua-relation-delete', 9, kernel, 'SetFn'(_G19647, _G19653, _G19659, _G19665, _G19671, _G19677, _G19683, _G19689, _G19695), 'ua-relation-delete', always).

%   (relation-located index 2 kernel  (SetFn ?h640  ?h642) index  always )  
'relation-located'(index, 2, kernel, 'SetFn'(_G16034, _G16040), index, always).

%   (relation-located index 3 kernel  (SetFn ?h641  ?h643  ?h645) index  always )  
'relation-located'(index, 3, kernel, 'SetFn'(_G16461, _G16467, _G16473), index, always).

%   (relation-located cutin 3 kernel  (SetFn ?h641  ?h643  ?h645) cutin  always )  
'relation-located'(cutin, 3, kernel, 'SetFn'(_G16461, _G16467, _G16473), cutin, always).

%   (relation-located sigma-P-wff 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-P-wff  always )  
'relation-located'('sigma-P-wff', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'sigma-P-wff', always).

%   (relation-located bracket 2 kernel  (SetFn ?h640  ?h642) bracket  always )  
'relation-located'(bracket, 2, kernel, 'SetFn'(_G16130, _G16136), bracket, always).

%   (relation-located write-name-value-pairs 2 kernel  (SetFn ?h640  ?h642) write-name-value-pairs  always )  
'relation-located'('write-name-value-pairs', 2, kernel, 'SetFn'(_G16850, _G16856), 'write-name-value-pairs', always).

%   (relation-located break 0 kernel  (EmptySetFn) break  always )  
'relation-located'(break, 0, kernel, 'EmptySetFn', break, always).

%   (relation-located system 1 kernel  (SetFn ?h639) system  always )  
'relation-located'(system, 1, kernel, 'SetFn'(_G15655), system, always).

%   (relation-located setof 3 kernel  (SetFn ?h641  ?h643  ?h645) setof  always )  
'relation-located'(setof, 3, kernel, 'SetFn'(_G16461, _G16467, _G16473), setof, always).

%   (relation-located read-canonical 1 kernel  (SetFn ?h639) read-canonical  always )  
'relation-located'('read-canonical', 1, kernel, 'SetFn'(_G16039), 'read-canonical', always).

%   (relation-located store-retraction 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) store-retraction  always )  
'relation-located'('store-retraction', 4, kernel, 'SetFn'(_G17416, _G17422, _G17428, _G17434), 'store-retraction', always).

%   (relation-located compile-atomic-rewrites-deterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-atomic-rewrites-deterministicly  always )  
'relation-located'('compile-atomic-rewrites-deterministicly', 3, kernel, 'SetFn'(_G18093, _G18099, _G18105), 'compile-atomic-rewrites-deterministicly', always).

%   (relation-located segfault-handler 1 kernel  (SetFn ?h639) segfault-handler  always )  
'relation-located'('segfault-handler', 1, kernel, 'SetFn'(_G16135), 'segfault-handler', always).

%   (relation-located compile-atomic-rewrites-nondeterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-atomic-rewrites-nondeterministicly  always )  
'relation-located'('compile-atomic-rewrites-nondeterministicly', 3, kernel, 'SetFn'(_G18237, _G18243, _G18249), 'compile-atomic-rewrites-nondeterministicly', always).

%   (relation-located compile-atomic-rewrites-nondeterministicly-hash-304 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) compile-atomic-rewrites-nondeterministicly-hash-304  always )  
'relation-located'('compile-atomic-rewrites-nondeterministicly-hash-304', 5, kernel, 'SetFn'(_G19811, _G19817, _G19823, _G19829, _G19835), 'compile-atomic-rewrites-nondeterministicly-hash-304', always).

%   (relation-located letter 1 kernel  (SetFn ?h639) letter  always )  
'relation-located'(letter, 1, kernel, 'SetFn'(_G15655), letter, always).

%   (relation-located flatten 2 kernel  (SetFn ?h640  ?h642) flatten  always )  
'relation-located'(flatten, 2, kernel, 'SetFn'(_G16130, _G16136), flatten, always).

%   (relation-located ua-resource-info 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-resource-info  always )  
'relation-located'('ua-resource-info', 6, kernel, 'SetFn'(_G18270, _G18276, _G18282, _G18288, _G18294, _G18300), 'ua-resource-info', always).

%   (relation-located file-read-line 3 kernel  (SetFn ?h641  ?h643  ?h645) file-read-line  always )  
'relation-located'('file-read-line', 3, kernel, 'SetFn'(_G16893, _G16899, _G16905), 'file-read-line', always).

%   (relation-located parse-single-char 1 kernel  (SetFn ?h639) parse-single-char  always )  
'relation-located'('parse-single-char', 1, kernel, 'SetFn'(_G16183), 'parse-single-char', always).

%   (relation-located fmt-write 2 kernel  (SetFn ?h640  ?h642) fmt-write  always )  
'relation-located'('fmt-write', 2, kernel, 'SetFn'(_G16226, _G16232), 'fmt-write', always).

%   (relation-located fmt-write 3 kernel  (SetFn ?h641  ?h643  ?h645) fmt-write  always )  
'relation-located'('fmt-write', 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), 'fmt-write', always).

%   (relation-located real-file-name 2 kernel  (SetFn ?h640  ?h642) real-file-name  always )  
'relation-located'('real-file-name', 2, kernel, 'SetFn'(_G16466, _G16472), 'real-file-name', always).

%   (relation-located prove-conj 7 kernel  (SetFn ?h645  ?h647  ?h649  ?h651  ?h653  ?h655  ?h657) prove-conj  always )  
'relation-located'('prove-conj', 7, kernel, 'SetFn'(_G18409, _G18415, _G18421, _G18427, _G18433, _G18439, _G18445), 'prove-conj', always).

%   (relation-located file-reopen 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) file-reopen  always )  
'relation-located'('file-reopen', 4, kernel, 'SetFn'(_G17176, _G17182, _G17188, _G17194), 'file-reopen', always).

%   (relation-located copy-term 2 kernel  (SetFn ?h640  ?h642) copy-term  always )  
'relation-located'('copy-term', 2, kernel, 'SetFn'(_G16226, _G16232), 'copy-term', always).

%   (relation-located roundz 2 kernel  (SetFn ?h640  ?h642) roundz  always )  
'relation-located'(roundz, 2, kernel, 'SetFn'(_G16082, _G16088), roundz, always).

%   (relation-located collect-getPrologVars-hash-304 3 kernel  (SetFn ?h641  ?h643  ?h645) collect-getPrologVars-hash-304  always )  
'relation-located'('collect-getPrologVars-hash-304', 3, kernel, 'SetFn'(_G17517, _G17523, _G17529), 'collect-getPrologVars-hash-304', always).

%   (relation-located var-start 1 kernel  (SetFn ?h639) var-start  always )  
'relation-located'('var-start', 1, kernel, 'SetFn'(_G15799), 'var-start', always).

%   (relation-located ith 3 kernel  (SetFn ?h641  ?h643  ?h645) ith  always )  
'relation-located'(ith, 3, kernel, 'SetFn'(_G16365, _G16371, _G16377), ith, always).

%   (relation-located get-next-num 1 kernel  (SetFn ?h639) get-next-num  always )  
'relation-located'('get-next-num', 1, kernel, 'SetFn'(_G15943), 'get-next-num', always).

%   (relation-located current-op 3 kernel  (SetFn ?h641  ?h643  ?h645) current-op  always )  
'relation-located'('current-op', 3, kernel, 'SetFn'(_G16701, _G16707, _G16713), 'current-op', always).

%   (relation-located current-output-port 1 kernel  (SetFn ?h639) current-output-port  always )  
'relation-located'('current-output-port', 1, kernel, 'SetFn'(_G16279), 'current-output-port', always).

%   (relation-located socket-select-destroy 1 kernel  (SetFn ?h639) socket-select-destroy  always )  
'relation-located'('socket-select-destroy', 1, kernel, 'SetFn'(_G16375), 'socket-select-destroy', always).

%   (relation-located process-answer 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) process-answer  always )  
'relation-located'('process-answer', 5, kernel, 'SetFn'(_G17747, _G17753, _G17759, _G17765, _G17771), 'process-answer', always).

%   (relation-located process-answer 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) process-answer  always )  
'relation-located'('process-answer', 6, kernel, 'SetFn'(_G18174, _G18180, _G18186, _G18192, _G18198, _G18204), 'process-answer', always).

%   (relation-located asserta 1 kernel  (SetFn ?h639) asserta  always )  
'relation-located'(asserta, 1, kernel, 'SetFn'(_G15703), asserta, always).

%   (relation-located bootstrap-userpackage 3 kernel  (SetFn ?h641  ?h643  ?h645) bootstrap-userpackage  always )  
'relation-located'('bootstrap-userpackage', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'bootstrap-userpackage', always).

%   (relation-located hex-code 3 kernel  (SetFn ?h641  ?h643  ?h645) hex-code  always )  
'relation-located'('hex-code', 3, kernel, 'SetFn'(_G16605, _G16611, _G16617), 'hex-code', always).

%   (relation-located console-post 2 kernel  (SetFn ?h640  ?h642) console-post  always )  
'relation-located'('console-post', 2, kernel, 'SetFn'(_G16370, _G16376), 'console-post', always).

%   (relation-located assert 1 kernel  (SetFn ?h639) assert  always )  
'relation-located'(assert, 1, kernel, 'SetFn'(_G15703), assert, always).

%   (relation-located sigma-H-use-kr-datafile 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-use-kr-datafile  always )  
'relation-located'('sigma-H-use-kr-datafile', 3, kernel, 'SetFn'(_G17325, _G17331, _G17337), 'sigma-H-use-kr-datafile', always).

%   (relation-located peek-counter 2 kernel  (SetFn ?h640  ?h642) peek-counter  always )  
'relation-located'('peek-counter', 2, kernel, 'SetFn'(_G16370, _G16376), 'peek-counter', always).

%   (relation-located bootstrap-syspackage 2 kernel  (SetFn ?h640  ?h642) bootstrap-syspackage  always )  
'relation-located'('bootstrap-syspackage', 2, kernel, 'SetFn'(_G16754, _G16760), 'bootstrap-syspackage', always).

%   (relation-located increment-counter 2 kernel  (SetFn ?h640  ?h642) increment-counter  always )  
'relation-located'('increment-counter', 2, kernel, 'SetFn'(_G16610, _G16616), 'increment-counter', always).

%   (relation-located display 1 kernel  (SetFn ?h639) display  always )  
'relation-located'(display, 1, kernel, 'SetFn'(_G15703), display, always).

%   (relation-located get-var 2 kernel  (SetFn ?h640  ?h642) get-var  always )  
'relation-located'('get-var', 2, kernel, 'SetFn'(_G16130, _G16136), 'get-var', always).

%   (relation-located multifile 1 kernel  (SetFn ?h639) multifile  always )  
'relation-located'((multifile), 1, kernel, 'SetFn'(_G15799), (multifile), always).

%   (relation-located store-assertion-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) store-assertion-list  always )  
'relation-located'('store-assertion-list', 4, kernel, 'SetFn'(_G17608, _G17614, _G17620, _G17626), 'store-assertion-list', always).

%   (relation-located compile-metalogics-deterministicly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-metalogics-deterministicly  always )  
'relation-located'('compile-metalogics-deterministicly', 3, kernel, 'SetFn'(_G17853, _G17859, _G17865), 'compile-metalogics-deterministicly', always).

%   (relation-located compile-metalogics-nondeterministicly-hash-303 2 kernel  (SetFn ?h640  ?h642) compile-metalogics-nondeterministicly-hash-303  always )  
'relation-located'('compile-metalogics-nondeterministicly-hash-303', 2, kernel, 'SetFn'(_G18290, _G18296), 'compile-metalogics-nondeterministicly-hash-303', always).

%   (relation-located compile-metalogics-nondeterministicly-hash-302 2 kernel  (SetFn ?h640  ?h642) compile-metalogics-nondeterministicly-hash-302  always )  
'relation-located'('compile-metalogics-nondeterministicly-hash-302', 2, kernel, 'SetFn'(_G18290, _G18296), 'compile-metalogics-nondeterministicly-hash-302', always).

%   (relation-located compile-metalogics-nondeterministicly-hash-301 2 kernel  (SetFn ?h640  ?h642) compile-metalogics-nondeterministicly-hash-301  always )  
'relation-located'('compile-metalogics-nondeterministicly-hash-301', 2, kernel, 'SetFn'(_G18290, _G18296), 'compile-metalogics-nondeterministicly-hash-301', always).

%   (relation-located compile-metalogics-nondeterministicly 2 kernel  (SetFn ?h640  ?h642) compile-metalogics-nondeterministicly  always )  
'relation-located'('compile-metalogics-nondeterministicly', 2, kernel, 'SetFn'(_G17570, _G17576), 'compile-metalogics-nondeterministicly', always).

%   (relation-located ua-process-command 2 kernel  (SetFn ?h640  ?h642) ua-process-command  always )  
'relation-located'('ua-process-command', 2, kernel, 'SetFn'(_G16658, _G16664), 'ua-process-command', always).

%   (relation-located ua-process-command 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-process-command  always )  
'relation-located'('ua-process-command', 6, kernel, 'SetFn'(_G18366, _G18372, _G18378, _G18384, _G18390, _G18396), 'ua-process-command', always).

%   (relation-located ua-resource-delete 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-resource-delete  always )  
'relation-located'('ua-resource-delete', 6, kernel, 'SetFn'(_G18366, _G18372, _G18378, _G18384, _G18390, _G18396), 'ua-resource-delete', always).

%   (relation-located ua-resource-create 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-resource-create  always )  
'relation-located'('ua-resource-create', 6, kernel, 'SetFn'(_G18366, _G18372, _G18378, _G18384, _G18390, _G18396), 'ua-resource-create', always).

%   (relation-located set-dcg-style 1 kernel  (SetFn ?h639) set-dcg-style  always )  
'relation-located'('set-dcg-style', 1, kernel, 'SetFn'(_G15991), 'set-dcg-style', always).

%   ;(relation-located sys-hid-cutto 1 kernel  (SetFn ?h639) sys-hid-cutto  always )   
%  (relation-located store-assertion-list-1 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) store-assertion-list-1  always )  
'relation-located'('store-assertion-list-1', 4, kernel, 'SetFn'(_G17788, _G17794, _G17800, _G17806), 'store-assertion-list-1', always).

%   (relation-located has-singleton-getPrologVars 1 kernel  (SetFn ?h639) has-singleton-getPrologVars  always )  
'relation-located'('has-singleton-getPrologVars', 1, kernel, 'SetFn'(_G16231), 'has-singleton-getPrologVars', always).

%   (relation-located listing 0 kernel  (EmptySetFn) listing  always )  
'relation-located'(listing, 0, kernel, 'EmptySetFn', listing, always).

%   (relation-located listing 1 kernel  (SetFn ?h639) listing  always )  
'relation-located'(listing, 1, kernel, 'SetFn'(_G15703), listing, always).

%   (relation-located show-query 1 kernel  (SetFn ?h639) show-query  always )  
'relation-located'('show-query', 1, kernel, 'SetFn'(_G15847), 'show-query', always).

%   (relation-located sigma-U-option-set 2 kernel  (SetFn ?h640  ?h642) sigma-U-option-set  always )  
'relation-located'('sigma-U-option-set', 2, kernel, 'SetFn'(_G16658, _G16664), 'sigma-U-option-set', always).

%   (relation-located meta-interpret-hash-301 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) meta-interpret-hash-301  always )  
'relation-located'('meta-interpret-hash-301', 6, kernel, 'SetFn'(_G18894, _G18900, _G18906, _G18912, _G18918, _G18924), 'meta-interpret-hash-301', always).

%   (relation-located meta-interpret-hash-302 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) meta-interpret-hash-302  always )  
'relation-located'('meta-interpret-hash-302', 6, kernel, 'SetFn'(_G18894, _G18900, _G18906, _G18912, _G18918, _G18924), 'meta-interpret-hash-302', always).

%   (relation-located meta-interpret-hash-303 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) meta-interpret-hash-303  always )  
'relation-located'('meta-interpret-hash-303', 6, kernel, 'SetFn'(_G18894, _G18900, _G18906, _G18912, _G18918, _G18924), 'meta-interpret-hash-303', always).

%   (relation-located add-file-p 2 kernel  (SetFn ?h640  ?h642) add-file-p  always )  
'relation-located'('add-file-p', 2, kernel, 'SetFn'(_G16274, _G16280), 'add-file-p', always).

%   (relation-located current-atom 1 kernel  (SetFn ?h639) current-atom  always )  
'relation-located'('current-atom', 1, kernel, 'SetFn'(_G15943), 'current-atom', always).

%   (relation-located sigma-sigma_X_kb-predicates 1 kernel  (SetFn ?h639) sigma-sigma_X_kb-predicates  always )  
'relation-located'('sigma-sigma_X_kb-predicates', 1, kernel, 'SetFn'(_G16663), 'sigma-sigma_X_kb-predicates', always).

%   (relation-located tmpfile-open 1 kernel  (SetFn ?h639) tmpfile-open  always )  
'relation-located'('tmpfile-open', 1, kernel, 'SetFn'(_G15943), 'tmpfile-open', always).

%   (relation-located nth-item 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) nth-item  always )  
'relation-located'('nth-item', 4, kernel, 'SetFn'(_G17032, _G17038, _G17044, _G17050), 'nth-item', always).

%   (relation-located var-number 2 kernel  (SetFn ?h640  ?h642) var-number  always )  
'relation-located'('var-number', 2, kernel, 'SetFn'(_G16274, _G16280), 'var-number', always).

%   (relation-located shift-var 2 kernel  (SetFn ?h640  ?h642) shift-var  always )  
'relation-located'('shift-var', 2, kernel, 'SetFn'(_G16226, _G16232), 'shift-var', always).

%   (relation-located nonotrace 0 kernel  (EmptySetFn) nonotrace  always )  
'relation-located'(nonotrace, 0, kernel, 'EmptySetFn', nonotrace, always).

%   (relation-located flag 3 kernel  (SetFn ?h641  ?h643  ?h645) flag  always )  
'relation-located'(flag, 3, kernel, 'SetFn'(_G16413, _G16419, _G16425), flag, always).

%   (relation-located collect-getPrologVars-nums 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) collect-getPrologVars-nums  always )  
'relation-located'('collect-getPrologVars-nums', 4, kernel, 'SetFn'(_G17464, _G17470, _G17476, _G17482), 'collect-getPrologVars-nums', always).

%   (relation-located compile-as-prolog 2 kernel  (SetFn ?h640  ?h642) compile-as-prolog  always )  
'relation-located'('compile-as-prolog', 2, kernel, 'SetFn'(_G16610, _G16616), 'compile-as-prolog', always).

%   (relation-located retract 1 kernel  (SetFn ?h639) retract  always )  
'relation-located'(retract, 1, kernel, 'SetFn'(_G15703), retract, always).

%   (relation-located process-ascii 1 kernel  (SetFn ?h639) process-ascii  always )  
'relation-located'('process-ascii', 1, kernel, 'SetFn'(_G15991), 'process-ascii', always).

%   (relation-located current-module 1 kernel  (SetFn ?h639) current-module  always )  
'relation-located'('current-module', 1, kernel, 'SetFn'(_G16039), 'current-module', always).

%   (relation-located current-module 2 kernel  (SetFn ?h640  ?h642) current-module  always )  
'relation-located'('current-module', 2, kernel, 'SetFn'(_G16466, _G16472), 'current-module', always).

%   (relation-located collect-failbranches 3 kernel  (SetFn ?h641  ?h643  ?h645) collect-failbranches  always )  
'relation-located'('collect-failbranches', 3, kernel, 'SetFn'(_G17181, _G17187, _G17193), 'collect-failbranches', always).

%   (relation-located handle-httpd-request 1 kernel  (SetFn ?h639) handle-httpd-request  always )  
'relation-located'('handle-httpd-request', 1, kernel, 'SetFn'(_G16327), 'handle-httpd-request', always).

%   (relation-located ensure-loaded 1 kernel  (SetFn ?h639) ensure-loaded  always )  
'relation-located'('ensure-loaded', 1, kernel, 'SetFn'(_G15991), 'ensure-loaded', always).

%   (relation-located ensure-loaded 2 kernel  (SetFn ?h640  ?h642) ensure-loaded  always )  
'relation-located'('ensure-loaded', 2, kernel, 'SetFn'(_G16418, _G16424), 'ensure-loaded', always).

%   (relation-located nd-sigma-H-command 2 kernel  (SetFn ?h640  ?h642) nd-sigma-H-command  always )  
'relation-located'('nd-sigma-H-command', 2, kernel, 'SetFn'(_G16658, _G16664), 'nd-sigma-H-command', always).

%   (relation-located garbage-collection 1 kernel  (SetFn ?h639) garbage-collection  always )  
'relation-located'('garbage-collection', 1, kernel, 'SetFn'(_G16231), 'garbage-collection', always).

%   (relation-located backwards-chain 3 kernel  (SetFn ?h641  ?h643  ?h645) backwards-chain  always )  
'relation-located'('backwards-chain', 3, kernel, 'SetFn'(_G16941, _G16947, _G16953), 'backwards-chain', always).

%   (relation-located backwards-chain 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) backwards-chain  always )  
'relation-located'('backwards-chain', 4, kernel, 'SetFn'(_G17368, _G17374, _G17380, _G17386), 'backwards-chain', always).

%   (relation-located delete-once-hash-302 3 kernel  (SetFn ?h641  ?h643  ?h645) delete-once-hash-302  always )  
'relation-located'('delete-once-hash-302', 3, kernel, 'SetFn'(_G17469, _G17475, _G17481), 'delete-once-hash-302', always).

%   (relation-located print 1 kernel  (SetFn ?h639) print  always )  
'relation-located'(print, 1, kernel, 'SetFn'(_G15607), print, always).

%   (relation-located sigma-call 2 kernel  (SetFn ?h640  ?h642) sigma-call  always )  
'relation-located'('sigma-call', 2, kernel, 'SetFn'(_G16274, _G16280), 'sigma-call', always).

%   (relation-located delete-once 3 kernel  (SetFn ?h641  ?h643  ?h645) delete-once  always )  
'relation-located'('delete-once', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'delete-once', always).

%   (relation-located ua-set-agent-location 1 kernel  (SetFn ?h639) ua-set-agent-location  always )  
'relation-located'('ua-set-agent-location', 1, kernel, 'SetFn'(_G16375), 'ua-set-agent-location', always).

%   (relation-located httpd-server 2 kernel  (SetFn ?h640  ?h642) httpd-server  always )  
'relation-located'('httpd-server', 2, kernel, 'SetFn'(_G16370, _G16376), 'httpd-server', always).

%   (relation-located httpd-server 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) httpd-server  always )  
'relation-located'('httpd-server', 4, kernel, 'SetFn'(_G17224, _G17230, _G17236, _G17242), 'httpd-server', always).

%   (relation-located ua-post-sdtout 2 kernel  (SetFn ?h640  ?h642) ua-post-sdtout  always )  
'relation-located'('ua-post-sdtout', 2, kernel, 'SetFn'(_G16466, _G16472), 'ua-post-sdtout', always).

%   (relation-located reverse 2 kernel  (SetFn ?h640  ?h642) reverse  always )  
'relation-located'(reverse, 2, kernel, 'SetFn'(_G16130, _G16136), reverse, always).

%   (relation-located reverse 3 kernel  (SetFn ?h641  ?h643  ?h645) reverse  always )  
'relation-located'(reverse, 3, kernel, 'SetFn'(_G16557, _G16563, _G16569), reverse, always).

%   (relation-located socket-write-list 1 kernel  (SetFn ?h639) socket-write-list  always )  
'relation-located'('socket-write-list', 1, kernel, 'SetFn'(_G16183), 'socket-write-list', always).

%   (relation-located global-get 2 kernel  (SetFn ?h640  ?h642) global-get  always )  
'relation-located'('global-get', 2, kernel, 'SetFn'(_G16274, _G16280), 'global-get', always).

%   (relation-located write 1 kernel  (SetFn ?h639) write  always )  
'relation-located'(write, 1, kernel, 'SetFn'(_G15607), write, always).

%   (relation-located write 2 kernel  (SetFn ?h640  ?h642) write  always )  
'relation-located'(write, 2, kernel, 'SetFn'(_G16034, _G16040), write, always).

%   (relation-located sigma-P-parse-proposition 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-P-parse-proposition  always )  
'relation-located'('sigma-P-parse-proposition', 3, kernel, 'SetFn'(_G17421, _G17427, _G17433), 'sigma-P-parse-proposition', always).

%   (relation-located ua-set-agent-callback 1 kernel  (SetFn ?h639) ua-set-agent-callback  always )  
'relation-located'('ua-set-agent-callback', 1, kernel, 'SetFn'(_G16375), 'ua-set-agent-callback', always).

%   (relation-located legal-char 2 kernel  (SetFn ?h640  ?h642) legal-char  always )  
'relation-located'('legal-char', 2, kernel, 'SetFn'(_G16274, _G16280), 'legal-char', always).

%   (relation-located exit-sub 0 kernel  (EmptySetFn) exit-sub  always )  
'relation-located'('exit-sub', 0, kernel, 'EmptySetFn', 'exit-sub', always).

%   (relation-located sigma-U-collect-confirmation 1 kernel  (SetFn ?h639) sigma-U-collect-confirmation  always )  
'relation-located'('sigma-U-collect-confirmation', 1, kernel, 'SetFn'(_G16711), 'sigma-U-collect-confirmation', always).

%   (relation-located evaluate-disp_modification 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) evaluate-disp_modification  always )  
'relation-located'('evaluate-disp_modification', 6, kernel, 'SetFn'(_G18750, _G18756, _G18762, _G18768, _G18774, _G18780), 'evaluate-disp_modification', always).

%   (relation-located writeq 1 kernel  (SetFn ?h639) writeq  always )  
'relation-located'(writeq, 1, kernel, 'SetFn'(_G15655), writeq, always).

%   (relation-located writeq 2 kernel  (SetFn ?h640  ?h642) writeq  always )  
'relation-located'(writeq, 2, kernel, 'SetFn'(_G16082, _G16088), writeq, always).

%   (relation-located ua-constant-info 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-constant-info  always )  
'relation-located'('ua-constant-info', 6, kernel, 'SetFn'(_G18270, _G18276, _G18282, _G18288, _G18294, _G18300), 'ua-constant-info', always).

%   (relation-located test-sigma-compiler 0 kernel  (EmptySetFn) test-sigma-compiler  always )  
'relation-located'('test-sigma-compiler', 0, kernel, 'EmptySetFn', 'test-sigma-compiler', always).

%   (relation-located is-chars 1 kernel  (SetFn ?h639) is-chars  always )  
'relation-located'('is-chars', 1, kernel, 'SetFn'(_G15751), 'is-chars', always).

%   (relation-located not 2 kernel  (SetFn ?h640  ?h642) not  always )  
'relation-located'(not, 2, kernel, 'SetFn'(_G15938, _G15944), not, always).

%   (relation-located not 1 kernel  (SetFn ?h639) not  always )  
'relation-located'(not, 1, kernel, 'SetFn'(_G15511), not, always).

%   (relation-located nonvar-meta-interpret 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) nonvar-meta-interpret  always )  
'relation-located'('nonvar-meta-interpret', 6, kernel, 'SetFn'(_G18510, _G18516, _G18522, _G18528, _G18534, _G18540), 'nonvar-meta-interpret', always).

%   (relation-located shared-variables 2 kernel  (SetFn ?h640  ?h642) shared-variables  always )  
'relation-located'('shared-variables', 2, kernel, 'SetFn'(_G16562, _G16568), 'shared-variables', always).

%   (relation-located atom-to-term 2 kernel  (SetFn ?h640  ?h642) atom-to-term  always )  
'relation-located'('atom-to-term', 2, kernel, 'SetFn'(_G16370, _G16376), 'atom-to-term', always).

%   (relation-located atom-to-term 3 kernel  (SetFn ?h641  ?h643  ?h645) atom-to-term  always )  
'relation-located'('atom-to-term', 3, kernel, 'SetFn'(_G16797, _G16803, _G16809), 'atom-to-term', always).

%   (relation-located atomical 1 kernel  (SetFn ?h639) atomical  always )  
'relation-located'(atomical, 1, kernel, 'SetFn'(_G15751), atomical, always).

%   (relation-located compile-as-function 2 kernel  (SetFn ?h640  ?h642) compile-as-function  always )  
'relation-located'('compile-as-function', 2, kernel, 'SetFn'(_G16706, _G16712), 'compile-as-function', always).

%   (relation-located parser-tokenize 2 kernel  (SetFn ?h640  ?h642) parser-tokenize  always )  
'relation-located'('parser-tokenize', 2, kernel, 'SetFn'(_G16514, _G16520), 'parser-tokenize', always).

%   (relation-located log-ith-bound 3 kernel  (SetFn ?h641  ?h643  ?h645) log-ith-bound  always )  
'relation-located'('log-ith-bound', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'log-ith-bound', always).

%   (relation-located socket-write-chars 1 kernel  (SetFn ?h639) socket-write-chars  always )  
'relation-located'('socket-write-chars', 1, kernel, 'SetFn'(_G16231), 'socket-write-chars', always).

%   (relation-located socket-write-chars 2 kernel  (SetFn ?h640  ?h642) socket-write-chars  always )  
'relation-located'('socket-write-chars', 2, kernel, 'SetFn'(_G16658, _G16664), 'socket-write-chars', always).

%   (relation-located atom-chars 2 kernel  (SetFn ?h640  ?h642) atom-chars  always )  
'relation-located'('atom-chars', 2, kernel, 'SetFn'(_G16274, _G16280), 'atom-chars', always).

%   (relation-located sigma-X-assert-new 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-X-assert-new  always )  
'relation-located'('sigma-X-assert-new', 5, kernel, 'SetFn'(_G17939, _G17945, _G17951, _G17957, _G17963), 'sigma-X-assert-new', always).

%   (relation-located grab-last-arg 3 kernel  (SetFn ?h641  ?h643  ?h645) grab-last-arg  always )  
'relation-located'('grab-last-arg', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'grab-last-arg', always).

%   (relation-located told 0 kernel  (EmptySetFn) told  always )  
'relation-located'(told, 0, kernel, 'EmptySetFn', told, always).

%   (relation-located compile-heuristics-deterministcly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-heuristics-deterministcly  always )  
'relation-located'('compile-heuristics-deterministcly', 3, kernel, 'SetFn'(_G17805, _G17811, _G17817), 'compile-heuristics-deterministcly', always).

%   (relation-located compile-heuristics-nondeterministcly 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-heuristics-nondeterministcly  always )  
'relation-located'('compile-heuristics-nondeterministcly', 3, kernel, 'SetFn'(_G17949, _G17955, _G17961), 'compile-heuristics-nondeterministcly', always).

%   (relation-located sigma-H-retract 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-retract  always )  
'relation-located'('sigma-H-retract', 3, kernel, 'SetFn'(_G16941, _G16947, _G16953), 'sigma-H-retract', always).

%   (relation-located sigma-X-retract 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-X-retract  always )  
'relation-located'('sigma-X-retract', 5, kernel, 'SetFn'(_G17795, _G17801, _G17807, _G17813, _G17819), 'sigma-X-retract', always).

%   (relation-located compile-rewrite-clause 3 kernel  (SetFn ?h641  ?h643  ?h645) compile-rewrite-clause  always )  
'relation-located'('compile-rewrite-clause', 3, kernel, 'SetFn'(_G17277, _G17283, _G17289), 'compile-rewrite-clause', always).

%   (relation-located is-prolog 1 kernel  (SetFn ?h639) is-prolog  always )  
'relation-located'('is-prolog', 1, kernel, 'SetFn'(_G15799), 'is-prolog', always).

%   (relation-located current-output 1 kernel  (SetFn ?h639) current-output  always )  
'relation-located'('current-output', 1, kernel, 'SetFn'(_G16039), 'current-output', always).

%   (relation-located recompile-forall 0 kernel  (EmptySetFn) recompile-forall  always )  
'relation-located'('recompile-forall', 0, kernel, 'EmptySetFn', 'recompile-forall', always).

%   (relation-located cb_error-type 2 kernel  (SetFn ?h640  ?h642) cb_error-type  always )  
'relation-located'('cb_error-type', 2, kernel, 'SetFn'(_G16418, _G16424), 'cb_error-type', always).

%   (relation-located sigma-C-assert-based-on-language-first 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) sigma-C-assert-based-on-language-first  always )  
'relation-located'('sigma-C-assert-based-on-language-first', 4, kernel, 'SetFn'(_G18472, _G18478, _G18484, _G18490), 'sigma-C-assert-based-on-language-first', always).

%   (relation-located sigma-C-assert-based-on-language 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-C-assert-based-on-language  always )  
'relation-located'('sigma-C-assert-based-on-language', 5, kernel, 'SetFn'(_G18611, _G18617, _G18623, _G18629, _G18635), 'sigma-C-assert-based-on-language', always).

%   (relation-located new-relation-exists-in-context 3 kernel  (SetFn ?h641  ?h643  ?h645) new-relation-exists-in-context  always )  
'relation-located'('new-relation-exists-in-context', 3, kernel, 'SetFn'(_G17661, _G17667, _G17673), 'new-relation-exists-in-context', always).

%   (relation-located inference-module-query 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) inference-module-query  always )  
'relation-located'('inference-module-query', 4, kernel, 'SetFn'(_G17704, _G17710, _G17716, _G17722), 'inference-module-query', always).

%   (relation-located write-canonical 1 kernel  (SetFn ?h639) write-canonical  always )  
'relation-located'('write-canonical', 1, kernel, 'SetFn'(_G16087), 'write-canonical', always).

%   (relation-located write-canonical 2 kernel  (SetFn ?h640  ?h642) write-canonical  always )  
'relation-located'('write-canonical', 2, kernel, 'SetFn'(_G16514, _G16520), 'write-canonical', always).

%   (relation-located tphrase 1 kernel  (SetFn ?h639) tphrase  always )  
'relation-located'(tphrase, 1, kernel, 'SetFn'(_G15703), tphrase, always).

%   (relation-located tphrase 2 kernel  (SetFn ?h640  ?h642) tphrase  always )  
'relation-located'(tphrase, 2, kernel, 'SetFn'(_G16130, _G16136), tphrase, always).

%   (relation-located uninstantiated 2 kernel  (SetFn ?h640  ?h642) uninstantiated  always )  
'relation-located'(uninstantiated, 2, kernel, 'SetFn'(_G16466, _G16472), uninstantiated, always).

%   (relation-located global-set 2 kernel  (SetFn ?h640  ?h642) global-set  always )  
'relation-located'('global-set', 2, kernel, 'SetFn'(_G16274, _G16280), 'global-set', always).

%   (relation-located sigma-console-end-char 2 kernel  (SetFn ?h640  ?h642) sigma-console-end-char  always )  
'relation-located'('sigma-console-end-char', 2, kernel, 'SetFn'(_G16850, _G16856), 'sigma-console-end-char', always).

%   (relation-located sigma-console-scan-text 2 kernel  (SetFn ?h640  ?h642) sigma-console-scan-text  always )  
'relation-located'('sigma-console-scan-text', 2, kernel, 'SetFn'(_G16898, _G16904), 'sigma-console-scan-text', always).

%   (relation-located unload-package 1 kernel  (SetFn ?h639) unload-package  always )  
'relation-located'('unload-package', 1, kernel, 'SetFn'(_G16039), 'unload-package', always).

%   (relation-located ua-query 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-query  always )  
'relation-located'('ua-query', 6, kernel, 'SetFn'(_G17886, _G17892, _G17898, _G17904, _G17910, _G17916), 'ua-query', always).

%   (relation-located put 1 kernel  (SetFn ?h639) put  always )  
'relation-located'(put, 1, kernel, 'SetFn'(_G15511), put, always).

%   (relation-located disp_debug-ctl 2 kernel  (SetFn ?h640  ?h642) disp_debug-ctl  always )  
'relation-located'('disp_debug-ctl', 2, kernel, 'SetFn'(_G16466, _G16472), 'disp_debug-ctl', always).

%   (relation-located sigma-console-read-stdin 2 kernel  (SetFn ?h640  ?h642) sigma-console-read-stdin  always )  
'relation-located'('sigma-console-read-stdin', 2, kernel, 'SetFn'(_G16946, _G16952), 'sigma-console-read-stdin', always).

%   (relation-located prove-not 2 kernel  (SetFn ?h640  ?h642) prove-not  always )  
'relation-located'('prove-not', 2, kernel, 'SetFn'(_G16226, _G16232), 'prove-not', always).

%   (relation-located re-var-list 2 kernel  (SetFn ?h640  ?h642) re-var-list  always )  
'relation-located'('re-var-list', 2, kernel, 'SetFn'(_G16322, _G16328), 're-var-list', always).

%   (relation-located is-absolute-filename 1 kernel  (SetFn ?h639) is-absolute-filename  always )  
'relation-located'('is-absolute-filename', 1, kernel, 'SetFn'(_G16327), 'is-absolute-filename', always).

%   (relation-located sigma-H-relation-add 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) sigma-H-relation-add  always )  
'relation-located'('sigma-H-relation-add', 6, kernel, 'SetFn'(_G18462, _G18468, _G18474, _G18480, _G18486, _G18492), 'sigma-H-relation-add', always).

%   (relation-located make-functsymbol2 2 kernel  (SetFn ?h640  ?h642) make-functsymbol2  always )  
'relation-located'('make-functsymbol2', 2, kernel, 'SetFn'(_G16706, _G16712), 'make-functsymbol2', always).

%   (relation-located set-global-compiler-options 1 kernel  (SetFn ?h639) set-global-compiler-options  always )  
'relation-located'('set-global-compiler-options', 1, kernel, 'SetFn'(_G16663), 'set-global-compiler-options', always).

%   (relation-located sigma-console-get-chars0 1 kernel  (SetFn ?h639) sigma-console-get-chars0  always )  
'relation-located'('sigma-console-get-chars0', 1, kernel, 'SetFn'(_G16615), 'sigma-console-get-chars0', always).

%   (relation-located query-done 1 kernel  (SetFn ?h639) query-done  always )  
'relation-located'('query-done', 1, kernel, 'SetFn'(_G15847), 'query-done', always).

%   (relation-located inference-module-create-query-workspace 1 kernel  (SetFn ?h639) inference-module-create-query-workspace  always )  
'relation-located'('inference-module-create-query-workspace', 1, kernel, 'SetFn'(_G17239), 'inference-module-create-query-workspace', always).

%   (relation-located keysort 2 kernel  (SetFn ?h640  ?h642) keysort  always )  
'relation-located'(keysort, 2, kernel, 'SetFn'(_G16130, _G16136), keysort, always).

%   (relation-located proper-hilog 1 kernel  (SetFn ?h639) proper-hilog  always )  
'relation-located'('proper-hilog', 1, kernel, 'SetFn'(_G15943), 'proper-hilog', always).

%   (relation-located sigma_X_kb-eval 2 kernel  (SetFn ?h640  ?h642) sigma_X_kb-eval  always )  
'relation-located'('sigma_X_kb-eval', 2, kernel, 'SetFn'(_G16514, _G16520), 'sigma_X_kb-eval', always).

%   (relation-located write-http-header 0 kernel  (EmptySetFn) write-http-header  always )  
'relation-located'('write-http-header', 0, kernel, 'EmptySetFn', 'write-http-header', always).

%   (relation-located call 1 kernel  (SetFn ?h639) call  always )  
'relation-located'(call, 1, kernel, 'SetFn'(_G15559), call, always).

%   (relation-located writeln 1 kernel  (SetFn ?h639) writeln  always )  
'relation-located'(writeln, 1, kernel, 'SetFn'(_G15703), writeln, always).

%   (relation-located sigma-console-readstring-stdin 1 kernel  (SetFn ?h639) sigma-console-readstring-stdin  always )  
'relation-located'('sigma-console-readstring-stdin', 1, kernel, 'SetFn'(_G16807), 'sigma-console-readstring-stdin', always).

%   (relation-located load-dyn 1 kernel  (SetFn ?h639) load-dyn  always )  
'relation-located'('load-dyn', 1, kernel, 'SetFn'(_G15751), 'load-dyn', always).

%   (relation-located current-predicate 1 kernel  (SetFn ?h639) current-predicate  always )  
'relation-located'('current-predicate', 1, kernel, 'SetFn'(_G16183), 'current-predicate', always).

%                                                                                                                                             (relation-located current-predicate 2 kernel  (SetFn ?h640  ?h642) current-predicate  always )  
'relation-located'('current-predicate', 2, kernel, 'SetFn'(_G18389, _G18395), 'current-predicate', always).

%   (relation-located listing-out 1 kernel  (SetFn ?h639) listing-out  always )  
'relation-located'('listing-out', 1, kernel, 'SetFn'(_G15895), 'listing-out', always).

%   (relation-located file-write 2 kernel  (SetFn ?h640  ?h642) file-write  always )  
'relation-located'('file-write', 2, kernel, 'SetFn'(_G16274, _G16280), 'file-write', always).

%   (relation-located clsid-gen 1 kernel  (SetFn ?h639) clsid-gen  always )  
'relation-located'('clsid-gen', 1, kernel, 'SetFn'(_G15799), 'clsid-gen', always).

%   (relation-located sigma-H-relation-info 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) sigma-H-relation-info  always )  
'relation-located'('sigma-H-relation-info', 6, kernel, 'SetFn'(_G18510, _G18516, _G18522, _G18528, _G18534, _G18540), 'sigma-H-relation-info', always).

%   (relation-located socket-bind 2 kernel  (SetFn ?h640  ?h642) socket-bind  always )  
'relation-located'('socket-bind', 2, kernel, 'SetFn'(_G16322, _G16328), 'socket-bind', always).

%   (relation-located socket-bind 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-bind  always )  
'relation-located'('socket-bind', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'socket-bind', always).

%   (relation-located forward-chain 2 kernel  (SetFn ?h640  ?h642) forward-chain  always )  
'relation-located'('forward-chain', 2, kernel, 'SetFn'(_G16418, _G16424), 'forward-chain', always).

%   (relation-located forward-chain 3 kernel  (SetFn ?h641  ?h643  ?h645) forward-chain  always )  
'relation-located'('forward-chain', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'forward-chain', always).

%   (relation-located float 1 kernel  (SetFn ?h639) float  always )  
'relation-located'(float, 1, kernel, 'SetFn'(_G15607), float, always).

%   (relation-located expand-term 2 kernel  (SetFn ?h640  ?h642) expand-term  always )  
'relation-located'('expand-term', 2, kernel, 'SetFn'(_G16322, _G16328), 'expand-term', always).

%   (relation-located sigma-H-retractAllProlog 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-retractAllProlog  always )  
'relation-located'('sigma-H-retractAllProlog', 3, kernel, 'SetFn'(_G17085, _G17091, _G17097), 'sigma-H-retractAllProlog', always).

%   (relation-located sigma-X-retractAllProlog 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-X-retractAllProlog  always )  
'relation-located'('sigma-X-retractAllProlog', 5, kernel, 'SetFn'(_G17939, _G17945, _G17951, _G17957, _G17963), 'sigma-X-retractAllProlog', always).

%   (relation-located sys-hid-savecp 1 kernel  (SetFn ?h639) sys-hid-savecp  always )  
'relation-located'('sys-hid-savecp', 1, kernel, 'SetFn'(_G16039), 'sys-hid-savecp', always).

%   (relation-located sigma-H-relation-delete 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) sigma-H-relation-delete  always )  
'relation-located'('sigma-H-relation-delete', 6, kernel, 'SetFn'(_G18606, _G18612, _G18618, _G18624, _G18630, _G18636), 'sigma-H-relation-delete', always).

%   (relation-located tokenize2 2 kernel  (SetFn ?h640  ?h642) tokenize2  always )  
'relation-located'(tokenize2, 2, kernel, 'SetFn'(_G16322, _G16328), tokenize2, always).

%   (relation-located tokenize3 2 kernel  (SetFn ?h640  ?h642) tokenize3  always )  
'relation-located'(tokenize3, 2, kernel, 'SetFn'(_G16322, _G16328), tokenize3, always).

%   (relation-located socket-write-pretty 2 kernel  (SetFn ?h640  ?h642) socket-write-pretty  always )  
'relation-located'('socket-write-pretty', 2, kernel, 'SetFn'(_G16706, _G16712), 'socket-write-pretty', always).

%   (relation-located socket-get_code 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-get_code  always )  
'relation-located'('socket-get_code', 3, kernel, 'SetFn'(_G16845, _G16851, _G16857), 'socket-get_code', always).

%   (relation-located see 1 kernel  (SetFn ?h639) see  always )  
'relation-located'(see, 1, kernel, 'SetFn'(_G15511), see, always).

%   (relation-located abort 0 kernel  (EmptySetFn) abort  always )  
'relation-located'(abort, 0, kernel, 'EmptySetFn', abort, always).

%   (relation-located abort 1 kernel  (SetFn ?h639) abort  always )  
'relation-located'(abort, 1, kernel, 'SetFn'(_G15607), abort, always).

%   (relation-located ground-getPrologVars 1 kernel  (SetFn ?h639) ground-getPrologVars  always )  
'relation-located'('ground-getPrologVars', 1, kernel, 'SetFn'(_G15895), 'ground-getPrologVars', always).

%   (relation-located current-input 1 kernel  (SetFn ?h639) current-input  always )  
'relation-located'('current-input', 1, kernel, 'SetFn'(_G15991), 'current-input', always).

%   (relation-located atomic 1 kernel  (SetFn ?h639) atomic  always )  
'relation-located'(atomic, 1, kernel, 'SetFn'(_G15655), atomic, always).

%   (relation-located close 1 kernel  (SetFn ?h639) close  always )  
'relation-located'(close, 1, kernel, 'SetFn'(_G15607), close, always).

%   (relation-located true 0 kernel  (EmptySetFn) true  always )  
'relation-located'(true, 0, kernel, 'EmptySetFn', true, always).

%   (relation-located get-chars 3 kernel  (SetFn ?h641  ?h643  ?h645) get-chars  always )  
'relation-located'('get-chars', 3, kernel, 'SetFn'(_G16653, _G16659, _G16665), 'get-chars', always).

%   (relation-located get-chars 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) get-chars  always )  
'relation-located'('get-chars', 5, kernel, 'SetFn'(_G17507, _G17513, _G17519, _G17525, _G17531), 'get-chars', always).

%   (relation-located term-to-atomlist 2 kernel  (SetFn ?h640  ?h642) term-to-atomlist  always )  
'relation-located'('term-to-atomlist', 2, kernel, 'SetFn'(_G16562, _G16568), 'term-to-atomlist', always).

%   (relation-located ttywritenl 1 kernel  (SetFn ?h639) ttywritenl  always )  
'relation-located'(ttywritenl, 1, kernel, 'SetFn'(_G15847), ttywritenl, always).

%   (relation-located fmt-write-string 3 kernel  (SetFn ?h641  ?h643  ?h645) fmt-write-string  always )  
'relation-located'('fmt-write-string', 3, kernel, 'SetFn'(_G16989, _G16995, _G17001), 'fmt-write-string', always).

%   (relation-located sigma-C-retract-based-on-language 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-C-retract-based-on-language  always )  
'relation-located'('sigma-C-retract-based-on-language', 5, kernel, 'SetFn'(_G18659, _G18665, _G18671, _G18677, _G18683), 'sigma-C-retract-based-on-language', always).

%   (relation-located any-to-string 2 kernel  (SetFn ?h640  ?h642) any-to-string  always )  
'relation-located'('any-to-string', 2, kernel, 'SetFn'(_G16418, _G16424), 'any-to-string', always).

%   (relation-located append 3 kernel  (SetFn ?h641  ?h643  ?h645) append  always )  
'relation-located'(append, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), append, always).

%   (relation-located ua-retract 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-retract  always )  
'relation-located'('ua-retract', 6, kernel, 'SetFn'(_G17982, _G17988, _G17994, _G18000, _G18006, _G18012), 'ua-retract', always).

%   (relation-located tab 1 kernel  (SetFn ?h639) tab  always )  
'relation-located'(tab, 1, kernel, 'SetFn'(_G15511), tab, always).

%   (relation-located spy 1 kernel  (SetFn ?h639) spy  always )  
'relation-located'(spy, 1, kernel, 'SetFn'(_G15511), spy, always).

%   (relation-located prove 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) prove  always )  
'relation-located'(prove, 6, kernel, 'SetFn'(_G17742, _G17748, _G17754, _G17760, _G17766, _G17772), prove, always).

%   (relation-located valid-char 1 kernel  (SetFn ?h639) valid-char  always )  
'relation-located'('valid-char', 1, kernel, 'SetFn'(_G15847), 'valid-char', always).

%   (relation-located valid-char 3 kernel  (SetFn ?h641  ?h643  ?h645) valid-char  always )  
'relation-located'('valid-char', 3, kernel, 'SetFn'(_G16701, _G16707, _G16713), 'valid-char', always).

%   (relation-located windows-os 0 kernel  (EmptySetFn) windows-os  always )  
'relation-located'('windows-os', 0, kernel, 'EmptySetFn', 'windows-os', always).

%   (relation-located ground 1 kernel  (SetFn ?h639) ground  always )  
'relation-located'(ground, 1, kernel, 'SetFn'(_G15655), ground, always).

%   (relation-located do-xor 2 kernel  (SetFn ?h640  ?h642) do-xor  always )  
'relation-located'('do-xor', 2, kernel, 'SetFn'(_G16082, _G16088), 'do-xor', always).

%   (relation-located xsb-flag 2 kernel  (SetFn ?h640  ?h642) xsb-flag  always )  
'relation-located'('xsb-flag', 2, kernel, 'SetFn'(_G16178, _G16184), 'xsb-flag', always).

%   (relation-located tfa 1 kernel  (SetFn ?h639) tfa  always )  
'relation-located'(tfa, 1, kernel, 'SetFn'(_G15511), tfa, always).

%   (relation-located never-assert-new 1 kernel  (SetFn ?h639) never-assert-new  always )  
'relation-located'('never-assert-new', 1, kernel, 'SetFn'(_G16135), 'never-assert-new', always).

%   (relation-located socket-close 1 kernel  (SetFn ?h639) socket-close  always )  
'relation-located'('socket-close', 1, kernel, 'SetFn'(_G15943), 'socket-close', always).

%   (relation-located predicate-property 2 kernel  (SetFn ?h640  ?h642) predicate-property  always )  
'relation-located'('predicate-property', 2, kernel, 'SetFn'(_G16658, _G16664), 'predicate-property', always).

%   (relation-located socket-close 2 kernel  (SetFn ?h640  ?h642) socket-close  always )  
'relation-located'('socket-close', 2, kernel, 'SetFn'(_G16370, _G16376), 'socket-close', always).

%   (relation-located collect-var 2 kernel  (SetFn ?h640  ?h642) collect-var  always )  
'relation-located'('collect-var', 2, kernel, 'SetFn'(_G16322, _G16328), 'collect-var', always).

%   (relation-located pred-args-legal-modes 3 kernel  (SetFn ?h641  ?h643  ?h645) pred-args-legal-modes  always )  
'relation-located'('pred-args-legal-modes', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'pred-args-legal-modes', always).

%   (relation-located unnumbervars-nil 2 kernel  (SetFn ?h640  ?h642) unnumbervars-nil  always )  
'relation-located'('unnumbervars-nil', 2, kernel, 'SetFn'(_G16562, _G16568), 'unnumbervars-nil', always).

%   (relation-located socket-flush 1 kernel  (SetFn ?h639) socket-flush  always )  
'relation-located'('socket-flush', 1, kernel, 'SetFn'(_G15943), 'socket-flush', always).

%   (relation-located ttt 0 kernel  (EmptySetFn) ttt  always )  
'relation-located'(ttt, 0, kernel, 'EmptySetFn', ttt, always).

%   (relation-located new-function-exists-in-context 3 kernel  (SetFn ?h641  ?h643  ?h645) new-function-exists-in-context  always )  
'relation-located'('new-function-exists-in-context', 3, kernel, 'SetFn'(_G17661, _G17667, _G17673), 'new-function-exists-in-context', always).

%   (relation-located seen 0 kernel  (EmptySetFn) seen  always )  
'relation-located'(seen, 0, kernel, 'EmptySetFn', seen, always).

%   (relation-located bootstrap-package 2 kernel  (SetFn ?h640  ?h642) bootstrap-package  always )  
'relation-located'('bootstrap-package', 2, kernel, 'SetFn'(_G16610, _G16616), 'bootstrap-package', always).

%   (relation-located reify-getPrologVars 2 kernel  (SetFn ?h640  ?h642) reify-getPrologVars  always )  
'relation-located'('reify-getPrologVars', 2, kernel, 'SetFn'(_G16274, _G16280), 'reify-getPrologVars', always).

%   (relation-located pred-args-legal-types 3 kernel  (SetFn ?h641  ?h643  ?h645) pred-args-legal-types  always )  
'relation-located'('pred-args-legal-types', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'pred-args-legal-types', always).

%   (relation-located store-retraction-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) store-retraction-list  always )  
'relation-located'('store-retraction-list', 4, kernel, 'SetFn'(_G17656, _G17662, _G17668, _G17674), 'store-retraction-list', always).

%   (relation-located handle-request 0 kernel  (EmptySetFn) handle-request  always )  
'relation-located'('handle-request', 0, kernel, 'EmptySetFn', 'handle-request', always).

%   (relation-located continue-the-query 0 kernel  (EmptySetFn) continue-the-query  always )  
'relation-located'('continue-the-query', 0, kernel, 'EmptySetFn', 'continue-the-query', always).

%   (relation-located socket-put 2 kernel  (SetFn ?h640  ?h642) socket-put  always )  
'relation-located'('socket-put', 2, kernel, 'SetFn'(_G16274, _G16280), 'socket-put', always).

%   (relation-located socket-put 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-put  always )  
'relation-located'('socket-put', 3, kernel, 'SetFn'(_G16701, _G16707, _G16713), 'socket-put', always).

%   (relation-located sigma-U-set-session-defaults 2 kernel  (SetFn ?h640  ?h642) sigma-U-set-session-defaults  always )  
'relation-located'('sigma-U-set-session-defaults', 2, kernel, 'SetFn'(_G17138, _G17144), 'sigma-U-set-session-defaults', always).

%   (relation-located var 1 kernel  (SetFn ?h639) var  always )  
'relation-located'(var, 1, kernel, 'SetFn'(_G15511), var, always).

%   (relation-located ltrim 2 kernel  (SetFn ?h640  ?h642) ltrim  always )  
'relation-located'(ltrim, 2, kernel, 'SetFn'(_G16034, _G16040), ltrim, always).

%   (relation-located phrase 2 kernel  (SetFn ?h640  ?h642) phrase  always )  
'relation-located'(phrase, 2, kernel, 'SetFn'(_G16082, _G16088), phrase, always).

%   (relation-located socket-connect 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-connect  always )  
'relation-located'('socket-connect', 3, kernel, 'SetFn'(_G16893, _G16899, _G16905), 'socket-connect', always).

%   (relation-located phrase 3 kernel  (SetFn ?h641  ?h643  ?h645) phrase  always )  
'relation-located'(phrase, 3, kernel, 'SetFn'(_G16509, _G16515, _G16521), phrase, always).

%   (relation-located socket-connect 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) socket-connect  always )  
'relation-located'('socket-connect', 4, kernel, 'SetFn'(_G17320, _G17326, _G17332, _G17338), 'socket-connect', always).

%   (relation-located get_code 1 kernel  (SetFn ?h639) get_code  always )  
'relation-located'(get_code, 1, kernel, 'SetFn'(_G15655), get_code, always).

%   (relation-located socket-recv 2 kernel  (SetFn ?h640  ?h642) socket-recv  always )  
'relation-located'('socket-recv', 2, kernel, 'SetFn'(_G16322, _G16328), 'socket-recv', always).

%   (relation-located socket-recv 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-recv  always )  
'relation-located'('socket-recv', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'socket-recv', always).

%   (relation-located ua-assert 6 kernel  (SetFn ?h644  ?h646  ?h648  ?h650  ?h652  ?h654) ua-assert  always )  
'relation-located'('ua-assert', 6, kernel, 'SetFn'(_G17934, _G17940, _G17946, _G17952, _G17958, _G17964), 'ua-assert', always).

%   (relation-located collect-getPrologVars 2 kernel  (SetFn ?h640  ?h642) collect-getPrologVars  always )  
'relation-located'('collect-getPrologVars', 2, kernel, 'SetFn'(_G16370, _G16376), 'collect-getPrologVars', always).

%   (relation-located sigma-U-option-lookup 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-U-option-lookup  always )  
'relation-located'('sigma-U-option-lookup', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'sigma-U-option-lookup', always).

%   (relation-located socket-send 2 kernel  (SetFn ?h640  ?h642) socket-send  always )  
'relation-located'('socket-send', 2, kernel, 'SetFn'(_G16322, _G16328), 'socket-send', always).

%   (relation-located socket-send 3 kernel  (SetFn ?h641  ?h643  ?h645) socket-send  always )  
'relation-located'('socket-send', 3, kernel, 'SetFn'(_G16749, _G16755, _G16761), 'socket-send', always).

%   (relation-located newstance-of 3 kernel  (SetFn ?h641  ?h643  ?h645) newstance-of  always )  
'relation-located'('newstance-of', 3, kernel, 'SetFn'(_G16797, _G16803, _G16809), 'newstance-of', always).

%   (relation-located table 1 kernel  (SetFn ?h639) table  always )  
'relation-located'((table), 1, kernel, 'SetFn'(_G15607), (table), always).

%   (relation-located disp_debug 0 kernel  (EmptySetFn) disp_debug  always )  
'relation-located'(disp_debug, 0, kernel, 'EmptySetFn', disp_debug, always).

%   (relation-located sigma_X_kb 2 kernel  (SetFn ?h640  ?h642) sigma_X_kb  always )  
'relation-located'(sigma_X_kb, 2, kernel, 'SetFn'(_G16274, _G16280), sigma_X_kb, always).

%   (relation-located not-backwards-chain 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) not-backwards-chain  always )  
'relation-located'('not-backwards-chain', 4, kernel, 'SetFn'(_G17560, _G17566, _G17572, _G17578), 'not-backwards-chain', always).

%   (relation-located sigma-H-context-info 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-context-info  always )  
'relation-located'('sigma-H-context-info', 3, kernel, 'SetFn'(_G17181, _G17187, _G17193), 'sigma-H-context-info', always).

%   (relation-located get-chars-hash-301 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) get-chars-hash-301  always )  
'relation-located'('get-chars-hash-301', 5, kernel, 'SetFn'(_G18227, _G18233, _G18239, _G18245, _G18251), 'get-chars-hash-301', always).

%   (relation-located get-chars-hash-302 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) get-chars-hash-302  always )  
'relation-located'('get-chars-hash-302', 5, kernel, 'SetFn'(_G18227, _G18233, _G18239, _G18245, _G18251), 'get-chars-hash-302', always).

%   (relation-located sigma-H-context-link 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-context-link  always )  
'relation-located'('sigma-H-context-link', 3, kernel, 'SetFn'(_G17181, _G17187, _G17193), 'sigma-H-context-link', always).

%   (relation-located abolish 1 kernel  (SetFn ?h639) abolish  always )  
'relation-located'(abolish, 1, kernel, 'SetFn'(_G15703), abolish, always).

%   (relation-located abolish 2 kernel  (SetFn ?h640  ?h642) abolish  always )  
'relation-located'(abolish, 2, kernel, 'SetFn'(_G16130, _G16136), abolish, always).

%   (relation-located sigma-H-context-list 1 kernel  (SetFn ?h639) sigma-H-context-list  always )  
'relation-located'('sigma-H-context-list', 1, kernel, 'SetFn'(_G16327), 'sigma-H-context-list', always).

%   (relation-located sigma-H-context-save 2 kernel  (SetFn ?h640  ?h642) sigma-H-context-save  always )  
'relation-located'('sigma-H-context-save', 2, kernel, 'SetFn'(_G16754, _G16760), 'sigma-H-context-save', always).

%   (relation-located sigma-H-context-dirty 1 kernel  (SetFn ?h639) sigma-H-context-dirty  always )  
'relation-located'('sigma-H-context-dirty', 1, kernel, 'SetFn'(_G16375), 'sigma-H-context-dirty', always).

%   (relation-located is-list 1 kernel  (SetFn ?h639) is-list  always )  
'relation-located'('is-list', 1, kernel, 'SetFn'(_G15703), 'is-list', always).

%   (relation-located sigma-H-context-merge 2 kernel  (SetFn ?h640  ?h642) sigma-H-context-merge  always )  
'relation-located'('sigma-H-context-merge', 2, kernel, 'SetFn'(_G16802, _G16808), 'sigma-H-context-merge', always).

%   (relation-located compile-as-predicate 2 kernel  (SetFn ?h640  ?h642) compile-as-predicate  always )  
'relation-located'('compile-as-predicate', 2, kernel, 'SetFn'(_G16754, _G16760), 'compile-as-predicate', always).

%   (relation-located wff 3 kernel  (SetFn ?h641  ?h643  ?h645) wff  always )  
'relation-located'(wff, 3, kernel, 'SetFn'(_G16365, _G16371, _G16377), wff, always).

%   (relation-located post-comment 1 kernel  (SetFn ?h639) post-comment  always )  
'relation-located'('post-comment', 1, kernel, 'SetFn'(_G15943), 'post-comment', always).

%   (relation-located compile-as-metalogical 2 kernel  (SetFn ?h640  ?h642) compile-as-metalogical  always )  
'relation-located'('compile-as-metalogical', 2, kernel, 'SetFn'(_G16850, _G16856), 'compile-as-metalogical', always).

%   (relation-located sigma-H-context-delete 1 kernel  (SetFn ?h639) sigma-H-context-delete  always )  
'relation-located'('sigma-H-context-delete', 1, kernel, 'SetFn'(_G16423), 'sigma-H-context-delete', always).

%   (relation-located subclass 2 kernel  (SetFn ?h640  ?h642) subclass  always )  
'relation-located'(subclass, 2, kernel, 'SetFn'(_G16178, _G16184), subclass, always).

%   (relation-located sigma-H-context-delete 2 kernel  (SetFn ?h640  ?h642) sigma-H-context-delete  always )  
'relation-located'('sigma-H-context-delete', 2, kernel, 'SetFn'(_G16850, _G16856), 'sigma-H-context-delete', always).

%   (relation-located sigma-H-context-delete 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-context-delete  always )  
'relation-located'('sigma-H-context-delete', 3, kernel, 'SetFn'(_G17277, _G17283, _G17289), 'sigma-H-context-delete', always).

%   (relation-located sigma-H-context-create 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-context-create  always )  
'relation-located'('sigma-H-context-create', 3, kernel, 'SetFn'(_G17277, _G17283, _G17289), 'sigma-H-context-create', always).

%   (relation-located valid-start 1 kernel  (SetFn ?h639) valid-start  always )  
'relation-located'('valid-start', 1, kernel, 'SetFn'(_G15895), 'valid-start', always).

%   (relation-located expand-filename 2 kernel  (SetFn ?h640  ?h642) expand-filename  always )  
'relation-located'('expand-filename', 2, kernel, 'SetFn'(_G16514, _G16520), 'expand-filename', always).

%   (relation-located term-to-list-hash-301 2 kernel  (SetFn ?h640  ?h642) term-to-list-hash-301  always )  
'relation-located'('term-to-list-hash-301', 2, kernel, 'SetFn'(_G17090, _G17096), 'term-to-list-hash-301', always).

%   (relation-located tokens-to-wff 2 kernel  (SetFn ?h640  ?h642) tokens-to-wff  always )  
'relation-located'('tokens-to-wff', 2, kernel, 'SetFn'(_G16418, _G16424), 'tokens-to-wff', always).

%   (relation-located sigma-H-context-changed 2 kernel  (SetFn ?h640  ?h642) sigma-H-context-changed  always )  
'relation-located'('sigma-H-context-changed', 2, kernel, 'SetFn'(_G16898, _G16904), 'sigma-H-context-changed', always).

%   (relation-located split-key-list 3 kernel  (SetFn ?h641  ?h643  ?h645) split-key-list  always )  
'relation-located'('split-key-list', 3, kernel, 'SetFn'(_G16893, _G16899, _G16905), 'split-key-list', always).

%   (relation-located split-key-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) split-key-list  always )  
'relation-located'('split-key-list', 4, kernel, 'SetFn'(_G17320, _G17326, _G17332, _G17338), 'split-key-list', always).

%   (relation-located ua-post 2 kernel  (SetFn ?h640  ?h642) ua-post  always )  
'relation-located'('ua-post', 2, kernel, 'SetFn'(_G16130, _G16136), 'ua-post', always).

%   (relation-located socket 1 kernel  (SetFn ?h639) socket  always )  
'relation-located'(socket, 1, kernel, 'SetFn'(_G15655), socket, always).

%   (relation-located socket 2 kernel  (SetFn ?h640  ?h642) socket  always )  
'relation-located'(socket, 2, kernel, 'SetFn'(_G16082, _G16088), socket, always).

%   (relation-located sigma-A-do-assert-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) sigma-A-do-assert-list  always )  
'relation-located'('sigma-A-do-assert-list', 4, kernel, 'SetFn'(_G17704, _G17710, _G17716, _G17722), 'sigma-A-do-assert-list', always).

%   (relation-located sigma-H-context-unlink 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-context-unlink  always )  
'relation-located'('sigma-H-context-unlink', 3, kernel, 'SetFn'(_G17277, _G17283, _G17289), 'sigma-H-context-unlink', always).

%   (relation-located t-cut 0 kernel  (EmptySetFn) t-cut  always )  
'relation-located'('t-cut', 0, kernel, 'EmptySetFn', 't-cut', always).

%   (relation-located otherwise 0 kernel  (EmptySetFn) otherwise  always )  
'relation-located'(otherwise, 0, kernel, 'EmptySetFn', otherwise, always).

%   (relation-located tilde-expand-filename 2 kernel  (SetFn ?h640  ?h642) tilde-expand-filename  always )  
'relation-located'('tilde-expand-filename', 2, kernel, 'SetFn'(_G16802, _G16808), 'tilde-expand-filename', always).

%   (relation-located httpd 0 kernel  (EmptySetFn) httpd  always )  
'relation-located'(httpd, 0, kernel, 'EmptySetFn', httpd, always).

%   (relation-located generate-internal-tracking-number 2 kernel  (SetFn ?h640  ?h642) generate-internal-tracking-number  always )  
'relation-located'('generate-internal-tracking-number', 2, kernel, 'SetFn'(_G17378, _G17384), 'generate-internal-tracking-number', always).

%   (relation-located httpd 1 kernel  (SetFn ?h639) httpd  always )  
'relation-located'(httpd, 1, kernel, 'SetFn'(_G15607), httpd, always).

%   (relation-located duplicated-answer 2 kernel  (SetFn ?h640  ?h642) duplicated-answer  always )  
'relation-located'('duplicated-answer', 2, kernel, 'SetFn'(_G16610, _G16616), 'duplicated-answer', always).

%   (relation-located sigma-H-constant-info 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-constant-info  always )  
'relation-located'('sigma-H-constant-info', 3, kernel, 'SetFn'(_G17229, _G17235, _G17241), 'sigma-H-constant-info', always).

%   (relation-located set-timer 1 kernel  (SetFn ?h639) set-timer  always )  
'relation-located'('set-timer', 1, kernel, 'SetFn'(_G15799), 'set-timer', always).

%   (relation-located atomic-compilation 2 kernel  (SetFn ?h640  ?h642) atomic-compilation  always )  
'relation-located'('atomic-compilation', 2, kernel, 'SetFn'(_G16658, _G16664), 'atomic-compilation', always).

%   (relation-located sigma-H-update 3 kernel  (SetFn ?h641  ?h643  ?h645) sigma-H-update  always )  
'relation-located'('sigma-H-update', 3, kernel, 'SetFn'(_G16893, _G16899, _G16905), 'sigma-H-update', always).

%   (relation-located ua-restart-inference-kb 3 kernel  (SetFn ?h641  ?h643  ?h645) ua-restart-inference-kb  always )  
'relation-located'('ua-restart-inference-kb', 3, kernel, 'SetFn'(_G17325, _G17331, _G17337), 'ua-restart-inference-kb', always).

%   (relation-located sigma-H-context-ensure-loaded 1 kernel  (SetFn ?h639) sigma-H-context-ensure-loaded  always )  
'relation-located'('sigma-H-context-ensure-loaded', 1, kernel, 'SetFn'(_G16759), 'sigma-H-context-ensure-loaded', always).

%   (relation-located collect-getPrologVars-nums-list 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) collect-getPrologVars-nums-list  always )  
'relation-located'('collect-getPrologVars-nums-list', 4, kernel, 'SetFn'(_G17704, _G17710, _G17716, _G17722), 'collect-getPrologVars-nums-list', always).

%   (relation-located file-flush 2 kernel  (SetFn ?h640  ?h642) file-flush  always )  
'relation-located'('file-flush', 2, kernel, 'SetFn'(_G16274, _G16280), 'file-flush', always).

%   (relation-located sigma-U-setup-notify 1 kernel  (SetFn ?h639) sigma-U-setup-notify  always )  
'relation-located'('sigma-U-setup-notify', 1, kernel, 'SetFn'(_G16327), 'sigma-U-setup-notify', always).

%   (relation-located sigma-H-constant-create 4 kernel  (SetFn ?h642  ?h644  ?h646  ?h648) sigma-H-constant-create  always )  
'relation-located'('sigma-H-constant-create', 4, kernel, 'SetFn'(_G17752, _G17758, _G17764, _G17770), 'sigma-H-constant-create', always).

%   (relation-located sigma-X-assert-new-1 5 kernel  (SetFn ?h643  ?h645  ?h647  ?h649  ?h651) sigma-X-assert-new-1  always )  
'relation-located'('sigma-X-assert-new-1', 5, kernel, 'SetFn'(_G18131, _G18137, _G18143, _G18149, _G18155), 'sigma-X-assert-new-1', always).

%       
end_of_file.

%   (end-of-file)  
'end-of-file'.

%                                                                                                                                                                                                                                                         
%   
end_of_file.


