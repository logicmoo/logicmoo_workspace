
(in-package "CYC")

(defvar *gameEval*  (find-or-create-constant "osim:gameEval"))
(defvar *gameProperty*  (find-or-create-constant "osim:gameProperty"))
(defvar *gameInstance* (find-or-create-constant "osim:gameInstance"))
(defvar *GAME-host* "10.10.10.193")
(defvar *GAME-PORT* 3699)

(define weakql (v1 v2) (ret (cor (eql v1 v2) (cand (numberp v1)(numberp v2)(= v1 v2))(cand (stringp v1) (cor (cand (stringp v2) (string-equal v1 v2)) (cand v2 (eql (find-constant v1) v2))))(cand	(stringp v2) (cand v1 (eql (find-constant v2) v1))))))
(define meakql (v1 v2) (ret (cor (weakql v1 v2)(cand (consp v2) (weakql v1 (car v2)))(cand (consp v1) (weakql v2 (car v1))))))
(define removal-gameApi-pos-check (values) (ret (meakql (removal-gameApi-unify-generate (first values)) (second values))))
(define removal-gameApi-neg-check (values) (ret (cnot (meakql (removal-gameApi-unify-generate (first values)) (second values)))))
(define removal-gameApi-unify-generate (value) (ret (game-eval value)))
(define removal-gameProperty-unify-generate (value) (ret (game-eval (list "gameProperty" value))))
(define removal-gameProperty-pos-check (values) (ret (meakql (removal-gameProperty-unify-generate (first values)) (second values))))
(define removal-gameInstance-unify-generate (value) (ret (game-eval (list "gameInstance" value))))
(define removal-gameInstance-pos-check (values) (ret (meakql (removal-gameInstance-unify-generate (first values)) (second values))))
(define removal-gameInstance-unify-unbound (values) (ret (game-eval (list "gameInstance" values))))

(define game-eval (outval) 
 (THROW-UNEVALUATABLE-ON-ERROR 
  (ret (clet (*retval* (*stream* (OPEN-TCP-STREAM *GAME-HOST* *GAME-PORT*)))
      (prin1 outval *stream*)
      (terpri *stream*)(force-output *stream*)
      (csetq *retval* (read *stream*))
      (close *stream*) *retval*))))

(define setup-osim3-ke ()
    (cyc-assert `(#$isa ,*gameEval* #$AsymmetricBinaryPredicate) #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameEval* #$IntangibleObjectPredicate) #$UniversalVocabularyMt)
    (cyc-assert `(#$comment ,*gameEval* "(,*gameEval* (#$TheList .....) ?Result)") #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameEval* #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
    (cyc-assert `(#$arity ,*gameEval* 2) #$UniversalVocabularyMt)
    (cyc-assert `(#$arg1Isa ,*gameEval* #$Thing) #$UniversalVocabularyMt)
    (cyc-assert `(#$arg2Isa ,*gameEval* #$Thing) #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameProperty* #$AsymmetricBinaryPredicate) #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameProperty* #$IntangibleObjectPredicate) #$UniversalVocabularyMt)
    (cyc-assert `(#$comment ,*gameProperty* "(,*gameProperty* ?Instance ?Result)") #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameProperty* #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
    (cyc-assert `(#$arity ,*gameProperty* 2) #$UniversalVocabularyMt)
    (cyc-assert `(#$arg1Isa ,*gameProperty* #$Thing) #$UniversalVocabularyMt)
    (cyc-assert `(#$arg2Isa ,*gameProperty* #$Thing) #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameInstance* #$AsymmetricBinaryPredicate) #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameInstance* #$IntangibleObjectPredicate) #$UniversalVocabularyMt)
    (cyc-assert `(#$comment ,*gameInstance* "(,*gameInstance* ?Instance ?Result)") #$UniversalVocabularyMt)
    (cyc-assert `(#$isa ,*gameInstance* #$RemovalModuleSupportedPredicate-Specific) #$CycAPIMt)
    (cyc-assert `(#$arity ,*gameInstance* 2) #$UniversalVocabularyMt)
    (cyc-assert `(#$arg1Isa ,*gameInstance* #$Thing) #$UniversalVocabularyMt)
    (cyc-assert `(#$arg2Isa ,*gameInstance* #$Thing) #$UniversalVocabularyMt))


(define setup-osim3-removals () 
    (inference-removal-module :removal-gameApi-osim-bound-unbound
     `(:sense :pos 
    	:predicate ,*gameEval* 
    	:required-pattern (,*gameEval* :fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete :input-extract-pattern (:template  (,*gameEval* (:bind the-value) :anything) (:value the-value))
    	:input-verify-pattern :anything
    	:output-generate-pattern (:call removal-gameApi-unify-generate :input)
    	:output-construct-pattern  (,*gameEval* (:value the-value) :input)
    	:documentation "(,*gameEval* <fully-bound> <not-fully-bound>)"
    	:example "(,*gameEval* -1 ?WHAT)"))
    
    (inference-removal-module :removal-gameApi-osim-bound-bound 
    `( :sense :pos 
    	:predicate ,*gameEval* 
    	:check t 
    	:required-pattern (,*gameEval* :fully-bound :fully-bound)
    	:cost-expression 0
    	;;*cheap-hl-module-check-cost*
    	:completeness :complete
    	:input-extract-pattern (:template (,*gameEval* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-check-pattern (:call removal-gameApi-pos-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    	:documentation "(,*gameEval* <fully-bound> <fully-bound>)"
    	:example "(,*gameEval* 1 -1)" ))
       
    (register-solely-specific-removal-module-predicate *gameEval*)
    
    
    (inference-removal-module :removal-gameProperty-osim-bound-unbound
     `(:sense :pos 
    	:predicate ,*gameProperty* 
    	:required-pattern (,*gameProperty* :fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete :input-extract-pattern (:template  (,*gameProperty* (:bind the-value) :anything) (:value the-value))
    	:input-verify-pattern :anything
    	:output-generate-pattern (:call removal-gameProperty-unify-generate :input)
    	:output-construct-pattern  (,*gameProperty* (:value the-value) :input)
    	:documentation "(,*gameProperty* <fully-bound> <not-fully-bound>)"
    	:example "(,*gameProperty* #osim:cyc_bot_1 ?WHAT)"))
    
    (inference-removal-module :removal-gameProperty-osim-bound-bound 
    `( :sense :pos 
    	:predicate ,*gameProperty* 
    	:check t 
    	:required-pattern (,*gameProperty* :fully-bound :fully-bound)
    	:cost-expression 0
    	;;*cheap-hl-module-check-cost*
    	:completeness :complete
    	:input-extract-pattern (:template (,*gameProperty* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-check-pattern (:call removal-gameProperty-pos-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    	:documentation "(,*gameProperty* <fully-bound> <fully-bound>)"
    	:example "(,*gameProperty* 1 -1)" ))
       
    (register-solely-specific-removal-module-predicate *gameProperty*)
    
    (inference-removal-module :removal-gameInstance-osim-bound-unbound
     `(:sense :pos 
    	:predicate ,*gameInstance* 
    	:required-pattern (,*gameInstance* :fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete :input-extract-pattern (:template  (,*gameInstance* (:bind the-value) :anything) (:value the-value))
    	:input-verify-pattern :anything
    	:output-generate-pattern (:call removal-gameInstance-unify-generate :input)
    	:output-construct-pattern  (,*gameInstance* (:value the-value) :input)
    	:documentation "(,*gameInstance* <fully-bound> <not-fully-bound>)"
    	:example "(,*gameInstance* #osim:cyc_bot_1 ?WHAT)"))
    
    (inference-removal-module :removal-gameInstance-osim-unbound-unbound
     `(:sense :pos 
    	:predicate ,*gameInstance* 
    	:required-pattern (,*gameInstance* :not-fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete 
    	:input-extract-pattern (:template (,*gameInstance* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-generate-pattern (:call removal-gameInstance-unify-unbound :input)
    	:output-construct-pattern  (,*gameInstance* (:call print :input) (:call print :input))
    	:documentation "(,*gameInstance* <not-fully-bound> <not-fully-bound>)"
    	:example "(,*gameInstance* #osim:cyc_bot_1 ?WHAT)"))
    
    (inference-removal-module :removal-gameInstance-osim-bound-bound 
    `( :sense :pos 
    	:predicate ,*gameInstance* 
    	:check t 
    	:required-pattern (,*gameInstance* :fully-bound :fully-bound)
    	:cost-expression 0 :completeness :complete
    	:input-extract-pattern (:template (,*gameInstance* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-check-pattern (:call removal-gameInstance-pos-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    	:documentation "(,*gameInstance* <fully-bound> <fully-bound>)"
    	:example "(,*gameInstance* 1 -1)" ))
       
    (register-solely-specific-removal-module-predicate *gameInstance*)
    
    (inference-removal-module
     :removal-gameApi-osim-bound-bound
     `(
       :sense :neg
       :predicate ,*gameEval*
       :check t
       :required-pattern
       (,*gameEval* :fully-bound :fully-bound)
       :cost-expression 0
       ;;*cheap-hl-module-check-cost*
       :completeness :complete
       :input-extract-pattern
       (:template 
        (,*gameEval* (:bind value-1) (:bind value-2))
        ((:value value-1) (:value value-2)))
       :input-verify-pattern
        (:anything :anything)
       :output-check-pattern
        (:call removal-gameApi-neg-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    
       :documentation
       "(,*gameEval* <fully-bound> <fully-bound>)"
       :example
       "(#$not (,*gameEval* 1 -1))"))
    
    (inference-removal-module :removal-gameApi-osim-unbound-bound
     `(
       :sense :pos
       :predicate ,*gameEval*
       :required-pattern (,*gameEval* :not-fully-bound :fully-bound)
       :cost-expression 0
       :completeness :complete
       :input-extract-pattern
       (:template 
        (,*gameEval* :anything (:bind the-value))
        (:value the-value))
       :input-verify-pattern
         :anything
       :output-generate-pattern
       (:call removal-gameApi-unify-generate :input)
       :output-construct-pattern 
       (,*gameEval* :input (:value the-value))
    
       :documentation
       "(,*gameEval* <not-fully-bound> <fully-bound>)"
       :example
       "(,*gameEval* ?WHAT -1)"
       ))
)


(define transd3 () 
    (clet ((ts-file (TRANSLATE-FILE "CynD" "osim3.lisp"))
        (fout (OPEN-TEXT "osim3.trans" :output)))
        (SHOW-TRANS-SUBL-FILE ts-file fout)(close FOUT)
        (C-BACKEND-OUTPUT-FILE-AND-HEADER-FILE ts-file "osim3.c")
        (ret ts-file)))

        

(setup-osim3-ke)
(setup-osim3-removals)
