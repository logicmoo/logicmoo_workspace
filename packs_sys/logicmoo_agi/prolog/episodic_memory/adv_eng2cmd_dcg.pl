(require 'asdf)

(asdf:oos 'asdf:load-op :gambol)

(defpackage :ptalk
  (:use :common-lisp :gambol))

(in-package :ptalk)

;; Load dictionary

(format t "~&Loading dictionary:~&")
; (compile-file "dictionary.lisp")
(load "dictionary.lisp")
(format t "~&")

;; Utilities

(defun ends-with (endseq seq)
 "Check if the seq ends with endseq."
 (let ((len1 (length seq))
       (len2 (length endseq)))
  (when (>= len1 len2)
   (equal (subseq seq (- len1 len2)) endseq))))

(defun symbol-ends-with (endsym sym)
 "Check if the symbol ends with endsym."
 (cond ((symbolp endsym) (ends-with (symbol-name endsym)
		                    (symbol-name sym)))
       ((listp   endsym) (some (lambda (lsym)
		                       (ends-with (symbol-name lsym)
			                          (symbol-name sym)))
	                       endsym))))

(defun replace-end (seq from to)
 "Substitute the end of seq."
 (let ((len1 (length seq))
       (len2 (length from)))
  (when (and (>= len1 len2)
             (equal (subseq seq (- len1 len2)) from))
   (concatenate 'string (subseq seq 0 (- len1 len2)) to)))) ;'

(defun symbol-replace-end (sym from to)
 "Substitute the end of sym."
 (cond ((symbolp from) (replace-end (symbol-name sym) (symbol-name from)
		                    (symbol-name to)))
       ((listp   from) (some (lambda (lfrom)
		                     (replace-end (symbol-name sym)
		                                  (symbol-name lfrom)
				                  (symbol-name to)))
	                     from))))

(defun symbol-concat (&rest sequences)
 "Concatenate symbols."
 (intern (apply #'concatenate
                (cons 'string (mapcar #'symbol-name sequences)))))

(*- (ends-with ?SUFFIX ?WORD) (lop (symbol-ends-with ?SUFFIX ?WORD)))

(*- (replace-end ?WORD ?FROM ?TO ?RESULT)
    (is ?RESULT (lop (symbol-replace-end ?WORD ?FROM ?TO))))

(*- (concat ?A ?B ?RES) (is ?RES (lop (symbol-concat ?A ?B))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Define Category for Derived Words ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(*- (base-verb-t   ?BASE-FORM) (verb-t ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (plural-verb-t ?PL-FORM)   (verb-t ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (imperfect-t   ?IMPERFECT) (verb-t ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (ing-form-t    ?ING-FORM)  (verb-t ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (past-part-t   ?PAST_PART) (verb-t ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))

(*- (base-verb-i   ?BASE-FORM) (verb-i ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (plural-verb-i ?PL-FORM)   (verb-i ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (imperfect-i   ?IMPERFECT) (verb-i ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (ing-form-i    ?ING-FORM)  (verb-i ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))
(*- (past-part-i   ?PAST_PART) (verb-i ?BASE-FORM ?PL-FORM ?IMPERFECT ?ING-FORM ?PAST-PART))

(*- (verb ?VERB) (base-verb-t   ?VERB) (cut))
(*- (verb ?VERB) (plural-verb-t ?VERB) (cut))
(*- (verb ?VERB) (imperfect-t   ?VERB) (cut))
(*- (verb ?VERB) (ing-form-t    ?VERB) (cut))
(*- (verb ?VERB) (past-part-t   ?VERB) (cut))

(*- (verb ?VERB) (base-verb-i   ?VERB) (cut))
(*- (verb ?VERB) (plural-verb-i ?VERB) (cut))
(*- (verb ?VERB) (imperfect-i   ?VERB) (cut))
(*- (verb ?VERB) (ing-form-i    ?VERB) (cut))
(*- (verb ?VERB) (past-part-i   ?VERB) (cut))

; TODO:
; (*- (verbal-noun ?VN) (verbal-noun ?BASE-VERB ?VN))

(*- (noun ?NOUN) (noun1 ?NOUN ?PL-NOUN) (cut))
(*- (noun ?NOUN) (sing-only ?NOUN))
(*- (plural-noun ?PL-NOUN) (noun1 ?NOUN ?PL-NOUN) (cut))
(*- (plural-noun ?PL-NOUN) (noun2 ?PL-NOUN))

; plural pronouns

(*- (pronoun ?PRONOUN) (pl-pronoun ?PRONOUN))

; another heuristic for adjectivable nouns

(*- (adjectivable ?NOUN) (adj ?NOUN) (cut) (fail))
(*- (adjectivable ?NOUN)
    (noun ?NOUN))

; for unknown words we use a simple heuristic to determine if
; they are possible nouns

(*- (possible-noun ?UNKN) (noun ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (plural-noun ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (pers-noun ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (verb ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (adverb ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (prep ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (conj ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (article ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (pronoun ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (prep ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (list ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN) (punctuation ?UNKN) (cut) (fail))
(*- (possible-noun ?UNKN))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Plurals of Nouns (rules) ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(*- (plural ?NOUN ?PLURAL) (base-pl ?NOUN ?PLURAL) (cut))

(*- (plural ?NOUN ?NOUN) (pl-noun ?NOUN) (cut))

(*- (plural ?NOUN ?PLURAL) (noun ?NOUN)
    (ends-with (ch sh s ss x o) ?NOUN) (cut) (concat ?NOUN es ?PLURAL))

(*- (plural ?NOUN ?PLURAL) (noun ?NOUN)
    (ends-with y ?NOUN) (cut) (replace-end ?NOUN y ies ?PLURAL))

(*- (plural ?NOUN ?PLURAL) (noun ?NOUN)
    (ends-with f ?NOUN) (cut) (replace-end ?NOUN f ves ?PLURAL))

(*- (plural ?NOUN ?PLURAL) (noun ?NOUN)
    (ends-with fe ?NOUN) (cut) (replace-end ?NOUN fe ves ?PLURAL))

(*- (plural ?NOUN ?PLURAL) (noun ?NOUN) (cut) (concat ?NOUN s ?PLURAL))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Articles and Similar  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(*- (article a))
(*- (article an))
(*- (article the))

(*- (article one)) ; by extension...

 ;;;;;;;;;;;;;;;;;;
 ;; Comparatives ;;
 ;;;;;;;;;;;;;;;;;;

(*- (more more))
(*- (most most))
(*- (than than))

(*- (comp ?WORD) (comp ?ADJ ?WORD))
(*- (superl ?WORD) (superl ?ADJ ?WORD))

 ;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Dependent sentences ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;

(*- (that that))
(*- (who who))
(*- (whom whom))
(*- (which which))

(*- (gap-pronoun that))
(*- (gap-pronoun who))
(*- (gap-pronoun whom))
(*- (gap-pronoun which))

(*- (whose whose))

 ;;;;;;;;;;;;;;;;;
 ;; Punctuation ;;
 ;;;;;;;;;;;;;;;;;

(*- (comma |, |))
(*- (dot |.|))

(defparameter punctuations '(#\. #\, #\! #\? #\;)
  "Recognized punctuation.") ; '


(defun punctuation (word)
  "Check if the word contains punctuation."
  (every (lambda (char)
	   (find char punctuations))
	 (if (symbolp word)
	     (symbol-name word)
	     word)))

(*- (punctuation |, |))
(*- (punctuation |.|))
(*- (punctuation ?PUNCT) (lop (punctuation ?PUNCT)))

 ;;;;;;;;;;;;;;;;;
 ;; Auxiliaries ;;
 ;;;;;;;;;;;;;;;;;

(*- (to-be am))
(*- (to-be are))
(*- (to-be is))

(*- (past-be was))
(*- (past-be were))

(*- (to-have have))
(*- (to-have has))

(*- (past-have had))

(*- (cond-aux would))
(*- (cond-aux should))
(*- (cond-aux might))

(*- (going going))
(*- (to to))
(*- (been been))
(*- (will will))

 ;;;;;;;;;;;;;;;;;;;;
 ;; List Utilities ;;
 ;;;;;;;;;;;;;;;;;;;;

(defun not-eq (val1 val2) (not (eq val1 val2)))

(*- (null ()))
(*- (not-null ?VAL) (lop (not-eq ?VAL ())))

(*- (symbolp ?SYM)  (lop (symbolp ?SYM)))
(*- (listp   ?LIST) (lop (listp ?LIST)))

(*- (first-rest () ?FIRST ?REST) (cut) (fail))
(*- (first-rest ?LIST ?FIRST ?REST)
    (is ?FIRST (lop (first ?LIST)))
    (is ?REST  (lop (rest ?LIST))))

(*- (prepend-not-null-one () ?LST ?LST) (cut))
(*- (prepend-not-null-one ?ELEM ?LST ?RES) (is ?RES (lop (cons ?ELEM ?LST))))

;;;;;;;;;;;;;;;;;;;;;
;; Benchmark Stuff ;;
;;;;;;;;;;;;;;;;;;;;;

; From: http://cl-cookbook.sourceforge.net/dates_and_times.html
(defmacro timing (&body forms)
    (let ((real1 (gensym))
	    (real2 (gensym))
	    (run1 (gensym))
	    (run2 (gensym))
	    (result (gensym)))
    `(let* ((, real1 (get-internal-real-time))  ; `
	      (, run1 (get-internal-run-time))
	      (, result (progn , @forms))
	      (, run2 (get-internal-run-time))
	      (, real2 (get-internal-real-time)))
	 (format *debug-io* ";;; Computation took:~%")
	 (format *debug-io* ";;;  ~f seconds of real time~%"
		 (/ (- , real2 , real1) internal-time-units-per-second))
	 (format t ";;;  ~f seconds of run time~%"
		 (/ (- , run2 , run1) internal-time-units-per-second))
	 , result)))

 ;;;;;;;;;;;;;;;;;;;;;;
 ;; Syntagms Parsers ;;
 ;;;;;;;;;;;;;;;;;;;;;;

;; Coded during my 2013 beach days... Pure Madness :P

;; Utilities

(defun add-tag (alist tag)
  (if (keywordp tag)
      (cons tag alist)
      alist))

(*- (add-tag ?SYNTAGM ?TAG ?RES)
    (is ?RES (lop (add-tag ?SYNTAGM ?TAG))))

(defun add-tag-word (word tag)
  (cons (intern (symbol-name tag) :keyword)
	(cons word nil)))

(*- (add-tag-word ?WORD ?TAG ?RES)
    (is ?RES (lop (add-tag-word ?WORD ?TAG))))

;; Options stuff

(defun just-return (foo) foo)

(*- (pass-opt () ()) (cut))
(*- (pass-opt ?IN-OPT ?OUT-OPT)
    (is ?OUT-OPT (lop (just-return ?IN-OPT))))

(defun in-options (if-opts in-opts)
  "Check if every if-option is satisfied."
  (every (lambda (opt)
	   (find opt in-opts))
	 if-opts))

(*- (in-options () ?IN-OPTS) (cut))
(*- (in-options ?IF-OPTS ?IN-OPTS)
    (lop (in-options ?IF-OPTS ?IN-OPTS)))

(defun not-in-options (if-not-opts in-opts)
  "Check if every if-not-option is NOT satisfied."
  (every (lambda (opt)
	   (not (find opt in-opts)))	
	 if-not-opts))

(*- (not-in-options () ?IN-OPTS) (cut))
(*- (not-in-options ?IF-NOT-OPTS ?IN-OPTS)
    (lop (not-in-options ?IF-NOT-OPTS ?IN-OPTS)))

(defun unset-set-options (to-unset to-set opts)
  "Update the options."
  (append to-set
	  (remove-if (lambda (opt)
		       (find opt to-unset))
		     opts)))

(*- (unset-set-options () () ?OPTS ?OPTS) (cut))
(*- (unset-set-options ?TO-UNSET ?TO-SET ?OPTS ?RES)
    (is ?RES (lop (unset-set-options ?TO-UNSET ?TO-SET ?OPTS))))

;; Mega-Macro (TM)

;; Note: we use finite state automata to recognize
;; the syntagms of sentences.

(defparameter detectors
  '(nominal ; '
    verbal
    prepositional
    pronominal
    sentence)
  "Links the kind of group with its detector-maker.")

(defmacro detector (name docstr &rest opts-and-arcs)
  "Define a finite state automaton detector."
  (let ((key-name      (intern (symbol-name name) :keyword))
	(start-name    (symbol-concat 'start-   name))
	(end-name      (symbol-concat 'end-     name))
	(arc-name      (symbol-concat 'arc-     name))
	(detector-name (symbol-concat name '-detector))

	(arcs (remove-if-not (lambda (el) (or (eq (car el) :arc)
					      (eq (car el) :jump)))
			     opts-and-arcs))

	(starts (remove-if-not (lambda (el) (eq (car el) :start))
			     opts-and-arcs))

	(ends (remove-if-not (lambda (el) (eq (car el) :end))
			     opts-and-arcs)))

    `(progn (quote (, name , docstr))  ; `

            (push ', name detectors) ; add to known detectors
	  
	    , @(mapcar (lambda (state) ; start states
			(list '*- (list start-name state)))
		      (cdar starts))
	  
	    , @(mapcar (lambda (state) ; valid end states
			(list '*- (list end-name state)))
		      (cdar ends))

	    , @(mapcar (lambda (arc) ; arcs
			
			(let* ((type  (first arc))
			       (from  (second arc))
			       (to    (third arc))
			       (cat   (when (eq type :arc) (fourth arc)))
			       (facts (remove-if #'listp (cdddr arc)))
			       (if-opt     (cdr (assoc :if     (remove-if-not #'listp arc))))
			       (if-not-opt (cdr (assoc :if-not (remove-if-not #'listp arc))))
			       (set-opt    (cdr (assoc :set    (remove-if-not #'listp arc))))
			       (unset-opt  (cdr (assoc :unset  (remove-if-not #'listp arc))))
			       (label      (cdr (assoc :label  (remove-if-not #'listp arc)))))
			
			  (cond ((eq type :jump)        ; a jump arc
				 `(*- (, arc-name , from , to ?PHRASE , label ?PHRASE ?IN-OPT ?OUT-OPT)				   
				      , @(when if-opt     (list `(in-options     , if-opt     ?IN-OPT)))
				      , @(when if-not-opt (list `(not-in-options , if-not-opt ?IN-OPT)))
				      , @(if (or unset-opt set-opt)
					    (list `(unset-set-options , unset-opt , set-opt ?IN-OPT ?OUT-OPT))
					    (list `(= ?IN-OPT ?OUT-OPT)))))
				
				((find cat detectors)   ; arc based on a detector
				 `(*- (, arc-name , from , to ?PHRASE ?SYNTAGM ?REST ?IN-OPT ?OUT-OPT)
				      , @(when if-opt     (list `(in-options     , if-opt     ?IN-OPT)))
				      , @(when if-not-opt (list `(not-in-options , if-not-opt ?IN-OPT)))

				      (lop (not-eq ?PHRASE nil))
				      (, (symbol-concat cat '-detector) ?PHRASE ?SYNTAGM ?REST ?IN-OPT ?TEMP-OPT)
				    
				      , @(if (or unset-opt set-opt)
					    (list `(unset-set-options , unset-opt , set-opt ?TEMP-OPT ?OUT-OPT))
					    (list `(= ?TEMP-OPT ?OUT-OPT)))))
				
				(t                      ; arc based on a simple prolog fact
				 `(*- (, arc-name , from , to ?PHRASE ?TAGGED-WORD ?REST ?IN-OPT ?OUT-OPT)
				      , @(when if-opt     (list `(in-options     , if-opt     ?IN-OPT)))
				      , @(when if-not-opt (list `(not-in-options , if-not-opt ?IN-OPT)))
				    
				      (first-rest ?PHRASE ?WORD ?REST)
				    
				      , @(mapcar (lambda (fact)        ; for this type of arc
						  (list fact '?WORD)) ; multiple facts are permitted
						facts)
				    
				      (add-tag-word ?WORD , (first facts) ?TAGGED-WORD)
				    
				      , @(if (or unset-opt set-opt)
					    (list `(unset-set-options , unset-opt , set-opt ?IN-OPT ?OUT-OPT))
					    (list `(= ?IN-OPT ?OUT-OPT))))))))
			
			  arcs)

	    (*- (, detector-name ?PHRASE ?FINAL-SYNTAGM ?REST ?IN-OPT ?OUT-OPT)  ; used directly by the user
	    	(, start-name ?STATE)                                            ; start from a valid state
	    	(, detector-name ?PHRASE ?STATE ?SYNTAGM ?REST ?END-STATE ?IN-OPT ?OUT-OPT)
		(add-tag ?SYNTAGM  ?END-STATE    ?SYNTAGM2)                     ; add the state tag (only if the state is a tag)
		(add-tag ?SYNTAGM2 , key-name ?FINAL-SYNTAGM))                   ; add the tag to the syntagm

	    (*- (, detector-name ?PHRASE ?STATE ?SYNTAGM ?REST ?END-STATE ?IN-OPT ?OUT-OPT)
	    	(, arc-name ?STATE ?NEW-STATE ?PHRASE ?HEAD-SYNTAGM ?REST-PHRASE ?IN-OPT ?NEW-IN-OPT)
	    	(, detector-name ?REST-PHRASE ?NEW-STATE ?REST-SYNTAGM ?REST ?END-STATE ?NEW-IN-OPT ?OUT-OPT)
	    	(prepend-not-null-one ?HEAD-SYNTAGM ?REST-SYNTAGM ?SYNTAGM))

	    (*- (, detector-name ?PHRASE ?STATE () ?PHRASE ?STATE ?IN-OPT ?IN-OPT) ; no more arcs:
		(, end-name ?STATE))                                ; are we in a valid end state?
	    	  
	    ; no more arcs, not in valid end state: fail miserably...
	  
	    )
    ))

(detector adverbal
  "Recognize stand alone adverbs."
  (:start 0) (:end 1)
  (:arc 0 1 adverb)
  (:arc 1 1 adverb))

(detector adjectival
  "Recognize list of adjectives."
  (:start 0) (:end 2)

  (:arc 0 1 adverbal)
  (:jump 0 1) ; adverb is optional

  (:arc 1 2 adj)
  (:arc 2 2 adj) ;one or more adjectives

  (:arc 2 3 conj)
  (:arc 2 3 comma)
  (:arc 3 3 conj)
  (:arc 3 3 comma) ; separated by comma or conjunctions

  (:arc 3 1 adverbal) ; but followeb by other adjectives
  (:jump 3 1))

(detector noun-adjectival
  "Recognize list of adjectived nouns."
  (:start 0) (:end 1)
  (:arc 0 1 adjectivable)
  (:arc 0 1 possible-noun)
  (:arc 1 1 adjectivable)
  (:arc 1 1 possible-noun))

(detector comparatival
  "Recognize comparisons."
  (:start 0) (:end 3 5)
  (:arc 0 1 more)
  (:arc 1 2 adjectival)
  (:arc 0 2 comp)
  (:arc 2 3 than)
  (:arc 0 4 most)
  (:arc 4 5 adjectival)
  (:arc 0 5 superl))

(detector prepositional
  "Recognize prepositional syntagms."
  (:start 0) (:end 2)
  (:arc 0 1 prep (:set :nested-nominal))

  (:jump 1 2 (:if :gap) (:unset :gap :nested-nominal) (:label :gap))

  (:arc 1 2 nominal))

(detector pronominal
  "Recognize a pronominal syntagm (possibly with gap sentence)."
  (:start 0) (:end 2 4)

  (:arc 0 1 gap-pronoun (:set :gap))
  (:arc 1 2 sentence)

  (:arc 0 3 whose (:set :nested-nominal))
  (:arc 3 4 sentence))

(detector nominal
  "Recognize nominal syntagms."
  (:start 0) (:end 15 21)

  ; article part

  (:arc 0 10 article (:unset :nested-nominal))

  ; (:arc 0 10 quantif (:unset :nested-nominal)) ; a quantificator can substitute an article

  (:jump 0 10                       ; article is skippable
	 (:if :nested-nominal)      ; if the nominal syntagm is nested in
	 (:unset :nested-nominal))  ; certain kinds of syntagm

  (:jump 0 10                       ; we can skip the article
	 (:if-not :nested-nominal)  ; for certain kind of nouns and pronouns
	 (:set :no-art))

  ; adjective part

  (:arc 10 11 adjectival)
  (:arc 10 11 comparatival) ; maybe adjectives and comparisons can coexists

  (:jump 10 11) ; adjectives and comparatives are optional

  ; noun-adjectival part

  (:arc 11 12 noun-adjectival)  ; to address the phenomenon of noun adjectivation

  (:jump 11 12) ; the adjectivable part is optional

  ; noun and pronoun part

  (:arc 11 13 pronoun (:unset :no-art)) ; a pronoun is not preceded nor followed by other nouns

  (:arc 11 13 pers-noun (:unset :no-art))
  (:arc 11 13 plural-noun (:unset :no-art))

  (:arc 12 13 noun (:if-not :no-art))
  (:arc 12 13 possible-noun (:if-not :no-art))

  (:arc 12 13 pers-noun (:unset :no-art))   ; personal or plural noun
  (:arc 12 13 plural-noun (:unset :no-art)) ; ends the sequence of nouns

  (:jump 13 20)
  (:jump 13 15)

  ; trailing prepositional or pronominal syntagm

  (:arc 20 21 prepositional)
  (:arc 20 21 pronominal))

(detector present-simple
  "Recognize present simple."
  (:start 0) (:end 1)
  (:arc 0 1 base-verb-i (:set :intransitive))
  (:arc 0 1 base-verb-t (:set :transitive)))

(detector present-continuous
  "Recognize present continuous."
  (:start 0) (:end 2)
  (:arc 0 1 to-be)
  (:arc 1 1 adverbal)
  (:arc 1 2 ing-form-i (:set :intransitive))
  (:arc 1 2 ing-form-t (:set :transitive)))

(detector present-perfect
  "Recognize present perfect."
  (:start 0) (:end 2)
  (:arc 0 1 to-have)
  (:arc 1 1 adverbal)
  (:arc 1 2 past-part-i (:set :intransitive))
  (:arc 1 2 past-part-t  (:set :transitive)))

(detector past-simple
  "Recognize past simple."
  (:start 0) (:end 1)
  (:arc 0 1 imperfect-i (:set :intransitive))
  (:arc 0 1 imperfect-t (:set :transitive)))

(detector future-simple
  "Recognize future simple."
  (:start 0) (:end 2)
  (:arc 0 1 will)
  (:arc 1 1 adverbal)
  (:arc 1 2 base-verb-i (:set :intransitive))
  (:arc 1 2 base-verb-t (:set :transitive)))

(detector future-be-going-to
  "Recognize future be-going-to."
  (:start 0) (:end 4)
  (:arc 0 1 to-be)
  (:arc 1 1 adverbal)
  (:arc 1 2 going)
  (:arc 2 3 to)
  (:arc 3 3 adverbal)
  (:arc 3 4 base-verb-i (:set :intransitive))
  (:arc 3 4 base-verb-t (:set :transitive)))

(detector present-conditional
  "Recognize present conditional."
  (:start 0) (:end 2)
  (:arc 0 1 cond-aux)
  (:arc 1 1 adverbal)
  (:arc 1 2 base-verb-i (:set :intransitive))
  (:arc 1 2 base-verb-t (:set :transitive)))

(detector present-perfect-continuous
  "Recognize present perfect continuous."
  (:start 0) (:end 3)
  (:arc 0 1 to-have)
  (:arc 1 1 adverbal)
  (:arc 1 2 been)
  (:arc 2 2 adverbal)
  (:arc 2 3 ing-form-i (:set :intransitive))
  (:arc 2 3 ing-form-t (:set :transitive)))

(detector past-continuous
  "Recognize present continuous."
  (:start 0) (:end 2)
  (:arc 0 1 past-be)
  (:arc 1 1 adverbal)
  (:arc 1 2 ing-form-i (:set :intransitive))
  (:arc 1 2 ing-form-t (:set :transitive)))

(detector verbal
  "Recognize verbal syntagms."
  (:start 0) (:end :intransitive :transitive)

  (:arc 0 0 adverbal)

  (:arc 0 1 present-simple)
  (:arc 0 1 present-continuous)
  (:arc 0 1 present-perfect)
  (:arc 0 1 past-simple)
  (:arc 0 1 future-simple)
  (:arc 0 1 future-be-going-to)
  (:arc 0 1 present-conditional)
  (:arc 0 1 present-perfect-continuous)
  (:arc 0 1 past-continuous)

  (:jump 1 12 (:if :intransitive) (:unset :intransitive))
  (:jump 1 22 (:if :transitive)   (:unset :transitive))

  ; intransitive

  (:arc 12 :intransitive nominal)
  (:jump 12 :intransitive) ; the nominal part is optional
                           ; we restrict to a position that follows immediately the verb
                           ; to avoid many inconsistent results (but it's an arbitrary decision...)

  (:arc :intransitive :intransitive adverbal)
  (:arc :intransitive :intransitive adjectival)
  (:arc :intransitive :intransitive prepositional)
  (:arc :intransitive :intransitive comparatival) ; TODO: check if it's the case to unset :gap

  ; transitive

  (:jump 22 23 (:if :gap) (:unset :gap) (:label :gap))

  (:arc 22 23 nominal) ; the nominal part is NOT optional

  (:arc 23 24 adverbal)
  (:arc 23 24 prepositional)

  (:jump 23 :transitive (:unset :gap))
  (:jump 24 :transitive (:unset :gap)))

(detector sentence
  "Recognize a sentence."
  (:start 0) (:end 2)

  (:jump 0 1 (:if :gap) (:unset :gap) (:label :gap))

  (:arc 0 1 nominal)
  (:arc 1 2 verbal))

(*- (parse ?SENTENCE ?RES)
    (sentence-detector ?SENTENCE ?RES ?REST () ?OUT)
    (null ?REST) (null ?OUT))

(defun benchme ()
  (print (timing (pl-solve-one '((parse (a red cat fastly jumped onto the table which is in the kitchen of the house) ?RES)))))
  (print ""))

(defparameter test-sentences
  '( ; '
    "After Slitscan, Laney heard about another job from Rydell, the night security man at the Chateau."
    "Rydell was a big quiet Tennessean with a sad shy grin , cheap sunglasses , and a walkie-talkie screwed permanently into one ear"
    "Concrete beams overhead had been hand-painted to vaguely resemble blond oak"
    "The chairs, like the rest of the furniture in the Chateau s lobby, were oversized to the extent that whoever sat in them seemed built to a smaller scale"
    "Rydell used his straw to stir the foam and ice remaining at the bottom of his tall plastic cup, as though he were hoping to find a secret prize"
  "A little tribute to Gibson.")

; the monkey heard about the very next ship which is yellow

 ;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Interactive Modes  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter dot-output-trigger nil "Activate dot output.")
(defparameter multiple-trees-trigger nil "Activate multiple parse trees.")

 ;;;;;;;;;;;;;;;;;
 ;; DOT Output  ;;
 ;;;;;;;;;;;;;;;;;

(defun dot-output (parsed-sentence)
  "Dump the sentence in dot format."
  (labels ((dot-output-inner (sentence &optional (coord '()))
	     (cond ((keywordp sentence)
		    (format t "~&\"~a ~a\" [style=filled, color=yellow]" sentence (reverse coord)))
		 
		   ((symbolp sentence)
		    (format t "~&\"~a ~a\" [shape=box, style=filled, color=\".7 .3 1.0\"]~%~%" sentence (reverse coord)))

		   ((equal sentence '(:gap))
		    (format t "~&\"~a ~a\" [style=filled, color=red]" (first sentence) (reverse coord)))
	
		   ((listp sentence)
		    (loop for i from 1 below (length sentence)
		       do (progn (format t "~&\"~a ~a\" -> \"~a ~a\"~%"
					 (first sentence) (reverse coord)
					 (if (listp (nth i sentence))
					     (first (nth i sentence))
					     (nth i sentence))
					 (reverse (cons i coord)))
				 (dot-output-inner (nth i sentence) (cons i coord))))))))

    (format t "digraph G {~%~%")
    (if multiple-trees-trigger
	(loop for i from 1 upto (length parsed-sentence)
	   do (dot-output-inner (nth (1- i) parsed-sentence) (list i)))
	(dot-output-inner parsed-sentence))
    (format t "~%}")))

 ;;;;;;;;;;;;;;;;
 ;; REPL Stuff ;;
 ;;;;;;;;;;;;;;;;

(defun read-sentence ()
  "Read a sentence to parse from STDIN."
  (read-from-string (concatenate 'string "(" (read-line) ")")))

(defun parse-sentence (sentence)
  "Parse a sentence."
  (if multiple-trees-trigger
      (mapcar (lambda (tree)
		(cdr (assoc '?RES tree)))
	      (pl-solve-all (list (list 'parse sentence '?RES))))
      (cdr (assoc '?RES (pl-solve-one (list (list 'parse sentence '?RES)))))))  ; '


(defun repl ()
  "Interface."

  #+SBCL (sb-ext:gc :full t)

  (format t "~&input> ")
  (finish-output nil)

  (let ((sentence (read-sentence)))
  
    (cond ((equal sentence '(dot output))     ; enable/disable dot output mode
	   (setf dot-output-trigger (not dot-output-trigger)))

	  ((equal sentence '(multiple trees)) ; enable/disable multiple parsing trees
	   (setf multiple-trees-trigger (not multiple-trees-trigger)))
	
	  (t (if dot-output-trigger           ; parse sentence
		 (dot-output (parse-sentence sentence))
		 (print (parse-sentence sentence))))))

  (finish-output nil)
  (repl))

(repl)


