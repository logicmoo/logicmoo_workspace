

;;(mapcar #'(lambda (a) (terpri) (print (ele 0 a)) (terpri)) (sentence-tokenize (string-tokenize "swimming pool tables are better than automatic teller machine guns") *nl-trie* :interval :diligent))

(defun sentence-preparse (string) 
  (let ((output (sentence-tokenize (string-tokenize string) *nl-trie* :string :greedy)))
   (fif (and (consp output) (= 1 (length output))) (car output) output)))


(defmacro ret-if (term)
  (ret `(clet ((temp nil)) (csetq temp ,term) (pwhen temp (ret temp)))))


(define loop-mts (a b) 
    (pwhen (equal a b) (ret T))
    ;;(ke-assert-now ` (#$genlMt  ,b ,a ) #$BaseKB)
    (ret (ke-assert-now ` (#$genlMt  ,a ,b ) #$BaseKB)))

;;(doom-assert-now `(#$isa ,(doom-Mt "TemplateParsing")  ,(doom-col "TemplateParsingMicrotheory") ) *VocabularyMt*)

(defvar *lmt* #$AllEnglishLexicalMicrotheoryPSC)
(defvar *dt* #$DoomTemplateParsingMt)
(defvar *SANDBOX* ()) ;;
(csetq *RTP-EXHAUSTIVE-KB-PARSE?* T)
(csetq *RTP-KB-PARSE-MT* ())
(csetq *RTP-KB-PARSE-MT* *SANDBOX*)
(csetq  *SANDBOX* (find-constant "ParseTreeSandboxMt"))
;;(fif *SANDBOX* (fi-kill (rename-constant *SANDBOX* "DeadParseTreeSandboxMt")))
;;(csetq  *SANDBOX* (find-or-create-constant "ParseTreeSandboxMt"))
;;(ke-assert-now `(#$isa ,*SANDBOX* #$Microtheory) #$BaseKB)

(unless *RTP-SYNTACTIC-MT* (csetq *RTP-SYNTACTIC-MT* *dt*))
(unless *RKF-MT*           (csetq *RKF-MT* *dt*))

(csetq *RTP-TRACE-LEVEL* 1)
(csetq *RTP-TRACE-AGGRESSIVELY?* nil)
(itp-sequel "what is X?")

(SubLQuoteFN ?VARS 
(callSubL ?X ?Y ?Z )




((Kappa 
			(?CITY-STRING ?PLACE) 
				(thereExists ?LIST 
					(thereExists ?STATE-STRING 
						(thereExists ?STATE 
							(and 
								(evaluate ?LIST (StringTokenizeFn ?CITY-STRING (TheList ", "))) 
								(placeName-Standard ?PLACE (NthInListFn ?LIST 1)) 
								(cityInState ?PLACE ?STATE) 
								(ist EnglishMt (geopoliticalEntityCodeDigraph ?STATE (NthInListFn ?LIST 2)))))))) "Austin, TX" ?WHERE)


;;(dt-loop *VocabularyMt*)
(define 
(define dt-loop (dt)
   #|(loop-mts *SANDBOX* dt)
    (loop-mts dt *mt*)
    (loop-mts dt *dt*)
    (loop-mts dt *RKF-MT*)
    (loop-mts dt *VocabularyMt*)
    (loop-mts *VocabularyMt*  dt)
    (loop-mts dt *lmt*)
    (loop-mts dt #$RKFParsingMt)
    (loop-mts dt *SANDBOX*)
    (loop-mts dt *RTP-SYNTACTIC-MT*)|#
    )

(dt-loop *dt*)
(dt-loop *SANDBOX*)

(defvar *state* (fif (SC-FIND-STATE-BY-ID  0)(SC-FIND-STATE-BY-ID  0) (SC-ACT-NEW-STATE)))
(defvar *sess* (SC-ACT-INITIALIZE-CONSTRUCTION-SESSION *state* "This is the first session"))
(defvar *scen* (SM-NEW-SCENARIO "This is a new scenario" *dt* *sess* ))
(defvar *state2* (SC-ACT-NEW-STATE-WITH-PHRASE "this is number two"))
(defvar *sess2* (SC-ACT-INITIALIZE-SCENARIO *state2* *sess* "this is number three"))
(defvar *temp* (SM-NEW-TEMPLATE *scen* ))
(defvar *mi* (SM-NEW-MERGE-INFO  ))
 ;;(SC-ACT-INITIALIZE-SCENARIO-FROM-EXPRESSION *state* *sess*)


(defvar *theIsa*)
(define  apply-the-isa (front list &optional (tfrm #'identity))
   (punless (consp front) (setq front (list front)))
    (clet ((res))
        (cdolist (ele list)
            (csetq ele (funcall tfrm ele))
            (unless (consp ele) (csetq ele (list ele)))
            (csetq ele (append front ele))
            (csetq res (cons ele res)))
         (ret res)))

;;(fi-kill #$RatioTemplate)
;;(fi-kill #$DateTemplate)
;;(fi-kill #$PercentTemplate)

;;(ParsingTemplateCategoryFn ?X)

(csetq *DOOM-NL-TEMPLATES* 
    (remove-duplicates '(#$OrdinalTemplate #$ResponseTemplate  #$CycConstantTemplate
     #$MovieTitleTemplate #$StringTemplate #$ImperativeTemplate #$LocationTemplate 
     #$QuestionTemplate #$ScenarioTemplate #$SinkTemplate #$InfinitivalVPTemplate
     #$PPTemplate #$TemporalTemplate
     #$VPTemplate-NPGap #$PerfectiveVPTemplate #$VerbParticipleTemplate #$STemplate-NPGap 
     #$ProgressiveVPTemplate #$PPByTemplate #$SubAtomicParticleTemplate #$RTPTensedVBarTemplate #$AdvPTemplate 
     #$NumberTemplate #$QuantityTemplate #$RTPUntensedVBarTemplate #$TokenTemplate 
     #$NBarTemplate #$RTPPassiveVBarTemplate #$PPTemporalTemplate #$VBarTemplate 
     #$GeopoliticalEntityTemplate #$VPTemplate #$RTPActiveVBarTemplate #$PossessiveTemplate 
     #$AdjPTemplate 
     ;;#$DateTemplate #$ParsingTemplateCategory #$PercentTemplate   #$RatioTemplate 
     #$STemplate-RC 
   ;;   #$STemplate #$GenericQuantityTemplate #$NPTemplate
       #$CycConstantTemplate 
       #$OrdinalTemplate #$STemplate #$GenericQuantityTemplate #$CycConstantTemplate #$NPTemplate #$ResponseTemplate #$STemplate-RC #$MovieTitleTemplate #$StringTemplate #$ImperativeTemplate #$LocationTemplate #$QuestionTemplate #$ScenarioTemplate #$TemporalTemplate #$SinkTemplate #$InfinitivalVPTemplate #$PPTemplate #$VPTemplate-NPGap #$PerfectiveVPTemplate #$VerbParticipleTemplate #$STemplate-NPGap #$ProgressiveVPTemplate #$PPByTemplate #$SubAtomicParticleTemplate #$RTPTensedVBarTemplate #$AdvPTemplate #$NumberTemplate #$QuantityTemplate #$RTPUntensedVBarTemplate #$TokenTemplate #$NBarTemplate #$RTPPassiveVBarTemplate #$PPTemporalTemplate #$VBarTemplate #$PercentTemplate #$GeopoliticalEntityTemplate #$VPTemplate #$RTPActiveVBarTemplate #$PossessiveTemplate #$AdjPTemplate #$DateTemplate #$ParsingTemplateCategory
         )))



         ;;(ALEPH-EXPORT-TARGET-PREDICATE-TO-FILES #$nlTemplateCatagoryForSpec '(#$Individual #$NPTemplate) *dt* "stem")
(define  DOOM-CYCLIFY (&rest args)
 (fif (= 1 (length args)) (setq args (car args)))
 (unless (consp args) (csetq args (list args)))
 (clet ((text "") (thing #$TheSentenceSubject)(mt *SANDBOX*)( parse ()))
  (cdolist (a args)
    (cond
       ((stringp a) (csetq text (concat text " " a)))
       ((MT? a) (csetq mt a))
       ((fort-p a) (csetq thing a))
       ((consp a) (ret (DOOM-CYCLIFY (flatten args))))))
      (clet ((*SANDBOX* ())(mt (fif mt mt (TEP-PREMISE-CONTEXT-FROM-STRING text))))
      ;;   (trace-warn (csetq parse (append parse (apply-the-isa `(#$conceptuallyRelated ,thing) (ps-get-cycls-for-phrase text :any)))))
       ;;  (trace-warn (csetq parse (append parse (apply-the-isa `(#$conceptuallyRelated ,thing) (denots-of-string  text nil :related t t )))))
         (csetq parse (remove-duplicates parse #'cdr-equal))
       ;;  (trace-warn (csetq parse (append parse (parse-a-sentence-completely  text *dt*))))
       ;;  (trace-warn (csetq parse (append parse (apply-the-isa `(#$ist-Asserted ,thing) (parse-a-question-completely text *dt*)))))
         ;;(force-print (list thing text mt parse 'from args ))
         (cdolist (type *DOOM-NL-TEMPLATES*) 
           (trace-warn (csetq parse (append parse (apply-the-isa `() (ITP-SEQUEL text type *dt* *dt*) #'cdr)))))
;;           (trace-warn (csetq parse (append parse (apply-the-isa `(CATEGORIZED-ITP-SEQUEL ,thing) (CATEGORIZED-ITP-SEQUEL text type *dt* *dt*) #'cdr)))))
         ;;(cdolist (type *DOOM-NL-TEMPLATES*) (trace-warn (csetq parse (append parse (apply-the-isa `(UTTERANCE-READER ,thing) (RKF-UTTERANCE-READER text type *dt* *dt*))))))
       ;;  (force-print (list thing text mt parse 'from args ))
       ;;  (trace-warn (csetq parse (append parse (CYCLIFY-STANFORD text))))
        ;; (trace-warn (csetq parse (append parse (cyclify text nil))))
         (force-print `(DOOM-CYCLIFY ,text ,mt  ,thing))
         (ret (remove-duplicates parse #'equal)))))
     ;;(DOOM-CYCLIFY "An actor can see books")
        
;; (car (ITP-SEQUEL "An actor can see two books on the shelf" #$NPTemplate *dt* *dt*))
 
(define  parse-room ( &optional (room '?Room) (contextstr ""))
    (clet (all (items (room-text room)))
      (cdolist (ele items)
        (format t "~&parsing ~s~&" (cons (car ele)(delete-if #'useless-string (cdr ele))))
        (clet (cycl desc named)
         (cdolist (string (cdr ele))
            (pwhen (stringp string)
              (fif (> (count #\Space string) 3)
                (csetq desc (cons string desc))
                (trace-warn (csetq cycl (append cycl (list (DOOM-CYCLIFY (localize-desc-string "" string)(car ele)))))))))
         (pwhen desc (trace-warn (csetq cycl (append cycl (list (DOOM-CYCLIFY (localize-desc-string (generate-phrase (car ele)) (join-strings desc ". ")) (car ele)))))))
         (trace-warn (csetq all  (append all `((#$ist (#$MtForTemporalThing ,(car ele)) (#$and ,@(remove-duplicates cycl #'equal)))))))))

      (ret all)))
                            
      '(cyclify "Two small curved ramps on either side of the
room lead north to the lower part of the bridge, and a large circular
skylight shows the space outside the ship")

;;(defvar *dict* (RKF-TEXTPROC-PARSE-TO-DICTIONARY  (RKF-UTTERANCE-READER  "this is a sentence"  #$STemplate *dt* *dt*)))
 ;;(DOOM-CYCLIFY "me")
 ;;(DOOM-CYCLIFY "i can see it.")
;;(RKF-TEXTPROC-PARSE-TO-DICTIONARY  )
(define SS2T (string)
  (pwhen (consp string) (csetq string (concat-words string)))
  (clet ((fnd (search " " string)))
    (fif (numberp fnd)
        (fif (= 0 fnd) (ret (SS2T (substring string 1)))
                (ret (append (SS2T (substring string 0 fnd)) (SS2T (substring string (1+ fnd)))))))
    (csetq fnd (search (concat " " string " ") " a an the some "))
    (fif (numberp fnd) (ret `((#$OptionalSome "a" "an" "the" "some"))))
    (fif (= 1 (length string)) (ret `((#$OptionalSome ,string))))

    (csetq fnd (search "#$" string))
    (fif (and (numberp fnd) (= fnd 0)) (ret (SS2T (substring string 2))))
    (csetq fnd 
      (ask-template `(#$NLPattern-Word ?WORD ?POS)
        `(#$and (#$wordStrings ?WORD ,string) (#$posForms ?WORD ?POS) (#$genls ?POS #$ClosedClassWord) (#$speechPartPreds ?POS ?PRED) (?PRED ?WORD ,string))
         #$EverythingPSC 0 1 2 10))
    (fif fnd (ret fnd))
    (csetq fnd 
      (ask-template `(#$NLPattern-Word ?WORD ?POS)
        `(#$and (#$wordStrings  ?WORD ,string) (#$posForms ?WORD ?POS) (#$speechPartPreds ?POS ?PRED) (?PRED ?WORD ,string))
         #$EverythingPSC 0 1 2 10))
    (ret (fif fnd (ret fnd) `((#$NLPattern-Exact ,string)))))) ;;(SS2T "a good idea")

(define nonnumber (char)
  (ret (not (find char "0123456789"))))

(defvar *s2t-pred* ())
(defvar *s2t-args* ())


(define concat-words (&rest string)
  (fif (= (length string) 1) (csetq string (car string)))
  (fif (cand (consp string) (= (length string) 1)) (csetq string (car string)))
  (pcond
    ((null string) (ret ""))
    ((stringp string) (ret string))
    ((cycterm-p string)
     (clet ((str (car (ask-template '?X `(#$wordStrings ,string ?X) #$EverythingPSC 1 1 1 ))))
      (ret (fif (stringp str) str (generate-phrase string)))))
    ((listp string)
        (ret (concatenate 'string (concat-words (car string)) " " (concat-words (cdr string)))))
    (T
     (write-to-string string))))

(defun sentence-preparse (string) 
 (csetq string (concat-words string))
  (let ((output (sentence-tokenize (string-tokenize string) *nl-trie* :string :greedy)))
   (fif (and (consp output) (= 1 (length output))) (car output) output)))

(define keyword-number (kw)
 (fif (keywordp kw) (csetq kw (symbol-name kw))
  (fif (not (stringp kw)) (ret 0)))
 (fif (= 0 (length kw)) (ret 0))
 (fif (search  "SUBJ" kw) (ret 1))
 (fif (search  "VERB" kw) (ret 1))
 (fif (search (substring kw 0 1) "ABCDEFGHIJKLMNOPQRSTUVWXYZ:-") (ret (keyword-number (substring kw 1))))
 (ret (string-to-number kw)))

(define min-arg-isa (pred n) 
 (clet ((all (flatten (list (argn-genl pred n) (argn-quoted-isa pred n) (argn-isa-implied pred n)))))
 (ret (car (MIN-COLS all)))))

(define posForPred (col)
  (punless col (ret #$NLWordForm))
  (clet ((res (ask-template `?POS `(#$speechPartPreds ?POS ,col) #$EverythingPSC 2 1 2 10)))
   (ret (fif res (car res) col))))

(define foc (string) (ret (find-or-create-constant string)))

(define itp-for (sent)
  (clet ((templates nil))
   (cdolist 
      (template (all-specs #$ParsingTemplateCategory))
      (pwhen (doom-warn (itp-sequel sent template))
           (print `(itp-sequel ,sent ,template))
           (csetq templates (cons template templates))))
     (ret templates)))

'(ITP-SEQUEL "the" '(#$NLTemplateForPatternFn (#$NLPatternList (#$NLPattern-Exact "the"))))

(foc "nlTemplateCatagoryForSpec")

(define template-for-col (col)
  (punless col (ret #$NPTemplate))
  (clet ((res (ask-template `?TEMPL `(#$and (#$genls ,col ?SUP) (#$nlTemplateCatagoryForSpec ?SUP ?TEMPL)) #$EverythingPSC 2 1 2 10)))
   (ret (fif res (ret (car res)) #$NPTemplate))))

(define template-for-pred-arg (pred n kw)
 (clet ((res nil))
 (csetq res (ask-template `?TEMPL `(#$and (#$argIsa ,pred ,n ?COL) (#$genls ?COL ?SUP) (#$nlTemplateCatagoryForSpec ?SUP ?TEMPL)) #$EverythingPSC 2 1 3))
 (fif res (ret `((#$NLPattern-Template ,(car res) ,kw))))
 (csetq res (ask-template `?TEMPL `(#$and (#$argGenl ,pred ,n ?COL) (#$genls ?COL ?SUP) (#$nlTemplateCatagoryForSpec ?SUP ?TEMPL)) #$EverythingPSC 2 1 3))
 (fif res (ret `((#$NLPattern-Template ,(car res) ,kw))))
 (csetq res (ask-template `?TEMPL `(#$and (#$argQuotedIsa ,pred ,n ?COL) (#$genls ?COL ?SUP) (#$nlTemplateCatagoryForSpec ?SUP ?TEMPL)) #$EverythingPSC 2 1 3))
 (fif res (ret `((#$NLPattern-Template ,(car res) ,kw))))
 (clet ((col (min-arg-isa pred n)) (pa (template-for-col col)))
  (ret `((#$NLPattern-Template ,pa ,kw))))))

'(ke-assert-now '(#$implies 
   (#$and 
     (#$isa ?PRED #$Predicate)
     (#$argIsa ?PRED ?N ?COL)
     (#$assertedPredicateArg ?ARG ?N ?PRED)
     (#$unknownSentence (#$isa ?ARG ?COL)))
  (#$isa ?ARG ?COL))  *mt* :DEFAULT :FORWARD)


(ke-unassert-now '(#$implies 
   (#$and 
     (#$isa ?PRED #$Predicate)
     (#$argIsa ?PRED ?N ?COL)
     (#$assertedPredicateArg ?PRED ?N  ?ARG )
     (#$unknownSentence (#$isa ?ARG ?COL)))
  (#$isa ?ARG ?COL))  *mt* )

 

#|
NC-RULE
EXTEND-RTP-FROM-JUST-MT-FOR-TP-TYPE
DECLARE-RTP-MADLIBS-FILE [function] NIL
*RTP-VBAR-TEMPLATE-CATEGORY?-UPDATE-LOCK* value: #<LOCK DEFINE-CACHED RTP-VBAR-TEMPLATE-CATEGORY? free 403A1FAC>
VALID-RTP-STRING-ITEM? [function] (REQ-0)
RTP-REQUIRE-SOME-ITEM? [function] (REQ-0)
DECLARE-RTP-CONSTITUENT-WEEDERS-FILE [function] NIL
RTP-REQUIRE-ONE-ITEM? [function] (REQ-0)
EXTEND-RTP-FROM-JUST-MT-FOR-TP-TYPE [function] (REQ-0 REQ-1)
RTP-MATCHED-CONSTIT-TEMPLATE [function] (REQ-0)
CLEAR-RTP-TEMPLATE-CATEGORY? [function] NIL
RTP-ITERATOR-SEMANTIC-MT [function] (REQ-0)
RTP-CHART-ENTRY-RULE [function] (REQ-0)
RTP-MATCHED-CONSTIT-WORDS [function] (REQ-0)
RTP-ITERATOR-RAW-PARSES [function] (REQ-0)
DECLARE-RTP-DATASTRUCTURES-FILE [function] NIL
RTP-CHART-ENTRY-SUBCONSTITS [function] (REQ-0)
RTP-GENERATE-MADLIBS [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
RTP-TRACE-OUT [function] (REQ-0 REQ-1)
RTP-STRING-IS-POS-ON-WU? [function] (REQ-0 REQ-1 REQ-2)
RTP-PERM-STATE-FIRST-UNUSED-TEMPLATE-ITEM [function] (REQ-0)
RTP-PARSE-W/VPP [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
GET-RTP-KB-PARSE-NODE [function] NIL
STORE-RTP-PARSES-IN-PIPELINE unbound
IS-EQUIVALENT-RTP-CHILD-ITERATOR? [function] (REQ-0 REQ-1)
RTP-CONTAINS-INVALID-SURROGATE? [function] (REQ-0 REQ-1)
RTP-CHART-ENTRY-PRINT-FUNCTION-TRAMPOLINE [function] (REQ-0 REQ-1)
*RTP-RELEVANT-TOKENIZATION* value: NIL
*DTP-RTP-ITERATOR* value: RTP-ITERATOR
MAKE-RTP-CHART-ENTRY [function] (&OPTIONAL OPT-0)
FIND-RTP-MATCHES [function] (REQ-0 REQ-1 REQ-2 REQ-3)
PUSH-TO-END-RTP-MATCHED-CONSTIT [function] (REQ-0 REQ-1)
RTP-CLEAR-RULES [function] NIL
RTP-PARSE-EXP [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
CLEAR-RTP-VBAR-TEMPLATES-CACHED [function] NIL
UNSET-RTP-KB-LOADED [function] NIL
*RTP-KB-LOADED?* value: T
RTP-BINDING-P [function] (REQ-0)
COPY-RTP-ENTRY [function] (REQ-0)
RTP-SEM-TEST? [function] (REQ-0)
SET-RTP-MATCHED-CONSTIT-WORDS [function] (REQ-0 REQ-1)
CLEAR-RTP-VBAR-TEMPLATE-CATEGORY? [function] NIL
EXTEND-RTP-FROM-MT [function] (REQ-0)
VALID-RTP-WORD-SEQUENCE? [function] (REQ-0)
_CSETF-RTP-CHART-ENTRY-WORDS [function] (REQ-0 REQ-1)
RTP-PARSE-RANKING-FINALIZE [function] NIL
NEW-RTP-PERM-STATE [function] NIL
RTP-TRACER-OUTPUT [function] (REQ-0 REQ-1)
RTP-CONTRACTION-ELEMENT? [function] (REQ-0)
NEW-RTP-STRING-ITEM [function] (REQ-0)
_CSETF-RTP-CHART-ENTRY-END [function] (REQ-0 REQ-1)
VALID-RTP-SYNTACTIC-ITEM? [function] (REQ-0)
RTP-COMPUTE-RECURSIVE-LOCATIONS [function] (REQ-0 REQ-1 REQ-2)
RTP-SYNTACTIC-CONSTRAINT [function] (REQ-0)
RTP-WORD-SEQUENCE-ITEM? [function] (REQ-0)
RTP-TOKEN-ITEM? [function] (REQ-0)
RTP-SORT-CONSTIT-PARSES [function] (REQ-0)
RTP-RECURSIVE-ITEM? [function] (REQ-0)
SET-RTP-PERM-STATE-START-INDEX [function] (REQ-0 REQ-1)
SET-RTP-ENTRY-REMAINING-TEMPLATE [function] (REQ-0 REQ-1)
RTP-PERM-STATE-UNUSED-TEMPLATE [function] (REQ-0)
*RTP-GRAPHING-FILTER* value: RTP
*DTP-RTP-CHART-ENTRY* value: RTP-CHART-ENTRY
RTP-PARSE-EXP-W/VPP [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
RTP-SENTENCE-PROBABILITY [function] (REQ-0)
RTP-WORD-ITEM-WORD [function] (REQ-0)
UNLOCATED-RTP-MATCHED-CONSTIT? [function] (REQ-0)
RTP-COMPUTE-PERMUTATIONS [function] (REQ-0 REQ-1 REQ-2)
RTP-ITERATOR-INITIALIZED [function] (REQ-0)
RTP-REQUIRED-ITEM? [function] (REQ-0)
RTP-CHART-ENTRY-TEMPLATE-INDEX [function] (REQ-0)
RTP-PARSE-EXP-RANKED [function] (REQ-0 REQ-1 REQ-2)
VALID-RTP-SLOTTED-ITEM? [function] (REQ-0)
RTP-SEM-TEST-VAR [function] (REQ-0)
RTP-ITERATOR-FORCE [function] (REQ-0)
RTP-CHART-ENTRY-WORDS [function] (REQ-0)
*RTP-RULES* value: #<HASH_TABLE TEST #<FUNCTION EQUALP Primitive Ordinary 400A1E7C> 35/512 41870CEC>
RTP-ITERATOR-ADD-RAW-PARSE [function] (REQ-0)
DECLARE-RTP-ITERATORS-FILE [function] NIL
_CSETF-RTP-ITERATOR-WFF-CHECK? [function] (REQ-0 REQ-1)
RTP-CHART-ENTRY unbound
RTP-WORD-ITEM? [function] (REQ-0)
RTP-VBAR-TEMPLATE-CATEGORY? [function] (REQ-0)
RTP-ITERATOR-PARSING-MT [function] (REQ-0)
*RTP-COMPONENT-TOKENIZATIONS* value: #<DICTIONARY (EQUAL ALIST) size=0 419005AC>
RTP-WORD-POS-PRED-ITEM? [function] (REQ-0)
_CSETF-RTP-ITERATOR-FORCE [function] (REQ-0 REQ-1)
SET-RTP-KB-LOADED [function] NIL
MAKE-UNLOCATED-RTP-MATCHED-CONSTIT [function] (REQ-0)
VALID-RTP-ACCEPT-ONE-ITEM? [function] (REQ-0)
RTP-MEMBER [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
RTP-PHRASE-PROBABILITY [function] (REQ-0)
POP-RTP-PERM-STATE-UNUSED-TEMPLATE [function] (REQ-0)
TEMPLATE-ITEM-FROM-RTP-ENTRY [function] (REQ-0)
VALID-RTP-WORD-ITEM? [function] (REQ-0)
EXTEND-RTP-WITH-TEMPLATE-ASSERTION [function] (REQ-0)
RTP-STRING-IS-PRED-ON-WU? [function] (REQ-0 REQ-1 REQ-2)
RTP-PHRASE-PARTS-OF-SPEECH [function] (REQ-0)
_CSETF-RTP-ITERATOR-INITIALIZED [function] (REQ-0 REQ-1)
*RTP-RANKING-DATA-DIR* value: data/rkf/parse-ranking/
_CSETF-RTP-ITERATOR-PARSING-MT [function] (REQ-0 REQ-1)
*SC-RTP-CATEGORY* value: NIL
RTP-RANKING-FILENAME [function] (REQ-0)
RTP-CHART-ENTRY-SEM-TEST [function] (REQ-0)
_CSETF-RTP-CHART-ENTRY-START [function] (REQ-0 REQ-1)
*RTP-DISPLAY-FILTER-RULES* value: NIL
SET-RTP-PERM-STATE-MATCHED [function] (REQ-0 REQ-1)
RTP-VBAR-TEMPLATE-CATEGORY?-INTERNAL [function] (REQ-0)
SET-RTP-MATCHED-CONSTIT-END [function] (REQ-0 REQ-1)
RTP-RULE-EXISTS? [function] (REQ-0)
*RTP-TRACE-AGGRESSIVELY?* value: NIL
ADD-RTP-RULE [function] (REQ-0)
SET-RTP-PERM-STATE-UNUSED-TEMPLATE [function] (REQ-0 REQ-1)
*RTP-USE-CACHES* value: T
INITIALIZE-RTP-KB-FEATURE [function] NIL
RTP-ITERATOR-MULTIPLE-QUANTIFICATIONS? [function] (REQ-0)
*RTP-RULES-INITIAL-SIZE* value: 256
SAVE-RTP-KB-PARSE-IF-DESIRED [function] (REQ-0)
RTP-WORD-POS-ITEM? [function] (REQ-0)
RTP-CAT-FOR-PRED [function] (REQ-0)
MAKE-RTP-MATCHED-CONSTIT [function] (REQ-0 REQ-1 REQ-2 REQ-3)
POPULATE-RTP-FROM-MT [function] (REQ-0)
NEW-RTP-ITERATOR [function] (REQ-0 &OPTIONAL OPT-1)
*RTP-RETURN-STYLE* value: SIMPLE
BEST-RTP-CAT-FOR-PREDS [function] (REQ-0)
*RTP-TRACE-LEVEL* value: 0
RTP-CHART-ENTRY-P [function] (REQ-0)
MAKE-RTP-RULE [function] (REQ-0 REQ-1 REQ-2)
RTP-ITERATOR-SBHL-RESOURCE [function] (REQ-0)
RTP-ITERATOR-P [function] (REQ-0)
RTP-VBAR-TEMPLATES-CACHED-INTERNAL [function] NIL
RTP-ITERATOR-RESULT-QUEUE [function] (REQ-0)
GET-AND-ERASE-RTP-KB-PARSE-NODE [function] NIL
*RTP-TEMPLATE-CATEGORY?-UPDATE-LOCK* value: #<LOCK DEFINE-CACHED RTP-TEMPLATE-CATEGORY? free 403A1FFC>
*RTP-CORE-CONSTANTS* value: (#$TemplateParsingMt)
RTP-KB-LOADED-P [function] NIL
_CSETF-RTP-ITERATOR-SEMANTIC-MT [function] (REQ-0 REQ-1)
RTP-MADLIBS-FORMULA-P [function] (REQ-0 &OPTIONAL OPT-1)
*RTP-TEMPLATE-CATEGORY?-HASH* value: NIL
RTP-PERM-STATE-MATCHED [function] (REQ-0)
RTP-TEMPLATE-CATEGORY?-INTERNAL [function] (REQ-0 REQ-1)
RTP-ITERATOR-TEMPLATE-CATEGORY [function] (REQ-0)
RTP-SEM-TEST-CONSTRAINT [function] (REQ-0)
RTP-PERM-STATE-COMPLETE? [function] (REQ-0)
*SC-RTP-SYNTACTIC-MT* value: NIL
RTP-CHART-ENTRY-END [function] (REQ-0)
RTP-ITERATOR-DONE [function] (REQ-0)
RTP-GENERATE-MADLIBS-VIA-DEFN [function] (REQ-0)
RTP-OPTIONAL-SOME-ITEM? [function] (REQ-0)
_CSETF-RTP-ITERATOR-STRENGTHEN? [function] (REQ-0 REQ-1)
WHILE-SAVING-RTP-PARSE-INTO-KB [function] (REQ-0 REQ-1)
RTP-TEMPLATE-CATEGORY? [function] (REQ-0 &OPTIONAL OPT-1)
*RTP-SEMANTIC-MT* value: NIL
VALID-RTP-SPEECH-PART-ITEM? [function] (REQ-0)
_CSETF-RTP-ITERATOR-MEMOIZATION-STATE [function] (REQ-0 REQ-1)
POP-RTP-PERM-STATE-UNUSED-WORDS [function] (REQ-0)
*RTP-INITIALIZED* value: T
RTP-PERM-STATE-UNUSED-WORDS [function] (REQ-0)
REMOVE-RTP-VBAR-TEMPLATE-CATEGORY? [function] (REQ-0)
RTP-CYC-TERM-ITEM [function] (REQ-0)
RTP-ITERATOR-WFF-CHECK? [function] (REQ-0)
*RTP-VBAR-TEMPLATES-CACHED-HASH* value: (NIL NIL)
RTP-WORD-ITEM-SYNTACTIC-CONSTRAINT [function] (REQ-0)
RTP-ITERATOR-NEXT [function] (REQ-0)
RTP-ITERATOR unbound
*RTP-VBAR-TEMPLATE-CATEGORY?-HASH* value: NIL
*RTP-CONTRACTIONS-TABLE* value: NIL
RTP-OPTIONAL-ONE-ITEM? [function] (REQ-0)
RTP-ENTRY-PRINT [function] (REQ-0 REQ-1 REQ-2)
REMOVE-RTP-RULE [function] (REQ-0)
_CSETF-RTP-ITERATOR-RESULT-QUEUE [function] (REQ-0 REQ-1)
RTP-NL-AGR-PRED-ITEM? [function] (REQ-0)
RESET-RTP-COMPLETELY [function] NIL
RTP-GROUPED-ITEM? [function] (REQ-0)
RTP-CAT-FOR-POS [function] (REQ-0)
VALID-RTP-ACCEPT-SOME-ITEM? [function] (REQ-0)
RTP-CHART-ENTRY-SEM [function] (REQ-0)
RTP-MATCHED-CONSTIT-END [function] (REQ-0)
_CSETF-RTP-CHART-ENTRY-SEM [function] (REQ-0 REQ-1)
VALID-RTP-EXACT-PATTERN? [function] (REQ-0)
*RTP-KB-PARSE-MT* value: NIL
*RTP-VBAR-TEMPLATES-CACHED-UPDATE-LOCK* value: #<LOCK DEFINE-CACHED RTP-VBAR-TEMPLATES-CACHED free 403A2010>
RTP-ITERATOR-STRING [function] (REQ-0)
RUN-RTP-TESTS [function] (&OPTIONAL OPT-0)
RTP-RULE-TERMINAL? [function] (REQ-0)
_CSETF-RTP-ITERATOR-TEMPLATE-CATEGORY [function] (REQ-0 REQ-1)
RTP-VBAR-TEMPLATES [function] NIL
_CSETF-RTP-ITERATOR-STRING [function] (REQ-0 REQ-1)
FIND-RTP-RULES-BY-ASSERTION [function] (REQ-0)
DECLARE-RTP-INITIALIZE-FILE [function] NIL
CATEGORIZED-RTP-PARSE-EXP [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
_CSETF-RTP-CHART-ENTRY-SUBCONSTITS [function] (REQ-0 REQ-1)
REMOVE-RTP-TEMPLATE-CATEGORY? [function] (REQ-0 &OPTIONAL OPT-1)
MAKE-RTP-ITERATOR [function] (&OPTIONAL OPT-0)
DECLARE-RTP-TYPE-CHECKERS-FILE [function] NIL
SET-RTP-MATCHED-CONSTIT-TEMPLATE [function] (REQ-0 REQ-1)
DECLARE-CB-RTP-FILE [function] NIL
CONVERT-RTP-ENTRY-TO-PARSE-RESULT [function] (REQ-0)
GET-RTP-USEFUL-SPEECH-PARTS [function] (REQ-0)
*RTP-LATEST-KB-PARSE-TREE* value: NIL
*RTP-EXHAUSTIVE-KB-PARSE?* value: NIL
*RTP-CONTRACTION-ELEMENTS* value: NIL
MAKE-RTP-PERM-STATE [function] (REQ-0 REQ-1 REQ-2 REQ-3)
_CSETF-RTP-CHART-ENTRY-BINDINGS [function] (REQ-0 REQ-1)
_CSETF-RTP-ITERATOR-SBHL-RESOURCE [function] (REQ-0 REQ-1)
_CSETF-RTP-CHART-ENTRY-RULE [function] (REQ-0 REQ-1)
SET-RTP-PERM-STATE-UNUSED-WORDS [function] (REQ-0 REQ-1)
RTP-CHART-ENTRY-SEM-TEST? [function] (REQ-0)
RTP-RULE [function] (REQ-0 REQ-1 REQ-2)
RTP-VBAR-TEMPLATES-CACHED [function] NIL
_CSETF-ITP-SEMANTICS-FORCE [function] (REQ-0 REQ-1)
_CSETF-ITP-STATE-WORKING-MT [function] (REQ-0 REQ-1)
ITP-CHILD-ITERATOR-PARENT [function] (REQ-0)
ITP-SEMANTICS-CATEGORY [function] (REQ-0)
ITP-CHILD-ITERATOR-PREV [function] (REQ-0)
ITP-NODE-SPAN [function] (REQ-0)
ITP-STATE-SEQUENCE [function] (REQ-0)
ITP-SEMANTICS unbound
ITP-NODE-P [function] (REQ-0)
ITP-NODE-SYNTACTIC-CHOICES [function] (REQ-0)
ITP-PS-PARSE-VBAR [function] (REQ-0 REQ-1 &OPTIONAL OPT-2 OPT-3)
REIFY-ITP [function] (&OPTIONAL OPT-0)
_CSETF-ITP-NODE-CHILDREN [function] (REQ-0 REQ-1)
ITP-NODE-CREATE-INITIAL-HEAD [function] (REQ-0 REQ-1 REQ-2)
_CSETF-ITP-SECTION-ITERATOR-CURRENT [function] (REQ-0 REQ-1)
ITP-RESULT-ITERATOR-RESET [function] (REQ-0)
PRIME-ITP-PUMP [function] (REQ-0 REQ-1)
_CSETF-ITP-SEMANTICS-CYCL [function] (REQ-0 REQ-1)
ITP-PS-GET-CYCLS-FOR-PHRASE [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4 REQ-5)
*RKF-UTTRDR-USE-VPP-IN-ITP* value: T
ITP-NODE-PARENT [function] (REQ-0)
ITP-CHILD-ITERATOR-PRINT-FUNCTION-TRAMPOLINE [function] (REQ-0 REQ-1)
NEW-ITP-STATE-PSP-CHART-DICTIONARY-INTERNAL [function] NIL
_CSETF-ITP-CHILD-ITERATOR-ROP-FORMULA [function] (REQ-0 REQ-1)
ITP-FIND-OR-CREATE-APPROPRIATE-CHART [function] (REQ-0 REQ-1 REQ-2 REQ-3)
ITP-PS-GET-CYCLS-FOR-PHRASE-INTERNAL [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4 REQ-5)
CONSTRUCT-ITP-NODE [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4)
ITP-CHILD-ITERATOR-SECTION [function] (REQ-0)
SORT-ITP-RESULTS [function] (REQ-0)
ITP-LONGEST-PARSE-CYCLS [function] (REQ-0)
EXTEND-ITP-STATE [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4 &OPTIONAL OPT-5 OPT-6 OPT-7)
ITP-CHILD-ITERATOR unbound
ADD-ITP-NODE-CHILD [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
_CSETF-ITP-RESULT-ITERATOR-PARSES [function] (REQ-0 REQ-1)
PRINT-ITP-STATE [function] (REQ-0 REQ-1 REQ-2)
*LAST-ITP-STATE-CREATED* value: NIL
_CSETF-ITP-SEMANTICS-SUPPORTS [function] (REQ-0 REQ-1)
ITP-SECTION-ITERATOR-CURRENT [function] (REQ-0)
ITP-CHILD-ITERATOR-CURR-ASSERTION [function] (REQ-0)
ITP-NODE-PRINT-FUNCTION-TRAMPOLINE [function] (REQ-0 REQ-1)
CATEGORIZED-ITP-SEQUEL [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
ITP-RESULT-ITERATOR-SPAN [function] (REQ-0)
NEW-ITP-SEMANTICS [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3 OPT-4)
ITP-SECTION-ITERATOR-PRINT-FUNCTION-TRAMPOLINE [function] (REQ-0 REQ-1)
NEW-ITP-RESULT-ITERATOR [function] (REQ-0 &OPTIONAL OPT-1 OPT-2)
ITP-SEMANTICS-CONFIDENCE [function] (REQ-0)
COPY-ITP-RESULT-ITERATOR [function] (REQ-0)
_CSETF-ITP-NODE-COMPONENT [function] (REQ-0 REQ-1)
*EXTEND-ITP-STATE-ANALYZE-ASSERTION?* value: NIL
ITP-SECTION-ITERATOR-SET-CURR [function] (REQ-0 REQ-1)
*DTP-ITP-STATE* value: ITP-STATE
*TRACE-ITP-STATE-EXPANSION* value: NIL
FIND-ITP-SECTION-ITERATOR [function] (REQ-0 REQ-1)
ITP-NODE-INITIALIZE-SYNTACTIC-CHOICES [function] (REQ-0 &OPTIONAL OPT-1)
ITP-PS-PARSE-VBAR-INTERNAL [function] (REQ-0 REQ-1 REQ-2 REQ-3)
PRINT-ITP-SEMANTICS [function] (REQ-0 REQ-1 REQ-2)
_CSETF-ITP-RESULT-ITERATOR-SPAN [function] (REQ-0 REQ-1)
_CSETF-ITP-NODE-SYNTACTIC-CHOICES [function] (REQ-0 REQ-1)
ITP-SECTION-ITERATOR unbound
ITP-STATE-PRINT-FUNCTION-TRAMPOLINE [function] (REQ-0 REQ-1)
ITP-CHILD-ITERATOR-NEXT [function] (REQ-0)
ITP-SEMANTICS-P [function] (REQ-0)
ITP-NODE-PERMUTATIONS [function] (REQ-0)
_CSETF-ITP-CHILD-ITERATOR-PARENT [function] (REQ-0 REQ-1)
ITP-STATE-P [function] (REQ-0)
ITP-RESULT-ITERATOR-P [function] (REQ-0)
ITP-OUTPUT-SORTING-FUNCTION [function] (REQ-0)
_CSETF-ITP-STATE-SOLUTION-SET [function] (REQ-0 REQ-1)
ITP-NODE-EQUAL? [function] (REQ-0 REQ-1)
ITP-STATE-COMPLETE-PARSES-ONLY [function] (REQ-0)
ITP-NODE-ID [function] (REQ-0)
_CSETF-ITP-RESULT-ITERATOR-SECTION-ITERATOR [function] (REQ-0 REQ-1)
ITP-CHILD-ITERATOR-CURR-CONFIDENCE [function] (REQ-0)
_CSETF-ITP-STATE-CACHE [function] (REQ-0 REQ-1)
NEW-ITP-SEMANTICS-CONF-HALF [function] (REQ-0)
*DTP-ITP-RESULT-ITERATOR* value: ITP-RESULT-ITERATOR
_CSETF-ITP-SECTION-ITERATOR-SECTION [function] (REQ-0 REQ-1)
ITP-STATE-PSP-CHARTS [function] (REQ-0)
*DTP-ITP-CHILD-ITERATOR* value: ITP-CHILD-ITERATOR
ITP-PRINT-SECTION-ITERATOR [function] (REQ-0 REQ-1 REQ-2)
MEMOIZE-ITP-ATTEMPT [function] (REQ-0 REQ-1)
ITP-NODE-NEW-PARSE [function] (REQ-0)
EXPAND-ITP-STATE-COMPLETELY [function] (REQ-0 REQ-1)
ITP-NODE-FIND-OR-INITIALIZE-SYNTACTIC-CHOICES [function] (REQ-0 REQ-1)
ITP-NODE-COMPONENT [function] (REQ-0)
ITP-NODE-SYNTACTIC-NODE [function] (REQ-0)
MAKE-ITP-NODE [function] (&OPTIONAL OPT-0)
IS-A-KNOWN-ITP-ATTEMPT? [function] (REQ-0 REQ-1)
NEW-ITP-SEMANTICS-CONF-1 [function] (REQ-0)
_CSETF-ITP-STATE-SENTENCE [function] (REQ-0 REQ-1)
ITP-CHILD-ITERATOR-CURR-SPAN [function] (REQ-0)
MAKE-ITP-SECTION-ITERATOR [function] (&OPTIONAL OPT-0)
ITP-CHILD-ITERATOR-CURR-CYCL [function] (REQ-0)
ITP-CHILD-ITERATOR-CURR-CAT [function] (REQ-0)
ITP-CHILD-ITERATOR-CURR [function] (REQ-0)
ITP-PRINT-CHILD-ITERATOR [function] (REQ-0 REQ-1 REQ-2)
MAKE-ITP-STATE [function] (&OPTIONAL OPT-0)
_CSETF-ITP-NODE-ASSERTION [function] (REQ-0 REQ-1)
_CSETF-ITP-NODE-SYNTACTIC-NODE [function] (REQ-0 REQ-1)
ITP-RESULT-ITERATOR-CURR [function] (REQ-0)
ITP-NODE-MAX-CHILD-SPAN-LENGTH [function] (REQ-0)
SC-ITP-SEQUEL [function] (REQ-0)
ITP-SECTION-ITERATOR-CURR-CAT [function] (REQ-0)
ITP-SECTION-ITERATOR-CURR-PRED [function] (REQ-0)
ITP-SECTION-ITERATOR-P [function] (REQ-0)
_CSETF-ITP-NODE-NEW-PARSE [function] (REQ-0 REQ-1)
ITP-NODE-CHILDREN [function] (REQ-0)
ITP-STATE-WORKING-MT [function] (REQ-0)
PRINT-ITP-NODE [function] (REQ-0 REQ-1 REQ-2)
_CSETF-ITP-STATE-PSP-CHARTS [function] (REQ-0 REQ-1)
REIFY-ITP-INT [function] (REQ-0)
ITP-CHOICE-NODE-FOR-COMPONENT [function] (REQ-0 REQ-1)
FIND-OR-CREATE-ITP-NODE-SYNTACTIC-NODE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2 OPT-3)
WAITP unbound
NEW-ITP-STATE-PSP-CHART-DICTIONARY [function] NIL
_CSETF-ITP-NODE-PERMUTATIONS [function] (REQ-0 REQ-1)
ITP-RESULT-ITERATOR-PARSES [function] (REQ-0)
_CSETF-ITP-STATE-CATEGORY [function] (REQ-0 REQ-1)
ITP-CHILD-ITERATOR-CURR-PRED [function] (REQ-0)
ITP-SECTION-ITERATOR-CURR-SPAN [function] (REQ-0)
ITP-SECTION-ITERATOR-CURR-ASSERTION [function] (REQ-0)
ITP-CHILD-ITERATOR-ROP-FORMULA [function] (REQ-0)
NEW-ITP-CHILD-ITERATOR [function] (REQ-0 REQ-1)
ITP-STATE-SENTENCE [function] (REQ-0)
ITP-RESULT-ITERATOR-PREV [function] (REQ-0)
ITP-NUKE-SYNTACTIC-NODES [function] (&OPTIONAL OPT-0)
ITP-STATE-CACHE [function] (REQ-0)
ITP-NODE-GET-LEGITIMATE-CHILDREN [function] (REQ-0)
ITP-NODE-TOKENIZATION [function] (REQ-0)
CYCL-OF-POSSIBLY-ITP-SEMANTICS [function] (REQ-0)
*DTP-ITP-NODE* value: ITP-NODE
ITP-SECTION-ITERATOR-CURR [function] (REQ-0)
ITP-RESULT-ITERATOR unbound
ITP-SEMANTICS-SUPPORTS [function] (REQ-0)
_CSETF-ITP-STATE-SEQUENCE [function] (REQ-0 REQ-1)
*DTP-ITP-SECTION-ITERATOR* value: ITP-SECTION-ITERATOR
_CSETF-ITP-SECTION-ITERATOR-PARENT [function] (REQ-0 REQ-1)
ITP-RESULT-ITERATOR-SECTION-ITERATOR [function] (REQ-0)
_CSETF-ITP-STATE-WORK-SET [function] (REQ-0 REQ-1)
ITP-SECTION-ITERATOR-CURR-CONFIDENCE [function] (REQ-0)
_CSETF-ITP-RESULT-ITERATOR-STYLE [function] (REQ-0 REQ-1)
NEW-ITP-STATE [function] (REQ-0 &OPTIONAL OPT-1 OPT-2)
ITP-SECTION-ITERATOR-NEXT [function] (REQ-0)
*DTP-ITP-SEMANTICS* value: ITP-SEMANTICS
DESTROY-ITP-STATE [function] (REQ-0)
ITP-SECTION-ITERATOR-PARENT [function] (REQ-0)
_CSETF-ITP-SEMANTICS-CATEGORY [function] (REQ-0 REQ-1)
ITP-RESULT-ITERATOR-DONE? [function] (REQ-0)
_CSETF-ITP-NODE-SPAN [function] (REQ-0 REQ-1)
_CSETF-ITP-SEMANTICS-CONFIDENCE [function] (REQ-0 REQ-1)
ITP-NODE-CLEAR-SYNTACTIC-CHOICES [function] (REQ-0 &OPTIONAL OPT-1)
EXPAND-ITP-STATE [function] (REQ-0 REQ-1)
ITP-SUBSPAN? [function] (REQ-0 REQ-1)
ITP-STATE-SOLUTION-SET [function] (REQ-0)
ITP-STATE-WORK-SET [function] (REQ-0)
ITP-RESULT-ITERATOR-STYLE [function] (REQ-0)
ITP-NODE-ASSERTION [function] (REQ-0)
MAKE-ITP-CHILD-ITERATOR [function] (&OPTIONAL OPT-0)
MAKE-ITP-RESULT-ITERATOR [function] (&OPTIONAL OPT-0)
NEW-ITP-SECTION-ITERATOR [function] (REQ-0 REQ-1)
IS-ITP-ROOT-CHILDREN-ITERATOR? [function] (REQ-0)
ITP-REWRITE [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
ITP-SEMANTICS-CYCL [function] (REQ-0)
UPDATE-ITP-RESULT-SECTION-ITERATOR [function] (REQ-0)
_CSETF-ITP-NODE-PARENT [function] (REQ-0 REQ-1)
NEW-ITP-ROOT-CHILDREN-ITERATOR [function] (REQ-0)
_CSETF-ITP-CHILD-ITERATOR-SECTION [function] (REQ-0 REQ-1)
EXTEND-ITP-STATE-BY-PS-PARSE [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4 REQ-5)
ITP-SECTION-ITERATOR-DONE? [function] (REQ-0)
_CSETF-ITP-NODE-ID [function] (REQ-0 REQ-1)
ITP-SECTION-ITERATOR-RESET [function] (REQ-0)
CAT-ITP [function] (&OPTIONAL OPT-0)
ITP-SEMANTICS-PRINT-FUNCTION-TRAMPOLINE [function] (REQ-0 REQ-1)
ADD-TO-ITP-SOLUTION [function] (REQ-0 REQ-1)
ITP-RESULT-ITERATOR-NEXT [function] (REQ-0)
ITP-NODE unbound
ITP-SECTION-ITERATOR-PREV [function] (REQ-0)
_CSETF-ITP-SECTION-ITERATOR-SPAN [function] (REQ-0 REQ-1)
ITP-STATE-CATEGORY [function] (REQ-0)
_CSETF-ITP-STATE-COMPLETE-PARSES-ONLY [function] (REQ-0 REQ-1)
ITP-SEMANTICS-FORCE [function] (REQ-0)
ITP-CHILD-ITERATOR-P [function] (REQ-0)
ITP-SECTION-ITERATOR-SPAN [function] (REQ-0)
MAKE-ITP-SEMANTICS [function] (&OPTIONAL OPT-0)
ITP-NODE-GET-DOMINANCE-GAFS [function] (REQ-0)
ITP-STATE unbound
ITP-CHILD-ITERATOR-RESET [function] (REQ-0)
EXTEND-ITP-STATE-MULTIPLE [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4)
ITP-SECTION-ITERATOR-CURR-CYCL [function] (REQ-0)
ITP-CHILD-ITERATOR-DONE? [function] (REQ-0)
ITP-SECTION-ITERATOR-SECTION [function] (REQ-0)
ITP-SEQUEL [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
ALPHADIGIT-TRAITP [function] (REQ-0)
LOGBITP [function] (REQ-0 REQ-1)
HAS-TRAITP [function] (REQ-0 REQ-1)
DECIMAL-POINT-TRAITP [function] (REQ-0)
DOT-TRAITP [function] (REQ-0)
EXPONENT-MARKER-TRAITP [function] (REQ-0)
MINUS-SIGN-TRAITP [function] (REQ-0)
DIGIT-TRAITP [function] (REQ-0)
ALPHABETIC-TRAITP [function] (REQ-0)
PLUS-SIGN-TRAITP [function] (REQ-0)
PACKAGE-MARKER-TRAITP [function] (REQ-0)
INVALID-TRAITP [function] (REQ-0)
RATIO-MARKER-TRAITP [function] (REQ-0)
PERMITPPCOMPPARSES value: PERMITPPCOMPPARSES


*CB-UIA-FRAME-REFRESH-SCRIPT-TEMPLATE* value: self.parent.frames[%FRAME%].location.href = self.parent.frames[%FRAME%].document.location;
LEXWIZ-PARSE-TEMPLATE-FORMULA [function] (REQ-0)
GEN-TEMPLATE-ADD-CONSTRAINT [function] (REQ-0 REQ-1)
XML-SERIALIZE-FORMULA-TEMPLATE-AS-DOCUMENT [function] (REQ-0 &OPTIONAL OPT-1)
*DTP-EBMT-TEMPLATE* value: EBMT-TEMPLATE
CREATION-TEMPLATE-EXEMPLARS [function] (REQ-0)
DOUBLY-LINKED-LIST-TEMPLATE-REMOVE-METHOD unbound
FORMULA-TEMPLATE-ORGANIZE-BY-ORDERING [function] (REQ-0 REQ-1 REQ-2)
CB-SHOW-TOE-TEMPLATE-RESULTS [function] (REQ-0)
INITIALIZE-QUA-SUBLOOP-MARKABLE-TEMPLATE unbound
QUERY-TEMPLATE-INVERTED-INDEX-INDEX-QUERY-WITH-FORMULA-AND-GLOSS-METHOD [function] (REQ-0 REQ-1 REQ-2 REQ-3)
KBQ-ASSERT-TEMPLATE-FOLDER [function] (REQ-0 REQ-1 REQ-2)
VERB-PHRASAL-TEMPLATE-CATEGORY?-INTERNAL [function] (REQ-0)
BAG-ENUMERATOR-TEMPLATE-PREVIOUS-METHOD unbound
PPH-FIND-TEMPLATE-FOR-INDEXICAL-DATES [function] (REQ-0 &OPTIONAL OPT-1)
RKF-AR-FORMULA-CHOICE-TEMPLATE [function] (REQ-0)
GEN-TEMPLATE-RECIPE-P [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
LIST-ELEMENT-TEMPLATE-ON-DELETE-METHOD unbound
*L2R-SENTENCE-LEVEL-PEG-CREATION-TEMPLATE-UPDATE-LOCK* value: #<LOCK DEFINE-CACHED L2R-SENTENCE-LEVEL-PEG-CREATION-TEMPLATE free 403A231C>
_CSETF-PC-SESSION-TEMPLATE [function] (REQ-0 REQ-1)
FORMULA-TEMPLATE-LOAD-TEMPLATE-GRAPH-MEMOIZED [function] (REQ-0 REQ-1)
TEMPLATE-TOPIC-TERM-PREFIX [function] (REQ-0)
TEMPLATE-POINTER unbound
ASK-TEMPLATE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2 OPT-3 OPT-4 OPT-5 OPT-6)
CB-RESET-CURRENT-FORMULA-TEMPLATE-EDITING-STATE [function] NIL
DESTROY-GEN-TEMPLATES-FOR-RELN [function] (REQ-0)
UPDATE-FTEMPLATE-ARGPOS-DETAIL-VALIDATION-REQUIRED [function] (REQ-0 REQ-1)
FORMULA-TEMPLATE-HAS-REFORMULATION-SPECIFICATION? [function] (REQ-0)
ENUMERATOR-TEMPLATE unbound
FOCAL-TERM-TYPE-FOR-INDUCED-TEMPLATE-TYPE [function] (REQ-0 REQ-1)
FORMULA-TEMPLATE-FOLLOW-UPS [function] (REQ-0)
PC-SESSION-TEMPLATE [function] (REQ-0)
*RFK-AR-TEMPLATE-CATEGORY-MAP* value: ((RKF-QUESTION-READER #$QuestionTemplate) (RKF-SENTENCE-READER #$STemplate))
*INTERVAL-RANGE-BINARY-UOM-NAT-RULE-ANTECEDENT-TEMPLATE* value: (((?NAT) (?UNIT ?MIN-VALUE ?MAX-VALUE) NIL) (NIL ((#$isa ?UNIT #$UnitOfMeasure) (#$operatorFormulas ?UNIT ?NAT) (#$argN ?MIN-VALUE 1 ?NAT) (#$argN ?MAX-VALUE 2 ?NAT))))
*TEMPLATE-TERMINAL-STYLE* value: STANDARD
LIST-TEMPLATE-REVERSE-METHOD unbound
ENSURE-NON-RECURSIVE-TEMPLATE [function] (REQ-0 REQ-1)
TEMPLATE-ISA unbound
TEMPLATE-CATS unbound
SKSI-TEMPLATE-APPROPRIATE-FOR-SCHEMAS? [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3)
PPH-TEMPLATE-BEST-PPH-PHRASE-FOR-FORMULA [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
TEMPLATE-LINK unbound
TEMPLATE-RULE-MEANING [function] (REQ-0)
NL-TRIE-WORD-SEMANTIC-SUPPORT-SEMTRANS-TEMPLATES [function] (REQ-0 &OPTIONAL OPT-1)
FORMULA-TEMPLATE-TOPIC [function] (REQ-0)
SUBLOOP-MARKABLE-TEMPLATE-CLEAR-ALL-MARKS-METHOD unbound
TEMPLATE-SPAN-ITEM-TEMPLATE-ITEM [function] (REQ-0)
PPH-TEMPLATE-CUSTOMIZE-TEMPLATE-FOR-FORMULA [function] (REQ-0 REQ-1 REQ-2 REQ-3)
*CB-FET-LAST-TEMPLATE-TOPIC-ID* value: NIL
MAKE-TEMPLATE-RULE [function] (&OPTIONAL OPT-0)
PPH-PHRASE-TEMPLATE-GENERATOR-NEXT-GEN-TEMPLATE [function] (REQ-0)
FTEMPLATE-GET-TEMPLATE-GLOSSES [function] (REQ-0 REQ-1 REQ-2)
RBP-TEMPLATE-WITH-ARG-SUBSTITUTION [function] (REQ-0 REQ-1)
_CSETF-FORMULA-TEMPLATE-EDITING-STATE-CURRENT-EDIT-PROBLEMS [function] (REQ-0 REQ-1)
TEMPLATE-ISA-FILTER-COL [function] NIL
UIA-SALIENT-TEMPLATE-TOPICS-FOR-INSTANCE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
_CSETF-EBMT-TEMPLATE-PARSING-INFO-LW-LINKS-PAIRS [function] (REQ-0 REQ-1)
LISPIFY-TEMPLATE-ARGS [function] (REQ-0)
LIST-ELEMENT-TEMPLATE-MEMBER-P-METHOD unbound
REMOVE-PREDICATIVE-ADJP-TEMPLATE-CATEGORY? [function] (REQ-0)
PPH-EXPAND-GEN-TEMPLATE-SET [function] (REQ-0 REQ-1 REQ-2)
DOUBLY-LINKED-QUEUE-TEMPLATE-ENQUEUE-METHOD unbound
SEQUENCE-TEMPLATE-P [function] (REQ-0)
LEXWIZ-PARSE-TEMPLATE-MT [function] (REQ-0)
EBMT-TEMPLATE-PARSING-INFO-CYCL [function] (REQ-0)
CB-HANDLE-TEMPLATE-UNASSERT [function] (REQ-0)
SM-GENERATE-TEXT-FROM-TEMPLATE-ELEMENT [function] (REQ-0 REQ-1)
INSTANTIATE-NEW-CYCL-QUERY-SPECIFICATION-FROM-TEMPLATE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
GEN-TEMPLATE-STORE-INITIALIZED? [function] NIL
CB-PREDICATE-CREATOR-SPECIFY-TEMPLATE [function] (REQ-0)
PRINT-FORMULA-TEMPLATE [function] (REQ-0 REQ-1 REQ-2)
TEMPLATE-AFTER-LINK unbound
MATCH-STRING-TEMPLATE-ITEM [function] (REQ-0 REQ-1)
RKF-CH-TEMPLATE-PARSE-CACHE-LOOKUP [function] (REQ-0)
LIST-TEMPLATE-PUSH-METHOD unbound
CB-ASSERT-FTEMPLATE-GENERATE-STATUS-GIF [function] (REQ-0)
_CSETF-FORMULA-TEMPLATE-EDITING-STATE-HIGHER-PRIORITY-TEMPLATE [function] (REQ-0 REQ-1)
EBMT-FIND-TEMPLATES-FOR-LINKAGE [function] (REQ-0 REQ-1 REQ-2)
CB-VERIFY-FORMULA-TEMPLATE-HEADER-INFORMATION [function] (REQ-0 REQ-1)
DOUBLY-LINKED-QUEUE-TEMPLATE-PEEK-METHOD unbound
ADD-GEN-TEMPLATE-EXPANSION [function] (REQ-0 REQ-1)
RKF-SALIENT-DESCRIPTOR-FORMULA-TEMPLATE-FORTS-FOR-FOCAL-TERM-TYPE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
*HTTP-POST-REQUEST-TEMPLATE-ORDER* value: (VERSION CONNECTION USER-AGENT HOST ACCEPT COOKIES CONTENT-TYPE CONTENT-LENGTH BLANK-LINE QUERY)
VERBAL-TEMPLATE-CATEGORY? [function] (REQ-0)
DECLARE-CB-TEMPLATE-OE-FILE [function] NIL
GET-SKS-HTTP-QUERY-TEMPLATE [function] (REQ-0)
_CSETF-CATEGORIZED-TEMPLATE-RULE-SET-KEY-TERMINAL [function] (REQ-0 REQ-1)
_CSETF-PPH-PHRASE-TEMPLATE-GENERATOR-RELNS-TO-USE [function] (REQ-0 REQ-1)
RKF-AR-UPDATE-ALL-TEMPLATE-CHOICES [function] (REQ-0)
NEW-GEN-TEMPLATE-STORE-ITERATOR [function] NIL
RTP-VBAR-TEMPLATES-CACHED [function] NIL
PPH-PROOF-TEMPLATE-VAR-NAME [function] (REQ-0 REQ-1)
CB-TEMPLATE-UNASSERT-INTERNALS [function] (REQ-0)
PPH-EXPAND-GEN-TEMPLATE-SETS [function] (REQ-0 REQ-1 REQ-2)
_CSETF-TEMPLATE-TOPIC-TOPIC [function] (REQ-0 REQ-1)
HANDLE-PARSING-TEMPLATE-LEXIFICATION [function] (REQ-0 REQ-1)
CB-HANDLE-TEMPLATE-BLAST [function] (REQ-0)
GET-TEMPLATE-TOPIC-LOADING-ELMT [function] (REQ-0)
FORMULA-TEMPLATE-EXAMPLES [function] (REQ-0)
END-POINTER-QUEUE-TEMPLATE-REMOVE-METHOD unbound
_CSETF-SM-TEMPLATE-PARAMETERS [function] (REQ-0 REQ-1)
_CSETF-FORMULA-TEMPLATE-EDITING-STATE-FORMULA-TEMPLATE [function] (REQ-0 REQ-1)
LEXWIZ-PARSE-TEMPLATE [function] (REQ-0)
GET-ALL-FORMULA-TEMPLATE-DEFINITION-TUPLES [function] NIL
NUMBER-TEMPLATE-CATEGORY? [function] (REQ-0)
ENSURE-POS-TEMPLATE-MAPPINGS-INITIALIZED [function] NIL
REMOVE-GEN-TEMPLATE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
*CB-FET-LAST-TEMPLATE-MT* value: NIL
FORMULA-TEMPLATE-SET-EXAMPLES [function] (REQ-0 REQ-1)
SM-TEMPLATE-ID [function] (REQ-0)
PPH-PHRASE-TEMPLATE-GENERATOR-GEN-TEMPLATE-SETS [function] (REQ-0)
GET-GENERIC-TEMPLATES-FOR-CATEGORY [function] (REQ-0 REQ-1)
*MWP-SEM-TEMPLATES-FOR-REIFIED-AFFIX-UPDATE-LOCK* value: #<LOCK DEFINE-CACHED MWP-SEM-TEMPLATES-FOR-REIFIED-AFFIX free 403A1AAC>
GEN-TEMPLATE-CONSTRAINT-PASSES? [function] (REQ-0 REQ-1 REQ-2)
_CSETF-FORMULA-TEMPLATE-FORMULA [function] (REQ-0 REQ-1)
_CSETF-SM-TEMPLATE-CONSTRAINTS [function] (REQ-0 REQ-1)
GET-TEMPLATE-TOPIC-IN-XML [function] (REQ-0 &OPTIONAL OPT-1)
AFTER-CREATING-FUNCTION-SPECS-FOR-TEMPLATE-ID [function] (REQ-0)
FTEMPLATE-GET-TEMPLATE-CANDIDATE-REPLACEMENTS-FOR-POSITION [function] (REQ-0 REQ-1)
CB-UPDATE-FORMULA-TEMPLATE-ARGPOS-DETAILS [function] (REQ-0 REQ-1 REQ-2)
PPH-PHRASE-TEMPLATES-FOR-FORMULA-W/RELN [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3 OPT-4 OPT-5)
BAG-TEMPLATE-ADD-METHOD unbound
FORMULA-TEMPLATE-EDITING-STATE-FORMULA-TEMPLATE [function] (REQ-0)
WF-COL-TEMPLATE-COMBO? [function] (REQ-0 REQ-1 REQ-2)
DOUBLY-LINKED-LIST-TEMPLATE-MEMBER-P-METHOD unbound
INDUCED-FORMULA-TEMPLATE-FORTS-FOR-FOCAL-TERM-TYPE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
REM-AR-STATE-ASSERT-TEMPLATE [function] (REQ-0 REQ-1)
END-POINTER-LIST-TEMPLATE-REMOVE-METHOD unbound
TEMPLATE-CONTENTS-SUBLIST unbound
XML-TEMPLATE-TOPIC-ASSERTIONS-CURRENT-REVISION [function] NIL
_CSETF-TEMPLATE-TOPIC-TITLE [function] (REQ-0 REQ-1)
PPH-BEST-PHRASE-TEMPLATE-FOR-FORMULA-W/RELN [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3 OPT-4)
CB-HANDLE-TEMPLATE-PARSING [function] (REQ-0)
EBMT-TEMPLATE-P [function] (REQ-0)
POPULATE-TEMPLATE-CATEGORY-FILL-WORDS-FROM-MT [function] (REQ-0 &OPTIONAL OPT-1)
_CSETF-KB-QUERY-STATE-TEMPLATE-FOLDER-MT-PAIRS [function] (REQ-0 REQ-1)
SM-ACT-DISCARD-TEMPLATE [function] (REQ-0 REQ-1)
CB-DRAW-FORMULA-TEMPLATE-BASE-EL-FORMULA [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
*CB-UIAT-CONCEPT-CREATOR-SHOW-TEMPLATE-TOPICS?* value: NIL
VALID-DATE-TEMPLATE-CHAR [function] (REQ-0)
SEQUENCE-ELEMENT-TEMPLATE-P [function] (REQ-0)
PPH-PHRASE-TEMPLATE-GENERATOR-PRINT-FUNCTION-TRAMPOLINE [function] (REQ-0 REQ-1)
AR-TEMPLATE-PROMPT [function] (REQ-0)
EBMT-TEMPLATE-INDEX-SERVER-INFO [function] (REQ-0)
_CSETF-TEMPLATE-TOPIC-SUBTOPICS [function] (REQ-0 REQ-1)
_CSETF-LEXWIZ-TEMPLATE-ARGS [function] (REQ-0 REQ-1)
CB-LINK-TEMPLATE-OE [function] (&OPTIONAL OPT-0)
*COUNTED-TEMPLATE-SET* value: NIL
_CSETF-PPH-PHRASE-TEMPLATE-GENERATOR-NL-PREDS [function] (REQ-0 REQ-1)
INSTANTIATE-TEMPLATE unbound
CB-TEMPLATE-CHANGE-ASSERTION-PROPERTIES [function] (REQ-0)
*FTEMPLATE-CONSTRAINT-TO-COLLECTION-SKIPLIST* value: ((#$SomeExampleFn #$TimeInterval))
PPH-EXPANDED-TEMPLATE-FORMULA-WF? [function] (REQ-0 &OPTIONAL OPT-1 OPT-2)
_CSETF-AR-STATE-TERM-TEMPLATES [function] (REQ-0 REQ-1)
NOTE-AR-TEMPLATE-CHOICE-TABLE [function] (REQ-0 REQ-1)
DOUBLY-LINKED-QUEUE-TEMPLATE-INITIALIZE-QUA-DOUBLY-LINKED-QUEUE-METHOD unbound
SET-DEFINITIONAL-QUESTION-THING-TEMPLATE-PARAMETERS [function] (REQ-0 REQ-1)
PPH-PHRASE-TEMPLATE-GENERATOR-RELNS-TO-USE-QUEUE [function] (REQ-0)
DOUBLY-LINKED-QUEUE-TEMPLATE-GET-CONTENTS-METHOD unbound
TEMPLATE-PARENT-LINK unbound
ASSERT-FORMULA-TEMPLATE-TYPE-SUBTOPIC [function] (REQ-0 REQ-1)
FORMULA-TEMPLATE-FOCAL-TERM [function] (REQ-0)
PPH-FIND-INDEXICAL-TEMPLATE-FOR-DATE [function] (REQ-0)
*FREE-PPH-PHRASE-TEMPLATE-GENERATORS* value: #<QUEUE size=2 418E0924>
_CSETF-TEMPLATE-SPAN-ITEM-END [function] (REQ-0 REQ-1)
ADD-TEMPLATE-WITH-FORMULA-AND-GLOSS [function] (REQ-0 REQ-1 REQ-2)
LIST-TEMPLATE-BUT-LAST-METHOD unbound
INDUCED-FORMULA-TEMPLATE-SUBTOPICS-FOR-TYPE [function] (REQ-0 REQ-1)
*TEMPLATE-RULE-SXHASHING-PRIME-B* value: 7
REMOVE-GEN-TEMPLATE-AS-FROM-SETS [function] (REQ-0 REQ-1)
EXTRACT-HEAD-TYPE-AND-ARGUMENT-ROLES-FROM-SEMTRANS-TEMPLATE [function] (REQ-0 REQ-1)
TEMPLATE-TOPIC-TOPIC [function] (REQ-0)
SM-TEMPLATE-PARAM-SUBS [function] (REQ-0)
TEMPLATE-COLLECTION-CONTENTS unbound
SUBLOOP-RESERVED-INITIALIZE-QUERY-TEMPLATE-INVERTED-INDEX-ENTRY-COMPUTER-CLASS [function] (REQ-0)
SM-TEMPLATE unbound
TEMPLATE-TOPIC-DEFINITIONAL-MT [function] (REQ-0)
PPH-QUESTION-TEMPLATE-UNIFY [function] (REQ-0 REQ-1)
SEQUENCE-TEMPLATE unbound
DOUBLY-LINKED-LIST-TEMPLATE-APPEND-METHOD unbound
MAKE-NL-TAG-TEMPLATE [function] (REQ-0 REQ-1 REQ-2)
PPH-TEMPLATED-PROOF? [function] (REQ-0)
REDUCE-RTP-BY-TEMPLATE-ASSERTION [function] (REQ-0)
PPH-CHOOSE-PROOF-TEMPLATE [function] (REQ-0 REQ-1)
END-POINTER-LIST-TEMPLATE-PUSH-METHOD unbound
PRINT-TEMPLATE-RULE [function] (REQ-0 REQ-1 REQ-2)
ADD-GEN-TEMPLATE-QUERY-SENTENCE [function] (REQ-0 REQ-1)
KBQ-ENSURE-TEMPLATE-FOLDERS [function] (REQ-0 REQ-1)
MAKE-QUESTION-TEMPLATE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
*MAKE-FTEMPLATE-LOADING-SUPPORTING-ASK-BROWSABLE?* value: NIL
_CSETF-EBMT-TEMPLATE-PARSING-INFO-LW [function] (REQ-0 REQ-1)
NEW-TEMPLATE-RULE [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3 OPT-4 OPT-5)
DOUBLY-LINKED-QUEUE-TEMPLATE-PRINT-METHOD unbound
END-POINTER-QUEUE-TEMPLATE-ENQUEUED-P-METHOD unbound
REFORMULATOR-TEMPLATE-CLAUSES? [function] (REQ-0)
CLEAR-L2R-SENTENCE-LEVEL-PEG-CREATION-TEMPLATE-SENTENCES [function] NIL
*TEMPLATE-META-MARKER-MAPPINGS* value: #<DICTIONARY (EQL ALIST) size=13 41900584>
CATEGORIZED-TEMPLATE-RULE-SET unbound
EBMT-DELETE-TEMPLATE-INT [function] (REQ-0 REQ-1 REQ-2)
*ALLOW-VARS-IN-THIS-PARSING-TEMPLATE* value: T
_CSETF-PPH-PHRASE-TEMPLATE-GENERATOR-MT [function] (REQ-0 REQ-1)
PPH-PHRASE-TEMPLATE-GENERATOR-GEN-TEMPLATES [function] (REQ-0)
CB-SC-SHOW-TEMPLATE-PARAMETER [function] (REQ-0)
END-POINTER-QUEUE-TEMPLATE unbound
RTP-OPTIONAL-TEMPLATE-ITEM? [function] (REQ-0)
EXPANDED-GEN-TEMPLATE-SETS-FOR-RELN [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
PPH-PHRASE-TEMPLATE-GENERATOR-SPECIFIED-RELN [function] (REQ-0)
GET-GEN-TEMPLATE-ASSERTION [function] (REQ-0)
SET-GEN-TEMPLATE-STORE-VALUE [function] (REQ-0 REQ-1)
?TEMPLATE-TYPE unbound
FTEMPLATE-GET-TEMPLATE-FORMULA [function] (REQ-0 REQ-1)
FORMULA-TEMPLATE-IS-SINGLE-ENTRY? [function] (REQ-0)
AR-TEMPLATE unbound
EBMT-TEMPLATE-LINKS [function] (REQ-0)
DECLARE-FORMULA-TEMPLATE-UTILITIES-FILE [function] NIL
OLD-COMPUTE-QUERY-TEMPLATE [function] (REQ-0)
DO-PPH-PHRASE-TEMPLATES-FOR-FORMULA [function] (REQ-0 REQ-1)
LIST-ELEMENT-TEMPLATE-GET-COLLECTIONS-METHOD unbound
GET-EVENT-LEARNING-TEMPLATED-CYCL [function] (REQ-0 REQ-1)
REMOVE-NC-RULE-TEMPLATE [function] (REQ-0 REQ-1)
RKF-AR-UPDATE-TEMPLATE-CHOICES-FOR-TEMPLATE [function] (REQ-0 REQ-1)
AR-STATE-TERM-TEMPLATES [function] (REQ-0)
PSP-DEVISE-DEVERBAL-NOUN-TEMPLATE [function] (REQ-0 REQ-1)
GKE-TEMPLATE-GET-VALUE [function] (REQ-0 REQ-1 REQ-2)
EL-TEMPLATE-ARG? [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
REGISTER-TEMPLATE-PARSER [function] NIL
COMPILE-TEMPLATE-EXPRESSION [function] (REQ-0 REQ-1)
INTERPRET-TEMPLATE-CATEGORY-FILL-WORD-ASSERTION [function] (REQ-0)
RBP-TEMPLATE-PREDICATE [function] (REQ-0 &OPTIONAL OPT-1)
SM-DE-APPLY-GEN-POINT-TO-TEMPLATE-CONSTRAINTS [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4)
_CSETF-TEMPLATE-TOPIC-TEMPLATES [function] (REQ-0 REQ-1)
QUERY-LIBRARY-TEMPLATE-FORWARD-RULES [function] (REQ-0)
CREATE-GENTEMPLATE-SENTENCE [function] (REQ-0 REQ-1)
CLEAR-GEN-TEMPLATE-QUERY-SENTENCE-INDEX [function] NIL
CLEAR-VERB-PHRASAL-TEMPLATE-CATEGORY? [function] NIL
PPH-SHOW-PROOF-TEMPLATE-BODY [function] (REQ-0 REQ-1 REQ-2 REQ-3)
TEMPLATE-PARSE-COMPLEXITY-WEIGHT [function] (REQ-0)
GET-FORMULA-TEMPLATE-FROM-ID [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
DO-TEMPLATES-FOR-TERMINAL [function] (REQ-0 REQ-1)
RBP-TEMPLATE-WITH-STRENGTHENED-ARGS [function] (REQ-0 REQ-1)
QSL-PPH-PHRASE-TO-TEMPLATE-SYNTAX-INT [function] (REQ-0 REQ-1)
KBQ-ENSURE-TEMPLATE-DEFINITION [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4)
CHECK-TEMPLATE-CANDIDATE-REPLACEMENT-GAF-PARSING [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
CFASL-OUTPUT-OBJECT-FORMULA-TEMPLATE-METHOD [function] (REQ-0 REQ-1)
REMOVAL-ASK-TEMPLATE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2 OPT-3 OPT-4)
TEMPLATE-ITEM-POS [function] (REQ-0)
GET-QUERY-TEMPLATE-INVERTED-INDEX-PHYSICAL-INDEX unbound
PARSE-TEMPLATES-FOR-TERM [function] (REQ-0)
GET-PPH-PHRASE-TEMPLATE-GENERATOR [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3 OPT-4 OPT-5)
EBMT-INDEX-TEMPLATE-INPUT [function] (REQ-0)
NEW-GKE-TEMPLATE-SPECIFICATION [function] (REQ-0 REQ-1)
PPH-SORT-GEN-TEMPLATES-BY-VERBOSITY [function] (REQ-0)
TEMPLATE-INTERNAL-STORAGE unbound
*PPH-PHRASE-TEMPLATES-FOR-FRAMES* value: ((#$DitransitiveNP-NPFrame #$PhraseFormFn #$NLSentence (#$ConcatenatePhrasesFn (#$TermParaphraseFn-NP SUBJECT) (#$BestHeadVerbForInitialSubjectFn VERB) (#$TermParaphraseFn-NP OBLIQUE-OBJECT) (#$TermParaphraseFn-NP OBJECT))) (#$TransitiveNPFrame #$PhraseFormFn #$NLSentence (#$ConcatenatePhrasesFn (#$TermParaphraseFn-NP SUBJECT) (#$HeadWordOfPhraseFn (#$PhraseFormFn #$VerbPhrase (#$ConcatenatePhrasesFn (#$HeadWordOfPhraseFn (#$PhraseCycLFn VERB-DENOT (#$BestHeadVerbForInitialSubjectFn VERB))) (#$TermParaphraseFn-NP OBJECT)))))) (#$IntransitiveVerbFrame #$PhraseFormFn #$NLSentence (#$ConcatenatePhrasesFn (#$TermParaphraseFn-NP SUBJECT) (#$HeadWordOfPhraseFn (#$PhraseFormFn #$VerbPhrase (#$PhraseCycLFn VERB-DENOT (#$BestHeadVerbForInitialSubjectFn VERB)))))))
GENTEMPLATE-PHRASE-FROM-PPH-PHRASE-INT [function] (REQ-0)
REINITIALIZE-GEN-TEMPLATES-REFERENCING-PHRASE-FN [function] (REQ-0)
VARIABLE-LENGTH-SEQUENCE-TEMPLATE-GET-NTH-METHOD unbound
BAG-TEMPLATE-MEMBER-P-METHOD unbound
CB-DRAW-ARGPOS-DETAILS-FOR-CURRENT-FORMULA-TEMPLATE [function] (REQ-0 REQ-1 REQ-2)
_CSETF-GEN-TEMPLATE-PHRASE [function] (REQ-0 REQ-1)
?RULE-GLOSS-TEMPLATE unbound
DOUBLY-LINKED-QUEUE-TEMPLATE-GET-ELEMENT-COUNT-METHOD unbound
*TEMPLATE-RULE-DEFAULT-MEANING* value: #$assertTemplate
EBMT-TEMPLATE-PARSING-INFO unbound
NL-TEMPLATES-FROM-SENTENCE-GLOSS-INFO [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4 &OPTIONAL OPT-5)
GEN-TEMPLATE-STORE-GREP [function] (REQ-0 &OPTIONAL OPT-1 OPT-2)
FET-TOPIC-FORT-HAS-TEMPLATES? [function] (REQ-0 REQ-1)
COLLECTION-TEMPLATE-REMOVE-METHOD unbound
_CSETF-LEXWIZ-PARSE-TEMPLATE [function] (REQ-0 REQ-1)
REFORMULATOR-TEMPLATE-SHARED-VAR-BINDINGS [function] (REQ-0)
DO-PPH-APPLY-TEMPLATE [function] (REQ-0 REQ-1)
ADD-AR-TEMPLATE-AGGLOMERATION [function] (REQ-0 REQ-1)
CLEAR-QUERY-LIBRARY-TEMPLATE-FORWARD-RULES [function] NIL
PSP-QUANTIFY-TEMPLATES [function] (REQ-0)
XML-SERIALIZE-TEMPLATE-TOPIC [function] (REQ-0 &OPTIONAL OPT-1)
DOUBLY-LINKED-LIST-TEMPLATE-POSITION-METHOD unbound
DOUBLY-LINKED-LIST-TEMPLATE-FIND-NTH-LINK-METHOD unbound
*MWP-SEM-TEMPLATES-FOR-REIFIED-AFFIX-HASH* value: NIL
SXHASH-AR-TEMPLATE-METHOD [function] (REQ-0)
SUBCOL-JUSTIFICATIONS-FROM-TEMPLATE [function] (REQ-0)
DOUBLY-LINKED-LIST-TEMPLATE-PUSH-METHOD unbound
DECLARE-FORMULA-TEMPLATES-FILE [function] NIL
FTEMPLATE-ASSERTION-NON-EDITABLE? [function] (REQ-0 REQ-1)
SEQUENCE-TEMPLATE-GET-SUBSEQUENCE-METHOD unbound
SUBLOOP-MARKABLE-TEMPLATE-INITIALIZE-QUA-SUBLOOP-MARKABLE-TEMPLATE-METHOD unbound
LEXWIZ-TEMPLATE [function] (REQ-0)
GENLS-TEMPLATES unbound
FORMULA-TEMPLATE-LOAD-TOPIC-TEMPLATE-ORDERING [function] (REQ-0 REQ-1)
XML-SERIALIZE-ASSERTIONS-FOR-FORMULA-TEMPLATE-INSTANCE [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3)
PPH-PHRASE-TEMPLATE-GENERATOR-DONE? [function] (REQ-0)
CREATION-TEMPLATE-FORWARD-RULES-VIA-EXEMPLARS [function] (REQ-0)
PPH-QUERY-SENTENCE-TEMPLATE-KEYWORDS [function] (REQ-0)
PPH-PHRASE-TEMPLATE-GENERATOR-SEARCH-LIMIT [function] (REQ-0)
PPH-PHRASE-TEMPLATE-GENERATOR-MT [function] (REQ-0)
*DTP-REFORMULATOR-TEMPLATE* value: REFORMULATOR-TEMPLATE
VALID-TIME-TEMPLATE-CHAR [function] (REQ-0)
GET-DEFINITIONAL-QUESTION-GENERAL-TEMPLATE-PARAMETERS [function] (REQ-0)
TEMPLATE-TOPIC-ADD-TEMPLATE [function] (REQ-0 REQ-1)
FORMULA-TEMPLATE-QUERY-SPECIFICATION [function] (REQ-0)
SM-REM-SCENARIO-TEMPLATE [function] (REQ-0 REQ-1)
UIA-AVAILABLE-TEMPLATE-TOPIC-GENLS [function] (REQ-0 &OPTIONAL OPT-1)
NEW-TEMPLATE-SPAN-ITEM [function] (REQ-0 REQ-1 REQ-2)
TEMPLATE-RULE-CATEGORY [function] (REQ-0)
PSP-COERCE-TEMPLATE-TO-COLLECTION-MEMOIZED [function] (REQ-0 REQ-1 REQ-2)
PLACE-GEN-TEMPLATE-IN-SETS [function] (REQ-0 REQ-1)
GEN-TEMPLATE-STORE-PRESENT-P [function] NIL
PPH-PHRASE-TEMPLATE-GENERATOR-DONE-P [function] (REQ-0)
_CSETF-TEMPLATE-RULE-TERMINALS [function] (REQ-0 REQ-1)
*NC-RULE-TEMPLATE* value: #$ncRuleTemplate
*CB-UIA-AGENDA-SNAPSHOT-SIGNATURE-TEMPLATE* value:
var uia_meta_agenda_id   = ~A;
var uia_agenda_id        = ~A;
var uia_current_total    = ~A;
var uia_current_finished = ~A;
KBQ-TEMPLATE [function] (REQ-0)
TRANSFORM-META-MARKERS-IN-TEMPLATE-EXPRESSION [function] (REQ-0)
LOAD-FORMULA-TEMPLATE-DETAILS-FROM-KB [function] (REQ-0 REQ-1)
INTERMEDIATE-CYCL-TEMPLATE-FOR-RMP-FORMULA [function] (REQ-0)
SM-TEMPLATE-STATE [function] (REQ-0)
SEQUENCE-TEMPLATE-GET-NTH-METHOD unbound
DOUBLY-LINKED-LIST-TEMPLATE-SET-CONTENTS-METHOD unbound
TEMPLATE-RULE-TEMPLATE [function] (REQ-0)
PPH-IDENTITY-TEMPLATE [function] NIL
DECLARE-GEN-TEMPLATE-STORE-STALENESS-UNACCEPTABLE [function] NIL
COUNT-ASSERTED-FORMULA-TEMPLATE-IDS-FOR-TYPE [function] (REQ-0 &OPTIONAL OPT-1)
TEMPLATE-TOPIC-QUERY-MT-CAN-SEE-ALL-ASSERTION-MTS [function] (REQ-0)
REMOVE-RELEVANT-TEMPLATE-MTS [function] (REQ-0)
DOUBLY-LINKED-LIST-TEMPLATE-GET-CONTENTS-METHOD unbound
TEMPLATE-LAST-LINK unbound
FTEMPLATE-POLYCANONICALIZED-ASSERTION-DATE [function] (REQ-0 REQ-1)
FORMULA-TEMPLATE-INDUCTION-MT [function] (REQ-0 REQ-1)
NEW-EBMT-TEMPLATE [function] (&OPTIONAL OPT-0 OPT-1 OPT-2 OPT-3)
*LATITUDE-VIA-LOCATION-RULE-ANTECEDENT-TEMPLATE* value: (((?PLACE) (?LAT ?LONG) (?LONG)) (NIL ((#$locatedAtPoint-SurfaceGeographical ?PLACE (#$LatLongFn ?LAT ?LONG)))))
GET-DEFINITIONAL-QUESTION-THING-TEMPLATE-PARAMETERS [function] (REQ-0)
_CSETF-RTP-CHART-ENTRY-TEMPLATE-INDEX [function] (REQ-0 REQ-1)
END-POINTER-LIST-TEMPLATE-POP-METHOD unbound
GET-FORMULA-TEMPLATE-IN-XML [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
PREDICATIVE-ADJP-TEMPLATE-CATEGORY? [function] (REQ-0)
KILL-QUERY-TEMPLATE-AND-QUERY [function] (REQ-0)
END-POINTER-QUEUE-TEMPLATE-EMPTY-P-METHOD unbound
QUERY-TEMPLATE-INVERTED-INDEX-ENTRY-COMPUTER-P [function] (REQ-0)
PPH-PHRASE-W/WU-ONLY-TO-GENTEMPLATE [function] (REQ-0)
BAG-TEMPLATE-GET-CONTENTS-METHOD unbound
TEMPLATE-TOPIC-INTRO-TEMPLATE [function] (REQ-0)
CB-BUILD-FORMULA-TEMPLATE-BASICS [function] (REQ-0)
ASSERT-FORMULA-TEMPLATE-ELMT [function] (REQ-0 REQ-1)
DOUBLY-LINKED-LIST-TEMPLATE unbound
CB-SC-SHOW-TEMPLATE-ACTIONS [function] (REQ-0 REQ-1)
DOUBLY-LINKED-QUEUE-TEMPLATE-GET-LIST-METHOD unbound
EAT-A-TEMPLATE-ITEM [function] (REQ-0)
TEMPLATE-FIRST-LINK unbound
FTEMPLATE-LOAD-ARGUMENT-POSITION-DETAIL-INFORMATION [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
COLLECTION-TEMPLATE-EMPTY-P-METHOD unbound
PPH-EXPAND-GEN-TEMPLATE [function] (REQ-0 REQ-1 REQ-2)
DECLARE-GEN-TEMPLATE-STORE-KNOWN-STALE [function] NIL
LIST-TEMPLATE-P [function] (REQ-0)
TEMPLATE-PARAM-TABLE unbound
*VERBAL-TEMPLATE-CATEGORY?-HASH* value: NIL
FTEMPLATE-GET-TEMPLATE-FOLLOW-UPS [function] (REQ-0 REQ-1)
EBMT-TEMPLATE-JUSTIFICATION-SENTENCE [function] (REQ-0)
FORMULA-TEMPLATE-LOAD-TOPIC-TEMPLATE-DETAILS [function] (REQ-0 REQ-1 REQ-2)
LIST-TEMPLATE-NTH-METHOD unbound
REMOVE-VERBAL-TEMPLATE-CATEGORY? [function] (REQ-0)
*TEMPLATE-RULE-SXHASHING-PRIME-A* value: 5
*MAKE-NL-TAG-TEMPLATE-UPDATE-LOCK* value: #<LOCK DEFINE-CACHED MAKE-NL-TAG-TEMPLATE free 403A1980>
PPH-TEMPLATE-NOTE-RELN-USED [function] (REQ-0 REQ-1)
FTEMPLATE-POLYCANONICALIZED-ASSERTION-P [function] (REQ-0)
NL-TRIE-WORD-SEMANTIC-SUPPORT-SEMTRANS-TEMPLATES-INTERNAL [function] (REQ-0 REQ-1)
BAG-TEMPLATE-SET-CONTENTS-METHOD unbound
DOUBLY-LINKED-LIST-TEMPLATE-FIND-LINK-METHOD unbound
GEN-TEMPLATE-CONSTRAINT-MORE-SPECIFIC? [function] (REQ-0 REQ-1)
CHECK-TEMPLATE-TOPIC-QUERY-MT-CAN-SEE-SUBTOPICS-ASSERTION-MTS [function] (REQ-0 REQ-1)
PPRINT-PPH-PHRASE-TEMPLATE-GENERATOR [function] (REQ-0 &OPTIONAL OPT-1 OPT-2)
CFASL-INPUT-TEMPLATE-TOPIC [function] (REQ-0)
MAKE-SM-TEMPLATE [function] (&OPTIONAL OPT-0)
*GKE-TEMPLATE-CURRENT-TEMPLATE* value: NIL
CHECK-TEMPLATE-ARG-POS-MENU-ITEM-PARSING [function] (REQ-0 REQ-1 &OPTIONAL OPT-2 OPT-3 OPT-4)
RBP-WF-TEMPLATE-ARGS?-INTERNAL [function] (REQ-0 REQ-1 REQ-2)
*RKF-TERM-READER-DEFAULT-TEMPLATES* value: (#$NPTemplate #$NBarTemplate #$AdjPTemplate)
FIND-FTEMPLATE-TOPIC-FROM-ID [function] (REQ-0 REQ-1)
FTEMPLATE-POLYCANONICALIZED-ASSERTION-SENTENCE [function] (REQ-0)
CREATE-NEW-INSTANCE-FROM-TEMPLATE [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3 OPT-4 OPT-5)
*MAKE-NL-TAG-TEMPLATE-HASH* value: NIL
XML-SERIALIZE-ASSERTION-SUIDS-FOR-FORMULA-TEMPLATE-INSTANCE [function] (REQ-0 REQ-1 REQ-2 REQ-3)
FORMULA-TEMPLATE-SET-ENTRY-FORMAT [function] (REQ-0 REQ-1)
DOUBLY-LINKED-QUEUE-TEMPLATE-GET-PRIORITIZER-METHOD unbound
ENUMERATOR-TEMPLATE-LAST-P [function] (REQ-0)
PPH-PHRASE-TEMPLATE-GENERATOR-ARG-POSITION-MAP [function] (REQ-0)
_CSETF-SM-TEMPLATE-STATE [function] (REQ-0 REQ-1)
CB-HANDLE-TEMPLATE-MATCHING [function] (REQ-0)
SET-QUERY-TEMPLATE-INVERTED-INDEX-INSTANCE-COUNT unbound
PPH-UNIFY-FORMULA-TEMPLATE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
CB-LINK-SC-ELABORATE-TEMPLATE [function] (REQ-0 REQ-1 &OPTIONAL OPT-2)
DOUBLY-LINKED-QUEUE-TEMPLATE-SET-CONTENTS-METHOD unbound
PPH-SUBJ-REL-CLAUSE-FROM-GENTEMPLATE [function] (REQ-0 REQ-1 REQ-2)
*DEFAULT-ASSERTION-TEMPLATE-ELMT* value: NIL
PSP-BAD-TEMPLATE? [function] (REQ-0 REQ-1)
RULE-TEMPLATE-DIRECTION [function] (REQ-0 &OPTIONAL OPT-1)
PPH-DET-NBAR-TO-GENTEMPLATE [function] (REQ-0)
FTEMPLATE-TOPIC-GET-FUNCTIONAL-SLOT-VALUE [function] (REQ-0 REQ-1 REQ-2)
TEMPLATE-CATEGORY-FROM-PARSE-SPEC [function] (REQ-0 REQ-1)
PARSE-TEMPLATE unbound
?TEMPLATE unbound
VARIABLE-LENGTH-SEQUENCE-TEMPLATE-ADD-METHOD unbound
GET-GEN-TEMPLATE-MT [function] (REQ-0)
SEQUENCE-TEMPLATE-GET-LENGTH-METHOD unbound
PPH-SHOW-PROOF-TEMPLATE-CONCLUSION [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4)
GEN-TEMPLATE-TOO-BROAD? [function] (REQ-0 REQ-1)
VARIABLE-LENGTH-SEQUENCE-TEMPLATE-GET-SUBSEQUENCE-METHOD unbound
PSP-DEVISE-VERB-TEMPLATES [function] (REQ-0 REQ-1)
PSP-DEVISE-WORD-TEMPLATES [function] (REQ-0 REQ-1 REQ-2 REQ-3)
TEMPLATE-CURRENT-LINK unbound
EBMT-INDEX-TEMPLATE [function] (REQ-0 &OPTIONAL OPT-1 OPT-2 OPT-3)
ASSERT-FORMULA-TEMPLATE-ASSERTION-GLOSSES [function] (REQ-0 REQ-1)
CATEGORIZED-TEMPLATE-RULE-SET-KEY-TERMINAL [function] (REQ-0)
NEW-FORMULA-TEMPLATE [function] (REQ-0 &OPTIONAL OPT-1)
REFORMULATOR-TEMPLATE-EXPRESSION [function] (REQ-0)
CB-UPDATE-FTEMPLATE-ARGPOS-DETAILS-ARGUMENT-POSITION [function] (REQ-0 REQ-1)
_CSETF-AR-TEMPLATE-CHOICE-TABLE [function] (REQ-0 REQ-1)
SET-DEFINITIONAL-QUESTION-GLIMPSE-THING-TEMPLATES [function] (REQ-0 REQ-1)
MAKE-MULTIPLE-ASSERTIONS-TO-MT-WITH-CREATION-TEMPLATE [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3 OPT-4 OPT-5)
*XML-TEMPLATE-TOPIC-ASSERTIONS-REVISIONS* value: ((1 Adds <templateTopicAssertionsRevision> to <knownAssertionsForTemplateTopic>
          Adds <knownAssertionSUIDs> to <knownAssertion>
          Adds <assertion-id> to <knownAssertionSUIDs>
          Adds <bookkeeping-info> to <knownAssertion>
          Adds <date> to <bookkeeping-info>
          Adds <time> to <bookkeeping-info>
          Adds <knownAssertionEvaluations> to <knownAssertion>
          Adds <knownAssertionEvaluation> to <knownAssertionEvaluations>
          Adds <evaluator> to <knownAssertionEvaluation>
          Adds <judgment> to <knownAssertionEvaluation>) (0 Initial version))
PRUNE-RKF-SD-FORMULA-TEMPLATE-TOPIC [function] (REQ-0)
DESTROY-TEMPLATE-TOPIC-PROBLEM-STORE [function] (&OPTIONAL OPT-0)
CLEAR-VERBAL-TEMPLATE-CATEGORY? [function] NIL
PPH-VST-HEAD-EXTRACTION-TEMPLATES [function] (REQ-0 REQ-1 REQ-2 REQ-3)
MATCH-OPTIONAL-TEMPLATE-ITEM [function] (REQ-0 REQ-1)
PPH-IMPRECISE-TEMPLATE-SET-EMPTY? [function] (REQ-0)
QUEUE-SIMPLE-SORTER-TEMPLATE-ORDER-QUEUE-INTERNAL-METHOD unbound
BAG-ENUMERATOR-TEMPLATE-FIRST-METHOD unbound
DOUBLY-LINKED-LIST-TEMPLATE-POP-METHOD unbound
XML-SERIALIZE-FORMULA-TEMPLATE [function] (REQ-0 &OPTIONAL OPT-1)
REFORMULATOR-TEMPLATE unbound
MAKE-FORMULA-TEMPLATE-TYPE-ASSERTIONS [function] (REQ-0)
KB-QUERY-STATE-TEMPLATE-FOLDER-MT-PAIRS [function] (REQ-0)
APPLICABLE-TEMPLATE-INSTANCE-TOPICS-FOR-COLLECTION [function] (REQ-0 REQ-1 REQ-2)
MATCH-SPEECH-PART-TEMPLATE-ITEM [function] (REQ-0 REQ-1)
_CSETF-TEMPLATE-TOPIC-INTRO-TEMPLATE [function] (REQ-0 REQ-1)
SM-GENERATE-TEXT-FROM-TEMPLATE-EXPR [function] (REQ-0 REQ-1)
PSP-QUANTIFY-TEMPLATE [function] (REQ-0)
PPH-REPHRASE-TEMPLATE-FROM-VST-CANONICAL [function] (REQ-0 REQ-1 REQ-2 REQ-3)
*DEFAULT-LEXIFICATION-TEMPLATE-MT* value: EnglishTemplateMt
*QUA-URL-TEMPLATES* value: (ALTAVISTA http://www.altavista.com/web/results?q=SEARCH-STRING&kgs=1&kls=0&nbq=ANSWER-COUNT GOOGLE http://www.google.com/search?hl=en&q=SEARCH-STRING&btnG=Google+Search&num=ANSWER-COUNT)
PSP-LOOKUP-TEMPLATES-MEMOIZED-INTERNAL [function] (REQ-0 REQ-1 REQ-2 REQ-3)
XML-SERIALIZE-ASSERTIONS-FOR-TEMPLATE-TOPIC-INSTANCE [function] (REQ-0 REQ-1 REQ-2 REQ-3 REQ-4 &OPTIONAL OPT-5)
SET-QUERY-TEMPLATE-INVERTED-INDEX-ENTRY-COMPUTER-ISOLATED-P unbound
GET-QUERY-TEMPLATE-INVERTED-INDEX-ENTRY-COMPUTER-GENERATION-MT [function] (REQ-0)
CB-UIA-DEBUG-AR-TEMPLATE [function] (REQ-0)
*USE-BETTER-TEMPLATE-RULE-SXHASH?* value: T
CB-HANDLE-TEMPLATE-OE [function] (REQ-0)
RKF-CH-INITIALIZE-TEMPLATE-PARSE-CACHE [function] (REQ-0)
CB-HANDLE-TEMPLATE-INTERNALS [function] (REQ-0 REQ-1)
CB-HANDLE-TEMPLATE-EDIT [function] (REQ-0)
FTEMPLATE-REFORMULATED-QUERY-MT [function] (REQ-0 REQ-1)
SET-DEFINITIONAL-QUESTION-GLIMPSE-GENERAL-TEMPLATES [function] (REQ-0 REQ-1)
KBQ-ENSURE-TEMPLATE-GLOSSES [function] (REQ-0 REQ-1 REQ-2)
CFASL-INPUT-FORMULA-TEMPLATE [function] (REQ-0)
TERM-TEMPLATES value: TERM-TEMPLATES
TEMPLATES value: TEMPLATES
SEMTRANS-TEMPLATE value: SEMTRANS-TEMPLATE
TEMPLATE-PARAMETERS value: TEMPLATE-PARAMETERS
TEMPLATE-SUCCESS value: TEMPLATE-SUCCESS
PPH-BAD-TEMPLATE-DUMMY value: PPH-BAD-TEMPLATE-DUMMY
TEMPLATE-ITEM value: TEMPLATE-ITEM
DENOT-TEMPLATE value: DENOT-TEMPLATE
OPAQUE-SUBL-TEMPLATE value: OPAQUE-SUBL-TEMPLATE
PROP-TEMPLATE value: PROP-TEMPLATE
TEMPLATE value: TEMPLATE
RECURSIVE-TEMPLATE value: RECURSIVE-TEMPLATE
TEMPLATE-MATCHING value: TEMPLATE-MATCHING
UIA-DEBUG-AR-TEMPLATE value: UIA-DEBUG-AR-TEMPLATE
TEMPLATE-PARSE value: TEMPLATE-PARSE
ASSERT-TEMPLATES value: ASSERT-TEMPLATES
TEMPLATE-ARGS value: TEMPLATE-ARGS
TEMPLATE-INDEX value: TEMPLATE-INDEX
FORMULA-TEMPLATE value: FORMULA-TEMPLATE
TEMPLATE-FOLDER value: TEMPLATE-FOLDER
TEMPLATE-OE-INFERENCE-HANDLER value: TEMPLATE-OE-INFERENCE-HANDLER
TEMPLATE-OE-INFERENCE value: TEMPLATE-OE-INFERENCE
TEMPLATE-GENLS value: TEMPLATE-GENLS
GEN-TEMPLATE-SETS value: GEN-TEMPLATE-SETS
GEN-TEMPLATES value: GEN-TEMPLATES
TEMPLATE-CATEGORY value: TEMPLATE-CATEGORY
INTRO-TEMPLATE value: INTRO-TEMPLATE
ISA-TEMPLATES value: ISA-TEMPLATES
TEMPLATE-GLOSSES value: TEMPLATE-GLOSSES
PARSE-TEMPLATE-MT value: PARSE-TEMPLATE-MT
AVAILABLE-GENLS-TEMPLATES value: AVAILABLE-GENLS-TEMPLATES
PPH-IMPOSSIBLE-TEMPLATE value: PPH-IMPOSSIBLE-TEMPLATE
HIGHER-PRIORITY-TEMPLATE value: HIGHER-PRIORITY-TEMPLATE
TEMPLATE-FOLDER-MT-PAIRS value: TEMPLATE-FOLDER-MT-PAIRS
TEMPLATE-OE value: TEMPLATE-OE
SC-ELABORATE-TEMPLATE value: SC-ELABORATE-TEMPLATE
TEMPLATE-ISA value: TEMPLATE-ISA
TEMPLATE-FAILURE value: TEMPLATE-FAILURE
TEMPLATE-PARSING value: TEMPLATE-PARSING
EXISTING-GEN-TEMPLATES-FAILED value: EXISTING-GEN-TEMPLATES-FAILED
INSTANTIATE-TEMPLATE value: INSTANTIATE-TEMPLATE
TEMPLATE-PARSER value: TEMPLATE-PARSER
GENLS-TEMPLATES value: GENLS-TEMPLATES
AVAILABLE-ISA-TEMPLATES value: AVAILABLE-ISA-TEMPLATES
PARSE-TEMPLATE-FORMULA value: PARSE-TEMPLATE-FORMULA
METRICS-TEMPLATE value: METRICS-TEMPLATE
NO-GEN-TEMPLATES value: NO-GEN-TEMPLATES
PARSE-TEMPLATE value: PARSE-TEMPLATE

RTP-PARSE-RANKING-INITIALIZED? [function] NIL
*RTP-SYNTACTIC-MT* value: #$AllEnglishTemplateMt
NEW-RTP-ITERATOR-STATE [function] (REQ-0 &OPTIONAL OPT-1)
RTP-CHART-ENTRY-BINDINGS [function] (REQ-0)
RTP-COMPONENT-TOKENIZATION [function] (REQ-0 &OPTIONAL OPT-1)
RTP-BINDING-VALS [function] (REQ-0)
RTP-CHART-ENTRY-START [function] (REQ-0)
VALID-RTP-AGR-PRED-ITEM? [function] (REQ-0)
MAYBE-INITIALIZE-RTP-CONTRACTIONS-TABLE [function] (&OPTIONAL OPT-0)
_CSETF-RTP-ITERATOR-RAW-PARSES [function] (REQ-0 REQ-1)
RTP-PERM-STATE-START-INDEX [function] (REQ-0)
RTP-INITIALIZED? [function] NIL
REDUCE-RTP-BY-TEMPLATE-ASSERTION [function] (REQ-0)
*RTP-PERFORM-SEMANTIC-TESTS* value: EXTERNAL
NEW-RTP-WORD-ITEM [function] (REQ-0 REQ-1)
ENSURE-RTP-RULES [function] NIL
RTP-RANK-PARSES [function] (REQ-0 REQ-1)
RTP-OPTIONAL-TEMPLATE-ITEM? [function] (REQ-0)
RKF-STORE-RTP-PARSES-IN-PIPELINE [function] (REQ-0 REQ-1 REQ-2 REQ-3)
POS-FOR-RTP-CAT [function] (REQ-0)
RTP-TRACE [function] (&OPTIONAL OPT-0)
_CSETF-RTP-ITERATOR-MULTIPLE-QUANTIFICATIONS? [function] (REQ-0 REQ-1)
RTP-MATCHED-CONSTIT-START [function] (REQ-0)
MAKE-NEW-RTP-ENTRY [function] (REQ-0 REQ-1 REQ-2 &OPTIONAL OPT-3 OPT-4)
RTP-ITER-CAT-FOR-FORCE [function] (REQ-0)
RESET-ALL-RTP-DATASTRUCTURE-CACHES [function] NIL
DECLARE-RTP-VARS-FILE [function] NIL
VALID-RTP-SYNTACTIC-CONSTRAINT? [function] (REQ-0)
_CSETF-RTP-CHART-ENTRY-TEMPLATE-INDEX [function] (REQ-0 REQ-1)
RTP-BINDING-VAR [function] (REQ-0)
VALID-RTP-RECURSIVE-ITEM? [function] (REQ-0 &OPTIONAL OPT-1)
WITH-THE-RTP-PARSE-TOKENIZATION [function] (REQ-0 REQ-1)
SET-RTP-MATCHED-CONSTIT-START [function] (REQ-0 REQ-1)
ALLOCATE-RTP-RULES [function] NIL
RTP-ITERATOR-MEMOIZATION-STATE [function] (REQ-0)
RTP-SPEECH-PART-ITEM? [function] (REQ-0)
RTP-ITERATOR-REFORMULATE-ONE-PARSE [function] (REQ-0)
RTP-MAKE-BINDING [function] (REQ-0 REQ-1)
VALID-RTP-CYC-TERM-PAIR? [function] (REQ-0)
*RTP-REIFY-DOOMED-PARSES?* value: NIL
RTP-ITERATOR-STRENGTHEN? [function] (REQ-0)
RTP-PARSE-RANKING-INIT [function] NIL


|#

;;(pattern-transform-tree '(:template (#$isa (:tuple (a b c  d ) :input)) :input) '(#$isa 1 2 3 4 ))

(defmacro ret-transform (old new)
 (ret `(clet ((temp1 (pattern-transform-tree '(:template ,old ,new) sent)))
     (fif temp1 (ret (fif (constant-p (car temp1)) (list temp1) temp1))))))

(define S2T (sent &optional (pred nil))
  (clet ((res (S2T2 sent)))
    (fif (and (consp res) (= 1 (length res))) (ret (car res)))
     (ret res)))

;;  (NLPattern-POS :PREP Preposition) 
;;            (NLPattern-POS :SITTYPE Verb) 
;;          (NLPattern-Term 
;;               (PerFn MetersPerSecond SecondsDuration) CountNoun)
;;     (NLPattern-Term 
;;               (CountyNamedFn "Osceola" Florida-State) nameString)
(define S2T2 (sent &optional (pred nil) (args nil))
 (clet ((*s2t-pred* (fif pred pred *s2t-pred*))
        (*s2t-args* (fif args args *s2t-args*)))
 (pwhen (keywordp sent) (ret (template-for-pred-arg *s2t-pred* (keyword-number sent) sent)))


 (punless (or (consp sent) (keywordp sent) ) (throw sent))
 (clet ((f1rst (car sent))(ta1l (cdr sent)))

  (pwhen (equal sent #$NLSentence) (ret #$STemplate))
  (pwhen (equal sent #$NounPhrase) (ret #$NPTemplate))
  (pwhen (equal sent `(#$PhraseFn-Bar1 #$Noun)) (ret #$NPTemplate))
  (fif (constant-p sent) (ret (#$NLPattern-Term sent #$NLWordForm)))
  (fif (and (consp sent)(keywordp (car sent))) (csetq sent (car sent)))
  (punless (consp sent) (ret (fif sent (list sent) ())))
    (pwhen (equal #$TermParaphraseFn f1rst) (ret (S2T (cdr sent) )))
    (pwhen (equal #$TermParaphraseFn-NP f1rst) (ret (S2T (cdr sent) )))
    (pwhen (equal #$ConcatenatePhrasesFn f1rst) (ret (S2T (cdr sent) ))))
  (ret-transform (#$BestNLPhraseOfStringFn (:bind zarg1)) (:call SS2T (:value zarg1)))
  (ret-transform (#$TermParaphraseFn-NPAndNP (:bind zarg1) (:bind zarg2))
       (:call join3 (:call s2t (:value zarg1)) (#$OptionalSome "," "and") (:call s2t (:value zarg2))))
  (ret-transform (#$NLSimpleBinaryConjunctionFn (:bind zarg1) (:bind zarg2))       
       (:call join3 (:call s2t (:value zarg1)) (#$OptionalSome "," "and") (:call s2t (:value zarg2))))
  (ret-transform (#$QuotedParaphraseFn (:bind zarg1))
       (:call join3 (#$OptionalOne #$TemplateDoubleQuoteMarker #$TemplateSingleQuoteMarker)(:call s2t (:value zarg1)) (#$OptionalOne #$TemplateDoubleQuoteMarker #$TemplateSingleQuoteMarker)))
  (ret-transform (#$ConditionalPhraseFn :anything (:bind zarg1) (:bind zarg2))
       ((#$RequireOne (:call asNLTemplate (:call s2t (:value zarg1)))(:call asNLTemplate (:call s2t (:value zarg2))))))
  (ret-transform (#$BestNLWordFormOfLexemeFn (:bind WORD))
       ((#$NLPattern-Word (:value WORD) #$NLWordForm)))
  (ret-transform (#$TermParaphraseFn-Possessive (:bind zarg1)) (#$NLPattern-Template #$PossessiveTemplate (:call s2t (:value zarg1))))
  (ret-transform (#$TermParaphraseFn-Constrained #$regularDegree (:bind zarg2)) ((#$NLPattern-Template #$AdjPTemplate (:call s2t (:value zarg2)))))
  (ret-transform (#$TermParaphraseFn-Constrained (:bind zarg1) (:bind zarg2)) (:call s2t (:value zarg2)) )
  (ret-transform (#$NbarHeadedByNounFormFn (:bind zarg1) (:bind zarg2)(:bind zarg3)) ( (#$NLPattern-Word (:call s2t (:value zarg1)) #$Noun) ( (:call s2t (:value zarg3)) )))
  (ret-transform (#$BestNLWordFormOfLexemeFn-Constrained (:bind CONSTRAINT) (:bind WORD)) ((#$NLPattern-Word (:value WORD) (:call posForPred (:value CONSTRAINT)))))
  (ret-transform (#$TensedPhraseFn-DefaultPast (:bind zarg1) (:bind zarg2))
          ((:call s2t (:value zarg1))
          (:call s2t (:value zarg2))))
  (ret-transform (#$RepeatForSubsequentArgsFn :anything (:bind zarg2)) ( (:call s2t (:value zarg2))))

  (ret-transform (#$PhraseFormFn (:and (:bind zarg1) (:isa #$TermPhrasesConstraintPredicate)) (#$TermParaphraseFn (:bind zarg2)))
        (#$NLPattern-Term (:call s2t (:value zarg2)) (:call posForPred (:call s2t (:value zarg1)))))
  (ret-transform (#$PhraseFormFn (:bind zarg1) (:bind zarg2))
       (:call s2t (:value zarg2)))

  ;;(ret-transform (#$PhraseFormFn (:bind NLTYPE)(:bind SYNTAX)))

  (ret-transform (#$NLConjunctionFn (:bind zarg1))  (:call s2t (:value zarg1)))
  (ret-transform (#$HeadWordOfPhraseFn (:bind zarg2))  (:call s2t (:value zarg2)))
  (ret-transform (#$GenTemplateRecipeOmitsArgFn (:bind zarg1) (:bind zarg2)) 
         (:call s2t (:value zarg2)))
  (ret-transform (#$BestVerbFormForSubjectFn (:bind word) :anything) (#$NLPattern-Word (:value word) #$Verb))
  (ret-transform (#$BestPPFn (:bind prep) (:bind zarg2))
         ((#$NLPattern-Word (:value prep) #$Preposition)( (:call s2t (:value zarg2)))))
   (ret-transform (#$XHasYAsAZ-NLSentenceFn (:bind zarg1) (:bind zarg2) (:bind zarg3))
         ((:call s2t (:value zarg1)) (:call SS2T "has") (:call s2t (:value zarg2)) (:call SS2T "as") (:call s2t (:value zarg3))))
   (ret-transform (#$NPIsXP-NLSentenceFn (:bind zarg1) (:bind zarg2))
         ((:call s2t (:value zarg1)) (:call SS2T "is")  (:call s2t (:value zarg2))))
    (ret-transform  (#$DefiniteNbarPPFn  (:bind zarg1) (:bind zarg2) (:bind zarg3))       
         (:call s2t 
          (#$BestDetNbarFn-Definite 
            (#$ConcatenatePhrasesFn 
               (#$HeadWordOfPhraseFn (:call s2t (:value zarg1)) )
                (#$BestNLPhraseOfStringFn (:call s2t (:value zarg2)) )
               (#$PhraseFormFn #$NounPhrase (:call s2t (:value zarg3)))))))
    (ret-transform  (#$IndefiniteNbarPPFn  (:bind zarg1) (:bind zarg2) (:bind zarg3))
         (:call s2t 
        (#$BestDetNbarFn-Indefinite 
            (#$ConcatenatePhrasesFn 
               (#$HeadWordOfPhraseFn (:call s2t (:value zarg1)) )
                (#$BestNLPhraseOfStringFn (:call s2t (:value zarg2)) )
               (#$PhraseFormFn #$NounPhrase (:call s2t (:value zarg3)))))))
   (ret-transform (#$IndefiniteNounPPFn (:bind zarg1) (:bind zarg2) (:bind zarg3))
         (:call s2t 
        (#$BestDetNbarFn 
          (#$TermParaphraseFn #$Indefinite-NLAttr) 
           (#$ConcatenatePhrasesFn 
             (#$HeadWordOfPhraseFn 
              (#$BestNLWordFormOfLexemeFn-Constrained #$CountNoun (:call s2t (:value zarg1))) 
                (#$BestNLPhraseOfStringFn (:call s2t (:value zarg2)) )
           (#$PhraseFormFn #$NounPhrase  (:call s2t (:value zarg3))))))))
   (ret-transform (#$DefiniteNounPPFn (:bind zarg1)(:and (:bind zarg2)(:test stringp)) (:bind zarg3))
         (:call s2t 
        (#$BestDetNbarFn-Definite (#$ConcatenatePhrasesFn 
             (#$HeadWordOfPhraseFn 
              (#$BestNLWordFormOfLexemeFn-Constrained #$CountNoun (:call s2t (:value zarg1))) 
                (#$BestNLPhraseOfStringFn (:call s2t (:value zarg2))))
           (#$PhraseFormFn #$NounPhrase (:call s2t (:value zarg3)))))))

  (ret-transform (#$BestHeadVerbForInitialSubjectFn (:bind zarg1)) (#$NLPattern-Word (:call s2t (:value zarg1)) #$Verb))
  (ret-transform (#$BestDetNbarFn-Indefinite (:bind zarg1))  (:call s2t (:value zarg1)))
  (ret-transform (#$BestDetNbarFn-Definite (:bind zarg1))  (:call s2t (:value zarg1)))
  (ret-transform (#$BestDetNbarFn (:bind zarg1)(:bind zarg2))
      (#$RequireOne (:call s2t (:value zarg1))  (:call s2t (:value zarg2))))
  (fif (constant-p (car sent)) (ret sent))
  (clet ((f1rst (S2T (car sent))))
    (ret (append (fif (constant-p (car f1rst)) (list f1rst) f1rst) (list (S2T (cdr sent))))))))

;;(csetq *RKF-MT* *dt*)
;;(CATEGORIZED-ITP-SEQUEL "The blue dog" #$DateTemplate) 

;;TermParaphraseFn-TemporalLocation ModalIndex
;;TermParaphraseFn-TemporalLocation-Date ModalIndex
 (doom-retract 
'(#$termTemplate-Reln #$DateTemplate #$Simply 
       (#$NLPatternList 
           (#$NLPattern-Exact "the") 
           (#$NLPattern-Template #$NBarTemplate :TEMP-INT)) 
       (#$Simply :TEMP-INT)) #$EnglishTemplateMt)


 (doom-retract 
'(#$ist ?MT (#$termTemplate-Reln #$NPTemplate ?Percent 
  (#$NLPatternList 
    (#$NLPattern-Template ?PercentTemplate ?PERC)) 
  (#$Simply ?PERC)) ))

 (doom-retract 
'(#$ist ?MT (#$termTemplate #$NPTemplate  
  (#$NLPatternList 
    (#$NLPattern-Template ?PercentTemplate ?PERC)) 
  (#$Simply ?PERC)) ))

 
 (doom-retract 
'(#$termTemplate-Reln #$NPTemplate #$VariableFn 
  (#$NLPatternList 
    (#$NLPattern-Exact "what")) 
  (#$VariableFn ?WHAT-VARIABLE #$Thing))#$EnglishTemplateMt)

 (ke-assert-now '
(#$termTemplate-Reln #$NPTemplate (#$NLTemplateForInstanceFn ?COL)
  (#$NLPatternList 
    (#$NLPattern-Template #$StringTemplate :STRING )) (#$InstanceNamedFn ?COL :STRING))
 #$EnglishTemplateMt)


 (ke-assert-now '
(#$termTemplate-Reln #$NPTemplate (#$NLTemplateForInstanceFn ?COL)
  (#$NLPatternList 
    (#$NLPattern-Agr ?DENOT (#$NLPredicateConstraintFn (?W ?S) (#$and (#$denotation ?W ?POS ??SN ?COL)(#$partOfSpeech ?W ?POS ?S)))))
  (#$InstanceNamedFn ?COL (TheList ?S . ?DENOT )))
 #$EnglishTemplateMt)

  (ke-assert-now '
(#$termTemplate-Reln #$NPTemplate (#$NLTemplateForInstanceFn #$Meat)
  (#$NLPatternList 
    (#$NLPattern-Agr ?DENOT (#$NLPredicateConstraintFn (?W ?S) (#$and (#$denotation ?W ?POS ??SN #$Meat)(#$partOfSpeech ?W ?POS ?S)))))
  (#$InstanceNamedFn ?COL (#$TheList ?S . ?DENOT )))
 #$EnglishTemplateMt)


  (ke-assert-now '
(#$termTemplate-Reln #$NPTemplate (#$NLTemplateForInstanceFn ?COL)
  (#$NLPatternList 
    (#$NLPattern-Agr ?DENOT (#$Kappa (?W ?S) (#$and (#$denotation ?W ?POS ??SN ?COL)(#$termStrings ?W ?S)))))
  (#$TheList ?COL ?DENOT ?POS ?S ?W))
 #$EnglishTemplateMt)

  (ke-assert-now '
(#$termTemplate-Reln #$NPTemplate (#$NLTemplateForInstanceFn #$NPTemplate  ?COL)
  (#$NLPatternList 
    (#$NLPattern-Agr ?DENOT (#$Kappa (?W ?S) (#$and (#$equals ?W #$Word-TheWord)(#$equals ?S "wword")))))
  (#$TheList ?COL ?DENOT ?POS ?S ?W))
 #$EnglishTemplateMt)




 (ke-assert-now '
(#$termTemplate-Test #$NPTemplate (#$NLTemplateForInstanceFn ?COL)
  (#$NLPatternList 
    (#$NLPattern-Template #$StringTemplate :STRING )) (#$InstanceNamedFn ?COL :STRING)
    #$True)
 #$EnglishTemplateMt)


  (ke-assert-now '
(#$termTemplate-Test (#$NLTemplateForInstanceFn #$NPTemplate #$Ocean) (#$NLTemplateForInstanceFn #$NPTemplate #$Ocean)
  (#$NLPatternList 
    (#$NLPattern-Exact "ocean" )) (#$InstanceNamedFn #$Ocean "ocean")
    #$True)
 #$EnglishTemplateMt)

  (ke-assert-now '
(#$rewriteTemplate #$NPTemplate (#$NLPatternList (#$NLPattern-Exact "dogg")) (#$NLPatternList (#$NLPattern-Exact "dogggg")))
 #$EnglishTemplateMt)


  (ke-assert-now '
(#$termTemplate #$NPTemplate 
    (#$NLPatternList (#$NLPattern-POS ?WHAT #$CountNoun))
    (#$isa :NOUN ?WHAT ))
 #$DoomTemplateParsingMt)

(define prs (test)
  '(populate-rtp-from-mt #$DoomTemplateParsingMt)
  (ret (itp-sequel test #$NPTemplate)))
(prs "dog")


  (ke-unassert-now '
(#$termTemplate-Test (#$NLTemplateForInstanceFn #$Ocean) #$Simply
  (#$NLPatternList 
    (#$NLPattern-Exact "ocean" )) (#$InstanceNamedFn #$Ocean "ocean")
    #$True)
 #$EnglishTemplateMt)

 (ke-assert-now '
(#$termTemplate-Test #$NPTemplate  #$InstanceNamedFn (#$NLPatternList
           (#$NLPattern-Template #$StringTemplate :VAR))
       (#$InstanceNamedFn ?W :VAR)
        (#$termStrings ?W :VAR))  #$EnglishTemplateMt)


  (ke-assert-now '
(#$termTemplate-Test #$NPTemplate 
       ?X
       (#$NLPatternList 
           (#$NLPattern-Template #$StringTemplate :VAR)) 
       (#$InstanceNamedFn #$Ocean :VAR) 
       #$True)
       #$DoomTemplateParsingMt)
       

 (fi-unassert 
'(#$termTemplate-Test #$NPTemplate #$InstanceNamedFn
  (#$NLPatternList 
    (#$NLPattern-Term #$Dog ?POS)) 
  (#$InstanceNamedFn #$Dog :DOOM-COL) #$True) #$EnglishTemplateMt)

 (fi-assert'
(#$termTemplate-Test #$NPTemplate #$InstanceNamedFn 
  (#$NLPatternList 
    (#$NLPattern-Word ?WORD #$Noun)) 
  (#$InstanceNamedFn ?COL "it") (#$denotation ?WORD ?POS ??NUM ?COL )) #$EnglishTemplateMt)


(defmacro s2t-test (pred val)
 (ret `(and (print ',pred) (print (s2t ',val ',pred)) (terpri) (force-output *standard-output*))))

;;(define-class ( cyc-dcg 'document))
;; nil ged nil wordstr nil parsetree nil))

;;(defun TAG-CONCEPT-STRING (string) (SENTENCE-DO-WORDS (words (new-document string)) (print words)))

(defun docify (string)
 (let ((doc (docify2 string)))
  doc))

(defun docify2 (string)
  (let ((spp (sentence-preparse string)) (doc (new-document string)) (tags (TAG-CONCEPTS  doc)))
  (p1p (length spp))
  (if (iterator-p tags) (ITERATOR-VALUE-LIST tags) tags)))

 
(setq ged (NEW-GUI-ENABLED-DOCUMENT 20))
(ADD-SENTENCE-TO-GED ged "i can feel the oil tanker" 0)
(ADD-SENTENCE-TO-GED ged "I see the oil tanker" 1)
(ADD-SENTENCE-TO-GED ged "I smell oil tanker" 2)

(setq parsetree (NEW-PARSE-TREE '()))
(setq doc (new-document "i can feel the oil tanker"))
(setq adoc (new-document "i can feel the oil tanker"))
;;(COPY-NL-TRIE  

(defun WORDTOKS-DATA (cur) (WORDTOKS-DATA-USEL (ele 2 cur)))
(defun WORDTOKS-DATA-USEL (cur)
 (if cur
  (cons  (WORDTOKS-DATA-USE (car cur))
   (WORDTOKS-DATA-USEL (cdr cur)))))

(defun WORDTOKS-DATA-USE (f)
 (cond 
	((stringp f) f)
	((NL-TRIE-WORD-P f) (WORDTOKS-DATA-USEL (NL-TRIE-WORD-ELEMENTS f )))
	((NL-TRIE-P f) (WORDTOKS-DATA-USEL (NL-TRIE-WORD-KEYS f )))
	((assertion-p f) (assertion-el-formula f))
	(t (p1p f))))

(defun p1p (cnt)
 (terpri) (terpri) (print `( ,(type-of cnt) ,cnt )) (terpri) (terpri) cnt)


(define cycterm-p (str)
 (fif (listp str) (csetq str (nth 0 str)))
 (ret (cor (fort-p str) (nart-p str) (el-variable-p str) (constant-p str) (hl-variable-p str))))


 ;;(FI-UNASSERT '(#$evaluationDefn #$CycificationFn (#$SubLQuoteFn CYCLIFY)) #$UniversalVocabularyMt)
 ;;(FI-ASSERT '(#$evaluationDefn #$CycificationFn (#$SubLQuoteFn CYCLIFY-CONCAT)) #$UniversalVocabularyMt)


(define CYCLIFY-CONCAT (&rest string)
  (ret (cyclify-stanford (concat-words string))))



;;(load "cl.lisp")
#|
;;(INITIALIZE-NL-TRIE)

 ;;(CORPUS-PASSAGE-IDENTIFY-ENTITIES-METHOD
 (SENTENCE-DO-WORDS (word doc) 
    
 (GET-TEXT-FROM-STRING  string)
 (NL-TRIE-SEARCH  )
 (NL-TRIE-ENTRIES-FOR-STRING  "I")

;;(get-np

|#

(defun UNKNOWN-INSTANCE-INDEXED-EXPRESSION? (&rest cnt)
  (p1p 'UNKNOWN-INSTANCE-INDEXED-EXPRESSION? cnt))

(defun testparse () (print (docify "can you see the oil tanker shimmer in the sunlight ?")))
(defun testparse () ())

(defun dcg () 
  (load "cycdcg.lisp")
  (eval '(testparse)))

(defun cycdcg () 
  (load "cycdcg.lisp")
  (eval '(testparse)))

;;(load "cycdcg_test.lisp")

(print `(cycdcg))
(print `(dcg))



(define forward-mt (mt)
(clet ((count 0)(ismt (mt? mt)))
   (cdolist (ass (all-term-assertions mt))
     (clet ((fw (forward-assertion? ass)))
       (pwhen ismt (punless fw (progn (cinc count)(tms-change-direction ass :forward))))
       (pwhen fw (progn (cinc count) (REPROPAGATE-FORWARD-ASSERTION ass)))))
     (ret count)))

 


'(ke-assert-now '(#$and (#$implies (#$and (#$arityMin ?PRED ??ARITY)  (#$isa ?PRED #$Function-Denotational) )(#$isa ?PRED #$VariableArityFunction))
(#$implies (#$and (#$arityMin ?PRED ??ARITY)  (#$isa ?PRED #$Predicate) )(#$isa ?PRED #$VariableArityPredicate))
(#$implies (#$arityMax ?PRED ??ARITY) (#$isa ?PRED #$VariableArityRelation))) #$BaseKB :MONOTONIC :FORWARD )


'(mapcar #'forward-mt (ask-template '?PRED '  (#$semTransArg ?PRED 4)  #$EverythingPSC))
(#$and 
  (#$arg1Isa ?PRED #$LexicalWord) 
  (#$arg2Isa ?PRED #$Integer) 
  (#$semTransArg ?PRED 4) 
  (#$semTransPredForPOS ?POSCLASS ?PRED)) #$EverythingPSC))


`(cdolist (terms (remove-duplicates (all-instances #$ParseTemplatePredicate)))
    (cdolist (ass (all-term-assertions terms ))
       (EXTEND-RTP-WITH-TEMPLATE-ASSERTION (ASSERTION-EL-FORMULA ass))))

#|       
(all-term-assertions )
(forward-mt  *dt*) 


(clet ((sents (sort (remove-duplicates (ask-template 
'(?PRED ?FRAME)
'(#$and 
    (#$argIsa ?PRED ?N #$SubcategorizationFrame))
;;(#$argIsa ?PRED ?WORDARG  #$LexicalWord)
;;(#$arity ?PRED ?ARITY)
;;(#$argIsa ?PRED ?ANY1 #$SubcategorizationFrame)
 #$EverythingPSC))  #'less-than (list sents (length sents)))
|#
;;(define doom-rtp (sent)
(defvar *SMT* #$EverythingPSC)

(define all-isa (term &OPTIONAL MT TV)
    (ret (ask-template '?COL `(#$isa ,term ?COL) (fif  MT  MT  *SMT*))))
(define isa (term &OPTIONAL MT TV)
    (ret (ask-template '?COL `(#$nearestIsa ,term ?COL) (fif  MT  MT  *SMT*))))

(define all-instances2 (term &OPTIONAL MT TV)
    (ret (ask-template '?COL `(#$isa ?COL ,term ) (fif  MT  MT  #$BaseKB) nil nil )))

(define instances2 (term &OPTIONAL MT TV)
    (ret (ask-template '?COL `(#$nearestIsa ?COL ,term ) (fif  MT  MT  #$EverythingPSC))))

(define all-specs (term &OPTIONAL MT TV)
    (ret (ask-template '?COL `(#$genls ?COL ,term ) (fif  MT  MT  *SMT*))))
(define specs (term &OPTIONAL MT TV)
    (ret (ask-template '?COL `(#$nearestGenls ?COL ,term ) (fif  MT  MT  *SMT*))))


(foc "NLTemplateForSpecFn")
(define rtp-col (sent col) (ret (itp-sequel sent `(#$NLTemplateForInstanceFn ,col))))
(foc "NLTemplateForPatternFn")
(define rtp-terms (sent &rest terms) (ret (itp-sequel sent `(#$NLTemplateForPatternFn (#$NLPatternList ,@terms)))))

(define denotes-isa (col string)(ret (ALL-INSTANCES-AMONG col (mapcar #'cdr (denotation-mapper string)))))
(define denotes-genls (col string)(ret (ALL-SPECS-AMONG col (mapcar #'cdr (denotation-mapper string)))))




