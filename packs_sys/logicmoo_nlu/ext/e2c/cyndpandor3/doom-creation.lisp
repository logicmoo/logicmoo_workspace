               

(define doom-equal (x y)
  ;;(pwhen (consp x) (ret (doom-equal y (constant-name x))))
    (pwhen (constant-p x) (fif (doom-equal y (constant-name x))(ret x)))
    (ret (fif (string-equal (format nil "~A" x)(format nil "~A" y)) x)))
    
(define doom-fn-name (name) 
  (csetq name (join-strings  (mapcar #'string-proper (string-tokenize name '(#\space #\-))) ""))
  (punless (string-equal (substring name 0 4) "doom") (csetq name (cconcatenate "Doom" (string-proper name))))
  (punless (string-equal (substring name (- (length name) 1)) "fn") (csetq name (cconcatenate (string-proper name) "Fn")))
  (ret (doom-cyc name)))

;;(doom-assert-now `(#$equiv (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$East-Directly) ?P1 ?P2)  (#$permanentlyEastOf ?P2 ?P1))  *CreationMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now `(#$equiv (#$containsPortals ?P1 (#$BoundsOfDirectionFn ?P1 #$North-Directly)) (#$permanentlyNorthOf ?P2 ?P1))  *CreationMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now `(#$equiv (#$containsPortals ?P1 (#$BoundsOfDirectionFn ?P1 #$South-Directly)) (#$southOf ?P2 ?P1))  *CreationMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now `(#$equiv (#$containsPortals ?P1 (#$BoundsOfDirectionFn #$West-Directly ?P2) (#$westOf ?P2 ?P1))  *UVMt* '(:DIRECTION :FORWARD))
;;;(doom-assert-now `(#$implies (#$isa #$BPVAgent)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z)))(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z 700)))) *UVMt* '(:DIRECTION :FORWARD)
;;(doom-assert-now '(#$implies (#$and (#$isa ?X #$BPVItem) (#$isa ?X ?Type) (#$relationAllInstance ?Pred ?Type ?Instance)) (#$ist *MappingMt* (?Pred ?X ?Instance))) *MappingMt* '(:DIRECTION :FORWARD))
;;(doom-assert-now '(#$implies (#$and (#$isa ?X #$BPVAgent) (#$isa ?X ?Type) (#$relationAllInstance ?Pred ?Type ?Instance)) (#$ist *MappingMt* (?Pred ?X ?Instance))) *MappingMt* '(:DIRECTION :FORWARD))
(define ccc (mt)
 (doom-assert-now 
  `(#$implies 
  (#$and
   (#$objectFoundInLocation?O ?L)
   (#$isa?O #$BPVAgent)
   (#$isa?L #$BPVLocation))
 (#$and
  (#$ist ,mt (#$doomPropertyNext (#$DoomItemFn (#$ConstantNamedFn ?O)) (#$DoomPropertyFn "classname") (#$DoomClassFn "cyc_bot")))
  (#$ist ,mt (#$doomPropertyNext (#$DoomItemFn (#$ConstantNamedFn ?L)) (#$DoomPropertyFn "classname") (#$DoomClassFn "mud_room"))))) mt))
    
    

 
;;(CCC #$DoomCurrentStateMt)


(define  recompute-boundries (Mt) 
    (doom-assert-now `(#$arg1Isa #$BoundsOfDirectionFn #$BPVLocation)  *CreationMt* '(:DIRECTION :FORWARD))
    (doom-assert-now `(#$resultIsa #$BoundsOfDirectionFn #$Path-Spatial)  *CreationMt* '(:DIRECTION :FORWARD))
    (doom-assert-now '(#$formsBorderPart (#$BoundsOfDirectionFn ?P1 ?Dir) (#$BorderFn ?P1)) *CreationMt* '(:DIRECTION :FORWARD))
    (doom-assert-now '(#$equiv (#$isa (#$BoundsOfDirectionFn ?P1 ?D1) #$Wall-GenericBarrier)(#$cavityHasWall ?P1 (#$BoundsOfDirectionFn ?P1 ?D1) ) ) *CreationMt* '(:DIRECTION :FORWARD))
    
    (doom-assert-now `(#$implies (#$and (#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X1 ?Y1 ?Z))(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn ?X2 ?Y2 ?Z))) (#$levelWith ?P1 ?P2))   *CreationMt*  '(:DIRECTION :FORWARD))
    
    (doom-assert-now '(#$implies (#$isa ?P1 #$BPVLocation)(#$and (#$isa (#$BorderFn ?P1) #$Border)(#$formsBorderPart (#$BoundsOfDirectionFn ?P1 ?Dir) (#$BorderFn ?P1)))) *CreationMt* '(:DIRECTION :FORWARD))
    (doom-assert-now `(#$implies (#$pathBetween (#$BoundsOfDirectionFn ?P1 ?DIR) ?P1 ?P2)(#$containsPortals ?P1 (#$BoundsOfDirectionFn ?P1 ?DIR)))  *CreationMt* '(:DIRECTION :FORWARD))
    (doom-assert-now `(#$implies (#$pathBetween ?PATH ?P1 ?P2)(#$containsPortals ?P1 ?PATH))  *CreationMt*)
    
    
    (doom-assert-now '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$West-Directly) #$Wall-GenericBarrier) (#$doomInstance (#$BoundsOfDirectionFn ?P1 #$West-Directly) (#$DoomClassFn "mud_wclosed" ))) *CreationMt*)
    (doom-assert-now '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$Up-Directly) #$Wall-GenericBarrier) (#$doomInstance (#$BoundsOfDirectionFn ?P1 #$Up-Directly) (#$DoomClassFn "mud_uclosed" ))) *CreationMt*)
    (doom-assert-now '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$South-Directly) #$Wall-GenericBarrier) (#$doomInstance (#$BoundsOfDirectionFn ?P1 #$South-Directly) (#$DoomClassFn "mud_sclosed" ))) *CreationMt*)
    (doom-assert-now '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$North-Directly) #$Wall-GenericBarrier) (#$doomInstance (#$BoundsOfDirectionFn ?P1 #$North-Directly)(#$DoomClassFn "mud_nclosed" ))) *CreationMt*)
    (doom-assert-now '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$East-Directly) #$Wall-GenericBarrier) (#$doomInstance (#$BoundsOfDirectionFn ?P1 #$East-Directly) (#$DoomClassFn "mud_eclosed" ))) *CreationMt*)
    (doom-assert-now '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$Down-Directly) #$Wall-GenericBarrier) (#$doomInstance (#$BoundsOfDirectionFn ?P1 #$Down-Directly) (#$DoomClassFn "mud_dclosed" ))) *CreationMt*)

    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$West-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X -700) ?Y ?Z))))  Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$Up-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z 300)))) Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$South-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y -700) ?Z))))  Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$North-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y 700) ?Z))))  Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$East-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X 700) ?Y ?Z))))  Mt)
   ;;(doom-assert-now `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$Down-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z -700)))) *CreationMt*  '(:DIRECTION :FORWARD))

    (doom-assert-now `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$West-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X -700) ?Y ?Z)))) Mt '(:DIRECTION :FORWARD))
    (doom-assert-now `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$Up-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z 300))))  Mt  '(:DIRECTION :FORWARD))
    (doom-assert-now `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$South-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y -700) ?Z)))) Mt  '(:DIRECTION :FORWARD))
    (doom-assert-now `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$North-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y 700) ?Z))))  Mt '(:DIRECTION :FORWARD))
    (doom-assert-now `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$East-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X 700) ?Y ?Z))))  Mt '(:DIRECTION :FORWARD))
)               


;;(recompute-boundries *CreationMt*)

    
(define  room-leads (loc dir)
  (ret (cdr (car (car  (cyc-query (list #$pathConnects (list #$BoundsOfDirectionFn loc (nsew-term dir) ) loc '?X) *MappingMt*))))))

(define  nsew-path (loc dir) (ret (list #$BoundsOfDirectionFn  loc (nsew-term dir))))
(define  nsew-term (dir) 
  (ret
   (fif (null dir)
    (ret (MAKE-EL-VAR "DIR"))
    (fif (stringp dir)
        (pcase (char (STRING-DOWNCASE dir) 0)
          (#\n #$North-Directly)
          (#\s #$South-Directly)
          (#\e #$East-Directly)
          (#\w #$West-Directly)
          (#\u #$Up-Directly)
          (#\d #$Down-Directly)
          (otherwise dir))
      (fif (constant-p dir)
        (pcase dir
          ((#$North-Generally #$North-Directly)#$North-Directly)
          ((#$South-Generally #$South-Directly)#$South-Directly)
          ((#$East-Generally #$East-Directly)#$East-Directly)
          ((#$West-Generally #$West-Directly)#$West-Directly)
          ((#$Up-Generally #$Up-Directly)#$Up-Directly)
          ((#$Down-Generally #$Down-Directly)#$Down-Directly)
          (t (ret (nsew-term (constan-name dir)))))
       (ret dir))))))
      
;; (cyc-query '(#$pathConnects (#$BoundsOfDirectionFn #$Area1000 #$North-Directly) ?P1 ?P2) *MappingMt*)
;;(room-walls #$Area1000)
(define  room-exits (loc) 
  (ret (cconcatenate 
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "n") loc '?X) *CurrentStateMt*) "n" "")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "s") loc '?X) *CurrentStateMt*) "s" "")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "e") loc '?X) *CurrentStateMt*) "e" "")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "w") loc '?X) *CurrentStateMt*) "w" "")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "u") loc '?X) *CurrentStateMt*) "u" "")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "d") loc '?X) *CurrentStateMt*) "d" ""))))

;;(room-exits #$Area1041)
(define  room-walls (loc) 
  (ret (cconcatenate 
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "n") loc '?X) *CurrentStateMt*) "" "n")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "s") loc '?X) *CurrentStateMt*)"" "s")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "e") loc '?X) *CurrentStateMt*)"" "e")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "w") loc '?X) *CurrentStateMt*)"" "w")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "u") loc '?X) *CurrentStateMt*) "" "u")
       (fif (cyc-query  (list #$pathConnects (nsew-path loc "d") loc '?X) *CurrentStateMt*) "" "d"))))


;;(room-exit-is-type #$Area1000 #$Border)
(define room-exit-is-type (loc col) 
    (format t "~&(room-exit-is-type ~s ~s)~&" loc col)(force-output)
     (ret (cconcatenate 
       (fif (cyc-query  (list #$isa (nsew-path loc "n") col) *CurrentStateMt*) "n" "")
       (fif (cyc-query  (list #$isa (nsew-path loc "s") col) *CurrentStateMt*) "s" "")
       (fif (cyc-query  (list #$isa (nsew-path loc "e") col) *CurrentStateMt*) "e" "")
       (fif (cyc-query  (list #$isa (nsew-path loc "w") col) *CurrentStateMt*) "w" "")
       (fif (cyc-query  (list #$isa (nsew-path loc "u") col) *CurrentStateMt*) "u" "")
       (fif (cyc-query  (list #$isa (nsew-path loc "d") col) *CurrentStateMt*) "d" ""))))

;;(room-exits #$Area1000)
;;;;(#$isa  #$doomClassToBinding)
;;(define  exits-open (loc) (ret (is-of-exit-type loc #$OpenPortal)))
;;(define  exits-closed (loc) (ret (is-of-exit-type loc #$ClosedPortal)))		'

 ;;PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
 ;;7224 root      3 -13 1955m 433m 103m R 99.8 21.4  23:45.48 latest-cyc.bin
 
 ;;PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
  ;;7224 root      3 -13 1955m 634m 104m R 99.5 31.3  56:26.76 latest-cyc.bin
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH1 ?D1 ?D2) (#$pathBetween ?PATH2 ?D2 ?D1))(#$equals ?PATH1 ?PATH2))  *StaticStateMt* '(:DIRECTION :BACKWARD :STRENGTH :MONOTONIC) )
                                                                                                                
(doom-assert-now '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Bedroom)) (#$isa ?PATH #$Doorway))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(doom-assert-now '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$OfficeSpace-Personal)) (#$isa ?PATH #$Doorway))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(doom-assert-now '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$isa ?PATH #$OpenPortal))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(doom-assert-now '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$not (#$isa ?PATH #$Doorway)))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(doom-assert-now '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$not (#$isa ?PATH #$ClosedPortal)))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(doom-assert-now '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2)(#$isa ?PATH #$Portal)) (#$bordersOn  ?D1 ?D2) )*CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
;;;;(doom-assert-now '(#$implies (#$and (#$pathBetween ?PATH ?P1 ?P2) (#$isa ?P2 #$Hallway)) (#$isa ?PATH #$Doorway))  *MappingMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
;;(loc-Of #$ArtifactCol1009-1009-10-Medical-Tricorder681)
(define  loc-Of1 (q &optional (hMt *SMT*)) (ret (car (cyc-query q hMt '(:backchain T  :time 5 :number 1)))))
(define  loc-Of (term) 
  (clet ((result NIL))
   (csetq result (cdr (car (loc-Of1 (list #$wornOn term '?W)))))
   (cand result (ret (list #$wornOn result)))
   (csetq result (cdr (car (loc-Of1 (list #$possesses '?W term)))))
   (cand result (ret (list #$possesses result)))
   (csetq result (cdr (car (loc-Of1 (list #$locatedAtPoint-Spatial term '?W)))))
   (cand result (ret result))
   (csetq result (cdr (car (loc-Of1 (list #$objectFoundInLocation term '?W)))))
   (cand result (ret (list #$objectFoundInLocation result)))
   (ret result)))


(define  useless-string (str) 
    (ret (cor (cnot (stringp str))(zerop (length str)) nil)))

(define  localize-desc-string (from text)
  (clet ((tokens (string-tokenize (STRING-TRIM (append '(#\; #\, #\:) *WHITESPACE-CHARS*) text))))
        (csetq tokens (delete-if #'useless-string tokens))
        (csetq text (join-strings tokens " "))
        (csetq tokens (string-trim (append '(#\; #\, #\:) *WHITESPACE-CHARS*) (string-upcase (car tokens))))
        (pwhen (string-equal tokens "FROM") (ret text))
        (pwhen (string-equal tokens "IT") (ret text))
        (pwhen (string-equal tokens "THE") (ret text))
        (pwhen (string-equal tokens "THIS") (ret text))
        (ret text)
        (ret (cconcatentate from ", " text))))

(define  room-text (&optional (room '?Room) (What '?What))
    (clet ((strings)(items (cons room (ask-template `,What `(#$and (#$isa ,room #$BPVLocation) (#$parts ,room ,What)) *CurrentStateMt*))))
            (cdolist (ele (remove-duplicates items))
                (csetq strings (append strings (list (cons ele (remove-duplicates 
                    (delete-if #'useless-string (ask-template `?STR `(#$termStrings ,ele ?STR) *CurrentStateMt*)) #'string-equal))))))
         (ret strings)))

(defvar *theIsa*)
(define  apply-the-isa (front list)
   (punless (consp front) (setq front (list front)))
    (clet ((res))
        (cdolist (ele list)
            (csetq res (cons (append front (list ele)) res)))
         (ret res)))

(defmacro trace-warn (&rest code)  (ret `(with-error-handler #'(lambda () (format t "~&:ERROR ~s in ~s" *ERROR-MESSAGE* ',code)(force-output)) (sl::progn ,@code))))


(define  cdr-equal (x y) (ret (cor (doom-equal x y)(cand (consp x)(consp y)(doom-equal (cdr x) (cdr y))))))

(define  CYCLIFY-DOOM (text &optional (thing #$TheSentenceSubject))
     (clet (parse)
         (trace-warn (csetq parse (append parse (apply-the-isa `(#$conceptuallyRelated ,thing) (ps-get-cycls-for-phrase text  :any)))))
         (trace-warn (csetq parse (append parse (apply-the-isa `(#$conceptuallyRelated ,thing) (denots-of-string  text nil :related t t )))))
         (csetq parse (remove-duplicates parse #'cdr-equal))
         (trace-warn (csetq parse (append parse (parse-a-sentence-completely  text #$RKFParsingMt))))
         (trace-warn (csetq parse (append parse (apply-the-isa `(#$ist-Asserted ,thing) (parse-a-question-completely  text #$RKFParsingMt)))))
         (trace-warn (csetq parse (append parse (CYCLIFY-STANFORD text))))
         (trace-warn (csetq parse (append parse (cyclify text nil))))
         (ret (remove-duplicates parse #'equal))))


'(CYCLIFY-DOOM "An actor can see two books on the shelf" '?MT)

 
(define  parse-room ( &optional (room '?Room) (contextstr ""))
    (clet (all (items (room-text room)))
      (cdolist (ele items)
        (format t "~&parsing ~s~&" (cons (car ele)(delete-if #'useless-string (cdr ele))))
        (clet (cycl desc named)
         (cdolist (string (cdr ele))
            (pwhen (stringp string)
              (fif (> (count #\Space string) 3)
                (csetq desc (cons string desc))
                (trace-warn (csetq cycl (append cycl (list (CYCLIFY-DOOM (localize-desc-string "" string)(car ele)))))))))
         (pwhen desc (trace-warn (csetq cycl (append cycl (list (CYCLIFY-DOOM (localize-desc-string (generate-phrase (car ele)) (join-strings desc ". ")) (car ele)))))))
         (trace-warn (csetq all  (append all `((#$ist (#$MtForTemporalThing ,(car ele)) (#$and ,@(remove-duplicates cycl #'equal)))))))))

      (ret all)))
                            
      '(cyclify "Two small curved ramps on either side of the
room lead north to the lower part of the bridge, and a large circular
skylight shows the space outside the ship")



;;(Doom-assert-now `(#$implies (#$and (#$isa ?ROOM #$BPVLocation) (#$doomParseRoom ?EXPANDSUBLFN ?ROOM "")) ?CYCL) *CreationMt*)

#|
(Doom-assert-now `(#$implies
          (#$and
           (#$isa ?PARSER #$CycNLParser)
           (#$isa ?ROOM #$BPVLocation)
           (#$definiteDescriptions ?ROOM ?STRING)
           (#$cyclificationOfStringFromParser ?STRING ?CYCL ?PARSER))
             (#$ist (#$MtForTemporalThing ?ROOM) ?CYCL)) *CreationMt*)



Jul 4, 2006 9:52:19 PM java.util.logging.LogManager$RootLogger log
INFO: About to run command: (GET-GKE-ARG-OPTIONS #$ConsciousActivity (list 2 2 4 2) :siblings (list #$thereExists '?GROUP-1 (list #$thereExists '?LOC-1 (list #$and (list #$isa '?GROUP-1 #$Organization) (list #$isa '?LOC-1 #$GeographicalRegion) (list #$affiliatedWith '?GROUP-1 '?IND) (list #$performsInsAtLocation '?GROUP-1 #$DrugTrafficking '?LOC-1)))) #$EnglishParaphraseMt *SMT* 1000 #$DrugTrafficking).
Jul 4, 2006 9:52:19 PM com.cyc.framework.ui.gke2.TermNavigatorPanel startGettingSpecializedTerms
INFO: About to run command: 
(GET-GKE-ARG-OPTIONS #$ConsciousActivity (list 2 2 4 2) :siblings (list #$thereExists '?GROUP-1 (list #$thereExists '?LOC-1 (list #$and (list #$isa '?GROUP-1 #$Organization) (list #$isa '?LOC-1 #$GeographicalRegion) (list #$affiliatedWith '?GROUP-1 '?IND) (list #$performsInsAtLocation '?GROUP-1 #$DrugTrafficking '?LOC-1)))) #$EnglishParaphraseMt *SMT* 1000 #$DrugTrafficking).
Jul 4, 2006 9:52:19 PM java.util.logging.LogManager$RootLogger log
INFO: About to run command: (GET-GKE-ARG-OPTIONS #$ConsciousActivity (list 2 2 4 2) :parents (list #$thereExists '?GROUP-1 (list #$thereExists '?LOC-1 (list #$and (list #$isa '?GROUP-1 #$Organization) (list #$isa '?LOC-1 #$GeographicalRegion) (list #$affiliatedWith '?GROUP-1 '?IND) (list #$performsInsAtLocation '?GROUP-1 #$DrugTrafficking '?LOC-1)))) #$EnglishParaphraseMt *SMT* 1000 #$DrugTrafficking).
Jul 4, 2006 9:52:19 PM com.cyc.framework.ui.gke2.TermNavigatorPanel startGettingSpecializedTerms
INFO: About to run command: 
(GET-GKE-ARG-OPTIONS #$ConsciousActivity (list 2 2 4 2) :parents (list #$thereExists '?GROUP-1 (list #$thereExists '?LOC-1 (list #$and (list #$isa '?GROUP-1 #$Organization) (list #$isa '?LOC-1 #$GeographicalRegion) (list #$affiliatedWith '?GROUP-1 '?IND) (list #$performsInsAtLocation '?GROUP-1 #$DrugTrafficking '?LOC-1)))) #$EnglishParaphraseMt *SMT* 1000 #$DrugTrafficking).
Jul 4, 2006 9:52:19 PM java.util.logging.LogManager$RootLogger log
INFO: About to run command: (GET-GKE-ARG-OPTIONS #$ConsciousActivity (list 2 2 4 2) :children (list #$thereExists '?GROUP-1 (list #$thereExists '?LOC-1 (list #$and (list #$isa '?GROUP-1 #$Organization) (list #$isa '?LOC-1 #$GeographicalRegion) (list #$affiliatedWith '?GROUP-1 '?IND) (list #$performsInsAtLocation '?GROUP-1 #$DrugTrafficking '?LOC-1)))) #$EnglishParaphraseMt *SMT* 1000 #$DrugTrafficking).
Jul 4, 2006 9:52:19 PM com.cyc.framework.ui.gke2.TermNavigatorPanel startGettingSpecializedTerms
INFO: About to run command: 
(GET-GKE-ARG-OPTIONS #$ConsciousActivity (list 2 2 4 2) :children (list #$thereExists '?GROUP-1 (list #$thereExists '?LOC-1 (list #$and (list #$isa '?GROUP-1 #$Organization) (list #$isa '?LOC-1 #$GeographicalRegion) (list #$affiliatedWith '?GROUP-1 '?IND) (list #$performsInsAtLocation '?GROUP-1 #$DrugTrafficking '?LOC-1)))) #$EnglishParaphraseMt *SMT* 1000 #$DrugTrafficking).
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ALLOW-HL-PREDICATE-TRANSFORMATION? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ALLOW-ABNORMALITY-CHECKING? null -> T]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ALLOW-EVALUATABLE-PREDICATE-TRANSFORMATION? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:EQUALITY-REASONING-DOMAIN null -> :ALL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:TRANSFORMATION-ALLOWED? null -> T]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ALLOW-UNBOUND-PREDICATE-TRANSFORMATION? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ANSWER-LANGUAGE null -> :EL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:BROWSABLE? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:CACHE-INFERENCE-RESULTS? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:EQUALITY-REASONING-METHOD null -> :CZER-EQUAL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:CONDITIONAL-SENTENCE? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:EVALUATE-SUBL-ALLOWED? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:NEGATION-BY-FAILURE? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:REWRITE-ALLOWED? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:FORGET-EXTRA-RESULTS? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:DIRECTION null -> :BACKWARD]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:NEW-TERMS-ALLOWED? null -> T]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:FORWARD-MAX-TIME null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:MAX-PROBLEM-COUNT null -> 100000]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:MAX-TRANSFORMATION-DEPTH null -> 0]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:PRODUCTIVITY-LIMIT null -> 2000000]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:MAX-PROOF-DEPTH null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:REMOVAL-BACKTRACKING-PRODUCTIVITY-LIMIT null -> 200]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:RETURN null -> :BINDINGS]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:BLOCK? null -> NIL]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:TRANSITIVE-CLOSURE-MODE null -> :NONE]
Jul 4, 2006 9:52:29 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:COMPLETENESS-MINIMIZATION-ALLOWED? null -> T]
Jul 4, 2006 9:52:38 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:MAX-TRANSFORMATION-DEPTH 0 -> NIL]
Jul 4, 2006 9:52:47 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ALLOW-HL-PREDICATE-TRANSFORMATION? false -> true]
Jul 4, 2006 9:53:02 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ALLOW-EVALUATABLE-PREDICATE-TRANSFORMATION? false -> true]
Jul 4, 2006 9:53:12 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:ALLOW-UNBOUND-PREDICATE-TRANSFORMATION? false -> true]
Jul 4, 2006 9:53:16 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:BROWSABLE? false -> true]
Jul 4, 2006 9:53:20 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:CACHE-INFERENCE-RESULTS? false -> true]
Jul 4, 2006 9:53:36 PM com.cyc.framework.ui.inference.InferenceParametersModel setParameterValue
INFO: firing InferenceParametersEvent[:EVALUATE-SUBL-ALLOWED? false -> true]

|#

(print "loaded doom-creation.lisp")


