
(define sim-equal (x y)
  ;;(pwhen (consp x) (ret (sim-equal y (constant-name x))))
    (pwhen (constant-p x) (fif (sim-equal y (constant-name x))(ret x)))
    (ret (fif (string-equal (format nil "~A" x)(format nil "~A" y)) x)))
    
(define sim-fn-name (name) 
  (csetq name (join-strings  (mapcar #'string-proper (string-tokenize name '(#\space #\-))) ""))
  (punless (string-equal (substring name 0 4) "sim") (csetq name (cconcatenate "Sim" (string-proper name))))
  (punless (string-equal (substring name (- (length name) 1)) "fn") (csetq name (cconcatenate (string-proper name) "Fn")))
  (ret (sim-cyc name)))

;;(sim-assert `(#$equiv (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$East-Directly) ?P1 ?P2)  (#$permanentlyEastOf ?P2 ?P1))  *CreationMt* '(:DIRECTION :FORWARD))
;;(sim-assert `(#$equiv (#$containsPortals ?P1 (#$BoundsOfDirectionFn ?P1 #$North-Directly)) (#$permanentlyNorthOf ?P2 ?P1))  *CreationMt* '(:DIRECTION :FORWARD))
;;(sim-assert `(#$equiv (#$containsPortals ?P1 (#$BoundsOfDirectionFn ?P1 #$South-Directly)) (#$southOf ?P2 ?P1))  *CreationMt* '(:DIRECTION :FORWARD))
;;(sim-assert `(#$equiv (#$containsPortals ?P1 (#$BoundsOfDirectionFn #$West-Directly ?P2) (#$westOf ?P2 ?P1))  *UVMt* '(:DIRECTION :FORWARD))
;;;(sim-assert `(#$implies (#$isa #$BPVAgent)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z)))(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z 700)))) *UVMt* '(:DIRECTION :FORWARD)
;;(sim-assert '(#$implies (#$and (#$isa ?X #$BPVItem) (#$isa ?X ?Type) (#$relationAllInstance ?Pred ?Type ?Instance)) (#$ist *MappingMt* (?Pred ?X ?Instance))) *MappingMt* '(:DIRECTION :FORWARD))
;;(sim-assert '(#$implies (#$and (#$isa ?X #$BPVAgent) (#$isa ?X ?Type) (#$relationAllInstance ?Pred ?Type ?Instance)) (#$ist *MappingMt* (?Pred ?X ?Instance))) *MappingMt* '(:DIRECTION :FORWARD))
(define ccc (mt)
 (sim-assert 
  `(#$implies 
  (#$and
   (#$objectFoundInLocation?O ?L)
   (#$isa?O #$BPVAgent)
   (#$isa?L #$BPVLocation))
 (#$and
  (#$ist ,mt (#$simPropertyNext (#$SimItemFn (#$ConstantNamedFn ?O)) (#$SimPropertyFn "classname") (#$SimClassFn "cyc_bot")))
  (#$ist ,mt (#$simPropertyNext (#$SimItemFn (#$ConstantNamedFn ?L)) (#$SimPropertyFn "classname") (#$SimClassFn "mud_room"))))) mt))

(sim-assert `(#$quotedIsa #$MtForTemporalThing #$ForwardReifiableCycLFunctor))

(sim-assert 
  `(#$implies     
    (#$and 
      (#$isa ?MT #$SimStateMicrotheory)
      (#$ist-Asserted ?MT (#$isa ?AGENT (#$SimClassFn ?CLASSNAME)))
      (#$genls (#$SimClassFn ?CLASSNAME) #$BPVAgent)
      (#$simItemToName ?AGENT  ?AGENTNAME))    
    (#$ist ?MT (#$and 
       (#$simPropertyNext ?AGENTNAME "classname" ?CLASSNAME) 
      (#$isa ?AGENT #$SimAvatar)))))
      


(sim-assert '(#$implies (#$isa ?AGENT #$SimAvatar)
    (#$and 
      (#$isa (#$MtForTemporalThing ?AGENT) #$SimAgentMicrotheory)
;;      (#$assertionMt ?SENTS (#$MtForTemporalThing ?AGENT)
      (#$mtSource (#$MtForTemporalThing ?AGENT)  ?AGENT)
    (#$equiv (#$ist-Asserted (#$MtForTemporalThing ?AGENT) ?SENTS)(#$beliefs ?AGENT ?SENTS)))))

 
;;(CCC #$SimCurrentStateMt)


(define  recompute-boundries (Mt) 
    (sim-assert `(#$arg1Isa #$BoundsOfDirectionFn #$BPVLocation)  *CreationMt* '(:DIRECTION :FORWARD))
    (sim-assert `(#$resultIsa #$BoundsOfDirectionFn #$Path-Spatial)  *CreationMt* '(:DIRECTION :FORWARD))
    (sim-assert '(#$formsBorderPart (#$BoundsOfDirectionFn ?P1 ?Dir) (#$BorderFn ?P1)) *CreationMt* '(:DIRECTION :FORWARD))
    (sim-assert '(#$equiv (#$isa (#$BoundsOfDirectionFn ?P1 ?D1) #$Wall-GenericBarrier)(#$cavityHasWall ?P1 (#$BoundsOfDirectionFn ?P1 ?D1) ) ) *CreationMt* '(:DIRECTION :FORWARD))
    
    (sim-assert `(#$implies (#$and (#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X1 ?Y1 ?Z))(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn ?X2 ?Y2 ?Z))) (#$levelWith ?P1 ?P2))   *CreationMt*  '(:DIRECTION :FORWARD))
    
    (sim-assert '(#$implies (#$isa ?P1 #$BPVLocation)(#$and (#$isa (#$BorderFn ?P1) #$Border)(#$formsBorderPart (#$BoundsOfDirectionFn ?P1 ?Dir) (#$BorderFn ?P1)))) *CreationMt* '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$pathBetween (#$BoundsOfDirectionFn ?P1 ?DIR) ?P1 ?P2)(#$containsPortals ?P1 (#$BoundsOfDirectionFn ?P1 ?DIR)))  *CreationMt* '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$pathBetween ?PATH ?P1 ?P2)(#$containsPortals ?P1 ?PATH))  *CreationMt*)
    
    
    (sim-assert '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$West-Directly) #$Wall-GenericBarrier) (#$simIsa (#$BoundsOfDirectionFn ?P1 #$West-Directly) (#$SimClassFn "mud_wclosed" ))) *CreationMt*)
    (sim-assert '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$Up-Directly) #$Wall-GenericBarrier) (#$simIsa (#$BoundsOfDirectionFn ?P1 #$Up-Directly) (#$SimClassFn "mud_uclosed" ))) *CreationMt*)
    (sim-assert '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$South-Directly) #$Wall-GenericBarrier) (#$simIsa (#$BoundsOfDirectionFn ?P1 #$South-Directly) (#$SimClassFn "mud_sclosed" ))) *CreationMt*)
    (sim-assert '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$North-Directly) #$Wall-GenericBarrier) (#$simIsa (#$BoundsOfDirectionFn ?P1 #$North-Directly)(#$SimClassFn "mud_nclosed" ))) *CreationMt*)
    (sim-assert '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$East-Directly) #$Wall-GenericBarrier) (#$simIsa (#$BoundsOfDirectionFn ?P1 #$East-Directly) (#$SimClassFn "mud_eclosed" ))) *CreationMt*)
    (sim-assert '(#$implies (#$isa (#$BoundsOfDirectionFn ?P1 #$Down-Directly) #$Wall-GenericBarrier) (#$simIsa (#$BoundsOfDirectionFn ?P1 #$Down-Directly) (#$SimClassFn  "mud_dclosed"))) *CreationMt*)

    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$West-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X -700) ?Y ?Z))))  Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$Up-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z 300)))) Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$South-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y -700) ?Z))))  Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$North-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y 700) ?Z))))  Mt)
    (cyc-unassert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$East-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X 700) ?Y ?Z))))  Mt)
   ;;(sim-assert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$Down-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z -700)))) *CreationMt*  '(:DIRECTION :FORWARD))

    (sim-assert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$West-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X -700) ?Y ?Z)))) Mt '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$Up-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X ?Y (#$PlusFn ?Z 300))))  Mt  '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$South-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y -700) ?Z)))) Mt  '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$North-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn  ?X (#$PlusFn ?Y 700) ?Z))))  Mt '(:DIRECTION :FORWARD))
    (sim-assert `(#$implies (#$and (#$pathBetween (#$BoundsOfDirectionFn ?P1 #$East-Directly) ?P1 ?P2)(#$locatedAtPoint-Spatial ?P1 (#$Point3Fn ?X ?Y ?Z))) (#$and (#$levelWith ?P1 ?P2)(#$locatedAtPoint-Spatial ?P2 (#$Point3Fn (#$PlusFn ?X 700) ?Y ?Z))))  Mt '(:DIRECTION :FORWARD))
)               


(recompute-boundries *CreationMt*)

    
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
;;;;(#$isa  #$simClassToBinding)
;;(define  exits-open (loc) (ret (is-of-exit-type loc #$OpenPortal)))
;;(define  exits-closed (loc) (ret (is-of-exit-type loc #$ClosedPortal)))		'

 ;;PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
 ;;7224 root      3 -13 1955m 433m 103m R 99.8 21.4  23:45.48 latest-cyc.bin
 
 ;;PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
  ;;7224 root      3 -13 1955m 634m 104m R 99.5 31.3  56:26.76 latest-cyc.bin
(cyc-assert '(#$implies (#$and (#$pathBetween ?PATH1 ?D1 ?D2) (#$pathBetween ?PATH2 ?D2 ?D1))(#$equals ?PATH1 ?PATH2))  *StaticStateMt* '(:DIRECTION :BACKWARD :STRENGTH :MONOTONIC) )
                                                                                                                
(sim-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Bedroom)) (#$isa ?PATH #$Doorway))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(sim-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$OfficeSpace-Personal)) (#$isa ?PATH #$Doorway))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(sim-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$isa ?PATH #$OpenPortal))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(sim-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$not (#$isa ?PATH #$Doorway)))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(sim-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2) (#$isa ?D1 #$Hallway)(#$isa ?D2 #$Hallway)) (#$not (#$isa ?PATH #$ClosedPortal)))  *CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
(sim-assert '(#$implies (#$and (#$pathBetween ?PATH ?D1 ?D2)(#$isa ?PATH #$Portal)) (#$bordersOn  ?D1 ?D2) )*CreationMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
;;;;(sim-assert '(#$implies (#$and (#$pathBetween ?PATH ?P1 ?P2) (#$isa ?P2 #$Hallway)) (#$isa ?PATH #$Doorway))  *MappingMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )
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



(defmacro trace-warn (&rest code)  (ret `(with-error-handler #'(lambda () (format t "~&:ERROR ~s in ~s" *ERROR-MESSAGE* ',code)(force-output)) (sl::progn ,@code))))
(define  cdr-equal (x y) (ret (cor (sim-equal x y)(cand (consp x)(consp y)(sim-equal (cdr x) (cdr y))))))



;;(sim-assert `(#$implies (#$and (#$isa ?ROOM #$BPVLocation) (#$simParseRoom ?EXPANDSUBLFN ?ROOM "")) ?CYCL) *CreationMt*)
(sim-assert `(#$implies
          (#$and
           (#$isa ?PARSER #$CycNLParser)
           (#$isa ?ROOM #$BPVLocation)
           (#$definiteDescriptions ?ROOM ?STRING)
           (#$cyclificationOfStringFromParser ?STRING ?CYCL ?PARSER))
             (#$ist (#$MtForTemporalThing ?ROOM) ?CYCL)) *CreationMt*)


#|


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

(print "loaded sim-creation.lisp")


