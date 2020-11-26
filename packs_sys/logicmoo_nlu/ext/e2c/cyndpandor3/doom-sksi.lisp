;;==================================================
;; INTERFACE MAPPING SETUP (doomEval)
;;==================================================
(defdoom-pred "eval" *CurrentStateMt* 2)
(doom-assert-now `(#$isa ,(doom-pred "eval")  #$AsymmetricBinaryPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "eval")  #$IntangibleObjectPredicate) *VocabularyMt*)
(doom-assert-now `(#$isa ,(doom-pred "eval")  #$FunctionalPredicate) *VocabularyMt*)
(doom-assert-now `(#$comment ,(doom-pred "eval")  " (#$doomEval  (#$TheList .....) ?Result)") *VocabularyMt*)
(doom-assert-now `(#$arity ,(doom-pred "eval")  2) *VocabularyMt*)
(doom-assert-now `(#$arg1Isa ,(doom-pred "eval")  #$Thing) *VocabularyMt*)
(doom-assert-now `(#$arg2Isa ,(doom-pred "eval")  #$Thing) *VocabularyMt*)
(doom-assert-now `(#$notAssertible ,(doom-pred "eval") ) *VocabularyMt*)
(defparameter *doom-host* "10.10.10.193")
(defparameter *doom-port* 3691)
(define doom-eval (&rest outval) 
 (THROW-UNEVALUATABLE-ON-ERROR 
  (ret (clet (*retval* (*stream* (OPEN-TCP-STREAM *DOOM-HOST* *DOOM-PORT*)))
      (prin1 outval *stream*)
      (terpri *stream*)(force-output *stream*)
      (csetq *retval* (read *stream*))
      (close *stream*) *retval*))))    
(define doom-eval (&rest outval) 
      (terpri)(princ ";; DOOM-EVAL")
       (print outval)(force-output)   
      (ret outval ))



#|
(defmacro defdoom-query (doomname value pred result) 
    (clet ((const (doom-clms (Arg2Isa pred) doomname  "DOOM-" (constant-name type) "-CMLS"))))(clet ((res (doom-eval `(,doomname ))))))        

(defdoom-query "classname" "light" #$isa #$LightingDevice)
(defdoom-query "orientation" ?P #$locatedAt-Point (coerce-pointfn ?P))
(doom-predicate "touches" ?P #$locatedAt-Point (coerce-pointfn ?P))

   ;;(doom-assert-now `(#$structuredKnowledgeSourceName #$DOOM-KS SKSIContentMicrotheory) ,*MappingMt*)
(defparameter *DOOM-KS* (find-or-create-constant "DOOM-KS"))
(defparameter *DOOM-PS* (find-or-create-constant "DOOM-PS"))
(defparameter *DOOM-LS* (find-or-create-constant "DOOM-LS"))

(define doom-clms (type doomname) 
    (clet ((clms (doom-cyc (cconcatenate "DOOM-" (constant-name type) "-CMLS"))))    
      (doom-assert-now `(#$isa ,clms #$ReifiedMapping ) *MappingMt*)      
      (doom-assert-now `(#$schemaIsa ,clms ,type ) *MappingMt*)        
      ;;(doom-assert-now `(#$logicalFieldMapping ,const #$DOOM-LS ) *MappingMt*)  
   (ret clms)))
(define doom-codemapping (type doomname value) 
    (clet ((clms (doom-clms type doomname)))
      (doom-assert-now `(#$codeMapping ,clms ,doomname ,value ) *MappingMt*)
   (ret clms)))
              
(doom-assert-now `(#$isa #$DOOM-KS #$StructuredKnowledgeSource) *MappingMt*)
(doom-assert-now `(#$structuredKnowledgeSourceName #$DOOM-KS "Doom Running") *MappingMt*)
(doom-assert-now `(#$sksSourceDescriptionMt #$DOOM-KS ,*MappingMt* ) *MappingMt*)
(doom-assert-now `(#$isa #$DOOM-LS #$LogicalSchema) *MappingMt*)
(doom-assert-now `(#$isa #$DOOM-PS #$PhysicalSchema) *MappingMt*)
(doom-assert-now `(#$physicalSchemaSourceMap #$DOOM-PS #$DOOM-KS) *MappingMt*)
(doom-assert-now `(#$schemaFieldNameList #$DOOM-PS  (#$TheList "classname" "id" "inherit" "model" "skin" "owner" "color" "spawnclass" "origin" "name" "orientation")) *MappingMT*)
(doom-assert-now `(#$logicalSchemaSourceMap #$DOOM-LS #$DOOM-KS) *MappingMt*)
(doom-assert-now `(#$logicalPhysicalSchemaMap #$DOOM-LS #$DOOM-PS) *MappingMt*)


(define doom-fm (type doomname &optional n)
    (clet ((clms (doom-clms type doomname))(pf `(#$PhysicalFieldFn #$DOOM-PS ,doomname))(lf (fif n `(#$LogicalFieldFn #$DOOM-LS ,type ,n))))
             (doom-assert-now `(#$schemaIsa ,clms ,type ) *MappingMt*)        
             (doom-assert-now `(#$physicalFields #$DOOM-PS ,pf) *MappingMt*)
             (doom-assert-now `(#$fieldName ,pf ,doomname)  *MappingMt*)
             (doom-assert-now `(#$fieldDataType ,pf #$StringObject)  *MappingMt*)      
                (pwhen lf 
                   (doom-assert-now `(#$logicalFields #$DOOM-LS ,lf) *MappingMt*)
                   (doom-assert-now `(#$fieldIsa ,lf ,type)  *MappingMt*)                
                   (doom-assert-now `(#$logicalPhysicalFieldMap ,lf ,pf )*MappingMt*))
   (ret clms)))
        
(doom-fm #$Collection "classname" 1)
(doom-fm #$Collection "inherit")
(doom-fm #$Color "color")


(doom-codemapping #$Collection "light" #$LightingDevice)
(doom-codemapping #$Collection "door" #$Doorway)
(doom-codemapping #$Color "blue" #$BlueColor)
(doom-codemapping #$OrientationVector "0 0 1" #$RightSideUp)

          
 
#|
 PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
 7224 root      12 -13 1954m 516m 142m R 99.7 25.5 413:06.24 latest.bin
;;==================================================
;; COLLECTION MAPPING SETUP  (AtemporalNecessarilyEssentialCollectionType)
;;==================================================
;;(parse-a-question-completely "Are sodium and chlorine the complete list of elements in the chemical formula of chloride?" #$AllLexicalMicrotheoryPSC )

                       
;;;;(load "cynd//opt/lotus/doom/src/daxmoo/worlds/10.kif")
|#

|#
