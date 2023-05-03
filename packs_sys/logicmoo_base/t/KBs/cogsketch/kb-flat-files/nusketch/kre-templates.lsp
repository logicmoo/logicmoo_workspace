;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: kre-templates.lsp
;;;;    System: nuSketch 
;;;;    Author: Madeline Usher
;;;;   Created: October 15, 2002 17:16:52
;;;;   Purpose: Templates for creating and editing non-visual facts about 
;;;;            a sketch.
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2016-11-29 18:27:53 -0600 (Tue, 29 Nov 2016) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions

;;; nonVisualFactTemplate
;;;
;;; Example:
;;;
;;; (nonVisualFactTemplate NuSketchCOA 
;;;   (enables-PropProp 
;;;    (wellConcealedAt ?actor ?position ?viewer) 
;;;    (hasAttributes ?task Successful-ActionAttribute))
;;;   (TheSet
;;;    (templateVarDef ?actor "actor"
;;;      (and (isa ?actor ModernMilitaryUnit-Deployable)
;;;           (unitAssignedToTask ?task ?actor)))
;;;    (templateVarDef ?position "position"
;;;      (isa ?position SpatialThing-Localized))
;;;    (templateVarDef ?viewer "viewer"
;;;      (isa ?viewer MilitaryAgent))
;;;    (templateVarDef ?task "task"
;;;      (and (isa ?task MilitaryTask)
;;;           (unitAssignedToTask ?task ?actor))))
;;;   (TheList
;;;    "Being hidden from " ?viewer " at " ?position " will enable "
;;;    ?actor " to perform " ?task "."))

(isa nonVisualFactTemplate Predicate)
(arity nonVisualFactTemplate 4)
(arg1Isa nonVisualFactTemplate NuSketchApplication)
(arg2Isa nonVisualFactTemplate ELSentence-Assertible)
(arg3Isa nonVisualFactTemplate Set-Mathematical)
(arg4Isa nonVisualFactTemplate List)
(comment nonVisualFactTemplate 
         "Describes a fact template for nuSketch facts that are  not 
          represented visually in a sketch.")



(isa templateVarDef Predicate)
(arity templateVarDef 3)
(arg1Isa templateVarDef ELVariable)
(arg2Isa templateVarDef CharacterString)
(arg3Isa templateVarDef ELSentence-Assertible)



(isa templateListVarDef Predicate)
(arity templateListVarDef 3)
(arg1Isa templateListVarDef ELVariable)
(arg2Isa templateListVarDef CharacterString)
(arg3Isa templateListVarDef ELSentence-Assertible)



;;; NonVisualFact

(isa NonVisualFact Collection)
(genls NonVisualFact ELSentence-Assertible)


;;; nonVisualFactLastModified

(isa nonVisualFactLastModified Predicate)
(arity nonVisualFactLastModified 2)
(arg1Isa nonVisualFactLastModified NonVisualFact)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
