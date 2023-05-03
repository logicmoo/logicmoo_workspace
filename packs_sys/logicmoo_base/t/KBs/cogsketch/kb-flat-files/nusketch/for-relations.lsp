;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: for-relations.lsp
;;;;    System: nuSketch
;;;;    Author: Andrew Lovett
;;;;   Created: 
;;;;   Purpose: Frame of Reference Relations
;;;; ---------------------------------------------------------------------------
;;;;  modified: Sunday, December 13, 2009 at 21:48:27 by Andrew
;;;; ---------------------------------------------------------------------------
 
(in-package :data)

;;(fire:load-kb-tools)
;;(fire:meld-file->kb (concatenate 'string (make-qrg-path) "nusketch\\v3\\kb-flat-files\\for-relations.lsp"))

;;(fire::dump-structural-cache)

(isa ReferenceFrameRelation Collection)

(isa onLeftHalfOf Predicate)
(isa onLeftHalfOf ReferenceFrameRelation)
(arity onLeftHalfOf 2)
(arg1Isa onLeftHalfOf SpatialThing-Localized)
(arg2Isa onLeftHalfOf SpatialThing-Localized)
(comment onLeftHalfOf "First argument is on the left half of second argument.")

(isa onRightHalfOf Predicate)
(isa onRightHalfOf ReferenceFrameRelation)
(arity onRightHalfOf 2)
(arg1Isa onRightHalfOf SpatialThing-Localized)
(arg2Isa onRightHalfOf SpatialThing-Localized)
(comment onRightHalfOf "First argument is on the right half of second argument.")

(isa onTopHalfOf Predicate)
(isa onTopHalfOf ReferenceFrameRelation)
(arity onTopHalfOf 2)
(arg1Isa onTopHalfOf SpatialThing-Localized)
(arg2Isa onTopHalfOf SpatialThing-Localized)
(comment onTopHalfOf "First argument is on the top half of second argument.")

(isa onBottomHalfOf Predicate)
(isa onBottomHalfOf ReferenceFrameRelation)
(arity onBottomHalfOf 2)
(arg1Isa onBottomHalfOf SpatialThing-Localized)
(arg2Isa onBottomHalfOf SpatialThing-Localized)
(comment onBottomHalfOf "First argument is on the bottom half of second argument.")

(isa centeredOn Predicate)
(isa centeredOn ReferenceFrameRelation)
(arity centeredOn 2)
(arg1Isa centeredOn SpatialThing-Localized)
(arg2Isa centeredOn SpatialThing-Localized)
(comment centeredOn "First argument is centered on the second argument.")

(isa onXAxisOf Predicate)
(isa onXAxisOf ReferenceFrameRelation)
(arity onXAxisOf 2)
(arg1Isa onXAxisOf SpatialThing-Localized)
(arg2Isa onXAxisOf SpatialThing-Localized)
(comment onXAxisOf "First argument is on the x-axis of second argument.")

(isa onYAxisOf Predicate)
(isa onYAxisOf ReferenceFrameRelation)
(arity onYAxisOf 2)
(arg1Isa onYAxisOf SpatialThing-Localized)
(arg2Isa onYAxisOf SpatialThing-Localized)
(comment onYAxisOf "First argument is on the y-axis of second argument.")