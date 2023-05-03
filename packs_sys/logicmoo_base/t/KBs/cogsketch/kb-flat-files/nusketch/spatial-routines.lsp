;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: spatial-routines.lsp
;;;;    System: nuSketch
;;;;    Author: Andrew Lovett
;;;;   Created: June 9, 2010 14:56:43
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2016-11-29 18:27:53 -0600 (Tue, 29 Nov 2016) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relations describing expressions that hold in image


(isa changeBetweenImages Relation)
(arity changeBetweenImages 3)
(arg1Isa changeBetweenImages CycLExpression)
(arg2Isa changeBetweenImages VisualImage)
(arg3Isa changeBetweenImages VisualImage)
(comment changeBetweenImages "Describes an expression that holds in the first image
                              but not the second image.")
      
(isa changeBetweenImagesFromTo Relation)
(arity changeBetweenImagesFromTo 4)
(arg1Isa changeBetweenImagesFromTo CycLExpression)
(arg2Isa changeBetweenImagesFromTo CycLExpression)
(arg3Isa changeBetweenImagesFromTo VisualImage)
(arg4Isa changeBetweenImagesFromTo VisualImage)
(comment changeBetweenImagesFromTo "Describes a change between an expression that holds
                                    in the first image and one that holds in the second.")

(isa reversalBetweenImages Relation)
(arity reversalBetweenImages 4)
(arg1Isa reversalBetweenImages CycLExpression)
(arg2Isa reversalBetweenImages CycLExpression)
(arg3Isa reversalBetweenImages VisualImage)
(arg4Isa reversalBetweenImages VisualImage)
(comment reversalBetweenImages "Describes a change between an expression that holds
                                    in the first image and the reverse, which holds in the second.")

(holdsInImage Relation)
(arity holdsInImage 2)
(arg1Isa holdsInImage CycLExpression)
(arg2Isa holdsInImage VisualImage)
(comment holdsInImage "Describes an expression that holds in an image.")

(isa EmptyImage Collection)
(comment EmptyImage "Set of images with no elements in them.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions specifying a particular element in a particular image

(isa ElementInImageFn Function-Denotational)
(arity ElementInImageFn 2)
(arg1Isa ElementInImageFn Entity)
(arg2Isa ElementInImageFn VisualImage)
(resultIsa ElementInImageFn Entity)
(comment ElementInImageFn "(ElementInImageFn ?element ?image) refers to the instance of
                           ?element that appears in image ?image.")

(isa ElementAddedInImageFn Function-Denotational)
(arity ElementAddedInImageFn 2)
(arg1Isa ElementAddedInImageFn Entity)
(arg2Isa ElementAddedInImageFn VisualImage)
(resultIsa ElementAddedInImageFn Entity)
(comment ElementAddedInImageFn "(ElementInImageFn ?element ?image) refers to the instance of
                               ?element that is added into image ?image (to contrast it with
                               some other image in which this element is not found).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions specifying a particular element in a particular image

(isa ElementInARowFn Function-Denotational)
(atomicAnalogyNat ElementInARowFn)
(arity ElementInARowFn 1)
(arg1Isa ElementInARowFn Number)
(resultIsa ElementInARowFn Collection)
(comment ElementInARowFn "(ElementInARowFn ?n) denotes the nth element in a row.")
      
      
(isa ReifiedAttFn Function-Denotational)
(atomicAnalogyNat ReifiedAttFn)
(arity ReifiedAttFn 2) 
(arg1Isa ReifiedAttFn Entity)
(arg1Isa ReifiedAttFn Collection)
(resultIsa ReifiedAttFn Entity)
(comment ReifiedAttFn "(ReifiedAttFn ?e ?a) is a reification of attribute ?a belonging
                        to entity ?e.")
      
      
(isa ImageFn Function-Denotational)
(atomicAnalogyNat ImageFn)
(arity ImageFn 2)
(arg1Isa ImageFn Entity)
(arg1Isa ImageFn Number)
(resultIsa ImageFn Entity)
(comment ImageFn "(ImageFn ?e ?n) is a reification of image ?n for entity
                   ?e.")
      

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code