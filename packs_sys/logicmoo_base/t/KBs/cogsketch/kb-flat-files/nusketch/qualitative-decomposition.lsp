;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: qualitative-decomposition.lsp
;;;;    System: nuSketch
;;;;    Author: Matthew Klenk
;;;;   Created: Thursday, July 15, 2004
;;;;   Purpose: KB assertions for glyph-decomposition
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2016-11-29 18:27:53 -0600 (Tue, 29 Nov 2016) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(isa NuSketchPoint Collection)
(comment NuSketchPoint "Each Instance of NuSketchPoint refers to a specific
sketch-polyline-point.")
(genls NuSketchPoint SpatialThing)

(isa NuSketchSegment Collection)
(comment NuSketchSegment "Each Instance of NuSketchSegment refers to conceptual ink segment")
(genls NuSketchSegment SpatialThing)

(isa NuSketchPointFn BinaryFunction)
(arity NuSketchPointFn 2)
(arg1Isa NuSketchPointFn RealNumber)
(arg2Isa NuSketchPointFn RealNumber)
(resultIsa NuSketchPointFn NuSketchPoint)

(isa LeftmostBottomPointFn UnaryFunction)
(arity LeftmostBottomPointFn 1)
(arg1Isa LeftmostBottomPointFn NuSketchGlyph)
(resultIsa LeftmostBottomPointFn NuSketchPoint)

(isa RightmostBottomPointFn UnaryFunction)
(arity RightmostBottomPointFn 1)
(arg1Isa RightmostBottomPointFn NuSketchGlyph)
(resultIsa RightmostBottomPointFn NuSketchPoint)

(isa LeftmostTopPointFn UnaryFunction)
(arity LeftmostTopPointFn 1)
(arg1Isa LeftmostTopPointFn NuSketchGlyph)
(resultIsa LeftmostTopPointFn NuSketchPoint)

(isa RightmostTopPointFn UnaryFunction)
(arity RightmostTopPointFn 1)
(arg1Isa RightmostTopPointFn NuSketchGlyph)
(resultIsa RightmostTopPointFn NuSketchPoint)

(isa TopLeftmostPointFn UnaryFunction)
(arity TopLeftmostPointFn  1)
(arg1Isa TopLeftmostPointFn NuSketchGlyph)
(resultIsa TopLeftmostPointFn NuSketchPoint)

(isa TopRightmostPointFn UnaryFunction)
(arity TopRightmostPointFn 1)
(arg1Isa TopRightmostPointFn NuSketchGlyph)
(resultIsa TopRightmostPointFn NuSketchPoint)

(isa BottomLeftmostPointFn UnaryFunction)
(arity BottomLeftmostPointFn 1)
(arg1Isa BottomLeftmostPointFn NuSketchGlyph)
(resultIsa BottomLeftmostPointFn NuSketchPoint)

(isa BottomRightmostPointFn UnaryFunction)
(arity BottomRightmostPointFn 1)
(arg1Isa BottomRightmostPointFn NuSketchGlyph)
(resultIsa BottomRightmostPointFn NuSketchPoint)

(isa CentroidPointFn UnaryFunction)
(arity CentroidPointFn 1)
(arg1Isa CentroidPointFn NuSketchGlyph)
(resultIsa CentroidPointFn NuSketchPoint)

(isa ArrowHeadPointFn UnaryFunction)
(arity ArrowHeadPointFn 1)
(arg1Isa ArrowHeadPointFn NuSketchGlyph)
(resultIsa ArrowHeadPointFn NuSketchPoint)

(isa ArrowTailPointFn UnaryFunction)
(arity ArrowTailPointFn 1)
(arg1Isa ArrowTailPointFn NuSketchGlyph)
(resultIsa ArrowTailPointFn NuSketchPoint)

(isa qualitativeVectorBetween Predicate)
(arity qualitativeVectorBetween 3)
(comment qualitativeVectorBetween "qualitativeDirection is true iff the third argument is the qualitative direction from the first argument to the second argument")
(arg1Isa qualitativeVectorBetween NuSketchPoint)
(arg2Isa qualitativeVectorBetween NuSketchPoint)
(arg3Isa qualitativeVectorBetween QualitativeVector)

(isa overlappingParts Predicate)
(arity overlappingParts 4)
(arg1Isa overlappingParts  NuSketchGlyph)
(arg2Isa overlappingParts  NuSketchSegment)
(arg3Isa overlappingParts  NuSketchGlyph)
(arg4Isa overlappingParts  NuSketchSegment)
(comment overlappingParts  "(overlappingParts <glyph1> <part1> <glyph2> <part2>) utilizes the glyph-decomposition reasoning source.  The two glyph arguments must be known.  Part1 is a reified segement of Glyph1 that is overlapping with Glyph2.")

(isa outsideNormal Predicate)
(arity outsideNormal 3)
(arg1Isa outsideNormal  NuSketchGlyph)
(arg2Isa outsideNormal  SpatialThing)
(arg3Isa outsideNormal  QualitativeVector)
(comment outsideNormal  "(outsideNormal <glyph> <point-or-segment> <qv-normal>) utilizes the glyph-decomposition reasoning source.  The first two arguments must be known.")

(isa distanceBetween Predicate)
(arity distanceBetween 3)
(arg1Isa distanceBetween  SpatialThing)
(arg2Isa distanceBetween  SpatialThing)
(arg3Isa distanceBetween  Number)
(comment distanceBetween  "(distanceBetween <point1> <point2> <number>) utilizes the glyph-decomposition reasoning source.  The first two arguments must be known.")

