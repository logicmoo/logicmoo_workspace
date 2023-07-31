(*
    Tests for: Daniel`ARC`ARCObjectImageShape
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCObjectImageShape]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCObjectImageShape[<|"Colors" -> {1}, "Shapes" -> {<|"Image" -> "HERE"|>}|>]
    ,
    "HERE"
    ,
    TestID -> "ARCObjectImageShape-20220902-II9ZXY"
]

Test[
    Daniel`ARC`ARCObjectImageShape[<|"Colors" -> {1, 2}, "Image" -> "HERE"|>]
    ,
    "HERE"
    ,
    TestID -> "ARCObjectImageShape-20220902-NP00NQ"
]

Test[
    Daniel`ARC`ARCObjectImageShape[Daniel`ARC`ARCScene[{{1, -1, 2}, {-1, 1, -1}}]]
    ,
    Daniel`ARC`ARCScene[{{1, -1, 2}, {-1, 1, -1}}]
    ,
    TestID -> "ARCObjectImageShape-20220911-HRQDT1"
]

Test[
    Daniel`ARC`ARCObjectImageShape[Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}}]]
    ,
    Daniel`ARC`ARCScene[{{10, -1, 10}, {-1, 10, -1}}]
    ,
    TestID -> "ARCObjectImageShape-20220911-GW225X"
]