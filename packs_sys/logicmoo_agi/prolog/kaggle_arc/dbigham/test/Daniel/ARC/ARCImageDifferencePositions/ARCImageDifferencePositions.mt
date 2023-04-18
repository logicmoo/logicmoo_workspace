(*
    Tests for: Daniel`ARC`ARCImageDifferencePositions
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageDifferencePositions]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCImageDifferencePositions[{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}]
    ,
    {{1, 1}, {1, 2}, {2, 1}, {2, 2}}
    ,
    TestID -> "ARCImageDifferencePositions-20221007-ZFTBT4"
]

Test[
    Daniel`ARC`ARCImageDifferencePositions[{{1, 2}, {3, 4}}, {{1, 2}, {3, 4}}]
    ,
    {}
    ,
    TestID -> "ARCImageDifferencePositions-20221007-HFYP5R"
]