(*
    Tests for: Daniel`ARC`ARCObjectEdgePixels
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCObjectEdgePixels]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCObjectEdgePixels[
        <|
            "PixelPositions" -> {{9, 2}, {9, 3}, {9, 4}, {10, 1}, {10, 2}, {10, 4}},
            "Y" -> 9,
            "X" -> 1,
            "Width" -> 4,
            "Height" -> 2
        |>,
        {-1, 0}
    ]
    ,
    {{9, 2}, {9, 3}, {9, 4}}
    ,
    TestID -> "ARCObjectEdgePixels-20220803-ZHWPOW"
]

Test[
    Daniel`ARC`ARCObjectEdgePixels[
        <|
            "PixelPositions" -> {{9, 2}, {9, 3}, {9, 4}, {10, 1}, {10, 2}, {10, 4}},
            "Y" -> 9,
            "X" -> 1,
            "Width" -> 4,
            "Height" -> 2
        |>,
        {1, 0}
    ]
    ,
    {{10, 1}, {10, 2}, {10, 4}}
    ,
    TestID -> "ARCObjectEdgePixels-20220803-2KCN67"
]

Test[
    Daniel`ARC`ARCObjectEdgePixels[
        <|
            "PixelPositions" -> {{9, 2}, {9, 3}, {9, 4}, {10, 1}, {10, 2}, {10, 4}},
            "Y" -> 9,
            "X" -> 1,
            "Width" -> 4,
            "Height" -> 2
        |>,
        {0, -1}
    ]
    ,
    {{10, 1}}
    ,
    TestID -> "ARCObjectEdgePixels-20220803-2MFAF0"
]

Test[
    Daniel`ARC`ARCObjectEdgePixels[
        <|
            "PixelPositions" -> {{9, 2}, {9, 3}, {9, 4}, {10, 1}, {10, 2}, {10, 4}},
            "Y" -> 9,
            "X" -> 1,
            "Width" -> 4,
            "Height" -> 2
        |>,
        {0, 1}
    ]
    ,
    {{9, 4}, {10, 4}}
    ,
    TestID -> "ARCObjectEdgePixels-20220803-R22OD9"
]