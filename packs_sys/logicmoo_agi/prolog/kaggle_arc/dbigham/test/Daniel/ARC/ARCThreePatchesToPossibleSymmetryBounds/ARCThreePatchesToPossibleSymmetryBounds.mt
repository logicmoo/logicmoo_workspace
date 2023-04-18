(*
    Tests for: Daniel`ARC`ARCThreePatchesToPossibleSymmetryBounds
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCThreePatchesToPossibleSymmetryBounds]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCThreePatchesToPossibleSymmetryBounds[
            {{8, 8}, {23, 8}, {23, 23}},
            3,
            3,
            30,
            30
        ]
    ]
    ,
    <|"CenterY" -> 16.5, "CenterX" -> 16.5, "Y" -> 3, "X" -> 3, "Y2" -> 30, "X2" -> 30|>
    ,
    TestID -> "ARCThreePatchesToPossibleSymmetryBounds-20221001-CGZN1E"
]

Test[
    Daniel`ARC`ARCThreePatchesToPossibleSymmetryBounds[
        {{2, 5}, {2, 10}, {13, 5}},
        3,
        3,
        30,
        30
    ]
    ,
    <|"CenterY" -> 8.5, "CenterX" -> 8.5, "Y" -> 1, "X" -> 1, "Y2" -> 16, "X2" -> 16|>
    ,
    TestID -> "ARCThreePatchesToPossibleSymmetryBounds-20221002-B6W0UP"
]