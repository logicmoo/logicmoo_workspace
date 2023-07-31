(*
    Tests for: Daniel`ARC`ARCImageFlips
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageFlips]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageFlips[{{1, -1, 1}, {-1, 1, -1}, {-1, -1, 1}}]
    ]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{-1, -1, 1}, {-1, 1, -1}, {1, -1, 1}}],
            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {1, -1, -1}}],
            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
        |>
    }
    ,
    TestID -> "ARCImageFlips-20220906-BUBGRS"
]

Test[
    Daniel`ARC`ARCImageFlips[{{1, 1}}]
    ,
    {}
    ,
    TestID -> "ARCImageFlips-20220908-LLB1P4"
]

Test[
    Daniel`ARC`ARCImageFlips[{{1, 1}}, "IncludeNoopTransforms" -> True]
    ,
    {
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1}}],
            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Vertical"|>
        |>,
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1}}],
            "Transform" -> <|"Type" -> "Flip", "Direction" -> "Horizontal"|>
        |>
    }
    ,
    TestID -> "ARCImageFlips-20220908-IJIICU"
]