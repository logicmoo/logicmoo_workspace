(*
    Tests for: Daniel`ARC`ARCCheckForImputedRectangleForSymmetry
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCheckForImputedRectangleForSymmetry]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCCheckForImputedRectangleForSymmetry[
                Daniel`ARC`ARCParseFile["9ecd008a"]["Train"]
            ]
        ]
    ]
    ,
    <|"Type" -> "Imputation", "Pattern" -> "Symmetry", "Output" -> "ImputedRectangle"|>
    ,
    TestID -> "ARCCheckForImputedRectangleForSymmetry-20220928-KG0CJN"
]