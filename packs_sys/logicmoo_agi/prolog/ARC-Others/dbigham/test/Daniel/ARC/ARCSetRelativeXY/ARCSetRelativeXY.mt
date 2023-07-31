(*
    Tests for: Daniel`ARC`ARCSetRelativeXY
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSetRelativeXY]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSetRelativeXY[<|"Y" -> 9, "X" -> 4|>, <|"Y" -> 2, "X" -> 3|>]
    ]
    ,
    <|"Y" -> 9, "X" -> 4, "YRelative" -> 7, "XRelative" -> 1|>
    ,
    TestID -> "ARCSetRelativeXY-20220817-7PCR2M"
]