(*
    Tests for: Daniel`ARC`ToXY
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ToXY]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[Daniel`ARC`ToXY[{1, 2}]]
    ,
    <|"Y" -> 1, "X" -> 2|>
    ,
    TestID -> "ToXY-20220724-2GTJ08"
]

Test[
    Daniel`ARC`ToXY[{1, 0}, "RemoveZeroes" -> True]
    ,
    <|"Y" -> 1|>
    ,
    TestID -> "ToXY-20220826-CC1TI0"
]

Test[
    Daniel`ARC`ToXY[{0, 1}, "RemoveZeroes" -> True]
    ,
    <|"X" -> 1|>
    ,
    TestID -> "ToXY-20220826-X9A0YD"
]

Test[
    Daniel`ARC`ToXY[{0, 0}, "RemoveZeroes" -> True]
    ,
    <||>
    ,
    TestID -> "ToXY-20220826-58XO4V"
]