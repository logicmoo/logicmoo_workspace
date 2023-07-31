(*
    Tests for: Daniel`ARC`ARCPrunePattern
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPrunePattern]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCPrunePattern[<|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Width" -> 1|>]
    ,
    <|"Image" -> Daniel`ARC`ARCScene[{{1}}]|>
    ,
    TestID -> "ARCPrunePattern-20220827-DDZO7R"
]

Test[
    Daniel`ARC`ARCPrunePattern[<|"Y" -> 1, "Y.InverseRank" -> 1|>]
    ,
    <|"Y" -> 1|>
    ,
    TestID -> "ARCPrunePattern-20220827-6KE83U"
]