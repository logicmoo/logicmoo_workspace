(*
    Tests for: Daniel`ARC`ToPosition
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ToPosition]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ToPosition[<|"Y" -> 1, "X" -> 2|>]
    ,
    {1, 2}
    ,
    TestID -> "ToPosition-20220807-9VPF3B"
]