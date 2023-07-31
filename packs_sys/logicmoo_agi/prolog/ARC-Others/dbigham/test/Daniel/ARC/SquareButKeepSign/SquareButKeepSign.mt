(*
    Tests for: Daniel`ARC`SquareButKeepSign
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`SquareButKeepSign]
    
    Author: danielb
*)

Test[
    Daniel`ARC`SquareButKeepSign[0.5]
    ,
    0.25
    ,
    TestID -> "SquareButKeepSign-20220826-EU1Q6M"
]

Test[
    Daniel`ARC`SquareButKeepSign[-0.5]
    ,
    -0.25
    ,
    TestID -> "SquareButKeepSign-20220826-1PXGNW"
]