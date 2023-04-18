(*
    Tests for: Daniel`ARC`SqrtButKeepSign
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`SqrtButKeepSign]
    
    Author: danielb
*)

Test[
    Daniel`ARC`SqrtButKeepSign[9]
    ,
    3
    ,
    TestID -> "SqrtButKeepSign-20220826-R4ERP9"
]

Test[
    Daniel`ARC`SqrtButKeepSign[-9]
    ,
    -3
    ,
    TestID -> "SqrtButKeepSign-20220826-NCX238"
]