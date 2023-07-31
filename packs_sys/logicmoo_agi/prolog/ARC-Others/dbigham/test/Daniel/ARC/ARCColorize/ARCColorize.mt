(*
    Tests for: Daniel`ARC`ARCColorize
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCColorize]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCColorize[Daniel`ARC`ARCScene[{{1, -1}, {-1, 1}}], 2]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, -1}, {-1, 2}}]
    ,
    TestID -> "ARCColorize-20220725-QRNPB0"
]