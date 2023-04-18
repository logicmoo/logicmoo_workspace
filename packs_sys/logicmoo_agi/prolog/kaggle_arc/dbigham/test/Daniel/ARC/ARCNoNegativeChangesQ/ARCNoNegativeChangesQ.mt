(*
    Tests for: Daniel`ARC`ARCNoNegativeChangesQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCNoNegativeChangesQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCNoNegativeChangesQ[{{1, 1}}, {{1, 2}}, {{2, 2}}]
    ]
    ,
    {{1, 2}}
    ,
    TestID -> "ARCNoNegativeChangesQ-20221007-48UA29"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCNoNegativeChangesQ[{{1, 1}}, {{1, 1}}, {{2, 2}}]
    ]
    ,
    {}
    ,
    TestID -> "ARCNoNegativeChangesQ-20221007-WTKJX3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCNoNegativeChangesQ[{{1, 1}}, {{2, 2}}, {{2, 2}}]
    ]
    ,
    {{1, 1}, {1, 2}}
    ,
    TestID -> "ARCNoNegativeChangesQ-20221007-NOWH71"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCNoNegativeChangesQ[{{2, 1}}, {{1, 2}}, {{2, 2}}]
    ]
    ,
    False
    ,
    TestID -> "ARCNoNegativeChangesQ-20221007-LRWSK3"
]