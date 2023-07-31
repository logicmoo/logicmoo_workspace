(*
    Tests for: Daniel`ARC`ARCContiguousSubImageQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCContiguousSubImageQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCContiguousSubImageQ[
            {{3, -1, -1, -1}, {3, -1, -1, -1}, {3, -1, -1, -1}, {3, 3, 3, 3}},
            {{3, -1}, {3, 3}},
            {3, 1}
        ]
    ]
    ,
    True
    ,
    TestID -> "ARCContiguousSubImageQ-20220830-7GH6QD"
]

Test[
    Daniel`ARC`ARCContiguousSubImageQ[
        {{3, -1, -1, -1}, {-1, -1, -1, -1}, {3, -1, -1, -1}, {3, 3, -1, 3}},
        {{3, -1}, {3, 3}},
        {3, 1}
    ]
    ,
    False
    ,
    TestID -> "ARCContiguousSubImageQ-20220830-3B2JSX"
]

Test[
    Daniel`ARC`ARCContiguousSubImageQ[
        {{3, -1, -1, -1}, {-1, 3, -1, -1}, {3, -1, -1, -1}, {3, 3, -1, 3}},
        {{3, -1}, {3, 3}},
        {3, 1}
    ]
    ,
    False
    ,
    TestID -> "ARCContiguousSubImageQ-20220830-EQEOJ9"
]