(*
    Tests for: Daniel`ARC`ARCCheckForWrappedRepeatedPattern
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCheckForWrappedRepeatedPattern]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCheckForWrappedRepeatedPattern[
            {{1, 1}, {1, 4}, {1, 7}, {1, 10}, {4, 1}},
            3,
            3,
            12,
            12
        ]
    ]
    ,
    <|"Linear" -> True, "Wrapped" -> True, "Count" -> 5|>
    ,
    TestID -> "ARCCheckForWrappedRepeatedPattern-20221008-7BUEE8"
]

Test[
    Daniel`ARC`ARCCheckForWrappedRepeatedPattern[
        {{1, 1}, {1, 4}, {1, 7}, {1, 10}, {5, 1}},
        3,
        3,
        12,
        12
    ]
    ,
    False
    ,
    TestID -> "ARCCheckForWrappedRepeatedPattern-20221008-IUGMRT"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCheckForWrappedRepeatedPattern[
            {{1, 1}, {1, 4}, {1, 7}, {1, 10}},
            3,
            3,
            12,
            12
        ]
    ]
    ,
    <|"Linear" -> True, "Wrapped" -> False, "Count" -> 4, "FullLinear" -> True|>
    ,
    TestID -> "ARCCheckForWrappedRepeatedPattern-20221008-OQ7NPZ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCheckForWrappedRepeatedPattern[
            {{1, 1}, {1, 4}, {1, 7}, {1, 10}},
            3,
            3,
            13,
            13
        ]
    ]
    ,
    <|"Linear" -> True, "Wrapped" -> False, "Count" -> 4, "FullLinear" -> False|>
    ,
    TestID -> "ARCCheckForWrappedRepeatedPattern-20221008-T0B5LE"
]