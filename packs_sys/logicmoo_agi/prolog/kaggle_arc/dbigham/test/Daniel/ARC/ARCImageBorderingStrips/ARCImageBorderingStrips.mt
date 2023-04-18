(*
    Tests for: Daniel`ARC`ARCImageBorderingStrips
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCImageBorderingStrips]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageBorderingStrips[
            {{1, 2, 3, 4}, {12, 1, 1, 5}, {11, 1, 1, 6}, {10, 9, 8, 7}},
            {2, 2},
            {2, 2},
            0
        ]
    ]
    ,
    {{2, 3}, {5, 6}, {9, 8}, {12, 11}}
    ,
    TestID -> "ARCImageBorderingStrips-20220725-SWWX1L"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageBorderingStrips[
            {{1, 2, 3, 4}, {12, 1, 1, 5}, {11, 1, 1, 6}, {10, 9, 8, 7}},
            {1, 1},
            {2, 2},
            0
        ]
    ]
    ,
    {{0, 0}, {3, 1}, {11, 1}, {0, 0}}
    ,
    TestID -> "ARCImageBorderingStrips-20220725-PVZFEG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCImageBorderingStrips[
            {{1, 2, 3, 4}, {12, 1, 1, 5}, {11, 1, 1, 6}, {10, 9, 8, 7}},
            {3, 3},
            {2, 2},
            -1
        ]
    ]
    ,
    {{1, 5}, {-1, -1}, {-1, -1}, {1, 9}}
    ,
    TestID -> "ARCImageBorderingStrips-20220725-5IRM5R"
]