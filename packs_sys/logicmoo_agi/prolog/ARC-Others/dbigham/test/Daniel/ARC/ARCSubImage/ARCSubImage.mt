(*
    Tests for: Daniel`ARC`ARCSubImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSubImage]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSubImage[{{1, 2}, {3, 4}}, -1, -1, 4, 4]
    ]
    ,
    {
        {-2, -2, -2, -2, -2, -2},
        {-2, -2, -2, -2, -2, -2},
        {-2, -2, 1, 2, -2, -2},
        {-2, -2, 3, 4, -2, -2},
        {-2, -2, -2, -2, -2, -2},
        {-2, -2, -2, -2, -2, -2}
    }
    ,
    TestID -> "ARCSubImage-20220929-5WCHUO"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCSubImage[{{1, 2}, {3, 4}}, 1, 1, 2, 2]
    ]
    ,
    {{1, 2}, {3, 4}}
    ,
    TestID -> "ARCSubImage-20220929-6EBSTL"
]

Test[
    Daniel`ARC`ARCSubImage[{{1, 2}, {3, 4}}, 2, 0, 2, 3]
    ,
    {{-2, 3, 4, -2}}
    ,
    TestID -> "ARCSubImage-20220929-ZXMTY0"
]

Test[
    Daniel`ARC`ARCSubImage[{{1, 2}, {4, 5}}, 1, 2, 2, 3]
    ,
    {{2, -2}, {5, -2}}
    ,
    TestID -> "ARCSubImage-20221008-XNOKT1"
]

Test[
    Daniel`ARC`ARCSubImage[{{1, 2}, {3, 4}}, 10, 10, 11, 11]
    ,
    {{-2, -2}, {-2, -2}}
    ,
    TestID -> "ARCSubImage-20221009-SCK9ZE"
]