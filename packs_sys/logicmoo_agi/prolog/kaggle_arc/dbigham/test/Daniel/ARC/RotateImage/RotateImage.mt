(*
    Tests for: Daniel`ARC`RotateImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`RotateImage]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`RotateImage[{{1, 0}, {0, 1}}, 90]
    ]
    ,
    {{0, 1}, {1, 0}}
    ,
    TestID -> "RotateImage-20220722-Q3VMGV"
]

Test[
    Daniel`ARC`RotateImage[{{1, 0}, {0, 1}}, 180]
    ,
    {{1, 0}, {0, 1}}
    ,
    TestID -> "RotateImage-20220722-DURFWR"
]

Test[
    Daniel`ARC`RotateImage[{{1, 0}, {0, 1}}, -90]
    ,
    {{0, 1}, {1, 0}}
    ,
    TestID -> "RotateImage-20220816-R3Y2FL"
]