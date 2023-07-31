(*
    Tests for: Daniel`ARC`ARCDrawSubImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCDrawSubImage]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCDrawSubImage[
        {{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}},
        <|"Image" -> Daniel`ARC`ARCScene[{{9}}], "Position" -> {2, 2}|>
    ]
    ,
    {{-1, -1, -1}, {-1, 9, -1}, {-1, -1, -1}}
    ,
    TestID -> "ARCDrawSubImage-20220925-C9ZJ2V"
]

Test[
    Daniel`ARC`ARCDrawSubImage[{{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}}, {{9}}, 2, 2]
    ,
    {{-1, -1, -1}, {-1, 9, -1}, {-1, -1, -1}}
    ,
    TestID -> "ARCDrawSubImage-20220929-KAGB26"
]

Test[
    Daniel`ARC`ARCDrawSubImage[
        {{-1, -1}, {-1, -1}},
        {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}, {13, 14, 15, 16}},
        0,
        0
    ]
    ,
    {{6, 7}, {10, 11}}
    ,
    TestID -> "ARCDrawSubImage-20220929-33LIGH"
]