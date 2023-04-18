(*
    Tests for: Daniel`ARC`ARCCombineRowOfImages
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCombineRowOfImages]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCCombineRowOfImages[{{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}]
    ,
    {{1, 2, 5, 6}, {3, 4, 7, 8}}
    ,
    TestID -> "ARCCombineRowOfImages-20220907-9MRTVH"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCombineRowOfImages[
            {Daniel`ARC`ARCScene[{{1, 2}, {3, 4}}], Daniel`ARC`ARCScene[{{5, 6}, {7, 8}}]}
        ]
    ]
    ,
    {{1, 2, 5, 6}, {3, 4, 7, 8}}
    ,
    TestID -> "ARCCombineRowOfImages-20220907-6AHPEQ"
]