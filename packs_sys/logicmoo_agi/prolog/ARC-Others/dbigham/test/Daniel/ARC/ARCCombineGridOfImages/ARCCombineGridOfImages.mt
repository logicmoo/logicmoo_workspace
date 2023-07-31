(*
    Tests for: Daniel`ARC`ARCCombineGridOfImages
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCombineGridOfImages]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCCombineGridOfImages[
        {{{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}, {{{9, 10}, {11, 12}}, {{13, 14}, {15, 16}}}}
    ]
    ,
    {{1, 2, 5, 6}, {3, 4, 7, 8}, {9, 10, 13, 14}, {11, 12, 15, 16}}
    ,
    TestID -> "ARCCombineGridOfImages-20220907-LG0HTY"
]

Test[
    Daniel`ARC`ARCCombineGridOfImages[
        {
            {Daniel`ARC`ARCScene[{{1, 2}, {3, 4}}], Daniel`ARC`ARCScene[{{5, 6}, {7, 8}}]},
            {
                Daniel`ARC`ARCScene[{{9, 10}, {11, 12}}],
                Daniel`ARC`ARCScene[{{13, 14}, {15, 16}}]
            }
        }
    ]
    ,
    {{1, 2, 5, 6}, {3, 4, 7, 8}, {9, 10, 13, 14}, {11, 12, 15, 16}}
    ,
    TestID -> "ARCCombineGridOfImages-20220907-43TW2U"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCombineGridOfImages[
            {
                {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}},
                {{{9, 10}, {11, 12}}, {{13, 14}, {15, 16}}}
            },
            "GridColor" -> 9
        ]
    ]
    ,
    {
        {1, 2, 9, 5, 6},
        {3, 4, 9, 7, 8},
        {9, 9, 9, 9, 9},
        {9, 10, 9, 13, 14},
        {11, 12, 9, 15, 16}
    }
    ,
    TestID -> "ARCCombineGridOfImages-20220910-FXNGLP"
]