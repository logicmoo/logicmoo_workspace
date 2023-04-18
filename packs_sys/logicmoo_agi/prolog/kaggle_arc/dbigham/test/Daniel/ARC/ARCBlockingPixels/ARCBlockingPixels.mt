(*
    Tests for: Daniel`ARC`ARCBlockingPixels
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCBlockingPixels]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCBlockingPixels[
            {{10, 1}, {10, 2}, {10, 4}},
            {1, 0},
            Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 2, 2, 2, 0, 0, 0, 0, 0},
                    {2, 2, 0, 2, 0, 0, 0, 0, 0},
                    {0, 0, 0, 8, 8, 0, 0, 0, 0},
                    {0, 0, 0, 8, 8, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0, 0, 0}
                }
            ],
            0
        ]
    ]
    ,
    {{11, 4}}
    ,
    TestID -> "ARCBlockingPixels-20220803-JYTDA0"
]

Test[
    Daniel`ARC`ARCBlockingPixels[
        <|
            "PixelPositions" -> {{9, 2}, {9, 3}, {9, 4}, {10, 1}, {10, 2}, {10, 4}},
            "Y" -> 9,
            "X" -> 1,
            "Width" -> 4,
            "Height" -> 2
        |>,
        {1, 0},
        Daniel`ARC`ARCScene[
            {
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 2, 2, 2, 0, 0, 0, 0, 0},
                {2, 2, 0, 2, 0, 0, 0, 0, 0},
                {0, 0, 0, 8, 8, 0, 0, 0, 0},
                {0, 0, 0, 8, 8, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0}
            }
        ],
        0
    ]
    ,
    {{11, 4}}
    ,
    TestID -> "ARCBlockingPixels-20220803-RRP039"
]