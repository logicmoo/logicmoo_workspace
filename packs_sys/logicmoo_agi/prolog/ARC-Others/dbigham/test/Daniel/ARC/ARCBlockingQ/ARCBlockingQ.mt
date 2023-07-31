(*
    Tests for: Daniel`ARC`ARCBlockingQ
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCBlockingQ]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCBlockingQ[
            <|
                "UUID" -> "69acc736-f8f9-48cc-924a-3c41d50517c0",
                "Image" -> Daniel`ARC`ARCScene[{{0, 2, 2}, {0, 2, 2}, {2, 2, 2}, {0, 2, 2}}],
                "PixelPositions" -> {
                    {2, 2},
                    {2, 3},
                    {3, 2},
                    {3, 3},
                    {4, 1},
                    {4, 2},
                    {4, 3},
                    {5, 2},
                    {5, 3}
                },
                "Colors" -> {2},
                "Position" -> {2, 1},
                "Width" -> 3,
                "Height" -> 4,
                "Y" -> 2,
                "X" -> 1
            |>,
            <|
                "UUID" -> "33115ed2-f17e-428e-a707-55002e8a8fa9",
                "Image" -> Daniel`ARC`ARCScene[{{8, 8}, {8, 8}}],
                "PixelPositions" -> {{5, 7}, {5, 8}, {6, 7}, {6, 8}},
                "Colors" -> {8},
                "Position" -> {5, 7},
                "Width" -> 2,
                "Height" -> 2,
                "Y" -> 5,
                "X" -> 7
            |>,
            {0, 1},
            <|
                "Background" -> "Black",
                "Width" -> 10,
                "Height" -> 9,
                "Scene" -> Daniel`ARC`ARCScene[
                    {
                        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 2, 2, 0, 0, 0, 0, 0, 0, 0},
                        {0, 2, 2, 0, 0, 0, 0, 0, 0, 0},
                        {2, 2, 2, 0, 0, 0, 0, 0, 0, 0},
                        {0, 2, 2, 0, 0, 0, 8, 8, 0, 0},
                        {0, 0, 0, 0, 0, 0, 8, 8, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
                    }
                ]
            |>
        ]
    ]
    ,
    {2, 4}
    ,
    TestID -> "ARCBlockingQ-20220804-HZHVFH"
]