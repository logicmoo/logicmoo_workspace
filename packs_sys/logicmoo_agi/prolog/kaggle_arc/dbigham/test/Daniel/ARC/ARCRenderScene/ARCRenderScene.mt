(*
    Tests for: Daniel`ARC`ARCRenderScene
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRenderScene]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRenderScene[
            <|
                "Background" -> "Black",
                "Width" -> 9,
                "Height" -> 9,
                "Objects" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{8}}], "Position" -> {1, 4}|>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
                        "Position" -> {2, 6}
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
                        "Position" -> {3, 2}
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
                        "Position" -> {6, 6}
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
                        "Position" -> {7, 1}
                    |>
                }
            |>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {0, 0, 0, 8, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 4, 0, 4, 0},
            {0, 0, 7, 0, 0, 0, 2, 0, 0},
            {0, 7, 1, 7, 0, 4, 0, 4, 0},
            {0, 0, 7, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 7, 0, 0},
            {4, 0, 4, 0, 0, 7, 1, 7, 0},
            {0, 2, 0, 0, 0, 0, 7, 0, 0},
            {4, 0, 4, 0, 0, 0, 0, 0, 0}
        }
    ]
    ,
    TestID -> "ARCRenderScene-20220722-GLEI69"
]

Test[
    Daniel`ARC`ARCRenderScene[
        Daniel`ARC`ARCParseScene[Daniel`ARC`ARCParseFile["178fcbfb"]["Train", 1, "Output"]]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {0, 0, 2, 0, 0, 0, 0, 0, 0},
            {0, 0, 2, 0, 0, 0, 0, 0, 0},
            {0, 0, 2, 0, 0, 0, 0, 0, 0},
            {0, 0, 2, 0, 0, 0, 0, 0, 0},
            {3, 3, 3, 3, 3, 3, 3, 3, 3},
            {0, 0, 2, 0, 0, 0, 0, 0, 0},
            {1, 1, 1, 1, 1, 1, 1, 1, 1},
            {0, 0, 2, 0, 0, 0, 0, 0, 0},
            {0, 0, 2, 0, 0, 0, 0, 0, 0}
        }
    ]
    ,
    TestID -> "ARCRenderScene-20220827-7Z6CZI"
]