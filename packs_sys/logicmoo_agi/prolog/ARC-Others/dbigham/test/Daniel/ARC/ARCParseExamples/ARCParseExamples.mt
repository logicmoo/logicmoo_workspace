(*
    Tests for: Daniel`ARC`ARCParseExamples
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCParseExamples]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`SimplifyObjects[
                ERPTesting`NormalizeOutput[
                    Daniel`ARC`ARCParseExamples[file = "0ca9ddb6"][[1 ;; 1]]
                ]
            ]
        ]
    ]
    ,
    {
        <|
            "Input" -> <|
                "Background" -> 0,
                "Width" -> 9,
                "Height" -> 9,
                "ObjectCount" -> 2,
                "Objects" -> {
                    <|"Image" -> Daniel`ARC`ARCScene[{{2}}], "Position" -> {4, 3}|>,
                    <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {7, 7}|>
                },
                "Scene" -> Daniel`ARC`ARCScene[
                    {
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 2, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 1, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0}
                    }
                ]
            |>,
            "Output" -> <|
                "Background" -> 0,
                "Width" -> 9,
                "Height" -> 9,
                "ObjectCount" -> 2,
                "Objects" -> {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
                        "Position" -> {3, 2}
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{-1, 7, -1}, {7, 1, 7}, {-1, 7, -1}}],
                        "Position" -> {6, 6}
                    |>
                },
                "Scene" -> Daniel`ARC`ARCScene[
                    {
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 4, 0, 4, 0, 0, 0, 0, 0},
                        {0, 0, 2, 0, 0, 0, 0, 0, 0},
                        {0, 4, 0, 4, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 7, 0, 0},
                        {0, 0, 0, 0, 0, 7, 1, 7, 0},
                        {0, 0, 0, 0, 0, 0, 7, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0}
                    }
                ]
            |>
        |>
    }
    ,
    TestID -> "ARCParseExamples-20220723-UDW1J2"
]