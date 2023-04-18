(*
    Tests for: Daniel`ARC`ARCFixUnwantedManyToOneMappings
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFixUnwantedManyToOneMappings]
    
    Author: danielb
*)

Test[
    Daniel`ARC`SimplifyObjects[
        Daniel`ARC`ARCFixUnwantedManyToOneMappings[
            <|
                <|
                    "UUID" -> "a9c98ff7-bbc1-4cc5-9199-40a43cb75a03",
                    "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 4, 4}, {4, -1, 4}}]
                |> -> Missing[
                    "NotFound"
                ],
                <|
                    "UUID" -> "21332107-4c75-46e3-9ad3-925615a2e6aa",
                    "Image" -> Daniel`ARC`ARCScene[{{6, -1, 6}, {-1, 6, 6}, {6, -1, 6}}]
                |> -> <|
                    "UUID" -> "10af9e93-2f41-41d5-8bd1-bcd05f4717fa",
                    "Image" -> Daniel`ARC`ARCScene[{{8, 8, 8}, {8, -1, 8}, {8, 8, 8}}]
                |>,
                <|
                    "UUID" -> "15061388-3ba9-4752-abd3-5ae3e59673b9",
                    "Image" -> Daniel`ARC`ARCScene[{{8, 8, 8}, {8, -1, 8}, {8, 8, 8}}]
                |> -> <|
                    "UUID" -> "10af9e93-2f41-41d5-8bd1-bcd05f4717fa",
                    "Image" -> Daniel`ARC`ARCScene[{{8, 8, 8}, {8, -1, 8}, {8, 8, 8}}],
                    "Transform" -> <|
                        "Type" -> "Move",
                        "Position" -> <|"Y" -> 1, "X" -> 1|>,
                        "Offset" -> <|"Y" -> -6|>
                    |>
                |>
            |>
        ]
    ]
    ,
    <|
        <|"Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 4, 4}, {4, -1, 4}}]|> -> Missing[
            "NotFound"
        ],
        <|"Image" -> Daniel`ARC`ARCScene[{{6, -1, 6}, {-1, 6, 6}, {6, -1, 6}}]|> -> Missing[
            "NotFound"
        ],
        <|"Image" -> Daniel`ARC`ARCScene[{{8, 8, 8}, {8, -1, 8}, {8, 8, 8}}]|> -> <|
            "Image" -> Daniel`ARC`ARCScene[{{8, 8, 8}, {8, -1, 8}, {8, 8, 8}}],
            "Transform" -> <|
                "Type" -> "Move",
                "Position" -> <|"Y" -> 1, "X" -> 1|>,
                "Offset" -> <|"Y" -> -6|>
            |>
        |>
    |>
    ,
    TestID -> "ARCFixUnwantedManyToOneMappings-20220904-DI844Y"
]