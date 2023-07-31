(*
    Tests for: Daniel`ARC`ARCRepositionObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRepositionObject]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            KeyTake[
                Daniel`ARC`ARCRepositionObject[
                    Daniel`ARC`ARCInferObjectProperties[
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{1, -1}, {-1, 1}}],
                            "PixelPositions" -> {{1, 1}, {2, 2}},
                            "Position" -> {1, 1},
                            "Components" -> {
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                                    "PixelPositions" -> {{1, 1}},
                                    "Position" -> {1, 1}
                                |>,
                                <|
                                    "Image" -> Daniel`ARC`ARCScene[{{1}}],
                                    "PixelPositions" -> {{2, 2}},
                                    "Position" -> {2, 2}
                                |>
                            }
                        |>,
                        10,
                        10
                    ],
                    {5, 5},
                    10,
                    10
                ],
                {"Image", "PixelPositions", "X", "Y", "YInverse", "XInverse"}
            ]
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1, -1}, {-1, 1}}],
        "PixelPositions" -> {{5, 5}, {6, 6}},
        "X" -> 5,
        "Y" -> 5,
        "YInverse" -> 6,
        "XInverse" -> 6
    |>
    ,
    TestID -> "ARCRepositionObject-20220725-ZLXXQD"
]