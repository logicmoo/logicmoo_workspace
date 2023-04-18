(*
    Tests for: Daniel`ARC`ARCAddComponentsTransform
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCAddComponentsTransform]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`SimplifyObjects[
            Daniel`ARC`ARCAddComponentsTransform[
                <|"Position" -> {2, 2}, "Y2" -> 4, "X2" -> 4|>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{2, -1, -1}, {5, 5, 5}, {5, -1, 5}, {5, 5, 5}}
                    ],
                    "Position" -> {1, 2},
                    "Components" -> {
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{2}}],
                            "Position" -> {1, 2},
                            "Shape" -> "SHAPE",
                            "Shapes" -> "SHAPES",
                            "Colors" -> {"COLOR"},
                            "Y2" -> 1,
                            "X2" -> 2,
                            "Width" -> 3,
                            "Height" -> 3
                        |>,
                        <|
                            "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, -1, 5}, {5, 5, 5}}],
                            "Position" -> {2, 2},
                            "Y2" -> 4,
                            "X2" -> 4,
                            "Width" -> 3,
                            "Height" -> 3
                        |>
                    }
                |>,
                {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, -1, 5}, {5, 5, 5}}],
                        "Position" -> {2, 2}
                    |>
                },
                <||>
            ]
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{2, -1, -1}, {5, 5, 5}, {5, -1, 5}, {5, 5, 5}}],
        "Position" -> {1, 2},
        "Transform" -> <|
            "Type" -> "AddComponents",
            "Components" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{2}}],
                    "Position" -> <|
                        "RelativePosition" -> <|
                            "Y" -> -1,
                            "X" -> 0,
                            "YInverse" -> -3,
                            "XInverse" -> -2
                        |>
                    |>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{5, 5, 5}, {5, -1, 5}, {5, 5, 5}}],
                    "Position" -> <|
                        "RelativePosition" -> <|
                            "Y" -> 0,
                            "X" -> 0,
                            "YInverse" -> -2,
                            "XInverse" -> -2
                        |>
                    |>
                |>
            }
        |>
    |>
    ,
    TestID -> "ARCAddComponentsTransform-20220807-6CUFJO"
]