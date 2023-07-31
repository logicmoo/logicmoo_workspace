(*
    Tests for: Daniel`ARC`ARCFindPropertyToInferImageValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindPropertyToInferImageValues]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Module[
            {image},
            Daniel`ARC`ARCFindPropertyToInferImageValues[
                {"Image"},
                {
                    <|
                        "Image" -> Daniel`ARC`ARCScene[image = {{1, 1}, {-1, -1}}],
                        "Colors" -> {1},
                        Daniel`ARC`ARCInferShapeAndShapes[image, {1}]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[image = {{-1, 1}, {-1, 1}}],
                        "Colors" -> {1},
                        Daniel`ARC`ARCInferShapeAndShapes[image, {1}]
                    |>,
                    <|
                        "Image" -> Daniel`ARC`ARCScene[image = {{1, 1}, {1, 1}}],
                        "Colors" -> {1},
                        Daniel`ARC`ARCInferShapeAndShapes[image, {1}]
                    |>
                },
                {
                    Daniel`ARC`ARCScene[{{1, -1}, {1, -1}}],
                    Daniel`ARC`ARCScene[{{1, 1}, {-1, -1}}],
                    Daniel`ARC`ARCScene[{{1, 1}, {1, 1}}]
                }
            ]
        ]
    ]
    ,
    Inactive[Daniel`ARC`Transform][
        Daniel`ARC`ObjectValue[Daniel`ARC`Private`TODO, "Image"],
        <|"Type" -> "Rotation", "Angle" -> 270|>
    ]
    ,
    TestID -> "ARCFindPropertyToInferImageValues-20220912-1LYXB5"
]

Test[
    Module[
        {image},
        Daniel`ARC`ARCFindPropertyToInferImageValues[
            {"Shape"},
            {
                <|
                    "Image" -> Daniel`ARC`ARCScene[image = {{1, 1}, {-1, -1}}],
                    "Colors" -> {1},
                    Daniel`ARC`ARCInferShapeAndShapes[image, {1}]
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[image = {{-1, 1}, {-1, 1}}],
                    "Colors" -> {1},
                    Daniel`ARC`ARCInferShapeAndShapes[image, {1}]
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[image = {{1, 1}, {1, 1}}],
                    "Colors" -> {1},
                    Daniel`ARC`ARCInferShapeAndShapes[image, {1}]
                |>
            },
            {
                Daniel`ARC`ARCScene[{{10, -1}, {10, -1}}],
                Daniel`ARC`ARCScene[{{10, 10}, {-1, -1}}],
                Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}]
            }
        ]
    ]
    ,
    Inactive[Daniel`ARC`Transform][
        Daniel`ARC`ObjectValue[Daniel`ARC`Private`TODO, "Shape"],
        <|"Type" -> "Rotation", "Angle" -> 270|>
    ]
    ,
    TestID -> "ARCFindPropertyToInferImageValues-20220912-R6LG0J"
]