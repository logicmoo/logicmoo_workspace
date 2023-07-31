(*
    Tests for: Daniel`ARC`ARCInferObjectProperties
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferObjectProperties]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferObjectProperties[
            <|
                "Image" -> Daniel`ARC`ARCScene[{{1, -1, -1}, {-1, 1, -1}, {-1, -1, 1}}],
                "Position" -> {1, 1},
                "Colors" -> {1}
            |>,
            10,
            10
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1, -1, -1}, {-1, 1, -1}, {-1, -1, 1}}],
        "Position" -> {1, 1},
        "Colors" -> {1},
        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10, -1, -1}, {-1, 10, -1}, {-1, -1, 10}}],
        "Color" -> 1,
        "ColorCount" -> 1,
        "MostUsedColor" -> 1,
        "Y" -> 1,
        "X" -> 1,
        "YInverse" -> 10,
        "XInverse" -> 10,
        "Y2" -> 3,
        "X2" -> 3,
        "Y2Inverse" -> 8,
        "X2Inverse" -> 8,
        "ZOrder" -> 0,
        "YMiddle" -> 2,
        "XMiddle" -> 2,
        "Width" -> 3,
        "Height" -> 3,
        "Length" -> 3,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 9,
        "FilledArea" -> 2,
        "FilledProportion" -> 0.2222222222222222,
        "SurfacePixelCount" -> 3,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCInferObjectProperties-20220725-4TUEPU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCInferObjectProperties[
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{1, 1}, {1, 1}}],
                    "Position" -> {1, 1},
                    "Colors" -> {1}
                |>,
                10,
                10
            ]
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1, 1}, {1, 1}}],
        "Position" -> {1, 1},
        "Colors" -> {1},
        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
        "Color" -> 1,
        "ColorCount" -> 1,
        "MostUsedColor" -> 1,
        "Y" -> 1,
        "X" -> 1,
        "YInverse" -> 10,
        "XInverse" -> 10,
        "Y2" -> 2,
        "X2" -> 2,
        "Y2Inverse" -> 9,
        "X2Inverse" -> 9,
        "ZOrder" -> 0,
        "Width" -> 2,
        "Height" -> 2,
        "Length" -> 2,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 4,
        "FilledArea" -> 2,
        "FilledProportion" -> 0.5,
        "SurfacePixelCount" -> 4,
        "VerticalLineSymmetry" -> True,
        "HorizontalLineSymmetry" -> True,
        "VerticalAndHorizontalLineSymmetry" -> True,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCInferObjectProperties-20220828-MJXIOU"
]

Test[
    Daniel`ARC`ARCInferObjectProperties[
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 1, 1}, {1, -1, 1}, {1, 1, 1}}],
            "Position" -> {1, 1},
            "Colors" -> {1}
        |>,
        10,
        10
    ]["HollowCount"]
    ,
    1
    ,
    TestID -> "ARCInferObjectProperties-20220903-BJ1CT1"
]

Test[
    Daniel`ARC`ARCInferObjectProperties[
        <|
            "Image" -> Daniel`ARC`ARCScene[{{1, 2}, {1, 1}}],
            "Position" -> {1, 1},
            "Colors" -> {1}
        |>,
        10,
        10
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{1, 2}, {1, 1}}],
        "Position" -> {1, 1},
        "Colors" -> {1},
        "MonochromeImage" -> Daniel`ARC`ARCScene[{{10, 10}, {10, 10}}],
        "Color" -> 1,
        "ColorCount" -> 1,
        "MostUsedColor" -> 1,
        "SecondMostUsedColor" -> 2,
        "Y" -> 1,
        "X" -> 1,
        "YInverse" -> 10,
        "XInverse" -> 10,
        "Y2" -> 2,
        "X2" -> 2,
        "Y2Inverse" -> 9,
        "X2Inverse" -> 9,
        "ZOrder" -> 0,
        "Width" -> 2,
        "Height" -> 2,
        "Length" -> 2,
        "PrimarySizeDimension" -> "None",
        "AspectRatio" -> 1,
        "Area" -> 4,
        "FilledArea" -> 2,
        "FilledProportion" -> 0.5,
        "SurfacePixelCount" -> 4,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCInferObjectProperties-20220924-T004CA"
]

Test[
    KeyTake[
        Daniel`ARC`ARCInferObjectProperties[
            <|"Image" -> Daniel`ARC`ARCScene[{{1}}], "Position" -> {1, 1}, "Colors" -> {1}|>,
            10,
            10
        ],
        {
            "VerticalLineSymmetry",
            "HorizontalLineSymmetry",
            "VerticalAndHorizontalLineSymmetry"
        }
    ]
    ,
    <|
        "VerticalLineSymmetry" -> True,
        "HorizontalLineSymmetry" -> True,
        "VerticalAndHorizontalLineSymmetry" -> True
    |>
    ,
    TestID -> "ARCInferObjectProperties-20221001-ZT4LAJ"
]