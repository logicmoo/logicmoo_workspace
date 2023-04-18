(*
    Tests for: Daniel`ARC`ARCFormCompositeObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFormCompositeObject]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCFormCompositeObject[
                Daniel`ARC`ARCScene[
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
                ],
                <|
                    "UUID" -> "89a9707c-edbe-4bf9-b0da-2fee6ba9b163",
                    "Image" -> Daniel`ARC`ARCScene[{{10, -1, 10}, {-1, 10, -1}, {10, -1, 10}}],
                    "PixelPositions" -> {{3, 2}, {3, 4}, {4, 3}, {5, 2}, {5, 4}},
                    "Colors" -> {10},
                    "Width" -> 3,
                    "Height" -> 3,
                    "Position" -> {3, 2},
                    "Y" -> 3,
                    "X" -> 2,
                    "AspectRatio" -> 1,
                    "Area" -> 5
                |>,
                {
                    <|
                        "UUID" -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{3, 2}},
                        "Shape" -> {<|"Name" -> "Pixel"|>},
                        "Colors" -> {4},
                        "Width" -> 1,
                        "Height" -> 1,
                        "Position" -> {3, 2},
                        "Y" -> 3,
                        "X" -> 2,
                        "AspectRatio" -> 1,
                        "Area" -> 1
                    |>,
                    <|
                        "UUID" -> "334e881a-c81d-4f55-b381-1ea1090a5ffc",
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{3, 4}},
                        "Shape" -> {<|"Name" -> "Pixel"|>},
                        "Colors" -> {4},
                        "Width" -> 1,
                        "Height" -> 1,
                        "Position" -> {3, 4},
                        "Y" -> 3,
                        "X" -> 4,
                        "AspectRatio" -> 1,
                        "Area" -> 1
                    |>,
                    <|
                        "UUID" -> "6a8b514b-f10a-499b-8bca-fa4bd26d3093",
                        "Image" -> Daniel`ARC`ARCScene[{{2}}],
                        "PixelPositions" -> {{4, 3}},
                        "Shape" -> {<|"Name" -> "Pixel"|>},
                        "Colors" -> {2},
                        "Width" -> 1,
                        "Height" -> 1,
                        "Position" -> {4, 3},
                        "Y" -> 4,
                        "X" -> 3,
                        "AspectRatio" -> 1,
                        "Area" -> 1
                    |>,
                    <|
                        "UUID" -> "b7049403-c19e-41e3-8edb-84ef0649a82b",
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{5, 2}},
                        "Shape" -> {<|"Name" -> "Pixel"|>},
                        "Colors" -> {4},
                        "Width" -> 1,
                        "Height" -> 1,
                        "Position" -> {5, 2},
                        "Y" -> 5,
                        "X" -> 2,
                        "AspectRatio" -> 1,
                        "Area" -> 1
                    |>,
                    <|
                        "UUID" -> "6f024852-92f6-44db-978a-0716313e5b74",
                        "Image" -> Daniel`ARC`ARCScene[{{4}}],
                        "PixelPositions" -> {{5, 4}},
                        "Shape" -> {<|"Name" -> "Pixel"|>},
                        "Colors" -> {4},
                        "Width" -> 1,
                        "Height" -> 1,
                        "Position" -> {5, 4},
                        "Y" -> 5,
                        "X" -> 4,
                        "AspectRatio" -> 1,
                        "Area" -> 1
                    |>
                }
            ]
        ]
    ]
    ,
    <|
        "UUID" -> "89a9707c-edbe-4bf9-b0da-2fee6ba9b163",
        "Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
        "PixelPositions" -> {{3, 2}, {3, 4}, {4, 3}, {5, 2}, {5, 4}},
        "Colors" -> {2, 4},
        "Width" -> 3,
        "Height" -> 3,
        "Position" -> {3, 2},
        "Y" -> 3,
        "X" -> 2,
        "AspectRatio" -> 1,
        "Area" -> 9,
        "Shape" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}],
        "Shapes" -> {
            <|"Image" -> Daniel`ARC`ARCScene[{{4, -1, 4}, {-1, 2, -1}, {4, -1, 4}}]|>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {4, 4, -1, -1, 4, 4},
                        {4, 4, -1, -1, 4, 4},
                        {-1, -1, 2, 2, -1, -1},
                        {-1, -1, 2, 2, -1, -1},
                        {4, 4, -1, -1, 4, 4},
                        {4, 4, -1, -1, 4, 4}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
            |>,
            <|
                "Image" -> Daniel`ARC`ARCScene[
                    {
                        {4, 4, 4, -1, -1, -1, 4, 4, 4},
                        {4, 4, 4, -1, -1, -1, 4, 4, 4},
                        {4, 4, 4, -1, -1, -1, 4, 4, 4},
                        {-1, -1, -1, 2, 2, 2, -1, -1, -1},
                        {-1, -1, -1, 2, 2, 2, -1, -1, -1},
                        {-1, -1, -1, 2, 2, 2, -1, -1, -1},
                        {4, 4, 4, -1, -1, -1, 4, 4, 4},
                        {4, 4, 4, -1, -1, -1, 4, 4, 4},
                        {4, 4, 4, -1, -1, -1, 4, 4, 4}
                    }
                ],
                "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
            |>
        },
        "Components" -> {
            <|
                "UUID" -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{3, 2}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Colors" -> {4},
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {3, 2},
                "Y" -> 3,
                "X" -> 2,
                "AspectRatio" -> 1,
                "Area" -> 1,
                "YRelative" -> 0,
                "XRelative" -> 0
            |>,
            <|
                "UUID" -> "334e881a-c81d-4f55-b381-1ea1090a5ffc",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{3, 4}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Colors" -> {4},
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {3, 4},
                "Y" -> 3,
                "X" -> 4,
                "AspectRatio" -> 1,
                "Area" -> 1,
                "YRelative" -> 0,
                "XRelative" -> 2
            |>,
            <|
                "UUID" -> "6a8b514b-f10a-499b-8bca-fa4bd26d3093",
                "Image" -> Daniel`ARC`ARCScene[{{2}}],
                "PixelPositions" -> {{4, 3}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Colors" -> {2},
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {4, 3},
                "Y" -> 4,
                "X" -> 3,
                "AspectRatio" -> 1,
                "Area" -> 1,
                "YRelative" -> 1,
                "XRelative" -> 1
            |>,
            <|
                "UUID" -> "b7049403-c19e-41e3-8edb-84ef0649a82b",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{5, 2}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Colors" -> {4},
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {5, 2},
                "Y" -> 5,
                "X" -> 2,
                "AspectRatio" -> 1,
                "Area" -> 1,
                "YRelative" -> 2,
                "XRelative" -> 0
            |>,
            <|
                "UUID" -> "6f024852-92f6-44db-978a-0716313e5b74",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{5, 4}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Colors" -> {4},
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {5, 4},
                "Y" -> 5,
                "X" -> 4,
                "AspectRatio" -> 1,
                "Area" -> 1,
                "YRelative" -> 2,
                "XRelative" -> 2
            |>
        },
        "ColorCount" -> 2,
        "YInverse" -> 7,
        "XInverse" -> 8,
        "Y2" -> 5,
        "X2" -> 4,
        "Y2Inverse" -> 5,
        "X2Inverse" -> 6,
        "ZOrder" -> 0,
        "YMiddle" -> 4,
        "XMiddle" -> 3,
        "Length" -> 3,
        "PrimarySizeDimension" -> "None",
        "FilledArea" -> 5,
        "FilledProportion" -> 0.5555555555555556,
        "SurfacePixelCount" -> 5,
        "VerticalLineSymmetry" -> False,
        "HorizontalLineSymmetry" -> False,
        "VerticalAndHorizontalLineSymmetry" -> False,
        "HollowCount" -> 0
    |>
    ,
    TestID -> "ARCFormCompositeObject-20220827-8TL849"
]