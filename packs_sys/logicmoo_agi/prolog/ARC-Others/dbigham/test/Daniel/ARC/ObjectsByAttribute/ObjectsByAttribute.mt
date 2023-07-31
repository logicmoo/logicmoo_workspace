(*
    Tests for: Daniel`ARC`ObjectsByAttribute
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ObjectsByAttribute]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ObjectsByAttribute[
            {
                <|
                    "UUID" -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
                    "Image" -> Daniel`ARC`ARCScene[{{4}}],
                    "PixelPositions" -> {{3, 2}},
                    "Shape" -> {<|"Name" -> "Pixel"|>},
                    "Color" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Position" -> {3, 2},
                    "AspectRatio" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "UUID" -> "334e881a-c81d-4f55-b381-1ea1090a5ffc",
                    "Image" -> Daniel`ARC`ARCScene[{{4}}],
                    "PixelPositions" -> {{3, 4}},
                    "Shape" -> {<|"Name" -> "Pixel"|>},
                    "Color" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Position" -> {3, 4},
                    "AspectRatio" -> 1,
                    "Area" -> 1
                |>
            },
            "UUID"
        ]
    ]
    ,
    <|
        "c352b306-971b-4632-b5a0-a66a4c8e4b27" -> <|
            "UUID" -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
            "Image" -> Daniel`ARC`ARCScene[{{4}}],
            "PixelPositions" -> {{3, 2}},
            "Shape" -> {<|"Name" -> "Pixel"|>},
            "Color" -> 4,
            "Width" -> 1,
            "Height" -> 1,
            "Position" -> {3, 2},
            "AspectRatio" -> 1,
            "Area" -> 1
        |>,
        "334e881a-c81d-4f55-b381-1ea1090a5ffc" -> <|
            "UUID" -> "334e881a-c81d-4f55-b381-1ea1090a5ffc",
            "Image" -> Daniel`ARC`ARCScene[{{4}}],
            "PixelPositions" -> {{3, 4}},
            "Shape" -> {<|"Name" -> "Pixel"|>},
            "Color" -> 4,
            "Width" -> 1,
            "Height" -> 1,
            "Position" -> {3, 4},
            "AspectRatio" -> 1,
            "Area" -> 1
        |>
    |>
    ,
    TestID -> "ObjectsByAttribute-20220718-JMR711"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ObjectsByAttribute[
            {
                <|
                    "UUID" -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
                    "Image" -> Daniel`ARC`ARCScene[{{4}}],
                    "PixelPositions" -> {{3, 2}},
                    "Shape" -> {<|"Name" -> "Pixel"|>},
                    "Color" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Position" -> {3, 2},
                    "AspectRatio" -> 1,
                    "Area" -> 1
                |>,
                <|
                    "UUID" -> "334e881a-c81d-4f55-b381-1ea1090a5ffc",
                    "Image" -> Daniel`ARC`ARCScene[{{4}}],
                    "PixelPositions" -> {{3, 4}},
                    "Shape" -> {<|"Name" -> "Pixel"|>},
                    "Color" -> 4,
                    "Width" -> 1,
                    "Height" -> 1,
                    "Position" -> {3, 4},
                    "AspectRatio" -> 1,
                    "Area" -> 1
                |>
            },
            "Color"
        ]
    ]
    ,
    <|
        4 -> {
            <|
                "UUID" -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{3, 2}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Color" -> 4,
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {3, 2},
                "AspectRatio" -> 1,
                "Area" -> 1
            |>,
            <|
                "UUID" -> "334e881a-c81d-4f55-b381-1ea1090a5ffc",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{3, 4}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Color" -> 4,
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {3, 4},
                "AspectRatio" -> 1,
                "Area" -> 1
            |>
        }
    |>
    ,
    TestID -> "ObjectsByAttribute-20220718-M1WUNF"
]

Test[
    Daniel`ARC`ObjectsByAttribute[
        {
            <|
                "UUID" -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{3, 2}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Color" -> 4,
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {3, 2},
                "AspectRatio" -> 1,
                "Area" -> 1
            |>,
            <|
                "UUID" -> "334e881a-c81d-4f55-b381-1ea1090a5ffc",
                "Image" -> Daniel`ARC`ARCScene[{{4}}],
                "PixelPositions" -> {{3, 4}},
                "Shape" -> {<|"Name" -> "Pixel"|>},
                "Color" -> 4,
                "Width" -> 1,
                "Height" -> 1,
                "Position" -> {3, 4},
                "AspectRatio" -> 1,
                "Area" -> 1
            |>
        },
        "PixelPosition"
    ]
    ,
    <|
        {3, 2} -> "c352b306-971b-4632-b5a0-a66a4c8e4b27",
        {3, 4} -> "334e881a-c81d-4f55-b381-1ea1090a5ffc"
    |>
    ,
    TestID -> "ObjectsByAttribute-20220718-PFBNHJ"
]