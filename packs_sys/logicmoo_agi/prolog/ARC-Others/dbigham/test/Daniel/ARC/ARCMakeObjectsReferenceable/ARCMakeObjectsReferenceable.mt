(*
    Tests for: Daniel`ARC`ARCMakeObjectsReferenceable
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMakeObjectsReferenceable]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Utility`BlockUUID[
                Daniel`ARC`ARCMakeObjectsReferenceable[
                    Daniel`ARC`ARCParseExamples[file = "1caeab9d"][[All, "Input"]]
                ]
            ]
        ]
    ]
    ,
    <|
        Daniel`ARC`Object[<|"Colors" -> {1}|>] -> <|
            "Color" -> 1,
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "MostUsedColor" -> 1,
            "MostUsedColor.Rank" -> 3,
            "MostUsedColor.InverseRank" -> 1
        |>,
        Daniel`ARC`Object[<|"Colors" -> {2}|>] -> <|
            "Color" -> 2,
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "MostUsedColor" -> 2,
            "MostUsedColor.Rank" -> 2,
            "MostUsedColor.InverseRank" -> 2
        |>,
        Daniel`ARC`Object[<|"Colors" -> {4}|>] -> <|
            "Color" -> 4,
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "MostUsedColor" -> 4,
            "MostUsedColor.Rank" -> 1,
            "MostUsedColor.InverseRank" -> 3
        |>,
        Daniel`ARC`Object[<|"Y" -> 3|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y2" -> 4
        |>,
        Daniel`ARC`Object[<|"X" -> 2|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "XInverse" -> 9,
            "X.Rank" -> 3,
            "X.InverseRank" -> 1,
            "XInverse.Rank" -> 1,
            "XInverse.InverseRank" -> 3,
            "X2.Rank" -> 3,
            "X2.InverseRank" -> 1,
            "X2Inverse.Rank" -> 1,
            "X2Inverse.InverseRank" -> 3
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 1|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y.InverseRank" -> 3,
            "YInverse.Rank" -> 3,
            "YInverse.InverseRank" -> 1,
            "Y2.Rank" -> 1,
            "Y2.InverseRank" -> 3,
            "Y2Inverse.Rank" -> 3,
            "Y2Inverse.InverseRank" -> 1
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 2|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y.InverseRank" -> 2,
            "YInverse.Rank" -> 2,
            "YInverse.InverseRank" -> 2,
            "Y2.Rank" -> 2,
            "Y2.InverseRank" -> 2,
            "Y2Inverse.Rank" -> 2,
            "Y2Inverse.InverseRank" -> 2
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 3|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y.InverseRank" -> 1,
            "YInverse.Rank" -> 1,
            "YInverse.InverseRank" -> 3,
            "Y2.Rank" -> 3,
            "Y2.InverseRank" -> 1,
            "Y2Inverse.Rank" -> 1,
            "Y2Inverse.InverseRank" -> 3
        |>,
        Daniel`ARC`Object[<|"X.Rank" -> 1|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "X.InverseRank" -> 3,
            "XInverse.Rank" -> 3,
            "XInverse.InverseRank" -> 1,
            "X2.Rank" -> 1,
            "X2.InverseRank" -> 3,
            "X2Inverse.Rank" -> 3,
            "X2Inverse.InverseRank" -> 1
        |>,
        Daniel`ARC`Object[<|"X.Rank" -> 2|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "X.InverseRank" -> 2,
            "XInverse.Rank" -> 2,
            "XInverse.InverseRank" -> 2,
            "X2.Rank" -> 2,
            "X2.InverseRank" -> 2,
            "X2Inverse.Rank" -> 2,
            "X2Inverse.InverseRank" -> 2
        |>
    |>
    ,
    TestID -> "ARCMakeObjectsReferenceable-20220723-84LXFB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Utility`BlockUUID[
                Daniel`ARC`ARCMakeObjectsReferenceable[
                    Daniel`ARC`ARCParseExamples[file = "0ca9ddb6"][[All, "Input"]]
                ]
            ]
        ]
    ]
    ,
    <|
        Daniel`ARC`Object[<|"X" -> 3|>] -> <|
            "XInverse" -> 7,
            "X2" -> 3,
            "X2Inverse" -> 7,
            "XMiddle" -> 3,
            "ColorUseCount.Rank" -> 1,
            "ImageUseCount.Rank" -> 1
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 1|>] -> <|
            "YInverse.InverseRank" -> 1,
            "Y2.Rank" -> 1,
            "Y2Inverse.InverseRank" -> 1,
            "YMiddle.Rank" -> 1,
            "ColorUseCount.Rank" -> 1,
            "ImageUseCount.Rank" -> 1
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 2|>] -> <|
            "YInverse.InverseRank" -> 2,
            "Y2.Rank" -> 2,
            "Y2Inverse.InverseRank" -> 2,
            "YMiddle.Rank" -> 2,
            "ColorUseCount.Rank" -> 1,
            "ImageUseCount.Rank" -> 1
        |>,
        Daniel`ARC`Object[<|"Y.InverseRank" -> 1|>] -> <|
            "ColorUseCount" -> 1,
            "ImageUseCount" -> 1,
            "YInverse.Rank" -> 1,
            "Y2.InverseRank" -> 1,
            "Y2Inverse.Rank" -> 1,
            "YMiddle.InverseRank" -> 1,
            "ColorUseCount.InverseRank" -> 1,
            "ImageUseCount.InverseRank" -> 1
        |>,
        Daniel`ARC`Object[<|"Y.InverseRank" -> 2|>] -> <|
            "X" -> 7,
            "XInverse" -> 3,
            "X2" -> 7,
            "X2Inverse" -> 3,
            "XMiddle" -> 7,
            "X.Rank" -> 1,
            "YInverse.Rank" -> 2,
            "XInverse.InverseRank" -> 1,
            "Y2.InverseRank" -> 2,
            "X2.Rank" -> 1,
            "Y2Inverse.Rank" -> 2,
            "X2Inverse.InverseRank" -> 1,
            "YMiddle.InverseRank" -> 2,
            "XMiddle.Rank" -> 1,
            "ColorUseCount.Rank" -> 1,
            "ImageUseCount.Rank" -> 1
        |>,
        Daniel`ARC`Object[<|"X.Rank" -> 2|>] -> <|
            "ColorUseCount" -> 1,
            "ImageUseCount" -> 1,
            "XInverse.InverseRank" -> 2,
            "X2.Rank" -> 2,
            "X2Inverse.InverseRank" -> 2,
            "XMiddle.Rank" -> 2,
            "ColorUseCount.InverseRank" -> 1,
            "ImageUseCount.InverseRank" -> 1
        |>,
        Daniel`ARC`Object[<|"X.InverseRank" -> 1|>] -> <|
            "Colors" -> {2},
            "Color" -> 2,
            "Image" -> Daniel`ARC`ARCScene[{{2}}],
            "XInverse.Rank" -> 1,
            "X2.InverseRank" -> 1,
            "X2Inverse.Rank" -> 1,
            "XMiddle.InverseRank" -> 1,
            "ColorUseCount.Rank" -> 1,
            "ImageUseCount.Rank" -> 1
        |>,
        Daniel`ARC`Object[<|"X.InverseRank" -> 2|>] -> <|
            "Colors" -> {1},
            "Color" -> 1,
            "Image" -> Daniel`ARC`ARCScene[{{1}}],
            "XInverse.Rank" -> 2,
            "X2.InverseRank" -> 2,
            "X2Inverse.Rank" -> 2,
            "XMiddle.InverseRank" -> 2,
            "ColorUseCount.Rank" -> 1,
            "ImageUseCount.Rank" -> 1
        |>
    |>
    ,
    TestID -> "ARCMakeObjectsReferenceable-20220723-V2H68K"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            First[
                Normal[
                    Utility`BlockUUID[
                        KeySelect[
                            Daniel`ARC`ARCMakeObjectsReferenceable[
                                Daniel`ARC`ARCParseExamples[
                                    file = "ifmyulnv8-dynamic-shape"
                                ][[
                                    All,
                                    "Input"
                                ]]
                            ],
                             !FreeQ[#1, _Except] & 
                        ]
                    ]
                ]
            ]
        ]
    ]
    ,
    Daniel`ARC`Object[<|"Colors" -> Except[{5}]|>] -> <|
        "HollowCount" -> 0,
        "Width.Rank" -> 2,
        "Width.InverseRank" -> 1,
        "Height.Rank" -> 2,
        "Height.InverseRank" -> 1,
        "Length.Rank" -> 2,
        "Length.InverseRank" -> 1,
        "X.Rank" -> 1,
        "XInverse.InverseRank" -> 1,
        "HollowCount.Rank" -> 2,
        "HollowCount.InverseRank" -> 1,
        "Area.Rank" -> 2,
        "Area.InverseRank" -> 1,
        "FilledArea.Rank" -> 2,
        "FilledArea.InverseRank" -> 1,
        "FilledProportion.Rank" -> 1,
        "FilledProportion.InverseRank" -> 2,
        "SurfacePixelCount.Rank" -> 2,
        "SurfacePixelCount.InverseRank" -> 1,
        "ColorUseCount.Rank" -> 2,
        "ColorUseCount.InverseRank" -> 1,
        "MostUsedColor.Rank" -> 2,
        "MostUsedColor.InverseRank" -> 1
    |>
    ,
    TestID -> "ARCMakeObjectsReferenceable-20220811-SKQ5BK"
]