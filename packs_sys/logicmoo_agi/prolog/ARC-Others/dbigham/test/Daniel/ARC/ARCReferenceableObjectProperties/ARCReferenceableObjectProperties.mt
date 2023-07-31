(*
    Tests for: Daniel`ARC`ARCReferenceableObjectProperties
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCReferenceableObjectProperties]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Utility`BlockUUID[
                Module[
                    {parsedScenes},
                    parsedScenes = Utility`ReturnIfFailure[
                        Daniel`ARC`ARCParseExamples[file = "1caeab9d"][[All, "Input"]]
                    ];
                    Daniel`ARC`ARCReferenceableObjectProperties[
                        Keys[Daniel`ARC`ARCMakeObjectsReferenceable[parsedScenes]],
                        parsedScenes[[All, "Objects"]]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        Daniel`ARC`Object[<|"Colors" -> {1}|>] -> <|
            "Color" -> 1,
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d4",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dg",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq"
            }
        |>,
        Daniel`ARC`Object[<|"Colors" -> {2}|>] -> <|
            "Color" -> 2,
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d5",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dh",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dr"
            }
        |>,
        Daniel`ARC`Object[<|"Colors" -> {4}|>] -> <|
            "Color" -> 4,
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d6",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742di",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742ds"
            }
        |>,
        Daniel`ARC`Object[<|"Y" -> 3|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y2" -> 4,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d6",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dh",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq"
            }
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
            "X2Inverse.InverseRank" -> 3,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d5",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dh",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq"
            }
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 1|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y.InverseRank" -> 3,
            "YInverse.Rank" -> 3,
            "YInverse.InverseRank" -> 1,
            "Y2.Rank" -> 1,
            "Y2.InverseRank" -> 3,
            "Y2Inverse.Rank" -> 3,
            "Y2Inverse.InverseRank" -> 1,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d6",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dg",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742ds"
            }
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 2|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y.InverseRank" -> 2,
            "YInverse.Rank" -> 2,
            "YInverse.InverseRank" -> 2,
            "Y2.Rank" -> 2,
            "Y2.InverseRank" -> 2,
            "Y2Inverse.Rank" -> 2,
            "Y2Inverse.InverseRank" -> 2,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d4",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dh",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq"
            }
        |>,
        Daniel`ARC`Object[<|"Y.Rank" -> 3|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "Y.InverseRank" -> 1,
            "YInverse.Rank" -> 1,
            "YInverse.InverseRank" -> 3,
            "Y2.Rank" -> 3,
            "Y2.InverseRank" -> 1,
            "Y2Inverse.Rank" -> 1,
            "Y2Inverse.InverseRank" -> 3,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d5",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742di",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dr"
            }
        |>,
        Daniel`ARC`Object[<|"X.Rank" -> 1|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "X.InverseRank" -> 3,
            "XInverse.Rank" -> 3,
            "XInverse.InverseRank" -> 1,
            "X2.Rank" -> 1,
            "X2.InverseRank" -> 3,
            "X2Inverse.Rank" -> 3,
            "X2Inverse.InverseRank" -> 1,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d4",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742di",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742ds"
            }
        |>,
        Daniel`ARC`Object[<|"X.Rank" -> 2|>] -> <|
            "Shapes" -> {<|"Name" -> "Rectangle"|>, <|"Name" -> "Rectangle", "Filled" -> True|>},
            "X.InverseRank" -> 2,
            "XInverse.Rank" -> 2,
            "XInverse.InverseRank" -> 2,
            "X2.Rank" -> 2,
            "X2.InverseRank" -> 2,
            "X2Inverse.Rank" -> 2,
            "X2Inverse.InverseRank" -> 2,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d6",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dg",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dr"
            }
        |>
    |>
    ,
    TestID -> "ARCReferenceableObjectProperties-20220723-VLXGQE"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Utility`BlockUUID[
                Module[
                    {parsedScenes},
                    parsedScenes = Utility`ReturnIfFailure[
                        Daniel`ARC`ARCParseExamples[file = "ce9e57f2"][[All, "Input"]]
                    ];
                    Daniel`ARC`ARCReferenceableObjectProperties[
                        Keys[Daniel`ARC`ARCMakeObjectsReferenceable[parsedScenes]],
                        parsedScenes[[All, "Objects"]]
                    ]
                ]
            ]
        ]
    ]
    ,
    <|
        Daniel`ARC`Object[<|"Image" -> Daniel`ARC`ARCScene[{{2}, {2}, {2}}]|>] -> <|
            "Shapes" -> {
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 90|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}],
                    "Transform" -> <|"Type" -> "Rotation", "Angle" -> 270|>
                |>,
                <|"Image" -> Daniel`ARC`ARCScene[{{10}, {10}, {10}}]|>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {{10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}, {10, 10}}
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.5|>
                |>,
                <|
                    "Image" -> Daniel`ARC`ARCScene[
                        {
                            {10, 10, 10},
                            {10, 10, 10},
                            {10, 10, 10},
                            {10, 10, 10},
                            {10, 10, 10},
                            {10, 10, 10},
                            {10, 10, 10},
                            {10, 10, 10},
                            {10, 10, 10}
                        }
                    ],
                    "Transform" -> <|"Type" -> "Scaled", "Factor" -> 0.3333333333333333|>
                |>,
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "Height" -> 3,
            "Length" -> 3,
            "HorizontalLineSymmetry" -> False,
            "YInverse" -> 3,
            "AspectRatio" -> 1/3,
            "Area" -> 3,
            "FilledArea" -> 3,
            "SurfacePixelCount" -> 3,
            "Height.Rank" -> 4,
            "Height.InverseRank" -> 1,
            "Length.Rank" -> 4,
            "Length.InverseRank" -> 1,
            "Y.Rank" -> 1,
            "Y.InverseRank" -> 4,
            "YInverse.Rank" -> 4,
            "YInverse.InverseRank" -> 1,
            "YMiddle.Rank" -> 1,
            "AspectRatio.Rank" -> 1,
            "AspectRatio.InverseRank" -> 4,
            "Area.Rank" -> 4,
            "Area.InverseRank" -> 1,
            "FilledArea.Rank" -> 4,
            "FilledArea.InverseRank" -> 1,
            "SurfacePixelCount.Rank" -> 4,
            "SurfacePixelCount.InverseRank" -> 1,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d7",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dr",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742eb"
            }
        |>,
        Daniel`ARC`Object[<|"Y" -> 2|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "Height.Rank" -> 1,
            "Height.InverseRank" -> 4,
            "Length.Rank" -> 1,
            "Length.InverseRank" -> 4,
            "Y.Rank" -> 4,
            "Y.InverseRank" -> 1,
            "YInverse.Rank" -> 1,
            "YInverse.InverseRank" -> 4,
            "AspectRatio.Rank" -> 4,
            "AspectRatio.InverseRank" -> 1,
            "Area.Rank" -> 1,
            "Area.InverseRank" -> 4,
            "FilledArea.Rank" -> 1,
            "FilledArea.InverseRank" -> 4,
            "SurfacePixelCount.Rank" -> 1,
            "SurfacePixelCount.InverseRank" -> 4,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d4",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742do",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742e8"
            }
        |>,
        Daniel`ARC`Object[<|"Y" -> 3|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "Height.Rank" -> 2,
            "Height.InverseRank" -> 3,
            "Length.Rank" -> 2,
            "Length.InverseRank" -> 3,
            "Y.Rank" -> 3,
            "Y.InverseRank" -> 2,
            "YInverse.Rank" -> 2,
            "YInverse.InverseRank" -> 3,
            "AspectRatio.Rank" -> 3,
            "AspectRatio.InverseRank" -> 2,
            "Area.Rank" -> 2,
            "Area.InverseRank" -> 3,
            "FilledArea.Rank" -> 2,
            "FilledArea.InverseRank" -> 3,
            "SurfacePixelCount.Rank" -> 2,
            "SurfacePixelCount.InverseRank" -> 3,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d5",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dp",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742e9"
            }
        |>,
        Daniel`ARC`Object[<|"X" -> 2|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "XInverse" -> 8,
            "X2" -> 2,
            "X2Inverse" -> 8,
            "XMiddle" -> 2,
            "X.Rank" -> 4,
            "X.InverseRank" -> 1,
            "XInverse.Rank" -> 1,
            "XInverse.InverseRank" -> 4,
            "X2.Rank" -> 4,
            "X2.InverseRank" -> 1,
            "X2Inverse.Rank" -> 1,
            "X2Inverse.InverseRank" -> 4,
            "XMiddle.Rank" -> 4,
            "XMiddle.InverseRank" -> 1,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d4",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742do",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742e9"
            }
        |>,
        Daniel`ARC`Object[<|"X" -> 4|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "HorizontalLineSymmetry" -> False,
            "XInverse" -> 6,
            "X2" -> 4,
            "X2Inverse" -> 6,
            "XMiddle" -> 4,
            "X.Rank" -> 3,
            "X.InverseRank" -> 2,
            "XInverse.Rank" -> 2,
            "XInverse.InverseRank" -> 3,
            "X2.Rank" -> 3,
            "X2.InverseRank" -> 2,
            "X2Inverse.Rank" -> 2,
            "X2Inverse.InverseRank" -> 3,
            "XMiddle.Rank" -> 3,
            "XMiddle.InverseRank" -> 2,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d5",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742eb"
            }
        |>,
        Daniel`ARC`Object[<|"X" -> 6|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "XInverse" -> 4,
            "X2" -> 6,
            "X2Inverse" -> 4,
            "XMiddle" -> 6,
            "X.Rank" -> 2,
            "X.InverseRank" -> 3,
            "XInverse.Rank" -> 3,
            "XInverse.InverseRank" -> 2,
            "X2.Rank" -> 2,
            "X2.InverseRank" -> 3,
            "X2Inverse.Rank" -> 3,
            "X2Inverse.InverseRank" -> 2,
            "XMiddle.Rank" -> 2,
            "XMiddle.InverseRank" -> 3,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d6",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dr",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742ea"
            }
        |>,
        Daniel`ARC`Object[<|"X" -> 8|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "XInverse" -> 2,
            "X2" -> 8,
            "X2Inverse" -> 2,
            "XMiddle" -> 8,
            "X.Rank" -> 1,
            "X.InverseRank" -> 4,
            "XInverse.Rank" -> 4,
            "XInverse.InverseRank" -> 1,
            "X2.Rank" -> 1,
            "X2.InverseRank" -> 4,
            "X2Inverse.Rank" -> 4,
            "X2Inverse.InverseRank" -> 1,
            "XMiddle.Rank" -> 1,
            "XMiddle.InverseRank" -> 4,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d7",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dp",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742e8"
            }
        |>,
        Daniel`ARC`Object[<|"YMiddle" -> 6|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "HorizontalLineSymmetry" -> False,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d7",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742e9"
            }
        |>,
        Daniel`ARC`Object[<|"Height.Rank" -> 3|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "Height.InverseRank" -> 2,
            "Length.Rank" -> 3,
            "Length.InverseRank" -> 2,
            "Y.Rank" -> 2,
            "Y.InverseRank" -> 3,
            "YInverse.Rank" -> 3,
            "YInverse.InverseRank" -> 2,
            "AspectRatio.Rank" -> 2,
            "AspectRatio.InverseRank" -> 3,
            "Area.Rank" -> 3,
            "Area.InverseRank" -> 2,
            "FilledArea.Rank" -> 3,
            "FilledArea.InverseRank" -> 2,
            "SurfacePixelCount.Rank" -> 3,
            "SurfacePixelCount.InverseRank" -> 2,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d6",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742ea"
            }
        |>,
        Daniel`ARC`Object[<|"YMiddle.Rank" -> 2|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "HorizontalLineSymmetry" -> False,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d5",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742e9"
            }
        |>,
        Daniel`ARC`Object[<|"YMiddle.InverseRank" -> 1|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "HorizontalLineSymmetry" -> False,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d5",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742do",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742e9"
            }
        |>,
        Daniel`ARC`Object[<|"YMiddle.InverseRank" -> 2|>] -> <|
            "Shapes" -> {
                <|"Name" -> "Line"|>,
                <|"Name" -> "Line", "Angle" -> 90|>,
                <|"Name" -> "Rectangle"|>,
                <|"Name" -> "Rectangle", "Filled" -> True|>
            },
            "HorizontalLineSymmetry" -> False,
            "Objects" -> {
                "e7a71aa3-1a87-4e68-a1ce-009fa20742d7",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742dq",
                "e7a71aa3-1a87-4e68-a1ce-009fa20742eb"
            }
        |>
    |>
    ,
    TestID -> "ARCReferenceableObjectProperties-20220723-G8TUB6"
]