(*
    Tests for: Daniel`ARC`ARCApplyRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCApplyRules]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            With[
                {trainingData = Daniel`ARC`ARCParseFile[file = "0ca9ddb6"]["Train"]},
                Daniel`ARC`ARCApplyRules[
                    trainingData[[2, "Input"]],
                    Utility`ReturnIfFailure[Daniel`ARC`ARCFindRules[trainingData]]
                ]
            ]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {0, 0, 0, 8, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 4, 0, 4, 0},
            {0, 0, 7, 0, 0, 0, 2, 0, 0},
            {0, 7, 1, 7, 0, 4, 0, 4, 0},
            {0, 0, 7, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 7, 0, 0},
            {4, 0, 4, 0, 0, 7, 1, 7, 0},
            {0, 2, 0, 0, 0, 0, 7, 0, 0},
            {4, 0, 4, 0, 0, 0, 0, 0, 0}
        }
    ]
    ,
    TestID -> "ARCApplyRules-20220722-MRMVB2"
]

Test[
    With[
        {trainingData = Daniel`ARC`ARCParseFile[file = "1caeab9d"]["Train"]},
        Daniel`ARC`ARCApplyRules[
            trainingData[[2, "Input"]],
            Utility`ReturnIfFailure[Daniel`ARC`ARCFindRules[trainingData]]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 2, 2, 2, 1, 1, 1, 4, 4, 4},
            {0, 2, 2, 2, 1, 1, 1, 4, 4, 4},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        }
    ]
    ,
    TestID -> "ARCApplyRules-20220724-QRNREW"
]

Test[
    With[
        {trainingData = Daniel`ARC`ARCParseFile[file = "05f2a901"]["Train"]},
        Daniel`ARC`ARCApplyRules[
            trainingData[[2, "Input"]],
            Utility`ReturnIfFailure[Daniel`ARC`ARCFindRules[trainingData]]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 2, 2, 0, 0, 0, 0},
            {0, 0, 0, 0, 2, 2, 0, 0, 0, 0},
            {0, 0, 0, 2, 2, 2, 0, 0, 0, 0},
            {0, 0, 0, 0, 2, 2, 8, 8, 0, 0},
            {0, 0, 0, 0, 0, 0, 8, 8, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        }
    ]
    ,
    TestID -> "ARCApplyRules-20220804-YG0J5Y"
]

Test[
    With[
        {trainingData = Daniel`ARC`ARCParseFile[file = "08ed6ac7"]["Train"]},
        Daniel`ARC`ARCApplyRules[
            trainingData[[1, "Input"]],
            Utility`ReturnIfFailure[Daniel`ARC`ARCFindRules[trainingData]]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {0, 0, 0, 0, 0, 1, 0, 0, 0},
            {0, 2, 0, 0, 0, 1, 0, 0, 0},
            {0, 2, 0, 0, 0, 1, 0, 0, 0},
            {0, 2, 0, 3, 0, 1, 0, 0, 0},
            {0, 2, 0, 3, 0, 1, 0, 0, 0},
            {0, 2, 0, 3, 0, 1, 0, 0, 0},
            {0, 2, 0, 3, 0, 1, 0, 4, 0},
            {0, 2, 0, 3, 0, 1, 0, 4, 0},
            {0, 2, 0, 3, 0, 1, 0, 4, 0}
        }
    ]
    ,
    TestID -> "ARCApplyRules-20220804-WHM3MW"
]

Test[
    With[
        {parsedFile = Daniel`ARC`ARCParseFile[file = "ihiz27k2n"]},
        Daniel`ARC`ARCApplyRules[
            parsedFile["Test", 1, "Input"],
            Utility`ReturnIfFailure[Daniel`ARC`ARCFindRules[parsedFile["Train"]]]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
            {5, 1, 5, 1, 1, 1, 1, 5, 1, 5},
            {5, 1, 5, 1, 1, 1, 1, 5, 1, 5},
            {5, 1, 5, 5, 5, 5, 5, 5, 1, 5},
            {5, 1, 5, 1, 1, 1, 1, 5, 1, 5},
            {5, 1, 5, 1, 1, 1, 1, 5, 1, 5},
            {5, 5, 5, 5, 5, 5, 5, 5, 5, 5},
            {5, 3, 5, 3, 3, 3, 3, 5, 3, 5},
            {5, 3, 5, 3, 3, 3, 3, 5, 3, 5},
            {5, 5, 5, 5, 5, 5, 5, 5, 5, 5}
        }
    ]
    ,
    TestID -> "ARCApplyRules-20220806-7U6ZB4"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            With[
                {parsedFile = Daniel`ARC`ARCParseFile[file = "25d8a9c8"]},
                Daniel`ARC`ARCApplyRules[
                    parsedFile["Train", 1, "Input"],
                    Utility`ReturnIfFailure[Daniel`ARC`ARCFindRules[parsedFile["Train"]]]
                ]
            ]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{5, 5, 5}, {0, 0, 0}, {0, 0, 0}}]
    ,
    TestID -> "ARCApplyRules-20220819-AU0PQC"
]

Test[
    With[
        {parsedFile = Daniel`ARC`ARCParseFile[file = "a61ba2ce"]},
        Daniel`ARC`ARCApplyRules[
            parsedFile["Train", 1, "Input"],
            Utility`ReturnIfFailure[Daniel`ARC`ARCFindRules[parsedFile["Train"]]]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{8, 8, 2, 2}, {8, 0, 0, 2}, {3, 0, 0, 1}, {3, 3, 1, 1}}]
    ,
    TestID -> "ARCApplyRules-20220906-3546LC"
]

Test[
    With[
        {parsedFile = Daniel`ARC`ARCParseFile[file = "272f95fa"]},
        Daniel`ARC`ARCApplyRules[
            parsedFile["Test", 1, "Input"],
            <|
                "Subdivision" -> "Grid",
                "Rules" -> {
                    {
                        <|"Rules" -> {}|>,
                        <|
                            "Rules" -> {
                                <|
                                    "Transform" -> <|
                                        "Type" -> "AddObjects",
                                        "Objects" -> {
                                            <|
                                                "Shape" -> <|
                                                    "Name" -> "Rectangle",
                                                    "Filled" -> True
                                                |>,
                                                "Color" -> 2,
                                                "X" -> 1,
                                                "Y" -> 1,
                                                "X2Inverse" -> 1,
                                                "Y2Inverse" -> 1
                                            |>
                                        }
                                    |>
                                |>
                            }
                        |>,
                        <|"Rules" -> {}|>
                    },
                    {
                        <|
                            "Rules" -> {
                                <|
                                    "Transform" -> <|
                                        "Type" -> "AddObjects",
                                        "Objects" -> {
                                            <|
                                                "Shape" -> <|
                                                    "Name" -> "Square",
                                                    "Filled" -> True
                                                |>,
                                                "Color" -> 4,
                                                "X" -> 1,
                                                "Y" -> 1,
                                                "X2Inverse" -> 1,
                                                "Y2Inverse" -> 1
                                            |>
                                        }
                                    |>
                                |>
                            }
                        |>,
                        <|
                            "Rules" -> {
                                <|
                                    "Transform" -> <|
                                        "Type" -> "AddObjects",
                                        "Objects" -> {
                                            <|
                                                "Shape" -> <|
                                                    "Name" -> "Rectangle",
                                                    "Filled" -> True
                                                |>,
                                                "Color" -> 6,
                                                "X" -> 1,
                                                "Y" -> 1,
                                                "X2Inverse" -> 1,
                                                "Y2Inverse" -> 1
                                            |>
                                        }
                                    |>
                                |>
                            }
                        |>,
                        <|
                            "Rules" -> {
                                <|
                                    "Transform" -> <|
                                        "Type" -> "AddObjects",
                                        "Objects" -> {
                                            <|
                                                "Shape" -> <|
                                                    "Name" -> "Rectangle",
                                                    "Filled" -> True
                                                |>,
                                                "Color" -> 3,
                                                "X" -> 1,
                                                "Y" -> 1,
                                                "X2Inverse" -> 1,
                                                "Y2Inverse" -> 1
                                            |>
                                        }
                                    |>
                                |>
                            }
                        |>
                    },
                    {
                        <|"Rules" -> {}|>,
                        <|
                            "Rules" -> {
                                <|
                                    "Transform" -> <|
                                        "Type" -> "AddObjects",
                                        "Objects" -> {
                                            <|
                                                "Shape" -> <|
                                                    "Name" -> "Rectangle",
                                                    "Filled" -> True
                                                |>,
                                                "Color" -> 1,
                                                "X" -> 1,
                                                "Y" -> 1,
                                                "X2Inverse" -> 1,
                                                "Y2Inverse" -> 1
                                            |>
                                        }
                                    |>
                                |>
                            }
                        |>,
                        <|"Rules" -> {}|>
                    }
                }
            |>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {0, 0, 0, 8, 2, 2, 2, 2, 8, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 8, 2, 2, 2, 2, 8, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 8, 2, 2, 2, 2, 8, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 8, 2, 2, 2, 2, 8, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 8, 2, 2, 2, 2, 8, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 8, 2, 2, 2, 2, 8, 0, 0, 0, 0, 0, 0},
            {8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8},
            {4, 4, 4, 8, 6, 6, 6, 6, 8, 3, 3, 3, 3, 3, 3},
            {4, 4, 4, 8, 6, 6, 6, 6, 8, 3, 3, 3, 3, 3, 3},
            {4, 4, 4, 8, 6, 6, 6, 6, 8, 3, 3, 3, 3, 3, 3},
            {4, 4, 4, 8, 6, 6, 6, 6, 8, 3, 3, 3, 3, 3, 3},
            {4, 4, 4, 8, 6, 6, 6, 6, 8, 3, 3, 3, 3, 3, 3},
            {4, 4, 4, 8, 6, 6, 6, 6, 8, 3, 3, 3, 3, 3, 3},
            {8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8},
            {0, 0, 0, 8, 1, 1, 1, 1, 8, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 8, 1, 1, 1, 1, 8, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 8, 1, 1, 1, 1, 8, 0, 0, 0, 0, 0, 0}
        }
    ]
    ,
    TestID -> "ARCApplyRules-20220910-4EHF2R"
]

Test[
    With[
        {parsedFile = Daniel`ARC`ARCParseFile[file = "8be77c9e"]},
        Daniel`ARC`ARCApplyRules[
            parsedFile["Test", 1, "Input"],
            <|
                "SceneAsSingleObject" -> True,
                "Height" -> Inactive[Daniel`ARC`ObjectValue["InputScene", "Height"]*2],
                "Rules" -> {
                    {
                        <|
                            "SceneAsSingleObject" -> True,
                            "Rules" -> {<||> -> <|"Same" -> True|>}
                        |>
                    },
                    {
                        <|
                            "SceneAsSingleObject" -> True,
                            "Rules" -> {
                                <||> -> <|
                                    "Transform" -> <|
                                        "Type" -> "Flip",
                                        "Direction" -> "Vertical"
                                    |>
                                |>
                            }
                        |>
                    }
                }
            |>
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{0, 0, 0}, {0, 0, 1}, {1, 0, 0}, {1, 0, 0}, {0, 0, 1}, {0, 0, 0}}]
    ,
    TestID -> "ARCApplyRules-20220910-597ORO"
]