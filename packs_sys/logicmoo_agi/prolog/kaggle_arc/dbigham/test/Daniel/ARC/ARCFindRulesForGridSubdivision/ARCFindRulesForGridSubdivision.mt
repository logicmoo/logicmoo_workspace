(*
    Tests for: Daniel`ARC`ARCFindRulesForGridSubdivision
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFindRulesForGridSubdivision]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCFindRulesForGridSubdivision[
                Daniel`ARC`ARCParseInputAndOutputScenes[
                    Daniel`ARC`ARCParseFile["272f95fa"]["Train"]
                ]
            ]
        ]
    ]
    ,
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
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
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
                                        "Shape" -> <|"Name" -> "Square", "Filled" -> True|>,
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
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
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
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
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
                                        "Shape" -> <|"Name" -> "Rectangle", "Filled" -> True|>,
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
    ,
    TestID -> "ARCFindRulesForGridSubdivision-20220910-2IXC3S"
]