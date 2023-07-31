(*
    Tests for: Daniel`ARC`ARCRuleForAddedObjects
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRuleForAddedObjects]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleForAddedObjects[
            {
                <|
                    "Transform" -> <|
                        "Type" -> "AddObjects",
                        "Objects" -> {<|"Shape" -> <|"Name" -> "Pixel"|>, "Colors" -> {1}|>}
                    |>,
                    "Example" -> 1
                |>,
                <|
                    "Transform" -> <|
                        "Type" -> "AddObjects",
                        "Objects" -> {<|"Shape" -> <|"Name" -> "Pixel"|>, "Colors" -> {1}|>}
                    |>,
                    "Example" -> 2
                |>
            },
            <||>,
            {
                <|"Input" -> <|"Objects" -> {}|>, "Output" -> <|"Objects" -> {}|>|>,
                <|"Input" -> <|"Objects" -> {}|>, "Output" -> <|"Objects" -> {}|>|>
            }
        ]
    ]
    ,
    <|
        "Transform" -> <|
            "Type" -> "AddObjects",
            "Objects" -> {<|"Shape" -> <|"Name" -> "Pixel"|>, "Colors" -> {1}|>}
        |>
    |>
    ,
    TestID -> "ARCRuleForAddedObjects-20220820-TXXAJX"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRuleForAddedObjects[
            {
                <|
                    "Transform" -> <|
                        "Type" -> "AddObjects",
                        "Objects" -> {
                            <|
                                "UUID" -> 1,
                                "Image" -> Daniel`ARC`ARCScene[{{1}}],
                                "Y" -> 1,
                                "X" -> 1
                            |>
                        }
                    |>
                |>,
                <|
                    "Transform" -> <|
                        "Type" -> "AddObjects",
                        "Objects" -> {
                            <|
                                "UUID" -> 2,
                                "Image" -> Daniel`ARC`ARCScene[{{2}}],
                                "Y" -> 1,
                                "X" -> 1
                            |>
                        }
                    |>
                |>
            },
            <|
                Daniel`ARC`Object[
                    <|"Shape" -> <|"Name" -> "Pixel"|>, "Context" -> "Output"|>
                ] -> <|
                |>
            |>,
            {
                <|
                    "Input" -> <|"Objects" -> {}|>,
                    "Output" -> <|
                        "Objects" -> {
                            <|
                                "UUID" -> 3,
                                "Image" -> Daniel`ARC`ARCScene[{{1}}],
                                "Shape" -> <|"Name" -> "Pixel"|>,
                                "Position" -> {5, 5}
                            |>
                        }
                    |>
                |>,
                <|
                    "Input" -> <|"Objects" -> {}|>,
                    "Output" -> <|
                        "Objects" -> {
                            <|
                                "UUID" -> 4,
                                "Image" -> Daniel`ARC`ARCScene[{{2}}],
                                "Shape" -> <|"Name" -> "Pixel"|>,
                                "Position" -> {5, 5}
                            |>
                        }
                    |>
                |>
            }
        ]
    ]
    ,
    <|
        "Transform" -> <|
            "Type" -> "AddObjects",
            "Objects" -> {
                <|
                    "Image" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>, "Context" -> "Output"|>,
                        "Image"
                    ],
                    "Y" -> 1,
                    "X" -> 1
                |>
            }
        |>
    |>
    ,
    TestID -> "ARCRuleForAddedObjects-20220820-LCD0WN"
]