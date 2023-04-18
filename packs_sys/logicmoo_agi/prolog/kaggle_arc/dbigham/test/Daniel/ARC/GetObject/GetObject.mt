(*
    Tests for: Daniel`ARC`GetObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`GetObject]
    
    Author: danielb
*)

Test[
    Daniel`ARC`GetObject[
        Daniel`ARC`Object[<|"Colors" -> {"Blue"}|>],
        <|"Objects" -> {<|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>}|>
    ]
    ,
    <|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>
    ,
    TestID -> "GetObject-20220723-G02VP3"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`GetObject[
            Daniel`ARC`Object[<|"Colors" -> {"Blue"}|>],
            <|"Objects" -> {<|"Colors" -> {"Red"}, "Position" -> {1, 1}|>}|>
        ]
    ]
    ,
    Failure[
        "ObjectNotFound",
        <|
            "MessageTemplate" -> "The object wasn't found in the scene.",
            "MessageParameters" -> <||>,
            "Object" -> <|"Colors" -> {"Blue"}|>,
            "Objects" -> {<|"Colors" -> {"Red"}, "Position" -> {1, 1}|>}
        |>
    ]
    ,
    TestID -> "GetObject-20220723-G1R39W"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`GetObject[
            Daniel`ARC`Object[<|"Colors" -> {"Blue"}|>],
            <|
                "Objects" -> {
                    <|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>,
                    <|"Colors" -> {"Blue"}, "Position" -> {2, 2}|>
                }
            |>
        ]
    ]
    ,
    Failure[
        "AmbiguousObject",
        <|
            "MessageTemplate" -> "Multiple matching objects were found, but only 1 match was expected.",
            "MessageParameters" -> <||>,
            "ObjectPattern" -> <|"Colors" -> {"Blue"}|>,
            "MatchingObjects" -> {
                <|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>,
                <|"Colors" -> {"Blue"}, "Position" -> {2, 2}|>
            }
        |>
    ]
    ,
    TestID -> "GetObject-20220723-DH0I2F"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`GetObject[
            "InputObject",
            <|"Objects" -> {<|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>}|>,
            "NamedObjects" -> <|"InputObject" -> "HERE"|>
        ]
    ]
    ,
    "HERE"
    ,
    TestID -> "GetObject-20220809-JZ27M2"
]

Test[
    Daniel`ARC`GetObject[
        Daniel`ARC`Class[<|"Colors" -> {"Blue"}|>],
        <|"Objects" -> {<|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>}|>
    ]
    ,
    {<|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>}
    ,
    TestID -> "GetObject-20220917-N15VX8"
]

Test[
    Daniel`ARC`GetObject[
        Daniel`ARC`Class[<|"Colors" -> {"Blue"}|>],
        <|
            "Objects" -> {
                <|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>,
                <|"Colors" -> {"Blue"}, "Position" -> {1, 2}|>
            }
        |>
    ]
    ,
    {
        <|"Colors" -> {"Blue"}, "Position" -> {1, 1}|>,
        <|"Colors" -> {"Blue"}, "Position" -> {1, 2}|>
    }
    ,
    TestID -> "GetObject-20220917-EM9VNL"
]

Test[
    Daniel`ARC`GetObject[Daniel`ARC`Class[<|"Colors" -> {"Blue"}|>], <|"Objects" -> {}|>]
    ,
    Failure[
        "NoObjectsOfClassFound",
        <|
            "MessageTemplate" -> "No objects of the given class were found in the scene.",
            "MessageParameters" -> <||>,
            "Class" -> <|"Colors" -> {"Blue"}|>,
            "Objects" -> {}
        |>
    ]
    ,
    TestID -> "GetObject-20220917-JPWP37"
]

Test[
    Daniel`ARC`GetObject[
        Daniel`ARC`Object[<|"Within" -> Daniel`ARC`Object[<|"Color" -> "Gray"|>]|>],
        <|
            "Objects" -> {
                <|
                    "Colors" -> {"Gray"},
                    "Color" -> "Gray",
                    "Position" -> {1, 1},
                    "Width" -> 10,
                    "Height" -> 10
                |>,
                <|"Colors" -> {"Blue"}, "Position" -> {5, 5}, "Width" -> 1, "Height" -> 1|>
            }
        |>
    ]
    ,
    <|"Colors" -> {"Blue"}, "Position" -> {5, 5}, "Width" -> 1, "Height" -> 1|>
    ,
    TestID -> "GetObject-20221016-IFL46N"
]