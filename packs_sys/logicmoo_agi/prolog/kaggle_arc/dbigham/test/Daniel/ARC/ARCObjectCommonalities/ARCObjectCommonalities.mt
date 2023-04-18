(*
    Tests for: Daniel`ARC`ARCObjectCommonalities
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCObjectCommonalities]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCObjectCommonalities[
        {<|"a" -> 1, "b" -> 2, "c" -> 3|>, <|"a" -> 1, "b" -> 2|>, <|"a" -> 1, "b" -> 3|>}
    ]
    ,
    <|"a" -> 1|>
    ,
    TestID -> "ARCObjectCommonalities-20220812-8TSP1Y"
]

Test[
    Daniel`ARC`ARCObjectCommonalities[
        {<|"b" -> 2, "c" -> 3|>, <|"a" -> 1, "b" -> 2|>, <|"a" -> 1, "b" -> 2|>}
    ]
    ,
    <|"b" -> 2|>
    ,
    TestID -> "ARCObjectCommonalities-20220812-T0RFMD"
]

Test[
    Daniel`ARC`ARCObjectCommonalities[{<|"Shapes" -> {1, 2, 3}|>, <|"Shapes" -> {1, 2}|>}]
    ,
    <|"Shapes" -> {1, 2}|>
    ,
    TestID -> "ARCObjectCommonalities-20220812-O86QYO"
]

Test[
    Daniel`ARC`ARCObjectCommonalities[
        {
            <|
                "Type" -> "Group",
                "FilledArea" -> 2,
                "Components" -> {<|"Color" -> 1, "Area" -> 1|>, <|"Color" -> 1, "Area" -> 1|>}
            |>,
            <|
                "Type" -> "Group",
                "FilledArea" -> 2,
                "Components" -> {<|"Color" -> 2, "Area" -> 1|>, <|"Color" -> 2, "Area" -> 1|>}
            |>
        }
    ]
    ,
    <|
        "Type" -> "Group",
        "FilledArea" -> 2,
        "Components" -> {Repeated[<|"Area" -> 1, "Color" -> "Same"|>, {2}]}
    |>
    ,
    TestID -> "ARCObjectCommonalities-20220827-8N6I86"
]

Test[
    Daniel`ARC`ARCObjectCommonalities[
        {<|"a" -> 1, "b" -> 2|>, <|"a" -> 1|>, <|"a" -> 1, "b" -> 2, "c" -> 3|>},
        "IgnoreMissing" -> True
    ]
    ,
    <|"a" -> 1, "b" -> 2, "c" -> 3|>
    ,
    TestID -> "ARCObjectCommonalities-20220904-BEU9JW"
]

Test[
    Daniel`ARC`ARCObjectCommonalities[{<|"a" -> 1, "b" -> 2|>, <|"a" -> 1|>}]
    ,
    <|"a" -> 1|>
    ,
    TestID -> "ARCObjectCommonalities-20220904-I37QPI"
]

Test[
    Daniel`ARC`ARCObjectCommonalities[
        {
            <|"a" -> 1, "b" -> <|"c" -> 2, "d" -> 3|>|>,
            <|"a" -> 1, "b" -> <|"c" -> 2, "d" -> 4|>|>
        },
        "IgnoreMissing" -> True,
        "Recursive" -> True
    ]
    ,
    <|"a" -> 1, "b" -> <|"c" -> 2|>|>
    ,
    TestID -> "ARCObjectCommonalities-20220904-W8XO3M"
]