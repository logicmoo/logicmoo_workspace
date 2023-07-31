(*
    Tests for: Daniel`ARC`ARCGroupRulesByConclusion
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGroupRulesByConclusion]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGroupRulesByConclusion[
            {<|"a" -> 1|> -> <|"b" -> 1|>, <|"a" -> 2|> -> <|"b" -> 1|>}
        ]
    ]
    ,
    {<|"a" -> 1 | 2|> -> <|"b" -> 1|>}
    ,
    TestID -> "ARCGroupRulesByConclusion-20220819-3BSZWW"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGroupRulesByConclusion[
            {<|"a" -> 1|> -> <|"b" -> 1|>, <|"a" -> Except[1 | 2]|> -> <|"b" -> 1|>}
        ]
    ]
    ,
    {<|"a" -> Except[2]|> -> <|"b" -> 1|>}
    ,
    TestID -> "ARCGroupRulesByConclusion-20220819-DOQQUD"
]

Test[
    Daniel`ARC`ARCGroupRulesByConclusion[
        {
            <|"Shapes" -> <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}]|>|> -> <|
                "Colors" -> {5}
            |>,
            <|"Shapes" -> <|"Image" -> Daniel`ARC`ARCScene[{{10}, {10}}]|>|> -> <|
                "Transform" -> <|"Type" -> "Delete"|>
            |>,
            <|"Shapes" -> <|"Name" -> "Square"|>|> -> <|
                "Transform" -> <|"Type" -> "Delete"|>
            |>,
            <|
                "Shapes" -> Except[
                    Alternatives[
                        <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}]|>,
                        <|"Image" -> Daniel`ARC`ARCScene[{{10}, {10}}]|>,
                        <|"Name" -> "Square"|>
                    ]
                ]
            |> -> <|
                "Transform" -> <|"Type" -> "Delete"|>
            |>
        }
    ]
    ,
    {
        <|"Shapes" -> <|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}]|>|> -> <|
            "Colors" -> {5}
        |>,
        <|"Shapes" -> Except[<|"Image" -> Daniel`ARC`ARCScene[{{10, 10, 10}}]|>]|> -> <|
            "Transform" -> <|"Type" -> "Delete"|>
        |>
    }
    ,
    TestID -> "ARCGroupRulesByConclusion-20220819-O5DTVT"
]

Test[
    Daniel`ARC`ARCGroupRulesByConclusion[
        {
            <|"a" -> 1|> -> <|"b" -> 1, "Examples" -> {1}|>,
            <|"a" -> 2|> -> <|"b" -> 1, "Examples" -> {2}|>
        }
    ]
    ,
    {<|"a" -> 1 | 2|> -> <|"b" -> 1, "Examples" -> {1, 2}|>}
    ,
    TestID -> "ARCGroupRulesByConclusion-20220819-4EF0AM"
]

Test[
    Daniel`ARC`ARCGroupRulesByConclusion[
        {
            <|"Shape" -> <|"Name" -> "Pixel"|>|> -> <|
                "Transform" -> <|"Type" -> "Delete"|>
            |>,
            <|"Shape" -> Except[<|"Name" -> "Pixel"|>]|> -> <|
                "Colors" -> Daniel`ARC`ObjectValue[
                    <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                    "Colors"
                ]
            |>
        }
    ]
    ,
    {
        <|"Shape" -> <|"Name" -> "Pixel"|>|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
        <|"Shape" -> Except[<|"Name" -> "Pixel"|>]|> -> <|
            "Colors" -> Daniel`ARC`ObjectValue[<|"Shape" -> <|"Name" -> "Pixel"|>|>, "Colors"]
        |>
    }
    ,
    TestID -> "ARCGroupRulesByConclusion-20220819-7EF0H4"
]

Test[
    Daniel`ARC`ARCGroupRulesByConclusion[{<||> -> <|"b" -> 1|>}]
    ,
    {<||> -> <|"b" -> 1|>}
    ,
    TestID -> "ARCGroupRulesByConclusion-20220819-I9OHVX"
]

Test[
    Daniel`ARC`ARCGroupRulesByConclusion[
        {<||> -> <|"b" -> 1|>, <|"a" -> 1|> -> <|"b" -> 2|>}
    ]
    ,
    {<||> -> <|"b" -> 1|>, <|"a" -> 1|> -> <|"b" -> 2|>}
    ,
    TestID -> "ARCGroupRulesByConclusion-20220819-ZFDBLZ"
]