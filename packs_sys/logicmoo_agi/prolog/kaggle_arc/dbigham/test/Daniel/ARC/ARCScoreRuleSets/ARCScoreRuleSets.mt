(*
    Tests for: Daniel`ARC`ARCScoreRuleSets
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCScoreRuleSets]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCScoreRuleSets[
            {
                {<|"Color" -> 1|> -> <|"X" -> 1|>, <|"Color" -> 2|> -> <|"X" -> 2|>},
                {<|"X" -> 1|> -> <|"Color" -> 1|>, <|"X" -> 2|> -> <|"Color" -> 2|>}
            }
        ]
    ]
    ,
    <|
        {<|"Color" -> 1|> -> <|"X" -> 1|>, <|"Color" -> 2|> -> <|"X" -> 2|>} -> -0.19742116167907758,
        {<|"X" -> 1|> -> <|"Color" -> 1|>, <|"X" -> 2|> -> <|"Color" -> 2|>} -> -1.0191866507758092
    |>
    ,
    TestID -> "ARCScoreRuleSets-20220825-RFGVRT"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCScoreRuleSets[
            {
                <|
                    "Rules" -> {
                        <|"Color" -> 1|> -> <|"X" -> 1|>,
                        <|"Color" -> 2|> -> <|"X" -> 2|>
                    }
                |>,
                <|
                    "Rules" -> {
                        <|"X" -> 1|> -> <|"Color" -> 1|>,
                        <|"X" -> 2|> -> <|"Color" -> 2|>
                    }
                |>
            }
        ]
    ]
    ,
    <|
        <|"Rules" -> {<|"Color" -> 1|> -> <|"X" -> 1|>, <|"Color" -> 2|> -> <|"X" -> 2|>}|> -> -0.19742116167907758,
        <|"Rules" -> {<|"X" -> 1|> -> <|"Color" -> 1|>, <|"X" -> 2|> -> <|"Color" -> 2|>}|> -> -1.0191866507758092
    |>
    ,
    TestID -> "ARCScoreRuleSets-20220905-HCS1LX"
]