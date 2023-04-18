(*
    Tests for: Daniel`ARC`ARCSortRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSortRules]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSortRules[
        {
            <|"Area.Rank" -> 2|> -> <|
                "Shape" -> <|
                    "Name" -> "Rectangle",
                    "Filled" -> True,
                    "Interior" -> <|"Color" -> 1|>,
                    "Border" -> <|"Color" -> 2|>
                |>
            |>,
            <|"Area.Rank" -> 1|> -> <|
                "Shape" -> <|
                    "Name" -> "Rectangle",
                    "Filled" -> True,
                    "Interior" -> <|"Color" -> 3|>,
                    "Border" -> <|"Color" -> 4|>
                |>
            |>
        }
    ]
    ,
    {
        <|"Area.Rank" -> 1|> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 3|>,
                "Border" -> <|"Color" -> 4|>
            |>
        |>,
        <|"Area.Rank" -> 2|> -> <|
            "Shape" -> <|
                "Name" -> "Rectangle",
                "Filled" -> True,
                "Interior" -> <|"Color" -> 1|>,
                "Border" -> <|"Color" -> 2|>
            |>
        |>
    }
    ,
    TestID -> "ARCSortRules-20220906-XS5QWM"
]