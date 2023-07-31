(*
    Tests for: Daniel`ARC`ARCSimplifyRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSimplifyRules]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSimplifyRules[
        {
            <|"Colors" -> {2}|> -> <|
                "Transform" -> <|
                    "Type" -> "Move",
                    "BlockedBy" -> Daniel`ARC`Object[<|"Colors" -> {8}|>]
                |>,
                "Examples" -> {1, 2, 3},
                "ExampleCount" -> 3,
                "UseCount" -> 3
            |>,
            <|"Colors" -> {8}|> -> <|
                "Same" -> True,
                "Examples" -> {1, 2, 3},
                "ExampleCount" -> 3,
                "UseCount" -> 3
            |>
        }
    ]
    ,
    {
        <|"Colors" -> {2}|> -> <|
            "Transform" -> <|
                "Type" -> "Move",
                "BlockedBy" -> Daniel`ARC`Object[<|"Colors" -> {8}|>]
            |>
        |>,
        <|"Colors" -> {8}|> -> <|"Same" -> True|>
    }
    ,
    TestID -> "ARCSimplifyRules-20220804-4J2LL8"
]