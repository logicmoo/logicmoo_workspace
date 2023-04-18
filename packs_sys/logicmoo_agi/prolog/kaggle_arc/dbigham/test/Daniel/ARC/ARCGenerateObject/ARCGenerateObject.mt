(*
    Tests for: Daniel`ARC`ARCGenerateObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGenerateObject]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCGenerateObject[
                Replace[
                    Daniel`ARC`ARCParseFile["e5790162"]["Train"][[1, "Input"]],
                    0 -> Daniel`ARC`Private`$nonImageColor,
                    {3}
                ],
                <|"Position" -> {3, 1}, "Color" -> 3|>,
                <|
                    "Type" -> "GenerateObject",
                    "StartPosition" -> <|"RelativePosition" -> <|"Y" -> 0, "X" -> 0|>|>,
                    "Color" -> 3,
                    "Rules" -> {
                        <|"ColorAhead" -> Missing[]|> -> <|"OutgoingDirection" -> {0, 1}|>,
                        <|"ColorAhead" -> -1|> -> <|"TurnDegrees" -> 0|>,
                        <|"ColorAhead" -> 6|> -> <|"TurnDegrees" -> 90|>,
                        <|"ColorAhead" -> -2|> -> <|"Stop" -> True|>,
                        <|"ColorAhead" -> 8|> -> <|"TurnDegrees" -> -90|>
                    }
                |>
            ]
        ]
    ]
    ,
    <|
        "Image" -> Daniel`ARC`ARCScene[{{3, 3, 3}, {-1, -1, 3}, {-1, -1, 3}, {-1, -1, 3}}],
        "Position" -> {3, 1},
        "Y" -> 3,
        "X" -> 1,
        "Width" -> 3,
        "Height" -> 4
    |>
    ,
    TestID -> "ARCGenerateObject-20220922-41B4JQ"
]