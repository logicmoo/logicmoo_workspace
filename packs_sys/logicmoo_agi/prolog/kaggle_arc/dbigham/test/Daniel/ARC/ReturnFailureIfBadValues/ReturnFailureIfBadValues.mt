(*
    Tests for: Daniel`ARC`ReturnFailureIfBadValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ReturnFailureIfBadValues]
    
    Author: danielb
*)

Test[
    Module[{}, Daniel`ARC`ReturnFailureIfBadValues[<|"Y" -> 0.5|>]]
    ,
    Failure[
        "IntegerExpected",
        <|
            "MessageTemplate" -> "The property Y is expected to have an integer value.",
            "MessageParameters" -> <||>,
            "Value" -> 0.5
        |>
    ]
    ,
    TestID -> "ReturnFailureIfBadValues-20221022-Q7XU1O"
]

Test[
    Module[{}, Daniel`ARC`ReturnFailureIfBadValues[<|"Y" -> 1|>]]
    ,
    Null
    ,
    TestID -> "ReturnFailureIfBadValues-20221022-OESQEB"
]

Test[
    Module[
        {},
        Daniel`ARC`ReturnFailureIfBadValues[
            <|
                "Y" -> 1,
                "Components" -> {
                    "Position" -> <|"RelativePosition" -> <|"Y" -> -3.25, "X" -> 8|>|>
                }
            |>
        ]
    ]
    ,
    Failure[
        "IntegerExpected",
        <|
            "MessageTemplate" -> "The property Y is expected to have an integer value.",
            "MessageParameters" -> <||>,
            "Value" -> -3.25
        |>
    ]
    ,
    TestID -> "ReturnFailureIfBadValues-20221022-7U378A"
]