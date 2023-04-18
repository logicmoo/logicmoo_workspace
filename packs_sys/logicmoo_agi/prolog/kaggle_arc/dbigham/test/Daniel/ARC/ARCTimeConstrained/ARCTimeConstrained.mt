(*
    Tests for: Daniel`ARC`ARCTimeConstrained
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCTimeConstrained]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCTimeConstrained[Pause[0.15], Quantity[100, "Milliseconds"]]
    ,
    Failure[
        "TimeLimitExceeded",
        <|
            "MessageTemplate" -> "The expression took too long to evaluate.",
            "MessageParameters" -> <||>,
            "MaximumTime" -> Quantity[100, "Milliseconds"],
            "Expression" -> Hold[Pause[0.15]]
        |>
    ]
    ,
    TestID -> "ARCTimeConstrained-20220918-NWNL6F"
]

Test[
    Daniel`ARC`ARCTimeConstrained[
        Pause[0.05];
        123,
        Quantity[200, "Milliseconds"]
    ]
    ,
    123
    ,
    TestID -> "ARCTimeConstrained-20220918-PDW0E2"
]

Test[
    Daniel`ARC`ARCTimeConstrained[Quantity[20, "Milliseconds"]][Pause[0.05]]
    ,
    Failure[
        "TimeLimitExceeded",
        <|
            "MessageTemplate" -> "The expression took too long to evaluate.",
            "MessageParameters" -> <||>,
            "MaximumTime" -> Quantity[20, "Milliseconds"],
            "Expression" -> Hold[Pause[0.05]]
        |>
    ]
    ,
    TestID -> "ARCTimeConstrained-20220918-5ZZXN9"
]