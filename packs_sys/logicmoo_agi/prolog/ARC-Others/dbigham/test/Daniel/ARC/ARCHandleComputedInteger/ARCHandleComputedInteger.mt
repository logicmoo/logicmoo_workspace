(*
    Tests for: Daniel`ARC`ARCHandleComputedInteger
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCHandleComputedInteger]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCHandleComputedInteger[0.99999, Inactive[Plus][0, 1]]
    ,
    1
    ,
    TestID -> "ARCHandleComputedInteger-20221010-7ECVXP"
]

Test[
    Module[{}, Daniel`ARC`ARCHandleComputedInteger[0.9, Inactive[Plus][0, 0.9]]]
    ,
    Failure[
        "NumericFailure",
        <|
            "MessageTemplate" -> "An expression produced a Real number, which is not currently supported.",
            "MessageParameters" -> <||>,
            "Expression" -> Inactive[Plus][0, 0.9],
            "EvaluationResult" -> 0.9
        |>
    ]
    ,
    TestID -> "ARCHandleComputedInteger-20221010-9YXVRA"
]

Test[
    Daniel`ARC`ARCHandleComputedInteger[1., Inactive[Plus][0, 1.]]
    ,
    1
    ,
    TestID -> "ARCHandleComputedInteger-20221010-TU51OQ"
]