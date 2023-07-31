(*
    Tests for: Daniel`ARC`ARCMovementAngle
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMovementAngle]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCMovementAngle[
        "MoveTransform",
        <|"Type" -> "Move", "Position" -> <|"Y" -> 3, "X" -> 3|>, "Offset" -> <|"X" -> 2|>|>
    ]
    ,
    0
    ,
    TestID -> "ARCMovementAngle-20221110-3BSYP0"
]

Test[
    Daniel`ARC`ARCMovementAngle[
        First[
            Utility`ReturnIfFailure[
                Daniel`ARC`ARCParseInputAndOutputScenes[
                    Daniel`ARC`ARCParseFile["5168d44c"]["Train"],
                    "FormMultiColorCompositeObjects" -> False
                ]
            ]
        ]
    ]
    ,
    0
    ,
    TestID -> "ARCMovementAngle-20221110-QFCQCC"
]

Test[
    Daniel`ARC`ARCMovementAngle[
        Utility`ReturnIfFailure[
            Daniel`ARC`ARCParseInputAndOutputScenes[
                Daniel`ARC`ARCParseFile["5168d44c"]["Train"],
                "FormMultiColorCompositeObjects" -> False
            ]
        ][[
            2
        ]]
    ]
    ,
    90
    ,
    TestID -> "ARCMovementAngle-20221110-90J0JK"
]