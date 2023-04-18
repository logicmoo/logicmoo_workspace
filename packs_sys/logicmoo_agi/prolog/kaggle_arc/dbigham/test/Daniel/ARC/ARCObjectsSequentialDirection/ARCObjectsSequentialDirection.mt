(*
    Tests for: Daniel`ARC`ARCObjectsSequentialDirection
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCObjectsSequentialDirection]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCObjectsSequentialDirection[
        Utility`ReturnIfFailure[
            Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["0a938d79"]["Train"][[1, "Output"]]
            ]
        ][
            "Objects"
        ]
    ]
    ,
    0
    ,
    TestID -> "ARCObjectsSequentialDirection-20221108-6W3D5O"
]

Test[
    Daniel`ARC`ARCObjectsSequentialDirection[
        Utility`ReturnIfFailure[
            Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["0a938d79"]["Train"][[3, "Output"]]
            ]
        ][
            "Objects"
        ]
    ]
    ,
    90
    ,
    TestID -> "ARCObjectsSequentialDirection-20221108-GYZEAK"
]

Test[
    Daniel`ARC`ARCObjectsSequentialDirection[
        Utility`ReturnIfFailure[
            Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["8ee62060"]["Train"][[1, "Input"]],
                "FollowDiagonals" -> False
            ]
        ][
            "Objects"
        ]
    ]
    ,
    135
    ,
    TestID -> "ARCObjectsSequentialDirection-20221108-SGTXUY"
]

Test[
    Daniel`ARC`ARCObjectsSequentialDirection[
        Utility`ReturnIfFailure[
            Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["8ee62060"]["Train"][[1, "Output"]]
            ]
        ][
            "Objects"
        ]
    ]
    ,
    45
    ,
    TestID -> "ARCObjectsSequentialDirection-20221108-ID1HOK"
]

Test[
    Daniel`ARC`ARCObjectsSequentialDirection[
        Utility`ReturnIfFailure[
            Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["5168d44c"]["Train"][[1, "Input"]],
                "FormMultiColorCompositeObjects" -> False
            ]
        ][
            "Objects"
        ]
    ]
    ,
    0
    ,
    TestID -> "ARCObjectsSequentialDirection-20221110-J3WUOB"
]