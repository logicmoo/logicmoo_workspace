(*
    Tests for: Daniel`ARC`ARCGeneralizeValue
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGeneralizeValue]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCGeneralizeValue[
            {1, 2, 3},
            {2, 4, 6},
            Daniel`ARC`ObjectValue["InputScene", "MyValue"]
        ]
    ]
    ,
    Inactive[Daniel`ARC`ObjectValue["InputScene", "MyValue"]*2]
    ,
    TestID -> "ARCGeneralizeValue-20220903-5FMURC"
]

Test[
    Daniel`ARC`ARCGeneralizeValue[
        {1, 2, 3},
        {2, 2, 2},
        Daniel`ARC`ObjectValue["InputScene", "MyValue"]
    ]
    ,
    2
    ,
    TestID -> "ARCGeneralizeValue-20220903-4BRKH7"
]

Test[
    Daniel`ARC`ARCGeneralizeValue[
        {1, 2, 3},
        {2, 4, 9},
        Daniel`ARC`ObjectValue["InputScene", "MyValue"]
    ]
    ,
    Missing[]
    ,
    TestID -> "ARCGeneralizeValue-20220903-Y6PT4N"
]

Test[
    Daniel`ARC`ARCGeneralizeValue[
        {1, 2, 3},
        {1, 2, 3},
        Daniel`ARC`ObjectValue["InputScene", "MyValue"]
    ]
    ,
    Missing[]
    ,
    TestID -> "ARCGeneralizeValue-20220903-6TG8SW"
]

Test[
    Daniel`ARC`ARCGeneralizeValue[
        {1, 2, 3},
        {0, 0, 0},
        Daniel`ARC`ObjectValue["InputScene", "MyValue"],
        "OnlyIfLarger" -> True
    ]
    ,
    Missing[]
    ,
    TestID -> "ARCGeneralizeValue-20220903-DI2VGX"
]

Test[
    Daniel`ARC`ARCGeneralizeValue[
        {1, 2, 3},
        {2, 2, 2},
        Daniel`ARC`ObjectValue["InputScene", "MyValue"],
        "OnlyIfLarger" -> True
    ]
    ,
    2
    ,
    TestID -> "ARCGeneralizeValue-20220903-Q0W7JB"
]

Test[
    Daniel`ARC`ARCGeneralizeValue[
        {1, 2, 3},
        {2, 4, 6},
        Daniel`ARC`ObjectValue["InputScene", "MyValue"],
        "OnlyIfLarger" -> True
    ]
    ,
    Inactive[Daniel`ARC`ObjectValue["InputScene", "MyValue"]*2]
    ,
    TestID -> "ARCGeneralizeValue-20220903-799Z8M"
]

Test[
    Daniel`ARC`ARCGeneralizeValue[
        {2, 4, 6},
        {1, 2, 3},
        Daniel`ARC`ObjectValue["InputScene", "MyValue"],
        "OnlyIfLarger" -> True
    ]
    ,
    Missing[]
    ,
    TestID -> "ARCGeneralizeValue-20220903-Z0MBME"
]