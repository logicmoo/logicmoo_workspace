(*
    Tests for: Daniel`ARC`ARCConstantScore
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCConstantScore]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCConstantScore[1]
    ,
    0
    ,
    TestID -> "ARCConstantScore-20221010-H7D45S"
]

Test[
    Daniel`ARC`ARCConstantScore[2]
    ,
    -0.1
    ,
    TestID -> "ARCConstantScore-20221010-LB8MV6"
]

Test[
    Daniel`ARC`ARCConstantScore[3]
    ,
    -0.2
    ,
    TestID -> "ARCConstantScore-20221010-AM7ECT"
]

Test[
    Daniel`ARC`ARCConstantScore[5]
    ,
    -0.3
    ,
    TestID -> "ARCConstantScore-20221010-67GZLT"
]

Test[
    Daniel`ARC`ARCConstantScore[10]
    ,
    -0.5
    ,
    TestID -> "ARCConstantScore-20221010-5X4QXG"
]

Test[
    Daniel`ARC`ARCConstantScore[11]
    ,
    -0.555
    ,
    TestID -> "ARCConstantScore-20221010-YI8MMR"
]

Test[
    Daniel`ARC`ARCConstantScore[15]
    ,
    -0.575
    ,
    TestID -> "ARCConstantScore-20221010-EMV4MK"
]

Test[
    Daniel`ARC`ARCConstantScore[20]
    ,
    -0.6
    ,
    TestID -> "ARCConstantScore-20221010-OB7U39"
]

Test[
    Daniel`ARC`ARCConstantScore[99]
    ,
    -0.995
    ,
    TestID -> "ARCConstantScore-20221010-1BPNI2"
]

Test[
    Daniel`ARC`ARCConstantScore[100]
    ,
    -2.
    ,
    TestID -> "ARCConstantScore-20221010-C60FVR"
]

Test[
    Daniel`ARC`ARCConstantScore[500]
    ,
    -2.
    ,
    TestID -> "ARCConstantScore-20221010-BO9B2J"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstantScore[0.14285699999999998]
    ]
    ,
    -32.
    ,
    TestID -> "ARCConstantScore-20221010-IT5TCV"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstantScore[N[1/2]]
    ]
    ,
    -0.1
    ,
    TestID -> "ARCConstantScore-20221010-RA78NS"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstantScore[N[1/3]]
    ]
    ,
    -0.2
    ,
    TestID -> "ARCConstantScore-20221010-V6DAZN"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstantScore[N[1/10]]
    ]
    ,
    -0.5
    ,
    TestID -> "ARCConstantScore-20221010-0PO8MG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConstantScore[N[1/100]]
    ]
    ,
    -2.
    ,
    TestID -> "ARCConstantScore-20221010-FR6HTR"
]