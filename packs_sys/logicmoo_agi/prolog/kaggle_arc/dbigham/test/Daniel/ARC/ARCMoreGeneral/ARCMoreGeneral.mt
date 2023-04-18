(*
    Tests for: Daniel`ARC`ARCMoreGeneral
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCMoreGeneral]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCMoreGeneral["Shape", "Rectangle", "Square"]
    ]
    ,
    True
    ,
    TestID -> "ARCMoreGeneral-20220810-3EDB4X"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCMoreGeneral["Shape", "Square", "Rectangle"]
    ]
    ,
    False
    ,
    TestID -> "ARCMoreGeneral-20220810-8Q6IDB"
]

Test[
    Daniel`ARC`ARCMoreGeneral["Shape", "Square", "Square"]
    ,
    False
    ,
    TestID -> "ARCMoreGeneral-20220810-O7FK5G"
]

Test[
    Daniel`ARC`ARCMoreGeneral["MyType", "Rectangle", "Square"]
    ,
    Missing["Unknown"]
    ,
    TestID -> "ARCMoreGeneral-20220810-IVCAD1"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCMoreGeneral[
            "Shape",
            <|"Name" -> "Rectangle"|>,
            <|"Name" -> "Square"|>
        ]
    ]
    ,
    True
    ,
    TestID -> "ARCMoreGeneral-20220810-AH9ZMJ"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCMoreGeneral["Property" -> "Shapes", "Rectangle", "Square"]
    ]
    ,
    True
    ,
    TestID -> "ARCMoreGeneral-20220810-TKT2JR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCMoreGeneral["Property" -> "UnknownProperty", "Rectangle", "Square"]
    ]
    ,
    Missing["KeyAbsent", "UnknownProperty"]
    ,
    TestID -> "ARCMoreGeneral-20220810-Y9118W"
]

Test[
    Daniel`ARC`ARCMoreGeneral["Shape", "A", "B"]
    ,
    False
    ,
    TestID -> "ARCMoreGeneral-20220810-JK08TR"
]

Test[
    Daniel`ARC`ARCMoreGeneral["Shape", <|"Name" -> "Rectangle"|>, <|"Name" -> "Pixel"|>]
    ,
    True
    ,
    TestID -> "ARCMoreGeneral-20220811-VFC61Q"
]

Test[
    Daniel`ARC`ARCMoreGeneral["Shape", <|"Name" -> "Pixel"|>, <|"Name" -> "Rectangle"|>]
    ,
    False
    ,
    TestID -> "ARCMoreGeneral-20220811-G927FK"
]

Test[
    Daniel`ARC`ARCMoreGeneral["Shape", <|"Name" -> "Square"|>, <|"Name" -> "Pixel"|>]
    ,
    True
    ,
    TestID -> "ARCMoreGeneral-20220811-AM6OCO"
]