(*
    Tests for: Daniel`ARC`ARCTurnDegrees
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCTurnDegrees]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {0, 1}, {6, 5}]
    ,
    90
    ,
    TestID -> "ARCTurnDegrees-20220915-9U1UUV"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {0, 1}, {4, 5}]
    ,
    -90
    ,
    TestID -> "ARCTurnDegrees-20220915-PKFW6V"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {0, 1}, {6, 6}]
    ,
    45
    ,
    TestID -> "ARCTurnDegrees-20220915-K2FZNF"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {0, 1}, {4, 6}]
    ,
    -45
    ,
    TestID -> "ARCTurnDegrees-20220915-RNPZKF"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {0, 1}, {4, 4}]
    ,
    -135
    ,
    TestID -> "ARCTurnDegrees-20220915-7SC6LR"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {-1, 0}, {5, 6}]
    ,
    90
    ,
    TestID -> "ARCTurnDegrees-20220915-JX5Y9F"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {-1, 0}, {5, 4}]
    ,
    -90
    ,
    TestID -> "ARCTurnDegrees-20220915-JI8P7X"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {-1, 0}, {4, 6}]
    ,
    45
    ,
    TestID -> "ARCTurnDegrees-20220915-CD6JMJ"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {-1, 0}, {4, 4}]
    ,
    -45
    ,
    TestID -> "ARCTurnDegrees-20220915-AER71M"
]

Test[
    Daniel`ARC`ARCTurnDegrees[{5, 5}, {-1, 0}, {6, 4}]
    ,
    -135
    ,
    TestID -> "ARCTurnDegrees-20220915-DCQ87D"
]