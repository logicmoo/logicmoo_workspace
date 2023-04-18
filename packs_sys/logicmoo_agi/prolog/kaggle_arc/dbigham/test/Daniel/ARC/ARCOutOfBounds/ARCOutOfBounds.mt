(*
    Tests for: Daniel`ARC`ARCOutOfBounds
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCOutOfBounds]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCOutOfBounds[{0, 1}, {{1, 0}, {0, 1}}]
    ,
    True
    ,
    TestID -> "ARCOutOfBounds-20220803-KLTMP3"
]

Test[
    Daniel`ARC`ARCOutOfBounds[{3, 1}, {{1, 0}, {0, 1}}]
    ,
    True
    ,
    TestID -> "ARCOutOfBounds-20220803-9EWJCH"
]

Test[
    Daniel`ARC`ARCOutOfBounds[{1, 0}, {{1, 0}, {0, 1}}]
    ,
    True
    ,
    TestID -> "ARCOutOfBounds-20220803-9HCC1R"
]

Test[
    Daniel`ARC`ARCOutOfBounds[{1, 3}, {{1, 0}, {0, 1}}]
    ,
    True
    ,
    TestID -> "ARCOutOfBounds-20220803-4DRUF7"
]

Test[
    Daniel`ARC`ARCOutOfBounds[{1, 1}, {{1, 0}, {0, 1}}]
    ,
    False
    ,
    TestID -> "ARCOutOfBounds-20220803-0R8A4J"
]

Test[
    Daniel`ARC`ARCOutOfBounds[{2, 2}, {{1, 0}, {0, 1}}]
    ,
    False
    ,
    TestID -> "ARCOutOfBounds-20220803-YXYWQP"
]

Test[
    Daniel`ARC`ARCOutOfBounds[{-1, 1}, {{1, 0}, {0, 1}}]
    ,
    True
    ,
    TestID -> "ARCOutOfBounds-20221002-XUZVPR"
]