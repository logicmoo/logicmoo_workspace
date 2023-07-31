(*
    Tests for: Daniel`ARC`ARCDirectionForAngle
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCDirectionForAngle]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCDirectionForAngle[{-1, 0}, 90]
    ,
    {0, 1}
    ,
    TestID -> "ARCDirectionForAngle-20220915-X6N7P0"
]

Test[
    Daniel`ARC`ARCDirectionForAngle[{-1, 1}, 90]
    ,
    {1, 1}
    ,
    TestID -> "ARCDirectionForAngle-20220915-WSJE1S"
]

Test[
    Daniel`ARC`ARCDirectionForAngle[{0, 1}, 90]
    ,
    {1, 0}
    ,
    TestID -> "ARCDirectionForAngle-20220915-RJPJDK"
]

Test[
    Daniel`ARC`ARCDirectionForAngle[{1, 1}, 90]
    ,
    {1, -1}
    ,
    TestID -> "ARCDirectionForAngle-20220915-5L965F"
]

Test[
    Daniel`ARC`ARCDirectionForAngle[{1, 0}, 90]
    ,
    {0, -1}
    ,
    TestID -> "ARCDirectionForAngle-20220915-LZ0F1L"
]

Test[
    Daniel`ARC`ARCDirectionForAngle[{1, -1}, 90]
    ,
    {-1, -1}
    ,
    TestID -> "ARCDirectionForAngle-20220915-8G6S6J"
]

Test[
    Daniel`ARC`ARCDirectionForAngle[{0, -1}, 90]
    ,
    {-1, 0}
    ,
    TestID -> "ARCDirectionForAngle-20220915-MZ6YCA"
]

Test[
    Daniel`ARC`ARCDirectionForAngle[{-1, -1}, 90]
    ,
    {-1, 1}
    ,
    TestID -> "ARCDirectionForAngle-20220915-NR0R9L"
]