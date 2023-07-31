(*
    Tests for: Daniel`ARC`ARCSelectMatchingObject
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSelectMatchingObject]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSelectMatchingObject[
        {
            <|"UUID" -> 1, "Image" -> {{1, 0}, {0, 1}}, "Position" -> {1, 1}|>,
            <|"UUID" -> 2, "Image" -> {{0, 1}, {1, 0}}, "Position" -> {3, 3}|>
        },
        <|"Image" -> {{1, 0}, {0, 1}}, "Position" -> {1, 1}|>
    ]
    ,
    <|"UUID" -> 1, "Image" -> {{1, 0}, {0, 1}}, "Position" -> {1, 1}|>
    ,
    TestID -> "ARCSelectMatchingObject-20220724-QH5531"
]

Test[
    Daniel`ARC`ARCSelectMatchingObject[
        {<|"UUID" -> 2, "Image" -> {{0, 1}, {1, 0}}, "Position" -> {3, 3}|>},
        <|"Image" -> {{1, 0}, {0, 1}}, "Position" -> {1, 1}|>
    ]
    ,
    Missing["NotFound"]
    ,
    TestID -> "ARCSelectMatchingObject-20220724-MZ2YRP"
]