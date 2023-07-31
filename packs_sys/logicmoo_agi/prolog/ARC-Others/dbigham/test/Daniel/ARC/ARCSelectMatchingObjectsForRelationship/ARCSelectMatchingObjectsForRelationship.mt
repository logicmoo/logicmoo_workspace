(*
    Tests for: Daniel`ARC`ARCSelectMatchingObjectsForRelationship
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSelectMatchingObjectsForRelationship]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSelectMatchingObjectsForRelationship[
        "Within",
        Daniel`ARC`Object[<|"Color" -> "Gray"|>],
        {
            <|
                "Colors" -> {"Gray"},
                "Color" -> "Gray",
                "Position" -> {1, 1},
                "Width" -> 10,
                "Height" -> 10
            |>,
            <|"Colors" -> {"Blue"}, "Position" -> {5, 5}, "Width" -> 1, "Height" -> 1|>,
            <|"Colors" -> {"Red"}, "Position" -> {15, 15}, "Width" -> 1, "Height" -> 1|>
        },
        {
            <|
                "Colors" -> {"Gray"},
                "Color" -> "Gray",
                "Position" -> {1, 1},
                "Width" -> 10,
                "Height" -> 10
            |>,
            <|"Colors" -> {"Blue"}, "Position" -> {5, 5}, "Width" -> 1, "Height" -> 1|>,
            <|"Colors" -> {"Red"}, "Position" -> {15, 15}, "Width" -> 1, "Height" -> 1|>
        }
    ]
    ,
    {<|"Colors" -> {"Blue"}, "Position" -> {5, 5}, "Width" -> 1, "Height" -> 1|>}
    ,
    TestID -> "ARCSelectMatchingObjectsForRelationship-20221016-3IUJOA"
]