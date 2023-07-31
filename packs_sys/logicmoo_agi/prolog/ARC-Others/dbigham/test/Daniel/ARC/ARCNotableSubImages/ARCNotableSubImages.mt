(*
    Tests for: Daniel`ARC`ARCNotableSubImages
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCNotableSubImages]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCNotableSubImages[
            {
                <|"Image" -> Daniel`ARC`ARCScene[{{1, -1}, {-1, 2}}]|>,
                <|"Image" -> Daniel`ARC`ARCScene[{{1, 1}}]|>,
                <|"Image" -> Daniel`ARC`ARCScene[{{1, -1, 1}, {-1, 1, -1}, {1, -1, 2}}]|>
            },
            3,
            3
        ]
    ]
    ,
    {
        <|
            "Image" -> {{1, -1}, {-1, 2}},
            "Count" -> 1,
            "ExampleObjects" -> {<|"Image" -> Daniel`ARC`ARCScene[{{1, -1}, {-1, 2}}]|>}
        |>
    }
    ,
    TestID -> "ARCNotableSubImages-20220725-9BDG1I"
]