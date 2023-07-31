(*
    Tests for: Daniel`ARC`ARCAddedObjectsMapping
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCAddedObjectsMapping]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCAddedObjectsMapping[
            {<|"UUID" -> 1|>, <|"UUID" -> 2|>, <|"UUID" -> 3|>}
        ]
    ]
    ,
    "AddObjects" -> <|
        "Transform" -> <|
            "Type" -> "AddObjects",
            "Objects" -> {<|"UUID" -> 1|>, <|"UUID" -> 2|>, <|"UUID" -> 3|>}
        |>
    |>
    ,
    TestID -> "ARCAddedObjectsMapping-20220820-RN9LF0"
]

Test[
    ToString[Daniel`ARC`ARCAddedObjectsMapping[{}]]
    ,
    "Nothing"
    ,
    TestID -> "ARCAddedObjectsMapping-20220820-8ADGPU"
]