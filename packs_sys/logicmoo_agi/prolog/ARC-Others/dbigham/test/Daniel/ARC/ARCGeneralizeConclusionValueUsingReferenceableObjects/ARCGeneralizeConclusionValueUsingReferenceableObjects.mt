(*
    Tests for: Daniel`ARC`ARCGeneralizeConclusionValueUsingReferenceableObjects
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCGeneralizeConclusionValueUsingReferenceableObjects]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCGeneralizeConclusionValueUsingReferenceableObjects[
                {"Y"},
                {
                    <|"Value" -> 2, "Example" -> 1|>,
                    <|"Value" -> 6, "Example" -> 2|>,
                    <|"Value" -> 3, "Example" -> 3|>
                },
                <|
                    Daniel`ARC`Object[<|"Colors" -> {1}|>] -> <|
                        "Height" -> 2,
                        "Shapes" -> {<|"Name" -> "Rectangle"|>}
                    |>,
                    Daniel`ARC`Object[<|"Colors" -> {2}|>] -> <|
                        "Height" -> 2,
                        "Shapes" -> {<|"Name" -> "Rectangle"|>}
                    |>,
                    Daniel`ARC`Object[<|"Colors" -> {4}|>] -> <|
                        "Height" -> 2,
                        "Shapes" -> {<|"Name" -> "Rectangle"|>}
                    |>
                |>,
                Daniel`ARC`ARCParseExamples[file = "1caeab9d"]
            ]
        ]
    ]
    ,
    Daniel`ARC`ObjectValue[<|"Colors" -> {1}|>, "Y"]
    ,
    TestID -> "ARCGeneralizeConclusionValueUsingReferenceableObjects-20220724-ZD9J9Z"
]