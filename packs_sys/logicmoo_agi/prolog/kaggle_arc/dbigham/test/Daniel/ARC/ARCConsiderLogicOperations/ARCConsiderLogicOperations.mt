(*
    Tests for: Daniel`ARC`ARCConsiderLogicOperations
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCConsiderLogicOperations]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            With[
                {
                    parsedExamples = Daniel`ARC`ARCParseInputAndOutputScenes[
                        Daniel`ARC`ARCParseFile["0520fde7"]["Train"]
                    ]
                },
                Daniel`ARC`ARCConsiderLogicOperations[
                    parsedExamples,
                    Daniel`ARC`ARCCheckForLogicOperationQ[parsedExamples]
                ]
            ]
        ]
    ]
    ,
    <|{1, 0} -> 0, {0, 0} -> 0, {0, 1} -> 0, {1, 1} -> 1|>
    ,
    TestID -> "ARCConsiderLogicOperations-20220925-DMNH1U"
]