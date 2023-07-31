(*
    Tests for: Daniel`ARC`ReturnIfDifferingInputAndOutputSize
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ReturnIfDifferingInputAndOutputSize]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Module[
                {},
                Daniel`ARC`ReturnIfDifferingInputAndOutputSize[
                    Utility`ReturnIfFailure[Daniel`ARC`ARCParseFile["007bbfb7"]]["Train"]
                ];
                "HERE"
            ]
        ]
    ]
    ,
    Missing["NotImplemented", "DifferingInputAndOutputSize"]
    ,
    TestID -> "ReturnIfDifferingInputAndOutputSize-20220725-PSI0FW"
]