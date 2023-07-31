(*
    Tests for: Daniel`ARC`Replace2
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`Replace2]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Module[
            {temporaryAssociationSymbol},
            Daniel`ARC`Replace2[
                <|1 -> 2, 3 -> 4|>,
                temporaryAssociationSymbol,
                1 -> 999,
                {0, Infinity}
            ]
        ]
    ]
    ,
    <|999 -> 2, 3 -> 4|>
    ,
    TestID -> "Replace2-20220719-UNI1C2"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Module[
            {temporaryAssociationSymbol},
            Daniel`ARC`Replace2[
                <|<|1 -> 2|> -> 3, 4 -> 5|>,
                temporaryAssociationSymbol,
                temporaryAssociationSymbol[___, 1 -> _, ___] :> <|1 -> "HERE"|>,
                {0, Infinity}
            ]
        ]
    ]
    ,
    <|<|1 -> "HERE"|> -> 3, 4 -> 5|>
    ,
    TestID -> "Replace2-20220719-6SHITM"
]