(*
    Tests for: Daniel`ARC`ARCExpressionComplexity
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCExpressionComplexity]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCExpressionComplexity[123]
    ]
    ,
    0.03
    ,
    TestID -> "ARCExpressionComplexity-20220825-WFZ99T"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCExpressionComplexity[{1, 2, 3}]
    ]
    ,
    0.06
    ,
    TestID -> "ARCExpressionComplexity-20220825-0A6DDL"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCExpressionComplexity[<|"a" -> 1|> -> <|"b" -> 2|>]
    ]
    ,
    0.15
    ,
    TestID -> "ARCExpressionComplexity-20220825-GVP5VR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCExpressionComplexity[<|"Shape" -> <|"Name" -> "Square"|>|>]
    ]
    ,
    0.15
    ,
    TestID -> "ARCExpressionComplexity-20220825-821RT1"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCExpressionComplexity[
            <|"Height" -> Inactive[Times][Daniel`ARC`ObjectValue["Parent", "Height"], 1/2]|>
        ]
    ]
    ,
    0.59
    ,
    TestID -> "ARCExpressionComplexity-20221111-QLYIUR"
]