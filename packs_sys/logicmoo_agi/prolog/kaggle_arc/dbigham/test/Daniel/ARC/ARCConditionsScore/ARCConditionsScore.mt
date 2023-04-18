(*
    Tests for: Daniel`ARC`ARCConditionsScore
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCConditionsScore]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Color" -> 1|>]
    ]
    ,
    -0.07269434305704396
    ,
    TestID -> "ARCConditionsScore-20220825-IHY3H7"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Shape" -> <|"Name" -> "Square"|>|>]
    ]
    ,
    0.13228756555322946
    ,
    TestID -> "ARCConditionsScore-20220825-CWZNUU"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"X" -> 1|>]
    ]
    ,
    -0.5008991914547277
    ,
    TestID -> "ARCConditionsScore-20220825-GWXD0K"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Shapes" -> <|"Name" -> "Square"|>|>]
    ]
    ,
    0.11146748404803987
    ,
    TestID -> "ARCConditionsScore-20220919-H71RYH"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Area.Rank" -> 1|>]
    ]
    ,
    -0.06244997998398395
    ,
    TestID -> "ARCConditionsScore-20220919-33M71R"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Area.Rank" -> Except[1]|>]
    ]
    ,
    -0.15198684153570663
    ,
    TestID -> "ARCConditionsScore-20220919-3BOBSB"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Shapes" -> <|"Name" -> Except["Square"]|>|>]
    ]
    ,
    -0.13991068579633215
    ,
    TestID -> "ARCConditionsScore-20220919-BISAI5"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Shape" -> <|"Name" -> Except["Square"]|>|>]
    ]
    ,
    -0.11357816691600557
    ,
    TestID -> "ARCConditionsScore-20220919-SEP3PR"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> 4 | 5|>]
    ]
    ,
    -0.32493012291119844
    ,
    TestID -> "ARCConditionsScore-20220920-5JHCEE"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> 4 | 5 | 6|>]
    ]
    ,
    -0.6181591714483211
    ,
    TestID -> "ARCConditionsScore-20220920-JZT7AD"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> 4 | 5 | 6 | 7|>]
    ]
    ,
    -1.1122751707034197
    ,
    TestID -> "ARCConditionsScore-20220920-TQQ6OA"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> 4 | 5 | 6 | 7 | 8|>]
    ]
    ,
    -2.1056178769184632
    ,
    TestID -> "ARCConditionsScore-20220920-XHYMVG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> 4 | 5 | 6 | 7 | 8 | 9 | 10|>]
    ]
    ,
    -4.104055045935568
    ,
    TestID -> "ARCConditionsScore-20220920-KI6U47"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> 4 | 6|>]
    ]
    ,
    -1.2969387699542259
    ,
    TestID -> "ARCConditionsScore-20220920-3TIQ48"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> 4 | 5|>]
    ]
    ,
    -0.32493012291119844
    ,
    TestID -> "ARCConditionsScore-20220920-F2LJT4"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCConditionsScore[<|"Height" -> "a" | "c"|>]
    ]
    ,
    -0.32493012291119844
    ,
    TestID -> "ARCConditionsScore-20220920-QUF9IB"
]