(*
    Tests for: Daniel`ARC`ARCInferRankProperties
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCInferRankProperties]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCInferRankProperties[
            {
                <|"Length" -> 1|>,
                <|"Length" -> 5|>,
                <|"Length" -> 10|>,
                <|"Length" -> 20, "GridOrDivider" -> <||>|>
            }
        ]
    ]
    ,
    {
        <|"Length" -> 1, "Length.Rank" -> 3, "Length.InverseRank" -> 1|>,
        <|"Length" -> 5, "Length.Rank" -> 2, "Length.InverseRank" -> 2|>,
        <|"Length" -> 10, "Length.Rank" -> 1, "Length.InverseRank" -> 3|>
    }
    ,
    TestID -> "ARCInferRankProperties-20220804-G0RK85"
]