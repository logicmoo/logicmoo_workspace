(*
    Tests for: Daniel`ARC`ARCSortRuleScore
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSortRuleScore]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSortRuleScore[<||> -> <|"a" -> 1|>]
    ,
    {0, 1, -1}
    ,
    TestID -> "ARCSortRuleScore-20220906-XMSHLU"
]

Test[
    Daniel`ARC`ARCSortRuleScore[<|"MyProperty" -> 1|> -> <|"a" -> 1|>]
    ,
    {0, 2, "MyProperty", 1}
    ,
    TestID -> "ARCSortRuleScore-20220906-BFFWET"
]

Test[
    Daniel`ARC`ARCSortRuleScore[<|"MyProperty" -> 2|> -> <|"a" -> 1|>]
    ,
    {0, 2, "MyProperty", 2}
    ,
    TestID -> "ARCSortRuleScore-20220906-SV1U4H"
]

Test[
    Daniel`ARC`ARCSortRuleScore[Null]
    ,
    {1, -1, -1}
    ,
    TestID -> "ARCSortRuleScore-20220906-O9MLZY"
]