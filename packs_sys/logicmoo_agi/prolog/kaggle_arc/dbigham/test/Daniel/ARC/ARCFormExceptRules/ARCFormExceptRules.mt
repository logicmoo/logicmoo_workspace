(*
    Tests for: Daniel`ARC`ARCFormExceptRules
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFormExceptRules]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCFormExceptRules[
        {
            <|"FilledArea.Rank" -> 1|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"FilledArea.Rank" -> 3 | 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        },
        {<|"FilledArea.Rank" -> 1|>, <|"FilledArea.Rank" -> 2|>, <|"FilledArea.Rank" -> 3|>}
    ]
    ,
    {
        <|"FilledArea.Rank" -> 1|> -> <|
            "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
        |>,
        <|"FilledArea.Rank" -> Except[1]|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
    }
    ,
    TestID -> "ARCFormExceptRules-20220902-TINB7Y"
]

Test[
    Daniel`ARC`ARCFormExceptRules[
        {
            <|"FilledArea.Rank" -> 1|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"FilledArea.Rank" -> 3 | 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        },
        {
            <|"FilledArea.Rank" -> 1|>,
            <|"FilledArea.Rank" -> 2|>,
            <|"FilledArea.Rank" -> 3|>,
            <|"FilledArea.Rank" -> 4|>
        }
    ]
    ,
    {
        <|"FilledArea.Rank" -> 1|> -> <|
            "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
        |>,
        <|"FilledArea.Rank" -> 3 | 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
    }
    ,
    TestID -> "ARCFormExceptRules-20220902-N90Q9C"
]

Test[
    Daniel`ARC`ARCFormExceptRules[
        {
            <|"FilledArea.Rank" -> 1|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"FilledArea.Rank" -> 3|> -> <|
                "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
            |>,
            <|"FilledArea.Rank" -> 4 | 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
        },
        {
            <|"FilledArea.Rank" -> 1|>,
            <|"FilledArea.Rank" -> 2|>,
            <|"FilledArea.Rank" -> 3|>,
            <|"FilledArea.Rank" -> 4|>
        }
    ]
    ,
    {
        <|"FilledArea.Rank" -> 1|> -> <|
            "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
        |>,
        <|"FilledArea.Rank" -> 3|> -> <|
            "Transform" -> <|"Type" -> "Move", "Position" -> <|"Y" -> 1, "X" -> 1|>|>
        |>,
        <|"FilledArea.Rank" -> 4 | 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
    }
    ,
    TestID -> "ARCFormExceptRules-20220902-I72XUI"
]

Test[
    Daniel`ARC`ARCFormExceptRules[
        {
            <|"FilledArea" -> 6|> -> <|"Color" -> 1|>,
            <|"FilledArea" -> 2 | 3 | 4 | 5 | 7|> -> <|"Color" -> 2|>
        },
        {}
    ]
    ,
    {
        <|"FilledArea" -> 6|> -> <|"Color" -> 1|>,
        <|"FilledArea" -> Except[6]|> -> <|"Color" -> 2|>
    }
    ,
    TestID -> "ARCFormExceptRules-20220919-W0DTK2"
]