(*
    Tests for: Daniel`ARC`ARCChooseBestRuleSet
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCChooseBestRuleSet]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCChooseBestRuleSet[
        {
            {<|"Color" -> 1|> -> <|"X" -> 1|>, <|"Color" -> 2|> -> <|"X" -> 2|>},
            {<|"X" -> 1|> -> <|"Color" -> 1|>, <|"X" -> 2|> -> <|"Color" -> 2|>}
        }
    ]
    ,
    {<|"Color" -> 1|> -> <|"X" -> 1|>, <|"Color" -> 2|> -> <|"X" -> 2|>}
    ,
    TestID -> "ARCChooseBestRuleSet-20220825-GQ1MJL"
]

Test[
    Daniel`ARC`ARCChooseBestRuleSet[
        {
            {
                <|"Shape" -> <|"Name" -> "Pixel"|>|> -> <|
                    "Transform" -> <|"Type" -> "Delete"|>
                |>,
                <|"Shape" -> Except[<|"Name" -> "Pixel"|>]|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>
            },
            {
                <|
                    "Shapes" -> Alternatives[
                        <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                        <|"Name" -> "Pixel"|>,
                        <|"Name" -> "Rectangle"|>
                    ]
                |> -> <|
                    "Transform" -> <|"Type" -> "Delete"|>
                |>,
                <|
                    "Shapes" -> Except[
                        Alternatives[
                            <|"Image" -> Daniel`ARC`ARCScene[{{10}}]|>,
                            <|"Name" -> "Rectangle"|>
                        ]
                    ]
                |> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>
            },
            {
                <|"Width" -> 4|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Width" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Height" -> 5|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Height" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Length" -> 5|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Length" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Position" -> {2, 2}|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Position" -> {7, 1}|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Y" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Y" -> 7|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"X" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"X" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Y2" -> 6|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Y2" -> 7|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"X2" -> 5|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"X2" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"PrimarySizeDimension" -> "Y"|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"PrimarySizeDimension" -> "None"|> -> <|
                    "Transform" -> <|"Type" -> "Delete"|>
                |>
            },
            {
                <|"AspectRatio" -> 4/5|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"AspectRatio" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Area" -> 20|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Area" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"FilledArea" -> 12|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"FilledArea" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"FilledProportion" -> 0.6|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"FilledProportion" -> 1.|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"ColorUseCount" -> 12|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"ColorUseCount" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Width.Rank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Width.Rank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Width.InverseRank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Width.InverseRank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Height.Rank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Height.Rank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Height.InverseRank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Height.InverseRank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Length.Rank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Length.Rank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Length.InverseRank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Length.InverseRank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Y.Rank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Y.Rank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Y.InverseRank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Y.InverseRank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"X.Rank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"X.Rank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"X.InverseRank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"X.InverseRank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Y2.Rank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Y2.Rank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"Y2.InverseRank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"Y2.InverseRank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"X2.Rank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"X2.Rank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"X2.InverseRank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"X2.InverseRank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"PrimarySizeDimension.Rank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"PrimarySizeDimension.Rank" -> 2|> -> <|
                    "Transform" -> <|"Type" -> "Delete"|>
                |>
            },
            {
                <|"PrimarySizeDimension.InverseRank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"PrimarySizeDimension.InverseRank" -> 1|> -> <|
                    "Transform" -> <|"Type" -> "Delete"|>
                |>
            },
            {
                <|"AspectRatio.Rank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"AspectRatio.Rank" -> 1|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"AspectRatio.InverseRank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"AspectRatio.InverseRank" -> 2|> -> <|
                    "Transform" -> <|"Type" -> "Delete"|>
                |>
            },
            {
                <|"ColorUseCount.Rank" -> 1|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"ColorUseCount.Rank" -> 2|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>
            },
            {
                <|"ColorUseCount.InverseRank" -> 2|> -> <|
                    "Color" -> Daniel`ARC`ObjectValue[
                        <|"Shape" -> <|"Name" -> "Pixel"|>|>,
                        "Color"
                    ]
                |>,
                <|"ColorUseCount.InverseRank" -> 1|> -> <|
                    "Transform" -> <|"Type" -> "Delete"|>
                |>
            }
        }
    ]
    ,
    {
        <|"Shape" -> <|"Name" -> "Pixel"|>|> -> <|"Transform" -> <|"Type" -> "Delete"|>|>,
        <|"Shape" -> Except[<|"Name" -> "Pixel"|>]|> -> <|
            "Color" -> Daniel`ARC`ObjectValue[<|"Shape" -> <|"Name" -> "Pixel"|>|>, "Color"]
        |>
    }
    ,
    TestID -> "ARCChooseBestRuleSet-20220825-8PPQWK"
]