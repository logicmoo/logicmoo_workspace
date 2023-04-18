(*
    Tests for: Daniel`ARC`ARCCompleteRuleSubsets
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCompleteRuleSubsets]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCompleteRuleSubsets[
            {
                <|"RuleID" -> 1, "CasesCovered" -> {1, 2, 3}|>,
                <|"RuleID" -> 2, "CasesCovered" -> {4, 5}|>,
                <|"RuleID" -> 3, "CasesCovered" -> {2, 4}|>,
                <|"RuleID" -> 4, "CasesCovered" -> {6, 7, 8}|>,
                <|"RuleID" -> 5, "CasesCovered" -> {1, 6, 7}|>,
                <|"RuleID" -> 6, "CasesCovered" -> {9, 10}|>,
                <|"RuleID" -> 7, "CasesCovered" -> {8, 9, 10}|>,
                <|"RuleID" -> 8, "CasesCovered" -> {1, 2, 3, 4, 5, 6, 7, 8}|>
            },
            10
        ]
    ]
    ,
    <|
        "Results" -> {
            <|"RuleIDs" -> {1, 2, 4, 6}, "CasesCovered" -> {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}|>,
            <|"RuleIDs" -> {6, 8}, "CasesCovered" -> {9, 10, 1, 2, 3, 4, 5, 6, 7, 8}|>
        },
        "PartialResults" -> {
            <|"RuleIDs" -> {1, 2, 7}, "CasesCovered" -> {1, 2, 3, 4, 5, 8, 9, 10}|>,
            <|"RuleIDs" -> {2, 5, 6}, "CasesCovered" -> {4, 5, 1, 6, 7, 9, 10}|>,
            <|"RuleIDs" -> {2, 5, 7}, "CasesCovered" -> {4, 5, 1, 6, 7, 8, 9, 10}|>,
            <|"RuleIDs" -> {3, 4, 6}, "CasesCovered" -> {2, 4, 6, 7, 8, 9, 10}|>,
            <|"RuleIDs" -> {3, 5, 6}, "CasesCovered" -> {2, 4, 1, 6, 7, 9, 10}|>,
            <|"RuleIDs" -> {3, 5, 7}, "CasesCovered" -> {2, 4, 1, 6, 7, 8, 9, 10}|>
        }
    |>
    ,
    TestID -> "ARCCompleteRuleSubsets-20220916-5DZZHH"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCCompleteRuleSubsets[
            {
                <|"MyProperty" -> 1|> -> <|
                    "MyProperty2" -> 2,
                    "InputObjects" -> {"A", "B", "C"}
                |>,
                <|"MyProperty" -> 2|> -> <|
                    "MyProperty2" -> 3,
                    "InputObjects" -> {"D", "E"}
                |>,
                <|"MyProperty" -> 3|> -> <|
                    "MyProperty2" -> 4,
                    "InputObjects" -> {"B", "D"}
                |>,
                <|"MyProperty" -> 4|> -> <|
                    "MyProperty2" -> 5,
                    "InputObjects" -> {"F", "G", "H"}
                |>,
                <|"MyProperty" -> 5|> -> <|
                    "MyProperty2" -> 6,
                    "InputObjects" -> {"A", "F", "G"}
                |>,
                <|"MyProperty" -> 6|> -> <|
                    "MyProperty2" -> 7,
                    "InputObjects" -> {"I", "J"}
                |>,
                <|"MyProperty" -> 7|> -> <|
                    "MyProperty2" -> 8,
                    "InputObjects" -> {"H", "I", "J"}
                |>,
                <|"MyProperty" -> 8|> -> <|
                    "MyProperty2" -> 9,
                    "InputObjects" -> {"A", "B", "C", "D", "E", "F", "G", "H"}
                |>,
                "Else" -> <|
                    "MyProperty2" -> 10,
                    "InputObjects" -> {"A", "B", "C", "D", "E", "F", "G", "H"}
                |>
            },
            {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J"}
        ]
    ]
    ,
    <|
        "Results" -> {
            {
                <|"MyProperty" -> 1|> -> <|
                    "MyProperty2" -> 2,
                    "InputObjects" -> {"A", "B", "C"}
                |>,
                <|"MyProperty" -> 2|> -> <|"MyProperty2" -> 3, "InputObjects" -> {"D", "E"}|>,
                <|"MyProperty" -> 4|> -> <|
                    "MyProperty2" -> 5,
                    "InputObjects" -> {"F", "G", "H"}
                |>,
                <|"MyProperty" -> 6|> -> <|"MyProperty2" -> 7, "InputObjects" -> {"I", "J"}|>
            },
            {
                <|"MyProperty" -> 6|> -> <|"MyProperty2" -> 7, "InputObjects" -> {"I", "J"}|>,
                <|"MyProperty" -> 8|> -> <|
                    "MyProperty2" -> 9,
                    "InputObjects" -> {"A", "B", "C", "D", "E", "F", "G", "H"}
                |>
            },
            {
                <|"MyProperty" -> 6|> -> <|"MyProperty2" -> 7, "InputObjects" -> {"I", "J"}|>,
                "Else" -> <|
                    "MyProperty2" -> 10,
                    "InputObjects" -> {"A", "B", "C", "D", "E", "F", "G", "H"}
                |>
            }
        },
        "PartialResults" -> {
            <|
                "Rules" -> {
                    <|"MyProperty" -> 3|> -> <|
                        "MyProperty2" -> 4,
                        "InputObjects" -> {"B", "D"}
                    |>,
                    <|"MyProperty" -> 5|> -> <|
                        "MyProperty2" -> 6,
                        "InputObjects" -> {"A", "F", "G"}
                    |>,
                    <|"MyProperty" -> 7|> -> <|
                        "MyProperty2" -> 8,
                        "InputObjects" -> {"H", "I", "J"}
                    |>
                },
                "InputObjects" -> {"B", "D", "A", "F", "G", "H", "I", "J"},
                "UseCount" -> 8,
                "UnhandledCount" -> 2,
                "Unhandled" -> {"C", "E"}
            |>,
            <|
                "Rules" -> {
                    <|"MyProperty" -> 2|> -> <|
                        "MyProperty2" -> 3,
                        "InputObjects" -> {"D", "E"}
                    |>,
                    <|"MyProperty" -> 5|> -> <|
                        "MyProperty2" -> 6,
                        "InputObjects" -> {"A", "F", "G"}
                    |>,
                    <|"MyProperty" -> 7|> -> <|
                        "MyProperty2" -> 8,
                        "InputObjects" -> {"H", "I", "J"}
                    |>
                },
                "InputObjects" -> {"D", "E", "A", "F", "G", "H", "I", "J"},
                "UseCount" -> 8,
                "UnhandledCount" -> 2,
                "Unhandled" -> {"B", "C"}
            |>,
            <|
                "Rules" -> {
                    <|"MyProperty" -> 1|> -> <|
                        "MyProperty2" -> 2,
                        "InputObjects" -> {"A", "B", "C"}
                    |>,
                    <|"MyProperty" -> 2|> -> <|
                        "MyProperty2" -> 3,
                        "InputObjects" -> {"D", "E"}
                    |>,
                    <|"MyProperty" -> 7|> -> <|
                        "MyProperty2" -> 8,
                        "InputObjects" -> {"H", "I", "J"}
                    |>
                },
                "InputObjects" -> {"A", "B", "C", "D", "E", "H", "I", "J"},
                "UseCount" -> 8,
                "UnhandledCount" -> 2,
                "Unhandled" -> {"F", "G"}
            |>,
            <|
                "Rules" -> {
                    <|"MyProperty" -> 3|> -> <|
                        "MyProperty2" -> 4,
                        "InputObjects" -> {"B", "D"}
                    |>,
                    <|"MyProperty" -> 5|> -> <|
                        "MyProperty2" -> 6,
                        "InputObjects" -> {"A", "F", "G"}
                    |>,
                    <|"MyProperty" -> 6|> -> <|
                        "MyProperty2" -> 7,
                        "InputObjects" -> {"I", "J"}
                    |>
                },
                "InputObjects" -> {"B", "D", "A", "F", "G", "I", "J"},
                "UseCount" -> 7,
                "UnhandledCount" -> 3,
                "Unhandled" -> {"C", "E", "H"}
            |>,
            <|
                "Rules" -> {
                    <|"MyProperty" -> 3|> -> <|
                        "MyProperty2" -> 4,
                        "InputObjects" -> {"B", "D"}
                    |>,
                    <|"MyProperty" -> 4|> -> <|
                        "MyProperty2" -> 5,
                        "InputObjects" -> {"F", "G", "H"}
                    |>,
                    <|"MyProperty" -> 6|> -> <|
                        "MyProperty2" -> 7,
                        "InputObjects" -> {"I", "J"}
                    |>
                },
                "InputObjects" -> {"B", "D", "F", "G", "H", "I", "J"},
                "UseCount" -> 7,
                "UnhandledCount" -> 3,
                "Unhandled" -> {"A", "C", "E"}
            |>,
            <|
                "Rules" -> {
                    <|"MyProperty" -> 2|> -> <|
                        "MyProperty2" -> 3,
                        "InputObjects" -> {"D", "E"}
                    |>,
                    <|"MyProperty" -> 5|> -> <|
                        "MyProperty2" -> 6,
                        "InputObjects" -> {"A", "F", "G"}
                    |>,
                    <|"MyProperty" -> 6|> -> <|
                        "MyProperty2" -> 7,
                        "InputObjects" -> {"I", "J"}
                    |>
                },
                "InputObjects" -> {"D", "E", "A", "F", "G", "I", "J"},
                "UseCount" -> 7,
                "UnhandledCount" -> 3,
                "Unhandled" -> {"B", "C", "H"}
            |>
        }
    |>
    ,
    TestID -> "ARCCompleteRuleSubsets-20220917-GYTRN3"
]