(*
    Tests for: Daniel`ARC`ResolveValues
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ResolveValues]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ResolveValues[
        <|"Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {1}|>, "Y"]|>,
        <|
            "UUID" -> "8b35d32a-3dde-48d7-9243-b50c19f0f138",
            "Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}, {4, 4, 4}}],
            "PixelPositions" -> {{1, 8}, {1, 9}, {1, 10}, {2, 8}, {2, 9}, {2, 10}},
            "Colors" -> {4},
            "Width" -> 3,
            "Height" -> 2,
            "Position" -> {1, 8},
            "Y" -> 1,
            "X" -> 8,
            "AspectRatio" -> 3/2,
            "Area" -> 6,
            "FilledArea" -> 6
        |>,
        <|"Objects" -> {<|"Colors" -> {1}, "Y" -> 9|>}|>
    ]
    ,
    <|"Y" -> 9|>
    ,
    TestID -> "ResolveValues-20220724-1AKERG"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ResolveValues[
            <|"Y" -> Daniel`ARC`ObjectValue[<|"Colors" -> {1}|>, "Y"]|>,
            <|
                "UUID" -> "8b35d32a-3dde-48d7-9243-b50c19f0f138",
                "Image" -> Daniel`ARC`ARCScene[{{4, 4, 4}, {4, 4, 4}}],
                "PixelPositions" -> {{1, 8}, {1, 9}, {1, 10}, {2, 8}, {2, 9}, {2, 10}},
                "Colors" -> {4},
                "Width" -> 3,
                "Height" -> 2,
                "Position" -> {1, 8},
                "Y" -> 1,
                "X" -> 8,
                "AspectRatio" -> 3/2,
                "Area" -> 6,
                "FilledArea" -> 6
            |>,
            <|"Objects" -> {<|"Colors" -> {1}|>}|>
        ]
    ]
    ,
    Failure[
        "MissingPropertyValue",
        <|
            "MessageTemplate" -> "A property's value couldn't be found for a resolved object.",
            "MessageParameters" -> <||>,
            "ObjectPattern" -> <|"Colors" -> {1}|>,
            "ResolvedObject" -> <|"Colors" -> {1}|>,
            "Property" -> "Y"
        |>
    ]
    ,
    TestID -> "ResolveValues-20220724-RSQDZ9"
]

Test[
    Daniel`ARC`ResolveValues[
        <|
            "Type" -> "MapComponents",
            "Rules" -> {
                <|"Width" -> 3|> -> <|
                    "X2" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X2"], 1]
                |>
            }
        |>,
        <||>,
        <||>
    ]
    ,
    <|
        "Type" -> "MapComponents",
        "Rules" -> {
            <|"Width" -> 3|> -> <|
                "X2" -> Inactive[Plus][Daniel`ARC`ObjectValue["InputObject", "X2"], 1]
            |>
        }
    |>
    ,
    TestID -> "ResolveValues-20221112-I1TDZ3"
]