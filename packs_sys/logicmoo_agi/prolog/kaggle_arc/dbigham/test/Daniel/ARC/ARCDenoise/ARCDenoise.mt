(*
    Tests for: Daniel`ARC`ARCDenoise
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCDenoise]
    
    Author: danielb
*)

Test[
    With[
        {
            parsedScene = Daniel`ARC`ARCParseScene[
                Daniel`ARC`ARCParseFile["5614dbcf"]["Train", 1, "Input"],
                "FormMultiColorCompositeObjects" -> False
            ]
        },
        Daniel`ARC`ARCDenoise[
            parsedScene["Scene"],
            Select[parsedScene["Objects"], EntityLink`EntityMatchQ[#1["Shape"], "Pixel"] & ]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[
        {
            {3, 3, 3, 0, 0, 0, 8, 8, 8},
            {3, 3, 3, 0, 0, 0, 8, 8, 8},
            {3, 3, 3, 0, 0, 0, 8, 8, 8},
            {0, 0, 0, 7, 7, 7, 0, 0, 0},
            {0, 0, 0, 7, 7, 7, 0, 0, 0},
            {0, 0, 0, 7, 7, 7, 0, 0, 0},
            {6, 6, 6, 0, 0, 0, 9, 9, 9},
            {6, 6, 6, 0, 0, 0, 9, 9, 9},
            {6, 6, 6, 0, 0, 0, 9, 9, 9}
        }
    ]
    ,
    TestID -> "ARCDenoise-20220904-PFHBUI"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCDenoise[Daniel`ARC`ARCParseFile["5614dbcf"]["Train", 1, "Input"]]
        ]
    ]
    ,
    <|
        "Denoised" -> True,
        "Image" -> Daniel`ARC`ARCScene[
            {
                {3, 3, 3, 0, 0, 0, 8, 8, 8},
                {3, 3, 3, 0, 0, 0, 8, 8, 8},
                {3, 3, 3, 0, 0, 0, 8, 8, 8},
                {0, 0, 0, 7, 7, 7, 0, 0, 0},
                {0, 0, 0, 7, 7, 7, 0, 0, 0},
                {0, 0, 0, 7, 7, 7, 0, 0, 0},
                {6, 6, 6, 0, 0, 0, 9, 9, 9},
                {6, 6, 6, 0, 0, 0, 9, 9, 9},
                {6, 6, 6, 0, 0, 0, 9, 9, 9}
            }
        ]
    |>
    ,
    TestID -> "ARCDenoise-20220904-EFVDAR"
]

Test[
    Daniel`ARC`ARCDenoise[Daniel`ARC`ARCParseFile["5614dbcf"]["Train"]]
    ,
    <|
        "Examples" -> {
            <|
                "Input" -> Daniel`ARC`ARCScene[
                    {
                        {3, 3, 3, 0, 0, 0, 8, 8, 8},
                        {3, 3, 3, 0, 0, 0, 8, 8, 8},
                        {3, 3, 3, 0, 0, 0, 8, 8, 8},
                        {0, 0, 0, 7, 7, 7, 0, 0, 0},
                        {0, 0, 0, 7, 7, 7, 0, 0, 0},
                        {0, 0, 0, 7, 7, 7, 0, 0, 0},
                        {6, 6, 6, 0, 0, 0, 9, 9, 9},
                        {6, 6, 6, 0, 0, 0, 9, 9, 9},
                        {6, 6, 6, 0, 0, 0, 9, 9, 9}
                    }
                ],
                "Output" -> Daniel`ARC`ARCScene[{{3, 0, 8}, {0, 7, 0}, {6, 0, 9}}]
            |>,
            <|
                "Input" -> Daniel`ARC`ARCScene[
                    {
                        {0, 0, 0, 2, 2, 2, 0, 0, 0},
                        {0, 0, 0, 2, 2, 2, 0, 0, 0},
                        {0, 0, 0, 2, 2, 2, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 0, 0, 0, 0, 0, 0},
                        {0, 0, 0, 7, 7, 7, 0, 0, 0},
                        {0, 0, 0, 7, 7, 7, 0, 0, 0},
                        {0, 0, 0, 7, 7, 7, 0, 0, 0}
                    }
                ],
                "Output" -> Daniel`ARC`ARCScene[{{0, 2, 0}, {0, 0, 0}, {0, 7, 0}}]
            |>
        },
        "Denoised" -> True
    |>
    ,
    TestID -> "ARCDenoise-20220904-J24Z5F"
]

Test[
    Daniel`ARC`ARCDenoise[Daniel`ARC`ARCParseFile["7e0986d6"]["Train", 1, "Input"]]
    ,
    <|
        "Denoised" -> True,
        "Image" -> Daniel`ARC`ARCScene[
            {
                {0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0},
                {0, 0, 3, 3, 3, 0, 3, 3, 3, 3, 3, 0, 0, 0},
                {0, 0, 3, 3, 3, 0, 3, 3, 3, 3, 3, 0, 0, 0},
                {0, 0, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3},
                {0, 0, 0, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3},
                {0, 0, 0, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3},
                {0, 0, 0, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3},
                {3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0},
                {3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
            }
        ]
    |>
    ,
    TestID -> "ARCDenoise-20221105-VM615F"
]