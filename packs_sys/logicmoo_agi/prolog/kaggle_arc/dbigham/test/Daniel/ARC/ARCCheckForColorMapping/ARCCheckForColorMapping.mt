(*
    Tests for: Daniel`ARC`ARCCheckForColorMapping
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCCheckForColorMapping]
    
    Author: danielb
*)

Test[
    With[
        {parsedFile = Daniel`ARC`ARCParseFile["6d75e8bb"]["Train"]},
        With[
            {
                parsedInputScene = Daniel`ARC`ARCParseScene[
                    parsedFile[[1, "Input"]]
                ]
            },
            With[
                {
                    parsedOutputScene = Daniel`ARC`ARCParseScene[
                        parsedFile[[1, "Output"]]
                    ]
                },
                Daniel`ARC`ARCCheckForColorMapping[
                    parsedInputScene[["Objects", 1, "Image", 1]],
                    parsedOutputScene[["Objects", 1, "Image", 1]]
                ]
            ]
        ]
    ]
    ,
    {{-1, 2}}
    ,
    TestID -> "ARCCheckForColorMapping-20221014-72LWAR"
]

Test[
    With[
        {parsedFile = Daniel`ARC`ARCParseFile["6d75e8bb"]["Train"]},
        With[
            {
                parsedInputScene = Daniel`ARC`ARCParseScene[
                    parsedFile[[1, "Input"]]
                ]
            },
            With[
                {
                    parsedOutputScene = Daniel`ARC`ARCParseScene[
                        parsedFile[[1, "Output"]]
                    ]
                },
                Daniel`ARC`ARCCheckForColorMapping[
                    parsedInputScene[["Objects", 1]],
                    parsedOutputScene[["Objects", 1]]
                ]
            ]
        ]
    ]
    ,
    <|"Transform" -> <|"Type" -> "ColorMapping", "Mapping" -> {{-1, 2}}|>|>
    ,
    TestID -> "ARCCheckForColorMapping-20221014-PGUA68"
]