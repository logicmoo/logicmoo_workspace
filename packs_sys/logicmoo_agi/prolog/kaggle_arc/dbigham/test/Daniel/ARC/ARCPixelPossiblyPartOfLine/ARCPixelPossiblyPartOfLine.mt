(*
    Tests for: Daniel`ARC`ARCPixelPossiblyPartOfLine
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCPixelPossiblyPartOfLine]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCPixelPossiblyPartOfLine[
        Daniel`ARC`ARCParseScene[Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]][[
            "Objects",
            1,
            "Image",
            1
        ]],
        {1, 2}
    ]
    ,
    <|"Result" -> True, "AdjacentPixels" -> {{1, 1}, {1, 3}}|>
    ,
    TestID -> "ARCPixelPossiblyPartOfLine-20220914-VEDPIU"
]

Test[
    Daniel`ARC`ARCPixelPossiblyPartOfLine[
        Daniel`ARC`ARCParseScene[Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]][[
            "Objects",
            1,
            "Image",
            1
        ]],
        {6, 1}
    ]
    ,
    <|"Result" -> True, "AdjacentPixels" -> {{5, 1}, {6, 2}}|>
    ,
    TestID -> "ARCPixelPossiblyPartOfLine-20220914-O9E5J4"
]

Test[
    Daniel`ARC`ARCPixelPossiblyPartOfLine[
        Daniel`ARC`ARCParseScene[Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]][[
            "Objects",
            1,
            "Image",
            1
        ]],
        {1, 1}
    ]
    ,
    <|"Result" -> True, "AdjacentPixels" -> {{1, 2}}|>
    ,
    TestID -> "ARCPixelPossiblyPartOfLine-20220914-Z8B0ST"
]

Test[
    Daniel`ARC`ARCPixelPossiblyPartOfLine[
        Daniel`ARC`ARCParseScene[Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]][[
            "Objects",
            1,
            "Image",
            1
        ]],
        {2, 1}
    ]
    ,
    <|"Result" -> False|>
    ,
    TestID -> "ARCPixelPossiblyPartOfLine-20220914-Q3P78M"
]

Test[
    Daniel`ARC`ARCPixelPossiblyPartOfLine[
        Daniel`ARC`ARCParseScene[
            Daniel`ARC`ARCParseFile["d9f24cd1"]["Train", 2, "Output"],
            "FormMultiColorCompositeObjects" -> False,
            "CheckForGridsAndDividers" -> False
        ][[
            "Objects",
            1,
            "Image",
            1
        ]],
        {7, 1}
    ]
    ,
    <|"Result" -> True, "AdjacentPixels" -> {{7, 2}, {8, 1}}|>
    ,
    TestID -> "ARCPixelPossiblyPartOfLine-20220925-VEVZ6O"
]