(*
    Tests for: Daniel`ARC`ARCFollowLine
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCFollowLine]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCFollowLine[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]
                ][[
                    "Objects",
                    1,
                    "Image",
                    1
                ]],
                3,
                {1, 1}
            ]
        ]
    ]
    ,
    {
        {1, 1},
        {1, 2},
        {1, 3},
        {1, 4},
        {1, 5},
        {1, 6},
        {2, 6},
        {3, 6},
        {4, 6},
        {5, 6},
        {6, 6},
        {6, 5},
        {6, 4},
        {6, 3},
        {6, 2},
        {6, 1},
        {5, 1},
        {4, 1},
        {3, 1},
        {3, 2},
        {3, 3},
        {3, 4},
        {4, 4},
        {4, 3}
    }
    ,
    TestID -> "ARCFollowLine-20220921-NIA4DI"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        DevTools`TestingTools`SlowTest[
            Daniel`ARC`ARCFollowLine[
                Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["e5790162"]["Train", 5, "Output"],
                    "FormMultiColorCompositeObjects" -> False
                ][[
                    "Objects",
                    1,
                    "Image",
                    1
                ]],
                3,
                {1, 6}
            ]
        ]
    ]
    ,
    {{1, 6}, {2, 6}, {2, 5}, {2, 4}, {2, 3}, {3, 3}, {4, 3}, {5, 3}, {5, 2}, {5, 1}}
    ,
    TestID -> "ARCFollowLine-20220921-JUT4OK"
]

Test[
    Daniel`ARC`ARCFollowLine[
        Daniel`ARC`ARCParseScene[Daniel`ARC`ARCParseFile["28e73c20"]["Train", 1, "Output"]][[
            "Objects",
            1,
            "Image",
            1
        ]],
        3,
        {3, 1}
    ]
    ,
    {
        {1, 1},
        {1, 2},
        {1, 3},
        {1, 4},
        {1, 5},
        {1, 6},
        {2, 6},
        {3, 6},
        {4, 6},
        {5, 6},
        {6, 6},
        {6, 5},
        {6, 4},
        {6, 3},
        {6, 2},
        {6, 1},
        {5, 1},
        {4, 1},
        {3, 1},
        {3, 2},
        {3, 3},
        {3, 4},
        {4, 4},
        {4, 3}
    }
    ,
    TestID -> "ARCFollowLine-20220921-BOM9XD"
]