(*
    Tests for: Daniel`ARC`ARCSubdivideImageUsingGrid
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSubdivideImageUsingGrid]
    
    Author: danielb
*)

Test[
    With[
        {parsedFile = Daniel`ARC`ARCParseFile["272f95fa"]["Train"]},
        Daniel`ARC`ARCSubdivideImageUsingGrid[
            image = parsedFile[[1, "Output"]],
            Daniel`ARC`ARCParseGrid[
                Replace[
                    parsedFile[[1, "Input"]][[1]],
                    0 -> -1,
                    {2}
                ]
            ]
        ]
    ]
    ,
    {
        {
            Daniel`ARC`ARCScene[{{0, 0, 0, 0}, {0, 0, 0, 0}}],
            Daniel`ARC`ARCScene[{{2, 2, 2, 2, 2, 2}, {2, 2, 2, 2, 2, 2}}],
            Daniel`ARC`ARCScene[{{0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0}}]
        },
        {
            Daniel`ARC`ARCScene[{{4, 4, 4, 4}, {4, 4, 4, 4}, {4, 4, 4, 4}, {4, 4, 4, 4}}],
            Daniel`ARC`ARCScene[
                {
                    {6, 6, 6, 6, 6, 6},
                    {6, 6, 6, 6, 6, 6},
                    {6, 6, 6, 6, 6, 6},
                    {6, 6, 6, 6, 6, 6}
                }
            ],
            Daniel`ARC`ARCScene[
                {
                    {3, 3, 3, 3, 3, 3, 3},
                    {3, 3, 3, 3, 3, 3, 3},
                    {3, 3, 3, 3, 3, 3, 3},
                    {3, 3, 3, 3, 3, 3, 3}
                }
            ]
        },
        {
            Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0}
                }
            ],
            Daniel`ARC`ARCScene[
                {
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1},
                    {1, 1, 1, 1, 1, 1}
                }
            ],
            Daniel`ARC`ARCScene[
                {
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0},
                    {0, 0, 0, 0, 0, 0, 0}
                }
            ]
        }
    }
    ,
    TestID -> "ARCSubdivideImageUsingGrid-20220910-7V2UAF"
]