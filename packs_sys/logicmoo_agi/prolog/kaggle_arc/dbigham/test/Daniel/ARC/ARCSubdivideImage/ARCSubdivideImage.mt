(*
    Tests for: Daniel`ARC`ARCSubdivideImage
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCSubdivideImage]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCSubdivideImage[
        Daniel`ARC`ARCParseFile["8be77c9e"][[1, "Train", 1, "Output"]],
        2,
        1
    ]
    ,
    {
        {Daniel`ARC`ARCScene[{{1, 1, 0}, {1, 1, 1}, {0, 0, 0}}]},
        {Daniel`ARC`ARCScene[{{0, 0, 0}, {1, 1, 1}, {1, 1, 0}}]}
    }
    ,
    TestID -> "ARCSubdivideImage-20220906-YSALK0"
]

Test[
    Daniel`ARC`ARCSubdivideImage[
        Daniel`ARC`ARCScene[
            {
                {0, 0, 8, 0, 0, 8, 0, 0},
                {0, 8, 0, 8, 8, 0, 8, 0},
                {0, 0, 8, 0, 0, 8, 0, 0},
                {0, 0, 8, 0, 0, 8, 0, 0},
                {0, 8, 0, 8, 8, 0, 8, 0},
                {0, 0, 8, 0, 0, 8, 0, 0}
            }
        ],
        2,
        2
    ]
    ,
    {
        {
            Daniel`ARC`ARCScene[{{0, 0, 8, 0}, {0, 8, 0, 8}, {0, 0, 8, 0}}],
            Daniel`ARC`ARCScene[{{0, 8, 0, 0}, {8, 0, 8, 0}, {0, 8, 0, 0}}]
        },
        {
            Daniel`ARC`ARCScene[{{0, 0, 8, 0}, {0, 8, 0, 8}, {0, 0, 8, 0}}],
            Daniel`ARC`ARCScene[{{0, 8, 0, 0}, {8, 0, 8, 0}, {0, 8, 0, 0}}]
        }
    }
    ,
    TestID -> "ARCSubdivideImage-20220909-KH30WN"
]