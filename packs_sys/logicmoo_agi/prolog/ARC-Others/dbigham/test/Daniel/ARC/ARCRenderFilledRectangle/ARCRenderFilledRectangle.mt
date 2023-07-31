(*
    Tests for: Daniel`ARC`ARCRenderFilledRectangle
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCRenderFilledRectangle]
    
    Author: danielb
*)

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRenderFilledRectangle[6, 2, {{2, 2}, {8, 8}}, Missing["Color"]]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 8, 2, 8, 2, 8}, {2, 8, 2, 8, 2, 8}}]
    ,
    TestID -> "ARCRenderFilledRectangle-20221023-QSID5Q"
]

Test[
    Utility`ReplaceAssociationsWithUnevaluatedAssociations[
        Daniel`ARC`ARCRenderFilledRectangle[
            2,
            6,
            <|"Pattern" -> {{2, 2}, {8, 8}}, "Orientation" -> "Vertical"|>,
            Missing["Color"]
        ]
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2}, {8, 8}, {2, 2}, {8, 8}, {2, 2}, {8, 8}}]
    ,
    TestID -> "ARCRenderFilledRectangle-20221023-QXLTIZ"
]

Test[
    Daniel`ARC`ARCRenderFilledRectangle[
        2,
        6,
        <|"Pattern" -> {{10, 10}, {-1, -1}}, "Orientation" -> "Vertical"|>,
        2
    ]
    ,
    Daniel`ARC`ARCScene[{{2, 2}, {-1, -1}, {2, 2}, {-1, -1}, {2, 2}, {-1, -1}}]
    ,
    TestID -> "ARCRenderFilledRectangle-20221024-U49NSL"
]