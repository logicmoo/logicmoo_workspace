(*
    Tests for: Daniel`ARC`ARCApplyValueMappingToGrid
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCApplyValueMappingToGrid]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCApplyValueMappingToGrid[
        With[
            {
                parsedScene = Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["0520fde7"]["Train"][[1, "Input"]]
                ]
            },
            Map[
                Function[
                    {image},
                    Daniel`ARC`ARCBinarizeImage[image, parsedScene["Background"]]
                ],
                Flatten[Daniel`ARC`ARCSubdivideImageUsingGrid[parsedScene]]
            ]
        ],
        <|
            "Type" -> "ValueMap",
            "Binarize" -> True,
            "OutputColor" -> 2,
            "Mapping" -> <|{1, 0} -> 0, {0, 0} -> 0, {0, 1} -> 0, {1, 1} -> 1|>
        |>
    ]
    ,
    Daniel`ARC`ARCScene[{{-1, -1, -1}, {-1, 2, -1}, {-1, -1, -1}}]
    ,
    TestID -> "ARCApplyValueMappingToGrid-20220925-1L34LY"
]