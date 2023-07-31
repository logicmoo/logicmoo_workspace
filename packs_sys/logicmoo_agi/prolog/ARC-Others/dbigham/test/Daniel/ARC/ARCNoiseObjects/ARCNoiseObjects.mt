(*
    Tests for: Daniel`ARC`ARCNoiseObjects
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCNoiseObjects]
    
    Author: danielb
*)

Test[
    Daniel`ARC`SimplifyObjects[
        With[
            {
                parsedScene = Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["5614dbcf"]["Train", 1, "Input"],
                    "FormMultiColorCompositeObjects" -> False
                ]
            },
            Daniel`ARC`ARCNoiseObjects[parsedScene["Objects"]]
        ]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{5}}], "Position" -> {2, 8}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{5}}], "Position" -> {4, 5}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{5}}], "Position" -> {7, 6}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{5}}], "Position" -> {9, 2}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{5}}], "Position" -> {9, 5}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{5}}], "Position" -> {9, 9}|>
    }
    ,
    TestID -> "ARCNoiseObjects-20220904-7BTW8V"
]

Test[
    Daniel`ARC`SimplifyObjects[
        With[
            {
                parsedScene = Daniel`ARC`ARCParseScene[
                    Daniel`ARC`ARCParseFile["7e0986d6"]["Test", 1, "Input"],
                    "FormMultiColorCompositeObjects" -> False
                ]
            },
            Daniel`ARC`ARCNoiseObjects[parsedScene["Objects"]]
        ]
    ]
    ,
    {
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {1, 15}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {2, 7}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {4, 17}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {6, 8}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {7, 15}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {8, 6}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {8, 13}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {10, 16}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {11, 12}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {12, 4}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}], "Position" -> {12, 10}|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}]|>,
        <|"Image" -> Daniel`ARC`ARCScene[{{4}}]|>
    }
    ,
    TestID -> "ARCNoiseObjects-20221105-YB9E11"
]