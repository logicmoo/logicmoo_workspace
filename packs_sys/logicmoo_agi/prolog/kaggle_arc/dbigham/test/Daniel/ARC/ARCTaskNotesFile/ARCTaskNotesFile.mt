(*
    Tests for: Daniel`ARC`ARCTaskNotesFile
    
    To run these tests:
    
        << DevTools`
        << Daniel`ARC`
        RunUnitTests[Daniel`ARC`ARCTaskNotesFile]
    
    Author: danielb
*)

Test[
    Daniel`ARC`ARCTaskNotesFile["my-task"]
    ,
    "TaskNotes/my-task/notes.md"
    ,
    TestID -> "ARCTaskNotesFile-20220808-QOPG5O"
]