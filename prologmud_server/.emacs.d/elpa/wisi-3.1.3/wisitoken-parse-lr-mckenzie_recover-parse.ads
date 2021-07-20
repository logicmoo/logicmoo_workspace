--  Abstract :
--
--  Config parsing subprograms.
--
--  Copyright (C) 2018 - 2019 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with SAL.Gen_Bounded_Definite_Vectors.Gen_Refs;
with WisiToken.Parse.LR.McKenzie_Recover.Base;
private package WisiToken.Parse.LR.McKenzie_Recover.Parse is

   function Reduce_Stack
     (Shared          : not null access Base.Shared;
      Stack           : in out          Recover_Stacks.Stack;
      Action          : in              Reduce_Action_Rec;
      Nonterm         :    out          Recover_Token;
      Default_Virtual : in              Boolean)
     return Semantic_Checks.Check_Status;
   --  Reduce Stack according to Action, setting Nonterm. If
   --  Action.Token_Count = 0, set Nonterm.Virtual := Default_Virtual.

   type Parse_Item is record
      Config      : Configuration;
      Action      : Parse_Action_Node_Ptr;
      Parsed      : Boolean;
      Shift_Count : Natural := 0;

      --  On return from Parse, if Parsed = False, this item was queued by a
      --  conflict, but not parsed; it should be ignored.
      --
      --  Otherwise, if Config.Error_Token.ID = Invalid_Token_ID and
      --  Config.Check_Status.Label = Ok, Config was parsed successfully to
      --  the goal.
      --
      --  Otherwise, the parser failed a semantic check, or encountered an
      --  Error action. Action gives the last action processed. Shift_Count
      --  gives the number of shifts performed. If Check_Status.Label is
      --  Error, Action.Item.Verb must be Reduce, and Config is in the
      --  pre-reduce state.
   end record;

   package Parse_Item_Arrays is new SAL.Gen_Bounded_Definite_Vectors (Positive, Parse_Item, Capacity => 10);
   --  Parse_Item_Arrays.Capacity sets maximum conflicts in one call to Parse

   package Parse_Item_Array_Refs is new Parse_Item_Arrays.Gen_Refs;

   function Parse
     (Super             :         not null access Base.Supervisor;
      Shared            :         not null access Base.Shared;
      Parser_Index      :         in              SAL.Peek_Type;
      Parse_Items       : aliased    out          Parse_Item_Arrays.Vector;
      Config            :         in              Configuration;
      Shared_Token_Goal :         in              Base_Token_Index;
      All_Conflicts     :         in              Boolean;
      Trace_Prefix      :         in              String)
     return Boolean;
   --  Attempt to parse Config and any conflict configs. If not
   --  All_Conflicts, return when Config.Insert_Delete is all processed,
   --  and either Shared_Token_Goal = Invalid_Token_Index or
   --  Shared_Token_Goal is shifted. If All_Conflicts, return when all
   --  conflict configs have been parsed.
   --
   --  Parsed configs are in Parse_Items; there is more than one if a
   --  conflict is encountered. Parse returns True if at least one
   --  Parse_Item parsed successfully to the goal. In that case, the
   --  other items are either not parsed or failed. See comment in
   --  Parse_Item for more detail.
   --
   --  Raises Bad_Config if parse encounters Unknown_State.

end WisiToken.Parse.LR.McKenzie_Recover.Parse;
