--  Abstract :
--
--  Common utilities for LR parser table generators.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with Ada.Containers.Doubly_Linked_Lists;
with WisiToken.Generate.LR1_Items;
with WisiToken.Parse.LR;
with WisiToken.Productions;
package WisiToken.Generate.LR is
   use WisiToken.Parse.LR;

   subtype Conflict_Parse_Actions is Parse_Action_Verbs range Shift .. Accept_It;
   type Conflict is record
      --  A typical conflict is:
      --
      --  SHIFT/REDUCE in state: 11 on token IS
      --
      --  State numbers change with minor changes in the grammar, so we
      --  attempt to identify the state by the LHS of the two productions
      --  involved; this is _not_ guarranteed to be unique, but is good
      --  enough for our purposes. We also store the state number for
      --  generated conflicts (not for known conflicts from the grammar
      --  definition file), for debugging.
      Action_A    : Conflict_Parse_Actions;
      LHS_A       : Token_ID;
      Action_B    : Conflict_Parse_Actions;
      LHS_B       : Token_ID;
      State_Index : Unknown_State_Index;
      On          : Token_ID;
   end record;

   package Conflict_Lists is new Ada.Containers.Doubly_Linked_Lists (Conflict);

   type Conflict_Count is record
      State : State_Index;
      Accept_Reduce : Integer             := 0;
      Shift_Reduce  : Integer             := 0;
      Reduce_Reduce : Integer             := 0;
   end record;

   package Conflict_Count_Lists is new Ada.Containers.Doubly_Linked_Lists (Conflict_Count);

   procedure Put
     (Item       : in Conflict_Lists.List;
      File       : in Ada.Text_IO.File_Type;
      Descriptor : in WisiToken.Descriptor);

   procedure Add_Action
     (Symbol               : in     Token_ID;
      Action               : in     Parse_Action_Rec;
      Action_List          : in out Action_Arrays.Vector;
      Closure              : in     LR1_Items.Item_Set;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflict_Counts      : in out Conflict_Count_Lists.List;
      Conflicts            : in out Conflict_Lists.List;
      Descriptor           : in     WisiToken.Descriptor);
   --  Add (Symbol, Action) to Action_List; check for conflicts
   --
   --  Closure .. Conflicts are for conflict reporting

   procedure Add_Actions
     (Closure              : in     LR1_Items.Item_Set;
      Table                : in out Parse_Table;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflict_Counts      : in out Conflict_Count_Lists.List;
      Conflicts            : in out Conflict_Lists.List;
      Descriptor           : in     WisiToken.Descriptor);
   --  Add actions for Closure to Table. Has_Empty_Production, First,
   --  Conflicts used for conflict reporting.

   procedure Add_Lookahead_Actions
     (Item                 : in     LR1_Items.Item;
      Action_List          : in out Action_Arrays.Vector;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflict_Counts      : in out Conflict_Count_Lists.List;
      Conflicts            : in out Conflict_Lists.List;
      Closure              : in     LR1_Items.Item_Set;
      Descriptor           : in     WisiToken.Descriptor);
   --  Add actions for Item.Lookaheads to Action_List
   --  Closure must be from the item set containing Item.
   --  Has_Empty_Production .. Closure used for conflict reporting.

   procedure Delete_Known
     (Conflicts       : in out Conflict_Lists.List;
      Known_Conflicts : in out Conflict_Lists.List);
   --  Delete Known_Conflicts from Conflicts.

   function Find
     (Closure              : in LR1_Items.Item_Set;
      Action               : in Parse_Action_Rec;
      Lookahead            : in Token_ID;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Descriptor           : in WisiToken.Descriptor)
     return Token_ID;
   --  Return the LHS of a production in kernel of Closure, for an Action
   --  conflict on Lookahead; for naming a Conflict object.

   function Image (Item : in Conflict; Descriptor : in WisiToken.Descriptor) return String;

   function Is_Present (Item : in Conflict; Conflicts : in Conflict_Lists.List) return Boolean;

   function Match (Known : in Conflict; Item : in Conflict_Lists.Constant_Reference_Type) return Boolean;

   ----------
   --  Minimal terminal sequences.

   package RHS_Sequence_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Natural, Token_ID_Arrays.Vector, Default_Element => Token_ID_Arrays.Empty_Vector);

   function Image is new RHS_Sequence_Arrays.Gen_Image_Aux (Descriptor, Trimmed_Image, Image_No_Assoc);

   function Min_Length (Item : in RHS_Sequence_Arrays.Vector) return Ada.Containers.Count_Type;
   --  Return minimum length of elements of Item.

   function Min (Item : in RHS_Sequence_Arrays.Vector) return Token_ID_Arrays.Vector;
   --  Return element of Item with minimum length;

   type Minimal_Sequence_Item is record
      Min_RHS  : Natural := Natural'Last;
      Sequence : RHS_Sequence_Arrays.Vector;
   end record;

   type Minimal_Sequence_Array is array (Token_ID range <>) of Minimal_Sequence_Item;

   function Compute_Minimal_Terminal_Sequences
     (Descriptor : in WisiToken.Descriptor;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector)
     return Minimal_Sequence_Array;
   --  For each production in Grammar, compute the minimal sequence of
   --  terminals that will complete it. Result is an empty sequence if
   --  the production may be empty.

   function Compute_Minimal_Terminal_First
     (Descriptor                 : in WisiToken.Descriptor;
      Minimal_Terminal_Sequences : in Minimal_Sequence_Array)
      return Token_Array_Token_ID;
   --  For each nonterminal in Grammar, return the first of the minimal
   --  sequence of terminals that will complete it; Invalid_Token_ID if
   --  the minimal sequence is empty.

   procedure Set_Minimal_Complete_Actions
     (State                      : in out Parse_State;
      Kernel                     : in     LR1_Items.Item_Set;
      Descriptor                 : in     WisiToken.Descriptor;
      Grammar                    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Nullable                   : in     Token_Array_Production_ID;
      Minimal_Terminal_Sequences : in     Minimal_Sequence_Array;
      Minimal_Terminal_First     : in     Token_Array_Token_ID);
   --  Set State.Minimal_Complete_Actions to the set of actions that will
   --  most quickly complete the productions in Kernel (which must be for
   --  State). Useful in error correction.
   --
   --  The Minimal_Complete_Actions will be empty in a state where there
   --  is nothing useful to do; the accept state, or one where all
   --  productions are recursive.
   --
   --  Also set State.Kernels; used to resolve multiple reduce actions at
   --  runtime.

   ----------
   --  Parse table output

   procedure Put_Text_Rep
     (Table        : in Parse_Table;
      File_Name    : in String;
      Action_Names : in Names_Array_Array;
      Check_Names  : in Names_Array_Array);
   --  Write machine-readable text format of Table.States to a file
   --  File_Name, to be read by the parser executable at startup, using
   --  WisiToken.Parse.LR.Get_Text_Rep.

   procedure Put (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor);
   procedure Put (Item : in McKenzie_Param_Type; Descriptor : in WisiToken.Descriptor);
   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Parse_Action_Rec);
   procedure Put (Descriptor : in WisiToken.Descriptor; Action : in Parse_Action_Node_Ptr);
   procedure Put (Descriptor : in WisiToken.Descriptor; State : in Parse_State);
   --  Put Item to Ada.Text_IO.Current_Output in parse table format.

   procedure Put_Parse_Table
     (Table                 : in Parse_Table_Ptr;
      Parse_Table_File_Name : in String;
      Title                 : in String;
      Grammar               : in WisiToken.Productions.Prod_Arrays.Vector;
      Recursions            : in Generate.Recursions;
      Kernels               : in LR1_Items.Item_Set_List;
      Conflicts             : in Conflict_Count_Lists.List;
      Descriptor            : in WisiToken.Descriptor;
      Include_Extra         : in Boolean := False);
   --  "Extra" is recursions.

end WisiToken.Generate.LR;
