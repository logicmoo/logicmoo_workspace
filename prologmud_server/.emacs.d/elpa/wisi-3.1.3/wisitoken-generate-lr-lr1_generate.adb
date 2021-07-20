--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers;
with Ada.Text_IO;
with WisiToken.Generate;
package body WisiToken.Generate.LR.LR1_Generate is

   function LR1_Goto_Transitions
     (Set                     : in LR1_Items.Item_Set;
      Symbol                  : in Token_ID;
      Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return LR1_Items.Item_Set
   is
      use all type Ada.Containers.Count_Type;
      use Token_ID_Arrays;
      use LR1_Items;

      Goto_Set : Item_Set;
   begin
      for Item of Set.Set loop
         if Item.Dot /= No_Index then
            declare
               Dot : constant Token_ID_Arrays.Cursor := Productions.Constant_Ref_RHS
                 (Grammar, Item.Prod).Tokens.To_Cursor (Item.Dot);
            begin
               if Element (Dot) = Symbol and
                 --  We don't need a state with dot after EOI in the
                 --  accept production. EOI should only appear in the
                 --  accept production.
                 Symbol /= Descriptor.EOI_ID
               then
                  Goto_Set.Set.Insert
                    ((Item.Prod,
                      To_Index (Next (Dot)),
                      new Token_ID_Set'(Item.Lookaheads.all)));
               end if;
            end;
         end if;
      end loop;

      if Goto_Set.Set.Length > 0 then
         return Closure (Goto_Set, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
      else
         return Goto_Set;
      end if;
   end LR1_Goto_Transitions;

   function LR1_Item_Sets
     (Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return LR1_Items.Item_Set_List
   is
      use all type Ada.Containers.Count_Type;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure
      --  "items", with some optimizations.

      use LR1_Items;

      First_State_Index : constant State_Index := 0;

      C               : LR1_Items.Item_Set_List;       -- result
      C_Tree          : LR1_Items.Item_Set_Trees.Tree; -- for fast find
      States_To_Check : State_Index_Queues.Queue;
      --  [dragon] specifies 'until no more items can be added', but we use
      --  a queue to avoid checking unecessary states. Ada LR1 has over
      --  100,000 states, so this is a significant gain (reduced time from
      --  600 seconds to 40).

      I       : State_Index;
      Dot_IDs : Token_ID_Arrays.Vector;

      New_Item_Set : Item_Set := Closure
        ((Set            => Item_Lists.To_List
            ((Prod       => (Grammar.First_Index, 0),
              Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First_Index,
              Lookaheads => new Token_ID_Set'(To_Lookahead (Descriptor.EOI_ID, Descriptor)))),
          Goto_List      => <>,
          Dot_IDs        => <>,
          State          => First_State_Index),
        Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

      Found_State  : Unknown_State_Index;

   begin
      C.Set_First_Last (First_State_Index, First_State_Index - 1);

      Add (Grammar, New_Item_Set, C, C_Tree, Descriptor, Include_Lookaheads => True);

      States_To_Check.Put (First_State_Index);
      loop
         exit when States_To_Check.Is_Empty;
         I := States_To_Check.Get;

         if Trace_Generate_Table > Outline then
            Ada.Text_IO.Put ("Checking ");
            Put (Grammar, Descriptor, C (I), Show_Lookaheads => True, Show_Goto_List => True);
         end if;

         Dot_IDs := C (I).Dot_IDs;
         --  We can't iterate on C (I).Dot_IDs when the loop adds items to C;
         --  it might be reallocated to grow.

         for Symbol of Dot_IDs loop
            --  [dragon] has 'for each grammar symbol X', but LR1_Goto_Transitions
            --  rejects Symbol that is not in Dot_IDs, so we iterate over that.

            New_Item_Set := LR1_Goto_Transitions
              (C (I), Symbol, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

            if New_Item_Set.Set.Length > 0 then -- 'goto (I, X) not empty'

               Found_State := Find (New_Item_Set, C_Tree, Match_Lookaheads => True); -- 'not in C'

               if Found_State = Unknown_State then
                  New_Item_Set.State := C.Last_Index + 1;

                  States_To_Check.Put (New_Item_Set.State);

                  Add (Grammar, New_Item_Set, C, C_Tree, Descriptor, Include_Lookaheads => True);

                  if Trace_Generate_Table > Outline then
                     Ada.Text_IO.Put_Line
                       ("  adding state" & Unknown_State_Index'Image (C.Last_Index) & ": from state" &
                          Unknown_State_Index'Image (I) & " on " & Image (Symbol, Descriptor));
                     Put (Grammar, Descriptor, New_Item_Set, Show_Lookaheads => True);
                  end if;

                  C (I).Goto_List.Insert ((Symbol, C.Last_Index));
               else

                  --  If there's not already a goto entry between these two sets, create one.
                  if not Is_In ((Symbol, Found_State), Goto_List => C (I).Goto_List) then
                     if Trace_Generate_Table > Outline then
                        Ada.Text_IO.Put_Line
                          ("  adding goto on " & Image (Symbol, Descriptor) & " to state" &
                             Unknown_State_Index'Image (Found_State));

                     end if;

                     C (I).Goto_List.Insert ((Symbol, Found_State));
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.New_Line;
      end if;

      return C;
   end LR1_Item_Sets;

   procedure Add_Actions
     (Item_Sets            : in     LR1_Items.Item_Set_List;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First_Nonterm_Set    : in     Token_Array_Token_Set;
      Conflict_Counts      :    out Conflict_Count_Lists.List;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Descriptor           : in     WisiToken.Descriptor)
   is
      --  Add actions for all Item_Sets to Table.
   begin
      for Item_Set of Item_Sets loop
         Add_Actions
           (Item_Set, Table, Grammar, Has_Empty_Production, First_Nonterm_Set, Conflict_Counts, Conflicts, Descriptor);
      end loop;

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   function Generate
     (Grammar               : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor            : in     WisiToken.Descriptor;
      Known_Conflicts       : in     Conflict_Lists.List := Conflict_Lists.Empty_List;
      McKenzie_Param        : in     McKenzie_Param_Type := Default_McKenzie_Param;
      Parse_Table_File_Name : in     String              := "";
      Include_Extra         : in     Boolean             := False;
      Ignore_Conflicts      : in     Boolean             := False;
      Partial_Recursion     : in     Boolean             := True)
     return Parse_Table_Ptr
   is
      use type Ada.Containers.Count_Type;

      Ignore_Unused_Tokens     : constant Boolean := WisiToken.Trace_Generate_Table > Detail;
      Ignore_Unknown_Conflicts : constant Boolean := Ignore_Conflicts or WisiToken.Trace_Generate_Table > Detail;
      Unused_Tokens            : constant Boolean := WisiToken.Generate.Check_Unused_Tokens (Descriptor, Grammar);

      Table : Parse_Table_Ptr;

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      Recursions : constant WisiToken.Generate.Recursions :=
        (if Partial_Recursion
         then WisiToken.Generate.Compute_Partial_Recursion (Grammar, Descriptor)
         else WisiToken.Generate.Compute_Full_Recursion (Grammar, Descriptor));
      Minimal_Terminal_Sequences : constant Minimal_Sequence_Array :=
        Compute_Minimal_Terminal_Sequences (Descriptor, Grammar);

      Minimal_Terminal_First : constant Token_Array_Token_ID :=
        Compute_Minimal_Terminal_First (Descriptor, Minimal_Terminal_Sequences);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Item_Sets : constant LR1_Items.Item_Set_List := LR1_Item_Sets
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

      Conflict_Counts      : Conflict_Count_Lists.List;
      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;
   begin
      if Trace_Generate_Table + Trace_Generate_Minimal_Complete > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR1_Generate:");
         if Trace_Generate_Table > Outline then
            Ada.Text_IO.Put_Line ("Item_Sets:");
            LR1_Items.Put (Grammar, Descriptor, Item_Sets);
         end if;
      end if;

      Table := new Parse_Table
        (State_First       => Item_Sets.First_Index,
         State_Last        => Item_Sets.Last_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      if McKenzie_Param = Default_McKenzie_Param then
         --  Descriminants in Default are wrong
         Table.McKenzie_Param :=
           (First_Terminal              => Descriptor.First_Terminal,
            Last_Terminal               => Descriptor.Last_Terminal,
            First_Nonterminal           => Descriptor.First_Nonterminal,
            Last_Nonterminal            => Descriptor.Last_Nonterminal,
            Insert                      => (others => 0),
            Delete                      => (others => 0),
            Push_Back                   => (others => 0),
            Undo_Reduce                 => (others => 0),
            Minimal_Complete_Cost_Delta => Default_McKenzie_Param.Minimal_Complete_Cost_Delta,
            Fast_Forward                => Default_McKenzie_Param.Fast_Forward,
            Matching_Begin              => Default_McKenzie_Param.Matching_Begin,
            Ignore_Check_Fail           => Default_McKenzie_Param.Ignore_Check_Fail,
            Task_Count                  => Default_McKenzie_Param.Task_Count,
            Check_Limit                 => Default_McKenzie_Param.Check_Limit,
            Check_Delta_Limit           => Default_McKenzie_Param.Check_Delta_Limit,
            Enqueue_Limit               => Default_McKenzie_Param.Enqueue_Limit);
      else
         Table.McKenzie_Param := McKenzie_Param;
      end if;

      Add_Actions
        (Item_Sets, Grammar, Has_Empty_Production, First_Nonterm_Set,
         Conflict_Counts, Unknown_Conflicts, Table.all, Descriptor);

      for State in Table.States'Range loop
         if Trace_Generate_Minimal_Complete > Extra then
            Ada.Text_IO.Put_Line ("Set_Minimal_Complete_Actions:" & State_Index'Image (State));
         end if;
         WisiToken.Generate.LR.Set_Minimal_Complete_Actions
           (Table.States (State),
            LR1_Items.Filter (Item_Sets (State), Grammar, Descriptor, LR1_Items.In_Kernel'Access),
            Descriptor, Grammar, Nullable, Minimal_Terminal_Sequences, Minimal_Terminal_First);
      end loop;

      if Parse_Table_File_Name /= "" then
         WisiToken.Generate.LR.Put_Parse_Table
           (Table, Parse_Table_File_Name, "LR1", Grammar, Recursions, Item_Sets, Conflict_Counts, Descriptor,
            Include_Extra);
      end if;

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Has_Empty_Production: " & Image (Has_Empty_Production, Descriptor));

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Minimal_Terminal_First:");
         for ID in Minimal_Terminal_First'Range loop
            Ada.Text_IO.Put_Line
              (Image (ID, Descriptor) & " =>" &
                 (if Minimal_Terminal_First (ID) = Invalid_Token_ID
                  then ""
                  else ' ' & Image (Minimal_Terminal_First (ID), Descriptor)));
         end loop;
      end if;

      Delete_Known (Unknown_Conflicts, Known_Conflicts_Edit);

      if Unknown_Conflicts.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "unknown conflicts:");
         Put (Unknown_Conflicts, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Unknown_Conflicts;
      end if;

      if Known_Conflicts_Edit.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "excess known conflicts:");
         Put (Known_Conflicts_Edit, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Unknown_Conflicts;
      end if;

      WisiToken.Generate.Error := WisiToken.Generate.Error or (Unused_Tokens and not Ignore_Unused_Tokens);

      return Table;
   end Generate;

end WisiToken.Generate.LR.LR1_Generate;
