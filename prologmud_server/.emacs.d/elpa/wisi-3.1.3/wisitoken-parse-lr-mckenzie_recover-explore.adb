--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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

with Ada.Exceptions;
with SAL.Gen_Bounded_Definite_Queues;
with WisiToken.Parse.LR.McKenzie_Recover.Parse;
with WisiToken.Parse.LR.Parser;
package body WisiToken.Parse.LR.McKenzie_Recover.Explore is

   procedure Do_Shift
     (Label             : in              String;
      Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      State             : in              State_Index;
      ID                : in              Token_ID;
      Cost_Delta        : in              Integer;
      Strategy          : in              Strategies)
   is
      use Config_Op_Arrays;
      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      Op : constant Config_Op := (Insert, ID, Config.Current_Shared_Token);
   begin
      Config.Strategy_Counts (Strategy) := Config.Strategy_Counts (Strategy) + 1;

      if Is_Full (Config.Ops) then
         Super.Config_Full ("do_shift ops", Parser_Index);
         raise Bad_Config;
      else
         Append (Config.Ops, Op);
      end if;

      if Cost_Delta = 0 then
         Config.Cost := Config.Cost + McKenzie_Param.Insert (ID);
      else
         --  Cost_Delta /= 0 comes from Insert_Minimal_Complete_Actions. That
         --  doesn't mean it is better than any other solution, so don't let
         --  cost be 0.
         --
         --  We don't just eliminate all cost for Minimal_Complete_Actions;
         --  that leads to using it far too much at the expense of better
         --  solutions.
         Config.Cost := Integer'Max (1, Config.Cost + McKenzie_Param.Insert (ID) + Cost_Delta);
      end if;

      Config.Error_Token.ID := Invalid_Token_ID;
      Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

      if Config.Stack.Is_Full then
         Super.Config_Full ("do_shift stack", Parser_Index);
         raise Bad_Config;
      else
         Config.Stack.Push ((State, Invalid_Node_Index, (ID, Virtual => True, others => <>)));
      end if;
      if Trace_McKenzie > Detail then
         Base.Put
           ((if Label'Length > 0 then Label & ": " else "") & "insert " & Image (ID, Super.Trace.Descriptor.all),
            Super, Shared, Parser_Index, Config);
      end if;

      Local_Config_Heap.Add (Config);
   end Do_Shift;

   procedure Do_Reduce_1
     (Label             : in              String;
      Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Action            : in              Reduce_Action_Rec;
      Do_Language_Fixes : in              Boolean := True)
   is
      use all type Semantic_Checks.Check_Status_Label;
      use all type WisiToken.Parse.LR.Parser.Language_Fixes_Access;

      Prev_State : constant Unknown_State_Index := Config.Stack.Peek.State;

      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;
      Nonterm    : Recover_Token;
      New_State  : Unknown_State_Index;
   begin
      Config.Check_Status := Parse.Reduce_Stack (Shared, Config.Stack, Action, Nonterm, Default_Virtual => True);
      case Config.Check_Status.Label is
      when Ok =>
         null;

      when Semantic_Checks.Error =>
         Config.Error_Token       := Nonterm;
         Config.Check_Token_Count := Action.Token_Count;

         if Do_Language_Fixes then
            if Shared.Language_Fixes /= null then
               Shared.Language_Fixes
                 (Super.Trace.all, Shared.Lexer, Super.Label (Parser_Index), Shared.Table.all, Shared.Terminals.all,
                  Super.Parser_State (Parser_Index).Tree, Local_Config_Heap,
                  Config);
            end if;
         end if;

         --  Finish the reduce; ignore the check fail.
         if Config.Stack.Depth < SAL.Base_Peek_Type (Config.Check_Token_Count) then
            raise SAL.Programmer_Error;
         else
            Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));
         end if;
         Config.Error_Token.ID := Invalid_Token_ID;
         Config.Check_Status   := (Label => Ok);
      end case;

      if Config.Stack.Depth = 0 or else Config.Stack.Peek.State = Unknown_State then
         raise Bad_Config;
      end if;

      New_State := Goto_For (Table, Config.Stack.Peek.State, Action.Production.LHS);

      if New_State = Unknown_State then
         if Trace_McKenzie > Extra then
            Put_Line
              (Super.Trace.all, Super.Label (Parser_Index), Label &
                 ": Do_Reduce_1: unknown_State " & Config.Stack.Peek.State'Image & " " &
                 Image (Action.Production.LHS, Descriptor));
         end if;
         raise Bad_Config;
      end if;

      Config.Stack.Push ((New_State, Invalid_Node_Index, Nonterm));

      if Trace_McKenzie > Extra and Label'Length > 0 then
         Put_Line
           (Super.Trace.all, Super.Label (Parser_Index), Label &
              ": state" & State_Index'Image (Prev_State) & " reduce" &
              Ada.Containers.Count_Type'Image (Action.Token_Count) & " to " &
              Image (Action.Production.LHS, Descriptor) & ", goto" &
              State_Index'Image (New_State) & " via" & State_Index'Image (Config.Stack.Peek (2).State));
      end if;
   end Do_Reduce_1;

   procedure Do_Reduce_2
     (Label             : in              String;
      Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in out          Configuration;
      Inserted_ID       : in              Token_ID;
      Cost_Delta        : in              Integer;
      Strategy          : in              Strategies)
   is
      --  Perform reduce actions until shift Inserted_ID; if all succeed,
      --  add the final configuration to the heap, return True. If a conflict is
      --  encountered, process the other action the same way. If a semantic
      --  check fails, enqueue possible solutions. For parse table error
      --  actions, or exception Bad_Config, return False.

      Orig_Config : Configuration;
      Table       : Parse_Table renames Shared.Table.all;
      Next_Action : Parse_Action_Node_Ptr := Action_For (Table, Config.Stack.Peek.State, Inserted_ID);
   begin
      if Next_Action.Next /= null then
         Orig_Config := Config;
      end if;

      case Next_Action.Item.Verb is
      when Shift =>
         Do_Shift
           (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item.State, Inserted_ID,
            Cost_Delta, Strategy);

      when Reduce =>
         Do_Reduce_1 (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Next_Action.Item);
         Do_Reduce_2
           (Label, Super, Shared, Parser_Index, Local_Config_Heap, Config, Inserted_ID, Cost_Delta, Strategy);

      when Accept_It =>
         raise SAL.Programmer_Error with "found test case for Do_Reduce Accept_It";

      when Error =>
         if Trace_McKenzie > Extra and Label'Length > 0 then
            Put_Line
              (Super.Trace.all, Super.Label (Parser_Index), Label & ": error on " &
                 Image (Inserted_ID, Super.Trace.Descriptor.all) &
                 " in state" & State_Index'Image (Config.Stack.Peek.State));
         end if;
      end case;

      loop
         exit when Next_Action.Next = null;
         --  There is a conflict; create a new config to shift or reduce.
         declare
            New_Config : Configuration := Orig_Config;
            Action     : Parse_Action_Rec renames Next_Action.Next.Item;
         begin
            case Action.Verb is
            when Shift =>
               Do_Shift
                 (Label, Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, Inserted_ID,
                  Cost_Delta, Strategy);

            when Reduce =>
               Do_Reduce_1 (Label, Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action);
               Do_Reduce_2
                 (Label, Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Inserted_ID,
                  Cost_Delta, Strategy);

            when Accept_It =>
               raise SAL.Programmer_Error with "found test case for Do_Reduce Accept_It conflict";

            when Error =>
               null;
            end case;
         end;

         Next_Action := Next_Action.Next;
      end loop;
   exception
   when Bad_Config =>
      if Debug_Mode then
         raise;
      end if;
   end Do_Reduce_2;

   function Edit_Point_Matches_Ops (Config : in Configuration) return Boolean
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;
      pragma Assert (Length (Config.Ops) > 0);
      Op : Config_Op renames Constant_Ref (Config.Ops, Last_Index (Config.Ops));
   begin
      return Config.Current_Shared_Token =
        (case Op.Op is
         when Fast_Forward => Op.FF_Token_Index,
         when Undo_Reduce  => Invalid_Token_Index, -- ie, "we don't know", so return False.
         when Push_Back    => Op.PB_Token_Index,
         when Insert       => Op.Ins_Token_Index,
         when Delete       => Op.Del_Token_Index + 1);
   end Edit_Point_Matches_Ops;

   procedure Fast_Forward
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in              Configuration)
   is
      --  Apply the ops in Config; they were inserted by some fix.
      --  Leaves Config.Error_Token, Config.Check_Status set.
      --  If there are conflicts, all are parsed; if more than one succeed.
      --  All configs are enqueued in Local_Config_Heap.

      use Parse.Parse_Item_Arrays;
      use Config_Op_Arrays;

      Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;

      Dummy : Boolean := Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config,
         Shared_Token_Goal => Invalid_Token_Index,
         All_Conflicts     => True,
         Trace_Prefix      => "fast_forward");
   begin
      --  This solution is from Language_Fixes (see gate on call site
      --  below); any cost increase is done there.
      --
      --  We used to handle the Parse_Items.Length = 1 case specially, and
      --  return Continue. Maintaining that requires too much code
      --  duplication.

      for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
         begin
            if Item.Parsed and Item.Config.Current_Insert_Delete = No_Insert_Delete then
               --  Item.Config.Error_Token.ID, Check_Status are correct.

               if not Edit_Point_Matches_Ops (Item.Config) then

                  if Is_Full (Item.Config.Ops) then
                     Super.Config_Full ("fast_forward 1", Parser_Index);
                     raise Bad_Config;
                  else
                     Append (Item.Config.Ops, (Fast_Forward, Item.Config.Current_Shared_Token));
                  end if;
               end if;

               Item.Config.Minimal_Complete_State := None;
               Item.Config.Matching_Begin_Done    := False;
               Local_Config_Heap.Add (Item.Config);

               if Trace_McKenzie > Detail then
                  Base.Put ("fast forward enqueue", Super, Shared, Parser_Index, Item.Config);
               end if;
            end if;
         exception
         when Bad_Config =>
            null;
         end;
      end loop;
   end Fast_Forward;

   function Check
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
     return Check_Status
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;
      use Parse.Parse_Item_Arrays;
      use all type Semantic_Checks.Check_Status_Label;

      Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;
      Result      : Check_Status := Continue;
   begin
      if Length (Config.Ops) > 0 then
         declare
            Op : Config_Op renames Constant_Ref (Config.Ops, Last_Index (Config.Ops));
         begin
            case Op.Op is
            when Push_Back =>
               --  Check would undo the Push_Back, leading to
               --  duplicate results. See test_mckenzie_recover.adb Do_Delete_First and
               --  three_action_conflict_lalr.parse_good for examples.
               return Continue;

            when Undo_Reduce =>
               if Config.Check_Status.Label /= Ok then
                  --  This is the "ignore error" solution for a check fail; check it.
                  Config.Check_Status   := (Label => Ok);
                  Config.Error_Token.ID := Invalid_Token_ID;

               else
                  --  Check would undo the Undo_Reduce, leading to
                  --  duplicate results.
                  return Continue;
               end if;
            when others =>
               --  Check it
               null;
            end case;
         end;
      end if;

      if Parse.Parse
        (Super, Shared, Parser_Index, Parse_Items, Config, Config.Resume_Token_Goal,
         All_Conflicts => False,
         Trace_Prefix  => "check")
      then
         Config.Error_Token.ID := Invalid_Token_ID;
         --  FIXME: if there were conflicts, enqueue them; they might yield a
         --  cheaper or same cost solution?
         if Trace_McKenzie > Extra then
            Put_Line (Super.Trace.all, Super.Label (Parser_Index), "check result: SUCCESS");
         end if;
         return Success;
      end if;

      --  Set Config.error to reflect failure, if it is at current token, so
      --  Use_Minimal_Complete_Actions can see it.
      declare
         Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Constant_Ref
           (Parse_Items, First_Index (Parse_Items));
      begin
         if Item.Config.Check_Status.Label /= Ok then
            Config.Check_Status := Item.Config.Check_Status;
            Config.Error_Token  := Item.Config.Error_Token;

            --  Explore cannot fix a check fail; only Language_Fixes can. The
            --  "ignore error" case is handled immediately on return from
            --  Language_Fixes in Process_One, below.
            Result := Abandon;

         elsif Item.Config.Error_Token.ID /= Invalid_Token_ID then

            if Item.Shift_Count = 0 then
               Config.Error_Token  := Item.Config.Error_Token;
               Config.Check_Status := (Label => Ok);
            else
               --  Error is not at current token, but Explore might find something
               --  that will help (see test_mckenzie_recover.adb Extra_Begin). On the
               --  other hand, this can lead to lots of bogus configs (see
               --  If_In_Handler).
               Config.Error_Token.ID := Invalid_Token_ID;
               Config.Check_Status   := (Label => Ok);
            end if;
         end if;
      end;

      --  All Parse_Items either failed or were not parsed; if they failed
      --  and made progress, enqueue them.
      for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
         declare
            Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
         begin
            --  When Parse starts above, Config.Current_Shared_Token matches
            --  Config.Ops. So if Item.Config.Current_Shared_Token >
            --  Config.Current_Shared_Token, it made some progress. Append or
            --  update a Fast_Forward to indicate the changed edit point.
            if Item.Config.Error_Token.ID /= Invalid_Token_ID and
              Item.Config.Current_Shared_Token > Config.Current_Shared_Token
            then
               Item.Config.Minimal_Complete_State := None;
               Item.Config.Matching_Begin_Done    := False;

               if Constant_Ref (Item.Config.Ops, Last_Index (Item.Config.Ops)).Op = Fast_Forward then
                  --  Update the trailing Fast_Forward.
                  Variable_Ref (Item.Config.Ops, Last_Index (Item.Config.Ops)).FF_Token_Index :=
                    Item.Config.Current_Shared_Token;
               else
                  if Is_Full (Item.Config.Ops) then
                     Super.Config_Full ("check 1", Parser_Index);
                     raise Bad_Config;
                  else
                     Append (Item.Config.Ops, (Fast_Forward, Item.Config.Current_Shared_Token));
                  end if;
               end if;
               Local_Config_Heap.Add (Item.Config);
               if Trace_McKenzie > Detail then
                  Base.Put ("new error point ", Super, Shared, Parser_Index, Item.Config);
               end if;
            end if;
         end;
      end loop;

      if Trace_McKenzie > Extra then
         Put_Line (Super.Trace.all, Super.Label (Parser_Index), "check result: " & Result'Image);
      end if;
      return Result;
   exception
   when Bad_Config =>
      return Abandon;
   end Check;

   function Check_Reduce_To_Start
     (Super        : not null access Base.Supervisor;
      Shared       : not null access Base.Shared;
      Parser_Index : in              SAL.Base_Peek_Type;
      Orig_Config  : in              Configuration)
     return Boolean
      --  Returns True if Config reduces to the start nonterm.
   is
      Table : Parse_Table renames Shared.Table.all;

      function To_Reduce_Action (Item : in Minimal_Action) return Reduce_Action_Rec
      is begin
         return (Reduce, Item.Production, null, null, Item.Token_Count);
      end To_Reduce_Action;

      Local_Config_Heap : Config_Heaps.Heap_Type; -- never used, because Do_Language_Fixes is False.

      Config  : Configuration                := Orig_Config;
      Actions : Minimal_Action_Arrays.Vector := Table.States (Config.Stack.Peek.State).Minimal_Complete_Actions;
   begin
      loop
         case Actions.Length is
         when 0 =>
            if (for some Item of Table.States (Config.Stack.Peek.State).Kernel =>
                  Item.Production.LHS = Super.Trace.Descriptor.Accept_ID)
            then
               return True;
            else
               return False;
            end if;

         when 1 =>
            case Actions (Actions.First_Index).Verb is
            when Shift =>
               return False;

            when Reduce =>
               Do_Reduce_1
                 ("", Super, Shared, Parser_Index, Local_Config_Heap, Config,
                  To_Reduce_Action (Actions (Actions.First_Index)),
                  Do_Language_Fixes => False);

               Actions := Table.States (Config.Stack.Peek.State).Minimal_Complete_Actions;
            end case;

         when others =>
            return False;
         end case;

         --  loop only exits via returns above
      end loop;
   exception
   when Bad_Config =>
      --  From Do_Reduce_1
      return False;
   end Check_Reduce_To_Start;

   procedure Try_Push_Back
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      Trace          : WisiToken.Trace'Class renames Super.Trace.all;
      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;
      Prev_Recover   : constant WisiToken.Base_Token_Index := Super.Parser_State (Parser_Index).Resume_Token_Goal;

      Token : constant Recover_Token := Config.Stack.Peek.Token;
   begin
      --  Try pushing back the stack top, to allow insert and other
      --  operations at that point.
      --
      --  Since we are not actually changing the source text, it is tempting
      --  to give this operation zero cost. But then we keep doing push_back
      --  forever, making no progress. So we give it a cost.

      if Token.Min_Terminal_Index /= Invalid_Token_Index and
        --  No point in pushing back an empty nonterm; that leads to duplicate
        --  solutions with Undo_Reduce; see test_mckenzie_recover.adb Error_2.

        (Prev_Recover = Invalid_Token_Index or else Prev_Recover < Token.Min_Terminal_Index)
        --  Don't push back past previous error recover (that would require
        --  keeping track of previous inserts/deletes, and would not be useful
        --  in most cases).
      then
         declare
            use Config_Op_Arrays;
            New_Config : Configuration := Config;
         begin
            New_Config.Error_Token.ID := Invalid_Token_ID;
            New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

            New_Config.Stack.Pop;

            if Is_Full (New_Config.Ops) then
               Super.Config_Full ("push_back 1", Parser_Index);
               raise Bad_Config;
            else
               if Token.Min_Terminal_Index = Invalid_Token_Index then
                  --  Token is empty; Config.current_shared_token does not change, no
                  --  cost increase.
                  Append (New_Config.Ops, (Push_Back, Token.ID, New_Config.Current_Shared_Token));
               else
                  New_Config.Cost := New_Config.Cost + McKenzie_Param.Push_Back (Token.ID);
                  Append (New_Config.Ops, (Push_Back, Token.ID, Token.Min_Terminal_Index));
                  New_Config.Current_Shared_Token := Token.Min_Terminal_Index;
               end if;
            end if;
            New_Config.Strategy_Counts (Push_Back) := New_Config.Strategy_Counts (Push_Back) + 1;

            Local_Config_Heap.Add (New_Config);

            if Trace_McKenzie > Detail then
               Base.Put ("push_back " & Image (Token.ID, Trace.Descriptor.all), Super, Shared,
                         Parser_Index, New_Config);
            end if;
         end;
      end if;
   end Try_Push_Back;

   function Just_Pushed_Back_Or_Deleted (Config : in Configuration; ID : in Token_ID) return Boolean
   is
      use Config_Op_Arrays, Config_Op_Array_Refs;
      Last_Token_Index : WisiToken.Token_Index := Config.Current_Shared_Token;
      --  Index of token in last op checked.
   begin
      --  This function is called when considering whether to insert ID before
      --  Config.Current_Shared_Token.
      --
      --  We need to consider more than one recent op here; see test_mckenzie_recover.adb
      --  Check_Multiple_Delete_For_Insert. Checking only one op allows this solution there:
      --
      --  ...  (DELETE, END, 7), (DELETE, SEMICOLON, 8), (INSERT, END, 9), (INSERT, SEMICOLON, 9)
      --
      for I in reverse First_Index (Config.Ops) .. Last_Index (Config.Ops) loop
         declare
            Op : Config_Op renames Constant_Ref (Config.Ops, I);
         begin
            case Op.Op is
            when Push_Back =>
               --  The case we are preventing for Push_Back is typically one of:
               --  (PUSH_BACK, Identifier, 2), (INSERT, Identifier, 2)
               --  (PUSH_BACK, Identifier, 2), (PUSH_BACK, END, 3), (INSERT, Identifier, 3), (INSERT, END, 3),
               if Op.PB_Token_Index = Last_Token_Index then
                  if Op.PB_ID = ID then
                     return True;
                  else
                     if Op.PB_Token_Index = WisiToken.Token_Index'First then
                        return False;
                     else
                        Last_Token_Index := Op.PB_Token_Index - 1;
                     end if;
                  end if;
               else
                  --  Op is at a different edit point.
                  return False;
               end if;

            when Delete =>
               if Op.Del_Token_Index = Last_Token_Index - 1 then
                  if Op.Del_ID = ID then
                     return True;
                  else
                     Last_Token_Index := Op.Del_Token_Index;
                  end if;
               else
                  --  Op is at a different edit point.
                  return False;
               end if;

            when Fast_Forward | Insert | Undo_Reduce =>
               return False;
            end case;
         end;
      end loop;
      return False;
   end Just_Pushed_Back_Or_Deleted;

   procedure Try_Undo_Reduce
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use Config_Op_Arrays;

      Trace          : WisiToken.Trace'Class renames Super.Trace.all;
      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;
      Token          : constant Recover_Token := Config.Stack.Peek.Token;
      New_Config     : Configuration          := Config;
      Token_Count    : Ada.Containers.Count_Type;
   begin
      --  Try expanding the nonterm on the stack top, to allow pushing_back
      --  its components, or insert and other operations at that point.

      New_Config.Error_Token.ID := Invalid_Token_ID;
      New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

      Token_Count := Undo_Reduce (New_Config.Stack, Super.Parser_State (Parser_Index).Tree);

      if Token.Min_Terminal_Index /= Invalid_Token_Index  then
         --  If Token is empty no cost increase.
         New_Config.Cost := New_Config.Cost + McKenzie_Param.Undo_Reduce (Token.ID);
      end if;

      if Is_Full (New_Config.Ops) then
         Super.Config_Full ("undo_reduce 1", Parser_Index);
         raise Bad_Config;
      else
         Append (New_Config.Ops, (Undo_Reduce, Token.ID, Token_Count));
      end if;
      New_Config.Strategy_Counts (Undo_Reduce) := New_Config.Strategy_Counts (Undo_Reduce) + 1;

      Local_Config_Heap.Add (New_Config);

      if Trace_McKenzie > Detail then
         Base.Put ("undo_reduce " & Image (Token.ID, Trace.Descriptor.all), Super, Shared,
                   Parser_Index, New_Config);
      end if;
   end Try_Undo_Reduce;

   procedure Insert_From_Action_List
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Minimal_Insert    : in              Token_ID_Arrays.Vector;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      Table      : Parse_Table renames Shared.Table.all;
      EOF_ID     : Token_ID renames Super.Trace.Descriptor.EOI_ID;
      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;

      --  Find terminal insertions from the current state's action_list to try.
      --
      --  We perform any needed reductions and one shift, so the config is
      --  in a consistent state, and enqueue the result. If there are any
      --  conflicts or semantic check fails encountered, they create other
      --  configs to enqueue.

      Current_Token : constant Token_ID := Current_Token_ID_Peek
        (Shared.Terminals.all, Config.Current_Shared_Token, Config.Insert_Delete, Config.Current_Insert_Delete);

      Cached_Config : Configuration;
      Cached_Action : Reduce_Action_Rec;
      --  Most of the time, all the reductions in a state are the same. So
      --  we cache the first result. This includes one reduction; if an
      --  associated semantic check failed, this does not include the fixes.

      I : Parse_Action_Node_Ptr;
   begin
      for Node of Table.States (Config.Stack.Peek.State).Action_List loop
         I := Node.Actions;
         loop
            exit when I = null;
            declare
               ID     : constant Token_ID := Node.Symbol;
               Action : Parse_Action_Rec renames I.Item;
            begin
               if ID /= EOF_ID and then -- can't insert eof
                 ID /= Invalid_Token_ID -- invalid when Verb = Error
               then
                  if Just_Pushed_Back_Or_Deleted (Config, ID) then
                     if Trace_McKenzie > Extra then
                        Put_Line
                          (Super.Trace.all, Super.Label (Parser_Index), "Insert: abandon " & Image (ID, Descriptor) &
                             ": undo push_back");
                     end if;
                  elsif ID = Current_Token then
                     --  This is needed because we allow explore when the error is not at
                     --  the explore point; it prevents inserting useless tokens (ie
                     --  'identifier ;' in ada_lite).
                     if Trace_McKenzie > Extra then
                        Put_Line
                          (Super.Trace.all, Super.Label (Parser_Index), "Insert: abandon " & Image (ID, Descriptor) &
                             ": current token");
                     end if;

                  elsif (for some Minimal of Minimal_Insert => ID = Minimal) then
                     --  Was inserted by Insert_Minimal_Complete_Actions
                     null;

                  else
                     case Action.Verb is
                     when Shift =>
                        declare
                           New_Config : Configuration := Config;
                        begin
                           Do_Shift
                             ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action.State, ID,
                              Cost_Delta => 0,
                              Strategy   => Insert);
                        end;

                     when Reduce =>
                        if not Equal (Action, Cached_Action) then
                           declare
                              New_Config : Configuration := Config;
                           begin
                              New_Config.Error_Token.ID := Invalid_Token_ID;
                              New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

                              Do_Reduce_1
                                ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, Action);
                              Cached_Config := New_Config;
                              Cached_Action := Action;

                              Do_Reduce_2
                                ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID,
                                 Cost_Delta => 0,
                                 Strategy   => Insert);
                           end;

                        else
                           declare
                              New_Config : Configuration := Cached_Config;
                           begin
                              Do_Reduce_2
                                ("Insert", Super, Shared, Parser_Index, Local_Config_Heap, New_Config, ID,
                                 Cost_Delta => 0,
                                 Strategy   => Insert);
                           end;
                        end if;

                     when Accept_It =>
                        raise SAL.Programmer_Error with "found test case for Process_One Accept_It";

                     when Error =>
                        null;
                     end case;
                  end if;
               end if;
            end;
            I := I.Next;
         end loop;
      end loop;
   end Insert_From_Action_List;

   function Insert_Minimal_Complete_Actions
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Orig_Config       : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
     return Token_ID_Arrays.Vector
      --  Return tokens inserted (empty if none).
   is
      use Ada.Containers;

      Table         : Parse_Table renames Shared.Table.all;
      Descriptor    : WisiToken.Descriptor renames Super.Trace.Descriptor.all;
      Inserted      : Token_ID_Array (1 .. 10) := (others => Invalid_Token_ID);
      Inserted_Last : Integer                  := Inserted'First - 1;

      type Work_Item is record
         Action     : Minimal_Action;
         Cost_Delta : Integer;
         Config     : Configuration;
      end record;

      package Item_Queues is new SAL.Gen_Bounded_Definite_Queues (Work_Item);
      use Item_Queues;

      Work : Queue_Type (10);
      --  The required queue size depends on the number of multiple-item
      --  Minimal_Complete_Actions encountered. That is limited by compound
      --  statement nesting, and by the frequency of such actions.

      procedure Safe_Add_Work (Label : in String; Item : in Work_Item)
      is begin
         if Is_Full (Work) then
            Super.Config_Full ("Minimal_Complete_Actions " & Label, Parser_Index);
            raise Bad_Config;
         else
            Add (Work, Item);
         end if;
      end Safe_Add_Work;

      function To_Reduce_Action (Action : in Minimal_Action) return Reduce_Action_Rec
        is (Reduce, Action.Production, null, null, Action.Token_Count);

      procedure Minimal_Do_Shift
        (Action     : in     Minimal_Action;
         Cost_Delta : in     Integer;
         Config     : in out Configuration)
      is begin
         if Just_Pushed_Back_Or_Deleted (Config, Action.ID) then
            if Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Super.Label (Parser_Index),
                  "Minimal_Complete_Actions: abandon " & Image (Action.ID, Descriptor) & ": undo push back");
            end if;
         else
            Config.Check_Status           := (Label => WisiToken.Semantic_Checks.Ok);
            Config.Minimal_Complete_State := Active;
            Inserted_Last                 := Inserted_Last + 1;
            if Inserted_Last <= Inserted'Last then
               Inserted (Inserted_Last)      := Action.ID;
            else
               Super.Config_Full ("minimal_do_shift Inserted", Parser_Index);
               raise Bad_Config;
            end if;

            Do_Shift
              ("Minimal_Complete_Actions", Super, Shared, Parser_Index, Local_Config_Heap, Config,
               Action.State, Action.ID, Cost_Delta,
               Strategy => Minimal_Complete);
         end if;
      end Minimal_Do_Shift;

      procedure Enqueue_Min_Actions
        (Label   : in String;
         Actions : in Minimal_Action_Arrays.Vector;
         Config  : in Configuration)
      is
         use SAL;
         Length : array (Actions.First_Index .. Actions.Last_Index) of Count_Type := (others => Count_Type'Last);

         Min_Length : Count_Type := Count_Type'Last;
      begin
         if Trace_McKenzie > Extra then
            Put_Line
              (Super.Trace.all, Super.Label (Parser_Index), "Minimal_Complete_Actions: " & Label &
                 Image (Actions, Descriptor));
         end if;

         if Actions.Length = 0 then
            return;
         elsif Actions.Length = 1 then
            Safe_Add_Work
              ("1", (Actions (Actions.First_Index), Table.McKenzie_Param.Minimal_Complete_Cost_Delta, Config));
            return;
         end if;

         --  More than one minimal action in State; try to use next states and
         --  recursion to pick one.
         Actions_Loop :
         for I in Actions.First_Index .. Actions.Last_Index loop
            declare
               function Matches (Item : in Kernel_Info; Action : in Minimal_Action) return Boolean
               is begin
                  case Action.Verb is
                  when Shift =>
                     return Item.Before_Dot = Action.ID;
                  when Reduce =>
                     return Item.Before_Dot = Action.Production.LHS;
                  end case;
               end Matches;

               function Length_After_Dot
                 (Item   : in Kernel_Info;
                  Action : in Minimal_Action;
                  Stack  : in Recover_Stacks.Stack)
                 return Ada.Containers.Count_Type
               is
                  Match_ID   : Token_ID;
                  New_Stack  : Recover_Stacks.Stack      := Stack;
                  Next_State : Unknown_State_Index;
                  Result     : Ada.Containers.Count_Type;
                  Min_Result : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
               begin
                  case Action.Verb is
                  when Shift =>
                     New_Stack.Push
                       ((Action.State, Invalid_Node_Index, (ID => Action.ID, others => <>)));
                     Next_State := Action.State;
                     Match_ID   := Action.ID;

                  when Reduce =>
                     New_Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
                     Next_State := Goto_For (Shared.Table.all, New_Stack.Peek.State, Action.Production.LHS);
                     if Next_State = Unknown_State then
                        --  We get here when Insert_From_Action_Table started us down a bad path
                        raise Bad_Config;
                     end if;

                     New_Stack.Push
                       ((Next_State, Invalid_Node_Index, (ID => Action.Production.LHS, others => <>)));
                     Match_ID   := Action.Production.LHS;
                  end case;

                  if Trace_McKenzie > Extra then
                     Super.Trace.Put (Next_State'Image & " " & Trimmed_Image (Item.Production));
                  end if;

                  for Item of Shared.Table.States (Next_State).Kernel loop
                     if Item.Before_Dot = Match_ID then
                        if Item.Length_After_Dot = 0 then
                           Result := Length_After_Dot
                             (Item, (Reduce, Item.Reduce_Production, Item.Reduce_Count), New_Stack);
                        else
                           Result := Item.Length_After_Dot;
                        end if;
                     end if;

                     if Result < Min_Result then
                        Min_Result := Result;
                     end if;
                  end loop;
                  return Min_Result;
               end Length_After_Dot;

               Action     : constant Minimal_Action := Actions (I);
               Next_State : constant State_Index    :=
                 (case Action.Verb is
                  when Shift  => Action.State,
                  when Reduce => Goto_For
                    (Shared.Table.all,
                     Config.Stack.Peek (Base_Peek_Type (Action.Token_Count) + 1).State,
                     Action.Production.LHS));
            begin
               if Trace_McKenzie > Extra then
                  Super.Trace.Put
                    ("task" & Task_Attributes.Value'Image &
                       Super.Label (Parser_Index)'Image & ": Minimal_Complete_Actions: " &
                       Image (Action, Descriptor));
               end if;

               for Item of Shared.Table.States (Next_State).Kernel loop

                  if Matches (Item, Action) then
                     --  For Action.Verb = Reduce, more than one item may match
                     if Item.Length_After_Dot = 0 then
                        --  Set Length from a non-zero-length non-recursive item.
                        Length (I) := Length_After_Dot (Item, Action, Config.Stack);

                     elsif Item.Length_After_Dot < Length (I) then
                        if Trace_McKenzie > Extra then
                           --  Length_After_Dot outputs this in other branch
                           Super.Trace.Put (Next_State'Image & " " & Trimmed_Image (Item.Production));
                        end if;
                        Length (I) := Item.Length_After_Dot;

                     end if;

                     if Trace_McKenzie > Extra then
                        Super.Trace.Put (" length" & Length (I)'Image);
                     end if;
                     if Length (I) < Min_Length then
                        Min_Length := Length (I);
                     end if;
                  end if;
               end loop;
               if Trace_McKenzie > Extra then
                  Super.Trace.New_Line;
               end if;
            end;
         end loop Actions_Loop;

         for I in Length'Range loop
            if Length (I) = Min_Length then
               Safe_Add_Work ("2", (Actions (I), Table.McKenzie_Param.Minimal_Complete_Cost_Delta, Config));

            elsif Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Super.Label (Parser_Index), "Minimal_Complete_Actions: drop " &
                    Image (Actions (I), Descriptor) & " not minimal or recursive");
            end if;
         end loop;
      end Enqueue_Min_Actions;

   begin
      if Orig_Config.Stack.Depth = 1 then
         --  Get here with an empty source file, or a syntax error on the first
         --  token.
         return Token_ID_Arrays.Empty_Vector;

      elsif Orig_Config.Minimal_Complete_State = Done then
         if Trace_McKenzie > Extra then
            Put_Line
              (Super.Trace.all, Super.Label (Parser_Index), "Minimal_Complete_Actions: done");
         end if;
         return Token_ID_Arrays.Empty_Vector;
      end if;

      Enqueue_Min_Actions ("", Table.States (Orig_Config.Stack.Peek.State).Minimal_Complete_Actions, Orig_Config);

      loop
         exit when Is_Empty (Work);

         declare
            Item : Work_Item := Get (Work);
         begin
            if Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Super.Label (Parser_Index), "Minimal_Complete_Actions: dequeue work item " &
                    Image (Item.Action, Descriptor));
            end if;

            case Item.Action.Verb is
            when Reduce =>
               --  Do a reduce, look at resulting state. Keep reducing until we can't
               --  anymore.
               declare
                  Reduce_Action : Reduce_Action_Rec := To_Reduce_Action (Item.Action);
                  Actions       : Minimal_Action_Arrays.Vector;
               begin
                  loop
                     Do_Reduce_1
                       ("Minimal_Complete_Actions", Super, Shared, Parser_Index, Local_Config_Heap, Item.Config,
                        Reduce_Action,
                        Do_Language_Fixes => False);

                     Actions := Table.States (Item.Config.Stack.Peek.State).Minimal_Complete_Actions;

                     case Actions.Length is
                     when 0 =>
                        if Trace_McKenzie > Extra then
                           Put_Line
                             (Super.Trace.all, Super.Label (Parser_Index),
                              "Minimal_Complete_Actions abandoned: no actions");
                        end if;
                        exit;
                     when 1 =>
                        case Actions (Actions.First_Index).Verb is
                        when Shift =>
                           Minimal_Do_Shift (Actions (Actions.First_Index), Item.Cost_Delta, Item.Config);
                           exit;
                        when Reduce =>
                           Reduce_Action := To_Reduce_Action (Actions (Actions.First_Index));
                        end case;

                     when others =>
                        Enqueue_Min_Actions ("multiple actions ", Actions, Item.Config);
                        exit;
                     end case;
                  end loop;
               end;

            when Shift =>
               Minimal_Do_Shift (Item.Action, Item.Cost_Delta, Item.Config);
            end case;
         end;
      end loop;

      if Inserted_Last = Inserted'First - 1 then
         --  Nothing inserted this round.
         if Orig_Config.Minimal_Complete_State = Active then
            Orig_Config.Minimal_Complete_State := Done;
         end if;
      end if;

      return To_Vector (Inserted (1 .. Inserted_Last));
   exception
   when Bad_Config =>
      return Token_ID_Arrays.Empty_Vector;
   end Insert_Minimal_Complete_Actions;

   procedure Insert_Matching_Begin
     (Super                 : not null access Base.Supervisor;
      Shared                : not null access Base.Shared;
      Parser_Index          : in              SAL.Base_Peek_Type;
      Config                : in              Configuration;
      Local_Config_Heap     : in out          Config_Heaps.Heap_Type;
      Matching_Begin_Tokens : in              Token_ID_Arrays.Vector)
   is
      Table      : Parse_Table renames Shared.Table.all;
      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;
   begin
      --  We don't check for insert = current token; that's either ok or a
      --  severe bug in Shared.Language_Matching_Begin_Tokens.

      if Config.Matching_Begin_Done then
         if Trace_McKenzie > Extra then
            Put_Line (Super.Trace.all, Super.Label (Parser_Index), "Matching_Begin abandoned: done");
         end if;
         return;
      end if;

      if Just_Pushed_Back_Or_Deleted (Config, Matching_Begin_Tokens (Matching_Begin_Tokens.First_Index)) then
         if Trace_McKenzie > Extra then
            Put_Line
              (Super.Trace.all, Super.Label (Parser_Index), "Matching_Begin abandoned " &
                 Image (Matching_Begin_Tokens (Matching_Begin_Tokens.First_Index), Descriptor) & ": undo push_back");
         end if;
         return;
      end if;

      declare
         New_Config  : Configuration := Config;
      begin
         for ID of Matching_Begin_Tokens loop
            Insert (New_Config, ID);
         end loop;

         declare
            use Parse.Parse_Item_Arrays;
            Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;
            Dummy : constant Boolean :=  Parse.Parse
              (Super, Shared, Parser_Index, Parse_Items, New_Config,
               Shared_Token_Goal => Invalid_Token_Index,
               All_Conflicts     => True,
               Trace_Prefix      => "parse Matching_Begin");
         begin
            for I in First_Index (Parse_Items) .. Last_Index (Parse_Items) loop
               declare
                  Item : Parse.Parse_Item renames Parse.Parse_Item_Array_Refs.Variable_Ref (Parse_Items, I);
               begin
                  if Item.Parsed and Item.Config.Current_Insert_Delete = No_Insert_Delete then
                     Item.Config.Matching_Begin_Done := True;
                     Item.Config.Cost := Item.Config.Cost + Table.McKenzie_Param.Matching_Begin;
                     Item.Config.Strategy_Counts (Matching_Begin) := Item.Config.Strategy_Counts (Matching_Begin) + 1;
                     Item.Config.Error_Token.ID := Invalid_Token_ID;
                     Item.Config.Check_Status := (Label => WisiToken.Semantic_Checks.Ok);

                     if Trace_McKenzie > Detail then
                        Base.Put
                          ("Matching_Begin: insert " & Image (Matching_Begin_Tokens, Descriptor),
                           Super, Shared, Parser_Index, Item.Config);
                     end if;
                     Local_Config_Heap.Add (Item.Config);
                  else
                     if Trace_McKenzie > Detail then
                        Base.Put
                          ("Matching_Begin: abandon " & Image (Matching_Begin_Tokens, Descriptor) & ": parse fail",
                           Super, Shared, Parser_Index, Item.Config);
                     end if;
                  end if;
               end;
            end loop;
         end;
      end;
   exception
   when SAL.Container_Full =>
      --  From config_ops_sorted
      Super.Config_Full ("Minimal_Complete_Actions 3", Parser_Index);
   end Insert_Matching_Begin;

   procedure Try_Insert_Terminal
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use all type WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Tokens                : Token_ID_Array_1_3;
      Matching_Begin_Tokens : Token_ID_Arrays.Vector;
      Forbid_Minimal_Insert : Boolean := False;

      Minimal_Inserted : Token_ID_Arrays.Vector;
   begin
      if Shared.Language_Matching_Begin_Tokens /= null then
         Current_Token_ID_Peek_3
           (Shared.Terminals.all, Config.Current_Shared_Token, Config.Insert_Delete, Config.Current_Insert_Delete,
            Tokens);

         Shared.Language_Matching_Begin_Tokens (Tokens, Config, Matching_Begin_Tokens, Forbid_Minimal_Insert);
      end if;

      if not Forbid_Minimal_Insert then
         --  See test_mckenzie_recover.adb Forbid_Minimal_Insert for rationale.
         Minimal_Inserted := Insert_Minimal_Complete_Actions
           (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      if Matching_Begin_Tokens.Length > 0 then
         Insert_Matching_Begin (Super, Shared, Parser_Index, Config, Local_Config_Heap, Matching_Begin_Tokens);
      end if;

      --  We always do all three; Insert_Minimal_Complete (unless
      --  Forbid_Minimal_Insert), Insert_Matching_Begin,
      --  Insert_From_Action_List; in general it's not possible to tell when
      --  one will be better (see test_mckenzie_recover.adb
      --  Always_Minimal_Complete, Always_Matching_Begin).
      --  Insert_From_Action_List does not insert the Minimal_Inserted tokens,
      --  and it will never insert the Matching_Begin_Tokens, so there is no
      --  duplication. Insert_From_Action_List will normally be more
      --  expensive.
      Insert_From_Action_List (Super, Shared, Parser_Index, Config, Minimal_Inserted, Local_Config_Heap);

      --  It is tempting to use the Goto_List to find nonterms to insert.
      --  But that can easily lead to error states, and it turns out to be
      --  not useful, especially if the grammar has been relaxed so most
      --  expressions and lists can be empty.

   exception
   when Bad_Config =>
      null;
   end Try_Insert_Terminal;

   procedure Try_Insert_Quote
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in out          Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      use Config_Op_Arrays;
      use all type Parser.Language_String_ID_Set_Access;

      Descriptor  : WisiToken.Descriptor renames Shared.Trace.Descriptor.all;
      Check_Limit : WisiToken.Token_Index renames Shared.Table.McKenzie_Param.Check_Limit;

      Current_Line            : constant Line_Number_Type := Shared.Terminals.all (Config.Current_Shared_Token).Line;
      Lexer_Error_Token       : Base_Token;

      function Recovered_Lexer_Error (Line : in Line_Number_Type) return Base_Token_Index
      is begin
         --  We are assuming the list of lexer errors is short, so binary
         --  search would not be significantly faster.
         for Err of reverse Shared.Lexer.Errors loop
            if Err.Recover_Token /= Invalid_Token_Index and then
              Shared.Terminals.all (Err.Recover_Token).Line = Line
            then
               return Err.Recover_Token;
            end if;
         end loop;
         return Invalid_Token_Index;
      end Recovered_Lexer_Error;

      Lexer_Error_Token_Index : constant Base_Token_Index := Recovered_Lexer_Error (Current_Line);

      function String_ID_Set (String_ID : in Token_ID) return Token_ID_Set
      is begin
         if Shared.Language_String_ID_Set = null then
            return (String_ID .. String_ID => True);
         else
            return Shared.Language_String_ID_Set (Descriptor, String_ID);
         end if;
      end String_ID_Set;

      procedure String_Literal_In_Stack
        (Label             : in     String;
         New_Config        : in out Configuration;
         Matching          : in     SAL.Peek_Type;
         String_Literal_ID : in     Token_ID)
      is
         Saved_Shared_Token : constant WisiToken.Token_Index := New_Config.Current_Shared_Token;

         Tok         : Recover_Token;
         J           : WisiToken.Token_Index;
         Parse_Items : aliased Parse.Parse_Item_Arrays.Vector;
      begin
         --  Matching is the index of a token on New_Config.Stack containing a string
         --  literal. Push back thru that token, then delete all tokens after
         --  the string literal to Saved_Shared_Token.
         if not Has_Space (New_Config.Ops, Ada.Containers.Count_Type (Matching)) then
            Super.Config_Full ("insert quote 1 " & Label, Parser_Index);
            raise Bad_Config;
         end if;
         for I in 1 .. Matching loop
            if Push_Back_Valid (New_Config) then
               Tok := New_Config.Stack.Pop.Token;
               Append (New_Config.Ops, (Push_Back, Tok.ID, Tok.Min_Terminal_Index));
            else
               --  Probably pushing back thru a previously inserted token
               raise Bad_Config;
            end if;
         end loop;

         New_Config.Current_Shared_Token := Tok.Min_Terminal_Index;

         --  Find last string literal in pushed back terminals.
         J := Saved_Shared_Token - 1;
         loop
            exit when Shared.Terminals.all (J).ID = String_Literal_ID;
            J := J - 1;
         end loop;

         begin
            if Parse.Parse
              (Super, Shared, Parser_Index, Parse_Items, New_Config,
               Shared_Token_Goal => J,
               All_Conflicts     => False,
               Trace_Prefix      => "insert quote parse pushback " & Label)
            then
               --  The non-deleted tokens parsed without error. We don't care if any
               --  conflicts were encountered; we are not using the parse result.
               New_Config := Parse.Parse_Item_Array_Refs.Constant_Ref (Parse_Items, 1).Config;
               Append (New_Config.Ops, (Fast_Forward, New_Config.Current_Shared_Token));
            else
               raise SAL.Programmer_Error;
            end if;
         exception
         when Bad_Config =>
            raise SAL.Programmer_Error;
         end;

         if New_Config.Current_Shared_Token < Saved_Shared_Token - 1 and then
           (not Has_Space
              (New_Config.Ops, Ada.Containers.Count_Type (Saved_Shared_Token - 1 - New_Config.Current_Shared_Token)))
         then
            Super.Config_Full ("insert quote 2 " & Label, Parser_Index);
            raise Bad_Config;
         end if;

         for J in New_Config.Current_Shared_Token .. Saved_Shared_Token - 1 loop
            Append (New_Config.Ops, (Delete, Shared.Terminals.all (J).ID, J));
         end loop;

         New_Config.Current_Shared_Token := Saved_Shared_Token;

      end String_Literal_In_Stack;

      procedure Push_Back_Tokens
        (Full_Label            : in     String;
         New_Config            : in out Configuration;
         Min_Pushed_Back_Index :    out Base_Token_Index)
      is
         Item : Recover_Stack_Item;
      begin
         loop
            Item := New_Config.Stack.Peek;
            if Item.Token.Virtual then
               --  Don't push back an inserted token
               exit;

            elsif Item.Token.Byte_Region = Null_Buffer_Region then
               --  Don't need push_back for an empty token
               New_Config.Stack.Pop;

            elsif Item.Tree_Index = Invalid_Node_Index then
               --  Item was pushed on stack during recovery, and we do not know
               --  its Line. To avoid crossing a line boundary, we stop push_backs
               --  here.
               exit;

            else
               if Shared.Terminals.all
                 (Super.Parser_State (Parser_Index).Tree.First_Shared_Terminal (Item.Tree_Index)).Line = Current_Line
                 --  Don't let push_back cross a line boundary.
               then
                  if Is_Full (New_Config.Ops) then
                     Super.Config_Full (Full_Label, Parser_Index);
                     raise Bad_Config;
                  else
                     New_Config.Stack.Pop;
                     Append (New_Config.Ops, (Push_Back, Item.Token.ID, Item.Token.Min_Terminal_Index));
                  end if;
               end if;

               exit;
            end if;
         end loop;
         Min_Pushed_Back_Index := Item.Token.Min_Terminal_Index;
      end Push_Back_Tokens;

      procedure Finish
        (Label       : in     String;
         New_Config  : in out Configuration;
         First, Last : in     Base_Token_Index)
      is
         Adj_First : constant Base_Token_Index := (if First = Invalid_Token_Index then Last else First);
         Adj_Last  : constant Base_Token_Index := (if Last = Invalid_Token_Index then First else Last);
      begin
         --  Delete tokens First .. Last; either First - 1 or Last + 1 should
         --  be a String_Literal. Leave Current_Shared_Token at Last + 1.

         if Adj_Last = Invalid_Token_Index or Adj_First = Invalid_Token_Index then
            pragma Assert (False);
            raise Bad_Config;
         end if;

         New_Config.Error_Token.ID := Invalid_Token_ID;
         New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

         --  This is a guess, so we give it a nominal cost
         New_Config.Cost := New_Config.Cost + 1;

         if not Has_Space (New_Config.Ops, Ada.Containers.Count_Type (Last - First)) then
            Super.Config_Full ("insert quote 3 " & Label, Parser_Index);
            raise Bad_Config;
         end if;

         for I in Adj_First .. Adj_Last loop
            Append (New_Config.Ops, (Delete, Shared.Terminals.all (I).ID, I));
         end loop;
         New_Config.Current_Shared_Token := Last + 1;

         --  Let explore do insert after these deletes.
         Append (New_Config.Ops, (Fast_Forward, New_Config.Current_Shared_Token));

         if New_Config.Resume_Token_Goal - Check_Limit < New_Config.Current_Shared_Token then
            New_Config.Resume_Token_Goal := New_Config.Current_Shared_Token + Check_Limit;
            if Trace_McKenzie > Extra then
               Put_Line
                 (Super.Trace.all, Super.Label (Parser_Index), "resume_token_goal:" & WisiToken.Token_Index'Image
                    (New_Config.Resume_Token_Goal));
            end if;
         end if;

         New_Config.Strategy_Counts (String_Quote) := New_Config.Strategy_Counts (String_Quote) + 1;

         if Trace_McKenzie > Detail then
            Base.Put ("insert quote " & Label & " ", Super, Shared, Parser_Index, New_Config);
         end if;
      end Finish;

   begin
      --  When the lexer finds an unbalanced quote, it inserts a virtual
      --  balancing quote at the same character position as the unbalanced
      --  quote, returning an empty string literal token there. The parser
      --  does not see that as an error; it encounters a syntax error
      --  before, at, or after that string literal.
      --
      --  Here we assume the parse error in Config.Error_Token is due to
      --  putting the balancing quote in the wrong place (although we do
      --  check that; see test_mckenzie_recover.adb String_Quote_6), and
      --  attempt to find a better place to put the balancing quote. Then
      --  all tokens from the balancing quote to the unbalanced quote are
      --  now part of a string literal, so delete them, leaving just the
      --  string literal created by Lexer error recovery.

      --  First we check to see if there is an unbalanced quote in the
      --  current line; if not, just return. Some lexer errors are for other
      --  unrecognized characters; see ada_mode-recover_bad_char.adb.
      --
      --  An alternate strategy is to treat the lexer error as a parse error
      --  immediately, but that complicates the parse logic.

      Config.String_Quote_Checked := Current_Line;

      if Lexer_Error_Token_Index = Invalid_Token_Index then
         return;
      end if;

      Lexer_Error_Token := Shared.Terminals.all (Lexer_Error_Token_Index);

      --  It is not possible to tell where the best place to put the
      --  balancing quote is, so we always try all reasonable places.

      if Lexer_Error_Token.Byte_Region.First = Config.Error_Token.Byte_Region.First then
         --  The parse error token is the string literal at the lexer error.
         --
         --  case a: Insert the balancing quote somewhere before the error
         --  point. There is no way to tell how far back to put the balancing
         --  quote, so we just do one non-empty token. See
         --  test_mckenzie_recover.adb String_Quote_0. So far we have not found
         --  a test case for more than one token.
         declare
            New_Config            : Configuration := Config;
            Min_Pushed_Back_Index : Base_Token_Index;
         begin
            Push_Back_Tokens ("insert quote 4 a", New_Config, Min_Pushed_Back_Index);
            Finish ("a", New_Config, Min_Pushed_Back_Index, Config.Current_Shared_Token - 1);
            Local_Config_Heap.Add (New_Config);
         end;

         --  Note that it is not reasonable to insert a quote after the error
         --  in this case. If that were the right solution, the parser error
         --  token would not be the lexer repaired string literal, since a
         --  string literal would be legal here.

      elsif Lexer_Error_Token.Byte_Region.First < Config.Error_Token.Byte_Region.First then
         --  The unbalanced quote is before the parse error token; see
         --  test_mckenzie_recover.adb String_Quote_2.
         --
         --  The missing quote belongs after the parse error token, before or
         --  at the end of the current line; try inserting it at the end of
         --  the current line.
         --
         --  The lexer repaired string literal may be in a reduced token on the
         --  stack.

         declare
            Matching : SAL.Peek_Type := 1;
         begin
            Find_Descendant_ID
              (Super.Parser_State (Parser_Index).Tree, Config, Lexer_Error_Token.ID,
               String_ID_Set (Lexer_Error_Token.ID), Matching);

            if Matching = Config.Stack.Depth then
               --  String literal is in a virtual nonterm; give up. So far this only
               --  happens in a high cost non critical config.
               if Trace_McKenzie > Detail then
                  Put_Line
                    (Super.Trace.all, Super.Label (Parser_Index), "insert quote b abandon; string literal in virtual");
               end if;
               return;
            end if;

            declare
               New_Config : Configuration := Config;
            begin
               String_Literal_In_Stack ("b", New_Config, Matching, Lexer_Error_Token.ID);

               Finish
                 ("b", New_Config, Config.Current_Shared_Token, Shared.Line_Begin_Token.all (Current_Line + 1) - 1);
               Local_Config_Heap.Add (New_Config);
            end;
         end;

      else
         --  The unbalanced quote is after the parse error token.

         --  case c: Assume a missing quote belongs immediately before the current token.
         --  See test_mckenzie_recover.adb String_Quote_3.
         declare
            New_Config : Configuration := Config;
         begin
            Finish ("c", New_Config, Config.Current_Shared_Token, Lexer_Error_Token_Index - 1);
            Local_Config_Heap.Add (New_Config);
         exception
         when Bad_Config =>
            null;
         end;

         --  case d: Assume a missing quote belongs somewhere farther before
         --  the current token; try one non-empty (as in case a above). See
         --  test_mckenzie_recover.adb String_Quote_4, String_Quote_6.
         declare
            New_Config            : Configuration := Config;
            Min_Pushed_Back_Index : Base_Token_Index;
         begin
            Push_Back_Tokens ("insert quote 5 d", New_Config, Min_Pushed_Back_Index);
            Finish ("d", New_Config, Min_Pushed_Back_Index, Lexer_Error_Token_Index - 1);
            Local_Config_Heap.Add (New_Config);
         exception
         when SAL.Container_Empty =>
            --  From Stack.Pop
            null;
         when Bad_Config =>
            null;
         end;

         --  case e: Assume the actual error is an extra quote that terminates
         --  an intended string literal early, in which case there is a token
         --  on the stack containing the string literal that should be extended
         --  to the found quote. See test_mckenzie_recover.adb String_Quote_1.
         declare
            Matching : SAL.Peek_Type := 1;
         begin
            --  Lexer_Error_Token is a string literal; find a matching one.
            Find_Descendant_ID
              (Super.Parser_State (Parser_Index).Tree, Config, Lexer_Error_Token.ID, String_ID_Set
                 (Lexer_Error_Token.ID), Matching);

            if Matching = Config.Stack.Depth then
               --  No matching string literal, so this case does not apply.
               null;
            else
               declare
                  New_Config : Configuration := Config;
               begin
                  String_Literal_In_Stack ("e", New_Config, Matching, Lexer_Error_Token.ID);

                  Finish ("e", New_Config, Config.Current_Shared_Token, Lexer_Error_Token_Index);
                  Local_Config_Heap.Add (New_Config);
               end;
            end if;
         end;
      end if;
   exception
   when Bad_Config =>
      null;
   end Try_Insert_Quote;

   procedure Try_Delete_Input
     (Super             : not null access Base.Supervisor;
      Shared            : not null access Base.Shared;
      Parser_Index      : in              SAL.Base_Peek_Type;
      Config            : in              Configuration;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type)
   is
      --  Try deleting (= skipping) the current shared input token.

      use Config_Op_Arrays, Config_Op_Array_Refs;
      Trace       : WisiToken.Trace'Class renames Super.Trace.all;
      EOF_ID      : Token_ID renames Trace.Descriptor.EOI_ID;
      Check_Limit : WisiToken.Token_Index renames Shared.Table.McKenzie_Param.Check_Limit;

      McKenzie_Param : McKenzie_Param_Type renames Shared.Table.McKenzie_Param;

      ID : constant Token_ID := Shared.Terminals.all (Config.Current_Shared_Token).ID;
   begin
      if ID /= EOF_ID and then
         --  can't delete EOF
         (Length (Config.Ops) = 0 or else
           --  Don't delete an ID we just inserted; waste of time
           (not Equal (Constant_Ref (Config.Ops, Last_Index (Config.Ops)),
                       (Insert, ID, Config.Current_Shared_Token))))
      then
         declare
            New_Config : Configuration := Config;

            function Matching_Push_Back return Boolean
            is begin
               for I in reverse First_Index (New_Config.Ops) .. Last_Index (New_Config.Ops) loop
                  declare
                     Op : Config_Op renames Config_Op_Array_Refs.Variable_Ref (New_Config.Ops, I).Element.all;
                  begin
                     exit when not (Op.Op in Undo_Reduce | Push_Back | Delete);
                     if Op = (Push_Back, ID, New_Config.Current_Shared_Token) then
                        return True;
                     end if;
                  end;
               end loop;
               return False;
            end Matching_Push_Back;
         begin
            New_Config.Error_Token.ID := Invalid_Token_ID;
            New_Config.Check_Status   := (Label => WisiToken.Semantic_Checks.Ok);

            New_Config.Cost := New_Config.Cost + McKenzie_Param.Delete (ID);
            New_Config.Strategy_Counts (Delete) := Config.Strategy_Counts (Delete) + 1;

            if Matching_Push_Back then
               --  We are deleting a push_back; cancel the push_back cost, to make
               --  this the same as plain deleting.
               New_Config.Cost := Natural'Max (Natural'First, New_Config.Cost - McKenzie_Param.Push_Back (ID));
            end if;

            if Is_Full (New_Config.Ops) then
               Super.Config_Full ("delete", Parser_Index);
               raise Bad_Config;
            else
               Append (New_Config.Ops, (Delete, ID, Config.Current_Shared_Token));
            end if;
            New_Config.Current_Shared_Token := New_Config.Current_Shared_Token + 1;

            if New_Config.Resume_Token_Goal - Check_Limit < New_Config.Current_Shared_Token then
               New_Config.Resume_Token_Goal := New_Config.Current_Shared_Token + Check_Limit;
            end if;

            Local_Config_Heap.Add (New_Config);

            if Trace_McKenzie > Detail then
               Base.Put
                 ("delete " & Image (ID, Trace.Descriptor.all), Super, Shared, Parser_Index, New_Config);
            end if;
         end;
      end if;
   end Try_Delete_Input;

   procedure Process_One
     (Super         : not null access Base.Supervisor;
      Shared        : not null access Base.Shared;
      Config_Status : out             Base.Config_Status)
   is
      --  Get one config from Super, check to see if it is a viable
      --  solution. If not, enqueue variations to check.

      use all type Base.Config_Status;
      use all type Parser.Language_Fixes_Access;
      use all type Semantic_Checks.Check_Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Parser_Index : SAL.Base_Peek_Type;
      Config       : Configuration;

      Local_Config_Heap : Config_Heaps.Heap_Type;
      --  We collect all the variants to enqueue, then deliver them all at
      --  once to Super, to minimizes task interactions.
   begin
      Super.Get (Parser_Index, Config, Config_Status);

      if Config_Status = All_Done then
         return;
      end if;

      if Trace_McKenzie > Detail then
         Base.Put ("dequeue", Super, Shared, Parser_Index, Config);
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Super.Label (Parser_Index), "stack: " & Image (Config.Stack, Trace.Descriptor.all));
         end if;
      end if;

      --  Fast_Forward; parse Insert, Delete in Config.Ops that have not
      --  been parsed yet. 'parse' here means adjusting Config.Stack and
      --  Current_Terminal_Index. Code in this file always parses when
      --  adding ops to Config (except as noted); Language_Fixes should use
      --  McKenzie_Recover.Insert, Delete instead.
      if Config.Current_Insert_Delete = 1 then
         --  Config is from Language_Fixes.

         Fast_Forward (Super, Shared, Parser_Index, Local_Config_Heap, Config);
         Super.Put (Parser_Index, Local_Config_Heap);
         return;
      end if;

      pragma Assert (Config.Current_Insert_Delete = 0);
      --  Config.Current_Insert_Delete > 1 is a programming error.

      if Config.Error_Token.ID /= Invalid_Token_ID then
         if Shared.Language_Fixes = null then
            null;
         else
            Shared.Language_Fixes
              (Trace, Shared.Lexer, Super.Label (Parser_Index), Shared.Table.all,
               Shared.Terminals.all, Super.Parser_State (Parser_Index).Tree, Local_Config_Heap,
               Config);

            --  The solutions enqueued by Language_Fixes should be lower cost than
            --  others (typically 0), so they will be checked first.
         end if;

         if Config.Check_Status.Label = Ok then
            --  Parse table Error action.
            --
            --  We don't clear Config.Error_Token here, because
            --  Language_Use_Minimal_Complete_Actions needs it. We only clear it
            --  when a parse results in no error (or a different error), or a
            --  push_back moves the Current_Token.
            null;

         else
            --  Assume "ignore check error" is a viable solution. But give it a
            --  cost, so a solution provided by Language_Fixes is preferred.

            declare
               New_State : Unknown_State_Index;
            begin
               Config.Cost := Config.Cost + Table.McKenzie_Param.Ignore_Check_Fail;
               Config.Strategy_Counts (Ignore_Error) := Config.Strategy_Counts (Ignore_Error) + 1;

               --  finish reduce.
               Config.Stack.Pop (SAL.Base_Peek_Type (Config.Check_Token_Count));

               New_State := Goto_For (Table, Config.Stack.Peek.State, Config.Error_Token.ID);

               if New_State = Unknown_State then
                  if Config.Stack.Depth = 1 then
                     --  Stack is empty, and we did not get Accept; really bad syntax got
                     --  us here; abandon this config. See ada_mode-recover_bad_char.adb.
                     Super.Put (Parser_Index, Local_Config_Heap);
                     return;
                  else
                     raise SAL.Programmer_Error with
                       "process_one found test case for new_state = Unknown; old state " &
                       Trimmed_Image (Config.Stack.Peek.State) & " nonterm " & Image
                         (Config.Error_Token.ID, Trace.Descriptor.all);
                  end if;
               end if;

               Config.Stack.Push ((New_State, Invalid_Node_Index, Config.Error_Token));

               --  We _don't_ clear Check_Status and Error_Token here; Check needs
               --  them, and sets them as appropriate.
            end;
         end if;
      end if;

      --  Call Check to see if this config succeeds.
      case Check (Super, Shared, Parser_Index, Config, Local_Config_Heap) is
      when Success =>
         Super.Success (Parser_Index, Config, Local_Config_Heap);
         return;

      when Abandon =>
         Super.Put (Parser_Index, Local_Config_Heap);
         return;

      when Continue =>
         null;

      end case;

      if Trace_McKenzie > Detail then
         Base.Put ("continuing", Super, Shared, Parser_Index, Config);
         if Trace_McKenzie > Extra then
            Put_Line (Trace, Super.Label (Parser_Index), "stack: " & Image (Config.Stack, Trace.Descriptor.all));
         end if;
      end if;

      --  Grouping these operations (push_back, delete, insert) ensures that
      --  there are no duplicate solutions found. We reset the grouping
      --  after each fast_forward.
      --
      --  We do delete before insert so Insert_Matching_Begin can operate on
      --  the new next token, before Fast_Forwarding past it.
      --
      --  All possible permutations will be explored.

      pragma Assert (Config.Stack.Depth > 0);

      Try_Insert_Terminal (Super, Shared, Parser_Index, Config, Local_Config_Heap);

      if Push_Back_Valid (Config) and then
        (not Check_Reduce_To_Start (Super, Shared, Parser_Index, Config))
        --  If Config reduces to the start nonterm, there's no point in Push_Back or Undo_Reduce.
      then
         Try_Push_Back (Super, Shared, Parser_Index, Config, Local_Config_Heap);

         if Undo_Reduce_Valid (Config.Stack, Super.Parser_State (Parser_Index).Tree) then
            Try_Undo_Reduce (Super, Shared, Parser_Index, Config, Local_Config_Heap);
         end if;
      end if;

      if None_Since_FF (Config.Ops, Insert) then
         Try_Delete_Input (Super, Shared, Parser_Index, Config, Local_Config_Heap);
      end if;

      --  This is run once per input line, independent of what other ops
      --  have been done.
      if Config.Check_Status.Label = Ok and
        (Descriptor.String_1_ID /= Invalid_Token_ID or Descriptor.String_2_ID /= Invalid_Token_ID) and
        (Config.String_Quote_Checked = Invalid_Line_Number or else
           Config.String_Quote_Checked < Shared.Terminals.all (Config.Current_Shared_Token).Line)
      then
         --  See if there is a mismatched quote. The solution is to delete
         --  tokens, nominally replacing them with an expanded string literal.
         --  So we try this when it is ok to try delete.
         if None_Since_FF (Config.Ops, Insert) then
            Try_Insert_Quote (Super, Shared, Parser_Index, Config, Local_Config_Heap);
         end if;
      end if;

      Super.Put (Parser_Index, Local_Config_Heap);
   exception
   when Bad_Config =>
      --  Just abandon this config; tell Super we are done.
      Super.Put (Parser_Index, Local_Config_Heap);

   when E : others =>
      Super.Put (Parser_Index, Local_Config_Heap);
      if Debug_Mode then
         raise;
      elsif Trace_McKenzie > Outline then
         Put_Line
           (Super.Trace.all, Super.Label (Parser_Index),
            "Process_One: unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
      end if;
   end Process_One;

end WisiToken.Parse.LR.McKenzie_Recover.Explore;
