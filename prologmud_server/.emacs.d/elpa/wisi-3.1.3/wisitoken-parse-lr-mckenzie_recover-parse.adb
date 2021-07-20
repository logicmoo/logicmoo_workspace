--  Abstract :
--
--  See spec
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

package body WisiToken.Parse.LR.McKenzie_Recover.Parse is

   procedure Compute_Nonterm
     (ID              : in     Token_ID;
      Stack           : in     Recover_Stacks.Stack;
      Tokens          : in out Recover_Token_Array;
      Nonterm         :    out Recover_Token;
      Default_Virtual : in     Boolean)
   is
      Min_Terminal_Index_Set : Boolean := False;
   begin
      Nonterm :=
        (ID      => ID,
         Virtual => (if Tokens'Length = 0 then Default_Virtual else False),
         others  => <>);

      for I in Tokens'Range loop
         Tokens (I) := Stack.Peek (Tokens'Last - I + 1).Token;
      end loop;

      for T of Tokens loop
         Nonterm.Virtual := Nonterm.Virtual or T.Virtual;

         if Nonterm.Byte_Region.First > T.Byte_Region.First then
            Nonterm.Byte_Region.First := T.Byte_Region.First;
         end if;

         if Nonterm.Byte_Region.Last < T.Byte_Region.Last then
            Nonterm.Byte_Region.Last := T.Byte_Region.Last;
         end if;

         if not Min_Terminal_Index_Set then
            if T.Min_Terminal_Index /= Invalid_Token_Index then
               Min_Terminal_Index_Set     := True;
               Nonterm.Min_Terminal_Index := T.Min_Terminal_Index;
            end if;
         end if;
      end loop;
   end Compute_Nonterm;

   function Reduce_Stack
     (Shared          : not null access Base.Shared;
      Stack           : in out          Recover_Stacks.Stack;
      Action          : in              Reduce_Action_Rec;
      Nonterm         :    out          Recover_Token;
      Default_Virtual : in              Boolean)
     return Semantic_Checks.Check_Status
   is
      use all type Semantic_Checks.Semantic_Check;
      use all type Semantic_Checks.Check_Status_Label;

      Last   : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type (Action.Token_Count);
      Tokens : Recover_Token_Array (1 .. Last);
   begin
      pragma Assert (Stack.Depth > Last);
      Compute_Nonterm (Action.Production.LHS, Stack, Tokens, Nonterm, Default_Virtual);

      if Action.Check = null then
         --  Now we can pop the stack.
         Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
         return (Label => Ok);
      else
         return Status : constant Semantic_Checks.Check_Status :=
           Action.Check (Shared.Lexer, Nonterm, Tokens, Recover_Active => True)
         do
            if Status.Label = Ok then
               Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
            end if;
         end return;
      end if;
   end Reduce_Stack;

   function Parse_One_Item
     (Super             :         not null access Base.Supervisor;
      Shared            :         not null access Base.Shared;
      Parser_Index      :         in              SAL.Peek_Type;
      Parse_Items       : aliased in out          Parse_Item_Arrays.Vector;
      Parse_Item_Index  :         in              Positive;
      Shared_Token_Goal :         in              Base_Token_Index;
      Trace_Prefix      :         in              String)
     return Boolean
   is
      --  Perform parse actions on Parse_Items (Parse_Item_Index), until one
      --  fails (return False) or Shared_Token_Goal is shifted (return
      --  True).
      --
      --  We return Boolean, not Check_Status, because Abandon and Continue
      --  are up to the caller.
      --
      --  If any actions have conflicts, append the conflict configs and actions to
      --  Parse_Items.

      use Parse_Item_Arrays;
      use Config_Op_Arrays;
      use all type Semantic_Checks.Check_Status_Label;

      Trace      : WisiToken.Trace'Class renames Super.Trace.all;
      Descriptor : WisiToken.Descriptor renames Super.Trace.Descriptor.all;
      Table      : Parse_Table renames Shared.Table.all;

      Item   : Parse_Item renames Parse_Item_Array_Refs.Variable_Ref (Parse_Items, Parse_Item_Index).Element.all;
      Config : Configuration renames Item.Config;
      Action : Parse_Action_Node_Ptr renames Item.Action;

      Conflict : Parse_Action_Node_Ptr;

      Restore_Terminals_Current : Base_Token_Index;
      Current_Token             : Base_Token := McKenzie_Recover.Current_Token
        (Terminals                 => Shared.Terminals.all,
         Terminals_Current         => Config.Current_Shared_Token,
         Restore_Terminals_Current => Restore_Terminals_Current,
         Insert_Delete             => Config.Insert_Delete,
         Current_Insert_Delete     => Config.Current_Insert_Delete);

      New_State : Unknown_State_Index;
      Success   : Boolean := True;

   begin
      if Trace_McKenzie > Detail then
         if Trace_McKenzie > Extra then
            if Config.Current_Insert_Delete /= No_Insert_Delete then
               Put_Line (Trace, Super.Label (Parser_Index), Trace_Prefix & ": Insert_Delete: " &
                           Image (Config.Insert_Delete, Trace.Descriptor.all));
            end if;
         end if;

         Base.Put (Trace_Prefix & ": " & Image (Current_Token, Descriptor), Super, Shared, Parser_Index, Config);
         if Shared_Token_Goal /= Invalid_Token_Index then
            Put_Line (Trace, Super.Label (Parser_Index), Trace_Prefix & ": Shared_Token_Goal :" &
                        WisiToken.Token_Index'Image (Shared_Token_Goal));
         end if;
      end if;

      Item.Parsed := True;

      if Action = null then
         Action := Action_For (Table, Config.Stack.Peek.State, Current_Token.ID);
      end if;

      loop
         Conflict := Action.Next;
         loop
            exit when Conflict = null;
            if Is_Full (Parse_Items) then
               if Trace_McKenzie > Outline then
                  Put_Line (Trace, Super.Label (Parser_Index), Trace_Prefix & ": too many conflicts; abandoning");
               end if;
            else
               declare
                  New_Config : Configuration := Config;
               begin
                  New_Config.Current_Shared_Token := Restore_Terminals_Current;

                  if Trace_McKenzie > Detail then
                     Put_Line
                       (Trace, Super.Label (Parser_Index), Trace_Prefix & ":" & State_Index'Image
                          (New_Config.Stack.Peek.State) & ": add conflict " &
                          Image (Conflict.Item, Descriptor));
                  end if;

                  Append (Parse_Items, (New_Config, Conflict, Parsed => False, Shift_Count => Item.Shift_Count));
               end;
            end if;
            Conflict := Conflict.Next;
         end loop;

         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Super.Label (Parser_Index), Trace_Prefix & ":" & State_Index'Image (Config.Stack.Peek.State) &
                 " :" & WisiToken.Token_Index'Image (Config.Current_Shared_Token) &
                 ":" & Image (Current_Token, Descriptor) &
                 " : " & Image (Action.Item, Descriptor) &
                 (if Action.Item.Verb = Reduce
                  then " via" & Config.Stack.Peek (SAL.Peek_Type (Action.Item.Token_Count + 1)).State'Image
                  else ""));
         end if;

         case Action.Item.Verb is
         when Shift =>
            Item.Shift_Count := Item.Shift_Count + 1;

            Config.Stack.Push
              ((Action.Item.State,
                Invalid_Node_Index,
                (Current_Token.ID,
                 Byte_Region        => Current_Token.Byte_Region,
                 Min_Terminal_Index =>
                   (if Config.Current_Insert_Delete = No_Insert_Delete
                    then Config.Current_Shared_Token
                    else Invalid_Token_Index),
                 Name              => Null_Buffer_Region,
                 Virtual           => Config.Current_Insert_Delete /= No_Insert_Delete)));

            Current_Token := Next_Token
              (Terminals                 => Shared.Terminals.all,
               Terminals_Current         => Config.Current_Shared_Token,
               Restore_Terminals_Current => Restore_Terminals_Current,
               Insert_Delete             => Config.Insert_Delete,
               Current_Insert_Delete     => Config.Current_Insert_Delete);

         when Reduce =>
            declare
               Nonterm : Recover_Token;
            begin
               Config.Check_Status := Reduce_Stack
                 (Shared, Config.Stack, Action.Item, Nonterm,
                  Default_Virtual => Config.Current_Insert_Delete /= No_Insert_Delete);

               case Config.Check_Status.Label is
               when Ok =>
                  New_State := Config.Stack.Peek.State;
                  New_State := Goto_For (Table, New_State, Action.Item.Production.LHS);

                  if New_State = Unknown_State then
                     --  Most likely from an inappropriate language fix.
                     if Trace_McKenzie > Outline then
                        Base.Put (Trace_Prefix & ": Unknown_State: ", Super, Shared, Parser_Index, Config);
                        Put_Line (Trace, Trace_Prefix & ": stack: " & Image (Config.Stack, Descriptor));
                     end if;

                     --  We can't just return False here; user must abandon this config.
                     raise Bad_Config;
                  end if;

                  Config.Stack.Push ((New_State, Invalid_Node_Index, Nonterm));

               when Semantic_Checks.Error =>
                  Config.Error_Token       := Nonterm;
                  Config.Check_Token_Count := Action.Item.Token_Count;
                  Success                  := False;
               end case;
            end;

         when Error =>

            Config.Error_Token :=
              (ID          => Current_Token.ID,
               Byte_Region => Current_Token.Byte_Region,
               others      => <>);
            Success            := False;

         when Accept_It =>
            null;
         end case;

         exit when not Success or
           Action.Item.Verb = Accept_It or
           (if Shared_Token_Goal = Invalid_Token_Index
            then Length (Config.Insert_Delete) = 0
            else Config.Current_Shared_Token > Shared_Token_Goal);

         Action := Action_For (Table, Config.Stack.Peek.State, Current_Token.ID);
      end loop;

      Config.Current_Shared_Token := Restore_Terminals_Current;

      return Success;
   end Parse_One_Item;

   function Parse
     (Super             :         not null access Base.Supervisor;
      Shared            :         not null access Base.Shared;
      Parser_Index      :         in              SAL.Peek_Type;
      Parse_Items       : aliased    out          Parse_Item_Arrays.Vector;
      Config            :         in              Configuration;
      Shared_Token_Goal :         in              Base_Token_Index;
      All_Conflicts     :         in              Boolean;
      Trace_Prefix      :         in              String)
     return Boolean
   is
      use Parse_Item_Arrays;
      Trace : WisiToken.Trace'Class renames Super.Trace.all;

      Last_Parsed : Natural;
      Success     : Boolean;
   begin
      Clear (Parse_Items);
      Append (Parse_Items, (Config, Action => null, Parsed => False, Shift_Count => 0));

      --  Clear any errors; so they reflect the parse result.
      declare
         Config : Configuration renames Parse_Item_Array_Refs.Variable_Ref
           (Parse_Items, First_Index (Parse_Items)).Config;
      begin
         Config.Error_Token.ID := Invalid_Token_ID;
         Config.Check_Status   := (Label => Semantic_Checks.Ok);
      end;

      Last_Parsed := First_Index (Parse_Items);
      loop
         --  Loop over initial config and any conflicts.
         Success := Parse_One_Item
           (Super, Shared, Parser_Index, Parse_Items, Last_Parsed, Shared_Token_Goal, Trace_Prefix);

         exit when Last_Index (Parse_Items) = Last_Parsed;

         exit when Success and not All_Conflicts;

         Last_Parsed := Last_Parsed + 1;
         if Trace_McKenzie > Detail then
            Put_Line (Trace, Super.Label (Parser_Index), Trace_Prefix & ": parse conflict");
         end if;
      end loop;

      return Success;
   end Parse;

end WisiToken.Parse.LR.McKenzie_Recover.Parse;
