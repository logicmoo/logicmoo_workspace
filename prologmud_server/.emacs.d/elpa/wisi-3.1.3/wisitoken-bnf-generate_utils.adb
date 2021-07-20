--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014, 2015, 2017 - 2020  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Exceptions;
with Ada.Text_IO;
with WisiToken.Generate; use WisiToken.Generate;
with WisiToken.Syntax_Trees;
with WisiToken.Wisi_Ada;
package body WisiToken.BNF.Generate_Utils is

   --  For Constant_Reference
   Aliased_EOI_Name              : aliased constant Ada.Strings.Unbounded.Unbounded_String := +EOI_Name;
   Aliased_WisiToken_Accept_Name : aliased constant Ada.Strings.Unbounded.Unbounded_String :=
     +WisiToken_Accept_Name;

   --  body specs, as needed.

   ----------
   --  Body subprograms

   function Find_Kind (Data : aliased Generate_Data; Target_Kind : in String) return Token_ID
   is begin
      for Cursor in All_Tokens (Data).Iterate loop
         if Kind (Cursor) = Target_Kind then
            return ID (Cursor);
         end if;
      end loop;
      return Invalid_Token_ID;
   end Find_Kind;

   function Name_1 (Cursor : in Token_Cursor) return String
   is begin
      --   This function is used to compute Descriptor.Image
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         return -Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Name;

      when Terminals_Keywords =>
         return -Cursor.Data.Tokens.Keywords (Cursor.Keyword).Name;

      when Terminals_Others =>
         return -Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Name;

      when EOI =>
         return EOI_Name;

      when WisiToken_Accept =>
         return WisiToken_Accept_Name;

      when Nonterminal =>
         return -Cursor.Data.Tokens.Rules (Cursor.Nonterminal).Left_Hand_Side;

      when Done =>
         raise SAL.Programmer_Error with "token cursor is done";
      end case;
   end Name_1;

   procedure To_Grammar
     (Data             : aliased in out Generate_Data;
      Source_File_Name :         in     String;
      Start_Token      :         in     String)
   is
      use WisiToken.Wisi_Ada;
      Descriptor : WisiToken.Descriptor renames Data.Descriptor.all;
   begin
      Data.Grammar.Set_First_Last (Descriptor.First_Nonterminal, Descriptor.Last_Nonterminal);
      Data.Source_Line_Map.Set_First_Last (Descriptor.First_Nonterminal, Descriptor.Last_Nonterminal);

      Data.Action_Names := new Names_Array_Array (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal);
      Data.Check_Names  := new Names_Array_Array (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal);

      pragma Assert (Descriptor.Accept_ID = Descriptor.First_Nonterminal);

      Data.Source_Line_Map (Descriptor.Accept_ID).Line := Line_Number_Type'First;
      Data.Source_Line_Map (Descriptor.Accept_ID).RHS_Map.Set_First_Last (0, 0);
      Data.Source_Line_Map (Descriptor.Accept_ID).RHS_Map (0) := Line_Number_Type'First;

      if Start_Token = "" then
         Put_Error (Error_Message (Source_File_Name, 1, "%start not specified"));
      else
         begin
            Data.Grammar (Descriptor.Accept_ID) :=
              Descriptor.Accept_ID <= Only
                (Find_Token_ID (Data, Start_Token) & Descriptor.EOI_ID + WisiToken.Syntax_Trees.Null_Action);
         exception
         when Not_Found =>
            Put_Error
              (Error_Message
                 (Source_File_Name, 1,
                  "start token '" & (Start_Token) & "' not found"));
         end;
      end if;

      for Rule of Data.Tokens.Rules loop
         declare
            RHS_Index        : Natural := 0;
            RHSs             : WisiToken.Productions.RHS_Arrays.Vector;
            LHS              : Token_ID; -- not initialized for exception handler
            Action_Names     : Names_Array (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            Action_All_Empty : Boolean := True;
            Check_Names      : Names_Array (0 .. Integer (Rule.Right_Hand_Sides.Length) - 1);
            Check_All_Empty  : Boolean := True;
         begin
            LHS := Find_Token_ID (Data, -Rule.Left_Hand_Side);

            RHSs.Set_First_Last (RHS_Index, Natural (Rule.Right_Hand_Sides.Length) - 1);

            Data.Source_Line_Map (LHS).Line := Rule.Source_Line;
            Data.Source_Line_Map (LHS).RHS_Map.Set_First_Last (RHSs.First_Index, RHSs.Last_Index);

            for Right_Hand_Side of Rule.Right_Hand_Sides loop
               declare
                  use Ada.Strings.Unbounded;
                  use all type Ada.Containers.Count_Type;
                  Tokens : WisiToken.Token_ID_Arrays.Vector;
                  I      : Integer := 1;
               begin
                  if Right_Hand_Side.Tokens.Length > 0 then
                     Tokens.Set_First_Last (I, Integer (Right_Hand_Side.Tokens.Length));
                     for Token of Right_Hand_Side.Tokens loop
                        Tokens (I) := Find_Token_ID (Data, -Token.Identifier);
                        I := I + 1;
                     end loop;
                  end if;
                  RHSs (RHS_Index) :=
                    (Tokens => Tokens, Action => null, Check => null, Recursion => <>);
                  if Length (Right_Hand_Side.Action) > 0 then
                     Action_All_Empty := False;
                     Action_Names (RHS_Index) := new String'
                       (-Rule.Left_Hand_Side & '_' & WisiToken.Trimmed_Image (RHS_Index));
                  end if;
                  if Length (Right_Hand_Side.Check) > 0 then
                     Check_All_Empty := False;
                     Check_Names (RHS_Index) := new String'
                       (-Rule.Left_Hand_Side & '_' & WisiToken.Trimmed_Image (RHS_Index) & "_check");
                  end if;

                  Data.Source_Line_Map (LHS).RHS_Map (RHS_Index) := Right_Hand_Side.Source_Line;
               exception
               when E : Not_Found =>
                  --  From "&"
                  Put_Error
                    (Error_Message
                       (Source_File_Name, Right_Hand_Side.Source_Line, Ada.Exceptions.Exception_Message (E)));
               end;
               RHS_Index := RHS_Index + 1;
            end loop;

            Data.Grammar (LHS) := LHS <= RHSs;
            if not Action_All_Empty then
               Data.Action_Names (LHS) := new Names_Array'(Action_Names);
            end if;
            if not Check_All_Empty then
               Data.Check_Names (LHS) := new Names_Array'(Check_Names);
            end if;

         exception
         when E : Not_Found =>
            --  From Find_Token_ID (left_hand_side)
            Put_Error
              (Error_Message
                 (Source_File_Name, Rule.Source_Line, Ada.Exceptions.Exception_Message (E)));
         end;
      end loop;

      WisiToken.Generate.Check_Consistent (Data.Grammar, Descriptor, Source_File_Name);
   end To_Grammar;

   ----------
   --  Public subprograms, declaration order

   function Initialize
     (Input_Data       : aliased in WisiToken_Grammar_Runtime.User_Data_Type;
      Ignore_Conflicts :         in Boolean := False)
     return Generate_Data
   is
      EOI_ID : constant Token_ID := Token_ID
        (Count (Input_Data.Tokens.Non_Grammar) + Count (Input_Data.Tokens.Tokens)) + Token_ID
          (Input_Data.Tokens.Keywords.Length) + Token_ID'First;
   begin
      return Result : aliased Generate_Data :=
        (Tokens => Input_Data.Tokens'Access,

         Descriptor => new WisiToken.Descriptor
           (First_Terminal    =>
              (if Count (Input_Data.Tokens.Non_Grammar) > 0
               then Token_ID (Count (Input_Data.Tokens.Non_Grammar)) + Token_ID'First
               else Token_ID'First),
            Last_Terminal     => EOI_ID,
            EOI_ID            => EOI_ID,
            Accept_ID         => EOI_ID + 1,
            First_Nonterminal => EOI_ID + 1,
            Last_Nonterminal  => EOI_ID + 1 + Token_ID (Input_Data.Tokens.Rules.Length)),

         others => <>)
      do
         Result.Descriptor.Case_Insensitive := Input_Data.Language_Params.Case_Insensitive;
         Result.Descriptor.New_Line_ID      := Find_Kind (Result, "new-line");
         Result.Descriptor.String_1_ID      := Find_Kind (Result, "string-single");
         Result.Descriptor.String_2_ID      := Find_Kind (Result, "string-double");

         --  Image set in loop below, which also updates these widths.
         Result.Descriptor.Terminal_Image_Width := 0;
         Result.Descriptor.Image_Width          := 0;

         Result.Descriptor.Last_Lookahead       :=
           (case (Input_Data.User_Parser) is
            when None                                  => Invalid_Token_ID,
            when LR1                                   => Result.Descriptor.Last_Terminal,
            when LALR                                  => Result.Descriptor.First_Nonterminal,
            when Packrat_Generate_Algorithm | External => Invalid_Token_ID);

         for Cursor in All_Tokens (Result).Iterate loop
            Result.Descriptor.Image (ID (Cursor)) := new String'(Name_1 (Cursor));
         end loop;

         for ID in Result.Descriptor.Image'Range loop
            if ID in Result.Descriptor.First_Terminal .. Result.Descriptor.Last_Terminal then
               if Result.Descriptor.Image (ID).all'Length > Result.Descriptor.Terminal_Image_Width then
                  Result.Descriptor.Terminal_Image_Width := Result.Descriptor.Image (ID).all'Length;
               end if;
            end if;

            if Result.Descriptor.Image (ID).all'Length > Result.Descriptor.Image_Width then
               Result.Descriptor.Image_Width := Result.Descriptor.Image (ID).all'Length;
            end if;
         end loop;

         To_Grammar (Result, Input_Data.Grammar_Lexer.File_Name, -Input_Data.Language_Params.Start_Token);
         Result.Ignore_Conflicts := Ignore_Conflicts;
      end return;
   end Initialize;

   function Find_Token_ID (Data : aliased in Generate_Data; Token : in String) return Token_ID
   is begin
      for Cursor in All_Tokens (Data).Iterate loop
         if Name (Cursor) = Token then
            return ID (Cursor);
         end if;
      end loop;
      raise Not_Found with "token '" & Token & "' not found";
   end Find_Token_ID;

   function All_Tokens (Data : aliased in Generate_Data) return Token_Container
   is begin
      return (Data => Data'Access);
   end All_Tokens;

   function Constant_Reference
     (Container : aliased in Token_Container'Class;
      Cursor    :         in Token_Cursor)
     return Token_Constant_Reference_Type
   is begin
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         return
           (Element => Container.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Name'Access);

      when Terminals_Keywords =>
         return (Element => Container.Data.Tokens.Keywords (Cursor.Keyword).Name'Access);

      when Terminals_Others =>
         return (Element => Container.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Name'Access);

      when EOI =>
         return (Element => Aliased_EOI_Name'Access);

      when WisiToken_Accept =>
         return (Element => Aliased_WisiToken_Accept_Name'Access);

      when Nonterminal =>
         return (Element => Container.Data.Tokens.Rules (Cursor.Nonterminal).Left_Hand_Side'Access);

      when Done =>
         raise SAL.Programmer_Error with "token cursor is done";
      end case;
   end Constant_Reference;

   type Iterator (Container : not null access constant Token_Container)
   is new Iterator_Interfaces.Forward_Iterator with record
      Non_Grammar  : Boolean;
      Nonterminals : Boolean;
   end record;

   overriding function First (Object : Iterator) return Token_Cursor;
   overriding function Next (Object : Iterator; Position : Token_Cursor) return Token_Cursor;

   overriding function First (Object : Iterator) return Token_Cursor
   is begin
      return First (Object.Container.Data.all, Object.Non_Grammar, Object.Nonterminals);
   end First;

   overriding function Next (Object  : Iterator; Position : Token_Cursor) return Token_Cursor
   is
      Next_Position : Token_Cursor := Position;
   begin
      Next (Next_Position, Object.Nonterminals);
      return Next_Position;
   end Next;

   function Iterate
     (Container    : aliased    Token_Container;
      Non_Grammar  :         in Boolean := True;
      Nonterminals :         in Boolean := True)
     return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container'Access, Non_Grammar, Nonterminals);
   end Iterate;

   function Next_Kind_Internal
     (Cursor       : in out Token_Cursor;
      Nonterminals : in     Boolean)
     return Boolean
   is begin
      --  Advance Cursor to the next kind; return True if any of that
      --  kind exist, or kind is Done; False otherwise.
      case Cursor.Kind is
      when Non_Grammar_Kind =>

         Cursor :=
           (Data        => Cursor.Data,
            Kind        => Terminals_Keywords,
            ID          => Cursor.Data.Descriptor.First_Terminal,
            Token_Kind  => WisiToken.BNF.Token_Lists.No_Element,
            Token_Item  => String_Triple_Lists.No_Element,
            Keyword     => Cursor.Data.Tokens.Keywords.First,
            Nonterminal => Rule_Lists.No_Element);

         return String_Pair_Lists.Has_Element (Cursor.Keyword);

      when Terminals_Keywords =>

         Cursor :=
           (Data        => Cursor.Data,
            Kind        => Terminals_Others,
            ID          => Cursor.ID,
            Token_Kind  => Cursor.Data.Tokens.Tokens.First,
            Token_Item  => String_Triple_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

         if WisiToken.BNF.Token_Lists.Has_Element (Cursor.Token_Kind) then
            Cursor.Token_Item := Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens.First;
            return WisiToken.BNF.String_Triple_Lists.Has_Element (Cursor.Token_Item);
         else
            return False;
         end if;

      when Terminals_Others =>

         Cursor :=
           (Data        => Cursor.Data,
            Kind        => EOI,
            ID          => Cursor.ID,
            Token_Kind  => WisiToken.BNF.Token_Lists.No_Element,
            Token_Item  => String_Triple_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Rule_Lists.No_Element);

         return True;

      when EOI =>
         if Nonterminals then
            if Rule_Lists.Has_Element (Cursor.Data.Tokens.Rules.First) then
               Cursor :=
                 (Data        => Cursor.Data,
                  Kind        => WisiToken_Accept,
                  ID          => Cursor.ID,
                  Token_Kind  => WisiToken.BNF.Token_Lists.No_Element,
                  Token_Item  => String_Triple_Lists.No_Element,
                  Keyword     => String_Pair_Lists.No_Element,
                  Nonterminal => Rule_Lists.No_Element);
            else
               Cursor.Kind := Done;
            end if;
            return True;
         else
            Cursor.Kind := Done;
            return True;
         end if;

      when WisiToken_Accept =>

         Cursor :=
           (Data        => Cursor.Data,
            Kind        => Nonterminal,
            ID          => Cursor.ID,
            Token_Kind  => WisiToken.BNF.Token_Lists.No_Element,
            Token_Item  => String_Triple_Lists.No_Element,
            Keyword     => String_Pair_Lists.No_Element,
            Nonterminal => Cursor.Data.Tokens.Rules.First);

         --  Can't get here with no rules
         return True;

      when Nonterminal =>
         Cursor.Kind := Done;
         return True;

      when Done =>
         return True;
      end case;
   end Next_Kind_Internal;

   function First
     (Data         : aliased in Generate_Data;
      Non_Grammar  :         in Boolean;
      Nonterminals :         in Boolean)
     return Token_Cursor
   is
      Cursor : Token_Cursor :=
        (Data        => Data'Access,
         Kind        => Non_Grammar_Kind,
         ID          => Token_ID'First,
         Token_Kind  => Data.Tokens.Non_Grammar.First,
         Token_Item  => String_Triple_Lists.No_Element,
         Keyword     => String_Pair_Lists.No_Element,
         Nonterminal => Rule_Lists.No_Element);
   begin
      if Non_Grammar then
         if WisiToken.BNF.Token_Lists.Has_Element (Cursor.Token_Kind) then
            Cursor.Token_Item := Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens.First;
            if WisiToken.BNF.String_Triple_Lists.Has_Element (Cursor.Token_Item) then
               return Cursor;
            end if;
         end if;
      end if;

      --  There are no non_grammar tokens, or Non_Grammar false
      loop
         exit when Next_Kind_Internal (Cursor, Nonterminals);
      end loop;
      return Cursor;
   end First;

   procedure Next (Cursor : in out Token_Cursor; Nonterminals : in Boolean)
   is begin
      Cursor.ID := Cursor.ID + 1;

      case Cursor.Kind is
      when Non_Grammar_Kind =>
         String_Triple_Lists.Next (Cursor.Token_Item);
         if String_Triple_Lists.Has_Element (Cursor.Token_Item) then
            return;
         else
            WisiToken.BNF.Token_Lists.Next (Cursor.Token_Kind);

            if WisiToken.BNF.Token_Lists.Has_Element (Cursor.Token_Kind) then
               Cursor.Token_Item := Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens.First;
               if String_Triple_Lists.Has_Element (Cursor.Token_Item) then
                  return;
               end if;
            end if;
         end if;

         loop
            exit when Next_Kind_Internal (Cursor, Nonterminals);
         end loop;
         return;

      when Terminals_Keywords =>
         --  Keywords before other terminals, so they have precedence over Identifiers

         String_Pair_Lists.Next (Cursor.Keyword);
         if String_Pair_Lists.Has_Element (Cursor.Keyword) then
            return;
         end if;

         loop
            exit when Next_Kind_Internal (Cursor, Nonterminals);
         end loop;
         return;

      when Terminals_Others =>
         WisiToken.BNF.String_Triple_Lists.Next (Cursor.Token_Item);
         if WisiToken.BNF.String_Triple_Lists.Has_Element (Cursor.Token_Item) then
            return;
         else
            WisiToken.BNF.Token_Lists.Next (Cursor.Token_Kind);
            if WisiToken.BNF.Token_Lists.Has_Element (Cursor.Token_Kind) then
               Cursor.Token_Item := Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens.First;
               if WisiToken.BNF.String_Triple_Lists.Has_Element (Cursor.Token_Item) then
                  return;
               end if;
            end if;
         end if;

         loop
            exit when Next_Kind_Internal (Cursor, Nonterminals);
         end loop;
         return;

      when EOI =>
         if Next_Kind_Internal (Cursor, Nonterminals) then
            return;
         else
            raise SAL.Programmer_Error;
         end if;

      when WisiToken_Accept =>
         if Next_Kind_Internal (Cursor, Nonterminals) then
            return;
         else
            raise SAL.Programmer_Error;
         end if;

      when Nonterminal =>
         Rule_Lists.Next (Cursor.Nonterminal);
         if Rule_Lists.Has_Element (Cursor.Nonterminal) then
            return;
         end if;

         loop
            exit when Next_Kind_Internal (Cursor, Nonterminals);
         end loop;
         return;

      when Done =>
         null;
      end case;
   end Next;

   function Is_Done (Cursor : in Token_Cursor) return Boolean
   is begin
      return Cursor.Kind = Done;
   end Is_Done;

   function ID (Cursor : in Token_Cursor) return Token_ID
   is begin
      return Cursor.ID;
   end ID;

   function Name (Cursor : in Token_Cursor) return String
   is begin
      return Cursor.Data.Descriptor.Image (Cursor.ID).all;
   end Name;

   function Kind (Cursor : in Token_Cursor) return String
   is begin
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         return -Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Kind;

      when Terminals_Keywords =>
         return "keyword";

      when Terminals_Others =>
         return -Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Kind;

      when EOI =>
         return "EOI";

      when WisiToken_Accept =>
         return "accept";

      when Nonterminal =>
         return "nonterminal";

      when Done =>
         raise SAL.Programmer_Error with "token cursor is done";
      end case;
   end Kind;

   function Value (Cursor : in Token_Cursor) return String
   is begin
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         return -Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Value;

      when Terminals_Keywords =>
         return -Cursor.Data.Tokens.Keywords (Cursor.Keyword).Value;

      when Terminals_Others =>
         return -Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Value;

      when EOI | WisiToken_Accept | Nonterminal =>
            return "";

      when Done =>
         raise SAL.Programmer_Error with "token cursor is done";
      end case;
   end Value;

   function Repair_Image (Cursor : in Token_Cursor) return String
   is begin
      case Cursor.Kind is
      when Non_Grammar_Kind =>
         return -Cursor.Data.Tokens.Non_Grammar (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Repair_Image;

      when Terminals_Keywords =>
         return "";

      when Terminals_Others =>
         return -Cursor.Data.Tokens.Tokens (Cursor.Token_Kind).Tokens (Cursor.Token_Item).Repair_Image;

      when EOI | WisiToken_Accept | Nonterminal =>
            return "";

      when Done =>
         raise SAL.Programmer_Error with "token cursor is done";
      end case;
   end Repair_Image;

   function To_Conflicts
     (Data             : aliased in out Generate_Data;
      Conflicts        :         in     WisiToken.BNF.Conflict_Lists.List;
      Source_File_Name :         in     String)
     return WisiToken.Generate.LR.Conflict_Lists.List
   is
      use WisiToken.Generate.LR;
      Result   : WisiToken.Generate.LR.Conflict_Lists.List;
      Conflict : WisiToken.Generate.LR.Conflict;
   begin
      for Item of Conflicts loop
         begin
            Conflict :=
              (Conflict_Parse_Actions'Value (-Item.Action_A),
               Find_Token_ID (Data, -Item.LHS_A),
               Conflict_Parse_Actions'Value (-Item.Action_B),
               Find_Token_ID (Data, -Item.LHS_B),
               -1,
               Find_Token_ID (Data, -Item.On));

            Result.Append (Conflict);
         exception
         when E : Not_Found =>
            if not Data.Ignore_Conflicts then
               Put_Error
                 (Error_Message
                    (Source_File_Name, Item.Source_Line, Ada.Exceptions.Exception_Message (E)));
            end if;
         end;
      end loop;
      return Result;
   end To_Conflicts;

   function To_Nonterminal_ID_Set
     (Data : aliased in Generate_Data;
      Item :         in String_Lists.List)
     return Token_ID_Set
   is
      Result : Token_ID_Set := (Data.Descriptor.First_Nonterminal .. Data.Descriptor.Last_Nonterminal => False);
   begin
      for Token of Item loop
         Result (Find_Token_ID (Data, Token)) := True;
      end loop;
      return Result;
   end To_Nonterminal_ID_Set;

   function To_McKenzie_Param
     (Data : aliased in Generate_Data;
      Item :         in McKenzie_Recover_Param_Type)
     return WisiToken.Parse.LR.McKenzie_Param_Type
   is
      use Ada.Strings.Unbounded;

      Result : WisiToken.Parse.LR.McKenzie_Param_Type :=
        --  We use an aggregate, and overwrite some below, so the compiler
        --  reminds us to change this when we modify McKenzie_Param_Type.
        (Data.Descriptor.First_Terminal,
         Data.Descriptor.Last_Terminal,
         Data.Descriptor.First_Nonterminal,
         Data.Descriptor.Last_Nonterminal,
         Insert                      => (others => Item.Default_Insert),
         Delete                      => (others => Item.Default_Delete_Terminal),
         Push_Back                   => (others => Item.Default_Push_Back),
         Undo_Reduce                 => (others => Item.Default_Push_Back), -- no separate default for undo_reduce
         Minimal_Complete_Cost_Delta => Item.Minimal_Complete_Cost_Delta,
         Fast_Forward                => Item.Fast_Forward,
         Matching_Begin              => Item.Matching_Begin,
         Ignore_Check_Fail           => Item.Ignore_Check_Fail,
         Task_Count                  => 0,
         Check_Limit                 => Item.Check_Limit,
         Check_Delta_Limit           => Item.Check_Delta_Limit,
         Enqueue_Limit               => Item.Enqueue_Limit);
   begin
      for Pair of Item.Delete loop
         Result.Delete (Find_Token_ID (Data, -Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;
      for Pair of Item.Insert loop
         Result.Insert (Find_Token_ID (Data, -Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;
      for Pair of Item.Push_Back loop
         Result.Push_Back (Find_Token_ID (Data, -Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;
      for Pair of Item.Undo_Reduce loop
         Result.Undo_Reduce (Find_Token_ID (Data, -Pair.Name)) := Natural'Value (-Pair.Value);
      end loop;

      return Result;
   end To_McKenzie_Param;

   procedure Put_Stats
     (Input_Data    : in WisiToken_Grammar_Runtime.User_Data_Type;
      Generate_Data : in Generate_Utils.Generate_Data)
   is
      use Ada.Text_IO;
   begin
      New_Line;
      Put_Line
        (Integer'Image (Input_Data.Rule_Count) & " rules," &
           Integer'Image (Input_Data.Action_Count) & " user actions," &
           Integer'Image (Input_Data.Check_Count) & " checks," &
           WisiToken.State_Index'Image (Generate_Data.Parser_State_Count) & " states");
   end Put_Stats;

end WisiToken.BNF.Generate_Utils;
