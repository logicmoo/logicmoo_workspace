--  Abstract :
--
--  See spec.
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

with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL;
with WisiToken.Semantic_Checks;
package body Wisi is
   use WisiToken;

   Chars_Per_Int : constant Integer := Integer'Width;

   ----------
   --  body subprogram specs (as needed), alphabetical

   function Indent_Nil_P (Indent : in Indent_Type) return Boolean;

   function Max_Anchor_ID
     (Data       : in out Parse_Data_Type;
      First_Line : in     Line_Number_Type;
      Last_Line  : in     Line_Number_Type)
     return Integer;

   function Paren_In_Anchor_Line
     (Data         : in out Parse_Data_Type'Class;
      Tree         : in     Syntax_Trees.Tree'Class;
      Anchor_Token : in     Augmented_Token;
      Offset       : in     Integer)
     return Integer;

   ----------
   --  body subprograms bodies, alphabetical

   procedure Adjust_Paren_State
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree'Class;
      First_Token_Index : in     Token_Index;
      First_Line        : in     Line_Number_Type;
      Adjust            : in     Integer)
   is begin
      for I in First_Token_Index .. Data.Terminals.Last_Index loop
         declare
            Aug : Augmented_Token renames Get_Aug_Token_Var (Data, Tree, I);
         begin
            Aug.Paren_State := Aug.Paren_State + Adjust;
         end;
      end loop;

      for Line in First_Line .. Data.Line_Paren_State.Last_Index loop
         Data.Line_Paren_State (Line) := Data.Line_Paren_State (Line) + Adjust;
      end loop;
   end Adjust_Paren_State;

   function Image (Aug : in WisiToken.Base_Token_Class_Access; Descriptor : in WisiToken.Descriptor) return String
   is begin
      return Image (Augmented_Token_Access (Aug).all, Descriptor);
   end Image;

   function Image (Action : in WisiToken.Syntax_Trees.Semantic_Action) return String
   is
      pragma Unreferenced (Action);
   begin
      return "action";
   end Image;

   function Image (Anchor_IDs : in Anchor_ID_Vectors.Vector) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +"(";
   begin
      for I in Anchor_IDs.First_Index .. Anchor_IDs.Last_Index loop
         Result := Result & Integer'Image (Anchor_IDs (I));
         if I /= Anchor_IDs.Last_Index then
            Result := Result & ", ";
         else
            Result := Result & ")";
         end if;
      end loop;
      return -Result;
   end Image;

   function Image (Indent : in Indent_Type) return String
   is begin
      case Indent.Label is
      when Not_Set =>
         return "(" & Indent_Label'Image (Indent.Label) & ")";

      when Int =>
         return "(" & Indent_Label'Image (Indent.Label) & Integer'Image (Indent.Int_Indent) & ")";

      when Anchor_Nil =>
         return "(" & Indent_Label'Image (Indent.Label) & ", " & Image (Indent.Anchor_Nil_IDs) & ", nil)";

      when Anchor_Int =>
         return "(" & Indent_Label'Image (Indent.Label) & ", " & Image (Indent.Anchor_Int_IDs) & ", " & Integer'Image
           (Indent.Anchor_Int_Indent) & ")";

      when Anchored =>
         return "(" & Indent_Label'Image (Indent.Label) & ", " & Integer'Image (Indent.Anchored_ID) & ", " &
           Integer'Image (Indent.Anchored_Delta) & ")";

      when Anchor_Anchored =>
         return "(" & Indent_Label'Image (Indent.Label) & ", " & Image (Indent.Anchor_Anchored_IDs) & Integer'Image
           (Indent.Anchor_Anchored_ID) & ", " & Integer'Image (Indent.Anchor_Anchored_Delta) & ")";
      end case;
   end Image;

   procedure Indent_Apply_Anchored
     (Delta_Indent : in     Simple_Delta_Type;
      Indent       : in out Indent_Type)
   with Pre => Delta_Indent.Label = Anchored
   is begin
      --  Add Delta_Indent to Indent

      case Indent.Label is
      when Not_Set =>
         Indent := (Anchored, Delta_Indent.Anchored_ID, Delta_Indent.Anchored_Delta);

      when Int =>
         if Delta_Indent.Anchored_Accumulate then
            Indent := (Anchored, Delta_Indent.Anchored_ID, Indent.Int_Indent + Delta_Indent.Anchored_Delta);
         end if;

      when Anchor_Nil =>
         Indent :=
           (Anchor_Anchored,
            Indent.Anchor_Nil_IDs,
            Delta_Indent.Anchored_ID,
            Delta_Indent.Anchored_Delta);

      when Anchor_Int =>
         if Delta_Indent.Anchored_Accumulate then
            Indent :=
              (Anchor_Anchored,
               Indent.Anchor_Int_IDs,
               Delta_Indent.Anchored_ID,
               Delta_Indent.Anchored_Delta + Indent.Anchor_Int_Indent);
         end if;

      when Anchored | Anchor_Anchored =>
         --  already anchored
         null;
      end case;
   end Indent_Apply_Anchored;

   procedure Indent_Apply_Int (Indent : in out Indent_Type; Offset : in Integer)
   is begin
      --  Add an Int indent to Indent
      case Indent.Label is
      when Not_Set =>
         Indent := (Int, Offset);

      when Int =>
         Indent.Int_Indent := Indent.Int_Indent + Offset;

      when Anchor_Nil         =>
         Indent :=
           (Label             => Anchor_Int,
            Anchor_Int_IDs    => Indent.Anchor_Nil_IDs,
            Anchor_Int_Indent => Offset);

      when Anchor_Int =>
         Indent.Anchor_Int_Indent := Indent.Anchor_Int_Indent + Offset;

      when Anchored | Anchor_Anchored =>
         null;
      end case;
   end Indent_Apply_Int;

   procedure Indent_Line
     (Data         : in out Parse_Data_Type;
      Line         : in     Line_Number_Type;
      Delta_Indent : in     Delta_Type)
   is
      --  See note in Indent_Anchored_2 for why we can't use renames here.
      Indent : Indent_Type := Data.Indents (Line);
   begin
      case Delta_Indent.Label is
      when Simple =>
         case Delta_Indent.Simple_Delta.Label is
         when None =>
            null;

         when Int =>
            Indent_Apply_Int (Indent, Delta_Indent.Simple_Delta.Int_Delta);

         when Anchored =>
            Indent_Apply_Anchored (Delta_Indent.Simple_Delta, Indent);
         end case;

      when Hanging =>
         if Delta_Indent.Hanging_Accumulate or Indent_Nil_P (Data.Indents (Line)) then
            if Line = Delta_Indent.Hanging_First_Line then
               --  Apply delta_1
               case Delta_Indent.Hanging_Delta_1.Label is
               when None =>
                  null;
               when Int =>
                  Indent_Apply_Int (Indent, Delta_Indent.Hanging_Delta_1.Int_Delta);
               when Anchored =>
                  Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_1, Indent);
               end case;
            else
               if Delta_Indent.Hanging_Paren_State = Data.Line_Paren_State (Line) then
                  case Delta_Indent.Hanging_Delta_2.Label is
                  when None =>
                     null;
                  when Int =>
                     Indent_Apply_Int (Indent, Delta_Indent.Hanging_Delta_2.Int_Delta);
                  when Anchored =>
                     Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_2, Indent);
                  end case;
               end if;
            end if;
         end if;
      end case;

      if Trace_Action > Extra then
         Ada.Text_IO.Put_Line (";; indent_line: " & Line_Number_Type'Image (Line) & " => " & Image (Indent));
      end if;

      Data.Indents.Replace_Element (Line, Indent);
   end Indent_Line;

   function Indent_Nil_P (Indent : in Indent_Type) return Boolean
   is begin
      return Indent.Label in Not_Set | Anchor_Nil;
   end Indent_Nil_P;

   function Max_Anchor_ID
     (Data       : in out Parse_Data_Type;
      First_Line : in     Line_Number_Type;
      Last_Line  : in     Line_Number_Type)
     return Integer
   is
      Result : Integer := First_Anchor_ID - 1;
   begin
      for Line in First_Line .. Last_Line loop
         declare
            Indent : Indent_Type renames Data.Indents (Line);
         begin
            case Indent.Label is
            when Not_Set | Int =>
               null;
            when Anchor_Nil =>
               Result := Integer'Max (Result, Indent.Anchor_Nil_IDs (Indent.Anchor_Nil_IDs.First_Index));
            when Anchor_Int =>
               Result := Integer'Max (Result, Indent.Anchor_Int_IDs (Indent.Anchor_Int_IDs.First_Index));
            when Anchored =>
               Result := Integer'Max (Result, Indent.Anchored_ID);
            when Anchor_Anchored =>
               Result := Integer'Max (Result, Indent.Anchor_Anchored_ID);
            end case;
         end;
      end loop;
      return Result;
   end Max_Anchor_ID;

   function Paren_In_Anchor_Line
     (Data         : in out Parse_Data_Type'Class;
      Tree         : in     Syntax_Trees.Tree'Class;
      Anchor_Token : in     Augmented_Token;
      Offset       : in     Integer)
     return Integer
   is
      use Valid_Node_Index_Arrays;
      use all type Ada.Containers.Count_Type;

      Left_Paren_ID  : Token_ID renames Data.Left_Paren_ID;
      Right_Paren_ID : Token_ID renames Data.Right_Paren_ID;

      I              : Base_Token_Index := Anchor_Token.First_Terminals_Index;
      Paren_Count    : Integer          := 0;
      Paren_Char_Pos : Buffer_Pos       := Invalid_Buffer_Pos;
      Text_Begin_Pos : Buffer_Pos       := Invalid_Buffer_Pos;
   begin
      Find_First :
      loop
         declare
            Tok : Aug_Token_Const_Ref renames Get_Aug_Token_Const (Data, Tree, I);
         begin
            if Tok.Deleted then
               null;

            elsif Tok.ID = Left_Paren_ID then
               Paren_Count := Paren_Count + 1;
               if Paren_Count = 1 then
                  Paren_Char_Pos := Tok.Char_Region.First;
               end if;

            elsif Tok.ID = Right_Paren_ID then
               Paren_Count := Paren_Count - 1;

            end if;

            if Tok.First then
               Text_Begin_Pos := Tok.Char_Region.First;
               exit Find_First;
            else
               if Length (Tok.Inserted_Before) > 0 then
                  for Node of Tok.Inserted_Before loop
                     declare
                        Ins_Tok : Augmented_Token renames Augmented_Token (Tree.Augmented (Node).all);
                     begin
                        if Ins_Tok.ID = Left_Paren_ID then
                           Paren_Count := Paren_Count + 1;
                           if Paren_Count = 1 then
                              Paren_Char_Pos := Tok.Char_Region.First;
                           end if;

                        elsif Ins_Tok.ID = Right_Paren_ID then
                           Paren_Count := Paren_Count - 1;

                        end if;

                        if Ins_Tok.First then
                           Text_Begin_Pos := Tok.Char_Region.First;
                           exit Find_First;
                        end if;
                     end;
                  end loop;
               end if;
            end if;
         end;
         I := I - 1;
      end loop Find_First;

      if Paren_Char_Pos /= Invalid_Buffer_Pos and Text_Begin_Pos /= Invalid_Buffer_Pos then
         return 1 + Offset + Integer (Paren_Char_Pos - Text_Begin_Pos);
      else
         return Offset;
      end if;
   end Paren_In_Anchor_Line;

   procedure Put (Cache : in Navigate_Cache_Type)
   is
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 2 + 11 * Chars_Per_Int);
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");

      procedure Append (Item : in Nil_Buffer_Pos)
      is begin
         if Item.Set then
            Append (Line, Buffer_Pos'Image (Item.Item));
         else
            Append (Line, " -1");
         end if;
      end Append;
   begin
      Append (Line, Navigate_Cache_Code);
      Append (Line, Buffer_Pos'Image (Cache.Pos));
      Append (Line, Token_ID'Image (Cache.Statement_ID));
      Append (Line, Token_ID'Image (Cache.ID));
      Append (Line, Integer'Image (Cache.Length));
      Append (Line, Integer'Image (Navigate_Class_Type'Pos (Cache.Class)));
      Append (Cache.Containing_Pos);
      Append (Cache.Prev_Pos);
      Append (Cache.Next_Pos);
      Append (Cache.End_Pos);
      Append (Line, ']');
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Put (Cache : in WisiToken.Buffer_Region)
   is begin
      Ada.Text_IO.Put_Line
        ("[" & Name_Property_Code & Buffer_Pos'Image (Cache.First) & Buffer_Pos'Image (Cache.Last) & "]");
   end Put;

   procedure Put (Cache : in Face_Cache_Type)
   is
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 2 + 4 * Chars_Per_Int);
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");
   begin
      if Cache.Face.Set then
         Append (Line, Face_Property_Code);
         Append (Line, Buffer_Pos'Image (Cache.Char_Region.First));
         Append (Line, Buffer_Pos'Image (Cache.Char_Region.Last));
         Append (Line, Integer'Image (Cache.Face.Item));
         Append (Line, ']');
         Ada.Text_IO.Put_Line (To_String (Line));
      end if;
   end Put;

   procedure Put (Line_Number : in Line_Number_Type; Item : in Indent_Type)
   is begin
      --  All Anchors must be resolved at this point, but not all lines have
      --  an indent computed. A negative indent is an error in either the
      --  grammar indent rules or the algorithms in this package.
      case Item.Label is
      when Not_Set =>
         --  Especially with partial parse, we have no idea what this indent should be.
         null;

      when Int =>
         declare
            --  We can easily get negative indents when there are syntax errors.
            Ind : constant Integer := Integer'Max (0, Item.Int_Indent);
         begin
            Ada.Text_IO.Put_Line
              ('[' & Indent_Code & Line_Number_Type'Image (Line_Number) & Integer'Image (Ind) & ']');
         end;

      when Anchor_Nil | Anchor_Int | Anchored | Anchor_Anchored =>
         raise SAL.Programmer_Error with "Indent item has non-int label: " & Indent_Label'Image (Item.Label);
      end case;
   end Put;

   procedure Put
     (Item : in Parse.LR.Configuration;
      Data : in Parse_Data_Type;
      Tree : in Syntax_Trees.Tree)
   is
      use Ada.Strings.Unbounded;
      use Parse.LR;
      use Parse.LR.Config_Op_Arrays, Parse.LR.Config_Op_Array_Refs;

      --  Output is a sequence of edit regions; each is:
      --  [edit-pos [inserted token-ids] [deleted token-ids] deleted-region]

      type State_Label is
        (None,     -- not started yet
         Inserted, -- edit-pos, some insert ids appended
         Deleted); -- some delete ids appended

      State : State_Label := None;
      --  State of the current edit region.

      Line           : Unbounded_String := To_Unbounded_String ("[");
      Deleted_Region : Buffer_Region    := Null_Buffer_Region;
      Last_Deleted : Config_Op (Delete) := (Delete, Invalid_Token_ID, Invalid_Token_Index);

      procedure Start_Edit_Region (Op : in Insert_Delete_Op)
      is begin
         Append (Line, "[");
         Append (Line, Get_Aug_Token_Const (Data, Tree, Parse.LR.Token_Index (Op)).Char_Region.First'Image);
         Append (Line, "[");
      end Start_Edit_Region;

      function Deleted_Region_Image return String
      is begin
         return "(" & Deleted_Region.First'Image & " . " & Buffer_Pos'Image (Deleted_Region.Last + 1) & ")";
      end Deleted_Region_Image;

      procedure Terminate_Edit_Region
      is begin
         case State is
         when None =>
            null;
         when Inserted =>
            Append (Line, "][]" & Deleted_Region_Image & "]");
         when Deleted =>
            Append (Line, "]" & Deleted_Region_Image & "]");
         end case;
         Deleted_Region := Null_Buffer_Region;
      end Terminate_Edit_Region;
   begin
      if Trace_Action > Outline then
         Ada.Text_IO.Put_Line (";; " & Parse.LR.Image (Item.Ops, Data.Descriptor.all));
      end if;

      Append (Line, Recover_Code);
      for I in First_Index (Item.Ops) .. Last_Index (Item.Ops) loop
         declare
            Op : Config_Op renames Constant_Ref (Item.Ops, I);
         begin
            case Op.Op is
            when Fast_Forward =>
               Terminate_Edit_Region;
               State := None;

            when Undo_Reduce | Push_Back =>
               null;

            when Insert =>
               case State is
               when None =>
                  Start_Edit_Region (Op);

               when Inserted =>
                  null;

               when Deleted =>
                  Terminate_Edit_Region;
                  Start_Edit_Region (Op);

               end case;
               Append (Line, Token_ID'Image (Op.Ins_ID));
               State := Inserted;

            when Delete =>
               Deleted_Region := Deleted_Region and Get_Aug_Token_Const (Data, Tree, Op.Del_Token_Index).Char_Region;
               declare
                  Skip : Boolean := False;
               begin
                  case State is
                  when None =>
                     Start_Edit_Region (Op);
                     Append (Line, "][");

                  when Inserted =>
                     Append (Line, "][");

                  when Deleted =>
                     if Data.Embedded_Quote_Escape_Doubled and then
                       ((Last_Deleted.Del_ID = Data.Descriptor.String_1_ID and
                           Op.Del_ID = Data.Descriptor.String_1_ID) or
                          (Last_Deleted.Del_ID = Data.Descriptor.String_2_ID and
                             Op.Del_ID = Data.Descriptor.String_2_ID))
                     then
                        declare
                           Tok_1 : Augmented_Token renames Get_Aug_Token_Const
                             (Data, Tree, Last_Deleted.Del_Token_Index);
                           Tok_2 : Augmented_Token renames Get_Aug_Token_Const (Data, Tree, Op.Del_Token_Index);
                        begin
                           if Tok_1.Char_Region.Last + 1 = Tok_2.Char_Region.First then
                              --  Buffer text was '"""', lexer repair changed it to '""""'. The
                              --  repaired text looks like a single string with an embedded quote.
                              --  But here, it is two STRING_LITERAL tokens. Don't send the second
                              --  delete to elisp. See test/ada_mode-recover_string_quote_1.adb
                              Skip := True;
                           end if;
                        end;
                     end if;
                  end case;
                  State := Deleted;

                  if not Skip then
                     Append (Line, Token_ID'Image (Op.Del_ID));
                  end if;
               end;
               Last_Deleted := Op;
            end case;
         end;
      end loop;

      case State is
      when None =>
         null;
      when Inserted | Deleted =>
         Terminate_Edit_Region;
      end case;
      Append (Line, "]");
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Resolve_Anchors (Data : in out Parse_Data_Type)
   is
      Begin_Indent  : Integer renames Data.Begin_Indent;
      Anchor_Indent : array (First_Anchor_ID .. Data.Max_Anchor_ID) of Integer;
   begin
      if Trace_Action > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line (";; Begin_Indent: " & Integer'Image (Data.Begin_Indent));
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Ada.Text_IO.Put_Line (";; " & Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
         end loop;
         Ada.Text_IO.Put_Line (";; resolve anchors");
      end if;

      for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
         declare
            Indent : constant Indent_Type := Data.Indents (I);
         begin
            case Indent.Label is
            when Not_Set =>
               --  Indent not computed, therefore not output.
               null;

            when Int =>
               Data.Indents.Replace_Element (I, (Int, Indent.Int_Indent + Begin_Indent));

            when Anchor_Nil =>
               for I of Indent.Anchor_Nil_IDs loop
                  Anchor_Indent (I) := Begin_Indent;
               end loop;
               Data.Indents.Replace_Element (I, (Int, Begin_Indent));

            when Anchor_Int =>
               for I of Indent.Anchor_Int_IDs loop
                  Anchor_Indent (I) := Indent.Anchor_Int_Indent + Begin_Indent;
               end loop;
               Data.Indents.Replace_Element (I, (Int, Indent.Anchor_Int_Indent + Begin_Indent));

            when Anchored =>
               Data.Indents.Replace_Element
                 (I, (Int, Anchor_Indent (Indent.Anchored_ID) + Indent.Anchored_Delta));

            when Anchor_Anchored =>
               declare
                  Temp : constant Integer :=
                    Anchor_Indent (Indent.Anchor_Anchored_ID) + Indent.Anchor_Anchored_Delta;
               begin
                  for I of Indent.Anchor_Anchored_IDs loop
                     Anchor_Indent (I) := Temp;
                  end loop;
                  Data.Indents.Replace_Element (I, (Int, Temp));
               end;
            end case;
         end;
      end loop;
   end Resolve_Anchors;

   procedure Set_End
     (Data           : in out Parse_Data_Type;
      Containing_Pos : in     Buffer_Pos;
      End_Pos        : in     Buffer_Pos)
   is
      use Navigate_Cursor_Lists;
      I            : Cursor := Data.End_Positions.First;
      Delete_Cache : Boolean;
      Temp         : Cursor;
   begin
      loop
         exit when not Has_Element (I);
         declare
            Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Element (I));
         begin
            if Cache.Pos in Containing_Pos .. End_Pos then
               Cache.End_Pos := (True, End_Pos);
               Delete_Cache := True;
            else
               Delete_Cache := False;
            end if;
         end;
         if Delete_Cache then
            Temp := Next (I);
            Delete (Data.End_Positions, I);

            I := Temp;
         else
            Next (I);
         end if;

      end loop;
   end Set_End;

   ----------
   --  public subprograms (declaration order)

   procedure Initialize
     (Data              : in out Parse_Data_Type;
      Lexer             : in     WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Base_Terminals    : in     Base_Token_Array_Access;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     Line_Number_Type;
      End_Line          : in     Line_Number_Type;
      Begin_Indent      : in     Integer;
      Params            : in     String)
   is
      pragma Unreferenced (Params);
   begin
      Data.Line_Begin_Char_Pos.Set_First_Last
        (First   => Begin_Line,
         Last    => End_Line);

      --  + 1 for data on line following last line; see Lexer_To_Augmented.
      Data.Line_Paren_State.Set_First_Last
        (First   => Begin_Line,
         Last    => End_Line + 1);

      Data.Lexer             := Lexer;
      Data.Descriptor        := Descriptor;
      Data.Base_Terminals    := Base_Terminals;
      Data.Post_Parse_Action := Post_Parse_Action;

      case Post_Parse_Action is
      when Navigate | Face =>
         null;
      when Indent =>
         Data.Indents.Set_First_Last
           (First   => Begin_Line,
            Last    => End_Line);

         Data.Begin_Indent := Begin_Indent;
      end case;

      Data.Reset;
   exception
   when E : others =>
      raise SAL.Programmer_Error with "wisi.initialize: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E);
   end Initialize;

   overriding procedure Reset (Data : in out Parse_Data_Type)
   is begin
      Data.Last_Terminal_Node := WisiToken.Invalid_Node_Index;

      Data.Leading_Non_Grammar.Clear;

      --  Data.Line_Begin_Char_Pos  set in Initialize, overwritten in Lexer_To_Augmented

      for S of Data.Line_Paren_State loop
         S := 0;
      end loop;
      Data.Current_Paren_State := 0;

      Data.Navigate_Caches.Finalize;
      Data.Navigate_Caches.Initialize;

      Data.Name_Caches.Finalize;
      Data.Name_Caches.Initialize;

      Data.End_Positions.Clear;

      Data.Face_Caches.Finalize;
      Data.Face_Caches.Initialize;

      for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
         Data.Indents.Replace_Element (I, (Label => Not_Set));
      end loop;
      Data.Max_Anchor_ID := First_Anchor_ID - 1;
   end Reset;

   function Source_File_Name (Data : in Parse_Data_Type) return String
   is begin
      return Data.Lexer.File_Name;
   end Source_File_Name;

   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type
   is begin
      return Data.Post_Parse_Action;
   end Post_Parse_Action;

   overriding
   procedure Lexer_To_Augmented
     (Data  : in out          Parse_Data_Type;
      Tree  : in out          Syntax_Trees.Tree'Class;
      Token : in              Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Lexer.First then
         Data.Line_Begin_Char_Pos (Token.Line) := Token.Char_Region.First;

         if Token.Line > Data.Line_Begin_Char_Pos.First_Index and then
           Data.Line_Begin_Char_Pos (Token.Line - 1) = Invalid_Buffer_Pos
         then
            --  Previous token contains multiple lines; ie %code in wisitoken_grammar.wy
            declare
               First_Set_Line : Line_Number_Type;
            begin
               for Line in reverse Data.Line_Begin_Char_Pos.First_Index .. Token.Line - 1 loop
                  if Data.Line_Begin_Char_Pos (Line) /= Invalid_Buffer_Pos then
                     First_Set_Line := Line;
                     exit;
                  end if;
               end loop;
               for Line in First_Set_Line + 1 .. Token.Line - 1 loop
                  Data.Line_Begin_Char_Pos (Line) := Data.Line_Begin_Char_Pos (First_Set_Line); -- good enough
               end loop;
            end;
         end if;
      end if;

      if Token.ID < Data.Descriptor.First_Terminal then
         --  Non-grammar token

         if Token.ID = Data.Descriptor.New_Line_ID then
            Data.Line_Paren_State (Token.Line + 1) := Data.Current_Paren_State;
         end if;

         if Data.Last_Terminal_Node = Invalid_Node_Index then
            Data.Leading_Non_Grammar.Append ((Token with Lexer.First));
         else
            declare
               Containing_Token : Aug_Token_Var_Ref renames Get_Aug_Token_Var (Tree, Data.Last_Terminal_Node);

               Trailing_Blank : constant Boolean :=
                 Token.ID = Data.Descriptor.New_Line_ID and
                 (Containing_Token.Non_Grammar.Length > 0 and then
                    Containing_Token.Non_Grammar
                      (Containing_Token.Non_Grammar.Last_Index).ID = Data.Descriptor.New_Line_ID);
            begin
               if Lexer.First and
                 (Token.ID in Data.First_Comment_ID .. Data.Last_Comment_ID or
                    Trailing_Blank)
               then
                  if Containing_Token.First_Trailing_Comment_Line = Invalid_Line_Number then
                     Containing_Token.First_Trailing_Comment_Line := Token.Line;
                  end if;
                  Containing_Token.Last_Trailing_Comment_Line  := Token.Line;
               end if;

               Containing_Token.Non_Grammar.Append ((Token with Lexer.First));
            end;
         end if;

      else
         --  grammar token
         declare
            Temp : constant Augmented_Token_Access := new Augmented_Token'
              (Token with
               Deleted                     => False,
               First                       => Lexer.First,
               Paren_State                 => Data.Current_Paren_State,
               First_Terminals_Index       => Data.Terminals.Last_Index,
               Last_Terminals_Index        => Data.Terminals.Last_Index,
               First_Indent_Line           => (if Lexer.First then Token.Line else Invalid_Line_Number),
               Last_Indent_Line            => (if Lexer.First then Token.Line else Invalid_Line_Number),
               First_Trailing_Comment_Line => Invalid_Line_Number, -- Set by Reduce
               Last_Trailing_Comment_Line  => Invalid_Line_Number,
               Non_Grammar                 => Non_Grammar_Token_Arrays.Empty_Vector,
               Inserted_Before             => Valid_Node_Index_Arrays.Empty_Vector);
         begin
            Data.Last_Terminal_Node := Token.Tree_Index;

            if Token.ID = Data.Left_Paren_ID then
               Data.Current_Paren_State := Data.Current_Paren_State + 1;

            elsif Token.ID = Data.Right_Paren_ID then
               Data.Current_Paren_State := Data.Current_Paren_State - 1;
            end if;

            Tree.Set_Augmented (Token.Tree_Index, Base_Token_Class_Access (Temp));
         end;
      end if;
   end Lexer_To_Augmented;

   overriding
   procedure Insert_Token
     (Data  : in out Parse_Data_Type;
      Tree  : in out Syntax_Trees.Tree'Class;
      Token : in     Valid_Node_Index)
   is
      use Valid_Node_Index_Arrays;

      Before_Index : constant Token_Index := Tree.Before (Token);
      Before_Aug : Aug_Token_Var_Ref renames Get_Aug_Token_Var (Data, Tree, Before_Index);

      --  Set data that allows using Token when computing indent.

      Indent_Line : constant Line_Number_Type :=
        (if Before_Aug.First
         then Before_Aug.Line
         else Invalid_Line_Number);

      --  Set for Insert_After False; see below for True.
      New_Aug : constant Augmented_Token_Access := new Augmented_Token'
        (ID                          => Tree.ID (Token),
         Tree_Index                  => Token,
         Byte_Region                 => (First | Last => Before_Aug.Byte_Region.First),
         Line                        => Before_Aug.Line,
         Column                      => Before_Aug.Column,
         Char_Region                 => (First | Last => Before_Aug.Char_Region.First),
         Deleted                     => False,
         First                       => Before_Aug.First,
         Paren_State                 => Before_Aug.Paren_State,
         First_Terminals_Index       => Invalid_Token_Index,
         Last_Terminals_Index        => Invalid_Token_Index,
         First_Indent_Line           => Indent_Line,
         Last_Indent_Line            => Indent_Line,
         First_Trailing_Comment_Line => Invalid_Line_Number,
         Last_Trailing_Comment_Line  => Invalid_Line_Number,
         Non_Grammar                 => Non_Grammar_Token_Arrays.Empty_Vector,
         Inserted_Before             => Valid_Node_Index_Arrays.Empty_Vector);

      Prev_Terminal : constant Node_Index := Tree.Prev_Terminal (Token);
      --  Invalid_Node_Index if Token is inserted before first grammar token

      Insert_After : Boolean := False;
   begin
      Tree.Set_Augmented (Token, Base_Token_Class_Access (New_Aug));

      Append (Before_Aug.Inserted_Before, Token);

      if Prev_Terminal /= Invalid_Node_Index and Before_Aug.First then
         declare
            use all type Ada.Containers.Count_Type;
            use all type Ada.Text_IO.Count;

            --  See test/ada_mode-interactive_2.adb, "Typing ..."; three tests.
            --
            --  When typing new code, we want a new blank line to be indented as
            --  if the code was there already. To accomplish that, we put the
            --  inserted tokens at the end of the line before the Before token;
            --  that will be after the non-grammar on the previous terminal.
            --
            --  Compare to test/ada_mode-recover_20.adb. There we are not typing
            --  new code, but there is a blank line; the right paren is placed at
            --  the end of the blank line, causing the comment to be indented.

            Prev_Aug : Aug_Token_Var_Ref renames Get_Aug_Token_Var (Tree, Prev_Terminal);

            --  Prev_Aug.Non_Grammar must have at least one New_Line, since
            --  Before_Aug.First is True. The whitespace after the New_Line is not
            --  given a token.
            --
            --  If the first two tokens in Prev_Non_Grammar are both New_Lines,
            --  there is a blank line after the code line (and before any
            --  comments); assume that is the edit point.
            Insert_On_Blank_Line : constant Boolean := Prev_Aug.Non_Grammar.Length >= 2 and then
              (Prev_Aug.Non_Grammar (Prev_Aug.Non_Grammar.First_Index).ID = Data.Descriptor.New_Line_ID and
                 Prev_Aug.Non_Grammar (Prev_Aug.Non_Grammar.First_Index + 1).ID = Data.Descriptor.New_Line_ID);

            --  In Ada, 'end' is Insert_After except when Insert_On_Blank_Line is
            --  True (see test/ada_mode-interactive_2.adb Record_1), so Insert_After
            --  needs Insert_On_Blank_Line.
         begin
            Insert_After := Parse_Data_Type'Class (Data).Insert_After (Tree, Token, Insert_On_Blank_Line);

            if Insert_After then
               if Insert_On_Blank_Line then
                  declare
                     Prev_Non_Grammar : constant Non_Grammar_Token :=
                       Prev_Aug.Non_Grammar (Prev_Aug.Non_Grammar.First_Index + 1);
                     --  The newline nominally after the inserted token.
                  begin
                     New_Aug.Byte_Region := (First | Last => Prev_Non_Grammar.Byte_Region.Last - 1);
                     New_Aug.Char_Region := (First | Last => Prev_Non_Grammar.Char_Region.Last - 1);

                     New_Aug.First  := True;
                     New_Aug.Line   := Prev_Non_Grammar.Line;
                     New_Aug.Column := Prev_Aug.Column + Ada.Text_IO.Count (Length (New_Aug.Char_Region)) - 1;

                     New_Aug.First_Indent_Line := Prev_Non_Grammar.Line;
                     New_Aug.Last_Indent_Line  := Prev_Non_Grammar.Line;

                     for I in Prev_Aug.Non_Grammar.First_Index + 1 .. Prev_Aug.Non_Grammar.Last_Index loop
                        New_Aug.Non_Grammar.Append (Prev_Aug.Non_Grammar (I));
                     end loop;

                     Prev_Aug.Non_Grammar.Set_First_Last
                       (Prev_Aug.Non_Grammar.First_Index, Prev_Aug.Non_Grammar.First_Index);
                  end;
               else
                  New_Aug.Byte_Region := (First | Last => Prev_Aug.Byte_Region.Last);
                  New_Aug.Char_Region := (First | Last => Prev_Aug.Char_Region.Last);

                  New_Aug.First  := False;
                  New_Aug.Line   := Prev_Aug.Line;
                  New_Aug.Column := Prev_Aug.Column + Ada.Text_IO.Count (Length (Prev_Aug.Char_Region)) - 1;

                  New_Aug.First_Indent_Line := Invalid_Line_Number;
                  New_Aug.Last_Indent_Line  := Invalid_Line_Number;

                  New_Aug.Non_Grammar  := Prev_Aug.Non_Grammar;
                  Prev_Aug.Non_Grammar := Non_Grammar_Token_Arrays.Empty_Vector;

               end if;

               New_Aug.First_Trailing_Comment_Line := Prev_Aug.First_Trailing_Comment_Line;
               New_Aug.Last_Trailing_Comment_Line  := Prev_Aug.Last_Trailing_Comment_Line;

               Prev_Aug.First_Trailing_Comment_Line := Invalid_Line_Number;
               Prev_Aug.Last_Trailing_Comment_Line  := Invalid_Line_Number;
            end if;
         end;
      end if;

      if New_Aug.First and not Insert_After then
         Before_Aug.First             := False;
         Before_Aug.First_Indent_Line := Invalid_Line_Number;
         Before_Aug.Last_Indent_Line  := Invalid_Line_Number;
      end if;

      if New_Aug.ID = Data.Left_Paren_ID then
         Adjust_Paren_State (Data, Tree, Before_Index, New_Aug.Line + 1, +1);

      elsif New_Aug.ID = Data.Right_Paren_ID then
         Adjust_Paren_State (Data, Tree, Before_Index, New_Aug.Line + 1, -1);
      end if;
   end Insert_Token;

   overriding
   procedure Delete_Token
     (Data                : in out Parse_Data_Type;
      Tree                : in out Syntax_Trees.Tree'Class;
      Deleted_Token_Index : in     WisiToken.Token_Index)
   is
      use all type Ada.Containers.Count_Type;
      Deleted_Token    : Augmented_Token renames Get_Aug_Token_Var (Data, Tree, Deleted_Token_Index);
      Prev_Token_Index : Base_Token_Index := Deleted_Token_Index - 1;
      Next_Token_Index : Base_Token_Index := Deleted_Token_Index + 1;
   begin
      if Deleted_Token.Deleted then
         --  This can happen if error recovery screws up.
         if WisiToken.Trace_Action > WisiToken.Detail then
            Ada.Text_IO.Put_Line (";; delete token again; ignored " & Image (Deleted_Token, Data.Descriptor.all));
         end if;
         return;
      end if;
      if WisiToken.Trace_Action > WisiToken.Detail then
         Ada.Text_IO.Put_Line (";; delete token " & Image (Deleted_Token, Data.Descriptor.all));
      end if;

      Deleted_Token.Deleted := True;

      if Deleted_Token.Non_Grammar.Length > 0 then
         --  Move Non_Grammar to previous non-deleted token

         loop
            exit when Prev_Token_Index = Base_Token_Index'First;
            exit when Get_Aug_Token_Const (Data, Tree, Prev_Token_Index).Deleted = False;
            Prev_Token_Index := Prev_Token_Index - 1;
         end loop;

         if Prev_Token_Index = Base_Token_Index'First then
            Deleted_Token.Non_Grammar (Deleted_Token.Non_Grammar.First_Index).First := Deleted_Token.First;
            Data.Leading_Non_Grammar.Append (Deleted_Token.Non_Grammar);
         else
            declare
               Prev_Token : Augmented_Token renames Get_Aug_Token_Var (Data, Tree, Prev_Token_Index);
            begin
               Prev_Token.Non_Grammar.Append (Deleted_Token.Non_Grammar);

               if Deleted_Token.First_Trailing_Comment_Line /= Invalid_Line_Number then
                  if Prev_Token.First_Trailing_Comment_Line = Invalid_Line_Number then
                     Prev_Token.First_Trailing_Comment_Line := Deleted_Token.First_Trailing_Comment_Line;
                  end if;
                  Prev_Token.Last_Trailing_Comment_Line  := Deleted_Token.Last_Trailing_Comment_Line;
               end if;
            end;
         end if;
      end if;

      --  Data.Terminals.Last_Index is Wisi_EOI; it is never deleted
      loop
         exit when Get_Aug_Token_Const (Data, Tree, Next_Token_Index).Deleted = False;
         Next_Token_Index := Next_Token_Index + 1;
         exit when Next_Token_Index = Data.Terminals.Last_Index;
      end loop;

      if Deleted_Token.First and
        (Next_Token_Index = Data.Terminals.Last_Index or else
           Get_Aug_Token_Const (Data, Tree, Next_Token_Index).Line > Deleted_Token.Line)
      then
         --  Deleted_Token.Line is now blank; add to previous token non
         --  grammar.
         if Prev_Token_Index > Base_Token_Index'First then
            declare
               Prev_Token : Augmented_Token renames Get_Aug_Token_Var (Data, Tree, Prev_Token_Index);
            begin
               if Prev_Token.First_Trailing_Comment_Line = Invalid_Line_Number then
                  Prev_Token.First_Trailing_Comment_Line := Deleted_Token.Line;
                  Prev_Token.Last_Trailing_Comment_Line  := Deleted_Token.Line;
               else
                  if Prev_Token.First_Trailing_Comment_Line > Deleted_Token.Line then
                     Prev_Token.First_Trailing_Comment_Line := Deleted_Token.Line;
                  end if;
                  if Prev_Token.Last_Trailing_Comment_Line < Deleted_Token.Line then
                     Prev_Token.Last_Trailing_Comment_Line := Deleted_Token.Line;
                  end if;
               end if;
            end;
         end if;
      end if;

      if Deleted_Token.First and Next_Token_Index < Data.Terminals.Last_Index then
         declare
            Next_Token : Augmented_Token renames Get_Aug_Token_Var (Data, Tree, Next_Token_Index);
         begin
            if not Next_Token.First then
               Next_Token.First             := True;
               Next_Token.First_Indent_Line := Deleted_Token.First_Indent_Line;
               Next_Token.Last_Indent_Line  := Deleted_Token.Last_Indent_Line;
            end if;
         end;
      end if;

      if Deleted_Token.ID = Data.Left_Paren_ID then
         Adjust_Paren_State (Data, Tree, Deleted_Token_Index + 1, Deleted_Token.Line + 1, -1);

      elsif Deleted_Token.ID = Data.Right_Paren_ID then
         Adjust_Paren_State (Data, Tree, Deleted_Token_Index + 1, Deleted_Token.Line + 1, +1);

      end if;
   end Delete_Token;

   overriding
   procedure Reduce
     (Data    : in out Parse_Data_Type;
      Tree    : in out Syntax_Trees.Tree'Class;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array)
   is
      Aug_Nonterm : constant Augmented_Token_Access := new Augmented_Token'
        (ID          => Tree.ID (Nonterm),
         Byte_Region => Tree.Byte_Region (Nonterm),
         others      => <>);

      Trailing_Comment_Done : Boolean := False;
   begin
      Tree.Set_Augmented (Nonterm, Base_Token_Class_Access (Aug_Nonterm));

      for I in reverse Tokens'Range loop
         --  'reverse' to find token containing trailing comments; last
         --  non-empty token.
         declare
            Aug_Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (I));
         begin

            if Data.Post_Parse_Action = Indent then
               if Aug_Token.First_Terminals_Index /= Invalid_Token_Index then
                  Aug_Nonterm.First_Terminals_Index := Aug_Token.First_Terminals_Index;
               end if;

               if Aug_Nonterm.Last_Terminals_Index = Invalid_Token_Index then
                  Aug_Nonterm.Last_Terminals_Index := Aug_Token.Last_Terminals_Index;
               end if;

               Aug_Nonterm.First := Aug_Nonterm.First or Aug_Token.First;

               if Aug_Token.First_Indent_Line /= Invalid_Line_Number then
                  Aug_Nonterm.First_Indent_Line := Aug_Token.First_Indent_Line;
               elsif Trailing_Comment_Done and Aug_Token.First_Trailing_Comment_Line /= Invalid_Line_Number then
                  Aug_Nonterm.First_Indent_Line := Aug_Token.First_Trailing_Comment_Line;
               end if;

               if Aug_Nonterm.Last_Indent_Line = Invalid_Line_Number then
                  if Trailing_Comment_Done and Aug_Token.Last_Trailing_Comment_Line /= Invalid_Line_Number then
                     Aug_Nonterm.Last_Indent_Line := Aug_Token.Last_Trailing_Comment_Line;
                  elsif Aug_Token.Last_Indent_Line /= Invalid_Line_Number then
                     Aug_Nonterm.Last_Indent_Line := Aug_Token.Last_Indent_Line;
                  end if;
               end if;

               if not Trailing_Comment_Done then
                  Aug_Nonterm.First_Trailing_Comment_Line := Aug_Token.First_Trailing_Comment_Line;
                  Aug_Nonterm.Last_Trailing_Comment_Line  := Aug_Token.Last_Trailing_Comment_Line;
                  Trailing_Comment_Done := True;
               end if;

            end if; --  Compute_Indent

            if Aug_Token.Line /= Invalid_Line_Number then
               Aug_Nonterm.Line   := Aug_Token.Line;
               Aug_Nonterm.Column := Aug_Token.Column;
            end if;

            if Aug_Nonterm.Char_Region.First > Aug_Token.Char_Region.First then
               Aug_Nonterm.Char_Region.First := Aug_Token.Char_Region.First;
            end if;

            if Aug_Nonterm.Char_Region.Last < Aug_Token.Char_Region.Last then
               Aug_Nonterm.Char_Region.Last := Aug_Token.Char_Region.Last;
            end if;

            Aug_Nonterm.Paren_State := Aug_Token.Paren_State;
         end;
      end loop;
   end Reduce;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      Params  : in     Statement_Param_Array)
   is
      Nonterm_Tok        : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Nonterm);
      First_Item         : Boolean                := True;
      Start_Set          : Boolean                := False;
      Override_Start_Set : Boolean                := False;
      Containing_Pos     : Nil_Buffer_Pos         := Nil;
   begin
      for Pair of Params loop
         if not (Pair.Index in Tokens'Range) then
            raise Fatal_Error with Error_Message
              (File_Name => Data.Lexer.File_Name,
               Line      => Nonterm_Tok.Line,
               Column    => Nonterm_Tok.Column,
               Message   => "wisi-statement-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) &
                 " token index" & SAL.Peek_Type'Image (Pair.Index) &
                 " not in tokens range (" & SAL.Peek_Type'Image (Tokens'First) & " .." &
                 SAL.Peek_Type'Image (Tokens'Last) & "); bad grammar action.");

         elsif Tree.Byte_Region (Tokens (Pair.Index)) /= Null_Buffer_Region then
            declare
               use all type WisiToken.Syntax_Trees.Node_Label;
               Token  : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1
                 (Tree,
                  (if Pair.Class = Statement_End and then
                     Tree.Label (Tokens (Pair.Index)) = WisiToken.Syntax_Trees.Nonterm
                   then Tree.Last_Terminal (Tokens (Pair.Index))
                   else Tokens (Pair.Index)));

               Cache_Pos : constant Buffer_Pos         := Token.Char_Region.First;
               Cursor    : Navigate_Cache_Trees.Cursor := Navigate_Cache_Trees.Find
                 (Data.Navigate_Caches.Iterate, Cache_Pos,
                  Direction => Navigate_Cache_Trees.Unknown);
            begin
               if Navigate_Cache_Trees.Has_Element (Cursor) then
                  declare
                     Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                  begin
                     if Pair.Class = Statement_Start then
                        if Start_Set then
                           Cache.Class := Motion;
                        else
                           Cache.Class := Statement_Start;
                           Start_Set   := True;
                        end if;
                     elsif Override_Start_Set then
                        Cache.Class := Statement_Start;
                        Start_Set   := True;
                     else
                        Cache.Class := Pair.Class;
                     end if;
                     Cache.Statement_ID   := Tree.ID (Nonterm);
                     Cache.Containing_Pos := Containing_Pos;
                  end;
               else
                  Cursor := Data.Navigate_Caches.Insert
                    ((Pos            => Cache_Pos,
                      Statement_ID   => Tree.ID (Nonterm),
                      ID             => Token.ID,
                      Length         => Length (Token.Char_Region),
                      Class          => (if Override_Start_Set then Statement_Start else Pair.Class),
                      Containing_Pos => Containing_Pos,
                      others         => Nil));
               end if;

               Data.End_Positions.Append (Cursor);

               if First_Item then
                  First_Item := False;
                  if Override_Start_Set or Pair.Class = Statement_Start then
                     Override_Start_Set := False;
                     Containing_Pos     := (True, Token.Char_Region.First);

                     --  Set containing on all contained caches
                     declare
                        use Navigate_Cache_Trees;
                        Iterator : constant Navigate_Cache_Trees.Iterator := Data.Navigate_Caches.Iterate;
                        Cursor   : Navigate_Cache_Trees.Cursor            := Find_In_Range
                          (Iterator, Ascending, Nonterm_Tok.Char_Region.First + 1, -- don't set containing on start
                           Nonterm_Tok.Char_Region.Last);
                     begin
                        loop
                           exit when not Has_Element (Cursor);
                           declare
                              Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                           begin
                              if not Cache.Containing_Pos.Set then
                                 Cache.Containing_Pos := Containing_Pos;
                              end if;
                              exit when Nonterm_Tok.Char_Region.Last < Cache.Pos + 1;
                           end;
                           Cursor := Iterator.Next (Cursor);
                        end loop;
                     end;
                  end if;
               end if;

               if Pair.Class = Statement_End and Containing_Pos.Set then
                  Set_End (Data, Containing_Pos.Item, Cache_Pos);
               end if;
            end;

         else
            --  Token.Byte_Region is null
            if First_Item and Pair.Class = Statement_Start then
               Override_Start_Set := True;
            end if;
         end if;
      end loop;
   end Statement_Action;

   procedure Name_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Name    : in     WisiToken.Positive_Index_Type)
   is begin
      if not (Name in Tokens'Range) then
         declare
            Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (Tokens'First));
         begin
            raise Fatal_Error with Error_Message
              (File_Name => Data.Lexer.File_Name,
               Line      => Token.Line,
               Column    => Token.Column,
               Message   => "wisi-name-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) & " name (" &
                 Trimmed_Image (Name) & ") not in Tokens range (" & SAL.Peek_Type'Image (Tokens'First) & " .." &
                 SAL.Peek_Type'Image (Tokens'Last) & "); bad grammar action.");
         end;
      end if;

      if Tree.Is_Virtual (Tokens (Name)) then
         --  Virtual tokens have the same Char_Region as the token they are
         --  inserted before (for indent purposes), which leads to Name_Action
         --  appearing to be applied twice. test/ada_mode-fatal_error_1.adb.
         --  They also don't appear in the actual buffer, so setting a face or
         --  completing on them is pointless.
         return;
      end if;

      declare
         use Name_Cache_Trees;
         Name_Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (Name));
         Cursor     : constant Name_Cache_Trees.Cursor := Find
           (Data.Name_Caches.Iterate, Name_Token.Char_Region.First,
            Direction => Name_Cache_Trees.Unknown);
      begin
         if Name_Token.Char_Region = Null_Buffer_Region then
            return;
         elsif Has_Element (Cursor) then
            raise Fatal_Error with Error_Message
              (File_Name            => Data.Lexer.File_Name,
               Line                 => Name_Token.Line,
               Column               => Name_Token.Column,
               Message              => Tree.Image
                 (Tokens (Name), Data.Descriptor.all,
                  Node_Numbers      => WisiToken.Trace_Action > Extra,
                  Include_RHS_Index => WisiToken.Trace_Action > Extra)
                 & ": wisi-name-action: name set twice.");
         else
            if Trace_Action > Detail then
               Ada.Text_IO.Put_Line
                 ("Name_Action " & Tree.Image
                    (Nonterm, Data.Descriptor.all,
                     Node_Numbers      => WisiToken.Trace_Action > Extra,
                     Include_RHS_Index => WisiToken.Trace_Action > Extra) & " " & Tree.Image
                       (Tokens (Name), Data.Descriptor.all,
                        Node_Numbers      => WisiToken.Trace_Action > Extra,
                        Include_RHS_Index => WisiToken.Trace_Action > Extra));
            end if;

            Data.Name_Caches.Insert (Name_Token.Char_Region);
         end if;
      end;
   end Name_Action;

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      Params  : in     Motion_Param_Array)
   is
      use Navigate_Cache_Trees;

      Start          : Nil_Buffer_Pos    := (Set => False);
      Iter           : constant Iterator := Data.Navigate_Caches.Iterate;
      Prev_Cache_Cur : Cursor;
      Cache_Cur      : Cursor;
   begin
      if WisiToken.Trace_Action > Outline then
         Ada.Text_IO.Put_Line
           ("Motion_Action " & Image (Tree.ID (Nonterm), Data.Descriptor.all) & " " &
              Image (Tree.Byte_Region (Nonterm)));
      end if;
      for Param of Params loop
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
            declare
               use all type WisiToken.Syntax_Trees.Node_Label;
               Token  : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (Param.Index));
               Region : constant Buffer_Region := Token.Char_Region;
               Skip   : Boolean                := False;
            begin
               if not Start.Set then
                  Start := (True, Region.First);
               end if;

               case Tree.Label (Tokens (Param.Index)) is
               when Shared_Terminal =>
                  Cache_Cur := Find (Iter, Region.First);
               when Virtual_Terminal | Virtual_Identifier =>
                  return;

               when Syntax_Trees.Nonterm =>
                  if Param.ID = Invalid_Token_ID then
                     Cache_Cur := Find (Iter, Region.First);

                  else
                     Skip      := True;
                     Cache_Cur := Find_In_Range (Iter, Ascending, Region.First, Region.Last);
                     loop
                        exit when not Has_Element (Cache_Cur);
                        if Data.Navigate_Caches (Cache_Cur).Pos > Region.Last then
                           Cache_Cur := No_Element;
                           exit;

                        elsif Data.Navigate_Caches (Cache_Cur).ID = Param.ID and
                          not Data.Navigate_Caches (Cache_Cur).Prev_Pos.Set
                        then
                           Skip := False;
                           exit;
                        end if;

                        Cache_Cur := Next (Iter, Cache_Cur);
                     end loop;
                  end if;
               end case;

               if not Skip then
                  if not Has_Element (Cache_Cur) then
                     raise Fatal_Error with Error_Message
                       (File_Name => Data.Lexer.File_Name,
                        Line      => Token.Line,
                        Column    => Token.Column,
                        Message   => "wisi-motion-action: token " &
                          WisiToken.Image (Token.ID, Data.Descriptor.all) &
                          " has no cache; add to statement-action for " &
                          Trimmed_Image (Tree.Production_ID (Nonterm)) & ".");
                  end if;

                  if Has_Element (Prev_Cache_Cur) then
                     declare
                        Cache      : Navigate_Cache_Type renames Data.Navigate_Caches (Cache_Cur);
                        Prev_Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Prev_Cache_Cur);
                     begin
                        if not Cache.Prev_Pos.Set then
                           Cache.Prev_Pos := (True, Prev_Cache.Pos);
                           if WisiToken.Trace_Action > Detail then
                              Ada.Text_IO.Put_Line ("   " & Cache.Pos'Image & " prev to " & Cache.Prev_Pos.Item'Image);
                           end if;
                        end if;

                        if not Prev_Cache.Next_Pos.Set then
                           Prev_Cache.Next_Pos := (True, Cache.Pos);
                           if WisiToken.Trace_Action > Detail then
                              Ada.Text_IO.Put_Line
                                ("   " & Prev_Cache.Pos'Image & " next to " & Prev_Cache.Next_Pos.Item'Image);
                           end if;
                        end if;
                     end;
                  end if;

                  loop
                     --  Set Prev_Cache_Cur to last motion cache in nonterm chain
                     exit when not Data.Navigate_Caches (Cache_Cur).Next_Pos.Set;

                     Cache_Cur := Find (Iter, Data.Navigate_Caches (Cache_Cur).Next_Pos.Item);
                     pragma Assert (Has_Element (Cache_Cur)); --  otherwise there's a bug in this subprogram.

                  end loop;
                  Prev_Cache_Cur := Cache_Cur;
               end if;
            end;
         end if;
      end loop;
   end Motion_Action;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      use Face_Cache_Trees;

      Iter       : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur  : Cursor;
      Suffix_Cur : Cursor;
   begin
      for Param of Params loop
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
            declare
               Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (Param.Index));
            begin
               Cache_Cur := Find (Iter, Token.Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                  begin
                     case Cache.Class is
                     when Prefix =>
                        Cache.Face := (True, Param.Prefix_Face);

                        --  Check for suffix
                        Suffix_Cur := Next (Iter, Cache_Cur);
                        if Has_Element (Suffix_Cur) then
                           declare
                              Suf_Cache : Face_Cache_Type renames Data.Face_Caches (Suffix_Cur);
                           begin
                              if Suffix = Suf_Cache.Class and
                                Inside (Suf_Cache.Char_Region.First, Token.Char_Region)
                              then
                                 Suf_Cache.Face := (True, Param.Suffix_Face);
                              end if;
                           end;
                        end if;

                     when Suffix =>
                        Cache.Face := (True, Param.Suffix_Face);
                     end case;
                  end;
               else
                  Data.Face_Caches.Insert ((Token.Char_Region, Suffix, (True, Param.Suffix_Face)));
               end if;
            end;
         end if;
      end loop;
   end Face_Apply_Action;

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
            declare
               Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (Param.Index));
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token.Char_Region.Last;
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                  begin
                     case Cache.Class is
                     when Prefix =>
                        Cache.Face := (True, Param.Prefix_Face);

                     when Suffix =>
                        Cache.Face := (True, Param.Suffix_Face);
                     end case;
                  end;
                  Cache_Cur := Next (Iter, Cache_Cur);
               end loop;
            end;
         end if;
      end loop;
   end Face_Apply_List_Action;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      Params  : in     Face_Mark_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
            declare
               Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (Param.Index));
            begin
               Cache_Cur := Find (Iter, Token.Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                     Other_Cur : Cursor := Find_In_Range
                       (Iter, Ascending, Cache.Char_Region.Last + 1, Token.Char_Region.Last);
                     Temp : Cursor;
                  begin
                     loop
                        exit when not Has_Element (Other_Cur) or else
                          Data.Face_Caches (Other_Cur).Char_Region.First > Token.Char_Region.Last;
                        Temp := Other_Cur;
                        Other_Cur := Next (Iter, Other_Cur);
                        Delete (Data.Face_Caches, Temp);
                     end loop;

                     Cache.Class            := Param.Class;
                     Cache.Char_Region.Last := Token.Char_Region.Last;
                  end;
               else
                  Data.Face_Caches.Insert ((Token.Char_Region, Param.Class, (Set => False)));
               end if;
            end;
         end if;
      end loop;
   end Face_Mark_Action;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      Params  : in     Face_Remove_Param_Array)
   is
      pragma Unreferenced (Nonterm);
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
      Temp      : Cursor;
   begin
      for I of Params loop
         if Tree.Byte_Region (Tokens (I)) /= Null_Buffer_Region then
            declare
               Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tokens (I));
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token.Char_Region.Last;
                  Temp := Cache_Cur;
                  Cache_Cur := Next (Iter, Cache_Cur);
                  Delete (Data.Face_Caches, Temp);
               end loop;
            end;
         end if;
      end loop;
   end Face_Remove_Action;

   function "+" (Item : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector do
         Result.Append (Item);
      end return;
   end "+";

   function "&" (List : in Indent_Arg_Arrays.Vector; Item : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector := List do
         Result.Append (Item);
      end return;
   end "&";

   function "&" (Left, Right : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   function Image (Item : in Simple_Indent_Param) return String
   is begin
      return "(" & Simple_Indent_Param_Label'Image (Item.Label) &
        (case Item.Label is
         when None => "",
         when Int => Integer'Image (Item.Int_Delta),
         when Anchored_Label => Positive_Index_Type'Image (Item.Anchored_Index) & "," &
              Integer'Image (Item.Anchored_Delta),
         when Language => "<language_function>") & ")";
   end Image;

   function Image (Item : in Indent_Param) return String
   is begin
      return "(" & Indent_Param_Label'Image (Item.Label) & ", " &
        (case Item.Label is
         when Simple => Image (Item.Param),
         when Hanging_Label =>
            Image (Item.Hanging_Delta_1) & ", "  & Image (Item.Hanging_Delta_2) & ")");
   end Image;

   function Image (Item : in Indent_Pair) return String
   is begin
      return "(" & Image (Item.Code_Delta) &
        (if Item.Comment_Present
         then ", " & Image (Item.Comment_Delta)
         else "") & ")";
   end Image;

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      Params  : in     Indent_Param_Array)
   is begin
      if Trace_Action > Outline then
         Ada.Text_IO.Put_Line (";; indent_action_0: " & Tree.Image (Nonterm, Data.Descriptor.all));
      end if;

      for I in Tokens'Range loop
         if (Tree.Is_Virtual_Terminal (Tokens (I)) or
               Tree.Byte_Region (Tokens (I)) /= Null_Buffer_Region) and
           I in Params'Range -- in some translated EBNF, not every token has an indent param
         then
            declare
               use all type SAL.Base_Peek_Type;
               Tree_Token        : constant Valid_Node_Index := Tokens (I);
               Token             : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tree_Token);
               Pair              : Indent_Pair renames Params (I);
               Code_Delta        : Delta_Type;
               Comment_Param     : Indent_Param;
               Comment_Param_Set : Boolean                                := False;
               Comment_Delta     : Delta_Type;
            begin
               if Trace_Action > Detail then
                  Ada.Text_IO.Put_Line
                    (";; indent_action_0 a: " & Tree.Image (Tree_Token, Data.Descriptor.all) & ": " & Image (Pair));
               end if;

               if Token.First_Indent_Line /= Invalid_Line_Number then
                  Code_Delta := Indent_Compute_Delta
                    (Data, Tree, Tokens, Pair.Code_Delta, Tree_Token, Indenting_Comment => False);

                  Indent_Token_1 (Data, Tree, Token, Code_Delta, Indenting_Comment => False);
               end if;

               if Token.First_Trailing_Comment_Line /= Invalid_Line_Number then
                  if Pair.Comment_Present then
                     Comment_Param     := Pair.Comment_Delta;
                     Comment_Param_Set := True;

                  elsif I < Tokens'Last then
                     Comment_Param     := Params (I + 1).Code_Delta;
                     Comment_Param_Set := True;

                  end if;

                  if Comment_Param_Set then
                     Comment_Delta := Indent_Compute_Delta
                       (Data, Tree, Tokens, Comment_Param, Tree_Token, Indenting_Comment => True);

                     Indent_Token_1 (Data, Tree, Token, Comment_Delta, Indenting_Comment => True);
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Indent_Action_0;

   procedure Indent_Action_1
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Valid_Node_Index;
      Tokens  : in     Valid_Node_Index_Array;
      N       : in     Positive_Index_Type;
      Params  : in     Indent_Param_Array)
   is
      use all type Syntax_Trees.Node_Label;
   begin
      for I in Tokens'First .. N loop
         declare
            Aug : Aug_Token_Const_Ref renames Wisi.Get_Aug_Token_Const_1 (Tree, Tokens (I));
         begin
            if Tree.Label (Tokens (I)) /= Virtual_Terminal and then Aug.First then
               Indent_Action_0 (Data, Tree, Nonterm, Tokens, Params);
               return;
            end if;
         end;
      end loop;
   end Indent_Action_1;

   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Option            : in     Boolean;
      Accumulate        : in     Boolean)
     return Delta_Type
   is
      Indenting_Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tree_Indenting);
   begin
      if Indenting_Comment then
         return Indent_Compute_Delta
           (Data, Tree, Tokens, (Simple, Delta_1), Tree_Indenting, Indenting_Comment);
      else
         return
           (Hanging,
            Hanging_First_Line  => Indenting_Token.Line,
            Hanging_Paren_State => Indenting_Token.Paren_State,
            Hanging_Delta_1     => Indent_Compute_Delta
              (Data, Tree, Tokens, (Simple, Delta_1), Tree_Indenting, Indenting_Comment).Simple_Delta,
            Hanging_Delta_2     =>
              (if (not Option) or
                 Indenting_Token.Line = Indenting_Token.First_Indent_Line -- first token in tok is first on line
               then Indent_Compute_Delta
                 (Data, Tree, Tokens, (Simple, Delta_2), Tree_Indenting, Indenting_Comment).Simple_Delta
               else Indent_Compute_Delta
                 (Data, Tree, Tokens, (Simple, Delta_1), Tree_Indenting, Indenting_Comment).Simple_Delta),
            Hanging_Accumulate => Accumulate);
      end if;
   end Indent_Hanging_1;

   procedure Put_Language_Action
     (Data    : in Parse_Data_Type;
      Content : in String)
   is
      pragma Unreferenced (Data);
   begin
      Ada.Text_IO.Put_Line ("[" & Language_Action_Code & Content & "]");
   end Put_Language_Action;

   procedure Put (Data : in out Parse_Data_Type; Parser : in Parse.Base_Parser'Class)
   is
      use all type Ada.Containers.Count_Type;

      Last_Term : constant Node_Index := Parser.Tree.Last_Terminal (Parser.Tree.Root);

      function Get_Last_Char_Pos return Buffer_Pos
      is begin

         if Last_Term = Invalid_Node_Index then
            --  All comments, or empty
            if Data.Leading_Non_Grammar.Length > 0 then
               return Data.Leading_Non_Grammar (Data.Leading_Non_Grammar.Last_Index).Char_Region.Last;
            else
               return Buffer_Pos'First;
            end if;
         else
            declare
               Aug : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Parser.Tree, Last_Term);
            begin
               if Aug.Non_Grammar.Length = 0 then
                  return Aug.Char_Region.Last;
               else
                  return Aug.Non_Grammar (Aug.Non_Grammar.Last_Index).Char_Region.Last;
               end if;
            end;
         end if;
      end Get_Last_Char_Pos;

      Last_Char_Pos : constant Buffer_Pos := Get_Last_Char_Pos;

      function Get_Last_Line return Line_Number_Type
      is begin
         for I in Data.Line_Begin_Char_Pos.First_Index .. Data.Line_Begin_Char_Pos.Last_Index loop
            if Data.Line_Begin_Char_Pos (I) = Invalid_Buffer_Pos then
               raise SAL.Programmer_Error with "line_begin_pos" & Line_Number_Type'Image (I) & " invalid";
            end if;
            if Data.Line_Begin_Char_Pos (I) > Last_Char_Pos then
               if I > Line_Number_Type'First then
                  return I - 1;
               else
                  return I;
               end if;
            end if;
         end loop;
         return Data.Line_Begin_Char_Pos.Last_Index;
      end Get_Last_Line;

   begin
      if Trace_Action > Outline then
         Ada.Text_IO.Put_Line
           (";; last_char_pos:" & Buffer_Pos'Image (Last_Char_Pos + 1) &
              " last_line:" & Line_Number_Type'Image (Get_Last_Line));
      end if;

      --  +1 to match Emacs region
      Ada.Text_IO.Put_Line ('[' & End_Code & Buffer_Pos'Image (Last_Char_Pos + 1) & ']');

      case Data.Post_Parse_Action is
      when Navigate =>
         for Cache of Data.Navigate_Caches loop
            Put (Cache);
         end loop;
         for Cache of Data.Name_Caches loop
            Put (Cache);
         end loop;

      when Face =>
         for Cache of Data.Face_Caches loop
            Put (Cache);
         end loop;

      when Indent =>

         Resolve_Anchors (Data);

         if Trace_Action > Outline then
            Ada.Text_IO.Put_Line (";; indent leading non_grammar");
         end if;
         for Token of Data.Leading_Non_Grammar loop
            if Token.First then
               Put (Token.Line, (Int, Data.Begin_Indent));
            end if;
         end loop;

         --  It may be that not all lines in Data.Indents were parsed.
         if Trace_Action > Outline then
            Ada.Text_IO.Put_Line (";; indent grammar");
         end if;
         for I in Data.Indents.First_Index .. Get_Last_Line loop
            Put (I, Data.Indents (I));
         end loop;
      end case;
   end Put;

   procedure Put (Lexer_Errors : in Lexer.Error_Lists.List)
   is begin
      for Item of Lexer_Errors loop
         Ada.Text_IO.Put_Line
           ('[' & Lexer_Error_Code & Buffer_Pos'Image (Item.Char_Pos) &
              " ""lexer error" &
              (if Item.Recover_Char (1) = ASCII.NUL
               then """"
               elsif Item.Recover_Char (1) = '"'
               then """ ?\"""
               else """ ?" & Item.Recover_Char (1)) &
              "]");
         if Item.Recover_Char (2) /= ASCII.NUL then
            raise SAL.Programmer_Error with "lexer error with non-ascii or multiple repair char";
         end if;
      end loop;
   end Put;

   procedure Put
     (Data         : in Parse_Data_Type;
      Lexer_Errors : in Lexer.Error_Lists.List;
      Parse_Errors : in Parse.LR.Parse_Error_Lists.List;
      Tree         : in Syntax_Trees.Tree)
   is
      use all type SAL.Base_Peek_Type;
      use Ada.Text_IO;
      use Semantic_Checks;

      function Safe_Pos (Node : in Valid_Node_Index) return Buffer_Pos
      is
         --  Return a reasonable position for the error at Node.
         --
         --  In a successful parse with error recovery, Node is a terminal with
         --  an augmented token in Data.Terminals, so that is the first
         --  choice.
         --
         --  If this is an error due to a bad recovery, Node may be a virtual
         --  token, with no position information, so we try to get information
         --  from its parent.
         use Syntax_Trees;

         N : Node_Index := Node;
      begin
         loop
            if Tree.Label (N) /= Virtual_Terminal then
               declare
                  Ref : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, N);
               begin
                  if Ref.Char_Region /= Null_Buffer_Region then
                     return Ref.Element.Char_Region.First;
                  end if;

               end;
            end if;
            N := Tree.Parent (N);
            exit when N = Invalid_Node_Index;
         end loop;
         return Buffer_Pos'First;
      end Safe_Pos;

      function Safe_Pos (Token : in Recover_Token) return Buffer_Pos
      is begin
         if Token.Name /= Null_Buffer_Region then
            return Token.Name.First;

         elsif Token.Byte_Region = Null_Buffer_Region then
            return Buffer_Pos'First;

         else
            return Token.Byte_Region.First;
         end if;
      end Safe_Pos;

   begin
      Put (Lexer_Errors);

      for Item of Parse_Errors loop
         case Item.Label is
         when Parse.LR.Action =>
            Put_Line
              ('[' & Parser_Error_Code & Buffer_Pos'Image (Safe_Pos (Item.Error_Token)) &
                 " ""syntax error: expecting " & Image (Item.Expecting, Data.Descriptor.all) &
                 ", found '" & Image (Tree.ID (Item.Error_Token), Data.Descriptor.all) & "'""]");

         when Parse.LR.Check =>
            Put_Line
              ('[' & Check_Error_Code & Integer'Image
                 (Semantic_Checks.Check_Status_Label'Pos (Item.Check_Status.Label)) &
                 (case Item.Check_Status.Label is
                  when Ok => "",
                  when Error =>
                     Buffer_Pos'Image (Safe_Pos (Item.Check_Status.Begin_Name)) &
                       Buffer_Pos'Image (Safe_Pos (Item.Check_Status.End_Name)) &
                       " ""block name error""]"));

         when Parse.LR.Message =>
            Put_Line
              ('[' & Parser_Error_Code & Buffer_Pos'Image (Buffer_Pos'First) &
                 " """ & (-Item.Msg) & """]");
         end case;

         if Item.Recover.Stack.Depth > 0 then
            Put (Item.Recover, Data, Tree);
         end if;
      end loop;
   end Put;

   procedure Put_Error (Data : in Parse_Data_Type; Line_Number : in Line_Number_Type; Message : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("(error """ & Error_Message (Data.Lexer.File_Name, Line_Number, 0, Message) & """)");
   end Put_Error;

   ----------
   --  Spec visible private subprograms, alphabetical

   function Image (Item : in Simple_Delta_Type) return String
   is begin
      return "(" & Simple_Delta_Labels'Image (Item.Label) &
        (case Item.Label is
         when None => "",
         when Int => Integer'Image (Item.Int_Delta),
         when Anchored => Integer'Image (Item.Anchored_ID) & Integer'Image (Item.Anchored_Delta) & " " &
              Boolean'Image (Item.Anchored_Accumulate))
        & ")";
   end Image;

   function Image (Item : in Delta_Type) return String
   is begin
      return "(" & Delta_Labels'Image (Item.Label) &
        (case Item.Label is
         when Simple => " " & Image (Item.Simple_Delta),
         when Hanging => Line_Number_Type'Image (Item.Hanging_First_Line) & Integer'Image (Item.Hanging_Paren_State) &
              " " & Image (Item.Hanging_Delta_1) & " " & Image (Item.Hanging_Delta_2) & " " &
              Boolean'Image (Item.Hanging_Accumulate)) & ")";
   end Image;

   function Current_Indent_Offset
     (Data         : in Parse_Data_Type;
      Anchor_Token : in Augmented_Token'Class;
      Offset       : in Integer)
     return Integer
   is begin
      return Offset + Integer (Anchor_Token.Char_Region.First - Data.Line_Begin_Char_Pos (Anchor_Token.Line));
   end Current_Indent_Offset;

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type
   is begin
      return
        (if Indenting_Comment then
           (if Token.First_Trailing_Comment_Line = Invalid_Line_Number
            then Token.Line
            else Token.First_Trailing_Comment_Line)
         else
           (if Token.First_Indent_Line = Invalid_Line_Number
            then Token.Line
            else Token.First_Indent_Line));
   end First_Line;

   function Get_Aug_Token_Const_1
     (Tree       : in Syntax_Trees.Tree'Class;
      Tree_Index : in Valid_Node_Index)
     return Aug_Token_Const_Ref
   is begin
      return To_Aug_Token_Const_Ref (Tree.Augmented (Tree_Index));
   end Get_Aug_Token_Const_1;

   function Get_Aug_Token_Const
     (Data  : in Parse_Data_Type;
      Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Token_Index)
     return Aug_Token_Const_Ref
   is begin
      return Get_Aug_Token_Const_1 (Tree, Data.Terminals.all (Token).Tree_Index);
   end Get_Aug_Token_Const;

   function Get_Aug_Token_Var
     (Tree       : in Syntax_Trees.Tree'Class;
      Tree_Index : in Valid_Node_Index)
     return Aug_Token_Var_Ref
   is begin
      return To_Aug_Token_Var_Ref (Tree.Augmented (Tree_Index));
   end Get_Aug_Token_Var;

   function Get_Aug_Token_Var
     (Data  : in Parse_Data_Type;
      Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Token_Index)
     return Aug_Token_Var_Ref
   is begin
      return Get_Aug_Token_Var (Tree, Data.Terminals.all (Token).Tree_Index);
   end Get_Aug_Token_Var;

   function Get_Text
     (Data       : in Parse_Data_Type;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Tree_Index : in WisiToken.Valid_Node_Index)
     return String
   is
      use all type Syntax_Trees.Node_Label;
   begin
      case Tree.Label (Tree_Index) is
      when Shared_Terminal | Nonterm =>
         return Data.Lexer.Buffer_Text (Tree.Byte_Region (Tree_Index));

      when Virtual_Terminal | Virtual_Identifier =>
         raise SAL.Programmer_Error;

      end case;
   end Get_Text;

   function Elisp_Escape_Quotes (Item : in String) return String
   is
      Result : String (Item'First .. Item'First + Item'Length * 2);
      Last   : Integer := Item'First - 1;
   begin
      for I in Item'Range loop
         if Item (I) = '"' then
            Last := Last + 1;
            Result (Last) := '\';
         end if;
         Last := Last + 1;
         Result (Last) := Item (I);
      end loop;
      return Result (Result'First .. Last);
   end Elisp_Escape_Quotes;

   overriding
   function Image
     (Item       : in Augmented_Token;
      Descriptor : in WisiToken.Descriptor)
     return String
   is
      ID_Image : constant String := Image (Item.ID, Descriptor);
   begin
      if Item.Line /= Invalid_Line_Number then
         return "(" & ID_Image &
           Line_Number_Type'Image (Item.Line) & ":" & Trimmed_Image (Integer (Item.Column)) & ")";

      elsif Item.Char_Region = Null_Buffer_Region then
         if Item.Byte_Region = Null_Buffer_Region then
            return "(" & ID_Image & ")";
         else
            return "(" & ID_Image & ", " & Image (Item.Byte_Region) & ")";
         end if;
      else
         return "(" & ID_Image & ", " & Image (Item.Char_Region) & ")";
      end if;
   end Image;

   function Indent_Anchored_2
     (Data        : in out Parse_Data_Type;
      Anchor_Line : in     Line_Number_Type;
      Last_Line   : in     Line_Number_Type;
      Offset      : in     Integer;
      Accumulate  : in     Boolean)
     return Delta_Type
   is
      --  Return an anchored delta
      use Anchor_ID_Vectors;
      --  We can't use a Reference here, because the Element in reference
      --  types is constrained (as are all allocated objects of access
      --  types; AARM 4.8 (6/3)), and we may need to change the Label.
      Indent    : Indent_Type      := Data.Indents (Anchor_Line);
      Anchor_ID : constant Integer := 1 + Max_Anchor_ID (Data, Anchor_Line, Last_Line);
   begin
      Data.Max_Anchor_ID := Integer'Max (Data.Max_Anchor_ID, Anchor_ID);

      case Indent.Label is
      when Not_Set =>
         Indent := (Anchor_Nil, To_Vector (Anchor_ID, 1));

         if Trace_Action > Extra then
            Ada.Text_IO.Put_Line
              (";; indent_anchored: " & Line_Number_Type'Image (Anchor_Line) & " => " & Image (Indent));
         end if;

      when Int =>
         Indent := (Anchor_Int, To_Vector (Anchor_ID, 1), Indent.Int_Indent);

         if Trace_Action > Extra then
            Ada.Text_IO.Put_Line
              (";; indent_anchored: " & Line_Number_Type'Image (Anchor_Line) & " => " & Image (Indent));
         end if;

      when Anchor_Nil =>
         Indent.Anchor_Nil_IDs := Anchor_ID & Indent.Anchor_Nil_IDs;

      when Anchor_Int =>
         Indent.Anchor_Int_IDs := Anchor_ID & Indent.Anchor_Int_IDs;

      when Anchored =>
         Indent := (Anchor_Anchored, To_Vector (Anchor_ID, 1), Indent.Anchored_ID, Indent.Anchored_Delta);

      when Anchor_Anchored =>
         Indent.Anchor_Anchored_IDs := Anchor_ID & Indent.Anchor_Anchored_IDs;
      end case;

      Data.Indents.Replace_Element (Anchor_Line, Indent);

      return (Simple, (Anchored, Anchor_ID, Offset, Accumulate));
   end Indent_Anchored_2;

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Tokens            : in     Valid_Node_Index_Array;
      Param             : in     Indent_Param;
      Tree_Indenting    : in     Valid_Node_Index;
      Indenting_Comment : in     Boolean)
     return Delta_Type
   is
      Indenting_Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1 (Tree, Tree_Indenting);
   begin
      --  Evaluate wisi-anchored*, wisi-hanging*.
      case Param.Label is
      when Simple =>
         case Param.Param.Label is
         when None =>
            return (Simple, (Label => None));

         when Int =>
            return (Simple, (Int, Param.Param.Int_Delta));

         when Anchored_Label =>
            declare
               Anchor_Token : Aug_Token_Const_Ref renames Get_Aug_Token_Const_1
                 (Tree, Tokens (Param.Param.Anchored_Index));
            begin
               case Anchored_Label'(Param.Param.Label) is
               when Anchored_0 =>
                  --  [2] wisi-anchored
                  return Indent_Anchored_2
                    (Data,
                     Anchor_Line => Anchor_Token.Line,
                     Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                     Offset      => Current_Indent_Offset (Data, Anchor_Token, Param.Param.Anchored_Delta),
                     Accumulate  => True);

               when Anchored_1 =>
                  --  [2] wisi-anchored%
                  return Indent_Anchored_2
                    (Data,
                     Anchor_Line => Anchor_Token.Line,
                     Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                     Offset      => Paren_In_Anchor_Line (Data, Tree, Anchor_Token, Param.Param.Anchored_Delta),
                     Accumulate  => True);

               when Anchored_2 =>
                  --  [2] wisi-anchored%-
                  return Indent_Anchored_2
                    (Data,
                     Anchor_Line => Anchor_Token.Line,
                     Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                     Offset      => Paren_In_Anchor_Line (Data, Tree, Anchor_Token, Param.Param.Anchored_Delta),
                     Accumulate  => False);

               when Anchored_3 =>
                  --  [2] wisi-anchored*
                  if Indenting_Token.First then
                     return Indent_Anchored_2
                       (Data,
                        Anchor_Line => Anchor_Token.Line,
                        Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                        Offset      => Current_Indent_Offset (Data, Anchor_Token, Param.Param.Anchored_Delta),
                        Accumulate  => True);

                  else
                     return Null_Delta;
                  end if;

               when Anchored_4 =>
                  --  [2] wisi-anchored*-
                  return Indent_Anchored_2
                    (Data,
                     Anchor_Line => Anchor_Token.Line,
                     Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                     Offset      => Current_Indent_Offset (Data, Anchor_Token, Param.Param.Anchored_Delta),
                     Accumulate  => False);

               end case;
            end;

         when Language =>
            return Param.Param.Function_Ptr
              (Data, Tree, Tokens, Tree_Indenting, Indenting_Comment, Param.Param.Args);
         end case;

      when Hanging_Label =>
         case Hanging_Label'(Param.Label) is
         when Hanging_0 => -- wisi-hanging
            return Indent_Hanging_1
              (Data, Tree, Tokens, Tree_Indenting, Indenting_Comment, Param.Hanging_Delta_1,
               Param.Hanging_Delta_2,
               Option => False, Accumulate => True);
         when Hanging_1 => -- wisi-hanging-
            return Indent_Hanging_1
              (Data, Tree, Tokens, Tree_Indenting, Indenting_Comment, Param.Hanging_Delta_1,
               Param.Hanging_Delta_2,
               Option => False, Accumulate => False);
         when Hanging_2 => -- wisi-hanging%
            return Indent_Hanging_1
              (Data, Tree, Tokens, Tree_Indenting, Indenting_Comment, Param.Hanging_Delta_1,
               Param.Hanging_Delta_2,
               Option => True, Accumulate => True);
         when Hanging_3 => -- wisi-hanging%-
            return Indent_Hanging_1
              (Data, Tree, Tokens, Tree_Indenting, Indenting_Comment, Param.Hanging_Delta_1,
               Param.Hanging_Delta_2,
               Option => True, Accumulate => False);
         end case;
      end case;
   end Indent_Compute_Delta;

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Indenting_Token   : in     Augmented_Token'Class;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Boolean)
   is
      --  Aplly Delta_Indent to Indenting_Token
      First_Line : constant Line_Number_Type := Indenting_Token.First_Line (Indenting_Comment);
      Last_Line  : constant Line_Number_Type := Indenting_Token.Last_Line (Indenting_Comment);
   begin
      if Trace_Action > Detail then
         Ada.Text_IO.Put_Line
           (";; indent_token_1: " & Indenting_Token.Image (Data.Descriptor.all) & " " & Image (Delta_Indent) &
              Line_Number_Type'Image (First_Line) & " .." & Line_Number_Type'Image (Last_Line) &
              (if Indenting_Comment then " comment" else ""));
      end if;

      for Line in First_Line .. Last_Line loop
         if Data.Indent_Comment_Col_0 then
            declare
               use all type Ada.Text_IO.Count;

               function Containing_Token return Base_Token_Index
               is
                  --  Return token index of terminal containing non_grammer on Line;
                  --  Invalid_Token_Index if none.
                  I : Line_Number_Type := Line;
                  J : Base_Token_Index;
               begin
                  if Line < Data.Line_Begin_Token.First_Index then
                     --  Line is before first grammar token; Leading_Non_Grammar checked
                     --  below.
                     return Invalid_Token_Index;
                  end if;

                  loop
                     exit when Data.Line_Begin_Token.all (I) /= Base_Token_Arrays.No_Index;
                     --  No_Index means Line is in a multi-line token, which could be a block comment.
                     I := I - 1;
                  end loop;

                  J := Data.Line_Begin_Token.all (I);
                  declare
                     Aug : Augmented_Token renames Get_Aug_Token_Const (Data, Tree, J);
                  begin
                     if Line in Aug.First_Trailing_Comment_Line .. Aug.Last_Trailing_Comment_Line then
                        return J;
                     else
                        return Invalid_Token_Index;
                     end if;
                  end;
               end Containing_Token;

               Indent     : Boolean                   := True;
               Containing : constant Base_Token_Index := Containing_Token;
            begin
               if Line < Data.Line_Begin_Token.First_Index then
                  --  Line is before the first grammar token. We may be doing a partial
                  --  parse where the initial indent is non-zero, so we still have to
                  --  check for column 0.
                  for Tok of Data.Leading_Non_Grammar loop
                     if Tok.Line = Line and then
                       Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                       Tok.Column = 0
                     then
                        Indent := False;
                        exit;
                     end if;
                  end loop;

               elsif Containing /= Invalid_Token_Index then
                  for Tok of Get_Aug_Token_Const (Data, Tree, Containing).Non_Grammar loop
                     if Tok.Line = Line and then
                       Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                       Tok.Column = 0
                     then
                        Indent := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if Indent then
                  Indent_Line (Data, Line, Delta_Indent);
               else
                  Indent_Line (Data, Line, (Simple, (Int, 0)));
               end if;
            end;
         else
            Indent_Line (Data, Line, Delta_Indent);
         end if;
      end loop;
   end Indent_Token_1;

   function Last_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type
   is begin
      return
        (if Indenting_Comment then
           (if Token.Last_Trailing_Comment_Line = Invalid_Line_Number
            then Token.Line
            else Token.Last_Trailing_Comment_Line)
         else
           (if Token.Last_Indent_Line = Invalid_Line_Number
            then Token.Line
            else Token.Last_Indent_Line));
   end Last_Line;

end Wisi;
