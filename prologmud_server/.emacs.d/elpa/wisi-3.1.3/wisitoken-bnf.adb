--  Abstract :
--
--  see spec
--
--  Copyright (C) 2012 - 2015, 2017 - 2019 Free Software Foundation, Inc.
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;
package body WisiToken.BNF is

   procedure Add
     (Set   : in out Generate_Set_Access;
      Tuple : in     Generate_Tuple)
   is
      Prev : Generate_Set_Access := Set;
      Last : constant Integer    := (if Prev = null then 1 else Prev.all'Length + 1);
   begin
      Set := new Generate_Set (1 .. Last);
      for I in 1 .. Last - 1 loop
         Set (I) := Prev (I);
      end loop;
      Set (Last) := Tuple;
      Free (Prev);
   end Add;

   function To_Generate_Algorithm (Item : in String) return Generate_Algorithm
   is begin
      for I in Generate_Algorithm loop
         if To_Lower (Generate_Algorithm_Image (I).all) = To_Lower (Item) then
            return I;
         end if;
      end loop;
      raise User_Error with "invalid generate algorithm name: '" & Item & "'";
   end To_Generate_Algorithm;

   function To_Output_Language (Item : in String) return Output_Language
   is begin
      for I in Output_Language loop
         if To_Lower (Output_Language_Image (I).all) = To_Lower (Item) then
            return I;
         end if;
      end loop;
      raise User_Error with "invalid output language name: '" & Item & "'";
   end To_Output_Language;

   function To_Lexer (Item : in String) return Lexer_Type
   is begin
      for I in Valid_Lexer loop
         if Lexer_Image (I).all = To_Lower (Item) then
            return I;
         end if;
      end loop;
      raise User_Error with "invalid lexer name: '" & Item & "'";
   end To_Lexer;

   function Split_Lines (Item : in String) return String_Lists.List
   is
      CR : Character renames ASCII.CR;
      LF : Character renames ASCII.LF;

      Result    : WisiToken.BNF.String_Lists.List;
      I         : Integer   := Item'First;
      First     : Integer   := Item'First;
      Last_Char : Character := ' ';
   begin
      loop
         exit when I > Item'Last;
         if Item (I) = LF then
            Result.Append (Item (First .. I - (if Last_Char = CR then 2 else 1)));
            First := I + 1;

         elsif I = Item'Last then
            Result.Append (Item (First .. I));
         end if;

         Last_Char := Item (I);

         I := I + 1;
      end loop;
      return Result;
   end Split_Lines;

   function Trim (Item : in String_Lists.List; Comment_Start : in String) return String_Lists.List
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Result : String_Lists.List;
      Comment : Integer;

      procedure Maybe_Append (Line : in String)
      is begin
         if Line'Length > 0 then
            Result.Append (Line);
         end if;
      end Maybe_Append;

   begin
      for Line of Item loop
         Comment := Index (Line, Comment_Start, Going => Backward);
         if Comment /= 0 then
            Maybe_Append (Trim (Line (Line'First .. Comment - 1), Both));
         else
            Maybe_Append (Trim (Line, Both));
         end if;
      end loop;
      return Result;
   end Trim;

   procedure Put_Raw_Code
     (Comment_Syntax : in String_2;
      Code           : in String_Lists.List;
      Comment_Only   : in Boolean := False)
   is
      use Ada.Text_IO;
      Real_Comment_Only : Boolean := Comment_Only;
   begin
      for Line of Code loop
         if Line'Length >= 2 and then
           ((Line (Line'First) = Line (Line'First + 1)) and
              Line (Line'First) /= ' ')
         then
            --  The line is a comment.
            Real_Comment_Only := Real_Comment_Only or Line (Line'First .. Line'First + 1) /= Comment_Syntax;

            Put_Line (Comment_Syntax & Line (Line'First + 2 .. Line'Last));

         elsif Comment_Syntax = Elisp_Comment and (Line'Length > 0 and then Line (Line'First) /= '(') then
            null;

         elsif not Comment_Only then
            Put_Line (Line);
         end if;
      end loop;
   end Put_Raw_Code;

   procedure Put_File_Header
     (Comment_Syntax : in String_2;
      Emacs_Mode     : in String         := "";
      Use_Tuple      : in Boolean        := False;
      Tuple          : in Generate_Tuple := (others => <>))
   is
      use Ada.Text_IO;
   begin
      Put_Line (Comment_Syntax & "  generated parser support file." & Emacs_Mode);
      Put_Command_Line  (Comment_Syntax & "  ", Use_Tuple, Tuple);
      Put_Line (Comment_Syntax);
   end Put_File_Header;

   function Is_Present (List : in WisiToken.BNF.String_Pair_Lists.List; Name : in String) return Boolean
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Pair of List loop
         if Pair.Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Is_Present;

   function Value (List : in WisiToken.BNF.String_Pair_Lists.List; Name : in String) return String
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Pair of List loop
         if Pair.Name = Name then
            return -Pair.Value;
         end if;
      end loop;
      raise Not_Found;
   end Value;

   function Is_Present (List : in Elisp_Action_Maps.Map; Name : in String) return Boolean
   is
      use Elisp_Action_Maps;
   begin
      return No_Element /= List.Find (+Name);
   end Is_Present;

   function Count (Tokens : in Token_Lists.List) return Integer
   is
      Result : Integer := 0;
   begin
      for Kind of Tokens loop
         Result := Result + Integer (Kind.Tokens.Length);
      end loop;
      return Result;
   end Count;

   procedure Add_Token
     (Tokens       : in out Token_Lists.List;
      Kind         : in     String;
      Name         : in     String;
      Value        : in     String;
      Repair_Image : in     String := "")
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Token_Kind of Tokens loop
         if Token_Kind.Kind = Kind then
            Token_Kind.Tokens.Append ((+Name, +Value, +Repair_Image));
            return;
         end if;
      end loop;

      --  Kind not found; add it
      declare
         Temp : String_Triple_Lists.List;
      begin
         Temp.Append ((+Name, +Value, +Repair_Image));
         Tokens.Append ((+Kind, Temp));
      end;
   end Add_Token;

   function Is_In (Tokens : in Token_Lists.List; Kind : in String) return Boolean
   is begin
      for Token of Tokens loop
         if -Token.Kind = Kind then
            return True;
         end if;
      end loop;
      return False;
   end Is_In;

   function Is_In
     (Tokens : in Token_Lists.List;
      Kind   : in String;
      Value  : in String)
     return Boolean
   is begin
      for Token of Tokens loop
         if -Token.Kind = Kind then
            for Item of Token.Tokens loop
               if -Item.Value = Value then
                  return True;
               end if;
            end loop;
         end if;
      end loop;
      return False;
   end Is_In;

   function Is_Present (Rules : in Rule_Lists.List; LHS : in String) return Boolean
   is
      use Rule_Lists;

      Found : Boolean := False;

      procedure Process (Position : in Cursor)
      is begin
         if -Rules (Position).Left_Hand_Side = LHS then
            Found := True;
         end if;
      end Process;

   begin
      Rules.Iterate (Process'Access);
      return Found;
   end Is_Present;

   function "+" (List : in String_Lists.List; Item : in String) return String_Lists.List
   is
      Result : String_Lists.List := List;
   begin
      Result.Append (Item);
      return Result;
   end "+";

   function String_To_String_List (Item : in String) return String_Lists.List
   is
      Result : String_Lists.List;
   begin
      Result.Append (Item);
      return Result;
   end String_To_String_List;

   function RHS_To_RHS_List (Item : in RHS_Type) return RHS_Lists.List
   is
      Result : RHS_Lists.List;
   begin
      Result.Append (Item);
      return Result;
   end RHS_To_RHS_List;

   function "+" (List : in RHS_Lists.List; Item : in RHS_Type) return RHS_Lists.List
   is
      Result : RHS_Lists.List := List;
   begin
      Result.Append (Item);
      return Result;
   end "+";

   procedure Put_Command_Line
     (Comment_Prefix : in String;
      Use_Tuple      : in Boolean        := False;
      Tuple          : in Generate_Tuple := (others => <>))
   is
      use Ada.Command_Line;
      use Ada.Text_IO;

      Max_Line_Length : constant := 120;
      Col : Integer := 0;

      procedure Put (Item : in String; Leading_Space : in Boolean)
      is begin
         if Col > 0 and Col + Item'Length + 1 > Max_Line_Length then
            New_Line;
            Col := Comment_Prefix'Length;
            Put (Comment_Prefix);
         else
            if Leading_Space then
               Put (" ");
               Col := Col + 1;
            end if;
         end if;

         Col := Col + Item'Length;
         Put (Item);
      end Put;
   begin
      Put (Comment_Prefix & "command line:", False);
      Put (Ada.Directories.Simple_Name (Command_Name), True);
      if Use_Tuple then
         Put (" --generate " & Generate_Algorithm'Image (Tuple.Gen_Alg) & " " &
                Output_Language_Image (Tuple.Out_Lang).all &
                (if Tuple.Lexer /= None then " " & Lexer_Image (Tuple.Lexer).all else "") &
                (if Tuple.Interface_Kind /= None then " " & Interface_Type'Image (Tuple.Interface_Kind) else "") &
                (if Tuple.Text_Rep then " text_rep" else "") &
                " " & Argument (Argument_Count), --  .wy file
              True);
      else
         for I in 1 .. Argument_Count loop
            Put (Argument (I), True);
         end loop;
      end if;
      New_Line;
   end Put_Command_Line;

end WisiToken.BNF;
