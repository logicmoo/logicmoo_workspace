--  Abstract :
--
--  See spec
--
--  Copyright (C) 2012, 2013, 2015, 2017 - 2019 Free Software Foundation, Inc.
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

with Ada.Text_IO;
with WisiToken.Generate;
package body WisiToken.BNF.Output_Elisp_Common is

   function Find_Elisp_ID (List : in String_Lists.List; Elisp_Name : in String) return Integer
   is
      I : Integer := 0; -- match elisp array
   begin
      for Name of List loop
         if Name = Elisp_Name then
            return I;
         end if;
         I := I + 1;
      end loop;
      raise Not_Found with "unknown elisp name: '" & Elisp_Name & "'";
   end Find_Elisp_ID;

   function Elisp_Name_To_Ada
     (Elisp_Name : in String;
      Append_ID  : in Boolean;
      Trim       : in Integer)
     return String
   is
      Result : String := Elisp_Name (Elisp_Name'First + Trim .. Elisp_Name'Last);
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '_';
            Result (I + 1) := To_Upper (Result (I + 1));
         elsif Result (I) = '_' then
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      if Append_ID then
         return Result & "_ID"; -- Some elisp names may be Ada reserved words;
      else
         return Result;
      end if;
   end Elisp_Name_To_Ada;

   procedure Indent_Keyword_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Keywords         : in     String_Pair_Lists.List;
      Image            : access function (Name : in Ada.Strings.Unbounded.Unbounded_String) return String)
   is
      use Ada.Text_IO;
      use WisiToken.Generate;
   begin
      Indent_Line ("(defconst " & Output_File_Root & "-" & Label & "-keyword-table-raw");
      Indent_Line ("  '(");
      Indent := Indent + 3;
      for Pair of Keywords loop
         Indent_Line ("(" & (-Pair.Value) & " . " & Image (Pair.Name) & ")");
      end loop;
      Indent_Line ("))");
      Indent := Indent - 3;
   end Indent_Keyword_Table;

   procedure Indent_Token_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Tokens           : in     Token_Lists.List;
      Image            : access function (Name : in Ada.Strings.Unbounded.Unbounded_String) return String)
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use WisiToken.Generate;

      function To_Double_Quotes (Item : in String) return String
      is
         Result : String := Item;
      begin
         if Result (Result'First) = ''' then
            Result (Result'First) := '"';
         end if;
         if Result (Result'Last) = ''' then
            Result (Result'Last) := '"';
         end if;
         return Result;
      end To_Double_Quotes;

   begin
      Indent_Line ("(defconst " & Output_File_Root & "-" & Label & "-token-table-raw");
      Indent_Line ("  '(");
      Indent := Indent + 3;
      for Kind of Tokens loop
         if not (-Kind.Kind = "line_comment" or -Kind.Kind = "whitespace") then
            Indent_Line ("(""" & (-Kind.Kind) & """");
            Indent := Indent + 1;
            for Token of Kind.Tokens loop
               if 0 = Length (Token.Value) then
                  Indent_Line ("(" & Image (Token.Name) & ")");
               else
                  if -Kind.Kind = "number" then
                     --  allow for (<token> <number-p> <require>)
                     Indent_Line ("(" & Image (Token.Name) & " " & (-Token.Value) & ")");
                  elsif -Kind.Kind = "symbol" or
                    -Kind.Kind = "string-double" or
                    -Kind.Kind = "string-single"
                  then
                     --  value not used by elisp
                     Indent_Line ("(" & Image (Token.Name) & " . """")");
                  else
                     Indent_Line ("(" & Image (Token.Name) & " . " & To_Double_Quotes (-Token.Value) & ")");
                  end if;
               end if;
            end loop;
            Indent_Line (")");
            Indent := Indent - 1;
         end if;
      end loop;
      Indent_Line ("))");
      Indent := Indent - 3;
   end Indent_Token_Table;

   procedure Indent_Name_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Names            : in     String_Lists.List)
   is
      use Ada.Text_IO;
      use WisiToken.Generate;
   begin
      Indent_Line ("(defconst " & Output_File_Root & "-" & Label);
      Indent_Line ("  [");
      Indent := Indent + 3;
      for Name of Names loop
         Indent_Line (Name);
      end loop;
      Indent_Line ("])");
      Indent := Indent - 3;
   end Indent_Name_Table;

   procedure Indent_Repair_Image
     (Output_File_Root : in String;
      Label            : in String;
      Tokens           : in WisiToken.BNF.Tokens)
   is
      use all type Ada.Text_IO.Count;
      use Ada.Strings.Unbounded;
      use WisiToken.Generate;

      function re2c_To_Elisp (Item : in String) return String
      is
         Result : String (1 .. Item'Length * 2);
         Last : Integer := Result'First - 1;
      begin
         --  Convert re2c case-insensitive string '...' to elisp string "...",
         --  with '"' escaped.
         if Item (Item'First) /= ''' then
            return Item;
         end if;

         for C of Item loop
            if C = ''' then
               Result (Last + 1) := '"';
               Last := Last + 1;
            elsif C = '"' then
               Result (Last + 1) := '\';
               Result (Last + 2) := '"';
               Last := Last + 2;
            else
               Result (Last + 1) := C;
               Last := Last + 1;
            end if;
         end loop;
         return Result (1 .. Last);
      end re2c_To_Elisp;

   begin
      Indent_Line ("(defconst " & Output_File_Root & "-" & Label & "-repair-image");
      Indent_Line ("  '(");
      Indent := Indent + 3;
      for Pair of Tokens.Keywords loop
         Indent_Line ("(" & (-Pair.Name) & " . " & (-Pair.Value) & ")");
      end loop;
      for Kind of Tokens.Tokens loop
         for Token of Kind.Tokens loop
            if Length (Token.Repair_Image) > 0 then
               Indent_Line ("(" & (-Token.Name) & " . " & re2c_To_Elisp (-Token.Repair_Image) & ")");
            else
               Indent_Line ("(" & (-Token.Name) & " . " & (-Token.Value) & ")");
            end if;
         end loop;
      end loop;
      Indent_Line ("))");
      Indent := Indent - 3;
   end Indent_Repair_Image;

end WisiToken.BNF.Output_Elisp_Common;
