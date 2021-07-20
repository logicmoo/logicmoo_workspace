--  Abstract :
--
--  Common utilities for Gen_Run_Wisi_*_Parse
--
--  Copyright (C) 2018 - 2019 Free Software Foundation, Inc.
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

with Ada.Strings.Unbounded;
with Wisi;
with WisiToken.Parse.LR.Parser;
package Run_Wisi_Common_Parse is

   Finish : exception;

   procedure Usage (Parser : in out WisiToken.Parse.LR.Parser.Parser);
   --  Puts parameter description to Current_Output.

   type Command_Type is (Parse, Refactor);

   type Command_Line_Params (Command : Command_Type) is record

      Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Lang_Params      : Ada.Strings.Unbounded.Unbounded_String;
      Repeat_Count     : Integer := 1;

      case Command is
      when Parse =>
         Post_Parse_Action : Wisi.Post_Parse_Action_Type;
         Begin_Byte_Pos    : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         End_Byte_Pos      : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Goal_Byte_Pos     : WisiToken.Buffer_Pos       := WisiToken.Invalid_Buffer_Pos;
         Begin_Char_Pos    : WisiToken.Buffer_Pos       := WisiToken.Buffer_Pos'First;
         Begin_Line        : WisiToken.Line_Number_Type := WisiToken.Line_Number_Type'First;
         End_Line          : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
         Begin_Indent      : Integer                    := 0;

      when Refactor =>
         --  We assume the file contains only the one statement/declaration
         --  that needs refactoring.

         Refactor_Action : Positive;
         --  Language-specific

         Edit_Begin : WisiToken.Buffer_Pos;
         --  Source file byte position at start of expression to refactor.
      end case;
   end record;

   function Get_CL_Params (Parser : in out WisiToken.Parse.LR.Parser.Parser) return Command_Line_Params;
   --  For any errors, calls Usage, raises SAL.Parameter_Error.
   --
   --  Handles --help by outputing help, raising Finish.

   procedure Parse_File
     (Parser     : in out WisiToken.Parse.LR.Parser.Parser;
      Parse_Data : in out Wisi.Parse_Data_Type'Class;
      Descriptor : in     WisiToken.Descriptor);
   --  Calls Get_CL_Params, reads in file, parses, does post-parse actions.

end Run_Wisi_Common_Parse;
