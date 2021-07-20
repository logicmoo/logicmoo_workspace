--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Emacs_Wisi_Common_Parse; use Emacs_Wisi_Common_Parse;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with System.Storage_Elements;
with WisiToken.Lexer;
with WisiToken.Parse.Packrat;
with WisiToken.Text_IO_Trace;
procedure Gen_Emacs_Wisi_Parse_Packrat
is
   use WisiToken; -- "+", "-" Unbounded_string

   Trace      : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);
   Parser     : WisiToken.Parse.Packrat.Parser;
   Parse_Data : aliased Parse_Data_Type (Parser.Line_Begin_Token'Access);

begin
   Create_Parser (Parser, Trace'Unrestricted_Access, Parse_Data'Unchecked_Access);

   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 0 =>
         null;

      when others =>
         Usage (Name);
         raise Programmer_Error with "invalid option count: " & Integer'Image (Argument_Count);
      end case;
   end;

   Put_Line (Name & " " & Version & ", protocol version " & Protocol_Version);

   --  Read commands and tokens from standard_input via GNAT.OS_Lib,
   --  send results to standard_output.
   loop
      Put (Prompt); Flush;
      declare
         Command_Length : constant Integer := Get_Command_Length;
         Command_Line   : aliased String (1 .. Command_Length);
         Last           : Integer;

         function Match (Target : in String) return Boolean
         is begin
            Last := Command_Line'First + Target'Length - 1;
            return Last <= Command_Line'Last and then Command_Line (Command_Line'First .. Last) = Target;
         end Match;
      begin
         Read_Input (Command_Line'Address, Command_Length);

         Put_Line (";; " & Command_Line);

         if Match ("parse") then
            --  Args: see Usage
            --  Input: <source text>
            --  Response:
            --  [response elisp vector]...
            --  [elisp error form]...
            --  prompt
            declare
               use Wisi;
               Cl_Params : constant Command_Line_Params := Get_Cl_Params (Command_Line, Last);
               Buffer    : Ada.Strings.Unbounded.String_Access;

               procedure Clean_Up
               is begin
                  Parser.Lexer.Discard_Rest_Of_Input;
                  Parser.Put_Errors (-Cl_Param.Source_File_Name);
                  Ada.Strings.Unbounded.Free (Buffer);
               end Clean_Up;

            begin
               --  Computing Line_Count in elisp allows parsing in parallel with
               --  sending source text.

               Trace_Parse    := Cl_Params.Parse_Verbosity;
               Trace_McKenzie := Cl_Params.McKenzie_Verbosity;
               Trace_Action   := Cl_Params.Action_Verbosity;
               Debug_Mode     := Cl_Params.Debug_Mode;

               Parse_Data.Initialize
                 (Post_Parse_Action => Cl_Params.Post_Parse_Action,
                  Descriptor        => Descriptor'Access,
                  Source_File_Name  => -Cl_Params.Source_File_Name,
                  Line_Count        => Cl_Params.Line_Count,
                  Params            => Command_Line (Last + 2 .. Command_Line'Last));

               Buffer := new String (1 .. Cl_Params.Byte_Count);
               Read_Input (Buffer (1)'Address, Cl_Params.Byte_Count);

               Parser.Lexer.Reset_With_String_Access (Buffer);
               Parser.Parse;
               Parser.Execute_Actions;
               Put (Parse_Data);
               Clean_Up;

            exception
            when Syntax_Error =>
               Clean_Up;
               Put_Line ("(parse_error)");

            when E : Parse_Error =>
               Clean_Up;
               Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Message (E) & """)");

            when E : Fatal_Error =>
               Clean_Up;
               Put_Line ("(error """ & Ada.Exceptions.Exception_Message (E) & """)");
            end;

         elsif Match ("noop") then
            --  Args: <source byte count>
            --  Input: <source text>
            --  Response: prompt
            declare
               Byte_Count  : constant Integer                             := Get_Integer (Command_Line, Last);
               Buffer      : constant Ada.Strings.Unbounded.String_Access := new String (1 .. Byte_Count);
               Token       : Base_Token;
               Lexer_Error : Boolean;
               pragma Unreferenced (Lexer_Error);
            begin
               Token.ID := Invalid_Token_ID;
               Read_Input (Buffer (1)'Address, Byte_Count);

               Parser.Lexer.Reset_With_String_Access (Buffer);
               loop
                  exit when Token.ID = Parser.Trace.Descriptor.EOF_ID;
                  Lexer_Error := Parser.Lexer.Find_Next (Token);
               end loop;
            exception
            when Syntax_Error =>
               Parser.Lexer.Discard_Rest_Of_Input;
            end;

         elsif Match ("quit") then
            exit;

         else
            Put_Line ("(error ""bad command: '" & Command_Line & "'"")");
         end if;
      exception
      when E : Protocol_Error =>
         --  don't exit the loop; allow debugging bad elisp
         Put_Line ("(error ""protocol error "": " & Ada.Exceptions.Exception_Message (E) & """)");
      end;
   end loop;
exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   New_Line (2);
   Put_Line
     ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E) & """)");
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Emacs_Wisi_Parse_Packrat;
