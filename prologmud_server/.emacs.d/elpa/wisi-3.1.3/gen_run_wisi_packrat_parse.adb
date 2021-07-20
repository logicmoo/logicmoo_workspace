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
with Ada.IO_Exceptions;
with Ada.Real_Time;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.Lexer;
with WisiToken.Text_IO_Trace;
procedure Gen_Run_Wisi_Parse_Packrat
is
   use WisiToken; -- Token_ID, "+", "-" Unbounded_string

   Trace      : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Access);
   Parser     : WisiToken.Parse.Packrat.Parser;
   Parse_Data : aliased Parse_Data_Type (Parser.Line_Begin_Token'Access);

   procedure Put_Usage
   is begin
      Put_Line ("usage: " & Name & "_wisi_parse <file_name> <parse_action> [options]");
      Put_Line ("parse_action: {Navigate | Face | Indent}");
      Put_Line ("options:");
      Put_Line ("--verbosity n m l:");
      Put_Line ("   n: parser; m: mckenzie; l: action");
      Put_Line ("   0 - only report parse errors");
      Put_Line ("   1 - shows spawn/terminate parallel parsers, error recovery enter/exit");
      Put_Line ("   2 - add each parser cycle, error recovery enqueue/check");
      Put_Line ("   3 - parse stack in each cycle, error recovery parse actions");
      Put_Line ("   4 - add lexer debug");
      Put_Line ("--lang_params <language-specific params>");
      Put_Line ("--lexer_only : only run lexer, for profiling");
      Put_Line ("--repeat_count n : repeat parse count times, for profiling; default 1");
      Put_Line ("--pause : when repeating, prompt for <enter> after each parse; allows seeing memory leaks");
      New_Line;
   end Put_Usage;

   Source_File_Name  : Ada.Strings.Unbounded.Unbounded_String;
   Post_Parse_Action : WisiToken.Wisi_Runtime.Post_Parse_Action_Type;

   Line_Count   : WisiToken.Line_Number_Type := 1;
   Lexer_Only   : Boolean                    := False;
   Repeat_Count : Integer                    := 1;
   Pause        : Boolean                    := False;
   Arg          : Integer;
   Lang_Params  : Ada.Strings.Unbounded.Unbounded_String;
   Start        : Ada.Real_Time.Time;
begin
   Create_Parser (Parser, Trace'Unrestricted_Access, Parse_Data'Unchecked_Access);

   declare
      use Ada.Command_Line;
   begin
      if Argument_Count < 1 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Source_File_Name  := +Ada.Command_Line.Argument (1);
      Post_Parse_Action := WisiToken.Wisi_Runtime.Post_Parse_Action_Type'Value (Ada.Command_Line.Argument (2));
      Arg               := 3;

      loop
         exit when Arg > Argument_Count;

         if Argument (Arg) = "--verbosity" then
            WisiToken.Trace_Parse    := Integer'Value (Argument (Arg + 1));
            WisiToken.Trace_McKenzie := Integer'Value (Argument (Arg + 2));
            WisiToken.Trace_Action   := Integer'Value (Argument (Arg + 3));
            Arg                      := Arg + 4;

         elsif Argument (Arg) = "--lang_params" then
            Lang_Params := +Argument (Arg + 1);
            Arg := Arg + 2;

         elsif Argument (Arg) = "--lexer_only" then
            Lexer_Only := True;
            Arg := Arg + 1;

         elsif Argument (Arg) = "--pause" then
            Pause := True;
            Arg := Arg + 1;

         elsif Argument (Arg) = "--repeat_count" then
            Repeat_Count := Integer'Value (Argument (Arg + 1));
            Arg := Arg + 2;

         else
            Put_Line ("unrecognized option: '" & Argument (Arg) & "'");
            Put_Usage;
            return;
         end if;
      end loop;
   end;

   --  Do this after setting Trace_Parse so lexer verbosity is set
   begin
      Parser.Lexer.Reset_With_File (-Source_File_Name);
   exception
   when Ada.IO_Exceptions.Name_Error =>
      Put_Line (Standard_Error, "'" & (-Source_File_Name) & "' cannot be opened");
      return;
   end;

   --  See comment in wisi-wisi_runtime.ads for why we still need this.
   declare
      Token : Base_Token;
      Lexer_Error : Boolean;
      pragma Unreferenced (Lexer_Error);
   begin
      loop
         begin
            Lexer_Error := Parser.Lexer.Find_Next (Token);
            exit when Token.ID = Descriptor.EOF_ID;
         exception
         when WisiToken.Syntax_Error =>
            Parser.Lexer.Discard_Rest_Of_Input;
            Parser.Put_Errors (-Source_File_Name);
            Put_Line ("(lexer_error)");
         end;
      end loop;
      Line_Count := Token.Line;
   end;

   if WisiToken.Trace_Action > WisiToken.Outline then
      Put_Line ("line_count:" & Line_Number_Type'Image (Line_Count));
   end if;

   Parse_Data.Initialize
     (Post_Parse_Action => Post_Parse_Action,
      Descriptor        => Descriptor'Access,
      Source_File_Name  => -Source_File_Name,
      Line_Count        => Line_Count,
      Params            => -Lang_Params);

   if Repeat_Count > 1 then
      Start := Ada.Real_Time.Clock;
   end if;

   for I in 1 .. Repeat_Count loop
      declare
         procedure Clean_Up
         is begin
            Parser.Lexer.Discard_Rest_Of_Input;
            if Repeat_Count = 1 then
               Parser.Put_Errors (-Source_File_Name);
            end if;
         end Clean_Up;

      begin
         Parse_Data.Reset;
         Parser.Lexer.Reset;

         if Lexer_Only then
            declare
               Token : Base_Token;
               Lexer_Error : Boolean;
               pragma Unreferenced (Lexer_Error);
            begin
               Parser.Lexer.Reset;
               loop
                  Lexer_Error := Parser.Lexer.Find_Next (Token);
                  exit when Token.ID = Descriptor.EOF_ID;
               end loop;
               --  We don't handle errors here; that was done in the count lines loop
               --  above.
            end;
         else
            Parser.Parse;
            Parser.Execute_Actions;

            if Repeat_Count = 1 then
               Parse_Data.Put;
               Parser.Put_Errors (-Source_File_Name);
            end if;
         end if;
      exception
      when WisiToken.Syntax_Error =>
         Clean_Up;
         Put_Line ("(parse_error)");

      when E : WisiToken.Parse_Error =>
         Clean_Up;
         Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Message (E) & """)");

      when E : WisiToken.Fatal_Error =>
         Clean_Up;
         Put_Line ("(error """ & Ada.Exceptions.Exception_Message (E) & """)");
      end;

      if Pause then
         Put_Line ("Enter to continue:");
         Flush (Standard_Output);
         declare
            Junk : constant String := Get_Line;
            pragma Unreferenced (Junk);
         begin
            null;
         end;
      end if;
   end loop;

   if Repeat_Count > 1 then
      declare
         use Ada.Real_Time;
         Finish : constant Time := Clock;
      begin
         Put_Line ("Total time:" & Duration'Image (To_Duration (Finish - Start)));
         Put_Line ("per iteration:" & Duration'Image (To_Duration ((Finish - Start) / Repeat_Count)));
      end;
   end if;

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   New_Line (2);
   Put_Line
     ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E) & """)");
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Run_Wisi_Parse_Packrat;
