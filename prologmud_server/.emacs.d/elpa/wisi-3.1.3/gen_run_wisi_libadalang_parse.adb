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
with Ada.Real_Time;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Wisi.Libadalang;
with WisiToken.Text_IO_Trace;
procedure Gen_Run_Wisi_Libadalang_Parse
is
   use WisiToken; -- Token_ID, "+", "-" Unbounded_string

   Trace      : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Unrestricted_Access);
   Parser     : Wisi.Libadalang.Parser;
   Parse_Data : aliased Parse_Data_Type (Parser.Line_Begin_Token'Access);

   procedure Put_Usage
   is begin
      Put_Line ("usage: <file_name> <parse_action> [options]");
      Put_Line ("parse_action: {Navigate | Face | Indent}");
      Put_Line ("options:");
      Put_Line ("--verbosity n m : parse, action");
      Put_Line ("--lang_params <language-specific params>");
      Put_Line ("--repeat_count n : repeat parse count times, for profiling; default 1");
      New_Line;
   end Put_Usage;

   Source_File_Name  : Ada.Strings.Unbounded.Unbounded_String;
   Post_Parse_Action : Wisi.Post_Parse_Action_Type;
   Lang_Params       : Ada.Strings.Unbounded.Unbounded_String;

   Repeat_Count : Integer := 1;
   Arg          : Integer;
   Start        : Ada.Real_Time.Time;

begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count < 1 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Source_File_Name  := +Ada.Command_Line.Argument (1);
      Post_Parse_Action := Wisi.Post_Parse_Action_Type'Value (Ada.Command_Line.Argument (2));
      Arg               := 3;

      loop
         exit when Arg > Argument_Count;

         if Argument (Arg) = "--lang_params" then
            Lang_Params := +Argument (Arg + 1);
            Arg := Arg + 2;

         elsif Argument (Arg) = "--repeat_count" then
            Repeat_Count := Integer'Value (Argument (Arg + 1));
            Arg := Arg + 2;

         elsif Argument (Arg) = "--verbosity" then
            WisiToken.Trace_Parse  := Integer'Value (Argument (Arg + 1));
            WisiToken.Trace_Action := Integer'Value (Argument (Arg + 2));
            Arg                    := Arg + 3;

         else
            Put_Line ("unrecognized option: '" & Argument (Arg) & "'");
            Put_Usage;
            return;
         end if;
      end loop;
   end;

   Parser.Trace            := Trace'Unrestricted_Access;
   Parser.Lexer            := new Wisi.Libadalang.Lexer (Trace'Unrestricted_Access);
   Parser.User_Data        := Parse_Data'Unrestricted_Access;
   Parser.Source_File_Name := Source_File_Name;

   Parser.Tree.Initialize (Shared_Tree => Parser.Base_Tree'Unrestricted_Access, Flush => True);

   Parse_Data.Initialize
     (Post_Parse_Action => Post_Parse_Action,
      Descriptor        => Descriptor'Unrestricted_Access,
      Source_File_Name  => -Source_File_Name,
      Line_Count        => 1, --  FIXME: fix wisi_runtime to not need this!
      Params            => -Lang_Params);

   if Repeat_Count > 1 then
      Start := Ada.Real_Time.Clock;
   end if;

   for I in 1 .. Repeat_Count loop
      declare
         procedure Clean_Up
         is begin
            if I = 1 then
               null;
               --  FIXME: Errors!
               --  Parse_Data.Put
               --    (Parser.Lexer.Errors,
               --     Parser.Parsers.First.State_Ref.Errors,
               --     Parser.Parsers.First.State_Ref.Tree);
            end if;
         end Clean_Up;

      begin
         Parse_Data.Reset;
         Parser.Parse;
         Parser.Execute_Actions;

         if Repeat_Count = 1 then
            Parse_Data.Put;

            --  FIXME: put errors via parse_data.put
            if Parser.Any_Errors then
               Parser.Put_Errors;
            end if;
            --  Parse_Data.Put
            --       (Parser.Lexer.Errors,
            --        Parser.Parsers.First.State_Ref.Errors,
            --        Parser.Parsers.First.State_Ref.Tree);
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
end Gen_Run_Wisi_Libadalang_Parse;
