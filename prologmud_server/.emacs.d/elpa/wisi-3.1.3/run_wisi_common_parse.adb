--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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
with Ada.Text_IO;
with SAL;
with System.Multiprocessors;
package body Run_Wisi_Common_Parse is

   procedure Usage (Parser : in out WisiToken.Parse.LR.Parser.Parser)
   is
      use all type WisiToken.Parse.LR.Parse_Table_Ptr;
      use Ada.Text_IO;
   begin
      Put_Line ("usage: parse <parse_action> <file_name> [partial parse params] [options]");
      Put_Line ("   or: refactor <refactor_action> <file_name> <edit_begin> [options]");
      Put_Line ("parse_action: {Navigate | Face | Indent}");
      Put_Line ("partial parse params: begin_byte_pos end_byte_pos goal_byte_pos begin_char_pos begin_line" &
                  " end_line begin_indent");
      Put_Line ("options:");
      Put_Line ("--verbosity n m l: (no 'm' for refactor)");
      Put_Line ("   n: parser; m: mckenzie; l: action");
      Put_Line ("   0 - only report parse errors");
      Put_Line ("   1 - shows spawn/terminate parallel parsers, error recovery enter/exit");
      Put_Line ("   2 - add each parser cycle, error recovery enqueue/check");
      Put_Line ("   3 - parse stack in each cycle, error recovery parse actions");
      Put_Line ("   4 - add lexer debug, dump syntax tree");
      Put_Line ("--check_limit n  : set error recover token check limit" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.McKenzie_Param.Check_Limit'Image));
      Put_Line ("--check_delta n  : set error recover delta check limit" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.McKenzie_Param.Check_Delta_Limit'Image));
      Put_Line ("--enqueue_limit n  : set error recover token enqueue limit" &
                  (if Parser.Table = null then ""
                   else "; default" & Parser.Table.McKenzie_Param.Enqueue_Limit'Image));
      Put_Line ("--max_parallel n  : set maximum count of parallel parsers (default" &
                  WisiToken.Parse.LR.Parser.Default_Max_Parallel'Image & ")");
      Put_Line ("--task_count n : worker tasks in error recovery");
      Put_Line ("--disable_recover : disable error recovery; default enabled");
      Put_Line ("--debug_mode : tracebacks from unhandled exceptions; default disabled");
      Put_Line ("--lang_params <language-specific params>");
      Put_Line ("--repeat_count n : repeat parse count times, for profiling; default 1");
      New_Line;
   end Usage;

   function Get_CL_Params (Parser : in out WisiToken.Parse.LR.Parser.Parser) return Command_Line_Params
   is
      use Ada.Command_Line;
      use WisiToken;
      Arg     : Integer := 1;
      Command : Command_Type;
   begin
      if Argument_Count < 1 then
         Usage (Parser);
         Set_Exit_Status (Failure);
         raise Finish;

      elsif Argument (Arg) = "--help" then
         Usage (Parser);
         raise Finish;

      elsif Argument_Count < 2 then
         Usage (Parser);
         Set_Exit_Status (Failure);
         raise Finish;
      end if;

      Command := Command_Type'Value (Ada.Command_Line.Argument (1));

      return Result : Command_Line_Params (Command) do
         Result.Source_File_Name  := +Ada.Command_Line.Argument (3);

         case Command is
         when Parse =>
            Result.Post_Parse_Action := Wisi.Post_Parse_Action_Type'Value (Ada.Command_Line.Argument (2));

            if Argument_Count >= 4 and then Argument (4)(1) /= '-' then
               Result.Begin_Byte_Pos := WisiToken.Buffer_Pos'Value (Argument (4));
               Result.End_Byte_Pos   := WisiToken.Buffer_Pos'Value (Argument (5)) - 1; -- match emacs region
               Result.Goal_Byte_Pos  := WisiToken.Buffer_Pos'Value (Argument (6));
               Result.Begin_Char_Pos := WisiToken.Buffer_Pos'Value (Argument (7));
               Result.Begin_Line     := WisiToken.Line_Number_Type'Value (Argument (8));
               Result.End_Line       := WisiToken.Line_Number_Type'Value (Argument (9));
               Result.Begin_Indent   := Integer'Value (Argument (10));
               Arg                   := 11;
            else
               Result.Begin_Byte_Pos := WisiToken.Invalid_Buffer_Pos;
               Result.End_Byte_Pos   := WisiToken.Invalid_Buffer_Pos;
               Result.Begin_Char_Pos := WisiToken.Buffer_Pos'First;
               Result.Begin_Line     := WisiToken.Line_Number_Type'First;
               Arg                   := 4;
            end if;

         when Refactor =>
            Result.Refactor_Action := Integer'Value (Argument (2));
            Result.Edit_Begin      := WisiToken.Buffer_Pos'Value (Argument (4));
            Arg                    := 5;
         end case;

         loop
            exit when Arg > Argument_Count;

            if Argument (Arg) = "--verbosity" then
               WisiToken.Trace_Parse    := Integer'Value (Argument (Arg + 1));
               case Command is
               when Parse =>
                  WisiToken.Trace_McKenzie := Integer'Value (Argument (Arg + 2));
                  WisiToken.Trace_Action   := Integer'Value (Argument (Arg + 3));
                  Arg                      := Arg + 4;
               when Refactor =>
                  WisiToken.Trace_Action   := Integer'Value (Argument (Arg + 2));
                  Arg                      := Arg + 3;
               end case;

               WisiToken.Debug_Mode := WisiToken.Trace_Parse > Outline or WisiToken.Trace_McKenzie > Outline;

            elsif Argument (Arg) = "--check_limit" then
               Parser.Table.McKenzie_Param.Check_Limit := Token_Index'Value (Argument (Arg + 1));
               Arg := Arg + 2;

            elsif Argument (Arg) = "--check_delta" then
               Parser.Table.McKenzie_Param.Check_Delta_Limit := Integer'Value (Argument (Arg + 1));
               Arg := Arg + 2;

            elsif Argument (Arg) = "--debug_mode" then
               WisiToken.Debug_Mode := True;
               Arg := Arg + 1;

            elsif Argument (Arg) = "--disable_recover" then
               Parser.Enable_McKenzie_Recover := False;
               Arg := Arg + 1;

            elsif Argument (Arg) = "--enqueue_limit" then
               Parser.Table.McKenzie_Param.Enqueue_Limit := Integer'Value (Argument (Arg + 1));
               Arg := Arg + 2;

            elsif Argument (Arg) = "--lang_params" then
               Result.Lang_Params := +Argument (Arg + 1);
               Arg := Arg + 2;

            elsif Argument (Arg) = "--max_parallel" then
               Parser.Max_Parallel := SAL.Base_Peek_Type'Value (Argument (Arg + 1));
               Arg := Arg + 2;

            elsif Argument (Arg) = "--repeat_count" then
               Result.Repeat_Count := Integer'Value (Argument (Arg + 1));
               Arg := Arg + 2;

            elsif Argument (Arg) = "--task_count" then
               Parser.Table.McKenzie_Param.Task_Count := System.Multiprocessors.CPU_Range'Value (Argument (Arg + 1));
               Arg := Arg + 2;

            else
               Ada.Text_IO.Put_Line ("unrecognized option: '" & Argument (Arg) & "'");
               Usage (Parser);
               Set_Exit_Status (Failure);
               raise SAL.Parameter_Error;
            end if;
         end loop;
      end return;
   exception
   when Finish =>
      raise;

   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
      Usage (Parser);
      Set_Exit_Status (Failure);
      raise SAL.Parameter_Error;
   end Get_CL_Params;

   procedure Parse_File
     (Parser     : in out WisiToken.Parse.LR.Parser.Parser;
      Parse_Data : in out Wisi.Parse_Data_Type'Class;
      Descriptor : in     WisiToken.Descriptor)
   is
      use Ada.Text_IO;
      use WisiToken;

      Start     : Ada.Real_Time.Time;
      End_Line  : WisiToken.Line_Number_Type;

      function Image_Augmented (Aug : in Base_Token_Class_Access) return String
      is begin
         --  For Syntax_Trees.Print_Tree
         return Wisi.Image (Aug, Descriptor);
      end Image_Augmented;

   begin
      Parser.Trace.Set_Prefix (";; "); -- so we get the same debug messages as Emacs_Wisi_Common_Parse

      declare
         Cl_Params : constant Command_Line_Params := Get_CL_Params (Parser);
      begin
         begin
            case Cl_Params.Command is
            when Parse =>
               Parser.Lexer.Reset_With_File
                 (-Cl_Params.Source_File_Name, Cl_Params.Begin_Byte_Pos, Cl_Params.End_Byte_Pos,
                  Cl_Params.Begin_Char_Pos, Cl_Params.Begin_Line);
            when Refactor =>
               Parser.Lexer.Reset_With_File (-Cl_Params.Source_File_Name);
            end case;
         exception
         when Ada.IO_Exceptions.Name_Error =>
            Put_Line (Standard_Error, "'" & (-Cl_Params.Source_File_Name) & "' cannot be opened");
            return;
         end;

         --  Parser.Line_Begin_Token First, Last set by Lex_All

         if Cl_Params.Command = Refactor or else Cl_Params.End_Line = Invalid_Line_Number then
            --  User did not provide; run lexer to get end line.
            declare
               Token       : Base_Token;
               Lexer_Error : Boolean;
               pragma Unreferenced (Lexer_Error);
            begin
               loop
                  Lexer_Error := Parser.Lexer.Find_Next (Token);
                  exit when Token.ID = Descriptor.EOI_ID;
               end loop;
               End_Line := Token.Line;
            end;
         else
            End_Line := Cl_Params.End_Line;
         end if;

         Parse_Data.Initialize
           (Post_Parse_Action =>
              (case Cl_Params.Command is
               when Parse    => Cl_Params.Post_Parse_Action,
               when Refactor => Wisi.Navigate),
            Lexer            => Parser.Lexer,
            Descriptor       => Descriptor'Unrestricted_Access,
            Base_Terminals   => Parser.Terminals'Unrestricted_Access,
            Begin_Line       =>
              (case Cl_Params.Command is
               when Parse => Cl_Params.Begin_Line,
               when Refactor => WisiToken.Line_Number_Type'First),
            End_Line         => End_Line,
            Begin_Indent     =>
              (case Cl_Params.Command is
               when Parse    => Cl_Params.Begin_Indent,
               when Refactor => 0),
            Params            => -Cl_Params.Lang_Params);

         if Cl_Params.Repeat_Count > 1 then
            Start := Ada.Real_Time.Clock;
         end if;

         for I in 1 .. Cl_Params.Repeat_Count loop
            declare
               procedure Clean_Up
               is
                  use all type SAL.Base_Peek_Type;
               begin
                  Parser.Lexer.Discard_Rest_Of_Input;
                  if Cl_Params.Repeat_Count = 1 and Parser.Parsers.Count > 0 then
                     Parse_Data.Put
                       (Parser.Lexer.Errors,
                        Parser.Parsers.First.State_Ref.Errors,
                        Parser.Parsers.First.State_Ref.Tree);
                  end if;
               end Clean_Up;

            begin
               Parse_Data.Reset;
               Parser.Lexer.Reset;

               begin
                  Parser.Parse;
               exception
               when WisiToken.Partial_Parse =>
                  null;
               end;

               Parser.Execute_Actions (Image_Augmented'Unrestricted_Access);

               case Cl_Params.Command is
               when Parse =>
                  if Cl_Params.Repeat_Count = 1 then
                     Parse_Data.Put (Parser);
                     Parse_Data.Put
                       (Parser.Lexer.Errors,
                        Parser.Parsers.First.State_Ref.Errors,
                        Parser.Parsers.First.State_Ref.Tree);
                  end if;

               when Refactor =>
                  Parse_Data.Refactor
                    (Parser.Parsers.First_State_Ref.Tree,
                     Cl_Params.Refactor_Action, Cl_Params.Edit_Begin);
               end case;
            exception
            when WisiToken.Syntax_Error =>
               Clean_Up;
               Put_Line ("(parse_error)");

            when E : WisiToken.Parse_Error =>
               Clean_Up;
               Put_Line ("(parse_error """ & Ada.Exceptions.Exception_Name (E) & " " &
                           Ada.Exceptions.Exception_Message (E) & """)");

            when E : others => -- includes Fatal_Error
               Clean_Up;
               Put_Line ("(error """ & Ada.Exceptions.Exception_Name (E) & " " &
                           Ada.Exceptions.Exception_Message (E) & """)");
            end;
         end loop;

         if Cl_Params.Repeat_Count > 1 then
            declare
               use Ada.Real_Time;
               Finish : constant Time := Clock;
            begin
               Put_Line ("Total time:" & Duration'Image (To_Duration (Finish - Start)));
               Put_Line ("per iteration:" & Duration'Image (To_Duration ((Finish - Start) / Cl_Params.Repeat_Count)));
            end;
         end if;
      end;
   exception
   when SAL.Parameter_Error | Finish =>
      --  From Get_CL_Params; already handled.
      null;

   when E : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      New_Line (2);
      Put_Line
        ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
           Ada.Exceptions.Exception_Message (E) & """)");
   end Parse_File;

end Run_Wisi_Common_Parse;
