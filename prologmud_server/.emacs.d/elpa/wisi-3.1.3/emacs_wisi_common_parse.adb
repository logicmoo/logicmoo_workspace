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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL;
with System.Multiprocessors;
with System.Storage_Elements;
with WisiToken.Lexer;
package body Emacs_Wisi_Common_Parse is

   procedure Usage (Name : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("usage: " & Name & "[--recover-log <file-name>]");
      Put_Line ("enters a loop waiting for commands:");
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("See wisi-process-parse.el *--send-parse, *--send-noop for arguments.");
   end Usage;

   procedure Read_Input (A : System.Address; N : Integer)
   is
      use System.Storage_Elements;

      B         : System.Address := A;
      Remaining : Integer        := N;
      Read      : Integer;
   begin
      --  We use GNAT.OS_Lib because it does not buffer input, so it runs
      --  under Emacs nicely; GNAT Text_IO does not return text until
      --  some fairly large buffer is filled.
      --
      --  With GNAT GPL 2016, GNAT.OS_Lib.Read does _not_ wait for all N
      --  bytes or EOF; it returns as soon as it gets some bytes.
      loop
         Read := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, B, Remaining);
         if Read = 0 then
            --  Pipe closed; probably parent Emacs crashed. Force exit.
            raise SAL.Programmer_Error with "input pipe closed";
         end if;
         Remaining := Remaining - Read;
         exit when Remaining <= 0;
         B := B + Storage_Offset (Read);
      end loop;
   end Read_Input;

   function Get_Command_Length return Integer
   is
      Temp : aliased String (1 .. 3) := (others => ' '); -- initialize for error message
   begin
      Read_Input (Temp'Address, Temp'Length);
      return Integer'Value (Temp);
   exception
   when Constraint_Error =>
      --  From Integer'Value
      raise Protocol_Error with "invalid command byte count; '" & Temp & "'";
   end Get_Command_Length;

   function Get_String
     (Source : in     String;
      Last   : in out Integer)
     return String
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Index
        (Source  => Source,
         Pattern => """",
         From    => Last + 1);
   begin
      Last := Index
        (Source  => Source,
         Pattern => """",
         From    => First + 1);

      if First = 0 or Last = 0 then
         raise Protocol_Error with "no '""' found for string";
      end if;

      return Source (First + 1 .. Last - 1);
   end Get_String;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 2; -- final char of previous item, space
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First);

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;

      return Integer'Value (Source (First .. Last));
   exception
   when others =>
      Ada.Text_IO.Put_Line ("bad integer '" & Source (First .. Source'Last) & "'");
      raise;
   end Get_Integer;

   function Get_Process_Start_Params return Process_Start_Params
   is
      use Ada.Command_Line;
      procedure Put_Usage
      is
         use Ada.Text_IO;
      begin
         Put_Line (Standard_Error, "process start args:");
         Put_Line (Standard_Error, "--help : put this help");
         Put_Line (Standard_Error, "--recover-log <file_name> : log recover actions to file");
      end Put_Usage;

      Next_Arg : Integer := 1;
   begin
      return Result : Process_Start_Params do
         loop
            exit when Next_Arg > Argument_Count;

            if Next_Arg <= Argument_Count and then Argument (Next_Arg) = "--help" then
               Put_Usage;
               raise Finish;

            elsif Next_Arg + 1 <= Argument_Count and then Argument (Next_Arg) = "--recover-log" then
               Result.Recover_Log_File_Name := Ada.Strings.Unbounded.To_Unbounded_String (Argument (Next_Arg + 1));
               Next_Arg := Next_Arg + 2;
            end if;
         end loop;
      end return;
   end Get_Process_Start_Params;

   function Get_Parse_Params (Command_Line : in String; Last : in out Integer) return Parse_Params
   is
      use WisiToken;
   begin
      return Result : Parse_Params do
         --  We don't use an aggregate, to enforce execution order.
         --  Match wisi-process-parse.el wisi-process--send-parse

         Result.Post_Parse_Action    := Wisi.Post_Parse_Action_Type'Val (Get_Integer (Command_Line, Last));
         Result.Source_File_Name     := +Get_String (Command_Line, Last);
         Result.Begin_Byte_Pos       := Get_Integer (Command_Line, Last);

         --  Emacs end is after last char.
         Result.End_Byte_Pos         := Get_Integer (Command_Line, Last) - 1;

         Result.Goal_Byte_Pos        := Get_Integer (Command_Line, Last);
         Result.Begin_Char_Pos       := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Begin_Line           := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
         Result.End_Line             := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
         Result.Begin_Indent         := Get_Integer (Command_Line, Last);
         Result.Partial_Parse_Active := 1 = Get_Integer (Command_Line, Last);
         Result.Debug_Mode           := 1 = Get_Integer (Command_Line, Last);
         Result.Parse_Verbosity      := Get_Integer (Command_Line, Last);
         Result.McKenzie_Verbosity   := Get_Integer (Command_Line, Last);
         Result.Action_Verbosity     := Get_Integer (Command_Line, Last);
         Result.McKenzie_Disable     := Get_Integer (Command_Line, Last);
         Result.Task_Count           := Get_Integer (Command_Line, Last);
         Result.Check_Limit          := Get_Integer (Command_Line, Last);
         Result.Enqueue_Limit        := Get_Integer (Command_Line, Last);
         Result.Max_Parallel         := Get_Integer (Command_Line, Last);
         Result.Byte_Count           := Get_Integer (Command_Line, Last);
      end return;
   end Get_Parse_Params;

   function Get_Refactor_Params (Command_Line : in String; Last : in out Integer) return Refactor_Params
   is
      use WisiToken;
   begin
      return Result : Refactor_Params do
         --  We don't use an aggregate, to enforce execution order.
         --  Match wisi-process-parse.el wisi-process--send-refactor

         Result.Refactor_Action    := Get_Integer (Command_Line, Last);
         Result.Source_File_Name   := +Get_String (Command_Line, Last);
         Result.Parse_Region.First := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Parse_Region.Last  := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last) - 1);

         Result.Edit_Begin           := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Parse_Begin_Char_Pos := WisiToken.Buffer_Pos (Get_Integer (Command_Line, Last));
         Result.Parse_Begin_Line     := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
         Result.Parse_End_Line       := WisiToken.Line_Number_Type (Get_Integer (Command_Line, Last));
         Result.Parse_Begin_Indent   := Get_Integer (Command_Line, Last);
         Result.Debug_Mode           := 1 = Get_Integer (Command_Line, Last);
         Result.Parse_Verbosity      := Get_Integer (Command_Line, Last);
         Result.Action_Verbosity     := Get_Integer (Command_Line, Last);
         Result.Max_Parallel         := Get_Integer (Command_Line, Last);
         Result.Byte_Count           := Get_Integer (Command_Line, Last);
      end return;
   end Get_Refactor_Params;

   procedure Process_Stream
     (Name                      : in     String;
      Language_Protocol_Version : in     String;
      Partial_Parse_Active      : in out Boolean;
      Params                    : in     Process_Start_Params;
      Parser                    : in out WisiToken.Parse.LR.Parser.Parser;
      Parse_Data                : in out Wisi.Parse_Data_Type'Class;
      Descriptor                : in     WisiToken.Descriptor)
   is
      use Ada.Text_IO;
      use WisiToken; -- "+", "-" Unbounded_string

      procedure Cleanup
      is begin
         if Is_Open (Parser.Recover_Log_File) then
            Close (Parser.Recover_Log_File);
         end if;
      end Cleanup;

   begin
      declare
         use Ada.Directories;
         use Ada.Strings.Unbounded;
      begin
         if Length (Params.Recover_Log_File_Name) > 0 then
            Put_Line (";; logging to '" & (-Params.Recover_Log_File_Name) & "'");
            --  to Current_Output, visible from Emacs

            if Exists (-Params.Recover_Log_File_Name) then
               Open (Parser.Recover_Log_File, Append_File, -Params.Recover_Log_File_Name);
            else
               Create (Parser.Recover_Log_File, Out_File, -Params.Recover_Log_File_Name);
            end if;
         end if;
      end;

      Parser.Trace.Set_Prefix (";; "); -- so debug messages don't confuse Emacs.

      Put_Line
        (Name & " protocol: process version " & Protocol_Version & " language version " & Language_Protocol_Version);

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
               --  Args: see wisi-process-parse.el wisi-process-parse--send-parse
               --  Input: <source text>
               --  Response:
               --  [response elisp vector]...
               --  [elisp error form]...
               --  prompt
               declare
                  Params : constant Parse_Params := Get_Parse_Params (Command_Line, Last);
                  Buffer : Ada.Strings.Unbounded.String_Access;

                  procedure Clean_Up
                  is
                     use all type SAL.Base_Peek_Type;
                  begin
                     Parser.Lexer.Discard_Rest_Of_Input;
                     if Parser.Parsers.Count > 0 then
                        Parse_Data.Put
                          (Parser.Lexer.Errors,
                           Parser.Parsers.First.State_Ref.Errors,
                           Parser.Parsers.First.State_Ref.Tree);
                     end if;
                     Ada.Strings.Unbounded.Free (Buffer);
                  end Clean_Up;

               begin
                  Trace_Parse    := Params.Parse_Verbosity;
                  Trace_McKenzie := Params.McKenzie_Verbosity;
                  Trace_Action   := Params.Action_Verbosity;
                  Debug_Mode     := Params.Debug_Mode;

                  Partial_Parse_Active        := Params.Partial_Parse_Active;
                  Parser.Partial_Parse_Active := Params.Partial_Parse_Active;

                  if WisiToken.Parse.LR.McKenzie_Defaulted (Parser.Table.all) then
                     --  There is no McKenzie information; don't override that.
                     null;
                  elsif Params.McKenzie_Disable = -1 then
                     --  Use default
                     Parser.Enable_McKenzie_Recover := True;
                  else
                     Parser.Enable_McKenzie_Recover := Params.McKenzie_Disable = 0;
                  end if;

                  Parse_Data.Initialize
                    (Post_Parse_Action => Params.Post_Parse_Action,
                     Lexer             => Parser.Lexer,
                     Descriptor        => Descriptor'Unrestricted_Access,
                     Base_Terminals    => Parser.Terminals'Unrestricted_Access,
                     Begin_Line        => Params.Begin_Line,
                     End_Line          => Params.End_Line,
                     Begin_Indent      => Params.Begin_Indent,
                     Params            => Command_Line (Last + 2 .. Command_Line'Last));

                  if Params.Task_Count > 0 then
                     Parser.Table.McKenzie_Param.Task_Count := System.Multiprocessors.CPU_Range (Params.Task_Count);
                  end if;
                  if Params.Check_Limit > 0 then
                     Parser.Table.McKenzie_Param.Check_Limit := Base_Token_Index (Params.Check_Limit);
                  end if;
                  if Params.Enqueue_Limit > 0 then
                     Parser.Table.McKenzie_Param.Enqueue_Limit := Params.Enqueue_Limit;
                  end if;

                  if Params.Max_Parallel > 0 then
                     Parser.Max_Parallel := SAL.Base_Peek_Type (Params.Max_Parallel);
                  end if;

                  Buffer := new String (Params.Begin_Byte_Pos .. Params.End_Byte_Pos);

                  Read_Input (Buffer (Params.Begin_Byte_Pos)'Address, Params.Byte_Count);

                  Parser.Lexer.Reset_With_String_Access
                    (Buffer, Params.Source_File_Name, Params.Begin_Char_Pos, Params.Begin_Line);

                  --  Parser.Line_Begin_Token First, Last set by Lex_All
                  begin
                     Parser.Parse;
                  exception
                  when WisiToken.Partial_Parse =>
                     null;
                  end;
                  Parser.Execute_Actions;
                  Parse_Data.Put (Parser);
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

            elsif Match ("refactor") then
               --  Args: see wisi-process-parse.el wisi-process-parse--send-refactor
               --  Input: <source text>
               --  Response:
               --  [edit elisp vector]...
               --  prompt
               declare
                  Params : constant Refactor_Params := Get_Refactor_Params (Command_Line, Last);
                  Buffer : Ada.Strings.Unbounded.String_Access;

                  procedure Clean_Up
                  is
                     use all type SAL.Base_Peek_Type;
                  begin
                     Parser.Lexer.Discard_Rest_Of_Input;
                     if Parser.Parsers.Count > 0 then
                        Parse_Data.Put
                          (Parser.Lexer.Errors,
                           Parser.Parsers.First.State_Ref.Errors,
                           Parser.Parsers.First.State_Ref.Tree);
                     end if;
                     Ada.Strings.Unbounded.Free (Buffer);
                  end Clean_Up;

               begin
                  Trace_Parse  := Params.Parse_Verbosity;
                  Trace_Action := Params.Action_Verbosity;
                  Debug_Mode   := Params.Debug_Mode;

                  Partial_Parse_Active := True;

                  Parse_Data.Initialize
                    (Post_Parse_Action => Wisi.Navigate, -- mostly ignored
                     Lexer             => Parser.Lexer,
                     Descriptor        => Descriptor'Unrestricted_Access,
                     Base_Terminals    => Parser.Terminals'Unrestricted_Access,
                     Begin_Line        => Params.Parse_Begin_Line,
                     End_Line          => Params.Parse_End_Line,
                     Begin_Indent      => Params.Parse_Begin_Indent,
                     Params            => "");

                  if Params.Max_Parallel > 0 then
                     Parser.Max_Parallel := SAL.Base_Peek_Type (Params.Max_Parallel);
                  end if;

                  Buffer := new String (Integer (Params.Parse_Region.First) .. Integer (Params.Parse_Region.Last));

                  Read_Input (Buffer (Buffer'First)'Address, Params.Byte_Count);

                  Parser.Lexer.Reset_With_String_Access
                    (Buffer, Params.Source_File_Name, Params.Parse_Begin_Char_Pos, Params.Parse_Begin_Line);
                  begin
                     Parser.Parse;
                  exception
                  when WisiToken.Partial_Parse =>
                     null;
                  end;
                  Parser.Execute_Actions;
                  Parse_Data.Refactor (Parser.Parsers.First_State_Ref.Tree, Params.Refactor_Action, Params.Edit_Begin);
                  Clean_Up;

               exception
               when Syntax_Error =>
                  Clean_Up;
                  Put_Line ("(parse_error ""refactor " & Params.Parse_Region.First'Image &
                              Params.Parse_Region.Last'Image & ": syntax error"")");

               when E : Parse_Error =>
                  Clean_Up;
                  Put_Line ("(parse_error ""refactor " & Params.Parse_Region.First'Image &
                              Params.Parse_Region.Last'Image & ": " & Ada.Exceptions.Exception_Message (E) & """)");

               when E : others => -- includes Fatal_Error
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

                  Parser.Lexer.Reset_With_String_Access (Buffer, +"");
                  loop
                     exit when Token.ID = Parser.Trace.Descriptor.EOI_ID;
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
      Cleanup;
   exception
   when Finish =>
      null;

   when E : others =>
      Cleanup;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      New_Line (2);
      Put_Line
        ("(error ""unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
           Ada.Exceptions.Exception_Message (E) & """)");
   end Process_Stream;

end Emacs_Wisi_Common_Parse;
