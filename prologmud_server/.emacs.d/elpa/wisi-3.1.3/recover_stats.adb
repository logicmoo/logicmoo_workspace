--  Abstract :
--
--  Summarize error recover log.
--
--  Copyright (C) 2019 - 2020 Stephen Leake All Rights Reserved.
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
with Ada.Long_Float_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Gen_Stats.Gen_Image;
with SAL.Long_Float_Stats;
with WisiToken.Parse.LR;
procedure Recover_Stats
is
   subtype Strategies is WisiToken.Parse.LR.Strategies;

   File : File_Type;

   Delimiters : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (",() ");
   Number     : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("0123456789");

   type Strategy_Counts is array (Strategies) of Natural;

   type Recover_Label is (Full, Partial);

   type Recover_Summary is record
      Event_Count : Integer := 0;
      --  1 per recover event (1 line in log file)

      Enqueue_Stats : SAL.Long_Float_Stats.Stats_Type;
      Check_Stats   : SAL.Long_Float_Stats.Stats_Type;

      Strat_Counts_Total   : Strategy_Counts := (others => 0);
      Strat_Counts_Present : Strategy_Counts := (others => 0);
      --  1 per recover event if used

      Recover_Count_Present : Integer := 0;
      --  1 per parser in recover result

      Recover_Count_Total : Integer := 0;
      --  Sum of all strategy counts

      Fail_Event_Count      : Integer := 0; -- for all reasons
      Fail_Enqueue_Limit    : Integer := 0;
      Fail_No_Configs_Left  : Integer := 0;
      Fail_Programmer_Error : Integer := 0;
      Fail_Other            : Integer := 0;
   end record;

   Summary : array (Recover_Label) of Recover_Summary;
begin
   Open (File, In_File, Ada.Command_Line.Argument (1));

   loop
      exit when End_Of_File (File);
      declare
         --  The recover log is written by code in
         --  wisitoken-parse-lr-parser.adb Parse (search for Recover_Log).
         --
         --  A line has the syntax:
         --  yyyy-mm-dd hh:mm:ss <partial> <success> pre_parser_count '<file_name>' (<parser_data>)...
         --
         --  where there is one (<parser_data) for each parser active after recover. <parser_data> is:
         --
         --  (<strategy_counts>) <enqueue_count> <check_count> <success>
         --
         --  Note that the per-parser success is always TRUE; it would not be
         --  active if recover had failed.

         Line  : constant String := Get_Line (File);
         First : Integer         := Index (Line, " "); -- after date
         Last  : Integer;

         Label : Recover_Label := Full;

         function Line_Eq (Item : in String) return Boolean
         is begin
            return Line (First .. First + Item'Length - 1) = Item;
         end Line_Eq;

         function Next_Integer return Integer
         is begin
            Find_Token
              (Line, Number,
               From  => Last + 1,
               Test  => Ada.Strings.Inside,
               First => First,
               Last  => Last);
            return Integer'Value (Line (First .. Last));
         exception
         when Constraint_Error =>
            raise Constraint_Error with "bad integer '" & Line (First .. Last - 1) & "' " &
              Ada.Text_IO.Count'Image (Ada.Text_IO.Line (File) - 1) & First'Image & Last'Image;
         end Next_Integer;

         function Next_Boolean return  Boolean
         is begin
            First := Last + 2;
            Last  := -1 + Index (Line, Delimiters, First);
            return Boolean'Value (Line (First .. Last));
         end Next_Boolean;

         function Read_Strat_Counts (Strategy_Found : out Boolean) return Strategy_Counts
         is begin
            Strategy_Found := False;
            Last := Index (Line, "(", Last + 1);
            return Result : Strategy_Counts do
               for I in Strategies loop
                  Result (I) := Next_Integer;
                  if Result (I) > 0 then
                     Strategy_Found := True;
                  end if;
               end loop;
               Last := 1 + Index (Line, ")", Last + 1);
            end return;
         end Read_Strat_Counts;

      begin
         First := Index (Line, " ", First + 1); -- after time
         Last  := Index (Line, " ", First + 1); -- after Partial_Parse_Active
         if Boolean'Value (Line (First + 1 .. Last - 1)) then
            Label := Partial;
         end if;

         Summary (Label).Event_Count := Summary (Label).Event_Count + 1;

         First := Last + 1;
         if Line (First .. First + 3) = "FAIL" then
            Summary (Label).Fail_Event_Count := Summary (Label).Fail_Event_Count + 1;
            First := First + 4;

            if Line_Eq ("NO_CONFIGS_LEFT") then
               Summary (Label).Fail_No_Configs_Left := Summary (Label).Fail_No_Configs_Left + 1;
            elsif Line_Eq ("ENQUEUE_LIMIT") then
               Summary (Label).Fail_Enqueue_Limit := Summary (Label).Fail_Enqueue_Limit + 1;
            elsif Line_Eq ("PROGRAMMER_ERROR") then
               Summary (Label).Fail_Programmer_Error := Summary (Label).Fail_Programmer_Error + 1;
            else
               Summary (Label).Fail_Other := Summary (Label).Fail_Other + 1;
            end if;

         else
            --  Process per-parser data
            Last := Index (Line, "(", Last + 1);
            loop
               exit when Line (Last + 1) = ')';
               declare
                  Strategy_Found : Boolean;
                  Strat_Counts   : constant Strategy_Counts := Read_Strat_Counts (Strategy_Found);
                  Enqueue_Count  : constant Integer         := Next_Integer;
                  Check_Count    : constant Integer         := Next_Integer;
                  Success        : constant Boolean         := Next_Boolean;
                  pragma Unreferenced (Success);
               begin
                  Summary (Label).Recover_Count_Present := Summary (Label).Recover_Count_Present + 1;

                  if not Strategy_Found then
                     raise SAL.Programmer_Error;
                  else
                     Summary (Label).Enqueue_Stats.Accumulate (Long_Float (Enqueue_Count));
                     Summary (Label).Check_Stats.Accumulate (Long_Float (Check_Count));
                     for I in Strategies loop
                        Summary (Label).Recover_Count_Total    :=
                          Summary (Label).Recover_Count_Total + Strat_Counts (I);
                        Summary (Label).Strat_Counts_Total (I) :=
                          Summary (Label).Strat_Counts_Total (I) + Strat_Counts (I);
                        if Strat_Counts (I) > 0 then
                           Summary (Label).Strat_Counts_Present (I) := Summary (Label).Strat_Counts_Present (I) + 1;
                        end if;
                     end loop;
                  end if;
               end;
            end loop;
         end if;
      end;
   end loop;

   declare
      use Ada.Strings;

      Label_Field     : String (1 .. 23); -- fits strategy and fail labels
      Count_Field     : String (1 .. 8);
      Percent_Field   : String (1 .. 4);
      --  Shared by Put_If, Put_Percent

      procedure Put_If
        (Summary_Label : in Recover_Label;
         Name          : in String;
         Count         : in Integer;
         Always        : in Boolean := False)
      is
         Percent_Present : constant Integer :=
           Integer (100.0 * Float (Count) / Float (Summary (Summary_Label).Event_Count));
      begin
         if Count > 0 or Always then
            Move (Name, Label_Field); Put (Label_Field & " => ");
            Move (Count'Image, Count_Field, Justify => Right); Put (Count_Field);
            Move (Percent_Present'Image & "%", Percent_Field, Justify => Right); Put_Line (Percent_Field);
         end if;
      end Put_If;

      package Stats_Image is new SAL.Long_Float_Stats.Gen_Image
        (Real_IO           => Ada.Long_Float_Text_IO,
         Default_Mean_Fore => 7,
         Default_Mean_Aft  => 0,
         Default_Mean_Exp  => 0,
         Default_Sd_Fore   => 7,
         Default_Sd_Aft    => 1,
         Default_Sd_Exp    => 0);

      procedure Put_Percent (Summary_Label : in Recover_Label; Present, Total : in Integer; Name : in String)
      is
         Percent_Present : constant Integer :=
           Integer (100.0 * Float (Present) / Float (Summary (Summary_Label).Recover_Count_Present));
         Percent_Total   : constant Integer :=
           Integer (100.0 * Float (Total) / Float (Summary (Summary_Label).Recover_Count_Total));
      begin
         Move (Name, Label_Field); Put (Label_Field);
         Move (Present'Image, Count_Field, Justify => Right); Put (Count_Field);
         Move (Percent_Present'Image & "%", Percent_Field, Justify => Right); Put (Percent_Field & " /");
         Move (Total'Image, Count_Field, Justify => Right); Put (Count_Field);
         Move (Percent_Total'Image & "%", Percent_Field, Justify => Right); Put_Line (Percent_Field);
      end Put_Percent;

   begin
      for I in Recover_Label loop
         Put_Line (I'Image);
         Put_Line ("present/total:" & Summary (I).Event_Count'Image & " /" & Summary (I).Recover_Count_Total'Image);
         if Summary (I).Event_Count > 0 then
            Put_Line ("           mean        std. dev.    min     max");
            Put_Line ("Enqueue: " & Stats_Image.Image (Summary (I).Enqueue_Stats.Display));
            Put_Line ("Check:   " & Stats_Image.Image (Summary (I).Check_Stats.Display));
            Put_If (I, "FAIL", Summary (I).Fail_Event_Count, Always => True);
            Put_If (I, "FAIL_ENQUEUE_LIMIT", Summary (I).Fail_Enqueue_Limit);
            Put_If (I, "FAIL_NO_CONFIGS_LEFT", Summary (I).Fail_No_Configs_Left);
            Put_If (I, "FAIL_PROGRAMMER_ERROR", Summary (I).Fail_Programmer_Error);
            Put_If (I, "FAIL_OTHER", Summary (I).Fail_Other);
            for J in Strategies loop
               Put_Percent
                 (I,
                  Summary (I).Strat_Counts_Present (J),
                  Summary (I).Strat_Counts_Total (J),
                  J'Image);
            end loop;
         end if;
         New_Line;
      end loop;
   end;
exception
when E : others =>
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Recover_Stats;
