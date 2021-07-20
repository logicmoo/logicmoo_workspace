--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2019 Free Software Foundation, Inc.
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

with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
package body WisiToken.Text_IO_Trace is

   function Insert_Prefix_At_Newlines (Trace : in Text_IO_Trace.Trace; Item : in String) return String
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      First : Integer := Item'First;
      Last : Integer;
   begin
      loop
         Last := Index (Pattern => "" & ASCII.LF, Source => Item (First .. Item'Last));
         exit when Last = 0;
         Result := Result & Item (First .. Last) & Trace.Prefix;
         First := Last + 1;
      end loop;
      Result := Result & Item (First .. Item'Last);
      return -Result;
   end Insert_Prefix_At_Newlines;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Set_Prefix (Trace : in out Text_IO_Trace.Trace; Prefix : in String)
   is begin
      Trace.Prefix := +Prefix;
   end Set_Prefix;

   overriding
   procedure Put (Trace : in out Text_IO_Trace.Trace; Item : in String; Prefix : in Boolean := True)
   is
      use Ada.Text_IO;
   begin
      if Trace.File /= null and then Is_Open (Trace.File.all) then
         Ada.Text_IO.Put (Trace.File.all, (if Prefix then -Trace.Prefix else "") & Item);
      else
         Ada.Text_IO.Put ((if Prefix then -Trace.Prefix else "") & Item);
      end if;
   end Put;

   overriding
   procedure Put_Line (Trace : in out Text_IO_Trace.Trace; Item : in String)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      Temp : constant String :=
        (if 0 /= Index (Item, "" & ASCII.LF)
         then Insert_Prefix_At_Newlines (Trace, Item)
         else Item);
   begin

      if Trace.File /= null and then Is_Open (Trace.File.all) then
         Ada.Text_IO.Put_Line (Trace.File.all, -Trace.Prefix & Temp);
         Ada.Text_IO.Flush (Trace.File.all);
      else
         Ada.Text_IO.Put_Line (-Trace.Prefix & Temp);
         Ada.Text_IO.Flush;
      end if;
   end Put_Line;

   overriding
   procedure New_Line (Trace : in out Text_IO_Trace.Trace)
   is
      use Ada.Text_IO;
   begin
      if Trace.File /= null and then Is_Open (Trace.File.all) then
         Ada.Text_IO.New_Line (Trace.File.all);
      else
         Ada.Text_IO.New_Line;
      end if;
   end New_Line;

   overriding
   procedure Put_Clock (Trace : in out Text_IO_Trace.Trace; Label : in String)
   is begin
      Trace.Put_Line
        (Ada.Calendar.Formatting.Image
           (Ada.Calendar.Clock, Include_Time_Fraction => True) & " " & Label);
   end Put_Clock;

   procedure Set_File (Trace : in out Text_IO_Trace.Trace; File : in Ada.Text_IO.File_Access)
   is begin
      Trace.File := File;
   end Set_File;

   procedure Clear_File (Trace : in out Text_IO_Trace.Trace)
   is begin
      Trace.File := null;
   end Clear_File;

end WisiToken.Text_IO_Trace;
