--  Abstract :
--
--  Trace output to Ada.Text_IO
--
--  Copyright (C) 2017, 2019, 2020 Free Software Foundation, Inc.
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

with Ada.Text_IO;
package WisiToken.Text_IO_Trace is

   type Trace is limited new WisiToken.Trace with private;
   --  Defaults to Ada.Text_IO.Standard_Output

   overriding
   procedure Set_Prefix (Trace : in out Text_IO_Trace.Trace; Prefix : in String);

   overriding
   procedure Put (Trace : in out Text_IO_Trace.Trace; Item : in String; Prefix : in Boolean := True);

   overriding
   procedure Put_Line (Trace : in out Text_IO_Trace.Trace; Item : in String);
   --  If Item contains ASCII.LF, Prefix is output after each one.

   overriding
   procedure New_Line (Trace : in out Text_IO_Trace.Trace);

   overriding
   procedure Put_Clock (Trace : in out Text_IO_Trace.Trace; Label : in String);

   procedure Set_File (Trace : in out Text_IO_Trace.Trace; File : in Ada.Text_IO.File_Access);
   --  Set file for trace output. Default is Text_IO.Current_Output.

   procedure Clear_File (Trace : in out Text_IO_Trace.Trace);
   --  Clear internal file; output to Text_IO.Current_Output.

private
   type Trace is limited new WisiToken.Trace with record
      File   : Ada.Text_IO.File_Access;
      Prefix : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end WisiToken.Text_IO_Trace;
