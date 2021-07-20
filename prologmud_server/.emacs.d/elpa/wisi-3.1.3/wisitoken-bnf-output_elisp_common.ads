--  Abstract :
--
--  Subprograms common to Output_Elisp and Output_Ada_Emacs
--
--  Copyright (C) 2012, 2013, 2015, 2017, 2018, 2019 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package WisiToken.BNF.Output_Elisp_Common is

   function Find_Elisp_ID (List : in WisiToken.BNF.String_Lists.List; Elisp_Name : in String) return Integer;

   function Elisp_Name_To_Ada
     (Elisp_Name : in String;
      Append_ID  : in Boolean;
      Trim       : in Integer)
     return String;
   --  Drop Trim chars from beginning of Elisp_Name, capitalize.

   procedure Indent_Keyword_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Keywords         : in     String_Pair_Lists.List;
      Image            : access function (Name : in Ada.Strings.Unbounded.Unbounded_String) return String);

   procedure Indent_Token_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Tokens           : in     Token_Lists.List;
      Image            : access function (Name : in Ada.Strings.Unbounded.Unbounded_String) return String);

   procedure Indent_Name_Table
     (Output_File_Root : in     String;
      Label            : in     String;
      Names            : in     String_Lists.List);

   procedure Indent_Repair_Image
     (Output_File_Root : in String;
      Label            : in String;
      Tokens           : in WisiToken.BNF.Tokens);

end WisiToken.BNF.Output_Elisp_Common;
