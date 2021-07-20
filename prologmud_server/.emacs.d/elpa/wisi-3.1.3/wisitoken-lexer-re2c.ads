--  Abstract:
--
--  WisiToken wrapper around the re2c lexer
--
--  References:
--
--  [1] http://re2c.org/
--
--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL); -- GNATCOLL.Mmap

pragma Warnings (Off, "license of withed unit ""GNATCOLL.Mmap"" may be inconsistent");

pragma Warnings (On);
with Interfaces.C;
with System;
generic
   --  These subprograms are provided by generated source code.

   with function New_Lexer
     (Buffer    : in System.Address;
      Length    : in Interfaces.C.size_t;
      Verbosity : in Interfaces.C.int)
     return System.Address;
   --  Create the re2c lexer object, passing it the full text to process.
   --  Length is buffer length in 8 bit bytes.
   --
   --  The C lexer does not know about Buffer_Nominal_First,
   --  Line_Nominal_First; its buffer positions and lines start at 1.

   with procedure Free_Lexer (Lexer : in out System.Address);
   --  Destruct the re2c lexer object

   with procedure Reset_Lexer (Lexer : in System.Address);
   --  Restart lexing, with previous input buffer.

   with function Next_Token
     (Lexer         : in     System.Address;
      ID            :    out Token_ID;
      Byte_Position :    out Interfaces.C.size_t;
      Byte_Length   :    out Interfaces.C.size_t;
      Char_Position :    out Interfaces.C.size_t;
      Char_Length   :    out Interfaces.C.size_t;
      Line_Start    :    out Interfaces.C.int)
     return Interfaces.C.int;
   --  *_Position and *_Length give the position and length in bytes and
   --  characters of the token from the start of the buffer, 0 indexed.
   --
   --  Line_Start gives the line number in the source file that the first
   --  character of the token is in, 1 indexed.
   --
   --  Result values:
   --
   --  0 - no error
   --  1 - there is an unrecognized character at Position.

package WisiToken.Lexer.re2c is

   Invalid_Input : exception;

   type Instance is new WisiToken.Lexer.Instance with private;

   overriding procedure Finalize (Object : in out Instance);

   function New_Lexer
     (Descriptor  : not null access constant WisiToken.Descriptor)
     return WisiToken.Lexer.Handle;
   --  If the tokens do not include a reporting New_Line token, set
   --  New_Line_ID to Invalid_Token_ID.

   overriding procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First);
   --  Copies Input to internal buffer.

   overriding procedure Reset_With_String_Access
     (Lexer      : in out Instance;
      Input      : in     Ada.Strings.Unbounded.String_Access;
      File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First);

   overriding procedure Reset_With_File
     (Lexer          : in out Instance;
      File_Name      : in     String;
      Begin_Byte_Pos : in     Buffer_Pos       := Invalid_Buffer_Pos;
      End_Byte_Pos   : in     Buffer_Pos       := Invalid_Buffer_Pos;
      Begin_Char     : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line     : in     Line_Number_Type := Line_Number_Type'First);
   --  Uses memory mapped file; no copies.

   overriding procedure Discard_Rest_Of_Input (Lexer : in out Instance) is null;

   overriding procedure Reset (Lexer : in out Instance);

   overriding function Buffer_Text (Lexer : in Instance; Byte_Bounds : in Buffer_Region) return String;

   overriding function First (Lexer : in Instance) return Boolean;

   overriding
   function Find_Next
     (Lexer : in out Instance;
      Token :    out Base_Token)
     return Boolean;

   overriding function File_Name (Lexer : in Instance) return String;

private

   type Instance is new WisiToken.Lexer.Instance with
   record
      Lexer         : System.Address := System.Null_Address;
      Source        : WisiToken.Lexer.Source;
      ID            : Token_ID; --  Last token read by find_next
      Byte_Position : Natural;  --  We don't use Buffer_Pos here, because Source.Buffer is indexed by Integer
      Byte_Length   : Natural;
      Char_Position : Natural;
      Char_Length   : Natural;
      --  Position and length in bytes and characters of last token from
      --  start of Managed.Buffer, 1 indexed.

      Line            : Line_Number_Type; -- after last (or current) New_Line token
      Char_Line_Start : Natural;          -- Character position after last New_Line token, lexer origin.
      Prev_ID         : Token_ID;         -- previous token_id
   end record;

end WisiToken.Lexer.re2c;
