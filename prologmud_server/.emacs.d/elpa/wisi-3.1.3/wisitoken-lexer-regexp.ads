--  Abstract:
--
--  WisiToken lexer using compiled regular expressions interpreted at runtime.
--
--  This is slower, but easier to use, than the Aflex lexer; it is
--  used in most of the WisiToken unit tests. Since it uses regexp, it
--  is easy to convert to an Aflex lexer.
--
--  Copyright (C) 2015, 2017 - 2019 Free Software Foundation, Inc.
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
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Unchecked_Deallocation;
with WisiToken.Regexp;
package WisiToken.Lexer.Regexp is

   type Syntax_Item is record
      Regexp : WisiToken.Regexp.Regexp;
      Report : Boolean;
   end record;

   function Get
     (Regexp         : in String;
      Case_Sensitive : in Boolean := True;
      Report         : in Boolean := True)
     return Syntax_Item;
   --  Compiles Regexp with Case_Sensitive.

   type Syntax is array (Token_ID range <>) of Syntax_Item;

   type Instance
     (Descriptor    : not null access constant WisiToken.Descriptor;
      Last_Terminal : Token_ID)
     is new WisiToken.Lexer.Instance with private;

   function New_Lexer
     (Descriptor : not null access constant WisiToken.Descriptor;
      Syntax     : in              WisiToken.Lexer.Regexp.Syntax)
     return WisiToken.Lexer.Handle;

   overriding procedure Finalize (Object : in out Instance);
   overriding procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First);
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
   overriding procedure Reset (Lexer : in out Instance);

   overriding procedure Discard_Rest_Of_Input (Lexer : in out Instance) is null;

   overriding function Find_Next
     (Lexer : in out Instance;
      Token :    out Base_Token)
     return Boolean;

   overriding function Buffer_Text (Lexer : in Instance; Byte_Region : in Buffer_Region) return String;

   overriding function First (Lexer : in Instance) return Boolean is (False);

   overriding function File_Name (Lexer : in Instance) return String is ("");

private

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   type Instance
     (Descriptor    : not null access constant WisiToken.Descriptor;
      Last_Terminal : Token_ID)
     is new WisiToken.Lexer.Instance (Descriptor => Descriptor) with
   record
      ID          : Token_ID; --  last token read by find_next
      Syntax      : WisiToken.Lexer.Regexp.Syntax (Token_ID'First .. Last_Terminal);
      Source      : Lexer.Source;
      Buffer_Head : Integer;
      Lexeme_Head : Integer;
      Lexeme_Tail : Integer;
   end record;

end WisiToken.Lexer.Regexp;
