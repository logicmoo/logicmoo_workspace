--  Abstract:
--
--  see spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with Ada.Directories;
with Ada.Strings.Unbounded;
with GNATCOLL.Mmap;
package body WisiToken.Lexer.re2c is

   overriding procedure Finalize (Object : in out Instance)
   is
      use all type System.Address;
   begin
      if Object.Lexer /= System.Null_Address then
         Free_Lexer (Object.Lexer);
         Object.Lexer := System.Null_Address;
      end if;

      Finalize (Object.Source);
   end Finalize;

   type Instance_Access is access Instance; --  silence compiler warning

   function New_Lexer
     (Descriptor  : not null access constant WisiToken.Descriptor)
     return Handle
   is begin
      return Handle (Instance_Access'(new Instance (Descriptor)));
   end New_Lexer;

   overriding procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label                     => String_Label,
         File_Name                 => +"",
         Buffer_Nominal_First_Byte => Base_Buffer_Pos (Input'First),
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Begin_Line,
         Buffer                    => new String'(Input),
         User_Buffer               => False);

      Lexer.Lexer := New_Lexer
        (Buffer    => Lexer.Source.Buffer.all'Address,
         Length    => Interfaces.C.size_t (Input'Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_String;

   overriding procedure Reset_With_String_Access
     (Lexer      : in out Instance;
      Input      : in     Ada.Strings.Unbounded.String_Access;
      File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is begin
      Finalize (Lexer);

      --  We assume Input is in UTF-8 encoding
      Lexer.Source :=
        (Label       => String_Label,
         File_Name   =>
           +(if Ada.Strings.Unbounded.Length (File_Name) = 0 then ""
             else Ada.Directories.Simple_Name (-File_Name)),
         Buffer_Nominal_First_Byte => Base_Buffer_Pos (Input'First),
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Begin_Line,
         Buffer                    => Input,
         User_Buffer               => True);

      Lexer.Lexer := New_Lexer
        (Buffer    => Lexer.Source.Buffer.all'Address,
         Length    => Interfaces.C.size_t (Input'Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_String_Access;

   overriding procedure Reset_With_File
     (Lexer          : in out Instance;
      File_Name      : in     String;
      Begin_Byte_Pos : in     Buffer_Pos       := Invalid_Buffer_Pos;
      End_Byte_Pos   : in     Buffer_Pos       := Invalid_Buffer_Pos;
      Begin_Char     : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line     : in     Line_Number_Type := Line_Number_Type'First)
   is
      use GNATCOLL.Mmap;
      Length : Buffer_Pos;
   begin
      Finalize (Lexer);

      --  We assume the file is in UTF-8 encoding
      Lexer.Source :=
        (File_Label, +Ada.Directories.Simple_Name (File_Name),
         Buffer_Nominal_First_Byte => Buffer_Pos'First, -- overwritten below,
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Line_Number_Type'First, -- overwritten below
         File                      => Open_Read (File_Name),
         Region                    => Invalid_Mapped_Region,
         Buffer_Last               => 1);

      if Begin_Byte_Pos = Invalid_Buffer_Pos then
         Lexer.Source.Region := Read (Lexer.Source.File);
         Length              := Buffer_Pos (Last (Lexer.Source.Region));
      else
         Length := End_Byte_Pos - Begin_Byte_Pos + 1;

         Lexer.Source.Buffer_Nominal_First_Byte := Begin_Byte_Pos;
         Lexer.Source.Line_Nominal_First        := Begin_Line;

         Lexer.Source.Region := Read
           (Lexer.Source.File,
            Offset => File_Size (Begin_Byte_Pos - 1), -- Offset is 0 indexed, Begin_Byte_Pos is 1 indexed
            Length => File_Size (Length));
      end if;

      Lexer.Source.Buffer_Last := Last (Lexer.Source.Region);

      Lexer.Lexer := New_Lexer
        (Buffer    => Data (Lexer.Source.Region).all'Address,
         Length    => Interfaces.C.size_t (Length),
         Verbosity => Interfaces.C.int (if Trace_Parse > 3 then Trace_Parse - 3 else 0));

      Reset (Lexer);
   end Reset_With_File;

   overriding procedure Reset (Lexer : in out Instance)
   is begin
      Reset_Lexer (Lexer.Lexer);
      Lexer.Line            := 1;
      Lexer.Char_Line_Start := 1;
      Lexer.ID :=
        --  First token is assumed to be first on a line.
        (if Lexer.Descriptor.New_Line_ID = Invalid_Token_ID
         then Invalid_Token_ID
         else Lexer.Descriptor.New_Line_ID);
      Lexer.Prev_ID := Invalid_Token_ID;
   end Reset;

   overriding function Find_Next
     (Lexer : in out Instance;
      Token :    out Base_Token)
     return Boolean
   is
      use Interfaces.C;

      procedure Build_Token
      is begin
         Token :=
           (ID         => Lexer.ID,
            Tree_Index => Invalid_Node_Index,

            Byte_Region =>
              (if Lexer.ID = Lexer.Descriptor.EOI_ID and then Lexer.Byte_Position = Integer (Base_Buffer_Pos'First)
               then
                  --  EOF in empty buffer
                 (Lexer.Source.Buffer_Nominal_First_Byte,
                  Lexer.Source.Buffer_Nominal_First_Byte - 1)
               else
                 (Base_Buffer_Pos (Lexer.Byte_Position) + Lexer.Source.Buffer_Nominal_First_Byte - Buffer_Pos'First,
                  Base_Buffer_Pos (Lexer.Byte_Position + Lexer.Byte_Length - 1) +
                    Lexer.Source.Buffer_Nominal_First_Byte - Buffer_Pos'First)),

            Line => Lexer.Line + Lexer.Source.Line_Nominal_First - Line_Number_Type'First,

            Column =>
              (if Lexer.ID = Lexer.Descriptor.New_Line_ID or
                 Lexer.ID = Lexer.Descriptor.EOI_ID
               then 0
               else Ada.Text_IO.Count (Lexer.Char_Position - Lexer.Char_Line_Start)),

            Char_Region =>
              (if Lexer.ID = Lexer.Descriptor.EOI_ID and then Lexer.Byte_Position = Integer (Base_Buffer_Pos'First)
               then
                  --  EOF in empty buffer
                 (Lexer.Source.Buffer_Nominal_First_Byte,
                  Lexer.Source.Buffer_Nominal_First_Byte - 1)
               else
                 (To_Char_Pos (Lexer.Source, Lexer.Char_Position),
                  To_Char_Pos (Lexer.Source, Lexer.Char_Position + Lexer.Char_Length - 1))));
      end Build_Token;

   begin
      Lexer.Prev_ID := Lexer.ID;
      loop
         declare
            Status : constant int := Next_Token
              (Lexer.Lexer, Lexer.ID,
               Byte_Position => Interfaces.C.size_t (Lexer.Byte_Position),
               Byte_Length   => Interfaces.C.size_t (Lexer.Byte_Length),
               Char_Position => Interfaces.C.size_t (Lexer.Char_Position),
               Char_Length   => Interfaces.C.size_t (Lexer.Char_Length),
               Line_Start    => Interfaces.C.int (Lexer.Line));
         begin
            case Status is
            when 0 =>
               if Lexer.ID = Lexer.Descriptor.New_Line_ID then
                  Lexer.Char_Line_Start := Lexer.Char_Position + 1;
               end if;

               Build_Token;
               return False;

            when 1 =>
               --  Unrecognized character from lexer. Handle missing quotes by
               --  inserting a virtual quote at the existing quote, and telling the
               --  lexer to skip the char.
               declare
                  Buffer : constant GNATCOLL.Mmap.Str_Access := WisiToken.Lexer.Buffer (Lexer.Source);
               begin
                  if Trace_Parse > Lexer_Debug then
                     --  We don't have a visible Trace object here.
                     Ada.Text_IO.Put_Line ("lexer error char " & Buffer (Lexer.Byte_Position));
                  end if;

                  if Buffer (Lexer.Byte_Position) = ''' then
                     --  Lexer has read to next new-line (or eof), then backtracked to next
                     --  char after '.
                     Lexer.Errors.Append
                       ((To_Char_Pos (Lexer.Source, Lexer.Char_Position),
                         Invalid_Token_Index,
                         (1 => ''', others => ASCII.NUL)));

                     Lexer.ID := Lexer.Descriptor.String_1_ID;
                     Build_Token;
                     return True;

                  elsif Buffer (Lexer.Byte_Position) = '"' then
                     --  Lexer has read to next new-line (or eof), then backtracked to next
                     --  char after ".
                     Lexer.Errors.Append
                       ((To_Char_Pos (Lexer.Source, Lexer.Char_Position),
                         Invalid_Token_Index,
                         (1 => '"', others => ASCII.NUL)));

                     Lexer.ID := Lexer.Descriptor.String_2_ID;
                     Build_Token;
                     return True;

                  else
                     --  Just skip the character; call Next_Token again.
                     Lexer.Errors.Append
                       ((To_Char_Pos (Lexer.Source, Lexer.Char_Position), Invalid_Token_Index, (others => ASCII.NUL)));
                  end if;
               end;

            when others =>
               raise Fatal_Error with " lexer returned unrecognized status code" & int'Image (Status);
            end case;
         end;
      end loop;
   end Find_Next;

   overriding function First (Lexer : in Instance) return Boolean
   is begin
      return Lexer.Descriptor.New_Line_ID /= Invalid_Token_ID and then
           Lexer.Prev_ID = Lexer.Descriptor.New_Line_ID;
   end First;

   overriding function Buffer_Text (Lexer : in Instance; Byte_Bounds : in Buffer_Region) return String
   is
      First : constant Integer := Integer
        (Byte_Bounds.First - Lexer.Source.Buffer_Nominal_First_Byte + Buffer_Pos'First);
      Last  : constant Integer := Integer
        (Byte_Bounds.Last - Lexer.Source.Buffer_Nominal_First_Byte + Buffer_Pos'First);
   begin
      return String (Buffer (Lexer.Source) (First .. Last));
   end Buffer_Text;

   overriding function File_Name (Lexer : in Instance) return String
   is begin
      return File_Name (Lexer.Source);
   end File_Name;

end WisiToken.Lexer.re2c;
