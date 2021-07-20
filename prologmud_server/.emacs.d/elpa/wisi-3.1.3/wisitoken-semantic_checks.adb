--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
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

with Ada.Characters.Handling;
package body WisiToken.Semantic_Checks is

   function Image (Item : in Check_Status; Descriptor : in WisiToken.Descriptor) return String
   is begin
      case Item.Label is
      when Ok =>
         return Check_Status_Label'Image (Item.Label);
      when Error =>
         return '(' & Check_Status_Label'Image (Item.Label) & ", " &
           Image (Item.Begin_Name, Descriptor) & ',' &
           Image (Item.End_Name, Descriptor) & ')';
      end case;
   end Image;

   function Match_Names
     (Lexer        : access constant WisiToken.Lexer.Instance'Class;
      Descriptor   : in     WisiToken.Descriptor;
      Tokens       : in     Recover_Token_Array;
      Start_Index  : in     Positive_Index_Type;
      End_Index    : in     Positive_Index_Type;
      End_Optional : in     Boolean)
     return Check_Status
   is
      Start_Name_Region : constant Buffer_Region :=
        (if Tokens (Start_Index).Name = Null_Buffer_Region
         then Tokens (Start_Index).Byte_Region
         else Tokens (Start_Index).Name);
      End_Name_Region : constant Buffer_Region :=
        (if Tokens (End_Index).Name = Null_Buffer_Region
         then Tokens (End_Index).Byte_Region
         else Tokens (End_Index).Name);

      function Equal return Boolean
      is
         use Ada.Characters.Handling;
      begin
         if Descriptor.Case_Insensitive then
            return To_Lower (Lexer.Buffer_Text (Start_Name_Region)) =
              To_Lower (Lexer.Buffer_Text (End_Name_Region));
         else
            return Lexer.Buffer_Text (Start_Name_Region) = Lexer.Buffer_Text (End_Name_Region);
         end if;
      end Equal;

   begin
      if Tokens (Start_Index).Virtual or Tokens (End_Index).Virtual then
         return (Label => Ok);

      elsif End_Optional then
         if End_Name_Region = Null_Buffer_Region then
            return (Label => Ok);
         elsif Start_Name_Region = Null_Buffer_Region then
            return (Extra_Name_Error, Tokens (Start_Index), Tokens (End_Index));
         else
            if Equal then
               return (Label => Ok);
            else
               return (Match_Names_Error, Tokens (Start_Index), Tokens (End_Index));
            end if;
         end if;

      else
         if Start_Name_Region = Null_Buffer_Region then
            if End_Name_Region = Null_Buffer_Region then
               return (Label => Ok);
            else
               return (Extra_Name_Error, Tokens (Start_Index), Tokens (End_Index));
            end if;

         elsif End_Name_Region = Null_Buffer_Region then
            return (Missing_Name_Error, Tokens (Start_Index), Tokens (End_Index));

         else
            if Equal then
               return (Label => Ok);
            else
               return (Match_Names_Error, Tokens (Start_Index), Tokens (End_Index));
            end if;
         end if;
      end if;
   end Match_Names;

   function Propagate_Name
     (Nonterm    : in out Recover_Token;
      Tokens     : in     Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Check_Status
   is begin
      if Tokens (Name_Index).Name = Null_Buffer_Region then
         Nonterm.Name := Tokens (Name_Index).Byte_Region;
      else
         Nonterm.Name := Tokens (Name_Index).Name;
      end if;
      return (Label => Ok);
   end Propagate_Name;

   function Merge_Names
     (Nonterm     : in out Recover_Token;
      Tokens      : in     Recover_Token_Array;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Check_Status
   is
      First_Name : Buffer_Region renames Tokens (First_Index).Name;
      Last_Name  : Buffer_Region renames Tokens (Last_Index).Name;
   begin
      Nonterm.Name :=
        First_Name and
          (if Last_Name = Null_Buffer_Region
           then Tokens (Last_Index).Byte_Region
           else Last_Name);
      return (Label => Ok);
   end Merge_Names;

   function Terminate_Partial_Parse
     (Partial_Parse_Active    : in Boolean;
      Partial_Parse_Byte_Goal : in Buffer_Pos;
      Recover_Active          : in Boolean;
      Nonterm                 : in Recover_Token)
     return Check_Status
   is begin
      if Partial_Parse_Active and then
        (not Recover_Active) and then
        Nonterm.Byte_Region.Last >= Partial_Parse_Byte_Goal
      then
         raise WisiToken.Partial_Parse;
      else
         return (Label => Ok);
      end if;
   end Terminate_Partial_Parse;

end WisiToken.Semantic_Checks;
