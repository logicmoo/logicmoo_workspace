--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017, 2018 Free Software Foundation, Inc.
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

pragma License (GPL);

with Ada.Characters.Handling;
with WisiToken.Wisi_Ada;
package body WisiToken.Gen_Token_Enum is

   function Token_Enum_Image return Token_ID_Array_String
   is
      use Ada.Characters.Handling;
      Result : Token_ID_Array_String (Token_ID'First .. +Last_Nonterminal);
   begin
      for I in Token_Enum_ID loop
         if I <= Last_Terminal then
            Result (+I) := new String'(Token_Enum_ID'Image (I));
         else
            Result (+I) := new String'(To_Lower (Token_Enum_ID'Image (I)));
         end if;
      end loop;
      return Result;
   end Token_Enum_Image;

   function To_Syntax (Item : in Enum_Syntax) return WisiToken.Lexer.Regexp.Syntax
   is
      Result : WisiToken.Lexer.Regexp.Syntax (Token_ID'First .. +Last_Terminal);
   begin
      for I in Result'Range loop
         Result (I) := Item (-I);
      end loop;
      return Result;
   end To_Syntax;

   function "&" (Left, Right : in Token_Enum_ID) return Token_ID_Arrays.Vector
   is begin
      return Result : Token_ID_Arrays.Vector do
         Result.Append (+Left);
         Result.Append (+Right);
      end return;
   end "&";

   function "&"
     (Left  : in Token_ID_Arrays.Vector;
      Right : in Token_Enum_ID)
     return Token_ID_Arrays.Vector
   is begin
      return Result : Token_ID_Arrays.Vector := Left do
         Result.Append (+Right);
      end return;
   end "&";

   function "+"
     (Left  : in Token_Enum_ID;
      Right : in WisiToken.Syntax_Trees.Semantic_Action)
     return WisiToken.Productions.Right_Hand_Side
   is begin
      return WisiToken.Wisi_Ada."+" (+Left, Right);
   end "+";

   function "<="
     (Left  : in Token_Enum_ID;
      Right : in WisiToken.Productions.Right_Hand_Side)
     return WisiToken.Productions.Instance
   is begin
      return WisiToken.Wisi_Ada."<=" (+Left, Productions.RHS_Arrays.To_Vector (Right, 1));
   end "<=";

   function To_Nonterminal_Array_Token_Set
     (Item : in Nonterminal_Array_Token_Set)
     return WisiToken.Token_Array_Token_Set
   is
      Result : Token_Array_Token_Set :=
        (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal =>
           (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Nonterminal => False));
   begin
      for I in Item'Range (1) loop
         for J in Item'Range (2) loop
            Result (+I, +J) := Item (I, J);
         end loop;
      end loop;
      return Result;
   end To_Nonterminal_Array_Token_Set;

   function To_Nonterminal_Array_Terminal_Set
     (Item : in Nonterminal_Array_Terminal_Set)
     return WisiToken.Token_Array_Token_Set
   is
      Result : Token_Array_Token_Set :=
        (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal =>
           (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Terminal => False));
   begin
      for I in Item'Range (1) loop
         for J in Item'Range (2) loop
            Result (+I, +J) := Item (I, J);
         end loop;
      end loop;
      return Result;
   end To_Nonterminal_Array_Terminal_Set;

   function "+" (Item : in Token_Array) return WisiToken.Token_ID_Set
   is
      Result : Token_ID_Set := (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Terminal => False);
   begin
      for I in Item'Range loop
         Result (+Item (I)) := True;
      end loop;
      return Result;
   end "+";

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID_Set
   is begin
      return +Token_Array'(1 => Item);
   end "+";

begin
   LR1_Descriptor.Image := Token_Enum_Image;
   LALR_Descriptor.Image := LR1_Descriptor.Image;
end WisiToken.Gen_Token_Enum;
