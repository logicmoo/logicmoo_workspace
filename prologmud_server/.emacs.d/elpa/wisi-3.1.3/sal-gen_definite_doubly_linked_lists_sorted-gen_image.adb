--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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

with Ada.Strings.Unbounded;
function SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_Image
  (Item : in List; Strict : in Boolean := False) return String
is
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   Result     : Unbounded_String := To_Unbounded_String ("(");
   Need_Comma : Boolean          := False;
begin
   if Strict and Item.Length = 0 then
      return "(1 .. 0 => <>)";

   elsif Strict and Item.Length = 1 then
      return "(1 => " & Element_Image (Element (Item.First)) & ")";

   else
      for El of Item loop
         if Need_Comma then
            Result := Result & ", ";
         else
            Need_Comma := True;
         end if;
         Result := Result & Element_Image (El);
      end loop;
      Result := Result & ")";
      return To_String (Result);
   end if;
end SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_Image;
