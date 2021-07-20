--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019 Free Software Foundation, Inc.
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
function SAL.Gen_Bounded_Definite_Vectors.Gen_Image (Item : in Vector) return String
is
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   Result : Unbounded_String        := To_Unbounded_String ("(");
   Last   : constant Base_Peek_Type := To_Peek_Index (Item.Last);
begin
   for I in 1 .. Last loop
      Result := Result &
        (if Trim
         then Fixed.Trim (Element_Image (Item.Elements (I)), Left)
         else Element_Image (Item.Elements (I)));
      if I /= Last then
         Result := Result & ", ";
      end if;
   end loop;
   Result := Result & ")";
   return To_String (Result);
end SAL.Gen_Bounded_Definite_Vectors.Gen_Image;
