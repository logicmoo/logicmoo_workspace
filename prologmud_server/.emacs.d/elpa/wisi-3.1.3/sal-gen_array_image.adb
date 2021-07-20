--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019 Free Software Foundation, Inc.
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
function SAL.Gen_Array_Image (Item : in Array_Type) return String
is
   Result : Unbounded_String := To_Unbounded_String ("(");
begin
   for I in Item'Range loop
      Result := Result & Element_Image (Item (I));
      if I = Item'Last then
         Result := Result & ")";
      else
         Result := Result & ", ";
      end if;
   end loop;
   return To_String (Result);
end SAL.Gen_Array_Image;
