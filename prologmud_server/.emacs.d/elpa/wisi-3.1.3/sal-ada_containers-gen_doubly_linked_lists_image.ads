--  Abstract :
--
--  Image for normal Ada array types
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
with Ada.Containers.Doubly_Linked_Lists;
generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with package Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type, "=");
   with function Element_Image (Item : in Element_Type) return String;
function SAL.Ada_Containers.Gen_Doubly_Linked_Lists_Image
  (Item   : in Lists.List;
   Strict : in Boolean := False)
  return String;
