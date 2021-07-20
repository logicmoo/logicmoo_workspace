--  Abstract :
--
--  Image of parent.
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

generic
   with function Element_Image (Item : in Element_Type) return String;
function SAL.Gen_Unbounded_Definite_Vectors.Gen_Image (Item : in Vector; Strict : in Boolean := False) return String;
--  Image of Item, in Ada aggregate syntax. If Strict, use correct
--  syntax for 0 and 1 item; otherwise, use () and (item).
