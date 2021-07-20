--  Abstract :
--
--  Add "<" to parent
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
   with function Element_Compare (Left, Right : in Element_Type) return Compare_Result;
package SAL.Gen_Unbounded_Definite_Vectors.Gen_Comparable is

   type Vector is new SAL.Gen_Unbounded_Definite_Vectors.Vector with null record;

   function Compare (Left, Right : in Vector) return Compare_Result;
   --  Similar to Ada "<" for arrays; Ada Reference Manual
   --  section 4.5.2 para 26/3.

end SAL.Gen_Unbounded_Definite_Vectors.Gen_Comparable;
