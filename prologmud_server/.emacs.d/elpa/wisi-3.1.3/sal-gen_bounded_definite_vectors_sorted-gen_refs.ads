--  Abstract :
--
--  Constant_Reference for parent.
--
--  In a child package because it's not Spark, and Spark does not
--  allow 'Spark_Mode => Off' on type declarations.
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

generic
package SAL.Gen_Bounded_Definite_Vectors_Sorted.Gen_Refs is

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Ref (Container : aliased Vector; Index : in Peek_Type) return Constant_Reference_Type with
     Inline;

private

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

end SAL.Gen_Bounded_Definite_Vectors_Sorted.Gen_Refs;
