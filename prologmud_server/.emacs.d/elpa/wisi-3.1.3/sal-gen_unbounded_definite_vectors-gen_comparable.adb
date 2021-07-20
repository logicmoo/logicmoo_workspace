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

package body SAL.Gen_Unbounded_Definite_Vectors.Gen_Comparable is

   function Compare (Left, Right : in Vector) return Compare_Result
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Left.Length = 0 then
         if Right.Length = 0 then
            return Equal;
         else
            --  null is less than non-null
            return Less;
         end if;

      elsif Right.Length = 0 then
         return Greater;

      else
         declare
            I : Base_Peek_Type := To_Peek_Type (Left.First);
            J : Base_Peek_Type := To_Peek_Type (Right.First);

            Left_Last  : constant Base_Peek_Type := To_Peek_Type (Left.Last);
            Right_Last : constant Base_Peek_Type := To_Peek_Type (Right.Last);
         begin
            loop
               exit when I > Left_Last or J > Right_Last;

               case Element_Compare (Left.Elements (I), Right.Elements (J)) is
               when Less =>
                  return Less;
               when Equal =>
                  I := I + 1;
                  J := J + 1;
               when Greater =>
                  return Greater;
               end case;
            end loop;
            if I > Left_Last then
               if J > Right_Last then
                  return Equal;
               else
                  --  right is longer
                  return Less;
               end if;
            else
               --  left is longer
               return Greater;
            end if;
         end;
      end if;
   end Compare;

end SAL.Gen_Unbounded_Definite_Vectors.Gen_Comparable;
