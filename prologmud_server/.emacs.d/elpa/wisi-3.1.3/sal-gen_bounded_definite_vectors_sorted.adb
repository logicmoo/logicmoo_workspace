--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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

package body SAL.Gen_Bounded_Definite_Vectors_Sorted is

   function Length (Container : in Vector) return Ada.Containers.Count_Type
     is (Ada.Containers.Count_Type (Container.Last));

   function Is_Full (Container : in Vector) return Boolean
   is begin
      return Container.Last = Peek_Type (Capacity);
   end Is_Full;

   procedure Clear (Container : in out Vector)
   is begin
      Container.Last := No_Index;
   end Clear;

   function Last_Index (Container : in Vector) return Base_Peek_Type
     is (Container.Last);

   function Element (Container : in Vector; Index : in Peek_Type) return Element_Type
     is (Container.Elements (Index));

   procedure Insert
     (Container       : in out Vector;
      New_Item        : in     Element_Type;
      Ignore_If_Equal : in     Boolean := False)
   is
      K : constant Base_Peek_Type := Container.Last;
      J : Base_Peek_Type := K;
   begin
      if K = 0 then
         --  Container empty
         Container.Last := 1;
         Container.Elements (1) := New_Item;
         return;
      end if;

      loop
         --  These seem obvious, but gnatprove needs them (in 2019).
         pragma Loop_Invariant (J <= Container.Last);
         pragma Loop_Invariant (J <= Container.Elements'Last);
         pragma Loop_Variant (Decreases => J);

         --  This is less obvious, helps a lot.
         pragma Loop_Invariant
           ((for all I in J + 1 .. Container.Last => Element_Compare
             (New_Item, Container.Elements (I)) = Less));

         exit when J < 1;

         case Element_Compare (New_Item, Container.Elements (J)) is
         when Less =>
            J := J - 1;
         when Equal =>
            if Ignore_If_Equal then
               return;
            else
               --  Insert after J
               exit;
            end if;
         when Greater =>
            --  Insert after J
            exit;
         end case;
      end loop;

      --  Note that this assertion is _not_ a Loop_Invariant; the whole
      --  point here is to find the right J.
      pragma Assert
        (for all I in 1 .. J - 1 =>
           Element_Compare (Container.Elements (I), New_Item) in Less | Equal);

      Container.Elements (J + 2 .. K + 1) := Container.Elements (J + 1 .. K);
      Container.Elements (J + 1) := New_Item;
      Container.Last := Container.Last + 1;
   end Insert;

end SAL.Gen_Bounded_Definite_Vectors_Sorted;
