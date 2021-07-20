--  Abstract :
--
--  A simple bounded vector of definite items, in Spark.
--
--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
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
   type Index_Type is range <>;
   type Element_Type is private;
   Capacity : in Ada.Containers.Count_Type;
package SAL.Gen_Bounded_Definite_Vectors
  with Spark_Mode
is
   use all type Ada.Containers.Count_Type;

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
           Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   pragma Assert (Capacity <= Ada.Containers.Count_Type (Index_Type'Last - Index_Type'First + 1));

   No_Index : constant Extended_Index := Extended_Index'First;

   type Vector is private with
     Default_Initial_Condition => Length (Vector) = 0;

   function Length (Container : in Vector) return Ada.Containers.Count_Type with
     Post => Length'Result in 0 .. Capacity;

   function Is_Full (Container : in Vector) return Boolean with
     Post => Is_Full'Result = (Length (Container) = Capacity);

   function Has_Space (Container : in Vector; Item_Count : in Ada.Containers.Count_Type) return Boolean
     is (Length (Container) + Item_Count <= Capacity)
     with Pre => Item_Count <= Ada.Containers.Count_Type'Last - Length (Container);

   procedure Clear (Container : in out Vector) with
     Post => Length (Container) = 0;

   function First_Index (Container : in Vector) return Index_Type is (Index_Type'First) with
       Depends => (First_Index'Result => null, null => Container);

   function Last_Index (Container : in Vector) return Extended_Index;
   --  No_Index when Container is empty.

   function Element (Container : in Vector; Index : in Index_Type) return Element_Type
   with Pre => Index <= Last_Index (Container);
   --  Index of first element in Vector is Index_Type'First.

   procedure Replace_Element
     (Container : in out Vector;
      Index     : in     Index_Type;
      New_Item  : in     Element_Type)
   with
     Pre  => Index <= Last_Index (Container),
     Post => Element (Container, Index) = New_Item;
   --  Index of first element in Vector is Index_Type'First.

   procedure Append (Container : in out Vector; New_Item : in Element_Type) with
     Pre  => Length (Container) < Capacity,
     Post => Length (Container) = Length (Container'Old) + 1 and
             Element (Container, Last_Index (Container)) = New_Item and
             (for all I in Index_Type'First .. Last_Index (Container) - 1 =>
                Element (Container'Old, I) = Element (Container, I));

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type) with
     Pre  => Length (Container) < Capacity,
     Post => Length (Container) = Length (Container'Old) + 1 and then
             (Element (Container, Index_Type'First) = New_Item and
              (for all I in Index_Type'First .. Last_Index (Container'Old) =>
                 Element (Container'Old, I) = Element (Container, I + 1)));
   --  Insert New_Item at beginning of Container; current elements slide right.

   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type;
      Before    : in     Extended_Index) with
     Pre  => Length (Container) < Capacity and Before <= Last_Index (Container),
     Contract_Cases =>
       (Before = No_Index =>
          Length (Container) = Length (Container'Old) + 1 and
          Element (Container, Last_Index (Container)) = New_Item and
          (for all I in Index_Type'First .. Last_Index (Container) - 1 =>
             Element (Container'Old, I) = Element (Container, I)),
        Before /= No_Index =>
          Length (Container) = Length (Container'Old) + 1 and
          Element (Container, Before) = New_Item and
             (for all I in Index_Type'First .. Before - 1 =>
                Element (Container'Old, I) = Element (Container, I)) and
             (for all I in Before + 1 .. Last_Index (Container) =>
                Element (Container'Old, I - 1) = Element (Container, I)));
   --  Insert New_Item before Before, or after Last_Index if Before is
   --  No_Index. Current elements at Before and after slide right.
   --  New_Item then has index Before.

   function "+" (Item : in Element_Type) return Vector with
     Post => Length ("+"'Result) = 1 and
             Element ("+"'Result, Index_Type'First) = Item;

   function "&" (Left : in Vector; Right : in Element_Type) return Vector with
     Pre  => Length (Left) < Capacity,
     Post => Length ("&"'Result) = Length (Left) + 1 and
             (for all I in Index_Type'First .. Last_Index (Left) => Element (Left, I) = Element ("&"'Result, I)) and
             Element ("&"'Result, Last_Index ("&"'Result)) = Right;

   procedure Delete_First (Container : in out Vector; Count : in Ada.Containers.Count_Type := 1) with
     Pre  => Length (Container) >= Count,
     Post => Length (Container) = Length (Container)'Old - Count and then
             (for all I in Index_Type'First .. Last_Index (Container) =>
                Element (Container'Old, Index_Type (Integer (I) + Integer (Count))) = Element (Container, I));
   --  Remaining elements slide down.

private

   type Array_Type is array (Peek_Type range 1 .. Peek_Type (Capacity)) of aliased Element_Type;

   type Vector is
   record
      Elements : Array_Type;
      Last     : Extended_Index := No_Index;
   end record with
     Type_Invariant => To_Peek_Index (Last) <= Elements'Last;
   pragma Annotate (GNATprove, Intentional, "type ""Vector"" is not fully initialized",
                    "Only items in Elements with index < Last are accessed");

   ----------
   --  For child units

   function To_Peek_Index (Index : in Extended_Index) return Base_Peek_Type is
     (Base_Peek_Type (Index - Index_Type'First + 1));

end SAL.Gen_Bounded_Definite_Vectors;
