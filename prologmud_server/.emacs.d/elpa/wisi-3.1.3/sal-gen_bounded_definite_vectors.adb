--  Abstract :
--
--  See spec.
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

package body SAL.Gen_Bounded_Definite_Vectors
  with Spark_Mode
is
   pragma Suppress (All_Checks);

   function Length (Container : in Vector) return Ada.Containers.Count_Type
   is (Ada.Containers.Count_Type (To_Peek_Index (Container.Last)));

   function Is_Full (Container : in Vector) return Boolean
   is begin
      return Length (Container) = Capacity;
   end Is_Full;

   procedure Clear (Container : in out Vector)
   is begin
      Container.Last := No_Index;
   end Clear;

   function Element (Container : Vector; Index : Index_Type) return Element_Type
   is (Container.Elements (Peek_Type (Index - Index_Type'First + 1)));

   procedure Replace_Element
     (Container : in out Vector;
      Index     : in     Index_Type;
      New_Item  : in     Element_Type)
   is begin
      Container.Elements (To_Peek_Index (Index)) := New_Item;
   end Replace_Element;

   function Last_Index (Container : Vector) return Extended_Index
   is (Container.Last);

   procedure Append (Container : in out Vector; New_Item : in Element_Type)
   is
      J : constant Peek_Type := To_Peek_Index (Container.Last + 1);
   begin
      Container.Elements (J) := New_Item;
      Container.Last := Container.Last + 1;
   end Append;

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type)
   is
      J : constant Peek_Type := Peek_Type (Container.Last + 1 - Index_Type'First + 1);
   begin
      Container.Elements (2 .. J) := Container.Elements (1 .. J - 1);
      Container.Elements (1) := New_Item;
      Container.Last := Container.Last + 1;
   end Prepend;

   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type;
      Before    : in     Extended_Index)
   is
      J : constant Peek_Type := To_Peek_Index ((if Before = No_Index then Container.Last + 1 else Before));
      K : constant Base_Peek_Type := To_Peek_Index (Container.Last);
   begin
      Container.Elements (J + 1 .. K + 1) := Container.Elements (J .. K);
      Container.Elements (J) := New_Item;
      Container.Last := Container.Last + 1;
   end Insert;

   function "+" (Item : in Element_Type) return Vector
   is begin
      return Result : Vector do
         Append (Result, Item);
      end return;
   end "+";

   function "&" (Left : in Vector; Right : in Element_Type) return Vector
   is begin
      --  WORKAROUND: If init Result with ":= Left", GNAT Community 2019
      --  checks Default_Initial_Condition (which fails when Left is not
      --  empty)! That is only supposed to be checked when initialized by
      --  default. Reported to AdaCore as ticket S724-042.
      return Result : Vector do
         Result := Left;
         Append (Result, Right);
      end return;
   end "&";

   procedure Delete_First (Container : in out Vector; Count : in Ada.Containers.Count_Type := 1)
   is
      use Ada.Containers;
   begin
      if Count = 0 then
         return;
      end if;

      declare
         New_Last : constant Extended_Index := Extended_Index (Integer (Container.Last) - Integer (Count));
         J        : constant Base_Peek_Type := Base_Peek_Type (Count);
         K        : constant Peek_Type      := To_Peek_Index (Container.Last);
      begin
         --  Delete items 1 .. J, shift remaining down.
         Container.Elements (1 .. K - J) := Container.Elements (J + 1 .. K);
         Container.Last := New_Last;
      end;
   end Delete_First;

end SAL.Gen_Bounded_Definite_Vectors;
