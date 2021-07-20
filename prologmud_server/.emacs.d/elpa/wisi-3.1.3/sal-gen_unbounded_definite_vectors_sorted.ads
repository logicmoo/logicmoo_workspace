--  Abstract :
--
--  A simple unbounded sorted vector of definite items.
--
--  Copyright (C) 2019 - 2020 Free Software Foundation, Inc.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
   type Key_Type is private;
   with function To_Key (Item : in Element_Type) return Key_Type;
   with function Key_Compare (Left, Right : in Key_Type) return Compare_Result;
package SAL.Gen_Unbounded_Definite_Vectors_Sorted is

   use all type Ada.Containers.Count_Type;

   type Vector is new Ada.Finalization.Controlled with private with
      Constant_Indexing => Constant_Ref,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   Empty_Vector : constant Vector;

   overriding procedure Finalize (Container : in out Vector);
   overriding procedure Adjust (Container : in out Vector);

   procedure Clear (Container : in out Vector)
   renames Finalize;

   function Length (Container : in Vector) return Ada.Containers.Count_Type;
   function Capacity (Container : in Vector) return Ada.Containers.Count_Type;

   procedure Set_Capacity
     (Container : in out Vector;
      Length    : in     Ada.Containers.Count_Type);
   --  Allocates uninitialized memory; does not change Container.First,
   --  Container.Last.

   function "&" (Left, Right : in Element_Type) return Vector;
   function "&" (Left : in Vector; Right : in Element_Type) return Vector;

   function Contains (Container : in Vector; Key : in Key_Type) return Boolean;

   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type);
   --  Insert New_Item in sorted position. Items are sorted in increasing
   --  order according to Element_Compare.
   --
   --  Raises Duplicate_Key if To_Key (New_Item) is already in Container.

   type Find_Reference_Type (Element : access Element_Type) is private with
     Implicit_Dereference => Element;

   function Find
     (Container : aliased in Vector;
      Key       :         in Key_Type)
     return Find_Reference_Type;
   --  Result.Element is null if Key not in Container. User must not modify Key.

   type Find_Reference_Constant_Type (Element : access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Find_Constant
     (Container : aliased in Vector;
      Key       : in Key_Type)
     return Find_Reference_Constant_Type;
   --  Result.Element is null if Key not in Container.

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : aliased in Vector) return Iterator_Interfaces.Reversible_Iterator'Class;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Ref (Container : aliased Vector; Position : in Cursor) return Constant_Reference_Type
   with Inline,
     Pre => To_Index (Position) in Container.First_Index .. Container.Last_Index;

   function First_Index (Container : in Vector) return Peek_Type is (Peek_Type'First);
   function Last_Index (Container : in Vector) return Base_Peek_Type
   with Inline;
   function To_Index (Position : in Cursor) return Base_Peek_Type;
   function Constant_Ref (Container : aliased Vector; Index : in Peek_Type) return Constant_Reference_Type
   with Inline,
     Pre => Index in Container.First_Index .. Container.Last_Index;

private

   type Array_Type is array (SAL.Peek_Type range <>) of aliased Element_Type;
   type Array_Access is access Array_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Array_Type, Array_Access);

   No_Index : constant Base_Peek_Type := 0;

   type Vector is new Ada.Finalization.Controlled with
   record
      Elements : Array_Access;
      --  Elements may be non-null with First = No_Index, after
      --  Set_Capacity. If First /= No_Index and Last >= First, Elements /=
      --  null.
      Last     : Base_Peek_Type := No_Index;
   end record;

   type Vector_Access is access constant Vector;
   for Vector_Access'Storage_Size use 0;

   type Cursor is record
      Index : Base_Peek_Type := No_Index;
   end record;

   type Iterator (Container : not null access constant Vector) is new Iterator_Interfaces.Reversible_Iterator
     with null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   type Find_Reference_Type (Element : access Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Find_Reference_Constant_Type (Element : access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   Empty_Vector : constant Vector := (Ada.Finalization.Controlled with others => <>);

   No_Element : constant Cursor := (others => <>);

   ----------
   --  Visible for child package

   procedure Grow (Elements : in out Array_Access; Index : in Base_Peek_Type);

end SAL.Gen_Unbounded_Definite_Vectors_Sorted;
