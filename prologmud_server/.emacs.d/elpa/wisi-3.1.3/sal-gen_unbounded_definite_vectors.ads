--  Abstract :
--
--  A simple unbounded vector of definite items, intended to be faster
--  than Ada.Containers.Vectors.
--
--  Prepend is as fast (in amortized time) as Append.
--
--  It provides no checking of cursor tampering; higher level code
--  must ensure that.
--
--  Design:
--
--  See ARM 3.10.2 "explicitly aliased" for why we need 'aliased' in
--  several subprogram argument modes, and why Container must be an
--  access discriminant in Cursor and Iterator.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Index_Type is range <>;
   type Element_Type is private;
   Default_Element : in Element_Type;
package SAL.Gen_Unbounded_Definite_Vectors is

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
           Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   type Vector is new Ada.Finalization.Controlled with private with
     Constant_Indexing => Constant_Ref,
     Variable_Indexing => Variable_Ref,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   Empty_Vector : constant Vector;

   overriding procedure Finalize (Container : in out Vector);
   overriding procedure Adjust (Container : in out Vector);

   overriding function "=" (Left, Right : in Vector) return Boolean is
     (raise Programmer_Error);
   --  Use Gen_Comparable child.

   function Length (Container : in Vector) return Ada.Containers.Count_Type;
   function Capacity (Container : in Vector) return Ada.Containers.Count_Type;

   procedure Set_Capacity
     (Container : in out Vector;
      First     : in     Index_Type;
      Last      : in     Extended_Index);
   --  Allocates memory, but does not change Container.First, Container.Last.

   procedure Clear (Container : in out Vector)
   renames Finalize;

   function First_Index (Container : Vector) return Extended_Index;
   --  No_Index + 1 when Container is empty, so "for I in C.First_Index
   --  .. C.Last_Index loop" works.
   --
   --  If you need No_Index for an empty Container, use To_Index (Container.First).

   function Last_Index (Container : Vector) return Extended_Index;
   --  No_Index when Container is empty.

   function Element (Container : Vector; Index : Index_Type) return Element_Type
   with Pre => Index >= Container.First_Index and Index <= Container.Last_Index;

   procedure Replace_Element (Container : Vector; Index : Index_Type; New_Item : in Element_Type);

   procedure Append (Container : in out Vector; New_Item : in Element_Type);
   --  Insert New_Item at end of Container.
   --
   --  Raises Constraint_Error if index of new item would be greater than
   --  Index_Type'Last.

   procedure Append (Container : in out Vector; New_Items : in Vector);
   --  Insert all elements of New_Items at end of Container.

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type);
   --  Insert New_Item at beginning of Container.
   --
   --  Raises Constraint_Error if index of new item would be less than
   --  Index_Type'First.

   procedure Prepend
     (Target       : in out Vector;
      Source       : in     Vector;
      Source_First : in     Index_Type;
      Source_Last  : in     Index_Type);
   --  Copy Source (Source_First .. Source_Last) to Target, before
   --  Target.First_Index.

   procedure Insert
     (Container : in out Vector;
      Element   : in     Element_Type;
      Before    : in     Index_Type);
   --  Existing elements at Before and after are slid to higher indices.

   procedure Merge
     (Target : in out Vector;
      Source : in out Vector);
   --  Copy all elements from Source to Target, to the same index range,
   --  deleting them from Source, and overwriting overlapping ranges.

   function To_Vector (Item : in Element_Type; Count : in Ada.Containers.Count_Type := 1) return Vector;

   function "+" (Element : in Element_Type) return Vector;

   function "&" (Left, Right : in Element_Type) return Vector;
   function "&" (Left : in Vector; Right : in Element_Type) return Vector;

   procedure Set_First_Last
     (Container : in out Vector;
      First     : in     Index_Type;
      Last      : in     Extended_Index);
   --  Elements in First .. Last that have not been set have
   --  Default_Element value.

   procedure Delete (Container : in out Vector; Index : in Index_Type);
   --  Replace Index element contents with default. If Index =
   --  Container.Last_Index, Container.Last_Index is decremented.

   function Contains (Container : in Vector; Element : in Element_Type) return Boolean;
   --  Return True if Element is in Container, False if not.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Ref (Container : aliased in Vector; Index : in Index_Type) return Constant_Reference_Type
   with Inline, Pre => Index in Container.First_Index .. Container.Last_Index;

   type Variable_Reference_Type (Element : not null access Element_Type) is private with
     Implicit_Dereference => Element;

   function Variable_Ref (Container : aliased in Vector; Index : in Index_Type) return Variable_Reference_Type
   with Inline, Pre => Index in Container.First_Index .. Container.Last_Index;

   type Cursor (<>) is private;

   function Has_Element (Position : Cursor) return Boolean;
   function Element (Position : Cursor) return Element_Type
   with Pre => Has_Element (Position);
   function First (Container : aliased in Vector) return Cursor;
   function Next (Position : in Cursor) return Cursor;
   procedure Next (Position : in out Cursor);
   function Prev (Position : in Cursor) return Cursor;
   procedure Prev (Position : in out Cursor);

   function To_Cursor
     (Container : aliased in Vector;
      Index     :         in Extended_Index)
     return Cursor
   with Pre => Index = No_Index or Index in Container.First_Index .. Container.Last_Index;

   function To_Index (Position : in Cursor) return Extended_Index;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : aliased in Vector) return Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Ref (Container : aliased in Vector; Position : in Cursor) return Constant_Reference_Type
   with Pre => Has_Element (Position) and then
               To_Index (Position) in Container.First_Index .. Container.Last_Index;

   function Variable_Ref (Container : aliased in Vector; Position  : in Cursor) return Variable_Reference_Type
   with Pre => Has_Element (Position) and then
               To_Index (Position) in Container.First_Index .. Container.Last_Index;
   pragma Inline (Variable_Ref);

private

   type Array_Type is array (SAL.Peek_Type range <>) of aliased Element_Type;
   type Array_Access is access Array_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Array_Type, Array_Access);

   type Vector is new Ada.Finalization.Controlled with
   record
      Elements : Array_Access;
      --  Elements may be non-null with First = No_Index, after
      --  Set_Capacity. If First /= No_Index and Last >= First, Elements /=
      --  null. First > Last means Vector is empty.
      First    : Extended_Index := No_Index;
      Last     : Extended_Index := No_Index;
   end record;

   type Vector_Access is access constant Vector;
   for Vector_Access'Storage_Size use 0;

   type Cursor (Container : not null access constant Vector) is
   record
      Index : Base_Peek_Type := Invalid_Peek_Index;
   end record;

   type Iterator (Container : not null access constant Vector) is new Iterator_Interfaces.Reversible_Iterator with
     null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Variable_Reference_Type (Element : not null access Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   Empty_Vector : constant Vector := (Ada.Finalization.Controlled with others => <>);

   ----------
   --  Visible for contracts/SPARK

   function Has_Element (Position : Cursor) return Boolean
     is (Position.Index /= Invalid_Peek_Index);

   ----------
   --  Visible for child package

   function To_Peek_Type (Item : in Extended_Index) return Base_Peek_Type with Inline;
   function To_Index_Type (Item : in Base_Peek_Type) return Extended_Index;

   procedure Grow (Elements : in out Array_Access; Index : in Base_Peek_Type);

end SAL.Gen_Unbounded_Definite_Vectors;
