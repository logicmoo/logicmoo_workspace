--  Abstract:
--
--  Stack implementation.
--
--  Copyright (C) 1998-2000, 2002-2003, 2009, 2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
package SAL.Gen_Unbounded_Definite_Stacks is

   package Sguds renames SAL.Gen_Unbounded_Definite_Stacks;

   type Stack is new Ada.Finalization.Controlled with private
   with
     Constant_Indexing => Constant_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   Empty_Stack : constant Stack;

   overriding procedure Finalize (Stack : in out Sguds.Stack);
   overriding procedure Adjust (Stack : in out Sguds.Stack);

   overriding function "=" (Left, Right : in Sguds.Stack) return Boolean;

   procedure Clear (Stack : in out Sguds.Stack);
   --  Empty Stack of all items.

   function Depth (Stack : in Sguds.Stack) return Base_Peek_Type;
   --  Returns current count of items in the Stack

   function Is_Empty (Stack : in Sguds.Stack) return Boolean;
   --  Returns true iff no items are in Stack.

   function Peek
     (Stack : in Sguds.Stack;
      Index : in Peek_Type := 1)
     return Element_Type with Inline;
   --  Return the Index'th item from the top of Stack; the Item is _not_ removed.
   --  Top item has index 1.
   --
   --  Raises Constraint_Error if Index > Depth.
   --
   --  See also Constant_Ref, implicit indexing

   procedure Pop (Stack : in out Sguds.Stack; Count : in Base_Peek_Type := 1);
   --  Remove Count Items from the top of Stack, discard them.
   --
   --  Raises Container_Empty if there are fewer than Count items on
   --  Stack.

   function Pop (Stack : in out Sguds.Stack) return Element_Type;
   --  Remove Item from the top of Stack, and return it.
   --
   --  Raises Container_Empty if Is_Empty.

   procedure Push (Stack : in out Sguds.Stack; Item : in Element_Type);
   --  Add Item to the top of Stack.
   --
   --  May raise Container_Full.

   function Top (Stack : in Sguds.Stack) return Element_Type;
   --  Return the item at the top of Stack; the Item is _not_ removed.
   --  Same as Peek (Stack, 1).
   --
   --  Raises Container_Empty if Is_Empty.

   procedure Set_Depth
     (Stack : in out Sguds.Stack;
      Depth : in     Peek_Type);
   --  Empty Stack, set its Depth to Depth. Must be followed by Set
   --  for each element.
   --
   --  Useful when creating a stack from pre-existing data.

   procedure Set
     (Stack   : in out Sguds.Stack;
      Index   : in     Peek_Type;
      Depth   : in     Peek_Type;
      Element : in     Element_Type);
   --  Set a Stack element. Index is the same as Peek Index; Depth is
   --  used to compute the index in the underlying array.
   --
   --  Stack must have been initialized by Set_Depth.
   --
   --  Useful when creating a stack from pre-existing data.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in Stack'Class;
      Position  :         in Peek_Type)
     return Constant_Reference_Type
   with Inline, Pre => Position in 1 .. Container.Depth;

   type Cursor (<>) is private;

   function Constant_Reference
     (Container : aliased in Stack'Class;
      Position  :         in Cursor)
     return Constant_Reference_Type
   with Inline, Pre => Has_Element (Position);

   function Has_Element (Position : in Cursor) return Boolean;

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : aliased in Stack) return Iterator_Interfaces.Forward_Iterator'Class;

private

   type Element_Array is array (Peek_Type range <>) of aliased Element_Type;
   type Element_Array_Access is access Element_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);

   type Stack is new Ada.Finalization.Controlled with record
      Top  : Base_Peek_Type := Invalid_Peek_Index; -- empty
      Data : Element_Array_Access;

      --  Top of stack is at Data (Top).
      --  Data (1 .. Top) has been set at some point.
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   Empty_Stack : constant Stack := (Ada.Finalization.Controlled with Invalid_Peek_Index, null);

   type Cursor (Container : not null access constant Stack) is
   record
      Ptr : Peek_Type;
   end record;

end SAL.Gen_Unbounded_Definite_Stacks;
