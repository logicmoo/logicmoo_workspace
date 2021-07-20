--  Abstract:
--
--  Bounded stack implementation, with full Spark verification,
--  optimized for speed.
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

generic
   type Element_Type is private;
package SAL.Gen_Bounded_Definite_Stacks
  with Spark_Mode
is
   pragma Pure;
   --  pragma Suppress (All_Checks); --  Users must check Is_Full before Push, Is_Empty before Pop etc.

   package Sgbds renames SAL.Gen_Bounded_Definite_Stacks;

   subtype Size_Type is Base_Peek_Type range 0 .. Base_Peek_Type'Last / 2;
   --  The upper limit is needed to avoid overflow in Peek.
   --  Zero included for Depth result.

   type Stack (Size : Size_Type) is tagged private;
   --  Tagged to allow Object.Method notation.

   --  No Empty_Stack constant, to avoid requiring a Default_Element.

   procedure Clear (Stack : in out Sgbds.Stack)
   with Post'Class => Depth (Stack) = 0;
   --  Empty Stack of all items.

   function Depth (Stack : in Sgbds.Stack) return Size_Type;
   --  Returns current count of items in Stack

   function Is_Empty (Stack : in Sgbds.Stack) return Boolean
   with Post'Class => Is_Empty'Result = (Depth (Stack) = 0);
   --  Returns true iff no items are in Stack.

   function Is_Full (Stack : in Sgbds.Stack) return Boolean
   with Post'Class => Is_Full'Result = (Depth (Stack) = Stack.Size);
   --  Returns true iff Stack is full.

   function Peek (Stack : in Sgbds.Stack; Index : in Peek_Type := 1) return Element_Type
   with Pre'Class  => Depth (Stack) in 1 .. Stack.Size and Index in 1 .. Depth (Stack);
   --  Return the Index'th item from the top of Stack; the Item is _not_ removed.
   --  Top item has index 1.

   procedure Pop (Stack : in out Sgbds.Stack; Count : in Base_Peek_Type := 1) with
     Pre'Class  => Depth (Stack) in 1 .. Stack.Size and Count in 0 .. Depth (Stack),
     Post'Class => Depth (Stack) = Depth (Stack)'Old - Count and then
                   (for all I in 1 .. Depth (Stack) => Peek (Stack'Old, I + Count) = Peek (Stack, I));
   --  Remove Count Items from the top of Stack, discard them.

   procedure Pop (Stack : in out Sgbds.Stack; Item : out Element_Type) with
     Pre'Class  => Depth (Stack) in 1 .. Stack.Size,
     Post'Class =>
       Depth (Stack) = Depth (Stack)'Old - 1 and then
       (Item = Peek (Stack'Old) and
        (for all I in 1 .. Depth (Stack) => Peek (Stack'Old, I + 1) = Peek (Stack, I)));
   --  Remove one item from the top of Stack, return in Item.

   function Pop (Stack : in out Sgbds.Stack) return Element_Type with
     Spark_Mode => Off;
   --  Remove one item from the top of Stack, and return it.

   procedure Push (Stack : in out Sgbds.Stack; Item : in Element_Type) with
     Pre'Class  => Depth (Stack) in 0 .. Stack.Size - 1,
     Post'Class =>
       Depth (Stack) = Depth (Stack)'Old + 1 and then
       (Item = Peek (Stack) and
        (for all I in 1 .. Depth (Stack'Old) => Peek (Stack'Old, I) = Peek (Stack, I + 1)));
   --  Add Item to the top of Stack.

private

   type Element_Array is array (Size_Type range <>) of aliased Element_Type;

   type Stack (Size : Size_Type) is tagged record
      Top  : Base_Peek_Type := Invalid_Peek_Index; -- empty
      Data : Element_Array (1 .. Size);
      --  Top of stack is at Data (Top).
      --  Data (1 .. Top) has been set at some point.
   end record with
     Dynamic_Predicate => Stack.Top in 0 .. Stack.Size;

end SAL.Gen_Bounded_Definite_Stacks;
