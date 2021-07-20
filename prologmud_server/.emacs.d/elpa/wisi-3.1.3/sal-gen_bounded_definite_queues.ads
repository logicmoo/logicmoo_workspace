--  Abstract:
--
--  A generic queue, allowing definite non-limited item types.
--
--  Copyright (C) 2004, 2008, 2009, 2011, 2017, 2019 Free Software Foundation  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

generic
   type Item_Type is private;
package SAL.Gen_Bounded_Definite_Queues
  with Spark_Mode
is
   pragma Pure;
   pragma Suppress (All_Checks); --  Users must check Is_Full before Add, Is_Empty before Remove.

   subtype Size_Type is Peek_Type range 1 .. Peek_Type'Last / 2;
   --  The upper limit is needed to avoid overflow in Peek.

   type Queue_Type (Size : Size_Type) is private;
   --  Size is maximum number of items in the queue.

   procedure Clear (Queue : in out Queue_Type)
   with Post => Count (Queue) = 0;
   --  Empty Queue of all items.

   function Count (Queue : in Queue_Type) return Base_Peek_Type;
   --  Returns count of items in the Queue

   function Is_Empty (Queue : in Queue_Type) return Boolean
   with Post => Is_Empty'Result = (Count (Queue) = 0);
   --  Returns true if no items are in Queue.

   function Is_Full (Queue : in Queue_Type) return Boolean
   with Post => Is_Full'Result = (Count (Queue) = Queue.Size);
   --  Returns true if Queue is full.

   procedure Remove (Queue : in out Queue_Type; Item : out Item_Type) with
     Pre  => Count (Queue) > 0,
     Post => Count (Queue) = Count (Queue)'Old - 1 and Item = Peek (Queue'Old) and
             (for all I in 1 .. Count (Queue) => Peek (Queue'Old, I + 1) = Peek (Queue, I));
   --  Remove head item from Queue, return it.

   function Remove (Queue : in out Queue_Type) return Item_Type with
     Spark_Mode => Off;

   function Get (Queue : in out Queue_Type) return Item_Type renames Remove;

   procedure Drop (Queue : in out Queue_Type) with
     Pre  => Count (Queue) > 0,
     Post => Count (Queue) = Count (Queue)'Old - 1 and
             (for all I in 1 .. Count (Queue) => Peek (Queue'Old, I + 1) = Peek (Queue, I));
   --  Remove head item from Queue, discard it.

   function Peek (Queue : in Queue_Type; N : Peek_Type := 1) return Item_Type with
     Pre  => Count (Queue) in 1 .. Queue.Size and N in 1 .. Count (Queue);
   --  Return a copy of a queue item, without removing it. N = 1 is
   --  the queue head.

   procedure Add (Queue : in out Queue_Type; Item : in Item_Type) with
     Pre  => Count (Queue) in 0 .. Queue.Size - 1,
     Post => Count (Queue) = Count (Queue)'Old + 1 and Peek (Queue, Count (Queue)) = Item and
             (for all I in 1 .. Count (Queue)'Old => Peek (Queue'Old, I) = Peek (Queue, I));
   --  Add Item to the tail of Queue.

   procedure Put (Queue : in out Queue_Type; Item : in Item_Type) renames Add;

   procedure Add_To_Head (Queue : in out Queue_Type; Item : in Item_Type) with
     Pre  => Count (Queue) in 0 .. Queue.Size - 1,
     Post => Count (Queue) = Count (Queue)'Old + 1 and
             (Peek (Queue) = Item and
              (for all I in 2 .. Count (Queue) => Peek (Queue'Old, I - 1) = Peek (Queue, I)));
   --  Add Item to the head of Queue.

private

   type Item_Array_Type is array (Peek_Type range <>) of Item_Type;

   type Queue_Type (Size : Size_Type) is
   record
      Head  : Peek_Type      := 1;
      Tail  : Peek_Type      := 1;
      Count : Base_Peek_Type := 0;
      Data  : Item_Array_Type (1 .. Size);
      --  Add at Tail + 1, remove at Head. Count is current count;
      --  easier to keep track of that than to compute Is_Empty for
      --  each Add and Remove.
      --
      --  Empty is indicated by Count = 0; head and tail are arbitrary
      --  in that case.
   end record with
     Type_Invariant =>
       (Head in 1 .. Size and
        Tail in 1 .. Size and
        Count in 0 .. Size) and then
       (Count = 0 or else Wrap (Size, Head + Count - 1) = Tail);

   function Wrap (Size : in Size_Type; I : in Base_Peek_Type) return Peek_Type
     is (if I > Size then I - Size
         elsif I = 0 then Size
         else I)
     with
       Pre  => I in 0 .. 2 * Size - 1,
       Post => Wrap'Result in 1 .. Size;

end SAL.Gen_Bounded_Definite_Queues;
