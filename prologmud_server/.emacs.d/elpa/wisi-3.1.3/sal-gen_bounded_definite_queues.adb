--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2004, 2008, 2009, 2011, 2017, 2019 Free Software Foundation All Rights Reserved.
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

package body SAL.Gen_Bounded_Definite_Queues
  with Spark_Mode
is
   pragma Suppress (All_Checks);

   ----------
   --  Public subprograms

   procedure Clear (Queue : in out Queue_Type) is
   begin
      Queue.Count := 0;
   end Clear;

   function Count (Queue : in Queue_Type) return Base_Peek_Type is (Queue.Count);

   function Is_Empty (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = 0;
   end Is_Empty;

   function Is_Full (Queue : in Queue_Type) return Boolean is
   begin
      return Queue.Count = Queue.Size;
   end Is_Full;

   procedure Remove (Queue : in out Queue_Type; Item : out Item_Type)
   is begin
      Item := Queue.Data (Queue.Head);

      Queue.Count := Queue.Count - 1;

      if Queue.Count > 0 then
         Queue.Head := Wrap (Queue.Size, Queue.Head + 1);
      end if;
   end Remove;

   function Remove (Queue : in out Queue_Type) return Item_Type with
     Spark_Mode => Off
   is begin
      return Item : Item_Type  do
         Remove (Queue, Item);
      end return;
   end Remove;

   procedure Drop (Queue : in out Queue_Type)
   is begin
      Queue.Count := Queue.Count - 1;

      if Queue.Count > 0 then
         Queue.Head := Wrap (Queue.Size, Queue.Head + 1);
      end if;
   end Drop;

   function Peek (Queue : in Queue_Type; N : Peek_Type := 1) return Item_Type
     is (Queue.Data (Wrap (Queue.Size, Queue.Head + N - 1)));
   --  Expression function to allow use in Spark proofs of conditions in spec.

   procedure Add (Queue : in out Queue_Type; Item : in Item_Type) is
   begin
      if Queue.Count = 0 then
         Queue.Tail  := 1;
         Queue.Head  := 1;
         Queue.Count := 1;
      else
         Queue.Tail  := Wrap (Queue.Size, Queue.Tail + 1);
         Queue.Count := Queue.Count + 1;
      end if;
      Queue.Data (Queue.Tail) := Item;
   end Add;

   procedure Add_To_Head (Queue : in out Queue_Type; Item : in Item_Type) is
   begin
      if Queue.Count = 0 then
         Queue.Tail  := 1;
         Queue.Head  := 1;
         Queue.Count := 1;
      else
         Queue.Head  := Wrap (Queue.Size, Queue.Head - 1);
         Queue.Count := Queue.Count + 1;
      end if;
      Queue.Data (Queue.Head) := Item;
   end Add_To_Head;

end SAL.Gen_Bounded_Definite_Queues;
