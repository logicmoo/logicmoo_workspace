--  Abstract:
--
--  An unbounded queue of definite non-limited elements.
--
--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
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

with SAL.Gen_Definite_Doubly_Linked_Lists;
generic
   type Element_Type is private;
package SAL.Gen_Unbounded_Definite_Queues is

   package Pkg renames SAL.Gen_Unbounded_Definite_Queues;

   type Queue is tagged private;

   Empty_Queue : constant Queue;

   procedure Clear (Queue : in out Pkg.Queue);
   --  Empty Queue.

   function Count (Queue : in Pkg.Queue) return Base_Peek_Type;
   --  Return count of items in the Queue

   function Length (Queue : in Pkg.Queue) return Base_Peek_Type renames Count;

   function Is_Empty (Queue : in Pkg.Queue) return Boolean;
   --  Return true if no items are in Queue.

   function Is_Full (Queue : in Pkg.Queue) return Boolean is (False);
   --  Return true if Queue is full.

   function Remove (Queue : in out Pkg.Queue) return Element_Type;
   --  Remove head/front item from Queue, return it.
   --
   --  Raise Container_Empty if Is_Empty.

   function Get (Queue : in out Pkg.Queue) return Element_Type renames Remove;

   procedure Drop (Queue : in out Pkg.Queue);
   --  Remove head/front item from Queue, discard it.
   --
   --  Raise Container_Empty if Is_Empty.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private
   with
      Implicit_Dereference => Element;

   function Peek (Queue : in Pkg.Queue; N : Peek_Type := 1) return Constant_Reference_Type;
   pragma Inline (Peek);
   --  Return a constant reference to a queue item. N = 1 is the queue
   --  head.
   --
   --  Raise Parameter_Error if N > Count

   type Variable_Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference => Element;

   function Variable_Peek (Queue : in out Pkg.Queue; N : Peek_Type := 1) return Variable_Reference_Type;
   pragma Inline (Variable_Peek);
   --  Return a variable reference to a queue item. N = 1 is the queue
   --  head.
   --
   --  Raises Parameter_Error if N > Count

   procedure Add (Queue : in out Pkg.Queue; Item : in Element_Type);
   --  Add Element to the tail/back of Queue.

   procedure Put (Queue : in out Pkg.Queue; Item : in Element_Type) renames Add;

   procedure Add_To_Head (Queue : in out Pkg.Queue; Item : in Element_Type);
   --  Add Element to the head/front of Queue.

private

   package Element_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Element_Type);

   --  We don't provide cursors or write access to queue elements, so we
   --  don't need any tampering checks.

   type Queue is tagged record
      Data : Element_Lists.List;
      --  Add at Tail/Back = Last, remove at Head/Front = First.
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Variable_Reference_Type (Element : not null access Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   Empty_Queue : constant Queue := (Data => Element_Lists.Empty_List);

end SAL.Gen_Unbounded_Definite_Queues;
