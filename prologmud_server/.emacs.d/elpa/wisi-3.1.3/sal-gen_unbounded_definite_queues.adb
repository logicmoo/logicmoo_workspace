--  Abstract:
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
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body SAL.Gen_Unbounded_Definite_Queues is

   procedure Clear (Queue : in out Pkg.Queue)
   is begin
      Queue.Data.Clear;
   end Clear;

   function Count (Queue : in Pkg.Queue) return Base_Peek_Type
   is begin
      return Base_Peek_Type (Queue.Data.Length);
   end Count;

   function Is_Empty (Queue : in Pkg.Queue) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      return Queue.Data.Length = 0;
   end Is_Empty;

   function Remove (Queue : in out Pkg.Queue) return Element_Type
   is
      use Element_Lists;
   begin
      return A : constant Element_Type := Element (Queue.Data.First) do
         Queue.Data.Delete_First;
      end return;
   end Remove;

   procedure Drop (Queue : in out Pkg.Queue)
   is begin
      Queue.Data.Delete_First;
   end Drop;

   function Peek (Queue : in Pkg.Queue; N : Peek_Type := 1) return Constant_Reference_Type
   is
      use Ada.Containers;
      use Element_Lists;
      I : Cursor := Queue.Data.First;
   begin
      if Count_Type (N) > Queue.Data.Length then
         raise Parameter_Error;
      end if;

      for K in 2 .. N loop
         Next (I);
      end loop;

      return (Element => Element_Lists.Constant_Reference (Queue.Data, I).Element, Dummy => 1);
   end Peek;

   function Variable_Peek (Queue : in out Pkg.Queue; N : Peek_Type := 1) return Variable_Reference_Type
   is
      use Ada.Containers;
      use Element_Lists;
      I : Cursor := Queue.Data.First;
   begin
      if Count_Type (N) > Queue.Data.Length then
         raise Parameter_Error;
      end if;

      for K in 2 .. N loop
         Next (I);
      end loop;

      return (Element => Element_Lists.Variable_Reference (Queue.Data, I).Element, Dummy => 1);
   end Variable_Peek;

   procedure Add (Queue : in out Pkg.Queue; Item : in Element_Type)
   is begin
      Queue.Data.Append (Item);
   end Add;

   procedure Add_To_Head (Queue : in out Pkg.Queue; Item : in Element_Type)
   is begin
      Queue.Data.Prepend (Item);
   end Add_To_Head;

end SAL.Gen_Unbounded_Definite_Queues;
