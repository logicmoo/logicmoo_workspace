--  Abstract:
--
--  see spec
--
--  Copyright (C) 1998, 2003, 2009, 2015, 2017 - 2020 Free Software Foundation, Inc.
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
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body SAL.Gen_Unbounded_Definite_Stacks is

   ----------
   --  local subprogram bodies

   procedure Grow (Stack : in out Sguds.Stack; Desired_Size : in Base_Peek_Type)
   is
      New_Data : constant Element_Array_Access := new Element_Array (1 .. Desired_Size);
   begin
      New_Data (1 .. Stack.Top) := Stack.Data (1 .. Stack.Top);
      Free (Stack.Data);
      Stack.Data := New_Data;
   end Grow;

   ----------
   --  Spec visible subprograms
   overriding procedure Finalize (Stack : in out Sguds.Stack)
   is begin
      if Stack.Data /= null then
         Free (Stack.Data);
         Stack.Top := Invalid_Peek_Index;
      end if;
   end Finalize;

   overriding procedure Adjust (Stack : in out Sguds.Stack)
   is begin
      if Stack.Data /= null then
         Stack.Data := new Element_Array'(Stack.Data.all);
      end if;
   end Adjust;

   overriding
   function "=" (Left, Right : in Sguds.Stack) return Boolean
   is begin
      if Left.Data = null then
         return Right.Data = null;
      elsif Left.Top /= Right.Top then
         return False;
      else
         --  Assume stacks differ near top.
         for I in reverse 1 .. Left.Top loop
            if Left.Data (I) /= Right.Data (I) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   procedure Clear (Stack : in out Sguds.Stack)
   is begin
      --  We don't change the reserved capacity, on the assumption the
      --  stack will be used again.
      Stack.Top := 0;
   end Clear;

   function Depth (Stack : in Sguds.Stack) return Base_Peek_Type
   is begin
      return Stack.Top;
   end Depth;

   function Is_Empty (Stack : in Sguds.Stack) return Boolean
   is begin
      return Stack.Top = 0;
   end Is_Empty;

   function Peek
     (Stack : in Sguds.Stack;
      Index : in Peek_Type := 1)
     return Element_Type
   is begin
      return Stack.Data (Stack.Top - Index + 1);
   end Peek;

   procedure Pop (Stack : in out Sguds.Stack; Count : in Base_Peek_Type := 1)
   is begin
      if Stack.Top < Count then
         raise Container_Empty;
      else
         Stack.Top := Stack.Top - Count;
      end if;
   end Pop;

   function Pop (Stack : in out Sguds.Stack) return Element_Type
   is begin
      if Stack.Top = 0 then
         raise Container_Empty;
      else
         return Result : constant Element_Type := Stack.Peek (1)
         do
            Stack.Top := Stack.Top - 1;
         end return;
      end if;
   end Pop;

   procedure Push (Stack : in out Sguds.Stack; Item : in Element_Type)
   is begin
      if Stack.Data = null then
         --  Adding a generic parameter for a reasonably large default initial
         --  size here makes Wisitoken McKenzie recover slightly slower,
         --  presumably due to increased cache thrashing.
         Stack.Data := new Element_Array (1 .. 2);
      elsif Stack.Top = Stack.Data'Last then
         Grow (Stack, Desired_Size => 2 * Stack.Data'Last);
      end if;
      Stack.Top := Stack.Top + 1;
      Stack.Data (Stack.Top) := Item;
   end Push;

   function Top (Stack : in Sguds.Stack) return Element_Type
   is begin
      if Stack.Top < 1 then
         raise SAL.Container_Empty;
      else
         return Peek (Stack, 1);
      end if;
   end Top;

   procedure Set_Depth
     (Stack : in out Sguds.Stack;
      Depth : in     Peek_Type)
   is begin
      if Stack.Data = null then
         Stack.Data := new Element_Array (1 .. 2 * Depth);
      elsif Depth > Stack.Data'Last then
         Grow (Stack, Desired_Size => 2 * Depth);
      end if;
   end Set_Depth;

   procedure Set
     (Stack   : in out Sguds.Stack;
      Index   : in     Peek_Type;
      Depth   : in     Peek_Type;
      Element : in     Element_Type)
   is begin
      --  Same Position algorithm as in Peek
      Stack.Top := Depth;
      Stack.Data (Depth - Index + 1) := Element;
   end Set;

   function Constant_Reference
     (Container : aliased in Stack'Class;
      Position  :         in Peek_Type)
     return Constant_Reference_Type
   is begin
      return
        (Element => Container.Data (Container.Top - Position + 1)'Access,
         Dummy => 1);
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased in Stack'Class;
      Position  :         in Cursor)
     return Constant_Reference_Type
   is begin
      return
        (Element => Container.Data (Container.Top - Position.Ptr + 1)'Access,
         Dummy => 1);
   end Constant_Reference;

   function Has_Element (Position : in Cursor) return Boolean
   is begin
      return Position.Container.Depth >= Position.Ptr;
   end Has_Element;

   type Iterator (Container : not null access constant Stack) is new Iterator_Interfaces.Forward_Iterator with
   null record;

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   function Iterate (Container : aliased in Stack) return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   overriding function First (Object : Iterator) return Cursor
   is begin
      return (Object.Container, 1);
   end First;

   overriding function Next (Object : in Iterator; Position : in Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return (Position.Container, Position.Ptr + 1);
   end Next;

end SAL.Gen_Unbounded_Definite_Stacks;
