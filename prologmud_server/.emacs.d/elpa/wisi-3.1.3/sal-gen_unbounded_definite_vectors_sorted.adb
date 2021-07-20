--  Abstract :
--
--  See spec.
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

package body SAL.Gen_Unbounded_Definite_Vectors_Sorted is

   ----------
   --  Body subprograms, arbitrary order

   procedure Grow (Elements : in out Array_Access; Index : in Base_Peek_Type)
   is
      --  Reallocate Elements so Elements (Index) is a valid element.

      Old_First  : constant Peek_Type := Elements'First;
      Old_Last   : constant Peek_Type := Elements'Last;
      New_First  : Peek_Type          := Old_First;
      New_Last   : Peek_Type          := Old_Last;
      New_Length : Peek_Type          := Elements'Length;

      New_Array : Array_Access;
   begin
      loop
         exit when New_First <= Index;
         New_Length := New_Length * 2;
         New_First  := Peek_Type'Max (Peek_Type'First, Old_Last - New_Length + 1);
      end loop;
      loop
         exit when New_Last >= Index;
         New_Length := New_Length * 2;
         New_Last   := Peek_Type'Min (Peek_Type'Last, New_First + New_Length - 1);
      end loop;

      New_Array := new Array_Type (New_First .. New_Last);

      --  We'd like to use this:
      --
      --  New_Array (New_First .. Old_First - 1) := (others => <>);
      --
      --  but that can overflow the stack, since the aggregate is allocated
      --  on the stack.

      for I in New_First .. Old_First - 1 loop
         New_Array (I .. I) := (others => <>);
      end loop;

      New_Array (Old_First .. Old_Last) := Elements.all;

      for I in Old_Last + 1 .. New_Last loop
         New_Array (I .. I)   := (others => <>);
      end loop;

      Free (Elements);
      Elements := New_Array;
   end Grow;

   procedure Find
     (Container : in     Vector;
      Key       : in     Key_Type;
      Found     :    out Boolean;
      At_After  :    out Base_Peek_Type)
   with Pre => Container.Last /= No_Index
   is
      --  If Found is True, item is at At_After. If False, item should be
      --  inserted after At_After.
      Low  : Base_Peek_Type := Peek_Type'First - 1;
      High : Base_Peek_Type := Container.Last + 1;
      I    : Base_Peek_Type := Low + High / 2;
   begin
      loop
         case Key_Compare (Key, To_Key (Container.Elements (I))) is
         when Less =>
            High := I;
            if I = Low then
               Found    := False;
               At_After := I;
               return;

            elsif I - 1 = Low then
               Found    := False;
               At_After := I - 1;
               return;

            else
               I := I - (I - Low) / 2;
            end if;

         when Equal =>
            Found    := True;
            At_After := I;
            return;

         when Greater =>
            Low := I;
            if I = High then
               Found    := False;
               At_After := I - 1;
               return;

            elsif I + 1 = High then
               Found    := False;
               At_After := I;
               return;

            else
               I := I + (High - I) / 2;
            end if;
         end case;
      end loop;
   end Find;

   ----------
   --  Public subprograms

   overriding procedure Finalize (Container : in out Vector)
   is begin
      Free (Container.Elements);
      Container.Last := No_Index;
   end Finalize;

   overriding procedure Adjust (Container : in out Vector)
   is begin
      if Container.Elements /= null then
         Container.Elements := new Array_Type'(Container.Elements.all);
      end if;
   end Adjust;

   function Length (Container : in Vector) return Ada.Containers.Count_Type
   is begin
      --  We assume the type ranges are sensible, so no exceptions occur
      --  here.
      if Container.Elements = null then
         return 0;
      else
         return Ada.Containers.Count_Type (Container.Last - Container.Elements'First + 1);
      end if;
   end Length;

   function Capacity (Container : in Vector) return Ada.Containers.Count_Type
   is begin
      if Container.Elements = null then
         return 0;
      else
         return Ada.Containers.Count_Type (Container.Elements'Length);
      end if;
   end Capacity;

   procedure Set_Capacity
     (Container : in out Vector;
      Length    : in     Ada.Containers.Count_Type)
   is
      First_Peek : constant Peek_Type      := Peek_Type'First;
      Last_Peek  : constant Base_Peek_Type := Base_Peek_Type (Length);
   begin
      if Length = 0 then
         return;
      elsif Container.Elements = null then
         Container.Elements := new Array_Type (First_Peek .. Last_Peek);
      else
         if First_Peek < Container.Elements'First then
            Grow (Container.Elements, First_Peek);
         end if;
         if Last_Peek < Container.Elements'Last then
            Grow (Container.Elements, Last_Peek);
         end if;
      end if;
   end Set_Capacity;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index /= Invalid_Peek_Index;
   end Has_Element;

   function "&" (Left, Right : in Element_Type) return Vector
   is begin
      return Result : Vector do
         Result.Insert (Left);
         Result.Insert (Right);
      end return;
   end "&";

   function "&" (Left : in Vector; Right : in Element_Type) return Vector
   is begin
      return Result : Vector := Left do
         Result.Insert (Right);
      end return;
   end "&";

   function Contains (Container : in Vector; Key : in Key_Type) return Boolean
   is
      Found : Boolean;
      I     : Base_Peek_Type;
   begin
      if Container.Last = No_Index then
         return False;
      end if;
      Find (Container, Key, Found, I);
      return Found;
   end Contains;

   procedure Insert
     (Container : in out Vector;
      New_Item  : in     Element_Type)
   is
      New_Key : constant Key_Type       := To_Key (New_Item);
      J       : constant Peek_Type      := Peek_Type'First;
      K       : constant Base_Peek_Type := Container.Last;
      I       : Base_Peek_Type          := K;
   begin
      if Container.Last = No_Index then
         Container.Last := Peek_Type'First;
         I              := Container.Last;

         if Container.Elements = null then
            Container.Elements := new Array_Type (I .. I);
            --  else Set_Capacity called.
         end if;
         Container.Elements (I) := New_Item;
         return;

      else
         Container.Last := Container.Last + 1;
      end if;

      pragma Assert (Container.Elements /= null);

      if I + 1 > Container.Elements'Last then
         Grow (Container.Elements, I + 1);
      end if;

      loop
         exit when I < J;

         case Key_Compare (New_Key, To_Key (Container.Elements (I))) is
         when Less =>
            --  Linear search is simple, we assume insert is used far less often
            --  than Find. And this is optimal when inserting in Key order.
            I :=  I - 1;
         when Equal =>
            --  Insert after I
            exit;
         when Greater =>
            --  Insert after I
            exit;
         end case;
      end loop;

      if I < J then
         --  Insert before all
         Container.Elements (J + 1 .. K + 1) := Container.Elements (J .. K);
         Container.Elements (J) := New_Item;
      else
         --  Insert after I
         Container.Elements (I + 2 .. K + 1) := Container.Elements (I + 1 .. K);
         Container.Elements (I + 1) := New_Item;
      end if;
   end Insert;

   function Find
     (Container : aliased in Vector;
      Key       :         in Key_Type)
     return Find_Reference_Type
   is
      Found : Boolean;
      I     : Base_Peek_Type;
   begin
      if Container.Last = No_Index then
         return (Element => null, Dummy => 1);
      end if;
      Find (Container, Key, Found, I);
      if Found then
         return (Element => Container.Elements (I)'Access, Dummy => 1);
      else
         return (Element => null, Dummy => 1);
      end if;
   end Find;

   function Find_Constant
     (Container : aliased in Vector;
      Key       :         in Key_Type)
     return Find_Reference_Constant_Type
   is
      Found : Boolean;
      I     : Base_Peek_Type;
   begin
      if Container.Last = No_Index then
         return (Element => null, Dummy => 1);
      end if;
      Find (Container, Key, Found, I);
      if Found then
         return (Element => Container.Elements (I)'Access, Dummy => 1);
      else
         return (Element => null, Dummy => 1);
      end if;
   end Find_Constant;

   overriding function First (Object : Iterator) return Cursor
   is begin
      if Object.Container.Elements = null then
         return (Index => Invalid_Peek_Index);
      else
         return (Index => Peek_Type'First);
      end if;
   end First;

   overriding function Last  (Object : Iterator) return Cursor
   is begin
      if Object.Container.Elements = null then
         return (Index => Invalid_Peek_Index);
      else
         return (Index => Object.Container.Last);
      end if;
   end Last;

   overriding function Next (Object : in Iterator; Position : in Cursor) return Cursor
   is begin
      if Position.Index = Object.Container.Last then
         return (Index => Invalid_Peek_Index);
      else
         return (Index => Position.Index + 1);
      end if;
   end Next;

   overriding function Previous (Object : in Iterator; Position : in Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      if Position.Index = Peek_Type'First then
         return (Index => Invalid_Peek_Index);
      else
         return (Index => Position.Index - 1);
      end if;
   end Previous;

   function Iterate (Container : aliased in Vector) return Iterator_Interfaces.Reversible_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   function Constant_Ref (Container : aliased Vector; Position : in Cursor) return Constant_Reference_Type
   is begin
      return (Element => Container.Elements (Position.Index)'Access, Dummy => 1);
   end Constant_Ref;

   function Last_Index (Container : in Vector) return Base_Peek_Type
   is begin
      return Container.Last;
   end Last_Index;

   function To_Index (Position : in Cursor) return Base_Peek_Type
   is begin
      return Position.Index;
   end To_Index;

   function Constant_Ref (Container : aliased Vector; Index : in Peek_Type) return Constant_Reference_Type
   is begin
      return (Element => Container.Elements (Index)'Access, Dummy => 1);
   end Constant_Ref;

end SAL.Gen_Unbounded_Definite_Vectors_Sorted;
