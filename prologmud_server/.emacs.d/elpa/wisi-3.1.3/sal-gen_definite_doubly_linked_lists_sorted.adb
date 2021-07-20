--  Abstract :
--
--  See spec.
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

package body SAL.Gen_Definite_Doubly_Linked_Lists_Sorted is

   ----------
   --  Body subprograms, alphabetical

   procedure Find
     (Container     : in     List;
      Element       : in     Element_Type;
      Found         :    out Node_Access;
      Found_Compare :    out Compare_Result)
   is
      --  Return pointer to first item in Container for which Compare (item,
      --  element) returns True or Greater. If no such element exists, Found
      --  is null, Found_Compare is Less.
      use Ada.Containers;
   begin
      if Container.Head = null then
         Found         := null;
         Found_Compare := Less;
         return;
      end if;

      declare
         Low_Index  : Count_Type  := 1;
         High_Index : Count_Type  := Container.Count;
         Next_Node  : Node_Access := Container.Head;
         Next_Index : Count_Type  := Low_Index;
         Old_Index  : Count_Type;
      begin
         loop
            Old_Index  := Next_Index;
            Next_Index := (Low_Index + High_Index) / 2;

            if Next_Index > Old_Index then
               for I in Old_Index + 1 .. Next_Index loop
                  Next_Node := Next_Node.Next;
               end loop;
            elsif Next_Index < Old_Index then
               for I in Next_Index .. Old_Index - 1 loop
                  Next_Node := Next_Node.Prev;
               end loop;
            end if;

            case Element_Compare (Next_Node.Element, Element) is
            when Less =>
               if Next_Index = High_Index then
                  --  no more nodes to check
                  Found         := null;
                  Found_Compare := Less;
                  return;
               elsif Next_Index = Low_Index then
                  --  force check of high_index
                  Low_Index := High_Index;
               else
                  Low_Index := Next_Index;
               end if;

            when Equal =>
               Found         := Next_Node;
               Found_Compare := Equal;
               return;

            when Greater =>
               if Low_Index = Next_Index then
                  --  no more nodes to check
                  Found         := Next_Node;
                  Found_Compare := Greater;
                  return;
               elsif High_Index = Next_Index then
                  --  Desired result is either high_index or low_index
                  pragma Assert (Low_Index + 1 = High_Index);
                  case Element_Compare (Next_Node.Prev.Element, Element) is
                  when Less =>
                     Found         := Next_Node;
                     Found_Compare := Greater;
                     return;
                  when Equal =>
                     Found         := Next_Node.Prev;
                     Found_Compare := Equal;
                     return;
                  when Greater =>
                     Found         := Next_Node.Prev;
                     Found_Compare := Greater;
                     return;
                  end case;
               else
                  High_Index := Next_Index;
               end if;
            end case;
         end loop;
      end;
   end Find;

   procedure Insert_Before
     (Container : in out List;
      Before    : in     Node_Access;
      Element   : in     Element_Type)
   is
      New_Node : constant Node_Access := new Node_Type'
        (Element => Element,
         Prev    => Before.Prev,
         Next    => Before);
   begin
      if Before = Container.Head then
         Before.Prev    := New_Node;
         Container.Head := New_Node;
      else
         Before.Prev.Next := New_Node;
         Before.Prev      := New_Node;
      end if;
   end Insert_Before;

   procedure Insert_After_Tail
     (Container : in out List;
      Element   : in     Element_Type)
   is
      New_Node : constant Node_Access := new Node_Type'
        (Element => Element,
         Prev    => Container.Tail,
         Next    => null);
   begin
      Container.Tail.Next := New_Node;
      Container.Tail      := New_Node;
   end Insert_After_Tail;

   ---------
   --  Public operations, declaration order.

   overriding
   procedure Adjust (Container : in out List)
   is
      Next_Source : Node_Access := Container.Head;
      New_Node    : Node_Access;
   begin
      if Next_Source = null then
         return;
      end if;

      Container.Tail := null;

      loop
         New_Node := new Node_Type'
           (Element => Next_Source.Element,
            Next    => null,
            Prev    => Container.Tail);
         if Container.Tail = null then
            Container.Head := New_Node;
            Container.Tail := New_Node;
         else
            Container.Tail.Next := New_Node;
            Container.Tail      := New_Node;
         end if;
         Next_Source := Next_Source.Next;
         exit when Next_Source = null;
      end loop;
   end Adjust;

   overriding
   procedure Finalize (Container : in out List)
   is
      Next : Node_Access := Container.Head;
   begin
      loop
         exit when Next = null;
         Next := Container.Head.Next;
         Free (Container.Head);
         Container.Head := Next;
      end loop;
      Container.Tail := null;
   end Finalize;

   overriding function "=" (Left, Right : in List) return Boolean
   is
      Left_I  : Node_Access := Left.Head;
      Right_I : Node_Access := Right.Head;
   begin
      loop
         exit when Left_I = null;

         if Right_I = null then
            return False;
         elsif Left_I.Element /= Right_I.Element then
            return False;
         end if;

         Left_I  := Left_I.Next;
         Right_I := Right_I.Next;
      end loop;
      return Right_I = null;
   end "=";

   function Length (Container : in List) return Ada.Containers.Count_Type
   is begin
      return Container.Count;
   end Length;

   function To_List (Element : in Element_Type) return List
   is
      New_Node : constant Node_Access := new Node_Type'
        (Element => Element,
         Prev    => null,
         Next    => null);
   begin
      return Result : constant List :=
        (Ada.Finalization.Controlled with
         Head  => New_Node,
         Tail  => New_Node,
         Count => 1);
   end To_List;

   procedure Insert (Container : in out List; Element : in Element_Type)
   is
      Node    : Node_Access := Container.Head;
      Compare : Compare_Result;
   begin
      if Node = null then
         Container := To_List (Element);
      else
         Find (Container, Element, Node, Compare);

         Container.Count := Container.Count + 1;

         if Node = null then
            Insert_After_Tail (Container, Element);
         else
            Insert_Before (Container, Node, Element);
         end if;
      end if;
   end Insert;

   function Contains (Container : in List; Element : in Element_Type) return Boolean
   is
      Node    : Node_Access := Container.Head;
      Compare : Compare_Result;
   begin
      Find (Container, Element, Node, Compare);
      return Compare = Equal;
   end Contains;

   procedure Merge
     (Target : in out List;
      Source : in     List;
      Added  :    out Boolean)
   is
      Target_I : Node_Access := Target.Head;
      Source_I : Node_Access := Source.Head;
   begin
      if Target_I = null then
         if Source_I = null then
            Added := False;
         else
            Target.Head  := Source.Head;
            Target.Tail  := Source.Tail;
            Target.Count := Source.Count;
            Adjust (Target);

            Added := True;
         end if;

      elsif Source_I = null then
         Added := False;

      else
         Added := False;
         loop
            exit when Source_I = null;

            if Target_I = null then
               Added := True;
               Target.Count := Target.Count + 1;
               Insert_After_Tail (Target, Source_I.Element);
               Source_I := Source_I.Next;

            else
               case Element_Compare (Target_I.Element, Source_I.Element) is
               when Greater =>
                  Added := True;
                  Target.Count := Target.Count + 1;
                  Insert_Before (Target, Target_I, Source_I.Element);
                  Source_I := Source_I.Next;

               when Equal =>
                  Target_I := Target_I.Next;
                  Source_I := Source_I.Next;

               when Less =>
                  Target_I := Target_I.Next;
               end case;
            end if;
         end loop;
      end if;
   end Merge;

   procedure Merge
     (Target  : in out List;
      Source  : in     List;
      Added   :    out Boolean;
      Exclude : in     Element_Type)
   is
      Target_I : Node_Access := Target.Head;
      Source_I : Node_Access := Source.Head;
   begin
      Added := False;

      if Target_I = null then
         if Source_I = null then
            return;
         else
            loop
               if Source_I = null then
                  return;
               end if;
               exit when Source_I.Element /= Exclude;
               Source_I := Source_I.Next;
            end loop;

            Added    := True;
            Target   := To_List (Source_I.Element);
            Source_I := Source_I.Next;
         end if;
      end if;

      loop
         exit when Source_I = null;

         if Source_I.Element = Exclude then
            Source_I := Source_I.Next;

         elsif Target_I = null then
            Added := True;
            Target.Count := Target.Count + 1;
            Insert_After_Tail (Target, Source_I.Element);
            Source_I := Source_I.Next;

         else
            case Element_Compare (Target_I.Element, Source_I.Element) is
            when Greater =>
               Added := True;
               Target.Count := Target.Count + 1;
               Insert_Before (Target, Target_I, Source_I.Element);
               Source_I := Source_I.Next;

            when Equal =>
               Target_I := Target_I.Next;
               Source_I := Source_I.Next;

            when Less =>
               Target_I := Target_I.Next;
            end case;
         end if;
      end loop;
   end Merge;

   function Has_Element (Position : in Cursor) return Boolean
   is begin
      return Position.Ptr /= null;
   end Has_Element;

   function First (Container : aliased in List) return Cursor
   is begin
      if Container.Head = null then
         return (Container'Access, null);
      else
         return (Container'Access, Container.Head);
      end if;
   end First;

   function Last (Container : aliased in List) return Cursor
   is begin
      if Container.Tail = null then
         return (Container'Access, null);
      else
         return (Container'Access, Container.Tail);
      end if;
   end Last;

   function Find (Container : aliased in List; Element : in Element_Type) return Cursor
   is
      Node    : Node_Access;
      Compare : Compare_Result;
   begin
      Find (Container, Element, Node, Compare);

      if Node = null then
         return (Container'Access, null);
      elsif Compare = Equal then
         return (Container'Access, Node);
      else
         return (Container'Access, null);
      end if;
   end Find;

   procedure Next (Position : in out Cursor)
   is begin
      if Position.Ptr /= null then
         if Position.Ptr.Next = null then
            Position.Ptr := null;
         else
            Position.Ptr := Position.Ptr.Next;
         end if;
      end if;
   end Next;

   function Next (Position : in Cursor) return Cursor
   is begin
      if Position.Ptr = null then
         return Position;
      else
         if Position.Ptr.Next = null then
            return (Position.Container, null);
         else
            return (Position.Container, Position.Ptr.Next);
         end if;
      end if;
   end Next;

   function Previous (Position : in Cursor) return Cursor
   is begin
      if Position.Ptr = null then
         return Position;
      else
         if Position.Ptr.Prev = null then
            return (Position.Container, null);
         else
            return (Position.Container, Position.Ptr.Prev);
         end if;
      end if;
   end Previous;

   function Element (Position : in Cursor) return Element_Type
   is begin
      return Position.Ptr.Element;
   end Element;

   procedure Delete (Container : in out List; Position : in out Cursor)
   is
      Node : Node_Access renames Position.Ptr;
   begin
      if Node.Next = null then
         Container.Tail := Node.Prev;
      else
         Node.Next.Prev := Node.Prev;
      end if;
      if Node.Prev = null then
         Container.Head := Node.Next;
      else
         Node.Prev.Next := Node.Next;
      end if;
      Free (Node);
      Position        := (Container'Access, null);
      Container.Count := Container.Count - 1;
   end Delete;

   function Pop (Container : in out List) return Element_Type
   is
      Node : Node_Access := Container.Head;
   begin
      return Result : constant Element_Type := Container.Head.Element do
         Container.Head := Node.Next;
         if Node.Next = null then
            Container.Tail := null;
         else
            Node.Next.Prev := null;
         end if;
         Free (Node);
         Container.Count := Container.Count - 1;
      end return;
   end Pop;

   function Constant_Reference (Container : in List; Position : in Cursor) return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Ptr.all.Element'Access, Dummy => 1);
   end Constant_Reference;

   function Constant_Ref (Position : in Cursor) return Constant_Reference_Type
   is begin
      return (Element => Position.Ptr.all.Element'Access, Dummy => 1);
   end Constant_Ref;

   function Variable_Reference (Container : in List; Position : in Cursor) return Variable_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position.Ptr.all.Element'Access, Dummy => 1);
   end Variable_Reference;

   function Variable_Ref (Position : in Cursor) return Variable_Reference_Type
   is begin
      return (Element => Position.Ptr.all.Element'Access, Dummy => 1);
   end Variable_Ref;

   function Iterate (Container : aliased in List) return Iterator_Interfaces.Reversible_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   overriding function First (Object : Iterator) return Cursor
   is begin
      return First (Object.Container.all);
   end First;

   overriding function Last  (Object : Iterator) return Cursor
   is begin
      return Last (Object.Container.all);
   end Last;

   overriding function Next (Object : in Iterator; Position : in Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Next (Position);
   end Next;

   overriding function Previous (Object : in Iterator; Position : in Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Previous (Position);
   end Previous;

end SAL.Gen_Definite_Doubly_Linked_Lists_Sorted;
