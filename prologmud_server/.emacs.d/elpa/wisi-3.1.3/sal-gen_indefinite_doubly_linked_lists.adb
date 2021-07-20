--  Abstract :
--
--  see spec
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body SAL.Gen_Indefinite_Doubly_Linked_Lists is

   ---------
   --  Public operations, declaration order.

   overriding
   procedure Adjust (Container : in out List)
   is
      Source   : Node_Access := Container.Head;
      New_Node : Node_Access;
   begin
      if Source = null then
         return;
      end if;

      Container.Tail := null;

      loop
         New_Node := new Node_Type'
           (Element => new Element_Type'(Source.Element.all),
            Next    => null,
            Prev    => Container.Tail);
         if Container.Tail = null then
            Container.Head := New_Node;
            Container.Tail := New_Node;
         else
            Container.Tail.Next := New_Node;
            Container.Tail      := New_Node;
         end if;
         Source := Source.Next;
         exit when Source = null;
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
         Free (Container.Head.Element);
         Free (Container.Head);
         Container.Head := Next;
      end loop;
      Container.Tail := null;
   end Finalize;

   function Length (Container : in List) return SAL.Base_Peek_Type
   is begin
      return Container.Count;
   end Length;

   procedure Append (Container : in out List; Element : in Element_Type)
   is
      New_Node : constant Node_Access := new Node_Type'
        (Element => new Element_Type'(Element),
         Prev    => Container.Tail,
         Next    => null);
   begin
      if Container.Tail = null then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Tail.Next := New_Node;
         Container.Tail      := New_Node;
      end if;
      Container.Count := Container.Count + 1;
   end Append;

   procedure Prepend (Container : in out List; Element : in Element_Type)
   is
      New_Node : constant Node_Access := new Node_Type'
        (Element => new Element_Type'(Element),
         Prev    => null,
         Next    => Container.Head);
   begin
      if Container.Tail = null then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Head.Prev := New_Node;
         Container.Head      := New_Node;
      end if;
      Container.Count := Container.Count + 1;
   end Prepend;

   function Has_Element (Position : in Cursor) return Boolean
   is begin
      return Position.Ptr /= null;
   end Has_Element;

   function First (Container : in List) return Cursor
   is begin
      if Container.Head = null then
         return (Ptr => null);
      else
         return (Ptr => Container.Head);
      end if;
   end First;

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
            return (Ptr => null);
         else
            return (Ptr => Position.Ptr.Next);
         end if;
      end if;
   end Next;

   function Element (Position : in Cursor) return Element_Type
   is begin
      return Position.Ptr.Element.all;
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
      Free (Node.Element);
      Free (Node);
      Position        := (Ptr => null);
      Container.Count := Container.Count - 1;
   end Delete;

   function Persistent_Ref (Position : in Cursor) return access Element_Type
   is begin
      return Position.Ptr.Element;
   end Persistent_Ref;

   function Constant_Ref (Position : in Cursor) return Constant_Reference_Type
   is begin
      return (Element => Position.Ptr.all.Element, Dummy => 1);
   end Constant_Ref;

   function Constant_Reference (Container : in List; Position : in Peek_Type) return Constant_Reference_Type
   is
      Ptr : Node_Access := Container.Head;
   begin
      for I in 2 .. Position loop
         Ptr := Ptr.Next;
      end loop;
      return (Element => Ptr.all.Element, Dummy => 1);
   end Constant_Reference;

   function Variable_Reference (Container : in List; Position : in Peek_Type) return Variable_Reference_Type
   is
      Ptr : Node_Access := Container.Head;
   begin
      for I in 2 .. Position loop
         Ptr := Ptr.Next;
      end loop;
      return (Element => Ptr.all.Element, Dummy => 1);
   end Variable_Reference;

   function Variable_Ref (Position : in Cursor) return Variable_Reference_Type
   is begin
      return (Element => Position.Ptr.all.Element, Dummy => 1);
   end Variable_Ref;

end SAL.Gen_Indefinite_Doubly_Linked_Lists;
