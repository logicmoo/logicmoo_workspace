--  Abstract :
--
--  Generic unbounded red-black tree with definite elements, definite
--  or indefinite key.
--
--  References :
--
--  [1] Introduction to Algorithms, Thomas H. Cormen, Charles E.
--  Leiserson, Ronald L. Rivest, Clifford Stein.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
   type Key_Type (<>) is private;
   with function Key (Element : in Element_Type) return Key_Type is <>;
   with function Key_Compare (Left, Right : in Key_Type) return Compare_Result is <>;
package SAL.Gen_Unbounded_Definite_Red_Black_Trees is

   package Pkg renames Gen_Unbounded_Definite_Red_Black_Trees;

   type Tree is new Ada.Finalization.Limited_Controlled with private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Variable_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   overriding procedure Finalize (Object : in out Tree);
   overriding procedure Initialize (Object : in out Tree);

   type Direction_Type is (Ascending, Descending, Unknown);
   subtype Known_Direction_Type is Direction_Type range Ascending .. Descending;
   --  Direction of Iterators.
   --  If Ascending, Next may be called.
   --  If Descending, Previous may be called.
   --  If Unknown, neither.

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Cursor : in Pkg.Cursor) return Boolean;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Constant_Reference_Type with
     Inline, Pre => Has_Element (Position);

   function Constant_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Constant_Reference_Type with
     Inline;
   --  Raises Not_Found if Key not found in Container.

   type Variable_Reference_Type (Element : not null access Element_Type) is private with
     Implicit_Dereference => Element;

   function Variable_Reference
     (Container : aliased in Tree;
      Position  :         in Cursor)
     return Variable_Reference_Type with
     Inline, Pre => Has_Element (Position);

   function Variable_Reference
     (Container : aliased in Tree;
      Key       :         in Key_Type)
     return Variable_Reference_Type with
     Inline;
   --  Raises Not_Found if Key not found in Container.

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator is new Iterators.Reversible_Iterator with private;

   function Iterate (Tree : in Pkg.Tree'Class) return Iterator;

   overriding function First (Iterator : in Pkg.Iterator) return Cursor;
   overriding function Next (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor;
   overriding function Last (Iterator : in Pkg.Iterator) return Cursor;
   overriding function Previous (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor;

   function Previous (Iterator : in Pkg.Iterator; Key : in Key_Type) return Cursor;
   --  Initialise Iterator to descending, starting at element with
   --  largest key < Key. Has_Element (result) is False if there is no
   --  such element.

   function Find
     (Iterator  : in Pkg.Iterator;
      Key       : in Key_Type;
      Direction : in Direction_Type := Ascending)
     return Cursor;
   --  Has_Element is False if Key is not in Container.

   function Find_In_Range
     (Iterator    : in Pkg.Iterator;
      Direction   : in Known_Direction_Type;
      First, Last : in Key_Type)
     return Cursor;
   --  Find first element with key in range First .. Last. If Direction
   --  is Ascending, start at First, otherwise start at Last.
   --
   --  Has_Element (result) is False if there is no such element.
   --
   --  The Iterator does not remember First, Last; the user must check
   --  those for any element that Next or Previous returns.

   function Count (Tree : in Pkg.Tree) return Ada.Containers.Count_Type;

   function Present (Container : in Tree; Key : in Key_Type) return Boolean;

   procedure Insert (Tree : in out Pkg.Tree; Element : in Element_Type);
   function Insert (Tree : in out Pkg.Tree; Element : in Element_Type) return Cursor;
   --  Result points to newly inserted element.

   procedure Delete (Tree : in out Pkg.Tree; Position : in out Cursor);
   --  Delete element at Position, set Position to No_Element.
private

   type Node;
   type Node_Access is access Node;

   type Color is (Red, Black);

   type Node is record
      Element : aliased Element_Type;
      Parent  : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Color   : Pkg.Color;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   type Tree is new Ada.Finalization.Limited_Controlled with record
      Root : Node_Access;
      Nil  : Node_Access;
      --  Nil is the node pointed to by all links that would otherwise be
      --  'null'. This simplifies several algorithm (for example,
      --  Node.Left.Color is always valid). Its parent, left, right links
      --  are used as temp storage for some algorithms (especially Delete).
      --  Nil.Color is Black.
   end record;

   type Cursor is record
      Node : Node_Access := null;

      Direction  : Direction_Type := Unknown;
      --  Set in First or Last, enforced in next/prev (cannot change direction).

      Left_Done  : Boolean := True;
      Right_Done : Boolean := True;
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Variable_Reference_Type (Element : not null access Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   No_Element : constant Cursor :=
     (Node       => null,
      Direction  => Unknown,
      Left_Done  => True,
      Right_Done => True);

   type Iterator is new Iterators.Reversible_Iterator with
   record
      Root : Node_Access;
      Nil  : Node_Access;
   end record;

end SAL.Gen_Unbounded_Definite_Red_Black_Trees;
