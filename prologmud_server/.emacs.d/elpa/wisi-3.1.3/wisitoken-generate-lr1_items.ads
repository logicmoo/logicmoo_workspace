--  Abstract :
--
--  Types and operatorion for LR(1) items.
--
--  Copyright (C) 2003, 2008, 2013 - 2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Interfaces;
with SAL.Gen_Definite_Doubly_Linked_Lists_Sorted;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Comparable;
with WisiToken.Productions;
package WisiToken.Generate.LR1_Items is

   use all type Interfaces.Integer_16;

   subtype Lookahead is Token_ID_Set;
   --  Picking a type for Lookahead is not straight-forward. The
   --  operations required are (called numbers are for LR1 generate
   --  ada_lite):
   --
   --  to_lookahead (token_id)
   --     Requires allocating memory dynamically:
   --        an unconstrained array range (first_terminal .. last_terminal) for (1),
   --        a smaller unconstrained array for (2), that grows as items are added
   --        individual list elements for (3).
   --
   --     lr1_items.to_lookahead        called 4_821_256 times in (2)
   --     sorted_token_id_lists.to_list called 4_821_256 times in (3)
   --
   --  for tok_id of lookaheads loop
   --     sorted_token_id_lists__iterate called 5_687 times in (3)
   --
   --  if lookaheads.contains (tok_id) then
   --     token_id_arrays__contains called 22_177_109 in (2)
   --
   --  new_item := (... , lookaheads => old_item.lookaheads)
   --  new_item := (... , lookaheads => null_lookaheads)
   --  new_item := (... , lookaheads => propagate_lookahead)
   --     token_id_arrays.adjust called 8_437_967 times in (2)
   --     sorted_token_id_lists.adjust  8_435_797 times in (3)
   --
   --  include: add tok_id to lookaheads
   --
   --      keep sorted in token_id order, so rest of algorithm is
   --      stable/faster
   --
   --      lr1_items.include called 6_818_725 times in (2)
   --
   --  lookaheads /= lookaheads
   --     if using a container, container must override "="
   --
   --  We've tried:
   --
   --  (1) Token_ID_Set (unconstrained array of boolean, allocated directly) - fastest
   --
   --     Allocates more memory than (2), but everything else is fast,
   --     and it's not enough memory to matter.
   --
   --     Loop over lookaheads is awkward:
   --     for tok_id in lookaheads'range loop
   --        if lookaheads (tok_id) then
   --           ...
   --     But apparently it's fast enough.
   --
   --  (2) Instantiation of SAL.Gen_Unbounded_Definite_Vectors (token_id_arrays) - slower than (1).
   --
   --      Productions RHS is also token_id_arrays, so gprof numbers are
   --      hard to sort out. Could be improved with a custom container, that
   --      does sort and insert internally. Insert is inherently slow.
   --
   --  (3) Instantiation of SAL.Gen_Definite_Doubly_Linked_Lists_Sorted - slower than (2)

   type Item is record
      Prod       : Production_ID;
      Dot        : Token_ID_Arrays.Extended_Index := Token_ID_Arrays.No_Index; -- token after item Dot
      Lookaheads : Token_ID_Set_Access := null;
      --  Programmer must remember to copy Item.Lookaheads.all, not
      --  Item.Lookaheads. Wrapping this in Ada.Finalization.Controlled
      --  would just slow it down.
      --
      --  We don't free Lookaheads; we assume the user is running
      --  wisi-generate, and not keeping LR1_Items around.
   end record;

   function To_Lookahead (Item : in Token_ID; Descriptor : in WisiToken.Descriptor) return Lookahead;

   function Contains (Item : in Lookahead; ID : in Token_ID) return Boolean
     is (Item (ID));

   function Lookahead_Image (Item : in Lookahead; Descriptor : in WisiToken.Descriptor) return String;
   --  Returns the format used in parse table output.

   function Image
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean)
     return String;

   function Item_Compare (Left, Right : in Item) return SAL.Compare_Result;
   --  Sort Item_Lists in ascending order of Prod.Nonterm, Prod.RHS, Dot;
   --  ignores Lookaheads.
   --
   --  In an LALR kernel there can be only one Item with Prod, but that
   --  is not true in an Item_Set produced by Closure.

   package Item_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Sorted (Item, Item_Compare);

   procedure Include
     (Item  : in out LR1_Items.Item;
      Value : in     Lookahead;
      Added :    out Boolean);
   --  Add Value to Item.Lookahead, if not already present.
   --
   --  Added is True if Value was not already present.
   --
   --  Does not exclude Propagate_ID.

   procedure Include
     (Item       : in out LR1_Items.Item;
      Value      : in     Lookahead;
      Descriptor : in     WisiToken.Descriptor);
   --  Add Value to Item.Lookahead. Does not check if already present.
   --  Excludes Propagate_ID.

   procedure Include
     (Item       : in out LR1_Items.Item;
      Value      : in     Lookahead;
      Added      :    out Boolean;
      Descriptor : in     WisiToken.Descriptor);
   --  Add Value to Item.Lookahead.

   type Goto_Item is record
      Symbol : Token_ID;
      --  If Symbol is a terminal, this is a shift and goto state action.
      --  If Symbol is a non-terminal, this is a post-reduce goto state action.
      State  : State_Index;
   end record;

   function Goto_Item_Compare (Left, Right : in Goto_Item) return SAL.Compare_Result is
     (if Left.Symbol > Right.Symbol then SAL.Greater
      elsif Left.Symbol < Right.Symbol then SAL.Less
      else SAL.Equal);
   --  Sort Goto_Item_Lists in ascending order of Symbol.

   package Goto_Item_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists_Sorted
     (Goto_Item, Goto_Item_Compare);

   type Item_Set is record
      Set       : Item_Lists.List;
      Goto_List : Goto_Item_Lists.List;
      Dot_IDs   : Token_ID_Arrays.Vector;
      State     : Unknown_State_Index := Unknown_State;
   end record;

   function Filter
     (Set        : in     Item_Set;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor;
      Include    : access function
        (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
         Descriptor : in WisiToken.Descriptor;
         Item       : in LR1_Items.Item)
        return Boolean)
     return Item_Set;
   --  Return a deep copy of Set, including only items for which Include returns True.

   function In_Kernel
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor;
      Item       : in LR1_Items.Item)
     return Boolean;
   --  For use with Filter; [dragon] sec 4.7 pg 240

   function Find
     (Item : in LR1_Items.Item;
      Set  : in Item_Set)
     return Item_Lists.Cursor;
   --  Return an item from Set that matches Item.Prod, Item.Dot.
   --
   --  Return No_Element if not found.

   function Find
     (Prod : in Production_ID;
      Dot : in Token_ID_Arrays.Extended_Index;
      Set  : in Item_Set)
     return Item_Lists.Cursor;
   --  Return an item from Set that matches Prod, Dot.
   --
   --  Return No_Element if not found.

   package Item_Set_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (State_Index, Item_Set, Default_Element => (others => <>));
   subtype Item_Set_List is Item_Set_Arrays.Vector;

   package Int_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive, Interfaces.Integer_16, Default_Element => Interfaces.Integer_16'Last);
   function Compare_Integer_16 (Left, Right : in Interfaces.Integer_16) return SAL.Compare_Result is
     (if Left > Right then SAL.Greater
      elsif Left < Right then SAL.Less
      else SAL.Equal);

   package Int_Arrays_Comparable is new Int_Arrays.Gen_Comparable (Compare_Integer_16);

   subtype Item_Set_Tree_Key is Int_Arrays_Comparable.Vector;
   --  We want a key that is fast to compare, and has enough info to
   --  significantly speed the search for an item set. So we convert all
   --  relevant data in an item into a string of integers. We need 16 bit
   --  because Ada token_ids max is 332. LR1 keys include lookaheads,
   --  LALR keys do not.

   type Item_Set_Tree_Node is record
      Key   : Item_Set_Tree_Key;
      State : Unknown_State_Index;
   end record;

   function To_Item_Set_Tree_Key
     (Item_Set           : in LR1_Items.Item_Set;
      Include_Lookaheads : in Boolean)
     return Item_Set_Tree_Key;

   function To_Item_Set_Tree_Key (Node : in Item_Set_Tree_Node) return Item_Set_Tree_Key is
     (Node.Key);

   package Item_Set_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Item_Set_Tree_Node,
      Key_Type     => Item_Set_Tree_Key,
      Key          => To_Item_Set_Tree_Key,
      Key_Compare  => Int_Arrays_Comparable.Compare);
   --  Item_Set_Arrays.Vector holds state item sets indexed by state, for
   --  iterating in state order. Item_Set_Trees.Tree holds lists of state
   --  indices sorted by LR1 item info, for fast Find in LR1_Item_Sets
   --  and LALR_Kernels.

   function Find
     (New_Item_Set     : in Item_Set;
      Item_Set_Tree    : in Item_Set_Trees.Tree;
      Match_Lookaheads : in Boolean)
     return Unknown_State_Index;
   --  Return the State of an element in Item_Set_Tree matching
   --  New_Item_Set, Unknown_State if not found.
   --
   --  Match_Lookaheads is True in LR1_Generate.

   procedure Add
     (Grammar            : in     WisiToken.Productions.Prod_Arrays.Vector;
      New_Item_Set       : in     Item_Set;
      Item_Set_Vector    : in out Item_Set_List;
      Item_Set_Tree      : in out Item_Set_Trees.Tree;
      Descriptor         : in     WisiToken.Descriptor;
      Include_Lookaheads : in     Boolean);
   --  Set New_Item_Set.Dot_IDs, add New_Item_Set to Item_Set_Vector, Item_Set_Tree

   function Is_In
     (Item      : in Goto_Item;
      Goto_List : in Goto_Item_Lists.List)
     return Boolean;
   --  Return True if a goto on Symbol to State is found in Goto_List

   function Goto_State
     (From   : in Item_Set;
      Symbol : in Token_ID)
     return Unknown_State_Index;
   --  Return state from From.Goto_List where the goto symbol is
   --  Symbol; Unknown_State if not found.

   function Closure
     (Set                     : in Item_Set;
      Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return Item_Set;
   --  Return the closure of Set over Grammar. First must be the
   --  result of First above. Makes a deep copy of Goto_List.
   --  Implements 'closure' from [dragon] algorithm 4.9 pg 232, but
   --  allows merging lookaheads into one item..

   function Productions (Set : in Item_Set) return Production_ID_Arrays.Vector;

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in LR1_Items.Item;
      Show_Lookaheads : in Boolean := True);

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in Item_Lists.List;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False);

   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in Item_Set;
      Show_Lookaheads : in Boolean := True;
      Kernel_Only     : in Boolean := False;
      Show_Goto_List  : in Boolean := False);

   procedure Put
     (Descriptor : in WisiToken.Descriptor;
      List       : in Goto_Item_Lists.List);
   procedure Put
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in WisiToken.Descriptor;
      Item            : in Item_Set_List;
      Show_Lookaheads : in Boolean := True);
   --  Put Item to Ada.Text_IO.Standard_Output. Does not end with New_Line.

end WisiToken.Generate.LR1_Items;
