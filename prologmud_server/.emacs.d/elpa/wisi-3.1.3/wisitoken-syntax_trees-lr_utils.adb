--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019, 2020 Stephen Leake All Rights Reserved.
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
package body WisiToken.Syntax_Trees.LR_Utils is

   procedure Raise_Programmer_Error
     (Label      : in String;
      Descriptor : in WisiToken.Descriptor;
      Lexer      : in WisiToken.Lexer.Handle;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Terminals  : in WisiToken.Base_Token_Arrays.Vector;
      Node       : in Node_Index)
   is
      Terminal_Index : constant Base_Token_Index := Tree.First_Shared_Terminal (Node);
   begin
      raise SAL.Programmer_Error with Error_Message
        (Lexer.File_Name,
         --  Not clear why we need Line + 1 here, to match Emacs.
         (if Terminal_Index = Invalid_Token_Index then 1 else Terminals (Terminal_Index).Line + 1), 0,
         Label & ": " &
           Tree.Image (Node, Descriptor, Include_Children => True, Include_RHS_Index => True, Node_Numbers => True));
   end Raise_Programmer_Error;

   function Count (Container : Constant_List) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Result : Count_Type := 0;
   begin
      for Item of Container loop
         Result := Result + 1;
      end loop;
      return Result;
   end Count;

   function Contains (Container : in Constant_List; Node : in Valid_Node_Index) return Boolean
   is begin
      return (for some N of Container => N = Node);
   end Contains;

   function To_Cursor (Container : in Constant_List; Node : in Valid_Node_Index) return Cursor
   is
      pragma Unreferenced (Container);
   begin
      return (Node => Node);
   end To_Cursor;

   function Contains (Container : in Constant_List; Item : in Cursor) return Boolean
   is begin
      return (for some N of Container => N = Item.Node);
   end Contains;

   function First
     (Tree       : in WisiToken.Syntax_Trees.Tree;
      Root       : in WisiToken.Node_Index;
      List_ID    : in WisiToken.Token_ID;
      Element_ID : in WisiToken.Token_ID)
     return Node_Index
   is begin
      if Root = Invalid_Node_Index then
         return Invalid_Node_Index;
      else
         return Result : Node_Index do
            Result := Root;
            loop
               declare
                  Children : constant Valid_Node_Index_Array := Tree.Children (Result);
               begin
                  if Tree.ID (Children (1)) = List_ID then
                     Result := Children (1);
                  elsif Tree.ID (Children (1)) = Element_ID then
                     Result := Children (1);
                     exit;
                  else
                     raise SAL.Programmer_Error;
                  end if;
               end;
            end loop;
         end return;
      end if;
   end First;

   function First (Container : in Constant_List) return Cursor
   is begin
      return (Node => First (Container.Tree.all, Container.Root, Container.List_ID, Container.Element_ID));
   end First;

   function Last
     (Tree : in WisiToken.Syntax_Trees.Tree;
      Root : in WisiToken.Node_Index)
     return Node_Index
   is begin
      if Root = Invalid_Node_Index then
         return Invalid_Node_Index;
      else
         --  Tree is one of:
         --
         --  case a: single element list
         --  element_list : root
         --  | element: Last
         --
         --  case c: no next
         --  element_list: root
         --  | element_list
         --  | | element:
         --  | element: Last
         return Tree.Child (Root, SAL.Base_Peek_Type (Tree.Child_Count (Root)));
      end if;
   end Last;

   function Last  (Container : in Constant_List) return Cursor
   is begin
      return (Node => Last (Container.Tree.all, Container.Root));
   end Last;

   function Next
     (Tree       : in Syntax_Trees.Tree;
      List_ID    : in Token_ID;
      Element_ID : in Token_ID;
      Position   : in Node_Index)
     return Node_Index
   is begin
      if Position = Invalid_Node_Index then
         return Position;
      else
         return Result : Node_Index do
            declare
               --  Tree is one of:
               --
               --  case a: first element, no next
               --  rhs
               --  | rhs_item_list
               --  | | rhs_item: Element
               --  | action
               --
               --  case b: first element, next
               --  rhs_item_list
               --  | rhs_item_list
               --  | | rhs_item: Element
               --  | rhs_item: next element
               --
               --  case c: non-first element, no next
               --  rhs
               --  | rhs_item_list : Grand_Parent
               --  | | rhs_item_list
               --  | | | rhs_item:
               --  | | rhs_item: Element
               --  | action : Aunt
               --
               --  case d: non-first element, next
               --  rhs_item_list
               --  | rhs_item_list : Grand_Parent
               --  | | rhs_item_list
               --  | | | rhs_item:
               --  | | rhs_item: Element
               --  | rhs_item: next element : Aunt

               Grand_Parent : constant Node_Index := Tree.Parent (Position, 2);

               Aunts           : constant Valid_Node_Index_Array :=
                 (if Grand_Parent = Invalid_Node_Index or else Tree.ID (Grand_Parent) /= List_ID
                  then (1 .. 0 => Invalid_Node_Index)
                  else Tree.Children (Grand_Parent));

               Last_List_Child : SAL.Base_Peek_Type := Aunts'First - 1;
            begin
               if Grand_Parent = Invalid_Node_Index or else Tree.ID (Grand_Parent) /= List_ID then
                  --  No next
                  Result := Invalid_Node_Index;
               else
                  for I in Aunts'Range loop
                     if Tree.ID (Aunts (I)) in List_ID | Element_ID then
                        Last_List_Child := I;
                     end if;
                  end loop;

                  if Last_List_Child = 1 then
                     --  No next
                     Result := Invalid_Node_Index;
                  else
                     Result := Aunts (Last_List_Child);
                  end if;
               end if;
            end;
         end return;
      end if;
   end Next;

   overriding function Next (Iter : Iterator; Position : Cursor) return Cursor
   is begin
      return
        (Node => Next
           (Iter.Container.Tree.all, Iter.Container.List_ID, Iter.Container.Element_ID, Position.Node));
   end Next;

   function Previous
     (Tree     : in Syntax_Trees.Tree;
      Position : in Node_Index)
     return Node_Index
   is begin
      if Position = Invalid_Node_Index then
         return Position;
      else
         return Result : Node_Index do
            --  Tree is one of:
            --
            --  case a: first element, no prev
            --  ?
            --  | rhs_item_list
            --  | | rhs_item: Element
            --
            --  case b: second element
            --  ?
            --  | rhs_item_list
            --  | | rhs_item: prev item
            --  | rhs_item: Element
            --
            --  case c: nth element
            --  ?
            --  | rhs_item_list
            --  | | rhs_item_list
            --  | | | rhs_item:
            --  | | rhs_item: prev element
            --  | rhs_item: Element
            declare
               Parent : constant Valid_Node_Index := Tree.Parent (Position);
            begin
               if Position = Tree.Child (Parent, 1) then
                  --  No prev
                  Result := Invalid_Node_Index;

               else
                  declare
                     Prev_Children : constant Valid_Node_Index_Array := Tree.Children
                       (Tree.Child (Parent, 1));
                  begin
                     Result := Prev_Children (Prev_Children'Last);
                  end;
               end if;
            end;
         end return;
      end if;
   end Previous;

   overriding function Previous (Iter : Iterator; Position : Cursor) return Cursor
   is begin
      return (Node => Previous (Iter.Container.Tree.all, Position.Node));
   end Previous;

   function List_Constant_Ref (Container : aliased in Constant_List'Class; Position : in Cursor) return Valid_Node_Index
   is
      pragma Unreferenced (Container);
   begin
      return Position.Node;
   end List_Constant_Ref;

   overriding function Next (Iter : in Constant_Iterator; Position : Cursor) return Cursor
   is begin
      return (Node => Next (Iter.Container.Tree.all, Iter.Container.List_ID, Iter.Container.Element_ID, Position.Node));
   end Next;

   overriding function Previous (Iter : in Constant_Iterator; Position : Cursor) return Cursor
   is begin
      return (Node => Previous (Iter.Container.Tree.all, Position.Node));
   end Previous;

   function Find
     (Container : in Constant_List;
      Target    : in Valid_Node_Index)
     return Cursor
   is begin
      for Cur in Container.Iterate_Constant loop
         if Target = Cur.Node then
            return Cur;
         end if;
      end loop;
      return No_Element;
   end Find;

   function Find
     (Container : in Constant_List;
      Target    : in String;
      Equal     : in Find_Equal)
     return Cursor
   is begin
      for Cur in Container.Iterate_Constant loop
         if Equal (Target, Container, Cur.Node) then
            return Cur;
         end if;
      end loop;
      return No_Element;
   end Find;

   package body Creators is

      function Create_List
        (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
         Root         :         in     Valid_Node_Index;
         List_ID      :         in     WisiToken.Token_ID;
         Element_ID   :         in     WisiToken.Token_ID;
         Separator_ID :         in     WisiToken.Token_ID)
        return List
      is
         pragma Unreferenced (List_ID); --  checked in precondition.

         Multi_Element_RHS : constant Natural :=
           (if Tree.Child_Count (Root) = 1
            then (if Tree.RHS_Index (Root) = 0 then 1 else 0)
            elsif Tree.Child_Count (Root) in 2 .. 3 --  3 if there is a separator
            then Tree.RHS_Index (Root)
            else raise SAL.Programmer_Error);
      begin
         return
           (Tree'Access, Root,
            List_ID           => Tree.ID (Root),
            One_Element_RHS   => (if Multi_Element_RHS = 0 then 1 else 0),
            Multi_Element_RHS => Multi_Element_RHS,
            Element_ID        => Element_ID,
            Separator_ID      => Separator_ID);
      end Create_List;

      function Create_List
        (Tree       : aliased in out WisiToken.Syntax_Trees.Tree;
         Root       :         in     Valid_Node_Index;
         List_ID    :         in     WisiToken.Token_ID;
         Element_ID :         in     WisiToken.Token_ID)
        return Constant_List
      is
         pragma Unreferenced (List_ID); --  in precondition
      begin
         return
           (Tree'Access, Root,
            List_ID    => Tree.ID (Root),
            Element_ID => Element_ID);
      end Create_List;

      function Create_List
        (Container :         in     Constant_List;
         Tree      : aliased in out WisiToken.Syntax_Trees.Tree;
         Root      :         in     Valid_Node_Index)
        return Constant_List
      is begin
         return Create_List (Tree, Root, Container.List_ID, Container.Element_ID);
      end Create_List;

      function Create_List (Container : in out List; Root : in Valid_Node_Index) return List
      is begin
         return Create_List (Container.Tree.all, Root, Container.List_ID, Container.Element_ID, Container.Separator_ID);
      end Create_List;

      function Create_From_Element
        (Tree         : aliased in out WisiToken.Syntax_Trees.Tree;
         Element      :         in     Valid_Node_Index;
         List_ID      :         in     WisiToken.Token_ID;
         Element_ID   :         in     WisiToken.Token_ID;
         Separator_ID :         in     WisiToken.Token_ID)
        return List
      is
         Root : Valid_Node_Index := Tree.Parent (Element);
      begin
         loop
            exit when Tree.Parent (Root) = Invalid_Node_Index or else Tree.ID (Tree.Parent (Root)) /= List_ID;
            Root := Tree.Parent (Root);
         end loop;
         return Create_List (Tree, Root, List_ID, Element_ID, Separator_ID);
      end Create_From_Element;

      function Create_From_Element (Container : in out List; Element : in Valid_Node_Index) return List
      is begin
         return Create_From_Element
           (Container.Tree.all, Element, Container.List_ID, Container.Element_ID, Container.Separator_ID);
      end Create_From_Element;

      function Create_From_Element
        (Tree       : aliased in out WisiToken.Syntax_Trees.Tree;
         Element    :         in     Valid_Node_Index;
         List_ID    :         in     WisiToken.Token_ID;
         Element_ID :         in     WisiToken.Token_ID)
        return Constant_List
      is
         Root : Valid_Node_Index := Tree.Parent (Element);
      begin
         loop
            exit when Tree.Parent (Root) = Invalid_Node_Index or else Tree.ID (Tree.Parent (Root)) /= List_ID;
            Root := Tree.Parent (Root);
         end loop;
         return Create_List (Tree, Root, List_ID, Element_ID);
      end Create_From_Element;

      function Invalid_List (Tree : aliased in out WisiToken.Syntax_Trees.Tree) return List
      is begin
         return
           (Tree              => Tree'Access,
            Root              => Invalid_Node_Index,
            List_ID           => Invalid_Token_ID,
            One_Element_RHS   => 0,
            Multi_Element_RHS => 0,
            Element_ID        => Invalid_Token_ID,
            Separator_ID      => Invalid_Token_ID);
      end Invalid_List;

      function Invalid_List (Tree : aliased in out WisiToken.Syntax_Trees.Tree) return Constant_List
      is begin
         return
           (Tree       => Tree'Access,
            Root       => Invalid_Node_Index,
            List_ID    => Invalid_Token_ID,
            Element_ID => Invalid_Token_ID);
      end Invalid_List;

      function Empty_List
        (Tree              : aliased in out WisiToken.Syntax_Trees.Tree;
         List_ID           :         in     WisiToken.Token_ID;
         Multi_Element_RHS :         in     Natural;
         Element_ID        :         in     WisiToken.Token_ID;
         Separator_ID      :         in     WisiToken.Token_ID)
        return List
      is begin
         return
           (Tree'Access,
            Root              => Invalid_Node_Index,
            List_ID           => List_ID,
            One_Element_RHS   => (if Multi_Element_RHS = 0 then 1 else 0),
            Multi_Element_RHS => Multi_Element_RHS,
            Element_ID        => Element_ID,
            Separator_ID      => Separator_ID);
      end Empty_List;

      function Empty_List (Container : in out List) return List
      is begin
         return Empty_List
           (Container.Tree.all, Container.List_ID, Container.Multi_Element_RHS, Container.Element_ID,
            Container.Separator_ID);
      end Empty_List;
   end Creators;

   procedure Append
     (Container   : in out List;
      New_Element : in     Valid_Node_Index)
   is
      Tree : Syntax_Trees.Tree renames Container.Tree.all;
   begin
      if Container.Root = Invalid_Node_Index then
         Container :=
           (Container.Tree,
            List_ID           => Container.List_ID,
            One_Element_RHS   => Container.One_Element_RHS,
            Multi_Element_RHS => Container.Multi_Element_RHS,
            Element_ID        => Container.Element_ID,
            Separator_ID      => Container.Separator_ID,
            Root              => Tree.Add_Nonterm
              (Production     => (Container.List_ID, Container.One_Element_RHS),
               Children       => (1 => New_Element)));

      else
         --  Adding element Last in spec example
         declare
            List_Parent : constant Node_Index         := Tree.Parent (Container.Root);
            Old_Root    : constant Valid_Node_Index   := Container.Root;
            Child_Index : constant SAL.Base_Peek_Type :=
              (if List_Parent = Invalid_Node_Index
               then 0
               else Tree.Child_Index (List_Parent, Old_Root));
         begin
            Container.Root :=
              Tree.Add_Nonterm
                (Production     => (Container.List_ID, Container.Multi_Element_RHS),
                 Children       =>
                   (if Container.Separator_ID = Invalid_Token_ID
                    then (Old_Root, New_Element)
                    else (Old_Root, Tree.Add_Terminal (Container.Separator_ID), New_Element)));

            if List_Parent = Invalid_Node_Index then
               if Tree.Root = Old_Root then
                  Tree.Root := Container.Root;
               end if;

            else
               Tree.Replace_Child
                 (List_Parent,
                  Child_Index,
                  Old_Child => Deleted_Child,
                  New_Child => Container.Root);
            end if;
         end;
      end if;
   end Append;

   procedure Prepend
     (Container   : in out List;
      New_Element : in     Valid_Node_Index)
   is
      Tree : Syntax_Trees.Tree renames Container.Tree.all;
   begin
      if Container.Root = Invalid_Node_Index then
         Container :=
           (Container.Tree,
            List_ID           => Container.List_ID,
            One_Element_RHS   => Container.One_Element_RHS,
            Multi_Element_RHS => Container.Multi_Element_RHS,
            Element_ID        => Container.Element_ID,
            Separator_ID      => Container.Separator_ID,
            Root              => Tree.Add_Nonterm
              (Production     => (Container.List_ID, Container.One_Element_RHS),
               Children       => (1 => New_Element)));

      else
         --  Inserting element First (with list parent node and separator) in spec example
         declare
            Old_First  : constant Valid_Node_Index := Container.First.Node;
            Parent : constant Valid_Node_Index := Tree.Parent (Old_First);

            List_Node : constant Valid_Node_Index := Tree.Add_Nonterm
              ((Container.List_ID, Container.One_Element_RHS),
               (1 => New_Element));
         begin
            Tree.Set_Children
              (Node     => Parent,
               New_ID   => (Container.List_ID, Container.Multi_Element_RHS),
               Children =>
                 (if Container.Separator_ID = Invalid_Token_ID
                  then (List_Node, Old_First)
                  else (List_Node, Tree.Add_Terminal (Container.Separator_ID), Old_First)));
         end;
      end if;
   end Prepend;

   procedure Insert
     (Container   : in out List;
      New_Element : in     Valid_Node_Index;
      After       : in     Cursor)
   is
      --  Current Tree (see wisitoken_syntax_trees-test.adb Test_Insert_1):
      --
      --  list: Tree.Root
      --  | list = Parent
      --  | | list
      --  | | | list
      --  | | | | element: 1 = First
      --  | | | separator
      --  | | | element: 2 = After
      --  | | separator
      --  | | element: 3 = Before
      --  | separator
      --  | element: 4 = Last

      --  Insert New_Element after 2:
      --
      --  list: Tree.Root
      --  | list
      --  | | list = Parent
      --  | | | list: new_list_nonterm
      --  | | | | list
      --  | | | | | element: First
      --  | | | | separator
      --  | | | | element: After
      --  | | | separator
      --  | | | element: new
      --  | | separator
      --  | | element: Before
      --  | separator
      --  | element: Last
      Iter   : constant Iterator   := Container.Iterate;
      Before : constant Node_Index := Iter.Next (After).Node;
   begin
      if After.Node = Invalid_Node_Index then
         Prepend (Container, New_Element);
      elsif Before = Invalid_Node_Index then
         Append (Container, New_Element);
      else
         declare
            Parent      : constant Valid_Node_Index := Container.Tree.Parent (Before);
            Old_Child   : constant Valid_Node_Index := Container.Tree.Parent (After.Node);
            Child_Index : constant SAL.Peek_Type    := Container.Tree.Child_Index (Parent, Old_Child);

            New_List_Nonterm : constant Valid_Node_Index := Container.Tree.Add_Nonterm
              (Production => (Container.List_ID, Container.Multi_Element_RHS),
               Children   =>
                 (if Container.Separator_ID = Invalid_Token_ID
                  then (Old_Child, New_Element)
                  else (Old_Child, Container.Tree.Add_Terminal (Container.Separator_ID), New_Element)));

         begin
            --  After = Container.First is not a special case:
            --
            --  list: Tree.Root
            --  | list
            --  | | list = Parent
            --  | | | list: new_list_nonterm
            --  | | | | list
            --  | | | | | element: First = After
            --  | | | | separator
            --  | | | | element: New_Element
            --  | | | separator
            --  | | | element: Before
            --
            --  Typical case:
            --
            --  | | list = Parent
            --  | | | list: New_list_nonterm
            --  | | | | | ...
            --  | | | | separator
            --  | | | | element: After
            --  | | | separator
            --  | | | element: New_Element
            --  | | separator
            --  | | element: Before

            Container.Tree.Replace_Child
              (Parent               => Parent,
               Child_Index          => Child_Index,
               Old_Child            => Deleted_Child,
               New_Child            => New_List_Nonterm,
               Old_Child_New_Parent => New_List_Nonterm);
         end;
      end if;
   end Insert;

   procedure Copy
     (Source_List  : in     Constant_List'Class;
      Source_First : in     Cursor := No_Element;
      Source_Last  : in     Cursor := No_Element;
      Dest_List    : in out List'Class)
   is
      Source_Iter : constant Constant_Iterator := Source_List.Iterate_Constant;

      Item : Cursor          := (if Source_First = No_Element then Source_List.First else Source_First);
      Last : constant Cursor := (if Source_Last = No_Element then Source_List.Last else Source_Last);
   begin
      for N of Source_List loop
         exit when not Has_Element (Item);

         Dest_List.Append (Dest_List.Tree.Copy_Subtree (Item.Node));

         exit when Item = Last;

         Item := Source_Iter.Next (Item);
      end loop;
   end Copy;

   procedure Delete
     (Container : in out List;
      Item      : in out Cursor)
   is
      Tree : Syntax_Trees.Tree renames Container.Tree.all;
   begin
      if Container.First = Container.Last then
         --  result is empty
         declare
            List_Parent : constant Node_Index := Tree.Parent (Container.Root);
         begin
            if List_Parent = Invalid_Node_Index then
               if Tree.Root = Container.Root then
                  Tree.Root := Invalid_Node_Index;
               end if;

            else
               Tree.Replace_Child
                 (List_Parent,
                  Child_Index => Tree.Child_Index (List_Parent, Container.Root),
                  Old_Child => Container.Root,
                  New_Child => Deleted_Child);
            end if;
            Container.Root := Invalid_Node_Index;
         end;

      elsif Item = Container.First then
         --  Before:
         --
         --  0011: | List_1: Parent_2
         --  0009: | | List_0: delete
         --  0008: | | | Element_0: old First: Item.Node: delete
         --  0001: | | | | ...
         --  0002: | | separator?: delete
         --  0010: | | Element_0: new First
         --  0003: | | | ...

         --
         --  After:
         --
         --  0011: | List_0: Parent_2
         --  0010: | | Element_0: new First
         --  0003: | | | ...

         declare
            Parent_2 : constant Valid_Node_Index := Tree.Parent (Item.Node, 2);
         begin
            Tree.Set_Children
              (Parent_2,
               (Container.List_ID, Container.One_Element_RHS),
               (1 => Tree.Child (Parent_2, (if Container.Separator_ID = Invalid_Token_ID then 2 else 3))));
         end;

      elsif Item = Container.Last then
         --  Before:
         --
         --   ?  ?: List_Parent
         --  15: | List_1 : Root, delete
         --  11: | | List_*: New_Root
         --  10: | | | Element_0
         --  03: | | ...
         --  06: | | separator?, delete
         --  14: | | Element_0 : Last. delete
         --  07: | | | ...

         --   ?  ?: List_Parent
         --  11: | List_*: Root
         --  10: | | Element_0
         --  03: | ...

         declare
            List_Parent : constant Node_Index       := Tree.Parent (Container.Root);
            New_Root    : constant Valid_Node_Index := Tree.Child (Container.Root, 1);
         begin
            if List_Parent = Invalid_Node_Index then
               Tree.Delete_Parent (New_Root);
               Container.Root := New_Root;

            else
               declare
                  Parent_Index : constant SAL.Peek_Type := Tree.Child_Index (List_Parent, Container.Root);
               begin
                  Tree.Replace_Child
                    (List_Parent, Parent_Index,
                     Old_Child            => Container.Root,
                     New_Child            => New_Root,
                     Old_Child_New_Parent => Invalid_Node_Index);
               end;
            end if;

            Container.Root := New_Root;
         end;

      else
         --  Node numbers from test_lr_utils test case 1.
         --
         --  before:
         --  15: list: Parent_2
         --  13: | list: Parent_1, Old_Child
         --  11: | | list: Parent_1_Child_1, New_Child
         --  09: | | | list:
         --  08: | | | | element: 1, First
         --  02: | | | separator?
         --  10: | | | element: 2
         --  04: | | separator?
         --  12: | | element: 3, Item.Node, delete
         --  06: | separator?
         --  14: | element: 4, Last
         --
         --  after
         --  15: list: Parent_2
         --  11: | list: Parent_1_Child_1
         --  09: | | list:
         --  08: | | | element: 1, First
         --  02: | | separator?
         --  10: | | element: 2
         --  06: | separator?
         --  14: | element: 4, Last

         declare
            Parent_1         : constant Valid_Node_Index := Tree.Parent (Item.Node);
            Parent_2         : constant Valid_Node_Index := Tree.Parent (Parent_1);
            Parent_1_Child_1 : constant Valid_Node_Index := Tree.Child (Parent_1, 1);
         begin
            Tree.Replace_Child
              (Parent_2, 1,
               Old_Child            => Parent_1,
               New_Child            => Parent_1_Child_1,
               Old_Child_New_Parent => Invalid_Node_Index);
         end;
      end if;

      Item.Node := Invalid_Node_Index;
   end Delete;

   function Valid_Skip_List (Tree : aliased in out Syntax_Trees.Tree; Skip_List : in Skip_Array) return Boolean
   is begin
      if Skip_List'Length = 0 then return False; end if;

      if Skip_List (Skip_List'Last).Label /= Skip then return False; end if;

      if (for some I in Skip_List'First .. Skip_List'Last - 1 => Skip_List (I).Label /= Nested) then
         return False;
      end if;

      for I in Skip_List'First + 1 .. Skip_List'Last loop
         if Tree.ID (Skip_List (I).Element) /= Skip_List (I - 1).Element_ID then
            return False;
         end if;
      end loop;

      if Skip_List'Length > 2 then
         declare
            I : constant Positive_Index_Type := Skip_List'Last - 1;
         begin
            if Creators.Create_From_Element
              (Tree, Skip_List (I - 1).Element, Skip_List (I).List_ID, Skip_List (I).Element_ID).Count = 1
            then
               return False;
            end if;
         end;
      end if;

      return True;
   end Valid_Skip_List;

   function Copy_Skip_Nested
     (Source_List       :         in     Constant_List'Class;
      Skip_List         :         in     Skip_Array;
      Skip_Found        :         in out Boolean;
      Tree              : aliased in out Syntax_Trees.Tree;
      Separator_ID      :         in     Token_ID;
      Multi_Element_RHS :         in     Natural)
      return Node_Index
   is
      Dest_List : List := Creators.Empty_List
        (Tree, Source_List.List_ID, Multi_Element_RHS, Source_List.Element_ID, Separator_ID);

      function Get_Dest_Child
        (Node      : in Valid_Node_Index;
         Skip_List : in Skip_Array)
        return Valid_Node_Index
      with Pre => Tree.Is_Nonterm (Node) and
                  (Skip_List'Length > 1 and then
                   (Skip_List (Skip_List'First).Label = Nested and Skip_List (Skip_List'Last).Label = Skip))
      is
         Skip_This : Nested_Skip_Item renames Skip_List (Skip_List'First);
      begin
         if Node = Skip_This.List_Root then
            return Copy_Skip_Nested
              (Creators.Create_List
                 (Tree,
                  Root       => Skip_This.List_Root,
                  List_ID    => Skip_This.List_ID,
                  Element_ID => Skip_This.Element_ID),
               Skip_List (Skip_List'First + 1 .. Skip_List'Last),
               Skip_Found, Tree, Skip_This.Separator_ID, Skip_This.Multi_Element_RHS);
         else
            declare
               Source_Children : constant Valid_Node_Index_Array := Tree.Children (Node);
               Dest_Children   : Valid_Node_Index_Array (Source_Children'Range);
            begin
               for I in Source_Children'Range loop
                  if Source_Children (I) = Skip_This.List_Root then
                     Dest_Children (I) := Copy_Skip_Nested
                       (Creators.Create_List
                          (Tree,
                           Root       => Skip_This.List_Root,
                           List_ID    => Skip_This.List_ID,
                           Element_ID => Skip_This.Element_ID),
                        Skip_List (Skip_List'First + 1 .. Skip_List'Last),
                        Skip_Found, Tree, Skip_This.Separator_ID, Skip_This.Multi_Element_RHS);
                  else
                     if Tree.Label (Source_Children (I)) = Nonterm then
                        Dest_Children (I) := Get_Dest_Child (Source_Children (I), Skip_List);
                     else
                        Dest_Children (I) := Tree.Copy_Subtree (Source_Children (I));
                     end if;
                  end if;
               end loop;

               return Tree.Add_Nonterm (Tree.Production_ID (Node), Dest_Children, Tree.Action (Node));
            end;
         end if;
      end Get_Dest_Child;

      Skip_This : Nested_Skip_Item renames Skip_List (Skip_List'First);
   begin
      --  See test_lr_utils.adb Test_Copy_Skip for an example.
      for N of Source_List loop
         if Skip_This.Element = N then
            case Skip_This.Label is
            when Skip =>
               --  Done nesting; skip this one
               Skip_Found := True;

            when Nested =>
               Dest_List.Append (Get_Dest_Child (N, Skip_List));
            end case;
         else
            Dest_List.Append (Tree.Copy_Subtree (N));
         end if;
      end loop;
      return Dest_List.Root;
   end Copy_Skip_Nested;

   function Copy_Skip_Nested
     (Skip_List :         in     Skip_Info;
      Tree      : aliased in out Syntax_Trees.Tree)
     return Node_Index
   is
      Source_List : constant Constant_List := Creators.Create_List
        (Tree,
         Root       => Skip_List.Start_List_Root,
         List_ID    => Skip_List.Start_List_ID,
         Element_ID => Skip_List.Start_Element_ID);

      Skip_Found : Boolean := False;
   begin
      return Result : constant Node_Index := Copy_Skip_Nested
        (Source_List, Skip_List.Skips, Skip_Found, Tree, Skip_List.Start_Separator_ID,
         Skip_List.Start_Multi_Element_RHS)
      do
         if not Skip_Found then
            raise SAL.Programmer_Error with "Skip not found";
         end if;
      end return;
   end Copy_Skip_Nested;

   function List_Root
     (Tree    : in Syntax_Trees.Tree;
      Node    : in Valid_Node_Index;
      List_ID : in Token_ID)
     return Valid_Node_Index
   is
      Root : Node_Index := Node;
   begin
      loop
         exit when Tree.Parent (Root) = Invalid_Node_Index or else Tree.ID (Tree.Parent (Root)) /= List_ID;
         Root := Tree.Parent (Root);
      end loop;
      return Root;
   end List_Root;

end WisiToken.Syntax_Trees.LR_Utils;
