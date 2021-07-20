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

with Ada.Containers;
with Ada.Text_IO;
with SAL.Generic_Decimal_Image;
package body WisiToken.Syntax_Trees is

   --  Body specs, alphabetical, as needed

   function Image
     (Tree              : in Syntax_Trees.Tree;
      N                 : in Syntax_Trees.Node;
      Node_Index        : in Valid_Node_Index;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String;

   procedure Move_Branch_Point (Tree : in out Syntax_Trees.Tree; Required_Node : in Valid_Node_Index);

   type Visit_Parent_Mode is (Before, After);

   function Process_Tree
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Valid_Node_Index;
      Visit_Parent : in Visit_Parent_Mode;
      Process_Node : access function
        (Tree : in Syntax_Trees.Tree;
         Node : in Valid_Node_Index)
        return Boolean)
     return Boolean;
   --  Call Process_Node on nodes in tree rooted at Node. Return when
   --  Process_Node returns False (Process_Tree returns False), or when
   --  all nodes have been processed (Process_Tree returns True).

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array);

   ----------
   --  Public and body operations, alphabetical

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).Action
         else Tree.Branched_Nodes (Node).Action);
   end Action;

   procedure Add_Child
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Index;
      Child  : in     Valid_Node_Index)
   is
      Node : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Parent);
   begin
      Node.Children.Append (Child);
      Tree.Shared_Tree.Nodes (Child).Parent := Parent;
   end Add_Child;

   function Add_Identifier
     (Tree        : in out Syntax_Trees.Tree;
      ID          : in     Token_ID;
      Identifier  : in     Identifier_Index;
      Byte_Region : in     WisiToken.Buffer_Region)
     return Valid_Node_Index
   is begin
      Tree.Shared_Tree.Nodes.Append
        ((Label       => Virtual_Identifier,
          Byte_Region => Byte_Region,
          ID          => ID,
          Identifier  => Identifier,
          others      => <>));
      Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
      return Tree.Last_Shared_Node;
   end Add_Identifier;

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     WisiToken.Production_ID;
      Children        : in     Valid_Node_Index_Array;
      Action          : in     Semantic_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Index
   is
      Nonterm_Node : Valid_Node_Index;
   begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label      => Syntax_Trees.Nonterm,
             ID         => Production.LHS,
             Action     => Action,
             RHS_Index  => Production.RHS,
             Virtual    => (if Children'Length = 0 then Default_Virtual else False),
             others     => <>));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         Nonterm_Node          := Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label     => Syntax_Trees.Nonterm,
             ID        => Production.LHS,
             Action    => Action,
             RHS_Index => Production.RHS,
             Virtual   => (if Children'Length = 0 then Default_Virtual else False),
             others    => <>));
         Nonterm_Node := Tree.Branched_Nodes.Last_Index;
      end if;

      if Children'Length = 0 then
         return Nonterm_Node;
      end if;

      Set_Children (Tree, Nonterm_Node, Children);

      return Nonterm_Node;
   end Add_Nonterm;

   function Add_Terminal
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Token_Index;
      Terminals : in     Base_Token_Arrays.Vector)
     return Valid_Node_Index
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label       => Shared_Terminal,
             ID          => Terminals (Terminal).ID,
             Byte_Region => Terminals (Terminal).Byte_Region,
             Terminal    => Terminal,
             others      => <>));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label       => Shared_Terminal,
             ID          => Terminals (Terminal).ID,
             Byte_Region => Terminals (Terminal).Byte_Region,
             Terminal    => Terminal,
             others      => <>));
         return Tree.Branched_Nodes.Last_Index;
      end if;
   end Add_Terminal;

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID;
      Before   : in     Base_Token_Index := Invalid_Token_Index)
     return Valid_Node_Index
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes.Append
           ((Label  => Virtual_Terminal,
             ID     => Terminal,
             Before => Before,
             others => <>));
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      else
         Tree.Branched_Nodes.Append
           ((Label  => Virtual_Terminal,
             ID     => Terminal,
             Before => Before,
             others => <>));
         return Tree.Branched_Nodes.Last_Index;
      end if;
   end Add_Terminal;

   function Before
     (Tree             : in Syntax_Trees.Tree;
      Virtual_Terminal : in Valid_Node_Index)
     return Base_Token_Index
   is begin
      if Tree.Flush then
         return Tree.Shared_Tree.Nodes (Virtual_Terminal).Before;
      else
         return Tree.Branched_Nodes (Virtual_Terminal).Before;
      end if;
   end Before;

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Base_Token_Class_Access
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Augmented;
      else
         return Tree.Branched_Nodes (Node).Augmented;
      end if;
   end Augmented;

   function Augmented_Const
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Base_Token_Class_Access_Constant
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Base_Token_Class_Access_Constant (Tree.Shared_Tree.Nodes (Node).Augmented);
      else
         return Base_Token_Class_Access_Constant (Tree.Branched_Nodes (Node).Augmented);
      end if;
   end Augmented_Const;

   function Buffer_Region_Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Byte_Region = Null_Buffer_Region;
      else
         return Tree.Branched_Nodes (Node).Byte_Region = Null_Buffer_Region;
      end if;
   end Buffer_Region_Is_Empty;

   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Buffer_Region
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).Byte_Region
         else Tree.Branched_Nodes (Node).Byte_Region);
   end Byte_Region;

   function Child
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Index;
      Child_Index : in Positive_Index_Type)
     return Node_Index
   is
      function Compute (N : in Syntax_Trees.Node) return Node_Index
      is begin
         if N.Label /= Nonterm then
            return Invalid_Node_Index;

         elsif Child_Index in N.Children.First_Index .. N.Children.Last_Index then
            return N.Children (Child_Index);
         else
            return Invalid_Node_Index;
         end if;
      end Compute;
   begin
      if Node <= Tree.Last_Shared_Node then
         return Compute (Tree.Shared_Tree.Nodes (Node));
      else
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end Child;

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Ada.Containers.Count_Type
   is begin
      return Tree.Get_Node_Const_Ref (Node).Children.Length;
   end Child_Count;

   function Child_Index
     (N     : in Node;
      Child : in Valid_Node_Index)
     return SAL.Peek_Type
   is begin
      for I in N.Children.First_Index .. N.Children.Last_Index loop
         if N.Children (I) = Child then
            return I;
         end if;
      end loop;
      raise SAL.Programmer_Error; -- Should be prevented by precondition
   end Child_Index;

   function Child_Index
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Index;
      Child  : in     Valid_Node_Index)
     return SAL.Peek_Type
   is
      N : Node_Var_Ref renames Get_Node_Var_Ref (Tree, Parent);
   begin
      return Child_Index (N, Child);
   end Child_Index;

   function Children (N : in Syntax_Trees.Node) return Valid_Node_Index_Array
   is begin
      if N.Children.Length = 0 then
         return (1 .. 0 => <>);
      else
         return Result : Valid_Node_Index_Array (N.Children.First_Index .. N.Children.Last_Index) do
            for I in Result'Range loop
               Result (I) := N.Children (I);
            end loop;
         end return;
      end if;
   end Children;

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Children (Tree.Shared_Tree.Nodes (Node));
      else
         return Children (Tree.Branched_Nodes (Node));
      end if;
   end Children;

   procedure Clear (Tree : in out Syntax_Trees.Base_Tree)
   is begin
      Tree.Finalize;
   end Clear;

   procedure Clear (Tree : in out Syntax_Trees.Tree)
   is begin
      if Tree.Shared_Tree.Augmented_Present then
         for Node of Tree.Branched_Nodes loop
            if Node.Label = Nonterm then
               Free (Node.Augmented);
            end if;
         end loop;
      end if;
      Tree.Shared_Tree.Finalize;
      Tree.Last_Shared_Node := Invalid_Node_Index;
      Tree.Branched_Nodes.Clear;
   end Clear;

   function Copy_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in     Valid_Node_Index)
     return Valid_Node_Index
   is
      function Copy_Node
        (Tree   : in out Syntax_Trees.Tree;
         Index  : in     Valid_Node_Index;
         Parent : in     Node_Index)
        return Valid_Node_Index
      is begin
         case Tree.Shared_Tree.Nodes (Index).Label is
         when Shared_Terminal =>
            declare
               Node : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Index);
            begin
               Tree.Shared_Tree.Nodes.Append
                 ((Label       => Shared_Terminal,
                   ID          => Node.ID,
                   Byte_Region => Node.Byte_Region,
                   Parent      => Parent,
                   State       => Unknown_State,
                   Augmented   => Node.Augmented,
                   Terminal    => Node.Terminal));
            end;

         when Virtual_Terminal =>
            declare
               Node : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Index);
            begin
               Tree.Shared_Tree.Nodes.Append
                 ((Label       => Virtual_Terminal,
                   ID          => Node.ID,
                   Byte_Region => Node.Byte_Region,
                   Parent      => Parent,
                   State       => Unknown_State,
                   Augmented   => Node.Augmented,
                   Before      => Node.Before));
            end;

         when Virtual_Identifier =>
            declare
               Node : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Index);
            begin
               Tree.Shared_Tree.Nodes.Append
                 ((Label       => Virtual_Identifier,
                   ID          => Node.ID,
                   Byte_Region => Node.Byte_Region,
                   Parent      => Parent,
                   State       => Unknown_State,
                   Augmented   => Node.Augmented,
                   Identifier  => Node.Identifier));
            end;

         when Nonterm =>
            declare
               Children     : constant Valid_Node_Index_Array := Tree.Children (Index);
               Parent       : Node_Index                      := Invalid_Node_Index;
               New_Children : Valid_Node_Index_Arrays.Vector;
            begin
               if Children'Length > 0 then
                  New_Children.Set_First_Last (Children'First, Children'Last);
                  for I in Children'Range loop
                     New_Children (I) := Copy_Node (Tree, Children (I), Parent);
                  end loop;
               end if;

               declare
                  Node : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Index);
               begin
                  Tree.Shared_Tree.Nodes.Append
                    ((Label              => Nonterm,
                      ID                 => Node.ID,
                      Byte_Region        => Node.Byte_Region,
                      Parent             => Parent,
                      State              => Unknown_State,
                      Augmented          => Node.Augmented,
                      Virtual            => Node.Virtual,
                      RHS_Index          => Node.RHS_Index,
                      Action             => Node.Action,
                      Name               => Node.Name,
                      Children           => New_Children,
                      Min_Terminal_Index => Node.Min_Terminal_Index));
               end;

               Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
               Parent := Tree.Last_Shared_Node;
               for I in New_Children.First_Index .. New_Children.Last_Index loop
                  Tree.Shared_Tree.Nodes (New_Children (I)).Parent := Parent;
               end loop;

               return Parent;
            end;
         end case;
         Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
         return Tree.Last_Shared_Node;
      end Copy_Node;

   begin
      return Copy_Node (Tree, Root, Invalid_Node_Index);
   end Copy_Subtree;

   function Count_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return SAL.Base_Peek_Type
   is
      function Compute (N : in Syntax_Trees.Node) return SAL.Base_Peek_Type
      is
         use all type SAL.Base_Peek_Type;
      begin
         return Result : SAL.Base_Peek_Type := 0 do
            if N.ID = ID then
               Result := 1;
            end if;
            case N.Label is
            when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
               null;
            when Nonterm =>
               for I of N.Children loop
                  --  We don't check for Deleted_Child here; encountering one indicates
                  --  an error in the user algorithm.
                  Result := Result + Count_IDs (Tree, I, ID);
               end loop;
            end case;
         end return;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Count_IDs;

   function Count_Terminals
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Integer
     --  Count_Terminals must return Integer for Get_Terminals,
     --  Positive_Index_Type for Get_Terminal_IDs.
   is
      function Compute (N : in Syntax_Trees.Node) return Integer
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return 1;

         when Nonterm =>
            return Result : Integer := 0 do
               for C of N.Children loop
                  --  This can be called to build a debugging image while editing the tree
                  if C /= Deleted_Child then
                     Result := Result + Count_Terminals (Tree, C);
                  end if;
               end loop;
            end return;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Count_Terminals;

   procedure Delete_Parent
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Index)
   is
      N      : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Node);
      Parent : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (N.Parent);
   begin
      Parent.Children (Child_Index (Parent, Node)) := Deleted_Child;

      if N.Parent = Tree.Root then
         Tree.Root := Node;
      end if;

      N.Parent := Invalid_Node_Index;
   end Delete_Parent;

   function Error_Message
     (Tree      : in Syntax_Trees.Tree;
      Terminals : in Base_Token_Array_Access_Constant;
      Node      : in Valid_Node_Index;
      File_Name : in String;
      Message   : in String)
     return String
   is
      First_Terminal : constant Valid_Node_Index  := Tree.First_Terminal (Node);
      Line           : Line_Number_Type  := Line_Number_Type'First;
      Column         : Ada.Text_IO.Count := Ada.Text_IO.Count'First;
   begin
      case Tree.Label (First_Terminal) is
      when Shared_Terminal =>
         declare
            Token : Base_Token renames Terminals.all (Tree.First_Shared_Terminal (First_Terminal));
         begin
            Line   := Token.Line;
            Column := Token.Column;
         end;

      when Virtual_Terminal | Virtual_Identifier =>
         Line   := Line_Number_Type'First;
         Column := Ada.Text_IO.Count (Tree.Byte_Region (First_Terminal).First);

      when others =>
         null;
      end case;
      return WisiToken.Error_Message (File_Name, Line, Column, Message);
   end Error_Message;

   overriding procedure Finalize (Tree : in out Base_Tree)
   is begin
      Tree.Traversing  := False;
      Tree.Parents_Set := False;
      if Tree.Augmented_Present then
         for Node of Tree.Nodes loop
            if Node.Label = Nonterm then
               Free (Node.Augmented);
            end if;
         end loop;
         Tree.Augmented_Present := False;
      end if;
      Tree.Nodes.Finalize;
   end Finalize;

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree)
   is begin
      if Tree.Last_Shared_Node /= Invalid_Node_Index then
         --  Tree.Branched_Nodes Augmented are shallow copies of
         --  Tree.Shared_Tree.Nodes Augmented, so we don't free them there;
         --  they are freed in Base_Tree.Finalize above.
         Tree.Branched_Nodes.Finalize;
         Tree.Last_Shared_Node := Invalid_Node_Index;
         Tree.Shared_Tree := null;
      end if;
   end Finalize;

   function Insert_After
     (User_Data            : in out User_Data_Type;
      Tree                 : in     Syntax_Trees.Tree'Class;
      Token                : in     Valid_Node_Index;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean
   is
      pragma Unreferenced (User_Data, Tree, Token, Insert_On_Blank_Line);
   begin
      return False;
   end Insert_After;

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      ID         : in Token_ID;
      Max_Parent : in Boolean := False)
     return Node_Index
   is
      N           : Node_Index := Node;
      Last_Parent : Node_Index := Invalid_Node_Index;
   begin
      loop
         N :=
           (if N <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (N).Parent
            else Tree.Branched_Nodes (N).Parent);

         exit when N = Invalid_Node_Index;
         Last_Parent := N;

         exit when ID =
           (if N <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (N).ID
            else Tree.Branched_Nodes (N).ID);
      end loop;

      return (if Max_Parent then Last_Parent else N);
   end Find_Ancestor;

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      IDs        : in Token_ID_Array;
      Max_Parent : in Boolean := False)
     return Node_Index
   is
      N           : Node_Index := Node;
      Last_Parent : Node_Index := Invalid_Node_Index;
   begin
      loop
         N :=
           (if N <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (N).Parent
            else Tree.Branched_Nodes (N).Parent);

         exit when N = Invalid_Node_Index;
         Last_Parent := N;

         exit when
           (for some ID of IDs => ID =
              (if N <= Tree.Last_Shared_Node
               then Tree.Shared_Tree.Nodes (N).ID
               else Tree.Branched_Nodes (N).ID));
      end loop;
      return (if Max_Parent then Last_Parent else N);
   end Find_Ancestor;

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      function Compute (N : in Syntax_Trees.Node) return Node_Index
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Invalid_Node_Index;
         when Nonterm =>
            for C of N.Children loop
               if C /= Deleted_Child then
                  if ID =
                    (if C <= Tree.Last_Shared_Node
                     then Tree.Shared_Tree.Nodes (C).ID
                     else Tree.Branched_Nodes (C).ID)
                  then
                     return C;
                  end if;
               end if;
            end loop;
            return Invalid_Node_Index;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Find_Child;

   function Find_Descendant
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      Found : Node_Index := Invalid_Node_Index;

      function Process (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
      is
         Node_ID : constant Token_ID :=
           (if Node <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (Node).ID
            else Tree.Branched_Nodes (Node).ID);
      begin
         if Node_ID = ID then
            Found := Node;
            return False;
         else
            return True;
         end if;
      end Process;

      Junk : constant Boolean := Process_Tree (Tree, Node, Before, Process'Access);
      pragma Unreferenced (Junk);
   begin
      return Found;
   end Find_Descendant;

   function Find_Descendant
     (Tree      : in     Syntax_Trees.Tree;
      Node      : in     Valid_Node_Index;
      Predicate : access function (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean)
     return Node_Index
   is
      Found : Node_Index := Invalid_Node_Index;

      function Process (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
      is begin
         if Predicate (Tree, Node) then
            Found := Node;
            return False;
         else
            return True;
         end if;
      end Process;

      Junk : constant Boolean := Process_Tree (Tree, Node, Before, Process'Access);
      pragma Unreferenced (Junk);
   begin
      return Found;
   end Find_Descendant;

   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   is
      function Compute_2 (N : in Syntax_Trees.Node) return Node_Index
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Invalid_Node_Index;

         when Nonterm =>
            for C of N.Children loop
               if C /= Deleted_Child then
                  if ID =
                    (if C <= Tree.Last_Shared_Node
                     then Tree.Shared_Tree.Nodes (C).ID
                     else Tree.Branched_Nodes (C).ID)
                  then
                     return C;
                  end if;
               end if;
            end loop;
            return Invalid_Node_Index;
         end case;
      end Compute_2;

      function Compute_1 (Parent : in Node_Index) return Node_Index
      is begin
         if Parent = Invalid_Node_Index then
            return Invalid_Node_Index;

         else
            return Compute_2
              ((if Parent <= Tree.Last_Shared_Node
                then Tree.Shared_Tree.Nodes (Parent)
                else Tree.Branched_Nodes (Parent)));
         end if;
      end Compute_1;
   begin
      return Compute_1
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node).Parent
          else Tree.Branched_Nodes (Node).Parent));
   end Find_Sibling;

   function First_Index (Tree : in Syntax_Trees.Tree) return Node_Index
   is begin
      return Tree.Shared_Tree.Nodes.First_Index;
   end First_Index;

   procedure Flush (Tree : in out Syntax_Trees.Tree)
   is begin
      --  This is the opposite of Move_Branch_Point
      Tree.Shared_Tree.Nodes.Merge (Tree.Branched_Nodes);
      Tree.Last_Shared_Node := Tree.Shared_Tree.Nodes.Last_Index;
      Tree.Flush            := True;
   end Flush;

   function Flushed (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Flush;
   end Flushed;

   procedure Get_IDs
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      ID     : in     Token_ID;
      Result : in out Valid_Node_Index_Array;
      Last   : in out SAL.Base_Peek_Type)
   is
      use all type SAL.Base_Peek_Type;

      procedure Compute (N : in Syntax_Trees.Node)
      is begin
         if N.ID = ID then
            Last := Last + 1;
            Result (Last) := Node;
         end if;
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            null;
         when Nonterm =>
            for I of N.Children loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               Get_IDs (Tree, I, ID, Result, Last);
            end loop;
         end case;
      end Compute;
   begin
      Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Get_IDs;

   function Get_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Valid_Node_Index_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      Tree.Shared_Tree.Traversing := True;
      return Result : Valid_Node_Index_Array (1 .. Count_IDs (Tree, Node, ID)) do
         Get_IDs (Tree, Node, ID, Result, Last);
         Tree.Shared_Tree.Traversing := False;
      end return;
   end Get_IDs;

   procedure Get_Terminals
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Result : in out Valid_Node_Index_Array;
      Last   : in out SAL.Base_Peek_Type)
   is
      use all type SAL.Base_Peek_Type;

      procedure Compute (N : in Syntax_Trees.Node)
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            Last := Last + 1;
            Result (Last) := Node;

         when Nonterm =>
            for C of N.Children loop
               --  This is called to build an edited source image while editing the tree
               if C /= Deleted_Child then
                  Get_Terminals (Tree, C, Result, Last);
               end if;
            end loop;
         end case;
      end Compute;
   begin
      Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Get_Terminals;

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      Tree.Shared_Tree.Traversing := True;
      return Result : Valid_Node_Index_Array (1 .. SAL.Base_Peek_Type (Count_Terminals (Tree, Node))) do
         Get_Terminals (Tree, Node, Result, Last);
         Tree.Shared_Tree.Traversing := False;
      end return;
   end Get_Terminals;

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index
   is
      function Compute (Index : in Valid_Node_Index; N : in Syntax_Trees.Node) return Node_Index
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Index;
         when Nonterm =>
            for C of N.Children loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               declare
                  Term : constant Node_Index := First_Terminal (Tree, C);
               begin
                  if Term /= Invalid_Node_Index then
                     return Term;
                  end if;
               end;
            end loop;
            return Invalid_Node_Index;
         end case;
      end Compute;
   begin
      return Compute
        (Node,
         (if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end First_Terminal;

   procedure Get_Terminal_IDs
     (Tree   : in     Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Result : in out Token_ID_Array;
      Last   : in out SAL.Base_Peek_Type)
   is
      procedure Compute (N : in Syntax_Trees.Node)
      is
         use all type SAL.Base_Peek_Type;
      begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            Last := Last + 1;
            Result (Integer (Last)) := N.ID;

         when Nonterm =>
            for I of N.Children loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               Get_Terminal_IDs (Tree, I, Result, Last);
            end loop;
         end case;
      end Compute;
   begin
      Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Get_Terminal_IDs;

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Token_ID_Array
   is
      Last : SAL.Base_Peek_Type := 0;
   begin
      Tree.Shared_Tree.Traversing := True;
      return Result : Token_ID_Array (1 .. Count_Terminals (Tree, Node))  do
         Get_Terminal_IDs (Tree, Node, Result, Last);
         Tree.Shared_Tree.Traversing := False;
      end return;
   end Get_Terminal_IDs;

   function First_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index
   is
      function Compute (N : in Syntax_Trees.Node) return Base_Token_Index
      is begin
         return
           (case N.Label is
            when Shared_Terminal => N.Terminal,
            when Virtual_Terminal |
              Virtual_Identifier => Invalid_Token_Index,
            when Nonterm         => N.Min_Terminal_Index);
      end Compute;

   begin
      if Node <= Tree.Last_Shared_Node then
         return Compute (Tree.Shared_Tree.Nodes (Node));
      else
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end First_Shared_Terminal;

   function First_Terminal_ID (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Token_ID
   is
      function Compute (N : in Syntax_Trees.Node) return Token_ID
      is begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return N.ID;

         when Nonterm =>
            for C of N.Children loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               declare
                  ID : constant Token_ID := First_Terminal_ID (Tree, C);
               begin
                  if ID /= Invalid_Token_ID then
                     return ID;
                  end if;
               end;
            end loop;
            return Invalid_Token_ID;
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end First_Terminal_ID;

   function Has_Branched_Nodes (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Branched_Nodes.Length > 0;
   end Has_Branched_Nodes;

   function Has_Child
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Index;
      Child : in Valid_Node_Index)
     return Boolean
   is begin
      for C of Tree.Get_Node_Const_Ref (Node).Children loop
         if C = Child then
            return True;
         end if;
      end loop;
      return False;
   end Has_Child;

   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Children.Length > 0;
      else
         return Tree.Branched_Nodes (Node).Children.Length > 0;
      end if;
   end Has_Children;

   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Index) return Boolean
   is begin
      return
        (if Child <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Child).Parent /= Invalid_Node_Index
         else Tree.Branched_Nodes (Child).Parent /= Invalid_Node_Index);
   end Has_Parent;

   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Valid_Node_Index_Array) return Boolean
   is begin
      return
        (for some Child of Children =>
           (if Child <= Tree.Last_Shared_Node
            then Tree.Shared_Tree.Nodes (Child).Parent /= Invalid_Node_Index
            else Tree.Branched_Nodes (Child).Parent /= Invalid_Node_Index));
   end Has_Parent;

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Token_ID
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).ID
         else Tree.Branched_Nodes (Node).ID);
   end ID;

   function Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Identifier_Index
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).Identifier
         else Tree.Branched_Nodes (Node).Identifier);
   end Identifier;

   function Image
     (Tree         : in Syntax_Trees.Tree;
      Children     : in Valid_Node_Index_Arrays.Vector;
      Descriptor   : in WisiToken.Descriptor;
      Node_Numbers : in Boolean)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I of Children loop
         Result := Result & (if Need_Comma then ", " else "") &
           (if I = Deleted_Child
            then "-"
            else Tree.Image (I, Descriptor, Include_Children => False, Node_Numbers => Node_Numbers));
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image
     (Tree              : in Syntax_Trees.Tree;
      N                 : in Syntax_Trees.Node;
      Node_Index        : in Valid_Node_Index;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +(if Node_Numbers then Image (Node_Index) & ":" else "");
   begin
      case N.Label is
      when Shared_Terminal =>
         Result := Result & Trimmed_Image (N.Terminal) & ":";

      when Virtual_Identifier =>
         Result := Result & Trimmed_Image (N.Identifier) & ";";

      when others =>
         null;
      end case;

      Result := Result & "(" & Image (N.ID, Descriptor) &
        (if Include_RHS_Index and N.Label = Nonterm then "_" & Trimmed_Image (N.RHS_Index) else "") &
        (if N.Byte_Region = Null_Buffer_Region then "" else ", " & Image (N.Byte_Region)) & ")";

      if Include_Children and N.Label = Nonterm then
         Result := Result & " <= " & Image (Tree, N.Children, Descriptor, Node_Numbers);
      end if;

      return -Result;
   end Image;

   function Image
     (Tree              : in Syntax_Trees.Tree;
      Node              : in Valid_Node_Index;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean := False;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String
   is begin
      return Tree.Image
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)),
         Node, Descriptor, Include_Children, Include_RHS_Index, Node_Numbers);
   end Image;

   function Image
     (Tree       : in Syntax_Trees.Tree;
      Nodes      : in Valid_Node_Index_Array;
      Descriptor : in WisiToken.Descriptor)
     return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean := False;
   begin
      for I in Nodes'Range loop
         Result := Result & (if Need_Comma then ", " else "") &
           Tree.Image (Nodes (I), Descriptor);
         Need_Comma := True;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image
     (Item     : in Node_Sets.Vector;
      Inverted : in Boolean := False)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for I in Item.First_Index .. Item.Last_Index loop
         if (if Inverted then not Item (I) else Item (I)) then
            Result := Result & Node_Index'Image (I);
         end if;
      end loop;
      return -Result;
   end Image;

   procedure Initialize
     (Branched_Tree : in out Syntax_Trees.Tree;
      Shared_Tree   : in     Base_Tree_Access;
      Flush         : in     Boolean;
      Set_Parents   : in     Boolean := False)
   is begin
      Branched_Tree :=
        (Ada.Finalization.Controlled with
         Shared_Tree      => Shared_Tree,
         Last_Shared_Node => Shared_Tree.Nodes.Last_Index,
         Branched_Nodes   => <>,
         Flush            => Flush,
         Root             => <>);

      Branched_Tree.Shared_Tree.Parents_Set := Set_Parents;
   end Initialize;

   function Is_Descendant_Of
     (Tree       : in Syntax_Trees.Tree;
      Root       : in Valid_Node_Index;
      Descendant : in Valid_Node_Index)
     return Boolean
   is
      Node : Node_Index := Descendant;
   begin
      loop
         exit when Node = Invalid_Node_Index;
         if Node = Root then
            return True;
         end if;

         Node := Tree.Parent (Node);
      end loop;
      return False;
   end Is_Descendant_Of;

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label = Nonterm;
      else
         return Tree.Branched_Nodes (Node).Label = Nonterm;
      end if;
   end Is_Nonterm;

   function Is_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label = Shared_Terminal;
      else
         return Tree.Branched_Nodes (Node).Label = Shared_Terminal;
      end if;
   end Is_Shared_Terminal;

   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label = Virtual_Terminal;
      else
         return Tree.Branched_Nodes (Node).Label = Virtual_Terminal;
      end if;
   end Is_Virtual_Terminal;

   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is
      function Compute (N : in Syntax_Trees.Node) return Boolean
      is begin
         return N.Label = Virtual_Terminal or (N.Label = Nonterm and then N.Virtual);
      end Compute;
   begin
      if Node <= Tree.Last_Shared_Node then
         return Compute (Tree.Shared_Tree.Nodes (Node));
      else
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end Is_Virtual;

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).Label = Virtual_Identifier
         else Tree.Branched_Nodes (Node).Label = Virtual_Identifier);
   end Is_Virtual_Identifier;

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Label
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Label;
      else
         return Tree.Branched_Nodes (Node).Label;
      end if;
   end Label;

   function Last_Index (Tree : in Syntax_Trees.Tree) return Node_Index
   is begin
      return
        (if Tree.Flush
         then Tree.Shared_Tree.Nodes.Last_Index
         else Tree.Branched_Nodes.Last_Index);
   end Last_Index;

   function Last_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index
   is
      --  Max_Terminal_Index is not cached, because it is not needed in recover.

      function Compute (N : in Syntax_Trees.Node) return Base_Token_Index
      is begin
         case N.Label is
         when Shared_Terminal =>
            return N.Terminal;

         when Virtual_Terminal | Virtual_Identifier =>
            return Invalid_Token_Index;

         when Nonterm =>
            for C of reverse N.Children loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               declare
                  Last_Term : constant Base_Token_Index := Tree.Last_Shared_Terminal (C);
               begin
                  if Last_Term /= Invalid_Token_Index then
                     return Last_Term;
                  end if;
               end;
            end loop;
            return Invalid_Token_Index;
         end case;
      end Compute;

   begin
      if Node <= Tree.Last_Shared_Node then
         return Compute (Tree.Shared_Tree.Nodes (Node));
      else
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end Last_Shared_Terminal;

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index
   is
      N : constant Node_Const_Ref := Tree.Get_Node_Const_Ref (Node);
   begin
      case N.Label is
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Node;
      when Nonterm =>
         for C of reverse N.Children loop
            --  Encountering Deleted_Child here is an error in the user algorithm.
            declare
               Term : constant Node_Index := Last_Terminal (Tree, C);
            begin
               if Term /= Invalid_Node_Index then
                  return Term;
               end if;
            end;
         end loop;
         return Invalid_Node_Index;
      end case;
   end Last_Terminal;

   function Min_Descendant (Nodes : in Node_Arrays.Vector; Node : in Valid_Node_Index) return Valid_Node_Index
   is
      N : Syntax_Trees.Node renames Nodes (Node);
   begin
      case N.Label is
      when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
         return Node;

      when Nonterm =>
         declare
            Min : Node_Index := Node;
         begin
            for C of N.Children loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               Min := Node_Index'Min (Min, Min_Descendant (Nodes, C));
            end loop;
            return Min;
         end;
      end case;
   end Min_Descendant;

   procedure Move_Branch_Point (Tree : in out Syntax_Trees.Tree; Required_Node : in Valid_Node_Index)
   is begin
      --  Note that this preserves all stored indices in Branched_Nodes.
      Tree.Branched_Nodes.Prepend (Tree.Shared_Tree.Nodes, Required_Node, Tree.Last_Shared_Node);
      Tree.Last_Shared_Node := Required_Node - 1;
   end Move_Branch_Point;

   function Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index
   is
      use Valid_Node_Index_Arrays;
      use all type SAL.Base_Peek_Type;

      function First_Child (Node : in Valid_Node_Index) return Node_Index
      is
         N : Node_Const_Ref renames Tree.Get_Node_Const_Ref (Node);
      begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty
            for J in N.Children.First_Index .. N.Children.Last_Index loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               declare
                  Result : constant Node_Index := First_Child (N.Children (J));
               begin
                  if Result /= Invalid_Node_Index then
                     return Result;
                  end if;
               end;
            end loop;
            --  All Children are empty
            return Invalid_Node_Index;
         end case;
      end First_Child;

      function Next_Child (Child : in Valid_Node_Index; Node : in Node_Index) return Node_Index
      is begin
         --  Node is Parent of Child; return node immediately after Child.
         if Node = Invalid_Node_Index then
            return Invalid_Node_Index;
         else
            declare
               N : Node_Const_Ref renames Tree.Get_Node_Const_Ref (Node);
            begin
               pragma Assert (N.Label = Nonterm);
               for I in N.Children.First_Index .. N.Children.Last_Index loop
                  --  Encountering Deleted_Child here is an error in the user algorithm.
                  if N.Children (I) = Child then
                     --  Use first non-empty next from I + 1.
                     for J in I + 1 .. N.Children.Last_Index loop
                        declare
                           Result : constant Node_Index := First_Child (N.Children (J));
                        begin
                           if Result /= Invalid_Node_Index then
                              return Result;
                           end if;
                        end;
                     end loop;
                     --  All next Children are empty
                     return Next_Child (Node, N.Parent);
                  end if;
               end loop;
               raise SAL.Programmer_Error;
            end;
         end if;
      end Next_Child;

      N : Node_Const_Ref renames Get_Node_Const_Ref (Tree, Node);
   begin
      return Next_Child (Node, N.Parent);
   end Next_Terminal;

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Index;
      Count : in Positive := 1)
     return Node_Index
   is
      Result : Node_Index := Node;
      N      : Natural    := 0;
   begin
      loop
         if Result <= Tree.Last_Shared_Node then
            Result := Tree.Shared_Tree.Nodes (Result).Parent;
         else
            Result := Tree.Branched_Nodes (Result).Parent;
         end if;
         N := N + 1;
         exit when N = Count or Result = Invalid_Node_Index;
      end loop;
      return Result;
   end Parent;

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index
   is
      use Valid_Node_Index_Arrays;
      use all type SAL.Base_Peek_Type;

      function Last_Child (Node : in Valid_Node_Index) return Node_Index
      is
         N : Node_Const_Ref renames Tree.Get_Node_Const_Ref (Node);
      begin
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            return Node;
         when Nonterm =>
            --  Use first non-empty from end.
            for J in reverse N.Children.First_Index .. N.Children.Last_Index loop
               --  Encountering Deleted_Child here is an error in the user algorithm.
               declare
                  Result : constant Node_Index := Last_Child (N.Children (J));
               begin
                  if Result /= Invalid_Node_Index then
                     return Result;
                  end if;
               end;
            end loop;
            --  All Children are empty
            return Invalid_Node_Index;
         end case;
      end Last_Child;

      function Prev_Child (Child : in Valid_Node_Index; Node : in Node_Index) return Node_Index
      is begin
         --  Node is Parent of Child; return node immediately previous to Child.
         if Node = Invalid_Node_Index then
            return Invalid_Node_Index;
         else
            declare
               N : Node_Const_Ref renames Tree.Get_Node_Const_Ref (Node);
            begin
               pragma Assert (N.Label = Nonterm);
               for I in reverse N.Children.First_Index .. N.Children.Last_Index loop
                  --  Encountering Deleted_Child here is an error in the user algorithm.
                  if N.Children (I) = Child then
                     --  Use first non-empty from I - 1.
                     for J in reverse N.Children.First_Index .. I - 1 loop
                        declare
                           Result : constant Node_Index := Last_Child (N.Children (J));
                        begin
                           if Result /= Invalid_Node_Index then
                              return Result;
                           end if;
                        end;
                     end loop;
                     --  All previous Children are empty
                     return Prev_Child (Node, N.Parent);
                  end if;
               end loop;
               raise SAL.Programmer_Error;
            end;
         end if;
      end Prev_Child;

      N : Node_Const_Ref renames Get_Node_Const_Ref (Tree, Node);
   begin
      return Prev_Child (Node, N.Parent);
   end Prev_Terminal;

   procedure Print_Tree
     (Tree            : in Syntax_Trees.Tree;
      Descriptor      : in WisiToken.Descriptor;
      Root            : in Node_Index                   := Invalid_Node_Index;
      Image_Augmented : in Syntax_Trees.Image_Augmented := null;
      Image_Action    : in Syntax_Trees.Image_Action    := null)
   is
      use Ada.Text_IO;

      Node_Printed : Node_Sets.Vector;

      procedure Print_Node (Node : in Valid_Node_Index; Level : in Integer)
      is
         function Image is new SAL.Generic_Decimal_Image (Node_Index);

         N : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Node);
      begin
         if Node_Printed (Node) then
            --  This does not catch all possible tree edit errors, but it does
            --  catch circles.
            raise SAL.Programmer_Error with "Print_Tree: invalid tree; loop:" & Node_Index'Image (Node);
         else
            Node_Printed (Node) := True;
         end if;

         Put (Image (Node, Width => 4) & ": ");
         for I in 1 .. Level loop
            Put ("| ");
         end loop;
         Put (Image (Tree, N, Node, Descriptor, Include_Children => False, Include_RHS_Index => True));
         if Image_Augmented /= null and N.Augmented /= null then
            Put (" - " & Image_Augmented (N.Augmented));
         end if;
         if N.Label = Nonterm and then (Image_Action /= null and N.Action /= null) then
            Put (" - " & Image_Action (N.Action));
         end if;

         New_Line;
         if N.Label = Nonterm then
            for Child of N.Children loop
               if Child = Deleted_Child then
                  Put ("    : ");
                  for I in 1 .. Level + 1 loop
                     Put ("| ");
                  end loop;
                  Put_Line (" <deleted>");
               else
                  Print_Node (Child, Level + 1);
               end if;
            end loop;
         end if;
      end Print_Node;

      Print_Root : constant Node_Index := (if Root = Invalid_Node_Index then Tree.Root else Root);
   begin
      Node_Printed.Set_First_Last (Tree.First_Index, Tree.Last_Index);
      if Print_Root = Invalid_Node_Index then
         Put_Line ("<empty tree>");
      else
         Print_Node (Print_Root, 0);
      end if;
   end Print_Tree;

   function Process_Tree
     (Tree         : in Syntax_Trees.Tree;
      Node         : in Valid_Node_Index;
      Visit_Parent : in Visit_Parent_Mode;
      Process_Node : access function
        (Tree : in Syntax_Trees.Tree;
         Node : in Valid_Node_Index)
        return Boolean)
     return Boolean
   is
      function Compute (N : in Syntax_Trees.Node) return Boolean
      is begin
         if Visit_Parent = Before then
            if not Process_Node (Tree, Node) then
               return False;
            end if;
         end if;

         if N.Label = Nonterm then
            for Child of N.Children loop
               if Child /= Deleted_Child then
                  if not Process_Tree (Tree, Child, Visit_Parent, Process_Node) then
                     return False;
                  end if;
               end if;
            end loop;
         end if;

         if Visit_Parent = After then
            return Process_Node (Tree, Node);
         else
            return True;
         end if;
      end Compute;
   begin
      if Node <= Tree.Last_Shared_Node then
         return Compute (Tree.Shared_Tree.Nodes (Node));
      else
         return Compute (Tree.Branched_Nodes (Node));
      end if;
   end Process_Tree;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Node         : in     Valid_Node_Index;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index))
   is
      procedure Compute (N : in Syntax_Trees.Node)
      is begin
         if N.Label = Nonterm then
            for Child of N.Children loop
               if Child /= Deleted_Child then
                  Process_Tree (Tree, Child, Process_Node);
               end if;
            end loop;
         end if;

         Process_Node (Tree, Node);
      end Compute;
   begin
      if Node <= Tree.Last_Shared_Node then
         Compute (Tree.Shared_Tree.Nodes (Node));
      else
         Compute (Tree.Branched_Nodes (Node));
      end if;
   end Process_Tree;

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index);
      Root         : in     Node_Index := Invalid_Node_Index)
   is begin
      Tree.Shared_Tree.Traversing := True;
      Process_Tree (Tree, (if Root = Invalid_Node_Index then Tree.Root else Root), Process_Node);
      Tree.Shared_Tree.Traversing := False;
   exception
   when others =>
      Tree.Shared_Tree.Traversing := False;
      raise;
   end Process_Tree;

   function Production_ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Production_ID
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then (Tree.Shared_Tree.Nodes (Node).ID, Tree.Shared_Tree.Nodes (Node).RHS_Index)
         else (Tree.Branched_Nodes (Node).ID, Tree.Branched_Nodes (Node).RHS_Index));
   end Production_ID;

   procedure Replace_Child
     (Tree                 : in out Syntax_Trees.Tree;
      Parent               : in     Valid_Node_Index;
      Child_Index          : in     SAL.Peek_Type;
      Old_Child            : in     Valid_Node_Index;
      New_Child            : in     Valid_Node_Index;
      Old_Child_New_Parent : in     Node_Index := Invalid_Node_Index)
   is
      N : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Parent);
   begin
      N.Children (Child_Index) := New_Child;

      if Old_Child /= Deleted_Child then
         Tree.Shared_Tree.Nodes (Old_Child).Parent := Old_Child_New_Parent;
      end if;

      Tree.Shared_Tree.Nodes (New_Child).Parent := Parent;
   end Replace_Child;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Natural
   is begin
      return
        (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes (Node).RHS_Index
         else Tree.Branched_Nodes (Node).RHS_Index);
   end RHS_Index;

   function Root (Tree : in Syntax_Trees.Tree) return Node_Index
   is begin
      return Tree.Root;
   end Root;

   procedure Set_Node_Identifier
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      ID         : in Token_ID;
      Identifier : in Identifier_Index)
   is
      Current : constant Syntax_Trees.Node := Tree.Shared_Tree.Nodes (Node);
   begin
      for C of Current.Children loop
         if C /= Deleted_Child then
            Tree.Shared_Tree.Nodes (C).Parent := Invalid_Node_Index;
         end if;
      end loop;

      Tree.Shared_Tree.Nodes.Replace_Element
        (Node,
         (Label       => Virtual_Identifier,
          ID          => ID,
          Identifier  => Identifier,
          Byte_Region => Current.Byte_Region,
          Parent      => Current.Parent,
          State       => Unknown_State,
          Augmented   => null));
   end Set_Node_Identifier;

   procedure Set_Parents (Tree : in out Syntax_Trees.Tree)
   is
      procedure Set_Parents
        (Tree   : in out Syntax_Trees.Tree;
         Node   : in     Valid_Node_Index;
         Parent : in     Node_Index)
      is
         N : Node_Var_Ref renames Tree.Get_Node_Var_Ref (Node);
      begin
         N.Parent := Parent;
         case N.Label is
         when Shared_Terminal | Virtual_Terminal | Virtual_Identifier =>
            null;

         when Nonterm =>
            for C of N.Children loop
               if C = Deleted_Child then
                  --  This can only happen if someone calls Set_Parents after parents
                  --  are already set.
                  raise SAL.Programmer_Error with "encountered Deleted_Child";
               end if;
               Set_Parents (Tree, C, Node);
            end loop;
         end case;
      end Set_Parents;
   begin
      Set_Parents (Tree, Root (Tree), Invalid_Node_Index);
      Tree.Shared_Tree.Parents_Set := True;
   end Set_Parents;

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; Root : in Valid_Node_Index)
   is begin
      Tree.Root := Root;
   end Set_Root;

   function Same_Token
     (Tree_1  : in Syntax_Trees.Tree'Class;
      Index_1 : in Valid_Node_Index;
      Tree_2  : in Syntax_Trees.Tree'Class;
      Index_2 : in Valid_Node_Index)
     return Boolean
   is
      function Compute (N_1, N_2 : in Syntax_Trees.Node) return Boolean
      is begin
         return N_1.Label = N_2.Label and
           N_1.ID = N_2.ID and
           N_1.Byte_Region = N_2.Byte_Region;
      end Compute;
   begin
      return Compute
        ((if Index_1 <= Tree_1.Last_Shared_Node
          then Tree_1.Shared_Tree.Nodes (Index_1)
          else Tree_1.Branched_Nodes (Index_1)),
         (if Index_2 <= Tree_2.Last_Shared_Node
          then Tree_2.Shared_Tree.Nodes (Index_2)
          else Tree_2.Branched_Nodes (Index_2)));
   end Same_Token;

   procedure Set_Augmented
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Index;
      Value : in     Base_Token_Class_Access)
   is begin
      if Node <= Tree.Last_Shared_Node then
         Tree.Shared_Tree.Nodes (Node).Augmented := Value;
      else
         Tree.Branched_Nodes (Node).Augmented := Value;
      end if;
      Tree.Shared_Tree.Augmented_Present := True;
   end Set_Augmented;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Parent   : in     Valid_Node_Index;
      Children : in     Valid_Node_Index_Array)
   is
      N : Node_Var_Ref renames Tree.Get_Node_Var_Ref (Parent);

      Min_Terminal_Index_Set : Boolean := False;
   begin
      --  See Design note in spec about Parents, Parent_Set.

      if Tree.Parents_Set then
         --  Clear current Children.Parent first, in case some are also in new
         --  children.
         for C of N.Children loop
            if C /= WisiToken.Deleted_Child then
               Tree.Shared_Tree.Nodes (C).Parent := Invalid_Node_Index;
            end if;
         end loop;
      end if;

      N.Children.Set_First_Last (Children'First, Children'Last);

      for I in Children'Range loop

         N.Children (I) := Children (I);

         if Tree.Parents_Set then
            declare
               Child_Node : Node renames Tree.Shared_Tree.Nodes (Children (I));
            begin
               if Child_Node.Parent /= Invalid_Node_Index then
                  declare
                     Other_Parent : Node renames Tree.Shared_Tree.Nodes (Child_Node.Parent);
                     Child_Index  : constant SAL.Base_Peek_Type := Syntax_Trees.Child_Index
                       (Other_Parent, Children (I));
                  begin
                     Other_Parent.Children (Child_Index) := WisiToken.Deleted_Child;
                  end;
               end if;

               Child_Node.Parent := Parent;
            end;
         end if;

         declare
            K : Node_Const_Ref renames Tree.Get_Node_Const_Ref (Children (I));
         begin
            N.Virtual := N.Virtual or
              (case K.Label is
               when Shared_Terminal                       => False,
               when Virtual_Terminal | Virtual_Identifier => True,
               when Nonterm                               => K.Virtual);

            if N.Byte_Region.First > K.Byte_Region.First then
               N.Byte_Region.First := K.Byte_Region.First;
            end if;

            if N.Byte_Region.Last < K.Byte_Region.Last then
               N.Byte_Region.Last := K.Byte_Region.Last;
            end if;

            if not Min_Terminal_Index_Set then
               case K.Label is
               when Shared_Terminal =>
                  Min_Terminal_Index_Set := True;
                  N.Min_Terminal_Index   := K.Terminal;

               when Virtual_Terminal | Virtual_Identifier =>
                  null;

               when Nonterm =>
                  if K.Min_Terminal_Index /= Invalid_Token_Index then
                     --  not an empty nonterm
                     Min_Terminal_Index_Set := True;
                     N.Min_Terminal_Index   := K.Min_Terminal_Index;
                  end if;
               end case;
            end if;
         end;
      end loop;
   end Set_Children;

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Node     : in     Valid_Node_Index;
      New_ID   : in     WisiToken.Production_ID;
      Children : in     Valid_Node_Index_Array)
   is
      Parent_Node  : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Node);
   begin
      if New_ID /= (Parent_Node.ID, Parent_Node.RHS_Index) then
         Parent_Node.Action := null;
      end if;

      Parent_Node.ID        := New_ID.LHS;
      Parent_Node.RHS_Index := New_ID.RHS;

      Set_Children (Tree, Node, Children);
   end Set_Children;

   procedure Set_State
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Index;
      State : in     State_Index)
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes (Node).State := State;
      else
         if Node <= Tree.Last_Shared_Node then
            Tree.Shared_Tree.Nodes (Node).State := State;
         else
            Tree.Branched_Nodes (Node).State := State;
         end if;
      end if;
   end Set_State;

   procedure Set_Flush_False (Tree : in out Syntax_Trees.Tree)
   is begin
      if Tree.Flush then
         Tree.Flush := False;
         Tree.Branched_Nodes.Set_First_Last (Tree.Last_Shared_Node + 1, Tree.Last_Shared_Node);
      end if;
   end Set_Flush_False;

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   is begin
      if Tree.Flush then
         Tree.Shared_Tree.Nodes (Node).Name := Region;

      else
         if Node <= Tree.Last_Shared_Node then
            Move_Branch_Point (Tree, Node);
         end if;

         Tree.Branched_Nodes (Node).Name := Region;
      end if;
   end Set_Name_Region;

   function Sub_Tree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index
   is
      N : Valid_Node_Index := Node;
   begin
      loop
         exit when Tree.Shared_Tree.Nodes (N).Parent = Invalid_Node_Index;
         N := Tree.Shared_Tree.Nodes (N).Parent;
      end loop;
      return N;
   end Sub_Tree_Root;

   function Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).Terminal;
      else
         return Tree.Branched_Nodes (Node).Terminal;
      end if;
   end Terminal;

   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean
   is begin
      return Tree.Shared_Tree.Traversing;
   end Traversing;

   function Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Recover_Token
   is
      function Compute (N : Syntax_Trees.Node) return WisiToken.Recover_Token
      is begin
         case N.Label is
         when Shared_Terminal =>
            return
              (ID                 => N.ID,
               Byte_Region        => N.Byte_Region,
               Min_Terminal_Index => N.Terminal,
               Name               => Null_Buffer_Region,
               Virtual            => False);

         when Virtual_Terminal | Virtual_Identifier =>
            return
              (ID                 => N.ID,
               Byte_Region        => Null_Buffer_Region,
               Min_Terminal_Index => Invalid_Token_Index,
               Name               => Null_Buffer_Region,
               Virtual            => True);

         when Nonterm =>
            return
              (ID                 => N.ID,
               Byte_Region        => N.Byte_Region,
               Min_Terminal_Index => N.Min_Terminal_Index,
               Name               => N.Name,
               Virtual            => N.Virtual);
         end case;
      end Compute;
   begin
      return Compute
        ((if Node <= Tree.Last_Shared_Node
          then Tree.Shared_Tree.Nodes (Node)
          else Tree.Branched_Nodes (Node)));
   end Recover_Token;

   function Recover_Token_Array
     (Tree  : in Syntax_Trees.Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Recover_Token_Array
   is begin
      return Result : WisiToken.Recover_Token_Array (Nodes'First .. Nodes'Last) do
         for I in Result'Range loop
            Result (I) := Tree.Recover_Token (Nodes (I));
         end loop;
      end return;
   end Recover_Token_Array;

   function State (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Unknown_State_Index
   is begin
      if Node <= Tree.Last_Shared_Node then
         return Tree.Shared_Tree.Nodes (Node).State;
      else
         return Tree.Branched_Nodes (Node).State;
      end if;
   end State;

   procedure Validate_Tree
     (Tree          : in out Syntax_Trees.Tree;
      Terminals     : in     Base_Token_Array_Access_Constant;
      Descriptor    : in     WisiToken.Descriptor;
      File_Name     : in     String;
      Root          : in     Node_Index                 := Invalid_Node_Index;
      Validate_Node : in     Syntax_Trees.Validate_Node := null)
   is
      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index)
      is
         use Ada.Text_IO;
         N : Syntax_Trees.Node renames Tree.Shared_Tree.Nodes (Node);
         Node_Image_Output : Boolean := False;
      begin
         if N.Label = Nonterm then
            for I in N.Children.First_Index .. N.Children.Last_Index loop
               if N.Children (I) = Deleted_Child then
                  if not Node_Image_Output then
                     Put_Line
                       (Current_Error,
                        Tree.Error_Message
                          (Terminals, Node, File_Name,
                           Image (Tree, N, Node, Descriptor,
                                  Include_Children => False,
                                  Node_Numbers     => True)));
                     Node_Image_Output := True;
                  end if;
                  Put_Line
                    (Current_Error, Tree.Error_Message
                       (Terminals, Node, File_Name, "... child" & I'Image & " deleted"));

               else
                  declare
                     Child_Parent : constant Node_Index := Tree.Shared_Tree.Nodes (N.Children (I)).Parent;
                  begin
                     if Child_Parent /= Node then
                        if not Node_Image_Output then
                           Put_Line
                             (Current_Error,
                              Tree.Error_Message
                                (Terminals, Node, File_Name,
                                 Image (Tree, N, Node, Descriptor,
                                        Include_Children => False,
                                        Node_Numbers     => True)));
                           Node_Image_Output := True;
                        end if;
                        if Child_Parent = Invalid_Node_Index then
                           Put_Line
                             (Current_Error, Tree.Error_Message
                                (Terminals, Node, File_Name, "... child.parent invalid"));
                        else
                           Put_Line
                             (Current_Error, Tree.Error_Message
                                (Terminals, Node, File_Name, "... child.parent" & Child_Parent'Image & " incorrect"));
                        end if;
                     end if;
                  end;
               end if;
            end loop;
         end if;

         if Validate_Node /= null then
            Validate_Node (Tree, Node, Node_Image_Output);
         end if;
      end Process_Node;

   begin
      Process_Tree (Tree, (if Root = Invalid_Node_Index then Tree.Root else Root), Process_Node'Access);
   end Validate_Tree;

end WisiToken.Syntax_Trees;
