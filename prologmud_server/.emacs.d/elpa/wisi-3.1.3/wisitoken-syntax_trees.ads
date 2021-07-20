--  Abstract :
--
--  Syntax tree type and operations.
--
--  Design :
--
--  There is one syntax tree for each parallel parser. There is one
--  shared Terminals array (provided by the master parser), matching
--  the actual input text.
--
--  Node contains a Parent component, to make it easy to traverse the
--  tree in any direction. However, we do not set the Parent nodes
--  while parsing, to simplify branching the syntax tree for parallel
--  parsing. When a new nonterm is added to a branched tree, if it set
--  the parent component of its children, it would first have to move
--  those children, and all intervening nodes, into the branched tree.
--  Since Shared_Terminals nodes are created before all other nodes
--  (when the lexer is run, to allow Lexer_To_Augmented to store info
--  in the node), that would mean every branched tree is a practically
--  complete copy of the entire tree, significantly slowing down
--  parsing (by a factor of 250 on ada-mode wisi.adb when we did this
--  by mistake!).
--
--  The parent components are set by Set_Parents, which is called by
--  Parser.Execute_Actions before the actions are executed.
--  Fortunately, we don't need the parent components during error
--  recover. After calling Set_Parents (ie, while editing the syntax
--  tree after parse), any functions that modify children or parents
--  update the corresponding links, setting them to Invalid_Node_Index
--  or Deleted_Child as appropriate.
--
--  We provide Base_Tree and Tree in one package, because only Tree
--  needs an API; the only way Base_Tree is accessed is via Tree.
--
--  Base_Tree and Tree are not limited to allow
--  wisitoken-parse-lr-parser_lists.ads Prepend_Copy to copy them. No
--  Adjust is needed; Shared_Tree is shared between parsers, and
--  Augmented pointers are also shared, since during parse they are
--  set only for Shared_Terminals.
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

with Ada.Finalization;
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Lexer;
package WisiToken.Syntax_Trees is

   type Base_Tree is new Ada.Finalization.Controlled with private;

   type Base_Tree_Access is access all Base_Tree;

   overriding procedure Finalize (Tree : in out Base_Tree);
   --  Free any allocated storage.

   function Is_Empty (Tree : in Base_Tree) return Boolean;

   type Tree is new Ada.Finalization.Controlled with private;

   type Tree_Variable_Reference (Element : not null access Tree) is null record with
     Implicit_Dereference => Element;

   type Tree_Constant_Reference (Element : not null access constant Tree) is null record with
     Implicit_Dereference => Element;

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean;

   procedure Initialize
     (Branched_Tree : in out Tree;
      Shared_Tree   : in     Base_Tree_Access;
      Flush         : in     Boolean;
      Set_Parents   : in     Boolean := False)
   with Pre => Branched_Tree.Is_Empty and Shared_Tree.Is_Empty;
   --  Set Branched_Tree to refer to Shared_Tree.

   overriding procedure Finalize (Tree : in out Syntax_Trees.Tree);
   --  Free any allocated storage.

   type Node_Label is
     (Shared_Terminal,    -- text is user input, accessed via Parser.Terminals
      Virtual_Terminal,   -- no text; inserted during error recovery
      Virtual_Identifier, -- text in user data, created during tree rewrite
      Nonterm             -- contains terminals/nonterminals/identifiers
     );

   type User_Data_Type is tagged limited null record;
   --  Many test languages don't need this, so we default the procedures
   --  to null.

   type User_Data_Access is access all User_Data_Type'Class;

   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access_Constant)
   is null;

   procedure Reset (User_Data : in out User_Data_Type) is null;
   --  Reset to start a new parse.

   procedure Initialize_Actions
     (User_Data : in out User_Data_Type;
      Tree      : in     Syntax_Trees.Tree'Class)
     is null;
   --  Called by Execute_Actions, before processing the tree.

   procedure Lexer_To_Augmented
     (User_Data : in out          User_Data_Type;
      Tree      : in out          Syntax_Trees.Tree'Class;
      Token     : in              Base_Token;
      Lexer     : not null access WisiToken.Lexer.Instance'Class)
     is null;
   --  Read auxiliary data from Lexer, do something useful with it.
   --  Called before parsing, once for each token in the input stream. If
   --  Token is a grammar token, client can use Tree.Set_Augmented
   --  (Token.Tree_Node).

   function Insert_After
     (User_Data            : in out User_Data_Type;
      Tree                 : in     Syntax_Trees.Tree'Class;
      Token                : in     Valid_Node_Index;
      Insert_On_Blank_Line : in     Boolean)
     return Boolean;
   --  Return True if ID should be treated as if inserted after the
   --  previous shared terminal, rather than before the next (which is
   --  the default). This can affect which line it appears on, which
   --  affects indentation. Called from Insert_Token.
   --
   --  The default implementation always returns False.

   procedure Insert_Token
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Token     : in     Valid_Node_Index)
   is null;
   --  Token was inserted in error recovery; update other tokens and Tree
   --  as needed. Called from Execute_Actions for each inserted token,
   --  before processing the syntax tree.

   procedure Delete_Token
     (User_Data   : in out User_Data_Type;
      Tree        : in out Syntax_Trees.Tree'Class;
      Token_Index : in     WisiToken.Token_Index)
   is null;
   --  Token at Token_Index was deleted in error recovery; update
   --  remaining tokens as needed. Called from Execute_Actions for each
   --  deleted token, before processing the syntax tree.

   procedure Reduce
     (User_Data : in out User_Data_Type;
      Tree      : in out Syntax_Trees.Tree'Class;
      Nonterm   : in     Valid_Node_Index;
      Tokens    : in     Valid_Node_Index_Array)
   is null;
   --  Reduce Tokens to Nonterm. Nonterm.Byte_Region is computed by
   --  caller.

   type Semantic_Action is access procedure
     (User_Data : in out User_Data_Type'Class;
      Tree      : in out Syntax_Trees.Tree;
      Nonterm   : in     Valid_Node_Index;
      Tokens    : in     Valid_Node_Index_Array);
   --  Routines of this type are called by
   --  WisiToken.LR.Parser.Execute_Actions when it processes a Nonterm
   --  node in the syntax tree. Tokens are the children of Nonterm.

   Null_Action : constant Semantic_Action := null;

   procedure Clear (Tree : in out Syntax_Trees.Base_Tree);
   procedure Clear (Tree : in out Syntax_Trees.Tree);
   --  Delete all Elements and free associated memory; keep results of
   --  Initialize.

   procedure Flush (Tree : in out Syntax_Trees.Tree);
   --  Move all nodes in branched part to shared tree, set Flush mode
   --  True.

   procedure Set_Flush_False (Tree : in out Syntax_Trees.Tree);
   --  Set Flush mode False; use Flush to set True.

   function Flushed (Tree : in Syntax_Trees.Tree) return Boolean;

   function Copy_Subtree
     (Tree : in out Syntax_Trees.Tree;
      Root : in     Valid_Node_Index)
     return Valid_Node_Index
   with Pre => Tree.Flushed and Tree.Parents_Set;
   --  Deep copy (into Tree) subtree of Tree rooted at Root. Return root
   --  of new subtree; it has no parent.
   --
   --  Parents of new child nodes are set. Node index order is preserved.
   --  References to objects external to tree are shallow copied
   --  (Terminals, Augmented, Action).

   function Add_Nonterm
     (Tree            : in out Syntax_Trees.Tree;
      Production      : in     Production_ID;
      Children        : in     Valid_Node_Index_Array;
      Action          : in     Semantic_Action := null;
      Default_Virtual : in     Boolean         := False)
     return Valid_Node_Index
   with Pre => not Tree.Traversing and
               (for all C of Children => C /= Deleted_Child);
   --  Add a new Nonterm node, which can be empty. Result points to the
   --  added node. If Children'Length = 0, set Nonterm.Virtual :=
   --  Default_Virtual.
   --
   --  If Tree.Parents_Set, then Children.Parent are set to the new node,
   --  and in previous parents of those children (if any), the
   --  corresponding entry in Children is set to Deleted_Child.

   function Add_Terminal
     (Tree      : in out Syntax_Trees.Tree;
      Terminal  : in     Token_Index;
      Terminals : in     Base_Token_Arrays.Vector)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Terminal node. Terminal must be an index into Terminals.
   --  Result points to the added node.

   function Add_Terminal
     (Tree     : in out Syntax_Trees.Tree;
      Terminal : in     Token_ID;
      Before   : in     Base_Token_Index := Invalid_Token_Index)
     return Valid_Node_Index
   with Pre => not Tree.Traversing;
   --  Add a new Virtual_Terminal node with no parent. Before is the
   --  index of the terminal in Terminals that this virtual is inserted
   --  before during error correction; if Invalid_Token_Index, it is
   --  inserted during EBNF translation, and there is no such terminal.
   --  Result points to the added node.

   function Before
     (Tree             : in Syntax_Trees.Tree;
      Virtual_Terminal : in Valid_Node_Index)
     return Base_Token_Index
   with Pre => Tree.Is_Virtual_Terminal (Virtual_Terminal);

   function Add_Identifier
     (Tree        : in out Syntax_Trees.Tree;
      ID          : in     Token_ID;
      Identifier  : in     Identifier_Index;
      Byte_Region : in     WisiToken.Buffer_Region)
     return Valid_Node_Index
   with Pre => Tree.Flushed and (not Tree.Traversing);
   --  Add a new Virtual_Identifier node with no parent. Byte_Region
   --  should point to an area in the source buffer related to the new
   --  identifier, to aid debugging. Result points to the added node.

   procedure Add_Child
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Index;
      Child  : in     Valid_Node_Index)
   with
     Pre => Tree.Flushed and Tree.Parents_Set and (not Tree.Traversing) and
            Tree.Is_Nonterm (Parent);
   --  Sets Child.Parent.

   function Child_Index
     (Tree   : in out Syntax_Trees.Tree;
      Parent : in     Valid_Node_Index;
      Child  : in     Valid_Node_Index)
     return SAL.Peek_Type
   with Pre => Tree.Has_Child (Parent, Child);

   procedure Replace_Child
     (Tree                 : in out Syntax_Trees.Tree;
      Parent               : in     Valid_Node_Index;
      Child_Index          : in     SAL.Peek_Type;
      Old_Child            : in     Valid_Node_Index;
      New_Child            : in     Valid_Node_Index;
      Old_Child_New_Parent : in     Node_Index := Invalid_Node_Index)
   with
     Pre => Tree.Flushed and Tree.Parents_Set and (not Tree.Traversing) and
            (Tree.Is_Nonterm (Parent) and then
             (Tree.Child (Parent, Child_Index) = Old_Child and
              (Old_Child = Deleted_Child or else
               Tree.Parent (Old_Child) = Parent)));
   --  In Parent.Children, replace child at Child_Index with New_Child.
   --  Unless Old_Child is Deleted_Child, set Old_Child.Parent to
   --  Old_Child_New_Parent (may be Invalid_Node_Index). Unless New_Child
   --  is Deleted_Child, set New_Child.Parent to Parent.
   --
   --  If Old_Child is Deleted_Child, Old_Child_New_Parent should be left
   --  to default.

   procedure Set_Children
     (Tree     : in out Syntax_Trees.Tree;
      Node     : in     Valid_Node_Index;
      New_ID   : in     WisiToken.Production_ID;
      Children : in     Valid_Node_Index_Array)
   with
     Pre => Tree.Flushed and Tree.Parents_Set and (not Tree.Traversing) and
            Tree.Is_Nonterm (Node) and
            (for all C of Children => C /= Deleted_Child);
   --  If parents of current Node.Children are not Invalid_Node_Index,
   --  set corresponding entry in those parents to Deleted_Child, then
   --  set Parent to Invalid_Node_Index.
   --
   --  Then set ID of Node to New_ID, and Node.Children to Children; set
   --  parents of Children to Node.
   --
   --  If New_ID /= Tree.Production_ID (Node), Node.Action is set
   --  to null, because the old Action probably no longer applies.

   procedure Delete_Parent
     (Tree : in out Syntax_Trees.Tree;
      Node : in     Valid_Node_Index)
   with
     Pre => Tree.Flushed and Tree.Parents_Set and (not Tree.Traversing) and
            Tree.Parent (Node) /= Invalid_Node_Index;
   --  Set child in Node.Parent to Deleted_Child. If Node.Parent =
   --  Tree.Root, set Tree.Root to Node. Set Node.Parent to
   --  Invalid_Node_Index.

   procedure Set_Node_Identifier
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      ID         : in Token_ID;
      Identifier : in Identifier_Index)
   with Pre => Tree.Flushed and Tree.Parents_Set and (not Tree.Traversing) and
               Tree.Is_Nonterm (Node);
   --  Set parents of current Node.Children to Invalid_Node_Index.
   --  Then change Node to a Virtual_Identifier.

   procedure Set_State
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Index;
      State : in     State_Index);

   function State (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Unknown_State_Index;

   function Label (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Label;

   function Child_Count (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Ada.Containers.Count_Type
   with Pre => Tree.Is_Nonterm (Node);

   function Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array
   with Pre => Tree.Is_Nonterm (Node);
   --  Any children that were cleared by Add_Nonterm are returned as
   --  Deleted_Child.

   function Child
     (Tree        : in Syntax_Trees.Tree;
      Node        : in Valid_Node_Index;
      Child_Index : in Positive_Index_Type)
     return Node_Index
   with Pre => Tree.Is_Nonterm (Node);

   function Has_Branched_Nodes (Tree : in Syntax_Trees.Tree) return Boolean;
   function Has_Children (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean
   with Pre => Tree.Is_Nonterm (Node);
   function Has_Child
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Index;
      Child : in Valid_Node_Index)
     return Boolean
   with Pre => Tree.Is_Nonterm (Node);
   function Has_Parent (Tree : in Syntax_Trees.Tree; Child : in Valid_Node_Index) return Boolean;
   function Has_Parent (Tree : in Syntax_Trees.Tree; Children : in Valid_Node_Index_Array) return Boolean;

   function Buffer_Region_Is_Empty (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   --  True if contained buffer region is empty; always the case for
   --  virtual tokens, and for most copied tokens. Use Has_Children or
   --  Child_Count to see if Node has children.

   function Is_Nonterm (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Is_Virtual_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;

   function Is_Virtual (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   --  Virtual_Terminal, Virtual_Identifier, or Nonterm that contains some Virtual tokens.

   function Is_Virtual_Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean;
   function Traversing (Tree : in Syntax_Trees.Tree) return Boolean;

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean;
   procedure Set_Parents (Tree : in out Syntax_Trees.Tree)
   with Pre => Tree.Flushed and Tree.Root /= Invalid_Node_Index;

   function Parent
     (Tree  : in Syntax_Trees.Tree;
      Node  : in Valid_Node_Index;
      Count : in Positive := 1)
     return Node_Index
   with Pre => Tree.Parents_Set;
   --  Return Count parent of Node.

   procedure Set_Name_Region
     (Tree   : in out Syntax_Trees.Tree;
      Node   : in     Valid_Node_Index;
      Region : in     Buffer_Region)
   with Pre => Tree.Is_Nonterm (Node);

   function ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Token_ID;

   function Production_ID
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Production_ID
   with Pre => Tree.Is_Nonterm (Node);

   function Byte_Region
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Buffer_Region;

   function RHS_Index
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Natural
   with Pre => Tree.Is_Nonterm (Node);

   function Same_Token
     (Tree_1  : in Syntax_Trees.Tree'Class;
      Index_1 : in Valid_Node_Index;
      Tree_2  : in Syntax_Trees.Tree'Class;
      Index_2 : in Valid_Node_Index)
     return Boolean;
   --  True if the two tokens have the same ID and Byte_Region.

   function Recover_Token
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return WisiToken.Recover_Token;

   function Recover_Token_Array
     (Tree  : in Syntax_Trees.Tree;
      Nodes : in Valid_Node_Index_Array)
     return WisiToken.Recover_Token_Array;

   procedure Set_Augmented
     (Tree  : in out Syntax_Trees.Tree;
      Node  : in     Valid_Node_Index;
      Value : in     Base_Token_Class_Access);
   --  Value will be deallocated when Tree is finalized.

   function Augmented
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Base_Token_Class_Access;
   --  Returns result of Set_Augmented.

   function Augmented_Const
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Base_Token_Class_Access_Constant;

   function Action
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Semantic_Action
   with Pre => Tree.Is_Nonterm (Node);

   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      ID         : in Token_ID;
      Max_Parent : in Boolean := False)
     return Node_Index
   with Pre => Tree.Parents_Set;
   function Find_Ancestor
     (Tree       : in Syntax_Trees.Tree;
      Node       : in Valid_Node_Index;
      IDs        : in Token_ID_Array;
      Max_Parent : in Boolean := False)
     return Node_Index
   with Pre => Tree.Parents_Set;
   --  Return the ancestor of Node that contains ID (starting search with
   --  Node.Parent), or Invalid_Node_Index if none match.
   --
   --  If Max_Parent, return max parent found if none match; this will be
   --  Invalid_Node_Index if Node has no parent.

   function Find_Sibling
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Parents_Set and then Tree.Has_Parent (Node);
   --  Return the sibling of Node that contains ID, or Invalid_Node_Index if
   --  none match.

   function Find_Child
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index
   with Pre => Tree.Is_Nonterm (Node);
   --  Return the child of Node whose ID is ID, or Invalid_Node_Index if
   --  none match.

   function Find_Descendant
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Node_Index;
   --  Return the descendant of Node (may be Node) whose ID is ID, or
   --  Invalid_Node_Index if none match.

   function Find_Descendant
     (Tree      : in     Syntax_Trees.Tree;
      Node      : in     Valid_Node_Index;
      Predicate : access function (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Boolean)
     return Node_Index;
   --  Return the descendant of Node (may be Node) for which Predicate
   --  returns True, or Invalid_Node_Index if none do.

   function Is_Descendant_Of
     (Tree       : in Syntax_Trees.Tree;
      Root       : in Valid_Node_Index;
      Descendant : in Valid_Node_Index)
     return Boolean
   with Pre => Tree.Parents_Set and Tree.Is_Nonterm (Root);

   procedure Set_Root (Tree : in out Syntax_Trees.Tree; Root : in Valid_Node_Index);

   function Root (Tree : in Syntax_Trees.Tree) return Node_Index;
   --  Return value set by Set_Root.
   --  returns Invalid_Node_Index if Tree is empty.

   function Sub_Tree_Root (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index
   with Pre => Tree.Parents_Set;
   --  Return top ancestor of Node.

   procedure Process_Tree
     (Tree         : in out Syntax_Trees.Tree;
      Process_Node : access procedure
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index);
      Root         : in     Node_Index := Invalid_Node_Index)
   with Pre => Root /= Invalid_Node_Index or Tree.Root /= Invalid_Node_Index;
   --  Traverse subtree of Tree rooted at Root (default Tree.Root) in
   --  depth-first order, calling Process_Node on each node.

   function Identifier (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Identifier_Index
   with Pre => Tree.Is_Virtual_Identifier (Node);

   function Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index
   with Pre => Tree.Is_Shared_Terminal (Node);

   function First_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index;
   --  Returns first shared terminal in subtree under Node
   --  (ignoring virtual terminals). If result is Invalid_Token_Index,
   --  all terminals are virtual, or a nonterm is empty.

   function Last_Shared_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Base_Token_Index;
   --  Returns last shared terminal in subtree under Node (ignoring
   --  virtual terminals). If result is Invalid_Token_Index, all
   --  terminals are virtual, or a nonterm is empty.

   function Get_Terminals (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Valid_Node_Index_Array;
   --  Return sequence of terminals in Node.
   --
   --  "Terminals" can be Shared_Terminal, Virtual_Terminal,
   --  Virtual_Identifier.

   function First_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index;
   --  First of Get_Terminals. Invalid_Node_Index if Node is an empty nonterminal.

   function Last_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index;
   --  Last of Get_Terminals. Invalid_Node_Index if Node is an empty nonterminal.

   function Prev_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index
   with Pre => Tree.Parents_Set and Tree.Label (Node) in Shared_Terminal | Virtual_Terminal | Virtual_Identifier;
   --  Return the terminal that is immediately before Node in Tree;
   --  Invalid_Node_Index if Node is the first terminal in Tree.

   function Next_Terminal (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Node_Index
   with Pre => Tree.Parents_Set and Tree.Label (Node) in Shared_Terminal | Virtual_Terminal | Virtual_Identifier;
   --  Return the terminal that is immediately after Node in Tree;
   --  Invalid_Node_Index if Node is the last terminal in Tree.

   function Get_Terminal_IDs (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Token_ID_Array;
   --  Same as Get_Terminals, but return the IDs.

   function First_Terminal_ID (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Index) return Token_ID;
   --  First of Get_Terminal_IDs; Invalid_Token_ID if Node is empty.

   function Get_IDs
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index;
      ID   : in Token_ID)
     return Valid_Node_Index_Array;
   --  Return all descendants of Node matching ID.

   function Image
     (Tree              : in Syntax_Trees.Tree;
      Node              : in Valid_Node_Index;
      Descriptor        : in WisiToken.Descriptor;
      Include_Children  : in Boolean := False;
      Include_RHS_Index : in Boolean := False;
      Node_Numbers      : in Boolean := False)
     return String;
   function Image
     (Tree       : in Syntax_Trees.Tree;
      Nodes      : in Valid_Node_Index_Array;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  For debug and error messages.

   function First_Index (Tree : in Syntax_Trees.Tree) return Node_Index;
   function Last_Index (Tree : in Syntax_Trees.Tree) return Node_Index;

   package Node_Sets is new SAL.Gen_Unbounded_Definite_Vectors (Valid_Node_Index, Boolean, Default_Element => False);

   function Image
     (Item     : in Node_Sets.Vector;
      Inverted : in Boolean := False)
     return String;
   --  Simple list of numbers, for debugging

   function Error_Message
     (Tree      : in Syntax_Trees.Tree;
      Terminals : in Base_Token_Array_Access_Constant;
      Node      : in Valid_Node_Index;
      File_Name : in String;
      Message   : in String)
     return String;
   --  Get Line, column from Node.

   type Validate_Node is access procedure
     (Tree              : in     Syntax_Trees.Tree;
      Node              : in     Valid_Node_Index;
      Node_Image_Output : in out Boolean);
   --  Called by Validate_Tree for each node visited; perform other
   --  checks, output to Text_IO.Current_Error. If Node_Image_Output is
   --  False, output Image (Tree, Node, Descriptor, Node_Numbers => True) once
   --  before any error messages.

   procedure Validate_Tree
     (Tree          : in out Syntax_Trees.Tree;
      Terminals     : in     Base_Token_Array_Access_Constant;
      Descriptor    : in     WisiToken.Descriptor;
      File_Name     : in     String;
      Root          : in     Node_Index                 := Invalid_Node_Index;
      Validate_Node : in     Syntax_Trees.Validate_Node := null)
   with Pre => Tree.Flushed and Tree.Parents_Set;
   --  Verify child/parent links, and that no children are Deleted_Child.
   --  Violations output a message to Text_IO.Current_Error.

   type Image_Augmented is access function (Aug : in Base_Token_Class_Access) return String;
   type Image_Action is access function (Action : in Semantic_Action) return String;

   procedure Print_Tree
     (Tree            : in Syntax_Trees.Tree;
      Descriptor      : in WisiToken.Descriptor;
      Root            : in Node_Index                   := Invalid_Node_Index;
      Image_Augmented : in Syntax_Trees.Image_Augmented := null;
      Image_Action    : in Syntax_Trees.Image_Action    := null)
   with Pre => Tree.Flushed;
   --  Print tree rooted at Root (default Tree.Root) to
   --  Text_IO.Current_Output, for debugging. For each node,
   --  Image_Augmented is called if it is not null and node.augmented is
   --  not null.

private
   use all type Ada.Containers.Count_Type;

   type Node (Label : Node_Label := Virtual_Terminal) is
   --  Label has a default to allow changing the label during tree editing.
   record
      ID : WisiToken.Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Computed by Set_Children, used in Semantic_Check actions and debug
      --  messages.

      Parent : Node_Index := Invalid_Node_Index;

      State : Unknown_State_Index := Unknown_State;
      --  Parse state that was on stack with this token, to allow undoing a
      --  reduce.

      Augmented : Base_Token_Class_Access := null;

      case Label is
      when Shared_Terminal =>
         Terminal : Token_Index; -- into Parser.Terminals

      when Virtual_Terminal =>
         Before : Base_Token_Index := Invalid_Token_Index; -- into Parser.Terminals

      when Virtual_Identifier =>
         Identifier : Identifier_Index; -- into user data

      when Nonterm =>
         Virtual : Boolean := False;
         --  True if any child node is Virtual_Terminal or Nonterm with Virtual
         --  set. Used by Semantic_Check actions.

         RHS_Index : Natural;
         --  With ID, index into Productions.
         --  Used for debug output, keep for future use.

         Action : Semantic_Action := null;

         Name : Buffer_Region := Null_Buffer_Region;
         --  Name is set and checked by Semantic_Check actions.

         Children : Valid_Node_Index_Arrays.Vector;

         Min_Terminal_Index : Base_Token_Index := Invalid_Token_Index;
         --  Cached for push_back of nonterminals during recovery
      end case;
   end record;

   subtype Nonterm_Node is Node (Nonterm);

   package Node_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Valid_Node_Index, Node, Default_Element => (others => <>));

   type Base_Tree is new Ada.Finalization.Controlled with record
      Nodes : Node_Arrays.Vector;
      --  During normal parsing, tokens are added to Nodes by "parallel"
      --  LALR parsers, but they are all run from one Ada task, so there's
      --  no need for Nodes to be Protected. Packrat parsing also has a
      --  single Ada task.
      --
      --  During McKenzie_Recover, which has multiple Ada tasks, the syntax
      --  tree is read but not modified.

      Augmented_Present : Boolean := False;
      --  True if Set_Augmented has been called on any node. Declared in
      --  Base_Tree so it can be checked by Finalize (Base_Tree) and
      --  Finalize (Tree).

      Traversing : Boolean := False;
      --  True while traversing tree in Process_Tree.
      --  Declared in Base_Tree so it is cleared by Finalize.

      Parents_Set : Boolean := False;
      --  We don't set Node.Parent until after parse is done; see Design
      --  note above.
   end record;

   function Is_Empty (Tree : in Base_Tree) return Boolean
   is (Tree.Nodes.Length = 0);

   type Tree is new Ada.Finalization.Controlled with record
      Shared_Tree : Base_Tree_Access;
      --  If we need to set anything (ie parent) in Shared_Tree, we move the
      --  branch point instead, unless Flush = True.

      Last_Shared_Node : Node_Index := Invalid_Node_Index;
      Branched_Nodes   : Node_Arrays.Vector;
      Flush            : Boolean    := False;
      --  If Flush is True, all nodes are in Shared_Tree. Otherwise, all
      --  greater than Last_Shared_Node are in Branched_Nodes.
      --
      --  We maintain Last_Shared_Node when Flush is True or False, so
      --  subprograms that have no reason to check Flush can rely on
      --  Last_Shared_Node.

      Root : Node_Index := Invalid_Node_Index;
   end record with
     Type_Invariant =>
       (Shared_Tree = null or else
        (if Tree.Flush
         then Last_Shared_Node = Shared_Tree.Nodes.Last_Index and
            Branched_Nodes.Length = 0
         else Last_Shared_Node <= Shared_Tree.Nodes.Last_Index and
            Last_Shared_Node < Branched_Nodes.First_Index));

   subtype Node_Const_Ref is Node_Arrays.Constant_Reference_Type;
   subtype Node_Var_Ref is Node_Arrays.Variable_Reference_Type;

   function Get_Node_Const_Ref
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Node_Const_Ref
   is (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes.Constant_Ref (Node)
         else Tree.Branched_Nodes.Constant_Ref (Node));

   function Get_Node_Var_Ref
     (Tree : in Syntax_Trees.Tree;
      Node : in Valid_Node_Index)
     return Node_Var_Ref
   is (if Node <= Tree.Last_Shared_Node
         then Tree.Shared_Tree.Nodes.Variable_Ref (Node)
         else Tree.Branched_Nodes.Variable_Ref (Node));

   function Is_Empty (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Branched_Nodes.Length = 0 and (Tree.Shared_Tree = null or else Tree.Shared_Tree.Is_Empty));

   function Parents_Set (Tree : in Syntax_Trees.Tree) return Boolean
   is (Tree.Shared_Tree.Parents_Set);

end WisiToken.Syntax_Trees;
