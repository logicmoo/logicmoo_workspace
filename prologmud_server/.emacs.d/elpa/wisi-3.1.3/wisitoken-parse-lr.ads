--  Abstract :
--
--  Root package of an implementation of an LR (Left-to-right scanning
--  Rightmost-deriving) parser. Includes operations for building the
--  parse table at runtime. See the child packages .Parse and
--  .Parse_No_Recover for running the parser.
--
--  References :
--
--  See wisitoken.ads
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013 - 2015, 2017 - 2020 Free Software Foundation, Inc.
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with SAL.Gen_Array_Image;
with SAL.Gen_Bounded_Definite_Stacks.Gen_Image_Aux;
with SAL.Gen_Bounded_Definite_Vectors.Gen_Image_Aux;
with SAL.Gen_Bounded_Definite_Vectors.Gen_Refs;
with SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
with SAL.Gen_Unbounded_Definite_Vectors_Sorted;
with System.Multiprocessors;
with WisiToken.Semantic_Checks;
with WisiToken.Syntax_Trees;
package WisiToken.Parse.LR is
   use all type SAL.Base_Peek_Type;

   type All_Parse_Action_Verbs is (Pause, Shift, Reduce, Accept_It, Error);
   subtype Parse_Action_Verbs is All_Parse_Action_Verbs range Shift .. Error;
   subtype Minimal_Verbs is All_Parse_Action_Verbs range Shift .. Reduce;
   --  Pause is only used for error recovery, to allow parallel parsers
   --  to re-sync on the same input terminal.

   subtype Token_ID_Array_1_3 is Token_ID_Array (1 .. 3);
   --  For Language_Matching_Begin_Tokens.

   type Parse_Action_Rec (Verb : Parse_Action_Verbs := Shift) is
   record
      Production : Production_ID;
      --  The production that produced this action. Used to find kernel
      --  items during error recovery.

      case Verb is
      when Shift =>
         State : State_Index := State_Index'Last;

      when Reduce | Accept_It =>
         --  Production.LHS is the result nonterm
         Action      : WisiToken.Syntax_Trees.Semantic_Action   := null;
         Check       : WisiToken.Semantic_Checks.Semantic_Check := null;
         Token_Count : Ada.Containers.Count_Type                := 0;

      when Error =>
         null;
      end case;
   end record;
   subtype Shift_Action_Rec is Parse_Action_Rec (Shift);
   subtype Reduce_Action_Rec is Parse_Action_Rec (Reduce);

   function Image (Item : in Parse_Action_Rec; Descriptor : in WisiToken.Descriptor) return String;
   --  Ada aggregate syntax, leaving out Action, Check in reduce; for debug output

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Parse_Action_Rec);
   --  Put a line for Item in parse trace format, with no prefix.

   function Equal (Left, Right : in Parse_Action_Rec) return Boolean;
   --  Ignore items not used by the canonical shift-reduce algorithm.

   type Parse_Action_Node;
   type Parse_Action_Node_Ptr is access Parse_Action_Node;

   type Parse_Action_Node is record
      Item : Parse_Action_Rec;
      Next : Parse_Action_Node_Ptr; -- non-null only for conflicts
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Parse_Action_Node, Parse_Action_Node_Ptr);

   function Is_In (Item : in Parse_Action_Rec; List : in Parse_Action_Node_Ptr) return Boolean;
   --  True if Item is Equal to any element of List.

   type Action_Node is record
      Symbol  : Token_ID := Invalid_Token_ID; -- ignored if Action is Error
      Actions : Parse_Action_Node_Ptr;
   end record;

   function To_Key (Item : in Action_Node) return Token_ID is (Item.Symbol);

   function Compare (Left, Right : in Token_ID) return SAL.Compare_Result;

   package Action_Arrays is new SAL.Gen_Unbounded_Definite_Vectors_Sorted
     (Action_Node, Token_ID, To_Key, Compare);

   procedure Add
     (List   : in out Action_Arrays.Vector;
      Symbol : in     Token_ID;
      Action : in     Parse_Action_Rec);
   --  Add action to List, sorted on ascending Symbol.

   type Goto_Node is record
      Symbol : Token_ID;
      State  : State_Index;
   end record;

   function To_Key (Item : in Goto_Node) return Token_ID is (Item.Symbol);

   package Goto_Arrays is  new SAL.Gen_Unbounded_Definite_Vectors_Sorted
     (Goto_Node, Token_ID, To_Key, Compare);

   type Kernel_Info is record
      Production       : Production_ID;
      Before_Dot       : Token_ID                  := Token_ID'First;
      Length_After_Dot : Ada.Containers.Count_Type := 0;

      Reduce_Production : Production_ID;
      Reduce_Count      : Ada.Containers.Count_Type := 0;
      --  The reduction that error recovery should do for this item if
      --  Length_After_Dot = 0. Reduce_Production /= Production when item
      --  after dot is nullable.
      --
      --  It is tempting to make Length_After_Dot a discriminant to
      --  eliminate Reduce_* when they are not needed, but we don't have a
      --  static value of Length_After_Dot when it is non-zero.
   end record;

   function Strict_Image (Item : in Kernel_Info) return String;

   type Kernel_Info_Array is array (Ada.Containers.Count_Type range <>) of Kernel_Info;
   package Kernel_Info_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Ada.Containers.Count_Type, Kernel_Info, (others => <>));

   function To_Vector (Item : in Kernel_Info_Array) return Kernel_Info_Arrays.Vector;

   function Image is new Kernel_Info_Arrays.Gen_Image (Strict_Image);

   type Minimal_Action (Verb : Minimal_Verbs := Shift) is
   record
      Production  : Production_ID := Invalid_Production_ID;

      case Verb is
      when Shift =>
         ID    : Token_ID    := Invalid_Token_ID;
         State : State_Index := State_Index'Last;

      when Reduce =>
         Token_Count : Ada.Containers.Count_Type;
      end case;
   end record;

   function Strict_Image (Item : in Minimal_Action) return String;
   --  Strict Ada aggregate syntax, for generated code.

   function Image (Item : in Minimal_Action; Descriptor : in WisiToken.Descriptor) return String;
   --  For debugging

   type Minimal_Action_Array is array (Ada.Containers.Count_Type range <>) of Minimal_Action;
   package Minimal_Action_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Ada.Containers.Count_Type, Minimal_Action, (others => <>));

   function To_Vector (Item : in Minimal_Action_Array) return Minimal_Action_Arrays.Vector;

   function Image is new Minimal_Action_Arrays.Gen_Image_Aux (Descriptor, Trimmed_Image, Image);
   function Strict_Image is new Minimal_Action_Arrays.Gen_Image (Strict_Image);

   type Parse_State is record
      Action_List : Action_Arrays.Vector;
      Goto_List   : Goto_Arrays.Vector;

      --  The following are used in error recovery.
      Kernel                   : Kernel_Info_Arrays.Vector;
      Minimal_Complete_Actions : Minimal_Action_Arrays.Vector;
      --  Parse actions that will most quickly complete a production in this
      --  state. Kernel is used to reduce the number of actions.
   end record;

   type Parse_State_Array is array (State_Index range <>) of Parse_State;

   procedure Add_Action
     (State       : in out Parse_State;
      Symbol      : in     Token_ID;
      Production  : in     Production_ID;
      State_Index : in     WisiToken.State_Index);
   --  Add a Shift action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbol          : in     Token_ID;
      Verb            : in     Parse_Action_Verbs;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add a Reduce or Accept_It action to tail of State action list.

   procedure Add_Action
     (State           : in out Parse_State;
      Symbols         : in     Token_ID_Array;
      Production      : in     Production_ID;
      RHS_Token_Count : in     Ada.Containers.Count_Type;
      Semantic_Action : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check  : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add duplicate Reduce actions, and final Error action, to tail of
   --  State action list.

   procedure Add_Conflict
     (State             : in out Parse_State;
      Symbol            : in     Token_ID;
      Reduce_Production : in     Production_ID;
      RHS_Token_Count   : in     Ada.Containers.Count_Type;
      Semantic_Action   : in     WisiToken.Syntax_Trees.Semantic_Action;
      Semantic_Check    : in     WisiToken.Semantic_Checks.Semantic_Check);
   --  Add a Reduce conflict to State.

   procedure Add_Goto
     (State    : in out Parse_State;
      Symbol   : in     Token_ID;
      To_State : in     State_Index);
   --  Add a goto item to State goto list; keep goto list sorted in ascending order on Symbol.

   type McKenzie_Param_Type
     (First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
   is record
      Insert      : Token_ID_Array_Natural (First_Terminal .. Last_Terminal);
      Delete      : Token_ID_Array_Natural (First_Terminal .. Last_Terminal);
      Push_Back   : Token_ID_Array_Natural (First_Terminal .. Last_Nonterminal);
      Undo_Reduce : Token_ID_Array_Natural (First_Nonterminal .. Last_Nonterminal);
      --  Cost of operations on config stack, input.

      Minimal_Complete_Cost_Delta : Integer;
      --  Reduction in cost due to using Minimal_Complete_Action.

      Matching_Begin : Integer;
      --  Cost of Matching_Begin strategy (applied once, independent of
      --  token count).

      Fast_Forward : Integer;
      --  Cost of moving the edit point forward over input tokens.

      Ignore_Check_Fail : Natural;
      --  Cost of ignoring a semantic check failure. Should be at least the
      --  cost of a typical fix for such a failure.

      Task_Count : System.Multiprocessors.CPU_Range;
      --  Number of parallel tasks during recovery. If 0, use
      --  System.Multiprocessors.Number_Of_CPUs - 1.

      Check_Limit       : Token_Index; -- max tokens to parse ahead when checking a configuration.
      Check_Delta_Limit : Natural;     -- max configs checked, delta over successful parser.
      Enqueue_Limit     : Natural;     -- max configs enqueued.
   end record;

   Default_McKenzie_Param : constant McKenzie_Param_Type :=
     (First_Terminal              => Token_ID'Last,
      Last_Terminal               => Token_ID'First,
      First_Nonterminal           => Token_ID'Last,
      Last_Nonterminal            => Token_ID'First,
      Insert                      => (others => 0),
      Delete                      => (others => 0),
      Push_Back                   => (others => 0),
      Undo_Reduce                 => (others => 0),
      Minimal_Complete_Cost_Delta => -1,
      Fast_Forward                => 0,
      Matching_Begin              => 0,
      Ignore_Check_Fail           => 0,
      Task_Count                  => System.Multiprocessors.CPU_Range'Last,
      Check_Limit                 => 4,
      Check_Delta_Limit           => Natural'Last,
      Enqueue_Limit               => Natural'Last);

   type Parse_Table
     (State_First       : State_Index;
      State_Last        : State_Index;
      First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID)
     is tagged
   record
      States         : Parse_State_Array (State_First .. State_Last);
      Error_Action   : Parse_Action_Node_Ptr;
      McKenzie_Param : McKenzie_Param_Type (First_Terminal, Last_Terminal, First_Nonterminal, Last_Nonterminal);
   end record;

   function Goto_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Unknown_State_Index;
   --  Return next state after reducing stack by nonterminal ID;
   --  Unknown_State if none (only possible during error recovery).
   --  Second form allows retrieving Production.

   function Action_For
     (Table : in Parse_Table;
      State : in State_Index;
      ID    : in Token_ID)
     return Parse_Action_Node_Ptr
   with Post => Action_For'Result /= null;
   --  Return the action for State, terminal ID.

   function Expecting (Table : in Parse_Table; State : in State_Index) return Token_ID_Set;

   function McKenzie_Defaulted (Table : in Parse_Table) return Boolean is
     --  We can't use Table.McKenzie_Param = Default_McKenzie_Param here,
     --  because the discriminants are different.
     (Table.McKenzie_Param.Check_Limit = Default_McKenzie_Param.Check_Limit and
        Table.McKenzie_Param.Check_Delta_Limit = Default_McKenzie_Param.Check_Delta_Limit and
        Table.McKenzie_Param.Enqueue_Limit = Default_McKenzie_Param.Enqueue_Limit);

   type Parse_Table_Ptr is access Parse_Table;
   procedure Free_Table (Table : in out Parse_Table_Ptr);

   type Semantic_Action is record
      Action : WisiToken.Syntax_Trees.Semantic_Action := null;
      Check  : WisiToken.Semantic_Checks.Semantic_Check := null;
   end record;

   package Semantic_Action_Arrays is new SAL.Gen_Unbounded_Definite_vectors (Natural, Semantic_Action, (others => <>));
   package Semantic_Action_Array_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Token_ID, Semantic_Action_Arrays.Vector, Semantic_Action_Arrays.Empty_Vector);

   function Get_Text_Rep
     (File_Name      : in String;
      McKenzie_Param : in McKenzie_Param_Type;
      Actions        : in Semantic_Action_Array_Arrays.Vector)
     return Parse_Table_Ptr;
   --  Read machine-readable text format of states (as output by
   --  WisiToken.Generate.LR.Put_Text_Rep) from file File_Name. Result
   --  has actions, checks from Productions.

   ----------
   --  For McKenzie_Recover. Declared here because Parser_Lists needs
   --  these, Mckenzie_Recover needs Parser_Lists.
   --
   --  We don't maintain a syntax tree during recover; it's too slow, and
   --  not needed for any operations. The parser syntax tree is used for
   --  Undo_Reduce, which is only done on nonterms reduced by the main
   --  parser, not virtual nonterms produced by recover.

   package Fast_Token_ID_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (SAL.Peek_Type, Token_ID, Capacity => 20);

   No_Insert_Delete : constant SAL.Base_Peek_Type := 0;

   function Image
     (Index      : in SAL.Peek_Type;
      Tokens     : in Fast_Token_ID_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor)
     return String
     is (SAL.Peek_Type'Image (Index) & ":" & SAL.Peek_Type'Image (Fast_Token_ID_Arrays.Last_Index (Tokens)) & ":" &
           Image (Fast_Token_ID_Arrays.Element (Tokens, Index), Descriptor));

   type Config_Op_Label is (Fast_Forward, Undo_Reduce, Push_Back, Insert, Delete);
   subtype Insert_Delete_Op_Label is Config_Op_Label range Insert .. Delete;
   --  Fast_Forward is a placeholder to mark a fast_forward parse; that
   --  resets what operations are allowed to be done on a config.
   --
   --  Undo_Reduce is the inverse of Reduce.
   --
   --  Push_Back pops the top stack item, and moves the input stream
   --  pointer back to the first shared_terminal contained by that item.
   --
   --  Insert inserts a new token in the token input stream, before the
   --  given point in Terminals.
   --
   --  Delete deletes one item from the token input stream, at the given
   --  point.

   type Config_Op (Op : Config_Op_Label := Fast_Forward) is record
      --  We store enough information to perform the operation on the main
      --  parser stack and input stream when the config is the result
      --  of a successful recover.

      case Op is
      when Fast_Forward =>
         FF_Token_Index : WisiToken.Token_Index;
         --  Config.Current_Shared_Token after the operation is done; the last
         --  token shifted.

      when Undo_Reduce =>
         Nonterm : Token_ID;
         --  The nonterminal popped off the stack.

         Token_Count : Ada.Containers.Count_Type;
         --  The number of tokens pushed on the stack.

      when Push_Back =>
         PB_ID : Token_ID;
         --  The nonterm ID popped off the stack.

         PB_Token_Index : WisiToken.Base_Token_Index;
         --  Config.Current_Shared_Token after
         --  the operation is done. If the token is empty, Token_Index is
         --  Invalid_Token_Index.

      when Insert =>
         Ins_ID : Token_ID;
         --  The token ID inserted.

         Ins_Token_Index : WisiToken.Base_Token_Index;
         --  Ins_ID is inserted before Token_Index.

      when Delete =>
         Del_ID : Token_ID;
         --  The token ID deleted.

         Del_Token_Index : WisiToken.Base_Token_Index;
         --  Token at Token_Index is deleted.

      end case;
   end record;
   subtype Insert_Delete_Op is Config_Op with Dynamic_Predicate => (Insert_Delete_Op.Op in Insert_Delete_Op_Label);
   subtype Insert_Op is Config_Op with Dynamic_Predicate => (Insert_Op.Op = Insert);

   function Token_Index (Op : in Insert_Delete_Op) return WisiToken.Token_Index
     is (case Insert_Delete_Op_Label'(Op.Op) is
         when Insert => Op.Ins_Token_Index,
         when Delete => Op.Del_Token_Index);

   function ID (Op : in Insert_Delete_Op) return WisiToken.Token_ID
     is (case Insert_Delete_Op_Label'(Op.Op) is
         when Insert => Op.Ins_ID,
         when Delete => Op.Del_ID);

   function Compare (Left, Right : in Insert_Delete_Op) return SAL.Compare_Result;
   --  Compare token_index.

   function Equal (Left : in Config_Op; Right : in Insert_Op) return Boolean;
   --  Ignore state, stack_depth

   package Config_Op_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive_Index_Type, Config_Op, Capacity => 80);
   --  Using a fixed size vector significantly speeds up
   --  McKenzie_Recover. The capacity is determined by the maximum number
   --  of repair operations, which is limited by the cost_limit McKenzie
   --  parameter plus an arbitrary number from the language-specific
   --  repairs; in practice, a capacity of 80 is enough so far. If a
   --  config does hit that limit, it is abandoned; some other config is
   --  likely to be cheaper.

   package Config_Op_Array_Refs is new Config_Op_Arrays.Gen_Refs;

   function Config_Op_Image (Item : in Config_Op; Descriptor : in WisiToken.Descriptor) return String
     is ("(" & Config_Op_Label'Image (Item.Op) & ", " &
           (case Item.Op is
            when Fast_Forward => WisiToken.Token_Index'Image (Item.FF_Token_Index),
            when Undo_Reduce => Image (Item.Nonterm, Descriptor) & "," &
                 Ada.Containers.Count_Type'Image (Item.Token_Count),
            when Push_Back => Image (Item.PB_ID, Descriptor) & "," &
                 WisiToken.Token_Index'Image (Item.PB_Token_Index),
            when Insert => Image (Item.Ins_ID, Descriptor) & "," &
                 WisiToken.Token_Index'Image (Item.Ins_Token_Index),
            when Delete => Image (Item.Del_ID, Descriptor) & "," &
                 WisiToken.Token_Index'Image (Item.Del_Token_Index))
           & ")");

   function Image (Item : in Config_Op; Descriptor : in WisiToken.Descriptor) return String
     renames Config_Op_Image;

   function Config_Op_Array_Image is new Config_Op_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Image);
   function Image (Item : in Config_Op_Arrays.Vector; Descriptor : in WisiToken.Descriptor) return String
     renames Config_Op_Array_Image;

   function None (Ops : aliased in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean;
   --  True if Ops contains no Op.

   function None_Since_FF (Ops : aliased in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean;
   --  True if Ops contains no Op after the last Fast_Forward (or ops.first, if
   --  no Fast_Forward).

   function Only_Since_FF (Ops : aliased in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean;
   --  True if Ops contains only Op (at least one) after the last Fast_Forward (or ops.first, if
   --  no Fast_Forward).

   function Any (Ops : aliased in Config_Op_Arrays.Vector; Op : in Config_Op_Label) return Boolean;
   --  True if Ops contains at least one Op.

   type Recover_Op (Op : Insert_Delete_Op_Label := Insert) is record
      --  Add Ins_Tree_Node to Config_Op info, set when item is
      --  parsed; used to create user augmented token.

      case Op is
      when Insert =>
         Ins_ID : Token_ID := Invalid_Token_ID;
         --  The token ID inserted.

         Ins_Token_Index : Base_Token_Index := Invalid_Token_Index;
         --  Ins_ID is inserted before Token_Index.

         Ins_Tree_Node : Node_Index := Invalid_Node_Index;

      when Delete =>
         Del_ID : Token_ID;
         --  The token ID deleted.

         Del_Token_Index : Base_Token_Index;
         --  Token at Token_Index is deleted.

      end case;
   end record;

   package Recover_Op_Arrays is new SAL.Gen_Bounded_Definite_Vectors
     (Positive_Index_Type, Recover_Op, Capacity => 80);

   package Recover_Op_Array_Refs is new Recover_Op_Arrays.Gen_Refs;

   function Image (Item : in Recover_Op; Descriptor : in WisiToken.Descriptor) return String
     is ("(" & Item.Op'Image & ", " &
           (case Item.Op is
            when Insert => Image (Item.Ins_ID, Descriptor) & "," &
                 Item.Ins_Token_Index'Image & "," &
                 Item.Ins_Tree_Node'Image,
            when Delete => Image (Item.Del_ID, Descriptor) & "," &
                 Item.Del_Token_Index'Image)
           & ")");

   function Image is new Recover_Op_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Image);

   type Recover_Stack_Item is record
      State : Unknown_State_Index := Unknown_State;

      Tree_Index : Node_Index := Invalid_Node_Index;
      --  Valid if copied at recover initialize, Invalid if pushed during
      --  recover.

      Token : Recover_Token;
      --  Virtual is False if token is from input text; True if inserted
      --  during recover.
   end record;

   package Recover_Stacks is new SAL.Gen_Bounded_Definite_Stacks (Recover_Stack_Item);

   function Image (Item : in Recover_Stack_Item; Descriptor : in WisiToken.Descriptor) return String
     is ((if Item.State = Unknown_State then " " else Trimmed_Image (Item.State)) & " : " &
           Image (Item.Token, Descriptor));

   function Recover_Stack_Image is new Recover_Stacks.Gen_Image_Aux (WisiToken.Descriptor, Image);
   --  Unique name for calling from debugger

   function Image
     (Stack      : in Recover_Stacks.Stack;
      Descriptor : in WisiToken.Descriptor;
      Depth      : in SAL.Base_Peek_Type := 0)
     return String
     renames Recover_Stack_Image;

   function Valid_Tree_Indices (Stack : in Recover_Stacks.Stack; Depth : in SAL.Base_Peek_Type) return Boolean with
     Pre => Stack.Depth >= Depth;
   --  Return True if Stack top Depth items have valid Tree_Indices,
   --  which is true if they were copied from the parser stack, and not
   --  pushed by recover.

   type Strategies is
     (Ignore_Error, Language_Fix, Minimal_Complete, Matching_Begin,
      Push_Back, Undo_Reduce, Insert, Delete, String_Quote);

   type Strategy_Counts is array (Strategies) of Natural;
   function Image is new SAL.Gen_Array_Image (Strategies, Natural, Strategy_Counts, Trimmed_Image);

   type Minimal_Complete_State is (None, Active, Done);

   type Configuration is record
      Stack : Recover_Stacks.Stack (70);
      --  Initially built from the parser stack, then the stack after the
      --  Ops below have been performed.
      --
      --  Required size is determined by source code structure nesting;
      --  larger size slows down recover due to memory cache thrashing and
      --  allocation.
      --
      --  Emacs Ada mode wisi.adb needs > 50

      Resume_Token_Goal : WisiToken.Token_Index := WisiToken.Token_Index'Last;
      --  A successful solution shifts this token. Per-config because it
      --  increases with Delete; we increase Shared_Parser.Resume_Token_Goal
      --  only from successful configs.

      Current_Shared_Token : Base_Token_Index := WisiToken.Token_Index'Last;
      --  Index into Shared_Parser.Terminals for current input token, after
      --  all of Inserted is input. Initially the error token.

      String_Quote_Checked : Line_Number_Type := Invalid_Line_Number;
      --  Max line checked for missing string quote.

      Insert_Delete : aliased Config_Op_Arrays.Vector;
      --  Edits to the input stream that are not yet parsed; contains only
      --  Insert and Delete ops, in token_index order.

      Current_Insert_Delete : SAL.Base_Peek_Type := No_Insert_Delete;
      --  Index of the next op in Insert_Delete. If No_Insert_Delete, use
      --  Current_Shared_Token.

      Error_Token       : Recover_Token;
      Check_Token_Count : Ada.Containers.Count_Type;
      Check_Status      : Semantic_Checks.Check_Status;
      --  If parsing this config ended with a parse error, Error_Token is
      --  the token that failed to shift, Check_Status.Label is Ok.
      --
      --  If parsing this config ended with a semantic check fail,
      --  Error_Token is the nonterm created by the reduction,
      --  Check_Token_Count the number of tokens in the right hand side, and
      --  Check_Status is the error.
      --
      --  Error_Token is set to Invalid_Token_ID when Config is parsed
      --  successfully, or modified so the error is no longer meaningful (ie
      --  in explore when adding an op, or in language_fixes when adding a
      --  fix).

      Ops : aliased Config_Op_Arrays.Vector;
      --  Record of operations applied to this Config, in application order.
      --  Insert and Delete ops that are not yet parsed are reflected in
      --  Insert_Delete, in token_index order.

      Cost : Natural := 0;

      Strategy_Counts : LR.Strategy_Counts := (others => 0);
      --  Count of strategies that produced Ops.

      Minimal_Complete_State : LR.Minimal_Complete_State := None;
      Matching_Begin_Done    : Boolean                   := False;
   end record;

   function Key (A : in Configuration) return Integer is (A.Cost);

   procedure Set_Key (Item : in out Configuration; Key : in Integer);

   package Config_Heaps is new SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci
     (Element_Type   => Configuration,
      Key_Type       => Integer,
      Key            => Key,
      Set_Key        => Set_Key);

   type Check_Status is (Success, Abandon, Continue);
   subtype Non_Success_Status is Check_Status range Abandon .. Continue;

   type McKenzie_Data is tagged record
      Config_Heap       : Config_Heaps.Heap_Type;
      Enqueue_Count     : Integer := 0;
      Config_Full_Count : Integer := 0;
      Check_Count       : Integer := 0;
      Results           : Config_Heaps.Heap_Type;
      Success           : Boolean := False;
   end record;
   type McKenzie_Access is access all McKenzie_Data;

   procedure Accumulate (Data : in McKenzie_Data; Counts : in out Strategy_Counts);
   --  Sum Results.Strategy_Counts.

   type Parse_Error_Label is (Action, Check, Message);

   type Parse_Error
     (Label          : Parse_Error_Label;
      First_Terminal : Token_ID;
      Last_Terminal  : Token_ID)
   is record
      Recover : Configuration;

      case Label is
      when Action =>
         Error_Token : Valid_Node_Index; -- index into Parser.Tree
         Expecting   : Token_ID_Set (First_Terminal .. Last_Terminal);

      when Check =>
         Check_Status : Semantic_Checks.Check_Status;

      when Message =>
         Msg : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   package Parse_Error_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Parse_Error);

end WisiToken.Parse.LR;
