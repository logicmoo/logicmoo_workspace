--  Abstract :
--
--  Ada implementation of wisi parser actions.
--
--  References
--
--  [1] wisi-parse-common.el - defines common stuff.
--
--  [2] wisi.texi - defines parse action functions.
--
--  [3] wisi-process-parse.el - defines elisp/process API
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Parse.LR;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package Wisi is
   use all type WisiToken.Base_Buffer_Pos;

   function Image (Aug : in WisiToken.Base_Token_Class_Access; Descriptor : in WisiToken.Descriptor) return String;
   function Image (Action : in WisiToken.Syntax_Trees.Semantic_Action) return String;
   --  For Syntax_Trees.Print_Tree, Parser.Execute_Action

   type Post_Parse_Action_Type is (Navigate, Face, Indent);

   type Parse_Data_Type
     (Terminals        : not null access constant WisiToken.Base_Token_Arrays.Vector;
      Line_Begin_Token : not null access constant WisiToken.Line_Begin_Token_Vectors.Vector)
     is new WisiToken.Syntax_Trees.User_Data_Type with private;

   procedure Initialize
     (Data              : in out Parse_Data_Type;
      Lexer             : in     WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Base_Terminals    : in     WisiToken.Base_Token_Array_Access;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     WisiToken.Line_Number_Type;
      End_Line          : in     WisiToken.Line_Number_Type;
      Begin_Indent      : in     Integer;
      Params            : in     String);
   --  Begin_Line, Begin_Indent, Line_Count only used for Indent. Params
   --  contains language-specific indent parameter values.

   overriding procedure Reset (Data : in out Parse_Data_Type);
   --  Reset for a new parse, with data from previous Initialize.

   function Source_File_Name (Data : in Parse_Data_Type) return String;
   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type;

   overriding
   procedure Lexer_To_Augmented
     (Data  : in out          Parse_Data_Type;
      Tree  : in out          WisiToken.Syntax_Trees.Tree'Class;
      Token : in              WisiToken.Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class);

   overriding
   procedure Insert_Token
     (Data  : in out Parse_Data_Type;
      Tree  : in out WisiToken.Syntax_Trees.Tree'Class;
      Token : in     WisiToken.Valid_Node_Index);

   overriding
   procedure Delete_Token
     (Data                : in out Parse_Data_Type;
      Tree                : in out WisiToken.Syntax_Trees.Tree'Class;
      Deleted_Token_Index : in     WisiToken.Token_Index);

   overriding
   procedure Reduce
     (Data    : in out Parse_Data_Type;
      Tree    : in out WisiToken.Syntax_Trees.Tree'Class;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array);

   type Navigate_Class_Type is (Motion, Statement_End, Statement_Override, Statement_Start, Misc);
   --  Matches [1] wisi-class-list.

   type Index_Navigate_Class is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      Class : Navigate_Class_Type;
   end record;

   type Statement_Param_Array is array (Natural range <>) of Index_Navigate_Class;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Params  : in     Statement_Param_Array);
   --  Implements [2] wisi-statement-action.

   procedure Name_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Name    : in     WisiToken.Positive_Index_Type);
   --  Implements [2] wisi-name-action.

   type Index_ID is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      ID    : WisiToken.Token_ID;
      --  If ID is not Invalid_Token_ID, it is the first token in the
      --  nonterm that Index points to that should have a navigate cache for
      --  Motion_Action to link to; an error is reported by Motion_Action if
      --  it does not.
      --
      --  If ID is Invalid_Token_ID, and the token at Index is a
      --  nonterminal, the first token in that nonterminal must have a
      --  navigate cache; an error is reported by Motion_Action if not.
   end record;

   package Index_ID_Vectors is new Ada.Containers.Vectors (Ada.Containers.Count_Type, Index_ID);

   subtype Motion_Param_Array is Index_ID_Vectors.Vector;

   Invalid_Token_ID : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
   --  So Create_Parser can just use "Invalid_Token_ID".

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Params  : in     Motion_Param_Array);
   --  Implements [2] wisi-motion-action.

   type Index_Faces is record
      Index       : WisiToken.Positive_Index_Type; -- into Tokens
      Prefix_Face : Integer; -- into grammar.Face_List
      Suffix_Face : Integer; -- into grammar.Face_List
   end record;

   type Face_Apply_Param_Array is array (Natural range <>) of Index_Faces;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [2] wisi-face-apply-action.

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Params  : in     Face_Apply_Param_Array);
   --  Implements [2] wisi-face-apply-list-action.

   type Face_Class_Type is (Prefix, Suffix);

   type Index_Face_Class is record
      Index : WisiToken.Positive_Index_Type; -- into Tokens
      Class : Face_Class_Type;
   end record;

   type Face_Mark_Param_Array is array (Natural range <>) of Index_Face_Class;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Params  : in     Face_Mark_Param_Array);
   --  Implements [2] wisi-face-mark-action.

   type Face_Remove_Param_Array is array (Natural range <>) of WisiToken.Positive_Index_Type;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Params  : in     Face_Remove_Param_Array);
   --  Implements [2] wisi-face-remove-action.

   ----------
   --  Indent
   --
   --  Indent functions are represented by the Indent_Param type.

   type Simple_Indent_Param_Label is -- not hanging
     (None,
      Int,
      Anchored_0, -- [2] wisi-anchored
      Anchored_1, -- [2] wisi-anchored%
      Anchored_2, -- [2] wisi-anchored%-
      Anchored_3, -- [2] wisi-anchored*
      Anchored_4, -- [2] wisi-anchored*-
      Language    -- [2] language-specific function
     );
   subtype Anchored_Label is Simple_Indent_Param_Label range Anchored_0 .. Anchored_4;

   --  Arguments to language-specific functions are integers; one of
   --  delta, Token_Number, or Token_ID - the syntax does not distinguish
   --  among these three types.

   package Indent_Arg_Arrays is new Ada.Containers.Vectors (WisiToken.Positive_Index_Type, Integer);

   function "+" (Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (List : in Indent_Arg_Arrays.Vector; Item : in Integer) return Indent_Arg_Arrays.Vector;
   function "&" (Left, Right : in Integer) return Indent_Arg_Arrays.Vector;

   type Delta_Type (<>) is private;

   type Language_Indent_Function is access function
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Tree_Tokens       : in     WisiToken.Valid_Node_Index_Array;
      Tree_Indenting    : in     WisiToken.Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Args              : in     Indent_Arg_Arrays.Vector)
     return Delta_Type;

   Null_Args : Indent_Arg_Arrays.Vector renames Indent_Arg_Arrays.Empty_Vector;

   type Simple_Indent_Param (Label : Simple_Indent_Param_Label := None) is
   record
      case Label is
      when None =>
         null;

      when Int =>
         Int_Delta : Integer;

      when Anchored_Label =>
         Anchored_Index : WisiToken.Positive_Index_Type;
         Anchored_Delta : Integer;

      when Language =>
         Function_Ptr : Language_Indent_Function;
         Args         : Indent_Arg_Arrays.Vector;
      end case;
   end record;

   function Image (Item : in Simple_Indent_Param) return String;

   type Indent_Param_Label is
     (Simple,
      Hanging_0, -- [2] wisi-hanging
      Hanging_1, -- [2] wisi-hanging-
      Hanging_2, -- [2] wisi-hanging%
      Hanging_3  -- [2] wisi-hanging%-
     );
   subtype Hanging_Label is Indent_Param_Label range Hanging_0 .. Hanging_3;

   type Indent_Param (Label : Indent_Param_Label := Simple) is
   record
      case Label is
      when Simple =>
         Param : Simple_Indent_Param;

      when Hanging_Label =>
         Hanging_Delta_1 : Simple_Indent_Param;
         Hanging_Delta_2 : Simple_Indent_Param;

      end case;
   end record;

   function Image (Item : in Indent_Param) return String;

   type Indent_Pair (Comment_Present : Boolean := False) is
   record
      Code_Delta : Indent_Param;
      case Comment_Present is
      when True =>
         Comment_Delta : Indent_Param;
      when False =>
         null;
      end case;
   end record;

   function Image (Item : in Indent_Pair) return String;

   type Indent_Param_Array is array (WisiToken.Positive_Index_Type range <>) of Indent_Pair;

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      Params  : in     Indent_Param_Array);
   --  Implements [2] wisi-indent-action.

   procedure Indent_Action_1
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     WisiToken.Syntax_Trees.Tree;
      Nonterm : in     WisiToken.Valid_Node_Index;
      Tokens  : in     WisiToken.Valid_Node_Index_Array;
      N       : in     WisiToken.Positive_Index_Type;
      Params  : in     Indent_Param_Array);
   --  Implements [2] wisi-indent-action*.

   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Tokens            : in     WisiToken.Valid_Node_Index_Array;
      Tree_Indenting    : in     WisiToken.Valid_Node_Index;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Option            : in     Boolean;
      Accumulate        : in     Boolean)
     return Delta_Type;
   --  Implements [2] wisi-hanging, wisi-hanging%, wisi-hanging%-.
   --
   --  Language specific child packages may override this to implement
   --  language-specific cases.

   ----------
   --  Other

   procedure Refactor
     (Data       : in out Parse_Data_Type;
      Tree       : in out WisiToken.Syntax_Trees.Tree;
      Action     : in     Positive;
      Edit_Begin : in     WisiToken.Buffer_Pos) is null;

   type Arg_Index_Array is array (Positive range <>) of WisiToken.Positive_Index_Type;

   procedure Put_Language_Action
     (Data    : in Parse_Data_Type;
      Content : in String);
   --  Send a Language_Action message to Emacs.

   procedure Put (Data : in out Parse_Data_Type; Parser : in WisiToken.Parse.Base_Parser'Class);
   --  Perform additional post-parse actions, then put result to
   --  Ada.Text_IO.Current_Output, as encoded responses as defined in [3]
   --  wisi-process-parse--execute.

   procedure Put (Lexer_Errors : in WisiToken.Lexer.Error_Lists.List);
   procedure Put
     (Data         : in Parse_Data_Type;
      Lexer_Errors : in WisiToken.Lexer.Error_Lists.List;
      Parse_Errors : in WisiToken.Parse.LR.Parse_Error_Lists.List;
      Tree         : in WisiToken.Syntax_Trees.Tree);
   --  Put Lexer_Errors and Parse_Errors to Ada.Text_IO.Current_Output,
   --  as encoded error responses as defined in [3]
   --  wisi-process-parse--execute.

   procedure Put_Error (Data : in Parse_Data_Type; Line_Number : in WisiToken.Line_Number_Type; Message : in String);
   --  Put an error elisp form to Ada.Text_IO.Standard_Output.

private

   type Non_Grammar_Token is new WisiToken.Base_Token with record
      First : Boolean := False;
   end record;

   package Non_Grammar_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Token_Index, Non_Grammar_Token, Default_Element => (others => <>));

   type Augmented_Token is new WisiToken.Base_Token with record
      --  Most fields are set by Lexer_To_Augmented at parse time; others
      --  are set by Reduce for nonterminals.

      Deleted : Boolean := False;
      --  Set True by Parse_Data_Type.Delete_Token; Non_Grammar tokens are
      --  moved to the previous non-deleted token.

      --  The following fields are only needed for indent.

      First : Boolean := False;
      --  For a terminal, True if the token is first on a line.
      --
      --  For a nonterminal, True if some contained token's First is True.

      Paren_State : Integer := 0;
      --  Parenthesis nesting count, before token.

      First_Terminals_Index : WisiToken.Base_Token_Index := WisiToken.Invalid_Token_Index;
      --  For virtual tokens, Invalid_Token_Index
      --
      --  For terminal tokens, index of this token in Parser.Terminals.
      --
      --  For nonterminal tokens, index of first contained token in
      --  Parser.Terminals.

      Last_Terminals_Index : WisiToken.Base_Token_Index := WisiToken.Base_Token_Arrays.No_Index;
      --  For non-virtual nonterminal tokens, index of last contained
      --  token in Parser.Terminals.
      --
      --  For all others, same as First_Terminals_Index.

      First_Indent_Line : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      Last_Indent_Line  : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      --  Lines that need indenting; first token on these lines is contained
      --  in this token. If First is False, these are Invalid_Line_Number.
      --
      --  First_, Last_Indent_Line include blank and comment lines between
      --  grammar tokens, but exclude trailing blanks and comments after the
      --  last token, so they can be indented differently.

      First_Trailing_Comment_Line : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      Last_Trailing_Comment_Line  : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      --  Trailing comment or blank lines (after the last contained grammar
      --  token) that need indenting. Excludes comments following code on a
      --  line. If there are no such lines, these are Invalid_Line_Number.

      Non_Grammar : Non_Grammar_Token_Arrays.Vector;
      --  For terminals, non-grammar tokens immediately following. For
      --  nonterminals, empty.

      Inserted_Before : WisiToken.Valid_Node_Index_Arrays.Vector;
      --  Tokens inserted before this token by error recovery.

   end record;
   type Augmented_Token_Access is access all Augmented_Token;
   type Augmented_Token_Access_Constant is access constant Augmented_Token;

   type Aug_Token_Const_Ref (Element : not null access constant Augmented_Token) is null record with
     Implicit_Dereference => Element;

   function To_Aug_Token_Const_Ref (Item : in WisiToken.Base_Token_Class_Access) return Aug_Token_Const_Ref
     is (Element => Augmented_Token_Access_Constant (Item));

   type Aug_Token_Var_Ref (Element : not null access Augmented_Token) is null record with
     Implicit_Dereference => Element;

   function To_Aug_Token_Var_Ref (Item : in WisiToken.Base_Token_Class_Access) return Aug_Token_Var_Ref
     is (Element => Augmented_Token_Access (Item));

   overriding
   function Image
     (Item       : in Augmented_Token;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  Return a string for debug/test messages

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return WisiToken.Line_Number_Type;
   function Last_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return WisiToken.Line_Number_Type;
   --  Return first and last line in Token's region.

   package Line_Paren_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Line_Number_Type, Integer, Default_Element => Integer'Last);
   package Line_Begin_Pos_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Line_Number_Type, WisiToken.Buffer_Pos, Default_Element => WisiToken.Invalid_Buffer_Pos);

   type Nil_Buffer_Pos (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : WisiToken.Buffer_Pos;
      when False =>
         null;
      end case;
   end record;

   Nil : constant Nil_Buffer_Pos := (Set => False);

   type Navigate_Cache_Type is record
      Pos            : WisiToken.Buffer_Pos; -- implicit in [1] wisi-cache
      Statement_ID   : WisiToken.Token_ID;   -- [1] wisi-cache-nonterm
      ID             : WisiToken.Token_ID;   -- [1] wisi-cache-token
      Length         : Natural;              -- [1] wisi-cache-last
      Class          : Navigate_Class_Type;  -- [1] wisi-cache-class
      Containing_Pos : Nil_Buffer_Pos;       -- [1] wisi-cache-containing
      Prev_Pos       : Nil_Buffer_Pos;       -- [1] wisi-cache-prev
      Next_Pos       : Nil_Buffer_Pos;       -- [1] wisi-cache-next
      End_Pos        : Nil_Buffer_Pos;       -- [1] wisi-cache-end
   end record;

   function Key (Cache : in Navigate_Cache_Type) return WisiToken.Buffer_Pos is (Cache.Pos);

   function Key_Compare (Left, Right : in WisiToken.Buffer_Pos) return SAL.Compare_Result is
     (if Left > Right then SAL.Greater
      elsif Left = Right then SAL.Equal
      else SAL.Less);

   package Navigate_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Navigate_Cache_Type, WisiToken.Buffer_Pos);

   function Key (Cache : in WisiToken.Buffer_Region) return WisiToken.Buffer_Pos is (Cache.First);

   package Name_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (WisiToken.Buffer_Region, WisiToken.Buffer_Pos);

   type Nil_Integer (Set : Boolean := False) is record
      case Set is
      when True =>
         Item : Integer;
      when False =>
         null;
      end case;
   end record;

   type Face_Cache_Type is record
      Char_Region : WisiToken.Buffer_Region;
      Class       : Face_Class_Type;
      Face        : Nil_Integer; -- not set, or index into *-process-faces-names
   end record;

   function Key (Cache : in Face_Cache_Type) return WisiToken.Buffer_Pos is (Cache.Char_Region.First);

   package Face_Cache_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees (Face_Cache_Type, WisiToken.Buffer_Pos);

   type Indent_Label is (Not_Set, Int, Anchor_Nil, Anchor_Int, Anchored, Anchor_Anchored);

   package Anchor_ID_Vectors is new Ada.Containers.Vectors (Natural, Positive);

   type Indent_Type (Label : Indent_Label := Not_Set) is record
      --  Indent values may be negative while indents are being computed.
      case Label is
      when Not_Set =>
         null;

      when Int =>
         Int_Indent : Integer;

      when Anchor_Nil =>
         Anchor_Nil_IDs : Anchor_ID_Vectors.Vector; --  Largest ID first.

      when Anchor_Int =>
         Anchor_Int_IDs    : Anchor_ID_Vectors.Vector; --  Largest ID first.
         Anchor_Int_Indent : Integer; --  Indent for this token.

      when Anchored =>
         Anchored_ID    : Positive;
         Anchored_Delta : Integer; -- added to Anchor_Indent of Anchor_ID

      when Anchor_Anchored =>
         Anchor_Anchored_IDs   : Anchor_ID_Vectors.Vector;
         Anchor_Anchored_ID    : Natural;
         Anchor_Anchored_Delta : Integer;
      end case;
   end record;
   First_Anchor_ID : constant Positive := Positive'First;

   package Indent_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Line_Number_Type, Indent_Type, Default_Element => (others => <>));
   package Navigate_Cursor_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Navigate_Cache_Trees.Cursor, Navigate_Cache_Trees."=");

   type Parse_Data_Type
     (Terminals        : not null access constant WisiToken.Base_Token_Arrays.Vector;
      Line_Begin_Token : not null access constant WisiToken.Line_Begin_Token_Vectors.Vector)
     is new WisiToken.Syntax_Trees.User_Data_Type with
   record
      --  Aux token info
      First_Comment_ID : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Last_Comment_ID  : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Left_Paren_ID    : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;
      Right_Paren_ID   : WisiToken.Token_ID := WisiToken.Invalid_Token_ID;

      Embedded_Quote_Escape_Doubled : Boolean := False;

      --  Data from parsing

      --  All Augmented_Tokens are stored in the syntax tree.
      Last_Terminal_Node : WisiToken.Node_Index := WisiToken.Invalid_Node_Index;

      Leading_Non_Grammar : Non_Grammar_Token_Arrays.Vector;
      --  non-grammar tokens before first grammar token.

      Line_Begin_Char_Pos : Line_Begin_Pos_Vectors.Vector;
      --  Character position at the start of the first token on each line.
      --  Cached from Line_Begin_Token to simplify indent computations.

      Line_Paren_State : Line_Paren_Vectors.Vector;
      --  Parenthesis nesting state at the start of each line; used by
      --  Indent. Set by Lexer_To_Augmented on New_Line_ID, updated by
      --  Insert_Token, Delete_Token.

      Current_Paren_State : Integer;
      --  Current parenthesis nesting state; used by Indent. Set by
      --  Lexer_To_Augmented on Left_Paren_ID, Right_Paren_ID.

      --  Data for post-parse actions

      Lexer             : WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Base_Terminals    : WisiToken.Base_Token_Array_Access;
      Post_Parse_Action : Post_Parse_Action_Type;
      Navigate_Caches   : Navigate_Cache_Trees.Tree;  -- Set by Navigate.
      Name_Caches       : Name_Cache_Trees.Tree;      -- Set by Navigate.
      End_Positions     : Navigate_Cursor_Lists.List; -- Dynamic data for Navigate.
      Face_Caches       : Face_Cache_Trees.Tree;      -- Set by Face.
      Indents           : Indent_Vectors.Vector;      -- Set by Indent.
      Begin_Indent      : Integer;                    -- Indentation of line at start of parse.

      --  Copied from language-specific parameters
      Indent_Comment_Col_0 : Boolean := False;

      --  Dynamic data for Indent
      Max_Anchor_ID : Integer;
   end record;

   type Simple_Delta_Labels is (None, Int, Anchored);

   --  subtype Non_Anchored_Delta_Labels is Simple_Delta_Labels range None .. Int;

   --  type Non_Anchored_Delta (Label : Non_Anchored_Delta_Labels := None) is
   --  record
   --     case Label is
   --     when None =>
   --        null;
   --     when Int =>
   --        Int_Delta : Integer;
   --     end case;
   --  end record;

   --  function Image (Item : in Non_Anchored_Delta) return String;
   --  For debugging

   type Simple_Delta_Type (Label : Simple_Delta_Labels := None) is
   record
      case Label is
      when None =>
         null;

      when Int =>
         Int_Delta : Integer;

      when Anchored =>
         Anchored_ID         : Natural;
         Anchored_Delta      : Integer;
         Anchored_Accumulate : Boolean;

      end case;
   end record;

   function Image (Item : in Simple_Delta_Type) return String;
   --  For debugging

   type Delta_Labels is (Simple, Hanging);

   type Delta_Type (Label : Delta_Labels := Simple) is
   record
      case Label is
      when Simple =>
         Simple_Delta : Simple_Delta_Type;

      when Hanging =>
         Hanging_First_Line  : WisiToken.Line_Number_Type;
         Hanging_Paren_State : Integer;
         Hanging_Delta_1     : Simple_Delta_Type; -- indentation of first line
         Hanging_Delta_2     : Simple_Delta_Type; -- indentation of continuation lines
         Hanging_Accumulate  : Boolean;
      end case;
   end record;

   Null_Delta : constant Delta_Type := (Simple, (Label => None));

   function Image (Item : in Delta_Type) return String;
   --  For debugging

   ----------
   --  Utilities for language-specific child packages

   function Current_Indent_Offset
     (Data         : in Parse_Data_Type;
      Anchor_Token : in Augmented_Token'Class;
      Offset       : in Integer)
     return Integer;
   --  Return offset from beginning of first token on line containing
   --  Anchor_Token, to beginning of Anchor_Token, plus Offset.

   function Get_Aug_Token_Const_1
     (Tree       : in WisiToken.Syntax_Trees.Tree'Class;
      Tree_Index : in WisiToken.Valid_Node_Index)
     return Aug_Token_Const_Ref;
   --  WORKAROUND: GNAT Community 2019 can't do the overload resolution
   --  between the two Get_Aug_Token_Const without an explicit renames,
   --  so we add _1 to this one.

   function Get_Aug_Token_Const
     (Data  : in Parse_Data_Type;
      Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Token_Index)
     return Aug_Token_Const_Ref;

   function Get_Aug_Token_Var
     (Tree       : in WisiToken.Syntax_Trees.Tree'Class;
      Tree_Index : in WisiToken.Valid_Node_Index)
     return Aug_Token_Var_Ref;

   function Get_Aug_Token_Var
     (Data  : in Parse_Data_Type;
      Tree  : in WisiToken.Syntax_Trees.Tree'Class;
      Token : in WisiToken.Token_Index)
     return Aug_Token_Var_Ref;

   --  function Get_First_Terminal
   --    (Data  : in Parse_Data_Type;
   --     Tree  : in WisiToken.Syntax_Trees.Tree'Class;
   --     Token : in WisiToken.Token_Index)
   --    return Aug_Token_Const_Ref;
   --  Return Augmented for first Token.Inserted_Before, or if that is
   --  empty, for Token.

   function Get_Text
     (Data       : in Parse_Data_Type;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Tree_Index : in WisiToken.Valid_Node_Index)
     return String;
   --  Return text contained by Tree_Index token in source file
   --  (lexer.buffer).

   function Elisp_Escape_Quotes (Item : in String) return String;
   --  Prefix any '"' in Item with '\' for elisp.

   function Indent_Anchored_2
     (Data        : in out Parse_Data_Type;
      Anchor_Line : in     WisiToken.Line_Number_Type;
      Last_Line   : in     WisiToken.Line_Number_Type;
      Offset      : in     Integer;
      Accumulate  : in     Boolean)
     return Delta_Type;

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Tokens            : in     WisiToken.Valid_Node_Index_Array;
      Param             : in     Indent_Param;
      Tree_Indenting    : in     WisiToken.Valid_Node_Index;
      Indenting_Comment : in     Boolean)
     return Delta_Type;

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     WisiToken.Syntax_Trees.Tree;
      Indenting_Token   : in     Augmented_Token'Class;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Boolean);
   --  Sets Data.Indents, so caller may not be in a renames for a
   --  Data.Indents element.

   --  Visible for language-specific children. Must match list in
   --  [3] wisi-process-parse--execute.
   Navigate_Cache_Code  : constant String := "1";
   Face_Property_Code   : constant String := "2";
   Indent_Code          : constant String := "3";
   Lexer_Error_Code     : constant String := "4";
   Parser_Error_Code    : constant String := "5";
   Check_Error_Code     : constant String := "6";
   Recover_Code         : constant String := "7 ";
   End_Code             : constant String := "8";
   Name_Property_Code   : constant String := "9";
   Edit_Action_Code     : constant String := "10";
   Language_Action_Code : constant String := "11 ";

end Wisi;
