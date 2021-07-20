--  Abstract:
--
--  Root of WisiToken lexer/parser generator and exector.
--
--  The token type is an integer subtype, not an enumeration type, to
--  avoid making this package generic, which would make all other
--  packages generic.
--
--  Additional information about a token can be stored in the
--  'augmented' field of the syntax tree; see
--  wisitoken-syntax_trees.ads.
--
--  References:
--
--  [dragon] "Compilers Principles, Techniques, and Tools" by Aho,
--  Sethi, and Ullman (aka: "The [Red] Dragon Book" due to the dragon
--  on the cover).
--
--  Copyright (C) 2009, 2010, 2013 - 2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.
--
--  This software was originally developed with the name OpenToken by
--  the following company, and was released as open-source software as
--  a service to the community:
--
--           FlightSafety International Simulation Systems Division
--                    Broken Arrow, OK  USA  918-259-4000

pragma License (Modified_GPL);

with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with SAL.Generic_Decimal_Image;
with SAL.Gen_Trimmed_Image;
with SAL.Gen_Unbounded_Definite_Queues;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Image;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_Image_Aux;
with SAL.Gen_Unconstrained_Array_Image;
package WisiToken is

   Partial_Parse : exception; -- a partial parse terminated.

   Syntax_Error : exception; -- no recovery for a syntax error was found

   Parse_Error : exception; -- a non-recoverable non-fatal error was encountered; editing the input can fix the error.

   Fatal_Error : exception; -- Error in code or grammar; editing input cannot fix error.

   Grammar_Error : exception;
   --  Grammar file has bad syntax, or grammar is not consistent (ie
   --  unused tokens, missing productions, invalid actions)

   User_Error : exception; -- other user error (ie command line parameter)

   --  SAL.Programmer_Error : exception; -- a programming convention has been violated

   subtype Positive_Index_Type is SAL.Peek_Type;
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (SAL.Base_Peek_Type);

   type Unknown_State_Index is new Integer range -1 .. Integer'Last;
   subtype State_Index is Unknown_State_Index range 0 .. Unknown_State_Index'Last;
   Unknown_State : constant Unknown_State_Index := -1;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Unknown_State_Index);

   package State_Index_Queues is new SAL.Gen_Unbounded_Definite_Queues (State_Index);
   package State_Index_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive, State_Index, Default_Element => State_Index'Last);
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Integer);
   function Image is new State_Index_Arrays.Gen_Image (Trimmed_Image);

   ----------
   --  Token IDs

   type Token_ID is range 0 .. Integer'Last; -- 0 origin to match elisp array

   Invalid_Token_ID : constant Token_ID := Token_ID'Last;

   type String_Access_Constant is access constant String;
   type Token_ID_Array_String is array (Token_ID range <>) of String_Access_Constant;
   type Token_ID_Array_Natural is array (Token_ID range <>) of Natural;

   type Descriptor
     (First_Terminal    : Token_ID;
      Last_Terminal     : Token_ID;
      First_Nonterminal : Token_ID;
      Last_Nonterminal  : Token_ID;
      EOI_ID            : Token_ID;
      Accept_ID         : Token_ID)
   is record
      --  Tokens in the range Token_ID'First .. First_Terminal - 1 are
      --  non-reporting (comments, whitespace), and thus are not used in
      --  generating parse tables.
      --
      --  Tokens in the range Last_Terminal + 1 .. Last_Nonterminal are
      --  the nonterminals of a grammar.
      --
      --  Components are discriminants if they can be specified statically.

      Case_Insensitive : Boolean;  -- keywords and names
      New_Line_ID      : Token_ID;

      String_1_ID : Token_ID;
      String_2_ID : Token_ID;
      --  String_1 delimited by '; String_2 by ".
      --
      --  Used by missing quote error recovery. If the language does not
      --  have two kinds of string literals, set one or both of these to
      --  Invalid_Token_ID.

      Image : Token_ID_Array_String (Token_ID'First .. Last_Nonterminal);
      --  User names for tokens.

      Terminal_Image_Width : Integer;
      Image_Width          : Integer; --  max width of Image

      Last_Lookahead : Token_ID;
      --  LALR generate needs a 'Propagate_ID' lookahead that is distinct
      --  from all terminals. Since lookaheads are Token_ID_Set, we need to
      --  allocate First_Terminal .. Last_Terminal for LR1 generate, and
      --  First_Terminal .. Propagate_ID for LALR generate, so we define
      --  Last_Lookahead. After the LR table is generated, Last_Lookahead is
      --  no longer used.
   end record;
   type Descriptor_Access is access Descriptor;
   type Descriptor_Access_Constant is access constant Descriptor;

   function Padded_Image (Item : in Token_ID; Desc : in Descriptor) return String;
   --  Return Desc.Image (Item), padded to Terminal_Image_Width (if Item
   --  is a terminal) or to Image_Width.

   function Image (Item : in Token_ID; Desc : in Descriptor) return String;
   --  Return Desc.Image (Item), or "-" for Invalid_Token_ID.

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Token_ID);

   procedure Put_Tokens (Descriptor : in WisiToken.Descriptor);
   --  Put user readable token list (token_id'first ..
   --  descriptor.last_nonterminal) to Ada.Text_IO.Current_Output

   function Find_ID (Descriptor : in WisiToken.Descriptor; Name : in String) return Token_ID;
   --  Return index of Name in Descriptor.Image. If not found, raise Programmer_Error.

   type Token_ID_Array is array (Positive range <>) of Token_ID;
   --  Index is not Positive_Index_Type, mostly for historical reasons.

   package Token_ID_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive, Token_ID, Default_Element => Invalid_Token_ID);

   function Image is new Token_ID_Arrays.Gen_Image_Aux (Descriptor, Trimmed_Image, Image);
   function Image_No_Assoc (Item : in Token_ID_Arrays.Vector; Aux : in Descriptor) return String
     is (Image (Item, Aux, Association => False));

   function Trimmed_Image is new Token_ID_Arrays.Gen_Image (Trimmed_Image);

   procedure To_Vector (Item : in Token_ID_Array; Vector : in out Token_ID_Arrays.Vector);
   function To_Vector (Item : in Token_ID_Array) return Token_ID_Arrays.Vector;

   function Shared_Prefix (A, B : in Token_ID_Arrays.Vector) return Natural;
   --  Return last index in A of a prefix shared between A, B; 0 if none.

   type Token_ID_Set is array (Token_ID range <>) of Boolean;
   type Token_ID_Set_Access is access Token_ID_Set;

   function "&" (Left : in Token_ID_Set; Right : in Token_ID) return Token_ID_Set;
   --  Include Left and Right in result.

   function To_Token_ID_Set (First, Last : in Token_ID; Item : in Token_ID_Array) return Token_ID_Set;
   --  First, Last determine size of result.
   --  For each element in Item, set result (element) True.

   procedure To_Set (Item : in Token_ID_Arrays.Vector; Set : out Token_ID_Set);
   --  For each element of Item, set Set (element) True.

   function To_Array (Item : in Token_ID_Set) return Token_ID_Arrays.Vector;

   function Any (Item : in Token_ID_Set) return Boolean;

   function Count (Item : in Token_ID_Set) return Integer;
   --  Count of True elements.

   function Image
     (Item      : in Token_ID_Set;
      Desc      : in Descriptor;
      Max_Count : in Integer := Integer'Last;
      Inverted  : in Boolean := False)
     return String;
   --  For diagnostics; not Ada syntax.

   type Token_Array_Token_Set is array (Token_ID range <>, Token_ID range <>) of Boolean;

   function Slice (Item : in Token_Array_Token_Set; I : in Token_ID) return Token_ID_Set;
   function Any (Item : in Token_Array_Token_Set; I : in Token_ID) return Boolean;
   function Any (Item : in Token_Array_Token_Set) return Boolean;
   procedure Or_Slice (Item : in out Token_Array_Token_Set; I : in Token_ID; Value : in Token_ID_Set);

   procedure Put (Descriptor : in WisiToken.Descriptor; Item : in Token_Array_Token_Set);
   --  Put Item to Ada.Text_IO.Current_Output, using valid Ada aggregate
   --  syntax.

   type Token_Array_Token_ID is array (Token_ID range <>) of Token_ID;

   package Token_Sequence_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Token_ID, Token_ID_Arrays.Vector, Default_Element => Token_ID_Arrays.Empty_Vector);

   ----------
   --  Production IDs; see wisitoken-productions.ads for more

   type Production_ID is record
      LHS : Token_ID := Invalid_Token_ID;
      RHS : Natural  := 0;
      --  Index into the production table.
   end record;

   Invalid_Production_ID : constant Production_ID := (others => <>);

   function Image (Item : in Production_ID) return String;
   --  Ada positional aggregate syntax, for code generation.

   function Trimmed_Image (Item : in Production_ID) return String;
   --  Nonterm.rhs_index, both integers, no leading or trailing space;
   --  for parse table output and diagnostics.

   Prod_ID_Image_Width : constant Integer := 7;
   --  Max width of Trimmed_Image

   function Padded_Image (Item : in Production_ID; Width : in Integer) return String;
   --  Trimmed_Image padded with leading spaces to Width

   package Production_ID_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive, Production_ID, Default_Element => Invalid_Production_ID);
   function Image is new Production_ID_Arrays.Gen_Image (Image);
   function Trimmed_Image is new Production_ID_Arrays.Gen_Image (Trimmed_Image);

   type Production_ID_Array is array (Natural range <>) of Production_ID;

   function To_Vector (Item : in Production_ID_Array) return Production_ID_Arrays.Vector;
   function "+" (Item : in Production_ID_Array) return Production_ID_Arrays.Vector renames To_Vector;
   function "+" (Item : in Production_ID) return Production_ID_Arrays.Vector is (To_Vector ((1 => Item)));

   type Token_Array_Production_ID is array (Token_ID range <>) of Production_ID;

   type Recursion_Class is (None, Direct_Left, Other_Left, Other, Other_Right, Direct_Right);
   function Image (Item : in Recursion_Class) return String
     is (case Item is
         when None         => "None",
         when Direct_Left  => "Direct_Left",
         when Other_Left   => "Other_Left",
         when Other        => "Other",
         when Other_Right  => "Other_Right",
         when Direct_Right => "Direct_Right");

   ----------
   --  Tokens

   type Base_Buffer_Pos is range 0 .. Integer'Last;
   subtype Buffer_Pos is Base_Buffer_Pos range 1 .. Base_Buffer_Pos'Last; -- match Emacs buffer origin.
   type Buffer_Region is record
      First : Buffer_Pos;
      Last  : Base_Buffer_Pos; --  allow representing null range.
   end record;

   Invalid_Buffer_Pos : constant Buffer_Pos    := Buffer_Pos'Last;
   Null_Buffer_Region : constant Buffer_Region := (Buffer_Pos'Last, Buffer_Pos'First);

   function Length (Region : in Buffer_Region) return Natural is (Natural (Region.Last - Region.First + 1));

   function Inside (Pos : in Buffer_Pos; Region : in Buffer_Region) return Boolean
     is (Region.First <= Pos and Pos <= Region.Last);

   function Image (Item : in Buffer_Region) return String;

   function "and" (Left, Right : in Buffer_Region) return Buffer_Region;
   --  Return region enclosing both Left and Right.

   type Line_Number_Type is range 1 .. Natural'Last; -- Match Emacs buffer line numbers.
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Line_Number_Type);

   Invalid_Line_Number : constant Line_Number_Type := Line_Number_Type'Last;

   --  Syntax tree nodes.
   type Node_Index is range 0 .. Integer'Last;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;
   --  Note that Valid_Node_Index includes Deleted_Child.

   Invalid_Node_Index : constant Node_Index := Node_Index'First;
   Deleted_Child      : constant Node_Index := Node_Index'Last;

   type Valid_Node_Index_Array is array (Positive_Index_Type range <>) of Valid_Node_Index;
   --  Index matches Base_Token_Array, Augmented_Token_Array

   function Image is new SAL.Generic_Decimal_Image (Valid_Node_Index);
   --  Has Width parameter

   function Image (Item : in Valid_Node_Index) return String
     is (Image (Item, 4));

   function Image is new SAL.Gen_Unconstrained_Array_Image
     (Positive_Index_Type, Valid_Node_Index, Valid_Node_Index_Array, Image);

   package Valid_Node_Index_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive_Index_Type, Valid_Node_Index, Default_Element => Valid_Node_Index'Last);
   --  Index matches Valid_Node_Index_Array.

   type Base_Token is tagged record
      --  Base_Token is used in the core parser. The parser only needs ID and Tree_Index;
      --  semantic checks need Byte_Region to compare names. Line, Col, and
      --  Char_Region are included for error messages.

      ID         : Token_ID   := Invalid_Token_ID;
      Tree_Index : Node_Index := Invalid_Node_Index;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Index into the Lexer buffer for the token text.

      Line   : Line_Number_Type  := Invalid_Line_Number;
      Column : Ada.Text_IO.Count := 0;
      --  At start of token.

      Char_Region : Buffer_Region := Null_Buffer_Region;
      --  Character position, useful for finding the token location in Emacs
      --  buffers.
   end record;

   type Base_Token_Class_Access is access all Base_Token'Class;
   type Base_Token_Class_Access_Constant is access constant Base_Token'Class;

   function Image
     (Item       : in Base_Token;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  For debug/test messages.

   procedure Free is new Ada.Unchecked_Deallocation (Base_Token'Class, Base_Token_Class_Access);

   Invalid_Token : constant Base_Token := (others => <>);

   type Base_Token_Index is range 0 .. Integer'Last;
   subtype Token_Index is Base_Token_Index range 1 .. Base_Token_Index'Last;

   Invalid_Token_Index : constant Base_Token_Index := Base_Token_Index'First;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Base_Token_Index);

   type Token_Index_Array is array (Natural range <>) of Token_Index;

   package Recover_Token_Index_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Natural, Base_Token_Index, Default_Element => Invalid_Token_Index);

   type Base_Token_Array is array (Positive_Index_Type range <>) of Base_Token;

   package Base_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Token_Index, Base_Token, Default_Element => (others => <>));
   type Base_Token_Array_Access is access all Base_Token_Arrays.Vector;
   type Base_Token_Array_Access_Constant is access constant Base_Token_Arrays.Vector;

   function Image is new Base_Token_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Trimmed_Image, Image);

   function Image
     (Token      : in Base_Token_Index;
      Terminals  : in Base_Token_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor)
     return String;

   package Line_Begin_Token_Vectors is new SAL.Gen_Unbounded_Definite_Vectors
     (Line_Number_Type, Base_Token_Index, Default_Element => Invalid_Token_Index);

   type Recover_Token is record
      --  Maintaining a syntax tree during error recovery is too slow, so we
      --  store enough information in the recover stack to perform
      --  Semantic_Checks, Language_Fixes, and Push_Back operations. and to
      --  apply the solution to the main parser state. We make thousands of
      --  copies of the parse stack during recover, so minimizing size and
      --  compute time for this is critical.
      ID : Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Byte_Region is used to detect empty tokens, for cost and other issues.

      Min_Terminal_Index : Base_Token_Index := Invalid_Token_Index;
      --  For terminals, index of this token in Shared_Parser.Terminals. For
      --  nonterminals, minimum of contained tokens (Invalid_Token_Index if
      --  empty). For virtuals, Invalid_Token_Index. Used for push_back of
      --  nonterminals.

      Name : Buffer_Region := Null_Buffer_Region;
      --  Set and used by semantic_checks.

      Virtual : Boolean := True;
      --  For terminals, True if inserted by recover. For nonterminals, True
      --  if any contained token has Virtual = True.
   end record;

   function Image
     (Item       : in Recover_Token;
      Descriptor : in WisiToken.Descriptor)
     return String;

   type Recover_Token_Array is array (Positive_Index_Type range <>) of Recover_Token;

   package Recover_Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Token_Index, Recover_Token, Default_Element => (others => <>));

   function Image is new Recover_Token_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Trimmed_Image, Image);

   type Base_Identifier_Index is range 0 .. Integer'Last;
   subtype Identifier_Index is Base_Identifier_Index range 1 .. Base_Identifier_Index'Last;
   --  For virtual identifiers created during syntax tree rewrite.

   Invalid_Identifier_Index : constant Base_Identifier_Index := Base_Identifier_Index'First;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Base_Identifier_Index);

   ----------
   --  Trace, debug

   Trace_Parse : Integer  := 0;
   --  If Trace_Parse > 0, Parse prints messages helpful for debugging
   --  the grammar and/or the parser; higher value prints more.
   --
   --  Trace_Parse levels; output info if Trace_Parse > than:
   --
   Outline     : constant := 0; -- spawn/terminate parallel parsers, error recovery enter/exit
   Detail      : constant := 1; -- add each parser cycle
   Extra       : constant := 2; -- add pending semantic state operations
   Lexer_Debug : constant := 3; -- add lexer debug

   Trace_McKenzie : Integer  := 0;
   --  If Trace_McKenzie > 0, Parse prints messages helpful for debugging error recovery.
   --
   --  Outline - error recovery enter/exit
   --  Detail  - add each error recovery configuration
   --  Extra   - add error recovery parse actions

   Trace_Action : Integer := 0;
   --  Output during Execute_Action, and unit tests.

   Trace_Generate_EBNF             : Integer := 0;
   Trace_Generate_Table            : Integer := 0;
   Trace_Generate_Minimal_Complete : Integer := 0;
   --  Output during grammar generation.

   Trace_Time : Boolean := False;
   --  Output execution time for various things.

   Debug_Mode : Boolean := False;
   --  If True, Output stack traces, propagate exceptions to top level.
   --  Otherwise, be robust to errors, so user does not notice them.

   type Trace (Descriptor : not null access constant WisiToken.Descriptor) is abstract tagged limited null record;
   --  Output for tests/debugging. Descriptor included here because many
   --  uses of Trace will use Image (Item, Descriptor);

   procedure Set_Prefix (Trace : in out WisiToken.Trace; Prefix : in String) is abstract;
   --  Prepend Prefix to all subsequent messages. Usefull for adding
   --  comment syntax.

   procedure Put (Trace : in out WisiToken.Trace; Item : in String; Prefix : in Boolean := True) is abstract;
   --  Put Item to the Trace display. If Prefix is True, prepend the stored prefix.

   procedure Put_Line (Trace : in out WisiToken.Trace; Item : in String) is abstract;
   --  Put Item to the Trace display, followed by a newline.

   procedure New_Line (Trace : in out WisiToken.Trace) is abstract;
   --  Put a newline to the Trace display.

   procedure Put_Clock (Trace : in out WisiToken.Trace; Label : in String) is abstract;
   --  Put Ada.Calendar.Clock to Trace.

   ----------
   --  Misc

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Ada.Containers.Count_Type);

   function Error_Message
     (File_Name : in String;
      Line      : in Line_Number_Type;
      Column    : in Ada.Text_IO.Count;
      Message   : in String)
     return String;
   --  Return Gnu-formatted error message.

   type Names_Array is array (Integer range <>) of String_Access_Constant;
   type Names_Array_Access is access Names_Array;
   type Names_Array_Array is array (WisiToken.Token_ID range <>) of Names_Array_Access;
   type Names_Array_Array_Access is access Names_Array_Array;

end WisiToken;
