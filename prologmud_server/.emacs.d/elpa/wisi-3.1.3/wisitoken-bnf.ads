--  Abstract :
--
--  Root package for generating a parser from a BNF source file; see [2]
--
--  The input file syntax is based on BNF syntax [1] with declarations
--  and grammar actions.
--
--  The Elisp and Ada_Emacs output languages are for use with the
--  Emacs wisi package.
--
--  Reference :
--
--  [1] https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
--  [2] http://www.nongnu.org/ada-mode/wisi/wisi-user_guide.html, (info "(wisi-user_guide)Top")
--
--  Copyright (C) 2012 - 2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with WisiToken.Parse.LR;
package WisiToken.BNF is

   --  See also WisiToken exceptions

   Not_Found : exception;
   --  something not found; should be handled and converted to Syntax_ or Grammar_Error

   type Generate_Algorithm is (None, LALR, LR1, Packrat_Gen, Packrat_Proc, External);
   subtype Valid_Generate_Algorithm is Generate_Algorithm range LALR .. Generate_Algorithm'Last;
   subtype LR_Generate_Algorithm is Generate_Algorithm range LALR .. LR1;
   subtype Packrat_Generate_Algorithm is Generate_Algorithm range Packrat_Gen .. Packrat_Proc;

   Generate_Algorithm_Image : constant array (Generate_Algorithm) of String_Access_Constant :=
     (None         => new String'("None"),
      LALR         => new String'("LALR"),
      LR1          => new String'("LR1"),
      Packrat_Gen  => new String'("Packrat_Gen"),
      Packrat_Proc => new String'("Packrat_Proc"),
      External     => new String'("External"));
   --  Suitable for Ada package names.

   function To_Generate_Algorithm (Item : in String) return Generate_Algorithm;
   --  Raises User_Error for invalid Item

   type Generate_Algorithm_Set is array (Generate_Algorithm) of Boolean;
   type Generate_Algorithm_Set_Access is access Generate_Algorithm_Set;

   type Output_Language is (Ada_Lang, Ada_Emacs_Lang);
   subtype Ada_Output_Language is Output_Language range Ada_Lang .. Ada_Emacs_Lang;
   --  _Lang to avoid colliding with the standard package Ada and
   --  WisiToken packages named *.Ada. In the grammar file, they
   --  are named by (case insensitive):
   Output_Language_Image : constant array (Output_Language) of String_Access_Constant :=
     (Ada_Lang       => new String'("Ada"),
      Ada_Emacs_Lang => new String'("Ada_Emacs"));

   function To_Output_Language (Item : in String) return Output_Language;
   --  Raises User_Error for invalid Item

   type Lexer_Type is (None, Elisp_Lexer, re2c_Lexer);
   subtype Valid_Lexer is Lexer_Type range Elisp_Lexer .. Lexer_Type'Last;
   --  We append "_Lexer" to these names to avoid colliding with the
   --  similarly-named WisiToken packages. In the grammar file, they
   --  are named by:
   Lexer_Image : constant array (Lexer_Type) of String_Access_Constant :=
     (None        => new String'("none"),
      Elisp_Lexer => new String'("elisp"),
      re2c_Lexer  => new String'("re2c"));

   function To_Lexer (Item : in String) return Lexer_Type;
   --  Raises User_Error for invalid Item

   type Lexer_Set is array (Lexer_Type) of Boolean;

   type Lexer_Generate_Algorithm_Set is array (Lexer_Type) of Generate_Algorithm_Set;
   --  %if lexer change change the generated parse table

   type Interface_Type is (None, Process, Module);
   subtype Valid_Interface is Interface_Type range Process .. Module;

   type Generate_Tuple is record
      Gen_Alg        : Generate_Algorithm := None;
      Out_Lang       : Output_Language    := Ada_Lang;
      Lexer          : Lexer_Type         := None;
      Interface_Kind : Interface_Type     := None;
      Text_Rep       : Boolean            := False;
   end record;

   type Generate_Set is array (Natural range <>) of Generate_Tuple;
   type Generate_Set_Access is access Generate_Set;
   procedure Free is new Ada.Unchecked_Deallocation (Generate_Set, Generate_Set_Access);

   procedure Add
     (Set   : in out Generate_Set_Access;
      Tuple : in     Generate_Tuple);

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package String_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (WisiToken.Identifier_Index, Ada.Strings.Unbounded.Unbounded_String,
      Default_Element => Ada.Strings.Unbounded.Null_Unbounded_String);

   type Language_Param_Type is record
      --  Set by grammar file declarations or command line options. Error
      --  recover parameters are in McKenzie_Recover_Param_Type below.
      Case_Insensitive          : Boolean := False;
      End_Names_Optional_Option : Ada.Strings.Unbounded.Unbounded_String;
      Use_Language_Runtime      : Boolean := True;
      Language_Runtime_Name     : Ada.Strings.Unbounded.Unbounded_String;
      Declare_Enums             : Boolean := True;
      Error_Recover             : Boolean := False;
      Start_Token               : Ada.Strings.Unbounded.Unbounded_String;
      Partial_Recursion         : Boolean := False;
   end record;

   type Raw_Code_Location is
     (Copyright_License,
      Actions_Spec_Context, Actions_Spec_Pre, Actions_Spec_Post,
      Actions_Body_Context, Actions_Body_Pre, Actions_Body_Post);
   --  So far we have not needed raw code other than license in the main
   --  package.

   type Raw_Code is array (Raw_Code_Location) of String_Lists.List;

   subtype String_2 is String (1 .. 2);

   Ada_Comment   : constant String_2 := "--";
   C_Comment     : constant String_2 := "//";
   Elisp_Comment : constant String_2 := ";;";

   function Split_Lines (Item : in String) return String_Lists.List;

   function Trim (Item : in String_Lists.List; Comment_Start : in String) return String_Lists.List;
   --  From each element, delete trailing comments starting with
   --  Comment_Start; delete leading and trailing spaces.

   procedure Put_Raw_Code
     (Comment_Syntax : in String_2;
      Code           : in String_Lists.List;
      Comment_Only   : in Boolean := False);
   --  Output Code to Ada.Text_IO.Current_Output.
   --
   --  If first two characters of a line are the same and not ' ', it is
   --  assumed to be a comment; ensure the output line has
   --  Comment_Syntax.
   --
   --  If Comment_Only is True, or if the comment syntax used in Code
   --  does not equal Comment_Syntax, only output comment lines.
   --
   --  If Comment_Syntax is Elisp_Comment, only output lines that are
   --  valid elisp comments or forms (ie start with ';;' or '(').
   --
   --  Otherwise output all lines.

   procedure Put_File_Header
     (Comment_Syntax : in String_2;
      Emacs_Mode     : in String         := "";
      Use_Tuple      : in Boolean        := False;
      Tuple          : in Generate_Tuple := (others => <>));
   --  Output "parser support file <emacs_mode> /n command line: " comment to Ada.Text_IO.Current_Output.

   type String_Pair_Type is record
      Name  : aliased Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package String_Pair_Lists is new Ada.Containers.Doubly_Linked_Lists (String_Pair_Type);
   function Is_Present (List : in String_Pair_Lists.List; Name : in String) return Boolean;
   function Value (List : in String_Pair_Lists.List; Name : in String) return String;

   type String_Triple_Type is record
      Name         : aliased Ada.Strings.Unbounded.Unbounded_String;
      Value        : Ada.Strings.Unbounded.Unbounded_String;
      Repair_Image : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package String_Triple_Lists is new Ada.Containers.Doubly_Linked_Lists (String_Triple_Type);

   type Elisp_Action_Type is record
      --  Elisp name is the key
      Action_Label : Ada.Strings.Unbounded.Unbounded_String;
      Ada_Name     : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Elisp_Action_Maps is new Ada.Containers.Ordered_Maps
     (Ada.Strings.Unbounded.Unbounded_String, Elisp_Action_Type, Ada.Strings.Unbounded."<");

   function Is_Present (List : in Elisp_Action_Maps.Map; Name : in String) return Boolean;

   type McKenzie_Recover_Param_Type is record
      Source_Line : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
      --  Of the %mckenzie_cost_default declaration; we assume the others
      --  are near.

      Default_Insert                        : Natural               := 0;
      Default_Delete_Terminal               : Natural               := 0;
      Default_Push_Back                     : Natural               := 0; -- also default for undo_reduce
      Delete                                : String_Pair_Lists.List;
      Insert                                : String_Pair_Lists.List;
      Push_Back                             : String_Pair_Lists.List;
      Undo_Reduce                           : String_Pair_Lists.List;
      Minimal_Complete_Cost_Delta           : Integer               :=
        WisiToken.Parse.LR.Default_McKenzie_Param.Minimal_Complete_Cost_Delta;
      Fast_Forward                          : Integer               :=
        WisiToken.Parse.LR.Default_McKenzie_Param.Fast_Forward;
      Matching_Begin                        : Integer               :=
        WisiToken.Parse.LR.Default_McKenzie_Param.Matching_Begin;
      Ignore_Check_Fail                     : Natural               :=
        WisiToken.Parse.LR.Default_McKenzie_Param.Ignore_Check_Fail;
      Check_Limit                           : WisiToken.Token_Index :=
        WisiToken.Parse.LR.Default_McKenzie_Param.Check_Limit;
      Check_Delta_Limit                     : Natural               :=
        WisiToken.Parse.LR.Default_McKenzie_Param.Check_Delta_Limit;
      Enqueue_Limit                         : Natural               :=
        WisiToken.Parse.LR.Default_McKenzie_Param.Enqueue_Limit;
   end record;

   type Token_Kind_Type is record
      Kind   : Ada.Strings.Unbounded.Unbounded_String;
      Tokens : String_Triple_Lists.List;
   end record;

   package Token_Lists is new Ada.Containers.Doubly_Linked_Lists (Token_Kind_Type);

   function Count (Tokens : in Token_Lists.List) return Integer;
   --  Count of all leaves.

   procedure Add_Token
     (Tokens       : in out Token_Lists.List;
      Kind         : in     String;
      Name         : in     String;
      Value        : in     String;
      Repair_Image : in     String := "");
   --  Add Name, Value, Repair_Image to Kind list in Tokens.

   function Is_In (Tokens : in Token_Lists.List; Kind : in String) return Boolean;
   function Is_In
     (Tokens : in Token_Lists.List;
      Kind   : in String;
      Value  : in String)
     return Boolean;

   type Conflict is record
      Source_Line : WisiToken.Line_Number_Type;
      Action_A    : Ada.Strings.Unbounded.Unbounded_String;
      LHS_A       : Ada.Strings.Unbounded.Unbounded_String;
      Action_B    : Ada.Strings.Unbounded.Unbounded_String;
      LHS_B       : Ada.Strings.Unbounded.Unbounded_String;
      On          : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Conflict_Lists is new Ada.Containers.Doubly_Linked_Lists (Conflict);

   type Labeled_Token is record
      Label      : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Labeled_Token_Arrays is new Ada.Containers.Vectors (Positive_Index_Type, Labeled_Token);
   --  Index matches Syntax_Trees.Valid_Node_Index_Array, used for Tokens
   --  in call to post parse grammar action.

   type RHS_Type is record
      Tokens      : Labeled_Token_Arrays.Vector;
      Action      : Ada.Strings.Unbounded.Unbounded_String;
      Check       : Ada.Strings.Unbounded.Unbounded_String;
      Source_Line : WisiToken.Line_Number_Type := WisiToken.Invalid_Line_Number;
   end record;
   package RHS_Lists is new Ada.Containers.Doubly_Linked_Lists (RHS_Type, "=");

   type Rule_Type is record
      Left_Hand_Side   : aliased Ada.Strings.Unbounded.Unbounded_String;
      Right_Hand_Sides : RHS_Lists.List;
      Labels           : String_Arrays.Vector;
      Source_Line      : WisiToken.Line_Number_Type;
   end record;

   package Rule_Lists is new Ada.Containers.Doubly_Linked_Lists (Rule_Type);

   function Is_Present (Rules : in Rule_Lists.List; LHS : in String) return Boolean;

   type Tokens is record
      Non_Grammar : Token_Lists.List;
      Keywords    : String_Pair_Lists.List;
      Tokens      : Token_Lists.List;
      Rules       : Rule_Lists.List;
      --  Rules included here because they define the nonterminal tokens, as
      --  well as the productions.

      Virtual_Identifiers : String_Arrays.Vector;
      --  Nonterminals and terminals introduced by translating from EBNF to
      --  BNF.

      --  The following are specified in grammar file declarations and used
      --  in other declarations or actions. Faces, Indents only used if .wy
      --  action language is elisp and output language is not elisp.

      re2c_Regexps : String_Pair_Lists.List; -- %re2c_regexp
      Faces        : String_Lists.List;      -- %elisp_face
      Indents      : String_Pair_Lists.List; -- %elisp_indent
      Actions      : Elisp_Action_Maps.Map;  -- %elisp_action
   end record;

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   function To_Lower (Item : in String) return String
     renames Ada.Characters.Handling.To_Lower;

   function To_Upper (Item : in String) return String
     renames Ada.Characters.Handling.To_Upper;

   function To_Upper (Item : in Character) return Character
     renames Ada.Characters.Handling.To_Upper;

   function "+" (List : in String_Lists.List; Item : in String) return String_Lists.List;

   function String_To_String_List (Item : in String) return String_Lists.List;
   function "+" (Item : in String) return String_Lists.List renames String_To_String_List;

   function RHS_To_RHS_List (Item : in RHS_Type) return RHS_Lists.List;
   function "+" (Item : in RHS_Type) return RHS_Lists.List renames RHS_To_RHS_List;

   function "+" (List : in RHS_Lists.List; Item : in RHS_Type) return RHS_Lists.List;

   function Image (Item : in Boolean) return String
     is (if Item then "True" else "False");
   --  Match casing in Standard.

   procedure Put_Command_Line
     (Comment_Prefix : in String;
      Use_Tuple      : in Boolean        := False;
      Tuple          : in Generate_Tuple := (others => <>));
   --  Put command line to current output; indicate current tuple.

end WisiToken.BNF;
