--  Abstract :
--
--  Runtime utils for wisi_grammar.wy actions.
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
with WisiToken.BNF;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
with Wisitoken_Grammar_Actions;
with WisiToken.Syntax_Trees.LR_Utils;
package WisiToken_Grammar_Runtime is

   type Meta_Syntax is (Unknown, BNF_Syntax, EBNF_Syntax);
   --  Syntax used in grammar file.

   type Action_Phase is (Meta, Other);

   type User_Data_Type is new WisiToken.Syntax_Trees.User_Data_Type with
   record
      Grammar_Lexer : WisiToken.Lexer.Handle; -- used to read the .wy file now.

      User_Lexer : WisiToken.BNF.Lexer_Type := WisiToken.BNF.None;
      --  Used to read the user language file, after user parser is generated;
      --  used now in '%if lexer' statements.

      User_Parser : WisiToken.BNF.Generate_Algorithm := WisiToken.BNF.None;
      --  Used to generate the user parser; used now in '%if parser'
      --  statements.

      Generate_Set : WisiToken.BNF.Generate_Set_Access;
      --  As specified by %generate directives or command line.

      Phase : Action_Phase := Meta;
      --  Determines which actions Execute_Actions executes:
      --  Meta  - meta declarations, like %meta_syntax, %generate
      --  Other - everything else

      Meta_Syntax      : WisiToken_Grammar_Runtime.Meta_Syntax := Unknown;
      Terminals        : WisiToken.Base_Token_Array_Access_Constant;
      Raw_Code         : WisiToken.BNF.Raw_Code;
      Language_Params  : WisiToken.BNF.Language_Param_Type;
      Tokens           : aliased WisiToken.BNF.Tokens;
      Conflicts        : WisiToken.BNF.Conflict_Lists.List;
      McKenzie_Recover : WisiToken.BNF.McKenzie_Recover_Param_Type;

      Leading_Non_Grammar : WisiToken.Base_Token_Arrays.Vector;
      --  leading blank lines and comments

      Last_Terminal_Node : WisiToken.Node_Index := WisiToken.Invalid_Node_Index;

      Rule_Count   : Integer                   := 0;
      Action_Count : Integer                   := 0;
      Check_Count  : Integer                   := 0;
      Label_Count  : Ada.Containers.Count_Type := 0;

      EBNF_Nodes : WisiToken.Syntax_Trees.Node_Sets.Vector;

      If_Lexer_Present  : Boolean := False;
      If_Parser_Present : Boolean := False;
      --  Set True by %if statements in Execute_Actions.

      Ignore_Lines : Boolean := False;
      --  An '%if' specified a different lexer, during Execute_Actions
   end record;

   type Augmented_Token is new WisiToken.Base_Token with
   record
      Non_Grammar : WisiToken.Base_Token_Arrays.Vector;
   end record;
   type Augmented_Token_Access is access all Augmented_Token;

   function Image (Item : in WisiToken.Base_Token_Class_Access) return String
     is (WisiToken.Image (Augmented_Token_Access (Item).Non_Grammar, Wisitoken_Grammar_Actions.Descriptor));

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     WisiToken.Base_Token_Array_Access_Constant);

   overriding procedure Reset (Data : in out User_Data_Type);

   overriding
   procedure Initialize_Actions
     (Data : in out User_Data_Type;
      Tree : in WisiToken.Syntax_Trees.Tree'Class);

   overriding
   procedure Lexer_To_Augmented
     (Data  : in out          User_Data_Type;
      Tree  : in out          WisiToken.Syntax_Trees.Tree'Class;
      Token : in              WisiToken.Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class);

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array);

   procedure End_If (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class);

   procedure Add_Declaration
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array);

   procedure Add_Nonterminal
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array);

   function Image_Grammar_Action (Action : in WisiToken.Syntax_Trees.Semantic_Action) return String;
   --  For Syntax_Trees.Print_Tree.

   procedure Check_EBNF
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array;
      Token     : in     WisiToken.Positive_Index_Type);

   procedure Raise_Programmer_Error
     (Label : in String;
      Data  : in User_Data_Type;
      Tree  : in WisiToken.Syntax_Trees.Tree;
      Node  : in WisiToken.Node_Index);
   pragma No_Return (Raise_Programmer_Error);

   function Find_Declaration
     (Data : in     User_Data_Type;
      Tree : in out WisiToken.Syntax_Trees.Tree;
      Name : in     String)
     return WisiToken.Node_Index;
   --  Return the node that declares Name, Invalid_Node_Index if none.
   --  The node is either a declaration or a nonterminal.

   procedure Translate_EBNF_To_BNF
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Data : in out User_Data_Type);
   --  Process EBNF nonterms, adding new nonterms as needed, resulting in
   --  a BNF tree.
   --
   --  Generator.LR.*_Generate requires a BNF grammar.

   procedure Print_Source
     (File_Name : in String;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Data      : in User_Data_Type);
   --  Print the wisitoken grammar source represented by Tree, Terminals
   --  to a new file File_Name.

end WisiToken_Grammar_Runtime;
