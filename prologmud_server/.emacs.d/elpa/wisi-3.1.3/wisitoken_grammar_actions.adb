--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c wisitoken_grammar.wy
--

--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with WisiToken_Grammar_Runtime; use WisiToken_Grammar_Runtime;

package body Wisitoken_Grammar_Actions is

   procedure declaration_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Declaration (User_Data, Tree, Tokens);
   end declaration_0;

   procedure declaration_1
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Declaration (User_Data, Tree, Tokens);
   end declaration_1;

   procedure declaration_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Declaration (User_Data, Tree, Tokens);
   end declaration_2;

   procedure declaration_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Declaration (User_Data, Tree, Tokens);
   end declaration_3;

   procedure declaration_4
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Start_If (User_Data, Tree, Tokens);
   end declaration_4;

   procedure declaration_5
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Tree, Nonterm, Tokens);
   begin
      End_If (User_Data);
   end declaration_5;

   procedure nonterminal_0
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Nonterminal (User_Data, Tree, Tokens);
   end nonterminal_0;

   procedure nonterminal_1
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Add_Nonterminal (User_Data, Tree, Tokens);
   end nonterminal_1;

   procedure rhs_item_1
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Check_EBNF (User_Data, Tree, Tokens, 1);
   end rhs_item_1;

   procedure rhs_item_2
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Check_EBNF (User_Data, Tree, Tokens, 1);
   end rhs_item_2;

   procedure rhs_item_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Check_EBNF (User_Data, Tree, Tokens, 1);
   end rhs_item_3;

   procedure rhs_item_4
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Check_EBNF (User_Data, Tree, Tokens, 1);
   end rhs_item_4;

   procedure rhs_item_5
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Check_EBNF (User_Data, Tree, Tokens, 1);
   end rhs_item_5;

   procedure rhs_optional_item_3
    (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
     Tree      : in out WisiToken.Syntax_Trees.Tree;
     Nonterm   : in     WisiToken.Valid_Node_Index;
     Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      Check_EBNF (User_Data, Tree, Tokens, 1);
   end rhs_optional_item_3;

end Wisitoken_Grammar_Actions;
