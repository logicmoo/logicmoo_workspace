--  Abstract :
--
--  A generalized LR parser.
--
--  In a child package of Parser.LR partly for historical reasons,
--  partly to allow McKenzie_Recover to be in a sibling package.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Lexer;
with WisiToken.Parse;
with WisiToken.Syntax_Trees;
package WisiToken.Parse.LR.Parser is

   Default_Max_Parallel : constant := 15;

   type Language_Fixes_Access is access procedure
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : access constant WisiToken.Lexer.Instance'Class;
      Parser_Label      : in     Natural;
      Parse_Table       : in     WisiToken.Parse.LR.Parse_Table;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration);
   --  Config encountered a parse table Error action, or failed a
   --  semantic check; attempt to provide a language-specific fix,
   --  enqueuing new configs on Local_Config_Heap.
   --
   --  For a failed semantic check, Config.Stack is in the pre-reduce
   --  state, Config.Error_Token gives the nonterm token,
   --  Config.Check_Token_Count the token count for the reduce. May be
   --  called with Nonterm.Virtual = True or Tree.Valid_Indices (stack
   --  top token_count items) false.
   --
   --  For an Error action, Config.Error_Token gives the terminal that
   --  caused the error.

   type Language_Matching_Begin_Tokens_Access is access procedure
     (Tokens                  : in     Token_ID_Array_1_3;
      Config                  : in     Configuration;
      Matching_Tokens         :    out Token_ID_Arrays.Vector;
      Forbid_Minimal_Complete :    out Boolean);
   --  Tokens (1) caused a parse error; Tokens (2 .. 3) are the following
   --  tokens (Invalid_Token_ID if none). Set Matching_Tokens to a token
   --  sequence that starts a production matching Tokens. If
   --  Minimal_Complete would produce a bad solution at this error point,
   --  set Forbid_Minimal_Complete True.
   --
   --  For example, if Tokens is a block end, return tokens that are the
   --  corresponding block begin. If the error point is inside a
   --  multi-token 'end' (ie 'end if;', or 'end <name>;'), set
   --  Forbid_Minimal_Complete True.

   type Language_String_ID_Set_Access is access function
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;
   --  Return a Token_ID_Set containing String_Literal_ID and
   --  nonterminals that can contain String_Literal_ID as part of an
   --  expression. Used in placing a missing string quote.

   type Post_Recover_Access is access procedure;

   type Parser is new WisiToken.Parse.Base_Parser with record
      Table                          : Parse_Table_Ptr;
      Language_Fixes                 : Language_Fixes_Access;
      Language_Matching_Begin_Tokens : Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : Language_String_ID_Set_Access;

      String_Quote_Checked : Line_Number_Type := Invalid_Line_Number;
      --  Max line checked for missing string quote.

      Post_Recover : Post_Recover_Access;
      --  Gather data for tests.

      Shared_Tree : aliased Syntax_Trees.Base_Tree;
      --  Each parser (normal and recover) has its own branched syntax tree,
      --  all branched from this tree. Terminals are added to the tree when
      --  they become the current token.
      --
      --  It is never the case that terminals are added to this shared tree
      --  when there is more than one task active, so we don't need a
      --  protected tree.
      --
      --  See WisiToken.LR.Parser_Lists Parser_State for more discussion of
      --  Shared_Tree.

      Parsers : aliased Parser_Lists.List;

      Max_Parallel            : SAL.Base_Peek_Type;
      Terminate_Same_State    : Boolean;
      Enable_McKenzie_Recover : Boolean;
      Recover_Log_File        : Ada.Text_IO.File_Type;
      Partial_Parse_Active    : Boolean := False;
      --  Partial_Parse_Active is only used in recover log messages.
   end record;

   overriding procedure Finalize (Object : in out LR.Parser.Parser);
   --  Deep free Object.Table.

   procedure New_Parser
     (Parser                         :    out          LR.Parser.Parser;
      Trace                          : not null access WisiToken.Trace'Class;
      Lexer                          : in              WisiToken.Lexer.Handle;
      Table                          : in              Parse_Table_Ptr;
      Language_Fixes                 : in              Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in              Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in              Language_String_ID_Set_Access;
      User_Data                      : in              WisiToken.Syntax_Trees.User_Data_Access;
      Max_Parallel                   : in              SAL.Base_Peek_Type := Default_Max_Parallel;
      Terminate_Same_State           : in              Boolean            := True);

   overriding procedure Parse (Shared_Parser : aliased in out LR.Parser.Parser);
   --  Attempt a parse. Calls Parser.Lexer.Reset, runs lexer to end of
   --  input setting Shared_Parser.Terminals, then parses tokens.
   --
   --  If an error is encountered, Parser.Lexer_Errors and
   --  Parsers(*).Errors contain information about the errors. If a
   --  recover strategy succeeds, no exception is raised. If recover does
   --  not succeed, raises Syntax_Error.
   --
   --  For errors where no recovery is possible, raises Parse_Error with
   --  an appropriate error message.

   overriding function Tree (Shared_Parser : in Parser) return Syntax_Trees.Tree;
   overriding function Tree_Var_Ref (Shared_Parser : aliased in out Parser) return Syntax_Trees.Tree_Variable_Reference;
   --  If there is one parser in Parsers, return its tree. Otherwise,
   --  raise Parse_Error for an ambiguous parse.

   overriding procedure Execute_Actions
     (Parser          : in out LR.Parser.Parser;
      Image_Augmented : in     Syntax_Trees.Image_Augmented := null);
   --  Call User_Data.Delete_Token on any tokens deleted by error
   --  recovery, then User_Data.Reduce and the grammar semantic actions
   --  on all nonterms in the syntax tree.

   overriding function Any_Errors (Parser : in LR.Parser.Parser) return Boolean;
   --  Return True if any errors where encountered, recovered or not.

   overriding procedure Put_Errors (Parser : in LR.Parser.Parser);
   --  Put user-friendly error messages from the parse to
   --  Ada.Text_IO.Current_Error.

end WisiToken.Parse.LR.Parser;
