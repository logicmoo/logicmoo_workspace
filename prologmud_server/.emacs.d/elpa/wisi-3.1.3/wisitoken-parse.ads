--  Abstract :
--
--  Subprograms common to more than one parser, higher-level than in wisitoken.ads
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
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package WisiToken.Parse is

   type Base_Parser is abstract new Ada.Finalization.Limited_Controlled with record
      Trace     : access WisiToken.Trace'Class;
      Lexer     : WisiToken.Lexer.Handle;
      User_Data : WisiToken.Syntax_Trees.User_Data_Access;
      Terminals : aliased WisiToken.Base_Token_Arrays.Vector;

      Line_Begin_Token : aliased WisiToken.Line_Begin_Token_Vectors.Vector;
      --  Line_Begin_Token (I) is the index into Terminals of the first
      --  grammar token on line I. Line_Begin_Token.First_Index is the first
      --  line containing a grammar token (after leading comments). However,
      --  if the only token on line I is a non_grammar token (ie a comment,
      --  or a newline for a blank line), Line_Begin_Token (I) is the last
      --  grammar token on the previous non-blank line. If Line (I) is a
      --  non-first line in a multi-line terminal token, Line_Begin_Token
      --  (I) is Invalid_Token_Index.
   end record;
   --  Common to all parsers. Finalize should free any allocated objects.

   function Next_Grammar_Token (Parser : in out Base_Parser'Class) return Token_ID;
   --  Get next token from Lexer, call User_Data.Lexer_To_Augmented. If
   --  it is a grammar token, store in Terminals and return its id.
   --  Otherwise, repeat.
   --
   --  Propagates Fatal_Error from Lexer.

   procedure Lex_All (Parser : in out Base_Parser'Class);
   --  Clear Terminals, Line_Begin_Token; reset User_Data. Then call
   --  Next_Grammar_Token repeatedly until EOF_ID is returned.
   --
   --  The user must first call Lexer.Reset_* to set the input text.

   procedure Parse (Parser : aliased in out Base_Parser) is abstract;
   --  Call Lex_All, then execute parse algorithm to parse the tokens,
   --  storing the result in Parser for Execute_Actions.
   --
   --  If a parse error is encountered, raises Syntax_Error.
   --  Parser.Lexer_Errors and Parser contain information about the
   --  errors.
   --
   --  For other errors, raises Parse_Error with an appropriate error
   --  message.

   function Tree (Parser : in Base_Parser) return Syntax_Trees.Tree is abstract;
   --  Return the syntax tree resulting from the parse.

   function Tree_Var_Ref (Parser : aliased in out Base_Parser) return Syntax_Trees.Tree_Variable_Reference is abstract;
   --  Return a writable reference to the syntax tree resulting from the parse.

   function Any_Errors (Parser : in Base_Parser) return Boolean is abstract;

   procedure Put_Errors (Parser : in Base_Parser) is abstract;
   --  Output error messages to Ada.Text_IO.Current_Error.

   procedure Execute_Actions
     (Parser          : in out Base_Parser;
      Image_Augmented : in     Syntax_Trees.Image_Augmented := null)
     is abstract;
   --  Execute all actions in Parser.Tree.

end WisiToken.Parse;
