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

package body WisiToken.Parse.Packrat.Generated is

   overriding procedure Parse (Parser : aliased in out Generated.Parser)
   is
      --  'aliased' required for Base_Tree'Access. WORKAROUND: that was
      --  enough when Parser type was declared in generated Main; now that
      --  it's a derived type, it doesn't work. So we use Unchecked_Access.

      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      Junk : WisiToken.Valid_Node_Index;
      pragma Unreferenced (Junk);

      Result : Memo_Entry;
   begin
      Parser.Base_Tree.Clear;
      Parser.Tree.Initialize (Parser.Base_Tree'Unchecked_Access, Flush => True);
      Parser.Lex_All;
      Parser.Derivs.Set_First_Last (Descriptor.First_Nonterminal, Descriptor.Last_Nonterminal);

      for Nonterm in Descriptor.First_Nonterminal .. Parser.Trace.Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear;
         Parser.Derivs (Nonterm).Set_First_Last (Parser.Terminals.First_Index, Parser.Terminals.Last_Index);
      end loop;

      for Token_Index in Parser.Terminals.First_Index .. Parser.Terminals.Last_Index loop
         Junk := Parser.Tree.Add_Terminal (Token_Index, Parser.Terminals);
         --  FIXME: move this into Lex_All, delete Terminals, just use Syntax_Tree
      end loop;

      Result := Parser.Parse_WisiToken_Accept (Parser, Parser.Terminals.First_Index - 1);

      if Result.State /= Success then
         if Trace_Parse > Outline then
            Parser.Trace.Put_Line ("parse failed");
         end if;

         raise Syntax_Error with "parse failed"; --  FIXME: need better error message!
      else
         Parser.Tree.Set_Root (Result.Result);
      end if;

   end Parse;

   overriding function Tree (Parser : in Generated.Parser) return Syntax_Trees.Tree
   is begin
      return Parser.Tree;
   end Tree;

   overriding function Tree_Var_Ref
     (Parser : aliased in out Generated.Parser)
     return Syntax_Trees.Tree_Variable_Reference
   is begin
      return (Element => Parser.Tree'Access);
   end Tree_Var_Ref;

   overriding function Any_Errors (Parser : in Generated.Parser) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      return Parser.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in Generated.Parser)
   is
      use Ada.Text_IO;
   begin
      for Item of Parser.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Parser.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      --  FIXME: Packrat parser does not report errors yet.
   end Put_Errors;

end WisiToken.Parse.Packrat.Generated;
