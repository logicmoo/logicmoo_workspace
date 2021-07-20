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

package body WisiToken.Parse is

   function Next_Grammar_Token (Parser : in out Base_Parser'Class) return Token_ID
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.User_Data_Access;

      Token : Base_Token;
      Error : Boolean;
   begin
      loop
         Error := Parser.Lexer.Find_Next (Token);

         --  We don't handle Error until later; we assume it was recovered.

         if Token.Line /= Invalid_Line_Number then
            --  Some lexers don't support line numbers.
            if Parser.Lexer.First then
               if Parser.Line_Begin_Token.Length = 0 then
                  Parser.Line_Begin_Token.Set_First_Last (Token.Line, Token.Line);
               else
                  Parser.Line_Begin_Token.Set_First_Last (Parser.Line_Begin_Token.First_Index, Token.Line);
               end if;
               Parser.Line_Begin_Token (Token.Line) := Parser.Terminals.Last_Index +
                 (if Token.ID >= Parser.Trace.Descriptor.First_Terminal then 1 else 0);

            elsif Token.ID = Parser.Trace.Descriptor.EOI_ID then
               Parser.Line_Begin_Token.Set_First_Last (Parser.Line_Begin_Token.First_Index, Token.Line + 1);
               Parser.Line_Begin_Token (Token.Line + 1) := Parser.Terminals.Last_Index + 1;
            end if;
         end if;

         if Trace_Parse > Lexer_Debug then
            Parser.Trace.Put_Line (Image (Token, Parser.Trace.Descriptor.all));
         end if;

         if Token.ID >= Parser.Trace.Descriptor.First_Terminal then

            Parser.Terminals.Append (Token);

            --  We create the syntax tree node here, so Lexer_To_Augmented can
            --  store augmented data in it.
            Parser.Terminals (Parser.Terminals.Last_Index).Tree_Index := Parser.Tree_Var_Ref.Add_Terminal
              (Parser.Terminals.Last_Index, Parser.Terminals);

            if Parser.User_Data /= null then
               Parser.User_Data.Lexer_To_Augmented
                 (Parser.Tree_Var_Ref, Parser.Terminals (Parser.Terminals.Last_Index), Parser.Lexer);
            end if;

            exit;
         else
            --  non-grammar; not in syntax tree
            if Parser.User_Data /= null then
               Parser.User_Data.Lexer_To_Augmented (Parser.Tree_Var_Ref, Token, Parser.Lexer);
            end if;
         end if;
      end loop;

      if Error then
         declare
            Error : WisiToken.Lexer.Error renames Parser.Lexer.Errors.Reference (Parser.Lexer.Errors.Last);
         begin
            if Error.Recover_Char (1) /= ASCII.NUL then
               Error.Recover_Token := Parser.Terminals.Last_Index;
            end if;
         end;
      end if;

      return Token.ID;
   end Next_Grammar_Token;

   procedure Lex_All (Parser : in out Base_Parser'Class)
   is
      EOF_ID : constant Token_ID := Parser.Trace.Descriptor.EOI_ID;
   begin
      Parser.Lexer.Errors.Clear;
      Parser.Terminals.Clear;
      Parser.Line_Begin_Token.Clear;
      loop
         exit when EOF_ID = Next_Grammar_Token (Parser);
      end loop;
      if Trace_Parse > Outline then
         Parser.Trace.Put_Line (Token_Index'Image (Parser.Terminals.Last_Index) & " tokens lexed");
      end if;

   end Lex_All;

end WisiToken.Parse;
