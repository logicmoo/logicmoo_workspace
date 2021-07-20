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

package body WisiToken.Parse.Packrat.Procedural is

   function Apply_Rule
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Base_Token_Index)
     return Memo_Entry
   with Post => Apply_Rule'Result.State in Failure .. Success;

   function Eval
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Base_Token_Index)
     return Memo_Entry
   with Post => Eval'Result.State in Failure .. Success;

   ----------
   --  bodies

   function Eval
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Base_Token_Index)
     return Memo_Entry
   is
      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;

      Pos : Base_Token_Index := Last_Pos; --  last token parsed.
   begin
      for RHS_Index in Parser.Grammar (R).RHSs.First_Index .. Parser.Grammar (R).RHSs.Last_Index loop
         declare
            use all type Ada.Containers.Count_Type;
            RHS  : WisiToken.Productions.Right_Hand_Side renames Parser.Grammar (R).RHSs (RHS_Index);
            Memo : Memo_Entry; --  for temporary or intermediate results
         begin
            if RHS.Tokens.Length = 0 then
               return
                 (State              => Success,
                  Result             => Parser.Tree.Add_Nonterm
                    (Production      => (R, RHS_Index),
                     Action          => RHS.Action,
                     Children        => (1 .. 0 => Invalid_Node_Index),
                     Default_Virtual => False),
                  Last_Pos           => Pos);
            else
               declare
                  Children : Valid_Node_Index_Array
                    (SAL.Base_Peek_Type (RHS.Tokens.First_Index) .. SAL.Base_Peek_Type (RHS.Tokens.Last_Index));
               begin
                  for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                     if RHS.Tokens (I) in Terminal then
                        if Pos = Parser.Terminals.Last_Index then
                           goto Fail_RHS;

                        elsif Parser.Terminals (Pos + 1).ID = RHS.Tokens (I) then
                           Pos := Pos + 1;
                           Children (SAL.Base_Peek_Type (I)) := Tree_Index (Pos);
                        else
                           goto Fail_RHS;
                        end if;
                     else
                        Memo := Apply_Rule (Parser, RHS.Tokens (I), Pos);
                        case Memo.State is
                        when Success =>
                           Children (SAL.Base_Peek_Type (I)) := Memo.Result;
                           Pos := Memo.Last_Pos;

                        when Failure =>
                           goto Fail_RHS;
                        when No_Result =>
                           raise SAL.Programmer_Error;
                        end case;
                     end if;
                  end loop;

                  return
                    (State              => Success,
                     Result             => Parser.Tree.Add_Nonterm
                       (Production      => (R, RHS_Index),
                        Action          => RHS.Action,
                        Children        => Children,
                        Default_Virtual => False),
                     Last_Pos           => Pos);

                  <<Fail_RHS>>
                  Pos := Last_Pos;
               end;
            end if;
         end;
      end loop;
      --  get here when all RHSs fail

      return (State => Failure);
   end Eval;

   function Apply_Rule
     (Parser   : in out Procedural.Parser;
      R        : in     Token_ID;
      Last_Pos : in     Base_Token_Index)
     return Memo_Entry
   is
      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      Pos       : Base_Token_Index     := Last_Pos;     --  last token parsed.
      Start_Pos : constant Token_Index := Last_Pos + 1; --  first token in current nonterm
      Memo      : Memo_Entry           := Parser.Derivs (R)(Start_Pos);

      Pos_Recurse_Last : Base_Token_Index := Last_Pos;
      Result_Recurse   : Memo_Entry;
   begin
      case Memo.State is
      when Success =>
         return Memo;

      when Failure =>
         return (State => Failure);

      when No_Result =>
         if Parser.Direct_Left_Recursive (R) then
            Parser.Derivs (R).Replace_Element (Start_Pos, (State => Failure));
         else
            Memo := Eval (Parser, R, Last_Pos);
            if (Trace_Parse > Detail and Memo.State = Success) or Trace_Parse > Extra then
               case Memo.State is
               when Success =>
                  Parser.Trace.Put_Line (Parser.Tree.Image (Memo.Result, Descriptor, Include_Children => True));
               when Failure =>
                  Parser.Trace.Put_Line (Image (R, Descriptor) & " failed at pos" & Last_Pos'Image);
               when No_Result =>
                  raise SAL.Programmer_Error;
               end case;
            end if;
            Parser.Derivs (R).Replace_Element (Start_Pos, Memo);
            return Memo;
         end if;
      end case;

      loop
         Pos := Last_Pos;

         if Pos > Parser.Terminals.Last_Index then --  FIXME: this can't pass here; Last_Pos never > last_index
            --  There might be an empty nonterm after the last token
            return (State => Failure);
         end if;

         Result_Recurse := Eval (Parser, R, Pos);

         if Result_Recurse.State = Success then
            if Result_Recurse.Last_Pos > Pos_Recurse_Last then
               Parser.Derivs (R).Replace_Element (Start_Pos, Result_Recurse);
               Pos              := Result_Recurse.Last_Pos;
               Pos_Recurse_Last := Pos;

               if WisiToken.Trace_Parse > Detail then
                  Parser.Trace.Put_Line
                    (Parser.Tree.Image (Result_Recurse.Result, Descriptor, Include_Children => True));
               end if;
               --  continue looping

            elsif Result_Recurse.Last_Pos = Pos_Recurse_Last then
               if Parser.Tree.Buffer_Region_Is_Empty (Result_Recurse.Result) then
                  Parser.Derivs (R).Replace_Element (Start_Pos, Result_Recurse);
               end if;
               exit;
            else
               --  Result_Recurse.Last_Pos < Pos_Recurse_Last
               exit;
            end if;
         else
            exit;
         end if;
      end loop;
      return Parser.Derivs (R)(Start_Pos);
   end Apply_Rule;

   ----------
   --  Public subprograms

   function Create
     (Grammar               : in     WisiToken.Productions.Prod_Arrays.Vector;
      Direct_Left_Recursive : in     Token_ID_Set;
      Start_ID              : in     Token_ID;
      Trace                 : access WisiToken.Trace'Class;
      Lexer                 :        WisiToken.Lexer.Handle;
      User_Data             :        WisiToken.Syntax_Trees.User_Data_Access)
     return Procedural.Parser
   is begin
      return Parser : Procedural.Parser (Grammar.First_Index, Grammar.Last_Index) do
         Parser.Trace                 := Trace;
         Parser.Lexer                 := Lexer;
         Parser.User_Data             := User_Data;
         Parser.Grammar               := Grammar;
         Parser.Start_ID              := Start_ID;
         Parser.Direct_Left_Recursive := Direct_Left_Recursive;
      end return;
   end Create;

   overriding procedure Parse (Parser : aliased in out Procedural.Parser)
   is
      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      Junk : Valid_Node_Index;
      pragma Unreferenced (Junk);

      Result : Memo_Entry;
   begin
      Parser.Base_Tree.Clear;
      Parser.Tree.Initialize (Parser.Base_Tree'Unchecked_Access, Flush => True);
      Parser.Lex_All;

      for Nonterm in Descriptor.First_Nonterminal .. Parser.Trace.Descriptor.Last_Nonterminal loop
         Parser.Derivs (Nonterm).Clear;
         Parser.Derivs (Nonterm).Set_First_Last (Parser.Terminals.First_Index, Parser.Terminals.Last_Index + 1);
         --  There might be an empty nonterm after the last token
      end loop;

      for Token_Index in Parser.Terminals.First_Index .. Parser.Terminals.Last_Index loop
         Junk := Parser.Tree.Add_Terminal (Token_Index, Parser.Terminals);
      end loop;

      Result := Apply_Rule (Parser, Parser.Start_ID, Parser.Terminals.First_Index - 1);

      if Result.State /= Success then
         if Trace_Parse > Outline then
            Parser.Trace.Put_Line ("parse failed");
         end if;

         raise Syntax_Error with "parse failed"; --  FIXME: need better error message!
      else
         Parser.Tree.Set_Root (Result.Result);
      end if;
   end Parse;

   overriding function Tree (Parser : in Procedural.Parser) return Syntax_Trees.Tree
   is begin
      return Parser.Tree;
   end Tree;

   overriding function Tree_Var_Ref
     (Parser : aliased in out Procedural.Parser)
     return Syntax_Trees.Tree_Variable_Reference
   is begin
      return (Element => Parser.Tree'Access);
   end Tree_Var_Ref;

end WisiToken.Parse.Packrat.Procedural;
