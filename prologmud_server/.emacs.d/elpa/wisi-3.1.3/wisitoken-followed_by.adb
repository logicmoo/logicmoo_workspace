--  Abstract :
--
--  Show productions where a token is followed by another token
--
--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Productions;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
procedure WisiToken.Followed_By
is
   use all type WisiToken_Grammar_Runtime.Meta_Syntax;

   procedure Put_Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("wisitoken-followed_by <grammar file> <token a> <token b>");
   end Put_Usage;

   function Last
     (Grammar              : in Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID)
     return Token_Array_Token_Set
   is
      function Last
        (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
         Has_Empty_Production : in Token_ID_Set;
         First_Terminal       : in Token_ID;
         Non_Terminal         : in Token_ID)
        return Token_ID_Set
      is
         Search_Tokens : Token_ID_Set := (Grammar.First_Index .. Grammar.Last_Index => False);
      begin
         Search_Tokens (Non_Terminal) := True;

         return Result : Token_ID_Set := (First_Terminal .. Grammar.Last_Index => False) do
            while Any (Search_Tokens) loop
               declare
                  Added_Tokens   : Token_ID_Set := (First_Terminal .. Grammar.Last_Index      => False);
                  Added_Nonterms : Token_ID_Set := (Grammar.First_Index .. Grammar.Last_Index => False);
               begin
                  for Prod of Grammar loop
                     if Search_Tokens (Prod.LHS) then
                        for RHS of Prod.RHSs loop
                           for ID of reverse RHS.Tokens loop
                              if not Result (ID) then
                                 Added_Tokens (ID) := True;
                                 if ID in Added_Nonterms'Range then
                                    Added_Nonterms (ID) := True;
                                 end if;
                              end if;

                              if ID in Has_Empty_Production'Range and then Has_Empty_Production (ID) then
                                 null;
                              else
                                 exit;
                              end if;
                           end loop;
                        end loop;
                     end if;
                  end loop;

                  Result        := Result or Added_Tokens;
                  Search_Tokens := Added_Nonterms;
               end;
            end loop;
         end return;
      end Last;

      procedure Set_Slice (Result : in out Token_Array_Token_Set; I : Token_ID; Value : in Token_ID_Set)
      is begin
         for J in Result'Range (2) loop
            Result (I, J) := Value (J);
         end loop;
      end Set_Slice;

   begin
      return Result : Token_Array_Token_Set :=
        (Grammar.First_Index .. Grammar.Last_Index =>
           (First_Terminal .. Grammar.Last_Index => False))
      do
         for I in Result'Range loop
            Set_Slice (Result, I, Last (Grammar, Has_Empty_Production, First_Terminal, I));
         end loop;
      end return;
   end Last;

   Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
   Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
   Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

   Token_A_Name : Ada.Strings.Unbounded.Unbounded_String;
   Token_B_Name : Ada.Strings.Unbounded.Unbounded_String;
begin
   Wisitoken_Grammar_Main.Create_Parser
     (Parser    => Grammar_Parser,
      Trace     => Trace'Unchecked_Access,
      User_Data => Input_Data'Unchecked_Access);

   declare
      use Ada.Command_Line;
   begin
      if Argument_Count /= 3 then
         Put_Usage;
      end if;

      Grammar_Parser.Lexer.Reset_With_File (Argument (1));

      Token_A_Name := +Argument (2);
      Token_B_Name := +Argument (3);
   end;

   Grammar_Parser.Parse;
   Grammar_Parser.Execute_Actions; -- Meta phase.

   if Input_Data.Meta_Syntax = WisiToken_Grammar_Runtime.EBNF_Syntax then
      WisiToken_Grammar_Runtime.Translate_EBNF_To_BNF (Grammar_Parser.Parsers.First_State_Ref.Tree, Input_Data);
      if WisiToken.Generate.Error then
         raise WisiToken.Grammar_Error with "errors during translating EBNF to BNF: aborting";
      end if;
   end if;

   Input_Data.Reset;
   Input_Data.Phase := WisiToken_Grammar_Runtime.Other;
   Grammar_Parser.Execute_Actions; -- populates Input_Data.Tokens

   declare
      use Ada.Text_IO;

      Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
        WisiToken.BNF.Generate_Utils.Initialize (Input_Data, Ignore_Conflicts => True);
      --  Builds Generate_Data.Descriptor, Generate_Data.Grammar

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Generate_Data.Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Generate_Data.Grammar, Has_Empty_Production, Generate_Data.Descriptor.First_Terminal);

      Last_Nonterm_Set : constant Token_Array_Token_Set := Last
        (Generate_Data.Grammar, Has_Empty_Production, Generate_Data.Descriptor.First_Terminal);

      Token_A    : constant Token_ID := BNF.Generate_Utils.Find_Token_ID (Generate_Data, -Token_A_Name);
      Token_B    : constant Token_ID := BNF.Generate_Utils.Find_Token_ID (Generate_Data, -Token_B_Name);
      Need_Comma : Boolean           := False;

      procedure Put (LHS : in Token_ID; RHS : in Natural)
      is
      begin
         if Need_Comma then
            Put (", ");
         else
            Need_Comma := True;
         end if;
         Put (Trimmed_Image ((LHS, RHS)));
      end Put;

   begin
      for LHS in Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index loop
         declare
            use WisiToken.Productions;
            Prod : Instance renames Generate_Data.Grammar (LHS);
         begin
            for I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
               declare
                  Tokens : Token_ID_Arrays.Vector renames Prod.RHSs (I).Tokens;
               begin
                  for J in Tokens.First_Index .. Tokens.Last_Index loop
                     if Tokens (J) = Token_A or
                       (Tokens (J) in Last_Nonterm_Set'Range (1) and then
                          Last_Nonterm_Set (Tokens (J), Token_A))
                     then
                        if J < Tokens.Last_Index then
                           if Tokens (J + 1) in First_Nonterm_Set'Range (1) then
                              if First_Nonterm_Set (Tokens (J + 1), Token_B) then
                                 Put (LHS, I);
                              end if;
                           elsif Tokens (J + 1) = Token_B then
                              Put (LHS, I);
                           end if;
                        end if;
                     end if;
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end;

end WisiToken.Followed_By;
