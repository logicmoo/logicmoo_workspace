--  Abstract :
--
--  Translate a wisitoken grammar file to a tree-sitter grammar file.
--
--  References:
--
--  [1] tree-sitter grammar: https://tree-sitter.github.io/tree-sitter/creating-parsers#the-grammar-dsl
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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.Syntax_Trees.LR_Utils;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
procedure WisiToken.To_Tree_Sitter
is
   procedure Put_Usage
   is begin
      Put_Line ("wisitoken-to_tree_sitter [--verbosity <level] <wisitoken grammar file> <language_name>");
   end Put_Usage;

   procedure Print_Tree_Sitter
     (Data             : in WisiToken_Grammar_Runtime.User_Data_Type;
      Tree             : in Syntax_Trees.Tree;
      Output_File_Name : in String;
      Language_Name    : in String)
   is
      use WisiToken.Syntax_Trees;

      File : File_Type;

      --  Local specs

      procedure Put_RHS_Item_List (Node : in Valid_Node_Index; First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_item_list_ID;

      --  Local bodies

      function Get_Text (Tree_Index : in Valid_Node_Index) return String
      is
         function Strip_Delimiters (Tree_Index : in Valid_Node_Index) return String
         is
            Region : Buffer_Region renames Data.Terminals.all (Tree.Terminal (Tree_Index)).Byte_Region;
         begin
            if -Tree.ID (Tree_Index) in RAW_CODE_ID | REGEXP_ID | ACTION_ID then
               --  Strip delimiters. We don't strip leading/trailing spaces to preserve indent.
               return Data.Grammar_Lexer.Buffer_Text ((Region.First + 2, Region.Last - 2));

               --  We don't strip string delimiters; tree-setter can use the same ones.
            else
               return Data.Grammar_Lexer.Buffer_Text (Region);
            end if;
         end Strip_Delimiters;

      begin
         case Tree.Label (Tree_Index) is
         when Shared_Terminal =>
            return Strip_Delimiters (Tree_Index);

         when Virtual_Terminal =>
            --  Terminal keyword inserted during tree edit. We could check for
            --  Identifier, but that will be caught later.
            return Image (Tree.ID (Tree_Index), Wisitoken_Grammar_Actions.Descriptor);

         when Virtual_Identifier =>
            raise SAL.Programmer_Error;

         when Nonterm =>
            declare
               use all type Ada.Strings.Unbounded.Unbounded_String;
               Result       : Ada.Strings.Unbounded.Unbounded_String;
               Tree_Indices : constant Valid_Node_Index_Array := Tree.Get_Terminals (Tree_Index);
               Need_Space   : Boolean                                      := False;
            begin
               for Tree_Index of Tree_Indices loop
                  Result := Result & (if Need_Space then " " else "") &
                    Get_Text (Tree_Index);
                  Need_Space := True;
               end loop;
               return -Result;
            end;
         end case;
      end Get_Text;

      procedure Not_Translated (Label : in String; Node : in Valid_Node_Index)
      is begin
         New_Line (File);
         Put (File, "// " & Label & ": not translated: " & Node_Index'Image (Node) & ":" &
                Tree.Image (Node, Wisitoken_Grammar_Actions.Descriptor, Include_Children => True));
      end Not_Translated;

      procedure Put_RHS_Alternative_List (Node : in Valid_Node_Index; First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_alternative_list_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            --  If only alternative, don't need "choice()".
            Put_RHS_Item_List (Tree.Child (Node, 1), First => True);

         when 1 =>
            if First then
               Put (File, "choice(");
            end if;

            Put_RHS_Alternative_List (Tree.Child (Node, 1), First => False);
            Put (File, ", ");
            Put_RHS_Item_List (Tree.Child (Node, 3), First => True);

            if First then
               Put (File, ")");
            end if;

         when others =>
            Not_Translated ("Put_RHS_Alternative_List", Node);
         end case;
      end Put_RHS_Alternative_List;

      procedure Put_RHS_Optional_Item (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_optional_item_ID
      is begin
         Put (File, "optional(");

         case Tree.RHS_Index (Node) is
         when 0 | 1 =>
            Put_RHS_Alternative_List (Tree.Child (Node, 2), First => True);
         when 2 =>
            Put (File, "$." & Get_Text (Tree.Child (Node, 1)));
         when 3 =>
            --  STRING_LITERAL_2
            Put (File, Get_Text (Tree.Child (Node, 1)));
         when others =>
            Not_Translated ("Put_RHS_Optional_Item", Node);
         end case;

         Put (File, ")");
      end Put_RHS_Optional_Item;

      procedure Put_RHS_Multiple_Item (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_multiple_item_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 | 3 =>
            Put (File, "repeat(");
            Put_RHS_Alternative_List (Tree.Child (Node, 2), First => True);
            Put (File, ")");

         when 1 | 2 =>
            Put (File, "repeat1(");
            Put_RHS_Alternative_List (Tree.Child (Node, 2), First => True);
            Put (File, ")");

         when 4 =>
            Put (File, "repeat1(");
            Put (File, "$." & Get_Text (Tree.Child (Node, 1)));
            Put (File, ")");

         when 5 =>
            Put (File, "repeat(");
            Put (File, "$." & Get_Text (Tree.Child (Node, 1)));
            Put (File, ")");

         when others =>
            Not_Translated ("Put_RHS_Multiple_Item", Node);
         end case;
      end Put_RHS_Multiple_Item;

      procedure Put_RHS_Group_Item (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_group_item_ID
      is begin
         Not_Translated ("Put_RHS_Group_Item", Node); -- maybe just plain ()?
      end Put_RHS_Group_Item;

      procedure Put_RHS_Item (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_item_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            declare
               use WisiToken_Grammar_Runtime;

               Ident : constant String     := Get_Text (Node);
               Decl  : constant Node_Index := Find_Declaration (Data, Tree, Ident);
            begin
               if Decl = Invalid_Node_Index then
                  Raise_Programmer_Error ("decl for '" & Ident & "' not found", Data, Tree, Node);

               elsif Tree.ID (Decl) = +nonterminal_ID then
                  Put (File, "$." & Get_Text (Tree.Child (Decl, 1)));

               else
                  case Tree.RHS_Index (Decl) is
                  when 0 =>
                     case To_Token_Enum (Tree.ID (Tree.Child (Tree.Child (Decl, 2), 1))) is
                     when KEYWORD_ID =>
                        Put (File, Get_Text (Tree.Child (Decl, 4)));

                     when NON_GRAMMAR_ID =>
                        Not_Translated ("put_rhs_item", Node);

                     when Wisitoken_Grammar_Actions.TOKEN_ID =>
                        declare
                           use WisiToken.Syntax_Trees.LR_Utils;
                           Iter : constant Syntax_Trees.LR_Utils.Iterator :=
                             Iterate (Data, Tree, Tree.Child (Decl, 4), +declaration_item_ID);
                           Item : constant Valid_Node_Index :=
                             Tree.Child (Syntax_Trees.LR_Utils.Node (First (Iter)), 1);
                        begin
                           case To_Token_Enum (Tree.ID (Item)) is
                           when REGEXP_ID =>
                              Put (File, "$." & Ident);

                           when STRING_LITERAL_1_ID | STRING_LITERAL_2_ID =>
                              --  FIXME: case insensitive?
                              Put (File, Get_Text (Item));

                           when others =>
                              Not_Translated ("put_rhs_item ident token", Node);
                           end case;
                        end;

                     when others =>
                        Not_Translated ("put_rhs_item ident", Node);
                     end case;

                  when others =>
                     Not_Translated ("put_rhs_item 0", Node);
                  end case;
               end if;
            end;

         when 1 =>
            --  STRING_LITERAL_2
            Put (File, Get_Text (Node));

         when 2 =>
            --  ignore attribute
            null;

         when 3 =>
            Put_RHS_Optional_Item (Tree.Child (Node, 1));

         when 4 =>
            Put_RHS_Multiple_Item (Tree.Child (Node, 1));

         when 5 =>
            Put_RHS_Group_Item (Tree.Child (Node, 1));

         when others =>
            Not_Translated ("Put_RHS_Item", Node);
         end case;
      end Put_RHS_Item;

      procedure Put_RHS_Element (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_element_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_RHS_Item (Tree.Child (Node, 1));

         when 1 =>
            --  Ignore the label
            Put_RHS_Item (Tree.Child (Node, 3));

         when others =>
            Not_Translated ("Put_RHS_Element", Node);
         end case;
      end Put_RHS_Element;

      procedure Put_RHS_Item_List (Node : in Valid_Node_Index; First : in Boolean)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put_RHS_Element (Children (1));
         else
            if First then
               Put (File, "seq(");
            end if;
            Put_RHS_Item_List (Children (1), First => False);
            Put (File, ", ");
            Put_RHS_Element (Children (2));

            if First then
               Put (File, ")");
            end if;
         end if;
      end Put_RHS_Item_List;

      procedure Put_RHS (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_ID
      is begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put (File, "/* empty */,");

         when 1 .. 3 =>
            Put_RHS_Item_List (Tree.Child (Node, 1), First => True);
            --  ignore actions

         when others =>
            Not_Translated ("put_rhs", Node);
         end case;
      end Put_RHS;

      procedure Put_RHS_List (Node : in Valid_Node_Index; First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_list_ID
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_RHS (Children (1));

         when 1 =>
            if First then
               Put (File, "choice(");
            end if;

            Put_RHS_List (Children (1), First => False);
            Put (File, ",");
            Put_RHS (Children (3));

            if First then
               Put (File, ")");
            end if;

         when others =>
            Not_Translated ("Put_RHS_List", Node);
         end case;
      end Put_RHS_List;

      procedure Process_Node (Node : in Valid_Node_Index)
      is begin
         case To_Token_Enum (Tree.ID (Node)) is
         --  Enum_Token_ID alphabetical order
         when compilation_unit_ID =>
            Process_Node (Tree.Child (Node, 1));

         when compilation_unit_list_ID =>
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
            begin
               case To_Token_Enum (Tree.ID (Children (1))) is
               when compilation_unit_list_ID =>
                  Process_Node (Children (1));
                  Process_Node (Children (2));
               when compilation_unit_ID =>
                  Process_Node (Children (1));
               when others =>
                  raise SAL.Programmer_Error;
               end case;
            end;

         when declaration_ID =>
            case Tree.RHS_Index (Node) is
            when 0 =>
               if Tree.ID (Tree.Child (Tree.Child (Node, 2), 1)) = +Wisitoken_Grammar_Actions.TOKEN_ID then
                  declare
                     use Ada.Strings;
                     use Ada.Strings.Fixed;
                     use WisiToken.Syntax_Trees.LR_Utils;
                     Name : constant String := Get_Text (Tree.Child (Node, 3));
                     Iter : constant Syntax_Trees.LR_Utils.Iterator :=
                       WisiToken_Grammar_Runtime.Iterate (Data, Tree, Tree.Child (Node, 4), +declaration_item_ID);
                     Item : constant Valid_Node_Index :=
                       Tree.Child (Syntax_Trees.LR_Utils.Node (First (Iter)), 1);
                  begin
                     case To_Token_Enum (Tree.ID (Item)) is
                     when REGEXP_ID =>
                        Put_Line (File, Name & ": $ => /" & Trim (Get_Text (Item), Both) & "/,");

                     when others =>
                        null;
                     end case;
                  end;
               end if;

            when others =>
               null;
            end case;

         when nonterminal_ID =>
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
            begin
               Put (File, Get_Text (Children (1)) & ": $ => ");

               Put_RHS_List (Children (3), First => True);

               Put_Line (File, ",");
            end;

         when wisitoken_accept_ID =>
            Process_Node (Tree.Child (Node, 1));

         when others =>
            raise SAL.Not_Implemented with Image (Tree.ID (Node), Wisitoken_Grammar_Actions.Descriptor);
         end case;
      end Process_Node;
   begin
      Create (File, Out_File, Output_File_Name);
      Put_Line (File, "// generated from " & Data.Grammar_Lexer.File_Name & " -*- buffer-read-only:t -*-");

      --  FIXME: copy copyright, license?

      Put_Line (File, "module.exports = grammar({");
      Put_Line (File, "  name: '" & Language_Name & "',");

      Put_Line (File, "  rules: {");

      Process_Node (Tree.Root);

      Put_Line (File, "  }");
      Put_Line (File, "});");
      Close (File);
   end Print_Tree_Sitter;

   Trace          : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
   Input_Data     : aliased WisiToken_Grammar_Runtime.User_Data_Type;
   Grammar_Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;

   Input_File_Name : Ada.Strings.Unbounded.Unbounded_String;
   Language_Name   : Ada.Strings.Unbounded.Unbounded_String;
begin
   Wisitoken_Grammar_Main.Create_Parser
     (Parser    => Grammar_Parser,
      Trace     => Trace'Unchecked_Access,
      User_Data => Input_Data'Unchecked_Access);

   declare
      use Ada.Command_Line;
      Arg : Integer := 1;
   begin
      if not (Argument_Count in 1 .. 4) then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      loop
         exit when Arg > Argument_Count;

         if Argument (Arg) = "--verbosity" then
            Arg := Arg + 1;
            Trace_Generate_EBNF := Integer'Value (Argument (Arg));
            Arg := Arg + 1;

         else
            exit;
         end if;
      end loop;

      --  no more options
      Input_File_Name := +Argument (Arg);
      Arg := Arg + 1;
      Language_Name := +Argument (Arg);
   end;

   begin
      Grammar_Parser.Lexer.Reset_With_File (-Input_File_Name);
   exception
   when Ada.Text_IO.Name_Error | Ada.Text_IO.Use_Error =>
      raise Ada.Text_IO.Name_Error with "input file '" & (-Input_File_Name) & "' could not be opened.";
   end;

   begin
      Grammar_Parser.Parse;
   exception
   when WisiToken.Syntax_Error =>
      Grammar_Parser.Put_Errors;
      raise;
   end;

   Grammar_Parser.Execute_Actions;

   declare
      use Ada.Directories;

      Output_File_Name : constant String := Base_Name (-Input_File_Name) & ".js";

      Tree  : WisiToken.Syntax_Trees.Tree renames Grammar_Parser.Parsers.First_State_Ref.Tree;
   begin
      if Trace_Generate_EBNF > Outline then
         Put_Line ("'" & (-Input_File_Name) & "' => '" & Output_File_Name & "'");
      end if;

      if Trace_Generate_EBNF > Detail then
         Put_Line ("wisitoken tree:");
         Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor);
         Ada.Text_IO.New_Line;
      end if;

      Print_Tree_Sitter (Input_Data, Tree, Output_File_Name, -Language_Name);
   end;

exception
when WisiToken.Syntax_Error | WisiToken.Parse_Error =>
   --  error message already output
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

when E :  others =>
   declare
      use Ada.Exceptions;
      use Ada.Command_Line;
   begin
      Put_Line (Standard_Error, Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Set_Exit_Status (Failure);
   end;
end WisiToken.To_Tree_Sitter;
