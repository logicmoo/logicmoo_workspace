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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Regexp;
with SAL.Generic_Decimal_Image;
with WisiToken.Generate;   use WisiToken.Generate;
package body WisiToken_Grammar_Runtime is

   use WisiToken;
   use Wisitoken_Grammar_Actions;

   ----------
   --  Body subprograms, misc order

   function Get_Line
     (Data : in User_Data_Type;
      Tree : in Syntax_Trees.Tree;
      Node : in WisiToken.Valid_Node_Index)
     return WisiToken.Line_Number_Type
   is
      --  Find a source line for Node.

      use WisiToken.Syntax_Trees;

      Temp : Node_Index := Node;
   begin
      loop
         if Tree.First_Shared_Terminal (Temp) = Invalid_Token_Index then
            --  Node is empty or all virtual_identifiers; try parents.
            Temp := Tree.Parent (Temp);
            exit when Temp = Invalid_Node_Index;
         else
            return Data.Terminals.all (Tree.First_Shared_Terminal (Temp)).Line;
         end if;
      end loop;
      return Invalid_Line_Number;
   end Get_Line;

   function Get_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Tree_Index   : in Valid_Node_Index;
      Strip_Quotes : in Boolean := False)
     return String
   is
      use all type Syntax_Trees.Node_Label;

      function Strip_Delimiters (Tree_Index : in Valid_Node_Index) return String
      is
         Region : Buffer_Region renames Data.Terminals.all (Tree.Terminal (Tree_Index)).Byte_Region;
      begin
         if -Tree.ID (Tree_Index) in RAW_CODE_ID | REGEXP_ID | ACTION_ID then
            --  Strip delimiters. We don't strip leading/trailing spaces to preserve indent.
            return Data.Grammar_Lexer.Buffer_Text ((Region.First + 2, Region.Last - 2));

         elsif -Tree.ID (Tree_Index) in STRING_LITERAL_1_ID | STRING_LITERAL_2_ID and Strip_Quotes then
            return Data.Grammar_Lexer.Buffer_Text ((Region.First + 1, Region.Last - 1));
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
         if Strip_Quotes then
            declare
               Quoted : constant String := -Data.Tokens.Virtual_Identifiers (Tree.Identifier (Tree_Index));
            begin
               return Quoted (Quoted'First + 1 .. Quoted'Last - 1);
            end;
         else
            return -Data.Tokens.Virtual_Identifiers (Tree.Identifier (Tree_Index));
         end if;

      when Nonterm =>
         declare
            use all type Ada.Strings.Unbounded.Unbounded_String;
            Result       : Ada.Strings.Unbounded.Unbounded_String;
            Tree_Indices : constant Valid_Node_Index_Array := Tree.Get_Terminals (Tree_Index);
            Need_Space   : Boolean                                      := False;
         begin
            for Tree_Index of Tree_Indices loop
               Result := Result & (if Need_Space then " " else "") &
                 Get_Text (Data, Tree, Tree_Index, Strip_Quotes);
               Need_Space := True;
            end loop;
            return -Result;
         end;
      end case;
   end Get_Text;

   function Get_Child_Text
     (Data         : in User_Data_Type;
      Tree         : in Syntax_Trees.Tree;
      Parent       : in Valid_Node_Index;
      Child        : in SAL.Peek_Type;
      Strip_Quotes : in Boolean := False)
     return String
   is
      Tree_Indices : constant Valid_Node_Index_Array := Tree.Get_Terminals (Parent);
   begin
      return Get_Text (Data, Tree, Tree_Indices (Child), Strip_Quotes);
   end Get_Child_Text;

   procedure Start_If_1
     (Data    : in out User_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      A_Index : in     Valid_Node_Index;
      B_Index : in     Valid_Node_Index)
   is
      use all type WisiToken.BNF.Generate_Algorithm;
      use all type WisiToken.BNF.Lexer_Type;
   begin
      if "lexer" = Get_Text (Data, Tree, A_Index) then
         Data.If_Lexer_Present := True;
         Data.Ignore_Lines     := Data.User_Lexer /= WisiToken.BNF.To_Lexer (Get_Text (Data, Tree, B_Index));

      elsif "parser" = Get_Text (Data, Tree, A_Index) then
         Data.If_Parser_Present := True;
         Data.Ignore_Lines := Data.User_Parser /= WisiToken.BNF.Generate_Algorithm'Value
           (Get_Text (Data, Tree, B_Index));

      else
         raise Grammar_Error with
           Error_Message
             (Data.Grammar_Lexer.File_Name, Data.Terminals.all (Tree.First_Shared_Terminal (A_Index)).Line,
              "invalid '%if'; must be one of {lexer | parser}");
      end if;
   end Start_If_1;

   function Get_RHS
     (Data   : in out User_Data_Type;
      Tree   : in     Syntax_Trees.Tree;
      Labels : in out WisiToken.BNF.String_Arrays.Vector;
      Token  : in     Valid_Node_Index)
     return WisiToken.BNF.RHS_Type
   with Pre => Tree.ID (Token) = +rhs_ID
   is
      use all type SAL.Base_Peek_Type;
      Children : constant Valid_Node_Index_Array := Tree.Children (Token);
   begin
      return RHS : WisiToken.BNF.RHS_Type do
         RHS.Source_Line := Get_Line (Data, Tree, Token);

         if Children'Length > 0 then
            for I of Tree.Get_IDs (Children (1), +rhs_element_ID) loop
               case Tree.RHS_Index (I) is
               when 0 =>
                  --  rhs_item
                  RHS.Tokens.Append
                    ((Label      => +"",
                      Identifier => +Get_Text (Data, Tree, Tree.Child (I, 1))));

               when 1 =>
                  --  IDENTIFIER = rhs_item
                  declare
                     Label : constant String := Get_Text (Data, Tree, Tree.Child (I, 1));
                  begin
                     RHS.Tokens.Append
                       ((Label      => +Label,
                         Identifier => +Get_Text (Data, Tree, Tree.Child (I, 3))));

                     if (for all L of Labels => -L /= Label) then
                        Labels.Append (+Label);
                     end if;
                  end;

               when others =>
                  Raise_Programmer_Error ("Get_RHS; unimplimented token", Data, Tree, I);
               end case;
            end loop;

            if Children'Last >= 2 then
               declare
                  Text : constant String := Get_Text (Data, Tree, Children (2));
               begin
                  if Text'Length > 0 and (for some C of Text => C /= ' ') then
                     RHS.Action := +Text;
                     Data.Action_Count := Data.Action_Count + 1;
                  end if;
               end;
            end if;

            if Children'Last >= 3 then
               RHS.Check := +Get_Text (Data, Tree, Children (3));
               Data.Check_Count := Data.Check_Count + 1;
            end if;
         end if;
      end return;
   exception
   when SAL.Programmer_Error =>
      raise;
   when E : others =>
      declare
         use Ada.Exceptions;
      begin
         Raise_Programmer_Error ("Get_RHS: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Token);
      end;
   end Get_RHS;

   procedure Get_Right_Hand_Sides
     (Data             : in out User_Data_Type;
      Tree             : in     WisiToken.Syntax_Trees.Tree;
      Right_Hand_Sides : in out WisiToken.BNF.RHS_Lists.List;
      Labels           : in out WisiToken.BNF.String_Arrays.Vector;
      Token            : in     WisiToken.Valid_Node_Index)
   with Pre => Tree.ID (Token) = +rhs_list_ID
   is
      Tokens : constant Valid_Node_Index_Array := Tree.Children (Token);
   begin
      case Tree.RHS_Index (Token) is
      when 0 =>
         --  | rhs
         if not Data.Ignore_Lines then
            Right_Hand_Sides.Append (Get_RHS (Data, Tree, Labels, Tokens (1)));
         end if;

      when 1 =>
         --  | rhs_list BAR rhs
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (1));

         if not Data.Ignore_Lines then
            Right_Hand_Sides.Append (Get_RHS (Data, Tree, Labels, Tokens (3)));
         end if;

      when 2 =>
         --  | rhs_list PERCENT IF IDENTIFIER EQUAL IDENTIFIER
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (1));
         Start_If_1 (Data, Tree, Tokens (4), Tokens (6));

      when 3 =>
         --  | rhs_list PERCENT END IF
         Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (1));
         Data.Ignore_Lines := False;

      when others =>
         Raise_Programmer_Error ("Get_Right_Hand_Sides", Data, Tree, Token);
      end case;
   end Get_Right_Hand_Sides;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Set_Lexer_Terminals
     (User_Data : in out User_Data_Type;
      Lexer     : in     WisiToken.Lexer.Handle;
      Terminals : in     Base_Token_Array_Access_Constant)
   is begin
      User_Data.Grammar_Lexer := Lexer;
      User_Data.Terminals     := Terminals;
   end Set_Lexer_Terminals;

   overriding procedure Reset (Data : in out User_Data_Type)
   is begin
      --  Preserve data set in Phase Meta, or by Set_Lexer_Terminals, or by
      --  wisitoken-bnf-generate.

      --  Preserve Grammar_Lexer
      --  Preserve User_Lexer
      --  Preserve User_Parser
      --  Perserve Generate_Set
      --  Preserve Meta_Syntax
      --  Preserve Phase
      --  Preserve Terminals
      --  Preserve Non_Grammar
      --  EBNF_Nodes handled in Initialize_Actions
      Data.Raw_Code          := (others => <>);
      Data.Language_Params   :=
        (Case_Insensitive => Data.Language_Params.Case_Insensitive,
         others => <>);
      Data.Tokens            :=
        (Virtual_Identifiers => Data.Tokens.Virtual_Identifiers,
         others => <>);
      Data.Conflicts.Clear;
      Data.McKenzie_Recover  := (others => <>);
      Data.Rule_Count        := 0;
      Data.Action_Count      := 0;
      Data.Check_Count       := 0;
      Data.Label_Count       := 0;
      Data.If_Lexer_Present  := False;
      Data.If_Parser_Present := False;
      Data.Ignore_Lines      := False;
   end Reset;

   overriding procedure Initialize_Actions
     (Data : in out User_Data_Type;
      Tree : in     WisiToken.Syntax_Trees.Tree'Class)
   is begin
      Data.EBNF_Nodes.Clear;
      Data.EBNF_Nodes.Set_First_Last (Tree.First_Index, Tree.Last_Index);
   end Initialize_Actions;

   overriding
   procedure Lexer_To_Augmented
     (Data  : in out          User_Data_Type;
      Tree  : in out          WisiToken.Syntax_Trees.Tree'Class;
      Token : in              WisiToken.Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      pragma Unreferenced (Lexer);
      use all type Ada.Containers.Count_Type;
   begin
      if Token.ID < Wisitoken_Grammar_Actions.Descriptor.First_Terminal then
         --  Non-grammar token
         if Data.Terminals.Length = 0 then
            Data.Leading_Non_Grammar.Append (Token);
         else
            declare
               Containing_Aug : Augmented_Token_Access := Augmented_Token_Access
                 (Tree.Augmented (Data.Last_Terminal_Node));
            begin
               if Containing_Aug = null then
                  Containing_Aug := new Augmented_Token'
                    (Data.Terminals.all (Tree.First_Shared_Terminal (Data.Last_Terminal_Node)) with Non_Grammar => <>);
                  Tree.Set_Augmented (Data.Last_Terminal_Node, WisiToken.Base_Token_Class_Access (Containing_Aug));
               end if;

               Containing_Aug.Non_Grammar.Append (Token);
            end;
         end if;
      else
         Data.Last_Terminal_Node := Token.Tree_Index;
      end if;
   end Lexer_To_Augmented;

   procedure Start_If
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is begin
      --  all phases
      Start_If_1 (User_Data_Type (User_Data), Tree, Tokens (3), Tokens (5));
   end Start_If;

   procedure End_If (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      --  all phases
      Data.Ignore_Lines := False;
   end End_If;

   procedure Add_Declaration
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      use all type WisiToken.Syntax_Trees.Node_Label;
      use all type Ada.Strings.Unbounded.Unbounded_String;

      Data : User_Data_Type renames User_Data_Type (User_Data);

      function Token (Index : in SAL.Peek_Type) return Base_Token
      is
         use all type SAL.Base_Peek_Type;
      begin
         if Tokens'Last < Index then
            raise SAL.Programmer_Error;
         elsif Tree.Label (Tokens (Index)) /= WisiToken.Syntax_Trees.Shared_Terminal then
            raise SAL.Programmer_Error with "token at " & Image (Tree.Byte_Region (Tokens (Index))) &
              " is a " & WisiToken.Syntax_Trees.Node_Label'Image (Tree.Label (Tokens (Index))) &
              ", expecting Shared_Terminal";
         else
            return Data.Terminals.all (Tree.Terminal (Tokens (Index)));
         end if;
      end Token;

      function Enum_ID (Index : in SAL.Peek_Type) return Token_Enum_ID
      is (To_Token_Enum (Token (Index).ID));

   begin
      if Data.Phase = Meta then
         if Tree.Label (Tokens (2)) = WisiToken.Syntax_Trees.Shared_Terminal then
            case Enum_ID (2) is
            when IDENTIFIER_ID =>
               declare
                  Kind : constant String := Data.Grammar_Lexer.Buffer_Text (Token (2).Byte_Region);
               begin
                  if Kind = "case_insensitive" then
                     Data.Language_Params.Case_Insensitive := True;

                  elsif Kind = "generate" then
                     declare
                        use all type SAL.Base_Peek_Type;
                        Children : constant Valid_Node_Index_Array := Tree.Get_Terminals (Tokens (3));
                        Tuple    : WisiToken.BNF.Generate_Tuple;
                     begin
                        Tuple.Gen_Alg  := WisiToken.BNF.To_Generate_Algorithm (Get_Text (Data, Tree, Children (1)));
                        if Children'Last >= 2 then
                           Tuple.Out_Lang := WisiToken.BNF.To_Output_Language (Get_Text (Data, Tree, Children (2)));
                        end if;
                        for I in 3 .. SAL.Base_Peek_Type (Children'Length) loop
                           declare
                              Text : constant String := Get_Text (Data, Tree, Children (I));
                           begin
                              if Text = "text_rep" then
                                 Tuple.Text_Rep := True;

                              elsif (for some I of WisiToken.BNF.Lexer_Image => Text = I.all) then
                                 Tuple.Lexer := WisiToken.BNF.To_Lexer (Text);

                              elsif (for some I in WisiToken.BNF.Valid_Interface =>
                                       WisiToken.BNF.To_Lower (Text) = WisiToken.BNF.To_Lower
                                         (WisiToken.BNF.Valid_Interface'Image (I)))
                              then
                                 Tuple.Interface_Kind := WisiToken.BNF.Valid_Interface'Value (Text);
                              else
                                 declare
                                    Token : Base_Token renames Data.Terminals.all (Tree.Terminal (Children (I)));
                                 begin
                                    raise Grammar_Error with Error_Message
                                      (Data.Grammar_Lexer.File_Name, Token.Line, Token.Column,
                                       "invalid generate param '" & Text & "'");
                                 end;
                              end if;
                           end;
                        end loop;
                        WisiToken.BNF.Add (Data.Generate_Set, Tuple);
                     end;

                  elsif Kind = "meta_syntax" then
                     if Data.Meta_Syntax = Unknown then
                        --  Don't overwrite; somebody set it for a reason.
                        declare
                           Value_Str : constant String := WisiToken.BNF.To_Lower (Get_Text (Data, Tree, Tokens (3)));
                        begin
                           if Value_Str = "bnf" then
                              Data.Meta_Syntax := BNF_Syntax;
                           elsif Value_Str = "ebnf" then
                              Data.Meta_Syntax := EBNF_Syntax;
                              Data.EBNF_Nodes (Tree.Find_Ancestor (Tokens (2), +declaration_ID)) := True;

                           else
                              Put_Error ("invalid value for %meta_syntax; must be BNF | EBNF.");
                           end if;
                        end;
                     end if;
                  end if;
               end;
            when others =>
               null;
            end case;
         end if;
         return;
      end if;

      --  Add declaration to User_Data.Generate_Set, Language_Params,
      --  Tokens, Conflicts, or McKenzie_Recover.

      if Data.Ignore_Lines then
         return;
      end if;

      case Tree.Label (Tokens (2)) is
      when Syntax_Trees.Nonterm =>
         --  must be token_keyword_non_grammar
         declare
            Children_2 : constant Valid_Node_Index_Array := Tree.Children (Tokens (2));
            Child_1_ID : constant Token_Enum_ID := To_Token_Enum (Tree.ID (Children_2 (1)));
         begin
            case Child_1_ID is
            when Wisitoken_Grammar_Actions.TOKEN_ID =>
               declare
                  Children_4 : constant Valid_Node_Index_Array := Tree.Children (Tokens (4));
               begin
                  WisiToken.BNF.Add_Token
                    (Data.Tokens.Tokens,
                     Kind         => Get_Text (Data, Tree, Children_2 (3)),
                     Name         => Get_Text (Data, Tree, Tokens (3)),
                     Value        => Get_Text (Data, Tree, Children_4 (1)),
                     Repair_Image => (if Children_4'Length = 1 then "" else Get_Text (Data, Tree, Children_4 (2))));
               end;

            when KEYWORD_ID =>

               Data.Tokens.Keywords.Append
                 ((Name  => +Get_Text (Data, Tree, Tokens (3)),
                   Value => +Get_Text (Data, Tree, Tokens (4))));

            when NON_GRAMMAR_ID =>

               WisiToken.BNF.Add_Token
                 (Data.Tokens.Non_Grammar,
                  Kind  => Get_Text (Data, Tree, Children_2 (3)),
                  Name  => Get_Text (Data, Tree, Tokens (3)),
                  Value => Get_Text (Data, Tree, Tokens (4)));

            when others =>
               raise SAL.Programmer_Error;
            end case;
         end;

      when Syntax_Trees.Shared_Terminal =>
         case Enum_ID (2) is
         when CODE_ID =>
            declare
               Location : WisiToken.BNF.Raw_Code_Location;

               --  % code identifier_list raw_code
               --  1 2    3               4
               --
               --  identifier_list = "action spec context"
               --  identifier_list children = identifier_list IDENTIFIER_ID
               --  children = identifier_list IDENTIFIER_ID
               --  children = IDENTIFIER_ID
               function Get_Loc_List return Base_Token_Array
               with Pre => Tree.ID (Tokens (3)) = +identifier_list_ID
               is
                  use all type SAL.Base_Peek_Type;
                  use WisiToken.Syntax_Trees;
                  Node   : Valid_Node_Index := Tokens (3);
                  Result : Base_Token_Array (1 .. 3);
                  First  : SAL.Peek_Type    := Result'Last + 1;
               begin
                  loop
                     pragma Assert (Tree.ID (Node) = +identifier_list_ID);
                     exit when not Tree.Has_Children (Node);
                     declare
                        Children : constant Valid_Node_Index_Array := Tree.Children (Node);
                     begin
                        if Children'Length = 1 then
                           --  identifier_list : IDENTIFIER
                           First := First - 1;
                           Result (First) := Data.Terminals.all (Tree.Terminal (Children (1)));
                           exit;

                        elsif Children'Length = 2 then
                           --  identifier_list : identifier_list IDENTIFIER
                           First := First - 1;
                           Result (First) := Data.Terminals.all (Tree.Terminal (Children (2)));

                           Node := Children (1);
                        else
                           raise SAL.Programmer_Error;
                        end if;
                     end;
                  end loop;
                  return Result (First .. Result'Last);
               end Get_Loc_List;

               Loc_List : constant Base_Token_Array := Get_Loc_List;

               function Get_Loc (Index : in SAL.Peek_Type) return String
               is (Data.Grammar_Lexer.Buffer_Text (Loc_List (Index).Byte_Region));

            begin
               if Get_Loc (Loc_List'First) = "actions" then
                  Location :=
                    (if Get_Loc (2) = "spec" then
                       (if Get_Loc (3) = "context" then WisiToken.BNF.Actions_Spec_Context
                        elsif Get_Loc (3) = "pre" then WisiToken.BNF.Actions_Spec_Pre
                        elsif Get_Loc (3) = "post" then WisiToken.BNF.Actions_Spec_Post
                        else raise Grammar_Error with
                          Error_Message
                            (Data.Grammar_Lexer.File_Name, Loc_List (2).Line,
                             "expecting {context | pre | post}"))

                     elsif Get_Loc (2) = "body" then
                       (if Get_Loc (3) = "context" then WisiToken.BNF.Actions_Body_Context
                        elsif Get_Loc (3) = "pre" then WisiToken.BNF.Actions_Body_Pre
                        elsif Get_Loc (3) = "post" then WisiToken.BNF.Actions_Body_Post
                        else raise Grammar_Error with
                          Error_Message
                            (Data.Grammar_Lexer.File_Name, Loc_List (2).Line,
                             "expecting {context | pre | post}"))

                     else raise Grammar_Error);

               elsif Get_Loc (Loc_List'First) = "copyright_license" then
                  Location := WisiToken.BNF.Copyright_License;

               else
                  raise Grammar_Error with
                    Error_Message
                      (Data.Grammar_Lexer.File_Name, Loc_List (Loc_List'First).Line,
                       "expecting {actions | copyright_license}");
               end if;

               Data.Raw_Code (Location) := WisiToken.BNF.Split_Lines (Get_Text (Data, Tree, Tokens (4)));
            exception
            when Grammar_Error =>
               Put_Error
                 (Error_Message
                    (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column,
                     "invalid raw code location; actions {spec | body} {context | pre | post}"));
            end;

         when IDENTIFIER_ID =>
            declare
               Kind : constant String := Data.Grammar_Lexer.Buffer_Text (Token (2).Byte_Region);
            begin
               --  Alphabetical by Kind

               if Kind = "case_insensitive" then
                  --  Not in phase Other
                  null;

               elsif Kind = "conflict" then
                  declare
                     Tree_Indices : constant Valid_Node_Index_Array := Tree.Get_Terminals
                       (Tokens (3));
                     --   %conflict <action_a>/<action_b> in state <LHS_A>, <LHS_B> on token <on>
                     --              1        2 3         4  5      6     7  8      9  10     11
                  begin
                     Data.Conflicts.Append
                       ((Source_Line => Data.Terminals.all (Tree.Terminal (Tree_Indices (1))).Line,
                         Action_A    => +Get_Text (Data, Tree, Tree_Indices (1)),
                         LHS_A       => +Get_Text (Data, Tree, Tree_Indices (6)),
                         Action_B    => +Get_Text (Data, Tree, Tree_Indices (3)),
                         LHS_B       => +Get_Text (Data, Tree, Tree_Indices (8)),
                         On          => +Get_Text (Data, Tree, Tree_Indices (11))));
                  end;

               elsif Kind = "end" then
                  --  matching '%if' specified current lexer.
                  null;

               elsif Kind = "elisp_face" then
                  Data.Tokens.Faces.Append (Get_Text (Data, Tree, Tokens (3), Strip_Quotes => True));

               elsif Kind = "elisp_indent" then
                  Data.Tokens.Indents.Append
                    ((Name  => +Get_Child_Text (Data, Tree, Tokens (3), 1, Strip_Quotes => True),
                      Value => +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "elisp_action" then
                  Data.Tokens.Actions.Insert
                    (Key             => +Get_Child_Text (Data, Tree, Tokens (3), 2),
                     New_Item        =>
                       (Action_Label => +Get_Child_Text (Data, Tree, Tokens (3), 1),
                        Ada_Name     => +Get_Child_Text (Data, Tree, Tokens (3), 3)));

               elsif Kind = "end_names_optional_option" then
                  Data.Language_Params.End_Names_Optional_Option := +Get_Text (Data, Tree, Tokens (3));

               elsif Kind = "generate" then
                  --  Not in Other phase
                  null;

               elsif Kind = "language_runtime" then
                  Data.Language_Params.Language_Runtime_Name :=
                    +Get_Text (Data, Tree, Tokens (3), Strip_Quotes => True);

               elsif Kind = "mckenzie_check_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Check_Limit := Token_Index'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_check_delta_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Check_Delta_Limit := Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_default" then
                  if Tree.Get_Terminals (Tokens (3))'Length /= 4 then
                     raise Grammar_Error with
                       Error_Message
                         (Data.Grammar_Lexer.File_Name,
                          Data.Terminals.all (Tree.First_Shared_Terminal (Tokens (3))).Line,
                          "too " & (if Tree.Get_Terminals (Tokens (3))'Length > 4 then "many" else "few") &
                            " default costs; should be 'insert, delete, push back, ignore check fail'.");
                  end if;

                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Source_Line := Data.Terminals.all
                    (Tree.First_Shared_Terminal (Tokens (1))).Line;

                  Data.McKenzie_Recover.Default_Insert          := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 1));
                  Data.McKenzie_Recover.Default_Delete_Terminal := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 2));
                  Data.McKenzie_Recover.Default_Push_Back       := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 3));
                  Data.McKenzie_Recover.Ignore_Check_Fail       := Natural'Value
                    (Get_Child_Text (Data, Tree, Tokens (3), 4));

               elsif Kind = "mckenzie_cost_delete" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Delete.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_fast_forward" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Fast_Forward :=
                    Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_insert" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Insert.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_matching_begin" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Matching_Begin :=
                    Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_cost_push_back" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Push_Back.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_cost_undo_reduce" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Undo_Reduce.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               elsif Kind = "mckenzie_enqueue_limit" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Enqueue_Limit := Natural'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "mckenzie_minimal_complete_cost_delta" then
                  Data.Language_Params.Error_Recover := True;
                  Data.McKenzie_Recover.Minimal_Complete_Cost_Delta :=
                    Integer'Value (Get_Text (Data, Tree, Tokens (3)));

               elsif Kind = "meta_syntax" then
                  --  not in Other phase
                  null;

               elsif Kind = "no_enum" then
                  Data.Language_Params.Declare_Enums := False;

               elsif Kind = "no_language_runtime" then
                  Data.Language_Params.Use_Language_Runtime := False;

               elsif Kind = "partial_recursion" then
                  Data.Language_Params.Partial_Recursion := True;

               elsif Kind = "start" then
                  Data.Language_Params.Start_Token := +Get_Text (Data, Tree, Tokens (3));

               elsif Kind = "re2c_regexp" then
                  Data.Tokens.re2c_Regexps.Append
                    ((+Get_Child_Text (Data, Tree, Tokens (3), 1),
                      +Get_Child_Text (Data, Tree, Tokens (3), 2)));

               else
                  raise Grammar_Error with Error_Message
                    (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column, "unexpected syntax");

               end if;
            end;

         when others =>
            raise Grammar_Error with Error_Message
              (Data.Grammar_Lexer.File_Name, Token (2).Line, Token (2).Column, "unexpected syntax");
         end case;

      when Syntax_Trees.Virtual_Terminal | Syntax_Trees.Virtual_Identifier =>
         raise SAL.Programmer_Error;
      end case;
   end Add_Declaration;

   procedure Add_Nonterminal
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array)
   is
      use all type Ada.Containers.Count_Type;
      use WisiToken.Syntax_Trees;

      Data : User_Data_Type renames User_Data_Type (User_Data);

      LHS_Node   : constant Valid_Node_Index := Tokens (1);
      LHS_String : constant String           := Get_Text (Data, Tree, LHS_Node);

      Right_Hand_Sides : WisiToken.BNF.RHS_Lists.List;
      Labels           : WisiToken.BNF.String_Arrays.Vector;
   begin
      if Data.Phase = Meta or Data.Ignore_Lines then
         return;
      end if;

      Data.Rule_Count := Data.Rule_Count + 1;

      Get_Right_Hand_Sides (Data, Tree, Right_Hand_Sides, Labels, Tokens (3));

      if WisiToken.BNF.Is_Present (Data.Tokens.Rules, LHS_String) then
         case Tree.Label (LHS_Node) is
         when Shared_Terminal =>
            declare
               LHS_Token : Base_Token renames Data.Terminals.all (Tree.Terminal (LHS_Node));
            begin
               raise Grammar_Error with Error_Message
                 (Data.Grammar_Lexer.File_Name, LHS_Token.Line, LHS_Token.Column, "duplicate nonterm");
            end;

         when Virtual_Identifier =>
            raise Grammar_Error with Error_Message
              (Data.Grammar_Lexer.File_Name, 1, 1, "duplicate virtual nonterm '" & LHS_String & "'");

         when others =>
            Raise_Programmer_Error ("Add_Nonterminal", Data, Tree, LHS_Node);
         end case;
      else
         Data.Label_Count := Data.Label_Count + Labels.Length;

         Data.Tokens.Rules.Append
           ((+LHS_String, Right_Hand_Sides, Labels,
             Source_Line =>
               (case Tree.Label (LHS_Node) is
                when Shared_Terminal    => Data.Terminals.all (Tree.First_Shared_Terminal (LHS_Node)).Line,
                when Virtual_Identifier => Invalid_Line_Number, -- IMPROVEME: get line from Right_Hand_Sides
                when others             => raise SAL.Programmer_Error)));
      end if;
   end Add_Nonterminal;

   function Image_Grammar_Action (Action : in WisiToken.Syntax_Trees.Semantic_Action) return String
   is
      pragma Unreferenced (Action);
   begin
      return "action";
   end Image_Grammar_Action;

   procedure Check_EBNF
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in     WisiToken.Syntax_Trees.Tree;
      Tokens    : in     WisiToken.Valid_Node_Index_Array;
      Token     : in     WisiToken.Positive_Index_Type)
   is
      Data : User_Data_Type renames User_Data_Type (User_Data);
   begin
      case Data.Phase is
      when Meta =>
         Data.EBNF_Nodes (Tokens (Token)) := True;

         if Data.Meta_Syntax /= EBNF_Syntax then
            declare
               Tok  : Base_Token renames Data.Terminals.all (Tree.First_Shared_Terminal (Tokens (Token)));
            begin
               raise Grammar_Error with Error_Message
                 (Data.Grammar_Lexer.File_Name, Tok.Line, Tok.Column,
                  "EBNF syntax used, but BNF specified; set '%meta_syntax EBNF'");
            end;
         end if;
      when Other =>
         Raise_Programmer_Error ("untranslated EBNF node", Data, Tree, Tree.Parent (Tokens (Token)));
      end case;
   end Check_EBNF;

   procedure Raise_Programmer_Error
     (Label : in String;
      Data  : in User_Data_Type;
      Tree  : in WisiToken.Syntax_Trees.Tree;
      Node  : in WisiToken.Node_Index)
   is begin
      WisiToken.Syntax_Trees.LR_Utils.Raise_Programmer_Error
        (Label, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer, Tree, Data.Terminals.all, Node);
   end Raise_Programmer_Error;

   function Find_Declaration
     (Data : in     User_Data_Type;
      Tree : in out WisiToken.Syntax_Trees.Tree;
      Name : in     String)
     return WisiToken.Node_Index
   is
      use WisiToken.Syntax_Trees.LR_Utils;
      use WisiToken.Syntax_Trees.LR_Utils.Creators;

      function Decl_Name (Decl : in Valid_Node_Index) return String
      is begin
         case To_Token_Enum (Tree.ID (Decl)) is
         when declaration_ID =>
            case Tree.RHS_Index (Decl) is
            when 0 =>
               return Get_Text (Data, Tree, Tree.Child (Decl, 3));

            when 2 | 3 =>
               return Get_Text (Data, Tree, Tree.Child (Decl, 2));

            when others =>
               return "";
            end case;

         when nonterminal_ID =>
            return Get_Text (Data, Tree, Tree.Child (Decl, 1));

         when others =>
            return "";
         end case;
      end Decl_Name;

      --  Tree.Root is wisitoken_accept
      List : constant Constant_List := Create_List
        (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
   begin
      for N of List loop
         declare
            Decl : constant Valid_Node_Index := Tree.Child (N, 1);
         begin
            if Name = Decl_Name (Decl) then
               return Decl;
            end if;
         end;
      end loop;
      return Invalid_Node_Index;
   end Find_Declaration;

   procedure Translate_EBNF_To_BNF
     (Tree : in out WisiToken.Syntax_Trees.Tree;
      Data : in out User_Data_Type)
   is
      use all type SAL.Base_Peek_Type;
      use WisiToken.Syntax_Trees;

      Copied_EBNF_Nodes : WisiToken.Valid_Node_Index_Arrays.Vector;

      Symbol_Regexp : constant GNAT.Regexp.Regexp := GNAT.Regexp.Compile
        ((if Data.Language_Params.Case_Insensitive
          then "[A-Z0-9_]+"
          else "[a-zA-Z0-9_]+"),
         Case_Sensitive => not Data.Language_Params.Case_Insensitive);

      procedure Erase_Copied_EBNF_Node (Node : in Valid_Node_Index)
      is
         use Ada.Text_IO;
         Found : Boolean := False;
      begin
         if Trace_Generate_EBNF > Outline then
            Put_Line ("erase copied deleted EBNF node" & Node'Image);
         end if;
         --  Vector Delete replaces content with
         --  Valid_Node_Index_Arrays.Default_Element = Valid_Node_Index'Last =
         --  Deleted_Child; this is clearer.

         for I in Copied_EBNF_Nodes.First_Index .. Copied_EBNF_Nodes.Last_Index loop
            if Copied_EBNF_Nodes (I) = Node then
               Copied_EBNF_Nodes (I) := Deleted_Child;
               Found := True;
               exit;
            end if;
         end loop;
         if not Found then
            Put_Line (Current_Error, Tree.Image
                        (Node, Wisitoken_Grammar_Actions.Descriptor, Node_Numbers => True) &
                        " not found in Copied_EBNF_Nodes");
            raise SAL.Programmer_Error;
         end if;
      end Erase_Copied_EBNF_Node;

      procedure Clear_EBNF_Node (Node : in Valid_Node_Index)
      is begin
         if Node in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index then
            if Trace_Generate_EBNF > Outline then
               Ada.Text_IO.Put_Line ("clear translated EBNF node" & Node'Image);
            end if;

            Data.EBNF_Nodes (Node) := False;
         else
            Erase_Copied_EBNF_Node (Node);
         end if;
      end Clear_EBNF_Node;

      function New_Identifier (Text : in String) return Identifier_Index
      is
         ID : constant Identifier_Index := Base_Identifier_Index (Data.Tokens.Virtual_Identifiers.Length) + 1;
      begin
         Data.Tokens.Virtual_Identifiers.Append (+Text);
         return ID;
      end New_Identifier;

      Keyword_Ident : constant Identifier_Index := New_Identifier ("keyword");
      Percent_Ident : constant Identifier_Index := New_Identifier ("percent");

      function Next_Nonterm_Name (Suffix : in String := "") return Identifier_Index
      is
         function Image is new SAL.Generic_Decimal_Image (Identifier_Index);
         ID : constant Identifier_Index := Identifier_Index (Data.Tokens.Virtual_Identifiers.Length) + 1;
      begin

         if ID > 999 then
            --  We assume 3 digits below
            raise SAL.Programmer_Error with "more than 3 digits needed for virtual identifiers in EBNF translate";
         end if;

         Data.Tokens.Virtual_Identifiers.Append (+("nonterminal_" & Image (ID, Width => 3) & Suffix));

         return ID;
      end Next_Nonterm_Name;

      function Find_Nonterminal
        (Target : in String;
         Equal  : in WisiToken.Syntax_Trees.LR_Utils.Find_Equal)
        return Node_Index
      is
         use WisiToken.Syntax_Trees.LR_Utils;
      begin
         return Get_Node
           (Creators.Create_List
              (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID).Find
              (Target, Equal));
      end Find_Nonterminal;

      function Tree_Add_Nonterminal
        (Child_1 : in Valid_Node_Index;
         Child_2 : in Valid_Node_Index;
         Child_3 : in Valid_Node_Index;
         Child_4 : in Valid_Node_Index)
        return Valid_Node_Index
      is begin
         --  Work around GNAT error about arbitrary evaluation order in
         --  aggregates (no error about the arbitrary order in subprogram
         --  parameter_assocation_lists!).
         return Tree.Add_Nonterm
           (Production => (+nonterminal_ID, 0),
            Children   => (Child_1, Child_2, Child_3, Child_4),
            Action     => Wisitoken_Grammar_Actions.nonterminal_0'Access);
      end Tree_Add_Nonterminal;

      function Duplicate
        (List        : in Syntax_Trees.LR_Utils.List;
         New_Content : in Node_Index)
        return Boolean
      is
         --  We don't require New_Content.ID = List.Element_ID; since we are
         --  comparing result of Get_Text.
         New_Content_Str : constant String :=
           (if New_Content = Invalid_Node_Index
            then "" --  Empty RHS
            else Get_Text (Data, Tree, New_Content));
      begin
         for N of List loop
            if New_Content_Str = Get_Text (Data, Tree, N) then
               return True;
            end if;
         end loop;
         return False;
      end Duplicate;

      procedure Insert_Empty_RHS
        (RHS_List : in out WisiToken.Syntax_Trees.LR_Utils.List;
         After    : in     Valid_Node_Index)
      with Pre => RHS_List.List_ID = +rhs_list_ID and RHS_List.Element_ID = +rhs_ID and
                  Tree.ID (After) = +rhs_ID and RHS_List.Contains (After)
      is begin
         RHS_List.Insert
           (New_Element => Tree.Add_Nonterm
              ((+rhs_ID, 0),
               (1 .. 0 => Invalid_Node_Index)),
            After => RHS_List.To_Cursor (After));
      end Insert_Empty_RHS;

      procedure Insert_RHS
        (RHS_List          : in out WisiToken.Syntax_Trees.LR_Utils.List;
         New_RHS_Item_List : in     Valid_Node_Index;
         After             : in     Valid_Node_Index)
      with Pre => RHS_List.List_ID = +rhs_list_ID and RHS_List.Element_ID = +rhs_ID and
                  Tree.ID (New_RHS_Item_List) = +rhs_item_list_ID and
                  Tree.ID (After) = +rhs_ID and RHS_List.Contains (After)
      is begin
         RHS_List.Insert
           (New_Element => Tree.Add_Nonterm
              (Production => (+rhs_ID, Tree.RHS_Index (After)),
               Children =>
                 (case Tree.RHS_Index (After) is
                  when 1 => (1 => New_RHS_Item_List),
                  when 2 => (New_RHS_Item_List, Tree.Copy_Subtree (Tree.Child (After, 2))),
                  when 3 => (New_RHS_Item_List,
                             Tree.Copy_Subtree (Tree.Child (After, 2)),
                             Tree.Copy_Subtree (Tree.Child (After, 3))),
                  when others => raise SAL.Programmer_Error)),
            After => RHS_List.To_Cursor (After));
      end Insert_RHS;

      procedure Record_Copied_EBNF_Nodes (Node : in Valid_Node_Index)
      is
         procedure Record_Copied_Node
           (Tree : in out WisiToken.Syntax_Trees.Tree;
            Node : in WisiToken.Valid_Node_Index)
         is begin
            if To_Token_Enum (Tree.ID (Node)) in
              rhs_optional_item_ID |
              rhs_multiple_item_ID |
              rhs_group_item_ID |
              rhs_attribute_ID |
              STRING_LITERAL_2_ID
            then
               if Trace_Generate_EBNF > Outline then
                  Ada.Text_IO.Put_Line
                    ("new EBNF node " & Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Node_Numbers => True));
               end if;
               Copied_EBNF_Nodes.Append (Node);
            end if;
         end Record_Copied_Node;
      begin
         Tree.Process_Tree (Record_Copied_Node'Access, Node);
      end Record_Copied_EBNF_Nodes;

      procedure Erase_Deleted_EBNF_Nodes (Node : in Valid_Node_Index)
      is
         procedure Erase_Deleted_Node
           (Tree : in out WisiToken.Syntax_Trees.Tree;
            Node : in WisiToken.Valid_Node_Index)
         is begin
            if To_Token_Enum (Tree.ID (Node)) in
              rhs_optional_item_ID |
              rhs_multiple_item_ID |
              rhs_group_item_ID |
              rhs_attribute_ID |
              STRING_LITERAL_2_ID
            then
               if Node in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index then
                  --  Node is original, not copied
                  if Trace_Generate_EBNF > Outline then
                     Ada.Text_IO.Put_Line ("erase original deleted EBNF node" & Node'Image);
                  end if;
                  Data.EBNF_Nodes (Node) := False;
               else
                  Erase_Copied_EBNF_Node (Node);
               end if;
            end if;
         end Erase_Deleted_Node;
      begin
         Tree.Process_Tree (Erase_Deleted_Node'Access, Node);
      end Erase_Deleted_EBNF_Nodes;

      function Insert_Optional_RHS (B : in Valid_Node_Index) return Valid_Node_Index
      with Pre => Tree.ID (B) in +rhs_multiple_item_ID | +rhs_optional_item_ID | +IDENTIFIER_ID
      is
         --  B is an optional item in an rhs_item_list:
         --  | A B? C
         --
         --  or B is a rhs_multiple_item that is allowed to be empty:
         --  | A B* C
         --
         --  or B is a virtual identifier naming the new nonterm replacing the
         --  original
         --
         --  A, C can be empty. The containing element may be rhs or
         --  rhs_alternative_list.
         --
         --  Insert either a second rhs, or a second rhs_item_list, after the
         --  one containing B, without B.
         --
         --  Return the List_Root of the edited list.

         use Syntax_Trees.LR_Utils;
         use Syntax_Trees.LR_Utils.Creators;
         use all type Ada.Containers.Count_Type;

         function Find_Skips return Skip_Info
         is
            Non_Empty_List : Node_Index := Invalid_Node_Index;
            --  First (nearest) rhs_item_list ancestor of B that will not be empty
            --  when B is skipped.

            Skip_Last        : Positive_Index_Type'Base := Positive_Index_Type'First;
            Last_Skip_Node   : Valid_Node_Index         := Tree.Find_Ancestor (B, +rhs_element_ID);
            Reset_Search_For : WisiToken.Token_ID       := +rhs_item_list_ID;

            procedure Search (Result : in out Skip_Info)
            is
               Skip_Node  : Valid_Node_Index   := Last_Skip_Node;
               Search_For : WisiToken.Token_ID := Reset_Search_For;
            begin
               loop
                  case To_Token_Enum (Search_For) is
                  when rhs_item_list_ID =>
                     Skip_Node := Tree.Find_Ancestor (Skip_Node, +rhs_item_list_ID);

                     Skip_Node := List_Root (Tree, Skip_Node, +rhs_item_list_ID);

                     Search_For := +rhs_element_ID;

                     if Result.Skips'Length = 0 then
                        declare
                           List_Count : constant Ada.Containers.Count_Type := Create_List
                             (Tree, Skip_Node, +rhs_item_list_ID, +rhs_element_ID).Count;
                        begin
                           if List_Count > 1 then
                              Non_Empty_List := List_Root (Tree, Skip_Node, +rhs_item_list_ID);
                              exit;

                           elsif Skip_Last = Positive_Index_Type'First and List_Count = 1 then
                              --  This list will be empty; no need to descend into it
                              Last_Skip_Node   := Skip_Node;
                              Reset_Search_For := Search_For;
                           else
                              Skip_Last := Skip_Last + 1;
                           end if;
                        end;
                     else
                        Result.Skips (Skip_Last) :=
                          (Label             => Nested,
                           Element           => Skip_Node,
                           List_Root         => Skip_Node,
                           List_ID           => +rhs_item_list_ID,
                           Element_ID        => +rhs_element_ID,
                           Separator_ID      => Invalid_Token_ID,
                           Multi_Element_RHS => 1);

                        Skip_Last := Skip_Last - 1;
                     end if;

                  when rhs_element_ID =>
                     declare
                        List_Node : Valid_Node_Index := Tree.Find_Ancestor
                          (Skip_Node, (+rhs_ID, +rhs_alternative_list_ID));
                     begin

                        if Result.Skips'Length = 0 and then
                          Tree.ID (List_Node) = +rhs_ID
                        then
                           Non_Empty_List := List_Root (Tree, Skip_Node, +rhs_item_list_ID);
                           Skip_Last      := Skip_Last - 1;
                           exit;
                        end if;

                        List_Node := List_Root (Tree, List_Node, +rhs_alternative_list_ID);
                        Skip_Node := Tree.Find_Ancestor (Skip_Node, +rhs_element_ID);

                        Search_For := +rhs_item_list_ID;

                        if Result.Skips'Length = 0 then
                           if Skip_Last = Positive_Index_Type'First then
                              --  This list will be empty; no need to descend into it
                              Last_Skip_Node   := Skip_Node;
                              Reset_Search_For := Search_For;
                           else
                              Skip_Last := Skip_Last + 1;
                           end if;
                        else
                           Result.Skips (Skip_Last) :=
                             (Label             => Nested,
                              Element           => Skip_Node,
                              List_Root         => List_Node,
                              List_ID           => +rhs_alternative_list_ID,
                              Element_ID        => +rhs_item_list_ID,
                              Separator_ID      => +BAR_ID,
                              Multi_Element_RHS => 1);

                           Skip_Last := Skip_Last - 1;
                        end if;
                     end;
                  when others =>
                     raise SAL.Programmer_Error;
                  end case;

               end loop;
            end Search;

            Result_1 : Skip_Info (Skip_Last => Positive_Index_Type'First - 1);
         begin
            --  First count the number of Skip_Items we need, and set
            --  Non_Empty_List.
            Search (Result_1);

            declare
               Result : Skip_Info (Skip_Last);
            begin
               if Result.Skips'Length = 0 then
                  return Result;
               end if;

               Result.Start_List_Root  := Non_Empty_List;
               Result.Start_List_ID    := +rhs_item_list_ID;
               Result.Start_Element_ID := +rhs_element_ID;

               Result.Start_Separator_ID      := Invalid_Token_ID;
               Result.Start_Multi_Element_RHS := 1;

               Result.Skips (Skip_Last) := (Skip, Last_Skip_Node);

               if Result.Skips'Length = 1 then
                  return Result;
               end if;

               Search (Result);
               return Result;
            end;
         end Find_Skips;

         Container : Valid_Node_Index := Tree.Find_Ancestor (B, (+rhs_ID, +rhs_alternative_list_ID));
         Container_ID : WisiToken.Token_ID := Tree.ID (Container);

         Container_List : Syntax_Trees.LR_Utils.List :=
           (if Container_ID = +rhs_ID
            then Create_From_Element
              (Tree,
               Element      => Container,
               List_ID      => +rhs_list_ID,
               Element_ID   => +rhs_ID,
               Separator_ID => +BAR_ID)
            else Create_List
              (Tree,
               Root         => List_Root (Tree, Container, +rhs_alternative_list_ID),
               List_ID      => +rhs_alternative_list_ID,
               Element_ID   => +rhs_item_list_ID,
               Separator_ID => +BAR_ID));

      begin
         if Trace_Generate_EBNF > Extra then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Insert_Optional_RHS start:");
            Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Container);
         end if;

         declare
            Skip_List : constant Skip_Info := Find_Skips;

            New_RHS_AC   : Node_Index := Invalid_Node_Index;
            Is_Duplicate : Boolean    := False;
         begin
            if WisiToken.Trace_Generate_EBNF > Extra then
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line ("skip: " & Image (Skip_List, Wisitoken_Grammar_Actions.Descriptor));
            end if;

            if Skip_List.Skips'Length = 0 or else
              +rhs_ID = Tree.ID (Tree.Parent (Skip_List.Start_List_Root))
            then
               --  Insert an edited rhs into the rhs_list.
               --
               --  We can't insert an empty rhs_item_list into an
               --  rhs_alterative_list, so we insert an empty rhs.

               if Container_ID = +rhs_alternative_list_ID then

                  Container := Tree.Find_Ancestor (B, +rhs_ID);

                  Container_ID := +rhs_ID;

                  Container_List := Create_From_Element
                    (Tree,
                     Element      => Container,
                     List_ID      => +rhs_list_ID,
                     Element_ID   => +rhs_ID,
                     Separator_ID => +BAR_ID);
               end if;

               if Skip_List.Skips'Length = 0 then
                  --  New rhs is empty; no rhs_item_list
                  null;
               else
                  New_RHS_AC := Copy_Skip_Nested (Skip_List, Tree);
               end if;

               if Duplicate (Container_List, New_RHS_AC) then
                  Is_Duplicate := True;
               else
                  if Skip_List.Skips'Length = 0 then
                     Insert_Empty_RHS (Container_List, Container);
                  else
                     Insert_RHS (Container_List, New_RHS_AC, After => Container);
                  end if;
               end if;

            else
               --  Insert an edited rhs_item_list into an rhs_alternative_list

               New_RHS_AC := Copy_Skip_Nested (Skip_List, Tree);

               if Duplicate (Container_List, New_RHS_AC) then
                  --  IMPROVEME: check for duplicate before do copy; requires version of
                  --  Get_Text that understands Skip_Info
                  Is_Duplicate := True;
               else
                  declare
                     After : Valid_Node_Index := B;
                  begin
                     loop
                        After := List_Root (Tree, Tree.Find_Ancestor (After, +rhs_item_list_ID), +rhs_item_list_ID);
                        exit when Container_List.Contains (After);
                     end loop;

                     Container_List.Insert
                       (New_Element => New_RHS_AC,
                        After       => Container_List.To_Cursor (After));
                  end;
               end if;
            end if;

            if Trace_Generate_EBNF > Detail then
               Ada.Text_IO.New_Line;
               if Is_Duplicate then
                  Ada.Text_IO.Put_Line ("Insert_Optional_RHS duplicate '" & Get_Text (Data, Tree, New_RHS_AC) & "'");
               else
                  if Container_ID = +rhs_ID then
                     Ada.Text_IO.Put_Line ("Insert_Optional_RHS old rhs, new rhs:");
                     Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Container_List.Root);
                  else
                     Ada.Text_IO.Put_Line ("Insert_Optional_RHS edited rhs_alternative_list:");
                     Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Tree.Parent (Container_List.Root, 1));
                  end if;
               end if;
            end if;

            if not (Skip_List.Skips'Length = 0 or Is_Duplicate) then
               Record_Copied_EBNF_Nodes (New_RHS_AC);
            end if;
         end;
         return Container_List.Root;
      end Insert_Optional_RHS;

      procedure Add_Compilation_Unit (Label : in String; Unit : in Valid_Node_Index; Prepend : in Boolean := False)
      with Pre => Tree.ID (Unit) in +declaration_ID | +nonterminal_ID
      is
         use WisiToken.Syntax_Trees.LR_Utils;

         List : Syntax_Trees.LR_Utils.List := Creators.Create_List
           (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID, Invalid_Token_ID);

         Comp_Unit : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+compilation_unit_ID, (if Tree.ID (Unit) = +declaration_ID then 0 else 1)),
            (1 => Unit));

         function Equal
           (Target    : in String;
            List      : in LR_Utils.Constant_List'Class;
            Comp_Unit : in Valid_Node_Index)
           return Boolean
         is
            pragma Unreferenced (List);
            Decl : constant Valid_Node_Index := Tree.Child (Comp_Unit, 1);
         begin
            return Tree.ID (Decl) = +declaration_ID and then Target =
              (case Tree.RHS_Index (Decl) is
               when 0      => Get_Text (Data, Tree, Tree.Child (Decl, 3)),
               when 2 | 3  => Get_Text (Data, Tree, Tree.Child (Decl, 2)),
               when others => "");
         end Equal;

      begin
         if Prepend then
            --  Prepend is true for keywords, which must be declared before they
            --  are used. We put them all after the %meta_syntax declaration, to
            --  closer match the likely original EBNF layout.
            declare
               Meta_Syntax : constant Cursor := List.Find ("meta_syntax", Equal'Unrestricted_Access);
            begin
               List.Insert (Comp_Unit, After => Meta_Syntax);
            end;
         else
            List.Append (Comp_Unit);
         end if;

         if Trace_Generate_EBNF > Extra then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("new " & Label & ":" & Comp_Unit'Image & ": '" & Get_Text (Data, Tree, Unit) & "'");
         end if;
      end Add_Compilation_Unit;

      function To_RHS_List (RHS_Element : in Valid_Node_Index) return Valid_Node_Index
      with Pre => Tree.ID (RHS_Element) = +rhs_element_ID
      is
         RHS_Item_List : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_item_list_ID, 0), (1 => RHS_Element));
         RHS           : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 1),           (1 => RHS_Item_List));
      begin
         return Tree.Add_Nonterm ((+rhs_list_ID, 0), (1 => RHS));
      end To_RHS_List;

      function Convert_RHS_Alternative (Content : in Valid_Node_Index) return Valid_Node_Index
      with Pre => Tree.ID (Content) = +rhs_alternative_list_ID
      is
         --  Convert rhs_alternative_list rooted at Content to an rhs_list
         Node : Valid_Node_Index := Content;
      begin
         loop
            exit when Tree.RHS_Index (Node) = 0;

            --  current tree:
            --  rhs_alternative_list : Node
            --  | rhs_alternative_list: Node.Child (1)
            --  | |  ...
            --  | BAR: Node.child (2)
            --  | rhs_item_list: Node.Child (3)

            --  new tree:
            --  rhs_list: Node
            --  | rhs_alternative_list: keep Node.Child (1)
            --  | |  ...
            --  | BAR: keep
            --  | rhs: new
            --  | | rhs_item_list: keep Node,Child (3)

            if not Tree.Has_Children (Tree.Child (Node, 3)) then
               --  Convert empty rhs_item_list to empty rhs
               Tree.Set_Children
                 (Tree.Child (Node, 3),
                  (+rhs_ID, 0),
                  (1 .. 0 => Invalid_Node_Index));

               Tree.Set_Children
                 (Node,
                  (+rhs_list_ID, 1),
                  (1 => Tree.Child (Node, 1),
                   2 => Tree.Child (Node, 2),
                   3 => Tree.Child (Node, 3)));
            else
               Tree.Set_Children
                 (Node,
                  (+rhs_list_ID, 1),
                  (1 => Tree.Child (Node, 1),
                   2 => Tree.Child (Node, 2),
                   3 => Tree.Add_Nonterm
                     ((+rhs_ID, 1),
                      (1 => Tree.Child (Node, 3)))));
            end if;

            Node := Tree.Child (Node, 1);
         end loop;

         --  current tree:
         --  rhs_alternative_list : Node
         --  | rhs_item_list: Node.Child (1)

         --  new tree:
         --  rhs_list: Node
         --  | rhs: new
         --  | | rhs_item_list: Node.Child (1)

         Tree.Set_Children
           (Node,
            (+rhs_list_ID, 0),
            (1 => Tree.Add_Nonterm ((+rhs_ID, 1), (1 => Tree.Child (Node, 1)))));

         return Content;
      end Convert_RHS_Alternative;

      procedure New_Nonterminal
        (Label          : in String;
         New_Identifier : in Identifier_Index;
         Content        : in Valid_Node_Index)
      with Pre => To_Token_Enum (Tree.ID (Content)) in rhs_alternative_list_ID | rhs_element_ID
      is
         --  Convert subtree rooted at Content to an rhs_list contained by a new nonterminal
         --  named New_Identifier.
      begin
         declare
            New_Nonterm : constant Valid_Node_Index := Tree_Add_Nonterminal
              (Child_1   => Tree.Add_Identifier (+IDENTIFIER_ID, New_Identifier, Tree.Byte_Region (Content)),
               Child_2   => Tree.Add_Terminal (+COLON_ID),
               Child_3   =>
                 (case To_Token_Enum (Tree.ID (Content)) is
                  when rhs_element_ID          => To_RHS_List (Content),
                  when rhs_alternative_list_ID => Convert_RHS_Alternative (Content),
                  when others => raise SAL.Programmer_Error),
               Child_4   => Tree.Add_Nonterm
                 ((+semicolon_opt_ID, 0),
                  (1     => Tree.Add_Terminal (+SEMICOLON_ID))));
         begin
            Add_Compilation_Unit (Label & New_Identifier'Image, New_Nonterm);
         end;
      end New_Nonterminal;

      procedure New_Nonterminal_List_1
        (List_Nonterm  : in Identifier_Index;
         RHS_Element_1 : in Valid_Node_Index;
         RHS_Element_3 : in Valid_Node_Index;
         Byte_Region   : in Buffer_Region)
      with Pre => Tree.ID (RHS_Element_1) = +rhs_element_ID and
                  Tree.ID (RHS_Element_3) = +rhs_element_ID
      is
         --  nonterminal: foo_list
         --  | IDENTIFIER: "foo_list" List_Nonterm
         --  | COLON:
         --  | rhs_list:
         --  | | rhs_list: RHS_List_2
         --  | | | rhs: RHS_2
         --  | | | | rhs_item_list: RHS_Item_List_1
         --  | | | | | rhs_element: RHS_Element_1
         --  | | | | | | rhs_item: RHS_Item_1
         --  | | | | | | | IDENTIFIER: List_Element
         --  | | BAR:
         --  | | rhs: RHS_3
         --  | | | rhs_item_list: RHS_Item_List_2
         --  | | | | | rhs_item_list: RHS_Item_List_3
         --  | | | | | |  rhs_element: RHS_Element_2
         --  | | | | | | | rhs_item: RHS_Item_2
         --  | | | | | | | | IDENTIFIER: List_Nonterm
         --  | | | | rhs_element: RHS_Element_3
         --  | | | | | rhs_item: RHS_Item_3
         --  | | | | | | IDENTIFIER: List_Element
         --  | semicolon_opt:
         --  | | SEMICOLON:

         RHS_Item_2 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 0), (1 => Tree.Add_Identifier (+IDENTIFIER_ID, List_Nonterm, Byte_Region)));

         RHS_Element_2 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_element_ID, 0), (1 => RHS_Item_2));

         RHS_Item_List_1 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_item_list_ID, 0), (1 => RHS_Element_1));
         RHS_Item_List_3 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_item_list_ID, 0), (1 => RHS_Element_2));
         RHS_Item_List_2 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_list_ID, 1), (1 => RHS_Item_List_3, 2 => RHS_Element_3));

         RHS_2 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 1), (1 => RHS_Item_List_1));
         RHS_3 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_ID, 1), (1 => RHS_Item_List_2));

         Bar_1 : constant Valid_Node_Index := Tree.Add_Terminal (+BAR_ID);

         RHS_List_2 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_list_ID, 0), (1 => RHS_2));

         List_Nonterminal : constant Valid_Node_Index := Tree_Add_Nonterminal
           (Child_1   => Tree.Add_Identifier (+IDENTIFIER_ID, List_Nonterm, Byte_Region),
            Child_2   => Tree.Add_Terminal (+COLON_ID),
            Child_3   => Tree.Add_Nonterm
              ((+rhs_list_ID, 1),
               (1     => RHS_List_2,
                2     => Bar_1,
                3     => RHS_3)),
            Child_4   => Tree.Add_Nonterm
              ((+semicolon_opt_ID, 0),
               (1     => Tree.Add_Terminal (+SEMICOLON_ID))));
      begin
         Add_Compilation_Unit ("canonical list" & List_Nonterm'Image, List_Nonterminal);
      end New_Nonterminal_List_1;

      procedure New_Nonterminal_List
        (List_Nonterm : in Identifier_Index;
         List_Element : in Identifier_Index;
         Byte_Region  : in Buffer_Region)
      is
         RHS_Item_1 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 0), (1 => Tree.Add_Identifier (+IDENTIFIER_ID, List_Element, Byte_Region)));
         RHS_Item_3 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 0), (1 => Tree.Add_Identifier (+IDENTIFIER_ID, List_Element, Byte_Region)));
         RHS_Element_1 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_element_ID, 0), (1 => RHS_Item_1));
         RHS_Element_3 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_element_ID, 0), (1 => RHS_Item_3));
      begin
         New_Nonterminal_List_1 (List_Nonterm, RHS_Element_1, RHS_Element_3, Byte_Region);
      end New_Nonterminal_List;

      procedure New_Nonterminal_List
        (List_Nonterm : in Identifier_Index;
         List_Element : in Token_Index;
         Terminals    : in Base_Token_Arrays.Vector;
         Byte_Region  : in Buffer_Region)
      is
         RHS_Item_1 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 0), (1 => Tree.Add_Terminal (List_Element, Terminals)));
         RHS_Item_3 : constant Valid_Node_Index := Tree.Add_Nonterm
           ((+rhs_item_ID, 0), (1 => Tree.Add_Terminal (List_Element, Terminals)));
         RHS_Element_1 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_element_ID, 0), (1 => RHS_Item_1));
         RHS_Element_3 : constant Valid_Node_Index := Tree.Add_Nonterm ((+rhs_element_ID, 0), (1 => RHS_Item_3));
      begin
         New_Nonterminal_List_1 (List_Nonterm, RHS_Element_1, RHS_Element_3, Byte_Region);
      end New_Nonterminal_List;

      procedure Copy_Non_Grammar
        (From : in Valid_Node_Index;
         To   : in Valid_Node_Index)
      is
         From_Aug : constant Base_Token_Class_Access := Tree.Augmented (From);
      begin
         if From_Aug /= null then
            declare
               New_Aug : constant Augmented_Token_Access := new Augmented_Token'
                 (ID          => Tree.ID (From),
                  Tree_Index  => To,
                  Non_Grammar => Augmented_Token_Access (From_Aug).Non_Grammar,
                  others => <>);
            begin
               Tree.Set_Augmented (To, Base_Token_Class_Access (New_Aug));
            end;
         end if;
      end Copy_Non_Grammar;

      procedure Translate_RHS_Group_Item (Node : in Valid_Node_Index)
      is
         --  Current tree:
         --
         --  rhs_element: Parent (Node, 2)
         --  | rhs_item: Parent (Node, 1)
         --  | | rhs_group_item: Node
         --  | | | LEFT_PAREN
         --  | | | rhs_alternative_list: Child (Node, 2)
         --  | | | RIGHT_PAREN

         use Syntax_Trees.LR_Utils;

         Element_Content  : constant String           := Get_Text (Data, Tree, Tree.Child (Node, 2));
         Right_Paren_Node : constant Valid_Node_Index := Tree.Child (Node, 3);
         List             : constant Constant_List    := Creators.Create_List
           (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
         Name_Node        : Node_Index;
         New_Ident        : Base_Identifier_Index     := Invalid_Identifier_Index;
      begin
         --  See if there's an existing nonterminal for this content.
         for N of List loop

            if Tree.Production_ID (Tree.Child (N, 1)) = (+nonterminal_ID, 0) then
               --  Target nonterm is:
               --
               --  (compilation_unit_1, (111 . 128))
               --  | (nonterminal_0, (111 . 128))
               --  | |  7;(IDENTIFIER, (111 . 128))
               --  | | (COLON)
               --  | | (rhs_list_1, (111 . 128))
               --  | | | ...
               declare
                  RHS_List_1 : constant Node_Index := Tree.Child (Tree.Child (N, 1), 3);
               begin
                  if RHS_List_1 /= Invalid_Node_Index and then
                    Element_Content = Get_Text (Data, Tree, RHS_List_1)
                  then
                     Name_Node := Tree.Child (Tree.Child (N, 1), 1);
                     case Tree.Label (Name_Node) is
                     when Shared_Terminal =>
                        New_Ident := New_Identifier (Get_Text (Data, Tree, Name_Node));
                     when Virtual_Identifier =>
                        New_Ident := Tree.Identifier (Name_Node);
                     when others =>
                        Raise_Programmer_Error ("process_node rhs_group_item", Data, Tree, Name_Node);
                     end case;

                     exit;
                  end if;
               end;
            end if;
         end loop;

         if New_Ident = Invalid_Identifier_Index then
            New_Ident := Next_Nonterm_Name;
            New_Nonterminal ("group item", New_Ident, Tree.Child (Node, 2));
         else
            Erase_Deleted_EBNF_Nodes (Tree.Child (Node, 2));
         end if;

         Tree.Set_Node_Identifier (Node, +IDENTIFIER_ID, New_Ident);
         Copy_Non_Grammar (Right_Paren_Node, Node);
         Tree.Set_Children (Tree.Parent (Node), (+rhs_item_ID, 0), (1 => Node));
         Clear_EBNF_Node (Node);
      end Translate_RHS_Group_Item;

      procedure Translate_RHS_Multiple_Item (Node : in Valid_Node_Index)
      is
         --  We have one of:
         --
         --  | a { b }  c
         --  | a { b } - c
         --  | a ( b ) + c
         --  | a ( b ) * c
         --  | a b+ c
         --  | a b* c
         --
         --  where a and/or c can be empty. Replace it with a new canonical
         --  list nonterminal:
         --
         --  nonterminal_nnn_list
         --  : b
         --  | nonterminal_nnn_list b
         --
         --  and a second RHS if it can be empty:
         --  | a c

         --  Current tree:
         --
         --  rhs_element : Parent (Node, 2)
         --  | rhs_item: Parent (Node, 1)
         --  | | rhs_multiple_item: Node
         --  | | | LEFT_BRACE | LEFT_PAREN
         --  | | | rhs_alternative_list
         --  | | | ...
         --  | | | RIGHT_BRACE | RIGHT_PAREN
         --  | | | [MINUS | PLUS | STAR]

         --  or:
         --
         --  rhs_element : Parent (Node, 2)
         --  | rhs_item: Parent (Node, 1)
         --  | | rhs_multiple_item: Node
         --  | | | IDENTIFIER
         --  | | | PLUS | STAR

         Done                       : Boolean                   := False;
         Parent_RHS_Item            : constant Valid_Node_Index := Tree.Parent (Node);
         List_Nonterm_Virtual_Name  : Base_Identifier_Index     := Invalid_Identifier_Index;
         List_Nonterm_Terminal_Name : Base_Token_Index          := Invalid_Token_Index;

         procedure Check_Canonical_List
         is
            --  In EBNF, a canonical list with a separator looks like:
            --
            --  enumConstants : enumConstant (',' enumConstant)* ;
            --
            --  or, with no separator:
            --
            --  SwitchLabels : SwitchLabel {SwitchLabel} ;
            --
            --  where Node is the rhs_multiple_item containing "(','
            --  enumConstant)*" or "{SwitchLabel}".
            --
            --  The tokens may have labels.
            --
            --  Handling these cases specially eliminates a conflict between
            --  reducing to enumConstants and reducing to the introduced nonterm
            --  list.
            --
            --  Alternately, the no separator case can be:
            --
            --  enumConstants : enumConstant+ ;
            --
            --  Handling this no separator case specially would not eliminate any conflicts.

            use Syntax_Trees.LR_Utils;
            use Syntax_Trees.LR_Utils.Creators;
            use all type Ada.Containers.Count_Type;

            List_Name_Node : constant Valid_Node_Index := Tree.Find_Ancestor (Node, +nonterminal_ID);
            RHS_List_Root  : constant Valid_Node_Index := Tree.Child (List_Name_Node, 3);

            RHS_2 : constant Valid_Node_Index := Tree.Find_Ancestor
              (Node, (+rhs_ID, +rhs_alternative_list_ID));
            --  If rhs_ID, the RHS containing the canonical list candidate.
            --  If rhs_alternative_list_ID, not useful (FIXME: actually a canonical list candidate)

            RHS_2_Item_List_List : constant Constant_List :=
              (if Tree.ID (RHS_2) = +rhs_ID
               then Create_List (Tree, Tree.Child (RHS_2, 1), +rhs_item_list_ID, +rhs_element_ID)
               else Invalid_List (Tree));

            Alt_List_List : constant Constant_List :=
              (case Tree.RHS_Index (Node) is
               when 0 | 3 =>
                  Create_List (Tree, Tree.Child (Node, 2), +rhs_alternative_list_ID, +rhs_item_list_ID),
               when others => Invalid_List (Tree));
            --  Iterator on the rhs_alternative_list of the rhs_multiple_item.

            Alt_List_Item_List : constant Constant_List :=
              (if Alt_List_List.Is_Invalid
               then Invalid_List (Tree)
               else Create_List (Tree, Get_Node (Alt_List_List.First), +rhs_item_list_ID, +rhs_element_ID));
            --  Iterator on the content of the rhs_multiple_item. Note that we
            --  don't support a non-empty multiple_item; a canonical list can be
            --  empty.

            RHS_2_Item_List_Iter : constant Constant_Iterator := RHS_2_Item_List_List.Iterate_Constant;

            Element_2 : constant Cursor :=
              (if Is_Invalid (RHS_2_Item_List_List)
               then No_Element
               else RHS_2_Item_List_List.To_Cursor (Tree.Parent (Node, 2)));
            --  The rhs_element containing the rhs_multiple_item

            Element_1 : constant Node_Index :=
              (if Is_Invalid (RHS_2_Item_List_List)
               then Invalid_Node_Index
               else Get_Node (RHS_2_Item_List_Iter.Previous (Element_2)));
            --  The list element
         begin
            if Tree.ID (RHS_2) = +rhs_alternative_list_ID or else
              Create_List (Tree, RHS_List_Root, +rhs_list_ID, +rhs_ID).Count /= 1
            then
               --  Something else going on
               return;
            end if;
            pragma Assert (Tree.ID (RHS_2) = +rhs_ID);

            if RHS_2_Item_List_List.Count = 2 and then
              (Tree.RHS_Index (Node) in 4 .. 5 or else
                 Alt_List_Item_List.Count in 1 .. 2)
            then
               null;
            else
               return;
            end if;

            if Element_1 = Invalid_Node_Index or else
              Get_Text (Data, Tree, Tree.Find_Descendant (Element_1, +rhs_item_ID)) /=
              Get_Text (Data, Tree, Tree.Find_Descendant (Get_Node (Alt_List_Item_List.Last), +rhs_item_ID))
            then
               return;
            end if;

            if Trace_Generate_EBNF > Detail then
               Ada.Text_IO.Put_Line ("canonical list");
            end if;

            --  We have a canonical list declaration. Rewrite it to:
            --
            --  with separator:
            --  rhs_list: keep
            --  | rhs_list:
            --  | | rhs: new, RHS_1
            --  | | | rhs_item_list: new, RHS_Item_List_1
            --  | | | | rhs_element: keep, Element_1
            --  | | | | | rhs_item: keep
            --  | | | | | | IDENTIFIER: keep; element name
            --  | BAR: new
            --  | rhs: keep, RHS_2
            --  | | rhs_item_list: new, RHS_Item_List_2
            --  | | | rhs_item_list: keep, rhs_item_list_3
            --  | | | | rhs_item_list: keep, rhs_item_list_4
            --  | | | | | rhs_element: new
            --  | | | | | | rhs_item: new
            --  | | | | | | | IDENTIFIER: new, list name
            --  | | | | rhs_element: keep
            --  | | | | | rhs_item: keep
            --  | | | | | | IDENTIFIER: keep, separator
            --  | | | rhs_element: keep, alt_list_elements (last)
            --  | | | | rhs_item: keep
            --  | | | | | IDENTIFIER: keep, element name
            --
            --  no separator:
            --  rhs_list: keep
            --  | rhs_list:
            --  | | rhs: new, RHS_1
            --  | | | rhs_item_list: new, RHS_Item_List_1
            --  | | | | rhs_element: keep, Element_1
            --  | | | | | rhs_item: keep
            --  | | | | | | IDENTIFIER: keep; element name
            --  | BAR: new
            --  | rhs: keep, RHS_2
            --  | | rhs_item_list: keep, rhs_item_list_3
            --  | | | rhs_item_list: new, rhs_item_list_4
            --  | | | | rhs_element: new
            --  | | | | | rhs_item: new
            --  | | | | | | IDENTIFIER: new, list name
            --  | | | rhs_element: keep, alt_list_elements (last)
            --  | | | | rhs_item: keep
            --  | | | | | IDENTIFIER: keep, element name

            declare
               List_Name_Tok    : constant Token_Index      := Tree.First_Shared_Terminal (List_Name_Node);
               List_Name_Region : constant Buffer_Region    := Data.Terminals.all (List_Name_Tok).Byte_Region;
               List_Name        : constant String           := Data.Grammar_Lexer.Buffer_Text (List_Name_Region);

               RHS_2_Index    : constant Integer       := Tree.RHS_Index (RHS_2);
               RHS_2_Children : Valid_Node_Index_Array := Tree.Children (RHS_2);

               RHS_1_Item_List    : constant Valid_Node_Index := Tree.Add_Nonterm
                 ((+rhs_item_list_ID, 0), (1 => Element_1));

               RHS_1_Action : constant Node_Index :=
                 (case RHS_2_Index is
                  when 2 | 3 => Tree.Add_Terminal
                    (Tree.First_Shared_Terminal (RHS_2_Children (2)), Data.Terminals.all),
                  when others => Invalid_Node_Index);

               RHS_1_Check : constant Node_Index :=
                 (case RHS_2_Index is
                  when 3 => Tree.Add_Terminal
                    (Tree.First_Shared_Terminal (RHS_2_Children (3)), Data.Terminals.all),
                  when others => Invalid_Node_Index);

               RHS_1              : constant Valid_Node_Index :=
                 (case RHS_2_Index is
                  when 1 => Tree.Add_Nonterm ((+rhs_ID, 1), (1 => RHS_1_Item_List)),
                  when 2 => Tree.Add_Nonterm ((+rhs_ID, 2), (1 => RHS_1_Item_List, 2 => RHS_1_Action)),
                  when 3 => Tree.Add_Nonterm
                    ((+rhs_ID, 3), (1 => RHS_1_Item_List, 2 => RHS_1_Action, 3 => RHS_1_Check)),
                  when others => raise SAL.Programmer_Error);

               Bar                   : constant Valid_Node_Index := Tree.Add_Terminal (+BAR_ID);
               RHS_Item_List_3       : constant Valid_Node_Index := Tree.Child (RHS_2, 1);
               RHS_Item_List_4       : constant Valid_Node_Index := Tree.Child (RHS_Item_List_3, 1);
               New_List_Name_Term    : constant Valid_Node_Index := Tree.Add_Terminal
                 (List_Name_Tok, Data.Terminals.all);
               New_List_Name_Item    : constant Valid_Node_Index := Tree.Add_Nonterm
                 ((+rhs_item_ID, 0),
                  (1      => New_List_Name_Term));

               New_List_Name_Label : constant Node_Index :=
                 (if Tree.RHS_Index (Element_1) = 1
                  then --  tokens have labels
                     Tree.Add_Identifier (+IDENTIFIER_ID, New_Identifier (List_Name), List_Name_Region)
                  else Invalid_Node_Index);

               New_List_Name_Element : constant Valid_Node_Index :=
                 (if Tree.RHS_Index (Element_1) = 1
                  then --  tokens have labels
                     Tree.Add_Nonterm
                       ((+rhs_element_ID, 1),
                        (1 => New_List_Name_Label,
                         2 => Tree.Add_Terminal (+EQUAL_ID),
                         3 => New_List_Name_Item))
                  else
                     Tree.Add_Nonterm ((+rhs_element_ID, 0), (1 => New_List_Name_Item)));

               Alt_List_Elements : constant Valid_Node_Index_Array := Tree.Get_IDs (Node, +rhs_element_ID);
               RHS_Item_List_2   : constant Node_Index       :=
                 (if Alt_List_Elements'Last = 1
                  then Invalid_Node_Index -- no separator
                  else Tree.Add_Nonterm
                    ((+rhs_item_list_ID, 1),
                     (1 => RHS_Item_List_3,
                      2 => Alt_List_Elements (Alt_List_Elements'Last))));

            begin
               Tree.Set_Children (RHS_Item_List_4, (+rhs_item_list_ID, 0), (1 => New_List_Name_Element));

               Tree.Set_Children
                 (RHS_Item_List_3,
                  (+rhs_item_list_ID, 1),
                  (1 => RHS_Item_List_4,
                   2 => Alt_List_Elements (1)));

               RHS_2_Children (1) :=
                 (if Alt_List_Elements'Last = 1
                  then RHS_Item_List_3 -- no separator
                  else RHS_Item_List_2);
               Tree.Set_Children (RHS_2, (+rhs_ID, Tree.RHS_Index (RHS_2)), RHS_2_Children);

               Tree.Set_Children
                 (Tree.Parent (RHS_2),
                  (+rhs_list_ID, 1),
                  (1 => Tree.Add_Nonterm ((+rhs_list_ID, 0), (1 => RHS_1)),
                   2 => Bar,
                   3 => RHS_2));
            end;

            Done := True;

            Clear_EBNF_Node (Node);

            if Trace_Generate_EBNF > Extra then
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line ("Check_Canonical_List edited rhs_list:");
               Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Tree.Parent (RHS_2));
            end if;
         end Check_Canonical_List;

         procedure Find_List_Nonterminal_1 (Element_Content : in String)
         is
            --  Search for a nonterm (virtual or not) implementing a list for
            --  Element_Content, which is a single rhs_element; no List_Element
            --  Nonterminal. If found, set List_Nonterm_Virtual_Name or
            --  List_Nonterm_Terminal_Name
            use Syntax_Trees.LR_Utils;

            List : constant Constant_List := Creators.Create_List
              (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
         begin
            for N of List loop

               if Tree.Production_ID (Tree.Child (N, 1)) = (+nonterminal_ID, 0) then
                  --  Target List_Nonterm is:
                  --
                  --  nonterminal_nnn_list
                  --     : element
                  --     | nonterminal_nnn_list element
                  --
                  --  compilation_unit
                  --  | nonterminal
                  --  | | IDENTIFIER : list_nonterm
                  --  | | COLON
                  --  | | rhs_list: rhs_list_1
                  --  | | | rhs_list: rhs_list_2
                  --  | | | | rhs
                  --  | | | | | ... List_element
                  --  | | | BAR
                  --  | | | rhs: ... list_nonterm list_element
                  declare
                     Name_Node  : constant Node_Index := Tree.Child (Tree.Child (N, 1), 1);
                     RHS_List_1 : constant Node_Index := Tree.Child (Tree.Child (N, 1), 3);
                     RHS_List_2 : constant Node_Index :=
                       (if RHS_List_1 = Invalid_Node_Index
                        then Invalid_Node_Index
                        else Tree.Child (RHS_List_1, 1));
                  begin
                     if RHS_List_2 /= Invalid_Node_Index and
                       Tree.Child (RHS_List_1, 3) /= Invalid_Node_Index and -- second rhs present
                       Tree.Child (RHS_List_2, 3) = Invalid_Node_Index -- no third rhs
                     then
                        declare
                           RHS_1 : constant String := Get_Text (Data, Tree, RHS_List_2);
                           RHS_2 : constant String := Get_Text (Data, Tree, Tree.Child (RHS_List_1, 3));
                           Expected_RHS_2 : constant String := Get_Text (Data, Tree, Name_Node) & " " &
                             Element_Content;
                        begin
                           if Element_Content = RHS_1 and RHS_2 = Expected_RHS_2 then
                              case Tree.Label (Name_Node) is
                              when Shared_Terminal =>
                                 List_Nonterm_Terminal_Name := Tree.First_Shared_Terminal (Name_Node);
                              when Virtual_Identifier =>
                                 List_Nonterm_Virtual_Name := Tree.Identifier (Name_Node);
                              when others =>
                                 Raise_Programmer_Error
                                   ("unimplemented Find_List_Nonterminal_1 case '" & Element_Content & "'",
                                    Data, Tree, Name_Node);
                              end case;

                              exit;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end loop;
         end Find_List_Nonterminal_1;

         procedure Find_List_Nonterminal_2 (Element_Content : in String)
         is
            --  Look for a pair of nonterms implementing a list of Element_Content.
            --  If found, set List_Nonterm_*_Name
            use Syntax_Trees.LR_Utils;

            List : constant Constant_List := Creators.Create_List
              (Tree, Tree.Child (Tree.Root, 1), +compilation_unit_list_ID, +compilation_unit_ID);
         begin
            for Comp_Unit of List loop
               declare
                  Nonterm : constant Valid_Node_Index := Tree.Child (Comp_Unit, 1);
               begin
                  if Tree.Production_ID (Nonterm) = (+nonterminal_ID, 0) and then
                    Element_Content = Get_Text (Data, Tree, Tree.Child (Nonterm, 3))
                  then
                     Find_List_Nonterminal_1 (Get_Text (Data, Tree, Tree.Child (Nonterm, 1)));
                     exit;
                  end if;
               end;
            end loop;
         end Find_List_Nonterminal_2;

         Container_List_Root : Node_Index := Invalid_Node_Index;
      begin
         --  Check if this is a recognized pattern
         Check_Canonical_List;
         if Done then return; end if;

         --  Check to see if there is an already declared nonterminal
         --  list with the same content; if not, create one.
         case Tree.RHS_Index (Node) is
         when 0 .. 3 =>
            --  0: { rhs_alternative_list }
            --  1: { rhs_alternative_list } -
            --  2: ( rhs_alternative_list ) +
            --  3: ( rhs_alternative_list ) *

            if Tree.RHS_Index (Node) in 0 | 3 then
               Container_List_Root := Insert_Optional_RHS (Node);
            end if;

            if 0 = Tree.RHS_Index (Tree.Child (Node, 2)) and then
              0 = Tree.RHS_Index (Tree.Child (Tree.Child (Node, 2), 1))
            then
               --  Only one element in the rhs_alternative_list, and in the rhs_item_list
               Find_List_Nonterminal_1 (Get_Text (Data, Tree, Tree.Child (Node, 2)));

               if List_Nonterm_Virtual_Name = Invalid_Identifier_Index and
                 List_Nonterm_Terminal_Name = Invalid_Token_Index
               then
                  List_Nonterm_Virtual_Name := Next_Nonterm_Name ("_list");
                  New_Nonterminal_List
                    (List_Nonterm_Virtual_Name, Tree.First_Shared_Terminal (Tree.Child (Node, 2)),
                     Data.Terminals.all, Tree.Byte_Region (Node));
               else
                  Erase_Deleted_EBNF_Nodes (Tree.Child (Node, 2));
               end if;
            else
               Find_List_Nonterminal_2 (Get_Text (Data, Tree, Tree.Child (Node, 2)));

               if List_Nonterm_Virtual_Name = Invalid_Identifier_Index then
                  List_Nonterm_Virtual_Name := Next_Nonterm_Name ("_list");
                  declare
                     List_Element_Virtual_Name : constant Identifier_Index := Next_Nonterm_Name;
                  begin
                     New_Nonterminal ("canonical list element", List_Element_Virtual_Name, Tree.Child (Node, 2));
                     New_Nonterminal_List
                       (List_Nonterm_Virtual_Name, List_Element_Virtual_Name, Tree.Byte_Region (Node));
                  end;
               else
                  Erase_Deleted_EBNF_Nodes (Tree.Child (Node, 2));
               end if;
            end if;

         when 4 | 5 =>
            --  IDENTIFIER + | *
            Find_List_Nonterminal_1 (Get_Text (Data, Tree, Tree.Child (Node, 1)));

            if List_Nonterm_Virtual_Name = Invalid_Identifier_Index then
               List_Nonterm_Virtual_Name := Next_Nonterm_Name ("_list");

               New_Nonterminal_List
                 (List_Nonterm_Virtual_Name,
                  Tree.First_Shared_Terminal (Tree.Child (Node, 1)), Data.Terminals.all,
                  Tree.Byte_Region (Node));
            else
               --  nothing to erase
               null;
            end if;

            if Tree.RHS_Index (Node) = 5 then
               Container_List_Root := Insert_Optional_RHS (Node);
            end if;

         when others =>
            Raise_Programmer_Error ("Translate_RHS_Multiple_Item unimplemented", Data, Tree, Node);
         end case;

         --  Edit rhs_item to use list name
         declare
            Child : constant Valid_Node_Index :=
              (if List_Nonterm_Virtual_Name /= Invalid_Identifier_Index
               then Tree.Add_Identifier
                 (+IDENTIFIER_ID, List_Nonterm_Virtual_Name, Tree.Byte_Region (Parent_RHS_Item))
               elsif List_Nonterm_Terminal_Name /= Invalid_Token_Index
               then Tree.Add_Terminal (List_Nonterm_Terminal_Name, Data.Terminals.all)
               else raise SAL.Programmer_Error);
         begin
            Tree.Set_Children (Parent_RHS_Item, (+rhs_item_ID, 0), (1 => Child));
         end;

         Clear_EBNF_Node (Node);

         if Trace_Generate_EBNF > Extra then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Translate_RHS_Multiple_Item edited:");
            Tree.Print_Tree
              (Wisitoken_Grammar_Actions.Descriptor,
               (if Container_List_Root = Invalid_Node_Index
                then Parent_RHS_Item
                else Container_List_Root));
         end if;
      end Translate_RHS_Multiple_Item;

      procedure Translate_RHS_Optional_Item (B : in Valid_Node_Index)
      is
         --  Source looks like:
         --
         --  | A [B] C
         --
         --  where A, B, C are token sequences. All are contained in one
         --  rhs_item_list, which may be contained in an rhs or an
         --  rhs_alternative_list. B contains an rhs_alternative_list.
         --
         --  First add a second rhs_item_list without B:
         --  | A C
         --
         --  then for each alternative in B, splice together rhs_item_lists A,
         --  B_i, C, copying A, C on all after the first:
         --  | A B_i C
         --
         --  See nested_ebnf_optional.wy for an example of nested optional
         --  items.
         --
         --  We don't create a separate nonterminal for B, so token labels stay
         --  in the same RHS for actions.
         --
         --  current tree:
         --
         --  rhs_list:
         --  | rhs | rhs_alternative_list:
         --  | | rhs_item_list
         --  | | | rhs_item_list
         --  | | ...
         --  | | | | | rhs_element: a.last
         --  | | | | | | rhs_item:
         --  | | | | rhs_element:
         --  | | | | | rhs_item: contains b
         --  | | | | | | rhs_optional_item: B
         --  | | | | | | | LEFT_BRACKET: B.Children (1)
         --  | | | | | | | rhs_alternative_list: B.Children (2) b
         --  | | | | | | | RIGHT_BRACKET: B.Children (3)
         --  | | | rhs_element: c.first
         --  | | | | rhs_item:

         use Syntax_Trees.LR_Utils;
         use Syntax_Trees.LR_Utils.Creators;

         Container_List_Root : constant Valid_Node_Index := Insert_Optional_RHS (B);
      begin
         if Trace_Generate_EBNF > Extra then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Translate_RHS_Optional_Item start");
         end if;

         case Tree.RHS_Index (B) is
         when 0 | 1 =>
            --  : LEFT_BRACKET rhs_alternative_list RIGHT_BRACKET
            --  | LEFT_PAREN rhs_alternative_list RIGHT_PAREN QUESTION

            declare
               Container_List : Syntax_Trees.LR_Utils.List :=
                 (if Tree.ID (Container_List_Root) = +rhs_list_ID
                  then Create_List
                    (Tree,
                     Root         => Container_List_Root,
                     List_ID      => +rhs_list_ID,
                     Element_ID   => +rhs_ID,
                     Separator_ID => +BAR_ID)
                  else Create_List
                    (Tree,
                     Root         => Container_List_Root,
                     List_ID      => +rhs_alternative_list_ID,
                     Element_ID   => +rhs_item_list_ID,
                     Separator_ID => +BAR_ID));

               Container_Cur : Cursor := Container_List.Find
                 (if Container_List.Element_ID = +rhs_ID
                  then Tree.Find_Ancestor (B, +rhs_ID)
                  else List_Root (Tree, Tree.Find_Ancestor (B, +rhs_item_list_ID), +rhs_item_list_ID));

               ABC_List : List := Create_From_Element
                 (Tree, Tree.Parent (B, 2),
                  List_ID      => +rhs_item_list_ID,
                  Element_ID   => +rhs_element_ID,
                  Separator_ID => Invalid_Token_ID);

               ABC_Iter : constant Iterator := ABC_List.Iterate;

               ABC_B_Cur   : constant Cursor := ABC_List.To_Cursor (Tree.Parent (B, 2));
               ABC_A_Last  : constant Cursor := ABC_Iter.Previous (ABC_B_Cur);
               ABC_C_First : constant Cursor := ABC_Iter.Next (ABC_B_Cur);

               B_Alternative_List : constant Constant_List := Create_List
                 (Tree, Tree.Child (B, 2), +rhs_alternative_list_ID, +rhs_item_list_ID);

            begin
               --  An alternate design would be to splice together the existing A,
               --  B_i, C; but it's too hard to get all the parent updates right.
               for Alt of reverse B_Alternative_List loop

                  declare
                     B_Item_List : constant Constant_List := Create_List
                       (Tree, Alt, +rhs_item_list_ID, +rhs_element_ID);

                     New_ABC : List := Empty_List (ABC_List);
                  begin
                     if Has_Element (ABC_A_Last) then
                        Copy (Source_List => ABC_List,
                              Source_Last => ABC_A_Last,
                              Dest_List   => New_ABC);
                     end if;

                     Copy (B_Item_List, Dest_List => New_ABC);

                     if Has_Element (ABC_C_First) then
                        Copy (ABC_List, Source_First => ABC_C_First, Dest_List => New_ABC);
                     end if;

                     if Container_List.Element_ID = +rhs_ID then
                        Insert_RHS (Container_List, New_ABC.Root, After => Get_Node (Container_Cur));
                     else
                        Container_List.Insert (New_ABC.Root, After => Container_Cur);
                     end if;

                     Record_Copied_EBNF_Nodes (New_ABC.Root);
                  end;
               end loop;

               Erase_Deleted_EBNF_Nodes (Get_Node (Container_Cur));
               --  This includes B, so we don't do 'Clear_EBNF_Node (B)'.

               Container_List.Delete (Container_Cur);
            end;

         when 2 =>
            --  | IDENTIFIER QUESTION
            --
            --  Current tree:
            --   rhs_item_3
            --   | rhs_optional_item_2: B
            --   | | IDENTIFIER
            --   | | QUESTION
            --
            --  Change to:
            --   rhs_item_0
            --   | IDENTIFIER

            Tree.Set_Children (Tree.Parent (B), (+rhs_item_ID, 0), (1 => Tree.Child (B, 1)));
            Clear_EBNF_Node (B);

         when 3 =>
            --  | STRING_LITERAL_2 QUESTION
            Tree.Set_Children (Tree.Parent (B), (+rhs_item_ID, 1), (1 => Tree.Child (B, 1)));
            Clear_EBNF_Node (B);

         when others =>
            Raise_Programmer_Error ("translate_ebnf_to_bnf rhs_optional_item unimplemented", Data, Tree, B);
         end case;

         if WisiToken.Trace_Generate_EBNF > Detail then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Translate_RHS_Optional_Item edited:");
            Tree.Print_Tree (Wisitoken_Grammar_Actions.Descriptor, Container_List_Root);
         end if;
      end Translate_RHS_Optional_Item;

      procedure Translate_Token_Literal (Node : in Valid_Node_Index)
      is
         use Syntax_Trees.LR_Utils;

         Name_Ident : Identifier_Index;

         function Equal
           (Target : in String;
            List   : in Constant_List'Class;
            N      : in Valid_Node_Index)
           return Boolean
         is
            pragma Unreferenced (List);
         begin
            if Tree.Production_ID (Tree.Child (N, 1)) = (+declaration_ID, 0) then
               declare
                  Decl       : constant Node_Index       := Tree.Child (N, 1);
                  Value_Node : constant Valid_Node_Index := Tree.Child (Tree.Child (Decl, 4), 1);
               begin
                  if Tree.ID (Value_Node) = +declaration_item_ID and then
                    Tree.ID (Tree.Child (Value_Node, 1)) in
                    +IDENTIFIER_ID | +STRING_LITERAL_1_ID | +STRING_LITERAL_2_ID and then
                    Target = Get_Text (Data, Tree, Tree.Child (Value_Node, 1), Strip_Quotes => True)
                  then
                     case Tree.Label (Tree.Child (Decl, 3)) is
                     when Shared_Terminal =>
                        Name_Ident := New_Identifier (Get_Text (Data, Tree, Tree.Child (Decl, 3)));
                     when Virtual_Identifier =>
                        Name_Ident := Tree.Identifier (Tree.Child (Decl, 3));
                     when others =>
                        raise SAL.Programmer_Error;
                     end case;
                     return True;
                  else
                     return False;
                  end if;
               end;
            else
               return False;
            end if;
         end Equal;

         Value : constant String     := Get_Text (Data, Tree, Node, Strip_Quotes => True);
         Found : constant Node_Index := Find_Nonterminal (Value, Equal'Unrestricted_Access);
      begin
         if Found = Invalid_Node_Index then
            if GNAT.Regexp.Match (Value, Symbol_Regexp) then
               Name_Ident := New_Identifier (Ada.Characters.Handling.To_Upper (Value));
            else
               Put_Error
                 (Error_Message
                    (Data.Grammar_Lexer.File_Name, Get_Line (Data, Tree, Node),
                     "punctuation token '" & Value & "' not declared"));
               return;
            end if;
         end if;

         --  Replace string literal in rhs_item
         declare
            Parent : constant Valid_Node_Index := Tree.Parent (Node);
         begin
            case To_Token_Enum (Tree.ID (Parent)) is
            when rhs_item_ID =>
               Tree.Set_Children
                 (Tree.Parent (Node),
                  (+rhs_item_ID, 0),
                  (1 => Tree.Add_Identifier (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node))));

            when rhs_optional_item_ID =>
               Tree.Set_Children
                 (Tree.Parent (Node),
                  (+rhs_optional_item_ID, 2),
                  (Tree.Add_Identifier (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node)),
                   Tree.Child (Tree.Parent (Node), 2)));

            when others =>
               Raise_Programmer_Error ("translate_ebnf_to_bnf string_literal_2 unimplemented", Data, Tree, Node);
            end case;
         end;

         Clear_EBNF_Node (Node);
         if Found /= Invalid_Node_Index then
            return;
         end if;

         --  Declare token for keyword string literal
         declare
            Keyword        : constant Valid_Node_Index := Tree.Add_Identifier
              (+KEYWORD_ID, Keyword_Ident, Tree.Byte_Region (Node));
            Kind           : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+token_keyword_non_grammar_ID, 0),
               (1 => Keyword));
            Value_Literal  : constant Valid_Node_Index := Tree.Add_Identifier
              (+STRING_LITERAL_1_ID, New_Identifier ('"' & Value & '"'), Tree.Byte_Region (Node));
            Decl_Item      : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+declaration_item_ID, 1),
               (1 => Value_Literal));
            Decl_Item_List : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+declaration_item_list_ID, 0),
               (1 => Decl_Item));

            Percent : constant Valid_Node_Index := Tree.Add_Identifier
              (+PERCENT_ID, Percent_Ident, Tree.Byte_Region (Node));
            Name    : constant Valid_Node_Index := Tree.Add_Identifier
              (+IDENTIFIER_ID, Name_Ident, Tree.Byte_Region (Node));
            Decl    : constant Valid_Node_Index := Tree.Add_Nonterm
              ((+declaration_ID, 0), (Percent, Kind, Name, Decl_Item_List), Action => declaration_0'Access);
         begin
            Add_Compilation_Unit ("literal token", Decl, Prepend => True);
         end;

      end Translate_Token_Literal;

      procedure Process_Node (Node : in Valid_Node_Index)
      is begin
         case To_Token_Enum (Tree.ID (Node)) is
         --  Token_Enum_ID alphabetical order
         when declaration_ID =>
            --  Must be "%meta_syntax EBNF"; change to BNF
            declare
               Decl_Item    : constant Valid_Node_Index       := Tree.Find_Descendant
                 (Tree.Child (Node, 3), +declaration_item_ID);
               Old_Children : constant Valid_Node_Index_Array := Tree.Children (Decl_Item);
               New_Children : constant Valid_Node_Index_Array :=
                 (1 => Tree.Add_Identifier
                    (+IDENTIFIER_ID, New_Identifier ("BNF"), Tree.Byte_Region (Decl_Item)));
            begin
               Copy_Non_Grammar (Old_Children (1), New_Children (1));
               Tree.Set_Children (Decl_Item, (+declaration_item_ID, 1), New_Children);
            end;
            Clear_EBNF_Node (Node);

         when rhs_alternative_list_ID =>
            --  All handled by New_Nonterminal*
            raise SAL.Programmer_Error;

         when rhs_attribute_ID =>
            --  Just delete it
            declare
               use WisiToken.Syntax_Trees.LR_Utils;
               RHS_Item_List : List := Creators.Create_From_Element
                 (Tree, Tree.Parent (Node, 2), +rhs_item_list_ID, +rhs_element_ID, Invalid_Token_ID);
               Element : Cursor := RHS_Item_List.To_Cursor (Tree.Parent (Node, 2));
            begin
               RHS_Item_List.Delete (Element);
            end;
            Clear_EBNF_Node (Node);

         when rhs_group_item_ID =>
            Translate_RHS_Group_Item (Node);

         when rhs_multiple_item_ID =>
            Translate_RHS_Multiple_Item (Node);

         when rhs_optional_item_ID =>
            Translate_RHS_Optional_Item (Node);

         when STRING_LITERAL_2_ID =>
            Translate_Token_Literal (Node);

         when others =>
            Raise_Programmer_Error ("unimplemented EBNF node", Data, Tree, Node);
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;
      when E : others =>
         Raise_Programmer_Error
           ("unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E),
            Data, Tree, Node);
      end Process_Node;

      EBNF_Allowed : Boolean := True;

      procedure Validate_Node
        (Tree              : in     Syntax_Trees.Tree;
         Node              : in     Valid_Node_Index;
         Node_Image_Output : in out Boolean)
      is
         use Ada.Text_IO;

         procedure Put_Error (Msg : in String)
         is begin
            if not Node_Image_Output then
               Node_Image_Output := True;
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Tree, Data.Terminals, Node, Data.Grammar_Lexer.File_Name,
                     Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Include_Children  => Trace_Generate_EBNF > Detail,
                        Node_Numbers      => True)));
            end if;
            Put_Line (Current_Error, "... " & Msg);
            WisiToken.Generate.Error := True;
         end Put_Error;

         procedure Check_EBNF_Allowed
         is begin
            if not EBNF_Allowed then
               Put_Error ("no EBNF allowed");
            end if;
         end Check_EBNF_Allowed;

      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Ada.Containers.Count_Type;
            Children  : constant Valid_Node_Index_Array := Tree.Children (Node);
            RHS_Index : constant Natural                := Tree.RHS_Index (Node);
         begin
            case To_Token_Enum (Tree.ID (Node)) is
            when nonterminal_ID =>
               null;

            when rhs_list_ID =>
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_ID then
                     Put_Error ("child 1 not rhs");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 3 then
                     Put_Error ("expected child_count 3");
                  elsif Tree.ID (Children (1)) /= +rhs_list_ID or
                    Tree.ID (Children (2)) /= +BAR_ID or
                    Tree.ID (Children (3)) /= +rhs_ID
                  then
                     Put_Error ("expecting rhs_list BAR rhs");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_ID =>
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 0 then
                     Put_Error ("expected child_count 0");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID then
                     Put_Error ("expecting rhs_item_list");
                  end if;

               when 2 =>
                  if Tree.Child_Count (Node) /= 2 then
                     Put_Error ("expected child_count 2");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID or
                    Tree.ID (Children (2)) /= +ACTION_ID
                  then
                     Put_Error ("expecting rhs_item_list ACTION");
                  end if;

               when 3 =>
                  if Tree.Child_Count (Node) /= 3 then
                     Put_Error ("expected child_count 3");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID or
                    Tree.ID (Children (2)) /= +ACTION_ID or
                    Tree.ID (Children (3)) /= +ACTION_ID
                  then
                     Put_Error ("expecting rhs_item_list ACTION ACTION");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_attribute_ID =>
               Check_EBNF_Allowed;

            when rhs_element_ID =>
               case RHS_Index is
               when 0 =>
                  if Tree.Child_Count (Node) /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_item_ID then
                     Put_Error ("expecting rhs_item");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 3 then
                     Put_Error ("expected child_count 3");
                  elsif Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                    Tree.ID (Children (2)) /= +EQUAL_ID or
                    Tree.ID (Children (3)) /= +rhs_item_ID
                  then
                     Put_Error ("expecting IDENTIFIER EQUAL rhs_item");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_item_list_ID =>
               case RHS_Index is
               when 0 =>
                  if Tree.Child_Count (Node) /= 1 then
                     Put_Error ("expected child_count 1");
                  elsif Tree.ID (Children (1)) /= +rhs_element_ID then
                     Put_Error ("expecting rhs_element");
                  end if;

               when 1 =>
                  if Tree.Child_Count (Node) /= 2 then
                     Put_Error ("expected child_count 2");
                  elsif Tree.ID (Children (1)) /= +rhs_item_list_ID or
                    Tree.ID (Children (2)) /= +rhs_element_ID
                  then
                     Put_Error ("expecting rhs_item_list ELEMENT");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_item_ID =>
               if Tree.Child_Count (Node) /= 1 then
                  Put_Error ("expected child_count 1");
               end if;

               case RHS_Index is
               when 0 =>
                  if Tree.ID (Children (1)) /= +IDENTIFIER_ID then
                     Put_Error ("expecting IDENTIFIER");
                  end if;

               when 1 =>
                  if Tree.ID (Children (1)) /= +STRING_LITERAL_2_ID then
                     Put_Error ("expecting STRING_LITERAL_2");
                  end if;

               when 2 =>
                  if Tree.ID (Children (1)) /= +rhs_attribute_ID then
                     Put_Error ("expecting rhs_attribute");
                  end if;

               when 3 =>
                  if Tree.ID (Children (1)) /= +rhs_optional_item_ID then
                     Put_Error ("expecting rhs_optional_item");
                  end if;

               when 4 =>
                  if Tree.ID (Children (1)) /= +rhs_multiple_item_ID then
                     Put_Error ("expecting rhs_multiple_item");
                  end if;

               when 5 =>
                  if Tree.ID (Children (1)) /= +rhs_group_item_ID then
                     Put_Error ("expecting rhs_group_item");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_group_item_ID =>
               Check_EBNF_Allowed;
               if RHS_Index /= 0 or
                 (Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID))
               then
                  Put_Error ("expecting RHS_Index 0, LEFT_PAREN rhs_alternative_list RIGHT_PAREN");
               end if;

            when rhs_optional_item_ID =>
               Check_EBNF_Allowed;
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +LEFT_BRACKET_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_BRACKET_ID)
                  then
                     Put_Error ("expecting LEFT_BRACKET rhs_alternative_list RIGHT_BRACKET");
                  end if;

               when 1 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID or
                       Tree.ID (Children (4)) /= +QUESTION_ID)
                  then
                     Put_Error ("expecting LEFT_PAREN rhs_alternative_list RIGHT_PAREN QUESTION");
                  end if;

               when 2 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                       Tree.ID (Children (2)) /= +QUESTION_ID)
                  then
                     Put_Error ("expecting IDENTIFIER QUESTION");
                  end if;

               when 3 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +STRING_LITERAL_2_ID or
                       Tree.ID (Children (2)) /= +QUESTION_ID)
                  then
                     Put_Error ("expecting STRING_LITERAL_2 QUESTION");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_multiple_item_ID =>
               Check_EBNF_Allowed;
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +LEFT_BRACE_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_BRACE_ID)
                  then
                     Put_Error ("expecting LEFT_BRACE rhs_alternative_list RIGHT_BRACE");
                  end if;

               when 1 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_BRACE_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_BRACE_ID or
                       Tree.ID (Children (4)) /= +MINUS_ID)
                  then
                     Put_Error ("expecting LEFT_BRACE rhs_alternative_list RIGHT_BRACE MINUS");
                  end if;

               when 2 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID or
                       Tree.ID (Children (4)) /= +PLUS_ID)
                  then
                     Put_Error ("expecting LEFT_PAREN rhs_alternative_list RIGHT_PAREN PLUS");
                  end if;

               when 3 =>
                  if Children'Length /= 4 or else
                    (Tree.ID (Children (1)) /= +LEFT_PAREN_ID or
                       Tree.ID (Children (2)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (3)) /= +RIGHT_PAREN_ID or
                       Tree.ID (Children (4)) /= +STAR_ID)
                  then
                     Put_Error ("expecting LEFT_PAREN rhs_alternative_list RIGHT_PAREN STAR");
                  end if;

               when 4 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                       Tree.ID (Children (2)) /= +PLUS_ID)
                  then
                     Put_Error ("expecting IDENTIFIER PLUS");
                  end if;

               when 5 =>
                  if Children'Length /= 2 or else
                    (Tree.ID (Children (1)) /= +IDENTIFIER_ID or
                       Tree.ID (Children (2)) /= +STAR_ID)
                  then
                     Put_Error ("expecting IDENTIFIER STAR");
                  end if;

               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when rhs_alternative_list_ID =>
               Check_EBNF_Allowed;
               case RHS_Index is
               when 0 =>
                  if Children'Length /= 1 or else
                    (Tree.ID (Children (1)) /= +rhs_item_list_ID)
                  then
                     Put_Error ("expecting rhs_item_list");
                  end if;

               when 1 =>
                  if Children'Length /= 3 or else
                    (Tree.ID (Children (1)) /= +rhs_alternative_list_ID or
                       Tree.ID (Children (2)) /= +BAR_ID or
                       Tree.ID (Children (3)) /= +rhs_item_list_ID)
                  then
                     Put_Error ("expecting rhs_alternative_list BAR rhs_item_list");
                  end if;
               when others =>
                  Put_Error ("unexpected RHS_Index");
               end case;

            when compilation_unit_ID =>
               null;

            when compilation_unit_list_ID =>
               null;

            when others =>
               null;
            end case;
         end;
      end Validate_Node;

      procedure Check_Original_EBNF
      is
         use Ada.Text_IO;
         Sub_Tree_Root : Node_Index;
      begin
         for N in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index loop
            if Data.EBNF_Nodes (N) then
               Sub_Tree_Root := Tree.Sub_Tree_Root (N);
               if Sub_Tree_Root /= Tree.Root then
                  Put_Line
                    (Current_Error,
                     Error_Message
                       (Tree, Data.Terminals, N, Data.Grammar_Lexer.File_Name,
                        Tree.Image
                          (N, Wisitoken_Grammar_Actions.Descriptor,
                           Node_Numbers      => True)));
                  Put_Line (Current_Error, "... not in tree; in root" & Sub_Tree_Root'Image);
                  WisiToken.Generate.Error := True;
               end if;
            end if;
         end loop;
      end Check_Original_EBNF;

      procedure Check_Copied_EBNF
      is
         use Ada.Text_IO;
         Sub_Tree_Root : Node_Index;
      begin
         for N of Copied_EBNF_Nodes loop
            if N /= Deleted_Child then
               Sub_Tree_Root := Tree.Sub_Tree_Root (N);
               if Sub_Tree_Root /= Tree.Root then
                  Put_Line
                    (Current_Error,
                     Error_Message
                       (Tree, Data.Terminals, N, Data.Grammar_Lexer.File_Name,
                        Tree.Image
                          (N, Wisitoken_Grammar_Actions.Descriptor,
                           Node_Numbers      => True)));
                  Put_Line (Current_Error, "... not in tree; in root" & Sub_Tree_Root'Image);
                  WisiToken.Generate.Error := True;
               end if;
            end if;
         end loop;
      end Check_Copied_EBNF;

   begin
      --  Process nodes in node increasing order, so contained items are
      --  translated first, so duplicates of the containing item can be found
      for I in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index loop
         if Data.EBNF_Nodes (I) then
            if Trace_Generate_EBNF > Outline then
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line
                 ("translate original node " & Tree.Image
                    (I, Wisitoken_Grammar_Actions.Descriptor,
                     Include_RHS_Index => True,
                     Node_Numbers      => True));
            end if;

            Process_Node (I);

            Tree.Validate_Tree
              (Data.Terminals, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer.File_Name, Tree.Root,
               Validate_Node'Unrestricted_Access);
            Check_Original_EBNF;
            Check_Copied_EBNF;
         end if;
      end loop;

      declare
         use Ada.Text_IO;
      begin
         for Node in Data.EBNF_Nodes.First_Index .. Data.EBNF_Nodes.Last_Index loop
            if Data.EBNF_Nodes (Node) then
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Tree, Data.Terminals, Node, Data.Grammar_Lexer.File_Name,
                     Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Include_Children  => Trace_Generate_EBNF > Detail,
                        Node_Numbers      => True)));
               Put_Line (Current_Error, "... original EBNF node not translated");
            end if;
         end loop;
      end;

      declare
         I : SAL.Base_Peek_Type := Copied_EBNF_Nodes.First_Index;
      begin
         --  Processing copied nodes may produce more copied nodes, so we can't
         --  use a 'for' loop.
         loop
            exit when I > Copied_EBNF_Nodes.Last_Index;
            if Copied_EBNF_Nodes (I) = Deleted_Child then
               --  Deleted
               if Trace_Generate_EBNF > Outline then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line
                    ("skipping deleted copied node " & Tree.Image
                       (Copied_EBNF_Nodes (I), Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Node_Numbers      => True));
               end if;
            else
               if Trace_Generate_EBNF > Outline then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line
                    ("translate copied node " & Tree.Image
                       (Copied_EBNF_Nodes (I), Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Node_Numbers      => True));
               end if;

               Process_Node (Copied_EBNF_Nodes (I));

               Tree.Validate_Tree
                 (Data.Terminals, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer.File_Name, Tree.Root,
                  Validate_Node'Unrestricted_Access);
               Check_Copied_EBNF;
            end if;
            I := I + 1;
         end loop;
      end;

      declare
         use Ada.Text_IO;
      begin
         for Node of Copied_EBNF_Nodes loop
            if Node /= Deleted_Child then
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Tree, Data.Terminals, Node, Data.Grammar_Lexer.File_Name,
                     Tree.Image
                       (Node, Wisitoken_Grammar_Actions.Descriptor,
                        Include_RHS_Index => True,
                        Include_Children  => Trace_Generate_EBNF > Detail,
                        Node_Numbers      => True)));
               Put_Line (Current_Error, "... copied EBNF node not translated");
            end if;
         end loop;
      end;

      EBNF_Allowed := False;
      Tree.Validate_Tree
        (Data.Terminals, Wisitoken_Grammar_Actions.Descriptor, Data.Grammar_Lexer.File_Name, Tree.Root,
         Validate_Node'Unrestricted_Access);

      Data.Meta_Syntax := BNF_Syntax;

      if Trace_Generate_EBNF > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Identifiers:");
         for I in Data.Tokens.Virtual_Identifiers.First_Index .. Data.Tokens.Virtual_Identifiers.Last_Index loop
            Ada.Text_IO.Put_Line (Base_Identifier_Index'Image (I) & " " & (-Data.Tokens.Virtual_Identifiers (I)));
         end loop;
      end if;
   end Translate_EBNF_To_BNF;

   procedure Print_Source
     (File_Name : in String;
      Tree      : in WisiToken.Syntax_Trees.Tree;
      Data      : in User_Data_Type)
   is
      use Ada.Text_IO;
      use WisiToken.Syntax_Trees;

      File : File_Type;

      procedure Put_Comments
        (Node           : in Valid_Node_Index;
         Force_New_Line : in Boolean := False;
         Force_Comment  : in String  := "")
      is
         Last_Term : constant Node_Index              := Tree.Last_Terminal (Node);
         Aug       : constant Base_Token_Class_Access :=
           (if Last_Term = Invalid_Node_Index
            then null
            else Tree.Augmented (Last_Term));

         Comments_Include_Newline : Boolean := False;
      begin
         if Aug = null then
            if Force_Comment /= "" then
               Put_Line (File, Force_Comment);

            elsif Force_New_Line then
               New_Line (File);
            end if;
         else
            for Token of Augmented_Token_Access (Aug).Non_Grammar loop
               if Token.ID = +NEW_LINE_ID then
                  Comments_Include_Newline := True;
               end if;
               Put (File, Data.Grammar_Lexer.Buffer_Text (Token.Byte_Region));
            end loop;
            if Force_New_Line and not Comments_Include_Newline then
               New_Line (File);
            end if;
         end if;
      end Put_Comments;

      procedure Put_Declaration_Item (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         case To_Token_Enum (Tree.ID (Children (1))) is
         when IDENTIFIER_ID | NUMERIC_LITERAL_ID | STRING_LITERAL_1_ID | STRING_LITERAL_2_ID =>
            Put (File, ' ' & Get_Text (Data, Tree, Children (1)));
         when REGEXP_ID =>
            Put (File, " %[" & Get_Text (Data, Tree, Children (1)) & "]%");
         when others =>
            Put (File, Image (Tree.ID (Children (1)), Wisitoken_Grammar_Actions.Descriptor));
         end case;
      end Put_Declaration_Item;

      procedure Put_Declaration_Item_List (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put_Declaration_Item (Children (1));
         else
            Put_Declaration_Item_List (Children (1));
            Put_Declaration_Item (Children (2));
         end if;
      end Put_Declaration_Item_List;

      procedure Put_Identifier_List (Node : in Valid_Node_Index)
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put (File, Get_Text (Data, Tree, Children (1)));
         else
            Put_Identifier_List (Children (1));
            Put (File, ' ');
            Put (File, Get_Text (Data, Tree, Children (2)));
         end if;
      end Put_Identifier_List;

      procedure Put_RHS_Element (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_element_ID
      is begin
         --  We don't raise an exception for errors here; it's easier to debug from the
         --  mangled source listing.

         case Tree.RHS_Index (Node) is
         when 0 =>
            Put (File, Get_Text (Data, Tree, Node));

         when 1 =>
            --  Output no spaces around "="
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
            begin
               Put (File, Get_Text (Data, Tree, Children (1)) & "=" & Get_Text (Data, Tree, Children (3)));
            end;

         when others =>
            New_Line (File);
            Put (File, " ;; not translated: " & Node_Index'Image (Node) & ":" &
                   Tree.Image (Node, Wisitoken_Grammar_Actions.Descriptor,
                               Include_Children  => True,
                               Include_RHS_Index => True,
                               Node_Numbers      => True));
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error
              ("Put_RHS_Element: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
      end Put_RHS_Element;

      procedure Put_RHS_Item_List (Node : in Valid_Node_Index)
      with Pre => Tree.ID (Node) = +rhs_item_list_ID
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         if Children'Length = 1 then
            Put_RHS_Element (Children (1));
         else
            Put_RHS_Item_List (Children (1));
            Put (File, ' ');
            Put_RHS_Element (Children (2));
         end if;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error
              ("Put_RHS_Item_List: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
      end Put_RHS_Item_List;

      procedure Put_RHS
        (Node  : in Valid_Node_Index;
         First : in Boolean)
      with Pre => Tree.ID (Node) = +rhs_ID
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         Put (File, (if First then "  : " else "  | "));
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_Comments (Tree.Parent (Node), Force_Comment => ";; empty");

         when 1 .. 3 =>
            Put_RHS_Item_List (Children (1));
            Put_Comments (Children (1), Force_New_Line => True);

            if Tree.RHS_Index (Node) > 1 then
               Put (File, "    %(" & Get_Text (Data, Tree, Children (2)) & ")%"); -- action
               Put_Comments (Children (2), Force_New_Line => True);

               if Tree.RHS_Index (Node) > 2 then
                  Put (File, "    %(" & Get_Text (Data, Tree, Children (3)) & ")%"); -- check
                  Put_Comments (Children (3), Force_New_Line => True);
               end if;
            end if;

         when others =>
            Raise_Programmer_Error ("Put_RHS", Data, Tree, Node);
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error ("Put_RHS: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
      end Put_RHS;

      procedure Put_RHS_List
        (Node    : in     Valid_Node_Index;
         First   : in out Boolean;
         Virtual : in     Boolean)
      with Pre => Tree.ID (Node) = +rhs_list_ID
      is
         Children : constant Valid_Node_Index_Array := Tree.Children (Node);
      begin
         case Tree.RHS_Index (Node) is
         when 0 =>
            Put_RHS (Children (1), First);
            First := False;
         when 1 =>
            Put_RHS_List (Children (1), First, Virtual);
            Put_RHS (Children (3), First => False);
         when 2 =>
            Put
              (File, "%if " & Get_Text (Data, Tree, Children (3)) & " = " & Get_Text (Data, Tree, Children (4)));
            Put_Comments (Node);

         when 3 =>
            Put (File, "%end if");
            Put_Comments (Node);

         when others =>
            Raise_Programmer_Error ("Put_RHS_List", Data, Tree, Node);
         end case;
      exception
      when SAL.Programmer_Error =>
         raise;

      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Raise_Programmer_Error
              ("Put_RHS_List: " & Exception_Name (E) & ": " & Exception_Message (E), Data, Tree, Node);
         end;
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
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
            begin
               case Tree.RHS_Index (Node) is
               when 0 =>
                  case Tree.RHS_Index (Children (2)) is
                  when 0 =>
                     Put (File, "%keyword");
                  when 1 =>
                     Put (File, "%non_grammar <" & Get_Text (Data, Tree, Tree.Child (Children (2), 3)) & ">");
                  when 2 =>
                     Put (File, "%token <" & Get_Text (Data, Tree, Tree.Child (Children (2), 3)) & ">");
                  when others =>
                     raise SAL.Programmer_Error;
                  end case;

                  Put (File, " " & Get_Text (Data, Tree, Children (3)));
                  Put_Declaration_Item_List (Children (4));
                  Put_Comments (Children (4), Force_New_Line => True);

               when 1 =>
                  Put (File, "%code ");
                  Put_Identifier_List (Children (3));
                  Put (File, " %{" & Get_Text (Data, Tree, Children (4)) & "}%"); -- RAW_CODE
                  Put_Comments (Node);

               when 2 =>
                  declare
                     Key : constant String := Get_Text (Data, Tree, Children (2));
                  begin
                     if Key = "conflict" then
                        Put (File, Data.Grammar_Lexer.Buffer_Text (Tree.Byte_Region (Node)));
                     else
                        Put (File, "%" & Key);
                        Put_Declaration_Item_List (Children (3));
                     end if;
                  end;
                  Put_Comments (Children (3));

               when 3 =>
                  Put (File, "%" & Get_Text (Data, Tree, Children (2)));
                  Put_Comments (Children (2));

               when 4 =>
                  Put
                    (File, "%if" & Get_Text (Data, Tree, Children (2)) & " = " & Get_Text (Data, Tree, Children (4)));
                  Put_Comments (Node);

               when 5 =>
                  Put (File, "%end if");
                  Put_Comments (Node);

               when others =>
                  raise SAL.Programmer_Error;
               end case;
            end;

         when nonterminal_ID =>
            declare
               Children : constant Valid_Node_Index_Array := Tree.Children (Node);
               Virtual  : constant Boolean                := Tree.Label (Children (1)) = Virtual_Identifier;
               First    : Boolean                         := True;
            begin
               Put (File, Get_Text (Data, Tree, Children (1)));
               Put_Comments (Children (1), Force_New_Line => True);

               Put_RHS_List (Children (3), First, Virtual);

               if Tree.Children (Children (4))'Length > 0 then
                  if Virtual then
                     Put_Line (File, "  ;");
                  else
                     Put (File, "  ;");
                     Put_Comments (Children (4));
                  end if;
               end if;
            end;

         when wisitoken_accept_ID =>
            Process_Node (Tree.Child (Node, 1));

         when others =>
            raise SAL.Not_Implemented with Image (Tree.ID (Node), Wisitoken_Grammar_Actions.Descriptor);
         end case;
      end Process_Node;
   begin
      Create (File, Out_File, File_Name);
      Put_Line (File, ";;; generated from " & Data.Grammar_Lexer.File_Name & " -*- buffer-read-only:t -*-");
      Put_Line (File, ";;;");

      for Token of Data.Leading_Non_Grammar loop
         Put (File, Data.Grammar_Lexer.Buffer_Text (Token.Byte_Region));
      end loop;

      Process_Node (Tree.Root);

      Close (File);
   exception
   when E : SAL.Not_Implemented =>
      Close (File);
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "Print_Source not implemented: " & Ada.Exceptions.Exception_Message (E));
   end Print_Source;

end WisiToken_Grammar_Runtime;
--  Local Variables:
--  ada-which-func-parse-size: 50000
--  End:
