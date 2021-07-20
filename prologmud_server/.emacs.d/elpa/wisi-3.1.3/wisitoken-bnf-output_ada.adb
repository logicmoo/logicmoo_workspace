--  Abstract :
--
--  Output Ada code implementing the grammar defined by input
--  parameters, and a parser for that grammar. The grammar parser
--  actions must be Ada.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regexp;
with WisiToken.BNF.Generate_Packrat;
with WisiToken.BNF.Generate_Utils;
with WisiToken.BNF.Output_Ada_Common; use WisiToken.BNF.Output_Ada_Common;
with WisiToken.Generate.Packrat;
with WisiToken_Grammar_Runtime;
procedure WisiToken.BNF.Output_Ada
  (Input_Data            :         in WisiToken_Grammar_Runtime.User_Data_Type;
   Output_File_Name_Root :         in String;
   Generate_Data         : aliased in WisiToken.BNF.Generate_Utils.Generate_Data;
   Packrat_Data          :         in WisiToken.Generate.Packrat.Data;
   Tuple                 :         in Generate_Tuple;
   Test_Main             :         in Boolean;
   Multiple_Tuples       :         in Boolean)
is
   Common_Data : Output_Ada_Common.Common_Data := WisiToken.BNF.Output_Ada_Common.Initialize
     (Input_Data, Tuple, Output_File_Name_Root, Check_Interface => False);

   Gen_Alg_Name : constant String :=
     (if Test_Main or Multiple_Tuples
      then "_" & Generate_Algorithm_Image (Common_Data.Generate_Algorithm).all
      else "");

   function Symbol_Regexp (Item : in String) return String
   is begin
      --  Return a regular expression string that matches Item as a symbol;
      --  it must be preceded and followed by non-symbol characters.
      --
      --  GNAT.Regexp does not have a char for 'end of string', so we hope
      --  that doesn't occur. Sigh.
      return ".*[ (\.]" & Item & "[ );\.,].*";
   end Symbol_Regexp;

   procedure Create_Ada_Actions_Body
     (Action_Names : not null access WisiToken.Names_Array_Array;
      Check_Names  : not null access WisiToken.Names_Array_Array;
      Label_Count  : in              Ada.Containers.Count_Type;
      Package_Name : in              String)
   is
      use all type Ada.Containers.Count_Type;
      use GNAT.Regexp;
      use Generate_Utils;
      use WisiToken.Generate;

      File_Name : constant String := Output_File_Name_Root & "_actions.adb";

      User_Data_Regexp : constant Regexp := Compile (Symbol_Regexp ("User_Data"), Case_Sensitive => False);
      Tree_Regexp      : constant Regexp := Compile (Symbol_Regexp ("Tree"), Case_Sensitive      => False);
      Nonterm_Regexp   : constant Regexp := Compile (Symbol_Regexp ("Nonterm"), Case_Sensitive   => False);
      Tokens_Regexp    : constant Regexp := Compile (Symbol_Regexp ("Tokens"), Case_Sensitive    => False);

      Body_File : File_Type;
   begin
      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;
      Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Body_Context));
      New_Line;

      if Label_Count > 0 then
         Put_Line ("with SAL;");
      end if;

      Put_Line ("package body " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      if Input_Data.Check_Count > 0 then
         Indent_Line ("use WisiToken.Semantic_Checks;");
         New_Line;
      end if;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Body_Pre));

      --  generate Action and Check subprograms.

      for Rule of Input_Data.Tokens.Rules loop
         --  No need for a Token_Cursor here, since we only need the
         --  nonterminals.
         declare
            use Ada.Strings.Unbounded;

            LHS_ID    : constant WisiToken.Token_ID := Find_Token_ID (Generate_Data, -Rule.Left_Hand_Side);
            RHS_Index : Integer                     := 0;

            function Is_Elisp (Action : in Unbounded_String) return Boolean
            is begin
               return Length (Action) >= 6 and then
                 (Slice (Action, 1, 6) = "(progn" or
                    Slice (Action, 1, 5) = "wisi-");
            end Is_Elisp;

            procedure Put_Labels (RHS : in RHS_Type; Line : in String)
            is
               Output : array (Rule.Labels.First_Index .. Rule.Labels.Last_Index) of Boolean := (others => False);

               procedure Update_Output (Label : in String)
               is begin
                  for I in Rule.Labels.First_Index .. Rule.Labels.Last_Index loop
                     if Label = Rule.Labels (I) then
                        Output (I) := True;
                     end if;
                  end loop;
               end Update_Output;
            begin
               for I in RHS.Tokens.First_Index .. RHS.Tokens.Last_Index loop
                  if Length (RHS.Tokens (I).Label) > 0 then
                     declare
                        Label : constant String := -RHS.Tokens (I).Label;
                     begin
                        if Match (Line, Compile (Symbol_Regexp (Label), Case_Sensitive => False)) then
                           Indent_Line
                             (Label & " : constant SAL.Peek_Type :=" & SAL.Peek_Type'Image (I) & ";");
                           Update_Output (Label);
                        end if;
                     end;
                  end if;
               end loop;

               for I in Rule.Labels.First_Index .. Rule.Labels.Last_Index loop
                  if not Output (I) and
                    Match (Line, Compile (Symbol_Regexp (-Rule.Labels (I)), Case_Sensitive => False))
                  then
                     Indent_Line (-Rule.Labels (I) & " : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type'First;");
                  end if;
               end loop;
            end Put_Labels;

         begin
            for RHS of Rule.Right_Hand_Sides loop
               if Length (RHS.Action) > 0 and then not Is_Elisp (RHS.Action) then
                  declare
                     Line : constant String := -RHS.Action;
                     --  Actually multiple lines; we assume the formatting is adequate.

                     Name : constant String := Action_Names (LHS_ID)(RHS_Index).all;

                     Unref_User_Data : Boolean := True;
                     Unref_Tree      : Boolean := True;
                     Unref_Nonterm   : Boolean := True;
                     Unref_Tokens    : Boolean := True;
                     Need_Comma      : Boolean := False;

                     procedure Check_Unref (Line : in String)
                     is begin
                        if Match (Line, User_Data_Regexp) then
                           Unref_User_Data := False;
                        end if;
                        if Match (Line, Tree_Regexp) then
                           Unref_Tree := False;
                        end if;
                        if Match (Line, Nonterm_Regexp) then
                           Unref_Nonterm := False;
                        end if;
                        if Match (Line, Tokens_Regexp) then
                           Unref_Tokens := False;
                        end if;
                     end Check_Unref;
                  begin
                     Check_Unref (Line);
                     Indent_Line ("procedure " & Name);
                     Indent_Line (" (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;");
                     Indent_Line ("  Tree      : in out WisiToken.Syntax_Trees.Tree;");
                     Indent_Line ("  Nonterm   : in     WisiToken.Valid_Node_Index;");
                     Indent_Line ("  Tokens    : in     WisiToken.Valid_Node_Index_Array)");
                     Indent_Line ("is");

                     Indent := Indent + 3;
                     if Unref_User_Data or Unref_Tree or Unref_Nonterm or Unref_Tokens then
                        Indent_Start ("pragma Unreferenced (");

                        if Unref_User_Data then
                           Put ("User_Data");
                           Need_Comma := True;
                        end if;
                        if Unref_Tree then
                           Put ((if Need_Comma then ", " else "") & "Tree");
                           Need_Comma := True;
                        end if;
                        if Unref_Nonterm then
                           Put ((if Need_Comma then ", " else "") & "Nonterm");
                           Need_Comma := True;
                        end if;
                        if Unref_Tokens then
                           Put ((if Need_Comma then ", " else "") & "Tokens");
                           Need_Comma := True;
                        end if;
                        Put_Line (");");
                     end if;

                     Put_Labels (RHS, Line);
                     Indent := Indent - 3;
                     Indent_Line ("begin");
                     Indent := Indent + 3;

                     Indent_Line (Line);
                     Indent := Indent - 3;
                     Indent_Line ("end " & Name & ";");
                     New_Line;
                  end;
               end if;

               if Length (RHS.Check) > 0 and then not Is_Elisp (RHS.Check) then
                  declare
                     use Ada.Strings.Fixed;
                     Line          : constant String  := -RHS.Check;
                     Name          : constant String  := Check_Names (LHS_ID)(RHS_Index).all;
                     Unref_Lexer   : constant Boolean := 0 = Index (Line, "Lexer");
                     Unref_Nonterm : constant Boolean := 0 = Index (Line, "Nonterm");
                     Unref_Tokens  : constant Boolean := 0 = Index (Line, "Tokens");
                     Unref_Recover : constant Boolean := 0 = Index (Line, "Recover_Active");
                     Need_Comma    : Boolean          := False;
                  begin
                     Indent_Line ("function " & Name);
                     Indent_Line (" (Lexer          : access constant WisiToken.Lexer.Instance'Class;");
                     Indent_Line ("  Nonterm        : in out WisiToken.Recover_Token;");
                     Indent_Line ("  Tokens         : in     WisiToken.Recover_Token_Array;");
                     Indent_Line ("  Recover_Active : in     Boolean)");
                     Indent_Line (" return WisiToken.Semantic_Checks.Check_Status");
                     Indent_Line ("is");

                     Indent := Indent + 3;
                     if Unref_Lexer or Unref_Nonterm or Unref_Tokens or Unref_Recover then
                        Indent_Start ("pragma Unreferenced (");

                        if Unref_Lexer then
                           Put ("Lexer");
                           Need_Comma := True;
                        end if;
                        if Unref_Nonterm then
                           Put ((if Need_Comma then ", " else "") & "Nonterm");
                           Need_Comma := True;
                        end if;
                        if Unref_Tokens then
                           Put ((if Need_Comma then ", " else "") & "Tokens");
                           Need_Comma := True;
                        end if;
                        if Unref_Recover then
                           Put ((if Need_Comma then ", " else "") & "Recover_Active");
                           Need_Comma := True;
                        end if;
                        Put_Line (");");
                     end if;

                     Put_Labels (RHS, Line);
                     Indent := Indent - 3;

                     Indent_Line ("begin");
                     Indent := Indent + 3;
                     Indent_Line (Line);
                     Indent := Indent - 3;
                     Indent_Line ("end " & Name & ";");
                     New_Line;
                  end;
               end if;

               RHS_Index := RHS_Index + 1;
            end loop;
         end;
      end loop;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Body_Post));

      Put_Line ("end " & Package_Name & ";");
      Close (Body_File);

      Set_Output (Standard_Output);

   end Create_Ada_Actions_Body;

   procedure Create_Ada_Main_Body
     (Actions_Package_Name : in String;
      Main_Package_Name    : in String)
   is
      use WisiToken.Generate;

      File_Name         : constant String := To_Lower (Main_Package_Name) & ".adb";
      re2c_Package_Name : constant String := -Common_Data.Lower_File_Name_Root & "_re2c_c";

      Body_File : File_Type;
   begin
      Create (Body_File, Out_File, File_Name);
      Set_Output (Body_File);
      Indent := 1;

      Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      if (case Common_Data.Generate_Algorithm is
          when LR_Generate_Algorithm => Input_Data.Action_Count > 0 or Input_Data.Check_Count > 0,
          when Packrat_Generate_Algorithm | External => Input_Data.Action_Count > 0)
      then
         Put_Line ("with " & Actions_Package_Name & "; use " & Actions_Package_Name & ";");
      end if;

      case Common_Data.Lexer is
      when None | Elisp_Lexer =>
         null;

      when re2c_Lexer =>
         Put_Line ("with WisiToken.Lexer.re2c;");
         Put_Line ("with " & re2c_Package_Name & ";");
      end case;

      case Common_Data.Generate_Algorithm is
      when LR_Generate_Algorithm =>
         null;

      when Packrat_Gen =>
         Put_Line ("with WisiToken.Parse.Packrat.Generated;");

      when Packrat_Proc =>
         Put_Line ("with WisiToken.Parse.Packrat.Procedural;");
         Put_Line ("with WisiToken.Productions;");

      when External =>
         null;
      end case;

      Put_Line ("package body " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Common_Data.Lexer is
      when None | Elisp_Lexer =>
         null;

      when re2c_Lexer =>
         Indent_Line ("package Lexer is new WisiToken.Lexer.re2c");
         Indent_Line ("  (" & re2c_Package_Name & ".New_Lexer,");
         Indent_Line ("   " & re2c_Package_Name & ".Free_Lexer,");
         Indent_Line ("   " & re2c_Package_Name & ".Reset_Lexer,");
         Indent_Line ("   " & re2c_Package_Name & ".Next_Token);");
         New_Line;
      end case;

      case Common_Data.Generate_Algorithm is
      when LR_Generate_Algorithm =>
         LR_Create_Create_Parser (Input_Data, Common_Data, Generate_Data);

      when Packrat_Gen =>
         WisiToken.BNF.Generate_Packrat (Packrat_Data, Generate_Data);

         Packrat_Create_Create_Parser (Common_Data, Generate_Data, Packrat_Data);

      when Packrat_Proc =>
         Packrat_Create_Create_Parser (Common_Data, Generate_Data, Packrat_Data);

      when External =>
         External_Create_Create_Grammar (Generate_Data);
      end case;

      Put_Line ("end " & Main_Package_Name & ";");
      Close (Body_File);
      Set_Output (Standard_Output);
   end Create_Ada_Main_Body;

   procedure Create_Ada_Test_Main
     (Actions_Package_Name : in String;
      Main_Package_Name    : in String)
   is
      use WisiToken.Generate;

      Generic_Package_Name : constant String :=
        (case Common_Data.Generate_Algorithm is
         when LR_Generate_Algorithm =>
           (if Input_Data.Language_Params.Error_Recover then
              (if Common_Data.Text_Rep
               then "Gen_LR_Text_Rep_Parser_Run"
               else "Gen_LR_Parser_Run")
            else
              (if Common_Data.Text_Rep
               then "Gen_LR_Text_Rep_Parser_No_Recover_Run"
               else "Gen_LR_Parser_No_Recover_Run")),

         when Packrat_Generate_Algorithm => "Gen_Packrat_Parser_Run",
         when External => raise SAL.Programmer_Error);

      Unit_Name : constant String := File_Name_To_Ada (Output_File_Name_Root) &
        "_" & Generate_Algorithm'Image (Common_Data.Generate_Algorithm) & "_Run";

      Default_Language_Runtime_Package : constant String := "WisiToken.Parse.LR.McKenzie_Recover." & File_Name_To_Ada
        (Output_File_Name_Root);

      File_Name : constant String := To_Lower (Unit_Name) & ".ads";

      File : File_Type;
   begin
      Create (File, Out_File, File_Name);
      Set_Output (File);
      Indent := 1;

      Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
      --  no Copyright_License; just a test file
      New_Line;

      Put_Line ("with " & Generic_Package_Name & ";");
      Put_Line ("with " & Actions_Package_Name & ";");
      Put_Line ("with " & Main_Package_Name & ";");
      if Input_Data.Language_Params.Error_Recover and
        Input_Data.Language_Params.Use_Language_Runtime
      then
         declare
            Pkg : constant String :=
              (if -Input_Data.Language_Params.Language_Runtime_Name = ""
               then Default_Language_Runtime_Package
               else -Input_Data.Language_Params.Language_Runtime_Name);
         begin
            --  For language-specific names in actions, checks.
            Put_Line ("with " & Pkg & ";");
            Put_Line ("use " & Pkg & ";");
         end;
      end if;

      Put_Line ("procedure " & Unit_Name & " is new " & Generic_Package_Name);
      Put_Line ("  (" & Actions_Package_Name & ".Descriptor,");
      if Common_Data.Text_Rep then
         Put_Line ("   """ & Output_File_Name_Root & "_" &
                     To_Lower (Generate_Algorithm_Image (Tuple.Gen_Alg).all) &
                     "_parse_table.txt"",");
      end if;
      if Input_Data.Language_Params.Error_Recover then
         if Input_Data.Language_Params.Use_Language_Runtime then
            Put_Line ("Fixes'Access, Matching_Begin_Tokens'Access, String_ID_Set'Access,");
         else
            Put_Line ("null, null, null,");
         end if;
      end if;
      Put_Line (Main_Package_Name & ".Create_Parser);");
      Close (File);
      Set_Output (Standard_Output);
   end Create_Ada_Test_Main;

begin
   case Common_Data.Lexer is
   when None | re2c_Lexer =>
      null;

   when Elisp_Lexer =>
      raise User_Error with WisiToken.Generate.Error_Message
        (Input_Data.Grammar_Lexer.File_Name, 1, "Ada output language does not support " & Lexer_Image
           (Common_Data.Lexer).all & " lexer");
   end case;

   case Tuple.Interface_Kind is
   when None  =>
      null;

   when Module | Process =>
      raise User_Error with WisiToken.Generate.Error_Message
        (Input_Data.Grammar_Lexer.File_Name, 1, "Ada output language does not support setting Interface");
   end case;

   declare
      Main_Package_Name    : constant String := File_Name_To_Ada (Output_File_Name_Root & Gen_Alg_Name) & "_Main";
      Actions_Package_Name : constant String := File_Name_To_Ada (Output_File_Name_Root) & "_Actions";
   begin
      if Input_Data.Action_Count > 0 or Input_Data.Check_Count > 0 then
         --  Some WisiToken tests have no actions or checks.
         Create_Ada_Actions_Body
           (Generate_Data.Action_Names, Generate_Data.Check_Names, Input_Data.Label_Count, Actions_Package_Name);
      end if;

      Create_Ada_Actions_Spec
        (Output_File_Name_Root & "_actions.ads", Actions_Package_Name, Input_Data, Common_Data, Generate_Data);

      if Tuple.Gen_Alg = External then
         Create_External_Main_Spec (Main_Package_Name, Tuple, Input_Data);
         Create_Ada_Main_Body (Actions_Package_Name, Main_Package_Name);
      else
         Create_Ada_Main_Body (Actions_Package_Name, Main_Package_Name);

         Create_Ada_Main_Spec (To_Lower (Main_Package_Name) & ".ads", Main_Package_Name, Input_Data, Common_Data);

         if Test_Main then
            Create_Ada_Test_Main (Actions_Package_Name, Main_Package_Name);
         end if;
      end if;
   end;

exception
when others =>
   Set_Output (Standard_Output);
   raise;
end WisiToken.BNF.Output_Ada;
