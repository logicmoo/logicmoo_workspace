--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

pragma License (GPL);

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with System.Multiprocessors;
with WisiToken.BNF.Generate_Grammar;
with WisiToken.BNF.Utils;
with WisiToken.Generate; use WisiToken.Generate;
with WisiToken.Parse.LR;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
package body WisiToken.BNF.Output_Ada_Common is

   --  Body subprograms, alphabetical

   function Duplicate_Reduce (State : in Parse.LR.Parse_State) return Boolean
   is
      use Parse.LR;
      Action_Node : Parse_Action_Node_Ptr;
      First       : Boolean := True;
      Action      : Reduce_Action_Rec;
   begin
      for Node of State.Action_List loop
         Action_Node := Node.Actions;
         if Action_Node.Next /= null then
            --  conflict
            return False;
         elsif Action_Node.Item.Verb /= Reduce then
            return False;
         end if;

         if First then
            Action    := Action_Node.Item;
            First     := False;
         else
            if not Equal (Action, Action_Node.Item) then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Duplicate_Reduce;

   function Image (Item : in Boolean) return String
     is (if Item then "True" else "False");

   function Symbols_Image (State : in Parse.LR.Parse_State) return String
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Strings.Unbounded;

      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      if State.Action_List.Length = 1 then
         return "(1 => " & Token_ID'Image (State.Action_List (1).Symbol) & ")";
      else
         Result := +"(";
         for Node of State.Action_List loop
            Result := Result &
              (if Need_Comma then ", " else "") &
              Trimmed_Image (Node.Symbol);
            Need_Comma := True;
         end loop;
         Result := Result & ")";
         return -Result;
      end if;
   end Symbols_Image;

   ----------
   --  Public subprograms in alphabetical order

   procedure Create_Ada_Actions_Spec
     (Output_File_Name :         in String;
      Package_Name     :         in String;
      Input_Data       :         in WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data      :         in Output_Ada_Common.Common_Data;
      Generate_Data    : aliased in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use Generate_Utils;

      Descriptor  : WisiToken.Descriptor renames Generate_Data.Descriptor.all;
      Spec_File : File_Type;
      Paren_Done  : Boolean      := False;
      Cursor      : Token_Cursor := First (Generate_Data, Non_Grammar => True, Nonterminals => True);
   begin
      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header
        (Ada_Comment, Use_Tuple => True, Tuple =>
           (Common_Data.Generate_Algorithm, Common_Data.Output_Language, Common_Data.Lexer, Common_Data.Interface_Kind,
            Common_Data.Text_Rep));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      if not (Input_Data.Action_Count > 0 or Input_Data.Check_Count > 0) then
         Put_Line ("with WisiToken;");
      end if;
      if Input_Data.Action_Count > 0 then
         Put_Line ("with WisiToken.Syntax_Trees;");
      end if;
      if Input_Data.Check_Count > 0 then
         Put_Line ("with WisiToken.Lexer;");
         Put_Line ("with WisiToken.Semantic_Checks;");
      end if;
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Context));
      Put_Line ("package " & Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Pre));

      Indent_Line ("Descriptor : aliased WisiToken.Descriptor :=");
      Indent_Line ("  (First_Terminal    =>" & WisiToken.Token_ID'Image (Descriptor.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal     =>" & WisiToken.Token_ID'Image (Descriptor.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal =>" & WisiToken.Token_ID'Image (Descriptor.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal  =>" & WisiToken.Token_ID'Image (Descriptor.Last_Nonterminal) & ",");
      Indent_Line ("EOI_ID            =>" & WisiToken.Token_ID'Image (Descriptor.EOI_ID) & ",");
      Indent_Line ("Accept_ID         =>" & WisiToken.Token_ID'Image (Descriptor.Accept_ID) & ",");
      Indent_Line ("Case_Insensitive  => " & Image (Input_Data.Language_Params.Case_Insensitive) & ",");
      Indent_Line ("New_Line_ID       =>" & WisiToken.Token_ID'Image (Descriptor.New_Line_ID) & ",");
      Indent_Line ("String_1_ID       =>" & WisiToken.Token_ID'Image (Descriptor.String_1_ID) & ",");
      Indent_Line ("String_2_ID       =>" & WisiToken.Token_ID'Image (Descriptor.String_2_ID) & ",");
      Indent_Line ("Image             =>");
      Indent_Start ("  (");
      Indent := Indent + 3;
      loop
         exit when Is_Done (Cursor);
         if Paren_Done then
            Indent_Start ("new String'(""" & (Name (Cursor)));
         else
            Put ("new String'(""" & (Name (Cursor)));
            Paren_Done := True;
         end if;
         Next (Cursor, Nonterminals => True);
         if Is_Done (Cursor) then
            Put_Line (""")),");
         else
            Put_Line ("""),");
         end if;
      end loop;

      Indent := Indent - 3;
      Indent_Line ("Terminal_Image_Width =>" & Integer'Image (Descriptor.Terminal_Image_Width) & ",");
      Indent_Line ("Image_Width          =>" & Integer'Image (Descriptor.Image_Width) & ",");
      Indent_Line ("Last_Lookahead       =>" & WisiToken.Token_ID'Image (Descriptor.Last_Lookahead) & ");");
      Indent := Indent - 3;
      New_Line;

      if Input_Data.Language_Params.Declare_Enums then
         Paren_Done := False;

         Cursor := First (Generate_Data, Non_Grammar => True, Nonterminals => True);
         Indent_Line ("type Token_Enum_ID is");
         Indent_Start ("  (");
         Indent := Indent + 3;
         loop
            exit when Is_Done (Cursor);
            if Paren_Done then
               Indent_Start (To_Token_Ada_Name (Name (Cursor)));
            else
               Put (To_Token_Ada_Name (Name (Cursor)));
               Paren_Done := True;
            end if;
            Next (Cursor, Nonterminals => True);
            if Is_Done (Cursor) then
               Put_Line (");");
            else
               Put_Line (",");
            end if;
         end loop;

         Indent := Indent - 3;
         New_Line;

         Indent_Line ("type Token_Enum_ID_Array is array (Positive range <>) of Token_Enum_ID;");
         Indent_Line ("use all type WisiToken.Token_ID;");
         Indent_Line ("function ""+"" (Item : in Token_Enum_ID) return WisiToken.Token_ID");
         Indent_Line ("  is (WisiToken.Token_ID'First + Token_Enum_ID'Pos (Item));");

         Indent_Line ("function To_Token_Enum (Item : in WisiToken.Token_ID) return Token_Enum_ID");
         Indent_Line ("  is (Token_Enum_ID'Val (Item - WisiToken.Token_ID'First));");
         Indent_Line ("function ""-"" (Item : in WisiToken.Token_ID) return Token_Enum_ID renames To_Token_Enum;");
         New_Line;

      end if;

      for Name_List of Generate_Data.Action_Names.all loop
         if Name_List /= null then
            for Name of Name_List.all loop
               if Name /= null then
                  Indent_Line ("procedure " & Name.all);
                  Indent_Line (" (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;");
                  Indent_Line ("  Tree      : in out WisiToken.Syntax_Trees.Tree;");
                  Indent_Line ("  Nonterm   : in     WisiToken.Valid_Node_Index;");
                  Indent_Line ("  Tokens    : in     WisiToken.Valid_Node_Index_Array);");
               end if;
            end loop;
         end if;
      end loop;

      for Name_List of Generate_Data.Check_Names.all loop
         if Name_List /= null then
            for Name of Name_List.all loop
               if Name /= null then
                  Indent_Line ("function " & Name.all);
                  Indent_Line (" (Lexer          : access constant WisiToken.Lexer.Instance'Class;");
                  Indent_Line ("  Nonterm        : in out WisiToken.Recover_Token;");
                  Indent_Line ("  Tokens         : in     WisiToken.Recover_Token_Array;");
                  Indent_Line ("  Recover_Active : in     Boolean)");
                  Indent_Line (" return WisiToken.Semantic_Checks.Check_Status;");
               end if;
            end loop;
         end if;
      end loop;

      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Actions_Spec_Post));

      Put_Line ("end " & Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);

   end Create_Ada_Actions_Spec;

   procedure Create_Ada_Main_Spec
     (Output_File_Name  : in String;
      Main_Package_Name : in String;
      Input_Data        : in WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data       : in Output_Ada_Common.Common_Data)
   is
      Lower_Package_Name : constant String := To_Lower (Main_Package_Name);

      Spec_File : File_Type;

      procedure LR_Process
      is begin
         Indent_Line ("procedure Create_Parser");
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("  (Parser                         :    out WisiToken.Parse.LR.Parser.Parser;");
            Indent_Line ("   Language_Fixes                 : in     WisiToken.Parse.LR.Parser.Language_Fixes_Access;");
            Indent_Line ("   Language_Matching_Begin_Tokens : in     " &
                           "WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;");
            Indent_Line ("   Language_String_ID_Set         : in     " &
                           "WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;");
         else
            Indent_Line ("  (Parser                       :    out WisiToken.Parse.LR.Parser_No_Recover.Parser;");
            Indent_Line ("   --  no error recovery");
         end if;
         Indent_Line ("   Trace                        : not null access WisiToken.Trace'Class;");
         Indent_Start ("   User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access");

         if Common_Data.Text_Rep then
            Put_Line (";");
            Indent_Line ("   Text_Rep_File_Name : in String);");
         else
            Put_Line (");");
         end if;
         New_Line;
      end LR_Process;

      procedure Packrat_Process
      is begin
         Indent_Line ("function Create_Parser");
         Indent_Line ("  (Trace     : not null access WisiToken.Trace'Class;");
         Indent_Line ("   User_Data : in     WisiToken.Syntax_Trees.User_Data_Access)");
         Indent_Line ("  return WisiToken.Parse.Base_Parser'Class;");
         New_Line;
      end Packrat_Process;

   begin
      if Common_Data.Generate_Algorithm = External then
         raise SAL.Programmer_Error;
      end if;

      Create (Spec_File, Out_File, Output_File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header
        (Ada_Comment, Use_Tuple => True, Tuple =>
           (Common_Data.Generate_Algorithm, Common_Data.Output_Language, Common_Data.Lexer, Common_Data.Interface_Kind,
            Common_Data.Text_Rep));
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      case Common_Data.Output_Language is
      when Ada_Lang =>
         Put_Line ("with WisiToken.Syntax_Trees;");

      when Ada_Emacs_Lang =>
         case Common_Data.Interface_Kind is
         when Process =>
            Put_Line ("with WisiToken.Syntax_Trees;");

         when Module =>
            Put_Line ("with Emacs_Module_Aux;");
            Put_Line ("with emacs_module_h;");
            Put_Line ("with Interfaces.C;");
            Put_Line ("with WisiToken.Semantic_State;");
         end case;
      end case;

      case Common_Data.Generate_Algorithm is
      when LR_Generate_Algorithm =>
         if Input_Data.Language_Params.Error_Recover then
            Put_Line ("with WisiToken.Parse.LR.Parser;");
         else
            Put_Line ("with WisiToken.Parse.LR.Parser_No_Recover;");
         end if;

      when Packrat_Generate_Algorithm =>
         Put_Line ("with WisiToken.Parse;");

      when External =>
         null;
      end case;

      Put_Line ("package " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      case Common_Data.Output_Language is
      when Ada_Lang =>
         case Common_Data.Generate_Algorithm is
         when LR_Generate_Algorithm =>
            LR_Process;
         when Packrat_Generate_Algorithm =>
            Packrat_Process;
         when External =>
            null;
         end case;

      when Ada_Emacs_Lang =>
         case Common_Data.Interface_Kind is
         when Process =>
            case Common_Data.Generate_Algorithm is
            when LR_Generate_Algorithm =>
               LR_Process;
            when Packrat_Generate_Algorithm =>
               Packrat_Process;
            when External =>
               null;
            end case;

         when Module =>
            Indent_Line ("function Parse (Env : Emacs_Module_Aux.Emacs_Env_Access) return emacs_module_h.emacs_value;");
            Indent_Line ("pragma Export (C, Parse, """ & Lower_Package_Name & "_wisi_module_parse"");");
            Indent_Line ("function Init (Env : Emacs_Module_Aux.Emacs_Env_Access) return Interfaces.C.int;");
            Indent_Line ("pragma Export (C, Init, """ & Lower_Package_Name & "_wisi_module_parse_init"");");
            New_Line;

         end case;
      end case;

      Put_Line ("end " & Main_Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);
   end Create_Ada_Main_Spec;

   procedure Create_External_Main_Spec
     (Main_Package_Name    : in String;
      Tuple                : in Generate_Tuple;
      Input_Data           : in WisiToken_Grammar_Runtime.User_Data_Type)
   is
      File_Name : constant String := To_Lower (Main_Package_Name) & ".ads";
      Spec_File : File_Type;
   begin
      Create (Spec_File, Out_File, File_Name);
      Set_Output (Spec_File);
      Indent := 1;

      Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      Put_Line ("with WisiToken.Productions;");
      Put_Line ("package " & Main_Package_Name & " is");
      Indent := Indent + 3;
      New_Line;

      Indent_Line ("function Create_Grammar return WisiToken.Productions.Prod_Arrays.Vector;");

      Indent := Indent - 3;
      Put_Line ("end " & Main_Package_Name & ";");
      Close (Spec_File);
      Set_Output (Standard_Output);
   end Create_External_Main_Spec;

   procedure Create_LR_Parser_Core_1
     (Common_Data   : in Output_Ada_Common.Common_Data;
      Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use Ada.Strings.Unbounded;

      subtype Nonterminal_ID is Token_ID range
        Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index;

      Table : WisiToken.Parse.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
      Line  : Unbounded_String;

      procedure Append (Item : in String)
      is begin
         Line := Line & Item;
      end Append;

      procedure Put (Label : in String; Item : in Token_ID_Array_Natural)
      is begin
         Indent_Line (Label & " =>");
         Indent_Start ("  (");
         Indent := Indent + 3;
         Line := +"";
         for I in Item'Range loop
            Append (Trimmed_Image (Item (I)));

            if I = Item'Last then
               Append ("),");

            else
               Append (", ");
            end if;
         end loop;
         Indent_Wrap (-Line);
         Indent := Indent - 3;
      end Put;

   begin
      Indent_Line ("McKenzie_Param : constant McKenzie_Param_Type :=");
      Indent_Line ("  (First_Terminal    =>" & Token_ID'Image (Table.McKenzie_Param.First_Terminal) & ",");
      Indent := Indent + 3;
      Indent_Line ("Last_Terminal     =>" & Token_ID'Image (Table.McKenzie_Param.Last_Terminal) & ",");
      Indent_Line ("First_Nonterminal =>" & Token_ID'Image (Table.McKenzie_Param.First_Nonterminal) & ",");
      Indent_Line ("Last_Nonterminal  =>" & Token_ID'Image (Table.McKenzie_Param.Last_Nonterminal) & ",");
      Put ("Insert", Table.McKenzie_Param.Insert);
      Put ("Delete", Table.McKenzie_Param.Delete);
      Put ("Push_Back", Table.McKenzie_Param.Push_Back);
      Put ("Undo_Reduce", Table.McKenzie_Param.Undo_Reduce);
      Indent_Line
        ("Minimal_Complete_Cost_Delta => " & Integer'Image (Table.McKenzie_Param.Minimal_Complete_Cost_Delta) & ",");
      Indent_Line ("Fast_Forward => " & Integer'Image (Table.McKenzie_Param.Fast_Forward) & ",");
      Indent_Line ("Matching_Begin => " & Integer'Image (Table.McKenzie_Param.Matching_Begin) & ",");
      Indent_Line ("Ignore_Check_Fail  =>" & Integer'Image (Table.McKenzie_Param.Ignore_Check_Fail) & ",");
      Indent_Line ("Task_Count  =>" & System.Multiprocessors.CPU_Range'Image
                     (Table.McKenzie_Param.Task_Count) & ",");
      Indent_Line ("Check_Limit =>" & Token_Index'Image (Table.McKenzie_Param.Check_Limit) & ",");
      Indent_Line ("Check_Delta_Limit =>" & Integer'Image (Table.McKenzie_Param.Check_Delta_Limit) & ",");
      Indent_Line ("Enqueue_Limit =>" & Integer'Image (Table.McKenzie_Param.Enqueue_Limit) & ");");
      Indent := Indent - 3;
      New_Line;

      if Common_Data.Text_Rep then
         Indent_Line ("function Actions return WisiToken.Parse.LR.Semantic_Action_Array_Arrays.Vector");
         Indent_Line ("is begin");
         Indent := Indent + 3;
         Indent_Line ("return Acts : WisiToken.Parse.LR.Semantic_Action_Array_Arrays.Vector do");
         Indent := Indent + 3;
         Indent_Line
           ("Acts.Set_First_Last (" & Trimmed_Image (Generate_Data.Grammar.First_Index) & ", " &
              Trimmed_Image (Generate_Data.Grammar.Last_Index) & ");");

         for I in Nonterminal_ID loop
            declare
               P : Productions.Instance renames Generate_Data.Grammar (I);
            begin
               if Generate_Data.Action_Names (P.LHS) /= null or Generate_Data.Check_Names (P.LHS) /= null then
                  Indent_Line
                    ("Acts (" & Trimmed_Image (P.LHS) & ").Set_First_Last (0," &
                       Integer'Image (P.RHSs.Last_Index) & ");");

                  for J in P.RHSs.First_Index .. P.RHSs.Last_Index loop
                     if (Generate_Data.Action_Names (P.LHS) /= null and then
                           Generate_Data.Action_Names (P.LHS)(J) /= null)
                       or
                       (Generate_Data.Check_Names (P.LHS) /= null and then
                          Generate_Data.Check_Names (P.LHS) /= null)
                     then
                        Indent_Wrap
                          ("Acts (" & Trimmed_Image (P.LHS) & ")(" & Trimmed_Image (J) & ") := (" &
                             (if Generate_Data.Action_Names (P.LHS) = null then "null"
                              elsif Generate_Data.Action_Names (P.LHS)(J) = null then "null"
                              else Generate_Data.Action_Names (P.LHS)(J).all & "'Access") & ", " &
                             (if Generate_Data.Check_Names (P.LHS) = null then "null"
                              elsif Generate_Data.Check_Names (P.LHS)(J) = null then "null"
                              else Generate_Data.Check_Names (P.LHS)(J).all & "'Access") & ");");
                     end if;
                  end loop;
               end if;
            end;
         end loop;
         Indent := Indent - 3;
         Indent_Line ("end return;");
         Indent := Indent - 3;
         Indent_Line ("end Actions;");
         New_Line;
      end if;
   end Create_LR_Parser_Core_1;

   procedure Create_LR_Parser_Table
     (Input_Data    : in WisiToken_Grammar_Runtime.User_Data_Type;
      Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      use all type Ada.Containers.Count_Type;
      use WisiToken.Parse.LR;
      use Ada.Strings.Unbounded;

      Table            : WisiToken.Parse.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
      Lines_Per_Subr   : constant := 1000;
      Subr_Count       : Integer  := 1;
      Last_Subr_Closed : Boolean  := False;
      Line             : Unbounded_String;

      procedure Append (Item : in String)
      is begin
         Line := Line & Item;
      end Append;
   begin
      --  Optimize source structure for GNAT compile time; one subroutine
      --  with thousands of "Table.States (*) := ..." takes forever to
      --  compile (apparently depending on available memory). But hundreds
      --  of subroutines, containing the same lines in chunks of 1000,
      --  compiles in acceptable time.

      Indent_Line ("declare");
      Indent := Indent + 3;

      Indent_Line ("procedure Subr_" & Trimmed_Image (Subr_Count));
      Indent_Line ("is begin");
      Indent     := Indent + 3;
      Line_Count := 0;

      Declare_Subroutines :
      for State_Index in Table.States'Range loop
         Actions :
         declare
            use Ada.Containers;
            Base_Indent : constant Ada.Text_IO.Count := Indent;
         begin
            Indent_Line
              ("Table.States (" & Trimmed_Image (State_Index) & ").Action_List.Set_Capacity (" &
                 Trimmed_Image (Table.States (State_Index).Action_List.Length) & ");");

            if Duplicate_Reduce (Table.States (State_Index)) then
               if Table.States (State_Index).Action_List.Length > 0 then
                  --  We only get here with Length = 0 when there's a bug in LALR_Generate.
                  declare
                     Node   : Action_Node renames Table.States (State_Index).Action_List (1);
                     Action : constant Reduce_Action_Rec := Node.Actions.Item;
                  begin
                     Set_Col (Indent);
                     Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                       Symbols_Image (Table.States (State_Index)) & ", " &
                       Image (Action.Production) & ", " &
                       Count_Type'Image (Action.Token_Count) & ", ";

                     Append
                       ((if Generate_Data.Action_Names (Action.Production.LHS) = null then "null"
                         elsif Generate_Data.Action_Names
                           (Action.Production.LHS)(Action.Production.RHS) = null then "null"
                         else Generate_Data.Action_Names
                           (Action.Production.LHS)(Action.Production.RHS).all & "'Access"));
                     Append (", ");
                     Append
                       ((if Generate_Data.Check_Names (Action.Production.LHS) = null then "null"
                         elsif Generate_Data.Check_Names
                           (Action.Production.LHS)(Action.Production.RHS) = null then "null"
                         else Generate_Data.Check_Names
                           (Action.Production.LHS)(Action.Production.RHS).all & "'Access"));

                     Indent_Wrap (-Line & ");");
                     Line_Count := Line_Count + 1;
                     Indent     := Base_Indent;
                  end;
               end if;

            else
               for Node of Table.States (State_Index).Action_List loop
                  Set_Col (Indent);
                  declare
                     Action_Node : Parse_Action_Node_Ptr := Node.Actions;
                  begin
                     case Action_Node.Item.Verb is
                     when Shift =>
                        Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                          Trimmed_Image (Node.Symbol) & ", ";
                        Append (Image (Action_Node.Item.Production) & ", ");
                        Append (Trimmed_Image (Action_Node.Item.State));
                        Append (");");

                     when Reduce | Accept_It =>
                        Line := +"Add_Action (Table.States (" & Trimmed_Image (State_Index) & "), " &
                          Trimmed_Image (Node.Symbol);
                        if Action_Node.Item.Verb = Reduce then
                           Append (", Reduce");
                        else
                           Append (", Accept_It");
                        end if;
                        Append (", ");
                        Append (Image (Action_Node.Item.Production) & ", ");
                        Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                        Append
                          ((if Generate_Data.Action_Names (Action_Node.Item.Production.LHS) = null then "null"
                            elsif Generate_Data.Action_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                            then "null"
                            else Generate_Data.Action_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                               "'Access"));
                        Append (", ");
                        Append
                          ((if Generate_Data.Check_Names (Action_Node.Item.Production.LHS) = null then "null"
                            elsif Generate_Data.Check_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                            then "null"
                            else Generate_Data.Check_Names
                              (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                               "'Access"));
                        Append (");");

                     when Parse.LR.Error =>
                        raise SAL.Programmer_Error;
                     end case;
                     Indent_Wrap (-Line);
                     Line_Count := Line_Count + 1;

                     loop
                        Action_Node := Action_Node.Next;
                        exit when Action_Node = null;
                        --  There is a conflict; must be Shift/{Reduce|Accept} or Reduce/{Reduce|Accept}.
                        --  The added parameters are the same in either case.
                        case Action_Node.Item.Verb is
                        when Reduce | Accept_It =>
                           Line := +"Add_Conflict (Table.States (" & Trimmed_Image (State_Index) & "), " &
                             Trimmed_Image (Node.Symbol) & ", ";
                           Append (Image (Action_Node.Item.Production) & ", ");
                           Append (Count_Type'Image (Action_Node.Item.Token_Count) & ", ");
                           Append
                             ((if Generate_Data.Action_Names (Action_Node.Item.Production.LHS) = null then "null"
                               elsif Generate_Data.Action_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                               then "null"
                               else Generate_Data.Action_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                                  "'Access"));
                           Append (", ");
                           Append
                             ((if Generate_Data.Check_Names (Action_Node.Item.Production.LHS) = null then "null"
                               elsif Generate_Data.Check_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS) = null
                               then "null"
                               else Generate_Data.Check_Names
                                 (Action_Node.Item.Production.LHS)(Action_Node.Item.Production.RHS).all &
                                  "'Access"));
                           Indent_Wrap (-Line & ");");
                           Line_Count := Line_Count + 1;

                        when others =>
                           raise SAL.Programmer_Error with "invalid conflict action verb: " &
                             Parse.LR.Parse_Action_Verbs'Image (Action_Node.Item.Verb);
                        end case;
                     end loop;
                  end;
                  Indent := Base_Indent;
               end loop;
            end if;
         end Actions;

         if Table.States (State_Index).Goto_List.Length > 0 then
            Indent_Line
              ("Table.States (" & Trimmed_Image (State_Index) & ").Goto_List.Set_Capacity (" &
                 Trimmed_Image (Table.States (State_Index).Goto_List.Length) & ");");
         end if;
         Gotos :
         for Node of Table.States (State_Index).Goto_List loop
            Set_Col (Indent);
            Put ("Add_Goto (Table.States (" & Trimmed_Image (State_Index) & "), ");
            Put_Line (Trimmed_Image (Node.Symbol) & ", " & Trimmed_Image (Node.State) & ");");
            Line_Count := Line_Count + 1;
         end loop Gotos;

         if Input_Data.Language_Params.Error_Recover then
            if Table.States (State_Index).Kernel.Length > 0 then
               Indent_Wrap
                 ("Table.States (" & Trimmed_Image (State_Index) & ").Kernel := To_Vector (" &
                    Image (Table.States (State_Index).Kernel, Strict => True) & ");");
            end if;
            if Table.States (State_Index).Minimal_Complete_Actions.Length > 0 then
               Indent_Wrap
                 ("Table.States (" & Trimmed_Image (State_Index) & ").Minimal_Complete_Actions := To_Vector (" &
                    Strict_Image (Table.States (State_Index).Minimal_Complete_Actions, Strict => True) & ");");
            end if;
         end if;

         if Line_Count > Lines_Per_Subr then
            Line_Count := 0;
            Indent := Indent - 3;
            Indent_Line ("end Subr_" & Trimmed_Image (Subr_Count) & ";");

            if State_Index < Table.States'Last then
               Subr_Count := Subr_Count + 1;
               Last_Subr_Closed := False;
               Indent_Line ("procedure Subr_" & Trimmed_Image (Subr_Count));
               Indent_Line ("is begin");
               Indent := Indent + 3;
            else
               Last_Subr_Closed := True;
            end if;
         end if;

      end loop Declare_Subroutines;

      if not Last_Subr_Closed then
         Indent := Indent - 3;
         Indent_Line ("end Subr_" & Trimmed_Image (Subr_Count) & ";");
      end if;

      Indent := Indent - 3;
      Indent_Line ("begin");
      Indent := Indent + 3;

      for Subr in 1 .. Subr_Count loop
         Indent_Line ("Subr_" & Trimmed_Image (Subr) & ";");
      end loop;
      Indent_Line ("Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);");
      Indent := Indent - 3;
      Indent_Line ("end;");
   end Create_LR_Parser_Table;

   procedure LR_Create_Create_Parser
     (Input_Data    :         in     WisiToken_Grammar_Runtime.User_Data_Type;
      Common_Data   :         in out Output_Ada_Common.Common_Data;
      Generate_Data : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data)
   is
      Table : WisiToken.Parse.LR.Parse_Table_Ptr renames Generate_Data.LR_Parse_Table;
   begin
      Indent_Line ("procedure Create_Parser");
      case Common_Data.Interface_Kind is
      when Process =>
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("  (Parser                         :    out WisiToken.Parse.LR.Parser.Parser;");
            Indent_Line ("   Language_Fixes                 : in     WisiToken.Parse.LR.Parser.Language_Fixes_Access;");
            Indent_Line ("   Language_Matching_Begin_Tokens : in     " &
                           "WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;");
            Indent_Line
              ("   Language_String_ID_Set       : in     WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;");
         else
            Indent_Line ("  (Parser                         :    out WisiToken.Parse.LR.Parser_No_Recover.Parser;");
         end if;
         Indent_Line ("   Trace                        : not null access WisiToken.Trace'Class;");
         Indent_Start ("   User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access");

      when Module =>
         Indent_Line ("  (Parser              :    out WisiToken.Parse.LR.Parser.Parser;");
         Indent_Line ("   Env                 : in     Emacs_Env_Access;");
         Indent_Start ("   Lexer_Elisp_Symbols : in     Lexers.Elisp_Array_Emacs_Value");
      end case;

      if Common_Data.Text_Rep then
         Put_Line (";");
         Indent_Line ("   Text_Rep_File_Name : in String)");
      else
         Put_Line (")");
      end if;

      Indent_Line ("is");
      Indent := Indent + 3;

      Indent_Line ("use WisiToken.Parse.LR;");

      if Common_Data.Text_Rep then
         Create_LR_Parser_Core_1 (Common_Data, Generate_Data);
         Indent_Line ("Table : constant Parse_Table_Ptr := Get_Text_Rep");
         Indent_Line ("  (Text_Rep_File_Name, McKenzie_Param, Actions);");
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;

      else
         if Input_Data.Language_Params.Error_Recover then
            Create_LR_Parser_Core_1 (Common_Data, Generate_Data);
         end if;

         Indent_Line ("Table : constant Parse_Table_Ptr := new Parse_Table");
         Indent_Line ("  (State_First       => 0,");
         Indent := Indent + 3;
         Indent_Line ("State_Last        =>" & State_Index'Image (Table.State_Last) & ",");
         Indent_Line ("First_Terminal    =>" & Token_ID'Image (Table.First_Terminal) & ",");
         Indent_Line ("Last_Terminal     =>" & Token_ID'Image (Table.Last_Terminal) & ",");
         Indent_Line ("First_Nonterminal =>" & Token_ID'Image (Table.First_Nonterminal) & ",");
         Indent_Line ("Last_Nonterminal  =>" & Token_ID'Image (Table.Last_Nonterminal) & ");");
         Indent := Indent - 3;

         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("Table.McKenzie_Param := McKenzie_Param;");
         end if;
         Create_LR_Parser_Table (Input_Data, Generate_Data);
         New_Line;
      end if;

      if Input_Data.Language_Params.Error_Recover then
         Indent_Line ("WisiToken.Parse.LR.Parser.New_Parser");
      else
         Indent_Line ("WisiToken.Parse.LR.Parser_No_Recover.New_Parser");
      end if;
      Indent_Line ("  (Parser,");
      case Common_Data.Interface_Kind is
      when Process =>
         Indent_Line ("   Trace,");
         Indent_Line ("   Lexer.New_Lexer (Trace.Descriptor),");
         Indent_Line ("   Table,");
         if Input_Data.Language_Params.Error_Recover then
            Indent_Line ("   Language_Fixes,");
            Indent_Line ("   Language_Matching_Begin_Tokens,");
            Indent_Line ("   Language_String_ID_Set,");
         end if;
         Indent_Line ("   User_Data,");
         Indent_Line ("   Max_Parallel         => 15,");
         Indent_Line ("   Terminate_Same_State => True);");

      when Module =>
         Indent_Line ("   Lexer.New_Lexer (Env, Lexer_Elisp_Symbols),");
         Indent_Line ("   Table, Max_Parallel => 15, Terminate_Same_State => True);");

      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
   end LR_Create_Create_Parser;

   procedure Packrat_Create_Create_Parser
     (Common_Data   :         in out Output_Ada_Common.Common_Data;
      Generate_Data : aliased in     WisiToken.BNF.Generate_Utils.Generate_Data;
      Packrat_Data  :         in     WisiToken.Generate.Packrat.Data)
   is
      use Ada.Strings.Unbounded;

      Text     : Unbounded_String;
      Need_Bar : Boolean := True;
   begin
      Indent_Line ("function Create_Parser");
      Indent_Line ("  (Trace     : not null access WisiToken.Trace'Class;");
      Indent_Line ("   User_Data : in     WisiToken.Syntax_Trees.User_Data_Access)");
      Indent_Line ("  return WisiToken.Parse.Base_Parser'Class");

      case Packrat_Generate_Algorithm'(Common_Data.Generate_Algorithm) is
      when Packrat_Gen =>
         Indent_Line ("is begin");
         Indent := Indent + 3;
         Indent_Line ("return Parser : WisiToken.Parse.Packrat.Generated.Parser do");
         Indent := Indent + 3;
         Indent_Line ("Parser.Trace := Trace;");
         Indent_Line ("Parser.Lexer := Lexer.New_Lexer (Trace.Descriptor);");
         Indent_Line ("Parser.User_Data := User_Data;");
         Indent_Line ("Parser.Parse_WisiToken_Accept := Parse_wisitoken_accept_1'Access;");
         Indent := Indent - 3;
         Indent_Line ("end return;");

      when Packrat_Proc =>
         Indent_Line ("is");
         Indent := Indent + 3;
         Indent_Line ("use WisiToken;");
         Indent_Line ("use WisiToken.Productions;");
         Indent_Line ("Grammar               : Prod_Arrays.Vector;");
         Indent_Line
           ("Direct_Left_Recursive : constant WisiToken.Token_ID_Set (" &
              Trimmed_Image (Generate_Data.Grammar.First_Index) & " .. " &
              Trimmed_Image (Generate_Data.Grammar.Last_Index) & ") :=");

         Need_Bar := False;
         if Any (Packrat_Data.Direct_Left_Recursive) then
            for I in Packrat_Data.Direct_Left_Recursive'Range loop
               if Packrat_Data.Direct_Left_Recursive (I) then
                  if Need_Bar then
                     Text := Text & " | ";
                  else
                     Need_Bar := True;
                  end if;
                  Text := Text & Trimmed_Image (I);
               end if;
            end loop;
            Indent_Start ("  (");
            Indent := Indent + 3;
            Indent_Wrap (-Text & " => True,");
            Indent_Line ("others => False);");
            Indent := Indent - 3;
         else
            Indent_Line ("  (others => False);");
         end if;
         Indent := Indent - 3;
         Indent_Line ("begin");
         Indent := Indent + 3;
         WisiToken.BNF.Generate_Grammar (Generate_Data.Grammar, Generate_Data.Action_Names.all);

         Indent_Line ("return WisiToken.Parse.Packrat.Procedural.Create");
         Indent_Line
           ("  (Grammar, Direct_Left_Recursive, " & Trimmed_Image (Generate_Data.Descriptor.Accept_ID) &
              ", Trace, Lexer.New_Lexer (Trace.Descriptor), User_Data);");
      end case;
      Indent := Indent - 3;
      Indent_Line ("end Create_Parser;");
      New_Line;
   end Packrat_Create_Create_Parser;

   procedure External_Create_Create_Grammar
     (Generate_Data : in WisiToken.BNF.Generate_Utils.Generate_Data)
   is begin
      Indent_Line ("function Create_Grammar return WisiToken.Productions.Prod_Arrays.Vector");
      Indent_Line ("is");
      Indent_Line ("   use WisiToken;");
      Indent_Line ("   use WisiToken.Productions;");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("return Grammar : WisiToken.Productions.Prod_Arrays.Vector do");
      Indent := Indent + 3;
      WisiToken.BNF.Generate_Grammar (Generate_Data.Grammar, Generate_Data.Action_Names.all);
      Indent := Indent - 3;
      Indent_Line ("end return;");
      Indent := Indent - 3;
      Indent_Line ("end Create_Grammar;");
   end External_Create_Create_Grammar;

   procedure Create_re2c
     (Input_Data            :         in WisiToken_Grammar_Runtime.User_Data_Type;
      Tuple                 :         in Generate_Tuple;
      Generate_Data         : aliased in WisiToken.BNF.Generate_Utils.Generate_Data;
      Output_File_Name_Root :         in String)
   is
      use Ada.Strings.Fixed;
      use Generate_Utils;
      use WisiToken.BNF.Utils;
      File : File_Type;
   begin
      Create (File, Out_File, Output_File_Name_Root & ".re2c");
      Set_Output (File);
      Indent := 1;

      Put_File_Header (C_Comment, " -*- mode: C -*-", Use_Tuple => True, Tuple => Tuple);
      Put_Raw_Code (C_Comment, Input_Data.Raw_Code (Copyright_License));
      New_Line;

      Indent_Line ("#include <stddef.h>"); -- size_t
      Indent_Line ("#include <stdio.h>"); -- printf
      Indent_Line ("#include <stdlib.h>"); -- malloc
      New_Line;

      Indent_Line ("typedef struct wisi_lexer");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("unsigned char* buffer;           // input text, in utf-8 encoding");
      Indent_Line ("unsigned char* buffer_last;      // last byte in buffer");
      Indent_Line ("unsigned char* cursor;           // current byte");
      Indent_Line ("unsigned char* byte_token_start; // byte position at start of current token");
      Indent_Line ("size_t         char_pos;         // character position of current character");
      Indent_Line ("size_t         char_token_start; // character position at start of current token");
      Indent_Line ("int            line;             // 1 indexed");
      Indent_Line ("int            line_token_start; // line at start of current token");
      Indent_Line ("unsigned char* marker;           // saved cursor");
      Indent_Line ("size_t         marker_pos;       // saved character position");
      Indent_Line ("size_t         marker_line;      // saved line");
      Indent_Line ("unsigned char* context;          // saved cursor");
      Indent_Line ("size_t         context_pos;      // saved character position");
      Indent_Line ("int            context_line;     // saved line");
      Indent_Line ("int            verbosity;");
      New_Line;
      Indent := Indent - 3;
      Indent_Line ("} wisi_lexer;");
      New_Line;
      Indent_Line ("#define YYCTYPE unsigned char");
      New_Line;

      --  Status values:
      Indent_Line ("#define NO_ERROR 0");
      Indent_Line ("#define ERROR_unrecognized_character 1");

      ----------
      --  new_lexer, free_lexer, reset_lexer

      --  It's normal to increment lexer->cursor one past the end of input,
      --  but not to read that character. To support memory mapped files, we
      --  enforce this strictly; YYPEEK returns EOT (end of text) when
      --  reading past end of buffer; that's how we recognize the end of
      --  text token.

      Indent_Line ("wisi_lexer* " & Output_File_Name_Root & "_new_lexer");
      Indent_Line ("   (unsigned char* input, size_t length, int verbosity)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("wisi_lexer* result        = malloc (sizeof (wisi_lexer));");
      Indent_Line ("result->buffer            = input;");
      Indent_Line ("result->buffer_last       = input + length - 1;");
      Indent_Line ("result->cursor            = input;");
      Indent_Line ("result->byte_token_start  = input;");
      Indent_Line ("result->char_pos          = 1; /* match WisiToken.Buffer_Region */");
      Indent_Line ("result->char_token_start  = 1;");
      Indent_Line ("result->line              = (*result->cursor == 0x0A) ? 2 : 1;");
      Indent_Line ("result->line_token_start  = result->line;");
      Indent_Line ("result->verbosity         = verbosity;");
      Indent_Line ("return result;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_free_lexer(wisi_lexer** lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("free(*lexer);");
      Indent_Line ("*lexer = 0;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("void");
      Indent_Line (Output_File_Name_Root & "_reset_lexer(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("lexer->cursor   = lexer->buffer;");
      Indent_Line ("lexer->char_pos = 1;");
      Indent_Line ("lexer->line     = (*lexer->cursor == 0x0A) ? 2 : 1;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      ----------
      --  next_token utils

      Indent_Line ("static void debug(wisi_lexer* lexer, int state, unsigned char ch)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->verbosity > 0)");
      Indent_Line ("   {");
      Indent_Line ("   if (ch < ' ')");
      Indent_Line ("      printf (""lexer: %d, 0x%x\n"", state, ch);");
      Indent_Line ("   else");
      Indent_Line ("      printf (""lexer: %d, '%c' 0x%x\n"", state, ch, ch);");
      Indent_Line ("   }");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Line ("#define YYDEBUG(state, ch) debug(lexer, state, ch)");

      --  YYCURSOR is only used in calls of YYDEBUG; we can't define it as
      --  YYPEEK because it is used as '*YYCURSOR'.
      Indent_Line ("#define YYCURSOR lexer->cursor");
      New_Line;

      Indent_Line ("#define YYPEEK() (lexer->cursor <= lexer->buffer_last) ? *lexer->cursor : 4");
      New_Line;

      Indent_Line ("static void skip(wisi_lexer* lexer)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last)");
      Indent_Line ("   ++lexer->cursor;");
      Indent_Line ("if (lexer->cursor <= lexer->buffer_last)");
      Indent_Line ("{");
      Indent_Line ("   /* UFT-8 encoding: https://en.wikipedia.org/wiki/UTF-8#Description */");
      Indent_Line ("   if (*lexer->cursor == 0x0A && lexer->cursor > lexer->buffer && *(lexer->cursor - 1) == 0x0D)");
      Indent_Line ("     {/* second byte of DOS line ending */");
      Indent_Line ("     }");
      Indent_Line ("   else if ((*lexer->cursor & 0x80) == 0x80 && (*lexer->cursor & 0xC0) != 0xC0)");
      Indent_Line ("     {/* byte 2, 3 or 4 of multi-byte UTF-8 char */");
      Indent_Line ("     }");
      Indent_Line ("   else");
      Indent_Line ("     ++lexer->char_pos;");
      Indent_Line ("   if (*lexer->cursor == 0x0A) ++lexer->line;");
      Indent_Line ("}");
      Indent := Indent - 3;
      Indent_Line ("}");
      Indent_Start ("#define YYSKIP() skip(lexer)");
      New_Line;

      Indent_Line ("#define YYBACKUP() lexer->marker = lexer->cursor; lexer->marker_pos = lexer->char_pos;" &
                     "lexer->marker_line = lexer->line");
      Indent_Line ("#define YYRESTORE() lexer->cursor = lexer->marker; lexer->char_pos = lexer->marker_pos;" &
                     "lexer->line = lexer->marker_line");
      Indent_Line ("#define YYBACKUPCTX() lexer->context = lexer->cursor; lexer->context_pos = lexer->char_pos;" &
                     "lexer->context_line = lexer->line");
      Indent_Line ("#define YYRESTORECTX() lexer->cursor = lexer->context; lexer->char_pos = lexer->context_pos;" &
                     "lexer->line = lexer->context_line");
      New_Line;

      if Is_In (Input_Data.Tokens.Tokens, "delimited-text") then
         Indent_Line ("static void skip_to(wisi_lexer* lexer, char* target)");
         Indent_Line ("{");
         Indent_Line ("  int i;");
         New_Line;
         Indent_Line ("  while (lexer->cursor <= lexer->buffer_last)");
         Indent_Line ("    {");
         Indent_Line ("      if (*lexer->cursor == target[0])");
         Indent_Line ("      {");
         Indent_Line ("        i = 0;");
         Indent_Line ("        do");
         Indent_Line ("          i++;");
         Indent_Line ("        while (0 != target[i] &&");
         Indent_Line ("               lexer->cursor + i <= lexer->buffer_last &&");
         Indent_Line ("               *(lexer->cursor + i) == target[i]);");
         New_Line;
         Indent_Line ("        if (0 == target[i])");
         Indent_Line ("          {");
         Indent_Line ("            for (i = 0; 0 != target[i]; i++)");
         Indent_Line ("               skip(lexer);");
         Indent_Line ("            break;");
         Indent_Line ("          }");
         Indent_Line ("      }");
         Indent_Line ("      skip(lexer);");
         Indent_Line ("    };");
         Indent_Line ("}");
         New_Line;
      end if;

      ----------
      --  next_token
      Indent_Line ("int " & Output_File_Name_Root & "_next_token");
      Indent_Line ("  (wisi_lexer* lexer,");
      Indent_Line ("   int* id,");
      Indent_Line ("   size_t* byte_position,");
      Indent_Line ("   size_t* byte_length,");
      Indent_Line ("   size_t* char_position,");
      Indent_Line ("   size_t* char_length,");
      Indent_Line ("   int*    line_start)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Indent_Line ("int status = NO_ERROR;");
      Indent_Line ("*id = -1;"); --  Token_ID'First = 0; see dragon_4_43.wy

      Indent_Line ("if (lexer->cursor > lexer->buffer_last)");
      Indent_Line ("{");
      Indent := Indent + 3;
      Indent_Line ("*id            =" & WisiToken.Token_ID'Image (Generate_Data.Descriptor.EOI_ID) & ";");
      Indent_Line ("*byte_position = lexer->buffer_last - lexer->buffer + 1;");
      Indent_Line ("*byte_length   = 0;");
      Indent_Line ("*char_position = lexer->char_token_start;");
      Indent_Line ("*char_length   = 0;");
      Indent_Line ("*line_start    = lexer->line;");
      Indent_Line ("return status;");
      Indent := Indent - 3;
      Indent_Line ("}");
      New_Line;

      Indent_Line ("lexer->byte_token_start = lexer->cursor;");
      Indent_Line ("lexer->char_token_start = lexer->char_pos;");
      Indent_Line ("if (*lexer->cursor == 0x0A)");
      Indent_Line ("   lexer->line_token_start = lexer->line-1;");
      Indent_Line ("else");
      Indent_Line ("   lexer->line_token_start = lexer->line;");
      New_Line;

      Indent_Line ("while (*id == -1 && status == 0)");
      Indent_Line ("{");
      Indent := Indent + 3;

      Put_Line ("/*!re2c");
      Indent_Line ("re2c:yyfill:enable   = 0;");
      Indent_Line ("re2c:sentinel   = 4;");
      New_Line;

      --  Regexps used in definitions
      for Pair of Input_Data.Tokens.re2c_Regexps loop
         Indent_Line (-Pair.Name & " = " & (-Pair.Value) & ";");
      end loop;
      New_Line;

      --  definitions
      for I in All_Tokens (Generate_Data).Iterate (Non_Grammar => True, Nonterminals => False) loop

         if 0 /= Index (Source => Value (I), Pattern => "/") then
            --  trailing context syntax; forbidden in definitions
            null;

         elsif Kind (I) = "EOI" then
            Indent_Line (Name (I) & " = [\x04];");

         elsif Kind (I) = "delimited-text" then
            --  not declared in definitions
            null;

         elsif Kind (I) = "keyword" and Input_Data.Language_Params.Case_Insensitive then
            --  This assumes re2c regular expression syntax, where single quote
            --  means case insensitive.
            Indent_Line (Name (I) & " = '" & Strip_Quotes (Value (I)) & "';");

         else
            --  Other kinds have values that are regular expressions, in re2c syntax
            Indent_Line (Name (I) & " = " & Value (I) & ";");
         end if;
      end loop;
      New_Line;

      --  lexer rules
      for I in All_Tokens (Generate_Data).Iterate (Non_Grammar => True, Nonterminals => False) loop
         declare
            Val : constant String := Value (I);
         begin

            if Kind (I) = "non-reporting" then
               Indent_Line (Name (I) & " { lexer->byte_token_start = lexer->cursor;");
               Indent_Line ("    lexer->char_token_start = lexer->char_pos;");
               Indent_Line ("    if (*lexer->cursor == 0x0A)");
               Indent_Line ("       lexer->line_token_start = lexer->line-1;");
               Indent_Line ("    else");
               Indent_Line ("       lexer->line_token_start = lexer->line;");
               Indent_Line ("    continue; }");

            elsif Kind (I) = "delimited-text" then
               Indent_Line
                    (Val & " {*id = " & WisiToken.Token_ID'Image (ID (I)) &
                       "; skip_to(lexer, " & Repair_Image (I) & "); continue;}");

            elsif 0 /= Index (Source => Val, Pattern => "/") then
               Indent_Line (Val & " {*id = " & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");

            else
               Indent_Line (Name (I) & " {*id = " & WisiToken.Token_ID'Image (ID (I)) & "; continue;}");
            end if;
         end;
      end loop;
      New_Line;

      --  Default action.
      Indent_Line ("* {status = ERROR_unrecognized_character; continue;}");

      Put_Line ("*/");
      Indent := Indent - 3;
      Indent_Line ("}");

      Indent_Line ("/* lexer->cursor and lexer ->char_pos are one char past end of token */");
      Indent_Line ("*byte_position = lexer->byte_token_start - lexer->buffer + 1;");
      Indent_Line ("*byte_length   = lexer->cursor - lexer->byte_token_start;");
      Indent_Line ("*char_position = lexer->char_token_start;");
      Indent_Line ("*char_length   = lexer->char_pos - lexer->char_token_start;");
      Indent_Line ("*line_start    = lexer->line_token_start;");
      Indent_Line ("return status;");
      Indent_Line ("}");
      Indent := Indent - 3;
      Set_Output (Standard_Output);
      Close (File);

      declare
         Ada_Name : constant String := Output_File_Name_Root & "_re2c_c";
         --  Output_File_Name_Root is the file name of the grammar file -
         --  assume it is a legal Ada name.
      begin
         Create (File, Out_File, Output_File_Name_Root & "_re2c_c.ads");
         Set_Output (File);
         Indent := 1;
         Put_File_Header (Ada_Comment, Use_Tuple => True, Tuple => Tuple);
         Put_Raw_Code (Ada_Comment, Input_Data.Raw_Code (Copyright_License));
         New_Line;

         Put_Line ("with Interfaces.C;");
         Put_Line ("with WisiToken;");
         Put_Line ("with System;");
         Put_Line ("package " & Ada_Name & " is");
         Indent := Indent + 3;
         New_Line;

         Indent_Line ("function New_Lexer");
         Indent_Line ("  (Buffer    : in System.Address;");
         Indent_Line ("   Length    : in Interfaces.C.size_t;");
         Indent_Line ("   Verbosity : in Interfaces.C.int)");
         Indent_Line ("  return System.Address");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_new_lexer"";");
         Indent_Line ("--  Create the lexer object, passing it the full text to process.");
         New_Line;
         Indent_Line ("procedure Free_Lexer (Lexer : in out System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_free_lexer"";");
         Indent_Line ("--  Free the lexer object");
         New_Line;

         Indent_Line ("procedure Reset_Lexer (Lexer : in System.Address)");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_reset_lexer"";");
         New_Line;

         Indent_Line ("function Next_Token");
         Indent_Line ("  (Lexer         : in     System.Address;");
         Indent_Line ("   ID            :    out WisiToken.Token_ID;");
         Indent_Line ("   Byte_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Byte_Length   :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Position :    out Interfaces.C.size_t;");
         Indent_Line ("   Char_Length   :    out Interfaces.C.size_t;");
         Indent_Line ("   Line_Start    :    out Interfaces.C.int)");
         Indent_Line ("  return Interfaces.C.int");
         Indent_Line ("with Import        => True,");
         Indent_Line ("     Convention    => C,");
         Indent_Line ("     External_Name => """ & Output_File_Name_Root & "_next_token"";");
         New_Line;

         Indent := Indent - 3;
         Put_Line ("end " & Ada_Name & ";");
         Set_Output (Standard_Output);
         Close (File);
      end;
   end Create_re2c;

   function File_Name_To_Ada (File_Name : in String) return String
   is
      Result : String := File_Name;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '.';
            Result (I + 1) := To_Upper (Result (I + 1));
         elsif Result (I) = '_' then
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result;
   end File_Name_To_Ada;

   function Initialize
     (Input_Data        : in WisiToken_Grammar_Runtime.User_Data_Type;
      Tuple             : in Generate_Tuple;
      Output_File_Root  : in String;
      Check_Interface   : in Boolean)
     return Common_Data
   is begin
      return Data : Common_Data do
         Data.Generate_Algorithm := Tuple.Gen_Alg;

         Data.Output_Language := Ada_Output_Language (Tuple.Out_Lang);

         if Tuple.Gen_Alg = External or else Input_Data.User_Lexer in Valid_Lexer then
            Data.Lexer := Input_Data.User_Lexer;
         else
            raise SAL.Programmer_Error with "tuple.alg " & Generate_Algorithm'Image (Tuple.Gen_Alg) &
              " input_data.user_lexer " & Lexer_Image (Input_Data.User_Lexer).all;
         end if;

         if Check_Interface then
            if Tuple.Interface_Kind in Valid_Interface then
               Data.Interface_Kind := Valid_Interface (Tuple.Interface_Kind);
            else
               Put_Error
                 (Error_Message
                    (Input_Data.Grammar_Lexer.File_Name, 1, "Interface_Kind not set"));
            end if;
         else
            Data.Interface_Kind := Process;
         end if;

         Data.Text_Rep := Tuple.Text_Rep;

         Data.Lower_File_Name_Root := +To_Lower (Output_File_Root);
      end return;
   end Initialize;

   function To_Token_Ada_Name (WY_Name : in String) return String
   is
      --  Convert WY_Name to a valid Ada identifier:
      --
      --  Add "_ID" to avoid collision with Ada reserved words
      --
      --  Replace '-' with '_'
      Image : String := WY_Name;
   begin
      for I in Image'Range loop
         if Image (I) = '-' then
            Image (I) := '_';
         end if;
      end loop;
      return Image & "_ID";
   end To_Token_Ada_Name;

end WisiToken.BNF.Output_Ada_Common;
