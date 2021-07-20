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

with Ada.Directories;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Text_IO;
package body WisiToken.Generate is

   function Error_Message
     (File_Name : in String;
      File_Line : in Line_Number_Type;
      Message   : in String)
     return String
   is
      use Ada.Directories;
      use Ada.Strings.Fixed;
      use Ada.Strings;
   begin
      return Simple_Name (File_Name) & ":" &
        Trim (Line_Number_Type'Image (File_Line), Left) & ":0: " & Message;
   end Error_Message;

   procedure Put_Error (Message : in String)
   is begin
      Error := True;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
   end Put_Error;

   procedure Check_Consistent
     (Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor       : in WisiToken.Descriptor;
      Source_File_Name : in String)
   is begin
      if Descriptor.Accept_ID /= Descriptor.First_Nonterminal then
         Put_Error
           (Error_Message
              (Source_File_Name, Line_Number_Type'First,
               "Descriptor.Accept_ID /= Descriptor.First_Nonterminal"));
      end if;
      if Grammar.First_Index /= Descriptor.First_Nonterminal then
         Put_Error
           (Error_Message
              (Source_File_Name, Line_Number_Type'First,
               "Grammar.First_Index /= Descriptor.First_Nonterminal"));
      end if;
      if Grammar.Last_Index /= Descriptor.Last_Nonterminal then
         Put_Error
           (Error_Message
              (Source_File_Name, Line_Number_Type'First,
               "Grammar.Last_Index /= Descriptor.Last_Nonterminal"));
      end if;

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         if Grammar (Nonterm).LHS /= Nonterm then
            Put_Error
              (Error_Message
                 (Source_File_Name, Line_Number_Type'First,
                  "Grammar (" & Image (Nonterm, Descriptor) & ").LHS = " &
                    Image (Grammar (Nonterm).LHS, Descriptor) & " /= " &
                    Image (Nonterm, Descriptor)));
         end if;
      end loop;
   end Check_Consistent;

   function Check_Unused_Tokens
     (Descriptor : in WisiToken.Descriptor;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector)
     return Boolean
   is
      subtype Terminals    is Token_ID range Descriptor.First_Terminal    .. Descriptor.Last_Terminal;
      subtype Nonterminals is Token_ID range Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal;

      Used_Tokens : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);

      Changed        : Boolean := False;
      Abort_Generate : Boolean := False;
      Unused_Tokens  : Boolean := False;
   begin
      Used_Tokens (Descriptor.Accept_ID) := True;

      --  First mark all nonterminals that occur in used nonterminals as
      --  used.
      loop
         for Prod of Grammar loop
            if Used_Tokens (Prod.LHS) then
               for RHS of Prod.RHSs loop
                  for J of RHS.Tokens loop
                     if J in Nonterminals then
                        Changed         := Changed or else not Used_Tokens (J);
                        Used_Tokens (J) := True;
                     end if;
                  end loop;
               end loop;
            end if;
         end loop;
         exit when not Changed;
         Changed := False;
      end loop;

      --  Now mark terminals used in used nonterminals
      for Prod of Grammar loop
         if Used_Tokens (Prod.LHS) then
            for RHS of Prod.RHSs loop
               for J of RHS.Tokens loop
                  if not (J in Used_Tokens'Range) then
                     WisiToken.Generate.Put_Error
                       ("non-grammar token " & Image (J, Descriptor) & " used in grammar");

                     --  This causes lots of problems with token_id not in terminal or
                     --  nonterminal range, so abort early.
                     Abort_Generate := True;
                  end if;

                  if J in Terminals then
                     Used_Tokens (J) := True;
                  end if;
               end loop;
            end loop;
         end if;
      end loop;

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               WisiToken.Generate.Put_Error ("Unused tokens:");
               Unused_Tokens := True;
            end if;
            WisiToken.Generate.Put_Error (Image (I, Descriptor));
         end if;
      end loop;

      if Abort_Generate then
         raise Grammar_Error;
      end if;

      return Unused_Tokens;
   end Check_Unused_Tokens;

   function Nullable (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Token_Array_Production_ID
   is
      use all type Ada.Containers.Count_Type;

      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;

      Result  : Token_Array_Production_ID := (Nonterminal => Invalid_Production_ID);
      Changed : Boolean                   := True;
   begin
      loop
         exit when not Changed;
         Changed := False;

         for Prod of Grammar loop
            if Result (Prod.LHS) = Invalid_Production_ID then
               for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                  declare
                     RHS : WisiToken.Productions.Right_Hand_Side renames Prod.RHSs (RHS_Index);
                  begin
                     if RHS.Tokens.Length = 0 or else
                       (RHS.Tokens (1) in Nonterminal and then Result (RHS.Tokens (1)) /= Invalid_Production_ID)
                     then
                        Result (Prod.LHS) := (Prod.LHS, RHS_Index);
                        Changed := True;
                     end if;
                  end;
               end loop;
            end if;
         end loop;
      end loop;
      return Result;
   end Nullable;

   function Has_Empty_Production (Nullable : in Token_Array_Production_ID) return Token_ID_Set
   is begin
      return Result : Token_ID_Set := (Nullable'First .. Nullable'Last => False) do
         for I in Result'Range loop
            Result (I) := Nullable (I) /= Invalid_Production_ID;
         end loop;
      end return;
   end Has_Empty_Production;

   function Has_Empty_Production (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Token_ID_Set
   is
      use all type Ada.Containers.Count_Type;

      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;

      Result  : Token_ID_Set := (Nonterminal => False);
      Changed : Boolean      := True;
   begin
      loop
         exit when not Changed;
         Changed := False;

         for Prod of Grammar loop
            for RHS of Prod.RHSs loop
               if (RHS.Tokens.Length = 0 or else
                     (RHS.Tokens (1) in Nonterminal and then Result (RHS.Tokens (1)))) and
                 not Result (Prod.LHS)
               then
                  Result (Prod.LHS) := True;
                  Changed := True;
               end if;
            end loop;
         end loop;
      end loop;
      return Result;
   end Has_Empty_Production;

   function First
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID;
      Non_Terminal         : in Token_ID)
     return Token_ID_Set
   is
      Search_Tokens : Token_ID_Set := (Grammar.First_Index .. Grammar.Last_Index => False);
   begin
      Search_Tokens (Non_Terminal) := True;

      return Derivations : Token_ID_Set := (First_Terminal .. Grammar.Last_Index => False) do
         while Any (Search_Tokens) loop
            declare
               Added_Tokens   : Token_ID_Set := (First_Terminal .. Grammar.Last_Index      => False);
               Added_Nonterms : Token_ID_Set := (Grammar.First_Index .. Grammar.Last_Index => False);
            begin
               for Prod of Grammar loop
                  if Search_Tokens (Prod.LHS) then
                     for RHS of Prod.RHSs loop
                        for Derived_Token of RHS.Tokens loop
                           if not Derivations (Derived_Token) then
                              Added_Tokens (Derived_Token) := True;
                              if Derived_Token in Added_Nonterms'Range then
                                 Added_Nonterms (Derived_Token) := True;
                              end if;
                           end if;

                           if Derived_Token in Has_Empty_Production'Range and then
                             Has_Empty_Production (Derived_Token)
                           then
                              null;
                           else
                              exit;
                           end if;
                        end loop;
                     end loop;
                  end if;
               end loop;
               Derivations   := Derivations or Added_Tokens;
               Search_Tokens := Added_Nonterms;
            end;
         end loop;
      end return;
   end First;

   function First
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID)
     return Token_Array_Token_Set
   is
      Matrix : Token_Array_Token_Set :=
        (Grammar.First_Index .. Grammar.Last_Index =>
           (First_Terminal .. Grammar.Last_Index => False));

      procedure Set_Slice (Matrix : in out Token_Array_Token_Set; I : Token_ID; Value : in Token_ID_Set)
      is begin
         for J in Matrix'Range (2) loop
            Matrix (I, J) := Value (J);
         end loop;
      end Set_Slice;

   begin
      for NT_Index in Matrix'Range loop
         Set_Slice (Matrix, NT_Index, First (Grammar, Has_Empty_Production, First_Terminal, NT_Index));
      end loop;

      return Matrix;
   end First;

   function To_Terminal_Sequence_Array
     (First      : in Token_Array_Token_Set;
      Descriptor : in WisiToken.Descriptor)
     return Token_Sequence_Arrays.Vector
   is
      subtype Terminal is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;
   begin
      return Result : Token_Sequence_Arrays.Vector do
         Result.Set_First_Last (First'First (1), First'Last (1));

         for I in First'Range (1) loop
            declare
               Row : Token_ID_Arrays.Vector renames Result (I);
            begin
               for J in First'Range (2) loop
                  if First (I, J) and then J in Terminal then
                     Row.Append (J);
                  end if;
               end loop;
            end;
         end loop;
      end return;
   end To_Terminal_Sequence_Array;

   function Follow
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor;
      First                : in Token_Array_Token_Set;
      Has_Empty_Production : in Token_ID_Set)
     return Token_Array_Token_Set
   is
      subtype Terminal    is Token_ID range Descriptor.First_Terminal    .. Descriptor.Last_Terminal;
      subtype Nonterminal is Token_ID range Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal;

      Prev_Result : Token_Array_Token_Set := (Nonterminal => (Terminal => False));
      Result      : Token_Array_Token_Set := (Nonterminal => (Terminal => False));

      ID : Token_ID;
   begin
      --  [dragon] pgp 189:
      --
      --  Rule 1 Follow (S, EOF) = True; EOF is explicit in the
      --  start symbol production, so this is covered by Rule 2.
      --
      --  Rule 2: If A => alpha B Beta, add First (Beta) to Follow (B)
      --
      --  Rule 3; if A => alpha B, or A -> alpha B Beta and Beta
      --  can be null, add Follow (A) to Follow (B)
      --
      --  We don't assume any order in the productions list, so we
      --  have to keep applying rule 3 until nothing changes.

      for B in Nonterminal loop
         for Prod of Grammar loop
            for A of Prod.RHSs loop
               for I in A.Tokens.First_Index .. A.Tokens.Last_Index loop
                  if A.Tokens (I) = B then
                     if I < A.Tokens.Last_Index then
                        --  Rule 1
                        ID := A.Tokens (1 + I);
                        if ID in Terminal then
                           Result (B, ID) := True;
                        else
                           Or_Slice (Result, B, Slice (First, ID));
                        end if;
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;

      Prev_Result := Result;
      loop
         for B in Nonterminal loop
            for Prod of Grammar loop
               for A of Prod.RHSs loop
                  for I in A.Tokens.First_Index .. A.Tokens.Last_Index loop
                     if A.Tokens (I) = B then
                        if I = A.Tokens.Last_Index or else
                          (A.Tokens (1 + I) in Nonterminal and then
                             Has_Empty_Production (A.Tokens (1 + I)))
                        then
                           --  rule 3
                           Or_Slice (Result, B, Slice (Result, Prod.LHS));
                        end if;
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;

         exit when Prev_Result = Result;
         Prev_Result := Result;
      end loop;
      return Result;
   end Follow;

   function To_Graph (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Grammar_Graphs.Graph
   is
      subtype Nonterminals is Token_ID range Grammar.First_Index .. Grammar.Last_Index;
      Graph : Grammar_Graphs.Graph;
      J     : Integer := 1;
   begin
      if Trace_Generate_Minimal_Complete > Outline then
         Ada.Text_IO.Put_Line ("grammar graph:");
      end if;

      for LHS in Grammar.First_Index .. Grammar.Last_Index loop
         declare
            Prod : WisiToken.Productions.Instance renames Grammar (LHS);
         begin
            for RHS in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
               declare
                  Tokens : Token_ID_Arrays.Vector renames Prod.RHSs (RHS).Tokens;
               begin
                  for I in Tokens.First_Index .. Tokens.Last_Index loop
                     if Tokens (I) in Nonterminals then
                        if Trace_Generate_Minimal_Complete > Detail then
                           Ada.Text_IO.Put_Line
                             ("(" & Trimmed_Image (LHS) & ", " & Trimmed_Image (Tokens (I)) & ","  & J'Image & ")");
                           J := J + 1;
                        end if;
                        Graph.Add_Edge (LHS, Tokens (I), (RHS, I));
                     end if;
                  end loop;
               end;
            end loop;
         end;
      end loop;

      if Trace_Generate_Minimal_Complete > Outline then
         Ada.Text_IO.Put_Line ("..." & Graph.Count_Nodes'Image & " nodes" & Graph.Count_Edges'Image & " edges.");
      end if;
      return Graph;
   end To_Graph;

   function Recursion
     (LHS         : in Token_ID;
      Token_Index : in Positive;
      Tokens      : in Token_ID_Arrays.Vector)
     return Recursion_Class
   is begin
      return
        (if Token_Index = Tokens.First_Index then
           (if LHS = Tokens (Tokens.First_Index)
            then Direct_Left
            else Other_Left)
         elsif Token_Index = Tokens.Last_Index then
           (if LHS = Tokens (Tokens.Last_Index)
            then Direct_Right
            else Other_Right)
         else Other);
   end Recursion;

   procedure Set_Grammar_Recursions
     (Recursions : in     WisiToken.Generate.Recursions;
      Grammar    : in out WisiToken.Productions.Prod_Arrays.Vector)
   is begin
      for LHS of Grammar loop
         for RHS of LHS.RHSs loop
            RHS.Recursion.Set_First_Last (RHS.Tokens.First_Index, RHS.Tokens.Last_Index);
         end loop;
      end loop;

      for Path of Recursions.Recursions loop
         declare
            use WisiToken.Productions;
            Previous_Item_LHS : Token_ID :=
              (if Recursions.Full then Path (Path'Last).Vertex else Token_ID'Last);
         begin
            for Item of Path loop
               for Edge of Item.Edges loop
                  declare
                     LHS : constant Token_ID := (if Recursions.Full then Previous_Item_LHS else Item.Vertex);
                     RHS : Right_Hand_Side renames Grammar (LHS).RHSs (Edge.Data.RHS);
                  begin
                     RHS.Recursion (Edge.Data.Token_Index) := Recursion
                       (LHS         => LHS,
                        Token_Index => Edge.Data.Token_Index,
                        Tokens      => RHS.Tokens);
                  end;
               end loop;
               Previous_Item_LHS := Item.Vertex;
            end loop;
         end;
      end loop;
   end Set_Grammar_Recursions;

   function Compute_Full_Recursion
     (Grammar    : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor)
     return Recursions
   is
      Time_Start : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      Graph : constant Grammar_Graphs.Graph := To_Graph (Grammar);
   begin
      return Result : Recursions :=
        (Full       => True,
         Recursions => Graph.Find_Cycles)
      do
         Grammar_Graphs.Sort_Paths.Sort (Result.Recursions);

         Set_Grammar_Recursions (Result, Grammar);

         if Trace_Time then
            declare
               use Ada.Real_Time;
               Time_End : constant Time := Clock;
            begin
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error, "compute partial recursion time:" &
                    Duration'Image (To_Duration (Time_End - Time_Start)));
            end;
         end if;

         if Trace_Generate_Minimal_Complete > Extra then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Productions:");
            WisiToken.Productions.Put (Grammar, Descriptor);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("full recursions:");
            for I in Result.Recursions.First_Index .. Result.Recursions.Last_Index loop
               Ada.Text_IO.Put_Line (Trimmed_Image (I) & " => " & Grammar_Graphs.Image (Result.Recursions (I)));
            end loop;
         end if;
      end return;
   end Compute_Full_Recursion;

   function Compute_Partial_Recursion
     (Grammar    : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor)
     return Recursions
   is
      use Grammar_Graphs;
      Time_Start : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      Graph      : constant Grammar_Graphs.Graph := To_Graph (Grammar);
      Components : constant Component_Lists.List := Strongly_Connected_Components
        (To_Adjancency (Graph), Non_Trivial_Only => True);
      Loops      : constant Vertex_Lists.List    := Graph.Loops;
   begin
      return Result : Recursions do
         Result.Full := False;
         for Comp of Components loop
            declare
               Path : Recursion_Cycle (1 .. Integer (Comp.Length));
               Last : Integer := Path'First - 1;
            begin
               for V of Comp loop
                  Last := Last + 1;
                  Path (Last) := (V, Graph.Edges (V));
               end loop;
               Result.Recursions.Append (Path);
            end;
         end loop;

         declare
            Path : Recursion_Cycle (1 .. Integer (Loops.Length));
            Last : Integer := Path'First - 1;
         begin
            for V of Loops loop
               Last := Last + 1;
               Path (Last) := (V, Graph.Edges (V));
            end loop;
            Result.Recursions.Append (Path);
         end;

         Set_Grammar_Recursions (Result, Grammar);

         if Trace_Time then
            declare
               use Ada.Real_Time;
               Time_End : constant Time := Clock;
            begin
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error, "compute full recursion time:" &
                    Duration'Image (To_Duration (Time_End - Time_Start)));
            end;
         end if;

         if Trace_Generate_Minimal_Complete > Extra then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Productions:");
            WisiToken.Productions.Put (Grammar, Descriptor);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("partial recursions:");
            for I in Result.Recursions.First_Index .. Result.Recursions.Last_Index loop
               Ada.Text_IO.Put_Line (Trimmed_Image (I) & " => " & Grammar_Graphs.Image (Result.Recursions (I)));
            end loop;
         end if;
      end return;
   end Compute_Partial_Recursion;

   ----------
   --  Indented text output

   procedure Indent_Line (Text : in String)
   is
      use Ada.Text_IO;
   begin
      Set_Col (Indent);
      Put_Line (Text);
      Line_Count := Line_Count + 1;
   end Indent_Line;

   procedure Indent_Start (Text : in String)
   is
      use Ada.Text_IO;
   begin
      Set_Col (Indent);
      Put (Text);
   end Indent_Start;

   procedure Indent_Wrap (Text : in String)
   is
      use all type Ada.Text_IO.Count;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      I     : Natural;
      First : Integer := Text'First;
   begin
      if Text'Length + Indent <= Max_Line_Length then
         Indent_Line (Text);
      else
         loop
            I := Text'Last;
            loop
               I := Index (Text (First .. Text'Last), " ", From => I, Going => Backward);
               exit when I - First + Integer (Indent) <= Max_Line_Length;
               I := I - 1;
            end loop;
            Indent_Line (Trim (Text (First .. I - 1), Right));
            First := I + 1;
            exit when Text'Last - First + Integer (Indent) <= Max_Line_Length;
         end loop;
         Indent_Line (Text (First .. Text'Last));
      end if;
   end Indent_Wrap;

   procedure Indent_Wrap_Comment (Text : in String; Comment_Syntax : in String)
   is
      use all type Ada.Text_IO.Count;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Prefix : constant String := Comment_Syntax & "  ";
      I      : Natural;
      First  : Integer         := Text'First;
   begin
      if Text'Length + Indent <= Max_Line_Length - 4 then
         Indent_Line (Prefix & Text);
      else
         loop
            I := Text'Last;
            loop
               I := Index (Text (First .. Text'Last), " ", From => I, Going => Backward);
               exit when I - First + Integer (Indent) <= Max_Line_Length - 4;
               I := I - 1;
            end loop;
            Indent_Line (Prefix & Text (First .. I - 1));
            First := I + 1;
            exit when Text'Last - First + Integer (Indent) <= Max_Line_Length - 4;
         end loop;
         Indent_Line (Prefix & Text (First .. Text'Last));
      end if;
   end Indent_Wrap_Comment;

end WisiToken.Generate;
