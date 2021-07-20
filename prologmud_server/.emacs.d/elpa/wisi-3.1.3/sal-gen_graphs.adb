--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2019 Free Software Foundation All Rights Reserved.
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Gen_Bounded_Definite_Queues;
with SAL.Gen_Unbounded_Definite_Stacks;
package body SAL.Gen_Graphs is

   package Vertex_Queues is new SAL.Gen_Bounded_Definite_Queues (Vertex_Index);
   package Vertex_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Vertex_Index);

   function Find (Data : in Edge_Data; List : in Edge_Node_Lists.List) return Edge_Node_Lists.Cursor
   is begin
      for I in List.Iterate loop
         if Edge_Node_Lists.Element (I).Data = Data then
            return I;
         end if;
      end loop;
      return Edge_Node_Lists.No_Element;
   end Find;

   ----------
   --  Visible subprograms

   procedure Add_Edge
     (Graph    : in out Gen_Graphs.Graph;
      Vertex_A : in     Vertex_Index;
      Vertex_B : in     Vertex_Index;
      Data     : in     Edge_Data)
   is
      Multigraph : Boolean := False;

      procedure Update_First_Last (Vertex : in Vertex_Index)
      is
         use all type Ada.Containers.Count_Type;
      begin
         if Graph.Vertices.Length = 0 then
            Graph.Vertices.Set_First_Last (Vertex, Vertex);
         else
            if Vertex < Graph.Vertices.First_Index then
               Graph.Vertices.Set_First_Last (Vertex, Graph.Vertices.Last_Index);
            elsif Vertex > Graph.Vertices.Last_Index then
               Graph.Vertices.Set_First_Last (Graph.Vertices.First_Index, Vertex);
            end if;
         end if;
      end Update_First_Last;

   begin
      Update_First_Last (Vertex_A);
      Update_First_Last (Vertex_B);

      Graph.Last_Edge_ID := Graph.Last_Edge_ID + 1;
      if (for some E of Graph.Vertices (Vertex_A) => E.Vertex_B = Vertex_B) then
         Multigraph       := True;
         Graph.Multigraph := True;
      end if;

      Graph.Vertices (Vertex_A).Append ((Graph.Last_Edge_ID, Vertex_B, Multigraph, Data));
   end Add_Edge;

   function Count_Nodes (Graph : in Gen_Graphs.Graph) return Ada.Containers.Count_Type
   is begin
      return Graph.Vertices.Length;
   end Count_Nodes;

   function Count_Edges (Graph : in Gen_Graphs.Graph) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Result : Count_Type := 0;
   begin
      for Edges of Graph.Vertices loop
         Result := Result + Edges.Length;
      end loop;
      return Result;
   end Count_Edges;

   function Multigraph (Graph : in Gen_Graphs.Graph) return Boolean
   is begin
      return Graph.Multigraph;
   end Multigraph;

   function "+" (Right : in Edge_Item) return Edge_Lists.List
   is
      use Edge_Lists;
   begin
      return Result : List do
         Append (Result, Right);
      end return;
   end "+";

   function Edges (Graph : in Gen_Graphs.Graph; Vertex : in Vertex_Index) return Edge_Lists.List
   is begin
      return Result : Edge_Lists.List do
         for E of Graph.Vertices (Vertex) loop
            Result.Append ((E.ID, E.Data));
         end loop;
      end return;
   end Edges;

   function Image (Item : in Path) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String := To_Unbounded_String ("(");
   begin
      for I in Item'Range loop
         Result := Result & Trimmed_Image (Item (I).Vertex) & " " &
           Image ((if I = Item'Last then Item (Item'First).Edges else Item (I + 1).Edges)) & " -> ";
      end loop;
      Result := Result & ")";
      return To_String (Result);
   end Image;

   function "<" (Left, Right : in Path) return Boolean
   is begin
      for I in Left'Range loop
         if I > Right'Last then
            return False;
         elsif Left (I).Vertex < Right (I).Vertex then
            return True;
         elsif Left (I).Vertex > Right (I).Vertex then
            return False;
         else
            --  =; check remaining elements
            null;
         end if;
      end loop;

      if Left'Last < Right'Last then
         return True;
      else
         --  All =
         return False;
      end if;
   end "<";

   function Find_Paths
     (Graph : in out Gen_Graphs.Graph;
      From  : in     Vertex_Index;
      To    : in     Edge_Data)
     return Path_Arrays.Vector
   is
      use Vertex_Queues;

      Vertex_Queue  : Queue_Type
        (Size => Peek_Type (Graph.Vertices.Last_Index - Graph.Vertices.First_Index + 1));

      type Colors is (White, Gray, Black);

      type Aux_Node is record
         Color       : Colors            := Colors'First;
         D           : Natural           := Natural'Last;
         Parent      : Vertex_Index'Base := Invalid_Vertex;
         Parent_Set  : Boolean           := False;
         Parent_Edge : Edge_Node_Lists.Cursor := Edge_Node_Lists.No_Element;
      end record;

      package Aux_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Vertex_Index, Aux_Node, (others => <>));
      Aux : Aux_Arrays.Vector;

      function Build_Path
        (Tail_Vertex : in Vertex_Index;
         Tail_Edge   : in Edge_Node_Lists.Cursor)
        return Path
      is
      begin
         return Result : Path (1 .. Aux (Tail_Vertex).D + 1)
         do
            declare
               use Edge_Node_Lists;
               V_Index   : Vertex_Index := Tail_Vertex;
               Last_Edge : Cursor       := Tail_Edge;
            begin
               for I in reverse 1 .. Result'Length loop
                  declare
                     V : Aux_Node renames Aux (V_Index);
                  begin
                     if Last_Edge = No_Element then
                        Result (I) := (V_Index, Edge_Lists.Empty_List);
                     else
                        Result (I) := (V_Index, +(Element (Last_Edge).ID, Element (Last_Edge).Data));
                     end if;

                     if V.Parent_Set then
                        Last_Edge := V.Parent_Edge;
                        V_Index   := V.Parent;
                     end if;
                  end;
               end loop;
            end;
         end return;
      end Build_Path;

      Result_List : Path_Arrays.Vector;
      Result_Edge : Edge_Node_Lists.Cursor;
   begin
      --  [1] figure 22.3 breadth-first search; 'From' = s.

      Aux.Set_First_Last (Graph.Vertices.First_Index, Graph.Vertices.Last_Index);

      for I in Aux.First_Index .. Aux.Last_Index loop
         if I = From then
            Aux (I).Color      := Gray;
            Aux (I).D          := 0;
            Aux (I).Parent_Set := False;

         else
            Aux (I).Color      := White;
            Aux (I).D          := Natural'Last;
            Aux (I).Parent_Set := False;
         end if;
      end loop;

      Put (Vertex_Queue, From);

      while not Is_Empty (Vertex_Queue) loop
         declare
            U_Index : constant Vertex_Index := Get (Vertex_Queue);
            U       : Aux_Node renames Aux (U_Index);
         begin
            Edges :
            for C in Graph.Vertices (U_Index).Iterate loop
               declare
                  use all type Edge_Node_Lists.Cursor;
                  V_Index : constant Vertex_Index := Edge_Node_Lists.Element (C).Vertex_B;
                  V       : Aux_Node renames Aux (V_Index);
               begin
                  if V.Color = White then
                     V.Color       := Gray;
                     V.D           := U.D + 1;
                     V.Parent      := U_Index;
                     V.Parent_Edge := C;
                     V.Parent_Set  := True;

                     Result_Edge := Find (To, Graph.Vertices (V_Index));
                     if Result_Edge /= Edge_Node_Lists.No_Element then
                        Result_List.Append (Build_Path (V_Index, Result_Edge));
                     end if;

                     Put (Vertex_Queue, V_Index);
                  end if;
               end;
            end loop Edges;
            U.Color := Black;
         end;
      end loop;
      return Result_List;
   end Find_Paths;

   function Find_Cycles_Tiernan (Graph : in Gen_Graphs.Graph)
     return Path_Arrays.Vector
   is
      --  Implements [2] "Algorithm EC"
      --
      --  vertex 0 = Invalid_Vertex
      --  vertex 1 = Graph.Vertices.First_Index
      --  vertex N = Graph.Vertices.Last_Index

      First : Vertex_Index renames Graph.Vertices.First_Index;
      Last  : Vertex_Index renames Graph.Vertices.Last_Index;

      G : Vertex_Arrays.Vector renames Graph.Vertices;
      P : Path (1 .. Integer (Last - First + 1));
      K : Positive := 1; -- ie P_Last

      type H_Row is array (G.First_Index .. G.Last_Index) of Vertex_Index'Base;
      H : array (G.First_Index .. G.Last_Index) of H_Row := (others => (others => Invalid_Vertex));

      Next_Vertex_Found : Boolean;

      Result : Path_Arrays.Vector;

      function Contains (P : in Path; V : in Vertex_Index) return Boolean
      is (for some N of P => N.Vertex = V);

      function Contains (Row : in H_Row; V : in Vertex_Index) return Boolean
      is (for some N of Row => N = V);

      function Contains (Edges : in Edge_Lists.List; ID : in Edge_ID) return Boolean
        is (for some E of Edges => E.ID = ID);

      procedure Add_Alternate_Edges (P : in out Path)
      is
         function Dec (I : in Positive) return Positive
           is (if I = P'First then P'Last else I - 1);
      begin
         for I in P'Range loop
            for New_Edge of G (P (Dec (I)).Vertex) loop
               if New_Edge.Vertex_B = P (I).Vertex and (not Contains (P (I).Edges, New_Edge.ID)) then
                  P (I).Edges.Append ((New_Edge.ID, New_Edge.Data));
               end if;
            end loop;
         end loop;
      end Add_Alternate_Edges;

   begin
      P (1) := (First, Edge_Lists.Empty_List);

      All_Initial_Vertices :
      loop
         Explore_Vertex :
         loop
            Path_Extension :
            loop  -- EC2 Path Extension
               Next_Vertex_Found := False;

               Find_Next_Vertex :
               for Edge of G (P (K).Vertex) loop
                  declare
                     Next_Vertex : constant Vertex_Index := Edge.Vertex_B; -- ie G[P[k],j]
                  begin
                     if Next_Vertex > P (1).Vertex and -- (1)
                       (not Contains (P, Next_Vertex)) and -- (2)
                       (not Contains (H (P (K).Vertex), Next_Vertex))
                     then
                        K     := K + 1;
                        P (K) := (Next_Vertex, +(Edge.ID, Edge.Data));

                        Next_Vertex_Found := True;
                        exit Find_Next_Vertex;
                     end if;
                  end;
               end loop Find_Next_Vertex;

               exit Path_Extension when not Next_Vertex_Found;
            end loop Path_Extension;

            --  EC3 Circuit Confirmation
            for Edge of G (P (K).Vertex) loop
               if Edge.Vertex_B = P (1).Vertex then
                  P (1).Edges := +(Edge.ID, Edge.Data);
                  if Graph.Multigraph then
                     Add_Alternate_Edges (P (1 .. K));
                  end if;
                  Result.Append (P (1 .. K));
                  exit;
               end if;
            end loop;

            --  EC4 Vertex Closure
            exit Explore_Vertex when K = 1;

            H (P (K).Vertex) := (others => Invalid_Vertex);
            for M in H (P (K - 1).Vertex)'Range loop
               if H (P (K - 1).Vertex)(M) = Invalid_Vertex then
                  H (P (K - 1).Vertex)(M) := P (K).Vertex;
                  P (K) := (Invalid_Vertex, Edge_Lists.Empty_List);
                  exit;
               end if;
            end loop;
            K := K - 1;
         end loop Explore_Vertex;

         --  EC5 Advance Initial Index
         exit All_Initial_Vertices when P (1).Vertex = Graph.Vertices.Last_Index;

         P (1) := (P (1).Vertex + 1, Edge_Lists.Empty_List);
         pragma Assert (K = 1);
         H := (others => (others => Invalid_Vertex));
      end loop All_Initial_Vertices;

      --  EC6 Terminate
      return Result;
   end Find_Cycles_Tiernan;

   function Find_Cycles (Graph : in Gen_Graphs.Graph) return Path_Arrays.Vector
   is
      --  Implements Circuit-Finding Algorithm from [3]

      use all type Ada.Containers.Count_Type;

      pragma Warnings (Off, """Edited_Graph"" is not modified, could be declared constant");
      Edited_Graph : Gen_Graphs.Graph := Graph;

      Result : Path_Arrays.Vector;

      A_K     : Adjacency_Structures.Vector;
      B       : Adjacency_Structures.Vector;
      Blocked : array (Graph.Vertices.First_Index .. Graph.Vertices.Last_Index) of Boolean := (others => False);

      Stack : Vertex_Stacks.Stack;
      S     : Vertex_Index := Graph.Vertices.First_Index;

      Dummy : Boolean;
      pragma Unreferenced (Dummy);

      function Circuit (V : in Vertex_Index) return Boolean
      is
         F : Boolean := False;

         procedure Unblock (U : in Vertex_Index)
         is begin
            Blocked (U) := False;
            declare
               use Vertex_Lists;
               Cur  : Cursor := B (U).First;
               Temp : Cursor;
               W    : Vertex_Index;
            begin
               loop
                  exit when not Has_Element (Cur);
                  W := Element (Cur);
                  Temp := Cur;
                  Next (Cur);
                  B (U).Delete (Temp);
                  if Blocked (W) then
                     Unblock (W);
                  end if;
               end loop;
            end;
         end Unblock;

         procedure Add_Result
         is
            Cycle : Path (1 .. Integer (Stack.Depth));
         begin
            for I in 1 .. Stack.Depth loop
               Cycle (Integer (Stack.Depth - I + 1)) := (Stack.Peek (I), Edge_Lists.Empty_List);
               --  We add the edge info later, after finding all the cycles.
            end loop;
            Result.Append (Cycle);
            if Trace > 0 then
               Ada.Text_IO.Put_Line ("cycle " & Image (Cycle));
            end if;
         end Add_Result;

      begin
         if Trace > 0 then
            Ada.Text_IO.Put_Line ("circuit start" & V'Image);
         end if;

         Stack.Push (V);
         Blocked (V) := True;
         if V in A_K.First_Index .. A_K.Last_Index then
            for W of A_K (V) loop
               if W = S then
                  Add_Result;
                  F := True;
               elsif not Blocked (W) then
                  if Circuit (W) then
                     F := True;
                  end if;
               end if;
            end loop;
         end if;
         if F then
            Unblock (V);
         else
            if V in A_K.First_Index .. A_K.Last_Index then
               for W of A_K (V) loop
                  if (for all V1 of B (W) => V /= V1) then
                     B (W).Append (V);
                  end if;
               end loop;
            end if;
         end if;
         Stack.Pop;
         if Trace > 0 then
            Ada.Text_IO.Put_Line ("circuit finish" & V'Image);
         end if;
         return F;
      end Circuit;

   begin
      --  [3] restricts the graph to not have loops (edge v-v) or multiple
      --  edges between two nodes. So we first delete any such edges.
      Delete_Loops_Multigraph :
      for V in Edited_Graph.Vertices.First_Index .. Edited_Graph.Vertices.Last_Index loop
         declare
            use Edge_Node_Lists;
            Cur        : Cursor  := Edited_Graph.Vertices (V).First;
            Temp       : Cursor;
            Found_Loop : Boolean := False;
         begin
            loop
               exit when not Has_Element (Cur);
               if Element (Cur).Vertex_B = V then
                  if not Found_Loop then
                     --  This is a cycle we want in the result. Edge data is added to all
                     --  cycles later.
                     Result.Append (Path'(1 => (V, Edge_Lists.Empty_List)));
                     Found_Loop := True;
                  end if;
                  Temp := Cur;
                  Next (Cur);
                  Edited_Graph.Vertices (V).Delete (Temp);
               elsif Element (Cur).Multigraph then
                  --  These will be added back from Graph after we find all cycles.
                  Temp := Cur;
                  Next (Cur);
                  Edited_Graph.Vertices (V).Delete (Temp);
               else
                  Next (Cur);
               end if;
            end loop;
         end;
      end loop Delete_Loops_Multigraph;

      B.Set_First_Last (Graph.Vertices.First_Index, Graph.Vertices.Last_Index);

      --  Start of body of Circuit-Finding Algorithm from [3]
      loop
         exit when S = Graph.Vertices.Last_Index;
         declare
            use Component_Lists;
            Subgraph         : Adjacency_Structures.Vector;
            Components       : Component_Lists.List;
            Cur              : Component_Lists.Cursor;
            Least_Vertex_Cur : Component_Lists.Cursor;
            Least_Vertex_V   : Vertex_Index := Vertex_Index'Last;

            function Delete_Edges (Edges : in Edge_Node_Lists.List) return Vertex_Lists.List
            is begin
               return Result : Vertex_Lists.List do
                  for Edge of Edges loop
                     if Edge.Vertex_B >= S then
                        Result.Append (Edge.Vertex_B);
                     end if;
                  end loop;
               end return;
            end Delete_Edges;
         begin
            Subgraph.Set_First_Last (S, Edited_Graph.Vertices.Last_Index);
            for V in S .. Edited_Graph.Vertices.Last_Index loop
               Subgraph (V) := Delete_Edges (Edited_Graph.Vertices (V));
            end loop;

            Components := Strongly_Connected_Components (Subgraph, Non_Trivial_Only => True);
            Cur        := Components.First;
            loop
               exit when not Has_Element (Cur);

               if Element (Cur).Length > 1 then
                  declare
                     Comp : Vertex_Lists.List renames Components.Constant_Reference (Cur);
                  begin
                     for W of Comp loop
                        if W < Least_Vertex_V then
                           Least_Vertex_Cur := Cur;
                           Least_Vertex_V   := W;
                        end if;
                     end loop;
                  end;
               end if;
               Next (Cur);
            end loop;

            A_K.Clear;
            if Has_Element (Least_Vertex_Cur) then
               declare
                  Component : Vertex_Lists.List renames Components (Least_Vertex_Cur);
                  Min : Vertex_Index := Vertex_Index'Last;
                  Max : Vertex_Index := Vertex_Index'First;
               begin
                  if Trace > 0 then
                     Ada.Text_IO.Put_Line ("strong component " & Least_Vertex_V'Image);
                     Ada.Text_IO.Put_Line (Image (Component));
                  end if;
                  for V of Component loop
                     if Min > V then
                        Min := V;
                     end if;
                     if Max < V then
                        Max := V;
                     end if;
                  end loop;
                  A_K.Set_First_Last (Min, Max);
                  for V of Component loop
                     for Edge of Edited_Graph.Vertices (V) loop
                        A_K (V).Append (Edge.Vertex_B);
                     end loop;
                  end loop;
               end;
            end if;
         end;

         if A_K.Length > 0 then
            S := A_K.First_Index;
            for I in A_K.First_Index .. A_K.Last_Index loop
               Blocked (I) := False;
               B (I).Clear;
            end loop;
            Dummy := Circuit (S);
            S := S + 1;
         else
            S := Graph.Vertices.Last_Index;
         end if;
      end loop;

      --  Add edge data.
      for Cycle of Result loop
         for I in Cycle'First .. Cycle'Last loop
            declare
               Prev_I : constant Positive := (if I = Cycle'First then Cycle'Last else I - 1);
            begin
               for Edge of Graph.Vertices (Cycle (Prev_I).Vertex) loop
                  if Cycle (I).Vertex = Edge.Vertex_B then
                     Cycle (I).Edges.Append ((Edge.ID, Edge.Data));
                  end if;
               end loop;
            end;
         end loop;
      end loop;
      return Result;
   end Find_Cycles;

   function Loops (Graph : in Gen_Graphs.Graph) return Vertex_Lists.List
   is begin
      return Result : Vertex_Lists.List do
         for V in Graph.Vertices.First_Index .. Graph.Vertices.Last_Index loop
            for Edge of Graph.Vertices (V) loop
               if V = Edge.Vertex_B then
                  Result.Append (V);
                  exit;
               end if;
            end loop;
         end loop;
      end return;
   end Loops;

   function To_Adjancency (Graph : in Gen_Graphs.Graph) return Adjacency_Structures.Vector
   is
      function To_Vertex_List (Edges : in Edge_Node_Lists.List) return Vertex_Lists.List
      is begin
         return Result : Vertex_Lists.List do
            for Edge of Edges loop
               Result.Append (Edge.Vertex_B);
            end loop;
         end return;
      end To_Vertex_List;
   begin
      return Result : Adjacency_Structures.Vector do
         Result.Set_First_Last (Graph.Vertices.First_Index, Graph.Vertices.Last_Index);
         for V in Graph.Vertices.First_Index .. Graph.Vertices.Last_Index loop
            Result (V) := To_Vertex_List (Graph.Vertices (V));
         end loop;
      end return;
   end To_Adjancency;

   function Strongly_Connected_Components
     (Graph            : in Adjacency_Structures.Vector;
      Non_Trivial_Only : in Boolean := False)
     return Component_Lists.List
   is
      --  Implements [4] section 4.

      Low_Link : array (Graph.First_Index .. Graph.Last_Index) of Vertex_Index'Base := (others => Invalid_Vertex);

      Number : array (Graph.First_Index .. Graph.Last_Index) of Vertex_Index'Base := (others => Invalid_Vertex);
      --  Number is the order visited in the depth-first search.

      Points : Vertex_Stacks.Stack;

      I : Vertex_Index'Base := Graph.First_Index - 1;

      Result : Component_Lists.List;

      procedure Strong_Connect (V : in Vertex_Index)
      is begin
         I            := I + 1;
         Number (V)   := I;
         Low_Link (V) := I;
         Points.Push (V);

         for W of Graph (V) loop
            if Number (W) = Invalid_Vertex then
               --  (v, w) is a tree arc
               Strong_Connect (W);
               Low_Link (V) := Vertex_Index'Min (Low_Link (V), Low_Link (W));

            elsif Number (W) < Number (V) then
               --  (v, w) is a frond or cross-link
               if (for some P of Points => P = W) then
                  Low_Link (V) := Vertex_Index'Min (Low_Link (V), Low_Link (W));
               end if;
            end if;
         end loop;
         if Low_Link (V) = Number (V) then
            --  v is the root of a component
            declare
               use all type Ada.Containers.Count_Type;
               Component : Vertex_Lists.List;
            begin
               while (not Points.Is_Empty) and then Number (Points.Peek) >= Number (V) loop
                  Component.Append (Points.Pop);
               end loop;
               if (not Non_Trivial_Only) or Component.Length > 1 then
                  Result.Append (Component);
               end if;
            end;
         end if;
      end Strong_Connect;
   begin
      for W in Graph.First_Index .. Graph.Last_Index loop
         if Number (W) = Invalid_Vertex then
            Strong_Connect (W);
         end if;
      end loop;
      return Result;
   end Strongly_Connected_Components;

end SAL.Gen_Graphs;
