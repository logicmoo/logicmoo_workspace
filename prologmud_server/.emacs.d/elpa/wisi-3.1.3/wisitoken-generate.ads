--  Abstract :
--
--  Types and operations for generating parsers, common to all parser
--  types.
--
--  The wisi* packages deal with reading *.wy files and generating
--  source code files. The wisitoken-generate* packages deal with
--  computing parser properties from the grammar. (For historical
--  reasons, not all packages follow this naming convention yet).
--
--  References :
--
--  See wisitoken.ads
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

with Ada.Containers.Doubly_Linked_Lists;
with SAL.Ada_Containers.Gen_Doubly_Linked_Lists_Image;
with SAL.Gen_Graphs;
with WisiToken.Productions;
package WisiToken.Generate is

   Error : Boolean := False;
   --  Set True by errors during grammar generation

   function Error_Message
     (File_Name : in String;
      File_Line : in WisiToken.Line_Number_Type;
      Message   : in String)
     return String;

   procedure Put_Error (Message : in String);
   --  Set Error True, output Message to Standard_Error

   procedure Check_Consistent
     (Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor       : in WisiToken.Descriptor;
      Source_File_Name : in String);
   --  Check requirements on Descriptor values.

   function Check_Unused_Tokens
     (Descriptor : in WisiToken.Descriptor;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector)
     return Boolean;
   --  Return False if there is a terminal or nonterminal that is not
   --  used in the grammar.
   --
   --  Raises Grammar_Error if there is a non-grammar token used in the
   --  grammar.

   function Nullable (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Token_Array_Production_ID;
   --  If ID is nullable, Result (ID) is the production that should be
   --  reduced to produce the null. Otherwise Result (ID) is
   --  Invalid_Production_ID.

   function Has_Empty_Production (Nullable : in Token_Array_Production_ID) return Token_ID_Set;
   function Has_Empty_Production (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Token_ID_Set;
   --  Result (ID) is True if any production for ID can be an empty
   --  production, recursively.

   function First
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in Token_ID_Set;
      First_Terminal       : in Token_ID)
     return Token_Array_Token_Set;
   --  For each nonterminal in Grammar, find the set of tokens
   --  (terminal or nonterminal) that any string derived from it can
   --  start with. Together with Has_Empty_Production, implements
   --  algorithm FIRST from [dragon], augmented with nonterminals.
   --
   --  LALR, LR1 generate want First as both Token_Sequence_Arrays.Vector
   --  and Token_Array_Token_Set, Packrat wants Token_Array_Token_Set,
   --  existing tests all use Token_Array_Token_Set. So for LR1 we use
   --  To_Terminal_Sequence_Array.

   function To_Terminal_Sequence_Array
     (First      : in Token_Array_Token_Set;
      Descriptor : in WisiToken.Descriptor)
     return Token_Sequence_Arrays.Vector;
   --  Only includes terminals.

   function Follow
     (Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor;
      First                : in Token_Array_Token_Set;
      Has_Empty_Production : in Token_ID_Set)
     return Token_Array_Token_Set;
   --  For each nonterminal in Grammar, find the set of terminal
   --  tokens that can follow it. Implements algorithm FOLLOW from
   --  [dragon] pg 189.

   ----------
   --  Recursion

   --  Recursion is the result of a cycle in the grammar. We can form a
   --  graph representing the grammar by taking the nonterminals as the
   --  graph vertices, and the occurrence of a nonterminal in a
   --  production right hand side as a directed edge from the left hand
   --  side of the production to that nonterminal. Then recursion is
   --  represented by a cycle in the graph.

   type Edge_Data is record
      --  The edge leading to this node.
      RHS         : Natural  := Natural'Last;
      Token_Index : Positive := Positive'Last;
   end record;

   function Edge_Image (Edge : in Edge_Data) return String is (Trimmed_Image (Edge.RHS));

   type Base_Recursion_Index is range 0 .. Integer'Last;
   subtype Recursion_Index is Base_Recursion_Index range 1 .. Base_Recursion_Index'Last;
   Invalid_Recursion_Index : constant Base_Recursion_Index := 0;
   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Base_Recursion_Index);

   package Grammar_Graphs is new SAL.Gen_Graphs
     (Edge_Data         => Generate.Edge_Data,
      Default_Edge_Data => (others => <>),
      Vertex_Index      => Token_ID,
      Invalid_Vertex    => Invalid_Token_ID,
      Path_Index        => Recursion_Index,
      Edge_Image        => Edge_Image);

   subtype Recursion_Cycle is Grammar_Graphs.Path;
   --  A recursion, with lowest numbered production first. If there is
   --  only one element, the recursion is direct; otherwise indirect.

   subtype Recursion_Array is Grammar_Graphs.Path_Arrays.Vector;
   --  For the collection of all cycles.

   type Recursions is record
      Full       : Boolean;
      Recursions : Recursion_Array;
      --  If Full, elements are paths; edges at path (I) are to path (I). If
      --  not Full, elements are strongly connected components; edges at
      --  path (I) are from path (I).
   end record;

   package Recursion_Lists is new Ada.Containers.Doubly_Linked_Lists (Recursion_Index);
   function Image is new SAL.Ada_Containers.Gen_Doubly_Linked_Lists_Image
     (Recursion_Index, "=", Recursion_Lists, Trimmed_Image);

   function To_Graph (Grammar : in WisiToken.Productions.Prod_Arrays.Vector) return Grammar_Graphs.Graph;

   function Compute_Full_Recursion
     (Grammar    : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor)
     return Recursions;
   --  Each element of result is a cycle in the grammar. Also sets
   --  Recursive components in Grammar.

   function Compute_Partial_Recursion
     (Grammar    : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor)
     return Recursions;
   --  Each element of the result contains all members of a non-trivial
   --  strongly connected component in the grammar, in arbitrary order.
   --  This is an approximation to the full recursion, when that is too
   --  hard to compute (ie for Java).
   --
   --  Also sets Recursive components in Grammar.

   ----------
   --  Indented text output. Mostly used for code generation in wisi,
   --  also used in outputing the parse_table and other debug stuff.

   Max_Line_Length : constant := 120;

   Indent     : Standard.Ada.Text_IO.Positive_Count := 1;
   Line_Count : Integer;

   procedure Indent_Line (Text : in String);
   --  Put Text, indented to Indent, to Current_Output, with newline.

   procedure Indent_Start (Text : in String);
   --  Put Text indented to Indent to Current_Output, without newline.
   --  Should be followed by Put_Line, not Indent_Line.

   procedure Indent_Wrap (Text : in String);
   --  Put Text, indented to Indent, wrapped at Max_Line_Length, to
   --  Current_Output, ending with newline.

   procedure Indent_Wrap_Comment (Text : in String; Comment_Syntax : in String);
   --  Put Text, prefixed by Comment_Syntax and two spaces, indented to
   --  Indent, wrapped at Max_Line_Length, to Current_Output, ending with
   --  newline.

end WisiToken.Generate;
