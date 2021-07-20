--  Abstract :
--
--  Type and operations for building grammar productions.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with SAL.Gen_Unbounded_Definite_Vectors;
with WisiToken.Semantic_Checks;
with WisiToken.Syntax_Trees;
package WisiToken.Productions is
   use all type Ada.Containers.Count_Type;

   package Recursion_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive, Recursion_Class, Default_Element => None);

   function Image (Item : in Recursion_Arrays.Vector) return String;
   --  For parse_table

   type Right_Hand_Side is record
      Tokens    : Token_ID_Arrays.Vector;
      Recursion : Recursion_Arrays.Vector;
      --  Recursion for each token. There may be more than one recursion cycle for any token,
      --  but we don't track that.

      Action : WisiToken.Syntax_Trees.Semantic_Action;
      Check  : WisiToken.Semantic_Checks.Semantic_Check;
   end record
   with Dynamic_Predicate =>
     (Tokens.Length = 0 or Tokens.First_Index = 1) and
     (Recursion.Length = 0 or
      (Recursion.First_Index = Tokens.First_Index and Recursion.Last_Index = Tokens.Last_Index));

   package RHS_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Natural, Right_Hand_Side, Default_Element => (others => <>));

   type Instance is record
      LHS  : Token_ID := Invalid_Token_ID;
      RHSs : RHS_Arrays.Vector;
   end record;

   package Prod_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Token_ID, Instance, Default_Element => (others => <>));

   function Constant_Ref_RHS
     (Grammar : in Prod_Arrays.Vector;
      ID      : in Production_ID)
     return RHS_Arrays.Constant_Reference_Type;

   function Image
     (LHS        : in Token_ID;
      RHS_Index  : in Natural;
      RHS        : in Token_ID_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  For comments in generated code, diagnostic messages.

   procedure Put (Grammar : Prod_Arrays.Vector; Descriptor : in WisiToken.Descriptor);
   --  Put Image of each production to Ada.Text_IO.Current_Output, for parse_table.

   package Line_Number_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Natural, Line_Number_Type, Default_Element => Invalid_Line_Number);

   type Prod_Source_Line_Map is record
      Line    : Line_Number_Type := Invalid_Line_Number;
      RHS_Map : Line_Number_Arrays.Vector;
   end record;

   package Source_Line_Maps is new SAL.Gen_Unbounded_Definite_Vectors
     (Token_ID, Prod_Source_Line_Map, Default_Element => (others => <>));
   --  For line numbers of productions in source files.

end WisiToken.Productions;
