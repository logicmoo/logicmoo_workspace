--  Abstract :
--
--  Types and operations for computing grammar properties used in
--  generating a packrat parser.
--
--  We use the terminology in [tratt 2010] for recursion in
--  productions.
--
--  References :
--
--  See wisitoken-parse-packrat.ads.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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

package WisiToken.Generate.Packrat is

   type Data (First_Terminal, First_Nonterminal, Last_Nonterminal : Token_ID) is tagged
   record
      --  Data needed to check a grammar and generate code. Tagged to allow
      --  Object.Method syntax. Descriptor not included to avoid duplicating
      --  lots of discriminants.
      Source_File_Name      : Ada.Strings.Unbounded.Unbounded_String;
      Grammar               : WisiToken.Productions.Prod_Arrays.Vector;
      Source_Line_Map       : Productions.Source_Line_Maps.Vector;
      Empty                 : Token_ID_Set (First_Nonterminal .. Last_Nonterminal);
      Direct_Left_Recursive : Token_ID_Set (First_Nonterminal .. Last_Nonterminal);
      First                 : Token_Array_Token_Set
        (First_Nonterminal .. Last_Nonterminal, First_Terminal .. Last_Nonterminal);
      Involved              : Token_Array_Token_Set
        (First_Nonterminal .. Last_Nonterminal, First_Nonterminal .. Last_Nonterminal);
   end record;

   function Initialize
     (Source_File_Name : in String;
      Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Source_Line_Map  : in Productions.Source_Line_Maps.Vector;
      First_Terminal   : in Token_ID)
     return Packrat.Data;

   procedure Check_Recursion (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor);
   --  Check that any rule recursion present is supported.

   procedure Check_RHS_Order (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor);
   --  For each production, check that right hand sides that share
   --  prefixes have the longest right hand side first, and that any
   --  empty right hand side is last.
   --
   --  Violations output a message to Ada.Text_IO.Standard_Error, and
   --  set WisiToken.Generate.Error True.

   procedure Check_All  (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor);
   --  Run all the above checks.
   --
   --  Note that WisiToken.Generate.Check_Consistent is run in
   --  wisi-gen_generate_utils.To_Grammar.

   function Potential_Direct_Left_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set;

end WisiToken.Generate.Packrat;
