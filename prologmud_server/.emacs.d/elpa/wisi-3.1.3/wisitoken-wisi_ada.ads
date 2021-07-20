--  Abstract :
--
--  Type and operations for building a grammar directly in Ada source.
--
--  Copyright (C) 2003, 2013 - 2015, 2017, 2018 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with WisiToken.Productions;
with WisiToken.Syntax_Trees;
package WisiToken.Wisi_Ada is

   function Only (Item : in Token_ID) return WisiToken.Token_ID_Arrays.Vector;
   function "&" (Left : in Token_ID; Right : in Token_ID) return WisiToken.Token_ID_Arrays.Vector;

   function "+"
     (Tokens : in WisiToken.Token_ID_Arrays.Vector;
      Action : in WisiToken.Syntax_Trees.Semantic_Action)
     return WisiToken.Productions.Right_Hand_Side;
   function "+"
     (Tokens : in Token_ID;
      Action : in WisiToken.Syntax_Trees.Semantic_Action)
     return WisiToken.Productions.Right_Hand_Side;
   function "+" (Action : in WisiToken.Syntax_Trees.Semantic_Action) return WisiToken.Productions.Right_Hand_Side;
   --  Create the right hand side of a production.

   function Only (Item : in WisiToken.Productions.Right_Hand_Side) return WisiToken.Productions.RHS_Arrays.Vector;
   function "+" (Item : in WisiToken.Productions.Right_Hand_Side) return WisiToken.Productions.RHS_Arrays.Vector
     renames Only;

   function "or"
     (Left  : in WisiToken.Productions.Instance;
      Right : in WisiToken.Productions.Right_Hand_Side)
     return WisiToken.Productions.Instance;

   function "<="
     (LHS  : in Token_ID;
      RHSs : in WisiToken.Productions.RHS_Arrays.Vector)
     return WisiToken.Productions.Instance;

   function Only (Subject : in WisiToken.Productions.Instance) return WisiToken.Productions.Prod_Arrays.Vector;
   function "+" (Subject : in WisiToken.Productions.Instance) return WisiToken.Productions.Prod_Arrays.Vector
     renames Only;
   --  First production in a grammar.

   function "and"
     (Left  : in WisiToken.Productions.Instance;
      Right : in WisiToken.Productions.Instance)
     return WisiToken.Productions.Prod_Arrays.Vector;
   function "and"
     (Left  : in WisiToken.Productions.Prod_Arrays.Vector;
      Right : in WisiToken.Productions.Instance)
     return WisiToken.Productions.Prod_Arrays.Vector;
   function "and"
     (Left : in WisiToken.Productions.Prod_Arrays.Vector;
      Right : in WisiToken.Productions.Prod_Arrays.Vector)
     return WisiToken.Productions.Prod_Arrays.Vector;
   --  Create a grammar

end WisiToken.Wisi_Ada;
