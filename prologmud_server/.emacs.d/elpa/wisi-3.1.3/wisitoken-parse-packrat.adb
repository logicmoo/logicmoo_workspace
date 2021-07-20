--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 Free Software Foundation, Inc.
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

package body WisiToken.Parse.Packrat is

   overriding
   procedure Execute_Actions
     (Parser          : in out Packrat.Parser;
      Image_Augmented : in     Syntax_Trees.Image_Augmented := null)
   is
      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Valid_Node_Index)
      is
         use all type Syntax_Trees.Node_Label;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Syntax_Trees.Semantic_Action;
            Tree_Children : constant Valid_Node_Index_Array := Tree.Children (Node);
         begin
            Parser.User_Data.Reduce (Tree, Node, Tree_Children);

            if Tree.Action (Node) /= null then
               Tree.Action (Node) (Parser.User_Data.all, Tree, Node, Tree_Children);
            end if;
         end;
      end Process_Node;

   begin
      if Trace_Action > Outline then
         if Trace_Action > Extra then
            Parser.Tree.Print_Tree (Descriptor, Parser.Tree.Root, Image_Augmented);
            Parser.Trace.New_Line;
         end if;
         Parser.Trace.Put_Line ("root node: " & Parser.Tree.Image (Parser.Tree.Root, Descriptor));
      end if;

      Parser.Tree.Process_Tree (Process_Node'Access);
   end Execute_Actions;

end WisiToken.Parse.Packrat;
