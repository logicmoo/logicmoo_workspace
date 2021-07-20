--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019, 2020 Free Software Foundation, Inc.
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

with Wisitoken_Grammar_1_Process_Actions;
package body Wisi.WisiToken_Grammar is

   Check_Parens_Action_Index : constant String := "0";

   overriding
   procedure Initialize
     (Data              : in out Parse_Data_Type;
      Lexer             : in     WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Base_Terminals    : in     WisiToken.Base_Token_Array_Access;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     WisiToken.Line_Number_Type;
      End_Line          : in     WisiToken.Line_Number_Type;
      Begin_Indent      : in     Integer;
      Params            : in     String)
   is
      pragma Unreferenced (Params);
      use all type Wisitoken_Grammar_1_Process_Actions.Token_Enum_ID;
   begin
      Wisi.Initialize
        (Wisi.Parse_Data_Type (Data), Lexer, Descriptor, Base_Terminals, Post_Parse_Action,
         Begin_Line, End_Line, Begin_Indent, "");

      Data.First_Comment_ID := +COMMENT_ID;
   end Initialize;

   procedure Check_Parens
     (Data        : in out Wisi.Parse_Data_Type'Class;
      Tree        : in     WisiToken.Syntax_Trees.Tree;
      Tree_Tokens : in     WisiToken.Valid_Node_Index_Array;
      Args        : in     Arg_Index_Array)
   is
      use WisiToken;
   begin
      for Index of Args loop
         declare
            Token : Augmented_Token renames Get_Aug_Token_Const_1 (Tree, Tree.First_Terminal (Tree_Tokens (Index)));
         begin
            Data.Put_Language_Action
              (Check_Parens_Action_Index & Buffer_Pos'Image (Token.Char_Region.First) &
                 Buffer_Pos'Image (Token.Char_Region.Last));
         end;
      end loop;
   end Check_Parens;

end Wisi.WisiToken_Grammar;
