--  Abstract :
--
--  Support for an enumerated token type
--
--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
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

with WisiToken.Lexer.Regexp;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
generic
   type Token_Enum_ID is (<>);
   First_Terminal    : Token_Enum_ID;
   Last_Terminal     : Token_Enum_ID;
   First_Nonterminal : Token_Enum_ID;
   Last_Nonterminal  : Token_Enum_ID;
   EOF_ID            : Token_Enum_ID;
   Accept_ID         : Token_Enum_ID;
   Case_Insensitive  : Boolean;
package WisiToken.Gen_Token_Enum is

   function "+" (Item : in Token_Enum_ID) return Token_ID
     is (Token_ID'First + Token_Enum_ID'Pos (Item));

   function "-" (Item : in Token_ID) return Token_Enum_ID
     is (Token_Enum_ID'Val (Item - Token_ID'First));

   function Token_Enum_Image return Token_ID_Array_String;

   subtype Terminal_Enum_ID is Token_Enum_ID range First_Terminal .. Last_Terminal;
   subtype Nonterminal_Enum_ID is Token_Enum_ID range First_Nonterminal .. Last_Nonterminal;

   LR1_Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal       => +First_Terminal,
      Last_Terminal        => +Last_Terminal,
      First_Nonterminal    => +First_Nonterminal,
      Last_Nonterminal     => +Last_Nonterminal,
      EOI_ID               => +EOF_ID,
      Accept_ID            => +Accept_ID,
      Case_Insensitive     => Case_Insensitive,
      New_Line_ID          => Invalid_Token_ID,
      String_1_ID          => Invalid_Token_ID,
      String_2_ID          => Invalid_Token_ID,
      Image                => (others => null), --  set in body elaboration time code
      Terminal_Image_Width => Terminal_Enum_ID'Width,
      Image_Width          => Token_Enum_ID'Width,
      Last_Lookahead       => +Last_Terminal);

   LALR_Descriptor : aliased WisiToken.Descriptor :=
     (First_Terminal       => +First_Terminal,
      Last_Terminal        => +Last_Terminal,
      First_Nonterminal    => +First_Nonterminal,
      Last_Nonterminal     => +Last_Nonterminal,
      EOI_ID               => +EOF_ID,
      Accept_ID            => +Accept_ID,
      Case_Insensitive     => Case_Insensitive,
      New_Line_ID          => Invalid_Token_ID,
      String_1_ID          => Invalid_Token_ID,
      String_2_ID          => Invalid_Token_ID,
      Image                => (others => null),
      Terminal_Image_Width => Terminal_Enum_ID'Width,
      Image_Width          => Token_Enum_ID'Width,
      Last_Lookahead       => +First_Nonterminal);

   type Enum_Syntax is array (Token_Enum_ID range Token_Enum_ID'First .. Last_Terminal) of
     WisiToken.Lexer.Regexp.Syntax_Item;

   function To_Syntax (Item : in Enum_Syntax) return WisiToken.Lexer.Regexp.Syntax;

   function "&" (Left, Right : in Token_Enum_ID) return Token_ID_Arrays.Vector;

   function "&"
     (Left  : in Token_ID_Arrays.Vector;
      Right : in Token_Enum_ID)
     return Token_ID_Arrays.Vector;

   function "+" (Left : in Token_Enum_ID; Right : in Syntax_Trees.Semantic_Action) return Productions.Right_Hand_Side;

   function "<="
     (Left  : in Token_Enum_ID;
      Right : in WisiToken.Productions.Right_Hand_Side)
     return WisiToken.Productions.Instance;

   ----------
   --  For unit tests

   subtype Terminal_ID is Token_Enum_ID range First_Terminal .. Last_Terminal;
   subtype Nonterminal_ID is Token_Enum_ID range First_Nonterminal .. Last_Nonterminal;
   subtype Grammar_ID is Token_Enum_ID range First_Terminal .. Last_Nonterminal;

   type Nonterminal_Array_Token_Set is array (Nonterminal_ID, Grammar_ID) of Boolean;

   function To_Nonterminal_Array_Token_Set
     (Item : in Nonterminal_Array_Token_Set)
     return WisiToken.Token_Array_Token_Set;

   type Nonterminal_Array_Terminal_Set is array (Nonterminal_ID, Terminal_ID) of Boolean;

   function To_Nonterminal_Array_Terminal_Set
     (Item : in Nonterminal_Array_Terminal_Set)
     return WisiToken.Token_Array_Token_Set;

   type Nonterminal_ID_Set is array (Nonterminal_ID) of Boolean;

   type Token_Array is array (Positive range <>) of Token_Enum_ID;

   function "+" (Item : in Token_Array) return WisiToken.Token_ID_Set;
   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID_Set;

end WisiToken.Gen_Token_Enum;
