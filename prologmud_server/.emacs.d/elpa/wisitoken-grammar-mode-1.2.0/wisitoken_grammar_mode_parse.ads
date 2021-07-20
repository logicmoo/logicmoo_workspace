--  Abstract :
--
--  External process parser for wisitoken-grammar mode
--
--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Wisitoken_Grammar_1_Process_Actions;
with Wisitoken_Grammar_1_Process_Main;
with Gen_Emacs_Wisi_LR_Parse;
with Wisi.WisiToken_Grammar;
procedure WisiToken_Grammar_Mode_Parse is new Gen_Emacs_Wisi_LR_Parse
  (Parse_Data_Type                => Wisi.WisiToken_Grammar.Parse_Data_Type,
   Name                           => "wisi_grammar_mode",
   Language_Protocol_Version      => Wisi.WisiToken_Grammar.Language_Protocol_Version,
   Descriptor                     => Wisitoken_Grammar_1_Process_Actions.Descriptor,
   Partial_Parse_Active           => Wisitoken_Grammar_1_Process_Actions.Partial_Parse_Active,
   Language_Fixes                 => null,
   Language_Matching_Begin_Tokens => null,
   Language_String_ID_Set         => null,
   Create_Parser                  => Wisitoken_Grammar_1_Process_Main.Create_Parser);
