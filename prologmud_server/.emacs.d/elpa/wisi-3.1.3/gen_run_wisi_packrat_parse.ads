--  Abstract :
--
--  Run an Emacs packrate parser as a standalone executable, for debugging.
--
--  See gen_emacs_wisi_parse_packrat.ads for the Emacs background process.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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

with WisiToken.Parse.Packrat;
with WisiToken.Syntax_Trees;
with WisiToken.Wisi_Runtime;
generic
   type Parse_Data_Type is new WisiToken.Wisi_Runtime.Parse_Data_Type with private;

   Descriptor : in WisiToken.Descriptor;

   with procedure Create_Parser
     (Parser    :    out          WisiToken.Parse.Packrat.Parser;
      Trace     : not null access WisiToken.Trace'Class;
      User_Data : in              WisiToken.Syntax_Trees.User_Data_Access);

procedure Gen_Run_Wisi_Parse_Packrat;
