--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 All Rights Reserved.
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

with Run_Wisi_Common_Parse;
with WisiToken.Text_IO_Trace;
procedure Gen_Run_Wisi_LR_Parse
is
   Trace      : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Unrestricted_Access);
   Parser     : WisiToken.Parse.LR.Parser.Parser;
   Parse_Data : aliased Parse_Data_Type (Parser.Terminals'Access, Parser.Line_Begin_Token'Access);
begin
   --  Create parser first so Put_Usage has defaults from Parser.Table,
   --  and Get_CL_Params can override them.
   Create_Parser
     (Parser, Language_Fixes, Language_Matching_Begin_Tokens, Language_String_ID_Set,
      Trace'Unrestricted_Access, Parse_Data'Unchecked_Access);

   Run_Wisi_Common_Parse.Parse_File (Parser, Parse_Data, Descriptor);

end Gen_Run_Wisi_LR_Parse;
