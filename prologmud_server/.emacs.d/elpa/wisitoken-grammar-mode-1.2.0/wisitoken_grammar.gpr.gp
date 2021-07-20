--  Abstract :
--
--  build executables
--
--  Copyright (C) 2017 Free Software Foundation, Inc.
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

with "wisi";
#if ELPA="no"
with "wisitoken";
#end if;
with "standard_common";
project WisiToken_Grammar is

   for Main use
     ("wisitoken_grammar_mode_parse.ads",
      "run_wisitoken_grammar_parse.ads"
     );

   for Source_Dirs use (".");

   case Standard_Common.Profile is
   when "On" =>
      for Object_Dir use "obj_pro";
      for Exec_Dir use "exec_pro";

   when "Off" =>
      for Object_Dir use "obj";
      for Exec_Dir use ".";
   end case;

   for Languages use ("Ada", "C");

   package Compiler is

      case Standard_Common.Build is
      when "Debug" =>
         for Default_Switches ("Ada") use
           Standard_Common.Compiler.Common_Switches &
           Standard_Common.Compiler.Style_Checks &
           Standard_Common.Compiler.Debug_Switches;

         for Default_Switches ("C") use Standard_Common.Compiler.Debug_Switches_C;

      when "Normal" =>
         for Default_Switches ("Ada") use
           Standard_Common.Compiler.Common_Switches &
           Standard_Common.Compiler.Style_Checks &
           Standard_Common.Compiler.Release_Switches;

         for Default_Switches ("C") use Standard_Common.Compiler.Release_Switches_C;
      end case;

   end Compiler;

   package Builder is
      --  We use ".exe" extension even on non-Windows, to simplify the makefiles.
      for Executable_Suffix use ".exe";
   end Builder;

   package Binder is
      for default_switches ("Ada") use ("-E"); -- symbolic traceback
   end Binder;

end WisiToken_Grammar;
