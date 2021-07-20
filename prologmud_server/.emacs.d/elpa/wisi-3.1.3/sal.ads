--  Abstract:
--
--  Root package for Stephe's Ada Library packages.
--
--  See sal.html for more information.
--
--  See http://stephe-leake.org/ada/sal.html for the
--  latest version.
--
--  Contact Stephe at stephen_leake@stephe-leake.org.
--
--  Copyright (C) 1997 - 2004, 2008, 2009, 2015, 2017, 2018 Free Software Foundation, Inc.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce
--  an executable, that does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Containers;

package SAL is
   pragma Pure;

   function Version return String;
   --  Returns string with format "SAL x.xx".

   Container_Empty                  : exception;
   Container_Full                   : exception;
   Config_File_Error                : exception;
   Domain_Error                     : exception;
   Duplicate_Key                    : exception;
   Initialization_Error             : exception;
   Invalid_Format                   : exception;
   Invalid_Limit                    : exception;
   Invalid_Operation                : exception;
   Invalid_Range                    : exception;
   Iterator_Error                   : exception;
   Not_Found                        : exception;
   Not_Implemented                  : exception;
   Parameter_Error                  : exception;
   Programmer_Error                 : exception;
   Range_Error                      : exception;

   --------------
   --  General options

   type Direction_Type is (Forward, Backward);

   type Duplicate_Action_Type is (Allow, Ignore, Error);

   type Overflow_Action_Type is (Overwrite, Error);

   --  We use a new type for Peek_Type, not just
   --  Ada.Containers.Count_Type, to enforce Peek_Type'First = top/first.
   type Base_Peek_Type is new Ada.Containers.Count_Type range 0 .. Ada.Containers.Count_Type'Last;
   subtype Peek_Type is Base_Peek_Type range 1 .. Base_Peek_Type'Last;
   Invalid_Peek_Index : constant Base_Peek_Type := 0;

   type Compare_Result is (Less, Equal, Greater);
end SAL;
