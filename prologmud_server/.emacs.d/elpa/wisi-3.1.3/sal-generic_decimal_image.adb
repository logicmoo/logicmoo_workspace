--  Abstract:
--
--  see spec
--
--  Copyright (C) 2005, 2006, 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

function SAL.Generic_Decimal_Image
  (Item  : in Number_Type;
   Width : in Natural)
  return String
is
   pragma Warnings (Off);
   --  Avoid warning about "abs applied to non-negative value has no
   --  effect" for some instantiations.
   Temp : Integer := abs Integer (Item);
   --  IMPROVEME: need test for Decimal_Image, include constrained positive number_type
   pragma Warnings (On);
   Digit : Integer;
   Image : String (1 .. Width);
begin
   for I in reverse Image'Range loop
      Digit     := Temp mod 10;
      Temp      := Temp / 10;
      Image (I) := Character'Val (Character'Pos ('0') + Digit);
   end loop;
   return Image;
end SAL.Generic_Decimal_Image;
