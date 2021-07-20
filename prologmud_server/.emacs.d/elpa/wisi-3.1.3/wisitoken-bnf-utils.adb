--  Abstract :
--
--  See spec
--
--  Copyright (C) 2012, 2013, 2015, 2017, 2018 Free Software Foundation, Inc.
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

package body WisiToken.BNF.Utils is

   function Strip_Quotes (Item : in String) return String
   is begin
      if Item'Length < 2 then
         return Item;
      else
         return Item
           ((if Item (Item'First) = '"' then Item'First + 1 else Item'First) ..
              (if Item (Item'Last) = '"' then Item'Last - 1 else Item'Last));
      end if;
   end Strip_Quotes;

   function Strip_Parens (Item : in String) return String
   is begin
      if Item'Length < 2 then
         return Item;
      else
         return Item
           ((if Item (Item'First) = '(' then Item'First + 1 else Item'First) ..
              (if Item (Item'Last) = ')' then Item'Last - 1 else Item'Last));
      end if;
   end Strip_Parens;

end WisiToken.BNF.Utils;
