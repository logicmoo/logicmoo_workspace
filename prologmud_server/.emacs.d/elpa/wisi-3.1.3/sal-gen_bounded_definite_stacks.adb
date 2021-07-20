--  Abstract:
--
--  see spec
--
--  Copyright (C) 1998, 2003, 2009, 2015, 2017 - 2019 Free Software Foundation, Inc.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body SAL.Gen_Bounded_Definite_Stacks
  with Spark_Mode
is
   pragma Suppress (All_Checks);

   procedure Clear (Stack : in out Sgbds.Stack)
   is begin
      Stack.Top := 0;
   end Clear;

   function Depth (Stack : in Sgbds.Stack) return Size_Type
     is (Stack.Top);

   function Is_Empty (Stack : in Sgbds.Stack) return Boolean
   is begin
      return Stack.Top = 0;
   end Is_Empty;

   function Is_Full (Stack : in Sgbds.Stack) return Boolean
   is begin
      return Stack.Top = Stack.Size;
   end Is_Full;

   function Peek
     (Stack : in Sgbds.Stack;
      Index : in Peek_Type := 1)
     return Element_Type
     is (Stack.Data (Stack.Top - Index + 1));

   procedure Pop (Stack : in out Sgbds.Stack; Count : in Base_Peek_Type := 1)
   is begin
      Stack.Top := Stack.Top - Count;
   end Pop;

   procedure Pop (Stack : in out Sgbds.Stack; Item : out Element_Type)
   is begin
      Item := Stack.Peek (1);
      Stack.Top := Stack.Top - 1;
   end Pop;

   function Pop (Stack : in out Sgbds.Stack) return Element_Type with
     Spark_Mode => Off
   is begin
      return Result : Element_Type do
         Pop (Stack, Result);
      end return;
   end Pop;

   procedure Push (Stack : in out Sgbds.Stack; Item : in Element_Type)
   is begin
      Stack.Top := Stack.Top + 1;
      Stack.Data (Stack.Top) := Item;
   end Push;

end SAL.Gen_Bounded_Definite_Stacks;
