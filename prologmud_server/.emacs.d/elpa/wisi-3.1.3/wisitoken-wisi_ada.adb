--  Abstract :
--
--  see spec
--
--  Copyright (C) 2013, 2014, 2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body WisiToken.Wisi_Ada is
   use WisiToken.Productions;

   function Only (Item : in Token_ID) return WisiToken.Token_ID_Arrays.Vector
   is begin
      return Result : WisiToken.Token_ID_Arrays.Vector do
         Result.Append (Item);
      end return;
   end Only;

   function "&" (Left : in Token_ID; Right : in Token_ID) return WisiToken.Token_ID_Arrays.Vector
   is begin
      return Result : WisiToken.Token_ID_Arrays.Vector do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   function "+" (Tokens : in Token_ID_Arrays.Vector; Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Tokens, Recursion => <>, Action => Action, Check => null);
   end "+";

   function "+" (Tokens : in Token_ID; Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Only (Tokens), Recursion => <>, Action => Action, Check => null);
   end "+";

   function "+" (Action : in Syntax_Trees.Semantic_Action) return Right_Hand_Side
   is begin
      return (Tokens => <>, Recursion => <>, Action => Action, Check => null);
   end "+";

   function Only (Item : in WisiToken.Productions.Right_Hand_Side) return WisiToken.Productions.RHS_Arrays.Vector
   is begin
      return Result : WisiToken.Productions.RHS_Arrays.Vector do
         Result.Append (Item);
      end return;
   end Only;

   function "or"
     (Left  : in WisiToken.Productions.Instance;
      Right : in WisiToken.Productions.Right_Hand_Side)
     return WisiToken.Productions.Instance
   is begin
      return Result : WisiToken.Productions.Instance := Left do
         Result.RHSs.Append (Right);
      end return;
   end "or";

   function "<=" (LHS : in Token_ID; RHSs : in WisiToken.Productions.RHS_Arrays.Vector) return Instance
   is begin
      return (LHS, RHSs);
   end "<=";

   function Only (Subject : in Instance) return Prod_Arrays.Vector
   is begin
      return Result : Prod_Arrays.Vector do
         Result.Set_First_Last (Subject.LHS, Subject.LHS);
         Result (Subject.LHS) := Subject;
      end return;
   end Only;

   function Merge (Left, Right : in Instance) return Instance
   is
      Index : Integer := Left.RHSs.Last_Index + 1;
   begin
      return Result : Instance := Left do
         Result.RHSs.Set_First_Last (Result.RHSs.First_Index, Left.RHSs.Last_Index + Integer (Right.RHSs.Length));
         for RHS of Right.RHSs loop
            Result.RHSs (Index) := RHS;
            Index := Index + 1;
         end loop;
      end return;
   end Merge;

   function "and" (Left : in Instance; Right : in Instance) return Prod_Arrays.Vector
   is begin
      return Result : Prod_Arrays.Vector do
         Result.Set_First_Last (Token_ID'Min (Left.LHS, Right.LHS), Token_ID'Max (Left.LHS, Right.LHS));
         if Left.LHS = Right.LHS then
            Result (Left.LHS) := Merge (Left, Right);
         else
            Result (Left.LHS) := Left;
            Result (Right.LHS) := Right;
         end if;
      end return;
   end "and";

   function "and" (Left : in Prod_Arrays.Vector; Right : in Instance) return Prod_Arrays.Vector
   is begin
      return Result : Prod_Arrays.Vector := Left do
         if Right.LHS < Result.First_Index then
            Result.Set_First_Last (Right.LHS, Result.Last_Index);
         elsif Right.LHS > Result.Last_Index then
            Result.Set_First_Last (Result.First_Index, Right.LHS);
         end if;

         if Result (Right.LHS).LHS = Invalid_Token_ID then
            Result (Right.LHS) := Right;
         else
            Result (Right.LHS) := Merge (Result (Right.LHS), Right);
         end if;
      end return;
   end "and";

   function "and" (Left : in Prod_Arrays.Vector; Right : in Prod_Arrays.Vector) return Prod_Arrays.Vector
   is begin
      return Result : Prod_Arrays.Vector := Left do
         if Right.First_Index < Result.First_Index then
            Result.Set_First_Last (Right.First_Index, Result.Last_Index);
         elsif Right.First_Index > Result.Last_Index then
            Result.Set_First_Last (Result.First_Index, Right.First_Index);
         end if;
         if Right.Last_Index < Result.First_Index then
            Result.Set_First_Last (Right.Last_Index, Result.Last_Index);
         elsif Right.Last_Index > Result.Last_Index then
            Result.Set_First_Last (Result.First_Index, Right.Last_Index);
         end if;

         for P of Right loop
            if Result (P.LHS).LHS = Invalid_Token_ID then
               Result (P.LHS) := P;
            else
               Result (P.LHS) := Merge (Result (P.LHS), P);
            end if;
         end loop;
      end return;
   end "and";

end WisiToken.Wisi_Ada;
