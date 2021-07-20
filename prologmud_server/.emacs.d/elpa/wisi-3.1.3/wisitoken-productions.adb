--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO;
package body WisiToken.Productions is

   function Image (Item : in Recursion_Arrays.Vector) return String
   is
      use Ada.Strings.Unbounded;
      Result     : Ada.Strings.Unbounded.Unbounded_String := +"(";
      Need_Comma : Boolean                                := False;
   begin
      --  We assume most are None, so we use named association
      for I in Item.First_Index .. Item.Last_Index loop
         if Item (I) /= None then
            Result := Result & (if Need_Comma then ", " else "") & Trimmed_Image (I) & " => " & Image (Item (I));
            Need_Comma := True;
         end if;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Constant_Ref_RHS
     (Grammar : in Prod_Arrays.Vector;
      ID      : in Production_ID)
     return RHS_Arrays.Constant_Reference_Type
   is begin
      return RHS_Arrays.Constant_Ref (Grammar (ID.LHS).RHSs, ID.RHS);
   end Constant_Ref_RHS;

   function Image
     (LHS        : in Token_ID;
      RHS_Index  : in Natural;
      RHS        : in Token_ID_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +Trimmed_Image ((LHS, RHS_Index)) & ": " & Image (LHS, Descriptor) & " <=";
   begin
      for ID of RHS loop
         Result := Result & ' ' & Image (ID, Descriptor);
      end loop;
      return To_String (Result);
   end Image;

   procedure Put (Grammar : Prod_Arrays.Vector; Descriptor : in WisiToken.Descriptor)
   is
      use Ada.Text_IO;
   begin
      for P of Grammar loop
         for R in P.RHSs.First_Index .. P.RHSs.Last_Index loop
            Put (Image (P.LHS, R, P.RHSs (R).Tokens, Descriptor));
            if (for all Item of Grammar (P.LHS).RHSs (R).Recursion => Item = None) then
               New_Line;
            else
               Put_Line (" ; " & Image (Grammar (P.LHS).RHSs (R).Recursion));
            end if;
         end loop;
      end loop;
   end Put;

end WisiToken.Productions;
