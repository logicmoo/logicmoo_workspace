--  Abstract :
--
--  Output Ada source code to recreate Grammar.
--
--  Copyright (C) 2018 - 2019 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Text_IO; use Ada.Text_IO;
with WisiToken.Generate;
with WisiToken.Productions;
procedure WisiToken.BNF.Generate_Grammar
  (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
   Action_Names : in WisiToken.Names_Array_Array)
is
   use all type Ada.Containers.Count_Type;
   use Ada.Strings.Unbounded;
   use WisiToken.Generate;
   use WisiToken.Productions;
   Text : Unbounded_String;
   Need_Comma : Boolean := False;
begin
   Indent_Line
     ("Grammar.Set_First_Last (" & Trimmed_Image (Grammar.First_Index) & ", " &
        Trimmed_Image (Grammar.Last_Index) & ");");

   for Prod of Grammar loop
      Indent_Line ("declare");
      Indent_Line ("   Prod : Instance;");
      Indent_Line ("begin");
      Indent := Indent + 3;
      Indent_Line ("Prod.LHS := " & Trimmed_Image (Prod.LHS) & ";");
      Indent_Line ("Prod.RHSs.Set_First_Last (0, " & Trimmed_Image (Prod.RHSs.Last_Index) & ");");
      for RHS_Index in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
         declare
            RHS : Right_Hand_Side renames Prod.RHSs (RHS_Index);
         begin
            Indent_Line ("declare");
            Indent_Line ("   RHS : Right_Hand_Side;");
            Indent_Line ("begin");
            Indent := Indent + 3;
            if RHS.Tokens.Length > 0 then
               Indent_Line
                 ("RHS.Tokens.Set_First_Last (1, " & Trimmed_Image (Prod.RHSs (RHS_Index).Tokens.Last_Index) & ");");

               if RHS.Tokens.Length = 1 then
                  Indent_Line ("To_Vector ((1 => " & Trimmed_Image (RHS.Tokens (1)) & "), RHS.Tokens);");
               else
                  Need_Comma := False;
                  Text := +"To_Vector ((";
                  for ID of RHS.Tokens  loop
                     if Need_Comma then
                        Text := Text & ", ";
                     else
                        Need_Comma := True;
                     end if;
                     Text := Text & Trimmed_Image (ID);
                  end loop;
                  Text := Text & "), RHS.Tokens);";
                  Indent_Wrap (-Text);
               end if;
            end if;
            if Action_Names (Prod.LHS) /= null and then Action_Names (Prod.LHS)(RHS_Index) /= null then
               Indent_Line ("RHS.Action     := " & Action_Names (Prod.LHS)(RHS_Index).all & "'Access;");
            end if;
            Indent_Line ("Prod.RHSs (" & Trimmed_Image (RHS_Index) & ") := RHS;");
            Indent := Indent - 3;
            Indent_Line ("end;");
         end;
      end loop;
      Indent_Line ("Grammar (" & Trimmed_Image (Prod.LHS) & ") := Prod;");
      Indent := Indent - 3;
      Indent_Line ("end;");
   end loop;
end WisiToken.BNF.Generate_Grammar;
