--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
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

package body WisiToken.Generate.Packrat is

   function Potential_Direct_Right_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set
   is
      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;
   begin
      return Result : Token_ID_Set (Nonterminal) := (others => False) do
         for Prod of Grammar loop
            RHS_Loop :
            for RHS of Prod.RHSs loop
               ID_Loop :
               for I in reverse RHS.Tokens.First_Index + 1 .. RHS.Tokens.Last_Index loop
                  declare
                     ID : constant Token_ID := RHS.Tokens (I);
                  begin
                     if ID = Prod.LHS then
                        Result (ID) := True;
                        exit RHS_Loop;
                     elsif not (ID in Nonterminal) then
                        exit ID_Loop;
                     elsif not Empty (ID) then
                        exit ID_Loop;
                     end if;
                  end;
               end loop ID_Loop;
            end loop RHS_Loop;
         end loop;
      end return;
   end Potential_Direct_Right_Recursive;

   procedure Indirect_Left_Recursive (Data : in out Packrat.Data)
   is
   begin
      for Prod_I of Data.Grammar loop
         for Prod_J of Data.Grammar loop
            Data.Involved (Prod_I.LHS, Prod_J.LHS) :=
              Data.First (Prod_I.LHS, Prod_J.LHS) and
              Data.First (Prod_J.LHS, Prod_I.LHS);
         end loop;
      end loop;
   end Indirect_Left_Recursive;

   ----------
   --  Public subprograms

   function Initialize
     (Source_File_Name : in String;
      Grammar          : in WisiToken.Productions.Prod_Arrays.Vector;
      Source_Line_Map  : in Productions.Source_Line_Maps.Vector;
      First_Terminal   : in Token_ID)
     return Packrat.Data
   is
      Empty : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Grammar);
   begin
      return Result : Packrat.Data :=
        (First_Terminal        => First_Terminal,
         First_Nonterminal     => Grammar.First_Index,
         Last_Nonterminal      => Grammar.Last_Index,
         Source_File_Name      => +Source_File_Name,
         Grammar               => Grammar,
         Source_Line_Map       => Source_Line_Map,
         Empty                 => Empty,
         Direct_Left_Recursive => Potential_Direct_Left_Recursive (Grammar, Empty),
         First                 => WisiToken.Generate.First (Grammar, Empty, First_Terminal => First_Terminal),
         Involved              => (others => (others => False)))
      do
         Indirect_Left_Recursive (Result);
      end return;
   end Initialize;

   procedure Check_Recursion (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor)
   is
      Right_Recursive : constant Token_ID_Set := Potential_Direct_Right_Recursive (Data.Grammar, Data.Empty);
   begin
      for Prod of Data.Grammar loop
         if Data.Direct_Left_Recursive (Prod.LHS) and Right_Recursive (Prod.LHS) then
            --  We only implement the simplest left recursion solution ([warth
            --  2008] figure 3); [tratt 2010] section 6.3 gives this condition for
            --  that to be valid.
            --  FIXME: not quite? definite direct right recursive ok?
            --  FIXME: for indirect left recursion, need potential indirect right recursive check?
            Put_Error
              (Error_Message
                 (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).Line, "'" & Image (Prod.LHS, Descriptor) &
                    "' is both left and right recursive; not supported."));
         end if;

         for I in Data.Involved'Range (2) loop
            if Prod.LHS /= I and then Data.Involved (Prod.LHS, I) then
               Put_Error
                 (Error_Message
                    (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).Line, "'" & Image (Prod.LHS, Descriptor) &
                       "' is indirect recursive with " & Image (I, Descriptor) & ", not supported"));
            end if;
         end loop;
      end loop;
   end Check_Recursion;

   procedure Check_RHS_Order (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor)
   is
      use all type Ada.Containers.Count_Type;
   begin
      for Prod of Data.Grammar loop
         --  Empty must be last
         for I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index - 1 loop
            if Prod.RHSs (I).Tokens.Length = 0 then
               Put_Error
                 (Error_Message
                    (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).RHS_Map (I),
                     "right hand side" & Integer'Image (I) & " in " & Image (Prod.LHS, Descriptor) &
                       " is empty, but not last; no later right hand side will match."));
               WisiToken.Generate.Error := True;
            end if;
         end loop;

         for I in Prod.RHSs.First_Index + 1 .. Prod.RHSs.Last_Index loop
            declare
               Cur : Token_ID_Arrays.Vector renames Prod.RHSs (I).Tokens;
            begin
               --  Shared prefix; longer must be first
               for J in Prod.RHSs.First_Index .. I - 1 loop
                  declare
                     Prev : Token_ID_Arrays.Vector renames Prod.RHSs (J).Tokens;
                     K    : constant Natural := Shared_Prefix (Prev, Cur);
                  begin
                     if K > 0 and Prev.Length < Cur.Length then
                        Put_Error
                          (Error_Message
                             (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).RHS_Map (I),
                              "right hand side" & Integer'Image (I) & " in " & Image (Prod.LHS, Descriptor) &
                                " may never match; it shares a prefix with a shorter previous rhs" &
                                Integer'Image (J) & "."));
                     end if;
                  end;
               end loop;

               --  recursion; typical LALR list is written:
               --
               --  statement_list
               --    : statement
               --    | statement_list statement
               --    ;
               --  association_list
               --    : association
               --    | association_list COMMA association
               --    ;
               --
               --  a different recursive definition:
               --
               --  name
               --    : IDENTIFIER
               --    | name LEFT_PAREN range_list RIGHT_PAREN
               --    | name actual_parameter_part
               --    ...
               --    ;
               --
               --  For packrat, the recursive RHSs must come before others:
               --
               --  statement_list
               --    : statement_list statement
               --    | statement
               --    ;
               --  association_list
               --    : association_list COMMA association
               --    | association
               --    ;
               --  name
               --    : name LEFT_PAREN range_list RIGHT_PAREN
               --    | name actual_parameter_part
               --    | IDENTIFIER
               --    ...
               --    ;
               declare
                  Prev : Token_ID_Arrays.Vector renames Prod.RHSs (I - 1).Tokens;
               begin
                  if Cur.Length > 0 and then Prev.Length > 0 and then
                    Cur (1) = Prod.LHS and then Prev (1) /= Prod.LHS
                  then
                     Put_Error
                       (Error_Message
                          (-Data.Source_File_Name, Data.Source_Line_Map (Prod.LHS).Line,
                           "recursive right hand sides must be before others."));
                  end if;
               end;
            end;
         end loop;
      end loop;
   end Check_RHS_Order;

   procedure Check_All (Data : in Packrat.Data; Descriptor : in WisiToken.Descriptor)
   is begin
      Check_Recursion (Data, Descriptor);
      Check_RHS_Order (Data, Descriptor);
   end Check_All;

   function Potential_Direct_Left_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set
   is
      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;
   begin
      --  FIXME: this duplicates the computation of First; if keep First,
      --  change this to use it.
      return Result : Token_ID_Set (Nonterminal) := (others => False) do
         for Prod of Grammar loop
            RHS_Loop :
            for RHS of Prod.RHSs loop
               ID_Loop :
               for ID of RHS.Tokens loop
                  if ID = Prod.LHS then
                     Result (ID) := True;
                     exit RHS_Loop;
                  elsif not (ID in Nonterminal) then
                     exit ID_Loop;
                  elsif not Empty (ID) then
                     exit ID_Loop;
                  end if;
               end loop ID_Loop;
            end loop RHS_Loop;
         end loop;
      end return;
   end Potential_Direct_Left_Recursive;

end WisiToken.Generate.Packrat;
