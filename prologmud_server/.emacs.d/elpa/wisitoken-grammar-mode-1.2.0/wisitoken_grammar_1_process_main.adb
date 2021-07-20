--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LR1 Ada_Emacs re2c PROCESS wisitoken_grammar_1.wy
--

--  Copyright (C) 2017 - 2019 Free Software Foundation, Inc.
--
--  Author: Stephen Leake <stephe-leake@stephe-leake.org>
--
--  This file is part of GNU Emacs.
--
--  GNU Emacs is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  GNU Emacs is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with Wisitoken_Grammar_1_Process_Actions; use Wisitoken_Grammar_1_Process_Actions;
with WisiToken.Lexer.re2c;
with wisitoken_grammar_1_re2c_c;
package body Wisitoken_Grammar_1_Process_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (wisitoken_grammar_1_re2c_c.New_Lexer,
      wisitoken_grammar_1_re2c_c.Free_Lexer,
      wisitoken_grammar_1_re2c_c.Reset_Lexer,
      wisitoken_grammar_1_re2c_c.Next_Token);

   procedure Create_Parser
     (Parser                         :    out WisiToken.Parse.LR.Parser.Parser;
      Language_Fixes                 : in     WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set       : in     WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.Parse.LR;
      McKenzie_Param : constant McKenzie_Param_Type :=
        (First_Terminal    => 3,
         Last_Terminal     => 36,
         First_Nonterminal => 37,
         Last_Nonterminal  => 57,
         Insert =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2),
         Delete =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Push_Back =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Undo_Reduce =>
           (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
         Minimal_Complete_Cost_Delta => -1,
         Fast_Forward =>  0,
         Matching_Begin =>  0,
         Ignore_Check_Fail  => 2,
         Task_Count  => 0,
         Check_Limit => 4,
         Check_Delta_Limit => 2147483647,
         Enqueue_Limit => 10000);

      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 209,
         First_Terminal    => 3,
         Last_Terminal     => 36,
         First_Nonterminal => 37,
         Last_Nonterminal  => 57);
   begin
      Table.McKenzie_Param := McKenzie_Param;
      declare
         procedure Subr_1
         is begin
            Table.States (0).Action_List.Set_Capacity (2);
            Add_Action (Table.States (0), 23, (38, 0), 1);
            Add_Action (Table.States (0), 33, (43, 0), 2);
            Table.States (0).Goto_List.Set_Capacity (4);
            Add_Goto (Table.States (0), 38, 3);
            Add_Goto (Table.States (0), 43, 4);
            Add_Goto (Table.States (0), 55, 5);
            Add_Goto (Table.States (0), 56, 6);
            Table.States (1).Action_List.Set_Capacity (7);
            Add_Action (Table.States (1), 3, (38, 1), 7);
            Add_Action (Table.States (1), 4, (38, 5), 8);
            Add_Action (Table.States (1), 5, (38, 4), 9);
            Add_Action (Table.States (1), 6, (39, 0), 10);
            Add_Action (Table.States (1), 7, (39, 1), 11);
            Add_Action (Table.States (1), 8, (39, 2), 12);
            Add_Action (Table.States (1), 33, (38, 2), 13);
            Table.States (1).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (1), 39, 14);
            Table.States (1).Kernel := To_Vector ((((38, 0),  23,  3, (2147483647, 0),  0), ((38, 1),  23,  3,
            (2147483647, 0),  0), ((38, 2),  23,  2, (2147483647, 0),  0), ((38, 3),  23,  1, (2147483647, 0),  0),
            ((38, 4),  23,  4, (2147483647, 0),  0), ((38, 5),  23,  2, (2147483647, 0),  0)));
            Table.States (1).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (38, 2),  33, 13)));
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 13, (57, 0), 15);
            Add_Action (Table.States (2), 14, (57, 1), 16);
            Table.States (2).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (2), 57, 17);
            Table.States (2).Kernel := To_Vector ((0 => ((43, 0),  33,  1, (2147483647, 0),  0)));
            Table.States (2).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (57, 0),  13, 15)));
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (23, 33, 36), (55, 0),  1, null, null);
            Table.States (3).Kernel := To_Vector ((0 => ((55, 0),  38,  0, (55, 0),  1)));
            Table.States (3).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 0),  1)));
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (23, 33, 36), (55, 1),  1, null, null);
            Table.States (4).Kernel := To_Vector ((0 => ((55, 1),  43,  0, (55, 1),  1)));
            Table.States (4).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (55, 1),  1)));
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (23, 33, 36), (56, 0),  1, compilation_unit_list_0'Access, null);
            Table.States (5).Kernel := To_Vector ((0 => ((56, 0),  55,  0, (56, 0),  1)));
            Table.States (5).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 0),  1)));
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 23, (38, 0), 1);
            Add_Action (Table.States (6), 33, (43, 0), 2);
            Add_Action (Table.States (6), 36, Accept_It, (37, 0),  1, null, null);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 38, 3);
            Add_Goto (Table.States (6), 43, 4);
            Add_Goto (Table.States (6), 55, 18);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 33, (40, 0), 19);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 40, 20);
            Table.States (7).Kernel := To_Vector ((0 => ((38, 1),  3,  2, (2147483647, 0),  0)));
            Table.States (7).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (40, 0),  33, 19)));
            Table.States (8).Action_List.Set_Capacity (1);
            Add_Action (Table.States (8), 5, (38, 5), 21);
            Table.States (8).Kernel := To_Vector ((0 => ((38, 5),  4,  1, (2147483647, 0),  0)));
            Table.States (8).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (38, 5),  5, 21)));
            Table.States (9).Action_List.Set_Capacity (1);
            Add_Action (Table.States (9), 33, (38, 4), 22);
            Table.States (9).Kernel := To_Vector ((0 => ((38, 4),  5,  3, (2147483647, 0),  0)));
            Table.States (9).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (38, 4),  33, 22)));
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), (1 =>  33), (39, 0),  1, null, null);
            Table.States (10).Kernel := To_Vector ((0 => ((39, 0),  6,  0, (39, 0),  1)));
            Table.States (10).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (39, 0),  1)));
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), 21, (39, 1), 23);
            Table.States (11).Kernel := To_Vector ((0 => ((39, 1),  7,  3, (2147483647, 0),  0)));
            Table.States (11).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (39, 1),  21, 23)));
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 21, (39, 2), 24);
            Table.States (12).Kernel := To_Vector ((0 => ((39, 2),  8,  3, (2147483647, 0),  0)));
            Table.States (12).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (39, 2),  21, 24)));
            Table.States (13).Action_List.Set_Capacity (13);
            Add_Action (Table.States (13), 8, (42, 10), 25);
            Add_Action (Table.States (13), 10, (42, 5), 26);
            Add_Action (Table.States (13), 15, (42, 0), 27);
            Add_Action (Table.States (13), 16, (42, 2), 28);
            Add_Action (Table.States (13), 20, (42, 3), 29);
            Add_Action (Table.States (13), 23, Reduce, (38, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (13), 28, (42, 6), 30);
            Add_Action (Table.States (13), 30, (42, 7), 31);
            Add_Action (Table.States (13), 32, (42, 4), 32);
            Add_Action (Table.States (13), 33, (42, 1), 33);
            Add_Conflict (Table.States (13), 33, (38, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (13), 34, (42, 8), 34);
            Add_Action (Table.States (13), 35, (42, 9), 35);
            Add_Action (Table.States (13), 36, Reduce, (38, 3),  2, declaration_3'Access, null);
            Table.States (13).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (13), 41, 36);
            Add_Goto (Table.States (13), 42, 37);
            Table.States (13).Kernel := To_Vector ((((38, 2),  33,  1, (2147483647, 0),  0), ((38, 3),  33,  0, (38,
            3),  2)));
            Table.States (13).Minimal_Complete_Actions := To_Vector (((Shift, (42, 0),  15, 27), (Reduce, (38, 3),
            2)));
            Table.States (14).Action_List.Set_Capacity (1);
            Add_Action (Table.States (14), 33, (38, 0), 38);
            Table.States (14).Kernel := To_Vector ((0 => ((38, 0),  39,  2, (2147483647, 0),  0)));
            Table.States (14).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (38, 0),  33, 38)));
            Table.States (15).Action_List.Set_Capacity (10);
            Add_Action (Table.States (15), (12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (57, 0),  1, null, null);
            Table.States (15).Kernel := To_Vector ((0 => ((57, 0),  13,  0, (57, 0),  1)));
            Table.States (15).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 0),  1)));
            Table.States (16).Action_List.Set_Capacity (10);
            Add_Action (Table.States (16), (12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (57, 1),  1, null, null);
            Table.States (16).Kernel := To_Vector ((0 => ((57, 1),  14,  0, (57, 1),  1)));
            Table.States (16).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (57, 1),  1)));
            Table.States (17).Action_List.Set_Capacity (10);
            Add_Action (Table.States (17), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (17), 18, (53, 0), 39);
            Add_Action (Table.States (17), 19, (52, 0), 40);
            Add_Action (Table.States (17), 20, (51, 0), 41);
            Add_Action (Table.States (17), 21, (47, 0), 42);
            Add_Action (Table.States (17), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (17), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (17), 33, (48, 1), 43);
            Add_Conflict (Table.States (17), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (17), 35, (50, 1), 44);
            Add_Action (Table.States (17), 36, Reduce, (46, 0),  0, null, null);
            Table.States (17).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (17), 45, 45);
            Add_Goto (Table.States (17), 46, 46);
            Add_Goto (Table.States (17), 47, 47);
            Add_Goto (Table.States (17), 48, 48);
            Add_Goto (Table.States (17), 49, 49);
            Add_Goto (Table.States (17), 50, 50);
            Add_Goto (Table.States (17), 51, 51);
            Add_Goto (Table.States (17), 52, 52);
            Add_Goto (Table.States (17), 53, 53);
            Table.States (17).Kernel := To_Vector ((0 => ((43, 0),  57,  0, (45, 3),  4)));
            Table.States (17).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 3),  4)));
            Table.States (18).Action_List.Set_Capacity (3);
            Add_Action (Table.States (18), (23, 33, 36), (56, 1),  2, compilation_unit_list_1'Access, null);
            Table.States (18).Kernel := To_Vector ((0 => ((56, 1),  55,  0, (56, 1),  2)));
            Table.States (18).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (56, 1),  2)));
            Table.States (19).Action_List.Set_Capacity (2);
            Add_Action (Table.States (19), (9, 33), (40, 0),  1, null, null);
            Table.States (19).Kernel := To_Vector ((0 => ((40, 0),  33,  0, (40, 0),  1)));
            Table.States (19).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 0),  1)));
            Table.States (20).Action_List.Set_Capacity (2);
            Add_Action (Table.States (20), 9, (38, 1), 54);
            Add_Action (Table.States (20), 33, (40, 1), 55);
            Table.States (20).Kernel := To_Vector ((((38, 1),  40,  1, (2147483647, 0),  0), ((40, 1),  40,  1,
            (2147483647, 0),  0)));
            Table.States (20).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (38, 1),  9, 54)));
            Table.States (21).Action_List.Set_Capacity (3);
            Add_Action (Table.States (21), (23, 33, 36), (38, 5),  3, declaration_5'Access, null);
            Table.States (21).Kernel := To_Vector ((0 => ((38, 5),  5,  0, (38, 5),  3)));
            Table.States (21).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (38, 5),  3)));
            Table.States (22).Action_List.Set_Capacity (1);
            Add_Action (Table.States (22), 16, (38, 4), 56);
            Table.States (22).Kernel := To_Vector ((0 => ((38, 4),  33,  2, (2147483647, 0),  0)));
            Table.States (22).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (38, 4),  16, 56)));
            Table.States (23).Action_List.Set_Capacity (1);
            Add_Action (Table.States (23), 33, (39, 1), 57);
            Table.States (23).Kernel := To_Vector ((0 => ((39, 1),  21,  2, (2147483647, 0),  0)));
            Table.States (23).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (39, 1),  33, 57)));
            Table.States (24).Action_List.Set_Capacity (1);
            Add_Action (Table.States (24), 33, (39, 2), 58);
            Table.States (24).Kernel := To_Vector ((0 => ((39, 2),  21,  2, (2147483647, 0),  0)));
            Table.States (24).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (39, 2),  33, 58)));
            Table.States (25).Action_List.Set_Capacity (13);
            Add_Action (Table.States (25), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 10),  1, null,
            null);
            Table.States (25).Kernel := To_Vector ((0 => ((42, 10),  8,  0, (42, 10),  1)));
            Table.States (25).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 10),  1)));
            Table.States (26).Action_List.Set_Capacity (13);
            Add_Action (Table.States (26), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 5),  1,
            declaration_item_5'Access, null);
            Table.States (26).Kernel := To_Vector ((0 => ((42, 5),  10,  0, (42, 5),  1)));
            Table.States (26).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 5),  1)));
            Table.States (27).Action_List.Set_Capacity (13);
            Add_Action (Table.States (27), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 0),  1, null,
            null);
            Table.States (27).Kernel := To_Vector ((0 => ((42, 0),  15,  0, (42, 0),  1)));
            Table.States (27).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 0),  1)));
            Table.States (28).Action_List.Set_Capacity (13);
            Add_Action (Table.States (28), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 2),  1, null,
            null);
            Table.States (28).Kernel := To_Vector ((0 => ((42, 2),  16,  0, (42, 2),  1)));
            Table.States (28).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 2),  1)));
            Table.States (29).Action_List.Set_Capacity (13);
            Add_Action (Table.States (29), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 3),  1, null,
            null);
            Table.States (29).Kernel := To_Vector ((0 => ((42, 3),  20,  0, (42, 3),  1)));
            Table.States (29).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 3),  1)));
            Table.States (30).Action_List.Set_Capacity (13);
            Add_Action (Table.States (30), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 6),  1, null,
            null);
            Table.States (30).Kernel := To_Vector ((0 => ((42, 6),  28,  0, (42, 6),  1)));
            Table.States (30).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 6),  1)));
            Table.States (31).Action_List.Set_Capacity (13);
            Add_Action (Table.States (31), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 7),  1, null,
            null);
            Table.States (31).Kernel := To_Vector ((0 => ((42, 7),  30,  0, (42, 7),  1)));
            Table.States (31).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 7),  1)));
            Table.States (32).Action_List.Set_Capacity (13);
            Add_Action (Table.States (32), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 4),  1, null,
            null);
            Table.States (32).Kernel := To_Vector ((0 => ((42, 4),  32,  0, (42, 4),  1)));
            Table.States (32).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 4),  1)));
            Table.States (33).Action_List.Set_Capacity (13);
            Add_Action (Table.States (33), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 1),  1, null,
            null);
            Table.States (33).Kernel := To_Vector ((0 => ((42, 1),  33,  0, (42, 1),  1)));
            Table.States (33).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 1),  1)));
            Table.States (34).Action_List.Set_Capacity (13);
            Add_Action (Table.States (34), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 8),  1,
            declaration_item_8'Access, null);
            Table.States (34).Kernel := To_Vector ((0 => ((42, 8),  34,  0, (42, 8),  1)));
            Table.States (34).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 8),  1)));
            Table.States (35).Action_List.Set_Capacity (13);
            Add_Action (Table.States (35), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 9),  1,
            declaration_item_9'Access, null);
            Table.States (35).Kernel := To_Vector ((0 => ((42, 9),  35,  0, (42, 9),  1)));
            Table.States (35).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (42, 9),  1)));
            Table.States (36).Action_List.Set_Capacity (13);
            Add_Action (Table.States (36), 8, (42, 10), 25);
            Add_Action (Table.States (36), 10, (42, 5), 26);
            Add_Action (Table.States (36), 15, (42, 0), 27);
            Add_Action (Table.States (36), 16, (42, 2), 28);
            Add_Action (Table.States (36), 20, (42, 3), 29);
            Add_Action (Table.States (36), 23, Reduce, (38, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (36), 28, (42, 6), 30);
            Add_Action (Table.States (36), 30, (42, 7), 31);
            Add_Action (Table.States (36), 32, (42, 4), 32);
            Add_Action (Table.States (36), 33, (42, 1), 33);
            Add_Conflict (Table.States (36), 33, (38, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (36), 34, (42, 8), 34);
            Add_Action (Table.States (36), 35, (42, 9), 35);
            Add_Action (Table.States (36), 36, Reduce, (38, 2),  3, declaration_2'Access, null);
            Table.States (36).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (36), 42, 59);
            Table.States (36).Kernel := To_Vector ((((38, 2),  41,  0, (38, 2),  3), ((41, 1),  41,  1, (2147483647,
            0),  0)));
            Table.States (36).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (38, 2),  3)));
            Table.States (37).Action_List.Set_Capacity (13);
            Add_Action (Table.States (37), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (41, 0),  1, null,
            null);
            Table.States (37).Kernel := To_Vector ((0 => ((41, 0),  42,  0, (41, 0),  1)));
            Table.States (37).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 0),  1)));
            Table.States (38).Action_List.Set_Capacity (11);
            Add_Action (Table.States (38), 8, (42, 10), 25);
            Add_Action (Table.States (38), 10, (42, 5), 26);
            Add_Action (Table.States (38), 15, (42, 0), 27);
            Add_Action (Table.States (38), 16, (42, 2), 28);
            Add_Action (Table.States (38), 20, (42, 3), 29);
            Add_Action (Table.States (38), 28, (42, 6), 30);
            Add_Action (Table.States (38), 30, (42, 7), 31);
            Add_Action (Table.States (38), 32, (42, 4), 32);
            Add_Action (Table.States (38), 33, (42, 1), 33);
            Add_Action (Table.States (38), 34, (42, 8), 34);
            Add_Action (Table.States (38), 35, (42, 9), 35);
            Table.States (38).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (38), 41, 60);
            Add_Goto (Table.States (38), 42, 37);
            Table.States (38).Kernel := To_Vector ((0 => ((38, 0),  33,  1, (2147483647, 0),  0)));
            Table.States (38).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (42, 0),  15, 27)));
            Table.States (39).Action_List.Set_Capacity (6);
            Add_Action (Table.States (39), 18, (53, 0), 61);
            Add_Action (Table.States (39), 19, (52, 0), 62);
            Add_Action (Table.States (39), 20, (51, 0), 63);
            Add_Action (Table.States (39), 21, (47, 0), 64);
            Add_Action (Table.States (39), 33, (48, 1), 65);
            Add_Action (Table.States (39), 35, (50, 1), 66);
            Table.States (39).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (39), 47, 67);
            Add_Goto (Table.States (39), 48, 68);
            Add_Goto (Table.States (39), 49, 69);
            Add_Goto (Table.States (39), 50, 70);
            Add_Goto (Table.States (39), 51, 71);
            Add_Goto (Table.States (39), 52, 72);
            Add_Goto (Table.States (39), 53, 73);
            Add_Goto (Table.States (39), 54, 74);
            Table.States (39).Kernel := To_Vector ((((53, 0),  18,  2, (2147483647, 0),  0), ((53, 1),  18,  3,
            (2147483647, 0),  0)));
            Table.States (39).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 65)));
            Table.States (40).Action_List.Set_Capacity (6);
            Add_Action (Table.States (40), 18, (53, 0), 75);
            Add_Action (Table.States (40), 19, (52, 0), 76);
            Add_Action (Table.States (40), 20, (51, 0), 77);
            Add_Action (Table.States (40), 21, (47, 0), 78);
            Add_Action (Table.States (40), 33, (48, 1), 79);
            Add_Action (Table.States (40), 35, (50, 1), 80);
            Table.States (40).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (40), 47, 81);
            Add_Goto (Table.States (40), 48, 82);
            Add_Goto (Table.States (40), 49, 83);
            Add_Goto (Table.States (40), 50, 84);
            Add_Goto (Table.States (40), 51, 85);
            Add_Goto (Table.States (40), 52, 86);
            Add_Goto (Table.States (40), 53, 87);
            Add_Goto (Table.States (40), 54, 88);
            Table.States (40).Kernel := To_Vector ((0 => ((52, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (40).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 79)));
            Table.States (41).Action_List.Set_Capacity (6);
            Add_Action (Table.States (41), 18, (53, 0), 89);
            Add_Action (Table.States (41), 19, (52, 0), 90);
            Add_Action (Table.States (41), 20, (51, 0), 91);
            Add_Action (Table.States (41), 21, (47, 0), 92);
            Add_Action (Table.States (41), 33, (48, 1), 93);
            Add_Action (Table.States (41), 35, (50, 1), 94);
            Table.States (41).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (41), 47, 95);
            Add_Goto (Table.States (41), 48, 96);
            Add_Goto (Table.States (41), 49, 97);
            Add_Goto (Table.States (41), 50, 98);
            Add_Goto (Table.States (41), 51, 99);
            Add_Goto (Table.States (41), 52, 100);
            Add_Goto (Table.States (41), 53, 101);
            Add_Goto (Table.States (41), 54, 102);
            Table.States (41).Kernel := To_Vector ((((51, 0),  20,  2, (2147483647, 0),  0), ((52, 1),  20,  3,
            (2147483647, 0),  0), ((53, 2),  20,  3, (2147483647, 0),  0), ((53, 3),  20,  3, (2147483647, 0),  0)));
            Table.States (41).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 93)));
            Table.States (42).Action_List.Set_Capacity (1);
            Add_Action (Table.States (42), 33, (47, 0), 103);
            Table.States (42).Kernel := To_Vector ((0 => ((47, 0),  21,  4, (2147483647, 0),  0)));
            Table.States (42).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 103)));
            Table.States (43).Action_List.Set_Capacity (15);
            Add_Action (Table.States (43), 11, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 16, (48, 1), 104);
            Add_Action (Table.States (43), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 23, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 24, (53, 4), 105);
            Add_Action (Table.States (43), 25, (52, 2), 106);
            Add_Action (Table.States (43), 29, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 31, (53, 5), 107);
            Add_Action (Table.States (43), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 35, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (43), 36, Reduce, (50, 0),  1, null, null);
            Table.States (43).Kernel := To_Vector ((((48, 1),  33,  2, (2147483647, 0),  0), ((50, 0),  33,  0, (50,
            0),  1), ((52, 2),  33,  1, (2147483647, 0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,
            1, (2147483647, 0),  0)));
            Table.States (43).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            106), (Shift, (53, 4),  24, 105), (Shift, (53, 5),  31, 107)));
            Table.States (44).Action_List.Set_Capacity (12);
            Add_Action (Table.States (44), 11, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 12, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 18, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 19, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 20, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 21, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 23, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 25, (52, 3), 108);
            Add_Action (Table.States (44), 29, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 33, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 35, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (44), 36, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Table.States (44).Kernel := To_Vector ((((50, 1),  35,  0, (50, 1),  1), ((52, 3),  35,  1, (2147483647,
            0),  0)));
            Table.States (44).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 1),  1), (Shift, (52, 3),  25,
            108)));
            Table.States (45).Action_List.Set_Capacity (5);
            Add_Action (Table.States (45), 12, (45, 1), 109);
            Add_Action (Table.States (45), 23, (45, 2), 110);
            Add_Conflict (Table.States (45), 23, (44, 1),  0, null, null);
            Add_Action (Table.States (45), 29, (44, 0), 111);
            Add_Action (Table.States (45), 33, Reduce, (44, 1),  0, null, null);
            Add_Action (Table.States (45), 36, Reduce, (44, 1),  0, null, null);
            Table.States (45).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (45), 44, 112);
            Table.States (45).Kernel := To_Vector ((((43, 0),  45,  0, (44, 1),  0), ((45, 1),  45,  1, (2147483647,
            0),  0), ((45, 2),  45,  5, (2147483647, 0),  0), ((45, 3),  45,  3, (2147483647, 0),  0)));
            Table.States (45).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 1),  0)));
            Table.States (46).Action_List.Set_Capacity (5);
            Add_Action (Table.States (46), (12, 23, 29, 33, 36), (45, 0),  1, rhs_list_0'Access, null);
            Table.States (46).Kernel := To_Vector ((0 => ((45, 0),  46,  0, (45, 0),  1)));
            Table.States (46).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 0),  1)));
            Table.States (47).Action_List.Set_Capacity (11);
            Add_Action (Table.States (47), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (50, 2),  1, null, null);
            Table.States (47).Kernel := To_Vector ((0 => ((50, 2),  47,  0, (50, 2),  1)));
            Table.States (47).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 2),  1)));
            Table.States (48).Action_List.Set_Capacity (11);
            Add_Action (Table.States (48), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (49, 0),  1, null, null);
            Table.States (48).Kernel := To_Vector ((0 => ((49, 0),  48,  0, (49, 0),  1)));
            Table.States (48).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 0),  1)));
            Table.States (49).Action_List.Set_Capacity (11);
            Add_Action (Table.States (49), 11, (46, 2), 113);
            Add_Action (Table.States (49), 12, Reduce, (46, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (49), 18, (53, 0), 39);
            Add_Action (Table.States (49), 19, (52, 0), 40);
            Add_Action (Table.States (49), 20, (51, 0), 41);
            Add_Action (Table.States (49), 21, (47, 0), 42);
            Add_Action (Table.States (49), 23, Reduce, (46, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (49), 29, Reduce, (46, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (49), 33, (48, 1), 43);
            Add_Conflict (Table.States (49), 33, (46, 1),  1, rhs_1'Access, null);
            Add_Action (Table.States (49), 35, (50, 1), 44);
            Add_Action (Table.States (49), 36, Reduce, (46, 1),  1, rhs_1'Access, null);
            Table.States (49).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (49), 47, 47);
            Add_Goto (Table.States (49), 48, 114);
            Add_Goto (Table.States (49), 50, 50);
            Add_Goto (Table.States (49), 51, 51);
            Add_Goto (Table.States (49), 52, 52);
            Add_Goto (Table.States (49), 53, 53);
            Table.States (49).Kernel := To_Vector ((((46, 1),  49,  0, (46, 1),  1), ((46, 2),  49,  1, (2147483647,
            0),  0), ((46, 3),  49,  2, (2147483647, 0),  0), ((49, 1),  49,  1, (2147483647, 0),  0)));
            Table.States (49).Minimal_Complete_Actions := To_Vector (((Reduce, (46, 1),  1), (Shift, (46, 2),  11,
            113)));
            Table.States (50).Action_List.Set_Capacity (11);
            Add_Action (Table.States (50), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (48, 0),  1, null, null);
            Table.States (50).Kernel := To_Vector ((0 => ((48, 0),  50,  0, (48, 0),  1)));
            Table.States (50).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 0),  1)));
            Table.States (51).Action_List.Set_Capacity (11);
            Add_Action (Table.States (51), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (50, 5),  1, null, null);
            Table.States (51).Kernel := To_Vector ((0 => ((50, 5),  51,  0, (50, 5),  1)));
            Table.States (51).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 5),  1)));
            Table.States (52).Action_List.Set_Capacity (11);
            Add_Action (Table.States (52), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (50, 3),  1, null, null);
            Table.States (52).Kernel := To_Vector ((0 => ((50, 3),  52,  0, (50, 3),  1)));
            Table.States (52).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 3),  1)));
            Table.States (53).Action_List.Set_Capacity (11);
            Add_Action (Table.States (53), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (50, 4),  1, null, null);
            Table.States (53).Kernel := To_Vector ((0 => ((50, 4),  53,  0, (50, 4),  1)));
            Table.States (53).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 4),  1)));
            Table.States (54).Action_List.Set_Capacity (3);
            Add_Action (Table.States (54), (23, 33, 36), (38, 1),  4, declaration_1'Access, null);
            Table.States (54).Kernel := To_Vector ((0 => ((38, 1),  9,  0, (38, 1),  4)));
            Table.States (54).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (38, 1),  4)));
            Table.States (55).Action_List.Set_Capacity (2);
            Add_Action (Table.States (55), (9, 33), (40, 1),  2, null, null);
            Table.States (55).Kernel := To_Vector ((0 => ((40, 1),  33,  0, (40, 1),  2)));
            Table.States (55).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (40, 1),  2)));
            Table.States (56).Action_List.Set_Capacity (1);
            Add_Action (Table.States (56), 33, (38, 4), 115);
            Table.States (56).Kernel := To_Vector ((0 => ((38, 4),  16,  1, (2147483647, 0),  0)));
            Table.States (56).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (38, 4),  33, 115)));
            Table.States (57).Action_List.Set_Capacity (1);
            Add_Action (Table.States (57), 17, (39, 1), 116);
            Table.States (57).Kernel := To_Vector ((0 => ((39, 1),  33,  1, (2147483647, 0),  0)));
            Table.States (57).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (39, 1),  17, 116)));
            Table.States (58).Action_List.Set_Capacity (1);
            Add_Action (Table.States (58), 17, (39, 2), 117);
            Table.States (58).Kernel := To_Vector ((0 => ((39, 2),  33,  1, (2147483647, 0),  0)));
            Table.States (58).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (39, 2),  17, 117)));
            Table.States (59).Action_List.Set_Capacity (13);
            Add_Action (Table.States (59), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (41, 1),  2, null,
            null);
            Table.States (59).Kernel := To_Vector ((0 => ((41, 1),  42,  0, (41, 1),  2)));
            Table.States (59).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (41, 1),  2)));
            Table.States (60).Action_List.Set_Capacity (13);
            Add_Action (Table.States (60), 8, (42, 10), 25);
            Add_Action (Table.States (60), 10, (42, 5), 26);
            Add_Action (Table.States (60), 15, (42, 0), 27);
            Add_Action (Table.States (60), 16, (42, 2), 28);
            Add_Action (Table.States (60), 20, (42, 3), 29);
            Add_Action (Table.States (60), 23, Reduce, (38, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (60), 28, (42, 6), 30);
            Add_Action (Table.States (60), 30, (42, 7), 31);
            Add_Action (Table.States (60), 32, (42, 4), 32);
            Add_Action (Table.States (60), 33, (42, 1), 33);
            Add_Conflict (Table.States (60), 33, (38, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (60), 34, (42, 8), 34);
            Add_Action (Table.States (60), 35, (42, 9), 35);
            Add_Action (Table.States (60), 36, Reduce, (38, 0),  4, declaration_0'Access, null);
            Table.States (60).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (60), 42, 59);
            Table.States (60).Kernel := To_Vector ((((38, 0),  41,  0, (38, 0),  4), ((41, 1),  41,  1, (2147483647,
            0),  0)));
            Table.States (60).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (38, 0),  4)));
            Table.States (61).Action_List.Set_Capacity (6);
            Add_Action (Table.States (61), 18, (53, 0), 61);
            Add_Action (Table.States (61), 19, (52, 0), 62);
            Add_Action (Table.States (61), 20, (51, 0), 63);
            Add_Action (Table.States (61), 21, (47, 0), 64);
            Add_Action (Table.States (61), 33, (48, 1), 65);
            Add_Action (Table.States (61), 35, (50, 1), 66);
            Table.States (61).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (61), 47, 67);
            Add_Goto (Table.States (61), 48, 68);
            Add_Goto (Table.States (61), 49, 69);
            Add_Goto (Table.States (61), 50, 70);
            Add_Goto (Table.States (61), 51, 71);
            Add_Goto (Table.States (61), 52, 72);
            Add_Goto (Table.States (61), 53, 73);
            Add_Goto (Table.States (61), 54, 118);
            Table.States (61).Kernel := To_Vector ((((53, 0),  18,  2, (2147483647, 0),  0), ((53, 1),  18,  3,
            (2147483647, 0),  0)));
            Table.States (61).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 65)));
            Table.States (62).Action_List.Set_Capacity (6);
            Add_Action (Table.States (62), 18, (53, 0), 75);
            Add_Action (Table.States (62), 19, (52, 0), 76);
            Add_Action (Table.States (62), 20, (51, 0), 77);
            Add_Action (Table.States (62), 21, (47, 0), 78);
            Add_Action (Table.States (62), 33, (48, 1), 79);
            Add_Action (Table.States (62), 35, (50, 1), 80);
            Table.States (62).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (62), 47, 81);
            Add_Goto (Table.States (62), 48, 82);
            Add_Goto (Table.States (62), 49, 83);
            Add_Goto (Table.States (62), 50, 84);
            Add_Goto (Table.States (62), 51, 85);
            Add_Goto (Table.States (62), 52, 86);
            Add_Goto (Table.States (62), 53, 87);
            Add_Goto (Table.States (62), 54, 119);
            Table.States (62).Kernel := To_Vector ((0 => ((52, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (62).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 79)));
            Table.States (63).Action_List.Set_Capacity (6);
            Add_Action (Table.States (63), 18, (53, 0), 89);
            Add_Action (Table.States (63), 19, (52, 0), 90);
            Add_Action (Table.States (63), 20, (51, 0), 91);
            Add_Action (Table.States (63), 21, (47, 0), 92);
            Add_Action (Table.States (63), 33, (48, 1), 93);
            Add_Action (Table.States (63), 35, (50, 1), 94);
            Table.States (63).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (63), 47, 95);
            Add_Goto (Table.States (63), 48, 96);
            Add_Goto (Table.States (63), 49, 97);
            Add_Goto (Table.States (63), 50, 98);
            Add_Goto (Table.States (63), 51, 99);
            Add_Goto (Table.States (63), 52, 100);
            Add_Goto (Table.States (63), 53, 101);
            Add_Goto (Table.States (63), 54, 120);
            Table.States (63).Kernel := To_Vector ((((51, 0),  20,  2, (2147483647, 0),  0), ((52, 1),  20,  3,
            (2147483647, 0),  0), ((53, 2),  20,  3, (2147483647, 0),  0), ((53, 3),  20,  3, (2147483647, 0),  0)));
            Table.States (63).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 93)));
            Table.States (64).Action_List.Set_Capacity (1);
            Add_Action (Table.States (64), 33, (47, 0), 121);
            Table.States (64).Kernel := To_Vector ((0 => ((47, 0),  21,  4, (2147483647, 0),  0)));
            Table.States (64).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 121)));
            Table.States (65).Action_List.Set_Capacity (12);
            Add_Action (Table.States (65), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (65), 16, (48, 1), 122);
            Add_Action (Table.States (65), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (65), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (65), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (65), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (65), 24, (53, 4), 123);
            Add_Action (Table.States (65), 25, (52, 2), 124);
            Add_Action (Table.States (65), 26, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (65), 31, (53, 5), 125);
            Add_Action (Table.States (65), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (65), 35, Reduce, (50, 0),  1, null, null);
            Table.States (65).Kernel := To_Vector ((((48, 1),  33,  2, (2147483647, 0),  0), ((50, 0),  33,  0, (50,
            0),  1), ((52, 2),  33,  1, (2147483647, 0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,
            1, (2147483647, 0),  0)));
            Table.States (65).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            124), (Shift, (53, 4),  24, 123), (Shift, (53, 5),  31, 125)));
            Table.States (66).Action_List.Set_Capacity (9);
            Add_Action (Table.States (66), 12, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (66), 18, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (66), 19, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (66), 20, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (66), 21, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (66), 25, (52, 3), 126);
            Add_Action (Table.States (66), 26, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (66), 33, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (66), 35, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Table.States (66).Kernel := To_Vector ((((50, 1),  35,  0, (50, 1),  1), ((52, 3),  35,  1, (2147483647,
            0),  0)));
            Table.States (66).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 1),  1), (Shift, (52, 3),  25,
            126)));
            Table.States (67).Action_List.Set_Capacity (8);
            Add_Action (Table.States (67), (12, 18, 19, 20, 21, 26, 33, 35), (50, 2),  1, null, null);
            Table.States (67).Kernel := To_Vector ((0 => ((50, 2),  47,  0, (50, 2),  1)));
            Table.States (67).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 2),  1)));
            Table.States (68).Action_List.Set_Capacity (8);
            Add_Action (Table.States (68), (12, 18, 19, 20, 21, 26, 33, 35), (49, 0),  1, null, null);
            Table.States (68).Kernel := To_Vector ((0 => ((49, 0),  48,  0, (49, 0),  1)));
            Table.States (68).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 0),  1)));
            Table.States (69).Action_List.Set_Capacity (8);
            Add_Action (Table.States (69), 12, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (69), 18, (53, 0), 61);
            Add_Action (Table.States (69), 19, (52, 0), 62);
            Add_Action (Table.States (69), 20, (51, 0), 63);
            Add_Action (Table.States (69), 21, (47, 0), 64);
            Add_Action (Table.States (69), 26, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (69), 33, (48, 1), 65);
            Add_Action (Table.States (69), 35, (50, 1), 66);
            Table.States (69).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (69), 47, 67);
            Add_Goto (Table.States (69), 48, 127);
            Add_Goto (Table.States (69), 50, 70);
            Add_Goto (Table.States (69), 51, 71);
            Add_Goto (Table.States (69), 52, 72);
            Add_Goto (Table.States (69), 53, 73);
            Table.States (69).Kernel := To_Vector ((((49, 1),  49,  1, (2147483647, 0),  0), ((54, 0),  49,  0, (54,
            0),  1)));
            Table.States (69).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 0),  1)));
            Table.States (70).Action_List.Set_Capacity (8);
            Add_Action (Table.States (70), (12, 18, 19, 20, 21, 26, 33, 35), (48, 0),  1, null, null);
            Table.States (70).Kernel := To_Vector ((0 => ((48, 0),  50,  0, (48, 0),  1)));
            Table.States (70).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 0),  1)));
            Table.States (71).Action_List.Set_Capacity (8);
            Add_Action (Table.States (71), (12, 18, 19, 20, 21, 26, 33, 35), (50, 5),  1, null, null);
            Table.States (71).Kernel := To_Vector ((0 => ((50, 5),  51,  0, (50, 5),  1)));
            Table.States (71).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 5),  1)));
            Table.States (72).Action_List.Set_Capacity (8);
            Add_Action (Table.States (72), (12, 18, 19, 20, 21, 26, 33, 35), (50, 3),  1, null, null);
            Table.States (72).Kernel := To_Vector ((0 => ((50, 3),  52,  0, (50, 3),  1)));
            Table.States (72).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 3),  1)));
            Table.States (73).Action_List.Set_Capacity (8);
            Add_Action (Table.States (73), (12, 18, 19, 20, 21, 26, 33, 35), (50, 4),  1, null, null);
            Table.States (73).Kernel := To_Vector ((0 => ((50, 4),  53,  0, (50, 4),  1)));
            Table.States (73).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 4),  1)));
            Table.States (74).Action_List.Set_Capacity (2);
            Add_Action (Table.States (74), 12, (54, 1), 128);
            Add_Action (Table.States (74), 26, (53, 0), 129);
            Table.States (74).Kernel := To_Vector ((((53, 0),  54,  1, (2147483647, 0),  0), ((53, 1),  54,  2,
            (2147483647, 0),  0), ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (74).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  26, 129)));
            Table.States (75).Action_List.Set_Capacity (6);
            Add_Action (Table.States (75), 18, (53, 0), 61);
            Add_Action (Table.States (75), 19, (52, 0), 62);
            Add_Action (Table.States (75), 20, (51, 0), 63);
            Add_Action (Table.States (75), 21, (47, 0), 64);
            Add_Action (Table.States (75), 33, (48, 1), 65);
            Add_Action (Table.States (75), 35, (50, 1), 66);
            Table.States (75).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (75), 47, 67);
            Add_Goto (Table.States (75), 48, 68);
            Add_Goto (Table.States (75), 49, 69);
            Add_Goto (Table.States (75), 50, 70);
            Add_Goto (Table.States (75), 51, 71);
            Add_Goto (Table.States (75), 52, 72);
            Add_Goto (Table.States (75), 53, 73);
            Add_Goto (Table.States (75), 54, 130);
            Table.States (75).Kernel := To_Vector ((((53, 0),  18,  2, (2147483647, 0),  0), ((53, 1),  18,  3,
            (2147483647, 0),  0)));
            Table.States (75).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 65)));
            Table.States (76).Action_List.Set_Capacity (6);
            Add_Action (Table.States (76), 18, (53, 0), 75);
            Add_Action (Table.States (76), 19, (52, 0), 76);
            Add_Action (Table.States (76), 20, (51, 0), 77);
            Add_Action (Table.States (76), 21, (47, 0), 78);
            Add_Action (Table.States (76), 33, (48, 1), 79);
            Add_Action (Table.States (76), 35, (50, 1), 80);
            Table.States (76).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (76), 47, 81);
            Add_Goto (Table.States (76), 48, 82);
            Add_Goto (Table.States (76), 49, 83);
            Add_Goto (Table.States (76), 50, 84);
            Add_Goto (Table.States (76), 51, 85);
            Add_Goto (Table.States (76), 52, 86);
            Add_Goto (Table.States (76), 53, 87);
            Add_Goto (Table.States (76), 54, 131);
            Table.States (76).Kernel := To_Vector ((0 => ((52, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (76).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 79)));
            Table.States (77).Action_List.Set_Capacity (6);
            Add_Action (Table.States (77), 18, (53, 0), 89);
            Add_Action (Table.States (77), 19, (52, 0), 90);
            Add_Action (Table.States (77), 20, (51, 0), 91);
            Add_Action (Table.States (77), 21, (47, 0), 92);
            Add_Action (Table.States (77), 33, (48, 1), 93);
            Add_Action (Table.States (77), 35, (50, 1), 94);
            Table.States (77).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (77), 47, 95);
            Add_Goto (Table.States (77), 48, 96);
            Add_Goto (Table.States (77), 49, 97);
            Add_Goto (Table.States (77), 50, 98);
            Add_Goto (Table.States (77), 51, 99);
            Add_Goto (Table.States (77), 52, 100);
            Add_Goto (Table.States (77), 53, 101);
            Add_Goto (Table.States (77), 54, 132);
            Table.States (77).Kernel := To_Vector ((((51, 0),  20,  2, (2147483647, 0),  0), ((52, 1),  20,  3,
            (2147483647, 0),  0), ((53, 2),  20,  3, (2147483647, 0),  0), ((53, 3),  20,  3, (2147483647, 0),  0)));
            Table.States (77).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 93)));
            Table.States (78).Action_List.Set_Capacity (1);
            Add_Action (Table.States (78), 33, (47, 0), 133);
            Table.States (78).Kernel := To_Vector ((0 => ((47, 0),  21,  4, (2147483647, 0),  0)));
            Table.States (78).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 133)));
            Table.States (79).Action_List.Set_Capacity (12);
            Add_Action (Table.States (79), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (79), 16, (48, 1), 134);
            Add_Action (Table.States (79), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (79), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (79), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (79), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (79), 24, (53, 4), 135);
            Add_Action (Table.States (79), 25, (52, 2), 136);
            Add_Action (Table.States (79), 27, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (79), 31, (53, 5), 137);
            Add_Action (Table.States (79), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (79), 35, Reduce, (50, 0),  1, null, null);
            Table.States (79).Kernel := To_Vector ((((48, 1),  33,  2, (2147483647, 0),  0), ((50, 0),  33,  0, (50,
            0),  1), ((52, 2),  33,  1, (2147483647, 0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,
            1, (2147483647, 0),  0)));
            Table.States (79).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            136), (Shift, (53, 4),  24, 135), (Shift, (53, 5),  31, 137)));
            Table.States (80).Action_List.Set_Capacity (9);
            Add_Action (Table.States (80), 12, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (80), 18, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (80), 19, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (80), 20, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (80), 21, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (80), 25, (52, 3), 138);
            Add_Action (Table.States (80), 27, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (80), 33, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (80), 35, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Table.States (80).Kernel := To_Vector ((((50, 1),  35,  0, (50, 1),  1), ((52, 3),  35,  1, (2147483647,
            0),  0)));
            Table.States (80).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 1),  1), (Shift, (52, 3),  25,
            138)));
            Table.States (81).Action_List.Set_Capacity (8);
            Add_Action (Table.States (81), (12, 18, 19, 20, 21, 27, 33, 35), (50, 2),  1, null, null);
            Table.States (81).Kernel := To_Vector ((0 => ((50, 2),  47,  0, (50, 2),  1)));
            Table.States (81).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 2),  1)));
            Table.States (82).Action_List.Set_Capacity (8);
            Add_Action (Table.States (82), (12, 18, 19, 20, 21, 27, 33, 35), (49, 0),  1, null, null);
            Table.States (82).Kernel := To_Vector ((0 => ((49, 0),  48,  0, (49, 0),  1)));
            Table.States (82).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 0),  1)));
            Table.States (83).Action_List.Set_Capacity (8);
            Add_Action (Table.States (83), 12, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (83), 18, (53, 0), 75);
            Add_Action (Table.States (83), 19, (52, 0), 76);
            Add_Action (Table.States (83), 20, (51, 0), 77);
            Add_Action (Table.States (83), 21, (47, 0), 78);
            Add_Action (Table.States (83), 27, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (83), 33, (48, 1), 79);
            Add_Action (Table.States (83), 35, (50, 1), 80);
            Table.States (83).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (83), 47, 81);
            Add_Goto (Table.States (83), 48, 139);
            Add_Goto (Table.States (83), 50, 84);
            Add_Goto (Table.States (83), 51, 85);
            Add_Goto (Table.States (83), 52, 86);
            Add_Goto (Table.States (83), 53, 87);
            Table.States (83).Kernel := To_Vector ((((49, 1),  49,  1, (2147483647, 0),  0), ((54, 0),  49,  0, (54,
            0),  1)));
            Table.States (83).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 0),  1)));
         end Subr_1;
         procedure Subr_2
         is begin
            Table.States (84).Action_List.Set_Capacity (8);
            Add_Action (Table.States (84), (12, 18, 19, 20, 21, 27, 33, 35), (48, 0),  1, null, null);
            Table.States (84).Kernel := To_Vector ((0 => ((48, 0),  50,  0, (48, 0),  1)));
            Table.States (84).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 0),  1)));
            Table.States (85).Action_List.Set_Capacity (8);
            Add_Action (Table.States (85), (12, 18, 19, 20, 21, 27, 33, 35), (50, 5),  1, null, null);
            Table.States (85).Kernel := To_Vector ((0 => ((50, 5),  51,  0, (50, 5),  1)));
            Table.States (85).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 5),  1)));
            Table.States (86).Action_List.Set_Capacity (8);
            Add_Action (Table.States (86), (12, 18, 19, 20, 21, 27, 33, 35), (50, 3),  1, null, null);
            Table.States (86).Kernel := To_Vector ((0 => ((50, 3),  52,  0, (50, 3),  1)));
            Table.States (86).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 3),  1)));
            Table.States (87).Action_List.Set_Capacity (8);
            Add_Action (Table.States (87), (12, 18, 19, 20, 21, 27, 33, 35), (50, 4),  1, null, null);
            Table.States (87).Kernel := To_Vector ((0 => ((50, 4),  53,  0, (50, 4),  1)));
            Table.States (87).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 4),  1)));
            Table.States (88).Action_List.Set_Capacity (2);
            Add_Action (Table.States (88), 12, (54, 1), 140);
            Add_Action (Table.States (88), 27, (52, 0), 141);
            Table.States (88).Kernel := To_Vector ((((52, 0),  54,  1, (2147483647, 0),  0), ((54, 1),  54,  2,
            (2147483647, 0),  0)));
            Table.States (88).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 0),  27, 141)));
            Table.States (89).Action_List.Set_Capacity (6);
            Add_Action (Table.States (89), 18, (53, 0), 61);
            Add_Action (Table.States (89), 19, (52, 0), 62);
            Add_Action (Table.States (89), 20, (51, 0), 63);
            Add_Action (Table.States (89), 21, (47, 0), 64);
            Add_Action (Table.States (89), 33, (48, 1), 65);
            Add_Action (Table.States (89), 35, (50, 1), 66);
            Table.States (89).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (89), 47, 67);
            Add_Goto (Table.States (89), 48, 68);
            Add_Goto (Table.States (89), 49, 69);
            Add_Goto (Table.States (89), 50, 70);
            Add_Goto (Table.States (89), 51, 71);
            Add_Goto (Table.States (89), 52, 72);
            Add_Goto (Table.States (89), 53, 73);
            Add_Goto (Table.States (89), 54, 142);
            Table.States (89).Kernel := To_Vector ((((53, 0),  18,  2, (2147483647, 0),  0), ((53, 1),  18,  3,
            (2147483647, 0),  0)));
            Table.States (89).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 65)));
            Table.States (90).Action_List.Set_Capacity (6);
            Add_Action (Table.States (90), 18, (53, 0), 75);
            Add_Action (Table.States (90), 19, (52, 0), 76);
            Add_Action (Table.States (90), 20, (51, 0), 77);
            Add_Action (Table.States (90), 21, (47, 0), 78);
            Add_Action (Table.States (90), 33, (48, 1), 79);
            Add_Action (Table.States (90), 35, (50, 1), 80);
            Table.States (90).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (90), 47, 81);
            Add_Goto (Table.States (90), 48, 82);
            Add_Goto (Table.States (90), 49, 83);
            Add_Goto (Table.States (90), 50, 84);
            Add_Goto (Table.States (90), 51, 85);
            Add_Goto (Table.States (90), 52, 86);
            Add_Goto (Table.States (90), 53, 87);
            Add_Goto (Table.States (90), 54, 143);
            Table.States (90).Kernel := To_Vector ((0 => ((52, 0),  19,  2, (2147483647, 0),  0)));
            Table.States (90).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 79)));
            Table.States (91).Action_List.Set_Capacity (6);
            Add_Action (Table.States (91), 18, (53, 0), 89);
            Add_Action (Table.States (91), 19, (52, 0), 90);
            Add_Action (Table.States (91), 20, (51, 0), 91);
            Add_Action (Table.States (91), 21, (47, 0), 92);
            Add_Action (Table.States (91), 33, (48, 1), 93);
            Add_Action (Table.States (91), 35, (50, 1), 94);
            Table.States (91).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (91), 47, 95);
            Add_Goto (Table.States (91), 48, 96);
            Add_Goto (Table.States (91), 49, 97);
            Add_Goto (Table.States (91), 50, 98);
            Add_Goto (Table.States (91), 51, 99);
            Add_Goto (Table.States (91), 52, 100);
            Add_Goto (Table.States (91), 53, 101);
            Add_Goto (Table.States (91), 54, 144);
            Table.States (91).Kernel := To_Vector ((((51, 0),  20,  2, (2147483647, 0),  0), ((52, 1),  20,  3,
            (2147483647, 0),  0), ((53, 2),  20,  3, (2147483647, 0),  0), ((53, 3),  20,  3, (2147483647, 0),  0)));
            Table.States (91).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 93)));
            Table.States (92).Action_List.Set_Capacity (1);
            Add_Action (Table.States (92), 33, (47, 0), 145);
            Table.States (92).Kernel := To_Vector ((0 => ((47, 0),  21,  4, (2147483647, 0),  0)));
            Table.States (92).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 145)));
            Table.States (93).Action_List.Set_Capacity (12);
            Add_Action (Table.States (93), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (93), 16, (48, 1), 146);
            Add_Action (Table.States (93), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (93), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (93), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (93), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (93), 24, (53, 4), 147);
            Add_Action (Table.States (93), 25, (52, 2), 148);
            Add_Action (Table.States (93), 28, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (93), 31, (53, 5), 149);
            Add_Action (Table.States (93), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (93), 35, Reduce, (50, 0),  1, null, null);
            Table.States (93).Kernel := To_Vector ((((48, 1),  33,  2, (2147483647, 0),  0), ((50, 0),  33,  0, (50,
            0),  1), ((52, 2),  33,  1, (2147483647, 0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,
            1, (2147483647, 0),  0)));
            Table.States (93).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            148), (Shift, (53, 4),  24, 147), (Shift, (53, 5),  31, 149)));
            Table.States (94).Action_List.Set_Capacity (9);
            Add_Action (Table.States (94), 12, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (94), 18, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (94), 19, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (94), 20, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (94), 21, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (94), 25, (52, 3), 150);
            Add_Action (Table.States (94), 28, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (94), 33, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (94), 35, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Table.States (94).Kernel := To_Vector ((((50, 1),  35,  0, (50, 1),  1), ((52, 3),  35,  1, (2147483647,
            0),  0)));
            Table.States (94).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 1),  1), (Shift, (52, 3),  25,
            150)));
            Table.States (95).Action_List.Set_Capacity (8);
            Add_Action (Table.States (95), (12, 18, 19, 20, 21, 28, 33, 35), (50, 2),  1, null, null);
            Table.States (95).Kernel := To_Vector ((0 => ((50, 2),  47,  0, (50, 2),  1)));
            Table.States (95).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 2),  1)));
            Table.States (96).Action_List.Set_Capacity (8);
            Add_Action (Table.States (96), (12, 18, 19, 20, 21, 28, 33, 35), (49, 0),  1, null, null);
            Table.States (96).Kernel := To_Vector ((0 => ((49, 0),  48,  0, (49, 0),  1)));
            Table.States (96).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 0),  1)));
            Table.States (97).Action_List.Set_Capacity (8);
            Add_Action (Table.States (97), 12, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (97), 18, (53, 0), 89);
            Add_Action (Table.States (97), 19, (52, 0), 90);
            Add_Action (Table.States (97), 20, (51, 0), 91);
            Add_Action (Table.States (97), 21, (47, 0), 92);
            Add_Action (Table.States (97), 28, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (97), 33, (48, 1), 93);
            Add_Action (Table.States (97), 35, (50, 1), 94);
            Table.States (97).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (97), 47, 95);
            Add_Goto (Table.States (97), 48, 151);
            Add_Goto (Table.States (97), 50, 98);
            Add_Goto (Table.States (97), 51, 99);
            Add_Goto (Table.States (97), 52, 100);
            Add_Goto (Table.States (97), 53, 101);
            Table.States (97).Kernel := To_Vector ((((49, 1),  49,  1, (2147483647, 0),  0), ((54, 0),  49,  0, (54,
            0),  1)));
            Table.States (97).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 0),  1)));
            Table.States (98).Action_List.Set_Capacity (8);
            Add_Action (Table.States (98), (12, 18, 19, 20, 21, 28, 33, 35), (48, 0),  1, null, null);
            Table.States (98).Kernel := To_Vector ((0 => ((48, 0),  50,  0, (48, 0),  1)));
            Table.States (98).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 0),  1)));
            Table.States (99).Action_List.Set_Capacity (8);
            Add_Action (Table.States (99), (12, 18, 19, 20, 21, 28, 33, 35), (50, 5),  1, null, null);
            Table.States (99).Kernel := To_Vector ((0 => ((50, 5),  51,  0, (50, 5),  1)));
            Table.States (99).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 5),  1)));
            Table.States (100).Action_List.Set_Capacity (8);
            Add_Action (Table.States (100), (12, 18, 19, 20, 21, 28, 33, 35), (50, 3),  1, null, null);
            Table.States (100).Kernel := To_Vector ((0 => ((50, 3),  52,  0, (50, 3),  1)));
            Table.States (100).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 3),  1)));
            Table.States (101).Action_List.Set_Capacity (8);
            Add_Action (Table.States (101), (12, 18, 19, 20, 21, 28, 33, 35), (50, 4),  1, null, null);
            Table.States (101).Kernel := To_Vector ((0 => ((50, 4),  53,  0, (50, 4),  1)));
            Table.States (101).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (50, 4),  1)));
            Table.States (102).Action_List.Set_Capacity (2);
            Add_Action (Table.States (102), 12, (54, 1), 152);
            Add_Action (Table.States (102), 28, (51, 0), 153);
            Table.States (102).Kernel := To_Vector ((((51, 0),  54,  1, (2147483647, 0),  0), ((52, 1),  54,  2,
            (2147483647, 0),  0), ((53, 2),  54,  2, (2147483647, 0),  0), ((53, 3),  54,  2, (2147483647, 0),  0),
            ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (102).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  28, 153)));
            Table.States (103).Action_List.Set_Capacity (1);
            Add_Action (Table.States (103), 16, (47, 0), 154);
            Table.States (103).Kernel := To_Vector ((0 => ((47, 0),  33,  3, (2147483647, 0),  0)));
            Table.States (103).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  16, 154)));
            Table.States (104).Action_List.Set_Capacity (6);
            Add_Action (Table.States (104), 18, (53, 0), 39);
            Add_Action (Table.States (104), 19, (52, 0), 40);
            Add_Action (Table.States (104), 20, (51, 0), 41);
            Add_Action (Table.States (104), 21, (47, 0), 42);
            Add_Action (Table.States (104), 33, (50, 0), 155);
            Add_Action (Table.States (104), 35, (50, 1), 44);
            Table.States (104).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (104), 47, 47);
            Add_Goto (Table.States (104), 50, 156);
            Add_Goto (Table.States (104), 51, 51);
            Add_Goto (Table.States (104), 52, 52);
            Add_Goto (Table.States (104), 53, 53);
            Table.States (104).Kernel := To_Vector ((0 => ((48, 1),  16,  1, (2147483647, 0),  0)));
            Table.States (104).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  33, 155)));
            Table.States (105).Action_List.Set_Capacity (11);
            Add_Action (Table.States (105), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (53, 4),  2, null, null);
            Table.States (105).Kernel := To_Vector ((0 => ((53, 4),  24,  0, (53, 4),  2)));
            Table.States (105).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  2)));
            Table.States (106).Action_List.Set_Capacity (11);
            Add_Action (Table.States (106), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (52, 2),  2, null, null);
            Table.States (106).Kernel := To_Vector ((0 => ((52, 2),  25,  0, (52, 2),  2)));
            Table.States (106).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 2),  2)));
            Table.States (107).Action_List.Set_Capacity (11);
            Add_Action (Table.States (107), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (53, 5),  2, null, null);
            Table.States (107).Kernel := To_Vector ((0 => ((53, 5),  31,  0, (53, 5),  2)));
            Table.States (107).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  2)));
            Table.States (108).Action_List.Set_Capacity (11);
            Add_Action (Table.States (108), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (52, 3),  2,
            rhs_optional_item_3'Access, null);
            Table.States (108).Kernel := To_Vector ((0 => ((52, 3),  25,  0, (52, 3),  2)));
            Table.States (108).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 3),  2)));
            Table.States (109).Action_List.Set_Capacity (10);
            Add_Action (Table.States (109), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (109), 18, (53, 0), 39);
            Add_Action (Table.States (109), 19, (52, 0), 40);
            Add_Action (Table.States (109), 20, (51, 0), 41);
            Add_Action (Table.States (109), 21, (47, 0), 42);
            Add_Action (Table.States (109), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (109), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (109), 33, (48, 1), 43);
            Add_Conflict (Table.States (109), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (109), 35, (50, 1), 44);
            Add_Action (Table.States (109), 36, Reduce, (46, 0),  0, null, null);
            Table.States (109).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (109), 46, 157);
            Add_Goto (Table.States (109), 47, 47);
            Add_Goto (Table.States (109), 48, 48);
            Add_Goto (Table.States (109), 49, 49);
            Add_Goto (Table.States (109), 50, 50);
            Add_Goto (Table.States (109), 51, 51);
            Add_Goto (Table.States (109), 52, 52);
            Add_Goto (Table.States (109), 53, 53);
            Table.States (109).Kernel := To_Vector ((0 => ((45, 1),  12,  0, (46, 0),  0)));
            Table.States (109).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 0),  0)));
            Table.States (110).Action_List.Set_Capacity (2);
            Add_Action (Table.States (110), 4, (45, 3), 158);
            Add_Action (Table.States (110), 5, (45, 2), 159);
            Table.States (110).Kernel := To_Vector ((((45, 2),  23,  4, (2147483647, 0),  0), ((45, 3),  23,  2,
            (2147483647, 0),  0)));
            Table.States (111).Action_List.Set_Capacity (3);
            Add_Action (Table.States (111), (23, 33, 36), (44, 0),  1, null, null);
            Table.States (111).Kernel := To_Vector ((0 => ((44, 0),  29,  0, (44, 0),  1)));
            Table.States (111).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (44, 0),  1)));
            Table.States (112).Action_List.Set_Capacity (3);
            Add_Action (Table.States (112), (23, 33, 36), (43, 0),  4, nonterminal_0'Access, null);
            Table.States (112).Kernel := To_Vector ((0 => ((43, 0),  44,  0, (43, 0),  4)));
            Table.States (112).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (43, 0),  4)));
            Table.States (113).Action_List.Set_Capacity (6);
            Add_Action (Table.States (113), 11, (46, 3), 160);
            Add_Action (Table.States (113), 12, Reduce, (46, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (113), 23, Reduce, (46, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (113), 29, Reduce, (46, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (113), 33, Reduce, (46, 2),  2, rhs_2'Access, null);
            Add_Action (Table.States (113), 36, Reduce, (46, 2),  2, rhs_2'Access, null);
            Table.States (113).Kernel := To_Vector ((((46, 2),  11,  0, (46, 2),  2), ((46, 3),  11,  1, (2147483647,
            0),  0)));
            Table.States (113).Minimal_Complete_Actions := To_Vector (((Reduce, (46, 2),  2), (Shift, (46, 3),  11,
            160)));
            Table.States (114).Action_List.Set_Capacity (11);
            Add_Action (Table.States (114), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (49, 1),  2, null, null);
            Table.States (114).Kernel := To_Vector ((0 => ((49, 1),  48,  0, (49, 1),  2)));
            Table.States (114).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 1),  2)));
            Table.States (115).Action_List.Set_Capacity (3);
            Add_Action (Table.States (115), (23, 33, 36), (38, 4),  5, declaration_4'Access, null);
            Table.States (115).Kernel := To_Vector ((0 => ((38, 4),  33,  0, (38, 4),  5)));
            Table.States (115).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (38, 4),  5)));
            Table.States (116).Action_List.Set_Capacity (1);
            Add_Action (Table.States (116), (1 =>  33), (39, 1),  4, token_keyword_non_grammar_1'Access, null);
            Table.States (116).Kernel := To_Vector ((0 => ((39, 1),  17,  0, (39, 1),  4)));
            Table.States (116).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (39, 1),  4)));
            Table.States (117).Action_List.Set_Capacity (1);
            Add_Action (Table.States (117), (1 =>  33), (39, 2),  4, token_keyword_non_grammar_2'Access, null);
            Table.States (117).Kernel := To_Vector ((0 => ((39, 2),  17,  0, (39, 2),  4)));
            Table.States (117).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (39, 2),  4)));
            Table.States (118).Action_List.Set_Capacity (2);
            Add_Action (Table.States (118), 12, (54, 1), 128);
            Add_Action (Table.States (118), 26, (53, 0), 161);
            Table.States (118).Kernel := To_Vector ((((53, 0),  54,  1, (2147483647, 0),  0), ((53, 1),  54,  2,
            (2147483647, 0),  0), ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (118).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  26, 161)));
            Table.States (119).Action_List.Set_Capacity (2);
            Add_Action (Table.States (119), 12, (54, 1), 140);
            Add_Action (Table.States (119), 27, (52, 0), 162);
            Table.States (119).Kernel := To_Vector ((((52, 0),  54,  1, (2147483647, 0),  0), ((54, 1),  54,  2,
            (2147483647, 0),  0)));
            Table.States (119).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 0),  27, 162)));
            Table.States (120).Action_List.Set_Capacity (2);
            Add_Action (Table.States (120), 12, (54, 1), 152);
            Add_Action (Table.States (120), 28, (51, 0), 163);
            Table.States (120).Kernel := To_Vector ((((51, 0),  54,  1, (2147483647, 0),  0), ((52, 1),  54,  2,
            (2147483647, 0),  0), ((53, 2),  54,  2, (2147483647, 0),  0), ((53, 3),  54,  2, (2147483647, 0),  0),
            ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (120).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  28, 163)));
            Table.States (121).Action_List.Set_Capacity (1);
            Add_Action (Table.States (121), 16, (47, 0), 164);
            Table.States (121).Kernel := To_Vector ((0 => ((47, 0),  33,  3, (2147483647, 0),  0)));
            Table.States (121).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  16, 164)));
            Table.States (122).Action_List.Set_Capacity (6);
            Add_Action (Table.States (122), 18, (53, 0), 61);
            Add_Action (Table.States (122), 19, (52, 0), 62);
            Add_Action (Table.States (122), 20, (51, 0), 63);
            Add_Action (Table.States (122), 21, (47, 0), 64);
            Add_Action (Table.States (122), 33, (50, 0), 165);
            Add_Action (Table.States (122), 35, (50, 1), 66);
            Table.States (122).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (122), 47, 67);
            Add_Goto (Table.States (122), 50, 166);
            Add_Goto (Table.States (122), 51, 71);
            Add_Goto (Table.States (122), 52, 72);
            Add_Goto (Table.States (122), 53, 73);
            Table.States (122).Kernel := To_Vector ((0 => ((48, 1),  16,  1, (2147483647, 0),  0)));
            Table.States (122).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  33, 165)));
            Table.States (123).Action_List.Set_Capacity (8);
            Add_Action (Table.States (123), (12, 18, 19, 20, 21, 26, 33, 35), (53, 4),  2, null, null);
            Table.States (123).Kernel := To_Vector ((0 => ((53, 4),  24,  0, (53, 4),  2)));
            Table.States (123).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  2)));
            Table.States (124).Action_List.Set_Capacity (8);
            Add_Action (Table.States (124), (12, 18, 19, 20, 21, 26, 33, 35), (52, 2),  2, null, null);
            Table.States (124).Kernel := To_Vector ((0 => ((52, 2),  25,  0, (52, 2),  2)));
            Table.States (124).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 2),  2)));
            Table.States (125).Action_List.Set_Capacity (8);
            Add_Action (Table.States (125), (12, 18, 19, 20, 21, 26, 33, 35), (53, 5),  2, null, null);
            Table.States (125).Kernel := To_Vector ((0 => ((53, 5),  31,  0, (53, 5),  2)));
            Table.States (125).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  2)));
            Table.States (126).Action_List.Set_Capacity (8);
            Add_Action (Table.States (126), (12, 18, 19, 20, 21, 26, 33, 35), (52, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (126).Kernel := To_Vector ((0 => ((52, 3),  25,  0, (52, 3),  2)));
            Table.States (126).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 3),  2)));
            Table.States (127).Action_List.Set_Capacity (8);
            Add_Action (Table.States (127), (12, 18, 19, 20, 21, 26, 33, 35), (49, 1),  2, null, null);
            Table.States (127).Kernel := To_Vector ((0 => ((49, 1),  48,  0, (49, 1),  2)));
            Table.States (127).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 1),  2)));
            Table.States (128).Action_List.Set_Capacity (6);
            Add_Action (Table.States (128), 18, (53, 0), 61);
            Add_Action (Table.States (128), 19, (52, 0), 62);
            Add_Action (Table.States (128), 20, (51, 0), 63);
            Add_Action (Table.States (128), 21, (47, 0), 64);
            Add_Action (Table.States (128), 33, (48, 1), 65);
            Add_Action (Table.States (128), 35, (50, 1), 66);
            Table.States (128).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (128), 47, 67);
            Add_Goto (Table.States (128), 48, 68);
            Add_Goto (Table.States (128), 49, 167);
            Add_Goto (Table.States (128), 50, 70);
            Add_Goto (Table.States (128), 51, 71);
            Add_Goto (Table.States (128), 52, 72);
            Add_Goto (Table.States (128), 53, 73);
            Table.States (128).Kernel := To_Vector ((0 => ((54, 1),  12,  1, (2147483647, 0),  0)));
            Table.States (128).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 65)));
            Table.States (129).Action_List.Set_Capacity (12);
            Add_Action (Table.States (129), 11, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 12, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 18, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 19, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 20, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 21, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 22, (53, 1), 168);
            Add_Action (Table.States (129), 23, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 29, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 33, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 35, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (129), 36, Reduce, (53, 0),  3, null, null);
            Table.States (129).Kernel := To_Vector ((((53, 0),  26,  0, (53, 0),  3), ((53, 1),  26,  1, (2147483647,
            0),  0)));
            Table.States (129).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  3), (Shift, (53, 1),  22,
            168)));
            Table.States (130).Action_List.Set_Capacity (2);
            Add_Action (Table.States (130), 12, (54, 1), 128);
            Add_Action (Table.States (130), 26, (53, 0), 169);
            Table.States (130).Kernel := To_Vector ((((53, 0),  54,  1, (2147483647, 0),  0), ((53, 1),  54,  2,
            (2147483647, 0),  0), ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (130).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  26, 169)));
            Table.States (131).Action_List.Set_Capacity (2);
            Add_Action (Table.States (131), 12, (54, 1), 140);
            Add_Action (Table.States (131), 27, (52, 0), 170);
            Table.States (131).Kernel := To_Vector ((((52, 0),  54,  1, (2147483647, 0),  0), ((54, 1),  54,  2,
            (2147483647, 0),  0)));
            Table.States (131).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 0),  27, 170)));
            Table.States (132).Action_List.Set_Capacity (2);
            Add_Action (Table.States (132), 12, (54, 1), 152);
            Add_Action (Table.States (132), 28, (51, 0), 171);
            Table.States (132).Kernel := To_Vector ((((51, 0),  54,  1, (2147483647, 0),  0), ((52, 1),  54,  2,
            (2147483647, 0),  0), ((53, 2),  54,  2, (2147483647, 0),  0), ((53, 3),  54,  2, (2147483647, 0),  0),
            ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (132).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  28, 171)));
            Table.States (133).Action_List.Set_Capacity (1);
            Add_Action (Table.States (133), 16, (47, 0), 172);
            Table.States (133).Kernel := To_Vector ((0 => ((47, 0),  33,  3, (2147483647, 0),  0)));
            Table.States (133).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  16, 172)));
            Table.States (134).Action_List.Set_Capacity (6);
            Add_Action (Table.States (134), 18, (53, 0), 75);
            Add_Action (Table.States (134), 19, (52, 0), 76);
            Add_Action (Table.States (134), 20, (51, 0), 77);
            Add_Action (Table.States (134), 21, (47, 0), 78);
            Add_Action (Table.States (134), 33, (50, 0), 173);
            Add_Action (Table.States (134), 35, (50, 1), 80);
            Table.States (134).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (134), 47, 81);
            Add_Goto (Table.States (134), 50, 174);
            Add_Goto (Table.States (134), 51, 85);
            Add_Goto (Table.States (134), 52, 86);
            Add_Goto (Table.States (134), 53, 87);
            Table.States (134).Kernel := To_Vector ((0 => ((48, 1),  16,  1, (2147483647, 0),  0)));
            Table.States (134).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  33, 173)));
            Table.States (135).Action_List.Set_Capacity (8);
            Add_Action (Table.States (135), (12, 18, 19, 20, 21, 27, 33, 35), (53, 4),  2, null, null);
            Table.States (135).Kernel := To_Vector ((0 => ((53, 4),  24,  0, (53, 4),  2)));
            Table.States (135).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  2)));
            Table.States (136).Action_List.Set_Capacity (8);
            Add_Action (Table.States (136), (12, 18, 19, 20, 21, 27, 33, 35), (52, 2),  2, null, null);
            Table.States (136).Kernel := To_Vector ((0 => ((52, 2),  25,  0, (52, 2),  2)));
            Table.States (136).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 2),  2)));
            Table.States (137).Action_List.Set_Capacity (8);
            Add_Action (Table.States (137), (12, 18, 19, 20, 21, 27, 33, 35), (53, 5),  2, null, null);
            Table.States (137).Kernel := To_Vector ((0 => ((53, 5),  31,  0, (53, 5),  2)));
            Table.States (137).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  2)));
            Table.States (138).Action_List.Set_Capacity (8);
            Add_Action (Table.States (138), (12, 18, 19, 20, 21, 27, 33, 35), (52, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (138).Kernel := To_Vector ((0 => ((52, 3),  25,  0, (52, 3),  2)));
            Table.States (138).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 3),  2)));
            Table.States (139).Action_List.Set_Capacity (8);
            Add_Action (Table.States (139), (12, 18, 19, 20, 21, 27, 33, 35), (49, 1),  2, null, null);
            Table.States (139).Kernel := To_Vector ((0 => ((49, 1),  48,  0, (49, 1),  2)));
            Table.States (139).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 1),  2)));
            Table.States (140).Action_List.Set_Capacity (6);
            Add_Action (Table.States (140), 18, (53, 0), 75);
            Add_Action (Table.States (140), 19, (52, 0), 76);
            Add_Action (Table.States (140), 20, (51, 0), 77);
            Add_Action (Table.States (140), 21, (47, 0), 78);
            Add_Action (Table.States (140), 33, (48, 1), 79);
            Add_Action (Table.States (140), 35, (50, 1), 80);
            Table.States (140).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (140), 47, 81);
            Add_Goto (Table.States (140), 48, 82);
            Add_Goto (Table.States (140), 49, 175);
            Add_Goto (Table.States (140), 50, 84);
            Add_Goto (Table.States (140), 51, 85);
            Add_Goto (Table.States (140), 52, 86);
            Add_Goto (Table.States (140), 53, 87);
            Table.States (140).Kernel := To_Vector ((0 => ((54, 1),  12,  1, (2147483647, 0),  0)));
            Table.States (140).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 79)));
            Table.States (141).Action_List.Set_Capacity (11);
            Add_Action (Table.States (141), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (52, 0),  3, null, null);
            Table.States (141).Kernel := To_Vector ((0 => ((52, 0),  27,  0, (52, 0),  3)));
            Table.States (141).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  3)));
            Table.States (142).Action_List.Set_Capacity (2);
            Add_Action (Table.States (142), 12, (54, 1), 128);
            Add_Action (Table.States (142), 26, (53, 0), 176);
            Table.States (142).Kernel := To_Vector ((((53, 0),  54,  1, (2147483647, 0),  0), ((53, 1),  54,  2,
            (2147483647, 0),  0), ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (142).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (53, 0),  26, 176)));
            Table.States (143).Action_List.Set_Capacity (2);
            Add_Action (Table.States (143), 12, (54, 1), 140);
            Add_Action (Table.States (143), 27, (52, 0), 177);
            Table.States (143).Kernel := To_Vector ((((52, 0),  54,  1, (2147483647, 0),  0), ((54, 1),  54,  2,
            (2147483647, 0),  0)));
            Table.States (143).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (52, 0),  27, 177)));
            Table.States (144).Action_List.Set_Capacity (2);
            Add_Action (Table.States (144), 12, (54, 1), 152);
            Add_Action (Table.States (144), 28, (51, 0), 178);
            Table.States (144).Kernel := To_Vector ((((51, 0),  54,  1, (2147483647, 0),  0), ((52, 1),  54,  2,
            (2147483647, 0),  0), ((53, 2),  54,  2, (2147483647, 0),  0), ((53, 3),  54,  2, (2147483647, 0),  0),
            ((54, 1),  54,  2, (2147483647, 0),  0)));
            Table.States (144).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (51, 0),  28, 178)));
            Table.States (145).Action_List.Set_Capacity (1);
            Add_Action (Table.States (145), 16, (47, 0), 179);
            Table.States (145).Kernel := To_Vector ((0 => ((47, 0),  33,  3, (2147483647, 0),  0)));
            Table.States (145).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  16, 179)));
            Table.States (146).Action_List.Set_Capacity (6);
            Add_Action (Table.States (146), 18, (53, 0), 89);
            Add_Action (Table.States (146), 19, (52, 0), 90);
            Add_Action (Table.States (146), 20, (51, 0), 91);
            Add_Action (Table.States (146), 21, (47, 0), 92);
            Add_Action (Table.States (146), 33, (50, 0), 180);
            Add_Action (Table.States (146), 35, (50, 1), 94);
            Table.States (146).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (146), 47, 95);
            Add_Goto (Table.States (146), 50, 181);
            Add_Goto (Table.States (146), 51, 99);
            Add_Goto (Table.States (146), 52, 100);
            Add_Goto (Table.States (146), 53, 101);
            Table.States (146).Kernel := To_Vector ((0 => ((48, 1),  16,  1, (2147483647, 0),  0)));
            Table.States (146).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (50, 0),  33, 180)));
            Table.States (147).Action_List.Set_Capacity (8);
            Add_Action (Table.States (147), (12, 18, 19, 20, 21, 28, 33, 35), (53, 4),  2, null, null);
            Table.States (147).Kernel := To_Vector ((0 => ((53, 4),  24,  0, (53, 4),  2)));
            Table.States (147).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 4),  2)));
            Table.States (148).Action_List.Set_Capacity (8);
            Add_Action (Table.States (148), (12, 18, 19, 20, 21, 28, 33, 35), (52, 2),  2, null, null);
            Table.States (148).Kernel := To_Vector ((0 => ((52, 2),  25,  0, (52, 2),  2)));
            Table.States (148).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 2),  2)));
            Table.States (149).Action_List.Set_Capacity (8);
            Add_Action (Table.States (149), (12, 18, 19, 20, 21, 28, 33, 35), (53, 5),  2, null, null);
            Table.States (149).Kernel := To_Vector ((0 => ((53, 5),  31,  0, (53, 5),  2)));
            Table.States (149).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 5),  2)));
            Table.States (150).Action_List.Set_Capacity (8);
            Add_Action (Table.States (150), (12, 18, 19, 20, 21, 28, 33, 35), (52, 3),  2, rhs_optional_item_3'Access,
            null);
            Table.States (150).Kernel := To_Vector ((0 => ((52, 3),  25,  0, (52, 3),  2)));
            Table.States (150).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 3),  2)));
            Table.States (151).Action_List.Set_Capacity (8);
            Add_Action (Table.States (151), (12, 18, 19, 20, 21, 28, 33, 35), (49, 1),  2, null, null);
            Table.States (151).Kernel := To_Vector ((0 => ((49, 1),  48,  0, (49, 1),  2)));
            Table.States (151).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (49, 1),  2)));
            Table.States (152).Action_List.Set_Capacity (6);
            Add_Action (Table.States (152), 18, (53, 0), 89);
            Add_Action (Table.States (152), 19, (52, 0), 90);
            Add_Action (Table.States (152), 20, (51, 0), 91);
            Add_Action (Table.States (152), 21, (47, 0), 92);
            Add_Action (Table.States (152), 33, (48, 1), 93);
            Add_Action (Table.States (152), 35, (50, 1), 94);
            Table.States (152).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (152), 47, 95);
            Add_Goto (Table.States (152), 48, 96);
            Add_Goto (Table.States (152), 49, 182);
            Add_Goto (Table.States (152), 50, 98);
            Add_Goto (Table.States (152), 51, 99);
            Add_Goto (Table.States (152), 52, 100);
            Add_Goto (Table.States (152), 53, 101);
            Table.States (152).Kernel := To_Vector ((0 => ((54, 1),  12,  1, (2147483647, 0),  0)));
            Table.States (152).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (48, 1),  33, 93)));
            Table.States (153).Action_List.Set_Capacity (14);
            Add_Action (Table.States (153), 11, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 12, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 18, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 19, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 20, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 21, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 23, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 24, (53, 2), 183);
            Add_Action (Table.States (153), 25, (52, 1), 184);
            Add_Action (Table.States (153), 29, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 31, (53, 3), 185);
            Add_Action (Table.States (153), 33, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 35, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (153), 36, Reduce, (51, 0),  3, null, null);
            Table.States (153).Kernel := To_Vector ((((51, 0),  28,  0, (51, 0),  3), ((52, 1),  28,  1, (2147483647,
            0),  0), ((53, 2),  28,  1, (2147483647, 0),  0), ((53, 3),  28,  1, (2147483647, 0),  0)));
            Table.States (153).Minimal_Complete_Actions := To_Vector (((Reduce, (51, 0),  3), (Shift, (52, 1),  25,
            184), (Shift, (53, 2),  24, 183), (Shift, (53, 3),  31, 185)));
            Table.States (154).Action_List.Set_Capacity (1);
            Add_Action (Table.States (154), 33, (47, 0), 186);
            Table.States (154).Kernel := To_Vector ((0 => ((47, 0),  16,  2, (2147483647, 0),  0)));
            Table.States (154).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 186)));
            Table.States (155).Action_List.Set_Capacity (14);
            Add_Action (Table.States (155), 11, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 23, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 24, (53, 4), 105);
            Add_Action (Table.States (155), 25, (52, 2), 106);
            Add_Action (Table.States (155), 29, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 31, (53, 5), 107);
            Add_Action (Table.States (155), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 35, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (155), 36, Reduce, (50, 0),  1, null, null);
            Table.States (155).Kernel := To_Vector ((((50, 0),  33,  0, (50, 0),  1), ((52, 2),  33,  1, (2147483647,
            0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,  1, (2147483647, 0),  0)));
            Table.States (155).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            106), (Shift, (53, 4),  24, 105), (Shift, (53, 5),  31, 107)));
            Table.States (156).Action_List.Set_Capacity (11);
            Add_Action (Table.States (156), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (48, 1),  3, null, null);
            Table.States (156).Kernel := To_Vector ((0 => ((48, 1),  50,  0, (48, 1),  3)));
            Table.States (156).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 1),  3)));
            Table.States (157).Action_List.Set_Capacity (5);
            Add_Action (Table.States (157), (12, 23, 29, 33, 36), (45, 1),  3, rhs_list_1'Access, null);
            Table.States (157).Kernel := To_Vector ((0 => ((45, 1),  46,  0, (45, 1),  3)));
            Table.States (157).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 1),  3)));
            Table.States (158).Action_List.Set_Capacity (1);
            Add_Action (Table.States (158), 5, (45, 3), 187);
            Table.States (158).Kernel := To_Vector ((0 => ((45, 3),  4,  1, (2147483647, 0),  0)));
            Table.States (158).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 3),  5, 187)));
            Table.States (159).Action_List.Set_Capacity (1);
            Add_Action (Table.States (159), 33, (45, 2), 188);
            Table.States (159).Kernel := To_Vector ((0 => ((45, 2),  5,  3, (2147483647, 0),  0)));
            Table.States (159).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 2),  33, 188)));
            Table.States (160).Action_List.Set_Capacity (5);
            Add_Action (Table.States (160), (12, 23, 29, 33, 36), (46, 3),  3, rhs_3'Access, null);
            Table.States (160).Kernel := To_Vector ((0 => ((46, 3),  11,  0, (46, 3),  3)));
            Table.States (160).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (46, 3),  3)));
            Table.States (161).Action_List.Set_Capacity (9);
            Add_Action (Table.States (161), 12, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (161), 18, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (161), 19, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (161), 20, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (161), 21, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (161), 22, (53, 1), 189);
            Add_Action (Table.States (161), 26, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (161), 33, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (161), 35, Reduce, (53, 0),  3, null, null);
            Table.States (161).Kernel := To_Vector ((((53, 0),  26,  0, (53, 0),  3), ((53, 1),  26,  1, (2147483647,
            0),  0)));
            Table.States (161).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  3), (Shift, (53, 1),  22,
            189)));
            Table.States (162).Action_List.Set_Capacity (8);
            Add_Action (Table.States (162), (12, 18, 19, 20, 21, 26, 33, 35), (52, 0),  3, null, null);
            Table.States (162).Kernel := To_Vector ((0 => ((52, 0),  27,  0, (52, 0),  3)));
            Table.States (162).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  3)));
            Table.States (163).Action_List.Set_Capacity (11);
            Add_Action (Table.States (163), 12, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (163), 18, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (163), 19, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (163), 20, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (163), 21, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (163), 24, (53, 2), 190);
            Add_Action (Table.States (163), 25, (52, 1), 191);
            Add_Action (Table.States (163), 26, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (163), 31, (53, 3), 192);
            Add_Action (Table.States (163), 33, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (163), 35, Reduce, (51, 0),  3, null, null);
            Table.States (163).Kernel := To_Vector ((((51, 0),  28,  0, (51, 0),  3), ((52, 1),  28,  1, (2147483647,
            0),  0), ((53, 2),  28,  1, (2147483647, 0),  0), ((53, 3),  28,  1, (2147483647, 0),  0)));
            Table.States (163).Minimal_Complete_Actions := To_Vector (((Reduce, (51, 0),  3), (Shift, (52, 1),  25,
            191), (Shift, (53, 2),  24, 190), (Shift, (53, 3),  31, 192)));
            Table.States (164).Action_List.Set_Capacity (1);
            Add_Action (Table.States (164), 33, (47, 0), 193);
            Table.States (164).Kernel := To_Vector ((0 => ((47, 0),  16,  2, (2147483647, 0),  0)));
            Table.States (164).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 193)));
            Table.States (165).Action_List.Set_Capacity (11);
            Add_Action (Table.States (165), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (165), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (165), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (165), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (165), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (165), 24, (53, 4), 123);
            Add_Action (Table.States (165), 25, (52, 2), 124);
            Add_Action (Table.States (165), 26, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (165), 31, (53, 5), 125);
            Add_Action (Table.States (165), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (165), 35, Reduce, (50, 0),  1, null, null);
            Table.States (165).Kernel := To_Vector ((((50, 0),  33,  0, (50, 0),  1), ((52, 2),  33,  1, (2147483647,
            0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,  1, (2147483647, 0),  0)));
            Table.States (165).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            124), (Shift, (53, 4),  24, 123), (Shift, (53, 5),  31, 125)));
            Table.States (166).Action_List.Set_Capacity (8);
            Add_Action (Table.States (166), (12, 18, 19, 20, 21, 26, 33, 35), (48, 1),  3, null, null);
            Table.States (166).Kernel := To_Vector ((0 => ((48, 1),  50,  0, (48, 1),  3)));
            Table.States (166).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 1),  3)));
            Table.States (167).Action_List.Set_Capacity (8);
            Add_Action (Table.States (167), 12, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (167), 18, (53, 0), 61);
            Add_Action (Table.States (167), 19, (52, 0), 62);
            Add_Action (Table.States (167), 20, (51, 0), 63);
            Add_Action (Table.States (167), 21, (47, 0), 64);
            Add_Action (Table.States (167), 26, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (167), 33, (48, 1), 65);
            Add_Action (Table.States (167), 35, (50, 1), 66);
            Table.States (167).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (167), 47, 67);
            Add_Goto (Table.States (167), 48, 127);
            Add_Goto (Table.States (167), 50, 70);
            Add_Goto (Table.States (167), 51, 71);
            Add_Goto (Table.States (167), 52, 72);
            Add_Goto (Table.States (167), 53, 73);
            Table.States (167).Kernel := To_Vector ((((49, 1),  49,  1, (2147483647, 0),  0), ((54, 1),  49,  0, (54,
            1),  3)));
            Table.States (167).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 1),  3)));
            Table.States (168).Action_List.Set_Capacity (11);
            Add_Action (Table.States (168), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (53, 1),  4, null, null);
            Table.States (168).Kernel := To_Vector ((0 => ((53, 1),  22,  0, (53, 1),  4)));
            Table.States (168).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  4)));
            Table.States (169).Action_List.Set_Capacity (9);
            Add_Action (Table.States (169), 12, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (169), 18, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (169), 19, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (169), 20, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (169), 21, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (169), 22, (53, 1), 194);
            Add_Action (Table.States (169), 27, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (169), 33, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (169), 35, Reduce, (53, 0),  3, null, null);
            Table.States (169).Kernel := To_Vector ((((53, 0),  26,  0, (53, 0),  3), ((53, 1),  26,  1, (2147483647,
            0),  0)));
            Table.States (169).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  3), (Shift, (53, 1),  22,
            194)));
            Table.States (170).Action_List.Set_Capacity (8);
            Add_Action (Table.States (170), (12, 18, 19, 20, 21, 27, 33, 35), (52, 0),  3, null, null);
            Table.States (170).Kernel := To_Vector ((0 => ((52, 0),  27,  0, (52, 0),  3)));
            Table.States (170).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  3)));
            Table.States (171).Action_List.Set_Capacity (11);
            Add_Action (Table.States (171), 12, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (171), 18, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (171), 19, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (171), 20, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (171), 21, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (171), 24, (53, 2), 195);
            Add_Action (Table.States (171), 25, (52, 1), 196);
            Add_Action (Table.States (171), 27, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (171), 31, (53, 3), 197);
            Add_Action (Table.States (171), 33, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (171), 35, Reduce, (51, 0),  3, null, null);
            Table.States (171).Kernel := To_Vector ((((51, 0),  28,  0, (51, 0),  3), ((52, 1),  28,  1, (2147483647,
            0),  0), ((53, 2),  28,  1, (2147483647, 0),  0), ((53, 3),  28,  1, (2147483647, 0),  0)));
            Table.States (171).Minimal_Complete_Actions := To_Vector (((Reduce, (51, 0),  3), (Shift, (52, 1),  25,
            196), (Shift, (53, 2),  24, 195), (Shift, (53, 3),  31, 197)));
            Table.States (172).Action_List.Set_Capacity (1);
            Add_Action (Table.States (172), 33, (47, 0), 198);
            Table.States (172).Kernel := To_Vector ((0 => ((47, 0),  16,  2, (2147483647, 0),  0)));
            Table.States (172).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 198)));
            Table.States (173).Action_List.Set_Capacity (11);
            Add_Action (Table.States (173), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (173), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (173), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (173), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (173), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (173), 24, (53, 4), 135);
            Add_Action (Table.States (173), 25, (52, 2), 136);
            Add_Action (Table.States (173), 27, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (173), 31, (53, 5), 137);
            Add_Action (Table.States (173), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (173), 35, Reduce, (50, 0),  1, null, null);
            Table.States (173).Kernel := To_Vector ((((50, 0),  33,  0, (50, 0),  1), ((52, 2),  33,  1, (2147483647,
            0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,  1, (2147483647, 0),  0)));
            Table.States (173).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            136), (Shift, (53, 4),  24, 135), (Shift, (53, 5),  31, 137)));
         end Subr_2;
         procedure Subr_3
         is begin
            Table.States (174).Action_List.Set_Capacity (8);
            Add_Action (Table.States (174), (12, 18, 19, 20, 21, 27, 33, 35), (48, 1),  3, null, null);
            Table.States (174).Kernel := To_Vector ((0 => ((48, 1),  50,  0, (48, 1),  3)));
            Table.States (174).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 1),  3)));
            Table.States (175).Action_List.Set_Capacity (8);
            Add_Action (Table.States (175), 12, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (175), 18, (53, 0), 75);
            Add_Action (Table.States (175), 19, (52, 0), 76);
            Add_Action (Table.States (175), 20, (51, 0), 77);
            Add_Action (Table.States (175), 21, (47, 0), 78);
            Add_Action (Table.States (175), 27, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (175), 33, (48, 1), 79);
            Add_Action (Table.States (175), 35, (50, 1), 80);
            Table.States (175).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (175), 47, 81);
            Add_Goto (Table.States (175), 48, 139);
            Add_Goto (Table.States (175), 50, 84);
            Add_Goto (Table.States (175), 51, 85);
            Add_Goto (Table.States (175), 52, 86);
            Add_Goto (Table.States (175), 53, 87);
            Table.States (175).Kernel := To_Vector ((((49, 1),  49,  1, (2147483647, 0),  0), ((54, 1),  49,  0, (54,
            1),  3)));
            Table.States (175).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 1),  3)));
            Table.States (176).Action_List.Set_Capacity (9);
            Add_Action (Table.States (176), 12, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (176), 18, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (176), 19, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (176), 20, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (176), 21, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (176), 22, (53, 1), 199);
            Add_Action (Table.States (176), 28, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (176), 33, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (176), 35, Reduce, (53, 0),  3, null, null);
            Table.States (176).Kernel := To_Vector ((((53, 0),  26,  0, (53, 0),  3), ((53, 1),  26,  1, (2147483647,
            0),  0)));
            Table.States (176).Minimal_Complete_Actions := To_Vector (((Reduce, (53, 0),  3), (Shift, (53, 1),  22,
            199)));
            Table.States (177).Action_List.Set_Capacity (8);
            Add_Action (Table.States (177), (12, 18, 19, 20, 21, 28, 33, 35), (52, 0),  3, null, null);
            Table.States (177).Kernel := To_Vector ((0 => ((52, 0),  27,  0, (52, 0),  3)));
            Table.States (177).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 0),  3)));
            Table.States (178).Action_List.Set_Capacity (11);
            Add_Action (Table.States (178), 12, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (178), 18, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (178), 19, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (178), 20, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (178), 21, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (178), 24, (53, 2), 200);
            Add_Action (Table.States (178), 25, (52, 1), 201);
            Add_Action (Table.States (178), 28, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (178), 31, (53, 3), 202);
            Add_Action (Table.States (178), 33, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (178), 35, Reduce, (51, 0),  3, null, null);
            Table.States (178).Kernel := To_Vector ((((51, 0),  28,  0, (51, 0),  3), ((52, 1),  28,  1, (2147483647,
            0),  0), ((53, 2),  28,  1, (2147483647, 0),  0), ((53, 3),  28,  1, (2147483647, 0),  0)));
            Table.States (178).Minimal_Complete_Actions := To_Vector (((Reduce, (51, 0),  3), (Shift, (52, 1),  25,
            201), (Shift, (53, 2),  24, 200), (Shift, (53, 3),  31, 202)));
            Table.States (179).Action_List.Set_Capacity (1);
            Add_Action (Table.States (179), 33, (47, 0), 203);
            Table.States (179).Kernel := To_Vector ((0 => ((47, 0),  16,  2, (2147483647, 0),  0)));
            Table.States (179).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  33, 203)));
            Table.States (180).Action_List.Set_Capacity (11);
            Add_Action (Table.States (180), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (180), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (180), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (180), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (180), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (180), 24, (53, 4), 147);
            Add_Action (Table.States (180), 25, (52, 2), 148);
            Add_Action (Table.States (180), 28, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (180), 31, (53, 5), 149);
            Add_Action (Table.States (180), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (180), 35, Reduce, (50, 0),  1, null, null);
            Table.States (180).Kernel := To_Vector ((((50, 0),  33,  0, (50, 0),  1), ((52, 2),  33,  1, (2147483647,
            0),  0), ((53, 4),  33,  1, (2147483647, 0),  0), ((53, 5),  33,  1, (2147483647, 0),  0)));
            Table.States (180).Minimal_Complete_Actions := To_Vector (((Reduce, (50, 0),  1), (Shift, (52, 2),  25,
            148), (Shift, (53, 4),  24, 147), (Shift, (53, 5),  31, 149)));
            Table.States (181).Action_List.Set_Capacity (8);
            Add_Action (Table.States (181), (12, 18, 19, 20, 21, 28, 33, 35), (48, 1),  3, null, null);
            Table.States (181).Kernel := To_Vector ((0 => ((48, 1),  50,  0, (48, 1),  3)));
            Table.States (181).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (48, 1),  3)));
            Table.States (182).Action_List.Set_Capacity (8);
            Add_Action (Table.States (182), 12, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (182), 18, (53, 0), 89);
            Add_Action (Table.States (182), 19, (52, 0), 90);
            Add_Action (Table.States (182), 20, (51, 0), 91);
            Add_Action (Table.States (182), 21, (47, 0), 92);
            Add_Action (Table.States (182), 28, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (182), 33, (48, 1), 93);
            Add_Action (Table.States (182), 35, (50, 1), 94);
            Table.States (182).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (182), 47, 95);
            Add_Goto (Table.States (182), 48, 151);
            Add_Goto (Table.States (182), 50, 98);
            Add_Goto (Table.States (182), 51, 99);
            Add_Goto (Table.States (182), 52, 100);
            Add_Goto (Table.States (182), 53, 101);
            Table.States (182).Kernel := To_Vector ((((49, 1),  49,  1, (2147483647, 0),  0), ((54, 1),  49,  0, (54,
            1),  3)));
            Table.States (182).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (54, 1),  3)));
            Table.States (183).Action_List.Set_Capacity (11);
            Add_Action (Table.States (183), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (53, 2),  4, null, null);
            Table.States (183).Kernel := To_Vector ((0 => ((53, 2),  24,  0, (53, 2),  4)));
            Table.States (183).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  4)));
            Table.States (184).Action_List.Set_Capacity (11);
            Add_Action (Table.States (184), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (52, 1),  4, null, null);
            Table.States (184).Kernel := To_Vector ((0 => ((52, 1),  25,  0, (52, 1),  4)));
            Table.States (184).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  4)));
            Table.States (185).Action_List.Set_Capacity (11);
            Add_Action (Table.States (185), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (53, 3),  4, null, null);
            Table.States (185).Kernel := To_Vector ((0 => ((53, 3),  31,  0, (53, 3),  4)));
            Table.States (185).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  4)));
            Table.States (186).Action_List.Set_Capacity (1);
            Add_Action (Table.States (186), 17, (47, 0), 204);
            Table.States (186).Kernel := To_Vector ((0 => ((47, 0),  33,  1, (2147483647, 0),  0)));
            Table.States (186).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  17, 204)));
            Table.States (187).Action_List.Set_Capacity (5);
            Add_Action (Table.States (187), (12, 23, 29, 33, 36), (45, 3),  4, null, null);
            Table.States (187).Kernel := To_Vector ((0 => ((45, 3),  5,  0, (45, 3),  4)));
            Table.States (187).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 3),  4)));
            Table.States (188).Action_List.Set_Capacity (1);
            Add_Action (Table.States (188), 16, (45, 2), 205);
            Table.States (188).Kernel := To_Vector ((0 => ((45, 2),  33,  2, (2147483647, 0),  0)));
            Table.States (188).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 2),  16, 205)));
            Table.States (189).Action_List.Set_Capacity (8);
            Add_Action (Table.States (189), (12, 18, 19, 20, 21, 26, 33, 35), (53, 1),  4, null, null);
            Table.States (189).Kernel := To_Vector ((0 => ((53, 1),  22,  0, (53, 1),  4)));
            Table.States (189).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  4)));
            Table.States (190).Action_List.Set_Capacity (8);
            Add_Action (Table.States (190), (12, 18, 19, 20, 21, 26, 33, 35), (53, 2),  4, null, null);
            Table.States (190).Kernel := To_Vector ((0 => ((53, 2),  24,  0, (53, 2),  4)));
            Table.States (190).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  4)));
            Table.States (191).Action_List.Set_Capacity (8);
            Add_Action (Table.States (191), (12, 18, 19, 20, 21, 26, 33, 35), (52, 1),  4, null, null);
            Table.States (191).Kernel := To_Vector ((0 => ((52, 1),  25,  0, (52, 1),  4)));
            Table.States (191).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  4)));
            Table.States (192).Action_List.Set_Capacity (8);
            Add_Action (Table.States (192), (12, 18, 19, 20, 21, 26, 33, 35), (53, 3),  4, null, null);
            Table.States (192).Kernel := To_Vector ((0 => ((53, 3),  31,  0, (53, 3),  4)));
            Table.States (192).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  4)));
            Table.States (193).Action_List.Set_Capacity (1);
            Add_Action (Table.States (193), 17, (47, 0), 206);
            Table.States (193).Kernel := To_Vector ((0 => ((47, 0),  33,  1, (2147483647, 0),  0)));
            Table.States (193).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  17, 206)));
            Table.States (194).Action_List.Set_Capacity (8);
            Add_Action (Table.States (194), (12, 18, 19, 20, 21, 27, 33, 35), (53, 1),  4, null, null);
            Table.States (194).Kernel := To_Vector ((0 => ((53, 1),  22,  0, (53, 1),  4)));
            Table.States (194).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  4)));
            Table.States (195).Action_List.Set_Capacity (8);
            Add_Action (Table.States (195), (12, 18, 19, 20, 21, 27, 33, 35), (53, 2),  4, null, null);
            Table.States (195).Kernel := To_Vector ((0 => ((53, 2),  24,  0, (53, 2),  4)));
            Table.States (195).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  4)));
            Table.States (196).Action_List.Set_Capacity (8);
            Add_Action (Table.States (196), (12, 18, 19, 20, 21, 27, 33, 35), (52, 1),  4, null, null);
            Table.States (196).Kernel := To_Vector ((0 => ((52, 1),  25,  0, (52, 1),  4)));
            Table.States (196).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  4)));
            Table.States (197).Action_List.Set_Capacity (8);
            Add_Action (Table.States (197), (12, 18, 19, 20, 21, 27, 33, 35), (53, 3),  4, null, null);
            Table.States (197).Kernel := To_Vector ((0 => ((53, 3),  31,  0, (53, 3),  4)));
            Table.States (197).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  4)));
            Table.States (198).Action_List.Set_Capacity (1);
            Add_Action (Table.States (198), 17, (47, 0), 207);
            Table.States (198).Kernel := To_Vector ((0 => ((47, 0),  33,  1, (2147483647, 0),  0)));
            Table.States (198).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  17, 207)));
            Table.States (199).Action_List.Set_Capacity (8);
            Add_Action (Table.States (199), (12, 18, 19, 20, 21, 28, 33, 35), (53, 1),  4, null, null);
            Table.States (199).Kernel := To_Vector ((0 => ((53, 1),  22,  0, (53, 1),  4)));
            Table.States (199).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 1),  4)));
            Table.States (200).Action_List.Set_Capacity (8);
            Add_Action (Table.States (200), (12, 18, 19, 20, 21, 28, 33, 35), (53, 2),  4, null, null);
            Table.States (200).Kernel := To_Vector ((0 => ((53, 2),  24,  0, (53, 2),  4)));
            Table.States (200).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 2),  4)));
            Table.States (201).Action_List.Set_Capacity (8);
            Add_Action (Table.States (201), (12, 18, 19, 20, 21, 28, 33, 35), (52, 1),  4, null, null);
            Table.States (201).Kernel := To_Vector ((0 => ((52, 1),  25,  0, (52, 1),  4)));
            Table.States (201).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (52, 1),  4)));
            Table.States (202).Action_List.Set_Capacity (8);
            Add_Action (Table.States (202), (12, 18, 19, 20, 21, 28, 33, 35), (53, 3),  4, null, null);
            Table.States (202).Kernel := To_Vector ((0 => ((53, 3),  31,  0, (53, 3),  4)));
            Table.States (202).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (53, 3),  4)));
            Table.States (203).Action_List.Set_Capacity (1);
            Add_Action (Table.States (203), 17, (47, 0), 208);
            Table.States (203).Kernel := To_Vector ((0 => ((47, 0),  33,  1, (2147483647, 0),  0)));
            Table.States (203).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (47, 0),  17, 208)));
            Table.States (204).Action_List.Set_Capacity (11);
            Add_Action (Table.States (204), (11, 12, 18, 19, 20, 21, 23, 29, 33, 35, 36), (47, 0),  5, null, null);
            Table.States (204).Kernel := To_Vector ((0 => ((47, 0),  17,  0, (47, 0),  5)));
            Table.States (204).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 0),  5)));
            Table.States (205).Action_List.Set_Capacity (1);
            Add_Action (Table.States (205), 33, (45, 2), 209);
            Table.States (205).Kernel := To_Vector ((0 => ((45, 2),  16,  1, (2147483647, 0),  0)));
            Table.States (205).Minimal_Complete_Actions := To_Vector ((0 => (Shift, (45, 2),  33, 209)));
            Table.States (206).Action_List.Set_Capacity (8);
            Add_Action (Table.States (206), (12, 18, 19, 20, 21, 26, 33, 35), (47, 0),  5, null, null);
            Table.States (206).Kernel := To_Vector ((0 => ((47, 0),  17,  0, (47, 0),  5)));
            Table.States (206).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 0),  5)));
            Table.States (207).Action_List.Set_Capacity (8);
            Add_Action (Table.States (207), (12, 18, 19, 20, 21, 27, 33, 35), (47, 0),  5, null, null);
            Table.States (207).Kernel := To_Vector ((0 => ((47, 0),  17,  0, (47, 0),  5)));
            Table.States (207).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 0),  5)));
            Table.States (208).Action_List.Set_Capacity (8);
            Add_Action (Table.States (208), (12, 18, 19, 20, 21, 28, 33, 35), (47, 0),  5, null, null);
            Table.States (208).Kernel := To_Vector ((0 => ((47, 0),  17,  0, (47, 0),  5)));
            Table.States (208).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (47, 0),  5)));
            Table.States (209).Action_List.Set_Capacity (5);
            Add_Action (Table.States (209), (12, 23, 29, 33, 36), (45, 2),  6, null, null);
            Table.States (209).Kernel := To_Vector ((0 => ((45, 2),  33,  0, (45, 2),  6)));
            Table.States (209).Minimal_Complete_Actions := To_Vector ((0 => (Reduce, (45, 2),  6)));
         end Subr_3;
      begin
         Subr_1;
         Subr_2;
         Subr_3;
         Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);
      end;

      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace.Descriptor),
         Table,
         Language_Fixes,
         Language_Matching_Begin_Tokens,
         Language_String_ID_Set,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisitoken_Grammar_1_Process_Main;
