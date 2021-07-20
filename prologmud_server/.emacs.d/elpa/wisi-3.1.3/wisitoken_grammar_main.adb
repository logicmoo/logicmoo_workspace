--  generated parser support file.
--  command line: wisitoken-bnf-generate.exe  --generate LALR Ada re2c wisitoken_grammar.wy
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

with Wisitoken_Grammar_Actions; use Wisitoken_Grammar_Actions;
with WisiToken.Lexer.re2c;
with wisitoken_grammar_re2c_c;
package body Wisitoken_Grammar_Main is

   package Lexer is new WisiToken.Lexer.re2c
     (wisitoken_grammar_re2c_c.New_Lexer,
      wisitoken_grammar_re2c_c.Free_Lexer,
      wisitoken_grammar_re2c_c.Reset_Lexer,
      wisitoken_grammar_re2c_c.Next_Token);

   procedure Create_Parser
     (Parser                         :    out WisiToken.Parse.LR.Parser_No_Recover.Parser;
      Trace                        : not null access WisiToken.Trace'Class;
      User_Data                    : in     WisiToken.Syntax_Trees.User_Data_Access)
   is
      use WisiToken.Parse.LR;
      Table : constant Parse_Table_Ptr := new Parse_Table
        (State_First       => 0,
         State_Last        => 102,
         First_Terminal    => 3,
         Last_Terminal     => 36,
         First_Nonterminal => 37,
         Last_Nonterminal  => 56);
   begin
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
            Table.States (2).Action_List.Set_Capacity (2);
            Add_Action (Table.States (2), 13, (43, 0), 15);
            Add_Action (Table.States (2), 14, (43, 1), 16);
            Table.States (3).Action_List.Set_Capacity (3);
            Add_Action (Table.States (3), (23, 33, 36), (55, 0),  1, null, null);
            Table.States (4).Action_List.Set_Capacity (3);
            Add_Action (Table.States (4), (23, 33, 36), (55, 1),  1, null, null);
            Table.States (5).Action_List.Set_Capacity (3);
            Add_Action (Table.States (5), (23, 33, 36), (56, 0),  1, null, null);
            Table.States (6).Action_List.Set_Capacity (3);
            Add_Action (Table.States (6), 23, (38, 0), 1);
            Add_Action (Table.States (6), 33, (43, 0), 2);
            Add_Action (Table.States (6), 36, Accept_It, (37, 0),  1, null, null);
            Table.States (6).Goto_List.Set_Capacity (3);
            Add_Goto (Table.States (6), 38, 3);
            Add_Goto (Table.States (6), 43, 4);
            Add_Goto (Table.States (6), 55, 17);
            Table.States (7).Action_List.Set_Capacity (1);
            Add_Action (Table.States (7), 33, (40, 0), 18);
            Table.States (7).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (7), 40, 19);
            Table.States (8).Action_List.Set_Capacity (1);
            Add_Action (Table.States (8), 5, (38, 5), 20);
            Table.States (9).Action_List.Set_Capacity (1);
            Add_Action (Table.States (9), 33, (38, 4), 21);
            Table.States (10).Action_List.Set_Capacity (1);
            Add_Action (Table.States (10), (1 =>  33), (39, 0),  1, null, null);
            Table.States (11).Action_List.Set_Capacity (1);
            Add_Action (Table.States (11), 21, (39, 1), 22);
            Table.States (12).Action_List.Set_Capacity (1);
            Add_Action (Table.States (12), 21, (39, 2), 23);
            Table.States (13).Action_List.Set_Capacity (13);
            Add_Action (Table.States (13), 8, (42, 10), 24);
            Add_Action (Table.States (13), 10, (42, 5), 25);
            Add_Action (Table.States (13), 15, (42, 0), 26);
            Add_Action (Table.States (13), 16, (42, 2), 27);
            Add_Action (Table.States (13), 20, (42, 3), 28);
            Add_Action (Table.States (13), 23, Reduce, (38, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (13), 28, (42, 6), 29);
            Add_Action (Table.States (13), 30, (42, 7), 30);
            Add_Action (Table.States (13), 32, (42, 4), 31);
            Add_Action (Table.States (13), 33, (42, 1), 32);
            Add_Conflict (Table.States (13), 33, (38, 3),  2, declaration_3'Access, null);
            Add_Action (Table.States (13), 34, (42, 8), 33);
            Add_Action (Table.States (13), 35, (42, 9), 34);
            Add_Action (Table.States (13), 36, Reduce, (38, 3),  2, declaration_3'Access, null);
            Table.States (13).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (13), 41, 35);
            Add_Goto (Table.States (13), 42, 36);
            Table.States (14).Action_List.Set_Capacity (1);
            Add_Action (Table.States (14), 33, (38, 0), 37);
            Table.States (15).Action_List.Set_Capacity (10);
            Add_Action (Table.States (15), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 18, (53, 0), 38);
            Add_Action (Table.States (15), 19, (52, 0), 39);
            Add_Action (Table.States (15), 20, (51, 0), 40);
            Add_Action (Table.States (15), 21, (47, 0), 41);
            Add_Action (Table.States (15), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 33, (48, 1), 42);
            Add_Conflict (Table.States (15), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (15), 35, (50, 1), 43);
            Add_Action (Table.States (15), 36, Reduce, (46, 0),  0, null, null);
            Table.States (15).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (15), 45, 44);
            Add_Goto (Table.States (15), 46, 45);
            Add_Goto (Table.States (15), 47, 46);
            Add_Goto (Table.States (15), 48, 47);
            Add_Goto (Table.States (15), 49, 48);
            Add_Goto (Table.States (15), 50, 49);
            Add_Goto (Table.States (15), 51, 50);
            Add_Goto (Table.States (15), 52, 51);
            Add_Goto (Table.States (15), 53, 52);
            Table.States (16).Action_List.Set_Capacity (10);
            Add_Action (Table.States (16), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 18, (53, 0), 38);
            Add_Action (Table.States (16), 19, (52, 0), 39);
            Add_Action (Table.States (16), 20, (51, 0), 40);
            Add_Action (Table.States (16), 21, (47, 0), 41);
            Add_Action (Table.States (16), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 33, (48, 1), 42);
            Add_Conflict (Table.States (16), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (16), 35, (50, 1), 43);
            Add_Action (Table.States (16), 36, Reduce, (46, 0),  0, null, null);
            Table.States (16).Goto_List.Set_Capacity (9);
            Add_Goto (Table.States (16), 45, 53);
            Add_Goto (Table.States (16), 46, 45);
            Add_Goto (Table.States (16), 47, 46);
            Add_Goto (Table.States (16), 48, 47);
            Add_Goto (Table.States (16), 49, 48);
            Add_Goto (Table.States (16), 50, 49);
            Add_Goto (Table.States (16), 51, 50);
            Add_Goto (Table.States (16), 52, 51);
            Add_Goto (Table.States (16), 53, 52);
            Table.States (17).Action_List.Set_Capacity (3);
            Add_Action (Table.States (17), (23, 33, 36), (56, 1),  2, null, null);
            Table.States (18).Action_List.Set_Capacity (2);
            Add_Action (Table.States (18), (9, 33), (40, 0),  1, null, null);
            Table.States (19).Action_List.Set_Capacity (2);
            Add_Action (Table.States (19), 9, (38, 1), 54);
            Add_Action (Table.States (19), 33, (40, 1), 55);
            Table.States (20).Action_List.Set_Capacity (3);
            Add_Action (Table.States (20), (23, 33, 36), (38, 5),  3, declaration_5'Access, null);
            Table.States (21).Action_List.Set_Capacity (1);
            Add_Action (Table.States (21), 16, (38, 4), 56);
            Table.States (22).Action_List.Set_Capacity (1);
            Add_Action (Table.States (22), 33, (39, 1), 57);
            Table.States (23).Action_List.Set_Capacity (1);
            Add_Action (Table.States (23), 33, (39, 2), 58);
            Table.States (24).Action_List.Set_Capacity (13);
            Add_Action (Table.States (24), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 10),  1, null,
            null);
            Table.States (25).Action_List.Set_Capacity (13);
            Add_Action (Table.States (25), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 5),  1, null,
            null);
            Table.States (26).Action_List.Set_Capacity (13);
            Add_Action (Table.States (26), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 0),  1, null,
            null);
            Table.States (27).Action_List.Set_Capacity (13);
            Add_Action (Table.States (27), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 2),  1, null,
            null);
            Table.States (28).Action_List.Set_Capacity (13);
            Add_Action (Table.States (28), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 3),  1, null,
            null);
            Table.States (29).Action_List.Set_Capacity (13);
            Add_Action (Table.States (29), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 6),  1, null,
            null);
            Table.States (30).Action_List.Set_Capacity (13);
            Add_Action (Table.States (30), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 7),  1, null,
            null);
            Table.States (31).Action_List.Set_Capacity (13);
            Add_Action (Table.States (31), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 4),  1, null,
            null);
            Table.States (32).Action_List.Set_Capacity (13);
            Add_Action (Table.States (32), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 1),  1, null,
            null);
            Table.States (33).Action_List.Set_Capacity (13);
            Add_Action (Table.States (33), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 8),  1, null,
            null);
            Table.States (34).Action_List.Set_Capacity (13);
            Add_Action (Table.States (34), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (42, 9),  1, null,
            null);
            Table.States (35).Action_List.Set_Capacity (13);
            Add_Action (Table.States (35), 8, (42, 10), 24);
            Add_Action (Table.States (35), 10, (42, 5), 25);
            Add_Action (Table.States (35), 15, (42, 0), 26);
            Add_Action (Table.States (35), 16, (42, 2), 27);
            Add_Action (Table.States (35), 20, (42, 3), 28);
            Add_Action (Table.States (35), 23, Reduce, (38, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (35), 28, (42, 6), 29);
            Add_Action (Table.States (35), 30, (42, 7), 30);
            Add_Action (Table.States (35), 32, (42, 4), 31);
            Add_Action (Table.States (35), 33, (42, 1), 32);
            Add_Conflict (Table.States (35), 33, (38, 2),  3, declaration_2'Access, null);
            Add_Action (Table.States (35), 34, (42, 8), 33);
            Add_Action (Table.States (35), 35, (42, 9), 34);
            Add_Action (Table.States (35), 36, Reduce, (38, 2),  3, declaration_2'Access, null);
            Table.States (35).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (35), 42, 59);
            Table.States (36).Action_List.Set_Capacity (13);
            Add_Action (Table.States (36), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (41, 0),  1, null,
            null);
            Table.States (37).Action_List.Set_Capacity (11);
            Add_Action (Table.States (37), 8, (42, 10), 24);
            Add_Action (Table.States (37), 10, (42, 5), 25);
            Add_Action (Table.States (37), 15, (42, 0), 26);
            Add_Action (Table.States (37), 16, (42, 2), 27);
            Add_Action (Table.States (37), 20, (42, 3), 28);
            Add_Action (Table.States (37), 28, (42, 6), 29);
            Add_Action (Table.States (37), 30, (42, 7), 30);
            Add_Action (Table.States (37), 32, (42, 4), 31);
            Add_Action (Table.States (37), 33, (42, 1), 32);
            Add_Action (Table.States (37), 34, (42, 8), 33);
            Add_Action (Table.States (37), 35, (42, 9), 34);
            Table.States (37).Goto_List.Set_Capacity (2);
            Add_Goto (Table.States (37), 41, 60);
            Add_Goto (Table.States (37), 42, 36);
            Table.States (38).Action_List.Set_Capacity (6);
            Add_Action (Table.States (38), 18, (53, 0), 38);
            Add_Action (Table.States (38), 19, (52, 0), 39);
            Add_Action (Table.States (38), 20, (51, 0), 40);
            Add_Action (Table.States (38), 21, (47, 0), 41);
            Add_Action (Table.States (38), 33, (48, 1), 42);
            Add_Action (Table.States (38), 35, (50, 1), 43);
            Table.States (38).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (38), 47, 46);
            Add_Goto (Table.States (38), 48, 47);
            Add_Goto (Table.States (38), 49, 61);
            Add_Goto (Table.States (38), 50, 49);
            Add_Goto (Table.States (38), 51, 50);
            Add_Goto (Table.States (38), 52, 51);
            Add_Goto (Table.States (38), 53, 52);
            Add_Goto (Table.States (38), 54, 62);
            Table.States (39).Action_List.Set_Capacity (6);
            Add_Action (Table.States (39), 18, (53, 0), 38);
            Add_Action (Table.States (39), 19, (52, 0), 39);
            Add_Action (Table.States (39), 20, (51, 0), 40);
            Add_Action (Table.States (39), 21, (47, 0), 41);
            Add_Action (Table.States (39), 33, (48, 1), 42);
            Add_Action (Table.States (39), 35, (50, 1), 43);
            Table.States (39).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (39), 47, 46);
            Add_Goto (Table.States (39), 48, 47);
            Add_Goto (Table.States (39), 49, 61);
            Add_Goto (Table.States (39), 50, 49);
            Add_Goto (Table.States (39), 51, 50);
            Add_Goto (Table.States (39), 52, 51);
            Add_Goto (Table.States (39), 53, 52);
            Add_Goto (Table.States (39), 54, 63);
            Table.States (40).Action_List.Set_Capacity (6);
            Add_Action (Table.States (40), 18, (53, 0), 38);
            Add_Action (Table.States (40), 19, (52, 0), 39);
            Add_Action (Table.States (40), 20, (51, 0), 40);
            Add_Action (Table.States (40), 21, (47, 0), 41);
            Add_Action (Table.States (40), 33, (48, 1), 42);
            Add_Action (Table.States (40), 35, (50, 1), 43);
            Table.States (40).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (40), 47, 46);
            Add_Goto (Table.States (40), 48, 47);
            Add_Goto (Table.States (40), 49, 61);
            Add_Goto (Table.States (40), 50, 49);
            Add_Goto (Table.States (40), 51, 50);
            Add_Goto (Table.States (40), 52, 51);
            Add_Goto (Table.States (40), 53, 52);
            Add_Goto (Table.States (40), 54, 64);
            Table.States (41).Action_List.Set_Capacity (1);
            Add_Action (Table.States (41), 33, (47, 0), 65);
            Table.States (42).Action_List.Set_Capacity (18);
            Add_Action (Table.States (42), 11, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 16, (48, 1), 66);
            Add_Action (Table.States (42), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 23, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 24, (53, 4), 67);
            Add_Action (Table.States (42), 25, (52, 2), 68);
            Add_Action (Table.States (42), 26, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 27, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 28, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 29, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 31, (53, 5), 69);
            Add_Action (Table.States (42), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 35, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (42), 36, Reduce, (50, 0),  1, null, null);
            Table.States (43).Action_List.Set_Capacity (15);
            Add_Action (Table.States (43), 11, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 12, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 18, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 19, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 20, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 21, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 23, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 25, (52, 3), 70);
            Add_Action (Table.States (43), 26, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 27, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 28, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 29, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 33, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 35, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Add_Action (Table.States (43), 36, Reduce, (50, 1),  1, rhs_item_1'Access, null);
            Table.States (44).Action_List.Set_Capacity (5);
            Add_Action (Table.States (44), 12, (45, 1), 71);
            Add_Action (Table.States (44), 23, (45, 2), 72);
            Add_Conflict (Table.States (44), 23, (44, 1),  0, null, null);
            Add_Action (Table.States (44), 29, (44, 0), 73);
            Add_Action (Table.States (44), 33, Reduce, (44, 1),  0, null, null);
            Add_Action (Table.States (44), 36, Reduce, (44, 1),  0, null, null);
            Table.States (44).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (44), 44, 74);
            Table.States (45).Action_List.Set_Capacity (5);
            Add_Action (Table.States (45), (12, 23, 29, 33, 36), (45, 0),  1, null, null);
            Table.States (46).Action_List.Set_Capacity (14);
            Add_Action (Table.States (46), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 2),  1,
            rhs_item_2'Access, null);
            Table.States (47).Action_List.Set_Capacity (14);
            Add_Action (Table.States (47), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (49, 0),  1, null,
            null);
            Table.States (48).Action_List.Set_Capacity (11);
            Add_Action (Table.States (48), 11, (46, 2), 75);
            Add_Action (Table.States (48), 12, Reduce, (46, 1),  1, null, null);
            Add_Action (Table.States (48), 18, (53, 0), 38);
            Add_Action (Table.States (48), 19, (52, 0), 39);
            Add_Action (Table.States (48), 20, (51, 0), 40);
            Add_Action (Table.States (48), 21, (47, 0), 41);
            Add_Action (Table.States (48), 23, Reduce, (46, 1),  1, null, null);
            Add_Action (Table.States (48), 29, Reduce, (46, 1),  1, null, null);
            Add_Action (Table.States (48), 33, (48, 1), 42);
            Add_Conflict (Table.States (48), 33, (46, 1),  1, null, null);
            Add_Action (Table.States (48), 35, (50, 1), 43);
            Add_Action (Table.States (48), 36, Reduce, (46, 1),  1, null, null);
            Table.States (48).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (48), 47, 46);
            Add_Goto (Table.States (48), 48, 76);
            Add_Goto (Table.States (48), 50, 49);
            Add_Goto (Table.States (48), 51, 50);
            Add_Goto (Table.States (48), 52, 51);
            Add_Goto (Table.States (48), 53, 52);
            Table.States (49).Action_List.Set_Capacity (14);
            Add_Action (Table.States (49), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (48, 0),  1, null,
            null);
            Table.States (50).Action_List.Set_Capacity (14);
            Add_Action (Table.States (50), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 5),  1,
            rhs_item_5'Access, null);
            Table.States (51).Action_List.Set_Capacity (14);
            Add_Action (Table.States (51), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 3),  1,
            rhs_item_3'Access, null);
            Table.States (52).Action_List.Set_Capacity (14);
            Add_Action (Table.States (52), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (50, 4),  1,
            rhs_item_4'Access, null);
            Table.States (53).Action_List.Set_Capacity (5);
            Add_Action (Table.States (53), 12, (45, 1), 71);
            Add_Action (Table.States (53), 23, (45, 2), 72);
            Add_Conflict (Table.States (53), 23, (44, 1),  0, null, null);
            Add_Action (Table.States (53), 29, (44, 0), 73);
            Add_Action (Table.States (53), 33, Reduce, (44, 1),  0, null, null);
            Add_Action (Table.States (53), 36, Reduce, (44, 1),  0, null, null);
            Table.States (53).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (53), 44, 77);
            Table.States (54).Action_List.Set_Capacity (3);
            Add_Action (Table.States (54), (23, 33, 36), (38, 1),  4, declaration_1'Access, null);
            Table.States (55).Action_List.Set_Capacity (2);
            Add_Action (Table.States (55), (9, 33), (40, 1),  2, null, null);
            Table.States (56).Action_List.Set_Capacity (1);
            Add_Action (Table.States (56), 33, (38, 4), 78);
            Table.States (57).Action_List.Set_Capacity (1);
            Add_Action (Table.States (57), 17, (39, 1), 79);
            Table.States (58).Action_List.Set_Capacity (1);
            Add_Action (Table.States (58), 17, (39, 2), 80);
            Table.States (59).Action_List.Set_Capacity (13);
            Add_Action (Table.States (59), (8, 10, 15, 16, 20, 23, 28, 30, 32, 33, 34, 35, 36), (41, 1),  2, null,
            null);
            Table.States (60).Action_List.Set_Capacity (13);
            Add_Action (Table.States (60), 8, (42, 10), 24);
            Add_Action (Table.States (60), 10, (42, 5), 25);
            Add_Action (Table.States (60), 15, (42, 0), 26);
            Add_Action (Table.States (60), 16, (42, 2), 27);
            Add_Action (Table.States (60), 20, (42, 3), 28);
            Add_Action (Table.States (60), 23, Reduce, (38, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (60), 28, (42, 6), 29);
            Add_Action (Table.States (60), 30, (42, 7), 30);
            Add_Action (Table.States (60), 32, (42, 4), 31);
            Add_Action (Table.States (60), 33, (42, 1), 32);
            Add_Conflict (Table.States (60), 33, (38, 0),  4, declaration_0'Access, null);
            Add_Action (Table.States (60), 34, (42, 8), 33);
            Add_Action (Table.States (60), 35, (42, 9), 34);
            Add_Action (Table.States (60), 36, Reduce, (38, 0),  4, declaration_0'Access, null);
            Table.States (60).Goto_List.Set_Capacity (1);
            Add_Goto (Table.States (60), 42, 59);
            Table.States (61).Action_List.Set_Capacity (10);
            Add_Action (Table.States (61), 12, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (61), 18, (53, 0), 38);
            Add_Action (Table.States (61), 19, (52, 0), 39);
            Add_Action (Table.States (61), 20, (51, 0), 40);
            Add_Action (Table.States (61), 21, (47, 0), 41);
            Add_Action (Table.States (61), 26, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (61), 27, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (61), 28, Reduce, (54, 0),  1, null, null);
            Add_Action (Table.States (61), 33, (48, 1), 42);
            Add_Action (Table.States (61), 35, (50, 1), 43);
            Table.States (61).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (61), 47, 46);
            Add_Goto (Table.States (61), 48, 76);
            Add_Goto (Table.States (61), 50, 49);
            Add_Goto (Table.States (61), 51, 50);
            Add_Goto (Table.States (61), 52, 51);
            Add_Goto (Table.States (61), 53, 52);
            Table.States (62).Action_List.Set_Capacity (2);
            Add_Action (Table.States (62), 12, (54, 1), 81);
            Add_Action (Table.States (62), 26, (53, 0), 82);
            Table.States (63).Action_List.Set_Capacity (2);
            Add_Action (Table.States (63), 12, (54, 1), 81);
            Add_Action (Table.States (63), 27, (52, 0), 83);
            Table.States (64).Action_List.Set_Capacity (2);
            Add_Action (Table.States (64), 12, (54, 1), 81);
            Add_Action (Table.States (64), 28, (51, 0), 84);
            Table.States (65).Action_List.Set_Capacity (1);
            Add_Action (Table.States (65), 16, (47, 0), 85);
            Table.States (66).Action_List.Set_Capacity (6);
            Add_Action (Table.States (66), 18, (53, 0), 38);
            Add_Action (Table.States (66), 19, (52, 0), 39);
            Add_Action (Table.States (66), 20, (51, 0), 40);
            Add_Action (Table.States (66), 21, (47, 0), 41);
            Add_Action (Table.States (66), 33, (50, 0), 86);
            Add_Action (Table.States (66), 35, (50, 1), 43);
            Table.States (66).Goto_List.Set_Capacity (5);
            Add_Goto (Table.States (66), 47, 46);
            Add_Goto (Table.States (66), 50, 87);
            Add_Goto (Table.States (66), 51, 50);
            Add_Goto (Table.States (66), 52, 51);
            Add_Goto (Table.States (66), 53, 52);
            Table.States (67).Action_List.Set_Capacity (14);
            Add_Action (Table.States (67), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 4),  2, null,
            null);
            Table.States (68).Action_List.Set_Capacity (14);
            Add_Action (Table.States (68), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 2),  2, null,
            null);
            Table.States (69).Action_List.Set_Capacity (14);
            Add_Action (Table.States (69), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 5),  2, null,
            null);
            Table.States (70).Action_List.Set_Capacity (14);
            Add_Action (Table.States (70), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 3),  2,
            rhs_optional_item_3'Access, null);
            Table.States (71).Action_List.Set_Capacity (10);
            Add_Action (Table.States (71), 12, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (71), 18, (53, 0), 38);
            Add_Action (Table.States (71), 19, (52, 0), 39);
            Add_Action (Table.States (71), 20, (51, 0), 40);
            Add_Action (Table.States (71), 21, (47, 0), 41);
            Add_Action (Table.States (71), 23, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (71), 29, Reduce, (46, 0),  0, null, null);
            Add_Action (Table.States (71), 33, (48, 1), 42);
            Add_Conflict (Table.States (71), 33, (46, 0),  0, null, null);
            Add_Action (Table.States (71), 35, (50, 1), 43);
            Add_Action (Table.States (71), 36, Reduce, (46, 0),  0, null, null);
            Table.States (71).Goto_List.Set_Capacity (8);
            Add_Goto (Table.States (71), 46, 88);
            Add_Goto (Table.States (71), 47, 46);
            Add_Goto (Table.States (71), 48, 47);
            Add_Goto (Table.States (71), 49, 48);
            Add_Goto (Table.States (71), 50, 49);
            Add_Goto (Table.States (71), 51, 50);
            Add_Goto (Table.States (71), 52, 51);
            Add_Goto (Table.States (71), 53, 52);
            Table.States (72).Action_List.Set_Capacity (2);
            Add_Action (Table.States (72), 4, (45, 3), 89);
            Add_Action (Table.States (72), 5, (45, 2), 90);
            Table.States (73).Action_List.Set_Capacity (3);
            Add_Action (Table.States (73), (23, 33, 36), (44, 0),  1, null, null);
            Table.States (74).Action_List.Set_Capacity (3);
            Add_Action (Table.States (74), (23, 33, 36), (43, 0),  4, nonterminal_0'Access, null);
            Table.States (75).Action_List.Set_Capacity (6);
            Add_Action (Table.States (75), 11, (46, 3), 91);
            Add_Action (Table.States (75), 12, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (75), 23, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (75), 29, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (75), 33, Reduce, (46, 2),  2, null, null);
            Add_Action (Table.States (75), 36, Reduce, (46, 2),  2, null, null);
            Table.States (76).Action_List.Set_Capacity (14);
            Add_Action (Table.States (76), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (49, 1),  2, null,
            null);
            Table.States (77).Action_List.Set_Capacity (3);
            Add_Action (Table.States (77), (23, 33, 36), (43, 1),  4, nonterminal_1'Access, null);
            Table.States (78).Action_List.Set_Capacity (3);
            Add_Action (Table.States (78), (23, 33, 36), (38, 4),  5, declaration_4'Access, null);
            Table.States (79).Action_List.Set_Capacity (1);
            Add_Action (Table.States (79), (1 =>  33), (39, 1),  4, null, null);
            Table.States (80).Action_List.Set_Capacity (1);
            Add_Action (Table.States (80), (1 =>  33), (39, 2),  4, null, null);
            Table.States (81).Action_List.Set_Capacity (6);
            Add_Action (Table.States (81), 18, (53, 0), 38);
            Add_Action (Table.States (81), 19, (52, 0), 39);
            Add_Action (Table.States (81), 20, (51, 0), 40);
            Add_Action (Table.States (81), 21, (47, 0), 41);
            Add_Action (Table.States (81), 33, (48, 1), 42);
            Add_Action (Table.States (81), 35, (50, 1), 43);
            Table.States (81).Goto_List.Set_Capacity (7);
            Add_Goto (Table.States (81), 47, 46);
            Add_Goto (Table.States (81), 48, 47);
            Add_Goto (Table.States (81), 49, 92);
            Add_Goto (Table.States (81), 50, 49);
            Add_Goto (Table.States (81), 51, 50);
            Add_Goto (Table.States (81), 52, 51);
            Add_Goto (Table.States (81), 53, 52);
            Table.States (82).Action_List.Set_Capacity (15);
            Add_Action (Table.States (82), 11, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 12, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 18, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 19, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 20, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 21, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 22, (53, 1), 93);
            Add_Action (Table.States (82), 23, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 26, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 27, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 28, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 29, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 33, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 35, Reduce, (53, 0),  3, null, null);
            Add_Action (Table.States (82), 36, Reduce, (53, 0),  3, null, null);
            Table.States (83).Action_List.Set_Capacity (14);
            Add_Action (Table.States (83), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 0),  3, null,
            null);
            Table.States (84).Action_List.Set_Capacity (17);
            Add_Action (Table.States (84), 11, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 12, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 18, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 19, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 20, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 21, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 23, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 24, (53, 2), 94);
            Add_Action (Table.States (84), 25, (52, 1), 95);
            Add_Action (Table.States (84), 26, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 27, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 28, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 29, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 31, (53, 3), 96);
            Add_Action (Table.States (84), 33, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 35, Reduce, (51, 0),  3, null, null);
            Add_Action (Table.States (84), 36, Reduce, (51, 0),  3, null, null);
            Table.States (85).Action_List.Set_Capacity (1);
            Add_Action (Table.States (85), 33, (47, 0), 97);
            Table.States (86).Action_List.Set_Capacity (17);
            Add_Action (Table.States (86), 11, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 12, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 18, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 19, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 20, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 21, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 23, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 24, (53, 4), 67);
            Add_Action (Table.States (86), 25, (52, 2), 68);
            Add_Action (Table.States (86), 26, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 27, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 28, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 29, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 31, (53, 5), 69);
            Add_Action (Table.States (86), 33, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 35, Reduce, (50, 0),  1, null, null);
            Add_Action (Table.States (86), 36, Reduce, (50, 0),  1, null, null);
            Table.States (87).Action_List.Set_Capacity (14);
            Add_Action (Table.States (87), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (48, 1),  3, null,
            null);
            Table.States (88).Action_List.Set_Capacity (5);
            Add_Action (Table.States (88), (12, 23, 29, 33, 36), (45, 1),  3, null, null);
            Table.States (89).Action_List.Set_Capacity (1);
            Add_Action (Table.States (89), 5, (45, 3), 98);
            Table.States (90).Action_List.Set_Capacity (1);
            Add_Action (Table.States (90), 33, (45, 2), 99);
            Table.States (91).Action_List.Set_Capacity (5);
            Add_Action (Table.States (91), (12, 23, 29, 33, 36), (46, 3),  3, null, null);
            Table.States (92).Action_List.Set_Capacity (10);
            Add_Action (Table.States (92), 12, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (92), 18, (53, 0), 38);
            Add_Action (Table.States (92), 19, (52, 0), 39);
            Add_Action (Table.States (92), 20, (51, 0), 40);
            Add_Action (Table.States (92), 21, (47, 0), 41);
            Add_Action (Table.States (92), 26, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (92), 27, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (92), 28, Reduce, (54, 1),  3, null, null);
            Add_Action (Table.States (92), 33, (48, 1), 42);
            Add_Action (Table.States (92), 35, (50, 1), 43);
            Table.States (92).Goto_List.Set_Capacity (6);
            Add_Goto (Table.States (92), 47, 46);
            Add_Goto (Table.States (92), 48, 76);
            Add_Goto (Table.States (92), 50, 49);
            Add_Goto (Table.States (92), 51, 50);
            Add_Goto (Table.States (92), 52, 51);
            Add_Goto (Table.States (92), 53, 52);
            Table.States (93).Action_List.Set_Capacity (14);
            Add_Action (Table.States (93), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 1),  4, null,
            null);
            Table.States (94).Action_List.Set_Capacity (14);
            Add_Action (Table.States (94), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 2),  4, null,
            null);
            Table.States (95).Action_List.Set_Capacity (14);
            Add_Action (Table.States (95), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (52, 1),  4, null,
            null);
            Table.States (96).Action_List.Set_Capacity (14);
            Add_Action (Table.States (96), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (53, 3),  4, null,
            null);
            Table.States (97).Action_List.Set_Capacity (1);
            Add_Action (Table.States (97), 17, (47, 0), 100);
            Table.States (98).Action_List.Set_Capacity (5);
            Add_Action (Table.States (98), (12, 23, 29, 33, 36), (45, 3),  4, null, null);
            Table.States (99).Action_List.Set_Capacity (1);
            Add_Action (Table.States (99), 16, (45, 2), 101);
            Table.States (100).Action_List.Set_Capacity (14);
            Add_Action (Table.States (100), (11, 12, 18, 19, 20, 21, 23, 26, 27, 28, 29, 33, 35, 36), (47, 0),  5,
            null, null);
            Table.States (101).Action_List.Set_Capacity (1);
            Add_Action (Table.States (101), 33, (45, 2), 102);
            Table.States (102).Action_List.Set_Capacity (5);
            Add_Action (Table.States (102), (12, 23, 29, 33, 36), (45, 2),  6, null, null);
         end Subr_1;
      begin
         Subr_1;
         Table.Error_Action := new Parse_Action_Node'((Verb => Error, others => <>), null);
      end;

      WisiToken.Parse.LR.Parser_No_Recover.New_Parser
        (Parser,
         Trace,
         Lexer.New_Lexer (Trace.Descriptor),
         Table,
         User_Data,
         Max_Parallel         => 15,
         Terminate_Same_State => True);
   end Create_Parser;
end Wisitoken_Grammar_Main;
