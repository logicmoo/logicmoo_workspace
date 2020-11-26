nw/wsj/16/wsj_1630.parse 0 6 gold try-v 61 Attempt try.01 null ----- 0:1*5:1-ARG0=Agent 6:0-rel 7:1-ARGM-MNR 8:2-ARG1=Theme 
nw/wsj/16/wsj_1630.parse 0 10 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*5:1*8:1-ARG0=Agent;Seller 10:0-rel 11:2-ARG1=Theme;Goods 
nw/wsj/16/wsj_1630.parse 0 21 gold exercise-v 26.8 NF exercise.01 1 ----- 0:1*5:1*8:1-ARG0=Agent 4:1-ARGM-TMP 21:0-rel 22:1-ARG1=Theme 
nw/wsj/16/wsj_1630.parse 0 26 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 0:1*24:1-ARG0=Agent;Buyer 26:0-rel 27:1-ARG1=Theme;Goods 0:1*24:1-LINK-PRO 
nw/wsj/16/wsj_1630.parse 1 2 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 0:1-ARG0=Agent;Buyer 2:0-rel 3:1-ARG1=Theme;Goods 19:1-ARG2=Source 24:1-ARGM-MNR 30:1-ARGM-TMP 
nw/wsj/16/wsj_1630.parse 1 7 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*5:1-ARG0=Agent;Buyer 7:0-rel 8:2-ARG1=Theme;Goods 15:1-ARG3=Asset 0:1*5:1-LINK-PRO 
nw/wsj/16/wsj_1630.parse 2 3 gold cost-v 54.2 Expensiveness cost.01 null ----- 0:1-ARG1=Theme;Goods 3:0-rel 4:1-ARG3 6:2-ARG2=Value;Asset 
nw/wsj/16/wsj_1630.parse 4 2 gold house-v 54.3 Provide_lodging house.01 1 ----- 0:1-ARG2=Location 2:0-rel 3:3-ARG1=Value 
nw/wsj/16/wsj_1630.parse 4 14 gold move-v 11.2 Cause_motion move.01 null ----- 3:2*8:2*11:1*15:1-ARG1 12:0-ARGM-MOD 14:0-rel 16:1-ARG2 19:1-ARGM-TMP 3:2*8:2-LINK-SLC 
nw/wsj/16/wsj_1630.parse 4 28 gold complete-v 55.2 Activity_finish complete.01 null ----- 20:2*29:1-ARG1=Theme 28:0-rel 30:1-ARGM-TMP 
nw/wsj/16/wsj_1630.parse 5 10 gold consolidate-v 22.2-1-1 Cause_to_amalgamate consolidate.01 1 ----- 8:1-ARG0=Agent;Agent 10:0-rel 11:1-ARG1=Patient;Part_1 14:1-ARGM-LOC 
nw/wsj/16/wsj_1630.parse 7 7 gold leave-v 51.2-1 Abandonment leave.01 null ----- 5:1-ARG0=Theme 7:0-rel 8:1-ARG1=Initial_Location 
nw/wsj/16/wsj_1630.parse 7 12 gold try-v 61 Attempt try.01 null ----- 0:1-ARGM-ADV 11:1*13:1-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 27:2-ARGM-TMP 
nw/wsj/16/wsj_1630.parse 7 15 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 11:1*13:1-ARG0=Agent;Seller 15:0-rel 16:2-ARG1=Theme;Goods 
nw/wsj/16/wsj_1630.parse 8 6 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/16/wsj_1630.parse 8 13 gold exercise-v 26.8 NF exercise.01 1 ----- 8:1*11:1-ARG0=Agent 13:0-rel 14:1-ARG1=Theme 
nw/wsj/16/wsj_1630.parse 8 18 gold fall-v 45.6-1 Motion_directional fall.01 null ----- 17:1-ARG1=Patient 18:0-rel 19:1-ARG4 
nw/wsj/16/wsj_1630.parse 9 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/16/wsj_1630.parse 9 12 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 3:2*10:1-ARG0=Agent;Seller 12:0-rel 13:1-ARG1=Theme;Goods 16:1-ARG3 
nw/wsj/16/wsj_1630.parse 9 29 gold get-v 13.5.1-1 IN get.01 null ----- 3:2*10:1-ARG0=Agent 28:0-ARGM-NEG 29:0-rel 30:2-ARG1=Theme 
nw/wsj/16/wsj_1630.parse 10 12 gold cut-v 26.1-1 NF cut.09 null ----- 10:1-ARG1=Agent 12:0,13:1-rel 14:1-ARG2=Beneficiary 
nw/wsj/16/wsj_1630.parse 11 24 gold plan-v 62 Purpose plan.01 null ----- 11:2*25:1-ARG0=Experiencer;Agent 23:1-ARGM-ADV 24:0-rel 25:2-ARG1 
nw/wsj/16/wsj_1630.parse 13 10 gold have-v 100 IN have.03 null ----- 0:3-ARG0=Pivot 9:0-ARGM-MOD 10:0-rel 11:2-ARG1=Theme 
nw/wsj/16/wsj_1630.parse 13 16 gold find-v 13.5.1 IN find.01 null ----- 0:3*15:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 0:3*15:1-LINK-PRO 
nw/wsj/16/wsj_1630.parse 14 14 gold consider-v 29.9-1-1-1 Categorization consider.01 null ----- 0:2-ARGM-ADV 14:0-rel 15:2-ARG1=Theme;Item 
nw/wsj/16/wsj_1630.parse 15 14 gold remove-v 10.2 Removing remove.01 null ----- 1:1*4:1*12:1-ARG0=Agent;Agent/Cause 14:0-rel 15:1-ARG1=Theme;Theme 16:1-ARG2=Source;Source 
