nw/wsj/22/wsj_2200.parse 0 9 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:2-ARG0=Agent 9:0-rel 10:2-ARG1=Patient 
nw/wsj/22/wsj_2200.parse 0 14 gold restrict-v 76 NF restrict.01 1 ----- 10:1*11:1*12:1-ARG0=Cause 13:0-ARGM-MOD 14:0-rel 15:2-ARG1=Patient 30:2-ARGM-ADV 10:1*11:1-LINK-SLC 
nw/wsj/22/wsj_2200.parse 0 31 gold create-v 27 Creating create.01 null ----- 10:1*11:1*12:1*30:1-ARG0=Cause 31:0-rel 32:2-ARG1=Theme 10:1*11:1-LINK-SLC 
nw/wsj/22/wsj_2200.parse 1 6 gold include-v 65 NF include.01 null ----- 0:1*3:1*5:1-ARG2=Location 6:0-rel 7:2-ARG1=Theme 0:1*3:1-LINK-SLC 
nw/wsj/22/wsj_2200.parse 1 17 gold prevent-v 67 Thwarting prevent.01 1 ----- 0:2-ARG3 16:0-ARGM-MOD 17:0-rel 18:1*23:1-ARG1=Theme;Protagonist/Action 22:1-ARG2=Theme;Protagonist/Action 28:1-ARGM-MNR 
nw/wsj/22/wsj_2200.parse 1 26 gold work-v 73-3 IN work.01 null ----- 26:0-rel 27:0-ARG0=Agent 
nw/wsj/22/wsj_2200.parse 1 30 gold have-v 100 IN have.03 null ----- 18:1*29:1-ARG0=Pivot 30:0-rel 31:2-ARG1=Theme 
nw/wsj/22/wsj_2200.parse 2 2 gold intend-v 62 Purpose intend.01 null ----- 0:1*3:1-ARG0 2:0-rel 3:2-ARG1 13:1-ARGM-ADV 
nw/wsj/22/wsj_2200.parse 2 5 gold restrict-v 76 NF restrict.01 1 ----- 0:1*3:1-ARG0=Cause 5:0-rel 6:1-ARG1=Patient 8:1-ARG2=Goal 
nw/wsj/22/wsj_2200.parse 2 16 gold receive-v 13.5.2 Receiving receive.01 null ----- 14:1-ARG0=Agent;Donor 16:0-rel 17:1-ARG1=Theme;Theme 
nw/wsj/22/wsj_2200.parse 3 23 gold say-v 37.7-1 IN say.01 null ----- 1:2*24:1-ARG1=Topic 23:0-rel 25:3-ARG0=Agent 
nw/wsj/22/wsj_2200.parse 4 9 gold include-v 65 NF include.01 null ----- 0:2*10:1-ARG2=Location 9:0-rel 10:2-ARG1=Theme 
nw/wsj/22/wsj_2200.parse 4 18 gold issue-v 13.3 NF issue.01 null ----- 17:1*19:1-ARG1=Theme 18:0-rel 20:1-ARG0=Agent 17:1*19:1-LINK-PSV 
nw/wsj/22/wsj_2200.parse 5 14 gold keep-v 55.6 Cause_to_continue keep.04 null ----- 0:1*5:1*12:1-ARG0=Agent 14:0-rel 15:1*19:1-ARG1=Theme 18:1-ARG2 
nw/wsj/22/wsj_2200.parse 5 20 gold swell-v 45.5 Cause_change_of_position_on_a_scale swell.01 1 ----- 15:1*19:1-ARG0 20:0-rel 21:1-ARG1=Patient;Attribute 
nw/wsj/22/wsj_2200.parse 7 4 gold require-v 103 NF require.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Pivot 3:1-ARGM-DIS 4:0-rel 5:2-ARG1=Theme 
nw/wsj/22/wsj_2200.parse 7 12 gold maintain-v 29.5-2 Statement maintain.01 null ----- 1:1*10:1-ARG0=Agent;Speaker 5:1*9:1*13:1-ARGM-MNR 12:0-rel 14:2-ARG1=Theme;Addressee 25:1-ARGM-TMP 1:1*10:1-LINK-PRO 5:1*9:1-LINK-SLC 
nw/wsj/22/wsj_2200.parse 7 22 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 18:1*19:1*20:1*23:1-ARG1=Theme;Goods 22:0-rel 18:1*19:1-LINK-SLC 
nw/wsj/22/wsj_2200.parse 7 30 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 26:1*31:1-ARG1=Theme;Goods 28:0-ARGM-MOD 30:0-rel 32:1-ARGM-MNR 
nw/wsj/22/wsj_2200.parse 8 11 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 8:1*12:1-ARG1=Theme;Goods 11:0-rel 
nw/wsj/22/wsj_2200.parse 8 15 gold leave-v 51.2-1 Departing leave.01 null ----- 14:1-ARG0=Theme;Theme 15:0-rel 16:2-ARG1=Initial_Location;Source 22:1-ARG2 
nw/wsj/22/wsj_2200.parse 9 16 gold say-v 37.7-1 IN say.01 null ----- 1:2*17:1-ARG1=Topic 16:0-rel 18:2-ARG0=Agent 
nw/wsj/22/wsj_2200.parse 10 11 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*10:1-ARG0=Agent;Seller 11:0-rel 12:1-ARG1=Theme;Goods 
nw/wsj/22/wsj_2200.parse 10 22 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 17:1*20:1-ARG0=Agent;Seller 22:0-rel 25:1-ARG1=Theme;Goods 26:1-ARGM-TMP 
nw/wsj/22/wsj_2200.parse 10 24 gold merge-v 22.1-1-1 Cause_to_amalgamate merge.01 1 ----- 17:1*20:1-ARG0=Agent;Agent 24:0-rel 25:1-ARG1=Patient;Part_1 26:1-ARGM-TMP 
nw/wsj/22/wsj_2200.parse 10 32 gold conclude-v 55.4-1 IN conclude.02 null ----- 27:1*29:1*33:1-ARGM-TMP 30:1-ARG1=Theme 32:0-rel 27:1*29:1-LINK-SLC 
nw/wsj/22/wsj_2200.parse 11 6 gold say-v 37.7-1 IN say.01 null ----- 5:1-ARG0=Agent 6:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/22/wsj_2200.parse 11 14 gold force-v 59 NF force.01 null ----- 0:2-ARGM-PRD 10:1*15:1-ARG1=Patient 12:0-ARGM-MOD 14:0-rel 16:2-ARG2=Result 
nw/wsj/22/wsj_2200.parse 11 18 gold delay-v 53.1-1 IN delay.01 2 ----- 10:1*15:1*16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 22:1-ARGM-TMP 
nw/wsj/22/wsj_2200.parse 11 30 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 29:1-ARG0=Agent;Seller 30:0-rel 31:1-ARG1=Theme;Goods 
nw/wsj/22/wsj_2200.parse 12 6 gold wait-v 47.1-1 IN wait.01 null ----- 1:1*4:1-ARG1=Theme 3:0-ARGM-MOD 6:0-rel 7:1-ARGM-TMP 14:1-ARGM-TMP 
nw/wsj/22/wsj_2200.parse 12 10 gold collect-v 13.5.2 Commerce_collect collect.01 null ----- 8:1-ARG0=Agent;Buyer 10:0-rel 11:1-ARG1=Theme;Goods 
nw/wsj/22/wsj_2200.parse 12 22 gold say-v 37.7-1 IN say.01 null ----- 1:2*23:1-ARG1=Topic 21:1-ARG0=Agent 22:0-rel 
nw/wsj/22/wsj_2200.parse 14 3 gold allow-v 64 IN allow.01 null ----- 0:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/22/wsj_2200.parse 14 7 gold borrow-v 13.5.2 NF borrow.01 1 ----- 4:1-ARG0=Agent 7:0-rel 8:1-ARG2=Source 11:2-ARG1=Theme 17:1-ARGM-TMP 
nw/wsj/22/wsj_2200.parse 15 3 gold say-v 37.7-1 IN say.01 null ----- 2:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/22/wsj_2200.parse 15 12 gold exceed-v 90-1 NF exceed.01 1 ----- 5:2-ARG0=Theme 10:0-ARGM-MOD 11:0-ARGM-NEG 12:0-rel 13:2-ARG1=Co-Theme 
nw/wsj/22/wsj_2200.parse 15 26 gold include-v 65 NF include.01 null ----- 25:1-ARG0=Agent 26:0-rel 27:2-ARG1=Theme 
nw/wsj/22/wsj_2200.parse 15 33 gold subtract-v 10.1 NF subtract.01 1 ----- 25:1-ARG0=Agent 33:0-rel 34:1-ARG2=Source 36:2-ARG1=Theme 
nw/wsj/22/wsj_2200.parse 15 45 gold hold-v 15.1-1 Manipulation hold.01 null ----- 40:1*42:1*46:1-ARG1=Theme;Entity 43:1-ARG0=Agent;Agent 45:0-rel 40:1*42:1-LINK-SLC 
nw/wsj/22/wsj_2200.parse 16 6 gold intend-v 62 Purpose intend.01 null ----- 0:0-ARGM-DIS 1:1-ARG0 3:0-ARGM-NEG 6:0-rel 7:1-ARG1 
nw/wsj/22/wsj_2200.parse 16 11 gold say-v 37.7-1 IN say.01 null ----- 11:0-rel 12:1-ARG1=Topic 14:2-ARG0=Agent 
nw/wsj/22/wsj_2200.parse 16 34 gold say-v 37.7-1 IN say.01 null ----- 25:2-ARG0=Agent 34:0-rel 35:1-ARG1=Topic 
nw/wsj/22/wsj_2200.parse 16 46 gold keep-v 55.6 Cause_to_continue keep.02 null ----- 36:1*44:1-ARG0=Agent 45:0-ARGM-NEG 46:0-rel 48:2-ARG1=Theme 
nw/wsj/22/wsj_2200.parse 17 3 gold lead-v 59 Causation lead.03 null ----- 1:1-ARG0=Agent 3:0-rel 4:1-ARG2=Result 
nw/wsj/22/wsj_2200.parse 17 27 gold say-v 37.7-1 IN say.01 null ----- 1:2*28:1-ARG1=Topic 26:1-ARG0=Agent 27:0-rel 
nw/wsj/22/wsj_2200.parse 18 21 gold want-v 32.1-1-1 Desiring want.01 null ----- 20:1-ARG0=Pivot;Experiencer 21:0-rel 22:2-ARG1=Theme;Event/Focal_participant 
