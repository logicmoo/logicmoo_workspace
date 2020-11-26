nw/wsj/24/wsj_2432.parse 1 3 gold signal-v 37.4 NF signal.01 1 ----- 0:2-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/24/wsj_2432.parse 1 9 gold prepare-v 55.5-1 Activity_prepare prepare.02 2 ----- 5:1-ARG1=Theme 9:0-rel 10:2-ARG2 
nw/wsj/24/wsj_2432.parse 1 12 gold inject-v 9.8 IN inject.01 2 ----- 5:1*10:1-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 17:1-ARG2=Destination 21:2-ARGM-PRP 5:1*10:1-LINK-PRO 
nw/wsj/24/wsj_2432.parse 1 23 gold prevent-v 67 Preventing prevent.01 1 ----- 21:1-ARG0=Agent;Preventing_cause 23:0-rel 24:1-ARG1=Theme;Event 
nw/wsj/24/wsj_2432.parse 2 12 gold keep-v 55.6 Cause_to_continue keep.02 1 ----- 11:1-ARG0=Agent 12:0-rel 17:2-ARGM-PNC 
nw/wsj/24/wsj_2432.parse 2 19 gold avoid-v 52 Avoiding avoid.01 1 ----- 11:1*17:1-ARG0=Agent;Agent 19:0-rel 20:2-ARG1=Theme;Undesirable_situation 
nw/wsj/24/wsj_2432.parse 2 21 gold make-v 29.3 Causation make.02 3 ----- 11:1*17:1*20:1-ARG0=Agent 21:0-rel 22:2-ARG1=Theme 
nw/wsj/24/wsj_2432.parse 5 12 gold soar-v 45.6-1 Change_position_on_a_scale soar.01 2 ----- 10:1-ARG1=Patient;Item 12:0-rel 
nw/wsj/24/wsj_2432.parse 5 16 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 14:1-ARG1=Patient;Item 16:0-rel 
nw/wsj/24/wsj_2432.parse 6 2 gold drop-v 45.6-1 Change_position_on_a_scale drop.01 2 ----- 0:1-ARG1=Patient;Item 2:0-rel 3:2-ARGM-TMP 
nw/wsj/24/wsj_2432.parse 7 3 gold trade-v 13.6-1 Exchange trade.01 1 ----- 0:1-ARG1=Theme 3:0-rel 4:2-ARG3=Co-Theme 6:1-ARGM-LOC 
nw/wsj/24/wsj_2432.parse 9 15 gold say-v 37.7-1 IN say.01 1 ----- 14:1-ARG0=Agent 15:0-rel 16:1-ARG1=Topic 
nw/wsj/24/wsj_2432.parse 10 3 gold base-v 97.1 NF base.02 1 ----- 0:1*4:1-ARG1=Theme 3:0-rel 5:1-ARGM-ADV 6:1-ARG2=Source 
nw/wsj/24/wsj_2432.parse 10 17 gold say-v 37.7-1 IN say.01 1 ----- 0:2*19:1-ARG1=Topic 16:1-ARG0=Agent 17:0-rel 
nw/wsj/24/wsj_2432.parse 11 7 gold follow-v 47.8 Cotheme follow.01 2 ----- 0:2-ARG1=Theme;Theme 7:0-rel 8:2-ARG2=Co-Theme;Cotheme 
nw/wsj/24/wsj_2432.parse 11 23 gold signal-v 37.4 NF signal.01 2 ----- 19:1-ARG0=Agent 23:0-rel 24:2-ARG1=Topic 
nw/wsj/24/wsj_2432.parse 12 4 gold grow-v 45.6-1 IN grow.01 4 ----- 0:1-ARGM-DIS 2:1-ARG1=Patient 4:0-rel 5:1-ARG2=Extent 7:1-ARGM-TMP 
nw/wsj/24/wsj_2432.parse 13 7 gold remove-v 10.1 Removing remove.01 1 ----- 0:1*8:1-ARG1=Theme;Theme 7:0-rel 9:1-ARG2 
nw/wsj/24/wsj_2432.parse 13 20 gold hope-v 32.2-1 Desiring hope.01 1 ----- 18:1-ARG0=Pivot 19:1-ARGM-TMP 20:0-rel 21:2-ARG1=Theme 
nw/wsj/24/wsj_2432.parse 14 20 gold meet-v 36.3-1 IN meet.03 2 ----- 15:1*24:1-ARGM-TMP 16:2-ARG0=Agent 20:0-rel 21:2-ARGM-TMP 
nw/wsj/24/wsj_2432.parse 15 2 gold signal-v 37.4 NF signal.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/24/wsj_2432.parse 15 18 gold try-v 61 Attempt try.01 1 ----- 17:1-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 
nw/wsj/24/wsj_2432.parse 16 5 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 0:1*23:1-ARG1=Patient;Item 5:0-rel 6:1-ARG2=Extent;Difference 8:1-ARGM-TMP 23:2-ARGM-ADV 
nw/wsj/24/wsj_2432.parse 16 24 gold drag-v 11.4 Cause_motion drag.01 6 ----- 0:1*23:1-ARG1=Theme;Theme 24:0-rel 25:1-ARG2=Destination;Goal 26:1-ARG0=Agent;Agent/Cause 
nw/wsj/24/wsj_2432.parse 17 4 gold entangle-v 22.2-2 NF entangle.01 null ----- 0:1-ARG1=Patient 4:0-rel 5:1-ARG2=Co-Patient 
nw/wsj/24/wsj_2432.parse 18 3 gold set-v 9.1-2 IN set.01 1 ----- 0:1-ARG0=Agent 2:0-ARGM-MOD 3:0-rel 4:1-ARG2=Destination 5:2-ARG1=Theme 
nw/wsj/24/wsj_2432.parse 19 4 gold receive-v 13.5.2 Receiving receive.01 1 ----- 0:2-ARG0=Agent;Donor 4:0-rel 5:1-ARG1=Theme;Theme 23:1-ARG2=Source 
nw/wsj/24/wsj_2432.parse 20 4 gold include-v 65 NF include.01 1 ----- 0:1-ARG2=Location 3:0-ARGM-NEG 4:0-rel 5:2-ARG1=Theme 
nw/wsj/24/wsj_2432.parse 21 4 gold start-v 55.1-1 Process_start start.01 2 ----- 0:1,5:2-ARG1=Theme;Event 4:0-rel 
nw/wsj/24/wsj_2432.parse 21 7 gold affect-v 31.1 NF affect.01 1 ----- 0:1*5:1-ARG0=Stimulus 7:0-rel 8:1-ARG1=Experiencer 
nw/wsj/24/wsj_2432.parse 22 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARGM-TMP 4:1-ARG1=Topic 
nw/wsj/24/wsj_2432.parse 22 19 gold receive-v 13.5.2 Receiving receive.01 1 ----- 18:1-ARG0=Agent;Donor 19:0-rel 20:1-ARG1=Theme;Theme 21:1-ARG2=Source 
nw/wsj/24/wsj_2432.parse 23 4 gold launch-v 55.5-1 IN launch.01 2 ----- 0:1-ARG0 3:0-ARGM-MOD 4:0-rel 5:1-ARG1 10:1-ARGM-ADV 
nw/wsj/24/wsj_2432.parse 23 14 gold emerge-v 48.1.1 IN emerge.01 2 ----- 11:1-ARG0=Theme 14:0-rel 
nw/wsj/24/wsj_2432.parse 23 18 gold say-v 37.7-1 IN say.01 1 ----- 0:2*20:1-ARG1=Topic 16:1-ARG0=Agent 18:0-rel 
nw/wsj/24/wsj_2432.parse 24 14 gold merge-v 22.1-1-1 Cause_to_amalgamate merge.01 1 ----- 0:2*12:1-ARG0=Agent;Agent 14:0-rel 15:2-ARG1=Patient;Part_1 0:2*12:1-LINK-PRO 
nw/wsj/24/wsj_2432.parse 25 7 gold give-v 13.1-1 Giving give.01 3 ----- 0:1-ARG0=Agent;Donor 6:0-ARGM-MOD 7:0-rel 8:1-ARG1=Theme;Theme 11:1-ARG2=Recipient;Recipient 
nw/wsj/24/wsj_2432.parse 25 12 gold develop-v 48.1.1 IN develop.01 2 ----- 12:0-rel 13:0-ARG1=Location 
nw/wsj/24/wsj_2432.parse 25 16 gold have-v 100 IN have.03 2 ----- 12:1*14:1*15:1-ARG0=Pivot 16:0-rel 17:2-ARG1=Theme 12:1*14:1-LINK-SLC 
nw/wsj/24/wsj_2432.parse 27 1 gold ail-v 40.8.1 Perception_body ail.01 null ----- 0:0-ARGM-EXT 1:0-rel 2:0-ARG1=Experiencer;Experiencer 
nw/wsj/24/wsj_2432.parse 27 4 gold sell-v 13.1-1 Commerce_sell sell.01 5 ----- 0:1-ARG1=Theme;Goods 4:0,5:1-rel 6:1-ARG0=Agent;Seller 
nw/wsj/24/wsj_2432.parse 27 13 gold prevent-v 67 Preventing prevent.01 1 ----- 11:1-ARG0=Agent;Preventing_cause 13:0-rel 14:2-ARG1=Theme;Event 
