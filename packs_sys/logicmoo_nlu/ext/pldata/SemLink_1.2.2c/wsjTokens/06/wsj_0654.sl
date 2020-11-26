nw/wsj/06/wsj_0654.parse 0 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/06/wsj_0654.parse 0 18 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 5:1*16:1-ARG0=Cause 18:0-rel 19:1-ARG1=Patient 
nw/wsj/06/wsj_0654.parse 1 8 gold issue-v 13.3 NF issue.01 null ----- 0:2*9:1-ARG1=Theme 8:0-rel 10:1-ARGM-TMP 13:1-ARG3 
nw/wsj/06/wsj_0654.parse 2 1 gold provide-v 13.4.1-2 Supply provide.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Theme 4:1-ARG2=Recipient 
nw/wsj/06/wsj_0654.parse 3 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/06/wsj_0654.parse 3 5 gold begin-v 55.1-1 Activity_start begin.01 null ----- 3:1-ARG0=Agent;Agent 5:0-rel 6:3-ARG1=Theme;Activity 
nw/wsj/06/wsj_0654.parse 3 15 gold review-v 34.1 NF review.01 1 ----- 3:1*13:1-ARG0=Agent 6:2*12:1-ARGM-ADV 15:0-rel 16:2-ARG1=Theme 3:1*13:1-LINK-PRO 6:2*12:1-LINK-SLC 
nw/wsj/06/wsj_0654.parse 3 25 gold exchange-v 13.6-1 IN exchange.01 2 ----- 16:1*22:1*23:1-ARG0=Agent 24:0-ARGM-MOD 25:0-rel 26:1-ARG1=Theme 28:1-ARG3=Co-Theme 16:1*22:1-LINK-SLC 
nw/wsj/06/wsj_0654.parse 4 16 gold say-v 37.7-1 IN say.01 null ----- 0:2*18:1-ARG1=Topic 14:1-ARG0=Agent 16:0-rel 
nw/wsj/06/wsj_0654.parse 5 4 gold call-v 60 IN call.03 null ----- 1:1-ARG0=Agent 4:0-rel 5:1-ARG1=Topic 
nw/wsj/06/wsj_0654.parse 6 6 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARGM-DIS 2:1*7:1-ARG3=Goal 4:0-ARGM-MOD 6:0-rel 8:2-ARG1=Theme 
nw/wsj/06/wsj_0654.parse 8 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:2-ARG1=Topic 
nw/wsj/06/wsj_0654.parse 8 31 gold make-v 29.3 Causation make.02 null ----- 22:1-ARG0=Agent 24:1-ARGM-TMP 31:0-rel 32:3-ARG1=Theme 
nw/wsj/06/wsj_0654.parse 8 37 gold implement-v 55.5-1 Execute_plan implement.01 1 ----- 35:1-ARG0=Agent 37:0-rel 38:2-ARG1=Theme 
