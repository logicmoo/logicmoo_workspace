nw/wsj/06/wsj_0641.parse 0 4 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Topic 
nw/wsj/06/wsj_0641.parse 0 15 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 6:1*13:1-ARG0=Agent;Buyer 15:0-rel 16:1-ARG1=Theme;Goods 
nw/wsj/06/wsj_0641.parse 1 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/06/wsj_0641.parse 1 6 gold move-v 11.2 Cause_motion move.01 null ----- 4:1*21:1-ARG0 5:0-ARGM-MOD 6:0-rel 7:2-ARG1 12:1-ARG2 21:2-ARGM-PRP 
nw/wsj/06/wsj_0641.parse 2 8 gold mix-v 22.1-1-1 Cause_to_amalgamate mix.01 null ----- 3:1*5:1*6:1*9:1-ARG1=Patient;Part_1 8:0-rel 10:1-ARG2=Co-Patient;Part_2 13:2-ARG3 3:1*5:1-LINK-SLC 
nw/wsj/06/wsj_0641.parse 2 15 gold make-v 26.1-1 Manufacturing make.01 null ----- 13:1-ARG0=Agent;Manufacturer 15:0-rel 16:2-ARG1=Product;Product 
nw/wsj/06/wsj_0641.parse 2 18 gold use-v 105 IN use.01 null ----- 16:1*19:1-ARG1 18:0-rel 20:1-ARGM-LOC 16:1*19:1-LINK-PSV 
nw/wsj/06/wsj_0641.parse 3 10 gold own-v 100 NF own.01 null ----- 4:2,8:1*11:1-ARG1=Theme 10:0-rel 12:1-ARG0=Pivot 4:2*11:1-LINK-PSV 
nw/wsj/06/wsj_0641.parse 6 4 gold destroy-v 44 Destroying destroy.01 null ----- 0:1*6:1-ARGM-TMP 1:1*5:1-ARG1=Patient;Undergoer 4:0-rel 
nw/wsj/06/wsj_0641.parse 6 10 gold think-v 29.9-2 IN think.01 null ----- 0:2-ARGM-TMP 9:1-ARG0 10:0-rel 11:2-ARG1 
nw/wsj/06/wsj_0641.parse 6 12 gold get-v 26.6.2 IN get.03 null ----- 11:1-ARG1=Patient 12:0-rel 13:1-ARG2=Goal 
nw/wsj/06/wsj_0641.parse 6 19 gold happen-v 48.3 Event happen.01 null ----- 15:1-ARG1=Theme;Event 18:0-ARGM-MOD 19:0-rel 20:1-ARGM-LOC 
nw/wsj/06/wsj_0641.parse 6 28 gold say-v 37.7-1 IN say.01 null ----- 0:3*30:1-ARG1=Topic 25:1-ARG0=Agent 28:0-rel 
nw/wsj/06/wsj_0641.parse 7 1 gold prompt-v 59 NF prompt.02 1 ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Patient 
nw/wsj/06/wsj_0641.parse 7 6 gold consider-v 29.9-1-1-1 Cogitation consider.01 null ----- 2:1-ARG0=Agent;Cognizer 6:0-rel 7:2-ARG1=Theme;Topic 
nw/wsj/06/wsj_0641.parse 7 8 gold move-v 11.2 Cause_motion move.01 null ----- 2:1*7:1-ARG0 8:0-rel 9:1-ARG1 17:1-ARGM-DIR 
nw/wsj/06/wsj_0641.parse 8 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/06/wsj_0641.parse 8 7 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 5:1*19:1-ARG0=Agent;Buyer 7:0-rel 8:1-ARG1=Theme;Goods 10:1-ARG2=Source 14:1-ARGM-LOC 
nw/wsj/06/wsj_0641.parse 8 21 gold begin-v 55.1-1 Activity_start begin.01 null ----- 5:1*19:1-ARG0=Agent;Agent 21:0-rel 22:1-ARG1=Theme;Activity 23:2-ARGM-TMP 
nw/wsj/06/wsj_0641.parse 9 4 gold expect-v 62 IN expect.01 null ----- 0:1,5:2-ARG1=Theme 4:0-rel 
nw/wsj/06/wsj_0641.parse 9 7 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:1*5:1-ARG0=Agent;Agent 7:0-rel 8:1-ARG1=Theme;Activity 9:1-ARGM-TMP 
nw/wsj/06/wsj_0641.parse 10 4 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 0:1-ARG0 3:0-ARGM-MOD 4:0-rel 5:1-ARG1 
nw/wsj/06/wsj_0641.parse 10 12 gold say-v 37.7-1 IN say.01 null ----- 0:2*14:1-ARG1=Topic 10:1-ARG0=Agent 12:0-rel 
