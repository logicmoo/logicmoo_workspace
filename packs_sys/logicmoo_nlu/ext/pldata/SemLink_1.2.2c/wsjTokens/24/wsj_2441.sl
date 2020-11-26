nw/wsj/24/wsj_2441.parse 0 6 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/24/wsj_2441.parse 0 10 gold ask-v 58.2 Request ask.02 2 ----- 8:1-ARG0 9:0-ARGM-MOD 10:0-rel 11:1-ARG2 15:2-ARG1 
nw/wsj/24/wsj_2441.parse 0 17 gold allow-v 64 IN allow.01 1 ----- 11:1*15:1-ARG0 17:0-rel 18:2-ARG1 
nw/wsj/24/wsj_2441.parse 0 20 gold hire-v 13.5.1 Hiring hire.01 1 ----- 18:1-ARG0=Agent;Employer 20:0-rel 21:2-ARG1=Theme;Employee 25:2-ARG2 
nw/wsj/24/wsj_2441.parse 0 27 gold help-v 72-1 Assistance help.01 1 ----- 21:2*25:1-ARG0=Agent;Helper 27:0-rel 28:1-ARG2=Beneficiary;Benefited_party 29:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/24/wsj_2441.parse 0 30 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 28:1*29:1-ARG0=Agent;Seller 30:0-rel 31:1-ARG1=Theme;Goods 
nw/wsj/24/wsj_2441.parse 1 2 gold assist-v 72-1 Assistance assist.01 1 ----- 0:1*3:1-ARG1=Beneficiary;Benefited_party 2:0-rel 4:1-ARG0=Agent;Helper 0:1*3:1-LINK-PSV 
nw/wsj/24/wsj_2441.parse 1 12 gold try-v 61 Attempt try.01 1 ----- 0:2-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 21:1-ARGM-TMP 25:2-ARGM-MNR 
nw/wsj/24/wsj_2441.parse 1 15 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 0:2*13:1-ARG0=Agent;Seller 15:0-rel 16:1-ARG1=Theme;Goods 
nw/wsj/24/wsj_2441.parse 2 8 gold hope-v 32.2-1 Desiring hope.01 1 ----- 0:1-ARG0=Pivot 8:0-rel 9:2-ARG1=Theme 
nw/wsj/24/wsj_2441.parse 2 11 gold use-v 105 IN use.01 1 ----- 0:1*9:1-ARG0 11:0-rel 12:2-ARG1 16:2-ARG2 
nw/wsj/24/wsj_2441.parse 2 18 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 1 ----- 0:1*9:1*16:1-ARG0=Cause 18:0-rel 19:1-ARG1=Patient 
nw/wsj/24/wsj_2441.parse 3 10 gold reach-v 13.5.1 NF reach.01 1 ----- 0:1*8:1*18:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 0:1*8:1-LINK-PRO 
nw/wsj/24/wsj_2441.parse 4 1 gold file-v 9.10 Submitting_documents file.02 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Theme 6:1-ARGM-MNR 14:1-ARGM-TMP 16:2-ARGM-PNC 
nw/wsj/24/wsj_2441.parse 4 18 gold give-v 13.1-1 Giving give.01 3 ----- 16:1-ARG0=Agent;Donor 18:0-rel 19:1-ARG2=Recipient;Recipient 20:2-ARG1=Theme;Theme 
nw/wsj/24/wsj_2441.parse 4 25 gold work-v 73-3 IN work.01 1 ----- 23:1-ARG0=Agent 25:0-rel 26:1-ARG1=Theme 
nw/wsj/24/wsj_2441.parse 5 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/24/wsj_2441.parse 5 8 gold own-v 100 NF own.01 1 ----- 3:1*6:1*7:1-ARG0=Pivot 8:0-rel 9:2-ARG1=Theme 3:1*6:1-LINK-SLC 
nw/wsj/24/wsj_2441.parse 5 17 gold continue-v 55.3 Activity_ongoing continue.01 1 ----- 3:2-ARG1 15:0-ARGM-MOD 16:0-ARGM-NEG 17:0-rel 18:1-ARGM-MNR 23:1-ARGM-CAU 
nw/wsj/24/wsj_2441.parse 6 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/24/wsj_2441.parse 6 4 gold choose-v 13.5.1 Choosing choose.01 1 ----- 3:1-ARG0=Agent;Cognizer 4:0-rel 5:1-ARG1=Theme;Chosen 6:1,8:1-ARGM-CAU 
