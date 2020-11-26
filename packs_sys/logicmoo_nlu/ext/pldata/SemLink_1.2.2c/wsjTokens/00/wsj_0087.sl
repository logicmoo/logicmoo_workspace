nw/wsj/00/wsj_0087.parse 1 3 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 2:1-ARG0=Agent;Buyer 3:0-rel 4:1-ARG1=Theme;Goods 
nw/wsj/00/wsj_0087.parse 3 3 gold send-v 11.1-1 Sending send.01 null ----- 2:1-ARG0=Agent;Sender 3:0-rel 4:1-ARG1=Theme;Theme 6:1-ARG2=Destination;Goal/Recipient 
nw/wsj/00/wsj_0087.parse 4 3 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 2:1-ARG0=Agent;Buyer 3:0-rel 4:1-ARG1=Theme;Goods 
nw/wsj/00/wsj_0087.parse 5 3 gold make-v 26.1-1 IN make.01 null ----- 2:1-ARG0=Agent 3:0-rel 4:2-ARG1=Product 20:1-ARGM-MNR 
nw/wsj/00/wsj_0087.parse 6 13 gold think-v 29.9-2 IN think.01 null ----- 10:1-ARG0 12:0-ARGM-NEG 13:0-rel 14:1-ARG1 15:1-ARG2 
nw/wsj/00/wsj_0087.parse 7 3 gold ail-v 40.8.1 Perception_body ail.01 null ----- 3:0-rel 4:1,9:0-ARG1=Experiencer;Experiencer 
nw/wsj/00/wsj_0087.parse 7 11 gold teeter-v 47.3 NF teeter.01 null ----- 2:1-ARG1=Theme 11:0-rel 12:1-ARGM-LOC 
nw/wsj/00/wsj_0087.parse 7 17 gold lead-v 59 Causation lead.03 null ----- 0:0-ARGM-DIS 1:1-ARGM-ADV 15:1-ARG0=Agent 16:0-ARGM-MOD 17:0-rel 18:2-ARG2=Result 
nw/wsj/00/wsj_0087.parse 8 5 gold help-v 72-1 Assistance help.01 null ----- 0:1*6:1-ARG0=Agent;Helper 4:0-ARGM-MOD 5:0-rel 6:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/00/wsj_0087.parse 8 7 gold keep-v 55.6 Cause_to_continue keep.02 null ----- 0:1*6:1-ARG0=Agent 7:0-rel 13:2-ARG1=Theme 
nw/wsj/00/wsj_0087.parse 9 9 gold help-v 72-1 Assistance help.01 null ----- 0:1-ARGM-ADV 6:1-ARG0=Agent;Helper 7:0-ARGM-MOD 9:0-rel 10:1-ARG1=Theme;Goal/Focal_entity 13:1-ARGM-LOC 
nw/wsj/00/wsj_0087.parse 12 0 gold perform-v 26.7-1 NF perform.01 null ----- 0:0-rel 1:0-ARG0=Agent 
nw/wsj/00/wsj_0087.parse 13 7 gold give-v 13.1-1 Giving give.01 null ----- 0:1-ARGM-TMP 5:1-ARG0=Agent;Donor 6:0-ARGM-MOD 7:0-rel 8:1-ARG2=Recipient;Recipient 12:3-ARG1=Theme;Theme 
nw/wsj/00/wsj_0087.parse 13 24 gold want-v 32.1-1-1 Desiring want.01 null ----- 19:1*22:1*23:1*25:1-ARG0=Pivot;Experiencer 24:0-rel 25:2-ARG1=Theme;Event/Focal_participant 19:1*22:1-LINK-SLC 
nw/wsj/00/wsj_0087.parse 14 8 gold experience-v 30.2 Feeling experience.01 1 ----- 0:1-ARGM-PRD 6:1-ARG0=Experiencer;Experiencer 7:0-ARGM-MOD 8:0-rel 9:2-ARG1=Stimulus;Emotion/Emotional_state 
nw/wsj/00/wsj_0087.parse 14 12 gold feel-v 30.1-1 Feeling feel.01 null ----- 9:1*13:1-ARG1=Stimulus 12:0-rel 14:1-ARG0=Experiencer 9:1*13:1-LINK-PSV 
nw/wsj/00/wsj_0087.parse 14 28 gold find-v 13.5.1 IN find.01 null ----- 15:2*26:1*27:1-ARG0=Agent 28:0-rel 29:2-ARG1=Theme 15:2*26:1-LINK-SLC 
nw/wsj/00/wsj_0087.parse 14 33 gold help-v 72-1 Assistance help.01 null ----- 15:2*31:1-ARG0=Agent;Helper 29:1*30:1-ARGM-MNR 33:0-rel 34:2-ARG2=Beneficiary;Benefited_party 40:1-ARG1=Theme;Goal/Focal_entity 15:2*31:1-LINK-PRO 29:1*30:1-LINK-SLC 
nw/wsj/00/wsj_0087.parse 14 34 gold trouble-v 31.1 Experiencer_obj trouble.01 null ----- 34:0-rel 35:0,36:0-ARG1=Experiencer;Experiencer 
nw/wsj/00/wsj_0087.parse 14 40 gold help-v 72-1 Assistance help.01 null ----- 34:2-ARG0=Agent;Helper 40:0-rel 41:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/00/wsj_0087.parse 15 9 gold mention-v 37.7-1 Statement mention.01 1 ----- 7:0-ARGM-NEG 9:0-rel 10:1-ARG1=Topic;Message 
nw/wsj/00/wsj_0087.parse 16 3 gold wait-v 47.1-1 IN wait.01 null ----- 0:1-ARG1=Theme 2:0-ARGM-NEG 3:0-rel 
nw/wsj/00/wsj_0087.parse 16 8 gold need-v 32.1-1-1 Needing need.01 null ----- 5:1-ARG0=Pivot 8:0-rel 9:1-ARG1=Theme 11:1-ARGM-TMP 
nw/wsj/00/wsj_0087.parse 17 4 gold delay-v 53.1-1 IN delay.01 1 ----- 0:1*2:1*5:1-ARGM-TMP 3:1-ARG0=Agent 4:0-rel 0:1*2:1-LINK-SLC 
nw/wsj/00/wsj_0087.parse 17 19 gold grow-v 48.1.1 NF grow.02 null ----- 0:2-ARGM-TMP 7:3-ARG1=Theme 19:0-rel 20:1-ARG2 
nw/wsj/00/wsj_0087.parse 18 1 gold think-v 29.9-2 IN think.01 null ----- 0:1-ARG0 1:0-rel 2:1-ARG2 
nw/wsj/00/wsj_0087.parse 19 2 gold send-v 11.1-1 Sending send.01 null ----- 0:0-ARGM-ADV 1:1-ARG0=Agent;Sender 2:0-rel 3:1-ARG1=Theme;Theme 5:1-ARG2=Destination;Goal/Recipient 
nw/wsj/00/wsj_0087.parse 20 6 gold send-v 11.1-1 Sending send.01 null ----- 0:1*4:1*7:1-ARG1=Theme;Theme 5:1-ARG0=Agent;Sender 6:0-rel 0:1*4:1-LINK-SLC 
nw/wsj/00/wsj_0087.parse 20 9 gold go-v 47.7 Motion go.01 null ----- 0:2-ARG1=Theme;Theme 8:0-ARGM-MOD 9:0-rel 10:1-ARG2 13:2-ARGM-PRP 
nw/wsj/00/wsj_0087.parse 20 15 gold boost-v 102 NF boost.01 1 ----- 0:2*13:1-ARG0=Agent 15:0-rel 16:1-ARG1=Theme 0:2*13:1-LINK-PRO 
nw/wsj/00/wsj_0087.parse 20 16 gold sag-v 47.6 NF sag.01 null ----- 16:0-rel 17:2-ARG1=Theme 
nw/wsj/00/wsj_0087.parse 20 24 gold keep-v 55.6 Cause_to_continue keep.04 null ----- 0:2*13:1-ARG0=Agent 24:0-rel 25:1-ARG1=Theme 29:1-ARG2 0:2*13:1-LINK-PRO 
