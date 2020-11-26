nw/wsj/22/wsj_2237.parse 0 11 gold send-v 11.1-1 Sending send.01 null ----- 9:1-ARG0=Agent;Sender 10:0-ARGM-NEG 11:0-rel 12:1-ARG1=Theme;Theme 14:1-ARG2=Destination;Goal/Recipient 
nw/wsj/22/wsj_2237.parse 0 20 gold get-v 26.6.2 IN get.03 null ----- 18:1*22:1-ARG1=Patient 20:0-rel 21:1-ARG2=Goal 
nw/wsj/22/wsj_2237.parse 0 24 gold send-v 11.1-1 Sending send.01 null ----- 18:1*22:1-ARG0=Agent;Sender 24:0-rel 25:2-ARG1=Theme;Theme 
nw/wsj/22/wsj_2237.parse 1 5 gold vote-v 29.3 NF vote.01 null ----- 0:1-ARGM-TMP 3:1*6:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/22/wsj_2237.parse 1 8 gold send-v 11.1-1 Sending send.01 null ----- 3:1*6:1-ARG0=Agent;Sender 8:0-rel 9:2*16:1-ARG1=Theme;Theme 14:1-ARG2=Destination;Goal/Recipient 16:2-ARGM-PRP 
nw/wsj/22/wsj_2237.parse 1 18 gold assist-v 72-1 Assistance assist.01 1 ----- 9:2*16:1-ARG0=Agent;Helper 18:0-rel 19:2-ARG1=Beneficiary;Benefited_party 25:1-ARG2=Theme;Goal/Focal_entity 
nw/wsj/22/wsj_2237.parse 2 3 gold call-v 29.3 IN call.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1*6:1-ARG1=Theme 6:2-ARG2=Result 
nw/wsj/22/wsj_2237.parse 4 12 gold help-v 72-1 Assistance help.01 null ----- 2:2*10:1*11:1*13:1-ARG0=Agent;Helper 12:0-rel 13:2-ARG1=Theme;Goal/Focal_entity 2:2*10:1-LINK-SLC 
nw/wsj/22/wsj_2237.parse 4 30 gold claim-v 37.7-1 Statement claim.01 null ----- 19:1*28:1*29:1-ARG0=Agent;Speaker 30:0-rel 31:2-ARG1=Topic;Message 19:1*28:1-LINK-SLC 
nw/wsj/22/wsj_2237.parse 5 4 gold explain-v 37.1.1 Justifying explain.01 null ----- 2:1-ARG0=Agent;Agent 4:0-rel 5:1-ARG1=Topic;State_of_affairs/Act 7:1-ARG2=Recipient 
