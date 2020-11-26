nw/wsj/20/wsj_2006.parse 0 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/20/wsj_2006.parse 0 8 gold agree-v 36.1-1 IN agree.01 null ----- 5:1*9:1-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 
nw/wsj/20/wsj_2006.parse 0 19 gold hold-v 15.1-1 Manipulation hold.01 null ----- 18:0-ARG1=Theme;Entity 19:0-rel 20:0-ARG0=Agent;Agent 
nw/wsj/20/wsj_2006.parse 1 19 gold say-v 37.7-1 IN say.01 null ----- 1:2*20:1-ARG1=Topic 17:1-ARG0=Agent 19:0-rel 
nw/wsj/20/wsj_2006.parse 2 5 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 0:1*3:1-ARG0=Agent;Speaker 5:0-rel 
nw/wsj/20/wsj_2006.parse 3 6 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-LOC 4:1-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/20/wsj_2006.parse 3 13 gold call-v 60 IN call.03 null ----- 8:2-ARG0=Agent 13:0-rel 14:1-ARG1=Topic 
nw/wsj/20/wsj_2006.parse 3 17 gold act-v 29.6-1 NF act.01 null ----- 15:1-ARG0=Agent 17:0-rel 18:1-ARG1=Attribute 24:1-ARGM-TMP 
nw/wsj/20/wsj_2006.parse 4 3 gold remain-v 47.1-1 State_continue remain.01 null ----- 0:1-ARG1=Theme 1:0-ARGM-MOD 2:1-ARGM-DIS 3:0-rel 4:1-ARG3 
nw/wsj/20/wsj_2006.parse 4 9 gold say-v 37.7-1 IN say.01 null ----- 7:1-ARG0=Agent 9:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/20/wsj_2006.parse 4 17 gold serve-v 29.6-2 Assistance serve.01 null ----- 14:1-ARG0=Agent 15:0-ARGM-MOD 16:0-ARGM-NEG 17:0-rel 18:1-ARG1=Attribute 
nw/wsj/20/wsj_2006.parse 6 8 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-LOC 5:1-ARG0=Agent 7:1-ARGM-DIS 8:0-rel 9:1-ARG1=Topic 
nw/wsj/20/wsj_2006.parse 6 12 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 10:1-ARG0=Agent;Buyer 12:0-rel 13:1-ARGM-ADV 14:2-ARG1=Theme;Goods 19:1-ARGM-LOC 
nw/wsj/20/wsj_2006.parse 7 3 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 0:1*4:1-ARG1=Topic;Information 2:0-ARGM-NEG 3:0-rel 
nw/wsj/20/wsj_2006.parse 8 14 gold base-v 97.1 NF base.02 null ----- 5:3-ARG1=Theme 14:0-rel 15:1-ARG2=Source 
