nw/wsj/24/wsj_2424.parse 0 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/24/wsj_2424.parse 0 6 gold terminate-v 55.4 Activity_stop terminate.01 1 ----- 5:1-ARG0=Agent;Agent 6:0-rel 7:2-ARG1=Theme;Activity 
nw/wsj/24/wsj_2424.parse 1 24 gold give-v 13.1-1 Giving give.01 2 ----- 0:2-ARG0=Agent;Donor 23:0-ARGM-NEG 24:0-rel 25:2-ARG1=Theme;Theme 
nw/wsj/24/wsj_2424.parse 1 34 gold acquire-v 13.5.2-1 Getting acquire.01 3 ----- 0:2*32:1-ARG0=Agent;Recipient 34:0-rel 35:1-ARG1=Theme;Theme 0:2*32:1-LINK-PRO 
nw/wsj/24/wsj_2424.parse 2 1 gold agree-v 36.1-1 IN agree.01 3 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARGM-TMP 4:2-ARG1=Theme 
nw/wsj/24/wsj_2424.parse 2 6 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 0:1*4:1-ARG0=Agent;Buyer 6:0-rel 7:1-ARG1=Theme;Goods 
nw/wsj/24/wsj_2424.parse 3 4 gold disclose-v 37.7 Reveal_secret disclose.01 1 ----- 0:1-ARG0=Agent;Speaker 2:0-ARGM-NEG 3:1-ARGM-ADV 4:0-rel 5:1-ARG1=Topic;Information 9:1-ARGM-ADV 
nw/wsj/24/wsj_2424.parse 3 6 gold propose-v 37.7-1 Statement propose.01 null ----- 6:0-rel 7:0-ARG1=Topic;Message 
nw/wsj/24/wsj_2424.parse 3 24 gold value-v 54.4 Assessing value.01 1 ----- 10:2-ARG0=Agent;Assessor 24:0-rel 25:2-ARG1=Theme;Feature 28:1-ARG2=Value;Value 
nw/wsj/24/wsj_2424.parse 4 18 gold involve-v 86.2-1 NF involve.01 1 ----- 12:1*15:1*16:1-ARG1 18:0-rel 19:1-ARG2 12:1*15:1-LINK-SLC 
nw/wsj/24/wsj_2424.parse 4 21 gold make-v 26.1-1 IN make.01 2 ----- 12:1*20:1-ARG0=Agent 21:0-rel 22:1-ARG1=Product 12:1*20:1-LINK-PRO 
