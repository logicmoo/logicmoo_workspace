nw/wsj/23/wsj_2302.parse 0 4 gold reach-v 13.5.1 NF reach.01 null ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 
nw/wsj/23/wsj_2302.parse 0 8 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*6:1-ARG0=Agent;Seller 8:0-rel 9:1-ARG1=Theme;Goods 13:1-ARG2=Recipient 0:1*6:1-LINK-PRO 
nw/wsj/23/wsj_2302.parse 0 10 gold remain-v 47.1-1 Remainder remain.01 null ----- 10:0-rel 11:0,12:0-ARG1=Theme 
nw/wsj/23/wsj_2302.parse 0 19 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 14:1*15:1*16:1*20:1-ARG1=Topic;Information 18:0-ARGM-NEG 19:0-rel 14:1*15:1-LINK-SLC 
nw/wsj/23/wsj_2302.parse 1 2 gold bring-v 11.3-1 Bringing bring.01 null ----- 0:1-ARG0=Instrument 2:0-rel 3:1-ARG3 8:3-ARG1=Theme 
nw/wsj/23/wsj_2302.parse 1 17 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 8:2*12:1*18:1-ARG1=Theme;Goods 13:1-ARG0=Agent;Seller 17:0-rel 19:1-ARGM-TMP 21:1-ARGM-MNR 8:2*12:1-LINK-SLC 
nw/wsj/23/wsj_2302.parse 2 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/23/wsj_2302.parse 2 19 gold use-v 105 IN use.01 null ----- 4:2*20:1-ARG1 17:0-ARGM-MOD 19:0-rel 21:2-ARG2 
nw/wsj/23/wsj_2302.parse 2 30 gold result-v 48.1.1 NF result.01 1 ----- 28:1-ARG2=Location 30:0-rel 31:1-ARG1=Theme 
nw/wsj/23/wsj_2302.parse 3 3 gold announce-v 37.7-1 Statement announce.01 null ----- 0:1-ARGM-TMP 1:1-ARG0=Agent;Speaker 3:0-rel 4:1-ARG1=Topic;Message 
nw/wsj/23/wsj_2302.parse 3 7 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 5:1-ARG0=Agent;Seller 6:0-ARGM-MOD 7:0-rel 8:2-ARG1=Theme;Goods 15:1-ARGM-CAU 
