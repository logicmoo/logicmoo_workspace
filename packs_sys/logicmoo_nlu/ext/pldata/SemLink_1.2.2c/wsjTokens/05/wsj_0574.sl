nw/wsj/05/wsj_0574.parse 0 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/05/wsj_0574.parse 0 5 gold agree-v 36.1-1 IN agree.01 null ----- 4:1*6:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/05/wsj_0574.parse 0 8 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 4:1*6:1-ARG0=Agent;Seller 8:0-rel 9:2-ARG1=Theme;Goods 25:1-ARG2=Recipient 
nw/wsj/05/wsj_0574.parse 1 4 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 0:1*5:1-ARG1=Topic;Information 3:0-ARGM-NEG 4:0-rel 
nw/wsj/05/wsj_0574.parse 3 1 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 0:1-ARG0=Agent;Recipient 1:0-rel 2:1-ARG1=Theme;Theme 6:1-ARGM-TMP 8:2-ARGM-TMP 
nw/wsj/05/wsj_0574.parse 3 10 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 8:1*23:1-ARGM-TMP 9:1-ARG0=Agent;Buyer 10:0-rel 11:2-ARG1=Theme;Goods 18:1-ARG3=Asset 
nw/wsj/05/wsj_0574.parse 4 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARGM-TMP 3:1-ARG1=Topic 
