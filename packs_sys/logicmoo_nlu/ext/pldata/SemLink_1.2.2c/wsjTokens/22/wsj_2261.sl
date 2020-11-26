nw/wsj/22/wsj_2261.parse 0 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/22/wsj_2261.parse 0 6 gold sign-v 13.5.3 NF sign.02 null ----- 5:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 
nw/wsj/22/wsj_2261.parse 0 12 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 5:1*10:1-ARG0=Agent;Seller 12:0-rel 13:2-ARG1=Theme;Goods 18:1-ARG2=Recipient 5:1*10:1-LINK-PRO 
nw/wsj/22/wsj_2261.parse 0 20 gold hold-v 15.1-1 Manipulation hold.01 null ----- 19:0-ARGM-MNR 20:0-rel 21:0,22:0-ARG1=Theme;Entity 
nw/wsj/22/wsj_2261.parse 1 3 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 0:1*4:1-ARG1=Topic;Information 2:0-ARGM-NEG 3:0-rel 
nw/wsj/22/wsj_2261.parse 1 11 gold say-v 37.7-1 IN say.01 null ----- 7:1-ARG0=Agent 11:0-rel 12:1-ARG1=Topic 
nw/wsj/22/wsj_2261.parse 1 19 gold get-v 13.5.1-1 IN get.01 null ----- 13:1*15:1*20:1-ARG1=Theme 16:1-ARG0=Agent 18:0-ARGM-MOD 19:0-rel 21:1-ARG3=Asset 13:1*15:1-LINK-SLC 
nw/wsj/22/wsj_2261.parse 1 31 gold expect-v 62 IN expect.01 null ----- 30:1-ARG1=Theme 31:0-rel 33:1-ARG0=Experiencer 
nw/wsj/22/wsj_2261.parse 1 42 gold expect-v 62 IN expect.01 null ----- 38:1-ARG0=Experiencer 42:0-rel 43:2-ARG1=Theme 
nw/wsj/22/wsj_2261.parse 3 14 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 14:0-rel 15:1-ARG1=Topic 
nw/wsj/22/wsj_2261.parse 3 18 gold complete-v 55.2 Activity_finish complete.01 null ----- 16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 
nw/wsj/22/wsj_2261.parse 3 21 gold announce-v 37.7-1 Statement announce.01 null ----- 20:0-ARGM-TMP 21:0-rel 22:0-ARG1=Topic;Message 
nw/wsj/22/wsj_2261.parse 3 25 gold divest-v 10.6 Emptying divest.01 2 ----- 0:2*23:1-ARG0=Agent;Agent/Cause 25:0-rel 26:1-ARG2=Source;Source 27:1-ARG1=Theme;Theme 0:2*23:1-LINK-PRO 
