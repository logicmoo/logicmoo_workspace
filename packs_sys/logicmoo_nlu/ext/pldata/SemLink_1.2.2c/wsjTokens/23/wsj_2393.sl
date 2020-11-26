nw/wsj/23/wsj_2393.parse 0 3 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 0:1*1:1*2:1-ARG0=Agent;Buyer 3:0-rel 4:1-ARG1=Theme;Goods 5:1-ARGM-MNR 0:1*1:1-LINK-SLC 
nw/wsj/23/wsj_2393.parse 1 6 gold require-v 103 NF require.01 2 ----- 3:1-ARG0=Pivot 5:0-ARGM-MOD 6:0-rel 7:1-ARG2=Source 8:2-ARG1=Theme 
nw/wsj/23/wsj_2393.parse 1 10 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 7:1*8:1-ARG0=Agent;Seller 10:0-rel 11:1-ARG1=Theme;Goods 
nw/wsj/23/wsj_2393.parse 2 8 gold think-v 29.9-2 IN think.01 1 ----- 0:1-ARGM-TMP 4:1,9:2-ARG1 8:0-rel 
nw/wsj/23/wsj_2393.parse 2 12 gold contribute-v 13.2-1-1 NF contribute.01 2 ----- 4:1*9:1-ARG0=Agent 12:0-rel 13:1-ARG2=Recipient 
nw/wsj/23/wsj_2393.parse 3 5 gold occur-v 48.3 Event occur.01 1 ----- 0:1-ARGM-ADV 2:1-ARG1=Theme;Event 5:0-rel 6:2-ARGM-TMP 
nw/wsj/23/wsj_2393.parse 3 12 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 6:1*20:1-ARGM-TMP 7:2-ARG1=Patient;Item 12:0-rel 13:1-ARG4 
nw/wsj/23/wsj_2393.parse 4 13 gold satisfy-v 31.1 Experiencer_obj satisfy.01 1 ----- 7:1*10:1*11:1-ARG2 13:0-rel 14:1-ARG1=Experiencer;Experiencer 7:1*10:1-LINK-SLC 
nw/wsj/23/wsj_2393.parse 4 21 gold begin-v 55.1-1 Activity_start begin.01 2 ----- 0:1-ARGM-ADV 17:1-ARG0=Agent;Agent 20:0-ARGM-MOD 21:0-rel 22:2-ARG1=Theme;Activity 
nw/wsj/23/wsj_2393.parse 4 23 gold liquidate-v 42.1 Killing liquidate.01 2 ----- 17:1*22:1-ARG0=Agent;Killer/Cause 23:0-rel 24:1-ARG1=Patient;Victim 
nw/wsj/23/wsj_2393.parse 5 5 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/23/wsj_2393.parse 5 10 gold expect-v 62 IN expect.01 1 ----- 7:1-ARG0=Experiencer 9:0-ARGM-NEG 10:0-rel 11:2-ARG1=Theme 
nw/wsj/23/wsj_2393.parse 6 22 gold say-v 37.7-1 IN say.01 1 ----- 0:2*24:1-ARG1=Topic 15:2-ARG0=Agent 22:0-rel 
nw/wsj/23/wsj_2393.parse 7 8 gold expect-v 62 IN expect.01 1 ----- 0:2-ARG0=Experiencer 7:0-ARGM-NEG 8:0-rel 9:1-ARG1=Theme 23:1-ARGM-CAU 
nw/wsj/23/wsj_2393.parse 7 28 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 24:1-ARG0=Agent;Buyer 28:0-rel 29:1-ARG1=Theme;Goods 30:1-ARGM-MNR 
nw/wsj/23/wsj_2393.parse 7 35 gold say-v 37.7-1 IN say.01 1 ----- 0:3*37:1-ARG1=Topic 33:1-ARG0=Agent 35:0-rel 
nw/wsj/23/wsj_2393.parse 8 20 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 20:0-rel 21:1-ARG1=Topic 
nw/wsj/23/wsj_2393.parse 8 25 gold expect-v 62 IN expect.01 1 ----- 22:1-ARG0=Experiencer 24:0-ARGM-NEG 25:0-rel 26:2-ARG1=Theme 
nw/wsj/23/wsj_2393.parse 9 1 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/23/wsj_2393.parse 9 11 gold have-v 100 IN have.03 2 ----- 10:1-ARG0=Pivot 11:0-rel 12:2-ARG1=Theme 
nw/wsj/23/wsj_2393.parse 10 1 gold add-v 37.7 NF add.01 1 ----- 0:1-ARG0=Agent 1:0-rel 4:2*11:1-ARG1=Topic 
