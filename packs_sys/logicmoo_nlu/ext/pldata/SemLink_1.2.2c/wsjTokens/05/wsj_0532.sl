nw/wsj/05/wsj_0532.parse 1 26 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 18:1*22:1*23:1-ARG0=Agent;Buyer 24:1-ARGM-TMP 26:0-rel 27:2-ARG1=Theme;Goods 18:1*22:1-LINK-SLC 
nw/wsj/05/wsj_0532.parse 2 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/05/wsj_0532.parse 2 8 gold know-v 29.5-1 IN know.01 1 ----- 5:1-ARG0=Agent 7:0-ARGM-NEG 8:0-rel 9:2-ARG1=Theme 
nw/wsj/05/wsj_0532.parse 3 8 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 8:0-rel 9:1-ARGM-LOC 12:1-ARG1=Topic 
nw/wsj/05/wsj_0532.parse 3 16 gold assume-v 93 Adopt_selection assume.01 null ----- 13:1-ARG0=Agent 14:0-ARGM-MOD 16:0-rel 17:2-ARG1=Theme 24:1-ARGM-TMP 
nw/wsj/05/wsj_0532.parse 3 28 gold name-v 29.3 IN name.01 null ----- 25:1*29:1-ARG1=Theme 28:0-rel 
nw/wsj/05/wsj_0532.parse 4 2 gold speculate-v 29.5-1 NF speculate.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 
nw/wsj/05/wsj_0532.parse 4 11 gold stem-v 48.1.1 NF stem.01 1 ----- 4:2-ARG1=Theme 9:0-ARGM-MOD 11:0-rel 12:1-ARG2 
nw/wsj/05/wsj_0532.parse 5 3 gold want-v 32.1-1-1 Desiring want.01 null ----- 0:1*4:1-ARG0=Pivot;Experiencer 3:0-rel 4:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/05/wsj_0532.parse 5 14 gold regret-v 31.2-1 Experiencer_focus regret.01 2 ----- 0:1*15:1-ARG0=Experiencer 12:0-ARGM-MOD 14:0-rel 15:2-ARG1=Attribute 
nw/wsj/05/wsj_0532.parse 5 16 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*15:1-ARG0=Agent;Seller 16:0-rel 17:1-ARG1=Theme;Goods 19:1-ARG2=Recipient 
nw/wsj/05/wsj_0532.parse 5 22 gold suggest-v 37.7-1-1 Statement suggest.01 null ----- 22:0-rel 23:1-ARG1=Topic;Message 25:2-ARG0=Agent;Speaker 
nw/wsj/05/wsj_0532.parse 6 5 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 0:1*3:1-ARG0=Agent;Speaker 5:0-rel 
nw/wsj/05/wsj_0532.parse 7 7 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-LOC 5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 
nw/wsj/05/wsj_0532.parse 7 11 gold leave-v 51.2-1 Quitting leave.01 null ----- 5:1*10:1-ARG0=Theme 11:0-rel 12:1-ARG1=Initial_Location 5:1*10:1-LINK-PRO 
nw/wsj/05/wsj_0532.parse 7 21 gold add-v 37.7 NF add.01 null ----- 0:1-ARGM-LOC 5:1-ARG0=Agent 21:0-rel 22:1-ARG1=Topic 
nw/wsj/05/wsj_0532.parse 9 6 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-TMP 4:1-ARG0=Agent 6:0-rel 7:1-ARG1=Topic 
nw/wsj/05/wsj_0532.parse 10 2 gold stun-v 31.1 Experiencer_obj stun.01 2 ----- 0:0-ARGM-DIS 1:1*8:1-ARG0=Stimulus;Stimulus 2:0-rel 3:1-ARG1=Experiencer;Experiencer 5:1-ARGM-TMP 7:1-ARG2 
nw/wsj/05/wsj_0532.parse 10 9 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 1:1*8:1-ARG0=Agent;Speaker 9:0-rel 10:1-ARG1=Topic;Information 
nw/wsj/05/wsj_0532.parse 10 12 gold expect-v 62 IN expect.01 null ----- 11:1-ARG0=Experiencer 12:0-rel 13:2-ARG1=Theme 
nw/wsj/05/wsj_0532.parse 10 15 gold post-v 11.1 Sending post.01 null ----- 13:1-ARG0=Agent;Sender 15:0-rel 16:2-ARG1=Theme;Theme 
nw/wsj/05/wsj_0532.parse 11 6 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-LOC 4:1-ARG0=Agent 5:1-ARGM-DIS 6:0-rel 7:1-ARG1=Topic 
nw/wsj/05/wsj_0532.parse 11 14 gold hire-v 13.5.1 Hiring hire.01 null ----- 8:1-ARG0=Agent;Employer 14:0-rel 15:1-ARG1=Theme;Employee 
nw/wsj/05/wsj_0532.parse 12 3 gold spend-v 66-1 NF spend.02 null ----- 0:1-ARG0=Agent 3:0-rel 4:2-ARG1=Asset 8:1-ARGM-TMP 11:1-ARGM-LOC 
nw/wsj/05/wsj_0532.parse 12 17 gold eliminate-v 10.1 Removing eliminate.01 null ----- 16:1-ARG0=Agent;Agent/Cause 17:0-rel 18:1-ARG1=Theme;Theme 
nw/wsj/05/wsj_0532.parse 12 21 gold fire-v 10.10 Firing fire.02 null ----- 16:1-ARG0=Agent 21:0-rel 22:1-ARG1=Theme 
nw/wsj/05/wsj_0532.parse 13 8 gold indicate-v 78-1 Communication indicate.01 null ----- 0:2*10:1-ARG1=Topic;Message 6:1-ARG0=Cause;Communicator 8:0-rel 
nw/wsj/05/wsj_0532.parse 14 8 gold make-v 26.1-1 Intentionally_create make.01 null ----- 3:1*7:1-ARG0=Agent;Creator 8:0-rel 9:1-ARG1=Product;Created_entity 10:1-ARGM-LOC 3:1*7:1-LINK-PRO 
nw/wsj/05/wsj_0532.parse 15 1 gold blame-v 33 IN blame.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 13:1-ARG2=Attribute 
nw/wsj/05/wsj_0532.parse 16 4 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 3:1*5:1-ARG1=Theme;Goods 4:0-rel 6:1-ARG2=Recipient 3:1*5:1-LINK-PSV 
nw/wsj/05/wsj_0532.parse 18 3 gold leave-v 51.2-1 Quitting leave.01 null ----- 0:1*4:1-ARG0=Theme 2:1-ARGM-TMP 3:0-rel 4:2-ARGM-PRP 
nw/wsj/05/wsj_0532.parse 19 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/05/wsj_0532.parse 19 8 gold hold-v 15.1-1 Manipulation hold.01 null ----- 5:1-ARG0=Agent;Agent 7:1-ARGM-TMP 8:0-rel 9:2-ARG1=Theme;Entity 
nw/wsj/05/wsj_0532.parse 19 16 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 9:1*17:1-ARG1=Theme;Theme 16:0-rel 18:1-ARGM-MNR 9:1*17:1-LINK-PSV 
nw/wsj/05/wsj_0532.parse 20 12 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 null ----- 0:1-ARGM-LOC 3:1-ARGM-LOC 8:1-ARGM-TMP 10:1-ARG1=Patient;Item 12:0-rel 13:1-ARG2=Extent;Difference 15:1-ARG4 
