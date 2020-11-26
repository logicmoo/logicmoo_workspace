nw/wsj/23/wsj_2303.parse 0 10 gold say-v 37.7-1 IN say.01 null ----- 0:3-ARG0=Agent 10:0-rel 11:1-ARG1=Topic 
nw/wsj/23/wsj_2303.parse 1 6 gold include-v 65 NF include.01 null ----- 0:1-ARG2=Location 6:0-rel 7:2-ARG1=Theme 23:1-ARGM-ADV 
nw/wsj/23/wsj_2303.parse 2 3 gold lead-v 51.7 Cotheme lead.02 null ----- 0:1*4:1-ARG1=Theme;Cotheme 3:0-rel 5:1-ARG0=Agent;Theme 
nw/wsj/23/wsj_2303.parse 3 12 gold claim-v 37.7-1 Statement claim.01 null ----- 0:2*13:1-ARG0=Agent;Speaker 12:0-rel 13:2-ARG1=Topic;Message 
nw/wsj/23/wsj_2303.parse 3 15 gold have-v 100 IN have.03 null ----- 0:2*13:1-ARG0=Pivot 15:0-rel 16:3-ARG1=Theme 
nw/wsj/23/wsj_2303.parse 4 8 gold require-v 103 NF require.01 null ----- 7:1-ARG0=Pivot 8:0-rel 9:2-ARG1=Theme 
nw/wsj/23/wsj_2303.parse 4 18 gold provide-v 13.4.1-2 Supply provide.01 null ----- 15:1*19:1-ARG1=Theme 16:0-ARGM-MOD 18:0-rel 20:1-ARGM-LOC 
nw/wsj/23/wsj_2303.parse 4 27 gold say-v 37.7-1 IN say.01 null ----- 27:0-rel 28:1-ARG1=Topic 30:1-ARG0=Agent 32:1-ARGM-LOC 
nw/wsj/23/wsj_2303.parse 5 6 gold build-v 26.1-1 Building build.01 null ----- 4:1-ARG0=Agent;Agent 6:0-rel 
nw/wsj/23/wsj_2303.parse 5 8 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 4:1-ARG0=Agent;Seller 8:0-rel 
nw/wsj/23/wsj_2303.parse 6 1 gold want-v 32.1-1-1 Desiring want.01 null ----- 0:1*2:1-ARG0=Pivot;Experiencer 1:0-rel 2:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/23/wsj_2303.parse 6 4 gold build-v 26.1-1 Building build.01 null ----- 0:1*2:1-ARG0=Agent;Agent 4:0-rel 
nw/wsj/23/wsj_2303.parse 6 6 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1*2:1-ARG0=Agent;Agent 6:0-rel 
nw/wsj/23/wsj_2303.parse 9 9 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 9:0-rel 10:1-ARGM-TMP 11:1-ARGM-LOC 14:2-ARG1=Topic 
nw/wsj/23/wsj_2303.parse 9 19 gold see-v 30.1-1 Perception_experience see.01 null ----- 15:1-ARG0=Experiencer;Perceiver_passive 17:0-ARGM-NEG 18:1-ARGM-TMP 19:0-rel 20:1-ARG1=Stimulus;Phenomenon 
nw/wsj/23/wsj_2303.parse 9 26 gold review-v 34.1 NF review.01 1 ----- 24:1-ARG0=Agent 25:0-ARGM-MOD 26:0-rel 27:1-ARG1=Theme 
nw/wsj/23/wsj_2303.parse 9 29 gold bring-v 11.3-1 Bringing bring.01 null ----- 24:1-ARG0=Instrument 25:0-ARGM-MOD 29:0-rel 30:1-ARG1=Theme 31:1-ARG3 
nw/wsj/23/wsj_2303.parse 10 7 gold estimate-v 54.4 Estimating estimate.01 1 ----- 0:1*8:1-ARG1=Theme 7:0-rel 9:1-ARG0=Agent 12:1-ARG2=Value 
nw/wsj/23/wsj_2303.parse 11 1 gold include-v 65 NF include.01 null ----- 0:0-ARGM-NEG 1:0-rel 2:1*8:3-ARG1=Theme 3:1-ARG2=Location 8:3*2:1-LINK-PSV 
nw/wsj/23/wsj_2303.parse 13 19 gold expect-v 62 IN expect.01 null ----- 15:1,20:2-ARG1=Theme 19:0-rel 
nw/wsj/23/wsj_2303.parse 14 3 gold include-v 65 NF include.01 null ----- 0:1*4:1-ARG1=Theme 3:0-rel 0:1*4:1-LINK-PSV 
nw/wsj/23/wsj_2303.parse 14 16 gold expect-v 62 IN expect.01 null ----- 6:2*17:2-ARG1=Theme 16:0-rel 
nw/wsj/23/wsj_2303.parse 14 20 gold complete-v 55.2 Activity_finish complete.01 null ----- 17:1*21:1-ARG1=Theme 20:0-rel 22:1-ARGM-TMP 
nw/wsj/23/wsj_2303.parse 16 17 gold say-v 37.7-1 IN say.01 null ----- 1:2-ARG1=Topic 17:0-rel 18:2-ARG0=Agent 31:1-ARGM-LOC 
nw/wsj/23/wsj_2303.parse 17 3 gold act-v 29.6-1 NF act.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Attribute 
nw/wsj/23/wsj_2303.parse 18 47 gold name-v 29.3 IN name.01 null ----- 44:1*48:1-ARG1=Theme 47:0-rel 49:2-ARG2=Result 44:1*48:1-LINK-PRO 
nw/wsj/23/wsj_2303.parse 19 19 gold hire-v 13.5.1 Hiring hire.01 null ----- 14:1*15:1*28:1-ARGM-TMP 16:1*20:1-ARG1=Theme;Employee 19:0-rel 21:2-ARG2 14:1*15:1-LINK-SLC 
nw/wsj/23/wsj_2303.parse 19 23 gold push-v 59 Cause_motion push.02 null ----- 16:1*20:1*21:1-ARG0=Agent 23:0-rel 24:1-ARG1=Patient 25:1-ARG2=Result 
nw/wsj/23/wsj_2303.parse 20 15 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 5:1*7:1*27:1-ARGM-TMP 8:2*16:1-ARG1=Theme;Theme 15:0-rel 17:1-ARG0=Agent;Recipient 5:1*7:1-LINK-SLC 
nw/wsj/23/wsj_2303.parse 21 2 gold proceed-v 55.1 NF proceed.01 3 ----- 0:1*3:1-ARG0=Agent 2:0-rel 3:2-ARG1=Theme 
nw/wsj/23/wsj_2303.parse 21 5 gold launch-v 55.5-1 IN launch.01 null ----- 0:1*3:1-ARG0 5:0-rel 6:2-ARG1 
nw/wsj/23/wsj_2303.parse 21 22 gold include-v 65 NF include.01 null ----- 6:1*20:1*21:1-ARG2=Location 22:0-rel 23:3-ARG1=Theme 6:1*20:1-LINK-SLC 
nw/wsj/23/wsj_2303.parse 22 6 gold have-v 100 IN have.03 null ----- 0:1-ARGM-TMP 1:2-ARG0=Pivot 6:0-rel 7:1-ARG1=Theme 10:1-ARGM-MNR 
nw/wsj/23/wsj_2303.parse 22 20 gold say-v 37.7-1 IN say.01 null ----- 18:1-ARG0=Agent 20:0-rel 21:1-ARG1=Topic 
nw/wsj/23/wsj_2303.parse 23 6 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARGM-TMP 3:1*22:1-ARG0=Agent;Seller 6:0-rel 7:2-ARG1=Theme;Goods 13:1-ARG2=Recipient 
nw/wsj/23/wsj_2303.parse 23 21 gold attempt-v 61 Attempt attempt.01 null ----- 0:1-ARGM-TMP 3:1*22:1-ARG0=Agent 20:1-ARGM-TMP 21:0-rel 22:2-ARG1=Theme 
nw/wsj/23/wsj_2303.parse 23 24 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 3:1*22:1-ARG0=Agent;Seller 24:0-rel 25:1-ARG1=Theme;Goods 
nw/wsj/23/wsj_2303.parse 24 19 gold buy-v 13.5.1 Commerce_buy buy.03 null ----- 15:1*16:1*17:1-ARG0=Agent;Buyer 19:0,20:1-rel 21:2-ARG1=Theme;Goods 15:1*16:1-LINK-SLC 
nw/wsj/23/wsj_2303.parse 25 6 gold offer-v 13.3 NF offer.01 null ----- 0:1*7:1-ARG1=Theme 4:1-ARGM-TMP 6:0-rel 8:1-ARGM-PNC 10:1-ARG0=Agent 
nw/wsj/23/wsj_2303.parse 26 8 gold say-v 37.7-1 IN say.01 null ----- 0:1*2:1*6:1-ARG0=Agent 0:2-ARGM-PRD 8:0-rel 9:1-ARG1=Topic 
nw/wsj/23/wsj_2303.parse 26 11 gold believe-v 29.5-1 Awareness believe.01 null ----- 10:1-ARG0 11:0-rel 12:1-ARG1 
nw/wsj/23/wsj_2303.parse 27 9 gold have-v 100 IN have.03 null ----- 8:1-ARG0=Pivot 9:0-rel 10:2-ARG1=Theme 
nw/wsj/23/wsj_2303.parse 27 20 gold say-v 37.7-1 IN say.01 null ----- 1:3*21:1-ARG1=Topic 20:0-rel 22:1-ARG0=Agent 
