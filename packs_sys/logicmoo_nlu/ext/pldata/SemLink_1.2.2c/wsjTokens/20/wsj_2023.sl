nw/wsj/20/wsj_2023.parse 0 4 gold expect-v 62 IN expect.01 null ----- 4:0-rel 5:2*15:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 0 7 gold reach-v 13.5.1 NF reach.01 null ----- 0:1*5:1-ARG0=Agent 7:0-rel 8:2*15:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 0 17 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1*15:1-ARG0=Agent;Seller 17:0-rel 18:1-ARG1=Theme;Goods 21:1-ARG2=Recipient 0:1*15:1-LINK-PRO 
nw/wsj/20/wsj_2023.parse 0 24 gold say-v 37.7-1 IN say.01 null ----- 24:0-rel 25:1-ARG1=Topic 27:2-ARG0=Agent 
nw/wsj/20/wsj_2023.parse 1 8 gold file-v 9.10 Submitting_documents file.02 null ----- 0:2-ARG0=Agent 8:0-rel 9:1-ARG1=Theme 14:2-ARGM-TMP 
nw/wsj/20/wsj_2023.parse 3 2 gold expect-v 62 IN expect.01 null ----- 2:0-rel 4:1-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 3 20 gold provide-v 13.4.1-2 Supply provide.01 null ----- 5:2-ARG0=Agent 19:0-ARGM-MOD 20:0-rel 21:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 3 27 gold propose-v 37.7-1 Statement propose.01 null ----- 27:0-rel 28:0,29:0,31:0-ARG1=Topic;Message 
nw/wsj/20/wsj_2023.parse 4 8 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 0:2*6:1-ARG0=Agent;Speaker 8:0-rel 
nw/wsj/20/wsj_2023.parse 5 2 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 6 2 gold provide-v 13.4.1-2 Supply provide.01 null ----- 0:1*16:1-ARG0=Agent 2:0-rel 3:3-ARG1=Theme 10:1-ARG2=Recipient 
nw/wsj/20/wsj_2023.parse 6 15 gold agree-v 36.1-1 IN agree.01 null ----- 0:1*16:1-ARG0=Agent 15:0-rel 16:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 6 18 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*16:1-ARG0=Agent;Buyer 18:0-rel 19:3-ARG1=Theme;Goods 28:1-ARG2=Source 
nw/wsj/20/wsj_2023.parse 6 42 gold own-v 100 NF own.01 null ----- 41:0-ARGM-EXT 42:0-rel 43:1,46:0-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 7 27 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 0:3*25:1-ARG0=Agent;Speaker 27:0-rel 
nw/wsj/20/wsj_2023.parse 8 4 gold own-v 100 NF own.01 null ----- 0:1-ARGM-TMP 2:1-ARG0=Pivot 4:0-rel 5:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 8 13 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 11:1-ARG0=Agent;Recipient 13:0-rel 14:2-ARG1=Theme;Theme 21:1-ARGM-TMP 
nw/wsj/20/wsj_2023.parse 9 7 gold have-v 100 IN have.03 null ----- 0:1-ARGM-TMP 4:1-ARG0=Pivot 7:0-rel 8:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 11 13 gold ask-v 60-1 Request ask.02 null ----- 0:1-ARGM-TMP 3:1-ARG0 13:0-rel 14:1*15:1-ARG2 15:2-ARG1 
nw/wsj/20/wsj_2023.parse 11 17 gold provide-v 13.4.1-2 Supply provide.01 null ----- 14:1*15:1-ARG0=Agent 17:0-rel 18:3-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 11 21 gold say-v 37.7-1 IN say.01 null ----- 18:2-ARG0=Agent 21:0-rel 22:1-ARG1=Topic 
nw/wsj/20/wsj_2023.parse 11 25 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 23:1-ARG0 24:0-ARGM-MOD 25:0-rel 26:2-ARG1 32:1-ARGM-TMP 
nw/wsj/20/wsj_2023.parse 11 27 gold ship-v 11.1-1 Sending ship.01 1 ----- 23:1*26:1-ARG0=Agent;Sender 27:0-rel 28:1-ARG1=Theme;Theme 29:1-ARG2=Destination;Goal/Recipient 
nw/wsj/20/wsj_2023.parse 11 38 gold say-v 37.7-1 IN say.01 null ----- 38:0-rel 39:1-ARG1=Topic 41:2-ARG0=Agent 
nw/wsj/20/wsj_2023.parse 12 10 gold fail-v 74-2 IN fail.01 null ----- 0:1-ARGM-TMP 3:2-ARG1 10:0-rel 12:2-ARGM-ADV 
nw/wsj/20/wsj_2023.parse 12 13 gold cause-v 27 Causation cause.01 1 ----- 12:1-ARG0=Cause;Cause 13:0-rel 14:1-ARG1=Theme;Effect 15:1-ARGM-LOC 
nw/wsj/20/wsj_2023.parse 12 22 gold want-v 32.1-1-1 Desiring want.01 null ----- 19:1-ARG0=Pivot;Experiencer 21:1-ARGM-ADV 22:0-rel 23:1-ARG1=Theme;Event/Focal_participant 
nw/wsj/20/wsj_2023.parse 12 28 gold have-v 100 IN have.03 null ----- 25:1-ARG0=Pivot 26:0-ARGM-MOD 27:0-ARGM-NEG 28:0-rel 29:1-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 13 3 gold end-v 55.4-1 Cause_to_end end.01 null ----- 1:1-ARG1=Theme 3:0-rel 4:1-ARGM-TMP 
nw/wsj/20/wsj_2023.parse 13 11 gold have-v 100 IN have.03 null ----- 0:1-ARGM-TMP 9:1-ARG0=Pivot 11:0-rel 12:4-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 15 10 gold include-v 65 NF include.01 null ----- 0:1-ARGM-ADV 8:1-ARG2=Location 10:0-rel 11:3-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 16 4 gold advise-v 37.9-1 IN advise.01 1 ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Recipient 7:1-ARG2=Topic 
nw/wsj/20/wsj_2023.parse 18 3 gold found-v 55.5-1 Intentionally_create found.01 1 ----- 0:1*4:1-ARG1 3:0-rel 5:1-ARGM-LOC 7:1-ARGM-TMP 9:1-ARG0 
nw/wsj/20/wsj_2023.parse 19 5 gold split-v 23.2 Cause_to_fragment split.01 1 ----- 0:1-ARGM-TMP 3:1-ARG0=Agent;Agent 5:0-rel 6:1-ARG1=Patient;Whole_patient 8:1-ARG2=Co-Patient 11:1-ARGM-ADV 
nw/wsj/20/wsj_2023.parse 19 19 gold agree-v 36.1-1 IN agree.01 null ----- 12:2*20:1-ARG0=Agent 19:0-rel 20:2-ARG1=Theme 
nw/wsj/20/wsj_2023.parse 21 3 gold report-v 37.7-1 Statement report.01 null ----- 1:1*4:1-ARG1 2:1-ARGM-TMP 3:0-rel 
nw/wsj/20/wsj_2023.parse 21 10 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARGM-ADV 6:1-ARG0=Agent;Donor 8:1-ARGM-TMP 10:0-rel 11:2-ARG1=Theme;Theme 26:1-ARG2=Source 
nw/wsj/20/wsj_2023.parse 21 29 gold lead-v 51.7 Cotheme lead.02 null ----- 27:1*30:1-ARG1=Theme;Cotheme 29:0-rel 31:1-ARG0=Agent;Theme 27:1*30:1-LINK-PSV 
nw/wsj/20/wsj_2023.parse 22 7 gold include-v 65 NF include.01 null ----- 0:1*3:1*4:1-ARG2=Location 6:0-ARGM-NEG 7:0-rel 8:1-ARG1=Theme 0:1*3:1-LINK-SLC 
nw/wsj/20/wsj_2023.parse 22 14 gold review-v 34.1 NF review.01 1 ----- 0:2*15:1-ARG1=Theme 14:0-rel 16:1-ARG0=Agent 
