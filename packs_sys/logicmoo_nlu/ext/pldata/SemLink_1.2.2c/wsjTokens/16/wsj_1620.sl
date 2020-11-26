nw/wsj/16/wsj_1620.parse 0 4 gold file-v 9.10 Submitting_documents file.01 null ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme 6:1-ARG2=Destination 9:1-ARGM-LOC 11:2-ARG4 
nw/wsj/16/wsj_1620.parse 0 12 gold allege-v 37.7 NF allege.01 1 ----- 0:1*11:1-ARG0=Agent 12:0-rel 13:1-ARG1=Topic 
nw/wsj/16/wsj_1620.parse 0 18 gold hold-v 15.1-1 NF hold.01 null ----- 14:1*16:1*17:1-ARG0=Agent 18:0-rel 19:2-ARG1=Theme 14:1*16:1-LINK-SLC 
nw/wsj/16/wsj_1620.parse 1 5 gold claim-v 37.7-1 Statement claim.01 null ----- 0:2-ARG0=Agent;Speaker 5:0-rel 6:2-ARG1=Topic;Message 
nw/wsj/16/wsj_1620.parse 1 10 gold lead-v 51.7 Cotheme lead.02 null ----- 7:1*11:1-ARG1=Theme;Cotheme 10:0-rel 12:1-ARG0=Agent;Theme 7:1*11:1-LINK-PSV 
nw/wsj/16/wsj_1620.parse 1 23 gold fail-v 75-1-1 NF fail.01 null ----- 7:2*22:1*24:1-ARG1 23:0-rel 24:2-ARG2 
nw/wsj/16/wsj_1620.parse 1 26 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 7:2*22:1*24:1-ARG0=Agent;Speaker 26:0-rel 27:1-ARG1=Topic;Information 
nw/wsj/16/wsj_1620.parse 1 30 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 7:2*28:1-ARG0=Agent;Buyer 30:0-rel 31:2-ARG1=Theme;Goods 7:2*28:1-LINK-PRO 
nw/wsj/16/wsj_1620.parse 1 43 gold require-v 103 NF require.01 null ----- 43:0-rel 44:1,49:0-ARG1=Theme 
nw/wsj/16/wsj_1620.parse 1 59 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 41:2-ARGM-TMP 56:1-ARG0=Agent;Speaker 58:0-ARGM-NEG 59:0-rel 60:3-ARG1=Topic;Information 
nw/wsj/16/wsj_1620.parse 2 10 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 0:1*8:1-ARG0=Agent;Speaker 10:0-rel 
nw/wsj/16/wsj_1620.parse 3 23 gold say-v 37.7-1 IN say.01 null ----- 0:0-ARGM-DIS 1:2-ARGM-TMP 17:1-ARG0=Agent 23:0-rel 25:2-ARG1=Topic 
nw/wsj/16/wsj_1620.parse 3 34 gold deny-v 29.5-1 Statement deny.01 null ----- 33:1-ARG0=Agent;Speaker 34:0-rel 35:1-ARG1=Predicate;Message 
nw/wsj/16/wsj_1620.parse 4 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARGM-LOC 11:1-ARGM-TMP 14:1-ARG1=Topic 
nw/wsj/16/wsj_1620.parse 5 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/16/wsj_1620.parse 5 7 gold intend-v 62 Purpose intend.01 null ----- 4:1*8:1-ARG0 6:0-ARGM-NEG 7:0-rel 8:2-ARG1 
nw/wsj/16/wsj_1620.parse 5 11 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 4:1*8:1*12:1-ARG1=Theme;Theme 11:0-rel 13:1-ARG0=Agent;Recipient 
