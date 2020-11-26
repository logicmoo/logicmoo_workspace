nw/wsj/17/wsj_1724.parse 1 2 gold complain-v 37.8 Statement complain.01 null ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/17/wsj_1724.parse 2 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/17/wsj_1724.parse 3 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/17/wsj_1724.parse 3 10 gold invite-v 102 Request invite.01 null ----- 5:2*28:1-ARG0 10:0-rel 11:1-ARG1 13:1-ARG2 
nw/wsj/17/wsj_1724.parse 3 27 gold offer-v 13.3 NF offer.01 null ----- 5:2*28:1-ARG0=Agent 27:0-rel 28:2-ARG1=Theme 
nw/wsj/17/wsj_1724.parse 3 30 gold dispatch-v 11.1 Sending dispatch.01 1 ----- 5:2*28:1-ARG0=Agent;Sender 30:0-rel 31:1*33:1-ARG1=Theme;Theme 33:2-ARGM-PRP 
nw/wsj/17/wsj_1724.parse 4 9 gold say-v 37.7-1 IN say.01 null ----- 0:2*11:1-ARG1=Topic 6:1-ARG0=Agent 9:0-rel 
nw/wsj/17/wsj_1724.parse 5 9 gold assert-v 29.5-2 Statement assert.03 1 ----- 0:2-ARG0=Agent;Speaker 8:1-ARGM-DIS 9:0-rel 10:1-ARG1=Theme;Addressee 
nw/wsj/17/wsj_1724.parse 5 14 gold fail-v 75-1-1 NF fail.01 null ----- 11:1-ARG1 14:0-rel 15:2-ARG2 
nw/wsj/17/wsj_1724.parse 5 17 gold return-v 13.2-2 NF return.02 null ----- 11:1*15:1-ARG0=Agent 17:0-rel 18:2-ARG1=Theme 
nw/wsj/17/wsj_1724.parse 6 2 gold regret-v 31.2-1 Experiencer_focus regret.01 1 ----- 1:1-ARG0=Experiencer 2:0-rel 3:1-ARGM-EXT 5:1-ARG1=Attribute 
nw/wsj/17/wsj_1724.parse 6 16 gold cooperate-v 36.1 Collaboration cooperate.01 1 ----- 6:2*13:1-ARG0=Agent;Partner_1 14:0-ARGM-NEG 16:0-rel 17:1-ARG1=Co-Agent;Partner_2 19:1-ARGM-LOC 22:1-ARG2=Theme;Undertaking 
nw/wsj/17/wsj_1724.parse 6 24 gold make-v 29.3 Causation make.02 null ----- 23:1-ARG0=Agent 24:0-rel 25:2-ARG1=Theme 
nw/wsj/17/wsj_1724.parse 6 42 gold say-v 37.7-1 IN say.01 null ----- 1:2*43:1-ARG1=Topic 40:1-ARG0=Agent 42:0-rel 
nw/wsj/17/wsj_1724.parse 7 11 gold bury-v 9.1-1 NF bury.01 1 ----- 4:1*7:1*9:1-ARG0=Agent 11:0-rel 12:1-ARG1=Theme 
nw/wsj/17/wsj_1724.parse 8 3 gold announce-v 37.7-1 Statement announce.01 null ----- 0:1-ARG0=Agent;Speaker 3:0-rel 4:1-ARG1=Topic;Message 
nw/wsj/17/wsj_1724.parse 8 20 gold accompany-v 51.7 Cotheme accompany.01 1 ----- 5:2*18:1-ARG0=Agent;Theme 20:0-rel 21:1-ARG1=Theme;Cotheme 23:1-ARG4=Destination 30:1-ARGM-TMP 
nw/wsj/17/wsj_1724.parse 9 5 gold report-v 37.7-1 Statement report.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent;Speaker 5:0-rel 6:1-ARG1=Topic;Message 
