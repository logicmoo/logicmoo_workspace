nw/wsj/04/wsj_0484.parse 0 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/04/wsj_0484.parse 0 10 gold deny-v 29.5-1 Statement deny.01 null ----- 4:2-ARG0=Agent;Speaker 10:0-rel 11:3-ARG1=Predicate;Message 
nw/wsj/04/wsj_0484.parse 0 23 gold lead-v 51.7 Cotheme lead.02 null ----- 21:1*24:1-ARG1=Theme;Cotheme 23:0-rel 25:1-ARG0=Agent;Theme 21:1*24:1-LINK-PSV 
nw/wsj/04/wsj_0484.parse 0 32 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 21:2*31:1-ARG0=Agent;Recipient 32:0-rel 33:2-ARG1=Theme;Theme 
nw/wsj/04/wsj_0484.parse 1 16 gold contend-v 29.5-2 Statement contend.01 1 ----- 0:2-ARG0=Agent;Speaker 16:0-rel 17:1-ARG1=Theme;Addressee 
nw/wsj/04/wsj_0484.parse 1 25 gold fail-v 75-1-1 NF fail.01 null ----- 18:2*26:1-ARG1 25:0-rel 26:2-ARG2 
nw/wsj/04/wsj_0484.parse 1 28 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 18:2*26:1-ARG0=Agent;Speaker 28:0-rel 29:1-ARG2=Recipient;Addressee 35:1-ARG1=Topic;Information 
nw/wsj/04/wsj_0484.parse 1 37 gold intend-v 62 Purpose intend.01 null ----- 36:1*38:1-ARG0 37:0-rel 38:2-ARG1 
nw/wsj/04/wsj_0484.parse 1 40 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 36:1*38:1-ARG0=Agent;Recipient 40:0-rel 41:1-ARG1=Theme;Theme 
nw/wsj/04/wsj_0484.parse 2 5 gold contend-v 29.5-2 Statement contend.01 1 ----- 0:1-ARG0=Agent;Speaker 3:1-ARGM-DIS 5:0-rel 6:1-ARG1=Theme;Addressee 
nw/wsj/04/wsj_0484.parse 2 23 gold connect-v 22.1-2-1 IN connect.01 null ----- 20:1*24:1-ARG1=Patient 23:0-rel 25:1-ARG1=Patient 20:1*24:1-LINK-PSV 
nw/wsj/04/wsj_0484.parse 2 42 gold have-v 100 IN have.03 null ----- 7:2-ARG0=Pivot 42:0-rel 43:2-ARG1=Theme 
nw/wsj/04/wsj_0484.parse 2 46 gold compete-v 36.4-1 Competition compete.01 1 ----- 46:0-rel 47:1,49:0-ARG0=Agent 
nw/wsj/04/wsj_0484.parse 3 4 gold assemble-v 47.5.2 Gathering_up assemble.02 2 ----- 0:1-ARG0=Agent;Agent 2:1-ARGM-TMP 4:0-rel 5:2-ARG1=Theme;Individuals 17:2-ARGM-PRP 
nw/wsj/04/wsj_0484.parse 3 19 gold analyze-v 34.1 Scrutiny analyze.01 1 ----- 5:2*17:1-ARG0=Agent;Cognizer 19:0-rel 20:1-ARG1=Theme;Ground 5:2*17:1-LINK-PRO 
nw/wsj/04/wsj_0484.parse 6 2 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 2:0-rel 3:1-ARG1=Theme 
nw/wsj/04/wsj_0484.parse 6 8 gold expect-v 62 IN expect.01 null ----- 0:1;9:2-ARG1=Theme 8:0-rel 
