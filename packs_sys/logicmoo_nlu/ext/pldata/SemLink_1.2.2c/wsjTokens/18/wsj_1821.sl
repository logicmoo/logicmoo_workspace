nw/wsj/18/wsj_1821.parse 0 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/18/wsj_1821.parse 0 5 gold sign-v 13.5.3 NF sign.02 null ----- 4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 
nw/wsj/18/wsj_1821.parse 0 11 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 4:1*9:1-ARG0=Agent;Recipient 11:0-rel 12:2-ARG1=Theme;Theme 19:1-ARG3 4:1*9:1-LINK-PRO 
nw/wsj/18/wsj_1821.parse 1 4 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 0:1*5:1-ARG1=Topic;Information 3:0-ARGM-NEG 4:0-rel 
nw/wsj/18/wsj_1821.parse 2 6 gold estimate-v 54.4 Estimating estimate.01 1 ----- 0:1-ARG0=Agent 5:1-ARGM-TMP 6:0-rel 7:2;21:1-ARG1=Theme 11:1-ARG2=Value 
nw/wsj/18/wsj_1821.parse 3 5 gold acquire-v 13.5.2-1 Getting acquire.01 null ----- 0:1-ARGM-DIS 3:1-ARG0=Agent;Recipient 4:0-ARGM-MOD 5:0-rel 6:2-ARG1=Theme;Theme 
nw/wsj/18/wsj_1821.parse 4 6 gold have-v 100 IN have.03 null ----- 0:1-ARGM-TMP 3:1-ARG0=Pivot 6:0-rel 7:2-ARG1=Theme 
nw/wsj/18/wsj_1821.parse 4 7 gold combine-v 22.1-1-1 Amalgamation combine.01 null ----- 7:0-rel 8:0-ARG3 
nw/wsj/18/wsj_1821.parse 5 3 gold give-v 13.1-1 Giving give.01 null ----- 0:1-ARG0=Agent;Donor 2:0-ARGM-MOD 3:0-rel 4:1-ARG2=Recipient;Recipient 5:2-ARG1=Theme;Theme 
nw/wsj/18/wsj_1821.parse 5 14 gold include-v 65 NF include.01 null ----- 5:1*12:1*13:1-ARG2=Location 14:0-rel 15:2-ARG1=Theme 5:1*12:1-LINK-SLC 
nw/wsj/18/wsj_1821.parse 6 15 gold say-v 37.7-1 IN say.01 null ----- 0:3*17:1-ARG1=Topic 13:1-ARG0=Agent 15:0-rel 
