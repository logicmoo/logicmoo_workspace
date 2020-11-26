nw/wsj/05/wsj_0582.parse 0 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/05/wsj_0582.parse 0 5 gold expect-v 62 IN expect.01 null ----- 4:1*6:1-ARG0=Experiencer 5:0-rel 6:2-ARG1=Theme 
nw/wsj/05/wsj_0582.parse 0 8 gold receive-v 13.5.2 Receiving receive.01 null ----- 4:1*6:1-ARG0=Agent;Donor 8:0-rel 9:3-ARG1=Theme;Theme 22:1-ARG2=Source 
nw/wsj/05/wsj_0582.parse 0 25 gold rule-v 29.3 Verdict rule.01 null ----- 24:0-ARG0=Agent;Judge 25:0-rel 
nw/wsj/05/wsj_0582.parse 1 5 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/05/wsj_0582.parse 1 8 gold expect-v 62 IN expect.01 null ----- 7:1-ARG0=Experiencer 8:0-rel 9:2-ARG1=Theme 
nw/wsj/05/wsj_0582.parse 1 13 gold include-v 65 NF include.01 null ----- 9:1*14:1-ARG1=Theme 13:0-rel 15:1-ARG2=Location 
nw/wsj/05/wsj_0582.parse 1 19 gold end-v 55.4-1 Cause_to_end end.01 null ----- 16:1-ARG1=Theme 19:0-rel 20:1-ARGM-TMP 
nw/wsj/05/wsj_0582.parse 3 0 gold accrue-v 13.5.2 NF accrue.01 1 ----- 0:0-rel 1:0-ARG1=Theme 
nw/wsj/05/wsj_0582.parse 4 2 gold stem-v 48.1.1 NF stem.01 1 ----- 0:1-ARG1=Theme 2:0-rel 3:1-ARG2 
nw/wsj/05/wsj_0582.parse 4 9 gold find-v 84 IN find.01 null ----- 4:1*7:1*8:1-ARG0 9:0-rel 10:1-ARG1 4:1*7:1-LINK-SLC 
