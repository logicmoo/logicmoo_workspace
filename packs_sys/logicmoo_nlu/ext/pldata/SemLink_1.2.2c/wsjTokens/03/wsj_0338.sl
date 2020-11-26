nw/wsj/03/wsj_0338.parse 0 4 gold extend-v 47.1-1 NF extend.01 null ----- 0:1-ARG0 4:0-rel 5:2-ARG1=Theme 14:1-ARG2 
nw/wsj/03/wsj_0338.parse 1 9 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 9:0-rel 10:1-ARG1=Topic 
nw/wsj/03/wsj_0338.parse 1 12 gold agree-v 36.1-1 IN agree.01 null ----- 11:1*13:1-ARG0=Agent 12:0-rel 13:2-ARG1=Theme 
nw/wsj/03/wsj_0338.parse 1 15 gold continue-v 55.3 Activity_ongoing continue.01 null ----- 11:1*13:1-ARG0 15:0-rel 16:2-ARG1 
nw/wsj/03/wsj_0338.parse 2 2 gold reach-v 13.5.1 NF reach.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:2-ARG1=Theme 
nw/wsj/03/wsj_0338.parse 2 10 gold say-v 37.7-1 IN say.01 null ----- 0:2*12:1-ARG1=Topic 7:1-ARG0=Agent 10:0-rel 
nw/wsj/03/wsj_0338.parse 3 8 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/03/wsj_0338.parse 3 14 gold extend-v 47.1-1 NF extend.01 null ----- 10:1*12:1-ARG0 14:0-rel 15:2-ARG1=Theme 20:1-ARG2 
nw/wsj/03/wsj_0338.parse 4 8 gold reach-v 13.5.1 NF reach.01 null ----- 2:1-ARGM-TMP 6:1-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 
nw/wsj/03/wsj_0338.parse 5 5 gold abandon-v 51.2 Departing abandon.01 1 ----- 0:2-ARGM-TMP 4:1*15:1-ARG0=Theme;Theme 5:0-rel 6:2-ARG1=Initial_Location;Source 14:1-ARGM-TMP 
nw/wsj/03/wsj_0338.parse 5 16 gold invest-v 13.4.2 NF invest.01 null ----- 4:1*15:1-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 21:1-ARG2=Recipient 
nw/wsj/03/wsj_0338.parse 6 15 gold end-v 55.4-1 Cause_to_end end.01 null ----- 11:1*16:1-ARG1=Theme 15:0-rel 17:1-ARGM-TMP 11:1*16:1-LINK-PSV 
nw/wsj/03/wsj_0338.parse 6 23 gold discontinue-v 55.2 IN discontinue.01 1 ----- 0:1*22:1-ARG0=Agent 23:0-rel 24:1-ARG1=Theme 0:1*22:1-LINK-PRO 
