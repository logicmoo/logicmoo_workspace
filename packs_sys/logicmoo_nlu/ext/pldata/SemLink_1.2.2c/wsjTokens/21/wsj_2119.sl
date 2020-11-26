nw/wsj/21/wsj_2119.parse 0 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/21/wsj_2119.parse 0 5 gold agree-v 36.1-1 IN agree.01 null ----- 4:1*6:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/21/wsj_2119.parse 0 8 gold team-v 22.2-2 Collaboration team.01 1 ----- 4:1*6:1-ARG1=Patient 8:0-rel 9:1-ARG2=Co-Patient 18:2-ARGM-PRP 
nw/wsj/21/wsj_2119.parse 0 20 gold provide-v 13.4.1-2 Supply provide.01 null ----- 4:1*6:1*18:1-ARG0=Agent 20:0-rel 21:1-ARG1=Theme 23:1-ARGM-PRP 
nw/wsj/21/wsj_2119.parse 0 30 gold propose-v 37.7-1 Statement propose.01 null ----- 24:1*31:1-ARG1=Topic;Message 30:0-rel 32:1-ARG0=Agent;Speaker 24:1*31:1-LINK-PSV 
nw/wsj/21/wsj_2119.parse 1 13 gold say-v 37.7-1 IN say.01 null ----- 12:1-ARG0=Agent 13:0-rel 0:4-ARG1-DSP=Topic 
nw/wsj/21/wsj_2119.parse 2 3 gold report-v 37.7-1 Statement report.01 null ----- 1:1-ARG0 2:1-ARGM-TMP 3:0-rel 
nw/wsj/21/wsj_2119.parse 2 6 gold emerge-v 48.1.1 IN emerge.01 null ----- 0:1-ARGM-ADV 5:1-ARG0=Theme 6:0-rel 7:1-ARG1=Location 14:1-ARGM-TMP 
