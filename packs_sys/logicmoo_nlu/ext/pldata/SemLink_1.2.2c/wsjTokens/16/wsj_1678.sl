nw/wsj/16/wsj_1678.parse 0 10 gold involve-v 86.2-1 NF involve.01 null ----- 5:1-ARG2 10:0-rel 11:1-ARG1 
nw/wsj/16/wsj_1678.parse 1 7 gold order-v 60-1 NF order.01 null ----- 0:1-ARG0=Agent 7:0-rel 8:1*10:1-ARG1=Recipient 10:2-ARG2=Topic 
nw/wsj/16/wsj_1678.parse 2 2 gold award-v 13.3 NF award.01 1 ----- 0:1-ARG0=Agent 1:1-ARGM-DIS 2:0-rel 3:2-ARG1=Theme 9:1-ARG2=Goal 
nw/wsj/16/wsj_1678.parse 3 2 gold award-v 13.3 NF award.01 1 ----- 0:1*3:1-ARG1=Theme 2:0-rel 4:1-ARG2=Goal 0:1*3:1-LINK-PSV 
nw/wsj/16/wsj_1678.parse 3 15 gold say-v 37.7-1 IN say.01 null ----- 12:1-ARG0=Agent 15:0-rel 16:1-ARG1=Topic 
nw/wsj/16/wsj_1678.parse 4 18 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 18:0-rel 19:1-ARG1=Topic 
nw/wsj/16/wsj_1678.parse 4 22 gold begin-v 55.1-1 Activity_start begin.01 null ----- 20:1-ARG0=Agent;Agent 22:0-rel 23:2-ARG1=Theme;Activity 
nw/wsj/16/wsj_1678.parse 4 24 gold work-v 73-3 IN work.01 null ----- 20:1*23:1-ARG0=Agent 24:0-rel 25:1-ARGM-LOC 28:1-ARGM-TMP 
nw/wsj/16/wsj_1678.parse 5 2 gold build-v 26.1-1 Building build.01 null ----- 0:1-ARG0=Agent;Agent 2:0-rel 3:3-ARG1=Product;Created_entity 
nw/wsj/16/wsj_1678.parse 6 6 gold involve-v 86.2-1 NF involve.01 null ----- 3:1-ARG2 6:0-rel 7:1-ARG1 
nw/wsj/16/wsj_1678.parse 7 2 gold end-v 55.4-1 Cause_to_end end.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:2-ARG1=Theme 
nw/wsj/16/wsj_1678.parse 8 18 gold comment-v 37.11-1-1 Statement comment.01 1 ----- 0:2*16:1-ARG0=Agent;Speaker 18:0-rel 
