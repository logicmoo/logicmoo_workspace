nw/wsj/11/wsj_1170.parse 0 3 gold report-v 37.7-1 Statement report.01 1 ----- 0:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/11/wsj_1170.parse 0 17 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 17:0-rel 18:1-ARG1=Topic 
nw/wsj/11/wsj_1170.parse 0 22 gold mask-v 9.8 Hiding_objects mask.01 2 ----- 19:1*23:1-ARG1=Destination 22:0-rel 24:1-ARG2=Theme 
nw/wsj/11/wsj_1170.parse 1 6 gold hold-v 15.1-1 Manipulation hold.01 5 ----- 0:1*7:1-ARG1=Theme;Entity 5:1-ARGM-MNR 6:0-rel 8:1-ARGM-TMP 0:1*7:1-LINK-PSV 
nw/wsj/11/wsj_1170.parse 1 21 gold say-v 37.7-1 IN say.01 1 ----- 0:2-ARG0=Agent 21:0-rel 22:1-ARG1=Topic 
nw/wsj/11/wsj_1170.parse 1 28 gold end-v 55.4-1 Cause_to_end end.01 1 ----- 25:1-ARG1=Theme 28:0-rel 29:1-ARGM-TMP 
nw/wsj/11/wsj_1170.parse 2 2 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/11/wsj_1170.parse 2 9 gold include-v 65 NF include.01 1 ----- 4:1-ARG2=Location 9:0-rel 10:2-ARG1=Theme 
nw/wsj/11/wsj_1170.parse 3 1 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 0:1-ARG1=Patient;Item 1:0-rel 2:1-ARG2=Extent;Difference 4:1-ARG4 9:1-ARG3 
nw/wsj/11/wsj_1170.parse 4 7 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-TMP 5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 
nw/wsj/11/wsj_1170.parse 4 10 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 9:1-ARG1=Patient;Item 10:0-rel 11:1-ARG2=Extent;Difference 13:1-ARG4 18:1-ARG3 
nw/wsj/11/wsj_1170.parse 4 32 gold include-v 65 NF include.01 1 ----- 19:3*30:1*31:1-ARG2=Location 32:0-rel 33:3-ARG1=Theme 19:3*30:1-LINK-SLC 
