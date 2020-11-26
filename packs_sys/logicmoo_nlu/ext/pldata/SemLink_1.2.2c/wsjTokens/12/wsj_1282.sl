nw/wsj/12/wsj_1282.parse 0 3 gold file-v 9.10 Submitting_documents file.02 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 11:2-ARGM-ADV 
nw/wsj/12/wsj_1282.parse 0 12 gold say-v 37.7-1 IN say.01 1 ----- 0:1*11:1-ARG0=Agent 12:0-rel 13:1-ARG1=Topic 
nw/wsj/12/wsj_1282.parse 1 18 gold spawn-v 27 NF spawn.01 1 ----- 0:1*3:2*16:1-ARG0=Cause 18:0-rel 19:2-ARG1=Theme 0:1*3:2-LINK-SLC 
nw/wsj/12/wsj_1282.parse 3 8 gold say-v 37.7-1 IN say.01 1 ----- 0:3-ARG0=Agent 8:0-rel 9:1-ARG1=Topic 
nw/wsj/12/wsj_1282.parse 3 13 gold force-v 59 NF force.01 1 ----- 10:1*14:1-ARG1=Patient 13:0-rel 16:1-ARG2=Result 22:1-ARGM-CAU 
nw/wsj/12/wsj_1282.parse 3 17 gold start-v 55.1-1 Activity_start start.01 2 ----- 10:1*14:1*15:1-ARG0=Agent;Agent 17:0-rel 18:2-ARG1=Theme;Activity 
nw/wsj/12/wsj_1282.parse 3 24 gold begin-v 55.1-1 Activity_start begin.01 2 ----- 23:1-ARG0=Agent;Agent 24:0-rel 25:2-ARG1=Theme;Activity 
nw/wsj/12/wsj_1282.parse 4 8 gold know-v 29.5-1 IN know.01 2 ----- 6:1*9:1-ARG1=Theme 8:0-rel 10:1-ARG2=Predicate 6:1*9:1-LINK-PSV 
nw/wsj/12/wsj_1282.parse 4 19 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 6:2*16:1*17:1*20:1-ARG1=Theme;Goods 19:0-rel 21:2-ARGM-MNR 6:2*16:1-LINK-SLC 
nw/wsj/12/wsj_1282.parse 5 15 gold develop-v 26.1 IN develop.02 1 ----- 14:1-ARG0=Agent 15:0-rel 18:2-ARG1=Product 
nw/wsj/12/wsj_1282.parse 5 20 gold fashion-v 26.1 Building fashion.01 1 ----- 18:1*21:1-ARG1=Product;Created_entity 20:0-rel 22:1-ARGM-MNR 26:1-ARGM-MNR 18:1*21:1-LINK-PSV 
nw/wsj/12/wsj_1282.parse 8 8 gold disclose-v 37.7 Reveal_secret disclose.01 1 ----- 0:2*9:1-ARG1=Topic;Information 7:0-ARGM-NEG 8:0-rel 
