nw/wsj/12/wsj_1274.parse 0 2 gold drink-v 39.1-2 Ingestion drink.01 2 ----- 0:1-ARG0=Agent;Ingestor 2:0-rel 3:1-ARG1=Patient;Ingestibles 
nw/wsj/12/wsj_1274.parse 0 17 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 6:2*15:1-ARG0=Agent;Buyer 17:0-rel 18:2-ARG1=Theme;Goods 
nw/wsj/12/wsj_1274.parse 1 5 gold snub-v 33 NF snub.01 null ----- 3:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 1 8 gold prefer-v 31.2 Preference prefer.01 1 ----- 6:1*9:1-ARG1 8:0-rel 10:1-ARG0 6:1*9:1-LINK-PSV 
nw/wsj/12/wsj_1274.parse 2 4 gold decline-v 45.6-1 Change_position_on_a_scale decline.01 3 ----- 1:1-ARG1=Patient;Item 4:0-rel 5:1-ARGM-MNR 
nw/wsj/12/wsj_1274.parse 3 9 gold jump-v 45.6-1 Change_position_on_a_scale jump.01 1 ----- 7:1-ARG1=Patient;Item 9:0-rel 10:2-ARG2=Extent;Difference 15:1-ARG4 22:1-ARG3 30:1-ARGM-ADV 
nw/wsj/12/wsj_1274.parse 4 11 gold say-v 37.7-1 IN say.01 1 ----- 1:2*12:1-ARG1 11:0-rel 13:2-ARG0 
nw/wsj/12/wsj_1274.parse 5 3 gold want-v 32.1-1-1 Desiring want.01 1 ----- 2:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/12/wsj_1274.parse 6 2 gold have-v 100 IN have.03 1 ----- 0:1-ARG0=Pivot 1:0-ARGM-MOD 2:0-rel 3:3-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 8 12 gold acquire-v 13.5.2-1 Getting acquire.01 3 ----- 0:1-ARGM-TMP 4:1-ARGM-DIS 7:2-ARG0=Agent;Recipient 12:0-rel 13:1-ARG1=Theme;Theme 16:1-ARGM-ADV 
nw/wsj/12/wsj_1274.parse 9 6 gold fuel-v 9.9 NF fuel.01 2 ----- 0:0-ARGM-DIS 1:1*7:1-ARG1=Destination 4:1-ARGM-DIS 6:0-rel 8:1-ARG2=Theme 
nw/wsj/12/wsj_1274.parse 10 14 gold rid-v 10.6 Emptying rid.01 null ----- 1:3-ARG0=Agent;Agent/Cause 14:0-rel 15:1-ARGM-MNR 16:1-ARGM-LOC 
nw/wsj/12/wsj_1274.parse 10 23 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 0:1-ARGM-ADV 20:2-ARG1 23:0-rel 24:1-ARG2 26:1-ARG4 32:1-ARG3 
nw/wsj/12/wsj_1274.parse 13 3 gold surge-v 45.6-1 NF surge.01 2 ----- 0:2-ARG1 3:0-rel 4:1-ARG2 6:1-ARG4 
nw/wsj/12/wsj_1274.parse 15 16 gold help-v 72-1 Assistance help.01 1 ----- 0:2-ARG0=Agent;Helper 16:0-rel 17:1-ARG1=Theme;Goal/Focal_entity 19:1-ARGM-TMP 
nw/wsj/12/wsj_1274.parse 15 23 gold lower-v 9.4 NF lower.01 2 ----- 22:1-ARG0=Agent 23:0-rel 24:2-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 16 5 gold lower-v 9.4 NF lower.01 null ----- 5:0-rel 6:0,7:0-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 17 6 gold promote-v 102 NF promote.02 1 ----- 0:1-ARGM-LOC 3:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 17 13 gold court-v 36.2 Personal_relationship court.01 1 ----- 0:1-ARGM-LOC 3:1-ARG0=Agent;Partner_1 13:0-rel 14:3-ARG1=Co-Agent;Partner_2 
nw/wsj/12/wsj_1274.parse 19 1 gold use-v 105 IN use.01 1 ----- 0:1*9:1-ARG0 1:0-rel 2:2-ARG1 
nw/wsj/12/wsj_1274.parse 20 9 gold make-v 26.1-1 IN make.01 2 ----- 0:1*10:1-ARG1=Product 5:1-ARGM-DIS 9:0-rel 11:1-ARG0=Agent 
nw/wsj/12/wsj_1274.parse 21 10 gold compete-v 36.4-1 Competition compete.01 1 ----- 1:2-ARG0=Agent 10:0-rel 11:1-ARG1=Co-Agent 13:1-ARGM-LOC 
nw/wsj/12/wsj_1274.parse 21 17 gold prohibit-v 67 Prohibiting prohibit.01 1 ----- 0:1-ARGM-ADV 15:1-ARG0=Agent;Principle 17:0-rel 18:1-ARG1=Theme;State_of_affairs 21:1-ARG2=Theme;State_of_affairs 
nw/wsj/12/wsj_1274.parse 21 23 gold make-v 26.1-1 Intentionally_create make.01 2 ----- 18:1*22:1-ARG0=Agent;Creator 23:0-rel 24:1-ARG1=Product;Created_entity 18:1*22:1-LINK-PRO 
nw/wsj/12/wsj_1274.parse 22 3 gold come-v 48.1.1 NF come.03 5 ----- 1:1-ARG1=Theme 3:0-rel 4:1-ARG2 7:1-ARGM-ADV 
nw/wsj/12/wsj_1274.parse 23 6 gold acquire-v 13.5.2-1 Getting acquire.01 2 ----- 4:1-ARG0=Agent;Recipient 6:0-rel 7:1-ARG1=Theme;Theme 10:1-ARGM-LOC 
nw/wsj/12/wsj_1274.parse 24 4 gold win-v 13.5.1 NF win.01 2 ----- 0:1*5:1-ARGM-MNR 2:1-ARG1=Theme 4:0-rel 
nw/wsj/12/wsj_1274.parse 26 14 gold appear-v 48.1.1 Coming_to_be appear.01 2 ----- 13:1-ARG1=Theme;Entity 14:0-rel 
nw/wsj/12/wsj_1274.parse 26 17 gold say-v 37.7-1 IN say.01 1 ----- 1:2*18:1-ARG1 17:0-rel 19:2-ARG0 
nw/wsj/12/wsj_1274.parse 27 1 gold distribute-v 13.2-1 Dispersal distribute.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 6:1-ARGM-LOC 
nw/wsj/12/wsj_1274.parse 28 1 gold make-v 26.1-1 IN make.07 6 ----- 0:1-ARG0=Material 1:0,2:1-rel 3:3-ARG1=Product 
nw/wsj/12/wsj_1274.parse 28 32 gold have-v 100 IN have.03 1 ----- 29:1-ARG0=Pivot 32:0-rel 33:2-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 29 5 gold vie-v 36.4-1 NF vie.01 null ----- 0:1-ARG0=Agent 4:1-ARGM-MNR 5:0-rel 6:1-ARG2=Topic 
nw/wsj/12/wsj_1274.parse 29 12 gold grow-v 45.6-1 IN grow.01 4 ----- 7:1*10:1*11:1-ARG1 12:0-rel 13:0,14:2-ARG4 18:1-ARGM-TMP 20:1-ARG3 28:1-ARGM-ADV 7:1*10:1-LINK-SLC 
nw/wsj/12/wsj_1274.parse 30 14 gold lead-v 51.7 Cotheme lead.01 2 ----- 14:0-rel 15:0-ARG0=Agent;Theme 
nw/wsj/12/wsj_1274.parse 31 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0 2:0-rel 3:1-ARG1 
nw/wsj/12/wsj_1274.parse 32 14 gold build-v 26.1-1 Building build.01 3 ----- 0:2-ARG0=Agent;Agent 14:0-rel 15:2-ARG1=Product;Created_entity 21:1-ARGM-LOC 24:2-ARGM-ADV 
nw/wsj/12/wsj_1274.parse 32 29 gold install-v 9.1-1 IN install.01 1 ----- 26:1*27:1-ARG0 29:0-rel 30:1-ARG1 
nw/wsj/12/wsj_1274.parse 33 8 gold lead-v 51.7 Cotheme lead.01 2 ----- 8:0-rel 9:0-ARG0=Agent;Theme 
nw/wsj/12/wsj_1274.parse 34 3 gold succeed-v 74-1 IN succeed.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARGM-LOC 13:2-ARGM-ADV 
nw/wsj/12/wsj_1274.parse 34 10 gold fail-v 74-2 IN fail.01 1 ----- 5:1*6:1*11:1-ARGM-LOC 7:1-ARG1 10:0-rel 5:1*6:1-LINK-SLC 
nw/wsj/12/wsj_1274.parse 34 14 gold avoid-v 52 Avoiding avoid.01 1 ----- 0:1*13:1-ARG0=Agent;Agent 14:0-rel 15:2-ARG1=Theme;Undesirable_situation 21:1-ARGM-MNR 
nw/wsj/12/wsj_1274.parse 35 3 gold have-v 100 IN have.03 2 ----- 0:1-ARG0=Pivot 3:0-rel 4:2-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 36 10 gold have-v 100 IN have.03 2 ----- 0:2-ARG0=Pivot 10:0-rel 11:2-ARG1=Theme 
nw/wsj/12/wsj_1274.parse 37 3 gold advertise-v 35.2 NF advertise.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARGM-LOC 5:1-ARGM-MNR 
nw/wsj/12/wsj_1274.parse 38 2 gold promote-v 102 NF promote.02 1 ----- 0:1*8:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 6:1-ARGM-LOC 
nw/wsj/12/wsj_1274.parse 38 11 gold use-v 105 IN use.01 1 ----- 0:1*8:1-ARG0 0:2-ARG2 11:0-rel 12:2-ARG1 
nw/wsj/12/wsj_1274.parse 40 13 gold lie-v 47.6 Being_located lie.01 1 ----- 0:1-ARGM-LOC 8:2-ARG1 13:0-rel 14:1-ARG2 
nw/wsj/12/wsj_1274.parse 41 2 gold get-v 13.5.1-1 IN get.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Theme 5:1-ARGM-MNR 
nw/wsj/12/wsj_1274.parse 43 6 gold carry-v 11.4 Bringing carry.01 2 ----- 3:1-ARG0 6:0-rel 7:2-ARG1 
nw/wsj/12/wsj_1274.parse 44 6 gold ask-v 37.1.2 Questioning ask.01 1 ----- 4:1-ARG0=Agent;Speaker 5:1-ARGM-ADV 6:0-rel 7:1-ARG1=Topic;Message 
nw/wsj/12/wsj_1274.parse 44 9 gold want-v 32.1-1-1 Desiring want.01 1 ----- 8:1-ARG0 9:0-rel 10:1-ARG1 
