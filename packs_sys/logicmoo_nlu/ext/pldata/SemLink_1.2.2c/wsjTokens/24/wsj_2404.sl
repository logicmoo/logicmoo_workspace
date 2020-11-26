nw/wsj/24/wsj_2404.parse 0 6 gold drag-v 11.4 Cause_motion drag.01 6 ----- 0:2-ARG0=Agent;Agent/Cause 6:0-rel 7:1-ARG1=Theme;Theme 13:1-ARG2=Destination;Goal 14:1-ARGM-LOC 19:1-ARGM-TMP 
nw/wsj/24/wsj_2404.parse 1 6 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 0:1-ARG0=Agent;Seller 6:0-rel 7:2-ARG1=Theme;Goods 13:1-ARGM-TMP 21:2-ARGM-ADV 
nw/wsj/24/wsj_2404.parse 2 1 gold build-v 26.1-1 Building build.01 1 ----- 0:0-ARGM-MNR 1:0-rel 2:0-ARG1=Product;Created_entity 
nw/wsj/24/wsj_2404.parse 3 7 gold clear-v 10.3-1 Removing clear.01 1 ----- 5:1-ARG0=Agent;Agent/Cause 7:0,8:1-rel 9:1-ARG1=Location;Source 
nw/wsj/24/wsj_2404.parse 3 15 gold take-v 11.3 IN take.01 11 ----- 0:2*12:1-ARG0=Instrument 15:0-rel 16:1-ARG1=Theme 19:2-ARGM-TMP 23:1-ARG2=Initial_Location 
nw/wsj/24/wsj_2404.parse 3 31 gold begin-v 55.1-1 Process_start begin.01 1 ----- 25:1*29:1*30:1-ARG1=Theme;Event 31:0-rel 32:1-ARGM-MNR 33:1-ARGM-TMP 25:1*29:1-LINK-SLC 
nw/wsj/24/wsj_2404.parse 4 10 gold have-v 100 IN have.03 4 ----- 8:1*9:1-ARG0=Pivot 10:0-rel 11:1-ARG1=Theme 13:1-ARGM-TMP 
nw/wsj/24/wsj_2404.parse 4 18 gold see-v 30.1-1 Perception_experience see.01 3 ----- 8:1*9:1-ARG0=Experiencer;Perceiver_passive 18:0-rel 19:2-ARG1=Stimulus;Phenomenon 
nw/wsj/24/wsj_2404.parse 4 26 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 19:1-ARG1=Patient;Item 26:0-rel 27:1-ARG2=Extent;Difference 29:1-ARG3 
nw/wsj/24/wsj_2404.parse 5 9 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 0:1-ARGM-ADV 2:2-ARG1=Patient;Item 9:0-rel 10:1-ARG2=Extent;Difference 12:1-ARG3 
nw/wsj/24/wsj_2404.parse 7 8 gold adopt-v 29.1 Adopt_selection adopt.01 1 ----- 0:2-ARG0=Agent 7:1-ARGM-TMP 8:0-rel 9:2-ARG1=Theme 
nw/wsj/24/wsj_2404.parse 7 26 gold keep-v 55.6 Cause_to_continue keep.02 1 ----- 16:1*24:1-ARG0=Agent 26:0-rel 27:1-ARG1=Theme 28:1-ARGM-PRD 
nw/wsj/24/wsj_2404.parse 8 19 gold say-v 37.7-1 IN say.01 1 ----- 1:2*20:1-ARG1=Topic 19:0-rel 21:2-ARG0=Agent 
nw/wsj/24/wsj_2404.parse 9 2 gold fall-v 45.6-1 Change_position_on_a_scale fall.01 1 ----- 0:1-ARG1=Patient;Item 2:0-rel 3:1-ARG4 
nw/wsj/24/wsj_2404.parse 9 6 gold adjust-v 26.9 NF adjust.01 null ----- 5:0-ARGM-MNR 6:0-rel 7:0,8:0,9:0-ARG1=Patient 
nw/wsj/24/wsj_2404.parse 10 3 gold contrast-v 22.2-2-1 NF contrast.01 1 ----- 0:1-ARG1=Patient 3:0-rel 4:1-ARG2=Co-Patient 
nw/wsj/24/wsj_2404.parse 11 7 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-DIS 2:2-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 
nw/wsj/24/wsj_2404.parse 11 12 gold see-v 30.1-1 Grasp see.01 2 ----- 9:1-ARG0=Experiencer 10:0-ARGM-MOD 11:0-ARGM-NEG 12:0-rel 13:2-ARG1=Stimulus 
nw/wsj/24/wsj_2404.parse 12 5 gold drop-v 45.6-1 Change_position_on_a_scale drop.01 2 ----- 0:2-ARG1=Patient;Item 5:0-rel 6:1-ARG2=Extent;Difference 
nw/wsj/24/wsj_2404.parse 13 7 gold show-v 78-1-1 IN show.01 1 ----- 0:2-ARG0 7:0-rel 8:1-ARG1 
nw/wsj/24/wsj_2404.parse 14 1 gold post-v 11.1 Sending post.01 1 ----- 0:1-ARG0=Agent;Sender 1:0-rel 2:1-ARG1=Theme;Theme 6:1-ARGM-ADV 
nw/wsj/24/wsj_2404.parse 15 13 gold report-v 37.7-1 Statement report.01 1 ----- 12:1-ARG0 13:0-rel 14:1-ARG1 16:2-ARGM-TMP 
nw/wsj/24/wsj_2404.parse 16 12 gold cause-v 27 Causation cause.01 1 ----- 0:2*13:1-ARG1=Theme;Effect 9:0-ARGM-MOD 12:0-rel 14:1-ARGM-ADV 16:1-ARG0=Cause;Cause 
nw/wsj/24/wsj_2404.parse 16 26 gold offer-v 13.3 NF offer.01 1 ----- 22:1*24:1*27:1-ARG1=Theme 25:1-ARG0=Agent 26:0-rel 28:1-ARGM-MNR 22:1*24:1-LINK-SLC 
nw/wsj/24/wsj_2404.parse 16 42 gold say-v 37.7-1 IN say.01 1 ----- 0:3*44:1-ARG1=Topic 39:1-ARG0=Agent 42:0-rel 
nw/wsj/24/wsj_2404.parse 17 4 gold have-v 100 IN have.03 1 ----- 0:1-ARGM-TMP 3:1-ARG0=Pivot 4:0-rel 5:2;11:2-ARG1=Theme 9:1-ARGM-MNR 
nw/wsj/24/wsj_2404.parse 17 13 gold continue-v 55.3 Activity_ongoing continue.01 1 ----- 5:1*11:1*12:1-ARG0 13:0-rel 14:2-ARG1 17:1-ARGM-TMP 5:1*11:1-LINK-SLC 
nw/wsj/24/wsj_2404.parse 17 24 gold sell-v 13.1-1 Commerce_sell sell.01 1 ----- 18:1*25:1-ARG1=Theme;Goods 24:0-rel 
nw/wsj/24/wsj_2404.parse 18 7 gold post-v 11.1 Sending post.01 1 ----- 0:1-ARGM-ADV 4:1-ARG0=Agent;Sender 7:0-rel 8:2-ARG1=Theme;Theme 
nw/wsj/24/wsj_2404.parse 18 9 gold mix-v 22.1-1-1 Cause_to_amalgamate mix.01 null ----- 8:0-ARGM-MNR 9:0-rel 10:0-ARG1=Patient;Part_1 
nw/wsj/24/wsj_2404.parse 19 3 gold have-v 100 IN have.03 4 ----- 0:1-ARG0=Pivot 3:0-rel 4:3-ARG1=Theme 
nw/wsj/24/wsj_2404.parse 20 2 gold have-v 100 IN have.03 4 ----- 0:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 11:2-ARGM-ADV 
nw/wsj/24/wsj_2404.parse 20 12 gold echo-v 47.4 Sound_movement echo.01 1 ----- 0:1*11:1-ARG0=Theme;Sound 12:0-rel 13:2-ARG1 0:1*11:1-LINK-PRO 
nw/wsj/24/wsj_2404.parse 21 13 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 0:1-ARGM-DIS 2:3-ARG1 13:0-rel 14:1-ARG2 
nw/wsj/24/wsj_2404.parse 22 7 gold build-v 26.1-1 Building build.01 1 ----- 6:0-ARGM-MNR 7:0-rel 8:0-ARG1=Product;Created_entity 
nw/wsj/24/wsj_2404.parse 22 9 gold plunge-v 45.6-1 Motion_directional plunge.01 2 ----- 0:3-ARG1=Patient 9:0-rel 10:1-ARG2=Extent 12:1-ARG3 
nw/wsj/24/wsj_2404.parse 23 9 gold gear-v 26.9 NF gear.02 2 ----- 0:3-ARG0=Agent 9:0,10:1-rel 11:2-ARG2 
nw/wsj/24/wsj_2404.parse 23 13 gold build-v 26.1-1 Building build.01 1 ----- 0:3*11:1-ARG0=Agent;Agent 13:0-rel 14:1-ARG1=Product;Created_entity 
nw/wsj/24/wsj_2404.parse 23 21 gold say-v 37.7-1 IN say.01 1 ----- 0:4*23:1-ARG1=Topic 18:1-ARG0=Agent 21:0-rel 
nw/wsj/24/wsj_2404.parse 24 15 gold add-v 37.7 NF add.01 1 ----- 1:2*16:1-ARG1=Topic 14:1-ARG0=Agent 15:0-rel 
nw/wsj/24/wsj_2404.parse 25 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-ADV 2:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/24/wsj_2404.parse 25 17 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 5:2-ARG1 16:0-ARGM-MOD 17:0-rel 18:2-ARG2 
nw/wsj/24/wsj_2404.parse 25 25 gold equip-v 13.4.2-1 Supply equip.01 1 ----- 24:0-ARGM-MNR 25:0-rel 26:0,27:0-ARG1=Recipient 
nw/wsj/24/wsj_2404.parse 26 10 gold announce-v 37.7-1 Statement announce.01 1 ----- 7:1*11:1-ARG1=Topic;Message 10:0-rel 12:2-ARGM-TMP 7:1*11:1-LINK-PSV 
nw/wsj/24/wsj_2404.parse 27 5 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARGM-TMP 4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/24/wsj_2404.parse 27 14 gold adjust-v 26.9 NF adjust.01 1 ----- 7:1*10:1*11:1*15:1-ARG1=Patient 13:0-ARGM-NEG 14:0-rel 16:1-ARG4 7:1*10:1-LINK-SLC 
nw/wsj/24/wsj_2404.parse 27 21 gold rise-v 45.6-1 Change_position_on_a_scale rise.01 2 ----- 7:2-ARG1 20:0-ARGM-MOD 21:0-rel 22:2-ARG2 29:1-ARGM-LOC 
nw/wsj/24/wsj_2404.parse 28 3 gold include-v 65 NF include.01 1 ----- 2:1-ARG2=Location 3:0-rel 4:2-ARG1=Theme 
nw/wsj/24/wsj_2404.parse 28 7 gold report-v 37.7-1 Statement report.01 1 ----- 4:1*8:1-ARG1 7:0-rel 9:1-ARGM-MNR 4:1*8:1-LINK-PSV 
nw/wsj/24/wsj_2404.parse 32 2 gold base-v 97.1 NF base.02 1 ----- 0:1-ARG1=Theme 2:0-rel 3:1-ARG2=Source 
