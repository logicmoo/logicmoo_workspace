nw/wsj/16/wsj_1600.parse 0 5 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 4:1-ARG0=Agent;Buyer 5:0-rel 8:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1600.parse 0 7 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 4:1-ARG0=Agent;Seller 7:0-rel 8:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1600.parse 1 15 gold trade-v 13.6-1 Exchange trade.01 null ----- 0:3*16:1-ARG1=Theme 15:0-rel 17:3-ARGM-TMP 
nw/wsj/16/wsj_1600.parse 1 26 gold estimate-v 54.4 Estimating estimate.01 1 ----- 0:4*28:1-ARG1=Theme 24:1-ARG0=Agent 26:0-rel 
nw/wsj/16/wsj_1600.parse 2 1 gold make-v 29.3 Cause_change make.02 null ----- 0:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 15:1-ARGM-ADV 
nw/wsj/16/wsj_1600.parse 3 2 gold wait-v 47.1-1 IN wait.01 null ----- 0:0-ARGM-DIS 1:1-ARG1=Theme 2:0-rel 3:1-ARGM-TMP 
nw/wsj/16/wsj_1600.parse 6 6 gold involve-v 86.2-1 NF involve.01 null ----- 0:1*7:1-ARG2 1:1-ARGM-ADV 6:0-rel 7:2-ARG1 20:2-ARGM-PRP 
nw/wsj/16/wsj_1600.parse 6 9 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*7:1-ARG0=Agent;Buyer 8:1-ARGM-ADV 9:0-rel 10:1*15:2-ARG1=Theme;Goods 
nw/wsj/16/wsj_1600.parse 6 13 gold hold-v 15.1-1 Manipulation hold.01 null ----- 0:1*7:1-ARG0=Agent;Agent 8:1-ARGM-ADV 12:1-ARGM-TMP 13:0-rel 14:1*15:2-ARG1=Theme;Entity 
nw/wsj/16/wsj_1600.parse 6 40 gold match-v 22.2-1 Compatibility match.01 1 ----- 0:1*20:1-ARG1=Patient;Item_1/Items 40:0-rel 41:1-ARG1=Patient;Item_1/Items 0:1*20:1-LINK-PRO 
nw/wsj/16/wsj_1600.parse 7 3 gold pour-v 43.4 Mass_motion pour.01 3 ----- 0:1-ARG0=Source;Source 3:0-rel 4:2-ARG1=Theme;Mass_theme 8:1-ARG3 13:1-ARGM-PRP 
nw/wsj/16/wsj_1600.parse 7 24 gold promise-v 13.3 Commitment promise.01 null ----- 14:2*22:1*23:1*25:1-ARG0=Agent 24:0-rel 25:2-ARG2=Theme 14:2*22:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 7 27 gold post-v 11.1 Sending post.01 null ----- 14:2*22:1*23:1*25:1-ARG0=Agent;Sender 27:0-rel 28:1-ARG1=Theme;Theme 14:2*22:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 8 4 gold flock-v 47.5.2 Mass_motion flock.02 1 ----- 0:1-ARG0=Agent 4:0-rel 5:1-ARG1=Theme;Mass_theme 7:1-ARGM-CAU 
nw/wsj/16/wsj_1600.parse 8 21 gold match-v 22.2-1 Compatibility match.01 2 ----- 8:2*18:1-ARG0=Agent 20:1-ARGM-MNR 21:0-rel 22:2-ARG1=Patient;Item_1/Items 
nw/wsj/16/wsj_1600.parse 10 14 gold keep-v 55.6 Cause_to_continue keep.04 null ----- 11:1-ARG0=Agent 14:0-rel 15:1-ARG1=Theme 17:1-ARG2 
nw/wsj/16/wsj_1600.parse 11 5 gold move-v 45.6-1 Motion move.01 null ----- 0:0-ARGM-DIS 3:1-ARG1 5:0-rel 6:1-ARGM-DIR 11:1-ARG2 
nw/wsj/16/wsj_1600.parse 12 8 gold satisfy-v 31.1 Experiencer_obj satisfy.01 1 ----- 1:2-ARG1=Experiencer;Experiencer 6:1-ARGM-NEG 8:0-rel 9:1-ARG2 
nw/wsj/16/wsj_1600.parse 12 17 gold develop-v 26.1 IN develop.02 null ----- 0:1-ARGM-CAU 15:1-ARG0=Agent 17:0-rel 18:2-ARG1=Product 
nw/wsj/16/wsj_1600.parse 12 26 gold intend-v 62 Purpose intend.01 null ----- 18:1*23:1*24:1*27:1-ARG2 26:0-rel 28:1-ARG1 18:1*23:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 13 24 gold sit-v 47.6 Being_located sit.01 null ----- 16:1-ARG1=Theme 21:1-ARGM-ADV 22:1-ARGM-ADV 24:0-rel 25:1-ARG2=Location 
nw/wsj/16/wsj_1600.parse 13 28 gold say-v 37.7-1 IN say.01 null ----- 1:3*29:1-ARG1=Topic 28:0-rel 30:2-ARG0=Agent 
nw/wsj/16/wsj_1600.parse 15 12 gold build-v 26.1-1 Building build.01 null ----- 0:1-ARGM-LOC 8:1*13:1-ARG1=Product;Created_entity 10:0-ARGM-MOD 12:0-rel 14:1-ARG2=Material;Components 25:1-ARGM-ADV 
nw/wsj/16/wsj_1600.parse 15 29 gold restrict-v 76 NF restrict.01 1 ----- 8:1*27:1*30:1-ARG1=Patient 29:0-rel 31:1-ARG2=Goal 
nw/wsj/16/wsj_1600.parse 16 9 gold focus-v 87.1 Place_weight_on focus.01 null ----- 0:1-ARG0=Experiencer 1:0-ARGM-MOD 9:0-rel 10:1-ARG2=Theme 
nw/wsj/16/wsj_1600.parse 17 13 gold trade-v 13.6-1 Exchange trade.01 2 ----- 0:1-ARGM-ADV 10:1-ARG1=Theme 12:0-ARGM-MOD 13:0-rel 14:1-ARGM-DIR 17:1-ARG2=Co-Agent 
nw/wsj/16/wsj_1600.parse 17 26 gold make-v 26.1-1 IN make.07 null ----- 23:1-ARG0=Material 26:0,27:1-rel 28:1,29:1-ARG1=Product 
nw/wsj/16/wsj_1600.parse 18 4 gold make-v 29.3 Causation make.02 null ----- 0:1-ARG0=Agent 3:1-ARGM-DIS 4:0-rel 5:3-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 18 10 gold build-v 26.1-1 Building build.01 null ----- 8:1-ARG0=Agent;Agent 10:0-rel 11:2-ARG1=Product;Created_entity 
nw/wsj/16/wsj_1600.parse 18 21 gold own-v 100 NF own.01 null ----- 11:1*16:1*17:1-ARG0=Pivot 19:0-ARGM-NEG 20:1-ARGM-ADV 21:0-rel 22:2-ARG1=Theme 11:1*16:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 18 34 gold match-v 22.2-1 Compatibility match.01 1 ----- 31:1*32:1*33:1-ARG1=Patient;Item_1/Items 34:0-rel 37:1-ARG1=Patient;Item_1/Items 31:1*32:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 18 36 gold exceed-v 90-1 NF exceed.01 1 ----- 31:1*32:1*33:1-ARG0=Theme 36:0-rel 37:1-ARG1=Co-Theme 31:1*32:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 20 15 gold wait-v 47.1-1 IN wait.01 null ----- 4:2*13:1*16:1-ARG1=Theme 14:1-ARGM-TMP 15:0-rel 16:2-ARG2 
nw/wsj/16/wsj_1600.parse 20 18 gold get-v 26.6.2 IN get.03 null ----- 4:2*13:1*16:1-ARG1=Patient 18:0-rel 19:1-ARG2=Goal 
nw/wsj/16/wsj_1600.parse 20 23 gold say-v 37.7-1 IN say.01 null ----- 1:2*24:1-ARG1=Topic 23:0-rel 25:2-ARG0=Agent 
nw/wsj/16/wsj_1600.parse 20 41 gold offer-v 13.3 NF offer.01 null ----- 32:2*39:1*40:1-ARG0=Agent 41:0-rel 42:1-ARG1=Theme 32:2*39:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 21 15 gold hold-v 15.1-1 Containing hold.01 null ----- 1:1-ARGM-TMP 12:1-ARG0=Agent 14:1-ARGM-TMP 15:0-rel 16:3-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 21 22 gold own-v 100 NF own.01 null ----- 20:1*23:1-ARG1=Theme 22:0-rel 24:1-ARG0=Pivot 20:1*23:1-LINK-PSV 
nw/wsj/16/wsj_1600.parse 23 13 gold charge-v 54.5 Commerce_collect charge.01 null ----- 9:1*10:1*14:1-ARG1=Asset 11:1-ARG0=Agent 12:0-ARGM-MOD 13:0-rel 9:1*10:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 23 16 gold plunge-v 45.6-1 Motion_directional plunge.01 2 ----- 9:2-ARG1=Patient 16:0-rel 17:1-ARG4 
nw/wsj/16/wsj_1600.parse 24 2 gold land-v 9.10-1 NF land.02 2 ----- 0:1*13:1-ARG0=Agent 2:0-rel 3:2-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 25 5 gold get-v 13.5.1-1 IN get.01 null ----- 4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 9:2-ARGM-TMP 
nw/wsj/16/wsj_1600.parse 25 17 gold say-v 37.7-1 IN say.01 null ----- 1:2*18:1-ARG1=Topic 17:0-rel 19:2-ARG0=Agent 
nw/wsj/16/wsj_1600.parse 26 13 gold give-v 13.1-1 Giving give.01 null ----- 4:1*11:1-ARG0=Agent;Donor 9:1*10:1-ARGM-MNR 13:0-rel 14:1-ARG2=Recipient;Recipient 15:2-ARG1=Theme;Theme 20:1-ARGM-PRP 4:1*11:1-LINK-PRO 9:1*10:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 28 3 gold use-v 105 IN use.01 null ----- 0:1*7:1-ARG0 3:0-rel 4:1-ARG1 7:2-ARG2 40:1-ARGM-ADV 
nw/wsj/16/wsj_1600.parse 28 34 gold hold-v 15.1-1 Manipulation hold.01 null ----- 33:1-ARG0=Agent;Agent 34:0-rel 35:1-ARG1=Theme;Entity 
nw/wsj/16/wsj_1600.parse 29 1 gold vary-v 45.6-1 NF vary.01 1 ----- 0:1-ARG1 1:0-rel 2:1-ARGM-PNC 
nw/wsj/16/wsj_1600.parse 29 8 gold have-v 100 IN have.03 null ----- 3:1*6:1*7:1-ARG0=Pivot 8:0-rel 9:3-ARG1=Theme 3:1*6:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 30 3 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*33:1-ARG0=Agent;Buyer 2:1-ARGM-TMP 3:0-rel 4:2-ARG1=Theme;Goods 7:2-ARGM-TMP 
nw/wsj/16/wsj_1600.parse 30 10 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 7:1*18:1-ARGM-TMP 8:1-ARG1=Theme;Goods 10:0-rel 11:1-ARG3 
nw/wsj/16/wsj_1600.parse 30 22 gold switch-v 26.6.2-1 IN switch.01 1 ----- 0:1-ARG0=Agent 2:1-ARGM-TMP 21:0-ARGM-MOD 22:0-rel 23:1-ARGM-DIR 26:1-ARG1=Patient 33:2-ARGM-PRP 
nw/wsj/16/wsj_1600.parse 30 35 gold take-v 10.5 IN take.01 null ----- 0:1*33:1-ARG0=Agent 35:0-rel 36:1-ARG1=Theme 37:1-ARG2=Source 
nw/wsj/16/wsj_1600.parse 31 8 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 5:1*6:1*22:1-ARGM-TMP 7:1*14:1-ARG0=Agent;Buyer 8:0-rel 9:1-ARG1=Theme;Goods 10:1-ARGM-MNR 14:2-ARGM-PRP 5:1*6:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 31 16 gold boost-v 102 NF boost.01 1 ----- 7:1*14:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 32 6 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:0-ARGM-DIS 1:1-ARGM-TMP 5:1-ARG0=Agent;Buyer 6:0-rel 7:3-ARG1=Theme;Goods 
nw/wsj/16/wsj_1600.parse 33 17 gold create-v 27 Creating create.01 null ----- 1:1-ARGM-ADV 5:1*15:1-ARG0=Cause 14:0-ARGM-MOD 17:0-rel 18:1-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 33 24 gold add-v 37.7 NF add.01 null ----- 1:2*25:1-ARG1=Topic 22:1-ARG0=Agent 24:0-rel 
nw/wsj/16/wsj_1600.parse 35 6 gold use-v 105 IN use.01 null ----- 0:2-ARG0 5:1-ARGM-DIS 6:0-rel 7:1-ARG1 8:1-ARGM-LOC 
nw/wsj/16/wsj_1600.parse 35 16 gold add-v 22.1-2 Statement add.02 null ----- 9:1*11:1*14:1-ARG0=Agent 12:1-ARGM-ADV 16:0-rel 17:1-ARG1=Patient 20:1-ARG2=Co-Patient 9:1*11:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 36 14 gold agree-v 36.1-1 IN agree.01 null ----- 0:2-ARG0=Agent 14:0-rel 15:1-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 37 3 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 1:1-ARG0=Agent;Buyer 3:0-rel 4:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1600.parse 37 10 gold want-v 32.1-1-1 Desiring want.01 null ----- 8:1*11:1-ARG0=Pivot;Experiencer 9:1-ARGM-TMP 10:0-rel 11:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/16/wsj_1600.parse 37 13 gold hold-v 15.1-1 Manipulation hold.01 null ----- 8:1*11:1-ARG0=Agent;Agent 13:0-rel 14:2-ARG1=Theme;Entity 
nw/wsj/16/wsj_1600.parse 37 22 gold say-v 37.7-1 IN say.01 null ----- 1:3*23:1-ARG1=Topic 21:1-ARG0=Agent 22:0-rel 
nw/wsj/16/wsj_1600.parse 38 12 gold say-v 37.7-1 IN say.01 null ----- 1:1*11:1-ARG0=Agent 12:0-rel 13:1-ARG1=Topic 
nw/wsj/16/wsj_1600.parse 38 16 gold present-v 13.4.1 NF present.01 null ----- 14:1-ARG0=Agent 16:0-rel 17:2-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 38 17 gold add-v 22.1-2 Statement add.02 null ----- 17:0-rel 18:0-ARG1=Patient 
nw/wsj/16/wsj_1600.parse 39 5 gold have-v 100 IN have.03 null ----- 2:2-ARG0=Pivot 5:0-rel 6:1-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 39 13 gold have-v 100 IN have.03 null ----- 1:1-ARGM-ADV 9:0-ARGM-DIS 10:1-ARG0=Pivot 12:0-ARGM-MOD 13:0-rel 14:1-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 39 18 gold say-v 37.7-1 IN say.01 null ----- 1:2*19:1-ARG1=Topic 18:0-rel 20:2-ARG0=Agent 
nw/wsj/16/wsj_1600.parse 40 7 gold offer-v 13.3 NF offer.01 null ----- 5:1-ARG0 7:0-rel 8:2-ARG1 
nw/wsj/16/wsj_1600.parse 40 11 gold offer-v 13.3 NF offer.01 null ----- 8:1-ARG0 11:0-rel 12:2-ARG1 
nw/wsj/16/wsj_1600.parse 41 1 gold add-v 22.1-2 Statement add.02 null ----- 1:0-rel 2:0-ARG1 
nw/wsj/16/wsj_1600.parse 41 11 gold guarantee-v 37.13 NF guarantee.01 1 ----- 5:1*12:1-ARG1 9:0-ARGM-MOD 11:0-rel 13:1-ARG0 
nw/wsj/16/wsj_1600.parse 43 1 gold include-v 65 NF include.01 null ----- 0:1-ARG2 1:0-rel 
nw/wsj/16/wsj_1600.parse 45 1 gold guarantee-v 99 NF guarantee.01 null ----- 0:1*8:1-ARG0 1:0-rel 2:2-ARG1 7:1-ARGM-TMP 
nw/wsj/16/wsj_1600.parse 45 10 gold pass-v 11.1-1 Giving pass.05 null ----- 0:1*8:1-ARG0 9:1-ARGM-ADV 10:0-rel 11:1-ARGM-DIR 12:1-ARG1 
nw/wsj/16/wsj_1600.parse 46 4 gold promise-v 13.3 Commitment promise.01 null ----- 0:1-ARGM-LOC 1:1*5:1-ARG0=Agent 3:0-ARGM-MOD 4:0-rel 5:2-ARG2=Theme 
nw/wsj/16/wsj_1600.parse 46 20 gold invest-v 13.4.2 NF invest.01 null ----- 16:1*21:1-ARG1=Theme 20:0-rel 22:1-ARGM-TMP 16:1*21:1-LINK-PSV 
nw/wsj/16/wsj_1600.parse 47 3 gold invest-v 13.4.2 NF invest.01 null ----- 0:1*16:1-ARG0=Agent 2:0-ARGM-MOD 3:0-rel 4:1-ARG1=Theme 7:1-ARGM-TMP 10:1-ARG2=Recipient 16:2-ARGM-PRP 
nw/wsj/16/wsj_1600.parse 47 18 gold return-v 11.1 NF return.02 null ----- 0:1*16:1-ARG0 18:0-rel 19:1-ARG1 
nw/wsj/16/wsj_1600.parse 47 20 gold guarantee-v 99 NF guarantee.01 null ----- 20:0-rel 21:0,22:0-ARG1 
nw/wsj/16/wsj_1600.parse 48 1 gold leave-v 13.4.1 NF leave.02 null ----- 0:1-ARG0 1:0-rel 2:2-ARG1 
nw/wsj/16/wsj_1600.parse 48 14 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 12:1-ARG0=Agent;Buyer 14:0-rel 15:3-ARG1=Theme;Goods 
nw/wsj/16/wsj_1600.parse 48 22 gold match-v 22.2-1 Compatibility match.01 1 ----- 15:2*18:1*19:1-ARG1=Patient;Item_1/Items 20:0-ARGM-MOD 21:1-ARGM-MNR 22:0-rel 23:2-ARG1=Patient;Item_1/Items 15:2*18:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 50 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/16/wsj_1600.parse 51 3 gold hire-v 13.5.3 Hiring hire.01 null ----- 2:1*15:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/16/wsj_1600.parse 51 29 gold maintain-v 55.6 Activity_ongoing maintain.01 null ----- 25:1*30:1-ARG1 27:0-ARGM-MOD 29:0-rel 31:1-ARG0 34:2-ARGM-ADV 
nw/wsj/16/wsj_1600.parse 51 35 gold reduce-v 76 Cause_change_of_position_on_a_scale reduce.01 null ----- 34:1-ARG0 35:0-rel 36:1-ARG1 45:1-ARGM-CAU 
nw/wsj/16/wsj_1600.parse 52 2 gold see-v 30.1-1 Perception_experience see.01 null ----- 1:1-ARG0 2:0-rel 3:2-ARG1 
nw/wsj/16/wsj_1600.parse 52 14 gold say-v 37.7-1 IN say.01 null ----- 1:2*15:1-ARG1 14:0-rel 16:2-ARG0 
nw/wsj/16/wsj_1600.parse 53 8 gold use-v 105 IN use.01 null ----- 1:2-ARG0 8:0-rel 9:1-ARG1 10:1-ARG2 
nw/wsj/16/wsj_1600.parse 57 10 gold consider-v 29.9-1-1-1 Categorization consider.01 null ----- 7:2*11:1-ARG1=Theme;Item 10:0-rel 12:1-ARGM-PRD 7:2*11:1-LINK-PRO 
nw/wsj/16/wsj_1600.parse 57 17 gold avoid-v 52 Avoiding avoid.01 null ----- 15:1-ARG0=Agent;Agent 16:0-ARGM-MOD 17:0-rel 18:1-ARG1=Theme;Undesirable_situation 
nw/wsj/16/wsj_1600.parse 57 24 gold hurt-v 31.1 Experience_bodily_harm hurt.01 null ----- 15:1-ARG0 22:1-ARGM-ADV 23:0-ARGM-MOD 24:0-rel 25:2-ARGM-TMP 
nw/wsj/16/wsj_1600.parse 58 11 gold use-v 105 IN use.01 null ----- 3:2*16:1-ARG0 10:0-ARGM-MOD 11:0-rel 12:2-ARG1 16:2-ARG2 
nw/wsj/16/wsj_1600.parse 58 18 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 3:2*16:1-ARG0=Agent;Buyer 18:0-rel 19:1-ARG1=Theme;Goods 
nw/wsj/16/wsj_1600.parse 60 8 gold serve-v 13.4.1-1 NF serve.02 null ----- 0:1*5:1*6:1-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 0:1*5:1-LINK-SLC 
nw/wsj/16/wsj_1600.parse 60 15 gold eliminate-v 10.1 Removing eliminate.01 null ----- 14:1-ARG0=Agent;Agent/Cause 15:0-rel 16:2-ARG1=Theme;Theme 
nw/wsj/16/wsj_1600.parse 61 7 gold concentrate-v 87.1 NF concentrate.01 1 ----- 0:1*5:1-ARG0=Experiencer 7:0-rel 8:1-ARG1=Theme 14:1-ARGM-PRP 
nw/wsj/16/wsj_1600.parse 61 18 gold encourage-v 102 Attempt_suasion encourage.02 null ----- 16:1-ARG0 18:0-rel 19:1-ARG1 
nw/wsj/16/wsj_1600.parse 62 9 gold have-v 100 IN have.03 null ----- 0:3-ARG0=Pivot 6:1-ARGM-ADV 9:0-rel 10:3-ARG1=Theme 
nw/wsj/16/wsj_1600.parse 62 15 gold invest-v 13.4.2 NF invest.01 null ----- 10:2*16:1-ARG1=Theme 15:0-rel 17:1-ARG2=Recipient 10:2*16:1-LINK-PSV 
nw/wsj/16/wsj_1600.parse 62 31 gold have-v 100 IN have.03 null ----- 21:1*23:1*24:1-ARG0=Pivot 31:0-rel 32:2-ARG1=Theme 38:1-ARGM-LOC 21:1*23:1-LINK-SLC 
