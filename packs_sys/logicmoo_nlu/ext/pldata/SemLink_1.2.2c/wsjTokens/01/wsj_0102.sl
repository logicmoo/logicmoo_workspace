nw/wsj/01/wsj_0102.parse 0 1 gold take-v 11.3 IN take.01 6 ----- 0:1-ARG0=Instrument 1:0-rel 2:1-ARG1=Theme 3:1-ARG2=Initial_Location 5:1-ARGM-LOC 
nw/wsj/01/wsj_0102.parse 1 1 gold agree-v 36.1-1 IN agree.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 1 10 gold need-v 32.1-1-1 Required_event need.01 2 ----- 3:2-ARG0=Pivot 10:0-rel 11:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 1 16 gold replace-v 13.6 IN replace.01 1 ----- 3:2*11:1*17:1-ARG1=Theme 16:0-rel 
nw/wsj/01/wsj_0102.parse 3 2 gold insist-v 37.7 Statement insist.01 2 ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/01/wsj_0102.parse 3 16 gold prevent-v 67 Preventing prevent.01 1 ----- 4:2*14:1-ARG0=Agent;Preventing_cause 16:0-rel 17:1-ARG1=Theme;Event 18:1-ARG2=Theme;Event 4:2*14:1-LINK-PRO 
nw/wsj/01/wsj_0102.parse 3 20 gold crash-v 18.4-1 Impact crash.01 1 ----- 17:1*19:1-ARG1=Theme;Impactor 20:0-rel 21:1-ARGM-DIR 17:1*19:1-LINK-PRO 
nw/wsj/01/wsj_0102.parse 4 5 gold want-v 32.1-1-1 Desiring want.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0=Pivot;Experiencer 4:0-ARGM-NEG 5:0-rel 6:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/01/wsj_0102.parse 4 8 gold lose-v 13.2 NF lose.02 6 ----- 1:1*6:1-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 5 16 gold provide-v 13.4.1-2 Supply provide.01 1 ----- 5:1*8:1-ARG0=Agent 14:0-ARGM-NEG 16:0-rel 17:1-ARG1=Theme 5:1*8:1-LINK-PRO 
nw/wsj/01/wsj_0102.parse 5 22 gold say-v 37.7-1 IN say.01 1 ----- 1:3*23:1-ARG1=Topic 22:0-rel 24:2-ARG0=Agent 
nw/wsj/01/wsj_0102.parse 6 3 gold prefer-v 31.2 Preference prefer.01 1 ----- 0:1*4:1-ARG0 3:0-rel 4:2-ARG1 
nw/wsj/01/wsj_0102.parse 6 6 gold install-v 9.1-1 IN install.01 1 ----- 0:1*4:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 7 11 gold use-v 105 IN use.01 1 ----- 0:1-ARGM-LOC 5:1*12:1-ARG1 11:0-rel 13:2-ARG2 
nw/wsj/01/wsj_0102.parse 7 15 gold replace-v 13.6 IN replace.01 1 ----- 5:1*12:1-ARG2=Co-Theme 13:1-ARG0=Agent 15:0-rel 16:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 8 5 gold teach-v 37.1.1-1-1 Education_teaching teach.01 1 ----- 0:1*3:1*4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 7:1-ARGM-LOC 0:1*3:1-LINK-SLC 
nw/wsj/01/wsj_0102.parse 8 11 gold call-v 29.3 Labeling call.01 5 ----- 0:2-ARG0=Agent 11:0-rel 12:1-ARG1=Theme 16:2-ARG2=Result 
nw/wsj/01/wsj_0102.parse 8 26 gold block-v 9.8 Eclipse block.01 1 ----- 23:1*24:1*25:1-ARG3=Theme 26:0-rel 27:2-ARG1=Destination 23:1*24:1-LINK-SLC 
nw/wsj/01/wsj_0102.parse 9 12 gold replace-v 13.6 IN replace.01 1 ----- 0:1-ARGM-LOC 5:1*13:1-ARG1=Theme 9:0-ARGM-MOD 10:1-ARGM-TMP 12:0-rel 15:3-ARGM-ADV 
nw/wsj/01/wsj_0102.parse 11 7 gold upset-v 31.1 Experiencer_obj upset.01 2 ----- 0:2*6:1-ARG1=Experiencer;Experiencer 7:0-rel 8:1-ARG0=Stimulus;Stimulus 
nw/wsj/01/wsj_0102.parse 11 14 gold negotiate-v 36.1 NF negotiate.01 1 ----- 0:2-ARG0=Agent 6:2-ARGM-ADV 14:0-rel 15:2-ARG2=Theme 
nw/wsj/01/wsj_0102.parse 11 34 gold have-v 100 IN have.03 2 ----- 30:1-ARG0=Pivot 33:0-ARGM-MOD 34:0-rel 35:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 12 4 gold agree-v 36.1-1 IN agree.01 3 ----- 0:1-ARGM-ADV 2:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 12 7 gold keep-v 15.2 Cause_to_continue keep.01 2 ----- 2:1*5:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme 20:2-ARGM-ADV 
nw/wsj/01/wsj_0102.parse 12 25 gold install-v 9.1-1 IN install.01 1 ----- 23:1-ARG0=Agent 24:0-ARGM-MOD 25:0-rel 26:1-ARG1=Theme 29:1-ARG2=Destination 
nw/wsj/01/wsj_0102.parse 14 2 gold compete-v 36.4-1 Competition compete.01 1 ----- 0:1-ARG0=Agent 2:0-rel 4:1-ARG1=Co-Agent 
nw/wsj/01/wsj_0102.parse 15 1 gold port-v 11.1 NF port.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 15 4 gold get-v 26.6.2 IN get.03 3 ----- 0:2-ARG1=Patient 3:1-ARGM-ADV 4:0-rel 5:1-ARG2=Goal 
nw/wsj/01/wsj_0102.parse 15 9 gold claim-v 37.7-1 Statement claim.01 1 ----- 8:1-ARG1=Topic;Message 9:0-rel 10:2-ARG0=Agent;Speaker 
nw/wsj/01/wsj_0102.parse 16 8 gold mean-v 29.5-1 NF mean.01 2 ----- 0:2*7:1-ARG0=Agent 8:0-rel 9:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 16 11 gold replace-v 13.6 IN replace.01 1 ----- 0:2*7:1*9:1-ARG2=Co-Theme 11:0-rel 12:1-ARG1=Theme 14:1-ARGM-LOC 
nw/wsj/01/wsj_0102.parse 16 27 gold use-v 105 IN use.01 1 ----- 24:1*28:1-ARG1 27:0-rel 29:1-ARGM-LOC 24:1*28:1-LINK-PSV 
nw/wsj/01/wsj_0102.parse 16 39 gold hang-v 9.2-1 IN hang.01 1 ----- 37:1-ARG1=Theme 39:0-rel 40:1-ARG2=Destination 
nw/wsj/01/wsj_0102.parse 17 4 gold tote-v 11.4 Bringing tote.01 null ----- 0:1-ARG0=Agent 3:0-ARGM-MOD 4:0-rel 5:2-ARG1=Theme 10:1-ARGM-TMP 
nw/wsj/01/wsj_0102.parse 18 3 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/01/wsj_0102.parse 18 8 gold eliminate-v 10.1 Removing eliminate.01 1 ----- 5:1-ARG0=Agent;Agent/Cause 7:1-ARGM-ADV 8:0-rel 9:1-ARG1=Theme;Theme 
nw/wsj/01/wsj_0102.parse 19 4 gold need-v 32.1-1-1 Needing need.01 1 ----- 0:1*5:1-ARG1=Theme 2:0-ARGM-NEG 3:1-ARGM-ADV 4:0-rel 
nw/wsj/01/wsj_0102.parse 20 2 gold claim-v 37.7-1 Statement claim.01 1 ----- 0:1-ARG0=Agent;Speaker 1:1-ARGM-DIS 2:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/01/wsj_0102.parse 20 6 gold cost-v 54.2 Expensiveness cost.01 1 ----- 4:1-ARG1=Theme;Goods 6:0-rel 7:1-ARG2=Value;Asset 
nw/wsj/01/wsj_0102.parse 21 7 gold give-v 13.1-1 Giving give.01 14 ----- 0:2-ARG0=Agent;Donor 7:0-rel 8:1-ARG2=Recipient;Recipient 9:1-ARG1=Theme;Theme 
nw/wsj/01/wsj_0102.parse 22 2 gold acknowledge-v 37.10 Statement acknowledge.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/01/wsj_0102.parse 23 5 gold find-v 13.5.1 IN find.01 1 ----- 0:1*3:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 23 10 gold hang-v 9.2-1 Placing hang.01 1 ----- 6:1*7:1*14:1-ARG2=Destination;Goal 8:1-ARG0=Agent;Agent/Cause 10:0-rel 11:1-ARG1=Theme;Theme 6:1*7:1-LINK-SLC 
nw/wsj/01/wsj_0102.parse 23 19 gold supply-v 13.4.1-1 NF supply.01 1 ----- 17:1-ARG0=Agent 19:0-rel 20:1-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 26 3 gold touch-v 20-1 Manipulation touch.01 3 ----- 0:2-ARG0=Agent 3:0-rel 4:1-ARG1=Experiencer 
nw/wsj/01/wsj_0102.parse 27 7 gold promise-v 13.3 Commitment promise.01 1 ----- 0:2-ARG0=Agent 7:0-rel 8:2-ARG2=Theme 
nw/wsj/01/wsj_0102.parse 29 2 gold run-v 47.7 IN run.04 2 ----- 0:1-ARG1=Theme 2:0-rel 3:2-ARG2 
nw/wsj/01/wsj_0102.parse 30 2 gold continue-v 55.3 Process_continue continue.01 1 ----- 0:1-ARG1 2:0-rel 3:1-ARGM-TMP 
nw/wsj/01/wsj_0102.parse 30 7 gold include-v 65 NF include.01 1 ----- 0:1-ARG2=Location 7:0-rel 8:1-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 30 13 gold swap-v 13.6-1-1 Exchange swap.01 1 ----- 11:1-ARG0=Agent 13:0-rel 14:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 31 11 gold see-v 30.1-1 Perception_experience see.01 2 ----- 0:2-ARG0=Experiencer;Perceiver_passive 11:0-rel 12:1-ARG1=Stimulus;Phenomenon 13:1-ARGM-LOC 
nw/wsj/01/wsj_0102.parse 32 6 gold have-v 100 IN have.03 5 ----- 0:1*3:1*4:1-ARG0=Pivot 5:1-ARGM-ADV 6:0-rel 7:2-ARG1=Theme 0:1*3:1-LINK-SLC 
nw/wsj/01/wsj_0102.parse 32 15 gold get-v 13.5.1-1 IN get.01 1 ----- 0:2-ARG0=Agent 14:0-ARGM-MOD 15:0-rel 16:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 32 25 gold say-v 37.7-1 IN say.01 1 ----- 0:3*27:1-ARG1=Topic 24:1-ARG0=Agent 25:0-rel 
nw/wsj/01/wsj_0102.parse 33 6 gold make-v 26.1-1 IN make.01 2 ----- 0:1*5:1-ARG0=Agent 6:0-rel 7:1-ARG1=Product 8:1-ARG3=Beneficiary 0:1*5:1-LINK-PRO 
nw/wsj/01/wsj_0102.parse 34 2 gold say-v 37.7-1 IN say.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/01/wsj_0102.parse 34 8 gold help-v 72-1 Assistance help.01 1 ----- 4:1-ARG0=Agent;Helper 6:0-ARGM-MOD 7:1-ARGM-ADV 8:0-rel 9:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/01/wsj_0102.parse 35 5 gold keep-v 55.6 Activity_ongoing keep.02 1 ----- 1:1-ARGM-LOC 4:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 35 7 gold ask-v 37.1.2 Questioning ask.01 1 ----- 4:1*6:1-ARG0=Agent;Speaker 7:0-rel 8:1-ARG2=Recipient;Addressee 9:2-ARG1=Topic;Message 
nw/wsj/01/wsj_0102.parse 35 16 gold make-v 26.1-1 Manufacturing make.01 2 ----- 13:1*32:1-ARGM-PNC 15:1-ARG0=Agent;Manufacturer 16:0-rel 17:1-ARG1=Product;Product 21:2-ARGM-ADV 
nw/wsj/01/wsj_0102.parse 35 25 gold need-v 32.1-1-1 Needing need.01 1 ----- 22:1*23:1*26:1-ARG1=Theme 24:1-ARG0=Pivot 25:0-rel 22:1*23:1-LINK-SLC 
nw/wsj/01/wsj_0102.parse 35 37 gold say-v 37.7-1 IN say.01 1 ----- 1:2*38:1-ARG1=Topic 36:1-ARG0=Agent 37:0-rel 
nw/wsj/01/wsj_0102.parse 36 2 gold get-v 26.6.2 IN get.04 3 ----- 1:1-ARG0=Agent 2:0-rel 3:2-ARG1=Patient 
nw/wsj/01/wsj_0102.parse 36 4 gold think-v 29.9-2 IN think.01 2 ----- 3:1-ARG0 4:0-rel 5:1-ARG1 
nw/wsj/01/wsj_0102.parse 36 10 gold help-v 72-1 Assistance help.01 1 ----- 6:1-ARGM-ADV 7:1-ARG0=Agent;Helper 8:0-ARGM-MOD 10:0-rel 11:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/01/wsj_0102.parse 36 14 gold exist-v 47.1-1 Existence exist.01 null ----- 14:0-rel 15:0-ARG1=Theme;Entity 
nw/wsj/01/wsj_0102.parse 36 20 gold develop-v 26.1 IN develop.02 1 ----- 7:1*18:1-ARG0=Agent 19:1-ARGM-TMP 20:0-rel 21:1-ARG1=Product 
nw/wsj/01/wsj_0102.parse 37 4 gold fail-v 74-2 IN fail.01 1 ----- 0:2-ARG1 4:0-rel 5:2-ARG2 
nw/wsj/01/wsj_0102.parse 38 13 gold propose-v 37.7-1 Statement propose.01 1 ----- 10:2-ARG0=Agent;Speaker 13:0-rel 14:2-ARG1=Topic;Message 
nw/wsj/01/wsj_0102.parse 38 15 gold use-v 105 IN use.01 1 ----- 10:2*14:1-ARG0 15:0-rel 16:1-ARG1 18:2-ARG2 
nw/wsj/01/wsj_0102.parse 38 20 gold house-v 9.10 Provide_lodging house.01 1 ----- 10:2*14:1*18:1-ARG0=Agent 20:0-rel 21:1-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 39 13 gold say-v 37.7-1 IN say.01 1 ----- 0:2*15:1-ARG1=Topic 11:1-ARG0=Agent 13:0-rel 
nw/wsj/01/wsj_0102.parse 40 16 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:3-ARG0=Agent 16:0-rel 18:1-ARG1=Topic 
nw/wsj/01/wsj_0102.parse 41 2 gold build-v 26.1-1 Building build.01 1 ----- 0:1*3:1-ARG1=Product;Created_entity 2:0-rel 4:1-ARGM-TMP 0:1*3:1-LINK-PSV 
nw/wsj/01/wsj_0102.parse 41 9 gold have-v 100 IN have.03 1 ----- 0:2-ARG0=Pivot 9:0-rel 10:3-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 41 23 gold contain-v 54.3 IN contain.01 1 ----- 16:1*19:1*20:1-ARG0=Agent 21:0-ARGM-MOD 22:1-ARGM-MNR 23:0-rel 24:2-ARG1 16:1*19:1-LINK-SLC 
nw/wsj/01/wsj_0102.parse 41 38 gold say-v 37.7-1 IN say.01 1 ----- 0:3*40:1-ARG1=Topic 34:1-ARG0=Agent 38:0-rel 
nw/wsj/01/wsj_0102.parse 42 18 gold permit-v 64 Grant_permission permit.01 1 ----- 16:1-ARG0=Agent 18:0-rel 19:1-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 43 3 gold say-v 37.7-1 IN say.01 1 ----- 2:1-ARG0=Agent 3:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/01/wsj_0102.parse 43 20 gold compare-v 22.2-2 Evaluative_comparison compare.01 2 ----- 17:1-ARG0=Agent 20:0-rel 21:1-ARG1=Patient 22:1-ARG2=Co-Patient 
nw/wsj/01/wsj_0102.parse 44 12 gold have-v 100 IN have.03 1 ----- 0:2-ARG0=Pivot 12:0-rel 13:1-ARG1=Theme 
nw/wsj/01/wsj_0102.parse 44 18 gold say-v 37.7-1 IN say.01 1 ----- 0:3*20:1-ARG1=Topic 17:1-ARG0=Agent 18:0-rel 
nw/wsj/01/wsj_0102.parse 45 9 gold say-v 37.7-1 IN say.01 1 ----- 8:1-ARG0=Agent 9:0-rel 0:3-ARG1-DSP=Topic 
nw/wsj/01/wsj_0102.parse 45 19 gold house-v 54.3 Provide_lodging house.01 1 ----- 0:2*16:1*17:1-ARG2=Location 19:0-rel 20:2-ARG1=Value 24:1-ARGM-MNR 
nw/wsj/01/wsj_0102.parse 45 30 gold build-v 26.1-1 Building build.01 1 ----- 29:1-ARG0=Agent;Agent 30:0-rel 31:1-ARG1=Product;Created_entity 34:1-ARGM-LOC 
nw/wsj/01/wsj_0102.parse 46 8 gold call-v 29.3 Labeling call.01 5 ----- 0:2-ARG0=Agent 6:1-ARGM-DIS 8:0-rel 9:1-ARG1=Theme 11:2-ARG2=Result 
