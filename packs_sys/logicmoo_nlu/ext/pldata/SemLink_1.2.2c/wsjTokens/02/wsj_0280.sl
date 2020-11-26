nw/wsj/02/wsj_0280.parse 1 8 gold decline-v 45.6-1 Change_position_on_a_scale decline.01 null ----- 8:0-rel 9:0,10:0-ARG1=Patient;Item 
nw/wsj/02/wsj_0280.parse 1 15 gold launch-v 55.5-1 IN launch.01 null ----- 0:1*12:1-ARG0 0:2-ARGM-PRP 15:0-rel 16:2-ARG1 
nw/wsj/02/wsj_0280.parse 2 4 gold dub-v 29.3 Name_conferral dub.01 1 ----- 1:1*5:1-ARG1=Theme;Entity 4:0-rel 6:2-ARG2=Result;Name 1:1*5:1-LINK-PRO 
nw/wsj/02/wsj_0280.parse 2 13 gold send-v 11.1-1 Sending send.01 null ----- 10:1*11:1*12:1-ARG0=Agent;Sender 13:0-rel 14:1-ARGM-DIR 15:2-ARG1=Theme;Theme 10:1*11:1-LINK-SLC 
nw/wsj/02/wsj_0280.parse 2 21 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARGM-LOC 10:2-ARG0=Agent;Donor 20:0-ARGM-MOD 21:0-rel 22:2-ARG1=Theme;Theme 
nw/wsj/02/wsj_0280.parse 3 4 gold purchase-v 13.5.2-1 Commerce_buy purchase.01 1 ----- 0:1-ARG0=Agent;Buyer 2:1-ARGM-DIS 3:0-ARGM-MOD 4:0-rel 5:1-ARG1=Theme;Goods 7:1-ARG3=Asset 
nw/wsj/02/wsj_0280.parse 4 5 gold mount-v 47.7 NF mount.02 null ----- 5:0-rel 6:0-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 4 14 gold consider-v 29.9-1-1-1 Categorization consider.01 null ----- 0:0-ARGM-DIS 1:1-ARGM-TMP 11:1-ARG0=Agent;Cognizer 14:0-rel 15:3-ARG1=Theme;Item 
nw/wsj/02/wsj_0280.parse 5 11 gold say-v 37.7-1 IN say.01 null ----- 1:2*12:1-ARG1=Topic 11:0-rel 13:2-ARG0=Agent 
nw/wsj/02/wsj_0280.parse 6 10 gold say-v 37.7-1 IN say.01 null ----- 7:1*8:1*9:1-ARG0=Agent 10:0-rel 11:2-ARG1=Topic 7:1*8:1-LINK-SLC 
nw/wsj/02/wsj_0280.parse 6 13 gold drink-v 39.1-2 Ingestion drink.01 2 ----- 12:1-ARG0=Agent;Ingestor 13:0-rel 14:1-ARG1=Patient;Ingestibles 
nw/wsj/02/wsj_0280.parse 6 17 gold get-v 13.5.1-1 IN get.01 null ----- 11:1-ARGM-ADV 16:1-ARG0=Agent 17:0-rel 18:1-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 8 7 gold say-v 37.7-1 IN say.01 null ----- 1:2*8:1-ARG1=Topic 7:0-rel 9:2-ARG0=Agent 
nw/wsj/02/wsj_0280.parse 9 2 gold have-v 100 IN have.03 null ----- 1:1-ARG0=Pivot 2:0-rel 3:2-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 9 10 gold savor-v 31.2 NF savor.01 1 ----- 6:1*7:1-ARG1=Attribute 8:1-ARG0=Experiencer 9:0-ARGM-MOD 10:0-rel 6:1*7:1-LINK-SLC 
nw/wsj/02/wsj_0280.parse 9 14 gold guzzle-v 39.3-2 Ingestion guzzle.01 null ----- 6:1*7:1*15:1-ARG1=Patient;Ingestibles 8:1-ARG0=Agent;Ingestor 14:0-rel 6:1*7:1-LINK-SLC 
nw/wsj/02/wsj_0280.parse 11 5 gold offer-v 13.3 NF offer.01 null ----- 0:1-ARGM-TMP 3:1-ARG0=Agent 5:0-rel 6:1-ARG2 11:1-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 12 3 gold give-v 13.1-1 Giving give.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent;Donor 3:0-rel 4:2-ARG1=Theme;Theme 8:1-ARG2=Recipient;Recipient 
nw/wsj/02/wsj_0280.parse 12 12 gold send-v 11.1-1 Sending send.01 null ----- 9:1*10:1*11:1-ARG0=Agent;Sender 12:0-rel 13:1-ARGM-DIR 14:1-ARG1=Theme;Theme 9:1*10:1-LINK-SLC 
nw/wsj/02/wsj_0280.parse 13 8 gold set-v 9.1-2 IN set.01 null ----- 0:0-ARGM-DIS 1:2-ARG0=Agent 8:0-rel 9:1-ARG1=Theme 10:1-ARG2=Destination 
nw/wsj/02/wsj_0280.parse 15 1 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 1:0-rel 2:1-ARG1=Topic 
nw/wsj/02/wsj_0280.parse 15 14 gold promote-v 102 NF promote.02 null ----- 3:1*7:1-ARG0=Agent 14:0-rel 15:1-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 16 1 gold ask-v 58.2 Request ask.02 null ----- 0:1-ARG0 1:0-rel 2:1*3:1-ARG2 3:2-ARG1 
nw/wsj/02/wsj_0280.parse 16 5 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 2:1*3:1-ARG0=Agent;Buyer 5:0-rel 6:2-ARG1=Theme;Goods 11:1-ARGM-TMP 
nw/wsj/02/wsj_0280.parse 16 18 gold say-v 37.7-1 IN say.01 null ----- 18:0-rel 19:1-ARG1=Topic 21:2-ARG0=Agent 
nw/wsj/02/wsj_0280.parse 17 4 gold ask-v 60-1 Request ask.02 null ----- 1:1-ARG0 3:0-ARGM-NEG 4:0-rel 5:1*6:1-ARG2 6:2-ARG1 
nw/wsj/02/wsj_0280.parse 18 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-ARGM-NEG 3:0-rel 4:2-ARG1=Topic 
nw/wsj/02/wsj_0280.parse 18 5 gold drink-v 39.1-2 Ingestion drink.01 2 ----- 4:1-ARG0=Agent;Ingestor 5:0-rel 6:1-ARG1=Patient;Ingestibles 
nw/wsj/02/wsj_0280.parse 18 10 gold say-v 37.7-1 IN say.01 null ----- 8:1-ARG0=Agent 10:0-rel 11:2-ARG1=Topic 
nw/wsj/02/wsj_0280.parse 18 12 gold trade-v 13.6-1 Exchange trade.01 2 ----- 11:1-ARG0=Agent 12:0-rel 13:1-ARGM-DIR 
nw/wsj/02/wsj_0280.parse 20 17 gold appeal-v 31.4-3 NF appeal.03 3 ----- 0:1*15:1-ARG0=Stimulus 17:0-rel 18:1-ARG1=Experiencer 
nw/wsj/02/wsj_0280.parse 21 4 gold believe-v 29.5-1 Awareness believe.01 null ----- 0:0-ARGM-DIS 1:1-ARG0 4:0-rel 5:1-ARG1 
nw/wsj/02/wsj_0280.parse 21 8 gold milk-v 10.7 NF milk.01 1 ----- 6:1*12:1-ARG0=Agent 7:0-ARGM-MOD 8:0-rel 9:1-ARG1=Theme 11:1-ARGM-MNR 
nw/wsj/02/wsj_0280.parse 21 13 gold buck-v 49 NF buck.01 1 ----- 6:1*12:1-ARG0=Agent 13:0-rel 14:1-ARG1=Patient 
nw/wsj/02/wsj_0280.parse 22 14 gold call-v 29.3 Being_named call.01 null ----- 12:1*15:1-ARG1=Theme;Entity 14:0-rel 16:2-ARG2=Result;Name 12:1*15:1-LINK-PRO 
nw/wsj/02/wsj_0280.parse 22 27 gold target-v 98 NF target.01 1 ----- 22:1*28:1-ARG2=Instrument 27:0-rel 29:1-ARG1=Theme 22:1*28:1-LINK-PSV 
nw/wsj/02/wsj_0280.parse 23 2 gold give-v 13.1-1 Giving give.01 null ----- 0:1-ARG0=Agent;Donor 2:0-rel 3:1-ARG2=Recipient;Recipient 5:2-ARG1=Theme;Theme 
nw/wsj/02/wsj_0280.parse 23 10 gold say-v 37.7-1 IN say.01 null ----- 9:1-ARG0=Agent 10:0-rel 11:1-ARG1=Topic 
nw/wsj/02/wsj_0280.parse 23 13 gold prefer-v 31.2 Preference prefer.01 1 ----- 5:1*8:1*14:1-ARG1 12:1-ARG0 13:0-rel 5:1*8:1-LINK-SLC 
nw/wsj/02/wsj_0280.parse 23 19 gold have-v 100 IN have.03 null ----- 0:2-ARGM-PRP 16:1-ARG0=Pivot 19:0-rel 20:2-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 24 1 gold compare-v 22.2-2 Evaluative_comparison compare.01 null ----- 0:1-ARG1=Patient 1:0-rel 2:1-ARG2=Co-Patient 
nw/wsj/02/wsj_0280.parse 26 1 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARG0=Agent;Seller 1:0-rel 2:3-ARG1=Theme;Goods 11:1-ARGM-LOC 
nw/wsj/02/wsj_0280.parse 26 18 gold say-v 37.7-1 IN say.01 null ----- 17:1-ARG0=Agent 18:0-rel 19:1-ARG1=Topic 
nw/wsj/02/wsj_0280.parse 27 2 gold test-v 35.4 IN test.01 1 ----- 0:1-ARG0=Agent 1:1-ARGM-ADV 2:0-rel 3:2-ARG1=Location 9:1-ARGM-LOC 
nw/wsj/02/wsj_0280.parse 28 2 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 0:1-ARG0=Agent;Seller 2:0-rel 3:1-ARG1=Theme;Goods 5:1-ARGM-LOC 
nw/wsj/02/wsj_0280.parse 29 3 gold add-v 22.1-2 Statement add.02 null ----- 1:1-ARG0=Agent 3:0-rel 4:3-ARG1=Patient 
nw/wsj/02/wsj_0280.parse 29 12 gold say-v 37.7-1 IN say.01 null ----- 0:1*14:1-ARG1=Topic 12:0-rel 15:2-ARG0=Agent 
nw/wsj/02/wsj_0280.parse 30 8 gold say-v 37.7-1 IN say.01 null ----- 7:1-ARG0=Agent 8:0-rel 0:2-ARG1-DSP=Topic 
nw/wsj/02/wsj_0280.parse 30 16 gold develop-v 26.1 IN develop.02 null ----- 0:1-ARGM-CAU 12:1-ARG0=Agent 16:0-rel 17:2-ARG1=Product 
nw/wsj/02/wsj_0280.parse 30 20 gold tailor-v 29.8-1 NF tailor.01 1 ----- 17:1*21:1-ARG1=Beneficiary 20:0-rel 22:1-ARG2 17:1*21:1-LINK-PSV 
nw/wsj/02/wsj_0280.parse 32 6 gold fight-v 36.3-2 Hostile_encounter fight.01 null ----- 0:1-ARGM-TMP 3:2-ARG0=Agent;Side_1 6:0-rel 7:1-ARG1=Co-Agent;Side_2 
nw/wsj/02/wsj_0280.parse 35 9 gold lose-v 13.2 NF lose.02 null ----- 0:1-ARGM-DIS 4:2-ARG0=Agent 8:1-ARGM-MNR 9:0-rel 10:1-ARG1=Theme 11:1-ARGM-LOC 14:1-ARG2=Recipient 
nw/wsj/02/wsj_0280.parse 36 4 gold say-v 37.7-1 IN say.01 null ----- 3:1-ARG0=Agent 4:0-rel 0:1-ARG1-DSP=Topic 
nw/wsj/02/wsj_0280.parse 37 4 gold begin-v 55.1-1 Activity_start begin.01 null ----- 0:0-ARGM-DIS 1:1-ARG0=Agent;Agent 4:0-rel 5:2-ARG1=Theme;Activity 
nw/wsj/02/wsj_0280.parse 37 7 gold use-v 105 IN use.01 null ----- 1:1*5:1-ARG0 7:0-rel 8:2-ARG1 
nw/wsj/02/wsj_0280.parse 38 10 gold top-v 90-1 NF top.02 1 ----- 0:1-ARGM-TMP 3:2-ARG0=Theme 10:0-rel 11:2-ARG1=Co-Theme 
nw/wsj/02/wsj_0280.parse 38 16 gold say-v 37.7-1 IN say.01 null ----- 0:2*18:1-ARG1=Topic 16:0-rel 19:2-ARG0=Agent 
nw/wsj/02/wsj_0280.parse 39 3 gold grow-v 45.6-1 IN grow.01 null ----- 0:0-ARGM-DIS 1:1-ARG1=Patient 3:0-rel 4:1-ARG2=Extent 6:1-ARGM-ADV 
nw/wsj/02/wsj_0280.parse 40 32 gold launch-v 55.5-1 IN launch.01 null ----- 0:1*9:2-ARG0 0:2-ARGM-PRP 32:0-rel 33:1-ARG1 35:1-ARGM-MNR 
nw/wsj/02/wsj_0280.parse 41 2 gold hire-v 13.5.1 Hiring hire.01 null ----- 0:1-ARG0=Agent;Employer 2:0-rel 3:1-ARG1=Theme;Employee 4:2-ARG2 
nw/wsj/02/wsj_0280.parse 41 19 gold begin-v 55.1-1 Activity_start begin.01 null ----- 16:1*20:1-ARG0=Agent;Agent 18:1-ARGM-ADV 19:0-rel 20:2-ARG1=Theme;Activity 
nw/wsj/02/wsj_0280.parse 41 22 gold borrow-v 13.5.2 NF borrow.01 2 ----- 16:1*20:1-ARG0=Agent 22:0-rel 23:2-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 41 32 gold offer-v 13.3 NF offer.01 null ----- 31:1-ARG0=Agent 32:0-rel 33:1-ARG1=Theme 35:2-ARGM-TMP 
nw/wsj/02/wsj_0280.parse 42 3 gold start-v 55.1-1 Activity_start start.01 null ----- 1:1*4:1-ARG0=Agent;Agent 3:0-rel 4:2-ARG1=Theme;Activity 
nw/wsj/02/wsj_0280.parse 42 5 gold try-v 61 Attempt try.01 null ----- 1:1*4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 9:2-ARGM-PRP 
nw/wsj/02/wsj_0280.parse 42 11 gold keep-v 55.6 Cause_to_continue keep.02 null ----- 1:1*4:1*9:1-ARG0=Agent 11:0-rel 12:2-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 42 19 gold say-v 37.7-1 IN say.01 null ----- 1:2*20:1-ARG1=Topic 19:0-rel 21:2-ARG0=Agent 
nw/wsj/02/wsj_0280.parse 44 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/02/wsj_0280.parse 45 4 gold bring-v 11.3-1 Bringing bring.01 null ----- 3:1-ARG0=Instrument 4:0-rel 5:1-ARG3 6:1-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 46 9 gold include-v 65 NF include.01 null ----- 0:1*2:1*5:1-ARG2=Location 6:2-ARGM-TMP 9:0-rel 10:2-ARG1=Theme 0:1*2:1-LINK-SLC 
nw/wsj/02/wsj_0280.parse 47 2 gold focus-v 87.1 Place_weight_on focus.01 null ----- 0:1-ARG0=Experiencer 2:0-rel 3:1-ARG2=Theme 
nw/wsj/02/wsj_0280.parse 47 10 gold say-v 37.7-1 IN say.01 null ----- 10:0-rel 11:1-ARG1=Topic 13:2-ARG0=Agent 
nw/wsj/02/wsj_0280.parse 48 2 gold train-v 13.5.3 Education_teaching train.01 1 ----- 0:1-ARG0=Agent 1:1-ARGM-TMP 2:0-rel 3:1-ARG2=Theme 5:1-ARG1=Attribute 
nw/wsj/02/wsj_0280.parse 48 6 gold advise-v 37.9-1 IN advise.01 1 ----- 3:1-ARG0=Agent 6:0-rel 7:1-ARG1=Recipient 8:1-ARG2=Topic 
nw/wsj/02/wsj_0280.parse 49 4 gold have-v 100 IN have.03 null ----- 0:0-ARGM-DIS 1:1-ARG0=Pivot 3:1-ARGM-TMP 4:0-rel 5:1-ARG1=Theme 
nw/wsj/02/wsj_0280.parse 49 11 gold motivate-v 59 Subjective_influence motivate.01 1 ----- 10:1-ARG0=Agent 11:0-rel 12:1-ARG1=Patient 14:1-ARGM-MNR 
nw/wsj/02/wsj_0280.parse 50 8 gold expect-v 62 IN expect.01 null ----- 0:1,9:2-ARG1=Theme 7:0-ARGM-NEG 8:0-rel 12:1-ARGM-TMP 
nw/wsj/02/wsj_0280.parse 52 11 gold show-v 78-1-1 IN show.01 null ----- 0:1-ARGM-ADV 7:1-ARG0 10:0-ARGM-NEG 11:0-rel 12:2-ARG1 
nw/wsj/02/wsj_0280.parse 53 5 gold introduce-v 22.2-3-1 NF introduce.02 null ----- 0:1*25:1-ARG0=Agent 4:1-ARGM-TMP 5:0-rel 6:2-ARG1=Patient 18:1-ARGM-LOC 
nw/wsj/02/wsj_0280.parse 53 24 gold plan-v 62 Purpose plan.01 null ----- 0:1*25:1-ARG0=Experiencer;Agent 23:0-ARGM-NEG 24:0-rel 25:2-ARG1 
nw/wsj/02/wsj_0280.parse 53 27 gold bring-v 11.3-1 Bringing bring.01 null ----- 0:1*25:1-ARG0=Instrument 27:0-rel 28:1-ARG1=Theme 29:1-ARG2=Destination 
nw/wsj/02/wsj_0280.parse 54 1 gold believe-v 29.5-1 Awareness believe.01 null ----- 0:1-ARG0 1:0-rel 2:1-ARG1 
nw/wsj/02/wsj_0280.parse 55 2 gold test-v 35.4 IN test.01 1 ----- 0:1-ARG0=Agent 2:0-rel 3:2-ARG1=Location 
nw/wsj/02/wsj_0280.parse 55 5 gold brew-v 26.3-1 NF brew.01 1 ----- 3:1*6:1-ARG1 5:0-rel 7:1-ARG2 3:1*6:1-LINK-PSV 
nw/wsj/02/wsj_0280.parse 56 1 gold call-v 29.3 Being_named call.01 null ----- 0:1*11:1-ARG1=Theme;Entity 1:0-rel 3:2-ARG2=Result;Name 
nw/wsj/02/wsj_0280.parse 56 13 gold cost-v 54.2 Expensiveness cost.01 null ----- 0:1*2:1*3:1*11:1-ARG1=Theme;Goods 0:2-ARGM-PRD 13:0-rel 14:3-ARG2=Value;Asset 
