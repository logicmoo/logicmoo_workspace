nw/wsj/17/wsj_1726.parse 0 5 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 5:0-rel 6:1-ARG1=Topic 
nw/wsj/17/wsj_1726.parse 0 13 gold agree-v 36.1-1 IN agree.01 null ----- 7:2*14:1-ARG0=Agent 13:0-rel 14:2-ARG1=Theme 
nw/wsj/17/wsj_1726.parse 0 16 gold offer-v 13.3 NF offer.01 null ----- 7:2*14:1-ARG0=Agent 16:0-rel 17:1-ARG1=Theme 19:1-ARGM-MNR 
nw/wsj/17/wsj_1726.parse 1 12 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 12:0-rel 13:1-ARG1=Topic 
nw/wsj/17/wsj_1726.parse 1 16 gold include-v 65 NF include.01 null ----- 14:1-ARG2=Location 16:0-rel 17:2-ARG1=Theme 
nw/wsj/17/wsj_1726.parse 3 3 gold agree-v 36.1-1 IN agree.01 null ----- 0:1*4:1-ARG0=Agent 3:0-rel 4:2-ARG1=Theme 
nw/wsj/17/wsj_1726.parse 3 6 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1*4:1-ARG0=Agent;Buyer 6:0-rel 7:3-ARG1=Theme;Goods 16:1-ARGM-LOC 
nw/wsj/17/wsj_1726.parse 3 27 gold offer-v 13.3 NF offer.01 null ----- 17:2*25:1*26:1-ARG0=Agent 27:0-rel 28:2-ARG1=Theme 32:1-ARG3=Goal 40:2-ARGM-PRP 17:2*25:1-LINK-SLC 
nw/wsj/17/wsj_1726.parse 3 42 gold help-v 72-1 Assistance help.01 null ----- 17:2*25:1*26:1*40:1-ARG0=Agent;Helper 42:0-rel 43:1-ARG2=Beneficiary;Benefited_party 44:2-ARG1=Theme;Goal/Focal_entity 17:2*25:1-LINK-SLC 
nw/wsj/17/wsj_1726.parse 3 44 gold maintain-v 29.5-2 Statement maintain.01 null ----- 43:1-ARG0=Agent;Speaker 44:0-rel 45:1-ARG1=Theme;Addressee 
nw/wsj/17/wsj_1726.parse 3 48 gold obtain-v 13.5.2-1 Getting obtain.01 null ----- 43:1-ARG0=Agent;Recipient 48:0-rel 49:1-ARG1=Theme;Theme 
nw/wsj/17/wsj_1726.parse 4 10 gold build-v 26.1-1 Building build.01 null ----- 9:1*11:1-ARG1=Product;Created_entity 10:0-rel 12:1-ARGM-LOC 9:1*11:1-LINK-PSV 
nw/wsj/17/wsj_1726.parse 5 2 gold make-v 26.1-1 IN make.01 null ----- 0:1-ARG0=Agent 2:0-rel 3:1-ARG1=Product 6:1-ARGM-LOC 
nw/wsj/17/wsj_1726.parse 6 1 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 0:1-ARG0=Agent;Buyer 1:0-rel 2:1-ARG1=Theme;Goods 3:1-ARG2=Source 
nw/wsj/17/wsj_1726.parse 6 6 gold package-v 22.3-2-1 Placing package.01 1 ----- 0:1-ARG0=Agent 6:0-rel 7:1-ARG1=Patient 8:1-ARG2=Co-Patient 
nw/wsj/17/wsj_1726.parse 6 15 gold hold-v 15.1-1 Containing hold.01 null ----- 0:1-ARG0=Agent 15:0-rel 16:1-ARG1=Theme 18:1-ARGM-LOC 
