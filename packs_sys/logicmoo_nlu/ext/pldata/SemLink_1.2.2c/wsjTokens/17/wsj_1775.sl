nw/wsj/17/wsj_1775.parse 0 3 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/17/wsj_1775.parse 0 6 gold license-v 101 NF license.01 1 ----- 5:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 13:1-ARG2 
nw/wsj/17/wsj_1775.parse 1 1 gold include-v 65 NF include.01 null ----- 0:1-ARG2=Location 1:0-rel 2:1-ARG1=Theme 
nw/wsj/17/wsj_1775.parse 1 10 gold disclose-v 37.7 Reveal_secret disclose.01 null ----- 7:1*11:1-ARG1=Topic;Information 9:0-ARGM-NEG 10:0-rel 
nw/wsj/17/wsj_1775.parse 2 7 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 7:0-rel 8:1-ARG1=Topic 
nw/wsj/17/wsj_1775.parse 2 16 gold sell-v 13.1-1 Commerce_sell sell.01 null ----- 9:2-ARG0=Agent;Seller 15:0-ARGM-MOD 16:0-rel 17:2-ARG1=Theme;Goods 21:1-ARG2=Recipient 
nw/wsj/17/wsj_1775.parse 3 3 gold receive-v 13.5.2 Receiving receive.01 null ----- 0:1-ARG0=Agent;Donor 2:0-ARGM-MOD 3:0-rel 4:2-ARG1=Theme;Theme 
nw/wsj/17/wsj_1775.parse 3 5 gold link-v 22.1-2-1 Make_cognitive_connection link.01 null ----- 4:1*6:1-ARG1=Patient;Concept_1 5:0-rel 7:1-ARG1=Patient;Concept_1 4:1*6:1-LINK-PSV 
nw/wsj/17/wsj_1775.parse 3 12 gold serve-v 13.4.1-1 NF serve.02 null ----- 11:1*13:1-ARG2=Recipient 12:0-rel 14:1-ARG0=Agent 11:1*13:1-LINK-PSV 
nw/wsj/17/wsj_1775.parse 3 19 gold say-v 37.7-1 IN say.01 null ----- 0:2*21:1-ARG1=Topic 18:1-ARG0=Agent 19:0-rel 
nw/wsj/17/wsj_1775.parse 4 2 gold help-v 72-1 Assistance help.01 null ----- 0:1-ARG0=Agent;Helper 2:0-rel 3:1-ARG2=Beneficiary;Benefited_party 4:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/17/wsj_1775.parse 4 4 gold solve-v 84 NF solve.01 null ----- 3:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/17/wsj_1775.parse 4 13 gold react-v 31.3-9 NF react.01 1 ----- 3:1*11:1-ARG0=Experiencer 10:1*20:1-ARGM-MNR 13:0-rel 14:1-ARG1=Stimulus 3:1*11:1-LINK-PRO 
nw/wsj/17/wsj_1775.parse 4 16 gold offer-v 13.3 NF offer.01 null ----- 15:1*17:1-ARG1=Theme 16:0-rel 18:1-ARG0=Agent 15:1*17:1-LINK-PSV 
nw/wsj/17/wsj_1775.parse 4 34 gold offer-v 13.3 NF offer.01 null ----- 26:2*31:1*35:1-ARG1=Theme 32:1-ARG0=Agent 34:0-rel 36:1-ARG2 26:2*31:1-LINK-SLC 
nw/wsj/17/wsj_1775.parse 4 38 gold give-v 13.1-1 Giving give.01 null ----- 38:0-rel 39:0-ARG1=Theme;Theme 
