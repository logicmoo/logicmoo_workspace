nw/wsj/11/wsj_1125.parse 0 14 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 13:1-ARG0=Agent;Buyer 14:0-rel 
nw/wsj/11/wsj_1125.parse 0 18 gold build-v 26.1-1 Building build.01 1 ----- 13:1-ARG0=Agent;Agent 18:0-rel 
nw/wsj/11/wsj_1125.parse 1 4 gold expect-v 62 IN expect.01 1 ----- 2:1,5:2-ARG1=Theme 4:0-rel 
nw/wsj/11/wsj_1125.parse 1 36 gold begin-v 55.1-1 Process_start begin.01 1 ----- 30:1-ARG1=Theme;Event 36:0-rel 37:1-ARGM-LOC 38:1-ARGM-TMP 
nw/wsj/11/wsj_1125.parse 1 40 gold sponsor-v 29.8-1 NF sponsor.01 1 ----- 30:1*41:1-ARG1=Beneficiary 40:0-rel 42:1-ARG0=Agent 30:1*41:1-LINK-PSV 
nw/wsj/11/wsj_1125.parse 3 7 gold find-v 13.5.1 IN find.01 1 ----- 0:1-ARGM-ADV 5:1-ARG0=Agent 7:0-rel 8:2-ARG1=Theme 
nw/wsj/11/wsj_1125.parse 6 3 gold expect-v 62 IN expect.01 1 ----- 0:1,4:2-ARG1=Theme 3:0-rel 
nw/wsj/11/wsj_1125.parse 6 7 gold buzz-v 43.2 Motion_noise buzz.01 1 ----- 0:1*4:1-ARG0=Agent 7:0-rel 8:1-ARG1=Theme;Theme 
nw/wsj/11/wsj_1125.parse 6 37 gold dump-v 9.3-1-1 NF dump.01 2 ----- 35:1*38:1-ARG1=Theme 37:0-rel 39:1-ARGM-DIR 42:1-ARG0=Agent 35:1*38:1-LINK-PSV 
nw/wsj/11/wsj_1125.parse 7 3 gold eye-v 30.2 NF eye.01 null ----- 0:1-ARG0=Experiencer 2:1-ARGM-DIS 3:0-rel 4:2-ARG1=Stimulus 
nw/wsj/11/wsj_1125.parse 8 1 gold plan-v 62 Purpose plan.01 1 ----- 0:1-ARG0=Experiencer;Agent 1:0-rel 2:2-ARG1 
nw/wsj/11/wsj_1125.parse 8 4 gold pursue-v 51.6 Cotheme pursue.01 1 ----- 0:1*2:1-ARG0=Agent;Theme 4:0-rel 5:3-ARG1=Theme;Cotheme 
nw/wsj/11/wsj_1125.parse 9 4 gold shift-v 11.1 NF shift.01 2 ----- 0:0-ARGM-DIS 1:1-ARG1=Theme 3:0-ARGM-MOD 4:0-rel 5:1-ARG3=Initial_Location 7:1-ARG2=Destination 10:1-ARGM-LOC 
nw/wsj/11/wsj_1125.parse 10 22 gold say-v 37.7-1 IN say.01 1 ----- 1:2*23:1-ARG1=Topic 22:0-rel 24:2-ARG0=Agent 
nw/wsj/11/wsj_1125.parse 11 3 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 3:0-rel 4:1-ARG1=Topic 
nw/wsj/11/wsj_1125.parse 14 12 gold rely-v 70 Reliance rely.01 1 ----- 0:1*5:1-ARG0=Agent;Protagonist 12:0-rel 13:1-ARG1=Theme;Means/Instrument/Intermediary/Benefit/Purpose 18:2-ARGM-PNC 
nw/wsj/11/wsj_1125.parse 14 20 gold stay-v 47.1-1 IN stay.01 1 ----- 0:1*5:1*18:1-ARG1=Theme 20:0-rel 21:1-ARG3 22:1-ARGM-LOC 
nw/wsj/11/wsj_1125.parse 15 9 gold act-v 29.6-1 NF act.01 1 ----- 1:1*7:1-ARG0=Agent 8:1-ARGM-TMP 9:0-rel 10:1-ARG1=Attribute 
nw/wsj/11/wsj_1125.parse 15 15 gold say-v 37.7-1 IN say.01 1 ----- 1:2*16:1-ARG1=Topic 15:0-rel 17:2-ARG0=Agent 
nw/wsj/11/wsj_1125.parse 16 3 gold grow-v 48.1.1 NF grow.02 1 ----- 1:1-ARG1=Theme 3:0-rel 4:2-ARG2 
nw/wsj/11/wsj_1125.parse 17 5 gold use-v 105 IN use.01 1 ----- 0:2-ARG0 4:1-ARGM-DIS 5:0-rel 6:2-ARG1 11:2-ARG2 
nw/wsj/11/wsj_1125.parse 17 13 gold amass-v 47.5.2 Gathering_up amass.01 1 ----- 0:2*11:1-ARG0=Agent;Agent 13:0-rel 14:1-ARG1=Theme;Individuals 
nw/wsj/11/wsj_1125.parse 18 9 gold fight-v 36.3-2 Quarreling fight.01 1 ----- 3:1*8:1-ARG0=Agent;Arguer1 9:0-rel 10:2-ARG1=Co-Agent;Arguer2 
nw/wsj/11/wsj_1125.parse 18 17 gold deal-v 83 Resolve_problem deal.01 1 ----- 3:1*8:1-ARG0=Agent 17:0-rel 18:1-ARG1=Theme 
nw/wsj/11/wsj_1125.parse 18 24 gold fight-v 36.3-2 Quarreling fight.01 1 ----- 3:1*8:1-ARG0=Agent;Arguer1 24:0-rel 25:1-ARG1=Co-Agent;Arguer2 
nw/wsj/11/wsj_1125.parse 18 30 gold try-v 61 Attempt try.01 1 ----- 2:0-ARGM-MOD 3:1-ARG0=Agent 29:0-ARGM-TMP 30:0-rel 31:2-ARG1=Theme 45:2-ARGM-TMP 
nw/wsj/11/wsj_1125.parse 18 33 gold convince-v 31.1 IN convince.01 1 ----- 3:1*31:1-ARG0=Stimulus 33:0-rel 34:1-ARG1=Experiencer 36:2-ARG2 
nw/wsj/11/wsj_1125.parse 18 38 gold lend-v 13.1 NF lend.01 2 ----- 34:1*36:1-ARG0=Agent 38:0-rel 39:1-ARG2=Recipient 40:1-ARG1=Theme 41:1-ARGM-LOC 
nw/wsj/11/wsj_1125.parse 18 48 gold get-v 13.5.1-1 IN get.01 1 ----- 46:1-ARG0=Agent 47:0-ARGM-MOD 48:0-rel 49:2-ARG1=Theme 
nw/wsj/11/wsj_1125.parse 18 53 gold buy-v 13.5.1 Commerce_buy buy.01 1 ----- 46:1-ARG0=Agent;Buyer 47:0-ARGM-MOD 53:0-rel 54:1-ARG1=Theme;Goods 
nw/wsj/11/wsj_1125.parse 18 57 gold sell-v 13.1-1 Commerce_sell sell.01 5 ----- 46:1-ARG0=Agent;Seller 47:0-ARGM-MOD 57:0,58:1-rel 59:1-ARG1=Theme;Goods 60:1-ARGM-DIR 
nw/wsj/11/wsj_1125.parse 18 63 gold play-v 26.7-1-1 Competition play.01 12.13 ----- 46:1-ARG0=Agent 47:0-ARGM-MOD 63:0-rel 64:1-ARG1=Theme 
nw/wsj/11/wsj_1125.parse 18 69 gold say-v 37.7-1 IN say.01 1 ----- 1:2-ARG1=Topic 69:0-rel 71:2-ARG0=Agent 
nw/wsj/11/wsj_1125.parse 19 2 gold say-v 37.7-1 IN say.01 1 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/11/wsj_1125.parse 19 13 gold find-v 13.5.1 IN find.01 1 ----- 4:2-ARGM-TMP 12:1-ARG0=Agent 13:0-rel 14:1-ARG1=Theme 15:1-ARGM-LOC 
nw/wsj/11/wsj_1125.parse 20 14 gold expect-v 62 IN expect.01 1 ----- 13:1*15:1-ARG1=Theme 14:0-rel 
nw/wsj/11/wsj_1125.parse 20 19 gold come-v 48.1.1 NF come.03 5 ----- 0:2-ARG1=Theme 19:0-rel 20:1-ARG2 
nw/wsj/11/wsj_1125.parse 20 29 gold say-v 37.7-1 IN say.01 1 ----- 0:3*31:1-ARG1=Topic 29:0-rel 32:2-ARG0=Agent 
nw/wsj/11/wsj_1125.parse 21 3 gold succeed-v 74-1 IN succeed.01 1 ----- 1:1*14:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme 
nw/wsj/11/wsj_1125.parse 21 10 gold say-v 37.7-1 IN say.01 1 ----- 9:1-ARG0=Agent 10:0-rel 0:1-ARG1-DSP=Topic 
nw/wsj/11/wsj_1125.parse 22 1 gold find-v 13.5.1 IN find.01 1 ----- 0:1-ARG0=Agent 1:0-rel 2:3-ARG1=Theme 
nw/wsj/11/wsj_1125.parse 22 10 gold invest-v 13.4.2 NF invest.01 1 ----- 8:1-ARG0=Agent 10:0-rel 
nw/wsj/11/wsj_1125.parse 23 27 gold take-v 11.3 Bringing take.01 4 ----- 26:1-ARG0 27:0-rel 28:1-ARG1 32:1-ARG2 
