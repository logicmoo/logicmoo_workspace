nw/wsj/07/wsj_0766.parse 0 2 gold pull-v 11.4 Cause_motion pull.01 null ----- 0:1-ARG0=Agent;Agent/Cause 2:0-rel 3:1-ARG1=Theme;Theme 6:1-ARG2=Initial_Location;Source 
nw/wsj/07/wsj_0766.parse 1 5 gold run-v 47.7 IN run.04 null ----- 0:2;11:1-ARG1=Theme 4:1-ARGM-TMP 5:0-rel 6:1-ARGM-LOC 
nw/wsj/07/wsj_0766.parse 4 5 gold lean-v 47.6 Posture lean.01 1 ----- 0:2-ARG1=Theme;Agent 5:0-rel 6:1-ARGM-MNR 7:1-ARG2=Location;Location 
nw/wsj/07/wsj_0766.parse 5 11 gold dress-v 41.1.1 Dressing dress.01 1 ----- 0:1*12:1-ARG1=Patient 11:0-rel 13:1-ARG2 18:1-ARGM-ADV 0:1*12:1-LINK-PSV 
nw/wsj/07/wsj_0766.parse 6 18 gold employ-v 13.5.3 Employing employ.01 1 ----- 13:2*19:1-ARG1=Theme 18:0-rel 20:1-ARG0=Agent 13:2*19:1-LINK-PSV 
nw/wsj/07/wsj_0766.parse 6 31 gold help-v 72-1 Assistance help.01 null ----- 13:3*29:1-ARG0=Agent;Helper 31:0-rel 32:1-ARG2=Beneficiary;Benefited_party 33:2-ARG1=Theme;Goal/Focal_entity 
nw/wsj/07/wsj_0766.parse 6 33 gold sift-v 23.3 Scouring sift.01 2 ----- 32:1-ARG0=Agent 33:0-rel 34:1-ARG1=Patient 
nw/wsj/07/wsj_0766.parse 6 38 gold restore-v 13.2 NF restore.01 1 ----- 32:1-ARG0=Agent 38:0-rel 39:1-ARG1=Theme 41:1-ARG2=Recipient 
nw/wsj/07/wsj_0766.parse 7 1 gold equip-v 13.4.2-1 Supply equip.01 1 ----- 0:1*18:1-ARG1=Recipient 1:0-rel 3:1-ARG2=Theme 
nw/wsj/07/wsj_0766.parse 7 26 gold find-v 13.5.1 IN find.01 null ----- 23:1-ARG0=Agent 25:0-ARGM-MOD 26:0-rel 27:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 7 31 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 23:1-ARG0=Agent;Buyer 25:0-ARGM-MOD 31:0-rel 32:1-ARG1=Theme;Goods 
nw/wsj/07/wsj_0766.parse 7 34 gold replace-v 13.6 IN replace.01 null ----- 23:1-ARG0=Agent 25:0-ARGM-MOD 34:0-rel 35:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 7 35 gold lose-v 13.2 NF lose.02 null ----- 35:0-rel 36:0-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 8 5 gold use-v 105 IN use.01 null ----- 0:2*6:1-ARG1 4:0-ARGM-MOD 5:0-rel 7:2-ARG2 
nw/wsj/07/wsj_0766.parse 8 9 gold demolish-v 44 Destroying demolish.01 1 ----- 7:1-ARG0=Agent;Cause/Destroyer 9:0-rel 10:1-ARG1=Patient;Undergoer 15:1-ARGM-PRP 
nw/wsj/07/wsj_0766.parse 8 13 gold clear-v 10.3-1 Emptying clear.01 1 ----- 7:1-ARG0=Agent;Agent/Cause 13:0-rel 14:1-ARG1=Location;Source 15:1-ARGM-PRP 
nw/wsj/07/wsj_0766.parse 9 7 gold write-v 25.2 Text_creation write.01 null ----- 0:1*4:1*5:1-ARG0=Agent;Author 7:0-rel 8:2-ARG1=Theme;Text 16:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 10 3 gold flinch-v 40.5 NF flinch.01 null ----- 0:1-ARG0=Experiencer 2:0-ARGM-NEG 3:0-rel 4:1-ARGM-ADV 
nw/wsj/07/wsj_0766.parse 10 6 gold write-v 25.2 Text_creation write.01 null ----- 5:1-ARG0=Agent;Author 6:0-rel 7:1-ARG1=Theme;Text 
nw/wsj/07/wsj_0766.parse 11 7 gold get-v 13.5.1-1 IN get.01 null ----- 6:1-ARG0=Agent 7:0-rel 8:1-ARG4=Beneficiary 11:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 11 19 gold say-v 37.7-1 IN say.01 null ----- 0:1*20:1-ARG1=Topic 19:0-rel 21:2-ARG0=Agent 
nw/wsj/07/wsj_0766.parse 11 29 gold fly-v 51.4.2 Ride_vehicle fly.01 null ----- 24:1*27:1*28:1-ARG1=Theme;Theme 29:0-rel 30:1-ARGM-DIR 31:1-ARGM-DIR 24:1*27:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 12 7 gold inspect-v 35.4 Inspecting inspect.01 1 ----- 0:1*3:1*8:1-ARG1=Location 4:1-ARG0=Agent 7:0-rel 0:1*3:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 13 2 gold ask-v 58.2 Request ask.02 null ----- 0:0-ARGM-DIS 1:1-ARG0 2:0-rel 3:2*11:1-ARG2 11:2-ARG1 
nw/wsj/07/wsj_0766.parse 13 13 gold give-v 13.1-1 Giving give.01 null ----- 3:2*11:1-ARG0=Agent;Donor 13:0-rel 14:1-ARG2=Recipient;Recipient 15:2-ARG1=Theme;Theme 
nw/wsj/07/wsj_0766.parse 14 4 gold spend-v 66-1 NF spend.02 null ----- 0:1-ARGM-TMP 3:1*8:1-ARG0=Agent 4:0-rel 5:2-ARG1=Asset 8:2-ARG2=Goal 23:2-ARGM-ADV 
nw/wsj/07/wsj_0766.parse 14 9 gold measure-v 54.1-1 Dimension measure.01 1 ----- 3:1*8:1-ARG0=Agent 9:0-rel 12:2-ARG1=Theme;Object 
nw/wsj/07/wsj_0766.parse 14 24 gold gather-v 13.5.1 Gathering_up gather.01 null ----- 3:1*23:1-ARG0=Agent 24:0-rel 25:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 14 29 gold estimate-v 54.4 Estimating estimate.01 1 ----- 3:1*27:1-ARG0=Agent 29:0-rel 30:2-ARG1=Theme 3:1*27:1-LINK-PRO 
nw/wsj/07/wsj_0766.parse 14 34 gold cost-v 54.2 Expensiveness cost.01 null ----- 30:1*35:1-ARG2=Value;Asset 32:1*36:2-ARG1=Theme;Goods 33:0-ARGM-MOD 34:0-rel 
nw/wsj/07/wsj_0766.parse 16 2 gold work-v 73-3 IN work.01 null ----- 1:1-ARG0=Agent 2:0-rel 3:1-ARGM-LOC 
nw/wsj/07/wsj_0766.parse 16 13 gold collect-v 13.5.2 Gathering_up collect.01 null ----- 5:1*11:1-ARG0=Agent 13:0-rel 14:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 17 14 gold start-v 55.1-1 Activity_start start.01 null ----- 0:2-ARG0=Agent;Agent 14:0-rel 15:2-ARG1=Theme;Activity 
nw/wsj/07/wsj_0766.parse 17 17 gold pack-v 9.7-1-1 Placing pack.01 1 ----- 0:2*15:1-ARG0=Agent;Agent/Cause 17:0-rel 18:2-ARG2=Theme;Theme 
nw/wsj/07/wsj_0766.parse 17 22 gold salvage-v 10.5-1 NF salvage.01 2 ----- 18:1*19:1*23:1-ARG1=Theme 20:0-ARGM-MOD 22:0-rel 24:1-ARG2=Source 
nw/wsj/07/wsj_0766.parse 18 1 gold grab-v 13.5.2 IN grab.01 1 ----- 0:1*13:1-ARG0=Agent 1:0-rel 2:2-ARG1=Theme 13:2-ARGM-ADV 
nw/wsj/07/wsj_0766.parse 18 14 gold work-v 73-3 IN work.01 null ----- 0:1*13:1-ARG0=Agent 14:0-rel 15:1-ARGM-MNR 16:1-ARGM-MNR 
nw/wsj/07/wsj_0766.parse 18 23 gold jolt-v 31.1 NF jolt.01 1 ----- 20:1-ARG0=Stimulus 22:0-ARGM-MOD 23:0-rel 24:1-ARG1=Experiencer 26:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 20 7 gold insist-v 37.7 Statement insist.01 null ----- 0:2-ARGM-TMP 5:1-ARG0=Agent;Speaker 7:0-rel 8:1-ARG1=Topic;Message 
nw/wsj/07/wsj_0766.parse 20 10 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 5:1*9:1-ARG0=Agent;Buyer 10:0-rel 11:2-ARG1=Theme;Goods 5:1*9:1-LINK-PRO 
nw/wsj/07/wsj_0766.parse 20 21 gold convert-v 26.6.1-1 IN convert.01 null ----- 14:1*17:1*18:1*22:1-ARG1=Patient 21:0-rel 23:1-ARG2=Result 14:1*17:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 21 10 gold carry-v 54.3 Bringing carry.01 null ----- 0:3-ARG0=Location 10:0-rel 11:1-ARG1=Value 
nw/wsj/07/wsj_0766.parse 22 11 gold suffer-v 31.3-4 Catastrophe suffer.01 null ----- 0:3-ARG0=Experiencer 11:0-rel 12:2-ARG1=Stimulus 
nw/wsj/07/wsj_0766.parse 23 5 gold work-v 73-3 IN work.01 null ----- 0:1*3:1*4:1-ARG0=Agent 5:0-rel 6:1-ARGM-LOC 0:1*3:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 24 13 gold say-v 37.7-1 IN say.01 null ----- 0:1*14:1-ARG1=Topic 12:1-ARG0=Agent 13:0-rel 
nw/wsj/07/wsj_0766.parse 27 10 gold hit-v 18.4 Cause_harm hit.01 null ----- 0:1-ARGM-ADV 6:1-ARG2=Theme 10:0-rel 11:2-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 27 18 gold stretch-v 47.7 Cause_expansion stretch.01 null ----- 11:1*20:1-ARGM-TMP 12:2*19:1-ARG1 16:1-ARGM-TMP 18:0-rel 
nw/wsj/07/wsj_0766.parse 28 4 gold try-v 61 Attempt try.01 null ----- 0:1*5:1-ARG0=Agent 2:1-ARGM-TMP 4:0-rel 5:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 28 7 gold sort-v 29.10 Differentiation sort.01 1 ----- 0:1*5:1-ARG0=Agent 7:0-rel 8:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 28 11 gold cause-v 27 Causation cause.01 1 ----- 9:1*12:1-ARG1=Theme;Effect 11:0-rel 13:1-ARG0=Cause;Cause 19:1-ARGM-TMP 9:1*12:1-LINK-PSV 
nw/wsj/07/wsj_0766.parse 29 4 gold have-v 100 IN have.03 null ----- 0:1*2:1*3:1-ARG0=Pivot 4:0-rel 5:2-ARG1=Theme 0:1*2:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 30 5 gold work-v 73-3 IN work.01 null ----- 0:1*1:1*2:1-ARG0=Agent 5:0-rel 6:1-ARGM-LOC 0:1*1:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 30 10 gold say-v 37.7-1 IN say.01 null ----- 0:2-ARG0=Agent 10:0-rel 11:1-ARG1=Topic 
nw/wsj/07/wsj_0766.parse 31 3 gold take-v 54.2 Taking_time take.10 null ----- 1:1*8:2-ARG0=Theme 2:0-ARGM-MOD 3:0-rel 4:2-ARG1=Value 
nw/wsj/07/wsj_0766.parse 31 10 gold handle-v 15.1-1 NF handle.01 null ----- 8:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 32 3 gold rock-v 47.3 Moving_in_place rock.01 1 ----- 0:1*8:1-ARGM-TMP 1:1-ARG0=Agent 3:0-rel 4:1-ARG1=Theme;Theme 6:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 32 36 gold handle-v 15.1-1 NF handle.01 null ----- 33:1*42:1-ARGM-MNR 34:1-ARG0=Agent 36:0-rel 37:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 33 10 gold charter-v 13.5.1 Renting charter.01 1 ----- 0:1-ARGM-CAU 8:1*13:1-ARG0=Agent 10:0-rel 11:1-ARG1=Theme 13:2-ARG2=Beneficiary 
nw/wsj/07/wsj_0766.parse 33 15 gold fly-v 11.5-1 Bringing fly.01 null ----- 8:1*13:1-ARG0=Agent 15:0-rel 16:1-ARG1=Theme 18:1-ARGM-DIR 19:1-ARGM-GOL 
nw/wsj/07/wsj_0766.parse 34 6 gold assess-v 34.1 Assessing assess.01 1 ----- 5:1-ARG0=Agent;Assessor 6:0-rel 7:2-ARG1=Theme;Phenomenon 
nw/wsj/07/wsj_0766.parse 34 9 gold cause-v 27 Causation cause.01 1 ----- 7:1*10:1-ARG1=Theme;Effect 9:0-rel 11:1-ARG0=Cause;Cause 7:1*10:1-LINK-PSV 
nw/wsj/07/wsj_0766.parse 34 16 gold pull-v 11.4 IN pull.01 null ----- 0:1-ARGM-CAU 15:1-ARG0=Agent 16:0-rel 17:1-ARG2=Initial_Location 18:2-ARG1=Theme 26:1-ARGM-DIR 
nw/wsj/07/wsj_0766.parse 35 6 gold deal-v 83 Resolve_problem deal.01 null ----- 0:1-ARGM-DIS 3:1-ARG0=Agent 5:1-ARGM-TMP 6:0-rel 7:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 36 12 gold work-v 73-3 IN work.01 null ----- 0:2-ARG0=Agent 12:0-rel 13:1-ARGM-LOC 15:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 37 6 gold pack-v 9.7-1-1 Filling pack.01 1 ----- 0:1-ARG0=Agent;Agent 6:0-rel 7:1-ARG1=Destination;Goal 9:1-ARG2=Theme;Theme 
nw/wsj/07/wsj_0766.parse 39 6 gold know-v 29.5-1 IN know.01 1 ----- 0:1-ARGM-TMP 5:1-ARG0=Agent 6:0-rel 7:1-ARG1=Theme 17:2-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 39 21 gold call-v 13.5.1 IN call.02 null ----- 19:1-ARG0=Agent 21:0,24:0-rel 25:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 39 23 gold fax-v 37.4 Contacting fax.01 null ----- 19:1-ARG0=Agent;Communicator 23:0,24:1-rel 25:1-ARG1=Topic;Communication 
nw/wsj/07/wsj_0766.parse 40 29 gold damage-v 44 Damaging damage.01 1 ----- 10:3*30:1-ARG1=Patient;Patient 28:1-ARGM-MNR 29:0-rel 31:2-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 40 33 gold see-v 30.1-1 Perception_experience see.01 null ----- 31:1*39:1-ARGM-TMP 32:1-ARG0=Experiencer;Perceiver_passive 33:0-rel 34:1-ARG1=Stimulus;Phenomenon 35:1-ARGM-LOC 
nw/wsj/07/wsj_0766.parse 41 13 gold say-v 37.7-1 IN say.01 null ----- 0:1*14:1-ARG1=Topic 13:0-rel 15:2-ARG0=Agent 
nw/wsj/07/wsj_0766.parse 43 2 gold count-v 108 Being_in_category count.01 null ----- 0:1-ARG0 1:0-ARGM-MOD 2:0-rel 3:2-ARG1 
nw/wsj/07/wsj_0766.parse 44 4 gold assign-v 13.3 NF assign.01 2 ----- 0:0-ARGM-DIS 1:1-ARG0=Agent 2:0-ARGM-MOD 3:1-ARGM-ADV 4:0-rel 5:1-ARG1=Theme 7:2-ARG2=Goal 24:2-ARGM-PRP 
nw/wsj/07/wsj_0766.parse 45 7 gold think-v 29.9-2 IN think.01 null ----- 0:1*5:1-ARG0 0:2-ARGM-PRP 6:0-ARGM-MOD 7:0-rel 8:1-ARGM-MNR 
nw/wsj/07/wsj_0766.parse 46 1 gold use-v 105 IN use.01 null ----- 0:1*27:1-ARG0 1:0-rel 2:2-ARG1 
nw/wsj/07/wsj_0766.parse 46 44 gold cost-v 54.2 Expensiveness cost.01 null ----- 41:1*45:1-ARG2=Value;Asset 43:0-ARGM-MOD 44:0-rel 46:2-ARG1=Theme;Goods 
nw/wsj/07/wsj_0766.parse 48 5 gold visit-v 36.3-1 IN visit.01 null ----- 0:1*13:1-ARGM-TMP 1:2-ARG0=Agent 5:0-rel 6:1-ARG1=Co-Agent 9:1-ARGM-LOC 11:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 48 16 gold find-v 84 IN find.01 null ----- 0:2-ARGM-TMP 15:1-ARG0 16:0-rel 17:2-ARG1 
nw/wsj/07/wsj_0766.parse 48 18 gold live-v 46 Residence live.01 null ----- 17:1-ARG0=Theme;Resident 18:0-rel 19:1-ARGM-LOC 
nw/wsj/07/wsj_0766.parse 49 22 gold push-v 15.1-1 Cause_motion push.01 null ----- 0:3*23:1-ARG1=Theme 22:0-rel 24:3-ARG2 
nw/wsj/07/wsj_0766.parse 50 6 gold present-v 13.4.1 NF present.01 null ----- 0:1-ARGM-TMP 4:1*16:1-ARG0=Agent 6:0-rel 7:1-ARG2=Recipient 9:1-ARG1=Theme 16:2-ARGM-PRP 
nw/wsj/07/wsj_0766.parse 50 18 gold help-v 72-1 Assistance help.01 null ----- 4:1*16:1-ARG0=Agent;Helper 18:0-rel 19:1-ARG2=Beneficiary;Benefited_party 20:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/07/wsj_0766.parse 50 20 gold build-v 26.1-1 Building build.01 null ----- 19:1-ARG0=Agent;Agent 20:0-rel 21:1-ARG1=Product;Created_entity 24:1-ARGM-LOC 
nw/wsj/07/wsj_0766.parse 51 3 gold work-v 73-3 IN work.01 null ----- 0:1-ARG0=Agent 1:1-ARGM-DIS 3:0-rel 4:1-ARG3=Co-Agent 10:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 51 12 gold help-v 72-1 Assistance help.01 null ----- 0:1*10:1-ARG0=Agent;Helper 12:0-rel 13:1-ARG1=Theme;Goal/Focal_entity 
nw/wsj/07/wsj_0766.parse 51 13 gold find-v 13.5.1 IN find.01 null ----- 0:1*10:1-ARG0=Agent 13:0-rel 14:1-ARG2=Beneficiary 15:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 51 20 gold rent-v 13.5.1 Renting rent.01 2 ----- 14:1*18:1-ARG0=Agent 15:1*17:1*21:1-ARG1=Theme 20:0-rel 22:1-ARGM-TMP 14:1*18:1-LINK-PRO 15:1*17:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 51 27 gold build-v 26.1-1 Building build.01 null ----- 23:1*28:1-ARG1=Product;Created_entity 27:0-rel 
nw/wsj/07/wsj_0766.parse 52 4 gold employ-v 13.5.3 Employing employ.01 1 ----- 2:1*5:1-ARG1=Theme 4:0-rel 7:2-ARG0=Agent 2:1*5:1-LINK-PSV 
nw/wsj/07/wsj_0766.parse 52 11 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 11:0-rel 12:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 55 2 gold mention-v 37.7-1 Statement mention.01 1 ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG1=Topic;Message 
nw/wsj/07/wsj_0766.parse 55 6 gold want-v 32.1-1-1 Desiring want.01 null ----- 4:1*7:1-ARG0=Pivot;Experiencer 5:0-ARGM-MOD 6:0-rel 7:2-ARG1=Theme;Event/Focal_participant 16:1-ARGM-ADV 
nw/wsj/07/wsj_0766.parse 56 1 gold sign-v 13.5.3 NF sign.03 null ----- 0:1*4:1-ARG1=Theme 1:0,2:1-rel 4:2-ARGM-PRD 
nw/wsj/07/wsj_0766.parse 56 5 gold start-v 55.1-1 Activity_start start.01 null ----- 0:1*4:1-ARG0=Agent;Agent 5:0-rel 6:1-ARG1=Theme;Activity 
nw/wsj/07/wsj_0766.parse 57 5 gold move-v 11.2 Motion move.01 null ----- 0:2-ARGM-TMP 4:1-ARG1 5:0-rel 6:1-ARG2 
nw/wsj/07/wsj_0766.parse 58 1 gold spend-v 66-1 NF spend.02 null ----- 0:1*13:1-ARG0=Agent 1:0-rel 2:1-ARG1=Asset 4:1-ARGM-LOC 13:2-ARG2=Goal 
nw/wsj/07/wsj_0766.parse 59 2 gold follow-v 47.8 Cotheme follow.01 null ----- 0:1*3:1-ARG2=Co-Theme;Cotheme 2:0-rel 4:1-ARG1=Theme;Theme 
nw/wsj/07/wsj_0766.parse 59 17 gold immerse-v 9.1 Placing immerse.01 null ----- 8:2*14:1*30:1-ARGM-LOC 15:1*18:1-ARG1=Theme;Theme 17:0-rel 19:1-ARG2=Destination;Goal 8:2*14:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 60 5 gold have-v 100 IN have.03 null ----- 0:2-ARG0=Pivot 5:0-rel 6:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 61 8 gold fall-v 45.6-1 Motion_directional fall.01 null ----- 5:1*7:1-ARG1=Patient 8:0-rel 9:1-ARG2=Extent 
nw/wsj/07/wsj_0766.parse 61 13 gold investigate-v 35.4 Scrutiny investigate.01 1 ----- 12:1-ARG0=Agent;Cognizer 13:0-rel 14:2*18:1-ARG1=Location;Ground 
nw/wsj/07/wsj_0766.parse 61 17 gold remain-v 47.1-1 Remainder remain.01 null ----- 14:1*15:1*16:1*18:1-ARG1=Theme 17:0-rel 
nw/wsj/07/wsj_0766.parse 62 2 gold owe-v 13.3 NF owe.01 1 ----- 1:1-ARG0=Agent 2:0-rel 3:1-ARG2=Goal 
nw/wsj/07/wsj_0766.parse 63 6 gold say-v 37.7-1 IN say.01 null ----- 0:2*7:1-ARG1=Topic 5:1-ARG0=Agent 6:0-rel 
nw/wsj/07/wsj_0766.parse 64 3 gold stand-v 50 Posture stand.01 null ----- 1:1*9:1-ARG1=Agent 3:0-rel 4:1-ARG2=Location 8:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 64 18 gold begin-v 55.1-1 Process_start begin.01 null ----- 0:1-ARGM-TMP 16:1*19:2-ARG1=Theme;Event 18:0-rel 
nw/wsj/07/wsj_0766.parse 64 21 gold creak-v 43.2 Make_noise creak.01 null ----- 16:1*19:1-ARG0=Agent 21:0-rel 
nw/wsj/07/wsj_0766.parse 64 23 gold sway-v 47.3 NF sway.01 1 ----- 16:1*19:1-ARG1=Theme 23:0-rel 
nw/wsj/07/wsj_0766.parse 65 3 gold shake-v 47.3 Moving_in_place shake.01 null ----- 0:2;4:1-ARG1=Theme;Theme 3:0-rel 4:1-ARGM-LOC 
nw/wsj/07/wsj_0766.parse 66 15 gold make-v 29.3 Causation make.02 null ----- 14:1-ARG0=Agent 15:0-rel 16:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 67 6 gold prepare-v 26.3-1 IN prepare.01 null ----- 5:1-ARG0=Agent 6:0-rel 7:3-ARG1 
nw/wsj/07/wsj_0766.parse 67 24 gold demolish-v 44 Destroying demolish.01 null ----- 23:1-ARG0=Agent;Cause/Destroyer 24:0-rel 25:1-ARG1=Patient;Undergoer 
nw/wsj/07/wsj_0766.parse 67 28 gold clear-v 10.3-1 Removing clear.01 1 ----- 23:1-ARG0=Agent;Agent/Cause 28:0-rel 29:1-ARGM-DIR 30:1-ARG1=Location;Source 
nw/wsj/07/wsj_0766.parse 68 5 gold admit-v 29.5-2 IN admit.01 null ----- 0:1-ARGM-DIS 4:1*7:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 68 8 gold venture-v 94 Self_motion venture.01 1 ----- 4:1*7:1-ARG0=Agent 8:0-rel 9:1-ARGM-GOL 14:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 69 10 gold review-v 34.1 NF review.01 1 ----- 0:1-ARGM-TMP 8:1-ARG0=Agent 10:0-rel 11:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 70 5 gold retrieve-v 13.5.2 NF retrieve.01 1 ----- 0:1*3:1-ARG0=Agent 5:0-rel 6:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 70 22 gold venture-v 94 Self_motion venture.01 1 ----- 20:1-ARG0=Agent 22:0-rel 23:1-ARGM-DIR 
nw/wsj/07/wsj_0766.parse 71 2 gold tell-v 37.2-1 Telling tell.01 null ----- 0:1-ARG0=Agent;Speaker 2:0-rel 3:1-ARG2=Recipient;Addressee 4:1-ARG1=Topic;Message 
nw/wsj/07/wsj_0766.parse 71 12 gold salvage-v 10.5-1 NF salvage.01 1 ----- 7:1*13:1-ARG1=Theme 9:0-ARGM-MOD 10:0-ARGM-NEG 12:0-rel 
nw/wsj/07/wsj_0766.parse 72 6 gold know-v 29.5-1 IN know.01 1 ----- 0:1*4:1-ARG0=Agent 6:0-rel 7:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 73 4 gold get-v 13.5.1-1 IN get.01 null ----- 1:1*11:1-ARGM-TMP 2:0-ARGM-MOD 3:1-ARG0=Agent 4:0-rel 5:2-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 74 4 gold complete-v 55.2 Activity_finish complete.01 null ----- 0:1-ARG0=Agent 3:0-ARGM-NEG 4:0-rel 5:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 74 10 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARG0=Agent 10:0-rel 13:2-ARG1=Topic 
nw/wsj/07/wsj_0766.parse 74 15 gold talk-v 37.5 IN talk.01 null ----- 13:1-ARG0=Agent 15:0-rel 16:1-ARG1=Topic 
nw/wsj/07/wsj_0766.parse 76 3 gold dawn-v 48.1.1 NF dawn.01 1 ----- 1:1*7:1-ARG1=Theme 2:1-ARGM-TMP 3:0-rel 4:1-ARG2 
nw/wsj/07/wsj_0766.parse 76 25 gold cost-v 54.2 Expensiveness cost.01 null ----- 8:2-ARG1=Theme;Goods 24:0-ARGM-MOD 25:0-rel 26:2-ARG2=Value;Asset 
nw/wsj/07/wsj_0766.parse 77 3 gold lose-v 13.2 NF lose.02 null ----- 1:1-ARG0=Agent 2:0-ARGM-MOD 3:0-rel 4:1-ARG1=Theme 5:1-ARGM-ADV 
nw/wsj/07/wsj_0766.parse 77 10 gold say-v 37.7-1 IN say.01 null ----- 0:1*11:1-ARG1=Topic 9:1-ARG0=Agent 10:0-rel 
nw/wsj/07/wsj_0766.parse 78 5 gold want-v 32.1-1-1 Desiring want.01 null ----- 1:0-ARGM-DIS 2:1-ARG0=Pivot;Experiencer 4:0-ARGM-NEG 5:0-rel 6:2-ARG1=Theme;Event/Focal_participant 
nw/wsj/07/wsj_0766.parse 78 8 gold buy-v 13.5.1 Commerce_buy buy.01 null ----- 6:1-ARG0=Agent;Buyer 8:0-rel 9:1-ARG1=Theme;Goods 
nw/wsj/07/wsj_0766.parse 78 13 gold say-v 37.7-1 IN say.01 null ----- 0:1*14:1-ARG1=Topic 13:0-rel 15:1*18:1-ARG0=Agent 18:2-ARGM-ADV 
nw/wsj/07/wsj_0766.parse 79 2 gold cost-v 54.2 Expensiveness cost.01 null ----- 0:1-ARG1=Theme;Goods 2:0-rel 3:3,11:1-ARG2=Value;Asset 
nw/wsj/07/wsj_0766.parse 79 22 gold mean-v 29.5-1 NF mean.01 null ----- 20:1-ARG0=Agent 22:0-rel 23:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 80 4 gold believe-v 29.5-1 Awareness believe.01 null ----- 0:1-ARGM-DIS 2:1-ARG0 4:0-rel 5:1-ARG1 
nw/wsj/07/wsj_0766.parse 82 3 gold know-v 29.5-1 IN know.01 1 ----- 1:1-ARG0=Agent 2:1-ARGM-TMP 3:0-rel 4:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 82 21 gold say-v 37.7-1 IN say.01 null ----- 0:1*22:1-ARG1=Topic 20:1-ARG0=Agent 21:0-rel 
nw/wsj/07/wsj_0766.parse 83 2 gold say-v 37.7-1 IN say.01 null ----- 0:1-ARGM-TMP 1:1-ARG0=Agent 2:0-rel 3:1-ARG1=Topic 
nw/wsj/07/wsj_0766.parse 83 6 gold think-v 29.9-2 IN think.01 null ----- 4:1*8:1-ARG0 6:0-rel 7:1-ARG2 
nw/wsj/07/wsj_0766.parse 84 5 gold deal-v 83 Resolve_problem deal.01 null ----- 1:1*4:1-ARG0=Agent 5:0-rel 6:1-ARG1=Theme 1:1*4:1-LINK-PRO 
nw/wsj/07/wsj_0766.parse 85 1 gold live-v 46 Residence live.01 null ----- 0:1-ARG0=Theme;Resident 1:0-rel 2:1-ARGM-LOC 
nw/wsj/07/wsj_0766.parse 85 7 gold hit-v 18.4 Experience_bodily_harm hit.01 null ----- 5:1*8:1-ARG1=Location 7:0-rel 9:1-ARGM-MNR 10:1-ARG2=Theme 5:1*8:1-LINK-PSV 
nw/wsj/07/wsj_0766.parse 86 3 gold have-v 100 IN have.03 null ----- 0:1-ARG0=Pivot 2:0-ARGM-NEG 3:0-rel 4:1-ARG1=Theme 6:1-ARGM-TMP 
nw/wsj/07/wsj_0766.parse 87 24 gold hit-v 18.4 Impact hit.01 null ----- 21:1-ARG0 24:0-rel 25:1-ARG1=Location;Impactee 
nw/wsj/07/wsj_0766.parse 88 5 gold wish-v 32.2-1 Desiring wish.01 null ----- 0:2-ARG0=Pivot 5:0-rel 6:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 88 8 gold have-v 100 IN have.03 null ----- 7:1-ARG0=Pivot 8:0-rel 9:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 89 10 gold say-v 37.7-1 IN say.01 null ----- 0:1*11:1-ARG1=Topic 9:1-ARG0=Agent 10:0-rel 
nw/wsj/07/wsj_0766.parse 90 6 gold get-v 13.5.1-1 IN get.01 null ----- 0:1-ARGM-TMP 3:1*9:1-ARG0=Agent 5:1-ARGM-TMP 6:0-rel 7:1-ARG1=Theme 
nw/wsj/07/wsj_0766.parse 90 11 gold water-v 9.9 NF water.01 1 ----- 3:1*9:1-ARG0=Agent 11:0-rel 12:1-ARG1=Destination 
nw/wsj/07/wsj_0766.parse 90 16 gold stop-v 55.4-1 Activity_stop stop.01 null ----- 0:1-ARGM-TMP 3:1*9:1-ARG0=Agent;Agent 16:0-rel 17:1-ARGM-MNR 
nw/wsj/07/wsj_0766.parse 91 2 gold realize-v 29.5-1 Coming_to_believe realize.01 null ----- 1:1-ARG0=Agent;Cognizer 2:0-rel 3:1-ARG1=Theme;Content 
nw/wsj/07/wsj_0766.parse 91 20 gold have-v 100 IN have.03 null ----- 13:2*16:1*17:1-ARG0=Pivot 19:0-ARGM-NEG 20:0-rel 21:2-ARG1=Theme 13:2*16:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 91 26 gold drink-v 39.1-2 Ingestion drink.01 1 ----- 13:2*24:1-ARG0=Agent;Ingestor 21:1*23:1*27:1-ARG1=Patient;Ingestibles 26:0-rel 13:2*24:1-LINK-PRO 21:1*23:1-LINK-SLC 
nw/wsj/07/wsj_0766.parse 92 3 gold play-v 26.7-1-1 Cause_to_make_noise play.01 null ----- 0:1-ARG0=Agent 2:0-ARGM-NEG 3:0-rel 4:1-ARG1=Theme 6:1-ARGM-TMP 11:1-ARGM-CAU 
nw/wsj/07/wsj_0766.parse 92 9 gold hit-v 18.4 Cause_harm hit.01 null ----- 7:1-ARG0 9:0-rel 
nw/wsj/07/wsj_0766.parse 92 18 gold die-v 48.2 Death die.01 null ----- 15:1*16:1*17:1-ARG1=Patient;Protagonist 18:0-rel 19:1-ARGM-LOC 15:1*16:1-LINK-SLC 
