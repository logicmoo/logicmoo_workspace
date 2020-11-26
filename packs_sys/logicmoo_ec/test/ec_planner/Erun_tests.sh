#!/bin/bash


CAT_TO="./ec_reader_test_foundations.e"


function push_file () 
{ 
 echo "cat $1 >> $CAT_TO"
 #touch "$2"
 echo -e "\n\n\n; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n; FILE: $1\n; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" | cat >> $CAT_TO
 cat $1 >> $CAT_TO
}

rm -f ./ec_reader_test_foundations.e

CAT_TO="./ec_reader_test_foundations.e"
push_file foundations/Root.e 
push_file foundations/EC.e
push_file foundations/DEC.e
push_file foundations/ECCausal.e
push_file foundations/ECTraj.e

cp $CAT_TO ./ec_reader_test_ecnet.e
CAT_TO="./ec_reader_test_ecnet.e"

push_file ecnet/Ontology.e 
push_file ecnet/RTSpace.e 
push_file ecnet/OTSpace.e 
push_file ecnet/OMSpace.e 
push_file ecnet/GSpace.e 
push_file ecnet/PolySpace.e 
push_file ecnet/HandTo.e 
push_file ecnet/Container.e 
push_file ecnet/SpeechAct.e

push_file ecnet/Sleep.e
push_file ecnet/Sleeping.e
push_file ecnet/Rest.e
push_file ecnet/RepRest.e

push_file ecnet/Diving.e
push_file ecnet/Dress.e

push_file ecnet/HungerNeed.e
push_file ecnet/Restaurant.e

push_file ecnet/EatingInAHouse.e

if [ "1" -gt "2" ]
then

push_file ecnet/Condition.e
push_file ecnet/Cognition.e
push_file ecnet/Feeling.e

push_file ecnet/PlayNeed.e
push_file ecnet/IPRel.e
push_file ecnet/MakingAnAcquaintance.e

push_file ecnet/Book.e
push_file ecnet/ReadingABook.e
push_file ecnet/ReadingALetter.e
push_file ecnet/WritingABook.e


push_file ecnet/Vehicle.e
push_file ecnet/RidingInACarriage.e
push_file ecnet/CTime.e
push_file ecnet/ShootingAttack.e
push_file ecnet/Arson.e
push_file ecnet/Baseball.e
push_file ecnet/Bomb.e
push_file ecnet/Fire.e
push_file ecnet/Gun.e
push_file ecnet/Kidnapping.e
push_file ecnet/Money.e
push_file ecnet/Rain.e
push_file ecnet/SmallFire.e
push_file ecnet/Smoke.e
push_file ecnet/Smoking.e
push_file ecnet/TakingATrain.e
push_file ecnet/TakingAWalk.e
push_file ecnet/TimeDelayBombing.e
push_file ecnet/Vision.e
push_file ecnet/XWalk.e


push_file includes/SaaSPatterns/input.e $PRE./includes/SaaSPatterns.$POST
push_file includes/SaaSPatterns/ordering.e $PRE./includes/SaaSPatterns.$POST
push_file includes/SaaSPatterns/PolicyPatterns/policyModel.e $PRE./includes/SaaSPatterns/PolicyPatterns.$POST
push_file includes/SaaSPatterns/PolicySetPatterns/policySetModel.e $PRE./includes/SaaSPatterns/PolicySetPatterns.$POST
push_file includes/SaaSPatterns/RulesPatterns/ConditionsVerification.e $PRE./includes/SaaSPatterns/RulesPatterns.$POST
push_file includes/SaaSPatterns/RulesPatterns/ruleModel.e $PRE./includes/SaaSPatterns/RulesPatterns.$POST
push_file includes/SaaSPatterns/RulesPatterns/ruleOutput.e $PRE./includes/SaaSPatterns/RulesPatterns.$POST
push_file includes/SaaSPatterns/RulesPatterns/targetHolds.e $PRE./includes/SaaSPatterns/RulesPatterns.$POST
push_file includes/SaaSPatterns/sorts.e $PRE./includes/SaaSPatterns.$POST


push_file examples/AkmanEtAl2004/ZooWorld.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest1.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest2.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest3.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest4.1.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest4.2.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest5.1.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest5.2.e $PRE./examples/AkmanEtAl2004.$POST
push_file examples/AkmanEtAl2004/ZooTest6.e $PRE./examples/AkmanEtAl2004.$POST

push_file examples/Antoniou1997/Dropout.e $PRE./examples/Antoniou1997.$POST
push_file examples/Antoniou1997/Student.e $PRE./examples/Antoniou1997.$POST
push_file examples/BrewkaDixKonolige1997/Wine.e $PRE./examples/BrewkaDixKonolige1997.$POST
push_file examples/Cassimatis2002/OneScreen.e $PRE./examples/Cassimatis2002.$POST
push_file examples/Cassimatis2002/PolySpace.e $PRE./examples/Cassimatis2002.$POST
push_file examples/Cassimatis2002/TwoScreens.e $PRE./examples/Cassimatis2002.$POST
push_file examples/FrankEtAl2003/FrankEtAl.e $PRE./examples/FrankEtAl2003.$POST
push_file examples/FrankEtAl2003/Story1.e $PRE./examples/FrankEtAl2003.$POST
push_file examples/GiunchigliaEtAl2004/MonkeyBananas.e $PRE./examples/GiunchigliaEtAl2004.$POST
push_file examples/GiunchigliaEtAl2004/MonkeyPlanning.e $PRE./examples/GiunchigliaEtAl2004.$POST
push_file examples/GiunchigliaEtAl2004/MonkeyPostdiction.e $PRE./examples/GiunchigliaEtAl2004.$POST
push_file examples/GiunchigliaEtAl2004/MonkeyPrediction.e $PRE./examples/GiunchigliaEtAl2004.$POST

push_file examples/Manual/Example1.e $PRE./examples/Manual.$POST
push_file examples/Manual/Example1a.e $PRE./examples/Manual.$POST
push_file examples/Manual/Example2.e $PRE./examples/Manual.$POST
push_file examples/Manual/Example3.e $PRE./examples/Manual.$POST
push_file examples/Manual/Example4.e $PRE./examples/Manual.$POST

push_file examples/MillerShanahan2002/Bowl.e $PRE./examples/MillerShanahan2002.$POST

push_file examples/Mueller2004b/Approve.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004a/Holding.e $PRE./examples/Mueller2004a.$POST
push_file examples/Mueller2004b/OffOn.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004b/PickUp.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004a/Leaf.e $PRE./examples/Mueller2004a.$POST
push_file examples/Mueller2004b/Leaf.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004b/RouletteWheel.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004b/RunningAndDriving1.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004b/RunningAndDriving2.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004b/TV1.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2004b/TV2.e $PRE./examples/Mueller2004b.$POST
push_file examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e $PRE./examples/Mueller2006/Chapter10.$POST
push_file examples/Mueller2006/Chapter10/OneScreen.e $PRE./examples/Mueller2006/Chapter10.$POST
push_file examples/Mueller2006/Chapter10/TwoScreens.e $PRE./examples/Mueller2006/Chapter10.$POST
push_file examples/Mueller2006/Chapter11/HungryCat.e $PRE./examples/Mueller2006/Chapter11.$POST
push_file examples/Mueller2006/Chapter11/Lottery.e $PRE./examples/Mueller2006/Chapter11.$POST

push_file examples/Mueller2006/Chapter12/DefaultEvent.e $PRE./examples/Mueller2006/Chapter12.$POST
push_file examples/Mueller2006/Chapter12/DefaultLocation.e $PRE./examples/Mueller2006/Chapter12.$POST
push_file examples/Mueller2006/Chapter12/MethodB.e $PRE./examples/Mueller2006/Chapter12.$POST
push_file examples/Mueller2006/Chapter12/MethodD.e $PRE./examples/Mueller2006/Chapter12.$POST
push_file examples/Mueller2006/Chapter13/Abduction.e $PRE./examples/Mueller2006/Chapter13.$POST
push_file examples/Mueller2006/Chapter13/Deduction1.e $PRE./examples/Mueller2006/Chapter13.$POST
push_file examples/Mueller2006/Chapter13/Deduction2.e $PRE./examples/Mueller2006/Chapter13.$POST
push_file examples/Mueller2006/Chapter13/ModelFinding.e $PRE./examples/Mueller2006/Chapter13.$POST

push_file examples/Mueller2006/Chapter12/Device.e $PRE./examples/Mueller2006/Chapter12.$POST
push_file examples/Mueller2006/Chapter12/BrokenDevice.e $PRE./examples/Mueller2006/Chapter12.$POST
push_file examples/Mueller2006/Chapter12/ErraticDevice.e $PRE./examples/Mueller2006/Chapter12.$POST

push_file examples/Mueller2006/Chapter13/Postdiction.e $PRE./examples/Mueller2006/Chapter13.$POST
push_file examples/Mueller2006/Chapter14/NetBill1.e $PRE./examples/Mueller2006/Chapter14.$POST
push_file examples/Mueller2006/Chapter14/NetBill2.e $PRE./examples/Mueller2006/Chapter14.$POST
push_file examples/Mueller2006/Chapter14/NetBill3.e $PRE./examples/Mueller2006/Chapter14.$POST
push_file examples/Mueller2006/Chapter14/Vision.e $PRE./examples/Mueller2006/Chapter14.$POST

push_file examples/Mueller2006/Chapter14/Workflow.e $PRE./examples/Mueller2006/Chapter14.$POST

push_file examples/Mueller2006/Chapter2/Inconsistency1.e $PRE./examples/Mueller2006/Chapter2.$POST
push_file examples/Mueller2006/Chapter2/Inconsistency2.e $PRE./examples/Mueller2006/Chapter2.$POST
push_file examples/Mueller2006/Chapter2/Inconsistency3.e $PRE./examples/Mueller2006/Chapter2.$POST
push_file examples/Mueller2006/Chapter2/Inconsistency4.e $PRE./examples/Mueller2006/Chapter2.$POST

push_file examples/Mueller2006/Chapter2/Sleep1.e $PRE./examples/Mueller2006/Chapter2.$POST
push_file examples/Mueller2006/Chapter2/Sleep2.e $PRE./examples/Mueller2006/Chapter2.$POST
push_file examples/Mueller2006/Chapter2/Sleep3.e $PRE./examples/Mueller2006/Chapter2.$POST
push_file examples/Mueller2006/Chapter2/Sleep4.e $PRE./examples/Mueller2006/Chapter2.$POST
push_file examples/Mueller2006/Chapter3/Telephone1.e $PRE./examples/Mueller2006/Chapter3.$POST
push_file examples/Mueller2006/Chapter3/Telephone2.e $PRE./examples/Mueller2006/Chapter3.$POST
push_file examples/Mueller2006/Chapter4/AlarmClock.e $PRE./examples/Mueller2006/Chapter4.$POST
push_file examples/Mueller2006/Chapter4/BankAccountServiceFee.e $PRE./examples/Mueller2006/Chapter4.$POST
push_file examples/Mueller2006/Chapter6/CarryingABook1.e $PRE./examples/Mueller2006/Chapter6.$POST
push_file examples/Mueller2006/Chapter6/CarryingABook2.e $PRE./examples/Mueller2006/Chapter6.$POST
push_file examples/Mueller2006/Chapter6/ShanahanCircuit.e $PRE./examples/Mueller2006/Chapter6.$POST
push_file examples/Mueller2006/Chapter6/ThielscherCircuit1.e $PRE./examples/Mueller2006/Chapter6.$POST
push_file examples/Mueller2006/Chapter6/ThielscherCircuit2.e $PRE./examples/Mueller2006/Chapter6.$POST

push_file examples/Mueller2006/Chapter7/FallingObjectWithAntiTrajectory.e $PRE./examples/Mueller2006/Chapter7.$POST
push_file examples/Mueller2006/Chapter7/FallingObjectWithEvents.e $PRE./examples/Mueller2006/Chapter7.$POST
push_file examples/Mueller2006/Chapter7/HotAirBalloon.e $PRE./examples/Mueller2006/Chapter7.$POST

push_file examples/Mueller2006/Chapter8/CameraWithFlash.e $PRE./examples/Mueller2006/Chapter8.$POST
push_file examples/Mueller2006/Chapter8/MovingRobot.e $PRE./examples/Mueller2006/Chapter8.$POST
push_file examples/Mueller2006/Chapter8/PatHeadRubStomach.e $PRE./examples/Mueller2006/Chapter8.$POST
push_file examples/Mueller2006/Chapter9/RouletteWheel.e $PRE./examples/Mueller2006/Chapter9.$POST
push_file examples/Mueller2006/Chapter9/RunningAndDriving.e $PRE./examples/Mueller2006/Chapter9.$POST

push_file examples/Mueller2006/Exercises/Counter.e $PRE./examples/Mueller2006/Exercises.$POST
push_file examples/Mueller2006/Exercises/MixingPaints.e $PRE./examples/Mueller2006/Exercises.$POST

push_file examples/Mueller2006/Exercises/SnoozeAlarm.e $PRE./examples/Mueller2006/Exercises.$POST
push_file examples/Mueller2006/Exercises/TeacherTells.e $PRE./examples/Mueller2006/Exercises.$POST
push_file examples/Mueller2006/Exercises/TelephoneBugs.e $PRE./examples/Mueller2006/Exercises.$POST
push_file examples/ReiterCriscuolo1981/NixonDiamond1.e $PRE./examples/ReiterCriscuolo1981.$POST
push_file examples/ReiterCriscuolo1981/NixonDiamond2.e $PRE./examples/ReiterCriscuolo1981.$POST
push_file examples/Shanahan1997/BusRide.e $PRE./examples/Shanahan1997.$POST
push_file examples/Shanahan1997/DeadOrAlive.e $PRE./examples/Shanahan1997.$POST
push_file examples/Shanahan1997/StolenCar.e $PRE./examples/Shanahan1997.$POST
push_file examples/Shanahan1997/StuffyRoom.e $PRE./examples/Shanahan1997.$POST
push_file examples/Shanahan1997/Supermarket.e $PRE./examples/Shanahan1997.$POST
push_file examples/Shanahan1997/Yale.e $PRE./examples/Shanahan1997.$POST
push_file examples/Shanahan1999/ChessBoard.e $PRE./examples/Shanahan1999.$POST
push_file examples/Shanahan1999/CoinToss.e $PRE./examples/Shanahan1999.$POST
push_file examples/Shanahan1999/Happy.e $PRE./examples/Shanahan1999.$POST
push_file examples/Shanahan1999/RussianTurkey.e $PRE./examples/Shanahan1999.$POST
push_file examples/Shanahan1999/ThielscherCircuit.e $PRE./examples/Shanahan1999.$POST


fi
