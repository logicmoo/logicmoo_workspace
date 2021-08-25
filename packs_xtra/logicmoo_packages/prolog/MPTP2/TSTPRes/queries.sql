-- all vampire successes (6633)
SELECT * FROM `Vampire___9_0` WHERE result="CSA" or result="THM" or result="UNS" or result="SAT";
-- all E successes (6978)
SELECT * FROM `E___0_999` WHERE result="CSA" or result="THM" or result="UNS" or result="SAT";
-- all SPASS successes (5842)
SELECT * FROM `SPASS___3_0` WHERE result="CSA" or result="THM" or result="UNS" or result="SAT";

-- count of vampire,fampire,E,SPASS,Leancop,SRASS sucesses on the SEU (Mizar) problems
-- 358
SELECT count(*) FROM `Vampire___9_0` WHERE (problem like "SEU%") and result="THM";
-- 405
SELECT count(*) FROM `Fampire___1_3` WHERE (problem like "SEU%") and result="THM";
-- 316
SELECT count(*) FROM `E___0_999` WHERE (problem like "SEU%") and result="THM";
-- 340
SELECT count(*) FROM `SPASS___3_0` WHERE (problem like "SEU%") and result="THM";
-- 301
SELECT count(*) FROM `leanCoP___2_0` WHERE (problem like "SEU%") and result="THM";
-- 414
SELECT count(*) FROM `SRASS___0_1` WHERE (problem like "SEU%") and result="THM";

-- order provers by their results on SEU problems
SELECT count( * ) as c , prover FROM `all` WHERE problem like "SEU%" and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC

-- order provers by their results on MPTPChallenge chainy  problems (SEU%+2)
SELECT count( * ) as c , prover FROM `all` WHERE problem like "SEU%+2" and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC

-- order provers by their results on isabelle problems (needs the isab_prob table)
SELECT count( * ) as c , prover FROM `all`,`isab_prob` WHERE `all`.problem= `isab_prob`.problem and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC


-- select SPASS results on CASC21 problems
SELECT `SPASS___3_0`.*,tptp.* FROM `SPASS___3_0`,tptp WHERE `SPASS___3_0`.problem=tptp.problem and tptp.casc21=1

-- count SPASS successes on CASC21 problems
SELECT count(*) FROM `SPASS___3_0`,tptp WHERE `SPASS___3_0`.problem=tptp.problem and tptp.casc21=1 and (`SPASS___3_0`.result="THM" or `SPASS___3_0`.result="UNS" or `SPASS___3_0`.result="SAT" or `SPASS___3_0`.result="CSA")

-- doing previous using the `all` table: (498)
SELECT count(*) FROM `all`,tptp WHERE `all`.problem=tptp.problem and tptp.casc21=1 and prover="SPASS___3_0" and (`all`.result="THM" or `all`.result="UNS" or `all`.result="SAT" or `all`.result="CSA")

-- counting  E and SPASS success on CASC21 problems (681)
SELECT count(distinct tptp.problem) FROM `all`,tptp WHERE `all`.problem=tptp.problem and tptp.casc21=1 and (prover="SPASS___3_0" or prover="E___0_999") and (`all`.result="THM" or `all`.result="UNS" or `all`.result="SAT" or `all`.result="CSA")

-- select casc21 data, sotr them by prover name
SELECT `all`.prover,`all`.problem,`all`.result,`all`.output_status,`all`.time,tptp.* FROM `all`,tptp WHERE `all`.problem=tptp.problem and tptp.casc21=1 order by `all`.prover asc

-- order systems by their success on CASC21 problems
SELECT count( * ) as c , prover FROM `all` , tptp WHERE `all`.problem = tptp.problem and tptp.casc21 = 1 and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover  ORDER BY `c`  DESC

-- order systems by their success on CASC21 divisions
SELECT count( * ) as c , prover,division FROM `all` , tptp WHERE `all`.problem = tptp.problem and tptp.casc21 = 1 and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover,division  ORDER BY division,`c`  DESC

-- the same for all of tptp
SELECT count( * ) as c , prover,division FROM `all` , tptp WHERE `all`.problem = tptp.problem and ( `all`.result = "THM" or `all`.result = "UNS" or `all`.result = "SAT" or `all`.result = "CSA" ) group by prover,division  ORDER BY division,`c`  DESC

-- cummulative times for the FOF division for 10 provers (takes 10 minutes)
select t1.n as time ,t1.cx as Vampire___9_0, t2.cx as  Vampire___8_1,
t3.cx as E___0_999, t4.cx as  SPASS___3_0,
t5.cx as Prover9___0607, t6.cx as  iProver___0_2,
t7.cx as SNARK___20061020, t8.cx as  SRASS___0_1,
t9.cx as Fampire___1_3, t10.cx as  Equinox___1_2, t11.cx as leanCoP___2_0
 from 
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___9_0` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t1,
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___8_1` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t2,
(SELECT n300.n as n,count(x.problem) as cx FROM `E___0_999` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t3,
(SELECT n300.n as n,count(x.problem) as cx FROM `SPASS___3_0` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t4,
(SELECT n300.n as n,count(x.problem) as cx FROM `Prover9___0607` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t5,
(SELECT n300.n as n,count(x.problem) as cx FROM `iProver___0_2` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t6,
(SELECT n300.n as n,count(x.problem) as cx FROM `SNARK___20061020` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t7,
(SELECT n300.n as n,count(x.problem) as cx FROM `SRASS___0_1` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t8,
(SELECT n300.n as n,count(x.problem) as cx FROM `Fampire___1_3` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t9,
(SELECT n300.n as n,count(x.problem) as cx FROM `Equinox___1_2` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t10,
(SELECT n300.n as n,count(x.problem) as cx FROM `leanCoP___2_0` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="FOF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t11
where t1.n=t2.n and t1.n=t3.n and t1.n=t4.n and t1.n=t5.n and t1.n=t6.n and t1.n=t7.n and t1.n=t8.n and t1.n=t9.n and t1.n=t10.n and t1.n=t11.n;



-- the same for CNF (some FOF provers removed)
select t1.n as time ,t1.cx as Vampire___9_0, t2.cx as  Vampire___8_1,
t3.cx as E___0_999, t4.cx as  SPASS___3_0,
t5.cx as Prover9___0607, t6.cx as  iProver___0_2,
t7.cx as SNARK___20061020, 
t10.cx as  Equinox___1_2
 from 
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___9_0` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t1,
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___8_1` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t2,
(SELECT n300.n as n,count(x.problem) as cx FROM `E___0_999` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t3,
(SELECT n300.n as n,count(x.problem) as cx FROM `SPASS___3_0` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t4,
(SELECT n300.n as n,count(x.problem) as cx FROM `Prover9___0607` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t5,
(SELECT n300.n as n,count(x.problem) as cx FROM `iProver___0_2` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t6,
(SELECT n300.n as n,count(x.problem) as cx FROM `SNARK___20061020` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t7,
(SELECT n300.n as n,count(x.problem) as cx FROM `Equinox___1_2` as x,tptp,n300 where x.time<n300.n and tptp.problem=x.problem and tptp.division="CNF" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t10
where t1.n=t2.n and t1.n=t3.n and t1.n=t4.n and t1.n=t5.n and t1.n=t6.n and t1.n=t7.n  and t1.n=t10.n;


-- cummulative times for the SEU%+2 (MPTP chainy division) for 10 provers (takes 1 second)
select t1.n as time ,t1.cx as Vampire___9_0, t2.cx as  Vampire___8_1,
t3.cx as E___0_999, t4.cx as  SPASS___3_0,
t5.cx as Prover9___0607, t6.cx as  iProver___0_2,
t7.cx as SNARK___20061020, t8.cx as  SRASS___0_1,
t9.cx as Fampire___1_3, t10.cx as  Equinox___1_2, t11.cx as leanCoP___2_0
 from 
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___9_0` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t1,
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___8_1` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t2,
(SELECT n300.n as n,count(x.problem) as cx FROM `E___0_999` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t3,
(SELECT n300.n as n,count(x.problem) as cx FROM `SPASS___3_0` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t4,
(SELECT n300.n as n,count(x.problem) as cx FROM `Prover9___0607` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t5,
(SELECT n300.n as n,count(x.problem) as cx FROM `iProver___0_2` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t6,
(SELECT n300.n as n,count(x.problem) as cx FROM `SNARK___20061020` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t7,
(SELECT n300.n as n,count(x.problem) as cx FROM `SRASS___0_1` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t8,
(SELECT n300.n as n,count(x.problem) as cx FROM `Fampire___1_3` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t9,
(SELECT n300.n as n,count(x.problem) as cx FROM `Equinox___1_2` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t10,
(SELECT n300.n as n,count(x.problem) as cx FROM `leanCoP___2_0` as x,n300 where x.time<n300.n  and x.problem LIKE "SEU%+2" and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t11
where t1.n=t2.n and t1.n=t3.n and t1.n=t4.n and t1.n=t5.n and t1.n=t6.n and t1.n=t7.n and t1.n=t8.n and t1.n=t9.n and t1.n=t10.n and t1.n=t11.n;

-- cummulative times for the MPTP chainy division for 10 provers (takes 1 second)
select t1.n as time ,t1.cx as Vampire___9_0, t2.cx as  Vampire___8_1,
t3.cx as E___0_999, t4.cx as  SPASS___3_0,
t5.cx as Prover9___0607, t6.cx as  iProver___0_2,
t7.cx as SNARK___20061020, t8.cx as  SRASS___0_1,
t9.cx as Fampire___1_3, t10.cx as  Equinox___1_2, t11.cx as leanCoP___2_0
 from 
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___9_0` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t1,
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___8_1` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t2,
(SELECT n300.n as n,count(x.problem) as cx FROM `E___0_999` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t3,
(SELECT n300.n as n,count(x.problem) as cx FROM `SPASS___3_0` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t4,
(SELECT n300.n as n,count(x.problem) as cx FROM `Prover9___0607` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t5,
(SELECT n300.n as n,count(x.problem) as cx FROM `iProver___0_2` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t6,
(SELECT n300.n as n,count(x.problem) as cx FROM `SNARK___20061020` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t7,
(SELECT n300.n as n,count(x.problem) as cx FROM `SRASS___0_1` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t8,
(SELECT n300.n as n,count(x.problem) as cx FROM `Fampire___1_3` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t9,
(SELECT n300.n as n,count(x.problem) as cx FROM `Equinox___1_2` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t10,
(SELECT n300.n as n,count(x.problem) as cx FROM `leanCoP___2_0` as x,bushy_prob,n300 where x.time<n300.n  and x.problem= bushy_prob.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t11
where t1.n=t2.n and t1.n=t3.n and t1.n=t4.n and t1.n=t5.n and t1.n=t6.n and t1.n=t7.n and t1.n=t8.n and t1.n=t9.n and t1.n=t10.n and t1.n=t11.n;


-- times for isabelle problems (CNF) (92s)
select t1.n as time ,t1.cx as Vampire___9_0, t2.cx as  Vampire___8_1,
t3.cx as E___0_999, t4.cx as  SPASS___3_0,
t5.cx as Prover9___0607, t6.cx as  iProver___0_2,
t7.cx as SNARK___20061020, 
t10.cx as  Equinox___1_2
 from 
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___9_0` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t1,
(SELECT n300.n as n,count(x.problem) as cx FROM `Vampire___8_1` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t2,
(SELECT n300.n as n,count(x.problem) as cx FROM `E___0_999` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t3,
(SELECT n300.n as n,count(x.problem) as cx FROM `SPASS___3_0` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t4,
(SELECT n300.n as n,count(x.problem) as cx FROM `Prover9___0607` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t5,
(SELECT n300.n as n,count(x.problem) as cx FROM `iProver___0_2` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t6,
(SELECT n300.n as n,count(x.problem) as cx FROM `SNARK___20061020` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t7,
(SELECT n300.n as n,count(x.problem) as cx FROM `Equinox___1_2` as x,isab_prob,n300 where x.time<n300.n and isab_prob.problem=x.problem and (result="CSA" or result="THM" or result="UNS" or result="SAT") group by n300.n) as t10
where t1.n=t2.n and t1.n=t3.n and t1.n=t4.n and t1.n=t5.n and t1.n=t6.n and t1.n=t7.n  and t1.n=t10.n;
