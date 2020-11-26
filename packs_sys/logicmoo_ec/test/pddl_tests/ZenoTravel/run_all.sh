



for i in p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p11a p12 p12a p13 p13a p14 p14a p14b p14c p15 p15a p15b p16 p17 p18 p18a p19
do
  echo "Planning for $i"
  ../../ffx/ff -o zeon_${i}_dp.pddl -f ${i}_dp.pddl > ${i}_out.txt 2> ${i}_err.txt
done