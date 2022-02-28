ls
fast-downward.py --run-all .
fast-downward.py --run-all domain-gripper.pddl 
fast-downward.py --run-all domain-gripper.pddl gripper2.pddl 
ls
cat output.sas 
fast-downward.py --run-all domain-gripper.pddl gripper2.pddl --help
fast-downward.py  --help
ls
fast-downward.py --run-all domain-gripper.pddl gripper8.pddl 
fast-downward.py domain-gripper.pddl gripper8.pddl 
cat output.sas 
fast-downward.py domain-gripper.pddl gripper8.pddl --help
fast-downward.py domain-gripper.pddl gripper8.pddl --help | grep -2 -i plan
cd ../sliding/
