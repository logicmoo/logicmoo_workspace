
default :
	${MAKE} RegulusGUI.jar

judge :
	${MAKE} JudgeGUI.jar

RegulusGUI.jar : RegulusGUI/RegulusGUI.class
	jar -cvf RegulusGUI.jar RegulusGUI/*.class 

RegulusGUI/RegulusGUI.class : RegulusGUI/RegulusGUI.java
	javac \
	-classpath "${SPINSTALLDIR}/bin/prologbeans.jar;." \
	RegulusGUI/*.java

JudgeGUI.jar : JudgeGUI/JudgeGUI.class
	jar -cvf JudgeGUI.jar JudgeGUI/*.class 

JudgeGUI/JudgeGUI.class : JudgeGUI/JudgeGUI.java
	javac JudgeGUI/*.java

clean :
	rm RegulusGUI/*.class RegulusGUI.jar

clean_judge :
	rm JudgeGUI/*.class JudgeGUI.jar

