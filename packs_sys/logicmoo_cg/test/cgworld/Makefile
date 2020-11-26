##############################################################
# Makefile for Conceptual Graph Processes (pCG) interpreter. #
##############################################################

###############
# Object code #
###############
OBJS = pCG cgp/CGP.class \
	cgp/CGPLexer.class cgp/CGPParser.class cgp/CGPInterpreter.class \
	cgp/CGPTokenTypes.class \
	cgp/runtime/Namespace.class \
	cgp/runtime/Scope.class \
	cgp/runtime/ScopeStack.class \
	cgp/runtime/KBase.class \
	cgp/runtime/KnowledgeBaseStack.class \
	cgp/runtime/Type.class \
	cgp/runtime/BooleanType.class \
	cgp/runtime/NumberType.class \
	cgp/runtime/StringType.class \
	cgp/runtime/ConceptType.class \
	cgp/runtime/GraphType.class \
	cgp/runtime/ListType.class \
	cgp/runtime/LambdaType.class \
	cgp/runtime/ActorType.class \
	cgp/runtime/FunctionType.class \
	cgp/runtime/ProcessType.class \
	cgp/runtime/Rule.class \
	cgp/runtime/FileType.class \
	cgp/runtime/SubActorInfo.class \
	cgp/runtime/FormalParameter.class\
	cgp/runtime/ReturnException.class \
	cgp/runtime/GraphException.class \
	cgp/runtime/ActorException.class \
	cgp/runtime/newtypes/Window.class \
	cgp/runtime/newtypes/Util.class \
	cgp/translators/AntlrCGIFLexer.class \
	cgp/translators/AntlrCGIFParser.class \
	cgp/translators/AntlrCGIFTokenTypes.class \
	cgp/translators/CGIFParser.class \
        cgp/translators/CGIFGenerator.class \
	cgp/translators/SimpleGenerator.class \
	cgp/translators/NumericQuantifier.class \
	cgp/translators/DefinedQuantifier.class \
	cgp/translators/ContextScope.class \
	cgp/translators/ContextScopeStack.class

#############
# Classpath #
#############
JAVA_TOOLS = $(HOME)/java-tools
ANTLR_HOME = $(JAVA_TOOLS)/antlr-2.7.0
NOTIO = $(JAVA_TOOLS)/notio/Notio.jar
CGP_HOME = $(HOME)/cgp
CLASSES = $(CGP_HOME):$(ANTLR_HOME):$(NOTIO)

#################
# Other Targets #
#################
all: $(OBJS)

clean:
	rm -f pcg.tar.gz
	rm -f cgp/*.class
	rm -f cgp/runtime/*.class
	rm -f cgp/runtime/newtypes/*.class
	rm -f cgp/CGPLexer.java
	rm -f cgp/CGPParser.java
	rm -f cgp/CGPInterpreter.java
	rm -f cgp/*TokenTypes.*
	rm -rf docs/*
	rm -f cgp/translators/*.class
	rm -f cgp/translators/AntlrCGIFLexer.java
	rm -f cgp/translators/AntlrCGIFParser.java
	rm -f cgp/translators/*TokenTypes.*

###################################
# Create the distribution archive #
#       *** UPDATE EXAMPLES ***   #
###################################
dist:
	zip -r cgp.zip $(CGP_HOME)

#################
# Documentation #
#################
doc:
	javadoc -classpath $(CLASSES) -private -d docs cgp cgp.runtime cgp.runtime.newtypes cgp.translators

#########################
# Generate EBNF as HTML #
#########################
bnf:
	java -classpath $(CLASSES) antlr.Tool -html -o docs cgp/CGPParser.g

	java -classpath $(CLASSES) antlr.Tool -html -o docs cgp/translators/AntlrCGIFParser.g

###################
# CGP interpreter #
#                 #
# Note: perhaps   #
# cgp.jar should  #
# be the target,  #
# not pCG.        #
###################
pCG: cgp/CGPLexer.class cgp/CGPParser.class cgp/CGPInterpreter.class \
     cgp/CGPTokenTypes.class cgp/CGP.class \
     cgp/runtime/Namespace.class \
     cgp/runtime/Scope.class \
     cgp/runtime/ScopeStack.class \
     cgp/runtime/KBase.class \
     cgp/runtime/KnowledgeBaseStack.class \
     cgp/runtime/Type.class \
     cgp/runtime/NumberType.class \
     cgp/runtime/BooleanType.class \
     cgp/runtime/StringType.class \
     cgp/runtime/ConceptType.class \
     cgp/runtime/GraphType.class \
     cgp/runtime/ListType.class \
     cgp/runtime/LambdaType.class \
     cgp/runtime/ActorType.class \
     cgp/runtime/FunctionType.class \
     cgp/runtime/ProcessType.class \
     cgp/runtime/Rule.class \
     cgp/runtime/FileType.class \
     cgp/runtime/SubActorInfo.class \
     cgp/runtime/FormalParameter.class \
     cgp/runtime/ReturnException.class \
     cgp/runtime/GraphException.class \
     cgp/runtime/ActorException.class \
     cgp/runtime/newtypes/Window.class \
     cgp/runtime/newtypes/Util.class \
     cgp/translators/AntlrCGIFLexer.class \
     cgp/translators/AntlrCGIFParser.class \
     cgp/translators/AntlrCGIFParser.class \
     cgp/translators/AntlrCGIFTokenTypes.class \
     cgp/translators/CGIFParser.class \
     cgp/translators/CGIFGenerator.class \
     cgp/translators/SimpleGenerator.class \
     cgp/translators/NumericQuantifier.class \
     cgp/translators/DefinedQuantifier.class \
     cgp/translators/ContextScope.class \
     cgp/translators/ContextScopeStack.class
	jar -cf lib/cgp.jar cgp/*.class cgp/runtime/*.class cgp/translators/*.class
	touch pCG
	chmod +x pCG

cgp/CGPLexer.class: cgp/CGPLexer.java
	javac -classpath $(CLASSES) cgp/CGPLexer.java

cgp/CGPParser.class: cgp/CGPParser.java
	javac -classpath $(CLASSES) cgp/CGPParser.java

cgp/CGPInterpreter.class: cgp/CGPInterpreter.java
	javac -classpath $(CLASSES) cgp/CGPInterpreter.java

cgp/CGPTokenTypes.class: cgp/CGPTokenTypes.java
	javac -classpath $(CLASSES) cgp/CGPTokenTypes.java

cgp/CGP.class: cgp/CGP.java
	javac -classpath $(CLASSES) cgp/CGP.java

cgp/CGPLexer.java: cgp/CGPParser.g
	java -classpath $(CLASSES) antlr.Tool -o cgp cgp/CGPParser.g

cgp/CGPParser.java: cgp/CGPParser.g
# CGPParser.java is generated by the preceding rule before this one is reached.

cgp/CGPInterpreter.java: cgp/CGPInterpreter.g
	java -classpath $(CLASSES) antlr.Tool -o cgp cgp/CGPInterpreter.g

#################
# Run-time code #
#################
cgp/runtime/Namespace.class: cgp/runtime/Namespace.java
	javac -classpath $(CLASSES) cgp/runtime/Namespace.java

cgp/runtime/Scope.class: cgp/runtime/Scope.java
	javac -classpath $(CLASSES) cgp/runtime/Scope.java

cgp/runtime/ScopeStack.class: cgp/runtime/ScopeStack.java
	javac -classpath $(CLASSES) cgp/runtime/ScopeStack.java

cgp/runtime/KBase.class: cgp/runtime/KBase.java
	javac -classpath $(CLASSES) cgp/runtime/KBase.java

cgp/runtime/KnowledgeBaseStack.class: cgp/runtime/KnowledgeBaseStack.java
	javac -classpath $(CLASSES) cgp/runtime/KnowledgeBaseStack.java

cgp/runtime/Type.class: cgp/runtime/Type.java
	javac -classpath $(CLASSES) cgp/runtime/Type.java

cgp/runtime/NumberType.class: cgp/runtime/NumberType.java
	javac -classpath $(CLASSES) cgp/runtime/NumberType.java

cgp/runtime/BooleanType.class: cgp/runtime/BooleanType.java
	javac -classpath $(CLASSES) cgp/runtime/BooleanType.java

cgp/runtime/StringType.class: cgp/runtime/StringType.java
	javac -classpath $(CLASSES) cgp/runtime/StringType.java

cgp/runtime/ConceptType.class: cgp/runtime/ConceptType.java
	javac -classpath $(CLASSES) cgp/runtime/ConceptType.java

cgp/runtime/GraphType.class: cgp/runtime/GraphType.java
	javac -classpath $(CLASSES) cgp/runtime/GraphType.java

cgp/runtime/ListType.class: cgp/runtime/ListType.java
	javac -classpath $(CLASSES) cgp/runtime/ListType.java

cgp/runtime/LambdaType.class: cgp/runtime/LambdaType.java
	javac -classpath $(CLASSES) cgp/runtime/LambdaType.java

cgp/runtime/ActorType.class: cgp/runtime/ActorType.java
	javac -classpath $(CLASSES) cgp/runtime/ActorType.java

cgp/runtime/SubActorInfo.class: cgp/runtime/SubActorInfo.java
	javac -classpath $(CLASSES) cgp/runtime/SubActorInfo.java

cgp/runtime/FunctionType.class: cgp/runtime/FunctionType.java
	javac -classpath $(CLASSES) cgp/runtime/FunctionType.java

cgp/runtime/ProcessType.class: cgp/runtime/ProcessType.java
	javac -classpath $(CLASSES) cgp/runtime/ProcessType.java

cgp/runtime/Rule.class: cgp/runtime/Rule.java
	javac -classpath $(CLASSES) cgp/runtime/Rule.java

cgp/runtime/FileType.class: cgp/runtime/FileType.java
	javac -classpath $(CLASSES) cgp/runtime/FileType.java

cgp/runtime/FormalParameter.class: cgp/runtime/FormalParameter.java
	javac -classpath $(CLASSES) cgp/runtime/FormalParameter.java

cgp/runtime/ReturnException.class: cgp/runtime/ReturnException.java
	javac -classpath $(CLASSES) cgp/runtime/ReturnException.java

cgp/runtime/GraphException.class: cgp/runtime/GraphException.java
	javac -classpath $(CLASSES) cgp/runtime/GraphException.java

cgp/runtime/ActorException.class: cgp/runtime/ActorException.java
	javac -classpath $(CLASSES) cgp/runtime/ActorException.java

####################################################
# User-defined types.                              #
# Add rules here and add targets in 2 places above #
# (look for newtypes in path).                     #
####################################################
cgp/runtime/newtypes/Window.class: cgp/runtime/newtypes/Window.java
	javac -classpath $(CLASSES) cgp/runtime/newtypes/Window.java

cgp/runtime/newtypes/Util.class: cgp/runtime/newtypes/Util.java
	javac -classpath $(CLASSES) cgp/runtime/newtypes/Util.java

###############
# CGIF Parser #
###############
cgp/translators/AntlrCGIFLexer.class: cgp/translators/AntlrCGIFLexer.java
	javac -classpath $(CLASSES) cgp/translators/AntlrCGIFLexer.java

cgp/translators/AntlrCGIFParser.class: cgp/translators/AntlrCGIFParser.java
	javac -classpath $(CLASSES) cgp/translators/AntlrCGIFParser.java

cgp/translators/AntlrCGIFTokenTypes.class: cgp/translators/AntlrCGIFTokenTypes.java
	javac -classpath $(CLASSES) cgp/translators/AntlrCGIFTokenTypes.java

cgp/translators/AntlrCGIFLexer.java: cgp/translators/AntlrCGIFParser.g
	java -classpath $(CLASSES) antlr.Tool -o cgp/translators cgp/translators/AntlrCGIFParser.g

cgp/translators/AntlrCGIFParser.java: cgp/translators/AntlrCGIFParser.g

cgp/translators/CGIFParser.class: cgp/translators/CGIFParser.java
	javac -classpath $(CLASSES) cgp/translators/CGIFParser.java

cgp/translators/NumericQuantifier.class: cgp/translators/NumericQuantifier.java
	javac -classpath $(CLASSES) cgp/translators/NumericQuantifier.java

cgp/translators/DefinedQuantifier.class: cgp/translators/DefinedQuantifier.java
	javac -classpath $(CLASSES) cgp/translators/DefinedQuantifier.java

cgp/translators/ContextScope.class: cgp/translators/ContextScope.java
	javac -classpath $(CLASSES) cgp/translators/ContextScope.java

cgp/translators/ContextScopeStack.class: cgp/translators/ContextScopeStack.java
	javac -classpath $(CLASSES) cgp/translators/ContextScopeStack.java

##################
# CGIF Generator #
##################
cgp/translators/CGIFGenerator.class: cgp/translators/CGIFGenerator.java
	javac -classpath $(CLASSES) cgp/translators/CGIFGenerator.java

cgp/translators/SimpleGenerator.class: cgp/translators/SimpleGenerator.java
	javac -classpath $(CLASSES) cgp/translators/SimpleGenerator.java
