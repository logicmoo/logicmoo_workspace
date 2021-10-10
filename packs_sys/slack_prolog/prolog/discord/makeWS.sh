export BIN_CLASS_DIR=~prologmud_server/classes

javac -classpath "$BIN_CLASS_DIR:/usr/share/java/*" PrologWebSocket.java -d . ; mv -f PrologWebSocket.class $BIN_CLASS_DIR
