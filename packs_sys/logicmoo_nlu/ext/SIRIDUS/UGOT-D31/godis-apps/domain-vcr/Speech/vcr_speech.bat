@echo off
set CLASSPATH=%OAA_HOME%/lib/jar/antlr-oaa.jar;%OAA_HOME%/lib/jar/concurrent-1.3.1.jar;%OAA_HOME%/lib/jar/log4j-1.2.7.jar;%OAA_HOME%/lib/jar/oaa2.jar;%OAA_HOME%/runtime/startit/jar/oaa2startit.jar;%OAA_HOME%/runtime/monitor/jar/oaa2monitor.jar  
set TRINDIKIT=D:/david/trindikit-3.2.0/dist
set GODIS=D:/david/godis

java com.sri.oaa2.agt.startit.Startit -delay 250 -props vcr_win.props vcr.config
