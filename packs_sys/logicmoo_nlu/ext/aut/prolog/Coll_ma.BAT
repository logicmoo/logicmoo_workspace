@echo off
cls

copy au\ma.ari+au\gen.ari+lists.ari+nl_util.ari+au\wkb_ma.ari au\matotal.ari

apc32 au\matotal.ari
link au\matotal.obj arity32i.lib /SUBSYSTEM:CONSOLE /ENTRY:main /OUT:ma.exe


