#============================================================
#		   METAGAME Game-Playing Workbench
#		  Copyright (c) 1992 Barney D. Pell
#============================================================

# make			to build metagame, leaving in this directory
# make install		to build metagame, installing in the bin directory
# make arch		as above, but installs in a specific dir for
#			this architecture.

# This must set the logical name of the parent directory in which the
# Metagame top-level directory will sit (probably the same one the
# original tar file was placed into).  This is necessary to 
# avoid auto-mounter problems. 
# The example below assumes it is in the topdirectory of the person
# making the system. 
# *IF NOT*, change it to the logical name of where the top directory
# will be. 
PARENTDIR=~

# Where you want the bin file to go, if you want to use
# make arch.
ARCHBINDIR=$(HOME)/bin/`arch`

# The command by which your prolog is invoked.
# Options:
# 	sicstus
#	quintus
#       < another command, like prolog >
#PROLOG=sicstus
PROLOG=quintus
#PROLOG=mixtus
#PROLOG=prolog

# If your sicstus or quintus prolog system is invoked
# by another name besides these two,  (third case above)
# you must explicitly tell
# Makefile which type your prolog is, ie. one of these:
#	sicstus (Sicstus Prolog)
# 	quintus (Quintus Prolog)
# If your system is invoked as sicstus or quintus, you don't
# need to change this line. 
PROLOGTYPE=$(PROLOG)
#PROLOGTYPE=sicstus
#PROLOGTYPE=quintus

# The name of the executable
NAME=metagame

# Options: 
# 	fastcode
# 	compactcode
#	profiledcode
#
# This is only important for Sicstus Prolog
COMPILING= fastcode
#COMPILING= profiledcode


# ----------------------------------------------------
# Don't change anything past here! 
# ----------------------------------------------------

# The number of this release of the Metagame Workbench
VERSION=3a

PROLOGVERSION= $(PROLOGTYPE)-version

#BIGDIR=/anfs/tiptmp/pell/`arch`

TOPDIR= Metagame

TOPDIRNAME=$(PARENTDIR)/$(TOPDIR)

BINDIR= ../bin


GENDIR= ../generator

LIBDIR= ../misc

STATEDIR= ../state

GAMESDIR= ../games

EVALSDIR= ../evals

ANALDIR= ../learning

THEORYDIR= ../theory

COMMSDIR= ../comms

DOCDIR = ../doc

COPYDIR = /tmp


COPYTOPDIR= $(COPYDIR)/$(TOPDIR)

COPYPLAYDIR= $(COPYTOPDIR)/play

FTPDIR= /homes/bdp/FTP
# FTPDIR= /homes/bdp/FTP/metagame/metagame$(VERSION)

#TARFILE= $(FTPDIR)/metagame.tar
TARFILE= $(FTPDIR)/metagame$(VERSION).tar

THEORY= $(THEORYDIR)/parse.pl $(THEORYDIR)/parse1.pl $(THEORYDIR)/parse2.pl \
	$(THEORYDIR)/invert.pl $(THEORYDIR)/matches.pl \
	$(THEORYDIR)/boards.pl \
	$(THEORYDIR)/print_boards.pl $(THEORYDIR)/legal.pl \
	$(THEORYDIR)/goals.pl 

PLAYER= param.pl ops.pl controller.pl \
	help.pl help_advisors.pl interface.pl setup.pl  \
	gen_menu.pl start_menu.pl local.pl \
	history.pl notation.pl advisors.pl mobility.pl alphabeta.pl value.pl

STATE= 	$(STATEDIR)/statify_theory.pl $(STATEDIR)/efficient_state.pl \
	$(STATEDIR)/compile_syms.pl $(STATEDIR)/stat.pl  \
	$(STATEDIR)/thread.pl

GENERATOR=$(GENDIR)/gen.pl $(GENDIR)/piece_names.pl $(GENDIR)/tokenizer.pl \
	$(GENDIR)/grammar.pl $(GENDIR)/gen_parameters.pl $(GENDIR)/genstructs.pl

LIBS= $(LIBDIR)/randoms.pl $(LIBDIR)/aux.pl $(LIBDIR)/shells.pl \
	$(LIBDIR)/menus.pl $(LIBDIR)/args.pl \
	$(LIBDIR)/tracing.pl $(LIBDIR)/timing.pl \
	$(LIBDIR)/mygraphs.pl $(LIBDIR)/floyd.pl \
	$(LIBDIR)/theoryl.pl $(LIBDIR)/dynamic_load.pl

COMMS= $(COMMSDIR)/comms.pl $(COMMSDIR)/serve_tourney.pl \
	$(COMMSDIR)/pipes.pl \
	$(COMMSDIR)/player.pl $(COMMSDIR)/referee.pl $(COMMSDIR)/humanist.pl \
	$(COMMSDIR)/randomist.pl $(COMMSDIR)/instantist.pl \
	$(COMMSDIR)/chinook.pl $(COMMSDIR)/chesstalk.pl \
	$(COMMSDIR)/morphstart $(COMMSDIR)/metastart.game


GAMES= $(GAMESDIR)/chess.game $(GAMESDIR)/shogi.game \
	$(GAMESDIR)/chinese_chess.game $(GAMESDIR)/turncoat_chess.game \
	$(GAMESDIR)/checkers.game $(GAMESDIR)/dames.game \
	$(GAMESDIR)/lose_chess.game $(GAMESDIR)/lose_checkers.game \
	$(GAMESDIR)/tic_tac_toe.game $(GAMESDIR)/chess.game \
	$(GAMESDIR)/ttt.game $(GAMESDIR)/chess3.game \
	$(GAMESDIR)/chess6.game \
	$(GAMESDIR)/game1.game $(GAMESDIR)/game2.game $(GAMESDIR)/game3.game \
	$(GAMESDIR)/game4.game $(GAMESDIR)/game5.game

ANAL= $(ANALDIR)/analysis.pl $(ANALDIR)/paths.pl \
	$(ANALDIR)/prom.pl $(ANALDIR)/arrive.pl \
	$(ANALDIR)/exclude.pl $(ANALDIR)/potent.pl \
	$(ANALDIR)/dominate.pl $(ANALDIR)/group.pl $(ANALDIR)/tables.pl \
	$(ANALDIR)/struct.pl $(ANALDIR)/possess.pl \
	$(ANALDIR)/step.pl $(ANALDIR)/flight.pl $(ANALDIR)/global.pl \
	$(ANALDIR)/tourney.pl



EVALS= $(EVALSDIR)/chess.eval $(EVALSDIR)/turncoat.eval \
	$(EVALSDIR)/traceab.eval $(EVALSDIR)/static.eval \
	$(EVALSDIR)/dynamic.eval

SYSDEV= sysdev.pl

SYSTEM=	Makefile build-metagame sicstus-version quintus-version \
	player_files.pl 

TOPSTUFF= Makefile README

DOC= $(DOCDIR)/theory_files

BINS= $(BINDIR)/dummy $(BINDIR)/tourn $(BINDIR)/tourney $(BINDIR)/tourneyq \
	$(BINDIR)/tourneyhost $(BINDIR)/tourneyhostq


metagame: $(THEORY) $(PLAYER) $(GENERATOR) $(STATE) $(ANAL) $(SYSDEV) $(LIBS)
	./build-metagame $(NAME) $(VERSION) $(PROLOGVERSION) $(COMPILING) $(TOPDIRNAME) | $(PROLOG)


install: metagame
	mv -f $(NAME) $(BINDIR)/
	echo Metagame installed.

arch: metagame
	mv -f $(NAME) $(ARCHBINDIR)/
	echo Metagame installed.


copy:
	mkdir -p $(COPYTOPDIR)
	mkdir -p $(COPYPLAYDIR)
	mkdir -p $(COPYPLAYDIR)/$(GENDIR)
	mkdir -p $(COPYPLAYDIR)/$(STATEDIR)
	mkdir -p $(COPYPLAYDIR)/$(THEORYDIR)
	mkdir -p $(COPYPLAYDIR)/$(EVALSDIR)
	mkdir -p $(COPYPLAYDIR)/$(ANALDIR)
	mkdir -p $(COPYPLAYDIR)/$(GAMESDIR)
	mkdir -p $(COPYPLAYDIR)/$(LIBDIR)
	mkdir -p $(COPYPLAYDIR)/$(BINDIR)
	mkdir -p $(COPYPLAYDIR)/$(COMMSDIR)
	mkdir -p $(COPYPLAYDIR)/$(DOCDIR)
	(cd .. ; cp $(TOPSTUFF) $(COPYTOPDIR))
	cp $(SYSTEM) $(COPYPLAYDIR)
	cp $(DOC) $(COPYPLAYDIR)/$(DOCDIR)
	cp $(BINS) $(COPYPLAYDIR)/$(BINDIR)
	./copyright  $(COPYPLAYDIR) $(SYSDEV)
	./copyright  $(COPYPLAYDIR) $(PLAYER) 
	./copyright  $(COPYPLAYDIR)/$(COMMSDIR) $(COMMS) 
	./copyright  $(COPYPLAYDIR)/$(GENDIR) $(GENERATOR) 
	./copyright  $(COPYPLAYDIR)/$(STATEDIR) $(STATE) 
	./copyright  $(COPYPLAYDIR)/$(THEORYDIR) $(THEORY)
	./copyright  $(COPYPLAYDIR)/$(ANALDIR) $(ANAL) 
	./copyright  $(COPYPLAYDIR)/$(GAMESDIR) $(GAMES) 
	./copyright  $(COPYPLAYDIR)/$(EVALSDIR) $(EVALS) 
	./copyright  $(COPYPLAYDIR)/$(LIBDIR) $(LIBS) 
	find $(COPYTOPDIR) -exec chmod u+w {} \;
	
clean:
	rm -r $(COPYTOPDIR)

tar: copy
	(cd $(COPYDIR); tar cvof $(TARFILE) $(TOPDIR) )
	rm -r $(COPYTOPDIR) 

compress: tar
	compress -f $(TARFILE)

copyclean:
	rm $(TARFILE).Z
