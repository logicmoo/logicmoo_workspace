# ##################################################
# $Id: Makefile,v 1.3 1998/04/07 10:48:49 bthomas Exp $
# $Log: Makefile,v $
# Revision 1.3  1998/04/07 10:48:49  bthomas
# removed make exec
#
# Revision 1.2  1998/01/16 15:35:10  bthomas
# minor change to ci -u
#
# Revision 1.1  1998/01/16 15:32:32  bthomas
# Initial revision
#

# where the whole package is located
ROOT_DIR = /home/bthomas/HI-WORK/PETER/EQTrafo
# sub dir where the sources are
SRC = src

SRC_DIR = $(ROOT_DIR)/$(SRC)

SRC_FILES =  Makefile eqtrafo.pl myread.pl

co: 
	co -l $(SRC_FILES)

ci: $(SRC_FILES)
	ci -u $(SRC_FILES)
