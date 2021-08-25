# ========================================
# Makefile for ProSpec
# --------------------
# ========================================

# where the whole package is located
ROOT_DIR = /home/bthomas/HI-WORK/PETER/ProSpec
# sub dir where the sources are
SRC = src

# where the binary should go to
# in fact its no binary, just a simple
# script that invokes eclipse ...
BIN_DIR = /lab/ki2/AGKI/systems/bin

# group allowd to modify
GROUP = agki-all

# ##################################################
# 
# ##################################################

SRC_DIR = $(ROOT_DIR)/$(SRC)

SRC_FILES =  $(SRC_DIR)/prospec.pl $(SRC_DIR)/misc.pl \
             $(SRC_DIR)/pl2tme_pro.pl $(SRC_DIR)/parser.pl

install :
	@echo "#!/bin/sh" > $(BIN_DIR)/prospec 
	@echo "eclipse -b $(ROOT_DIR)/src/prospec.pl -e \"prospec('\$$1'), halt.\"" >> $(BIN_DIR)/prospec
	@chgrp $(GROUP) $(BIN_DIR)/prospec
	@chmod a+rx $(BIN_DIR)/prospec
	@chmod g+w $(BIN_DIR)/prospec	

tar :
	( cd $(ROOT_DIR) ; cd .. ; \
	tar cvf prospec.tar ProSpec ; \
	gzip prospec.tar )



