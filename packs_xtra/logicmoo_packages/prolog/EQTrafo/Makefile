# ========================================
# Makefile for EQTrafo
# --------------------
# ========================================

# where this package is located
ROOT_DIR = /home/bthomas/HI-WORK/PETER/EQTrafo
SRC = src

# where the binary should go to
BIN_DIR = /lab/ki2/AGKI/systems/bin

# group allowed to modify
GROUP = agki-all

# ==================================================

SRC_DIR = $(ROOT_DIR)/$(SRC)

install :
	@echo "#!/bin/sh" > $(BIN_DIR)/eqtrafo 
	@echo "eclipse -b $(ROOT_DIR)/src/eqtrafo.pl -e \"eqtrafo('\$$1'), halt.\"" >> $(BIN_DIR)/eqtrafo
	@chgrp $(GROUP) $(BIN_DIR)/eqtrafo
	@chmod a+rx $(BIN_DIR)/eqtrafo
	@chmod g+w $(BIN_DIR)/eqtrafo	

tar :
	( cd $(ROOT_DIR) ; cd .. ; \
	tar cvf eqtrafo.tar EQTrafo ; gzip eqtrafo.tar )



