

.PHONY: all download build clean

URL = 'http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data'
WGET = wget

all: build

download: 
	$(WGET) $(URL)

build: download
	cat iris.data | bash mkdata.sh

clean:
	rm -f *.f *.n *.b *.bg iris.data
