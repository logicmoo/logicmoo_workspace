# Build a LPS server image based on the latest SWISH/SWI-Prolog with
# 	docker build -t logicalcontracts/lps.swi .
# Grabed some recipes from https://hub.docker.com/r/dimitriirtimid/lps-swish/dockerfile
# Execute with
#	docker run -p 3051:3050 -v /Users/mc/git/logicalcontracts/swish/docker/tempData:/data -v /Users/mc/git/logicalcontracts/engine/logs:/home/lps/engine/logs logicalcontracts/lps.swi
#
FROM logicalcontracts/swish-valentine2019
LABEL maintainer = "mc@logicalcontracts.com"

# Set environment variables
ENV SHELL /bin/bash
ENV LPS_DIR /home/lps
ENV MY_USER lpspersona

# Create user
RUN useradd -m -s /bin/bash $MY_USER

WORKDIR /home
RUN git clone https://bitbucket.org/lpsmasters/lps_corner $LPS_DIR

RUN chown -R $MY_USER $LPS_DIR

# RUN mkdir -p $LPS_DIR/engine/logs
VOLUME $LPS_DIR/engine/logs
# probably unneeded: RUN chmod a+w logs  # ...or something finer

WORKDIR $LPS_DIR/swish/web/lps
RUN bower install vis paper

# Configure container startup
WORKDIR $LPS_DIR/swish
ENTRYPOINT /usr/bin/swipl user_module_file.pl ../../swish/daemon.pl --no-fork --port=3050 --user=$MY_USER --workers=16
