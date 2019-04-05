# Build a LPS server image based on the latest SWISH/SWI-Prolog with
#
# 	docker build --build-arg LPS_SHA1=$(git log -1 --pretty=%H) -t logicalcontracts/lps.swi .
#
# Grabed some recipes from https://hub.docker.com/r/dimitriirtimid/lps-swish/dockerfile
# Execute with
#	docker run -p 3051:3050 -v /Users/mc/git/lps_corner/swish/docker/tempData:/data -v /Users/mc/git/lps_corner/engine/logs:/home/lps/engine/logs logicalcontracts/lps.swi
#
FROM logicalcontracts/swish-valentine2019
LABEL maintainer = "mc@logicalcontracts.com"

# Set environment variables
ENV SHELL /bin/bash
ENV LPS_DIR /home/lps_corner
ARG LPS_SHA1
ENV MY_USER lpspersona
ENV SWISH_DIR /home/swish
ENV SWISH_DATA /data

# Create user
RUN useradd -m -s /bin/bash $MY_USER

# WORKDIR /home
RUN git clone https://bitbucket.org/lpsmasters/lps_corner $LPS_DIR && \
	cd ${LPS_DIR} && git checkout -q ${LPS_SHA1} && chown -R $MY_USER $LPS_DIR
#RUN chown -R $MY_USER $LPS_DIR

# RUN mkdir -p $LPS_DIR/engine/logs
VOLUME $LPS_DIR/engine/logs
# probably unneeded: RUN chmod a+w logs  # ...or something finer

WORKDIR $LPS_DIR/swish/web/lps
RUN bower install vis paper

# Configure container startup
WORKDIR /
ENTRYPOINT /usr/bin/swipl $LPS_DIR/swish/user_module_file.pl ${SWISH_DIR}/daemon.pl --no-fork --port=3050 --user=$MY_USER --workers=16
