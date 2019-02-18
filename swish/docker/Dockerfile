# Build a LPS server image based on the latest SWISH/SWI-Prolog with
# 	docker build -t lps.swi .
# Inspired by https://hub.docker.com/r/dimitriirtimid/lps-swish/dockerfile
FROM swish-valentine2019
LABEL maintainer = "mc@logicalcontracts.com"

# Set environment variables
ENV SHELL /bin/bash
ENV LPS_DIR /home/lps
ENV MY_USER lpspersona

# Create user
RUN useradd -m -s /bin/bash $MY_USER

WORKDIR /home
RUN git clone https://bitbucket.org/lpsmasters/lps_corner $LPS_DIR
RUN mkdir -p $LPS_DIR/engine/logs

RUN chown -R $MY_USER $LPS_DIR
# USER $MY_USER
# probably unneeded: RUN chmod a+w logs  # ...or something finer

WORKDIR $LPS_DIR/swish/web/lps
RUN bower install vis paper

# Configure container startup
WORKDIR $LPS_DIR/swish
ENTRYPOINT /usr/bin/swipl user_module_file.pl ../../swish/daemon.pl --no-fork --port=3050 --user=$MY_USER --workers=16
