FROM ubuntu:latest
WORKDIR /opt/application
RUN apt-get -y update
RUN apt-get install -y wget git ant
RUN apt-get install -y swi-prolog swi-prolog-java
RUN apt-get install -y openjdk-7-jdk
RUN git clone --depth=1 https://github.com/Attempto/APE.git
RUN mv APE ape
WORKDIR /opt/application/ape
RUN bash make_exe.sh
WORKDIR /opt/application
RUN git clone --depth=1 https://github.com/tkuhn/AceRules.git
WORKDIR /opt/application/AceRules/webapp
RUN ant createwar
RUN wget -O jetty-runner.jar http://repo2.maven.org/maven2/org/mortbay/jetty/jetty-runner/8.1.9.v20130131/jetty-runner-8.1.9.v20130131.jar
ENV LD_PRELOAD /usr/lib/swi-prolog/lib/amd64/libjpl.so
ENV LD_LIBRARY_PATH /usr/lib/jvm/default-java/jre/lib/amd64:/usr/lib/jvm/default-java/jre/lib/amd64/server
EXPOSE 9078
CMD java -Xmx400m -Xss4m -Djava.library.path=/usr/lib/swi-prolog/lib/amd64/ -Djava.awt.headless=true -jar jetty-runner.jar --port 9078 --jar /usr/lib/swi-prolog/lib/jpl.jar acerules.war
