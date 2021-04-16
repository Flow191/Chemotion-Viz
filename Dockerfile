FROM rocker/verse:latest

MAINTAINER Florian Schmidt <florianxschmidt@web.de>

LABEL Description="Chemotion_viz R environment."

RUN apt-get -y update && apt-get -y install wget default-jdk-headless

ADD binder/install.R /tmp
RUN R -e "source('/tmp/install.R')"
