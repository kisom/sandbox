# C++ build environment. Useful with the following script that should
# be run from a repo:
##!/bin/sh
#
#CONTAINER="{CONTAINER:-cc-build:latest}"
#
#docker run -i -t -v $(pwd):/workspace ${CONTAINER} bash
#
# NB: Ubuntu 17.10 seems to be the only common distro with C++17 support.
FROM ubuntu:17.10
MAINTAINER kyle

# install tools
RUN apt-get -y update && apt-get -y install clang-5.0 autoconf automake libtool cmake ninja-build texinfo curl mg

# make the env pick up the correct compilers.
ENV CXX=clang++-5.0
ENV CC=clang-5.0

# set up workspace
RUN mkdir /workspace
WORKDIR /workspace

# start compiling already
CMD "bash"
