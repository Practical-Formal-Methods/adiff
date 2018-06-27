# This image has the following verifiers installed:
# * CBMC
# * Ultimate Automizer
# * Ultimate Taipan
# * CPAchecker
# * Seahorn / Seacrab
# * KLEE

FROM ubuntu:16.04

USER root
RUN apt-get update && \
    apt-get -y install \
    bison \
    build-essential \
    cbmc \
    clang-3.8 \
    clang-3.9 \
    clang-5.0 \
    cmake \
    flex \
    g++-multilib \
    gcc-multilib \
    git \
    java-common \
    libasound2 \
    libboost-system-dev \
    libcairo2-dev \
    libfontconfig1 \
    libfreetype6 \
    libgmp-dev \
    libwww-perl \
    libx11-6 \
    libxau6 \
    libxcb1 \
    libxdmcp6 \
    libxext6 \
    libxi6 \
    libxrender1 \
    libxtst6 \
    libz3-dev \
    llvm-3.9 \
    lsb \
    ncurses-dev \
    openjdk-8-jdk-headless \
    patch \
    python2.7 \
    software-properties-common\
    subversion \
    sudo \
    time \
    unzip \
    vim \
    wget \
    zlib1g-dev


RUN export MAKEFLAGS=-j$(nproc)

# We will add symlinks to all tools into this folder
RUN mkdir -p "/root/.local/bin"



# Ultimate Automizer
RUN cd /tmp && \
    wget "https://github.com/ultimate-pa/ultimate/releases/download/v0.1.23/UltimateAutomizer-linux.zip" && \
    mkdir -p /verifiers && \
    unzip UltimateAutomizer-linux.zip -d /verifiers/ && \
    rm UltimateAutomizer-linux.zip && \
    ln -s /verifiers/UAutomizer-linux/Ultimate.py ~/.local/bin/Automizer.py && \
    ln -s /verifiers/UAutomizer-linux/z3 ~/.local/bin && \
    ln -s /verifiers/UAutomizer-linux/cvc4 ~/.local/bin

# Ultimate Taipan
RUN cd /tmp && \
    wget "https://github.com/ultimate-pa/ultimate/releases/download/v0.1.23/UltimateTaipan-linux.zip" && \
    unzip UltimateTaipan-linux.zip -d /verifiers/ && \
    rm UltimateTaipan-linux.zip && \
    ln -s /verifiers/UTaipan-linux/Ultimate.py ~/.local/bin/Taipan.py




# Seahorn
RUN cd /tmp && \
    git clone "https://github.com/seahorn/seahorn.git" && \
    cd seahorn && \
    git checkout 9c4a917a595e11d673eed216701df106b73fdbda


RUN cd /tmp/seahorn && \
    mkdir build; cd build && \
    cmake -DCMAKE_INSTALL_PREFIX="/verifiers/seahorn" ../ && \
    cmake --build .

RUN cd /tmp/seahorn/build && \
    cmake --build . --target extra && cmake ..

RUN cd /tmp/seahorn/build && \
    cmake --build . --target crab && cmake ..

RUN cd /tmp/seahorn/build && \
    cmake --build . --target install

RUN rm -rf /tmp/seahorn
RUN ln -s /verifiers/seahorn/bin/sea ~/.local/bin/sea

# KLEE
RUN git config --global user.email "x@y.com"  &&\
    git config --global user.name  "x y"

RUN cd /tmp && git clone https://github.com/klee/klee.git && \
	cd /tmp/klee && \
  git checkout 4211cea27f1903f68c2b32267eb34cb95c24b9f3 && \
	git fetch origin pull/605/head:pull605 ;\
	git fetch origin pull/729/head:pull729 ;\
	git merge pull605 ;\
	git merge -X theirs pull729 ;\
	mkdir build &&\
	cd build &&\
	cmake -DENABLE_SOLVER_Z3=ON \
		-DENABLE_SYSTEM_TESTS=OFF \
		-DENABLE_UNIT_TESTS=OFF \
		-DLLVM_CONFIG_BINARY=$(which llvm-config-3.8) .. && \
	make && \
	make install && \
	cd /tmp && \
	rm -Rf klee

# 2LS
 RUN cd /tmp && git clone https://github.com/diffblue/2ls.git && \
     cd 2ls && git checkout 2ls-0.6 && \
     ./install.sh &&  \
     cp /tmp/2ls/src/2ls/2ls /usr/local/bin && \
     rm -Rf /tmp/2ls

# Smack (this is not a very clean installation)
RUN cd /tmp && git clone https://github.com/smackers/smack.git && \
    cd /tmp/smack && git checkout v1.9.0 && \
    sed -i 's/TEST_SMACK=1/TEST_SMACK=0/' bin/build.sh && \
    ./bin/build.sh

# CPAchecker (latest stable release 1.7)
RUN cd /tmp && \
  wget "https://cpachecker.sosy-lab.org/CPAchecker-1.7-unix.tar.bz2" && \
  tar -xf CPAchecker-1.7-unix.tar.bz2 --directory /verifiers && \
#  ln -s /verifiers/CPAchecker-1.7-unix/scripts/cpa.sh ~/.local/bin && \
  rm CPAchecker-1.7-unix.tar.bz2

# CPAchecker (using trunk)
RUN cd /tmp && \
wget "https://vcloud.sosy-lab.org/cpachecker/webclient/tool/" -O cpachecker.zip && \
mkdir /verifiers/cpachecker && \
unzip cpachecker.zip -d /verifiers/cpachecker && \
ln -s /verifiers/cpachecker/scripts/cpa.sh ~/.local/bin && \
chmod +x /verifiers/cpachecker/scripts/cpa.sh && \
rm cpachecker.zip
