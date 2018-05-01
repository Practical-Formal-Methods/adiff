# this is the environment in which we want to build (and test) vdiff

FROM vdiff/all-verifiers

# install stack
RUN wget -qO- https://get.haskellstack.org/ | sh
RUN stack install happy --resolver=lts-11.3

ENV PATH "$PATH:/root/.local/bin"
