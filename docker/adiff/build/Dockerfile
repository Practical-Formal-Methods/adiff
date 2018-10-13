# this is the environment in which we want to build (and test) adiff

FROM adiff/all-verifiers

# install stack
RUN wget -qO- https://get.haskellstack.org/ | sh
RUN stack install happy --resolver=lts-11.10

ENV PATH "$PATH:/root/.local/bin"
