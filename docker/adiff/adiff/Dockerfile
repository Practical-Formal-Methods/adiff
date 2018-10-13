FROM adiff/all-verifiers


#COPY .stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.3/8.2.2/bin/adiff-viewer /root/.local/bin
#COPY .stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.3/8.2.2/bin/adiff /root/.local/bin

COPY .stack-work/docker/_home/.local/bin/adiff-viewer /root/.local/bin
COPY .stack-work/docker/_home/.local/bin/adiff-server /root/.local/bin
COPY .stack-work/docker/_home/.local/bin/adiff /root/.local/bin
COPY .stack-work/docker/_home/.local/bin/adiff-run-package /root/.local/bin

COPY docker/adiff/adiff/bashrc /root/.bashrc
