FROM vdiff/all-verifiers


#COPY .stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.3/8.2.2/bin/vdiff-viewer /root/.local/bin
#COPY .stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.3/8.2.2/bin/vdiff /root/.local/bin

COPY .stack-work/docker/_home/.local/bin/vdiff-viewer /root/.local/bin
COPY .stack-work/docker/_home/.local/bin/vdiff-server /root/.local/bin
COPY .stack-work/docker/_home/.local/bin/vdiff /root/.local/bin
COPY .stack-work/docker/_home/.local/bin/vdiff-run-package /root/.local/bin

COPY docker/vdiff/vdiff/bashrc /root/.bashrc
