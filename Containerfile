FROM quay.io/fedora/fedora:42 as ocicl

RUN dnf -y install sbcl git
RUN git clone https://github.com/ocicl/ocicl /tmp/ocicl
RUN cd /tmp/ocicl && sbcl --load setup.lisp

FROM quay.io/fedora/fedora:42 as ctfg
COPY --from=ocicl /root/.local/bin/ocicl /usr/bin/ocicl
RUN dnf -y install sbcl make pkg-config libfixposix-devel gcc redhat-rpm-config sqlite
RUN mkdir /ctfg
COPY . /ctfg
WORKDIR /ctfg
RUN ocicl setup | tee ~/.sbclrc
RUN ocicl install
RUN make
RUN mv ctfg /usr/bin/
RUN chown -R 1000 /ctfg

USER 1000
ENTRYPOINT [ "ctfg" ]
