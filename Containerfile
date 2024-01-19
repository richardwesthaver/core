FROM registry.compiler.company/comp/infra/box:latest
WORKDIR /usr/local/src/infra
RUN hg pull -u
RUN make build
RUN scripts/install-nu-pack.sh
RUN make clean
WORKDIR /usr/local/src/core
RUN hg pull -u