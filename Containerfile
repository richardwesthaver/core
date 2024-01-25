FROM registry.compiler.company/comp/infra/box:latest
WORKDIR /usr/local/src/core
RUN hg pull -u