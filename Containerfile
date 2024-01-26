FROM registry.compiler.company/comp/infra/box:latest
RUN hg clone https://vc.compiler.company/comp/core
WORKDIR core