FROM infra/box
RUN hg clone https://vc.compiler.company/comp/core
WORKDIR core