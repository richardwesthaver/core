FROM registry.compiler.company/comp/infra/box:latest
COPY --chown=${DEV} . core
WORKDIR core