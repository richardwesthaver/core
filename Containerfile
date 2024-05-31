ARG IMAGE=registry.compiler.company/comp/infra/worker
ARG VERSION=latest
FROM ${IMAGE}:${VERSION}
COPY --chown=${DEV} . core
WORKDIR core