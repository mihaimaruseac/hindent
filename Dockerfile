ARG debian_version='stretch-slim'
ARG ghc_version='8.2.2'


FROM haskell:"${ghc_version}" as build
ARG hindent_version='5.3.1'
ARG stack_resolver_version='lts-11.22'
RUN stack --resolver "${stack_resolver_version}" install \
    --ghc-options='-fPIC -optl-static' "hindent-${hindent_version}"


FROM debian:"${debian_version}"

LABEL org.opencontainers.image.title='hindent'
LABEL org.opencontainers.image.url='https://github.com/mihaimaruseac/hindent'

ARG libgmp_dev_version='2:6.1.2+dfsg-1'

RUN apt-get update \
  && apt-get install --assume-yes --no-install-recommends \
    "libgmp-dev=${libgmp_dev_version}" \
  && apt-get clean \
  && rm --force --recursive /var/lib/apt/lists/*

COPY --from=build /root/.local/bin/hindent /usr/local/bin/hindent

ENTRYPOINT ["hindent"]
