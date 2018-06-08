FROM alpine:3.7 AS builder

RUN apk update
RUN apk add ghc cabal alpine-sdk binutils linux-headers musl-dev gmp-dev zlib-dev postgresql-dev libpq
ENV PATH ${PATH}:/root/.cabal/bin

# Update cabal and install required packages
RUN cabal update
RUN cabal install happy

COPY ./cabal.project /opt/src/
COPY ./matter-language/ /opt/src/matter-language/
COPY ./matter-service/ /opt/src/matter-service/
COPY ./matter-server/ /opt/src/matter-server/

WORKDIR /opt/src/matter-server

# Build in a cabal sandbox
RUN cabal sandbox init
RUN cabal sandbox add-source /opt/src/matter-language/
RUN cabal sandbox add-source /opt/src/matter-service/
RUN cabal install --only-dependencies
RUN cabal configure -O2 --disable-executable-dynamic --disable-shared
RUN cabal build
RUN cabal --bindir=/opt/bin/ install

FROM alpine:3.7

RUN apk update
RUN apk add --no-cache linux-headers musl gmp zlib libpq libffi

WORKDIR /app/
ENV PATH "$PATH:/app"
COPY --from=builder /opt/bin /app

## Check dynamic library dependencies
#RUN ldd matter-server-app

ENTRYPOINT ["matter-server-app"]