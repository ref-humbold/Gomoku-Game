FROM ocaml/opam:debian

WORKDIR /code

# Download dependencies
RUN apt-get update \
    && apt-get install -y pkg-config
RUN opam install -y dune graphics

# Copy project files
COPY --chown=opam . .

# Build
RUN make compile
