FROM debian:stable

WORKDIR /code

# Download dependencies
RUN apt-get update \
    && apt-get install -y ocaml opam make
RUN opam install -y dune graphics

# Copy project files
COPY . .

# Build
RUN make compile
