FROM ocaml/opam:debian

# Download dependencies
RUN sudo apt-get update \
    && sudo apt-get install -y pkg-config
RUN opam install -y dune graphics

# Set working directory
WORKDIR /home/opam/code

# Copy project files
COPY . .
RUN sudo chown -R opam .

# Build
RUN eval $(opam env) \
    && make compile

ENTRYPOINT [ "./bin/gomoku" ]
