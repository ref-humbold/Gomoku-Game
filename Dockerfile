FROM ocaml/opam:debian

RUN sudo apt-get update \
    && sudo apt-get install -y pkg-config
RUN opam install -y dune graphics

WORKDIR /home/opam/code

COPY --chown=opam . .

RUN eval $(opam env) \
    && make compile

ENTRYPOINT ["./bin/gomoku"]
