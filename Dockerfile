FROM rust:1-buster
MAINTAINER Mikkel Kroman <mk@maero.dk>

WORKDIR /usr/src/app

COPY . .

RUN cd irc && cargo build --release
RUN strip --strip-unneeded target/release/rink-irc

FROM debian:buster-slim

WORKDIR /app

RUN apt update && \
  apt install -y libssl1.1 ca-certificates && \
  rm -rf /var/lib/apt/lists/*

COPY --from=0 /usr/src/app/target/release/rink-irc .

ADD https://gist.githubusercontent.com/mkroman/f90d4bc9fc97d665ebb596daaa5a1db2/raw servers/uplink.json

CMD ["./rink-irc"]
