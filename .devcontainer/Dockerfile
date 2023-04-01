FROM ubuntu

RUN apt-get update \
  && apt-get install -y build-essential curl libffi-dev libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 \
  && mkdir -p "$HOME/.ghcup/bin" \
  && curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "$HOME/.ghcup/bin/ghcup" \
  && chmod +x "$HOME/.ghcup/bin/ghcup"

# I don't know why, but $HOME cannot be used here. Assumed $HOME = /root.
ENV PATH "/root/.cabal/bin:/root/.ghcup/bin:$PATH"

RUN ghcup install ghc recommended --set \
  && ghcup install cabal recommended --set \
  && ghcup install hls recommended --set
# todo: cabal update
