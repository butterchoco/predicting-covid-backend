FROM haskell:8.8.4-buster

RUN mkdir -p /usr/src/app

WORKDIR /usr/src/app

COPY . /usr/src/app/

RUN cabal update

RUN cabal install --only-dependencies -v

EXPOSE 3000

CMD [ "cabal", "run" ]