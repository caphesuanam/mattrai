FROM haskell:8.6.3
RUN apt-get update
RUN apt-get -y install curl
RUN stack setup --resolver lts-13.13
RUN stack build --fast safe-0.3.17 --resolver lts-13.13
#RUN stack build --fast haskell-src-exts-1.20.3 --resolver lts-13.13
RUN stack build --fast vector-0.12.0.2 --resolver lts-13.13
RUN stack build --fast scientific-0.3.6.2 --resolver lts-13.13
RUN stack build --fast mtl-2.2.2 --resolver lts-13.13
RUN stack build --fast aeson-1.4.2.0 --resolver lts-13.13
RUN stack build --fast happstack-server-7.5.1.3 --resolver lts-13.13
RUN stack build --fast wreq-0.5.3.1 --resolver lts-13.13
RUN stack build --fast strings-1.1 --resolver lts-13.13
RUN stack build --fast aeson-lens-0.5.0.0 --resolver lts-13.13
RUN mkdir -p /service/app
RUN mkdir -p /service/test
RUN mkdir -p /service/src
RUN mkdir -p /service/static
COPY ./stack.yaml /service
COPY ./package.yaml /service
WORKDIR /service
RUN stack build --dependencies-only --fast
COPY ./src /service/src
COPY ./app /service/app
COPY ./static /service/static
RUN mkdir -p /service/conf
#RUN stack build --fast
EXPOSE 8000
CMD stack build --fast && stack exec dashboard-exe
