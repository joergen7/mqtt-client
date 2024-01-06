FROM alpine:3.18.4
LABEL maintainer="joergen@cuneiform-lang.org"

ENV RACKET_VSN=8.11.1

RUN apk add --update git make gcc musl-dev fontconfig cairo # libjpeg glib pango

WORKDIR /root
RUN git clone https://github.com/racket/racket.git -b v${RACKET_VSN}

WORKDIR /root/racket
RUN PREFIX=/usr make base && ln -s /root/racket/racket/bin/racket /usr/bin && ln -s /root/racket/racket/bin/raco /usr/bin

#RUN raco pkg install --deps search-auto mqtt-client --catalog ${RACKET_VSN}

ENTRYPOINT ["sh"]