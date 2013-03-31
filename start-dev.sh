#!/bin/sh
APP=zerpc
ERL=erl
COOKIE=erlang
NODE_NAME=$APP
CONFIG=priv/app.config
LIBS_DIR="deps"

exec $ERL -pa ebin \
    -boot start_sasl \
    -sname $NODE_NAME \
    -setcookie $COOKIE \
    -config $CONFIG \
    -env ERL_LIBS $LIBS_DIR \
    -eval "application:start($APP)"
    -s sync go
