#!/bin/sh

TOPDIR=`pwd`
EBIN_DIR=${TOPDIR}/ebin/
EBIN_DIR=`cygpath -w $EBIN_DIR`
echo $EBIN_DIR
erl -noshell -pa $EBIN_DIR -s octane_mel mel_main -s erlang halt

