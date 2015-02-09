#**********************************************************
# Build the .beam erlang VM files
# Makefile for irc bot (based on orbitz bot)
# Laughing man IRC library uses orbitz
#
# Date: 3/1/2008
# Author: Berlin Brown
#
# Note:
# To add eunit 
# ~/.erlang
# code:add_pathz("/Users/dave/Erlang/common/eunit/ebin").
#**********************************************************

TOPDIR := $(shell pwd)
DATE   = $(shell date +%Y%m%d)
WIN_TOPDIR := $(shell cygpath -w `pwd`)

APPLICATION = octane_mel 
VERSION     = 0.0.1

ESRC  = ./src
EBIN  = ./ebin

TEST_DIR     = ./test/erlang
TEST_DIR_SRC = $(TEST_DIR)

ERLC  = erlc
ERL   = erl

OPT   = -W
INC   = $(ESRC)/inc

SED   = $(shell which sed)

TMP         = $(wildcard *~) $(wildcard src/*~) $(wildcard inc/*~)
INC_FILES   = $(wildcard $(INC)/*.hrl)
SRC         = $(wildcard $(ESRC)/*.erl)
CONFFILES   = conf/config.xml $(wildcard conf/*fortune)

TARGET      = $(addsuffix .beam, $(basename \
                 $(addprefix $(EBIN)/, $(notdir $(SRC)))))

LIB_TARGET_OBJS = $(EBIN)/octane_mel.beam     \
				$(EBIN)/mel_scheme.beam       \
				$(EBIN)/mel_input_reader.beam \
				$(EBIN)/mel_procedures.beam   \
				$(EBIN)/mel_scheme_util.beam  \
				$(EBIN)/mel_object.beam       \

TEST_OBJS = $(EBIN)/test_whitespace.beam      \
			$(EBIN)/test_input_reader.beam    \
			$(EBIN)/test_object.beam    \
			$(EBIN)/test_all.beam

${APPLICATION}: $(TARGET) $(LIB_TARGET_OBJS) $(TEST_OBJS)

all: clean ${APPLICATION}

clean:
	-rm -vf $(TARGET) $(TMP)
	-rm -vf erl_crash.dump
	-rm -vf ebin/*.beam

ebin/%.beam: $(TEST_DIR_SRC)/%.erl
	$(ERLC) $(OPT) -I $(INC) -o ebin $<

ebin/%.beam: $(ESRC)/%.erl
	$(ERLC) $(OPT) -I $(INC) -o ebin $<

# Start the erl emulator process with the social stats module
# Also -s erlang halt
run: $(APPLICATION)
	$(ERL) -noshell -pa $(TOPDIR)/ebin/ -s octane_mel mel_main -s erlang halt

test: $(APPLICATION)
	$(ERL) -noshell -pa $(TOPDIR)/ebin/ -s test_all run_tests -s erlang halt

test_main: $(APPLICATION)
	$(ERL) -noshell -pa $(TOPDIR)/ebin/ -s test_all test_main -s erlang halt

winrun: $(APPLICATION)
	${TOPDIR}/test.sh

# End of the file 
